program peflags;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

const
  IMAGE_DOS_SIGNATURE = $5A4D;
  IMAGE_NT_SIGNATURE  = $00004550;
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10B;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $20B;

  IMAGE_FILE_RELOCS_STRIPPED = $0001;

  IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA = $0020;
  IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE    = $0040;
  IMAGE_DLLCHARACTERISTICS_NX_COMPAT       = $0100;

type
  TImageDosHeader = packed record
    e_magic: Word;
    e_cblp: Word;
    e_cp: Word;
    e_crlc: Word;
    e_cparhdr: Word;
    e_minalloc: Word;
    e_maxalloc: Word;
    e_ss: Word;
    e_sp: Word;
    e_csum: Word;
    e_ip: Word;
    e_cs: Word;
    e_lfarlc: Word;
    e_ovno: Word;
    e_res: array[0..3] of Word;
    e_oemid: Word;
    e_oeminfo: Word;
    e_res2: array[0..9] of Word;
    e_lfanew: LongInt;
  end;

  TImageFileHeader = packed record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: Cardinal;
    PointerToSymbolTable: Cardinal;
    NumberOfSymbols: Cardinal;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;

  TImageSectionHeader = packed record
    Name: array[0..7] of AnsiChar;
    VirtualSize: Cardinal;
    VirtualAddress: Cardinal;
    SizeOfRawData: Cardinal;
    PointerToRawData: Cardinal;
    PointerToRelocations: Cardinal;
    PointerToLinenumbers: Cardinal;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: Cardinal;
  end;

  TPEInfo = record
    IsPE32Plus: Boolean;
    NumberOfSections: Word;
    FileCharacteristics: Word;
    DllCharacteristics: Word;
    ChecksumOffset: Int64;
    DllCharacteristicsOffset: Int64;
    RelocDirRVA: Cardinal;
    RelocDirSize: Cardinal;
    SectionTableOffset: Int64;
    HasRelocSectionMapping: Boolean;
    RelocSectionName: string;
    RelocFileOffset: Cardinal;
  end;

function BoolToYesNo(B: Boolean): string;
begin
  if B then
    Result := 'yes'
  else
    Result := 'no';
end;

function HasFlag(Value, Flag: Word): Boolean;
begin
  Result := (Value and Flag) <> 0;
end;

procedure ReadBufferExact(Stream: TStream; var Buffer; Count: Longint);
begin
  if Stream.Read(Buffer, Count) <> Count then
    raise Exception.Create('Unexpected end of file');
end;

procedure WriteBufferExact(Stream: TStream; const Buffer; Count: Longint);
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise Exception.Create('Write error');
end;

function TrimSectionName(const Name: array of AnsiChar): string;
var
  I: Integer;
  L: Integer;
begin
  L := Length(Name);
  while (L > 0) and (Name[L - 1] = #0) do
    Dec(L);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I + 1] := Char(Name[I]);
end;

function RVAToFileOffset(Stream: TStream; SectionTableOffset: Int64; NumberOfSections: Word;
  RVA: Cardinal; out FileOffset: Cardinal; out SectionName: string): Boolean;
var
  I: Integer;
  SH: TImageSectionHeader;
  SectSize: Cardinal;
begin
  Result := False;
  FileOffset := 0;
  SectionName := '';

  Stream.Position := SectionTableOffset;
  for I := 0 to NumberOfSections - 1 do
  begin
    ReadBufferExact(Stream, SH, SizeOf(SH));

    if SH.VirtualSize <> 0 then
      SectSize := SH.VirtualSize
    else
      SectSize := SH.SizeOfRawData;

    if (RVA >= SH.VirtualAddress) and (RVA < SH.VirtualAddress + SectSize) then
    begin
      if SH.PointerToRawData = 0 then
        Exit;
      FileOffset := SH.PointerToRawData + (RVA - SH.VirtualAddress);
      if FileOffset >= Cardinal(Stream.Size) then
        Exit;
      SectionName := TrimSectionName(SH.Name);
      Result := True;
      Exit;
    end;
  end;
end;

function LoadPEInfo(Stream: TStream; out Info: TPEInfo): Boolean;
var
  Dos: TImageDosHeader;
  Sig: Cardinal;
  FH: TImageFileHeader;
  Magic: Word;
  OptStart: Int64;
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);

  Stream.Position := 0;
  if Stream.Size < SizeOf(Dos) then
    Exit;

  ReadBufferExact(Stream, Dos, SizeOf(Dos));
  if Dos.e_magic <> IMAGE_DOS_SIGNATURE then
    Exit;
  if Dos.e_lfanew < 0 then
    Exit;

  Stream.Position := Dos.e_lfanew;
  ReadBufferExact(Stream, Sig, SizeOf(Sig));
  if Sig <> IMAGE_NT_SIGNATURE then
    Exit;

  ReadBufferExact(Stream, FH, SizeOf(FH));
  Info.NumberOfSections := FH.NumberOfSections;
  Info.FileCharacteristics := FH.Characteristics;

  OptStart := Stream.Position;
  ReadBufferExact(Stream, Magic, SizeOf(Magic));

  case Magic of
    IMAGE_NT_OPTIONAL_HDR32_MAGIC:
      begin
        Info.IsPE32Plus := False;
        Info.ChecksumOffset := OptStart + 64;
        Info.DllCharacteristicsOffset := OptStart + 70;
      end;
    IMAGE_NT_OPTIONAL_HDR64_MAGIC:
      begin
        Info.IsPE32Plus := True;
        Info.ChecksumOffset := OptStart + 64;
        Info.DllCharacteristicsOffset := OptStart + 70;
      end;
  else
    Exit;
  end;

  Stream.Position := Info.DllCharacteristicsOffset;
  ReadBufferExact(Stream, Info.DllCharacteristics, SizeOf(Info.DllCharacteristics));

  if Info.IsPE32Plus then
    Stream.Position := OptStart + 176
  else
    Stream.Position := OptStart + 160;
  ReadBufferExact(Stream, Info.RelocDirRVA, SizeOf(Info.RelocDirRVA));
  ReadBufferExact(Stream, Info.RelocDirSize, SizeOf(Info.RelocDirSize));

  Info.SectionTableOffset := OptStart + FH.SizeOfOptionalHeader;

  if (Info.RelocDirRVA <> 0) and (Info.RelocDirSize <> 0) then
    Info.HasRelocSectionMapping := RVAToFileOffset(Stream, Info.SectionTableOffset,
      Info.NumberOfSections, Info.RelocDirRVA, Info.RelocFileOffset, Info.RelocSectionName)
  else
    Info.HasRelocSectionMapping := False;

  Result := True;
end;

function ComputePEChecksum(Stream: TStream; ChecksumOffset: Int64): Cardinal;
var
  Buffer: array[0..8191] of Byte;
  Sum: UInt64;
  FilePos, I: Int64;
  BytesRead: Integer;
  W: Word;
  B: Byte;
begin
  Sum := 0;
  FilePos := 0;
  Stream.Position := 0;

  while True do
  begin
    BytesRead := Stream.Read(Buffer, SizeOf(Buffer));
    if BytesRead <= 0 then
      Break;

    I := 0;
    while I < BytesRead do
    begin
      if (FilePos + I = ChecksumOffset) or (FilePos + I = ChecksumOffset + 1) or
         (FilePos + I = ChecksumOffset + 2) or (FilePos + I = ChecksumOffset + 3) then
      begin
        Inc(I);
        Continue;
      end;

      if I + 1 < BytesRead then
      begin
        W := 0;
        if not ((FilePos + I >= ChecksumOffset) and (FilePos + I < ChecksumOffset + 4)) then
          W := Buffer[I];
        if not ((FilePos + I + 1 >= ChecksumOffset) and (FilePos + I + 1 < ChecksumOffset + 4)) then
          W := W or (Word(Buffer[I + 1]) shl 8);
        Sum := Sum + W;
        Sum := (Sum and $FFFF) + (Sum shr 16);
        Inc(I, 2);
      end
      else
      begin
        B := Buffer[I];
        if (FilePos + I >= ChecksumOffset) and (FilePos + I < ChecksumOffset + 4) then
          B := 0;
        Sum := Sum + B;
        Sum := (Sum and $FFFF) + (Sum shr 16);
        Inc(I);
      end;
    end;

    Inc(FilePos, BytesRead);
  end;

  Sum := (Sum and $FFFF) + (Sum shr 16);
  Sum := Sum + (Sum shr 16);
  Result := Cardinal((Sum and $FFFF) + UInt64(Stream.Size));
end;

procedure WriteChecksum(Stream: TStream; ChecksumOffset: Int64);
var
  Checksum: Cardinal;
begin
  Checksum := ComputePEChecksum(Stream, ChecksumOffset);
  Stream.Position := ChecksumOffset;
  WriteBufferExact(Stream, Checksum, SizeOf(Checksum));
end;

function ASLREffective(const Info: TPEInfo): Boolean;
begin
  Result := HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE) and
            ((Info.FileCharacteristics and IMAGE_FILE_RELOCS_STRIPPED) = 0) and
            Info.HasRelocSectionMapping and
            (Info.RelocDirSize <> 0);
end;

function GetPolicyExitCode(const Info: TPEInfo): Integer;
begin
  Result := 0;
  if not HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE) then
    Result := Result or 1;
  if not ASLREffective(Info) then
    Result := Result or 2;
  if not HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_NX_COMPAT) then
    Result := Result or 4;
  if Info.IsPE32Plus and not HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA) then
    Result := Result or 8;
end;

procedure PrintPolicyExitCodeHelp;
begin
  Writeln('Policy exit code bits:');
  Writeln('  1 = DYNAMICBASE not set');
  Writeln('  2 = ASLR not effective (missing/invalid relocations)');
  Writeln('  4 = NXCOMPAT not set');
  Writeln('  8 = HIGH_ENTROPY_VA not set on PE32+');
end;

procedure ShowUsage;
begin
  Writeln('peflags - inspect/set/clear PE mitigation flags');
  Writeln('');
  Writeln('Usage:');
  Writeln('  peflags <file.exe|file.dll> --check');
  Writeln('  peflags <file.exe|file.dll> --check --policy');
  Writeln('  peflags <file.exe|file.dll> --set --all');
  Writeln('  peflags <file.exe|file.dll> --set --aslr --nx --high-entropy');
  Writeln('  peflags <file.exe|file.dll> --clear --all');
  Writeln('  peflags <file.exe|file.dll> --clear --high-entropy');
  Writeln('');
  Writeln('Flags:');
  Writeln('  --aslr          DYNAMICBASE');
  Writeln('  --nx            NXCOMPAT');
  Writeln('  --high-entropy  HIGH_ENTROPY_VA (PE32+ only)');
  Writeln('  --all           all three flags');
  Writeln('  --policy        return CI exit code from --check');
  PrintPolicyExitCodeHelp;
end;

procedure PrintInfo(const FileName: string; const Info: TPEInfo);
begin
  Writeln('File: ', FileName);
  if Info.IsPE32Plus then
    Writeln('Format: PE32+ (64-bit)')
  else
    Writeln('Format: PE32 (32-bit)');

  Writeln('ASLR (DYNAMICBASE): ', BoolToYesNo(HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE)));
  Writeln('DEP  (NXCOMPAT):    ', BoolToYesNo(HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_NX_COMPAT)));
  Writeln('HighEntropyVA:      ', BoolToYesNo(HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA)));
  Writeln('Relocs stripped:    ', BoolToYesNo((Info.FileCharacteristics and IMAGE_FILE_RELOCS_STRIPPED) <> 0));
  Writeln('Reloc dir present:  ', BoolToYesNo((Info.RelocDirRVA <> 0) and (Info.RelocDirSize <> 0)));
  Writeln('Reloc dir mapped:   ', BoolToYesNo(Info.HasRelocSectionMapping));
  if Info.HasRelocSectionMapping then
    Writeln('Reloc section:      ', Info.RelocSectionName, ' @ file offset ', IntToHex(Info.RelocFileOffset, 8));
  Writeln('ASLR effective:     ', BoolToYesNo(ASLREffective(Info)));

  if HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE) and not ASLREffective(Info) then
    Writeln('Warning: DYNAMICBASE is set, but relocation data does not look usable.');
  if HasFlag(Info.DllCharacteristics, IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA) and not Info.IsPE32Plus then
    Writeln('Warning: HIGH_ENTROPY_VA on non-PE32+ image has no effect.');
end;

procedure ModifyFlags(const FileName: string; DoSet: Boolean; FlagASLR, FlagNX, FlagHighEntropy: Boolean);
var
  FS: TFileStream;
  Info: TPEInfo;
  DllChars: Word;
begin
  FS := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
  try
    if not LoadPEInfo(FS, Info) then
      raise Exception.Create('Not a valid PE executable');

    DllChars := Info.DllCharacteristics;

    if FlagASLR then
      if DoSet then
        DllChars := DllChars or IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE
      else
        DllChars := DllChars and not IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE;

    if FlagNX then
      if DoSet then
        DllChars := DllChars or IMAGE_DLLCHARACTERISTICS_NX_COMPAT
      else
        DllChars := DllChars and not IMAGE_DLLCHARACTERISTICS_NX_COMPAT;

    if FlagHighEntropy then
    begin
      if Info.IsPE32Plus then
      begin
        if DoSet then
          DllChars := DllChars or IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA
        else
          DllChars := DllChars and not IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA;
      end
      else if DoSet then
        Writeln('Note: skipping HIGH_ENTROPY_VA for PE32 image.');
    end;

    if DllChars <> Info.DllCharacteristics then
    begin
      FS.Position := Info.DllCharacteristicsOffset;
      WriteBufferExact(FS, DllChars, SizeOf(DllChars));
      WriteChecksum(FS, Info.ChecksumOffset);
      if DoSet then
        Writeln('Updated flags.')
      else
        Writeln('Cleared flags.');
    end
    else
      Writeln('No changes needed.');
  finally
    FS.Free;
  end;
end;

var
  FileName: string;
  DoCheck, DoSet, DoClear, DoPolicy, FlagASLR, FlagNX, FlagHighEntropy, FlagAll: Boolean;
  ExitCodeValue: Integer;
  I: Integer;
  S: string;
  FS: TFileStream;
  Info: TPEInfo;
begin
  try
    if ParamCount < 2 then
    begin
      ShowUsage;
      Halt(1);
    end;

    FileName := ParamStr(1);
    DoCheck := False;
    DoSet := False;
    DoClear := False;
    DoPolicy := False;
    FlagASLR := False;
    FlagNX := False;
    FlagHighEntropy := False;
    FlagAll := False;
    ExitCodeValue := 0;

    for I := 2 to ParamCount do
    begin
      S := LowerCase(ParamStr(I));
      if S = '--check' then
        DoCheck := True
      else if S = '--set' then
        DoSet := True
      else if S = '--clear' then
        DoClear := True
      else if S = '--policy' then
        DoPolicy := True
      else if S = '--aslr' then
        FlagASLR := True
      else if S = '--nx' then
        FlagNX := True
      else if (S = '--high-entropy') or (S = '--highentropy') then
        FlagHighEntropy := True
      else if S = '--all' then
        FlagAll := True
      else if (S = '--help') or (S = '-h') or (S = '/?') then
      begin
        ShowUsage;
        Halt(0);
      end
      else
        raise Exception.Create('Unknown option: ' + ParamStr(I));
    end;

    if DoSet and DoClear then
      raise Exception.Create('Use either --set or --clear, not both.');

    if DoPolicy and not DoCheck and not DoSet and not DoClear then
      DoCheck := True;

    if FlagAll then
    begin
      FlagASLR := True;
      FlagNX := True;
      FlagHighEntropy := True;
    end;

    if DoSet or DoClear then
    begin
      if not (FlagASLR or FlagNX or FlagHighEntropy) then
        raise Exception.Create('No flags selected. Use --aslr, --nx, --high-entropy or --all.');
      ModifyFlags(FileName, DoSet, FlagASLR, FlagNX, FlagHighEntropy);
    end;

    if DoCheck or DoSet or DoClear then
    begin
      FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      try
        if not LoadPEInfo(FS, Info) then
          raise Exception.Create('Not a valid PE executable');
        PrintInfo(FileName, Info);
        if DoPolicy then
          ExitCodeValue := GetPolicyExitCode(Info);
      finally
        FS.Free;
      end;
    end
    else
      ShowUsage;

    if DoPolicy then
      Halt(ExitCodeValue);
  except
    on E: Exception do
    begin
      Writeln(StdErr, 'Error: ', E.Message);
      Halt(1);
    end;
  end;
end.

