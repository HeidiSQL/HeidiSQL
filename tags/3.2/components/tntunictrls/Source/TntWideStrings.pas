
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntWideStrings;

{$INCLUDE TntCompilers.inc}

interface

{$IFDEF COMPILER_10_UP}
  {$MESSAGE FATAL 'Do not refer to TntWideStrings.pas.  It works correctly in Delphi 2006.'}
{$ENDIF}

uses
  Classes;

{******************************************************************************}
{                                                                              }
{  Delphi 2005 introduced TWideStrings in WideStrings.pas.                     }
{    Unfortunately, it was not ready for prime time.                           }
{      Setting CommaText is not consistent, and it relies on CharNextW         }
{         Which is only available on Windows NT+.                              }
{                                                                              }
{******************************************************************************}

type
  TWideStrings = class;

{ IWideStringsAdapter interface }
{ Maintains link between TWideStrings and IWideStrings implementations }

  IWideStringsAdapter = interface
    ['{25FE0E3B-66CB-48AA-B23B-BCFA67E8F5DA}']
    procedure ReferenceStrings(S: TWideStrings);
    procedure ReleaseStrings;
  end;

  TWideStringsEnumerator = class
  private
    FIndex: Integer;
    FStrings: TWideStrings;
  public
    constructor Create(AStrings: TWideStrings);
    function GetCurrent: WideString;
    function MoveNext: Boolean;
    property Current: WideString read GetCurrent;
  end;

{ TWideStrings class }

  TWideStrings = class(TPersistent)
  private
    FDefined: TStringsDefined;
    FDelimiter: WideChar;
    FQuoteChar: WideChar;
    {$IFDEF COMPILER_7_UP}
    FNameValueSeparator: WideChar;
    {$ENDIF}
    FUpdateCount: Integer;
    FAdapter: IWideStringsAdapter;
    function GetCommaText: WideString;
    function GetDelimitedText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetDelimitedText(const Value: WideString);
    procedure SetStringsAdapter(const Value: IWideStringsAdapter);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
    function GetDelimiter: WideChar;
    procedure SetDelimiter(const Value: WideChar);
    function GetQuoteChar: WideChar;
    procedure SetQuoteChar(const Value: WideChar);
    function GetNameValueSeparator: WideChar;
    {$IFDEF COMPILER_7_UP}
    procedure SetNameValueSeparator(const Value: WideChar);
    {$ENDIF}
    function GetValueFromIndex(Index: Integer): WideString;
    procedure SetValueFromIndex(Index: Integer; const Value: WideString);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: WideString; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: WideString): WideString;
    function Get(Index: Integer): WideString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: WideString): Integer; virtual;
  public
    destructor Destroy; override;
    function Add(const S: WideString): Integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TStrings{TNT-ALLOW TStrings}); overload; virtual;
    procedure AddStrings(Strings: TWideStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TWideStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TWideStringsEnumerator;
    function GetTextW: PWideChar; virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: WideString;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: WideString); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: WideString); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetTextW(const Text: PWideChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: WideChar read GetDelimiter write SetDelimiter;
    property DelimitedText: WideString read GetDelimitedText write SetDelimitedText;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: WideChar read GetQuoteChar write SetQuoteChar;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: WideString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: WideChar read GetNameValueSeparator {$IFDEF COMPILER_7_UP} write SetNameValueSeparator {$ENDIF};
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;
    property StringsAdapter: IWideStringsAdapter read FAdapter write SetStringsAdapter;
  end;

  PWideStringItem = ^TWideStringItem;
  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  PWideStringItemList = ^TWideStringItemList;
  TWideStringItemList = array[0..MaxListSize] of TWideStringItem;

implementation

uses
  Windows, SysUtils, TntSystem, {$IFDEF COMPILER_9_UP} WideStrUtils, {$ELSE} TntWideStrUtils, {$ENDIF}
  TntSysUtils, TntClasses;

{ TWideStringsEnumerator }

constructor TWideStringsEnumerator.Create(AStrings: TWideStrings);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

function TWideStringsEnumerator.GetCurrent: WideString;
begin
  Result := FStrings[FIndex];
end;

function TWideStringsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TWideStrings }

destructor TWideStrings.Destroy;
begin
  StringsAdapter := nil;
  inherited;
end;

function TWideStrings.Add(const S: WideString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TWideStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TWideStrings.AddStrings(Strings: TStrings{TNT-ALLOW TStrings});
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.AddStrings(Strings: TWideStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Assign(Source: TPersistent);
begin
  if Source is TWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TWideStrings(Source).FDefined;
      {$IFDEF COMPILER_7_UP}
      FNameValueSeparator := TWideStrings(Source).FNameValueSeparator;
      {$ENDIF}
      FQuoteChar := TWideStrings(Source).FQuoteChar;
      FDelimiter := TWideStrings(Source).FDelimiter;
      AddStrings(TWideStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else if Source is TStrings{TNT-ALLOW TStrings} then
  begin
    BeginUpdate;
    try
      Clear;
      {$IFDEF COMPILER_7_UP}
      FNameValueSeparator := WideChar(TStrings{TNT-ALLOW TStrings}(Source).NameValueSeparator);
      {$ENDIF}
      FQuoteChar := WideChar(TStrings{TNT-ALLOW TStrings}(Source).QuoteChar);
      FDelimiter := WideChar(TStrings{TNT-ALLOW TStrings}(Source).Delimiter);
      AddStrings(TStrings{TNT-ALLOW TStrings}(Source));
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TWideStrings.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TWideStrings then Dest.Assign(Self)
  else if Dest is TStrings{TNT-ALLOW TStrings} then
  begin
    TStrings{TNT-ALLOW TStrings}(Dest).BeginUpdate;
    try
      TStrings{TNT-ALLOW TStrings}(Dest).Clear;
      {$IFDEF COMPILER_7_UP}
      TStrings{TNT-ALLOW TStrings}(Dest).NameValueSeparator := AnsiChar(NameValueSeparator);
      {$ENDIF}
      TStrings{TNT-ALLOW TStrings}(Dest).QuoteChar := AnsiChar(QuoteChar);
      TStrings{TNT-ALLOW TStrings}(Dest).Delimiter := AnsiChar(Delimiter);
      for I := 0 to Count - 1 do
        TStrings{TNT-ALLOW TStrings}(Dest).AddObject(Strings[I], Objects[I]);
    finally
      TStrings{TNT-ALLOW TStrings}(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TWideStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TWideStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TWideStrings then
        Result := not Equals(TWideStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TWideStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TWideStrings.Equals(Strings: TWideStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TWideStrings.Error(const Msg: WideString; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TWideStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(WideLoadResString(Msg), Data);
end;

procedure TWideStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TWideStrings.ExtractName(const S: WideString): WideString;
var
  P: Integer;
begin
  Result := S;
  P := Pos(NameValueSeparator, Result);
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TWideStrings.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

function TWideStrings.GetCommaText: WideString;
var
  LOldDefined: TStringsDefined;
  LOldDelimiter: WideChar;
  LOldQuoteChar: WideChar;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

function TWideStrings.GetDelimitedText: WideString;
var
  S: WideString;
  P: PWideChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := WideString(QuoteChar) + QuoteChar
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not ((P^ in [WideChar(#0)..WideChar(' ')]) or (P^ = QuoteChar) or (P^ = Delimiter)) do
        Inc(P);
      if (P^ <> #0) then S := WideQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TWideStrings.GetName(Index: Integer): WideString;
begin
  Result := ExtractName(Get(Index));
end;

function TWideStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TWideStrings.GetEnumerator: TWideStringsEnumerator;
begin
  Result := TWideStringsEnumerator.Create(Self);
end;

function TWideStrings.GetTextW: PWideChar;
begin
  Result := WStrNew(PWideChar(GetTextStr));
end;

function TWideStrings.GetTextStr: WideString;
var
  I, L, Size, Count: Integer;
  P: PWideChar;
  S, LB: WideString;
begin
  Count := GetCount;
  Size := 0;
  LB := sLineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(WideChar));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L * SizeOf(WideChar));
      Inc(P, L);
    end;
  end;
end;

function TWideStrings.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TWideStrings.IndexOf(const S: WideString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TWideStrings.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  S: WideString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TWideStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TWideStrings.InsertObject(Index: Integer; const S: WideString;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TWideStrings.LoadFromFile(const FileName: WideString);
var
  Stream: TStream;
begin
  Stream := TTntFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWideStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: WideString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size div SizeOf(WideChar));
    Stream.Read(Pointer(S)^, Length(S) * SizeOf(WideChar));
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TWideStrings.Put(Index: Integer; const S: WideString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TWideStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TWideStrings.ReadData(Reader: TReader);
begin
  if Reader.NextValue in [vaString, vaLString] then
    SetTextStr(Reader.ReadString) {JCL compatiblity}
  else if Reader.NextValue = vaWString then
    SetTextStr(Reader.ReadWideString) {JCL compatiblity}
  else begin
    BeginUpdate;
    try
      Clear;
      Reader.ReadListBegin;
      while not Reader.EndOfList do
        if Reader.NextValue in [vaString, vaLString] then
          Add(Reader.ReadString) {TStrings compatiblity}
        else
          Add(Reader.ReadWideString);
      Reader.ReadListEnd;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TWideStrings.SaveToFile(const FileName: WideString);
var
  Stream: TStream;
begin
  Stream := TTntFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWideStrings.SaveToStream(Stream: TStream);
var
  SW: WideString;
begin
  SW := GetTextStr;
  Stream.WriteBuffer(PWideChar(SW)^, Length(SW) * SizeOf(WideChar));
end;

procedure TWideStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

procedure TWideStrings.SetCommaText(const Value: WideString);
begin
  Delimiter := ',';
  QuoteChar := '"';
  SetDelimitedText(Value);
end;

procedure TWideStrings.SetStringsAdapter(const Value: IWideStringsAdapter);
begin
  if FAdapter <> nil then FAdapter.ReleaseStrings;
  FAdapter := Value;
  if FAdapter <> nil then FAdapter.ReferenceStrings(Self);
end;

procedure TWideStrings.SetTextW(const Text: PWideChar);
begin
  SetTextStr(Text);
end;

procedure TWideStrings.SetTextStr(const Value: WideString);
var
  P, Start: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [WideChar(#0), WideChar(#10), WideChar(#13)]) and (P^ <> WideLineSeparator) do
          Inc(P);
        SetString(S, Start, P - Start);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
        if P^ = WideLineSeparator then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TWideStrings.SetValue(const Name, Value: WideString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TWideStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count-1 do begin
    Writer.WriteWideString(Get(I));
  end;
  Writer.WriteListEnd;
end;

procedure TWideStrings.SetDelimitedText(const Value: WideString);
var
  P, P1: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while P^ in [WideChar(#1)..WideChar(' ')] do
      Inc(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := WideExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while (P^ > ' ') and (P^ <> Delimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      while P^ in [WideChar(#1)..WideChar(' ')] do
        Inc(P);
      if P^ = Delimiter then
      begin
        P1 := P;
        Inc(P1);
        if P1^ = #0 then
          Add('');
        repeat
          Inc(P);
        until not (P^ in [WideChar(#1)..WideChar(' ')]);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TWideStrings.GetDelimiter: WideChar;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TWideStrings.GetQuoteChar: WideChar;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

procedure TWideStrings.SetDelimiter(const Value: WideChar);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TWideStrings.SetQuoteChar(const Value: WideChar);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

function TWideStrings.CompareStrings(const S1, S2: WideString): Integer;
begin
  Result := WideCompareText(S1, S2);
end;

function TWideStrings.GetNameValueSeparator: WideChar;
begin
  {$IFDEF COMPILER_7_UP}
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
  {$ELSE}
  Result := '=';
  {$ENDIF}
end;

{$IFDEF COMPILER_7_UP}
procedure TWideStrings.SetNameValueSeparator(const Value: WideChar);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;
{$ENDIF}

function TWideStrings.GetValueFromIndex(Index: Integer): WideString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TWideStrings.SetValueFromIndex(Index: Integer; const Value: WideString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

end.
