{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynUnicode.pas by Maël Hörz, released 2004-05-30.
All Rights Reserved.
TUnicodeStrings/TUnicodeStringList-code (originally written by Mike Lischke) is based
on JclUnicode.pas which is part of the JCL (www.delphi-jedi.org).

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynUnicode.pas,v 1.1.3.19 2012/11/07 08:54:20 CodehunterWorks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Provides:
- Unicode(PWideChar) versions of the most important PAnsiChar-functions in
  SysUtils and some functions unavailable in Delphi 5.
- An adapted and lighter version of TUnicodeStrings/TUnicodeStringList taken
  from JCL, but made portable.
- function for loading and saving of Unicode files, and detecting the encoding
- Unicode clipboard support
- Unicode-version of TCanvas-methods
- Some character constants like CR&LF.

Last Changes:
- 1.1.3.19: Added TUnicodeStringList.CustomSort
-------------------------------------------------------------------------------}

{$IFNDEF QSYNUNICODE}
unit SynUnicode;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Messages,
  Controls,
  Forms,
  Graphics,
  Clipbrd,
  {$IFDEF SYN_COMPILER_6_UP}
  Types,
  {$ENDIF}
  Classes,
  SysUtils,
  TypInfo;

{$IFNDEF SYN_COMPILER_6_UP}
type
  UTF8String = type string;
  PUTF8String = ^UTF8String;
{$ENDIF}
{$IFNDEF UNICODE}
type
  UnicodeString = WideString;
{$ENDIF}

const
  SLineBreak = {$IFDEF SYN_LINUX} #10 {$ELSE} #13#10 {$ENDIF};
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  UTF16BOMLE: array[0..1] of Byte = ($FF, $FE);
  UTF16BOMBE: array[0..1] of Byte = ($FE, $FF);
  UTF32BOMLE: array[0..3] of Byte = ($FF, $FE, $00, $00);
  UTF32BOMBE: array[0..3] of Byte = ($00, $00, $FE, $FF);

const
  // constants describing range of the Unicode Private Use Area (Unicode 3.2)
  PrivateUseLow = WideChar($E000);
  PrivateUseHigh = WideChar($F8FF);
  // filler char: helper for painting wide glyphs 
  FillerChar = PrivateUseLow;

const
  WideNull = WideChar(#0);
  WideTabulator = WideChar(#9);
  WideSpace = WideChar(#32);

  // logical line breaks
  WideLF = WideChar(#10);
  WideLineFeed = WideChar(#10);
  WideVerticalTab = WideChar(#11);
  WideFormFeed = WideChar(#12);
  WideCR = WideChar(#13);
  WideCarriageReturn = WideChar(#13);
  WideCRLF = UnicodeString(#13#10);
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

  // byte order marks for Unicode files
  // Unicode text files (in UTF-16 format) should contain $FFFE as first character to
  // identify such a file clearly. Depending on the system where the file was created
  // on this appears either in big endian or little endian style.
  BOM_LSB_FIRST = WideChar($FEFF);
  BOM_MSB_FIRST = WideChar($FFFE);

type
  TSaveFormat = (sfUTF16LSB, sfUTF16MSB, sfUTF8, sfAnsi);

const
  sfUnicodeLSB = sfUTF16LSB;
  sfUnicodeMSB = sfUTF16MSB;

type
  TFontCharSet = 0..255;

{$IFDEF UNICODE}
  TUnicodeStrings = TStrings;
{$ELSE}
{ TUnicodeStrings }

  TUnicodeStrings = class;

  // Event used to give the application a chance to switch the way of how to save
  // the text in TUnicodeStrings if the text contains characters not only from the
  // ANSI block but the save type is ANSI. On triggering the event the application
  // can change the property SaveUnicode as needed. This property is again checked
  // after the callback returns.
  TConfirmConversionEvent = procedure (Sender: TUnicodeStrings; var Allowed: Boolean) of object;

  TUnicodeStrings = class(TPersistent)
  private
    FUpdateCount: Integer;
    FSaved: Boolean;        // set in SaveToStream, True in case saving was successfull otherwise False
    FOnConfirmConversion: TConfirmConversionEvent;
    FSaveFormat: TSaveFormat;  // overrides the FSaveUnicode flag, initialized when a file is loaded,
                               // expect losses if it is set to sfAnsi before saving
    function GetCommaText: UnicodeString;
    function GetName(Index: Integer): UnicodeString;
    function GetValue(const Name: UnicodeString): UnicodeString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: UnicodeString);
    procedure SetValue(const Name, Value: UnicodeString);
    procedure WriteData(Writer: TWriter);
    function GetSaveUnicode: Boolean;
    procedure SetSaveUnicode(const Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoConfirmConversion(var Allowed: Boolean); virtual;
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): UnicodeString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: UnicodeString; virtual;
    procedure Put(Index: Integer; const S: UnicodeString); virtual; abstract;
    procedure PutObject(Index: Integer; AObject: TObject); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
  public
    constructor Create;

    function Add(const S: UnicodeString): Integer; virtual;
    function AddObject(const S: UnicodeString; AObject: TObject): Integer; virtual;
    procedure Append(const S: UnicodeString);
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure AddStrings(Strings: TUnicodeStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TUnicodeStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetSeparatedText(Separators: UnicodeString): UnicodeString; virtual;
    function GetText: PWideChar; virtual;
    function IndexOf(const S: UnicodeString): Integer; virtual;
    function IndexOfName(const Name: UnicodeString): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: UnicodeString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: UnicodeString; AObject: TObject);
    procedure LoadFromFile(const FileName: TFileName); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: TFileName); overload; virtual;
    procedure SaveToFile(const FileName: TFileName; WithBOM: Boolean); overload; virtual;
    procedure SaveToStream(Stream: TStream; WithBOM: Boolean = True); virtual;
    procedure SetTextStr(const Value: UnicodeString); virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: UnicodeString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: UnicodeString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: UnicodeString]: UnicodeString read GetValue write SetValue;
    property Saved: Boolean read FSaved;
    property SaveUnicode: Boolean read GetSaveUnicode write SetSaveUnicode default True;
    property SaveFormat: TSaveFormat read FSaveFormat write FSaveFormat default sfUnicodeLSB;
    property Strings[Index: Integer]: UnicodeString read Get write Put; default;
    property Text: UnicodeString read GetTextStr write SetTextStr;

    property OnConfirmConversion: TConfirmConversionEvent read FOnConfirmConversion write FOnConfirmConversion;
  end;
{$ENDIF}

{$IFDEF UNICODE}
  TUnicodeStringList = TStringList;
{$ELSE}
{ TUnicodeStringList }
  
  //----- TUnicodeStringList class
  TDynWideCharArray = array of WideChar;
  TUnicodeStringItem = record
    {$IFDEF OWN_UnicodeString_MEMMGR}
    FString: PWideChar; // "array of WideChar";
    {$ELSE}
    FString: UnicodeString;
    {$ENDIF OWN_UnicodeString_MEMMGR}
    FObject: TObject;
  end;

  TUnicodeStringList = class;
  TUnicodeStringItemList = array of TUnicodeStringItem;
  TUnicodeStringListSortCompare = function (AString1, AString2: UnicodeString): Integer;

  TUnicodeStringList = class(TUnicodeStrings)
  private
    FList: TUnicodeStringItemList;
    FCount: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer); overload;
    procedure QuickSort(L, R: Integer; SCompare: TUnicodeStringListSortCompare); overload;
    procedure InsertItem(Index: Integer; const S: UnicodeString);
    procedure SetSorted(Value: Boolean);
    {$IFDEF OWN_UnicodeString_MEMMGR}
    procedure SetListString(Index: Integer; const S: UnicodeString);
    {$ENDIF OWN_UnicodeString_MEMMGR}
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): UnicodeString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: UnicodeString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    destructor Destroy; override;

    function Add(const S: UnicodeString): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: UnicodeString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: UnicodeString): Integer; override;
    procedure Insert(Index: Integer; const S: UnicodeString); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TUnicodeStringListSortCompare); virtual;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;
{$ENDIF}

{$IFNDEF UNICODE}
{ PWideChar versions of important PAnsiChar functions from SysUtils }
function WStrLen(const Str: PWideChar): Cardinal;
function WStrEnd(const Str: PWideChar): PWideChar;
function WStrMove(Dest: PWideChar; const Source: PWideChar; Count: Integer): PWideChar;
function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
function WStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
function WStrAlloc(Size: Cardinal): PWideChar;
function WStrNew(const Str: PWideChar): PWideChar;
procedure WStrDispose(Str: PWideChar);
{$ENDIF}


{$IFNDEF SYN_COMPILER_6_UP}
{$IFDEF MSWINDOWS}
function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal;
  Source: PWideChar; SourceChars: Cardinal): Cardinal;
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal;
  Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
function UTF8Encode(const WS: UnicodeString): UTF8String;
function UTF8Decode(const S: UTF8String): UnicodeString;
function AnsiToUtf8(const S: string): UTF8String;
function Utf8ToAnsi(const S: UTF8String): string;

function WideCompareStr(const S1, S2: UnicodeString): Integer;
function WideCompareText(const S1, S2: UnicodeString): Integer;
{$ENDIF}
{$ENDIF}

// Kylix has them, but Delphi 5 doesn't and Delphi 6&7 versions are buggy
// in Win9X (fix taken from Troy Wolbrinks TntUnicode-package)
{$IFDEF MSWINDOWS}
{$IFNDEF UNICODE}
var
  DefaultSystemCodePage: Cardinal; // implicitly used when converting AnsiString <--> UnicodeString.
{$ENDIF}
  
function WCharUpper(lpsz: PWideChar): PWideChar;
function WCharUpperBuff(lpsz: PWideChar; cchLength: DWORD): DWORD;
function WCharLower(lpsz: PWideChar): PWideChar;
function WCharLowerBuff(lpsz: PWideChar; cchLength: DWORD): DWORD;
{$ENDIF}
function SynWideUpperCase(const S: UnicodeString): UnicodeString;
function SynWideLowerCase(const S: UnicodeString): UnicodeString;
function SynIsCharAlpha(const C: WideChar): Boolean;
function SynIsCharAlphaNumeric(const C: WideChar): Boolean;
{$IFNDEF UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}

function WideLastDelimiter(const Delimiters, S: UnicodeString): Integer;
function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;
  Flags: TReplaceFlags): UnicodeString;

{ functions taken from JCLUnicode.pas }
function WStrComp(Str1, Str2: PWideChar): Integer;
function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
procedure StrSwapByteOrder(Str: PWideChar);
function WideQuotedStr(const S: UnicodeString; Quote: WideChar): UnicodeString;
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): UnicodeString;
function UnicodeStringOfChar(C: WideChar; Count: Cardinal): UnicodeString;
function WideTrim(const S: UnicodeString): UnicodeString;
function WideTrimLeft(const S: UnicodeString): UnicodeString;
function WideTrimRight(const S: UnicodeString): UnicodeString;
{$IFDEF MSWINDOWS}
function CharSetFromLocale(Language: LCID): TFontCharSet;
function CodePageFromLocale(Language: LCID): Integer;
function KeyboardCodePage: Word;
function KeyUnicode(C: AnsiChar): WideChar;
function StringToUnicodeStringEx(const S: AnsiString; CodePage: Word): UnicodeString;
function UnicodeStringToStringEx(const WS: UnicodeString; CodePage: Word): AnsiString;
{$ENDIF}

{ functions providing same behavior on Win9x and WinNT based systems}
function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;

{ Unicode versions of TCanvas-methods }
function TextExtent(ACanvas: TCanvas; const Text: UnicodeString): TSize;
function TextWidth(ACanvas: TCanvas; const Text: UnicodeString): Integer;
function TextHeight(ACanvas: TCanvas; const Text: UnicodeString): Integer;
procedure TextOut(ACanvas: TCanvas; X, Y: Integer; const Text: UnicodeString);
procedure TextRect(ACanvas: TCanvas; Rect: TRect; X, Y: Integer;
  const Text: UnicodeString);

{ Unicode streaming-support }
type
  TSynEncoding = (seUTF8, seUTF16LE, seUTF16BE, seAnsi);
  TSynEncodings = set of TSynEncoding;

{$IFDEF UNICODE}
  TWideFileStream = TFileStream;
{$ELSE}
  TWideFileStream = class(THandleStream)
  public
    constructor Create(const FileName: UnicodeString; Mode: Word); overload;
    constructor Create(const FileName: UnicodeString; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
  end;

function WideFileOpen(const FileName: UnicodeString; Mode: LongWord): Integer;
function WideFileCreate(const FileName: UnicodeString): Integer; overload;
function WideFileCreate(const FileName: UnicodeString; Rights: Integer): Integer; overload;
{$ENDIF}

function IsAnsiOnly(const WS: UnicodeString): Boolean;
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean; overload;
function IsUTF8(const FileName: UnicodeString; out WithBOM: Boolean): Boolean; overload;
function GetEncoding(const FileName: UnicodeString; out WithBOM: Boolean): TSynEncoding; overload;
function GetEncoding(Stream: TStream; out WithBOM: Boolean): TSynEncoding; overload;
procedure SaveToFile(const WS: UnicodeString; const FileName: UnicodeString;
  Encoding: TSynEncoding; WithBom: Boolean = True); overload;
procedure SaveToFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  Encoding: TSynEncoding; WithBom: Boolean = True); overload;
function LoadFromFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding; overload;
procedure SaveToStream(const WS: UnicodeString; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean  = True); overload;
procedure SaveToStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean  = True); overload;
function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding): TSynEncoding; overload;

function ClipboardProvidesText: Boolean;
function GetClipboardText: UnicodeString;
procedure SetClipboardText(const Text: UnicodeString);

{ misc functions }
{$IFNDEF UNICODE}
{$IFNDEF SYN_COMPILER_6_UP}
function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo): UnicodeString;
procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: UnicodeString);
{$ENDIF}
procedure UnicodeDefineProperties(Filer: TFiler; Instance: TPersistent);
{$ENDIF}
{$IFDEF MSWINDOWS}
function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
function IsUnicodeStringMappableToAnsi(const WS: UnicodeString): Boolean;
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  Win32PlatformIsUnicode: Boolean;
{$ENDIF}

implementation

uses
  SynEditTextBuffer,
  {$IFDEF SYN_UNISCRIBE}
  SynUsp10,
  {$ENDIF}
  Math,
  {$IFDEF SYN_LINUX}
  Libc,
  {$ENDIF}
  {$IFDEF USE_TNT_RUNTIME_SUPPORT}
  TntSysUtils, TntClasses,
  {$ENDIF}
  SysConst,
  {$IFDEF SYN_COMPILER_6_UP}
  RTLConsts;
  {$ELSE}
  Consts;
  {$ENDIF}

{$IFNDEF UNICODE}
{ TUnicodeStrings }

constructor TUnicodeStrings.Create;
begin
  inherited;
  FSaveFormat := sfUnicodeLSB;
end;

function TUnicodeStrings.GetSaveUnicode: Boolean;
begin
  Result := SaveFormat in [sfUTF16LSB, sfUTF16MSB, sfUTF8];
end;

procedure TUnicodeStrings.SetSaveUnicode(const Value: Boolean);
begin
  if Value then
    SaveFormat := sfUnicodeLSB
  else
    SaveFormat := sfAnsi;
end;

function TUnicodeStrings.Add(const S: UnicodeString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TUnicodeStrings.AddObject(const S: UnicodeString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TUnicodeStrings.Append(const S: UnicodeString);
begin
  Add(S);
end;

procedure TUnicodeStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
{$IFDEF MSWINDOWS}
  S: UnicodeString;
  CP: Integer;
{$ENDIF}
begin
  BeginUpdate;
  try
    {$IFDEF MSWINDOWS}
    CP := CodePageFromLocale(GetThreadLocale);
    for I := 0 to Strings.Count - 1 do
    begin
      S := StringToUnicodeStringEx(Strings[I], CP);
      AddObject(S, Strings.Objects[I]);
    end;
    {$ELSE}
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
    {$ENDIF}
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.AddStrings(Strings: TUnicodeStrings);
var
  I: Integer;
begin
  Assert(Strings <> nil);
  
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.Assign(Source: TPersistent);
// usual assignment routine, but able to assign wide and small strings
begin
  if Source is TUnicodeStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TUnicodeStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
  begin
    if Source is TStrings then
    begin
      BeginUpdate;
      try
        Clear;
        AddStrings(TStrings(Source));
      finally
        EndUpdate;
      end;
    end
    else
      inherited Assign(Source);
  end; 
end;

procedure TUnicodeStrings.AssignTo(Dest: TPersistent);
// need to do also assignment to old style TStrings, but this class doesn't know
// TUnicodeStrings, so we need to do it from here
var
  I: Integer;
begin
  if Dest is TStrings then
  begin
    with Dest as TStrings do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to Self.Count - 1 do
          AddObject(Self[I], Self.Objects[I]);
      finally
        EndUpdate;
      end;
    end;
  end
  else
  begin
    if Dest is TUnicodeStrings then
    begin
      with Dest as TUnicodeStrings do
      begin
        BeginUpdate;
        try
          Clear;
          AddStrings(Self);
        finally
          EndUpdate;
        end;
      end;
    end
    else
      inherited;
  end;
end;

procedure TUnicodeStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TUnicodeStrings.DefineProperties(Filer: TFiler);
// Defines a private property for the content of the list.
// There's a bug in the handling of text DFMs in Classes.pas which prevents
// UnicodeStrings from loading under some circumstances. Zbysek Hlinka
// (zhlinka att login dott cz) brought this to my attention and supplied also a solution.
// See ReadData and WriteData methods for implementation details.

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TUnicodeStrings then
        Result := not Equals(TUnicodeStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineProperty('UnicodeStrings', ReadData, WriteData, DoWrite);
end;

procedure TUnicodeStrings.DoConfirmConversion(var Allowed: Boolean);
begin
  if Assigned(FOnConfirmConversion) then
    FOnConfirmConversion(Self, Allowed);
end;

procedure TUnicodeStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

function TUnicodeStrings.Equals(Strings: TUnicodeStrings): Boolean;
var
  I, Count: Integer;
begin
  Assert(Strings <> nil);

  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then
    Exit;
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then
      Exit;
  Result := True;
end;

procedure TUnicodeStrings.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX, [EBP + 4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TUnicodeStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: UnicodeString;
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

function TUnicodeStrings.GetCapacity: Integer;
// Descendants may optionally override/replace this default implementation.
begin
  Result := Count;
end;

function TUnicodeStrings.GetCommaText: UnicodeString;
var
  S: UnicodeString;
  P: PWideChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := '""'
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not (P^ in [WideNull..WideSpace, WideChar('"'), WideChar(',')]) do
        Inc(P);
      if P^ <> WideNull then
        S := WideQuotedStr(S, '"');
      Result := Result + S + ',';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TUnicodeStrings.GetName(Index: Integer): UnicodeString;
var
  P: Integer;
begin
  Result := Get(Index);
  P := Pos('=', Result);
  if P > 0 then
    SetLength(Result, P - 1)
  else
    Result := '';
end;

function TUnicodeStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TUnicodeStrings.GetSeparatedText(Separators: UnicodeString): UnicodeString;
// Same as GetText but with customizable separator characters.
var
  I, L,
  Size,
  Count,
  SepSize: Integer;
  P: PWideChar;
  S: UnicodeString;
begin
  Count := GetCount;
  SepSize := Length(Separators);
  Size := 0;
  for I := 0 to Count - 1 do
    Inc(Size, Length(Get(I)) + SepSize);
    
  // set one separator less, the last line does not need a trailing separator
  SetLength(Result, Size - SepSize);
  if Size > 0 then
  begin
    P := Pointer(Result);
    I := 0;
    while True do
    begin
      S := Get(I);
      L := Length(S);
      if L <> 0 then
      begin
        // add current string
        System.Move(Pointer(S)^, P^, 2 * L);
        Inc(P, L);
      end;
      Inc(I);
      if I = Count then
        Break;
        
      // add separators
      System.Move(Pointer(Separators)^, P^, SizeOf(WideChar) * SepSize);
      Inc(P, SepSize);
    end;
  end;
end;

function TUnicodeStrings.GetTextStr: UnicodeString;
begin
  Result := GetSeparatedText(WideCRLF);
end;

function TUnicodeStrings.GetText: PWideChar;
begin
  Result := WStrNew(PWideChar(GetTextStr));
end;

function TUnicodeStrings.GetValue(const Name: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt)
  else
    Result := '';
end;

function TUnicodeStrings.IndexOf(const S: UnicodeString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if WideCompareText(Get(Result), S) = 0 then
      Exit;
  Result := -1;
end;

function TUnicodeStrings.IndexOfName(const Name: UnicodeString): Integer;
var
  P: Integer;
  S: UnicodeString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P > 0) and (WideCompareText(Copy(S, 1, P - 1), Name) = 0) then
      Exit;
  end;
  Result := -1;
end;

function TUnicodeStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then
      Exit;
  Result := -1;
end;

procedure TUnicodeStrings.InsertObject(Index: Integer; const S: UnicodeString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TUnicodeStrings.LoadFromFile(const FileName: TFileName);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUnicodeStrings.LoadFromStream(Stream: TStream);
// usual loader routine, but enhanced to handle byte order marks in stream
var
  Size,
  BytesRead: Integer;
  ByteOrderMask: array[0..5] of Byte; // BOM size is max 5 bytes (cf: wikipedia)
                                      // but it is easier to implement with a multiple of 2
  Loaded: Boolean;
  SW: UnicodeString;
  SA: AnsiString;
begin
  BeginUpdate;
  try
    Loaded := False;

    Size := Stream.Size - Stream.Position;
    BytesRead := Stream.Read(ByteOrderMask[0], SizeOf(ByteOrderMask));

    // UTF16 LSB = Unicode LSB/LE
    if (BytesRead >= 2) and (ByteOrderMask[0] = UTF16BOMLE[0])
      and (ByteOrderMask[1] = UTF16BOMLE[1]) then
    begin
      FSaveFormat := sfUTF16LSB;
      SetLength(SW, (Size - 2) div SizeOf(WideChar));
      Assert((Size and 1) <> 1, 'Number of chars must be a multiple of 2');
      if BytesRead > 2 then
      begin
        System.Move(ByteOrderMask[2], SW[1], BytesRead - 2); // max 4 bytes = 2 widechars
        if Size > BytesRead then
          // first 2 chars (maximum) were copied by System.Move
          Stream.Read(SW[3], Size - BytesRead);
      end;
      SetTextStr(SW);
      Loaded := True;
    end;

    // UTF16 MSB = Unicode MSB/BE
    if (BytesRead >= 2) and (ByteOrderMask[0] = UTF16BOMBE[0])
      and (ByteOrderMask[1] = UTF16BOMBE[1]) then
    begin
      FSaveFormat := sfUTF16MSB;
      SetLength(SW, (Size - 2) div SizeOf(WideChar));
      Assert((Size and 1) <> 1, 'Number of chars must be a multiple of 2');
      if BytesRead > 2 then
      begin
        System.Move(ByteOrderMask[2], SW[1] ,BytesRead - 2); // max 4 bytes = 2 widechars
        if Size > BytesRead then
          // first 2 chars (maximum) were copied by System.Move
          Stream.Read(SW[3], Size - BytesRead);
        StrSwapByteOrder(PWideChar(SW));
      end;
      SetTextStr(SW);
      Loaded := True;
    end;

    // UTF8
    if (BytesRead >= 3) and (ByteOrderMask[0] = UTF8BOM[0])
      and (ByteOrderMask[1] = UTF8BOM[1]) and (ByteOrderMask[2] = UTF8BOM[2]) then
    begin
      FSaveFormat := sfUTF8;
      SetLength(SA, (Size - 3) div SizeOf(AnsiChar));
      if BytesRead > 3 then
      begin
        System.Move(ByteOrderMask[3], SA[1], BytesRead - 3); // max 3 bytes = 3 chars
        if Size > BytesRead then
          // first 3 chars were copied by System.Move
          Stream.Read(SA[4], Size - BytesRead);
        SW := UTF8Decode(SA);
      end;
      SetTextStr(SW);
      Loaded := True;
    end;

    // default case (Ansi)
    if not Loaded then
    begin
      FSaveFormat := sfAnsi;
      SetLength(SA, Size div SizeOf(AnsiChar));
      if BytesRead > 0 then
      begin
        System.Move(ByteOrderMask[0], SA[1], BytesRead); // max 6 bytes = 6 chars
        if Size > BytesRead then
          Stream.Read(SA[7], Size - BytesRead); // first 6 chars were copied by System.Move
          SW := UTF8Decode(SA);
          if SW <> '' then
           begin
           FSaveFormat := sfUTF8;
           SetTextStr(SW);
           Loaded := True;
           end;
      end;
      if not Loaded then
      SetTextStr(SA);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: UnicodeString;
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

procedure TUnicodeStrings.ReadData(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
      SetTextStr(Reader.ReadString);
  else
    SetTextStr(Reader.ReadWideString);
  end;
end;

procedure TUnicodeStrings.SaveToFile(const FileName: TFileName);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUnicodeStrings.SaveToFile(const FileName: TFileName; WithBOM: Boolean);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

procedure TUnicodeStrings.SaveToStream(Stream: TStream; WithBOM: Boolean = True);
// Saves the currently loaded text into the given stream. WithBOM determines whether to write a
// byte order mark or not. Note: when saved as ANSI text there will never be a BOM.
var
  SW: UnicodeString;
  SA: AnsiString;
  Allowed: Boolean;
  Run: PWideChar;
begin
  // The application can decide in which format to save the content.
  // If FSaveUnicode is False then all strings are saved in standard ANSI format
  // which is also loadable by TStrings but you should be aware that all Unicode
  // strings are then converted to ANSI based on the current system locale.
  // An extra event is supplied to ask the user about the potential loss of
  // information when converting Unicode to ANSI strings.
  SW := GetTextStr;
  Allowed := True;
  FSaved := False; // be pessimistic
  // A check for potential information loss makes only sense if the application has
  // set an event to be used as call back to ask about the conversion.
  if not SaveUnicode and Assigned(FOnConfirmConversion) then
  begin
    // application requests to save only ANSI characters, so check the text and
    // call back in case information could be lost
    Run := PWideChar(SW);
    // only ask if there's at least one Unicode character in the text
    while Run^ in [WideChar(#1)..WideChar(#255)] do
      Inc(Run);
    // Note: The application can still set FSaveUnicode to True in the callback.
    if Run^ <> WideNull then
      DoConfirmConversion(Allowed);
  end;

  if Allowed then
  begin
    // only save if allowed
    case SaveFormat of
      sfUTF16LSB:
        begin
          if WithBOM then
            Stream.WriteBuffer(UTF16BOMLE[0], SizeOf(UTF16BOMLE));
          Stream.WriteBuffer(SW[1], Length(SW) * SizeOf(WideChar));
          FSaved := True;
        end;
      sfUTF16MSB:
        begin
          if WithBOM then
            Stream.WriteBuffer(UTF16BOMBE[0], SizeOf(UTF16BOMBE));
          StrSwapByteOrder(PWideChar(SW));
          Stream.WriteBuffer(SW[1], Length(SW) * SizeOf(WideChar));
          FSaved := True;
        end;
      sfUTF8:
        begin
          if WithBOM then
            Stream.WriteBuffer(UTF8BOM[0], SizeOf(UTF8BOM));
          SA := UTF8Encode(SW);
          Stream.WriteBuffer(SA[1], Length(SA) * SizeOf(AnsiChar));
          FSaved := True;
        end;
      sfAnsi:
        begin
          SA := SW;
          Stream.WriteBuffer(SA[1], Length(SA) * SizeOf(AnsiChar));
          FSaved := True;
        end;
    end;
  end;
end;

procedure TUnicodeStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TUnicodeStrings.SetCommaText(const Value: UnicodeString);
var
  P, P1: PWideChar;
  S: UnicodeString;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while P^ in [WideChar(#1)..WideSpace] do
      Inc(P);
    while P^ <> WideNull do
    begin
      if P^ = '"' then
        S := WideExtractQuotedStr(P, '"')
      else
      begin
        P1 := P;
        while (P^ > WideSpace) and (P^ <> ',') do 
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);

      while P^ in [WideChar(#1)..WideSpace] do
        Inc(P);
      if P^ = ',' then
      begin
        repeat
          Inc(P);
        until not (P^ in [WideChar(#1)..WideSpace]);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.SetTextStr(const Value: UnicodeString);
var
  Head,
  Tail: PWideChar;
  S: UnicodeString;
begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);
    while Head^ <> WideNull do
    begin
      Tail := Head;
      while not (Tail^ in [WideNull, WideLineFeed, WideCarriageReturn, WideVerticalTab, WideFormFeed]) and
        (Tail^ <> WideLineSeparator) and (Tail^ <> WideParagraphSeparator) do
        Inc(Tail);
      SetString(S, Head, Tail - Head);
      Add(S);
      Head := Tail;
      if Head^ <> WideNull then
      begin
        Inc(Head);
        if (Tail^ = WideCarriageReturn) and (Head^ = WideLineFeed) then
          Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TUnicodeStrings.SetValue(const Name, Value: UnicodeString);
var
  I : Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then
      I := Add('');
    Put(I, Name + '=' + Value);
  end
  else
  begin
    if I >= 0 then
      Delete(I);
  end;
end;

procedure TUnicodeStrings.WriteData(Writer: TWriter);
begin
  Writer.WriteWideString(GetTextStr);
end;


{ TUnicodeStringList }

destructor TUnicodeStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  Clear;
  inherited;
end;

function TUnicodeStringList.Add(const S: UnicodeString): Integer;
begin
  if not Sorted then
    Result := FCount
  else
  begin
    if Find(S, Result) then
    begin
      case Duplicates of
        dupIgnore:
          Exit;
        dupError:
          Error(SDuplicateString, 0);
      end;
    end;
  end;
  InsertItem(Result, S);
end;

procedure TUnicodeStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TUnicodeStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TUnicodeStringList.Clear;
{$IFDEF OWN_UnicodeString_MEMMGR}
var
  I: Integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if FCount <> 0 then
  begin
    Changing;
    {$IFDEF OWN_UnicodeString_MEMMGR}
    for I := 0 to FCount - 1 do
      with FList[I] do
        if TDynWideCharArray(FString) <> nil then
          TDynWideCharArray(FString) := nil;
    {$ENDIF OWN_UnicodeString_MEMMGR}
    // this will automatically finalize the array
    FList := nil;
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TUnicodeStringList.Delete(Index: Integer);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;

  {$IFDEF OWN_UnicodeString_MEMMGR}
  SetListString(Index, '');
  {$ELSE}
  FList[Index].FString := '';
  {$ENDIF OWN_UnicodeString_MEMMGR}
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TUnicodeStringItem));
    Pointer(FList[FCount].FString) := nil; // avoid freeing the string, the address is now used in another element
  end;
  Changed;
end;

procedure TUnicodeStringList.Exchange(Index1, Index2: Integer);
begin
  if Cardinal(Index1) >= Cardinal(FCount) then
    Error(SListIndexError, Index1);
  if Cardinal(Index2) >= Cardinal(FCount) then
    Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TUnicodeStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TUnicodeStringItem;
begin
  Temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Temp;
end;

function TUnicodeStringList.Find(const S: UnicodeString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := WideCompareText(FList[I].FString, S);
    if C < 0 then
      L := I+1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function TUnicodeStringList.Get(Index: Integer): UnicodeString;
{$IFDEF OWN_UnicodeString_MEMMGR}
var
  Len: Integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  {$IFDEF OWN_UnicodeString_MEMMGR}
  with FList[Index] do
  begin
    Len := Length(TDynWideCharArray(FString));
    if Len > 0 then
    begin
      SetLength(Result, Len - 1); // exclude #0
      if Result <> '' then
        System.Move(FString^, Result[1], Len * SizeOf(WideChar));
    end
    else
      Result := '';
  end;
  {$ELSE}
  Result := FList[Index].FString;
  {$ENDIF OWN_UnicodeString_MEMMGR}
end;

function TUnicodeStringList.GetCapacity: Integer;
begin
  Result := Length(FList);
end;

function TUnicodeStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TUnicodeStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Result := FList[Index].FObject;
end;

procedure TUnicodeStringList.Grow;
var
  Delta,
  Len: Integer;
begin
  Len := Length(FList);
  if Len > 64 then
    Delta := Len div 4
  else
  begin
    if Len > 8 then
      Delta := 16
    else
      Delta := 4;
  end;
  SetCapacity(Len + Delta);
end;

function TUnicodeStringList.IndexOf(const S: UnicodeString): Integer;
begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else
    if not Find(S, Result) then
      Result := -1;
end;

procedure TUnicodeStringList.Insert(Index: Integer; const S: UnicodeString);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if Cardinal(Index) > Cardinal(FCount) then
    Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

{$IFDEF OWN_UnicodeString_MEMMGR}
procedure TUnicodeStringList.SetListString(Index: Integer; const S: UnicodeString);
var
  Len: Integer;
  A: TDynWideCharArray;
begin
  with FList[Index] do
  begin
    Pointer(A) := TDynWideCharArray(FString);
    if A <> nil then
      A := nil; // free memory

    Len := Length(S);
    if Len > 0 then
    begin
      SetLength(A, Len + 1); // include #0
      System.Move(S[1], A[0], Len * SizeOf(WideChar));
      A[Len] := #0;
    end;

    FString := PWideChar(A);
    Pointer(A) := nil; // do not release the array on procedure exit
  end;
end;
{$ENDIF OWN_UnicodeString_MEMMGR}

procedure TUnicodeStringList.InsertItem(Index: Integer; const S: UnicodeString);
begin
  Changing;
  if FCount = Length(FList) then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TUnicodeStringItem));
  with FList[Index] do
  begin
    Pointer(FString) := nil; // avoid freeing the string, the address is now used in another element
    FObject := nil;
    {$IFDEF OWN_UnicodeString_MEMMGR}
      SetListString(Index, S);
    {$ELSE}
      FString := S;
    {$ENDIF OWN_UnicodeString_MEMMGR}
  end;
  Inc(FCount);
  Changed;
end;

procedure TUnicodeStringList.Put(Index: Integer; const S: UnicodeString);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;

  {$IFDEF OWN_UnicodeString_MEMMGR}
    SetListString(Index, S);
  {$ELSE}
    FList[Index].FString := S;
  {$ENDIF OWN_UnicodeString_MEMMGR}
  Changed;
end;

procedure TUnicodeStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

procedure TUnicodeStringList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: UnicodeString;
begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FString;
    repeat
      while WideCompareText(FList[I].FString, P) < 0 do
        Inc(I);
      while WideCompareText(FList[J].FString, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TUnicodeStringList.QuickSort(L, R: Integer; SCompare: TUnicodeStringListSortCompare);
var
  I, J: Integer;
  P: UnicodeString;
begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FString;
    repeat
      while SCompare(FList[I].FString, P) < 0 do
        Inc(I);
      while SCompare(FList[J].FString, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TUnicodeStringList.CustomSort(Compare: TUnicodeStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

procedure TUnicodeStringList.SetCapacity(NewCapacity: Integer);
begin
  SetLength(FList, NewCapacity);
  if NewCapacity < FCount then
    FCount := NewCapacity;
end;

procedure TUnicodeStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

procedure TUnicodeStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

procedure TUnicodeStringList.Sort;
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1);
    Changed;
  end;
end;
{$ENDIF}

function WStrLen(const Str: PWideChar): Cardinal;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;

function WStrEnd(const Str: PWideChar): PWideChar;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        LEA     EAX,[EDI-2]
        MOV     EDI,EDX
end;

function WStrMove(Dest: PWideChar; const Source: PWideChar; Count: Integer): PWideChar;
begin
  Result := Dest;
  System.Move(Source^, Dest^, Count * SizeOf(WideChar));
end;

function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
{$IFDEF SYN_COMPILER_16_UP}
begin
  Result := SysUtils.StrCopy(Dest, Source)
{$ELSE}
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,1
        REP     MOVSW
        POP     ESI
        POP     EDI
{$ENDIF}
end;

function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
{$IFDEF SYN_COMPILER_16_UP}
begin
  Result := SysUtils.StrLCopy(Dest, Source, MaxLen)
{$ELSE}
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        XOR     AX,AX
        TEST    ECX,ECX
        JZ      @@1
        REPNE   SCASW
        JNE     @@1
        Inc     ECX
@@1:    SUB     EBX,ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,EDI
        MOV     ECX,EBX
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EBX
        AND     ECX,1
        REP     MOVSW
        STOSW
        MOV     EAX,EDX
        POP     EBX
        POP     ESI
        POP     EDI
{$ENDIF}
end;

function WStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  WStrCopy(WStrEnd(Dest), Source);
  Result := Dest;
end;

function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function WStrAlloc(Size: Cardinal): PWideChar;
begin
  Size := SizeOf(WideChar) * Size + SizeOf(Cardinal);
  GetMem(Result, Size);
  Cardinal(Pointer(Result)^) := Size;
  Inc(PByte(Result), SizeOf(Cardinal));
end;

function WStrNew(const Str: PWideChar): PWideChar;
var
  Size: Cardinal;
begin
  if Str = nil then
    Result := nil
  else
  begin
    Size := WStrLen(Str) + 1;
    Result := WStrMove(WStrAlloc(Size), Str, Size);
  end;
end;

procedure WStrDispose(Str: PWideChar);
begin
  if Str <> nil then
  begin
    Dec(PByte(Str), SizeOf(Cardinal));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;

{$IFNDEF SYN_COMPILER_6_UP}
{$IFDEF MSWINDOWS}
function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal;
  Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          Break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count+1] := Char($80 or ((c shr 6) and $3F));
        Dest[count+2] := Char($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          Break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count+1] := Char($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal;
  Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
end;

function Utf8Encode(const WS: UnicodeString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Decode(const S: UTF8String): UnicodeString;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PAnsiChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function AnsiToUtf8(const S: string): UTF8String;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: UTF8String): string;
begin
  Result := Utf8Decode(S);
end;

function DumbItDownFor95(const S1, S2: UnicodeString; CmpFlags: Integer): Integer;
var
  a1, a2: AnsiString;
begin
  a1 := s1;
  a2 := s2;
  Result := CompareStringA(LOCALE_USER_DEFAULT, CmpFlags, PAnsiChar(a1),
    Length(a1), PAnsiChar(a2), Length(a2)) - 2;
end;

function WideCompareStr(const S1, S2: UnicodeString): Integer;
begin
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(S1), Length(S1),
    PWideChar(S2), Length(S2)) - 2;
  case GetLastError of
    0: ;
    ERROR_CALL_NOT_IMPLEMENTED: Result := DumbItDownFor95(S1, S2, 0);
  else
    RaiseLastWin32Error;
  end;
end;

function WideCompareText(const S1, S2: UnicodeString): Integer;
begin
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(S1),
    Length(S1), PWideChar(S2), Length(S2)) - 2;
  case GetLastError of
    0: ;
    ERROR_CALL_NOT_IMPLEMENTED: Result := DumbItDownFor95(S1, S2, NORM_IGNORECASE);
  else
    RaiseLastWin32Error;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
// The Win9X fix for SynWideUpperCase and SynWideLowerCase was taken
// from Troy Wolbrinks, TntUnicode-package.

function WCharUpper(lpsz: PWideChar): PWideChar;
var
  AStr: AnsiString;
  WStr: UnicodeString;
begin
  if Win32PlatformIsUnicode then
    Result := Windows.CharUpperW(lpsz)
  else
  begin
    if HiWord(Cardinal(lpsz)) = 0 then
    begin
      // literal char mode
      Result := lpsz;
      if IsWideCharMappableToAnsi(WideChar(lpsz)) then
      begin
        AStr := AnsiString(WideChar(lpsz)); // single character may be more than one byte
        Windows.CharUpperA(PAnsiChar(AStr));
        WStr := UnicodeString(AStr); // should always be single wide char
        if Length(WStr) = 1 then
          Result := PWideChar(WStr[1]);
      end
    end
    else
    begin
      // null-terminated string mode
      Result := lpsz;
      while lpsz^ <> #0 do
      begin
        lpsz^ := WideChar(SynUnicode.WCharUpper(PWideChar(lpsz^)));
        Inc(lpsz);
      end;
    end;
  end;
end;

function WCharUpperBuff(lpsz: PWideChar; cchLength: DWORD): DWORD;
var
  i: Integer;
begin
  if Win32PlatformIsUnicode then
    Result := Windows.CharUpperBuffW(lpsz, cchLength)
  else
  begin
    Result := cchLength;
    for i := 1 to cchLength do
    begin
      lpsz^ := WideChar(SynUnicode.WCharUpper(PWideChar(lpsz^)));
      Inc(lpsz);
    end;
  end;
end;

function WCharLower(lpsz: PWideChar): PWideChar;
var
  AStr: AnsiString;
  WStr: UnicodeString;
begin
  if Win32PlatformIsUnicode then
    Result := Windows.CharLowerW(lpsz)
  else
  begin
    if HiWord(Cardinal(lpsz)) = 0 then
    begin
      // literal char mode
      Result := lpsz;
      if IsWideCharMappableToAnsi(WideChar(lpsz)) then
      begin
        AStr := AnsiString(WideChar(lpsz)); // single character may be more than one byte
        Windows.CharLowerA(PAnsiChar(AStr));
        WStr := UnicodeString(AStr); // should always be single wide char
        if Length(WStr) = 1 then
          Result := PWideChar(WStr[1]);
      end
    end
    else
    begin
      // null-terminated string mode
      Result := lpsz;
      while lpsz^ <> #0 do
      begin
        lpsz^ := WideChar(SynUnicode.WCharLower(PWideChar(lpsz^)));
        Inc(lpsz);
      end;
    end;
  end;
end;

function WCharLowerBuff(lpsz: PWideChar; cchLength: DWORD): DWORD;
var
  i: Integer;
begin
  if Win32PlatformIsUnicode then
    Result := Windows.CharLowerBuffW(lpsz, cchLength)
  else
  begin
    Result := cchLength;
    for i := 1 to cchLength do
    begin
      lpsz^ := WideChar(SynUnicode.WCharLower(PWideChar(lpsz^)));
      Inc(lpsz);
    end;
  end;
end;

function SynWideUpperCase(const S: UnicodeString): UnicodeString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then
    SynUnicode.WCharUpperBuff(Pointer(Result), Len);
end;

function SynWideLowerCase(const S: UnicodeString): UnicodeString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then
    SynUnicode.WCharLowerBuff(Pointer(Result), Len);
end;
{$ELSE}
function SynWideUpperCase(const S: UnicodeString): UnicodeString;
begin
  Result := WideUpperCase(S);
end;

function SynWideLowerCase(const S: UnicodeString): UnicodeString;
begin
  Result := WideLowerCase(S);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function SynIsCharAlpha(const C: WideChar): Boolean;
begin
  if Win32PlatformIsUnicode then
    Result := IsCharAlphaW(C)
  else
    // returns false if C is not mappable to ANSI
    Result := IsCharAlphaA(AnsiChar(C));
end;

function SynIsCharAlphaNumeric(const C: WideChar): Boolean;
begin
  if Win32PlatformIsUnicode then
    Result := IsCharAlphaNumericW(C)
  else
    // returns false if C is not mappable to ANSI
    Result := IsCharAlphaNumericA(AnsiChar(C));
end;
{$ELSE}
function SynIsCharAlpha(const C: WideChar): Boolean;
begin
  Result := IsAlpha(Integer(ch)) <> 0;
end;

function SynIsCharAlphaNumeric(const C: WideChar): Boolean;
begin
  Result := IsAlNum(Integer(ch)) <> 0;
end;
{$ENDIF}

{$IFNDEF UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (C < #$0100) and (AnsiChar(C) in CharSet);
end;
{$ENDIF}

function WideLastDelimiter(const Delimiters, S: UnicodeString): Integer;
var
  P: PWideChar;
begin
  Result := Length(S);
  P := PWideChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (WStrScan(P, S[Result]) <> nil) then
      Exit;
    Dec(Result);
  end;
end;

function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;
  Flags: TReplaceFlags): UnicodeString;
var
  SearchStr, Patt, NewStr: UnicodeString;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := SynWideUpperCase(S);
    Patt := SynWideUpperCase(OldPattern);
  end
  else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

const
  // data used to bring UTF-16 coded strings into correct UTF-32 order for correct comparation
  UTF16Fixup: array[0..31] of Word = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    $2000, $F800, $F800, $F800, $F800
  );

// Binary comparation of Str1 and Str2 with surrogate fix-up.
// Returns < 0 if Str1 is smaller in binary order than Str2, = 0 if both strings are
// equal and > 0 if Str1 is larger than Str2.
//
// This code is based on an idea of Markus W. Scherer (IBM).
// Note: The surrogate fix-up is necessary because some single value code points have
//       larger values than surrogates which are in UTF-32 actually larger.
function WStrComp(Str1, Str2: PWideChar): Integer;
var
  C1, C2: Word;
  Run1, Run2: PWideChar;
begin
  Run1 := Str1;
  Run2 := Str2;
  repeat
    C1 := Word(Run1^);
    C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
    C2 := Word(Run2^);
    C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

    // now C1 and C2 are in UTF-32-compatible order
    Result := Integer(C1) - Integer(C2);
    if(Result <> 0) or (C1 = 0) or (C2 = 0) then
      Break;
    Inc(Run1);
    Inc(Run2);
  until False;

  // If the strings have different lengths but the comparation returned equity so far
  // then adjust the result so that the longer string is marked as the larger one.
  if Result = 0 then
    Result := (Run1 - Str1) - (Run2 - Str2);
end;

// compares strings up to MaxLen code points
// see also StrCompW
function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
var
  C1, C2: Word;
begin
  if MaxLen > 0 then
  begin
    repeat
      C1 := Word(Str1^);
      C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
      C2 := Word(Str2^);
      C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

      // now C1 and C2 are in UTF-32-compatible order
      { TODO : surrogates take up 2 words and are counted twice here, count them only once }
      Result := Integer(C1) - Integer(C2);
      Dec(MaxLen);
      if(Result <> 0) or (C1 = 0) or (C2 = 0) or (MaxLen = 0) then
        Break;
      Inc(Str1);
      Inc(Str2);
    until False;
  end
  else
    Result := 0;
end;

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string
procedure StrSwapByteOrder(Str: PWideChar);
{$IFDEF SYN_COMPILER_16_UP}
var
  P: PWord;
begin
  P := PWord(Str);
  while P^ <> 0 do 
  begin
    P^ := MakeWord(HiByte(P^), LoByte(P^));
    Inc(P);
  end;
{$ELSE}
asm
       PUSH    ESI
       PUSH    EDI
       MOV     ESI, EAX
       MOV     EDI, ESI
       XOR     EAX, EAX // clear high order byte to be able to use 32bit operand below
@@1:
       LODSW
       OR      EAX, EAX
       JZ      @@2
       XCHG    AL, AH
       STOSW
       JMP     @@1


@@2:
       POP     EDI
       POP     ESI
{$ENDIF}
end;

// works like QuotedStr from SysUtils.pas but can insert any quotation character
function WideQuotedStr(const S: UnicodeString; Quote: WideChar): UnicodeString;
var
  P, Src,
  Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := WStrScan(PWideChar(S), Quote);
  while (P <> nil) do
  begin
    Inc(P);
    Inc(AddCount);
    P := WStrScan(P, Quote);
  end;

  if AddCount = 0 then
    Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := WStrScan(Src, Quote);
    repeat
      Inc(P);
      Move(Src^, Dest^, 2 * (P - Src));
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := WStrScan(Src, Quote);
    until P = nil;
    P := WStrEnd(Src);
    Move(Src^, Dest^, 2 * (P - Src));
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;
end;

// extracts a string enclosed in quote characters given by Quote
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): UnicodeString;
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then
    Exit;

  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := WStrScan(Src, Quote);

  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then
      Break;
    Inc(Src);
    Inc(DropCount);
    Src := WStrScan(Src, Quote);
  end;

  if Src = nil then
    Src := WStrEnd(P);
  if (Src - P) <= 1 then
    Exit;

  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := WStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then
        Break;
      Move(P^, Dest^, 2 * (Src - P));
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := WStrScan(Src, Quote);
    end;
    if Src = nil then
      Src := WStrEnd(P);
    Move(P^, Dest^, 2 * (Src - P - 1));
  end;
end;

// returns a string of Count characters filled with C
function UnicodeStringOfChar(C: WideChar; Count: Cardinal): UnicodeString;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := C;
end;

function WideTrim(const S: UnicodeString): UnicodeString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function WideTrimLeft(const S: UnicodeString): UnicodeString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function WideTrimRight(const S: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

{$IFDEF MSWINDOWS}
function TranslateCharsetInfoEx(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall;
  external 'gdi32.dll' name 'TranslateCharsetInfo';

function CharSetFromLocale(Language: LCID): TFontCharSet;
var
  CP: Cardinal;
  CSI: TCharsetInfo;
begin
  CP:= CodePageFromLocale(Language);
  TranslateCharsetInfoEx(Pointer(CP), CSI, TCI_SRCCODEPAGE);
  Result:= CSI.ciCharset;
end;

// determines the code page for a given locale
function CodePageFromLocale(Language: LCID): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

// converts the given character (as it comes with a WM_CHAR message) into its
// corresponding Unicode character depending on the active keyboard layout
function KeyUnicode(C: AnsiChar): WideChar;
begin
  MultiByteToWideChar(KeyboardCodePage, MB_USEGLYPHCHARS, @C, 1, @Result, 1);
end;

function StringToUnicodeStringEx(const S: AnsiString; CodePage: Word): UnicodeString;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(S);
  OutputLength := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength,
    nil, 0);
  SetLength(Result, OutputLength);
  MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, PWideChar(Result),
    OutputLength);
end;

function UnicodeStringToStringEx(const WS: UnicodeString; CodePage: Word): AnsiString;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(WS);
  OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength,
    nil, 0, nil, nil);
  SetLength(Result, OutputLength);
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, PAnsiChar(Result),
    OutputLength, nil, nil);
end;
{$ENDIF}

function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
{$IFDEF SYN_UNISCRIBE}
const
  SSAnalyseFlags = SSA_GLYPHS or SSA_FALLBACK or SSA_LINK;
{$ENDIF}
var
  tm: TTextMetricA;
  {$IFDEF SYN_UNISCRIBE}
  GlyphBufferSize: Integer;
  saa: TScriptStringAnalysis;
  lpSize: PSize;
  {$ENDIF}
begin
  Result.cx := 0;
  Result.cy := 0;

{$IFDEF SYN_UNISCRIBE}
  if Usp10IsInstalled then
  begin
    if Count <= 0 then Exit;

    // According to the MS Windows SDK (1.5 * Count + 16) is the recommended
    // value for GlyphBufferSize (see documentation of cGlyphs parameter of
    // ScriptStringAnalyse function)
    GlyphBufferSize := (3 * Count) div 2 + 16;
    
    if Succeeded(ScriptStringAnalyse(DC, Str, Count, GlyphBufferSize, -1,
      SSAnalyseFlags, 0, nil, nil, nil, nil, nil, @saa)) then
    begin
      lpSize := ScriptString_pSize(saa);
      if lpSize <> nil then
      begin
        Result := lpSize^;
        if Result.cx = 0 then
        begin
          GetTextMetricsA(DC, tm);
          Result.cx := tm.tmAveCharWidth;
        end;
      end;
      ScriptStringFree(@saa);
    end;
  end
  else
{$ENDIF}
  begin
    GetTextExtentPoint32W(DC, Str, Count, Result);
    if not Win32PlatformIsUnicode then
    begin
      GetTextMetricsA(DC, tm);
      if tm.tmPitchAndFamily and TMPF_TRUETYPE <> 0 then
        Result.cx := Result.cx - tm.tmOverhang
      else
        Result.cx := tm.tmAveCharWidth * Count;
    end;
  end;
end;

type
  TAccessCanvas = class(TCanvas)
  end;

function TextExtent(ACanvas: TCanvas; const Text: UnicodeString): TSize;
begin
  with TAccessCanvas(ACanvas) do
  begin
    RequiredState([csHandleValid, csFontValid]);
    Result := GetTextSize(Handle, PWideChar(Text), Length(Text));
  end;
end;

function TextWidth(ACanvas: TCanvas; const Text: UnicodeString): Integer;
begin
  Result := TextExtent(ACanvas, Text).cX;
end;

function TextHeight(ACanvas: TCanvas; const Text: UnicodeString): Integer;
begin
  Result := TextExtent(ACanvas, Text).cY;
end;

procedure TextOut(ACanvas: TCanvas; X, Y: Integer; const Text: UnicodeString);
begin
  with TAccessCanvas(ACanvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    if CanvasOrientation = coRightToLeft then
      Inc(X, SynUnicode.TextWidth(ACanvas, Text) + 1);
    Windows.ExtTextOutW(Handle, X, Y, TextFlags, nil, PWideChar(Text),
     Length(Text), nil);
    MoveTo(X + SynUnicode.TextWidth(ACanvas, Text), Y);
    Changed;
  end;
end;

procedure TextRect(ACanvas: TCanvas; Rect: TRect; X, Y: Integer;
  const Text: UnicodeString);
var
  Options: Longint;
begin
  with TAccessCanvas(ACanvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := ETO_CLIPPED or TextFlags;
    if Brush.Style <> bsClear then
      Options := Options or ETO_OPAQUE;
    if ((TextFlags and ETO_RTLREADING) <> 0) and
       (CanvasOrientation = coRightToLeft)
    then
      Inc(X, SynUnicode.TextWidth(ACanvas, Text) + 1);
    Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text),
      Length(Text), nil);
    Changed;
  end;
end;

{$IFNDEF UNICODE}
{ TWideFileStream }

constructor TWideFileStream.Create(const FileName: UnicodeString; Mode: Word);
begin
{$IFDEF MSWINDOWS}
  Create(Filename, Mode, 0);
{$ELSE}
  Create(Filename, Mode, FileAccessRights);
{$ENDIF}
end;

constructor TWideFileStream.Create(const FileName: UnicodeString; Mode: Word;
  Rights: Cardinal);
{$IFDEF USE_TNT_RUNTIME_SUPPORT}
var
  ErrorMessage: UnicodeString;
{$ENDIF}
begin
  if ((Mode and fmCreate) = fmCreate) then
  begin
    inherited Create(WideFileCreate(FileName, Rights));
    if Handle < 0 then
    begin
{$IFDEF USE_TNT_RUNTIME_SUPPORT}
  {$IFDEF SYN_COMPILER_7_UP}
      ErrorMessage := WideSysErrorMessage(GetLastError);
      raise EWideFCreateError.CreateResFmt(PResStringRec(@SFCreateErrorEx),
        [WideExpandFileName(FileName), ErrorMessage]);
  {$ELSE}
      raise EWideFCreateError.CreateResFmt(@SFCreateError, [FileName]);
  {$ENDIF}
{$ELSE}
  {$IFDEF SYN_COMPILER_7_UP}
      raise EFCreateError.CreateResFmt(PResStringRec(@SFCreateErrorEx),
        [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
  {$ELSE}
      raise EFCreateError.CreateResFmt(PResStringRec(@SFCreateError), [FileName]);
  {$ENDIF}
{$ENDIF}
    end
  end
  else
  begin
    inherited Create(WideFileOpen(FileName, Mode));
    if Handle < 0 then
    begin
{$IFDEF USE_TNT_RUNTIME_SUPPORT}
  {$IFDEF SYN_COMPILER_7_UP}
      ErrorMessage := WideSysErrorMessage(GetLastError);
      raise EWideFOpenError.CreateResFmt(PResStringRec(@SFOpenErrorEx),
        [WideExpandFileName(FileName), ErrorMessage]);
  {$ELSE}
      raise EWideFOpenError.CreateResFmt(@SFOpenError, [FileName]);
  {$ENDIF}
{$ELSE}
  {$IFDEF SYN_COMPILER_7_UP}
      raise EFOpenError.CreateResFmt(PResStringRec(@SFOpenErrorEx),
        [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
  {$ELSE}
      raise EFOpenError.CreateResFmt(PResStringRec(@SFOpenError), [FileName]);
  {$ENDIF}
{$ENDIF}
    end;
  end;
end;

destructor TWideFileStream.Destroy;
begin
  if Handle >= 0 then FileClose(Handle);
  inherited Destroy;
end;

function WideFileOpen(const FileName: UnicodeString; Mode: LongWord): Integer;
{$IFDEF MSWINDOWS}
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := -1;
  if ((Mode and 3) <= fmOpenReadWrite) and
    ((Mode and $F0) <= fmShareDenyNone) then
  begin
    if Win32PlatformIsUnicode then
      Result := Integer(CreateFileW(PWideChar(FileName), AccessMode[Mode and 3],
        ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, 0))
    else
      Result := Integer(CreateFileA(PAnsiChar(AnsiString(FileName)), AccessMode[Mode and 3],
        ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, 0));
  end;
end;
{$ENDIF}
{$IFDEF SYN_LINUX}
const
  ShareMode: array[0..fmShareDenyNone shr 4] of Byte = (
    0,        //No share mode specified
    F_WRLCK,  //fmShareExclusive
    F_RDLCK,  //fmShareDenyWrite
    0);       //fmShareDenyNone
var
  FileHandle, Tvar: Integer;
  LockVar: TFlock;
  smode: Byte;
begin
  Result := -1;
  if FileExists(FileName) and
     ((Mode and 3) <= fmOpenReadWrite) and
     ((Mode and $F0) <= fmShareDenyNone) then
  begin
    FileHandle := open(PChar(AnsiString(FileName)), (Mode and 3), FileAccessRights);

    if FileHandle = -1 then  Exit;

    smode := Mode and $F0 shr 4;
    if ShareMode[smode] <> 0 then
    begin
      with LockVar do
      begin
        l_whence := SEEK_SET;
        l_start := 0;
        l_len := 0;
        l_type := ShareMode[smode];
      end;
      Tvar :=  fcntl(FileHandle, F_SETLK, LockVar);
      if Tvar = -1 then
      begin
        __close(FileHandle);
        Exit;
      end;
    end;
    Result := FileHandle;
  end;
end;
{$ENDIF}

function WideFileCreate(const FileName: UnicodeString): Integer;
{$IFDEF MSWINDOWS}
begin
  if Win32PlatformIsUnicode then
    Result := Integer(CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0))
  else
    Result := Integer(CreateFileA(PAnsiChar(AnsiString(FileName)), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
end;
{$ENDIF}
{$IFDEF SYN_LINUX}
begin
  Result := FileCreate(FileName, FileAccessRights);
end;
{$ENDIF}

function WideFileCreate(const FileName: UnicodeString; Rights: Integer): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := WideFileCreate(FileName);
end;
{$ENDIF}
{$IFDEF SYN_LINUX}
begin
  Result := Integer(open(PChar(AnsiString(FileName)), O_RDWR or O_CREAT or O_TRUNC, Rights));
end;
{$ENDIF}
{$ENDIF}

function IsAnsiOnly(const WS: UnicodeString): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := IsUnicodeStringMappableToAnsi(WS);
end;
{$ELSE}
var
  Run: PWideChar;
begin
  Run := PWideChar(WS);
  while Run^ in [WideChar(#1)..WideChar(#255)] do
    Inc(Run);
  Result := Run^ = WideNull;
end;
{$ENDIF}

function IsUTF8(const FileName: UnicodeString; out WithBOM: Boolean): Boolean;
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsUTF8(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

// checks for a BOM in UTF-8 format or searches the first 4096 bytes for
// typical UTF-8 octet sequences
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean;
const
  MinimumCountOfUTF8Strings = 1;
  MaxBufferSize = $4000;
var
  Buffer: array of Byte;
  BufferSize, i, FoundUTF8Strings: Integer;

  // 3 trailing bytes are the maximum in valid UTF-8 streams,
  // so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    Inc(i);
    while (i < BufferSize) and (Result < 4) do
    begin
      if Buffer[i] in [$80..$BF] then
        Inc(Result)
      else
        Break;
      Inc(i);
    end;
  end;

begin
  // if Stream is nil, let Delphi raise the exception, by accessing Stream,
  // to signal an invalid result

  // start analysis at actual Stream.Position
  BufferSize := Min(MaxBufferSize, Stream.Size - Stream.Position);

  // if no special characteristics are found it is not UTF-8
  Result := False;
  WithBOM := False;

  if BufferSize > 0 then
  begin
    SetLength(Buffer, BufferSize);
    Stream.ReadBuffer(Buffer[0], BufferSize);
    Stream.Seek(-BufferSize, soFromCurrent);

    { first search for BOM }

    if (BufferSize >= Length(UTF8BOM)) and CompareMem(@Buffer[0], @UTF8BOM[0], Length(UTF8BOM)) then
    begin
      WithBOM := True;
      Result := True;
      Exit;
    end;

    { If no BOM was found, check for leading/trailing byte sequences,
      which are uncommon in usual non UTF-8 encoded text.

      NOTE: There is no 100% save way to detect UTF-8 streams. The bigger
            MinimumCountOfUTF8Strings, the lower is the probability of
            a false positive. On the other hand, a big MinimumCountOfUTF8Strings
            makes it unlikely to detect files with only little usage of non
            US-ASCII chars, like usual in European languages. }

    FoundUTF8Strings := 0;
    i := 0;
    while i < BufferSize do
    begin
      case Buffer[i] of
        $00..$7F: // skip US-ASCII characters as they could belong to various charsets
          ;
        $C2..$DF:
          if CountOfTrailingBytes = 1 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $E0:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$A0..$BF]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $E1..$EC, $EE..$EF:
          if CountOfTrailingBytes = 2 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $ED:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$80..$9F]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F0:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$90..$BF]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F1..$F3:
          if CountOfTrailingBytes = 3 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $F4:
          begin
            Inc(i);
            if (i < BufferSize) and (Buffer[i] in [$80..$8F]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $C0, $C1, $F5..$FF: // invalid UTF-8 bytes
          Break;
        $80..$BF: // trailing bytes are consumed when handling leading bytes,
                   // any occurence of "orphaned" trailing bytes is invalid UTF-8
          Break;
      end;

      if FoundUTF8Strings = MinimumCountOfUTF8Strings then
      begin
        Result := True;
        Break;
      end;

      Inc(i);
    end;
  end;
end;

function GetEncoding(const FileName: UnicodeString; out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := GetEncoding(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

function GetEncoding(Stream: TStream; out WithBOM: Boolean): TSynEncoding;
var
  BOM: WideChar;
  Size: Integer;
begin
  // if Stream is nil, let Delphi raise the exception, by accessing Stream,
  // to signal an invalid result
  
  // start analysis at actual Stream.Position
  Size := Stream.Size - Stream.Position;

  // if no special characteristics are found it is probably ANSI
  Result := seAnsi;

  if IsUTF8(Stream, WithBOM) then
  begin
    Result := seUTF8;
    Exit;
  end;

  { try to detect UTF-16 by finding a BOM in UTF-16 format }

  if Size >= 2 then
  begin
    Stream.ReadBuffer(BOM, sizeof(BOM));
    Stream.Seek(-sizeof(BOM), soFromCurrent);
    if BOM = WideChar(UTF16BOMLE) then
    begin
      Result := seUTF16LE;
      WithBOM := True;
      Exit;
    end
    else if BOM = WideChar(UTF16BOMBE) then
    begin
      Result := seUTF16BE;
      WithBOM := True;
      Exit;
    end
  end;
end;

procedure SaveToFile(const WS: UnicodeString; const FileName: UnicodeString;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(WS, Stream, Encoding, WithBom);
  finally
    Stream.Free;
  end;
end;

procedure SaveToFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(UnicodeStrings, Stream, Encoding, WithBom);
  finally
    Stream.Free;
  end;
end;

function LoadFromFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(UnicodeStrings, Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

function LoadFromFile(UnicodeStrings: TUnicodeStrings; const FileName: UnicodeString;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(UnicodeStrings, Stream, Encoding, WithBOM);
  finally
    Stream.Free;
  end;
end;

procedure SaveToStream(const WS: UnicodeString; Stream: TStream; Encoding: TSynEncoding;
  WithBom: Boolean  = True);
var
  UTF16BOM: UnicodeString;

  UTF8Str: UTF8String;
  AnsiStr: AnsiString;
begin
  if WithBom then
    case Encoding of
      seUTF8:
        Stream.WriteBuffer(UTF8BOM, 3);
      seUTF16LE:
        begin
          UTF16BOM := BOM_LSB_FIRST;
          Stream.WriteBuffer(PWideChar(UTF16BOM)^, 2);
        end;
      seUTF16BE:
        begin
          UTF16BOM := BOM_MSB_FIRST;
          Stream.WriteBuffer(PWideChar(UTF16BOM)^, 2);
        end;
    end;

  case Encoding of
    seUTF8:
      begin
        UTF8Str := UTF8Encode(WS);
        Stream.WriteBuffer(UTF8Str[1], Length(UTF8Str));
      end;
    seUTF16LE:
      Stream.WriteBuffer(WS[1], Length(WS) * sizeof(WideChar));
    seUTF16BE:
      begin
        StrSwapByteOrder(PWideChar(WS));
        Stream.WriteBuffer(WS[1], Length(WS) * sizeof(WideChar));
      end;
    seAnsi:
      begin
        AnsiStr := AnsiString(PWideChar(WS));
        Stream.WriteBuffer(AnsiStr[1], Length(AnsiStr));
      end;
  end;
end;

type
  TSynEditStringListAccess = class(TSynEditStringList);

procedure SaveToStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  SText: UnicodeString;
  SaveFStreaming: Boolean;
begin
  // if UnicodeStrings or Stream is nil, let Delphi raise the exception to flag the error

  if UnicodeStrings is TSynEditStringList then
  begin
    SaveFStreaming := TSynEditStringListAccess(UnicodeStrings).FStreaming;
    TSynEditStringListAccess(UnicodeStrings).FStreaming := True;
    SText := UnicodeStrings.Text;
    TSynEditStringListAccess(UnicodeStrings).FStreaming := SaveFStreaming;
  end
  else
    SText := UnicodeStrings.Text;
  SaveToStream(SText, Stream, Encoding, WithBom);
end;

function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  out WithBOM: Boolean): TSynEncoding;
var
  Dummy: Boolean;
begin
  Result := LoadFromStream(UnicodeStrings, Stream, GetEncoding(Stream, WithBOM),
    Dummy);
end;

function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding): TSynEncoding; overload;
var
  Dummy: Boolean;
begin
  Result := LoadFromStream(UnicodeStrings, Stream, Encoding, Dummy);
end;

function LoadFromStream(UnicodeStrings: TUnicodeStrings; Stream: TStream;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding;
var
  WideStr: UnicodeString;
  UTF8Str: UTF8String;
  AnsiStr: AnsiString;
  Size: Integer;

  function SkipBOM: Boolean;
  var
    BOM: array of Byte;
  begin
    Result := False;
    case Encoding of
      seUTF8:
        begin
          SetLength(BOM, Min(Length(UTF8BOM), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF8BOM)) or
            not CompareMem(@BOM[0], @UTF8BOM[0], Length(UTF8BOM))
          then
            Stream.Seek(-Length(BOM), {$IFDEF SYN_DELPHI_XE4_UP}soCurrent{$ELSE}soFromCurrent{$ENDIF})
          else
            Result := True;
        end;
      seUTF16LE:
        begin
          SetLength(BOM, Min(Length(UTF16BOMLE), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF16BOMLE)) or
            not CompareMem(@BOM[0], @UTF16BOMLE[0], Length(UTF16BOMLE))
          then
            Stream.Seek(-Length(BOM), {$IFDEF SYN_DELPHI_XE4_UP}soCurrent{$ELSE}soFromCurrent{$ENDIF})
          else
            Result := True;
        end;
      seUTF16BE:
        begin
          SetLength(BOM, Min(Length(UTF16BOMBE), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF16BOMBE)) or
            not CompareMem(@BOM[0], @UTF16BOMBE[0], Length(UTF16BOMBE))
          then
            Stream.Seek(-Length(BOM), {$IFDEF SYN_DELPHI_XE4_UP}soCurrent{$ELSE}soFromCurrent{$ENDIF})
          else
            Result := True;
        end;
    end;
    Size := Stream.Size - Stream.Position;
  end;

begin
  // if UnicodeStrings or Stream is nil, let Delphi raise the exception to
  // signal an invalid result
  UnicodeStrings.BeginUpdate;
  try
    Result := Encoding;
    // start decoding at actual Stream.Position
    Size := Stream.Size - Stream.Position;

    // skip BOM, if it exists
    WithBOM := SkipBOM;

    case Result of
      seUTF8:
        begin
          SetLength(UTF8Str, Size);
          Stream.ReadBuffer(UTF8Str[1], Size);
{$IFDEF UNICODE}
          UnicodeStrings.Text := UTF8ToUnicodeString(UTF8Str);
{$ELSE}
          UnicodeStrings.Text := UTF8Decode(UTF8Str);
          UnicodeStrings.SaveFormat := sfUTF8;
{$ENDIF}
        end;
      seUTF16LE:
        begin
          SetLength(WideStr, Size div 2);
          Stream.ReadBuffer(WideStr[1], Size);
          UnicodeStrings.Text := WideStr;
{$IFNDEF UNICODE}
          UnicodeStrings.SaveFormat := sfUTF16LSB;
{$ENDIF}
        end;
      seUTF16BE:
        begin
          SetLength(WideStr, Size div 2);
          Stream.ReadBuffer(WideStr[1], Size);
          StrSwapByteOrder(PWideChar(WideStr));
          UnicodeStrings.Text := WideStr;
{$IFNDEF UNICODE}
          UnicodeStrings.SaveFormat := sfUTF16MSB;
{$ENDIF}
        end;
      seAnsi:
        begin
          SetLength(AnsiStr, Size);
          Stream.ReadBuffer(AnsiStr[1], Size);
          UnicodeStrings.Text := UnicodeString(AnsiStr);
{$IFNDEF UNICODE}
          UnicodeStrings.SaveFormat := sfAnsi;
{$ENDIF}
        end;
    end;
  finally
    UnicodeStrings.EndUpdate
  end
end;

function ClipboardProvidesText: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_TEXT) or IsClipboardFormatAvailable(CF_UNICODETEXT);
end;

function GetClipboardText: UnicodeString;
var
  Mem: HGLOBAL;
  LocaleID: LCID;
  P: PByte;
begin
  Result := '';
  Clipboard.Open;
  try
    if Clipboard.HasFormat(CF_UNICODETEXT) then
    begin
      Mem := Clipboard.GetAsHandle(CF_UNICODETEXT);
        try
          if Mem <> 0 then
            Result := PWideChar(GlobalLock(Mem));
        finally
          if Mem <> 0 then GlobalUnlock(Mem);
        end;
    end
    else
    begin
      LocaleID := 0;
      Mem := Clipboard.GetAsHandle(CF_LOCALE);
      try
        if Mem <> 0 then LocaleID := PInteger(GlobalLock(Mem))^;
      finally
        if Mem <> 0 then GlobalUnlock(Mem);
      end;

      Mem := Clipboard.GetAsHandle(CF_TEXT);
      try
        if Mem <> 0 then
        begin
          P := GlobalLock(Mem);
          Result := StringToUnicodeStringEx(PAnsiChar(P), CodePageFromLocale(LocaleID));
        end
      finally
        if Mem <> 0 then GlobalUnlock(Mem);
      end;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure SetClipboardText(const Text: UnicodeString);
var
  Mem: HGLOBAL;
  P: PByte;
  SLen: Integer;
begin
  if Text = '' then Exit;
  SLen := Length(Text);
  Clipboard.Open;
  try
    Clipboard.Clear;

    // set ANSI text only on Win9X, WinNT automatically creates ANSI from Unicode
    if Win32Platform <> VER_PLATFORM_WIN32_NT then
    begin
      Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen + 1);
      if Mem <> 0 then
      begin
        P := GlobalLock(Mem);
        try
          if P <> nil then
          begin
            Move(PAnsiChar(AnsiString(Text))^, P^, SLen + 1);
            Clipboard.SetAsHandle(CF_TEXT, Mem);
          end;
        finally
          GlobalUnlock(Mem);
        end;
      end;
    end;

    // set unicode text, this also works on Win9X, even if the clipboard-viewer
    // can't show it, Word 2000+ can paste it including the unicode only characters
    Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,
      (SLen + 1) * sizeof(WideChar));
    if Mem <> 0 then
    begin
      P := GlobalLock(Mem);
      try
        if P <> nil then
        begin
          Move(PWideChar(Text)^, P^, (SLen + 1) * sizeof(WideChar));
          Clipboard.SetAsHandle(CF_UNICODETEXT, Mem);
        end;
      finally
        GlobalUnlock(Mem);
      end;
    end;
    // Don't free Mem!  It belongs to the clipboard now, and it will free it
    // when it is done with it.
  finally
    Clipboard.Close;
  end;
end;

{$IFNDEF UNICODE}
{$IFNDEF SYN_COMPILER_6_UP}
procedure AssignWideStr(var Dest: UnicodeString; const Source: UnicodeString);
begin
  Dest := Source;
end;

procedure IntGetWideStrProp(Instance: TObject; PropInfo: PPropInfo;
  var Value: UnicodeString); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to result string    }

        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EDX

        MOV     EDX,[EDI].TPropInfo.Index       { pass index in EDX }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX }
@@hasIndex:
        MOV     ESI,[EDI].TPropInfo.GetProc
        CMP     [EDI].TPropInfo.GetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

@@isVirtualMethod:
        MOVSX   ESI,SI                          { sign extend slot offset }
        ADD     ESI,[EAX]                       { vmt + slot offset }
        CALL    DWORD PTR [ESI]
        JMP     @@exit

@@isStaticMethod:
        CALL    ESI
        JMP     @@exit

@@isField:
  AND  ESI,$00FFFFFF
  MOV  EDX,[EAX+ESI]
  MOV  EAX,ECX
  CALL  AssignWideStr

@@exit:
        POP     EDI
        POP     ESI
end;

function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo): UnicodeString;
begin
  IntGetWideStrProp(Instance, PropInfo, Result);
end;

procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: UnicodeString); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to string value     }

        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX

        MOV     EDX,[ESI].TPropInfo.Index       { pass index in EDX }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX }
@@hasIndex:
        MOV     EDI,[ESI].TPropInfo.SetProc
        CMP     [ESI].TPropInfo.SetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

@@isVirtualMethod:
        MOVSX   EDI,DI
        ADD     EDI,[EAX]
        CALL    DWORD PTR [EDI]
        JMP     @@exit

@@isStaticMethod:
        CALL    EDI
        JMP     @@exit

@@isField:
  AND  EDI,$00FFFFFF
  ADD  EAX,EDI
  MOV  EDX,ECX
  CALL  AssignWideStr

@@exit:
        POP     EDI
        POP     ESI
end;
{$ENDIF}

type
  TUnicodeStringPropertyFiler = class
  private
    FInstance: TPersistent;
    FPropInfo: PPropInfo;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
  public
    procedure DefineProperties(Filer: TFiler; Instance: TPersistent; PropName: AnsiString);
  end;

  TWideCharPropertyFiler = class
  private
    FInstance: TPersistent;
    FPropInfo: PPropInfo;
    FWriter: TWriter;
    procedure GetLookupInfo(var Ancestor: TPersistent;
      var Root, LookupRoot, RootAncestor: TComponent);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    function ReadChar(Reader: TReader): WideChar;
  public
    procedure DefineProperties(Filer: TFiler; Instance: TPersistent; PropName: AnsiString);
  end;

type
  TGetLookupInfoEvent = procedure(var Ancestor: TPersistent;
    var Root, LookupRoot, RootAncestor: TComponent) of object;

function AncestorIsValid(Ancestor: TPersistent; Root, RootAncestor: TComponent): Boolean;
begin
  Result := (Ancestor <> nil) and (RootAncestor <> nil) and
            Root.InheritsFrom(RootAncestor.ClassType);
end;

function IsDefaultOrdPropertyValue(Instance: TObject; PropInfo: PPropInfo;
  OnGetLookupInfo: TGetLookupInfoEvent): Boolean;
var
  Ancestor: TPersistent;
  LookupRoot: TComponent;
  RootAncestor: TComponent;
  Root: TComponent;
  AncestorValid: Boolean;
  Value: Longint;
  Default: LongInt;
begin
  Ancestor := nil;
  Root := nil;
  LookupRoot := nil;
  RootAncestor := nil;

  if Assigned(OnGetLookupInfo) then
    OnGetLookupInfo(Ancestor, Root, LookupRoot, RootAncestor);

  AncestorValid := AncestorIsValid(Ancestor, Root, RootAncestor);

  Result := True;
  if (PropInfo^.GetProc <> nil) and (PropInfo^.SetProc <> nil) then
  begin
    Value := GetOrdProp(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetOrdProp(Ancestor, PropInfo)
    else
    begin
      Default := PPropInfo(PropInfo)^.Default;
      Result :=  (Default <> LongInt($80000000)) and (Value = Default);
    end;
  end;
end;

procedure ReadError(S: string);
begin
  raise EReadError.Create(S);
end;

procedure PropValueError;
begin
  ReadError(SInvalidPropertyValue);
end;

{ TUnicodeStringPropertyFiler }

procedure TUnicodeStringPropertyFiler.DefineProperties(Filer: TFiler; Instance: TPersistent;
  PropName: AnsiString);

  function HasData: Boolean;
  var
    CurrPropValue: UnicodeString;
  begin
    // must be stored
    Result := IsStoredProp(Instance, FPropInfo);
    if Result
    and (Filer.Ancestor <> nil)
    and (GetPropInfo(Filer.Ancestor, PropName, [tkWString]) <> nil) then
    begin
      // must be different than ancestor
      CurrPropValue := GetWideStrProp(Instance, FPropInfo);
      Result := CurrPropValue <> GetWideStrProp(Filer.Ancestor, GetPropInfo(Filer.Ancestor, PropName));
    end;
    if Result then
      Result := GetWideStrProp(Instance, FPropInfo) <> '';
  end;

begin
  FInstance := Instance;
  FPropInfo := GetPropInfo(Instance, PropName, [tkWString]);
  if FPropInfo <> nil then
    // must be published (and of type UnicodeString)
    Filer.DefineProperty(PropName + 'W', ReadData, WriteData, HasData);
  FInstance := nil;
  FPropInfo := nil;
end;

procedure TUnicodeStringPropertyFiler.ReadData(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
      SetWideStrProp(FInstance, FPropInfo, Reader.ReadString);
  else
    SetWideStrProp(FInstance, FPropInfo, Reader.ReadWideString);
  end;
end;

procedure TUnicodeStringPropertyFiler.WriteData(Writer: TWriter);
begin
  Writer.WriteWideString(GetWideStrProp(FInstance, FPropInfo));
end;

{ TWideCharPropertyFiler }

procedure TWideCharPropertyFiler.GetLookupInfo(var Ancestor: TPersistent;
  var Root, LookupRoot, RootAncestor: TComponent);
begin
  Ancestor := FWriter.Ancestor;
  Root := FWriter.Root;
  LookupRoot := FWriter.LookupRoot;
  RootAncestor := FWriter.RootAncestor;
end;

function TWideCharPropertyFiler.ReadChar(Reader: TReader): WideChar;
var
  Temp: UnicodeString;
begin
  case Reader.NextValue of
    vaWString:
      Temp := Reader.ReadWideString;
    vaString:
      Temp := Reader.ReadString;
    else
      PropValueError;
  end;

  if Length(Temp) > 1 then
    PropValueError;
  Result := Temp[1];
end;

procedure TWideCharPropertyFiler.ReadData(Reader: TReader);
begin
  SetOrdProp(FInstance, FPropInfo, Ord(ReadChar(Reader)));
end;

type
  TAccessWriter = class(TWriter)
  end;

procedure TWideCharPropertyFiler.WriteData(Writer: TWriter);
var
  L: Integer;
  Temp: UnicodeString;
begin
  Temp := WideChar(GetOrdProp(FInstance, FPropInfo));

  TAccessWriter(Writer).WriteValue(vaWString);
  L := Length(Temp);
  Writer.Write(L, SizeOf(Integer));
  Writer.Write(Pointer(@Temp[1])^, L * 2);
end;

procedure TWideCharPropertyFiler.DefineProperties(Filer: TFiler;
  Instance: TPersistent; PropName: AnsiString);

  function HasData: Boolean;
  var
    CurrPropValue: Integer;
  begin
    // must be stored
    Result := IsStoredProp(Instance, FPropInfo);
    if Result and (Filer.Ancestor <> nil) and
      (GetPropInfo(Filer.Ancestor, PropName, [tkWChar]) <> nil) then
    begin
      // must be different than ancestor
      CurrPropValue := GetOrdProp(Instance, FPropInfo);
      Result := CurrPropValue <> GetOrdProp(Filer.Ancestor, GetPropInfo(Filer.Ancestor, PropName));
    end;

    if Result and (Filer is TWriter) then
    begin
      FWriter := TWriter(Filer);
      Result := not IsDefaultOrdPropertyValue(Instance, FPropInfo, GetLookupInfo);
    end;
  end;

begin
  FInstance := Instance;
  FPropInfo := GetPropInfo(Instance, PropName, [tkWChar]);
  if FPropInfo <> nil then // must be published (and of type WideChar)
  begin
    // W suffix causes Delphi's native streaming system to ignore the property
    // and let us do the reading.
    Filer.DefineProperty(PropName + 'W', ReadData, WriteData, HasData);
  end;
  FInstance := nil;
  FPropInfo := nil;
end;

procedure UnicodeDefineProperties(Filer: TFiler; Instance: TPersistent);
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  UnicodeStringFiler: TUnicodeStringPropertyFiler;
  WideCharFiler: TWideCharPropertyFiler;
begin
  Count := GetTypeData(Instance.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    UnicodeStringFiler := TUnicodeStringPropertyFiler.Create;
    try
      WideCharFiler := TWideCharPropertyFiler.Create;
      try
        GetMem(PropList, Count * SizeOf(Pointer));
        try
          GetPropInfos(Instance.ClassInfo, PropList);
          for I := 0 to Count - 1 do
          begin
            PropInfo := PropList^[I];
            if (PropInfo = nil) then
              Break;
            if (PropInfo.PropType^.Kind = tkWString) then
              UnicodeStringFiler.DefineProperties(Filer, Instance, PropInfo.Name)
            else if (PropInfo.PropType^.Kind = tkWChar) then
              WideCharFiler.DefineProperties(Filer, Instance, PropInfo.Name)
          end;
        finally
          FreeMem(PropList, Count * SizeOf(Pointer));
        end;
      finally
        WideCharFiler.Free;
      end;
    finally
      UnicodeStringFiler.Free;
    end;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(@WC), 1, nil, 0, nil,
    @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

function IsUnicodeStringMappableToAnsi(const WS: UnicodeString): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(WS), Length(WS), nil, 0,
    nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;
{$ENDIF}

initialization
{$IFDEF MSWINDOWS}
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);
  {$IFNDEF UNICODE}
  DefaultSystemCodePage := GetACP;
  {$ENDIF}
{$ENDIF}

end.
