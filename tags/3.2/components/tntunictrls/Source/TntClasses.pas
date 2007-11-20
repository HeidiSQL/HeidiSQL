
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntClasses;

{$INCLUDE TntCompilers.inc}

interface

{ TODO: Consider: TTntRegIniFile, TTntMemIniFile (consider if UTF8 fits into this solution). }

{***********************************************}
{  WideChar-streaming implemented by Maël Hörz  }
{***********************************************}

uses
  Classes, SysUtils, Windows, TntSysUtils,
  {$IFNDEF COMPILER_10_UP}
  TntWideStrings,
  {$ELSE}
  WideStrings,
  {$ENDIF}
  ActiveX, Contnrs;
  
{ Exception classes }

type
  EWideFileStreamError = class(WideException)
    constructor Create(ResStringRec: PResStringRec; const FileName: WideString);
  end;
  EWideFCreateError = class(EWideFileStreamError);
  EWideFOpenError = class(EWideFileStreamError);

// ......... introduced .........
type
  TTntStreamCharSet = (csAnsi, csUnicode, csUnicodeSwapped, csUtf8);

function AutoDetectCharacterSet(Stream: TStream): TTntStreamCharSet;

//---------------------------------------------------------------------------------------------
//                                 Tnt - Classes
//---------------------------------------------------------------------------------------------

{TNT-WARN ExtractStrings}
{TNT-WARN LineStart}
{TNT-WARN TStringStream}   // TODO: Implement a TWideStringStream

// A potential implementation of TWideStringStream can be found at:
//   http://kdsxml.cvs.sourceforge.net/kdsxml/Global/KDSClasses.pas?revision=1.10&view=markup

procedure TntPersistent_AfterInherited_DefineProperties(Filer: TFiler; Instance: TPersistent);

type
{TNT-WARN TFileStream}
  TTntFileStream = class(THandleStream)
  public
    constructor Create(const FileName: WideString; Mode: Word);
    destructor Destroy; override;
  end;

{TNT-WARN TMemoryStream}
  TTntMemoryStream = class(TMemoryStream{TNT-ALLOW TMemoryStream})
  public
    procedure LoadFromFile(const FileName: WideString);
    procedure SaveToFile(const FileName: WideString);
  end;

{TNT-WARN TResourceStream}
  TTntResourceStream = class(TCustomMemoryStream)
  private
    HResInfo: HRSRC;
    HGlobal: THandle;
    procedure Initialize(Instance: THandle; Name, ResType: PWideChar);
  public
    constructor Create(Instance: THandle; const ResName: WideString; ResType: PWideChar);
    constructor CreateFromID(Instance: THandle; ResID: Word; ResType: PWideChar);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SaveToFile(const FileName: WideString);
  end;

  TTntStrings = class;

{TNT-WARN TAnsiStrings}
  TAnsiStrings{TNT-ALLOW TAnsiStrings} = class(TStrings{TNT-ALLOW TStrings})
  public
    procedure LoadFromFile(const FileName: WideString); reintroduce;
    procedure SaveToFile(const FileName: WideString); reintroduce;
    procedure LoadFromFileEx(const FileName: WideString; CodePage: Cardinal);
    procedure SaveToFileEx(const FileName: WideString; CodePage: Cardinal);
    procedure LoadFromStreamEx(Stream: TStream; CodePage: Cardinal); virtual; abstract;
    procedure SaveToStreamEx(Stream: TStream; CodePage: Cardinal); virtual; abstract;
  end;

  TAnsiStringsForWideStringsAdapter = class(TAnsiStrings{TNT-ALLOW TAnsiStrings})
  private
    FWideStrings: TTntStrings;
    FAdapterCodePage: Cardinal;
  protected
    function Get(Index: Integer): AnsiString; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function AdapterCodePage: Cardinal; dynamic;
  public
    constructor Create(AWideStrings: TTntStrings; _AdapterCodePage: Cardinal = 0);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure LoadFromStreamEx(Stream: TStream; CodePage: Cardinal); override;
    procedure SaveToStreamEx(Stream: TStream; CodePage: Cardinal); override;
  end;

{TNT-WARN TStrings}
  TTntStrings = class(TWideStrings)
  private
    FLastFileCharSet: TTntStreamCharSet;
    FAnsiStrings: TAnsiStrings{TNT-ALLOW TAnsiStrings};
    procedure SetAnsiStrings(const Value: TAnsiStrings{TNT-ALLOW TAnsiStrings});
    procedure ReadData(Reader: TReader);
    procedure ReadDataUTF7(Reader: TReader);
    procedure ReadDataUTF8(Reader: TReader);
    procedure WriteDataUTF7(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: WideString); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromStream_BOM(Stream: TStream; WithBOM: Boolean); virtual;

    procedure SaveToFile(const FileName: WideString); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToStream_BOM(Stream: TStream; WithBOM: Boolean); virtual;

    property LastFileCharSet: TTntStreamCharSet read FLastFileCharSet;
  published
    property AnsiStrings: TAnsiStrings{TNT-ALLOW TAnsiStrings} read FAnsiStrings write SetAnsiStrings stored False;
  end;

{ TTntStringList class }

  TTntStringList = class;
  TWideStringListSortCompare = function(List: TTntStringList; Index1, Index2: Integer): Integer;

{TNT-WARN TStringList}
  TTntStringList = class(TTntStrings)
  private
    FUpdating: Boolean;
    FList: PWideStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TWideStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): WideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: WideString): Integer; override;
    procedure InsertItem(Index: Integer; const S: WideString; AObject: TObject); virtual;
  public
    destructor Destroy; override;
    function Add(const S: WideString): Integer; override;
    function AddObject(const S: WideString; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: WideString): Integer; override;
    function IndexOfName(const Name: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure InsertObject(Index: Integer; const S: WideString;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TWideStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

// ......... introduced .........
type
  TListTargetCompare = function (Item, Target: Pointer): Integer;

function FindSortedListByTarget(List: TList; TargetCompare: TListTargetCompare;
  Target: Pointer; var Index: Integer): Boolean;

function ClassIsRegistered(const clsid: TCLSID): Boolean;

var
  RuntimeUTFStreaming: Boolean;

type
  TBufferedAnsiString = class(TObject)
  private
    FStringBuffer: AnsiString;
    LastWriteIndex: Integer;
  public
    procedure Clear;
    procedure AddChar(const wc: AnsiChar);
    procedure AddString(const s: AnsiString);
    procedure AddBuffer(Buff: PAnsiChar; Chars: Integer);
    function Value: AnsiString;
    function BuffPtr: PAnsiChar;
  end;

  TBufferedWideString = class(TObject)
  private
    FStringBuffer: WideString;
    LastWriteIndex: Integer;
  public
    procedure Clear;
    procedure AddChar(const wc: WideChar);
    procedure AddString(const s: WideString);
    procedure AddBuffer(Buff: PWideChar; Chars: Integer);
    function Value: WideString;
    function BuffPtr: PWideChar;
  end;

  TBufferedStreamReader = class(TStream)
  private
    FStream: TStream;
    FStreamSize: Integer;
    FBuffer: array of Byte;
    FBufferSize: Integer;
    FBufferStartPosition: Integer;
    FVirtualPosition: Integer;
    procedure UpdateBufferFromPosition(StartPos: Integer);
  public
    constructor Create(Stream: TStream; BufferSize: Integer = 1024);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

// "synced" wide string
type TSetAnsiStrEvent = procedure(const Value: AnsiString) of object;
function GetSyncedWideString(var WideStr: WideString; const AnsiStr: AnsiString): WideString;
procedure SetSyncedWideString(const Value: WideString; var WideStr: WideString;
  const AnsiStr: AnsiString; SetAnsiStr: TSetAnsiStrEvent);

type
  TWideComponentHelper = class(TComponent)
  private
    FComponent: TComponent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateHelper(AOwner: TComponent; ComponentHelperList: TComponentList);
  end;

function FindWideComponentHelper(ComponentHelperList: TComponentList; Component: TComponent): TWideComponentHelper;

implementation

uses
  RTLConsts, ComObj, Math,
  Registry, TypInfo, TntSystem;

{ EWideFileStreamError }

constructor EWideFileStreamError.Create(ResStringRec: PResStringRec;
  const FileName: WideString);
begin
  inherited CreateResFmt(ResStringRec, [WideExpandFileName(FileName),
    WideSysErrorMessage(GetLastError)]);
end;

{ TntPersistent }

//===========================================================================
//   The Delphi 5 Classes.pas never supported the streaming of WideStrings.
//   The Delphi 6 Classes.pas supports WideString streaming.  But it's too bad that
//     the Delphi 6 IDE doesn't use the updated Classes.pas.  Switching between Form/Text
//       mode corrupts extended characters in WideStrings even under Delphi 6.
//   Delphi 7 seems to finally get right.  But let's keep the UTF7 support at design time
//     to enable sharing source code with previous versions of Delphi.
//
//   The purpose of this solution is to store WideString properties which contain
//     non-ASCII chars in the form of UTF7 under the old property name + '_UTF7'.
//
//   Special thanks go to Francisco Leong for helping to develop this solution.
//

{ TTntWideStringPropertyFiler }
type
  TTntWideStringPropertyFiler = class
  private
    FInstance: TPersistent;
    FPropInfo: PPropInfo;
    procedure ReadDataUTF8(Reader: TReader);
    procedure ReadDataUTF7(Reader: TReader);
    procedure WriteDataUTF7(Writer: TWriter);
  public
    procedure DefineProperties(Filer: TFiler; Instance: TPersistent; PropName: AnsiString);
  end;

function ReaderNeedsUtfHelp(Reader: TReader): Boolean;
begin
  if Reader.Owner = nil then
    Result := False { designtime - visual form inheritance ancestor }
  else if csDesigning in Reader.Owner.ComponentState then
    {$IFDEF COMPILER_7_UP}
    Result := False { Delphi 7+: designtime - doesn't need UTF help. }
    {$ELSE}
    Result := True { Delphi 6: designtime - always needs UTF help. }
    {$ENDIF}
  else
    Result := RuntimeUTFStreaming; { runtime }
end;

procedure TTntWideStringPropertyFiler.ReadDataUTF8(Reader: TReader);
begin
  if ReaderNeedsUtfHelp(Reader) then
    SetWideStrProp(FInstance, FPropInfo, UTF8ToWideString(Reader.ReadString))
  else
    Reader.ReadString; { do nothing with Result }
end;

procedure TTntWideStringPropertyFiler.ReadDataUTF7(Reader: TReader);
begin
  if ReaderNeedsUtfHelp(Reader) then
    SetWideStrProp(FInstance, FPropInfo, UTF7ToWideString(Reader.ReadString))
  else
    Reader.ReadString; { do nothing with Result }
end;

procedure TTntWideStringPropertyFiler.WriteDataUTF7(Writer: TWriter);
begin
  Writer.WriteString(WideStringToUTF7(GetWideStrProp(FInstance, FPropInfo)));
end;

procedure TTntWideStringPropertyFiler.DefineProperties(Filer: TFiler; Instance: TPersistent;
  PropName: AnsiString);

  {$IFNDEF COMPILER_7_UP}
  function HasData: Boolean;
  var
    CurrPropValue: WideString;
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
    if Result then begin
      // must be non-blank and different than UTF8 (implies all ASCII <= 127)
      CurrPropValue := GetWideStrProp(Instance, FPropInfo);
      Result := (CurrPropValue <> '') and (WideStringToUTF8(CurrPropValue) <> CurrPropValue);
    end;
  end;
  {$ENDIF}

begin
  FInstance := Instance;
  FPropInfo := GetPropInfo(Instance, PropName, [tkWString]);
  if FPropInfo <> nil then begin
    // must be published (and of type WideString)
    Filer.DefineProperty(PropName + 'W', ReadDataUTF8, nil, False);
    {$IFDEF COMPILER_7_UP}
    Filer.DefineProperty(PropName + '_UTF7', ReadDataUTF7, WriteDataUTF7, False);
    {$ELSE}
    Filer.DefineProperty(PropName + '_UTF7', ReadDataUTF7, WriteDataUTF7, HasData);
    {$ENDIF}
  end;
  FInstance := nil;
  FPropInfo := nil;
end;

{ TTntWideCharPropertyFiler }
type
  TTntWideCharPropertyFiler = class
  private
    FInstance: TPersistent;
    FPropInfo: PPropInfo;
    {$IFNDEF COMPILER_9_UP}
    FWriter: TWriter;
    procedure GetLookupInfo(var Ancestor: TPersistent;
      var Root, LookupRoot, RootAncestor: TComponent);
    {$ENDIF}
    procedure ReadData_W(Reader: TReader);
    procedure ReadDataUTF7(Reader: TReader);
    procedure WriteData_W(Writer: TWriter);
    function ReadChar(Reader: TReader): WideChar;
  public
    procedure DefineProperties(Filer: TFiler; Instance: TPersistent; PropName: AnsiString);
  end;

{$IFNDEF COMPILER_9_UP}
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

procedure TTntWideCharPropertyFiler.GetLookupInfo(var Ancestor: TPersistent;
  var Root, LookupRoot, RootAncestor: TComponent);
begin
  Ancestor := FWriter.Ancestor;
  Root := FWriter.Root;
  LookupRoot := FWriter.LookupRoot;
  RootAncestor := FWriter.RootAncestor;
end;
{$ENDIF}

function TTntWideCharPropertyFiler.ReadChar(Reader: TReader): WideChar;
var
  Temp: WideString;
begin
  case Reader.NextValue of
    vaWString:
      Temp := Reader.ReadWideString;
    vaString:
      Temp := Reader.ReadString;
    else
      raise EReadError.Create(SInvalidPropertyValue);
  end;

  if Length(Temp) > 1 then
    raise EReadError.Create(SInvalidPropertyValue);
  Result := Temp[1];
end;

procedure TTntWideCharPropertyFiler.ReadData_W(Reader: TReader);
begin
  SetOrdProp(FInstance, FPropInfo, Ord(ReadChar(Reader)));
end;

procedure TTntWideCharPropertyFiler.ReadDataUTF7(Reader: TReader);
var
  S: WideString;
begin
  S := UTF7ToWideString(Reader.ReadString);
  if S = '' then
    SetOrdProp(FInstance, FPropInfo, 0)
  else
    SetOrdProp(FInstance, FPropInfo, Ord(S[1]))
end;

type TAccessWriter = class(TWriter);

procedure TTntWideCharPropertyFiler.WriteData_W(Writer: TWriter);
var
  L: Integer;
  Temp: WideString;
begin
  Temp := WideChar(GetOrdProp(FInstance, FPropInfo));

  TAccessWriter(Writer).WriteValue(vaWString);
  L := Length(Temp);
  Writer.Write(L, SizeOf(Integer));
  Writer.Write(Pointer(@Temp[1])^, L * 2);
end;

procedure TTntWideCharPropertyFiler.DefineProperties(Filer: TFiler;
  Instance: TPersistent; PropName: AnsiString);

  {$IFNDEF COMPILER_9_UP}
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
  {$ENDIF}

begin
  FInstance := Instance;
  FPropInfo := GetPropInfo(Instance, PropName, [tkWChar]);
  if FPropInfo <> nil then
  begin
    // must be published (and of type WideChar)
    {$IFDEF COMPILER_9_UP}
    Filer.DefineProperty(PropName + 'W', ReadData_W, WriteData_W, False);
    {$ELSE}
    Filer.DefineProperty(PropName + 'W', ReadData_W, WriteData_W, HasData);
    {$ENDIF}
    Filer.DefineProperty(PropName + '_UTF7', ReadDataUTF7, nil, False);
  end;
  FInstance := nil;
  FPropInfo := nil;
end;

procedure TntPersistent_AfterInherited_DefineProperties(Filer: TFiler; Instance: TPersistent);
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  WideStringFiler: TTntWideStringPropertyFiler;
  WideCharFiler: TTntWideCharPropertyFiler;
begin
  Count := GetTypeData(Instance.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    WideStringFiler := TTntWideStringPropertyFiler.Create;
    try
      WideCharFiler := TTntWideCharPropertyFiler.Create;
      try
        GetMem(PropList, Count * SizeOf(Pointer));
        try
          GetPropInfos(Instance.ClassInfo, PropList);
          for I := 0 to Count - 1 do
          begin
            PropInfo := PropList^[I];
            if (PropInfo = nil) then
              break;
            if (PropInfo.PropType^.Kind = tkWString) then
              WideStringFiler.DefineProperties(Filer, Instance, PropInfo.Name)
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
      WideStringFiler.Free;
    end;
  end;
end;

{ TTntFileStream }

constructor TTntFileStream.Create(const FileName: WideString; Mode: Word);
var
  CreateHandle: Integer;
  {$IFDEF DELPHI_7_UP}
  ErrorMessage: WideString;
  {$ENDIF}
begin
  if Mode = fmCreate then
  begin
    CreateHandle := WideFileCreate(FileName);
    if CreateHandle < 0 then begin
      {$IFDEF DELPHI_7_UP}
      ErrorMessage := WideSysErrorMessage(GetLastError);
      raise EWideFCreateError.CreateFmt(SFCreateErrorEx, [WideExpandFileName(FileName), ErrorMessage]);
      {$ELSE}
      raise EWideFCreateError.CreateFmt(SFCreateError, [WideExpandFileName(FileName)]);
      {$ENDIF}
    end;
  end
  else
  begin
    CreateHandle := WideFileOpen(FileName, Mode);
    if CreateHandle < 0 then begin
      {$IFDEF DELPHI_7_UP}
      ErrorMessage := WideSysErrorMessage(GetLastError);
      raise EWideFOpenError.CreateFmt(SFOpenErrorEx, [WideExpandFileName(FileName), ErrorMessage]);
      {$ELSE}
      raise EWideFOpenError.CreateFmt(SFOpenError, [WideExpandFileName(FileName)]);
      {$ENDIF}
    end;
  end;
  inherited Create(CreateHandle);
end;

destructor TTntFileStream.Destroy;
begin
  if Handle >= 0 then FileClose(Handle);
end;

{ TTntMemoryStream }

procedure TTntMemoryStream.LoadFromFile(const FileName: WideString);
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

procedure TTntMemoryStream.SaveToFile(const FileName: WideString);
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

{ TTntResourceStream }

constructor TTntResourceStream.Create(Instance: THandle; const ResName: WideString;
  ResType: PWideChar);
begin
  inherited Create;
  Initialize(Instance, PWideChar(ResName), ResType);
end;

constructor TTntResourceStream.CreateFromID(Instance: THandle; ResID: Word;
  ResType: PWideChar);
begin
  inherited Create;
  Initialize(Instance, PWideChar(ResID), ResType);
end;

procedure TTntResourceStream.Initialize(Instance: THandle; Name, ResType: PWideChar);

  procedure Error;
  begin
    raise EResNotFound.CreateFmt(SResNotFound, [Name]);
  end;

begin
  HResInfo := FindResourceW(Instance, Name, ResType);
  if HResInfo = 0 then Error;
  HGlobal := LoadResource(Instance, HResInfo);
  if HGlobal = 0 then Error;
  SetPointer(LockResource(HGlobal), SizeOfResource(Instance, HResInfo));
end;

destructor TTntResourceStream.Destroy;
begin
  UnlockResource(HGlobal);
  FreeResource(HGlobal); { Technically this is not necessary (MS KB #193678) }
  inherited Destroy;
end;

function TTntResourceStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamError.CreateRes(PResStringRec(@SCantWriteResourceStreamError));
end;

procedure TTntResourceStream.SaveToFile(const FileName: WideString);
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

{ TAnsiStrings }

procedure TAnsiStrings{TNT-ALLOW TAnsiStrings}.LoadFromFile(const FileName: WideString);
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

procedure TAnsiStrings{TNT-ALLOW TAnsiStrings}.SaveToFile(const FileName: WideString);
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

procedure TAnsiStrings{TNT-ALLOW TAnsiStrings}.LoadFromFileEx(const FileName: WideString; CodePage: Cardinal);
var
  Stream: TStream;
begin
  Stream := TTntFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStreamEx(Stream, CodePage);
  finally
    Stream.Free;
  end;
end;

procedure TAnsiStrings{TNT-ALLOW TAnsiStrings}.SaveToFileEx(const FileName: WideString; CodePage: Cardinal);
var
  Stream: TStream;
begin
  Stream := TTntFileStream.Create(FileName, fmCreate);
  try
    if (CodePage = CP_UTF8) then
      Stream.WriteBuffer(PAnsiChar(UTF8_BOM)^, Length(UTF8_BOM));
    SaveToStreamEx(Stream, CodePage);
  finally
    Stream.Free;
  end;
end;

{ TAnsiStringsForWideStringsAdapter }

constructor TAnsiStringsForWideStringsAdapter.Create(AWideStrings: TTntStrings; _AdapterCodePage: Cardinal);
begin
  inherited Create;
  FWideStrings := AWideStrings;
  FAdapterCodePage := _AdapterCodePage;
end;

function TAnsiStringsForWideStringsAdapter.AdapterCodePage: Cardinal;
begin
  if FAdapterCodePage = 0 then
    Result := TntSystem.DefaultSystemCodePage
  else
    Result := FAdapterCodePage;
end;

procedure TAnsiStringsForWideStringsAdapter.Clear;
begin
  FWideStrings.Clear;
end;

procedure TAnsiStringsForWideStringsAdapter.Delete(Index: Integer);
begin
  FWideStrings.Delete(Index);
end;

function TAnsiStringsForWideStringsAdapter.Get(Index: Integer): AnsiString;
begin
  Result := WideStringToStringEx(FWideStrings.Get(Index), AdapterCodePage);
end;

procedure TAnsiStringsForWideStringsAdapter.Put(Index: Integer; const S: AnsiString);
begin
  FWideStrings.Put(Index, StringToWideStringEx(S, AdapterCodePage));
end;

function TAnsiStringsForWideStringsAdapter.GetCount: Integer;
begin
  Result := FWideStrings.GetCount;
end;

procedure TAnsiStringsForWideStringsAdapter.Insert(Index: Integer; const S: AnsiString);
begin
  FWideStrings.Insert(Index, StringToWideStringEx(S, AdapterCodePage));
end;

function TAnsiStringsForWideStringsAdapter.GetObject(Index: Integer): TObject;
begin
  Result := FWideStrings.GetObject(Index);
end;

procedure TAnsiStringsForWideStringsAdapter.PutObject(Index: Integer; AObject: TObject);
begin
  FWideStrings.PutObject(Index, AObject);
end;

procedure TAnsiStringsForWideStringsAdapter.SetUpdateState(Updating: Boolean);
begin
  FWideStrings.SetUpdateState(Updating);
end;

procedure TAnsiStringsForWideStringsAdapter.LoadFromStreamEx(Stream: TStream; CodePage: Cardinal);
var
  Size: Integer;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    FWideStrings.SetTextStr(StringToWideStringEx(S, CodePage));
  finally
    EndUpdate;
  end;
end;

procedure TAnsiStringsForWideStringsAdapter.SaveToStreamEx(Stream: TStream; CodePage: Cardinal);
var
  S: AnsiString;
begin
  S := WideStringToStringEx(FWideStrings.GetTextStr, CodePage);
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

{ TTntStrings }

constructor TTntStrings.Create;
begin
  inherited;
  FAnsiStrings := TAnsiStringsForWideStringsAdapter.Create(Self);
  FLastFileCharSet := csUnicode;
end;

destructor TTntStrings.Destroy;
begin
  FreeAndNil(FAnsiStrings);
  inherited;
end;

procedure TTntStrings.SetAnsiStrings(const Value: TAnsiStrings{TNT-ALLOW TAnsiStrings});
begin
  FAnsiStrings.Assign(Value);
end;

procedure TTntStrings.DefineProperties(Filer: TFiler);

  {$IFNDEF COMPILER_7_UP}
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

  function DoWriteAsUTF7: Boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to Count - 1 do begin
      if (Strings[i] <> '') and (WideStringToUTF8(Strings[i]) <> Strings[i]) then begin
        Result := True;
        break; { found a string with non-ASCII chars (> 127) }
      end;
    end;
  end;
  {$ENDIF}

begin
  inherited DefineProperties(Filer); { Handles main 'Strings' property.' }
  Filer.DefineProperty('WideStrings', ReadData, nil, False);
  Filer.DefineProperty('WideStringsW', ReadDataUTF8, nil, False);
  {$IFDEF COMPILER_7_UP}
  Filer.DefineProperty('WideStrings_UTF7', ReadDataUTF7, WriteDataUTF7, False);
  {$ELSE}
  Filer.DefineProperty('WideStrings_UTF7', ReadDataUTF7, WriteDataUTF7, DoWrite and DoWriteAsUTF7);
  {$ENDIF}
end;

procedure TTntStrings.LoadFromFile(const FileName: WideString);
var
  Stream: TStream;
begin
  Stream := TTntFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    FLastFileCharSet := AutoDetectCharacterSet(Stream);
    Stream.Position := 0;
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTntStrings.LoadFromStream(Stream: TStream);
begin
  LoadFromStream_BOM(Stream, True);
end;

procedure TTntStrings.LoadFromStream_BOM(Stream: TStream; WithBOM: Boolean);
var
  DataLeft: Integer;
  StreamCharSet: TTntStreamCharSet;
  SW: WideString;
  SA: AnsiString;
begin
  BeginUpdate;
  try
    if WithBOM then
      StreamCharSet := AutoDetectCharacterSet(Stream)
    else
      StreamCharSet := csUnicode;
    DataLeft := Stream.Size - Stream.Position;
    if (StreamCharSet in [csUnicode, csUnicodeSwapped]) then
    begin
      // BOM indicates Unicode text stream
      if DataLeft < SizeOf(WideChar) then
        SW := ''
      else begin
        SetLength(SW, DataLeft div SizeOf(WideChar));
        Stream.Read(PWideChar(SW)^, DataLeft);
        if StreamCharSet = csUnicodeSwapped then
          StrSwapByteOrder(PWideChar(SW));
      end;
      SetTextStr(SW);
    end
    else if StreamCharSet = csUtf8 then
    begin
      // BOM indicates UTF-8 text stream
      SetLength(SA, DataLeft div SizeOf(AnsiChar));
      Stream.Read(PAnsiChar(SA)^, DataLeft);
      SetTextStr(UTF8ToWideString(SA));
    end
    else
    begin
      // without byte order mark it is assumed that we are loading ANSI text
      SetLength(SA, DataLeft div SizeOf(AnsiChar));
      Stream.Read(PAnsiChar(SA)^, DataLeft);
      SetTextStr(SA);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TTntStrings.ReadData(Reader: TReader);
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

procedure TTntStrings.ReadDataUTF7(Reader: TReader);
begin
  Reader.ReadListBegin;
  if ReaderNeedsUtfHelp(Reader) then
  begin
    BeginUpdate;
    try
      Clear;
      while not Reader.EndOfList do
        Add(UTF7ToWideString(Reader.ReadString))
    finally
      EndUpdate;
    end;
  end else begin
    while not Reader.EndOfList do
      Reader.ReadString; { do nothing with Result }
  end;
  Reader.ReadListEnd;
end;

procedure TTntStrings.ReadDataUTF8(Reader: TReader);
begin
  Reader.ReadListBegin;
  if ReaderNeedsUtfHelp(Reader)
  or (Count = 0){ Legacy support where 'WideStrings' was never written in lieu of WideStringsW }
  then begin
    BeginUpdate;
    try
      Clear;
      while not Reader.EndOfList do
        Add(UTF8ToWideString(Reader.ReadString))
    finally
      EndUpdate;
    end;
  end else begin
    while not Reader.EndOfList do
      Reader.ReadString; { do nothing with Result }
  end;
  Reader.ReadListEnd;
end;

procedure TTntStrings.SaveToFile(const FileName: WideString);
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

procedure TTntStrings.SaveToStream(Stream: TStream);
begin
  SaveToStream_BOM(Stream, True);
end;

procedure TTntStrings.SaveToStream_BOM(Stream: TStream; WithBOM: Boolean);
// Saves the currently loaded text into the given stream.
// WithBOM determines whether to write a byte order mark or not.
var
  SW: WideString;
  BOM: WideChar;
begin
  if WithBOM then begin
    BOM := UNICODE_BOM;
    Stream.WriteBuffer(BOM, SizeOf(WideChar));
  end;
  SW := GetTextStr;
  Stream.WriteBuffer(PWideChar(SW)^, Length(SW) * SizeOf(WideChar));
end;

procedure TTntStrings.WriteDataUTF7(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count-1 do
    Writer.WriteString(WideStringToUTF7(Get(I)));
  Writer.WriteListEnd;
end;

{ TTntStringList }

destructor TTntStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TTntStringList.Add(const S: WideString): Integer;
begin
  Result := AddObject(S, nil);
end;

function TTntStringList.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(PResStringRec(@SDuplicateString), 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TTntStringList.Changed;
begin
  if (not FUpdating) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTntStringList.Changing;
begin
  if (not FUpdating) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TTntStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TTntStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(PResStringRec(@SListIndexError), Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TWideStringItem));
  Changed;
end;

procedure TTntStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(PResStringRec(@SListIndexError), Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(PResStringRec(@SListIndexError), Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TTntStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PWideStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TTntStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TTntStringList.Get(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= FCount) then Error(PResStringRec(@SListIndexError), Index);
  Result := FList^[Index].FString;
end;

function TTntStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TTntStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TTntStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(PResStringRec(@SListIndexError), Index);
  Result := FList^[Index].FObject;
end;

procedure TTntStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TTntStringList.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

function TTntStringList.IndexOfName(const Name: WideString): Integer;
var
  NameKey: WideString;
begin
  if not Sorted then
    Result := inherited IndexOfName(Name)
  else begin
    // use sort to find index more quickly
    NameKey := Name + NameValueSeparator;
    Find(NameKey, Result);
    if (Result < 0) or (Result > Count - 1) then
      Result := -1
    else if CompareStrings(NameKey, Copy(Strings[Result], 1, Length(NameKey))) <> 0 then
      Result := -1
  end;
end;

procedure TTntStringList.Insert(Index: Integer; const S: WideString);
begin
  InsertObject(Index, S, nil);
end;

procedure TTntStringList.InsertObject(Index: Integer; const S: WideString;
  AObject: TObject);
begin
  if Sorted then Error(PResStringRec(@SSortedListError), 0);
  if (Index < 0) or (Index > FCount) then Error(PResStringRec(@SListIndexError), Index);
  InsertItem(Index, S, AObject);
end;

procedure TTntStringList.InsertItem(Index: Integer; const S: WideString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TWideStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TTntStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then Error(PResStringRec(@SSortedListError), 0);
  if (Index < 0) or (Index >= FCount) then Error(PResStringRec(@SListIndexError), Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TTntStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(PResStringRec(@SListIndexError), Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TTntStringList.QuickSort(L, R: Integer; SCompare: TWideStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TTntStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TWideStringItem));
  FCapacity := NewCapacity;
end;

procedure TTntStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TTntStringList.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if Updating then Changing else Changed;
end;

function WideStringListCompareStrings(List: TTntStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList^[Index1].FString,
                                List.FList^[Index2].FString);
end;

procedure TTntStringList.Sort;
begin
  CustomSort(WideStringListCompareStrings);
end;

procedure TTntStringList.CustomSort(Compare: TWideStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TTntStringList.CompareStrings(const S1, S2: WideString): Integer;
begin
  if CaseSensitive then
    Result := WideCompareStr(S1, S2)
  else
    Result := WideCompareText(S1, S2);
end;

procedure TTntStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

//------------------------- TntClasses introduced procs ----------------------------------

function AutoDetectCharacterSet(Stream: TStream): TTntStreamCharSet;
var
  ByteOrderMark: WideChar;
  BytesRead: Integer;
  Utf8Test: array[0..2] of AnsiChar;
begin
  // Byte Order Mark
  ByteOrderMark := #0;
  if (Stream.Size - Stream.Position) >= SizeOf(ByteOrderMark) then begin
    BytesRead := Stream.Read(ByteOrderMark, SizeOf(ByteOrderMark));
    if (ByteOrderMark <> UNICODE_BOM) and (ByteOrderMark <> UNICODE_BOM_SWAPPED) then begin
      ByteOrderMark := #0;
      Stream.Seek(-BytesRead, soFromCurrent);
      if (Stream.Size - Stream.Position) >= Length(Utf8Test) * SizeOf(AnsiChar) then begin
        BytesRead := Stream.Read(Utf8Test[0], Length(Utf8Test) * SizeOf(AnsiChar));
        if Utf8Test <> UTF8_BOM then
          Stream.Seek(-BytesRead, soFromCurrent);
      end;
    end;
  end;
  // Test Byte Order Mark
  if ByteOrderMark = UNICODE_BOM then
    Result := csUnicode
  else if ByteOrderMark = UNICODE_BOM_SWAPPED then
    Result := csUnicodeSwapped
  else if Utf8Test = UTF8_BOM then
    Result := csUtf8
  else
    Result := csAnsi;
end;

function FindSortedListByTarget(List: TList; TargetCompare: TListTargetCompare;
  Target: Pointer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := List.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := TargetCompare(List[i], Target);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function ClassIsRegistered(const clsid: TCLSID): Boolean;
var
  OleStr: POleStr;
  Reg: TRegIniFile;
  Key, Filename: WideString;
begin
  // First, check to see if there is a ProgID.  This will tell if the
  // control is registered on the machine.  No ProgID, control won't run
  Result := ProgIDFromCLSID(clsid, OleStr) = S_OK;
  if not Result then Exit;  //Bail as soon as anything goes wrong.

  // Next, make sure that the file is actually there by rooting it out
  // of the registry
  Key := WideFormat('\SOFTWARE\Classes\CLSID\%s', [GUIDToString(clsid)]);
  Reg := TRegIniFile.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := Reg.OpenKeyReadOnly(Key);
    if not Result then Exit; // Bail as soon as anything goes wrong.

    FileName := Reg.ReadString('InProcServer32', '', EmptyStr);
    if (Filename = EmptyStr) then // try another key for the file name
    begin
      FileName := Reg.ReadString('InProcServer', '', EmptyStr);
    end;
    Result := Filename <> EmptyStr;
    if not Result then Exit;
    Result := WideFileExists(Filename);
  finally
    Reg.Free;
  end;
end;

{ TBufferedAnsiString }

procedure TBufferedAnsiString.Clear;
begin
  LastWriteIndex := 0;
  if Length(FStringBuffer) > 0 then
    FillChar(FStringBuffer[1], Length(FStringBuffer) * SizeOf(AnsiChar), 0);
end;

procedure TBufferedAnsiString.AddChar(const wc: AnsiChar);
const
  MIN_GROW_SIZE = 32;
  MAX_GROW_SIZE = 256;
var
  GrowSize: Integer;
begin
  Inc(LastWriteIndex);
  if LastWriteIndex > Length(FStringBuffer) then begin
    GrowSize := Max(MIN_GROW_SIZE, Length(FStringBuffer));
    GrowSize := Min(GrowSize, MAX_GROW_SIZE);
    SetLength(FStringBuffer, Length(FStringBuffer) + GrowSize);
    FillChar(FStringBuffer[LastWriteIndex], GrowSize * SizeOf(AnsiChar), 0);
  end;
  FStringBuffer[LastWriteIndex] := wc;
end;

procedure TBufferedAnsiString.AddString(const s: AnsiString);
var
  LenS: Integer;
  BlockSize: Integer;
  AllocSize: Integer;
begin
  LenS := Length(s);
  if LenS > 0 then begin
    Inc(LastWriteIndex);
    if LastWriteIndex + LenS - 1 > Length(FStringBuffer) then begin
      // determine optimum new allocation size
      BlockSize := Length(FStringBuffer) div 2;
      if BlockSize < 8 then
        BlockSize := 8;
      AllocSize := ((LenS div BlockSize) + 1) * BlockSize;
      // realloc buffer
      SetLength(FStringBuffer, Length(FStringBuffer) + AllocSize);
      FillChar(FStringBuffer[Length(FStringBuffer) - AllocSize + 1], AllocSize * SizeOf(AnsiChar), 0);
    end;
    CopyMemory(@FStringBuffer[LastWriteIndex], @s[1], LenS * SizeOf(AnsiChar));
    Inc(LastWriteIndex, LenS - 1);
  end;
end;

procedure TBufferedAnsiString.AddBuffer(Buff: PAnsiChar; Chars: Integer);
var
  i: integer;
begin
  for i := 1 to Chars do begin
    if Buff^ = #0 then
      break;
    AddChar(Buff^);
    Inc(Buff);
  end;
end;

function TBufferedAnsiString.Value: AnsiString;
begin
  Result := PAnsiChar(FStringBuffer);
end;

function TBufferedAnsiString.BuffPtr: PAnsiChar;
begin
  Result := PAnsiChar(FStringBuffer);
end;

{ TBufferedWideString }

procedure TBufferedWideString.Clear;
begin
  LastWriteIndex := 0;
  if Length(FStringBuffer) > 0 then
    FillChar(FStringBuffer[1], Length(FStringBuffer) * SizeOf(WideChar), 0);
end;

procedure TBufferedWideString.AddChar(const wc: WideChar);
const
  MIN_GROW_SIZE = 32;
  MAX_GROW_SIZE = 256;
var
  GrowSize: Integer;
begin
  Inc(LastWriteIndex);
  if LastWriteIndex > Length(FStringBuffer) then begin
    GrowSize := Max(MIN_GROW_SIZE, Length(FStringBuffer));
    GrowSize := Min(GrowSize, MAX_GROW_SIZE);
    SetLength(FStringBuffer, Length(FStringBuffer) + GrowSize);
    FillChar(FStringBuffer[LastWriteIndex], GrowSize * SizeOf(WideChar), 0);
  end;
  FStringBuffer[LastWriteIndex] := wc;
end;

procedure TBufferedWideString.AddString(const s: WideString);
var
  i: integer;
begin
  for i := 1 to Length(s) do
    AddChar(s[i]);
end;

procedure TBufferedWideString.AddBuffer(Buff: PWideChar; Chars: Integer);
var
  i: integer;
begin
  for i := 1 to Chars do begin
    if Buff^ = #0 then
      break;
    AddChar(Buff^);
    Inc(Buff);
  end;
end;

function TBufferedWideString.Value: WideString;
begin
  Result := PWideChar(FStringBuffer);
end;

function TBufferedWideString.BuffPtr: PWideChar;
begin
  Result := PWideChar(FStringBuffer);
end;

{ TBufferedStreamReader }

constructor TBufferedStreamReader.Create(Stream: TStream; BufferSize: Integer = 1024);
begin
  // init stream
  FStream := Stream;
  FStreamSize := Stream.Size;
  // init buffer
  FBufferSize := BufferSize;
  SetLength(FBuffer, BufferSize);
  FBufferStartPosition := -FBufferSize; { out of any useful range }
  // init virtual position
  FVirtualPosition := 0;
end;

function TBufferedStreamReader.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FVirtualPosition := Offset;
    soFromCurrent:   Inc(FVirtualPosition, Offset);
    soFromEnd:       FVirtualPosition := FStreamSize + Offset;
  end;
  Result := FVirtualPosition;
end;

procedure TBufferedStreamReader.UpdateBufferFromPosition(StartPos: Integer);
begin
  try
    FStream.Position := StartPos;
    FStream.Read(FBuffer[0], FBufferSize);
    FBufferStartPosition := StartPos;
  except
    FBufferStartPosition := -FBufferSize; { out of any useful range }
    raise;
  end;
end;

function TBufferedStreamReader.Read(var Buffer; Count: Integer): Longint;
var
  BytesLeft: Integer;
  FirstBufferRead: Integer;
  StreamDirectRead: Integer;
  Buf: PAnsiChar;
begin
  if (FVirtualPosition >= 0) and (Count >= 0) then
  begin
    Result := FStreamSize - FVirtualPosition;
    if Result > 0 then
    begin
      if Result > Count then
        Result := Count;

      Buf := @Buffer;
      BytesLeft := Result;

      // try to read what is left in buffer
      FirstBufferRead := FBufferStartPosition + FBufferSize - FVirtualPosition;
      if (FirstBufferRead < 0) or (FirstBufferRead > FBufferSize) then
        FirstBufferRead := 0;
      FirstBufferRead := Min(FirstBufferRead, Result);
      if FirstBufferRead > 0 then begin
        Move(FBuffer[FVirtualPosition - FBufferStartPosition], Buf[0], FirstBufferRead);
        Dec(BytesLeft, FirstBufferRead);
      end;

      if BytesLeft > 0 then begin
        // The first read in buffer was not enough
        StreamDirectRead := (BytesLeft div FBufferSize) * FBufferSize;
        FStream.Position := FVirtualPosition + FirstBufferRead;
        FStream.Read(Buf[FirstBufferRead], StreamDirectRead);
        Dec(BytesLeft, StreamDirectRead);

        if BytesLeft > 0 then begin
          // update buffer, and read what is left
          UpdateBufferFromPosition(FStream.Position);
          Move(FBuffer[0], Buf[FirstBufferRead + StreamDirectRead], BytesLeft);
        end;
      end;

      Inc(FVirtualPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TBufferedStreamReader.Write(const Buffer; Count: Integer): Longint;
begin
  raise ETntInternalError.Create('Internal Error: class can not write.');
  Result := 0;
end;

//-------- synced wide string -----------------

function GetSyncedWideString(var WideStr: WideString; const AnsiStr: AnsiString): WideString;
begin
  if AnsiString(WideStr) <> (AnsiStr) then begin
    WideStr := AnsiStr; {AnsiStr changed.  Keep WideStr in sync.}
  end;
  Result := WideStr;
end;

procedure SetSyncedWideString(const Value: WideString; var WideStr: WideString;
  const AnsiStr: AnsiString; SetAnsiStr: TSetAnsiStrEvent);
begin
  if Value <> GetSyncedWideString(WideStr, AnsiStr) then
  begin
    if (not WideSameStr(Value, AnsiString(Value))) {unicode chars lost in conversion}
    and (AnsiStr = AnsiString(Value))  {AnsiStr is not going to change}
    then begin
      SetAnsiStr(''); {force the change}
    end;
    WideStr := Value;
    SetAnsiStr(Value);
  end;
end;

{ TWideComponentHelper }

function CompareComponentHelperToTarget(Item, Target: Pointer): Integer;
begin
  if Integer(TWideComponentHelper(Item).FComponent) < Integer(Target) then
    Result := -1
  else if Integer(TWideComponentHelper(Item).FComponent) > Integer(Target) then
    Result := 1
  else
    Result := 0;
end;

function FindWideComponentHelperIndex(ComponentHelperList: TComponentList; Component: TComponent; var Index: Integer): Boolean;
begin
  // find Component in sorted wide caption list (list is sorted by TWideComponentHelper.FComponent)
  Result := FindSortedListByTarget(ComponentHelperList, CompareComponentHelperToTarget, Component, Index);
end;

constructor TWideComponentHelper.Create(AOwner: TComponent);
begin
  raise ETntInternalError.Create('TNT Internal Error: TWideComponentHelper.Create should never be encountered.');
end;

constructor TWideComponentHelper.CreateHelper(AOwner: TComponent; ComponentHelperList: TComponentList);
var
  Index: Integer;
begin
  // don't use direct ownership for memory management
  inherited Create(nil);
  FComponent := AOwner;
  FComponent.FreeNotification(Self);

  // insert into list according to sort
  FindWideComponentHelperIndex(ComponentHelperList, FComponent, Index);
  ComponentHelperList.Insert(Index, Self);
end;

procedure TWideComponentHelper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FComponent) and (Operation = opRemove) then begin
    FComponent := nil;
    Free;
  end;
end;

function FindWideComponentHelper(ComponentHelperList: TComponentList; Component: TComponent): TWideComponentHelper;
var
  Index: integer;
begin
  if FindWideComponentHelperIndex(ComponentHelperList, Component, Index) then begin
  	Result := TWideComponentHelper(ComponentHelperList[Index]);
    Assert(Result.FComponent = Component, 'TNT Internal Error: FindWideComponentHelperIndex failed.');
  end else
    Result := nil;
end;

initialization
  RuntimeUTFStreaming := False; { Delphi 6 and higher don't need UTF help at runtime. }

end.
