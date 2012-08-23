unit helpers;


// -------------------------------------
// Functions-library
// -------------------------------------


interface

uses
  Classes, SysUtils, Graphics, GraphUtil, ClipBrd, Dialogs, Forms, Controls, ShellApi,
  Windows, ShlObj, ActiveX, VirtualTrees, SynRegExpr, Messages, Math,
  Registry, DateUtils, Generics.Collections, StrUtils, AnsiStrings, TlHelp32, Types,
  dbconnection, mysql_structures, SynMemo, Menus, WinInet;

type

  TOrderCol = class(TObject)
    ColumnName: String;
    SortDirection: Byte;
  end;
  TOrderColArray = Array of TOrderCol;

  TLineBreaks = (lbsNone, lbsWindows, lbsUnix, lbsMac, lbsWide, lbsMixed);

  TDBObjectEditor = class(TFrame)
    private
      FModified: Boolean;
      FDefiners: TStringList;
      procedure SetModified(Value: Boolean);
    protected
    public
      DBObject: TDBObject;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Init(Obj: TDBObject); virtual;
      function DeInit: TModalResult;
      function GetDefiners: TStringList;
      property Modified: Boolean read FModified write SetModified;
      function ApplyModifications: TModalResult; virtual; abstract;
  end;
  TDBObjectEditorClass = class of TDBObjectEditor;

  TWndProc = function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  PGripInfo = ^TGripInfo;
  TGripInfo = record
    OldWndProc: TWndProc;
    Enabled: boolean;
    GripRect: TRect;
  end;

  TSQLBatch = class;
  TSQLSentence = class(TObject)
    private
      FOwner: TSQLBatch;
      function GetSize: Integer;
      function GetSQL: String;
    public
      LeftOffset, RightOffset: Integer;
      constructor Create(Owner: TSQLBatch);
      property SQL: String read GetSQL;
      property Size: Integer read GetSize;
  end;
  TSQLBatch = class(TObjectList<TSQLSentence>)
    private
      FSQL: String;
      procedure SetSQL(Value: String);
      function GetSize: Integer;
    public
      property Size: Integer read GetSize;
      property SQL: String read FSQL write SetSQL;
  end;

  // Download
  THttpDownload = class(TObject)
    private
      FOwner: TComponent;
      FURL: String;
      FBytesRead: Integer;
      FContentLength: Integer;
      FOnProgress: TNotifyEvent;
    public
      constructor Create(Owner: TComponent);
      procedure SendRequest(Filename: String);
      property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
      property URL: String read FURL write FURL;
      property BytesRead: Integer read FBytesRead;
      property ContentLength: Integer read FContentLength;
  end;

  // Threading stuff
  TQueryThread = class(TThread)
  private
    FConnection: TDBConnection;
    FBatch: TSQLBatch;
    FTabNumber: Integer;
    FBatchInOneGo: Boolean;
    FStopOnErrors: Boolean;
    FAborted: Boolean;
    FErrorMessage: String;
    FBatchPosition: Integer;
    FQueriesInPacket: Integer;
    FQueryTime: Cardinal;
    FQueryNetTime: Cardinal;
    FRowsAffected: Int64;
    FRowsFound: Int64;
    FWarningCount: Int64;
    FLogMsg: String;
    FLogCategory: TDBLogCategory;
    procedure BeforeQuery;
    procedure AfterQuery;
    procedure BatchFinished;
    procedure Log;
  public
    property Connection: TDBConnection read FConnection;
    property Batch: TSQLBatch read FBatch;
    property TabNumber: Integer read FTabNumber;
    property BatchPosition: Integer read FBatchPosition;
    property QueriesInPacket: Integer read FQueriesInPacket;
    property QueryTime: Cardinal read FQueryTime;
    property QueryNetTime: Cardinal read FQueryNetTime;
    property RowsAffected: Int64 read FRowsAffected;
    property RowsFound: Int64 read FRowsFound;
    property WarningCount: Int64 read FWarningCount;
    property Aborted: Boolean read FAborted write FAborted;
    property ErrorMessage: String read FErrorMessage;
    constructor Create(Connection: TDBConnection; Batch: TSQLBatch; TabNumber: Integer);
    procedure Execute; override;
    procedure LogFromOutside(Msg: String; Category: TDBLogCategory);
  end;

  TAppSettingDataType = (adInt, adBool, adString);
  TAppSettingIndex = (asHiddenColumns, asFilter, asSort, asDisplayedColumnsSorted, asLastSessions,
    asLastActiveSession, asAutoReconnect, asRestoreLastUsedDB, asLastUsedDB, asTreeBackground,
    asFontName, asFontSize, asTabWidth, asDataFontName, asDataFontSize,
    asLogsqlnum, asLogsqlwidth, asSessionLogsDirectory, asLogHorizontalScrollbar, asSQLColActiveLine,
    asMaxColWidth, asDatagridMaximumRows, asDatagridRowsPerStep, asGridRowLineCount, asRememberFilters,
    asLogToFile, asMainWinMaximized, asMainWinLeft, asMainWinTop, asMainWinWidth,
    asMainWinHeight, asMainWinOnMonitor, asToolBar2Left, asToolBar2Top, asToolBarDataLeft,
    asToolBarDataTop, asToolBarQueryLeft, asToolBarQueryTop, asQuerymemoheight, asDbtreewidth,
    asDataPreviewHeight, asDataPreviewEnabled, asLogHeight, asQueryhelperswidth, asStopOnErrorsInBatchMode,
    asWrapLongLines, asDisplayBLOBsAsText, asSingleQueries, asMemoEditorWidth, asMemoEditorHeight,
    asMemoEditorWrap, asDelimiter, asSQLHelpWindowLeft, asSQLHelpWindowTop, asSQLHelpWindowWidth,
    asSQLHelpWindowHeight, asSQLHelpPnlLeftWidth, asSQLHelpPnlRightTopHeight, asTableEditorTabsHeight, asHost,
    asUser, asPassword, asWindowsAuth, asLoginPrompt, asPort,
    asPlinkExecutable, asSSHtunnelHost, asSSHtunnelHostPort, asSSHtunnelPort, asSSHtunnelUser,
    asSSHtunnelPassword, asSSHtunnelTimeout, asSSHtunnelPrivateKey, asSSLActive, asSSLKey,
    asSSLCert, asSSLCA, asNetType, asCompressed, asLocalTimeZone,
    asStartupScriptFilename, asDatabases, asDatabaseFilter, asExportSQLCreateDatabases, asExportSQLDropDatabases,
    asExportSQLCreateTables, asExportSQLDropTables, asExportSQLDataHow, asExportSQLFilenames, asExportSQLDirectories,
    asExportSQLDatabase, asExportSQLServerDatabase, asExportSQLOutput, asGridExportOutputCopy, asGridExportOutputFile,
    asGridExportFilename, asGridExportRecentFiles, asGridExportEncoding, asGridExportFormat, asGridExportSelection,
    asGridExportColumnNames, asGridExportSeparator, asGridExportEncloser, asGridExportTerminator, asCSVImportSeparator,
    asCSVImportEncloser, asCSVImportTerminator, asCSVImportFieldEscaper, asCSVImportWindowWidth, asCSVImportWindowHeight,
    asCSVImportFilename, asCSVImportFieldsEnclosedOptionally, asCSVImportIgnoreLines, asCSVImportLowPriority, asCSVImportLocalNumbers,
    asCSVImportTruncateTable, asCSVImportDuplicateHandling, asCSVImportParseMethod, asUpdatecheck, asUpdatecheckBuilds,
    asUpdatecheckInterval, asUpdatecheckLastrun, asTableToolsWindowWidth, asTableToolsWindowHeight, asTableToolsTreeWidth,
    asTableToolsFindText, asTableToolsDatatype, asTableToolsFindCaseSensitive, asFileImportWindowWidth, asFileImportWindowHeight,
    asEditVarWindowWidth, asEditVarWindowHeight, asUsermanagerWindowWidth, asUsermanagerWindowHeight, asUsermanagerListWidth,
    asSelectDBOWindowWidth, asSelectDBOWindowHeight, asSessionManagerListWidth, asSessionManagerWindowWidth, asSessionManagerWindowHeight,
    asCopyTableWindowHeight, asCopyTableWindowWidth, asCopyTableColumns, asCopyTableKeys, asCopyTableForeignKeys,
    asCopyTableData, asCopyTableRecentFilter, asServerVersion, asServerVersionFull, asLastConnect,
    asConnectCount, asRefusedCount, asSessionCreated, asDoUsageStatistics,
    asLastUsageStatisticCall, asDisplayBars, asBarColor, asMySQLBinaries, asPromptSaveFileOnTabClose,
    asCompletionProposal, asTabsToSpaces, asFilterPanel, asAllowMultipleInstances, asFindDialogSearchHistory,
    asFindDialogReplaceHistory, asMaxQueryResults, asSetEditorWidth, asSetEditorHeight, asLogErrors,
    asLogUserSQL, asLogSQL, asLogInfos, asLogDebug, asFieldColorNumeric,
    asFieldColorReal, asFieldColorText, asFieldColorBinary, asFieldColorDatetime, asFieldColorSpatial,
    asFieldColorOther, asFieldEditorBinary, asFieldEditorDatetime, asFieldEditorDatetimePrefill, asFieldEditorEnum,
    asFieldEditorSet, asFieldNullBackground, asGroupTreeObjects, asDisplayObjectSizeColumn, asSQLfile,
    asActionShortcut1, asActionShortcut2, asHighlighterForeground, asHighlighterBackground, asHighlighterStyle,
    asListColWidths, asListColsVisible, asListColPositions, asListColSort, asSessionFolder,
    asRecentFilter, asDateTimeEditorCursorPos);
  TAppSetting = record
    Name: String;
    Session: Boolean;
    DefaultInt, CurrentInt: Integer;
    DefaultBool, CurrentBool: Boolean;
    DefaultString, CurrentString: String;
    Synced: Boolean;
  end;
  TAppSettings = class(TObject)
    private
      FReads, FWrites: Integer;
      FBasePath: String;
      FSessionPath: String;
      FRegistry: TRegistry;
      FPortableMode: Boolean;
      FSettingsFile: String;
      FSettings: Array[TAppSettingIndex] of TAppSetting;
      procedure InitSetting(Index: TAppSettingIndex; Name: String;
        DefaultInt: Integer=0; DefaultBool: Boolean=False; DefaultString: String='';
        Session: Boolean=False);
      procedure SetSessionPath(Value: String);
      procedure PrepareRegistry;
      procedure Read(Index: TAppSettingIndex; FormatName: String;
        DataType: TAppSettingDataType; var I: Integer; var B: Boolean; var S: String;
        DI: Integer; DB: Boolean; DS: String);
      procedure Write(Index: TAppSettingIndex; FormatName: String;
        DataType: TAppSettingDataType; I: Integer; B: Boolean; S: String);
    public
      constructor Create;
      destructor Destroy; override;
      function ReadInt(Index: TAppSettingIndex; FormatName: String=''; Default: Integer=0): Integer;
      function ReadBool(Index: TAppSettingIndex; FormatName: String=''; Default: Boolean=False): Boolean;
      function ReadString(Index: TAppSettingIndex; FormatName: String=''; Default: String=''): String; overload;
      function ReadString(ValueName: String): String; overload;
      procedure WriteInt(Index: TAppSettingIndex; Value: Integer; FormatName: String='');
      procedure WriteBool(Index: TAppSettingIndex; Value: Boolean; FormatName: String='');
      procedure WriteString(Index: TAppSettingIndex; Value: String; FormatName: String=''); overload;
      procedure WriteString(ValueName, Value: String); overload;
      function GetDefaultInt(Index: TAppSettingIndex): Integer;
      function GetDefaultString(Index: TAppSettingIndex): String;
      function GetValueName(Index: TAppSettingIndex): String;
      function GetValueNames: TStringList;
      function GetKeyNames: TStringList;
      function GetSessionNames(ParentPath: String; var Folders: TStringList): TStringList;
      procedure GetSessionPaths(ParentPath: String; var Sessions: TStringList);
      function DeleteValue(Index: TAppSettingIndex; FormatName: String=''): Boolean; overload;
      function DeleteValue(ValueName: String): Boolean; overload;
      procedure DeleteCurrentKey;
      procedure MoveCurrentKey(TargetPath: String);
      function ValueExists(Index: TAppSettingIndex): Boolean;
      function SessionPathExists(SessionPath: String): Boolean;
      function IsEmptyKey: Boolean;
      procedure ResetPath;
      property SessionPath: String read FSessionPath write SetSessionPath;
      property PortableMode: Boolean read FPortableMode;
      procedure ImportSettings(Filename: String);
      procedure ExportSettings(Filename: String);
  end;


{$I const.inc}

  function implodestr(seperator: String; a: TStrings) :String;
  function Explode(Separator, Text: String) :TStringList;
  procedure ExplodeQuotedList(Text: String; var List: TStringList);
  function RemoveComments(SQL: String): String;
  function sstr(str: String; len: Integer) : String;
  function encrypt(str: String): String;
  function decrypt(str: String): String;
  function htmlentities(str: String): String;
  function BestTableName(Data: TDBQuery): String;
  function EncodeURL(const Src: String): String;
  procedure StreamWrite(S: TStream; Text: String = '');
  function _GetFileSize(Filename: String): Int64;
  function MakeInt( Str: String ) : Int64;
  function MakeFloat( Str: String ): Extended;
  function CleanupNumber(Str: String): String;
  function IsNumeric(Str: String): Boolean;
  function esc(Text: String; ProcessJokerChars: Boolean=false; DoQuote: Boolean=True): String;
  function ScanNulChar(Text: String): Boolean;
  function ScanLineBreaks(Text: String): TLineBreaks;
  function RemoveNulChars(Text: String): String;
  function fixNewlines(txt: String): String;
  function GetShellFolder(CSIDL: integer): string;
  // Common directories
  function DirnameCommonAppData: String;
  function DirnameUserAppData: String;
  function DirnameSnippets: String;
  function goodfilename( str: String ): String;
  function FormatNumber( str: String; Thousands: Boolean=True): String; Overload;
  function UnformatNumber(Val: String): String;
  function FormatNumber( int: Int64; Thousands: Boolean=True): String; Overload;
  function FormatNumber( flt: Double; decimals: Integer = 0; Thousands: Boolean=True): String; Overload;
  procedure setLocales;
  procedure ShellExec(cmd: String; path: String=''; params: String='');
  function getFirstWord( text: String ): String;
  function FormatByteNumber( Bytes: Int64; Decimals: Byte = 1 ): String; Overload;
  function FormatByteNumber( Bytes: String; Decimals: Byte = 1 ): String; Overload;
  function FormatTimeNumber(Seconds: Cardinal; DisplaySeconds: Boolean): String;
  function GetTempDir: String;
  procedure SetWindowSizeGrip(hWnd: HWND; Enable: boolean);
  procedure SaveUnicodeFile(Filename: String; Text: String);
  procedure OpenTextFile(const Filename: String; out Stream: TFileStream; var Encoding: TEncoding);
  function DetectEncoding(Stream: TStream): TEncoding;
  function ReadTextfileChunk(Stream: TFileStream; Encoding: TEncoding; ChunkSize: Int64 = 0): String;
  function ReadTextfile(Filename: String; Encoding: TEncoding): String;
  function ReadBinaryFile(Filename: String; MaxBytes: Int64): AnsiString;
  procedure StreamToClipboard(Text, HTML: TStream; CreateHTMLHeader: Boolean);
  function WideHexToBin(text: String): AnsiString;
  function BinToWideHex(bin: AnsiString): String;
  procedure FixVT(VT: TVirtualStringTree; MultiLineCount: Word=1);
  function GetTextHeight(Font: TFont): Integer;
  function ColorAdjustBrightness(Col: TColor; Shift: SmallInt): TColor;
  function ComposeOrderClause(Cols: TOrderColArray): String;
  procedure DeInitializeVTNodes(Sender: TBaseVirtualTree);
  function ListIndexByRegExpr(List: TStrings; Expression: String): Integer;
  function FindNode(VT: TVirtualStringTree; idx: Cardinal; ParentNode: PVirtualNode): PVirtualNode;
  procedure SelectNode(VT: TVirtualStringTree; idx: Cardinal; ParentNode: PVirtualNode=nil); overload;
  procedure SelectNode(VT: TVirtualStringTree; Node: PVirtualNode); overload;
  function GetVTSelection(VT: TVirtualStringTree): TStringList;
  procedure SetVTSelection(VT: TVirtualStringTree; Captions: TStringList);
  function GetNextNode(Tree: TVirtualStringTree; CurrentNode: PVirtualNode; Selected: Boolean=False): PVirtualNode;
  function DateBackFriendlyCaption(d: TDateTime): String;
  procedure InheritFont(AFont: TFont);
  function GetLightness(AColor: TColor): Byte;
  function ReformatSQL(SQL: String): String;
  function ParamBlobToStr(lpData: Pointer): TStringlist;
  function ParamStrToBlob(out cbData: DWORD): Pointer;
  function CheckForSecondInstance: Boolean;
  function GetParentFormOrFrame(Comp: TWinControl): TWinControl;
  function GetIndexIcon(IndexType: String): Integer;
  function KeyPressed(Code: Integer): Boolean;
  function GeneratePassword(Len: Integer): String;
  procedure InvalidateVT(VT: TVirtualStringTree; RefreshTag: Integer; ImmediateRepaint: Boolean);
  function CharAtPos(Str: String; Pos: Integer): Char;
  function CompareAnyNode(Text1, Text2: String): Integer;
  function StringListCompareAnythingAsc(List: TStringList; Index1, Index2: Integer): Integer;
  function StringListCompareAnythingDesc(List: TStringList; Index1, Index2: Integer): Integer;
  function GetColumnDefaultType(var Text: String): TColumnDefaultType;
  function GetColumnDefaultClause(DefaultType: TColumnDefaultType; Text: String): String;
  function GetImageLinkTimeStamp(const FileName: string): TDateTime;
  function IsEmpty(Str: String): Boolean;
  function IsNotEmpty(Str: String): Boolean;
  function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer; overload;
  function MessageDialog(const Title, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer; overload;
  function ErrorDialog(Msg: string): Integer; overload;
  function ErrorDialog(const Title, Msg: string): Integer; overload;
  function GetHTMLCharsetByEncoding(Encoding: TEncoding): String;

var
  AppSettings: TAppSettings;
  MutexHandle: THandle = 0;
  DecimalSeparatorSystemdefault: Char;


implementation

uses main;



function WideHexToBin(text: String): AnsiString;
var
  buf: AnsiString;
begin
  buf := AnsiString(text);
  SetLength(Result, Length(text) div 2);
  HexToBin(PAnsiChar(buf), @Result[1], Length(Result));
end;

function BinToWideHex(bin: AnsiString): String;
var
  buf: AnsiString;
begin
  SetLength(buf, Length(bin) * 2);
  BinToHex(@bin[1], PAnsiChar(buf), Length(bin));
  Result := String(buf);
end;


{***
  Convert a TStringList to a string using a separator-string

  @todo Look at each caller to see if escaping is necessary.
  @param string Separator
  @param a TStringList Containing strings
  @return string
}
function implodestr(seperator: String; a: TStrings) :String;
var
  i : Integer;
begin
  Result := '';
  for i:=0 to a.Count-1 do
  begin
    Result := Result + a[i];
    if i < a.Count-1 then
      Result := Result + seperator;
  end;
end;



function Explode(Separator, Text: String): TStringList;
var
  i: Integer;
  Item: String;
begin
  // Explode a string by separator into a TStringList
  Result := TStringList.Create;
  while true do begin
    i := Pos(Separator, Text);
    if i = 0 then begin
      // Last or only segment: Add to list if it's the last. Add also if it's not empty and list is empty.
      // Do not add if list is empty and text is also empty.
      if (Result.Count > 0) or (Text <> '') then
        Result.Add(Text);
      break;
    end;
    Item := Trim(Copy(Text, 1, i-1));
    Result.Add(Item);
    Delete(Text, 1, i-1+Length(Separator));
  end;
end;


function RemoveComments(SQL: String): String;
begin
  // Remove all kinds of comments from given SQL string
end;


{***
  Shorten string to length len and append 3 dots

  @param string String to shorten
  @param integer Wished Length of string
  @return string
}
function sstr(str: String; len: Integer) : String;
begin
  if length(str) > len then
  begin
    str := copy(str, 0, len-1);
    str := str + '…';
  end;
  result := str;
end;



{***
  Password-encryption, used to store session-passwords in registry

  @param string Text to encrypt
  @return string Encrypted Text
}
function encrypt(str: String) : String;
var
  i, salt, nr : integer;
  h : String;
begin
  randomize();
  result := '';
  salt := random(9) + 1;
  for i:=1 to length(str) do begin
    nr := ord(str[i])+salt;
    if nr > 255 then
      nr := nr - 255;
    h := inttohex(nr,0);
    if length(h) = 1 then
      h := '0' + h;
    result := result + h;
  end;
  result := result + inttostr(salt);
end;



{***
  Password-decryption, used to restore session-passwords from registry

  @param string Text to decrypt
  @return string Decrypted Text
}
function decrypt(str: String) : String;
var
  j, salt, nr : integer;
begin
  result := '';
  if str = '' then exit;
  j := 1;
  salt := StrToIntDef(str[length(str)],0);
  result := '';
  while j < length(str)-1 do begin
    nr := StrToInt('$' + str[j] + str[j+1]) - salt;
    if nr < 0 then
      nr := nr + 255;
    result := result + chr(nr);
    inc(j, 2);
  end;
end;



{***
  Convert HTML-characters to their corresponding entities

  @param string Text used for search+replace
  @return string Text with entities
}
function htmlentities(str: String) : String;
begin
  result := StringReplace(str, '&', '&amp;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := StringReplace(result, '>', '&gt;', [rfReplaceAll]);
end;


function BestTableName(Data: TDBQuery): String;
begin
  // Get table name from result if possible. Used by GridToXYZ() functions.
  try
    Result := Data.TableName;
  except
    Result := 'UnknownTable';
  end;
end;


{***
  Encode critical characters in URL segments
  @param string URL to encode
  @return string
}
function EncodeURL(const Src: String): String;
var
  i: Integer;
const
  SafeChars = ['A'..'Z','a'..'z','*','@','.','_','-','0'..'9','$','!','''','(',')'];
begin
  Result := '';
  for i:=1 to Length(Src) do begin
    if CharInSet(Src[i], SafeChars) then
      Result := Result + Src[i]
    else
      Result := Result + '%' + IntToHex(Ord(Src[i]), 2);
  end;
end;


{**
  Write some UTF8 text to a file- or memorystream
}
procedure StreamWrite(S: TStream; Text: String = '');
var
  utf8: AnsiString;
begin
  utf8 := Utf8Encode(Text);
  S.Write(utf8[1], Length(utf8));
end;


{***
  Return filesize of a given file
  @param string Filename
  @return int64 Size in bytes
}
function _GetFileSize(Filename: String): Int64;
var
  Attr: _WIN32_FILE_ATTRIBUTE_DATA;
begin
  if FileExists(Filename) then begin
    GetFileAttributesEx(PChar(Filename), GetFileExInfoStandard, @Attr);
    Result := Int64(Attr.nFileSizeHigh) shl 32 + Int64(Attr.nFileSizeLow);
  end else
    Result := -1;
end;


{***
  Convert a string-number to an integer-number

  @param string String-number
  @return int64
}
function MakeInt(Str: String): Int64;
begin
  // Result has to be of integer type
  try
    Result := Trunc(MakeFloat(Str));
  except
    on E:EInvalidOp do
      Result := 0;
  end;
end;


function CleanupNumber(Str: String): String;
var
  i: Integer;
  HasDecimalSep: Boolean;
begin
  // Ensure the passed string contains a valid number, which is convertable by StrToFloat afterwards
  // Return it as string again, as there are callers which need to handle unsigned bigint's somehow -
  // there is no unsigned 64 bit integer type in Delphi.
  Result := '';

  // Unformatted float coming in? Detect by order of thousand and decimal char
  if ((Pos(',', Str) > 0) and (Pos(',', Str) < Pos('.', Str)))
    or ((Pos('.', Str) > 0) and (Pos('.', ReverseString(Str)) <> 4))
    then begin
    Str := StringReplace(Str, '.', '*', [rfReplaceAll]);
    Str := StringReplace(Str, ',', FormatSettings.ThousandSeparator, [rfReplaceAll]);
    Str := StringReplace(Str, '*', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  end;

  HasDecimalSep := False;
  for i:=1 to Length(Str) do begin
    if CharInSet(Str[i], ['0'..'9', FormatSettings.DecimalSeparator]) or ((Str[i] = '-') and (Result='')) then
    begin
      // Avoid confusion and AV in StrToFloat()
      if (FormatSettings.ThousandSeparator = FormatSettings.DecimalSeparator) and (Str[i] = FormatSettings.DecimalSeparator) then
        continue;
      // Ensure only 1 decimalseparator is left
      if (Str[i] = FormatSettings.DecimalSeparator) and HasDecimalSep then
        continue;
      if Str[i] = FormatSettings.DecimalSeparator then
        HasDecimalSep := True;
      Result := Result + Str[i];
    end;
  end;
  if (Result = '') or (Result = '-') then
    Result := '0';
end;


function IsNumeric(Str: String): Boolean;
begin
  Result := IntToStr(MakeInt(Str)) = Str;
end;


{***
  Convert a string-number to an floatingpoint-number

  @param String text representation of a number
  @return Extended
}
function MakeFloat( Str: String ): Extended;
var
  p_kb, p_mb, p_gb, p_tb, p_pb : Integer;
begin
  // Convert result to a floating point value to ensure
  // we don't discard decimal digits for the next step
  Result := StrToFloat(CleanupNumber(Str));

  // Detect if the string was previously formatted by FormatByteNumber
  // and convert it back by multiplying it with its byte unit
  p_kb := Pos(NAME_KB, Str);
  p_mb := Pos(NAME_MB, Str);
  p_gb := Pos(NAME_GB, Str);
  p_tb := Pos(NAME_TB, Str);
  p_pb := Pos(NAME_PB, Str);

  if (p_kb > 0) and (p_kb = Length(Str)-Length(NAME_KB)+1) then
    Result := Result * SIZE_KB
  else if (p_mb > 0) and (p_mb = Length(Str)-Length(NAME_MB)+1) then
    Result := Result * SIZE_MB
  else if (p_gb > 0) and (p_gb = Length(Str)-Length(NAME_GB)+1) then
    Result := Result * SIZE_GB
  else if (p_tb > 0) and (p_tb = Length(Str)-Length(NAME_TB)+1) then
    Result := Result * SIZE_TB
  else if (p_pb > 0) and (p_pb = Length(Str)-Length(NAME_PB)+1) then
    Result := Result * SIZE_PB;
end;


function esc(Text: String; ProcessJokerChars: Boolean=false; DoQuote: Boolean=True): String;
begin
  Result := MainForm.ActiveConnection.EscapeString(Text, ProcessJokerChars, DoQuote);
end;


{***
  Detect NUL character in a text.
  Useful because fx SynEdit cuts of all text after it encounters a NUL.
}
function ScanNulChar(Text: String): boolean;
var
  i: integer;
begin
  result := false;
  for i:=1 to length(Text) do
  begin
    if Text[i] = #0 then
    begin
      result := true;
      exit;
    end;
  end;
end;



{***
  SynEdit removes all newlines and semi-randomly decides a
  new newline format to use for any text edited.
  See also: Delphi's incomplete implementation of TTextLineBreakStyle in System.pas

  @param string Text to test
  @return TLineBreaks
}
function ScanLineBreaks(Text: String): TLineBreaks;
var
  i: integer;
  c: Char;
  procedure SetResult(Style: TLineBreaks);
  begin
    // Note: Prefer "(foo <> a) and (foo <> b)" over "not (foo in [a, b])" in excessive loops
    // for performance reasons - there is or was a Delphi bug leaving those inline SETs in memory
    // after usage. Unfortunately can't remember which bug id it was and if it still exists.
    if (Result <> lbsNone) and (Result <> Style) then
      Result := lbsMixed
    else
      Result := Style;
  end;
begin
  Result := lbsNone;
  if length(Text) = 0 then exit;
  i := 1;
  repeat
    c := Text[i];
    if c = #13 then begin
      if (i < length(Text)) and (Text[i+1] = #10) then begin
        Inc(i);
        SetResult(lbsWindows);
      end else
        SetResult(lbsMac);
    end else if c = LB_UNIX then
      SetResult(lbsUnix)
    else if c = LB_WIDE then
      SetResult(lbsWide);
    i := i + 1;
    // No need to do more checks after detecting mixed style
    if Result = lbsMixed then
      break;
  until i > length(Text);
end;



{***
  Mangle input text so that SynEdit can load it.

  @param string Text to test
  @return Boolean
}
function RemoveNulChars(Text: String): String;
var
  i: integer;
  c: Char;
begin
  SetLength(Result, Length(Text));
  if Length(Text) = 0 then Exit;
  i := 1;
  repeat
    c := Text[i];
    if c = #0 then Result[i] := #32
    else Result[i] := c;
    i := i + 1;
  until i > length(Text);
end;



{***
  Unify CR's and LF's to CRLF

  @param string Text to fix
  @return string
}
function fixNewlines(txt: String): String;
begin
  txt := StringReplace(txt, CRLF, #10, [rfReplaceAll]);
  txt := StringReplace(txt, #13, #10, [rfReplaceAll]);
  txt := StringReplace(txt, #10, CRLF, [rfReplaceAll]);
  result := txt;
end;



{***
  Get the path of a Windows(r)-shellfolder, specified by an integer or a constant

  @param integer Number or constant
  @return string Path
}
function GetShellFolder(CSIDL: integer): string;
var
  pidl                   : PItemIdList;
  FolderPath             : string;
  SystemFolder           : Integer;
  Malloc                 : IMalloc;
begin
  Malloc := nil;
  FolderPath := '';
  SHGetMalloc(Malloc);
  if Malloc = nil then
  begin
    Result := FolderPath;
    Exit;
  end;
  try
    SystemFolder := CSIDL;
    if SUCCEEDED(SHGetSpecialFolderLocation(0, SystemFolder, pidl)) then
    begin
      SetLength(FolderPath, max_path);
      if SHGetPathFromIDList(pidl, PChar(FolderPath)) then
      begin
        SetLength(FolderPath, length(PChar(FolderPath)));
      end;
    end;
    Result := FolderPath;
  finally
    Malloc.Free(pidl);
  end;
end;


function DirnameCommonAppData: String;
begin
  // "All users" folder for HeidiSQL's data (All Users\Application Data)
  Result := GetShellFolder(CSIDL_COMMON_APPDATA) + '\' + APPNAME + '\';
end;


function DirnameUserAppData: String;
begin
  // User folder for HeidiSQL's data (<user name>\Application Data)
  Result := GetShellFolder(CSIDL_APPDATA) + '\' + APPNAME + '\';
end;


function DirnameSnippets: String;
begin
  // Folder for snippets
  Result := DirnameCommonAppData + 'Snippets\'
end;


{***
  Remove special characters from a filename

  @param string Filename
  @return string
}
function goodfilename( str: String ): String;
var
  c : Char;
begin
  result := str;
  for c in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] do
    result := StringReplace( result, c, '_', [rfReplaceAll] );
end;



{**
  Unformat a formatted integer or float. Used for CSV export and composing WHERE clauses for grid editing.
}
function UnformatNumber(Val: String): String;
var
  i: Integer;
  HasDecim: Boolean;
  c: Char;
const
  Numbers = ['0'..'9'];
begin
  Result := '';
  HasDecim := False;
  for i:=1 to Length(Val) do begin
    c := Val[i];
    if CharInSet(c, Numbers) or ((c = '-') and (i = 1)) then
      Result := Result + c
    else if (c = FormatSettings.DecimalSeparator) and (not HasDecim) then begin
      Result := Result + '.';
      HasDecim := True;
    end else if c <> FormatSettings.ThousandSeparator then
      break;
  end;
  if Result = '' then
    Result := '0';
end;


{***
  Return a formatted integer or float from a string
  @param string Text containing a number
  @return string
}
function FormatNumber(str: String; Thousands: Boolean=True): String; Overload;
var
  i, p, Left: Integer;
begin
  Result := StringReplace(str, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  if Thousands then begin
    // Do not add thousand separators to zerofilled numbers
    if ((Length(Result) >= 1) and (Result[1] = '0'))
      or ((Length(Result) >= 2) and (Result[1] = '-') and (Result[2] = '0'))
    then
      Exit;
    p := Pos(FormatSettings.DecimalSeparator, Result);
    if p = 0 then p := Length(Result)+1;
    Left := 2;
    if (Length(Result) >= 1) and (Result[1] = '-') then
      Left := 3;
    if p > 0 then for i:=p-1 downto Left do begin
      if (p-i) mod 3 = 0 then
        Insert(FormatSettings.ThousandSeparator, Result, i);
    end;
  end;
end;



{***
  Return a formatted number from an integer

  @param int64 Number to format
  @return string
}
function FormatNumber(int: Int64; Thousands: Boolean=True): String; Overload;
begin
  result := FormatNumber(IntToStr(int), Thousands);
end;



{***
  Return a formatted number from a float
  This function is called by two overloaded functions

  @param double Number to format
  @param integer Number of decimals
  @return string
}
function FormatNumber(flt: Double; decimals: Integer = 0; Thousands: Boolean=True): String; Overload;
begin
  Result := Format('%10.'+IntToStr(decimals)+'f', [flt]);
  Result := Trim(Result);
  Result := FormatNumber(Result, Thousands);
end;



{***
  Set global variables containing the standard local format for date and time
  values. Standard means the MySQL-standard format, which is YYYY-MM-DD HH:MM:SS

  @note Be aware that Delphi internally converts the slashes in ShortDateFormat
        to the DateSeparator
}
procedure setLocales;
begin
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  FormatSettings.LongTimeFormat := 'hh:nn:ss';
  if DecimalSeparatorSystemdefault = '' then
    DecimalSeparatorSystemdefault := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := DecimalSeparatorSystemdefault;
end;



{***
  Open URL or execute system command

  @param string Command or URL to execute
  @param string Working directory, only usefull is first param is a system command
}
procedure ShellExec(cmd: String; path: String=''; params: String='');
begin
  ShellExecute(0, 'open', PChar(cmd), PChar(params), PChar(path), SW_SHOWNORMAL);
end;



{***
  Returns first word of a given text
  @param string Given text
  @return string First word-boundary
}
function getFirstWord( text: String ): String;
var
  i : Integer;
  wordChars, wordCharsFirst : TSysCharSet;
begin
  result := '';
  text := trim( text );
  // First char in word must not be numerical. Fixes queries like
  // /*!40000 SHOW ENGINES */ to be recognized as "result"-queries
  // while not breaking getFirstWord in situations where the second
  // or later char can be a number (fx the collation in createdatabase).
  wordChars := ['a'..'z', 'A'..'Z', '0'..'9', '_', '-'];
  wordCharsFirst := wordChars - ['0'..'9'];
  i := 1;

  // Find beginning of the first word, ignoring non-alphanumeric chars at the very start
  // @see bug #1692828
  while i < Length(text) do
  begin
    if CharInSet(text[i], wordCharsFirst) then
    begin
      // Found beginning of word!
      break;
    end;
    if i = Length(text)-1 then
    begin
      // Give up in the very last loop, reset counter
      // and break. We can't find the start of a word
      i := 1;
      break;
    end;
    inc(i);
  end;

  // Add chars as long as they're alpha-numeric
  while i <= Length(text) do
  begin
    if ((result = '') and CharInSet(text[i], wordCharsFirst)) or CharInSet(text[i], wordChars) then
    begin
      result := result + text[i];
    end
    else
    begin
      // Stop here because we found a non-alphanumeric char.
      // This applies to all different whitespaces, brackets, commas etc.
      break;
    end;
    inc(i);
  end;
end;


{**
  Format a filesize to automatically use the best fitting expression
  16 100 000 Bytes -> 16,1 MB
  4 500 Bytes -> 4,5 KB
  @param Int64 Number of Bytes
  @param Byte Decimals to display when bytes is bigger than 1M
}
function FormatByteNumber( Bytes: Int64; Decimals: Byte = 1 ): String; Overload;
begin
  if Bytes >= SIZE_PB then
    Result := FormatNumber( Bytes / SIZE_PB, Decimals ) + NAME_PB
  else if Bytes >= SIZE_TB then
    Result := FormatNumber( Bytes / SIZE_TB, Decimals ) + NAME_TB
  else if Bytes >= SIZE_GB then
    Result := FormatNumber( Bytes / SIZE_GB, Decimals ) + NAME_GB
  else if Bytes >= SIZE_MB then
    Result := FormatNumber( Bytes / SIZE_MB, Decimals ) + NAME_MB
  else if Bytes >= SIZE_KB then
    Result := FormatNumber( Bytes / SIZE_KB, Decimals ) + NAME_KB
  else
    Result := FormatNumber( Bytes ) + NAME_BYTES
end;


{**
  An overloaded function of the previous one which can
  take a string as input
}
function FormatByteNumber( Bytes: String; Decimals: Byte = 1 ): String; Overload;
begin
  Result := FormatByteNumber( MakeInt(Bytes), Decimals );
end;


{**
  Format a number of seconds to a human readable time format
  @param Cardinal Number of seconds
  @result String 12:34:56
}
function FormatTimeNumber(Seconds: Cardinal; DisplaySeconds: Boolean): String;
var
  d, h, m, s : Integer;
begin
  s := Seconds;
  d := s div (60*60*24);
  s := s mod (60*60*24);
  h := s div (60*60);
  s := s mod (60*60);
  m := s div 60;
  s := s mod 60;
  if d > 0 then begin
    if DisplaySeconds then
      Result := Format('%d days, %.2d:%.2d:%.2d', [d, h, m, s])
    else
      Result := Format('%d days, %.2d:%.2d h', [d, h, m]);
  end else begin
    if DisplaySeconds then
      Result := Format('%.2d:%.2d:%.2d', [h, m, s])
    else
      Result := Format('%.2d:%.2d h', [h, m])
  end;
end;


function GetTempDir: String;
var
  TempPath: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, PChar(@TempPath));
  Result := StrPas(TempPath);
end;


{
  Code taken from SizeGripHWND.pas:
  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  Alle Rechte vorbehalten.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
}
function SizeGripWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Info: PGripInfo;
  dc: HDC;
  pt: TPoint;

  // Invalidate the current grip rectangle
  procedure InvalidateGrip;
  begin
    with Info^ do
      if (GripRect.Right > GripRect.Left) and
         (GripRect.Bottom > GripRect.Top) then
        InvalidateRect(hWnd, @GripRect, true);
  end;

  // Update (and invalidate) the current grip rectangle
  procedure UpdateGrip;
  begin
    with Info^ do
    begin
      GetClientRect(hWnd, GripRect);
      GripRect.Left := GripRect.Right - GetSystemMetrics(SM_CXHSCROLL);
      GripRect.Top := GripRect.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    end;

    InvalidateGrip;
  end;

  function CallOld: LRESULT;
  begin
    Result := CallWindowProc(@Info^.OldWndProc, hWnd, Msg, wParam, lParam);
  end;

begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if Info = nil then
    Result := DefWindowProc(hWnd, Msg, wParam, lParam)
  else if not Info^.Enabled then
    Result := CallOld
  else
  begin
    case Msg of
      WM_NCDESTROY: begin
        Result := CallOld;

        SetWindowLong(hWnd, GWL_WNDPROC, LongInt(@Info^.OldWndProc));
        RemoveProp(hWnd, SizeGripProp);
        Dispose(Info);
      end;

      WM_PAINT: begin
        Result := CallOld;
        if wParam = 0 then
        begin
          dc := GetDC(hWnd);
          DrawFrameControl(dc, Info^.GripRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
          ReleaseDC(hWnd, dc);
        end;
      end;

      WM_NCHITTEST: begin
        pt.x := TSmallPoint(lParam).x;
        pt.y := TSmallPoint(lParam).y;
        ScreenToClient(hWnd, pt);
        if PtInRect(Info^.GripRect, pt) then
          Result := HTBOTTOMRIGHT
        else
          Result := CallOld;
      end;

      WM_SIZE: begin
        InvalidateGrip;
        Result := CallOld;
        UpdateGrip;
      end;

      else
        Result := CallOld;
    end;
  end;
end;

{ Note that SetWindowSizeGrip(..., false) does not really remove the hook -
  it just sets "Enabled" to false. The hook plus all data is removed when
  the window is destroyed.
}
procedure SetWindowSizeGrip(hWnd: HWND; Enable: boolean);
var
  Info: PGripInfo;
begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if (Info = nil) and Enable then
  begin
    New(Info);
    FillChar(Info^, SizeOf(TGripInfo), 0);

    with Info^ do
    begin
      Info^.OldWndProc := TWndProc(Pointer(GetWindowLong(hWnd, GWL_WNDPROC)));

      GetClientRect(hWnd, GripRect);
      GripRect.Left := GripRect.Right - GetSystemMetrics(SM_CXHSCROLL);
      GripRect.Top := GripRect.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    end;

    SetProp(hWnd, SizeGripProp, Cardinal(Info));
    SetWindowLong(hWnd, GWL_WNDPROC, LongInt(@SizeGripWndProc));
  end;

  if (Info <> nil) then
    if Enable <> Info^.Enabled then
      with Info^ do
      begin
        Enabled := Enable;
        if (GripRect.Right > GripRect.Left) and
           (GripRect.Bottom > GripRect.Top) then
          InvalidateRect(hWnd, @GripRect, true);
      end;
end;


{**
  Save a textfile with unicode
}
procedure SaveUnicodeFile(Filename: String; Text: String);
var
  f: TFileStream;
begin
  f := TFileStream.Create(Filename, fmCreate or fmOpenWrite);
  StreamWrite(f, Text);
  f.Free;
end;


{**
  Open a textfile unicode safe and return a stream + its charset
}
procedure OpenTextFile(const Filename: String; out Stream: TFileStream; var Encoding: TEncoding);
var
  Header: TBytes;
  BomLen: Integer;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  if Encoding = nil then
    Encoding := DetectEncoding(Stream);
  // If the file contains a BOM, advance the stream's position
  BomLen := 0;
  if Length(Encoding.GetPreamble) > 0 then begin
    SetLength(Header, Length(Encoding.GetPreamble));
    Stream.ReadBuffer(Pointer(Header)^, Length(Header));
    if CompareMem(Header, Encoding.GetPreamble, SizeOf(Header)) then
      BomLen := Length(Encoding.GetPreamble);
  end;
  Stream.Position := BomLen;
end;


{**
  Detect stream's content encoding by examing first 100k bytes (MaxBufferSize). Result can be:
    UTF-16 BE with BOM
    UTF-16 LE with BOM
    UTF-8 with or without BOM
    ANSI
  Aimed to work better than WideStrUtils.IsUTF8String() which didn't work in any test case here.
  @see http://en.wikipedia.org/wiki/Byte_Order_Mark
}
function DetectEncoding(Stream: TStream): TEncoding;
var
  ByteOrderMark: Char;
  BytesRead: Integer;
  Utf8Test: array[0..2] of AnsiChar;
  Buffer: array of Byte;
  BufferSize, i, FoundUTF8Strings: Integer;
const
  UNICODE_BOM = Char($FEFF);
  UNICODE_BOM_SWAPPED = Char($FFFE);
  UTF8_BOM = AnsiString(#$EF#$BB#$BF);
  MinimumCountOfUTF8Strings = 1;
  MaxBufferSize = 100000;

  // 3 trailing bytes are the maximum in valid UTF-8 streams,
  // so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    inc(i);
    while (i < BufferSize) and (Result < 4) do begin
      if Buffer[i] in [$80..$BF] then
        inc(Result)
      else
        Break;
      inc(i);
    end;
  end;

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
    Result := TEncoding.Unicode
  else if ByteOrderMark = UNICODE_BOM_SWAPPED then
    Result := TEncoding.BigEndianUnicode
  else if Utf8Test = UTF8_BOM then
    Result := TEncoding.UTF8
  else begin
    { @note Taken from SynUnicode.pas }
    { If no BOM was found, check for leading/trailing byte sequences,
      which are uncommon in usual non UTF-8 encoded text.

      NOTE: There is no 100% save way to detect UTF-8 streams. The bigger
            MinimumCountOfUTF8Strings, the lower is the probability of
            a false positive. On the other hand, a big MinimumCountOfUTF8Strings
            makes it unlikely to detect files with only little usage of non
            US-ASCII chars, like usual in European languages. }

    // if no special characteristics are found it is not UTF-8
    Result := TEncoding.Default;

    // start analysis at actual Stream.Position
    BufferSize := Min(MaxBufferSize, Stream.Size - Stream.Position);

    if BufferSize > 0 then begin
      SetLength(Buffer, BufferSize);
      Stream.ReadBuffer(Buffer[0], BufferSize);
      Stream.Seek(-BufferSize, soFromCurrent);

      FoundUTF8Strings := 0;
      i := 0;
      while i < BufferSize do begin
        if FoundUTF8Strings = MinimumCountOfUTF8Strings then begin
          Result := TEncoding.UTF8;
          Break;
        end;
        case Buffer[i] of
          $00..$7F: // skip US-ASCII characters as they could belong to various charsets
            ;
          $C2..$DF:
            if CountOfTrailingBytes = 1 then
              inc(FoundUTF8Strings)
            else
              Break;
          $E0:
            begin
              inc(i);
              if (i < BufferSize) and (Buffer[i] in [$A0..$BF]) and (CountOfTrailingBytes = 1) then
                inc(FoundUTF8Strings)
              else
                Break;
            end;
          $E1..$EC, $EE..$EF:
            if CountOfTrailingBytes = 2 then
              inc(FoundUTF8Strings)
            else
              Break;
          $ED:
            begin
              inc(i);
              if (i < BufferSize) and (Buffer[i] in [$80..$9F]) and (CountOfTrailingBytes = 1) then
                inc(FoundUTF8Strings)
              else
                Break;
            end;
          $F0:
            begin
              inc(i);
              if (i < BufferSize) and (Buffer[i] in [$90..$BF]) and (CountOfTrailingBytes = 2) then
                inc(FoundUTF8Strings)
              else
                Break;
            end;
          $F1..$F3:
            if CountOfTrailingBytes = 3 then
              inc(FoundUTF8Strings)
            else
              Break;
          $F4:
            begin
              inc(i);
              if (i < BufferSize) and (Buffer[i] in [$80..$8F]) and (CountOfTrailingBytes = 2) then
                inc(FoundUTF8Strings)
              else
                Break;
            end;
          $C0, $C1, $F5..$FF: // invalid UTF-8 bytes
            Break;
          $80..$BF: // trailing bytes are consumed when handling leading bytes,
                     // any occurence of "orphaned" trailing bytes is invalid UTF-8
            Break;
        end;
        inc(i);
      end;
    end;
  end;
end;


{**
  Read a chunk out of a textfile unicode safe by passing a stream and its charset
}
function ReadTextfileChunk(Stream: TFileStream; Encoding: TEncoding; ChunkSize: Int64 = 0): String;
var
  DataLeft: Int64;
  LBuffer: TBytes;
begin
  DataLeft := Stream.Size - Stream.Position;
  if (ChunkSize = 0) or (ChunkSize > DataLeft) then
    ChunkSize := DataLeft;
  SetLength(LBuffer, ChunkSize);
  Stream.ReadBuffer(Pointer(LBuffer)^, ChunkSize);
  LBuffer := Encoding.Convert(Encoding, TEncoding.Unicode, LBuffer, 0, Length(LBuffer));
  Result := TEncoding.Unicode.GetString(LBuffer);
end;

{**
  Read a unicode or ansi file into memory
}
function ReadTextfile(Filename: String; Encoding: TEncoding): String;
var
  Stream: TFileStream;
begin
  OpenTextfile(Filename, Stream, Encoding);
  Result := ReadTextfileChunk(Stream, Encoding);
  Stream.Free;
end;

function ReadBinaryFile(Filename: String; MaxBytes: Int64): AnsiString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  Stream.Position := 0;
  if (MaxBytes < 1) or (MaxBytes > Stream.Size) then MaxBytes := Stream.Size;
  SetLength(Result, MaxBytes);
  Stream.Read(PAnsiChar(Result)^, Length(Result));
  Stream.Free;
end;


procedure StreamToClipboard(Text, HTML: TStream; CreateHTMLHeader: Boolean);
var
  TextContent, HTMLContent: AnsiString;
  GlobalMem: HGLOBAL;
  lp: PChar;
  ClpLen: Integer;
  CF_HTML: Word;
begin
  // Copy unicode text to clipboard
  if Assigned(Text) then begin
    SetLength(TextContent, Text.Size);
    Text.Position := 0;
    Text.Read(PAnsiChar(TextContent)^, Text.Size);
    Clipboard.AsText := Utf8ToString(TextContent);
    SetString(TextContent, nil, 0);
  end;

  if Assigned(HTML) then begin
    // If wanted, add a HTML portion, so formatted text can be pasted in WYSIWYG
    // editors (mostly MS applications).
    // Note that the content is UTF8 encoded ANSI. Using unicode variables results in raw
    // text pasted in editors. TODO: Find out why and optimize redundant code away by a loop.
    OpenClipBoard(0);
    CF_HTML := RegisterClipboardFormat('HTML Format');
    SetLength(HTMLContent, HTML.Size);
    HTML.Position := 0;
    HTML.Read(PAnsiChar(HTMLContent)^, HTML.Size);
    if CreateHTMLHeader then begin
      HTMLContent := 'Version:0.9' + CRLF +
        'StartHTML:000089' + CRLF +
        'EndHTML:°°°°°°' + CRLF +
        'StartFragment:000089' + CRLF +
        'EndFragment:°°°°°°' + CRLF +
        HTMLContent + CRLF;
      HTMLContent := AnsiStrings.StringReplace(
        HTMLContent, '°°°°°°',
        AnsiStrings.Format('%.6d', [Length(HTMLContent)]),
        [rfReplaceAll]);
    end;
    ClpLen := Length(HTMLContent) + 1;
    GlobalMem := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, ClpLen);
    lp := GlobalLock(GlobalMem);
    Move(PAnsiChar(HTMLContent)^, lp[0], ClpLen);
    SetString(HTMLContent, nil, 0);
    GlobalUnlock(GlobalMem);
    SetClipboardData(CF_HTML, GlobalMem);
    CloseClipboard;
  end;
end;


procedure FixVT(VT: TVirtualStringTree; MultiLineCount: Word=1);
var
  SingleLineHeight: Integer;
  Node: PVirtualNode;
begin
  // Resize hardcoded node height to work with different DPI settings
  VT.BeginUpdate;
  SingleLineHeight := GetTextHeight(VT.Font);
  VT.DefaultNodeHeight := SingleLineHeight * MultiLineCount + 5;
  // The header needs slightly more height than the normal nodes
  VT.Header.Height := Trunc(SingleLineHeight * 1.5);
  // Apply new height to multi line grid nodes
  Node := VT.GetFirstInitialized;
  while Assigned(Node) do begin
    VT.NodeHeight[Node] := VT.DefaultNodeHeight;
    VT.MultiLine[Node] := MultiLineCount > 1;
    Node := VT.GetNextInitialized(Node);
  end;
  VT.EndUpdate;
  // Disable hottracking in non-Vista mode, looks ugly in XP, but nice in Vista
  if (toUseExplorerTheme in VT.TreeOptions.PaintOptions) and (Win32MajorVersion >= 6) then
    VT.TreeOptions.PaintOptions := VT.TreeOptions.PaintOptions + [toHotTrack]
  else
    VT.TreeOptions.PaintOptions := VT.TreeOptions.PaintOptions - [toHotTrack];
  VT.OnGetHint := MainForm.AnyGridGetHint;
  VT.OnScroll := MainForm.AnyGridScroll;
  VT.OnMouseWheel := MainForm.AnyGridMouseWheel;
  VT.ShowHint := True;
  VT.HintMode := hmToolTip;
  // Apply case insensitive incremental search event
  if VT.IncrementalSearch <> isNone then
    VT.OnIncrementalSearch := Mainform.AnyGridIncrementalSearch;
  VT.OnStartOperation := Mainform.AnyGridStartOperation;
  VT.OnEndOperation := Mainform.AnyGridEndOperation;
end;


function GetTextHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  // Code taken from StdCtrls.TCustomEdit.AdjustHeight
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;


function ColorAdjustBrightness(Col: TColor; Shift: SmallInt): TColor;
var
  Lightness: Byte;
begin
  // If base color is bright, make bg color darker (grey), and vice versa, so that
  // colors work with high contrast mode for accessibility
  Lightness := GetLightness(Col);
  if (Lightness < 128) and (Shift < 0) then
    Shift := Abs(Shift)
  else if (Lightness > 128) and (Shift > 0) then
    Shift := 0 - Abs(Shift);
  Result := ColorAdjustLuma(Col, Shift, true);
end;


{**
  Concat all sort options to a ORDER clause
}
function ComposeOrderClause(Cols: TOrderColArray): String;
var
  i : Integer;
  sort : String;
begin
  result := '';
  for i := 0 to Length(Cols) - 1 do
  begin
    if result <> '' then
      result := result + ', ';
    if Cols[i].SortDirection = ORDER_ASC then
      sort := TXT_ASC
    else
      sort := TXT_DESC;
    result := result + MainForm.ActiveConnection.QuoteIdent( Cols[i].ColumnName ) + ' ' + sort;
  end;
end;


procedure DeInitializeVTNodes(Sender: TBaseVirtualTree);
var
  Node: PVirtualNode;
begin
  // Forces a VirtualTree to (re-)initialize its nodes.
  // I wonder why this is not implemented in VirtualTree.
  Node := Sender.GetFirstInitialized;
  while Assigned(Node) do begin
    Node.States := Node.States - [vsInitialized];
    Node := Sender.GetNextInitialized(Node);
  end;
end;


function ListIndexByRegExpr(List: TStrings; Expression: String): Integer;
var
  rx: TRegExpr;
  i: Integer;
begin
  // Find item in stringlist by passing a regular expression
  rx := TRegExpr.Create;
  rx.Expression := Expression;
  rx.ModifierI := True;
  Result := -1;
  for i := 0 to List.Count - 1 do begin
    if rx.Exec(List[i]) then begin
      Result := i;
      break;
    end;
  end;
  FreeAndNil(rx);
end;


function FindNode(VT: TVirtualStringTree; idx: Cardinal; ParentNode: PVirtualNode): PVirtualNode;
var
  Node: PVirtualNode;
begin
  // Helper to find a node by its index
  Result := nil;
  if Assigned(ParentNode) then
    Node := VT.GetFirstChild(ParentNode)
  else
    Node := VT.GetFirst;
  while Assigned(Node) do begin
    if Node.Index = idx then begin
      Result := Node;
      break;
    end;
    Node := VT.GetNextSibling(Node);
  end;
end;


procedure SelectNode(VT: TVirtualStringTree; idx: Cardinal; ParentNode: PVirtualNode=nil); overload;
var
  Node: PVirtualNode;
begin
  // Helper to focus and highlight a node by its index
  Node := FindNode(VT, idx, ParentNode);
  if Assigned(Node) then
    SelectNode(VT, Node);
end;


procedure SelectNode(VT: TVirtualStringTree; Node: PVirtualNode); overload;
var
  OldFocus: PVirtualNode;
begin
  if Node = VT.RootNode then
    Node := nil;
  OldFocus := VT.FocusedNode;
  VT.ClearSelection;
  VT.FocusedNode := Node;
  VT.Selected[Node] := True;
  VT.ScrollIntoView(Node, False);
  if (OldFocus = Node) and Assigned(VT.OnFocusChanged) then
    VT.OnFocusChanged(VT, Node, VT.Header.MainColumn);
end;


function GetVTSelection(VT: TVirtualStringTree): TStringList;
var
  Node: PVirtualNode;
  InvalidationTag: Integer;
begin
  // Return captions of selected nodes
  InvalidationTag := vt.Tag;
  vt.Tag := VTREE_LOADED;
  Result := TStringList.Create;
  Node := GetNextNode(VT, nil, true);
  while Assigned(Node) do begin
    Result.Add(VT.Text[Node, VT.Header.MainColumn]);
    Node := GetNextNode(VT, Node, true);
  end;
  vt.Tag := InvalidationTag;
end;


procedure SetVTSelection(VT: TVirtualStringTree; Captions: TStringList);
var
  Node: PVirtualNode;
  idx: Integer;
begin
  // Restore selected nodes based on captions list
  Node := GetNextNode(VT, nil, false);
  while Assigned(Node) do begin
    idx := Captions.IndexOf(VT.Text[Node, VT.Header.MainColumn]);
    if idx > -1 then
      VT.Selected[Node] := True;
    Node := GetNextNode(VT, Node, false);
  end;
end;


function GetNextNode(Tree: TVirtualStringTree; CurrentNode: PVirtualNode; Selected: Boolean=False): PVirtualNode;
begin
  // Get next visible + selected node. Not possible with VTree's own functions.
  Result := CurrentNode;
  while True do begin
    if Selected then begin
      if not Assigned(Result) then
        Result := Tree.GetFirstSelected
      else
        Result := Tree.GetNextSelected(Result);
    end else begin
      if not Assigned(Result) then
        Result := Tree.GetFirst
      else
        Result := Tree.GetNext(Result);
    end;
    if (not Assigned(Result)) or Tree.IsVisible[Result] then
      break;
  end;
end;


function DateBackFriendlyCaption(d: TDateTime): String;
var
  MonthsAgo, DaysAgo, HoursAgo, MinutesAgo: Int64;
begin
  MonthsAgo := MonthsBetween(Now, d);
  DaysAgo := DaysBetween(Now, d);
  HoursAgo := HoursBetween(Now, d);
  MinutesAgo := MinutesBetween(Now, d);
  if MonthsAgo = 1 then Result := FormatNumber(MonthsAgo)+' month ago'
  else if MonthsAgo > 1 then Result := FormatNumber(MonthsAgo)+' months ago'
  else if DaysAgo = 1 then Result := FormatNumber(DaysAgo)+' day ago'
  else if DaysAgo > 1 then Result := FormatNumber(DaysAgo)+' days ago'
  else if HoursAgo = 1 then Result := FormatNumber(HoursAgo)+' hour ago'
  else if HoursAgo > 1 then Result := FormatNumber(HoursAgo)+' hours ago'
  else if MinutesAgo = 1 then Result := FormatNumber(MinutesAgo)+' minute ago'
  else if MinutesAgo > 0 then Result := FormatNumber(MinutesAgo)+' minutes ago'
  else Result := 'less than a minute ago';
end;


procedure ExplodeQuotedList(Text: String; var List: TStringList);
var
  i: Integer;
  Quote: Char;
  Opened, Closed: Boolean;
  Item: String;
begin
  Text := Trim(Text);
  if Length(Text) > 0 then
    Quote := Text[1]
  else
    Quote := '`';
  Opened := False;
  Closed := True;
  Item := '';
  for i:=1 to Length(Text) do begin
    if Text[i] = Quote then begin
      Opened := not Opened;
      Closed := not Closed;
      if Closed then begin
        List.Add(Item);
        Item := '';
      end;
      Continue;
    end;
    if Opened and (not Closed) then
      Item := Item + Text[i];
  end;
end;


procedure InheritFont(AFont: TFont);
begin
  AFont.Name := Mainform.Font.Name;
  AFont.Size := Mainform.Font.Size;
end;


function GetLightness(AColor: TColor): Byte;
var
  R, G, B: Byte;
  MaxValue, MinValue: Double;
  Lightness: Double;
begin
  R := GetRValue(ColorToRGB(AColor));
  G := GetGValue(ColorToRGB(AColor));
  B := GetBValue(ColorToRGB(AColor));
  MaxValue := Max(Max(R,G),B);
  MinValue := Min(Min(R,G),B);
  Lightness := (((MaxValue + MinValue) * 240) + 255 ) / 510;
  Result := Round(Lightness);
end;


function ReformatSQL(SQL: String): String;
var
  AllKeywords, ImportantKeywords, PairKeywords: TStringList;
  i, Run, KeywordMaxLen: Integer;
  IsEsc, IsQuote, InComment, InBigComment, InString, InKeyword, InIdent, LastWasComment: Boolean;
  c, p: Char;
  Keyword, PreviousKeyword, TestPair: String;
  Datatypes: TDBDataTypeArray;
const
  WordChars = ['a'..'z', 'A'..'Z', '0'..'9', '_', '.'];
  WhiteSpaces = [#9, #10, #13, #32];
begin
  // Known SQL keywords, get converted to UPPERCASE
  AllKeywords := TStringList.Create;
  AllKeywords.Text := MySQLKeywords.Text;
  for i:=Low(MySQLFunctions) to High(MySQLFunctions) do begin
    // Leave out operator functions like ">>", and the "X()" function so hex values don't get touched
    if (MySQLFunctions[i].Declaration <> '') and (MySQLFunctions[i].Name <> 'X') then
      AllKeywords.Add(MySQLFunctions[i].Name);
  end;
  Datatypes := Mainform.ActiveConnection.Datatypes;
  for i:=Low(Datatypes) to High(Datatypes) do
    AllKeywords.Add(Datatypes[i].Name);
  KeywordMaxLen := 0;
  for i:=0 to AllKeywords.Count-1 do
    KeywordMaxLen := Max(KeywordMaxLen, Length(AllKeywords[i]));

  // A subset of the above list, each of them will get a linebreak left to it
  ImportantKeywords := Explode(',', 'SELECT,FROM,LEFT,RIGHT,STRAIGHT,NATURAL,INNER,JOIN,WHERE,GROUP,ORDER,HAVING,LIMIT,CREATE,DROP,UPDATE,INSERT,REPLACE,TRUNCATE,DELETE');
  // Keywords which followers should not get separated into a new line
  PairKeywords := Explode(',', 'LEFT,RIGHT,STRAIGHT,NATURAL,INNER,ORDER,GROUP');

  IsEsc := False;
  InComment := False;
  InBigComment := False;
  LastWasComment := False;
  InString := False;
  InIdent := False;
  Run := 1;
  Result := '';
  SQL := SQL + ' ';
  SetLength(Result, Length(SQL)*2);
  Keyword := '';
  PreviousKeyword := '';
  for i:=1 to Length(SQL) do begin
    c := SQL[i]; // Current char
    if i > 1 then p := SQL[i-1] else p := #0; // Previous char

    // Detection logic - where are we?
    if c = '\' then IsEsc := not IsEsc
    else IsEsc := False;
    IsQuote := (c = '''') or (c = '"');
    if c = '`' then InIdent := not InIdent;
    if (not IsEsc) and IsQuote then InString := not InString;
    if (c = '#') or ((c = '-') and (p = '-')) then InComment := True;
    if ((c = #10) or (c = #13)) and InComment then begin
      LastWasComment := True;
      InComment := False;
    end;
    if (c = '*') and (p = '/') and (not InComment) and (not InString) then InBigComment := True;
    if (c = '/') and (p = '*') and (not InComment) and (not InString) then InBigComment := False;
    InKeyword := (not InComment) and (not InBigComment) and (not InString) and (not InIdent) and CharInSet(c, WordChars);

    // Creation of returning text
    if InKeyword then begin
      Keyword := Keyword + c;
    end else begin
      if Keyword <> '' then begin
        if AllKeywords.IndexOf(KeyWord) > -1 then begin
          while (Run > 1) and CharInSet(Result[Run-1], WhiteSpaces) do
            Dec(Run);
          Keyword := UpperCase(Keyword);
          if Run > 1 then begin
            // SELECT, WHERE, JOIN etc. get a new line, but don't separate LEFT JOIN with linebreaks
            if LastWasComment or ((ImportantKeywords.IndexOf(Keyword) > -1) and (PairKeywords.IndexOf(PreviousKeyword) = -1)) then
              Keyword := CRLF + Keyword
            else if (Result[Run-1] <> '(') then
              Keyword := ' ' + Keyword;
          end;
          LastWasComment := False;
        end;
        PreviousKeyword := Trim(Keyword);
        Insert(Keyword, Result, Run);
        Inc(Run, Length(Keyword));
        Keyword := '';
      end;
      if (not InComment) and (not InBigComment) and (not InString) and (not InIdent) then begin
        TestPair := Result[Run-1] + c;
        if (TestPair = '  ') or (TestPair = '( ') then begin
          c := Result[Run-1];
          Dec(Run);
        end;
        if (TestPair = ' )') or (TestPair = ' ,') then
          Dec(Run);
      end;
      Result[Run] := c;
      Inc(Run);
    end;

  end;

  // Cut overlength
  SetLength(Result, Run-2);
end;



{ *** TDBObjectEditor }

constructor TDBObjectEditor.Create(AOwner: TComponent);
begin
  inherited;
  // Do not set alClient via DFM! In conjunction with ExplicitXXX properties that
  // repeatedly breaks the GUI layout when you reload the project
  Align := alClient;
  InheritFont(Font);
  ScaleControls(Screen.PixelsPerInch, FORMS_DPI);
end;

destructor TDBObjectEditor.Destroy;
begin
  inherited;
end;

procedure TDBObjectEditor.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

procedure TDBObjectEditor.Init(Obj: TDBObject);
var
  editName: TWinControl;
  SynMemo: TSynMemo;
  popup: TPopupMenu;
  Item: TMenuItem;
begin
  Mainform.ShowStatusMsg('Initializing editor ...');
  Mainform.LogSQL(Self.ClassName+'.Init, using object "'+Obj.Name+'"', lcDebug);
  DBObject := TDBObject.Create(Obj.Connection);
  DBObject.Assign(Obj);
  Mainform.UpdateEditorTab;
  Screen.Cursor := crHourglass;
  // Enable user to start typing immediately when creating a new object
  if DBObject.Name = '' then begin
    editName := FindComponent('editName') as TWinControl;
    if Assigned(editName) and editName.CanFocus then
      editName.SetFocus;
  end;
  SynMemo := FindComponent('SynMemoBody') as TSynMemo;
  if Assigned(SynMemo) and (not Assigned(SynMemo.PopupMenu)) then begin
    popup := TPopupMenu.Create(Self);
    popup.Images := MainForm.ImageListMain;
    Item := TMenuItem.Create(popup);
    Item.Action := MainForm.actCopy;
    popup.Items.Add(Item);
    Item := TMenuItem.Create(popup);
    Item.Action := MainForm.actCut;
    popup.Items.Add(Item);
    Item := TMenuItem.Create(popup);
    Item.Action := MainForm.actPaste;
    popup.Items.Add(Item);
    Item := TMenuItem.Create(popup);
    Item.Action := MainForm.actSelectAll;
    popup.Items.Add(Item);
    SynMemo.PopupMenu := popup;
  end;

end;

function TDBObjectEditor.DeInit: TModalResult;
var
  Msg, ObjType: String;
begin
  // Ask for saving modifications
  Result := mrOk;
  if Modified then begin
    ObjType := LowerCase(DBObject.ObjType);
    if DBObject.Name <> '' then
      Msg := 'Save modified '+ObjType+' "'+DBObject.Name+'"?'
    else
      Msg := 'Save new '+ObjType+'?';
    Result := MessageDialog(Msg, mtConfirmation, [mbYes, mbNo, mbCancel]);
    case Result of
      mrYes: Result := ApplyModifications;
      mrNo: Modified := False;
    end;
  end;
end;


function TDBObjectEditor.GetDefiners: TStringList;
  function q(s: String): String;
  begin
    Result := DBObject.Connection.QuoteIdent(s);
  end;
begin
  // For populating combobox items
  if not Assigned(FDefiners) then begin
    try
      FDefiners := DBObject.Connection.GetCol('SELECT CONCAT('+q('User')+', '+esc('@')+', '+q('Host')+') FROM '+
        q('mysql')+'.'+q('user')+' WHERE '+q('User')+'!='+esc('')+' ORDER BY '+q('User')+', '+q('Host'));
    except on E:EDatabaseError do
      FDefiners := TStringList.Create;
    end;
  end;
  Result := FDefiners;
end;




// Following code taken from OneInst.pas, http://assarbad.net/de/stuff/!import/nico.old/
// Slightly modified to better integrate that into our code, comments translated from german.

// Fetch and separate command line parameters into strings
function ParamBlobToStr(lpData: Pointer): TStringlist;
var
  pStr: PChar;
begin
  Result := TStringlist.Create;
  pStr := lpData;
  while pStr[0] <> #0 do
  begin
    Result.Add(string(pStr));
    pStr := @pStr[lstrlen(pStr) + 1];
  end;
end;

// Pack current command line parameters
function ParamStrToBlob(out cbData: DWORD): Pointer;
var
  Loop: Integer;
  pStr: PChar;
begin
  for Loop := 1 to ParamCount do
    cbData := cbData + DWORD(Length(ParamStr(Loop))*2 + 1);
  cbData := cbData + 2; // include appending #0#0
  Result := GetMemory(cbData);
  ZeroMemory(Result, cbData);
  pStr := Result;
  for Loop := 1 to ParamCount do
  begin
    lstrcpy(pStr, PChar(ParamStr(Loop)));
    pStr := @pStr[lstrlen(pStr) + 1];
  end;
end;

procedure HandleSecondInstance;
var
  Run: DWORD;
  Now: DWORD;
  Msg: TMsg;
  Wnd: HWND;
  Dat: TCopyDataStruct;
begin
  // MessageBox(0, 'already running', nil, MB_ICONINFORMATION);
  // Send a message to all main windows (HWND_BROADCAST) with the identical,
  // previously registered message id. We should only get reply from 0 or 1
  // instances.
  // (Broadcast should only be called with registered message ids!)

  SendMessage(HWND_BROADCAST, SecondInstMsgId, GetCurrentThreadId, 0);

  // Waiting for reply by first instance. For those of you which didn't knew:
  // Threads have message queues too ;o)
  Wnd := 0;
  Run := GetTickCount;
  while True do
  begin
    if PeekMessage(Msg, 0, SecondInstMsgId, SecondInstMsgId, PM_NOREMOVE) then
    begin
      GetMessage(Msg, 0, SecondInstMsgId, SecondInstMsgId);
      if Msg.message = SecondInstMsgId then
      begin
        Wnd := Msg.wParam;
        Break;
      end;
    end;
    Now := GetTickCount;
    if Now < Run then
      Run := Now;  // Avoid overflow, each 48 days.
    if Now - Run > 5000 then
      Break;
  end;

  if (Wnd <> 0) and IsWindow(Wnd) then
  begin
    // As a reply we got a handle to which we now send current parameters
    Dat.dwData := SecondInstMsgId;
    Dat.lpData := ParamStrToBlob(Dat.cbData);
    SendMessage(Wnd, WM_COPYDATA, 0, LPARAM(@Dat));
    FreeMemory(Dat.lpData);

    // Bring first instance to front
    if not IsWindowVisible(Wnd) then
      ShowWindow(Wnd, SW_RESTORE);
    BringWindowToTop(Wnd);
    SetForegroundWindow(Wnd);
  end;
end;

function CheckForSecondInstance: Boolean;
var
  Loop: Integer;
  MutexName: PChar;
begin
  // Try to create a system wide named kernel object (mutex). And check if that
  // already exists.
  // The name of such a mutex must not be longer than MAX_PATH (260) chars and
  // can contain all chars but not '\'

  Result := False;
  MutexName := PChar(APPNAME);
  for Loop := lstrlen(MutexName) to MAX_PATH - 1 do
  begin
    MutexHandle := CreateMutex(nil, False, MutexName);
    if (MutexHandle = 0) and (GetLastError = INVALID_HANDLE_VALUE) then
      // Looks like there is already a mutex using this name
      // Try to solve that by appending an underscore
      lstrcat(MutexName, '_')
    else
      // At least no naming conflict
      Break;
  end;

  case GetLastError of
    0: begin
        // We created the mutex, so this is the first instance
      end;
    ERROR_ALREADY_EXISTS:
      begin
        // There is already one instance
        try
          HandleSecondInstance;
        finally
          // Terminating is done in .dpr file, before Application.Initialize
          Result := True;
        end;
      end;
  else
    // No clue why we should get here. Oh, maybe Microsoft has changed rules, again.
    // However, we return false and let the application start
  end;
end;


function GetParentFormOrFrame(Comp: TWinControl): TWinControl;
begin
  Result := Comp;
  while True do begin
    Result := Result.Parent;
    // On a windows shutdown, GetParentForm() seems sporadically unable to find the owner form
    // In that case we would cause an exception when accessing it. Emergency break in that case.
    // See issue #1462
    if (not Assigned(Result)) or (Result is TCustomForm) or (Result is TFrame) then
      break;
  end;
end;


function GetIndexIcon(IndexType: String): Integer;
begin
  // Detect key icon index for specified index
  if IndexType = PKEY then Result := ICONINDEX_PRIMARYKEY
  else if IndexType = KEY then Result := ICONINDEX_INDEXKEY
  else if IndexType = UKEY then Result := ICONINDEX_UNIQUEKEY
  else if IndexType = FKEY then Result := ICONINDEX_FULLTEXTKEY
  else if IndexType = SKEY then Result := ICONINDEX_SPATIALKEY
  else Result := -1;
end;


function KeyPressed(Code: Integer): Boolean;
var
  State: TKeyboardState;
begin
  // Checks whether a key is pressed, defined by virtual key code
  GetKeyboardState(State);
  Result := (State[Code] and 128) <> 0;
end;


function GeneratePassword(Len: Integer): String;
var
  i: Integer;
  CharTable: String;
const
  Consos = 'bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ';
  Vocals = 'aeiouAEIOU';
  Numbers = '123456789';
begin
  // Create a random, mnemonic password
  SetLength(Result, Len);
  for i:=1 to Len do begin
    if Random(4) = 1 then
      CharTable := Numbers
    else if i mod 2 = 0 then
      CharTable := Vocals
    else
      CharTable := Consos;
    Result[i] := CharTable[Random(Length(CharTable)-1)+1];
  end;
end;


procedure InvalidateVT(VT: TVirtualStringTree; RefreshTag: Integer; ImmediateRepaint: Boolean);
begin
  // Avoid AVs in OnDestroy events
  if not Assigned(VT) then
    Exit;
  VT.Tag := RefreshTag;
  if ImmediateRepaint then
    VT.Repaint
  else
    VT.Invalidate;
end;


function CharAtPos(Str: String; Pos: Integer): Char;
begin
  // Access char in string without causing access violation
  if Length(Str) < Pos then
    Result := #0
  else
    Result := Str[Pos];
end;


function CompareAnyNode(Text1, Text2: String): Integer;
var
  Number1, Number2 : Extended;
  a1, a2, b1, b2: Char;
  NumberMode: Boolean;
const
  Numbers = ['0'..'9'];
begin
  Result := 0;
  // Apply different comparisons for numbers and text
  a1 := CharAtPos(Text1, 1);
  a2 := CharAtPos(Text1, 2);
  b1 := CharAtPos(Text2, 1);
  b2 := CharAtPos(Text2, 2);
  NumberMode := ((a1='-') and (CharInSet(a2, Numbers)) or CharInSet(a1, Numbers))
    and ((b1='-') and (CharInSet(b2, Numbers)) or CharInSet(b1, Numbers));
  if NumberMode then begin
    // Assuming numeric values
    Number1 := MakeFloat(Text1);
    Number2 := MakeFloat(Text2);
    if Number1 > Number2 then
      Result := 1
    else if Number1 = Number2 then
      Result := 0
    else if Number1 < Number2 then
      Result := -1;
  end;
  if (not NumberMode) or (Result=0) then begin
    // Compare Strings
    Result := CompareText(Text1, Text2);
  end;
end;


function StringListCompareAnythingAsc(List: TStringList; Index1, Index2: Integer): Integer;
begin
  // Sort TStringList items, containing numbers or strings, ascending
  Result := CompareAnyNode(List[Index1], List[Index2]);
end;


function StringListCompareAnythingDesc(List: TStringList; Index1, Index2: Integer): Integer;
begin
  // Sort TStringList items, containing numbers or strings, descending
  Result := CompareAnyNode(List[Index2], List[Index1]);
end;


function GetColumnDefaultType(var Text: String): TColumnDefaultType;
begin
  Result := TColumnDefaultType(MakeInt(Copy(Text, 1, 1)));
  Text := Copy(Text, 2, Length(Text)-1);
end;


function GetColumnDefaultClause(DefaultType: TColumnDefaultType; Text: String): String;
begin
  case DefaultType of
    cdtNothing:        Result := '';
    cdtText:           Result := 'DEFAULT '+esc(Text);
    cdtTextUpdateTS:   Result := 'DEFAULT '+esc(Text)+' ON UPDATE CURRENT_TIMESTAMP';
    cdtNull:           Result := 'DEFAULT NULL';
    cdtNullUpdateTS:   Result := 'DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP';
    cdtCurTS:          Result := 'DEFAULT CURRENT_TIMESTAMP';
    cdtCurTSUpdateTS:  Result := 'DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP';
    cdtAutoInc:        Result := 'AUTO_INCREMENT';
  end;
end;


{**
  Return compile date/time from passed .exe name
  Code taken and modified from Michael Puff
  http://www.michael-puff.de/Programmierung/Delphi/Code-Snippets/GetImageLinkTimeStamp.shtml
}
function GetImageLinkTimeStamp(const FileName: string): TDateTime;
const
  INVALID_SET_FILE_POINTER = DWORD(-1);
  BorlandMagicTimeStamp = $2A425E19; // Delphi 4-6 (and above?)
  FileTime1970: TFileTime = (dwLowDateTime:$D53E8000; dwHighDateTime:$019DB1DE);
type
  PImageSectionHeaders = ^TImageSectionHeaders;
  TImageSectionHeaders = array [Word] of TImageSectionHeader;
type
  PImageResourceDirectory = ^TImageResourceDirectory;
  TImageResourceDirectory = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    NumberOfNamedEntries: Word;
    NumberOfIdEntries: Word;
  end;
var
  FileHandle: THandle;
  BytesRead: DWORD;
  ImageDosHeader: TImageDosHeader;
  ImageNtHeaders: TImageNtHeaders;
  SectionHeaders: PImageSectionHeaders;
  Section: Word;
  ResDirRVA: DWORD;
  ResDirSize: DWORD;
  ResDirRaw: DWORD;
  ResDirTable: TImageResourceDirectory;
  FileTime: TFileTime;
  TimeStamp: DWord;
begin
  TimeStamp := 0;
  Result := 0;
  // Open file for read access
  FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if (FileHandle <> INVALID_HANDLE_VALUE) then try
    // Read MS-DOS header to get the offset of the PE32 header
    // (not required on WinNT based systems - but mostly available)
    if not ReadFile(FileHandle, ImageDosHeader, SizeOf(TImageDosHeader),
      BytesRead, nil) or (BytesRead <> SizeOf(TImageDosHeader)) or
      (ImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then begin
      ImageDosHeader._lfanew := 0;
    end;
    // Read PE32 header (including optional header
    if (SetFilePointer(FileHandle, ImageDosHeader._lfanew, nil, FILE_BEGIN) = INVALID_SET_FILE_POINTER) then
      Exit;
    if not(ReadFile(FileHandle, ImageNtHeaders, SizeOf(TImageNtHeaders), BytesRead, nil) and (BytesRead = SizeOf(TImageNtHeaders))) then
      Exit;
    // Validate PE32 image header
    if (ImageNtHeaders.Signature <> IMAGE_NT_SIGNATURE) then
      Exit;
    // Seconds since 1970 (UTC)
    TimeStamp := ImageNtHeaders.FileHeader.TimeDateStamp;

    // Check for Borland's magic value for the link time stamp
    // (we take the time stamp from the resource directory table)
    if (ImageNtHeaders.FileHeader.TimeDateStamp = BorlandMagicTimeStamp) then
    with ImageNtHeaders, FileHeader, OptionalHeader do begin
      // Validate Optional header
      if (SizeOfOptionalHeader < IMAGE_SIZEOF_NT_OPTIONAL_HEADER) or (Magic <> IMAGE_NT_OPTIONAL_HDR_MAGIC) then
        Exit;
      // Read section headers
      SectionHeaders :=
        GetMemory(NumberOfSections * SizeOf(TImageSectionHeader));
      if Assigned(SectionHeaders) then try
        if (SetFilePointer(FileHandle, SizeOfOptionalHeader - IMAGE_SIZEOF_NT_OPTIONAL_HEADER, nil, FILE_CURRENT) = INVALID_SET_FILE_POINTER) then
          Exit;
        if not(ReadFile(FileHandle, SectionHeaders^, NumberOfSections * SizeOf(TImageSectionHeader), BytesRead, nil) and (BytesRead = NumberOfSections * SizeOf(TImageSectionHeader))) then
          Exit;
        // Get RVA and size of the resource directory
        with DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE] do begin
          ResDirRVA := VirtualAddress;
          ResDirSize := Size;
        end;
        // Search for section which contains the resource directory
        ResDirRaw := 0;
        for Section := 0 to NumberOfSections - 1 do
        with SectionHeaders[Section] do
          if (VirtualAddress <= ResDirRVA) and (VirtualAddress + SizeOfRawData >= ResDirRVA + ResDirSize) then begin
            ResDirRaw := PointerToRawData - (VirtualAddress - ResDirRVA);
            Break;
          end;
        // Resource directory table found?
        if (ResDirRaw = 0) then
          Exit;
        // Read resource directory table
        if (SetFilePointer(FileHandle, ResDirRaw, nil, FILE_BEGIN) = INVALID_SET_FILE_POINTER) then
          Exit;
        if not(ReadFile(FileHandle, ResDirTable, SizeOf(TImageResourceDirectory), BytesRead, nil) and (BytesRead = SizeOf(TImageResourceDirectory))) then
          Exit;
        // Convert from DosDateTime to SecondsSince1970
        if DosDateTimeToFileTime(HiWord(ResDirTable.TimeDateStamp), LoWord(ResDirTable.TimeDateStamp), FileTime) then begin
          // FIXME: Borland's linker uses the local system time
          // of the user who linked the executable image file.
          // (is that information anywhere?)
          TimeStamp := (ULARGE_INTEGER(FileTime).QuadPart - ULARGE_INTEGER(FileTime1970).QuadPart) div 10000000;
        end;
      finally
        FreeMemory(SectionHeaders);
      end;
    end;
  finally
    CloseHandle(FileHandle);
  end;
  Result := UnixToDateTime(TimeStamp);
end;


function IsEmpty(Str: String): Boolean;
begin
  // Alternative version of "Str = ''"
  Result := Str = '';
end;

function IsNotEmpty(Str: String): Boolean;
begin
  // Alternative version of "Str <> ''"
  Result := Str <> '';
end;


function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
begin
  Result := MessageDialog('', Msg, DlgType, Buttons);
end;


function MessageDialog(const Title, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
var
  m: String;
begin
 if (Win32MajorVersion >= 6) and (Title <> '') then
   Result := TaskMessageDlg(Title, Msg, DlgType, Buttons, 0)
 else begin
   m := Msg;
   if Title <> '' then
     m := Title + CRLF + CRLF + m;
   Result := MessageDlg(m, DlgType, Buttons, 0);
 end;
end;


function ErrorDialog(Msg: string): Integer;
begin
  Result := MessageDialog(Msg, mtError, [mbOK]);
end;


function ErrorDialog(const Title, Msg: string): Integer;
begin
  Result := MessageDialog(Title, Msg, mtError, [mbOK]);
end;


function GetHTMLCharsetByEncoding(Encoding: TEncoding): String;
begin
  Result := '';
  if Encoding = TEncoding.Default then
    Result := 'Windows-'+IntToStr(GetACP)
  else if Encoding = TEncoding.ASCII then
    Result := 'ascii'
  else if Encoding = TEncoding.Unicode then
    Result := 'utf-16le'
  else if Encoding = TEncoding.BigEndianUnicode then
    Result := 'utf-16'
  else if Encoding = TEncoding.UTF8 then
    Result := 'utf-8'
  else if Encoding = TEncoding.UTF7 then
    Result := 'utf-7';
end;



{ Threading stuff }

constructor TQueryThread.Create(Connection: TDBConnection; Batch: TSQLBatch; TabNumber: Integer);
begin
  inherited Create(False);
  FConnection := Connection;
  FAborted := False;
  FBatch := Batch;
  FTabNumber := TabNumber;
  FBatchPosition := 0;
  FQueryTime := 0;
  FQueryNetTime := 0;
  FRowsAffected := 0;
  FRowsFound := 0;
  FWarningCount := 0;
  FErrorMessage := '';
  FBatchInOneGo := MainForm.actBatchInOneGo.Checked;
  FStopOnErrors := MainForm.actQueryStopOnErrors.Checked;
  FreeOnTerminate := True;
  Priority := tpNormal;
end;


procedure TQueryThread.Execute;
var
  SQL: String;
  i, BatchStartOffset, ResultCount: Integer;
  PacketSize, MaxAllowedPacket: Int64;
  DoStoreResult, ErrorAborted: Boolean;
begin
  inherited;

  MaxAllowedPacket := 0;
  i := 0;
  ResultCount := 0;
  ErrorAborted := False;

  while i < FBatch.Count do begin
    SQL := '';
    if not FBatchInOneGo then begin
      SQL := FBatch[i].SQL;
      Inc(i);
    end else begin
      // Concat queries up to a size of max_allowed_packet
      if MaxAllowedPacket = 0 then begin
        if FConnection.Parameters.NetTypeGroup = ngMySQL then begin
          FConnection.LockedByThread := Self;
          MaxAllowedPacket := MakeInt(FConnection.GetVar('SHOW VARIABLES LIKE '+esc('max_allowed_packet'), 1));
          FConnection.LockedByThread := nil;
        end else
          MaxAllowedPacket := SIZE_MB;
        // TODO: Log('Detected maximum allowed packet size: '+FormatByteNumber(MaxAllowedPacket), lcDebug);
      end;
      BatchStartOffset := FBatch[i].LeftOffset;
      while i < FBatch.Count do begin
        PacketSize := FBatch[i].RightOffset - BatchStartOffset + ((i-FBatchPosition) * 10);
        if (PacketSize >= MaxAllowedPacket) or (i-FBatchPosition >= 50) then begin
          // TODO: Log('Limiting batch packet size to '+FormatByteNumber(Length(SQL))+' with '+FormatNumber(i-FUserQueryOffset)+' queries.', lcDebug);
          break;
        end;
        SQL := SQL + FBatch[i].SQL + ';';
        Inc(i);
      end;
      FQueriesInPacket := i - FBatchPosition;
    end;
    Synchronize(BeforeQuery);
    try
      FConnection.LockedByThread := Self;
      DoStoreResult := ResultCount < AppSettings.ReadInt(asMaxQueryResults);
      FConnection.Query(SQL, DoStoreResult, lcUserFiredSQL);
      Inc(ResultCount, FConnection.ResultCount);
      FBatchPosition := i;
      Inc(FQueryTime, FConnection.LastQueryDuration);
      Inc(FQueryNetTime, FConnection.LastQueryNetworkDuration);
      Inc(FRowsAffected, FConnection.RowsAffected);
      Inc(FRowsFound, FConnection.RowsFound);
      Inc(FWarningCount, FConnection.WarningCount);
    except
      on E:EDatabaseError do begin
        if FStopOnErrors or (i = FBatch.Count - 1) then begin
          FErrorMessage := E.Message;
          ErrorAborted := True;
        end;
      end;
    end;
    FConnection.LockedByThread := nil;
    Synchronize(AfterQuery);
    // Check if FAborted is set by the main thread, to avoid proceeding the loop in case
    // FStopOnErrors is set to false
    if FAborted or ErrorAborted then
      break;
  end;

  Synchronize(BatchFinished);
end;


procedure TQueryThread.BeforeQuery;
begin
  MainForm.BeforeQueryExecution(Self);
end;


procedure TQueryThread.LogFromOutside(Msg: String; Category: TDBLogCategory);
begin
  FLogMsg := Msg;
  FLogCategory := Category;
  Synchronize(Log);
end;


procedure TQueryThread.Log;
begin
  FConnection.OnLog(FLogMsg, FLogCategory, FConnection);
end;


procedure TQueryThread.AfterQuery;
begin
  MainForm.AfterQueryExecution(Self);
end;


procedure TQueryThread.BatchFinished;
begin
  MainForm.FinishedQueryExecution(Self);
end;


{ TSQLSentence }

constructor TSQLSentence.Create(Owner: TSQLBatch);
begin
  // Use a back reference to the parent batch object, so we can extract SQL from it
  FOwner := Owner;
end;


function TSQLSentence.GetSize: Integer;
begin
  Result := RightOffset - LeftOffset;
end;


function TSQLSentence.GetSQL: String;
begin
  Result := Copy(FOwner.SQL, LeftOffset, RightOffset-LeftOffset);
end;


{ TSQLBatch }

function TSQLBatch.GetSize: Integer;
var
  Query: TSQLSentence;
begin
  // Return overall string length of batch
  Result := 0;
  for Query in Self do
    Inc(Result, Query.Size);
end;


procedure TSQLBatch.SetSQL(Value: String);
var
  i, AllLen, DelimLen, DelimStart, LastLeftOffset, RightOffset, LastNewLineOffset: Integer;
  c, n, LastStringEncloser: Char;
  Delim, DelimTest, QueryTest: String;
  InString, InComment, InBigComment, InEscape: Boolean;
  Marker: TSQLSentence;
  rx: TRegExpr;
const
  StringEnclosers = ['"', '''', '`'];
  NewLines = [#13, #10];
  WhiteSpaces = NewLines + [#9, ' '];
begin
  // Scan SQL batch for delimiters and store a list with start + end offsets
  FSQL := Value;
  Clear;
  AllLen := Length(FSQL);
  i := 0;
  LastLeftOffset := 1;
  Delim := Mainform.Delimiter;
  InString := False; // Loop in "enclosed string" or `identifier`
  InComment := False; // Loop in one-line comment (# or --)
  InBigComment := False; // Loop in /* multi-line */ or /*! condictional comment */
  InEscape := False; // Previous char was backslash
  LastStringEncloser := #0;
  DelimLen := Length(Delim);
  rx := TRegExpr.Create;
  rx.Expression := '^\s*DELIMITER\s+(\S+)';
  rx.ModifierG := True;
  rx.ModifierI := True;
  rx.ModifierM := False;
  while i < AllLen do begin
    Inc(i);
    // Current and next char
    c := FSQL[i];
    if i < AllLen then n := FSQL[i+1]
    else n := #0;

    // Check for comment syntax and for enclosed literals, so a query delimiter can be ignored
    if (not InComment) and (not InBigComment) and (not InString) and ((c + n = '--') or (c = '#')) then
      InComment := True;
    if (not InComment) and (not InBigComment) and (not InString) and (c + n = '/*') then
      InBigComment := True;
    if InBigComment and (not InComment) and (not InString) and (c + n = '*/') then
      InBigComment := False;
    if (not InEscape) and (not InComment) and (not InBigComment) and CharInSet(c, StringEnclosers) then begin
      if (not InString) or (InString and (c = LastStringEncloser)) then begin
        InString := not InString;
        LastStringEncloser := c;
      end;
    end;
    if (CharInSet(c, NewLines) and (not CharInSet(n, NewLines))) or (i = 1) then begin
      if i > 1 then
        InComment := False;
      if (not InString) and (not InBigComment) and rx.Exec(copy(FSQL, i, 100)) then begin
        Delim := rx.Match[1];
        DelimLen := rx.MatchLen[1];
        Inc(i, rx.MatchLen[0]);
        LastLeftOffset := i;
        continue;
      end;
    end;
    if not InEscape then
      InEscape := c = '\'
    else
      InEscape := False;

    // Prepare delimiter test string
    if (not InComment) and (not InString) and (not InBigComment) then begin
      DelimStart := Max(1, i+1-DelimLen);
      DelimTest := Copy(FSQL, DelimStart, i-Max(i-DelimLen, 0));
    end else
      DelimTest := '';

    // End of query or batch reached. Add query markers to result list if sentence is not empty.
    if (DelimTest = Delim) or (i = AllLen) then begin
      RightOffset := i+1;
      if DelimTest = Delim then
        Dec(RightOffset, DelimLen);
      QueryTest := Trim(Copy(FSQL, LastLeftOffset, RightOffset-LastLeftOffset));
      if (QueryTest <> '') and (QueryTest <> Delim) then begin
        Marker := TSQLSentence.Create(Self);
        while CharInSet(FSQL[LastLeftOffset], WhiteSpaces) do
          Inc(LastLeftOffset);
        Marker.LeftOffset := LastLeftOffset;
        Marker.RightOffset := RightOffset;
        Add(Marker);
        LastLeftOffset := i+1;
      end;
    end;
  end;
end;


{ THttpDownload }

constructor THttpDownload.Create(Owner: TComponent);
begin
  FBytesRead := -1;
  FContentLength := -1;
  FOwner := Owner;
end;


procedure THttpDownload.SendRequest(Filename: String);
var
  NetHandle: HINTERNET;
  UrlHandle: HINTERNET;
  Buffer: array[1..4096] of Byte;
  Head: array[1..1024] of Char;
  BytesInChunk, HeadSize, Reserved: Cardinal;
  LocalFile: File;
  DoStore: Boolean;
  UserAgent, OS: String;
  HttpStatus: Integer;
begin
  DoStore := False;
  if MainForm.IsWine then
    OS := 'Linux/Wine'
  else
    OS := 'Windows NT '+IntToStr(Win32MajorVersion)+'.'+IntToStr(Win32MinorVersion);
  UserAgent := APPNAME+'/'+MainForm.AppVersion+' ('+OS+'; '+ExtractFilename(Application.ExeName)+'; '+FOwner.Name+')';
  NetHandle := InternetOpen(PChar(UserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  try
    UrlHandle := InternetOpenURL(NetHandle, PChar(FURL), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if not Assigned(UrlHandle) then
      raise Exception.Create('Could not open URL: '+FURL);

    // Detect content length
    HeadSize := SizeOf(Head);
    Reserved := 0;
    if HttpQueryInfo(UrlHandle, HTTP_QUERY_CONTENT_LENGTH, @Head, HeadSize, Reserved) then
      FContentLength := StrToIntDef(Head, -1)
    else
      raise Exception.Create('Server did not send required "Content-Length" header: '+FURL);

    // Check if we got HTTP status 200
    HeadSize := SizeOf(Head);
    Reserved := 0;
    if HttpQueryInfo(UrlHandle, HTTP_QUERY_STATUS_CODE, @Head, HeadSize, Reserved) then begin
      HttpStatus := StrToIntDef(Head, -1);
      if HttpStatus <> 200 then
        raise Exception.Create('Got HTTP status '+IntToStr(HttpStatus)+' from '+FURL);
    end;

    // Create local file
    if Filename <> '' then begin
      AssignFile(LocalFile, FileName);
      Rewrite(LocalFile, 1);
      DoStore := True;
    end;

    // Stream contents
    while true do begin
      InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesInChunk);
      if DoStore then
        BlockWrite(LocalFile, Buffer, BytesInChunk);
      Inc(FBytesRead, BytesInChunk);
      if Assigned(FOnProgress) then
        FOnProgress(Self);
      if BytesInChunk = 0 then
        break;
    end;

  finally
    if DoStore then
      CloseFile(LocalFile);
    if Assigned(UrlHandle) then
      InternetCloseHandle(UrlHandle);
    if Assigned(NetHandle) then
      InternetCloseHandle(NetHandle);
  end;
end;



{ TAppSettings }

constructor TAppSettings.Create;
var
  rx: TRegExpr;
  i: Integer;
begin
  inherited;
  FRegistry := TRegistry.Create;
  FReads := 0;
  FWrites := 0;

  // Use filename from command line. If not given, use file in directory of executable.
  rx := TRegExpr.Create;
  rx.Expression := '^\-\-?psettings\=(.+)$';
  for i:=1 to ParamCount do begin
    if rx.Exec(ParamStr(i)) then begin
      FSettingsFile := rx.Match[1];
      break;
    end;
  end;
  if FSettingsFile = '' then
    FSettingsFile := ExtractFilePath(ParamStr(0)) + 'portable_settings.txt';
  if FileExists(FSettingsFile) then begin
    FPortableMode := True;
    FBasePath := '\Software\' + APPNAME + ' Portable '+IntToStr(GetCurrentProcessId)+'\';
    try
      ImportSettings(FSettingsFile);
    except
      on E:Exception do
        ErrorDialog(E.Message);
    end;
  end else begin
    FPortableMode := False;
    FBasePath := '\Software\' + APPNAME + '\';
    FSettingsFile := '';
  end;

  PrepareRegistry;

  InitSetting(asHiddenColumns,                    'HiddenColumns',                         0, False, '', True);
  InitSetting(asFilter,                           'Filter',                                0, False, '', True);
  InitSetting(asSort,                             'Sort',                                  0, False, '', True);
  InitSetting(asDisplayedColumnsSorted,           'DisplayedColumnsSorted',                0, False);
  InitSetting(asLastSessions,                     'LastSessions',                          0, False, '');
  InitSetting(asLastActiveSession,                'LastActiveSession',                     0, False, '');
  InitSetting(asAutoReconnect,                    'AutoReconnect',                         0, False);
  InitSetting(asRestoreLastUsedDB,                'RestoreLastUsedDB',                     0, True);
  InitSetting(asLastUsedDB,                       'lastUsedDB',                            0, False, '');
  InitSetting(asTreeBackground,                   'TreeBackground',                        clNone, False, '', True);
  InitSetting(asFontName,                         'FontName',                              0, False, 'Courier New');
  InitSetting(asFontSize,                         'FontSize',                              9);
  InitSetting(asTabWidth,                         'TabWidth',                              3);
  InitSetting(asDataFontName,                     'DataFontName',                          0, False, 'Tahoma');
  InitSetting(asDataFontSize,                     'DataFontSize',                          8);
  InitSetting(asLogsqlnum,                        'logsqlnum',                             300);
  InitSetting(asLogsqlwidth,                      'logsqlwidth',                           2000);
  InitSetting(asSessionLogsDirectory,             'SessionLogsDirectory',                  0, False, DirnameUserAppData + 'Sessionlogs\');
  InitSetting(asLogHorizontalScrollbar,           'LogHorizontalScrollbar',                0, False);
  InitSetting(asSQLColActiveLine,                 'SQLColActiveLine',                      0, False, 'clNone');
  InitSetting(asMaxColWidth,                      'MaxColWidth',                           300);
  InitSetting(asDatagridMaximumRows,              'DatagridMaximumRows',                   100000);
  InitSetting(asDatagridRowsPerStep,              'DatagridRowsPerStep',                   1000);
  InitSetting(asGridRowLineCount,                 'GridRowLineCount',                      1);
  InitSetting(asRememberFilters,                  'RememberFilters',                       0, True);
  InitSetting(asLogToFile,                        'LogToFile',                             0, False);
  InitSetting(asMainWinMaximized,                 'MainWinMaximized',                      0, False);
  InitSetting(asMainWinLeft,                      'MainWinLeft',                           100);
  InitSetting(asMainWinTop,                       'MainWinTop',                            100);
  InitSetting(asMainWinWidth,                     'MainWinWidth',                          800);
  InitSetting(asMainWinHeight,                    'MainWinHeight',                         600);
  InitSetting(asMainWinOnMonitor,                 'MainWinOnMonitor',                      1);
  InitSetting(asToolBar2Left,                     'ToolBar2Left',                          11);
  InitSetting(asToolBar2Top,                      'ToolBar2Top',                           2);
  InitSetting(asToolBarDataLeft,                  'ToolBarDataLeft',                       343);
  InitSetting(asToolBarDataTop,                   'ToolBarDataTop',                        2);
  InitSetting(asToolBarQueryLeft,                 'ToolBarQueryLeft',                      494);
  InitSetting(asToolBarQueryTop,                  'ToolBarQueryTop',                       2);
  InitSetting(asQuerymemoheight,                  'querymemoheight',                       100);
  InitSetting(asDbtreewidth,                      'dbtreewidth',                           170);
  InitSetting(asDataPreviewHeight,                'DataPreviewHeight',                     100);
  InitSetting(asDataPreviewEnabled,               'DataPreviewEnabled',                    0, False);
  InitSetting(asLogHeight,                        'sqloutheight',                          80);
  InitSetting(asQueryhelperswidth,                'queryhelperswidth',                     200);
  InitSetting(asStopOnErrorsInBatchMode,          'StopOnErrorsInBatchMode',               0, True);
  InitSetting(asWrapLongLines,                    'WrapLongLines',                         0, False);
  InitSetting(asDisplayBLOBsAsText,               'DisplayBLOBsAsText',                    0, False);
  InitSetting(asSingleQueries,                    'SingleQueries',                         0, True);
  InitSetting(asMemoEditorWidth,                  'MemoEditorWidth',                       100);
  InitSetting(asMemoEditorHeight,                 'MemoEditorHeight',                      100);
  InitSetting(asMemoEditorWrap,                   'MemoEditorWrap',                        0, False);
  InitSetting(asDelimiter,                        'Delimiter',                             0, False, ';');
  InitSetting(asSQLHelpWindowLeft,                'SQLHelp_WindowLeft',                    0);
  InitSetting(asSQLHelpWindowTop,                 'SQLHelp_WindowTop',                     0);
  InitSetting(asSQLHelpWindowWidth,               'SQLHelp_WindowWidth',                   600);
  InitSetting(asSQLHelpWindowHeight,              'SQLHelp_WindowHeight',                  400);
  InitSetting(asSQLHelpPnlLeftWidth,              'SQLHelp_PnlLeftWidth',                  150);
  InitSetting(asSQLHelpPnlRightTopHeight,         'SQLHelp_PnlRightTopHeight',             150);
  InitSetting(asTableEditorTabsHeight,            'TableEditorTabsHeight',                 150);
  InitSetting(asHost,                             'Host',                                  0, False, '127.0.0.1', True);
  InitSetting(asUser,                             'User',                                  0, False, 'root', True);
  InitSetting(asPassword,                         'Password',                              0, False, '', True);
  InitSetting(asWindowsAuth,                      'WindowsAuth',                           0, False, '', True);
  InitSetting(asLoginPrompt,                      'LoginPrompt',                           0, False, '', True);
  InitSetting(asPort,                             'Port',                                  0, False, '3306', True);
  InitSetting(asPlinkExecutable,                  'PlinkExecutable',                       0, False, '');
  InitSetting(asSSHtunnelHost,                    'SSHtunnelHost',                         0, False, '', True);
  InitSetting(asSSHtunnelHostPort,                'SSHtunnelHostPort',                     22, False, '', True);
  InitSetting(asSSHtunnelPort,                    'SSHtunnelPort',                         0, False, '', True);
  InitSetting(asSSHtunnelUser,                    'SSHtunnelUser',                         0, False, '', True);
  InitSetting(asSSHtunnelPassword,                'SSHtunnelPassword',                     0, False, '', True);
  InitSetting(asSSHtunnelTimeout,                 'SSHtunnelTimeout',                      4, False, '', True);
  InitSetting(asSSHtunnelPrivateKey,              'SSHtunnelPrivateKey',                   0, False, '', True);
  InitSetting(asSSLActive,                        'SSL_Active',                            0, False, '', True);
  InitSetting(asSSLKey,                           'SSL_Key',                               0, False, '', True);
  InitSetting(asSSLCert,                          'SSL_Cert',                              0, False, '', True);
  InitSetting(asSSLCA,                            'SSL_CA',                                0, False, '', True);
  InitSetting(asNetType,                          'NetType',                               Integer(ntMySQL_TCPIP), False, '', True);
  InitSetting(asCompressed,                       'Compressed',                            0, False, '', True);
  InitSetting(asLocalTimeZone,                    'LocalTimeZone',                         0, False, '', True);
  InitSetting(asStartupScriptFilename,            'StartupScriptFilename',                 0, False, '', True);
  InitSetting(asDatabases,                        'Databases',                             0, False, '', True);
  InitSetting(asDatabaseFilter,                   'DatabaseFilter',                        0, False, '');
  InitSetting(asExportSQLCreateDatabases,         'ExportSQL_CreateDatabases',             0, False);
  InitSetting(asExportSQLDropDatabases,           'ExportSQL_DropDatabases',               0, False);
  InitSetting(asExportSQLCreateTables,            'ExportSQL_CreateTables',                0, False);
  InitSetting(asExportSQLDropTables,              'ExportSQL_DropTables',                  0, False);
  InitSetting(asExportSQLDataHow,                 'ExportSQL_DataHow',                     0);
  InitSetting(asExportSQLFilenames,               'ExportSQL_Filenames',                   0, False, '');
  InitSetting(asExportSQLDirectories,             'ExportSQL_Directories',                 0, False, '');
  InitSetting(asExportSQLDatabase,                'ExportSQL_Database',                    0, False, '');
  InitSetting(asExportSQLServerDatabase,          'ExportSQL_ServerDatabase',              0, False, '');
  InitSetting(asExportSQLOutput,                  'ExportSQL_Output',                      0);
  InitSetting(asGridExportOutputCopy,             'GridExportOutputCopy',                  0, True);
  InitSetting(asGridExportOutputFile,             'GridExportOutputFile',                  0, False);
  InitSetting(asGridExportFilename,               'GridExportFilename',                    0, False, '');
  InitSetting(asGridExportRecentFiles,            'GridExportRecentFiles',                 0, False, '');
  InitSetting(asGridExportEncoding,               'GridExportEncoding',                    4);
  InitSetting(asGridExportFormat,                 'GridExportFormat',                      0);
  InitSetting(asGridExportSelection,              'GridExportSelection',                   1);
  InitSetting(asGridExportColumnNames,            'GridExportColumnNames',                 0, True);
  InitSetting(asGridExportSeparator,              'GridExportSeparator',                   0, False, ';');
  InitSetting(asGridExportEncloser,               'GridExportEncloser',                    0, False, '');
  InitSetting(asGridExportTerminator,             'GridExportTerminator',                  0, False, '\r\n');
  InitSetting(asCSVImportSeparator,               'CSVSeparatorV2',                        0, False, ';');
  InitSetting(asCSVImportEncloser,                'CSVEncloserV2',                         0, False, '"');
  InitSetting(asCSVImportTerminator,              'CSVTerminator',                         0, False, '\r\n');
  InitSetting(asCSVImportFieldEscaper,            'CSVImportFieldEscaperV2',               0, False, '"');
  InitSetting(asCSVImportWindowWidth,             'CSVImportWindowWidth',                  530);
  InitSetting(asCSVImportWindowHeight,            'CSVImportWindowHeight',                 530);
  InitSetting(asCSVImportFilename,                'loadfilename',                          0, False, '');
  InitSetting(asCSVImportFieldsEnclosedOptionally, 'CSVImportFieldsEnclosedOptionallyV2',  0, True);
  InitSetting(asCSVImportIgnoreLines,             'CSVImportIgnoreLines',                  1);
  InitSetting(asCSVImportLowPriority,             'CSVImportLowPriority',                  0, True);
  InitSetting(asCSVImportLocalNumbers,            'CSVImportLocalNumbers',                 0, False);
  InitSetting(asCSVImportTruncateTable,           'CSVImportTruncateTable',                0, False);
  InitSetting(asCSVImportDuplicateHandling,       'CSVImportDuplicateHandling',            2);
  InitSetting(asCSVImportParseMethod,             'CSVImportParseMethod',                  0);
  InitSetting(asUpdatecheck,                      'Updatecheck',                           0, False);
  InitSetting(asUpdatecheckBuilds,                'UpdatecheckBuilds',                     0, False);
  InitSetting(asUpdatecheckInterval,              'UpdatecheckInterval',                   3);
  InitSetting(asUpdatecheckLastrun,               'UpdatecheckLastrun',                    0, False, '2000-01-01');
  InitSetting(asTableToolsWindowWidth,            'TableTools_WindowWidth',                560);
  InitSetting(asTableToolsWindowHeight,           'TableTools_WindowHeight',               420);
  InitSetting(asTableToolsTreeWidth,              'TableTools_TreeWidth',                  150);
  InitSetting(asTableToolsFindText,               'TableTools_FindText',                   0, False, '');
  InitSetting(asTableToolsDatatype,               'TableTools_Datatype',                   0);
  InitSetting(asTableToolsFindCaseSensitive,      'TableTools_FindCaseSensitive',          0, False);
  InitSetting(asFileImportWindowWidth,            'FileImport_WindowWidth',                530);
  InitSetting(asFileImportWindowHeight,           'FileImport_WindowHeight',               530);
  InitSetting(asEditVarWindowWidth,               'EditVar_WindowWidth',                   300);
  InitSetting(asEditVarWindowHeight,              'EditVar_WindowHeight',                  260);
  InitSetting(asUsermanagerWindowWidth,           'Usermanager_WindowWidth',               500);
  InitSetting(asUsermanagerWindowHeight,          'Usermanager_WindowHeight',              400);
  InitSetting(asUsermanagerListWidth,             'Usermanager_ListWidth',                 180);
  InitSetting(asSelectDBOWindowWidth,             'SelectDBO_WindowWidth',                 250);
  InitSetting(asSelectDBOWindowHeight,            'SelectDBO_WindowHeight',                350);
  InitSetting(asSessionManagerListWidth,          'SessionManager_ListWidth',              170);
  InitSetting(asSessionManagerWindowWidth,        'SessionManager_WindowWidth',            500);
  InitSetting(asSessionManagerWindowHeight,       'SessionManager_WindowHeight',           400);
  InitSetting(asCopyTableWindowHeight,            'CopyTable_WindowHeight',                340);
  InitSetting(asCopyTableWindowWidth,             'CopyTable_WindowWidth',                 380);
  InitSetting(asCopyTableColumns,                 'CopyTable_Columns',                     0, True);
  InitSetting(asCopyTableKeys,                    'CopyTable_Keys',                        0, True);
  InitSetting(asCopyTableForeignKeys,             'CopyTable_ForeignKeys',                 0, True);
  InitSetting(asCopyTableData,                    'CopyTable_Data',                        0, True);
  InitSetting(asCopyTableRecentFilter,            'CopyTable_RecentFilter_%s',             0, False, '');
  InitSetting(asServerVersion,                    'ServerVersion',                         0, False, '', True);
  InitSetting(asServerVersionFull,                'ServerVersionFull',                     0, False, '', True);
  InitSetting(asLastConnect,                      'LastConnect',                           0, False, '2000-01-01', True);
  InitSetting(asConnectCount,                     'ConnectCount',                          0, False, '', True);
  InitSetting(asRefusedCount,                     'RefusedCount',                          0, False, '', True);
  InitSetting(asSessionCreated,                   'SessionCreated',                        0, False, '', True);
  InitSetting(asDoUsageStatistics,                'DoUsageStatistics',                     0, False);
  InitSetting(asLastUsageStatisticCall,           'LastUsageStatisticCall',                0, False, '2000-01-01');
  InitSetting(asDisplayBars,                      'DisplayBars',                           0, true);
  InitSetting(asBarColor,                         'BarColor',                              $00BBFFDD);
  InitSetting(asMySQLBinaries,                    'MySQL_Binaries',                        0, False, '');
  InitSetting(asPromptSaveFileOnTabClose,         'PromptSaveFileOnTabClose',              0, True);
  InitSetting(asCompletionProposal,               'CompletionProposal',                    0, True);
  InitSetting(asTabsToSpaces,                     'TabsToSpaces',                          0, False);
  InitSetting(asFilterPanel,                      'FilterPanel',                           0, False);
  InitSetting(asAllowMultipleInstances,           'AllowMultipleInstances',                0, True);
  InitSetting(asFindDialogSearchHistory,          'FindDialogSearchHistory',               0, False, '');
  InitSetting(asFindDialogReplaceHistory,         'FindDialogReplaceHistory',              0, False, '');
  InitSetting(asMaxQueryResults,                  'MaxQueryResults',                       10);
  InitSetting(asSetEditorWidth,                   'SetEditorWidth',                        100);
  InitSetting(asSetEditorHeight,                  'SetEditorHeight',                       130);
  InitSetting(asLogErrors,                        'LogErrors',                             0, True);
  InitSetting(asLogUserSQL,                       'LogUserSQL',                            0, True);
  InitSetting(asLogSQL,                           'LogSQL',                                0, True);
  InitSetting(asLogInfos,                         'LogInfos',                              0, True);
  InitSetting(asLogDebug,                         'LogDebug',                              0, False);
  InitSetting(asFieldColorNumeric,                'FieldColor_Numeric',                    $00FF0000);
  InitSetting(asFieldColorReal,                   'FieldColor_Real',                       $00FF0048);
  InitSetting(asFieldColorText,                   'FieldColor_Text',                       $00008000);
  InitSetting(asFieldColorBinary,                 'FieldColor_Binary',                     $00800080);
  InitSetting(asFieldColorDatetime,               'FieldColor_Datetime',                   $00000080);
  InitSetting(asFieldColorSpatial,                'FieldColor_Spatial',                    $00808000);
  InitSetting(asFieldColorOther,                  'FieldColor_Other',                      $00008080);
  InitSetting(asFieldEditorBinary,                'FieldEditor_Binary',                    0, True);
  InitSetting(asFieldEditorDatetime,              'FieldEditor_Datetime',                  0, True);
  InitSetting(asFieldEditorDatetimePrefill,       'FieldEditor_Datetime_Prefill',          0, True);
  InitSetting(asFieldEditorEnum,                  'FieldEditor_Enum',                      0, True);
  InitSetting(asFieldEditorSet,                   'FieldEditor_Set',                       0, True);
  InitSetting(asFieldNullBackground,              'Field_NullBackground',                  $00FF00FF);
  InitSetting(asGroupTreeObjects,                 'GroupTreeObjects',                      0, False);
  InitSetting(asDisplayObjectSizeColumn,          'DisplayObjectSizeColumn',               0, True);
  InitSetting(asActionShortcut1,                  'Shortcut1_%s',                          0);
  InitSetting(asActionShortcut2,                  'Shortcut2_%s',                          0);
  InitSetting(asHighlighterForeground,            'SQL Attr %s Foreground',                0);
  InitSetting(asHighlighterBackground,            'SQL Attr %s Background',                0);
  InitSetting(asHighlighterStyle,                 'SQL Attr %s Style',                     0);
  InitSetting(asSQLfile,                          'SQLFile%s',                             0, False, '');
  InitSetting(asListColWidths,                    'ColWidths_%s',                          0, False, '');
  InitSetting(asListColsVisible,                  'ColsVisible_%s',                        0, False, '');
  InitSetting(asListColPositions,                 'ColPositions_%s',                       0, False, '');
  InitSetting(asListColSort,                      'ColSort_%s',                            0, False, '');
  InitSetting(asSessionFolder,                    'Folder',                                0, False);
  InitSetting(asRecentFilter,                     '',                                      0, False, '');
  InitSetting(asDateTimeEditorCursorPos,          'DateTimeEditor_CursorPos_Type%s',       0);
end;


destructor TAppSettings.Destroy;
var
  AllKeys: TStringList;
  i: Integer;
  Proc: TProcessEntry32;
  ProcRuns: Boolean;
  SnapShot: THandle;
  rx: TRegExpr;
begin
  // Export settings into textfile in portable mode.
  if FPortableMode then try
    ExportSettings(FSettingsFile);
    FRegistry.CloseKey;
    FRegistry.DeleteKey(FBasePath);

    // Remove dead keys from instances which didn't close clean, e.g. because of an AV
    SnapShot := CreateToolhelp32Snapshot(TH32CS_SnapProcess, 0);
    Proc.dwSize := Sizeof(Proc);
    FRegistry.OpenKeyReadOnly('\Software\');
    AllKeys := TStringList.Create;
    FRegistry.GetKeyNames(AllKeys);
    rx := TRegExpr.Create;
    rx.Expression := '^' + QuoteRegExprMetaChars(APPNAME) + ' Portable (\d+)$';
    for i:=0 to AllKeys.Count-1 do begin
      if not rx.Exec(AllKeys[i]) then
        Continue;
      ProcRuns := False;
      if Process32First(SnapShot, Proc) then while True do begin
        ProcRuns := rx.Match[1] = IntToStr(Proc.th32ProcessID);
        if ProcRuns or (not Process32Next(SnapShot, Proc)) then
          break;
      end;
      if not ProcRuns then
        FRegistry.DeleteKey(AllKeys[i]);
    end;
    FRegistry.CloseKey;
    CloseHandle(SnapShot);
    AllKeys.Free;
    rx.Free;
  except
    on E:Exception do
      ErrorDialog(E.Message);
  end;
  FRegistry.Free;
  inherited;
end;


procedure TAppSettings.InitSetting(Index: TAppSettingIndex; Name: String;
  DefaultInt: Integer=0; DefaultBool: Boolean=False; DefaultString: String='';
  Session: Boolean=False);
begin
  FSettings[Index].Name := Name;
  FSettings[Index].Session := Session;
  FSettings[Index].DefaultInt := DefaultInt;
  FSettings[Index].DefaultBool := DefaultBool;
  FSettings[Index].DefaultString := DefaultString;
  FSettings[Index].Synced := False;
end;


procedure TAppSettings.SetSessionPath(Value: String);
begin
  // Following calls may want to read or write some session specific setting
  FSessionPath := Value;
  PrepareRegistry;
end;


procedure TAppSettings.ResetPath;
begin
  SessionPath := '';
end;


procedure TAppSettings.PrepareRegistry;
var
  Folder: String;
begin
  // Open the wanted registry path
  Folder := FBasePath;
  if FSessionPath <> '' then
    Folder := Folder + REGKEY_SESSIONS + '\' + FSessionPath;
  if '\'+FRegistry.CurrentPath <> Folder then
    FRegistry.OpenKey(Folder, True);
end;


function TAppSettings.GetValueNames: TStringList;
begin
  PrepareRegistry;
  Result := TStringList.Create;
  FRegistry.GetValueNames(Result);
end;


function TAppSettings.GetValueName(Index: TAppSettingIndex): String;
begin
  Result := FSettings[Index].Name;
end;


function TAppSettings.GetKeyNames: TStringList;
begin
  PrepareRegistry;
  Result := TStringList.Create;
  FRegistry.GetKeyNames(Result);
end;


function TAppSettings.DeleteValue(Index: TAppSettingIndex; FormatName: String=''): Boolean;
var
  ValueName: String;
begin
  PrepareRegistry;
  ValueName := GetValueName(Index);
  if FormatName <> '' then
    ValueName := Format(ValueName, [FormatName]);
  Result := FRegistry.DeleteValue(ValueName);
end;


function TAppSettings.DeleteValue(ValueName: String): Boolean;
begin
  Result := FRegistry.DeleteValue(ValueName);
end;


procedure TAppSettings.DeleteCurrentKey;
var
  KeyPath: String;
begin
  PrepareRegistry;
  if IsEmpty(FSessionPath) then
    raise Exception.Create('No path set, won''t delete root key '+FRegistry.CurrentPath)
  else begin
    KeyPath := REGKEY_SESSIONS + '\' + FSessionPath;
    ResetPath;
    FRegistry.DeleteKey(KeyPath);
  end;
end;


procedure TAppSettings.MoveCurrentKey(TargetPath: String);
var
  KeyPath: String;
begin
  PrepareRegistry;
  if IsEmpty(FSessionPath) then
    raise Exception.Create('No path set, won''t move root key '+FRegistry.CurrentPath)
  else begin
    KeyPath := REGKEY_SESSIONS + '\' + FSessionPath;
    ResetPath;
    FRegistry.MoveKey(KeyPath, TargetPath, True);
  end;
end;


function TAppSettings.ValueExists(Index: TAppSettingIndex): Boolean;
var
  ValueName: String;
begin
  PrepareRegistry;
  ValueName := GetValueName(Index);
  Result := FRegistry.ValueExists(ValueName);
end;


function TAppSettings.SessionPathExists(SessionPath: String): Boolean;
begin
  Result := FRegistry.KeyExists(FBasePath + REGKEY_SESSIONS + '\' + SessionPath);
end;


function TAppSettings.IsEmptyKey: Boolean;
var
  TestList: TStringList;
begin
  TestList := GetValueNames;
  Result := (not FRegistry.HasSubKeys) and (TestList.Count = 0);
  TestList.Free;
end;


function TAppSettings.GetDefaultInt(Index: TAppSettingIndex): Integer;
begin
  // Return default integer value
  Result := FSettings[Index].DefaultInt;
end;


function TAppSettings.GetDefaultString(Index: TAppSettingIndex): String;
begin
  // Return default string value
  Result := FSettings[Index].DefaultString;
end;


procedure TAppSettings.Read(Index: TAppSettingIndex; FormatName: String;
  DataType: TAppSettingDataType; var I: Integer; var B: Boolean; var S: String;
  DI: Integer; DB: Boolean; DS: String);
var
  ValueName: String;
begin
  // Read user setting value from registry
  I := FSettings[Index].DefaultInt;
  B := FSettings[Index].DefaultBool;
  S := FSettings[Index].DefaultString;
  if DI<>0 then I := DI;
  if DB<>False then B := DB;
  if DS<>'' then S := DS;
  ValueName := FSettings[Index].Name;
  if FormatName <> '' then
    ValueName := Format(ValueName, [FormatName]);
  if FSettings[Index].Session and IsEmpty(FSessionPath) then
    raise Exception.Create('Attempt to read session setting without session path');
  if (not FSettings[Index].Session) and IsNotEmpty(FSessionPath) then
    FSessionPath := '';
  PrepareRegistry;
  if FSettings[Index].Synced then begin
    case DataType of
      adInt: I := FSettings[Index].CurrentInt;
      adBool: B := FSettings[Index].CurrentBool;
      adString: S := FSettings[Index].CurrentString;
      else raise Exception.CreateFmt(SUnsupportedSettingsDatatype, [FSettings[Index].Name]);
    end;
  end else if FRegistry.ValueExists(ValueName) then begin
    Inc(FReads);
    case DataType of
      adInt: I := FRegistry.ReadInteger(ValueName);
      adBool: B := FRegistry.ReadBool(ValueName);
      adString: S := FRegistry.ReadString(ValueName);
      else raise Exception.CreateFmt(SUnsupportedSettingsDatatype, [FSettings[Index].Name]);
    end;
  end;
  if (FormatName = '') and (FSessionPath = '') then begin
    FSettings[Index].Synced := True;
    FSettings[Index].CurrentInt := I;
    FSettings[Index].CurrentBool := B;
    FSettings[Index].CurrentString := S;
  end;
end;


function TAppSettings.ReadInt(Index: TAppSettingIndex; FormatName: String=''; Default: Integer=0): Integer;
var
  S: String;
  B: Boolean;
begin
  Read(Index, FormatName, adInt, Result, B, S, Default, False, '');
end;


function TAppSettings.ReadBool(Index: TAppSettingIndex; FormatName: String=''; Default: Boolean=False): Boolean;
var
  I: Integer;
  S: String;
begin
  Read(Index, FormatName, adBool, I, Result, S, 0, Default, '');
end;


function TAppSettings.ReadString(Index: TAppSettingIndex; FormatName: String=''; Default: String=''): String;
var
  I: Integer;
  B: Boolean;
begin
  Read(Index, FormatName, adString, I, B, Result, 0, False, Default);
end;


function TAppSettings.ReadString(ValueName: String): String;
begin
  PrepareRegistry;
  Result := FRegistry.ReadString(ValueName);
end;


procedure TAppSettings.Write(Index: TAppSettingIndex; FormatName: String;
  DataType: TAppSettingDataType; I: Integer; B: Boolean; S: String);
var
  ValueName: String;
  SameAsDefault, SameAsCurrent: Boolean;
begin
  // Write user setting value to registry
  ValueName := FSettings[Index].Name;
  if FormatName <> '' then
    ValueName := Format(ValueName, [FormatName]);
  if FSettings[Index].Session and IsEmpty(FSessionPath) then
    raise Exception.Create('Attempt to write session setting without session path');
  if (not FSettings[Index].Session) and IsNotEmpty(FSessionPath) then
    FSessionPath := '';
  PrepareRegistry;
  case DataType of
    adInt: begin
      SameAsDefault := I = FSettings[Index].DefaultInt;
      SameAsCurrent := FSettings[Index].Synced and (I = FSettings[Index].CurrentInt);
      if (not SameAsDefault) and (not SameAsCurrent) then begin
        FRegistry.WriteInteger(ValueName, I);
        Inc(FWrites);
      end;
      FSettings[Index].CurrentInt := I;
    end;
    adBool:  begin
      SameAsDefault := B = FSettings[Index].DefaultBool;
      SameAsCurrent := FSettings[Index].Synced and (B = FSettings[Index].CurrentBool);
      if (not SameAsDefault) and (not SameAsCurrent) then begin
        FRegistry.WriteBool(ValueName, B);
        Inc(FWrites);
      end;
      FSettings[Index].CurrentBool := B;
    end;
    adString: begin
      SameAsDefault := S = FSettings[Index].DefaultString;
      SameAsCurrent := FSettings[Index].Synced and (S = FSettings[Index].CurrentString);
      if (not SameAsDefault) and (not SameAsCurrent) then begin
        FRegistry.WriteString(ValueName, S);
        Inc(FWrites);
      end;
      FSettings[Index].CurrentString := S;
    end;
    else
      raise Exception.CreateFmt(SUnsupportedSettingsDatatype, [FSettings[Index].Name]);
  end;
  if SameAsDefault and FRegistry.ValueExists(ValueName) then
    FRegistry.DeleteValue(ValueName);
  if (FormatName = '') and (FSessionPath = '') then
    FSettings[Index].Synced := True;
end;


procedure TAppSettings.WriteInt(Index: TAppSettingIndex; Value: Integer; FormatName: String='');
begin
  Write(Index, FormatName, adInt, Value, False, '');
end;


procedure TAppSettings.WriteBool(Index: TAppSettingIndex; Value: Boolean; FormatName: String='');
begin
  Write(Index, FormatName, adBool, 0, Value, '');
end;


procedure TAppSettings.WriteString(Index: TAppSettingIndex; Value: String; FormatName: String='');
begin
  Write(Index, FormatName, adString, 0, False, Value);
end;


procedure TAppSettings.WriteString(ValueName, Value: String);
begin
  PrepareRegistry;
  FRegistry.WriteString(ValueName, Value);
end;


function TAppSettings.GetSessionNames(ParentPath: String; var Folders: TStringList): TStringList;
var
  i: Integer;
  CurPath: String;
begin
  ResetPath;
  CurPath := FBasePath + REGKEY_SESSIONS + '\' + ParentPath;
  FRegistry.OpenKey(CurPath, False);
  Result := TStringList.Create;
  FRegistry.GetKeyNames(Result);
  for i:=Result.Count-1 downto 0 do begin
    FRegistry.OpenKey(CurPath+'\'+Result[i], False);
    if FRegistry.ValueExists(GetValueName(asSessionFolder)) then begin
      Folders.Add(Result[i]);
      Result.Delete(i);
    end;
  end;
end;


procedure TAppSettings.GetSessionPaths(ParentPath: String; var Sessions: TStringList);
var
  Folders, Names: TStringList;
  i: Integer;
begin
  Folders := TStringList.Create;
  Names := GetSessionNames(ParentPath, Folders);
  for i:=0 to Names.Count-1 do
    Sessions.Add(ParentPath+Names[i]);
  for i:=0 to Folders.Count-1 do
    GetSessionPaths(ParentPath+Folders[i]+'\', Sessions);
  Names.Free;
  Folders.Free;
end;


procedure TAppSettings.ImportSettings(Filename: String);
var
  Content, Name, Value, KeyPath: String;
  Lines, Segments: TStringList;
  i: Integer;
  DataType: TRegDataType;
begin
  // Load registry settings from file
  Content := ReadTextfile(FileName, nil);
  Lines := Explode(CRLF, Content);
  for i:=0 to Lines.Count-1 do begin
    // Each line has 3 segments: reg path | data type | value. Continue if explode finds less or more than 3.
    Segments := Explode(DELIMITER, Lines[i]);
    if Segments.Count <> 3 then
      continue;
    KeyPath := FBasePath + ExtractFilePath(Segments[0]);
    Name := ExtractFileName(Segments[0]);
    DataType := TRegDataType(StrToInt(Segments[1]));
    FRegistry.OpenKey(KeyPath, True);
    if FRegistry.ValueExists(Name) then
      Continue; // Don't touch value if already there
    Value := '';
    if Segments.Count >= 3 then
      Value := Segments[2];
    case DataType of
      rdString: begin
        Value := StringReplace(Value, CHR13REPLACEMENT, #13, [rfReplaceAll]);
        Value := StringReplace(Value, CHR10REPLACEMENT, #10, [rfReplaceAll]);
        FRegistry.WriteString(Name, Value);
      end;
      rdInteger:
        FRegistry.WriteInteger(Name, MakeInt(Value));
      rdBinary, rdUnknown, rdExpandString:
        ErrorDialog(Name+' has an unsupported data type.');
    end;
    Segments.Free;
  end;
  Lines.Free;
end;


procedure TAppSettings.ExportSettings(Filename: String);
var
  Content, Value: String;
  DataType: TRegDataType;

  procedure ReadKeyToContent(Path: String);
  var
    Names: TStringList;
    i: Integer;
    SubPath: String;
  begin
    // Recursively read values in keys and their subkeys into "content" variable
    FRegistry.OpenKey(Path, True);
    SubPath := Copy(Path, Length(FBasePath)+1, MaxInt);
    Names := TStringList.Create;
    FRegistry.GetValueNames(Names);
    for i:=0 to Names.Count-1 do begin
      DataType := FRegistry.GetDataType(Names[i]);
      Content := Content +
        SubPath + Names[i] + DELIMITER +
        IntToStr(Integer(DataType)) + DELIMITER;
      case DataType of
        rdString: begin
          Value := FRegistry.ReadString(Names[i]);
          Value := StringReplace(Value, #13, CHR13REPLACEMENT, [rfReplaceAll]);
          Value := StringReplace(Value, #10, CHR10REPLACEMENT, [rfReplaceAll]);
        end;
        rdInteger:
          Value := IntToStr(FRegistry.ReadInteger(Names[i]));
        rdBinary, rdUnknown, rdExpandString:
          ErrorDialog(Names[i]+' has an unsupported data type.');
      end;
      Content := Content + Value + CRLF;
    end;
    Names.Clear;
    FRegistry.GetKeyNames(Names);
    for i:=0 to Names.Count-1 do
      ReadKeyToContent(Path + Names[i] + '\');
    Names.Free;
  end;

begin
  // Save registry settings to file
  Content := '';
  ReadKeyToContent(FBasePath);
  SaveUnicodeFile(FileName, Content);
end;




end.


