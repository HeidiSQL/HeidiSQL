
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntSysUtils;

{$INCLUDE TntCompilers.inc}

interface

{ TODO: Consider: more filename functions from SysUtils }
{ TODO: Consider: string functions from StrUtils. }

uses
  Types, SysUtils, Windows;

//---------------------------------------------------------------------------------------------
//                                 Tnt - Types
//---------------------------------------------------------------------------------------------

// ......... introduced .........
type
  WideException = class(Exception)
  private
    FMessage: WideString;
    procedure SetMessage(const Value: WideString);
  public
    constructor Create(const Msg: WideString);
    constructor CreateFmt(const Msg: WideString; const Args: array of const);
    constructor CreateRes(Ident: Integer); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    constructor CreateResFmt(Ident: Integer; const Args: array of const); overload;
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const); overload;
    constructor CreateHelp(const Msg: WideString; AHelpContext: Integer);
    constructor CreateFmtHelp(const Msg: WideString; const Args: array of const;
      AHelpContext: Integer);
    constructor CreateResHelp(Ident: Integer; AHelpContext: Integer); overload;
    constructor CreateResHelp(ResStringRec: PResStringRec; AHelpContext: Integer); overload;
    constructor CreateResFmtHelp(ResStringRec: PResStringRec; const Args: array of const;
      AHelpContext: Integer); overload;
    constructor CreateResFmtHelp(Ident: Integer; const Args: array of const;
      AHelpContext: Integer); overload;
    property Message: WideString read FMessage write SetMessage;
  end;

  EWideOSError = class(WideException)
  public
    ErrorCode: DWORD;
  end;

  // The user of the application did something plainly wrong.
  ETntUserError = class(Exception);
  // A general error occured. (ie. file didn't exist, server didn't return data, etc.)
  ETntGeneralError = class(Exception);
  // Like Assert().  An error occured that should never have happened, send me a bug report now!
  ETntInternalError = class(Exception);

//---------------------------------------------------------------------------------------------
//                                 Tnt - SysUtils
//---------------------------------------------------------------------------------------------

// ......... SBCS and MBCS functions with WideString replacements in SysUtils.pas .........

{TNT-WARN CompareStr}                   {TNT-WARN AnsiCompareStr}
{TNT-WARN SameStr}                      {TNT-WARN AnsiSameStr}
{TNT-WARN SameText}                     {TNT-WARN AnsiSameText}
{TNT-WARN CompareText}                  {TNT-WARN AnsiCompareText}
{TNT-WARN UpperCase}                    {TNT-WARN AnsiUpperCase}
{TNT-WARN LowerCase}                    {TNT-WARN AnsiLowerCase}

{TNT-WARN AnsiPos}  { --> Pos() supports WideString. }
{TNT-WARN FmtStr}
{TNT-WARN Format}
{TNT-WARN FormatBuf}

// ......... MBCS Byte Type Procs .........

{TNT-WARN ByteType}
{TNT-WARN StrByteType}
{TNT-WARN ByteToCharIndex}
{TNT-WARN ByteToCharLen}
{TNT-WARN CharToByteIndex}
{TNT-WARN CharToByteLen}

// ........ null-terminated string functions .........

{TNT-WARN StrEnd}
{TNT-WARN StrLen}
{TNT-WARN StrLCopy}
{TNT-WARN StrCopy}
{TNT-WARN StrECopy}
{TNT-WARN StrPLCopy}
{TNT-WARN StrPCopy}
{TNT-WARN StrLComp}
{TNT-WARN AnsiStrLComp}
{TNT-WARN StrComp}
{TNT-WARN AnsiStrComp}
{TNT-WARN StrLIComp}
{TNT-WARN AnsiStrLIComp}
{TNT-WARN StrIComp}
{TNT-WARN AnsiStrIComp}
{TNT-WARN StrLower}
{TNT-WARN AnsiStrLower}
{TNT-WARN StrUpper}
{TNT-WARN AnsiStrUpper}
{TNT-WARN StrPos}
{TNT-WARN AnsiStrPos}
{TNT-WARN StrScan}
{TNT-WARN AnsiStrScan}
{TNT-WARN StrRScan}
{TNT-WARN AnsiStrRScan}
{TNT-WARN StrLCat}
{TNT-WARN StrCat}
{TNT-WARN StrMove}
{TNT-WARN StrPas}
{TNT-WARN StrAlloc}
{TNT-WARN StrBufSize}
{TNT-WARN StrNew}
{TNT-WARN StrDispose}

{TNT-WARN AnsiExtractQuotedStr}
{TNT-WARN AnsiLastChar}
{TNT-WARN AnsiStrLastChar}
{TNT-WARN QuotedStr}
{TNT-WARN AnsiQuotedStr}
{TNT-WARN AnsiDequotedStr}

// ........ string functions .........

{$IFNDEF COMPILER_9_UP}
  //
  // pre-Delphi 9 issues w/ WideFormatBuf, WideFmtStr and WideFormat
  //

  {$IFDEF COMPILER_7_UP}
  type
    PFormatSettings = ^TFormatSettings;
  {$ENDIF}

  // SysUtils.WideFormatBuf doesn't correctly handle numeric specifiers.
  function Tnt_WideFormatBuf(var Buffer; BufLen: Cardinal; const FormatStr;
    FmtLen: Cardinal; const Args: array of const): Cardinal; {$IFDEF COMPILER_7_UP} overload; {$ENDIF}

  {$IFDEF COMPILER_7_UP}
  function Tnt_WideFormatBuf(var Buffer; BufLen: Cardinal; const FormatStr;
    FmtLen: Cardinal; const Args: array of const;
      const FormatSettings: TFormatSettings): Cardinal; overload;
  {$ENDIF}

  // SysUtils.WideFmtStr doesn't handle string lengths > 4096.
  procedure Tnt_WideFmtStr(var Result: WideString; const FormatStr: WideString;
    const Args: array of const); {$IFDEF COMPILER_7_UP} overload; {$ENDIF}

  {$IFDEF COMPILER_7_UP}
  procedure Tnt_WideFmtStr(var Result: WideString; const FormatStr: WideString;
    const Args: array of const; const FormatSettings: TFormatSettings); overload;
  {$ENDIF}

  {----------------------------------------------------------------------------------------
    Without the FormatSettings parameter, Tnt_WideFormat is *NOT* necessary...
      TntSystem.InstallTntSystemUpdates([tsFixWideFormat]);
        will fix WideFormat as well as WideFmtStr.
  ----------------------------------------------------------------------------------------}
  function Tnt_WideFormat(const FormatStr: WideString; const Args: array of const): WideString; {$IFDEF COMPILER_7_UP} overload; {$ENDIF}

  {$IFDEF COMPILER_7_UP}
  function Tnt_WideFormat(const FormatStr: WideString; const Args: array of const;
    const FormatSettings: TFormatSettings): WideString; overload;
  {$ENDIF}

{$ENDIF}

function WideLoadStr(Ident: Integer): WideString;
function WideFmtLoadStr(Ident: Integer; const Args: array of const): WideString;

{TNT-WARN WideUpperCase} // SysUtils.WideUpperCase is broken on Win9x for D6, D7, D9.
function Tnt_WideUpperCase(const S: WideString): WideString;
{TNT-WARN WideLowerCase} // SysUtils.WideLowerCase is broken on Win9x for D6, D7, D9.
function Tnt_WideLowerCase(const S: WideString): WideString;

function TntWideLastChar(const S: WideString): WideChar;

{TNT-WARN StringReplace}
{TNT-WARN WideStringReplace} // <-- WideStrUtils.WideStringReplace uses SysUtils.WideUpperCase which is broken on Win9x.
function Tnt_WideStringReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags; WholeWord: Boolean = False): WideString;

{TNT-WARN AdjustLineBreaks}
type TTntTextLineBreakStyle = (tlbsLF, tlbsCRLF, tlbsCR);
function TntAdjustLineBreaksLength(const S: WideString; Style: TTntTextLineBreakStyle = tlbsCRLF): Integer;
function TntAdjustLineBreaks(const S: WideString; Style: TTntTextLineBreakStyle = tlbsCRLF): WideString;

{TNT-WARN WrapText}
function WideWrapText(const Line, BreakStr: WideString; const BreakChars: TSysCharSet;
  MaxCol: Integer): WideString; overload;
function WideWrapText(const Line: WideString; MaxCol: Integer): WideString; overload;

// ........ filename manipulation .........

{TNT-WARN SameFileName}           // doesn't apply to Unicode filenames, use WideSameText
{TNT-WARN AnsiCompareFileName}    // doesn't apply to Unicode filenames, use WideCompareText
{TNT-WARN AnsiLowerCaseFileName}  // doesn't apply to Unicode filenames, use WideLowerCase
{TNT-WARN AnsiUpperCaseFileName}  // doesn't apply to Unicode filenames, use WideUpperCase

{TNT-WARN IncludeTrailingBackslash}
function WideIncludeTrailingBackslash(const S: WideString): WideString;
{TNT-WARN IncludeTrailingPathDelimiter}
function WideIncludeTrailingPathDelimiter(const S: WideString): WideString;
{TNT-WARN ExcludeTrailingBackslash}
function WideExcludeTrailingBackslash(const S: WideString): WideString;
{TNT-WARN ExcludeTrailingPathDelimiter}
function WideExcludeTrailingPathDelimiter(const S: WideString): WideString;
{TNT-WARN IsDelimiter}
function WideIsDelimiter(const Delimiters, S: WideString; Index: Integer): Boolean;
{TNT-WARN IsPathDelimiter}
function WideIsPathDelimiter(const S: WideString; Index: Integer): Boolean;
{TNT-WARN LastDelimiter}
function WideLastDelimiter(const Delimiters, S: WideString): Integer;
{TNT-WARN ChangeFileExt}
function WideChangeFileExt(const FileName, Extension: WideString): WideString;
{TNT-WARN ExtractFilePath}
function WideExtractFilePath(const FileName: WideString): WideString;
{TNT-WARN ExtractFileDir}
function WideExtractFileDir(const FileName: WideString): WideString;
{TNT-WARN ExtractFileDrive}
function WideExtractFileDrive(const FileName: WideString): WideString;
{TNT-WARN ExtractFileName}
function WideExtractFileName(const FileName: WideString): WideString;
{TNT-WARN ExtractFileExt}
function WideExtractFileExt(const FileName: WideString): WideString;
{TNT-WARN ExtractRelativePath}
function WideExtractRelativePath(const BaseName, DestName: WideString): WideString;

// ........ file management routines .........

{TNT-WARN ExpandFileName}
function WideExpandFileName(const FileName: WideString): WideString;
{TNT-WARN ExtractShortPathName}
function WideExtractShortPathName(const FileName: WideString): WideString;
{TNT-WARN FileCreate}
function WideFileCreate(const FileName: WideString): Integer;
{TNT-WARN FileOpen}
function WideFileOpen(const FileName: WideString; Mode: LongWord): Integer;
{TNT-WARN FileAge}
function WideFileAge(const FileName: WideString): Integer; overload;
function WideFileAge(const FileName: WideString; out FileDateTime: TDateTime): Boolean; overload;
{TNT-WARN DirectoryExists}
function WideDirectoryExists(const Name: WideString): Boolean;
{TNT-WARN FileExists}
function WideFileExists(const Name: WideString): Boolean;
{TNT-WARN FileGetAttr}
function WideFileGetAttr(const FileName: WideString): Cardinal;
{TNT-WARN FileSetAttr}
function WideFileSetAttr(const FileName: WideString; Attr: Integer): Boolean;
{TNT-WARN FileIsReadOnly}
function WideFileIsReadOnly(const FileName: WideString): Boolean;
{TNT-WARN FileSetReadOnly}
function WideFileSetReadOnly(const FileName: WideString; ReadOnly: Boolean): Boolean;
{TNT-WARN ForceDirectories}
function WideForceDirectories(Dir: WideString): Boolean;
{TNT-WARN FileSearch}
function WideFileSearch(const Name, DirList: WideString): WideString;
{TNT-WARN RenameFile}
function WideRenameFile(const OldName, NewName: WideString): Boolean;
{TNT-WARN DeleteFile}
function WideDeleteFile(const FileName: WideString): Boolean;
{TNT-WARN CopyFile}
function WideCopyFile(FromFile, ToFile: WideString; FailIfExists: Boolean): Boolean;


{TNT-WARN TFileName}
type
  TWideFileName = type WideString;

{TNT-WARN TSearchRec} // <-- FindFile - warning on TSearchRec is all that is necessary
type
  TSearchRecW = record
    Time: Integer;
    Size: Int64;
    Attr: Integer;
    Name: TWideFileName;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWin32FindDataW;
  end;
function WideFindFirst(const Path: WideString; Attr: Integer; var F: TSearchRecW): Integer;
function WideFindNext(var F: TSearchRecW): Integer;
procedure WideFindClose(var F: TSearchRecW);

{TNT-WARN CreateDir}
function WideCreateDir(const Dir: WideString): Boolean;
{TNT-WARN RemoveDir}
function WideRemoveDir(const Dir: WideString): Boolean;
{TNT-WARN GetCurrentDir}
function WideGetCurrentDir: WideString;
{TNT-WARN SetCurrentDir}
function WideSetCurrentDir(const Dir: WideString): Boolean;


// ........ date/time functions .........

{TNT-WARN TryStrToDateTime}
function TntTryStrToDateTime(Str: WideString; out DateTime: TDateTime): Boolean;
{TNT-WARN TryStrToDate}
function TntTryStrToDate(Str: WideString; out DateTime: TDateTime): Boolean;
{TNT-WARN TryStrToTime}
function TntTryStrToTime(Str: WideString; out DateTime: TDateTime): Boolean;

{ introduced }
function ValidDateTimeStr(Str: WideString): Boolean;
function ValidDateStr(Str: WideString): Boolean;
function ValidTimeStr(Str: WideString): Boolean;

{TNT-WARN StrToDateTime}
function TntStrToDateTime(Str: WideString): TDateTime;
{TNT-WARN StrToDate}
function TntStrToDate(Str: WideString): TDateTime;
{TNT-WARN StrToTime}
function TntStrToTime(Str: WideString): TDateTime;
{TNT-WARN StrToDateTimeDef}
function TntStrToDateTimeDef(Str: WideString; Default: TDateTime): TDateTime;
{TNT-WARN StrToDateDef}
function TntStrToDateDef(Str: WideString; Default: TDateTime): TDateTime;
{TNT-WARN StrToTimeDef}
function TntStrToTimeDef(Str: WideString; Default: TDateTime): TDateTime;

{TNT-WARN CurrToStr}
{TNT-WARN CurrToStrF}
function TntCurrToStr(Value: Currency; lpFormat: PCurrencyFmtW = nil): WideString;
{TNT-WARN StrToCurr}
function TntStrToCurr(const S: WideString): Currency;
{TNT-WARN StrToCurrDef}
function ValidCurrencyStr(const S: WideString): Boolean;
function TntStrToCurrDef(const S: WideString; const Default: Currency): Currency;
function GetDefaultCurrencyFmt: TCurrencyFmtW;

// ........ misc functions .........

{TNT-WARN GetLocaleStr}
function WideGetLocaleStr(LocaleID: LCID; LocaleType: Integer; const Default: WideString): WideString;
{TNT-WARN SysErrorMessage}
function WideSysErrorMessage(ErrorCode: Integer): WideString;
procedure WideRaiseLastOSError;

// ......... introduced .........

function WideLibraryErrorMessage(const LibName: WideString; Dll: THandle; ErrorCode: Integer): WideString;

const
  CR = WideChar(#13);
  LF = WideChar(#10);
  CRLF = WideString(#13#10);
  WideLineSeparator = WideChar($2028);

var
  Win32PlatformIsUnicode: Boolean;
  Win32PlatformIsXP: Boolean;
  Win32PlatformIs2003: Boolean;
  Win32PlatformIsVista: Boolean;

{$IFNDEF COMPILER_7_UP}
function CheckWin32Version(AMajor: Integer; AMinor: Integer = 0): Boolean;
{$ENDIF}
function WinCheckH(RetVal: Cardinal): Cardinal;
function WinCheckFileH(RetVal: Cardinal): Cardinal;
function WinCheckP(RetVal: Pointer): Pointer;

function WideGetModuleFileName(Instance: HModule): WideString;
function WideSafeLoadLibrary(const Filename: Widestring;
  ErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE;
function WideLoadPackage(const Name: Widestring): HMODULE;

function IsWideCharUpper(WC: WideChar): Boolean;
function IsWideCharLower(WC: WideChar): Boolean;
function IsWideCharDigit(WC: WideChar): Boolean;
function IsWideCharSpace(WC: WideChar): Boolean;
function IsWideCharPunct(WC: WideChar): Boolean;
function IsWideCharCntrl(WC: WideChar): Boolean;
function IsWideCharBlank(WC: WideChar): Boolean;
function IsWideCharXDigit(WC: WideChar): Boolean;
function IsWideCharAlpha(WC: WideChar): Boolean;
function IsWideCharAlphaNumeric(WC: WideChar): Boolean;

function WideTextPos(const SubStr, S: WideString): Integer;

function ExtractStringArrayStr(P: PWideChar): WideString;
function ExtractStringFromStringArray(var P: PWideChar; Separator: WideChar = #0): WideString;
function ExtractStringsFromStringArray(P: PWideChar; Separator: WideChar = #0): TWideStringDynArray;

function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
function IsWideStringMappableToAnsi(const WS: WideString): Boolean;
function IsRTF(const Value: WideString): Boolean;

function ENG_US_FloatToStr(Value: Extended): WideString;
function ENG_US_StrToFloat(const S: WideString): Extended;

//---------------------------------------------------------------------------------------------
//                                 Tnt - Variants
//---------------------------------------------------------------------------------------------

// ........ Variants.pas has WideString versions of these functions .........
{TNT-WARN VarToStr}
{TNT-WARN VarToStrDef}

var
  _SettingChangeTime: Cardinal;

implementation

uses
  ActiveX, ComObj, SysConst,
  {$IFDEF COMPILER_9_UP} WideStrUtils, {$ENDIF} TntWideStrUtils,
  TntSystem, TntWindows, TntFormatStrUtils;

//---------------------------------------------------------------------------------------------
//                                 Tnt - Types
//---------------------------------------------------------------------------------------------

{ WideException }

constructor WideException.Create(const Msg: WideString);
begin
  FMessage := Msg;
  inherited Message := FMessage;
end;

constructor WideException.CreateFmt(const Msg: WideString;
  const Args: array of const);
begin
  FMessage := WideFormat(Msg, Args);
  inherited Message := FMessage;
end;

constructor WideException.CreateRes(Ident: Integer);
begin
  FMessage := WideLoadStr(Ident);
  inherited Message := FMessage;
end;

constructor WideException.CreateRes(ResStringRec: PResStringRec);
begin
  FMessage := WideLoadResString(ResStringRec);
  inherited Message := FMessage;
end;

constructor WideException.CreateResFmt(Ident: Integer;
  const Args: array of const);
begin
  FMessage := WideFormat(WideLoadStr(Ident), Args);
  inherited Message := FMessage;
end;

constructor WideException.CreateResFmt(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  FMessage := WideFormat(WideLoadResString(ResStringRec), Args);
  inherited Message := FMessage;
end;

constructor WideException.CreateHelp(const Msg: WideString;
  AHelpContext: Integer);
begin
  FMessage := Msg;
  inherited Message := FMessage;
  inherited HelpContext := AHelpContext;
end;

constructor WideException.CreateFmtHelp(const Msg: WideString;
  const Args: array of const; AHelpContext: Integer);
begin
  FMessage := WideFormat(Msg, Args);
  inherited Message := FMessage;
  inherited HelpContext := AHelpContext;
end;

constructor WideException.CreateResHelp(Ident: Integer; AHelpContext: Integer);
begin
  FMessage := WideLoadStr(Ident);
  inherited Message := FMessage;
  inherited HelpContext := AHelpContext;
end;

constructor WideException.CreateResHelp(ResStringRec: PResStringRec;
  AHelpContext: Integer);
begin
  FMessage := WideLoadResString(ResStringRec);
  inherited Message := FMessage;
  inherited HelpContext := AHelpContext;
end;

constructor WideException.CreateResFmtHelp(Ident: Integer;
  const Args: array of const; AHelpContext: Integer);
begin
  FMessage := WideFormat(WideLoadStr(Ident), Args);
  inherited Message := FMessage;
  inherited HelpContext := AHelpContext;
end;

constructor WideException.CreateResFmtHelp(ResStringRec: PResStringRec;
  const Args: array of const; AHelpContext: Integer);
begin
  FMessage := WideFormat(WideLoadResString(ResStringRec), Args);
  inherited Message := FMessage;
  inherited HelpContext := AHelpContext;  
end;

procedure WideException.SetMessage(const Value: WideString);
begin
  FMessage := Value;
  inherited Message := FMessage;
end;

//---------------------------------------------------------------------------------------------
//                                 Tnt - SysUtils
//---------------------------------------------------------------------------------------------

{$IFNDEF COMPILER_9_UP}

  function _Tnt_WideFormatBuf(var Buffer; BufLen: Cardinal; const FormatStr;
    FmtLen: Cardinal; const Args: array of const
      {$IFDEF COMPILER_7_UP}; const FormatSettings: PFormatSettings {$ENDIF}): Cardinal;
  var
    OldFormat: WideString;
    NewFormat: WideString;
  begin
    SetString(OldFormat, PWideChar(@FormatStr), FmtLen);
    { The reason for this is that WideFormat doesn't correctly format floating point specifiers.
      See QC#4254. }
    NewFormat := ReplaceFloatingArgumentsInFormatString(OldFormat, Args{$IFDEF COMPILER_7_UP}, FormatSettings{$ENDIF});
    {$IFDEF COMPILER_7_UP}
    if FormatSettings <> nil then
      Result := WideFormatBuf(Buffer, BufLen, Pointer(NewFormat)^,
        Length(NewFormat), Args, FormatSettings^)
    else
    {$ENDIF}
      Result := WideFormatBuf(Buffer, BufLen, Pointer(NewFormat)^,
        Length(NewFormat), Args);
  end;

  function Tnt_WideFormatBuf(var Buffer; BufLen: Cardinal; const FormatStr;
    FmtLen: Cardinal; const Args: array of const): Cardinal;
  begin
    Result := _Tnt_WideFormatBuf(Buffer, BufLen, FormatStr, FmtLen, Args{$IFDEF COMPILER_7_UP}, nil{$ENDIF});
  end;

  {$IFDEF COMPILER_7_UP}
  function Tnt_WideFormatBuf(var Buffer; BufLen: Cardinal; const FormatStr;
    FmtLen: Cardinal; const Args: array of const; const FormatSettings: TFormatSettings): Cardinal;
  begin
    Result := _Tnt_WideFormatBuf(Buffer, BufLen, FormatStr, FmtLen, Args, @FormatSettings);
  end;
  {$ENDIF}

  procedure _Tnt_WideFmtStr(var Result: WideString; const FormatStr: WideString;
    const Args: array of const{$IFDEF COMPILER_7_UP}; const FormatSettings: PFormatSettings{$ENDIF});
  var
    Len, BufLen: Integer;
    Buffer: array[0..4095] of WideChar;
  begin
    BufLen := Length(Buffer); // Fixes buffer overwrite issue. (See QC #4703, #4744)
    if Length(FormatStr) < (Length(Buffer) - (Length(Buffer) div 4)) then
      Len := _Tnt_WideFormatBuf(Buffer, Length(Buffer) - 1, Pointer(FormatStr)^,
        Length(FormatStr), Args{$IFDEF COMPILER_7_UP}, FormatSettings{$ENDIF})
    else
    begin
      BufLen := Length(FormatStr);
      Len := BufLen;
    end;
    if Len >= BufLen - 1 then
    begin
      while Len >= BufLen - 1 do
      begin
        Inc(BufLen, BufLen);
        Result := '';          // prevent copying of existing data, for speed
        SetLength(Result, BufLen);
        Len := _Tnt_WideFormatBuf(Pointer(Result)^, BufLen - 1, Pointer(FormatStr)^,
          Length(FormatStr), Args{$IFDEF COMPILER_7_UP}, FormatSettings{$ENDIF});
      end;
      SetLength(Result, Len);
    end
    else
      SetString(Result, Buffer, Len);
  end;

  procedure Tnt_WideFmtStr(var Result: WideString; const FormatStr: WideString;
    const Args: array of const);
  begin
    _Tnt_WideFmtStr(Result, FormatStr, Args{$IFDEF COMPILER_7_UP}, nil{$ENDIF});
  end;

  {$IFDEF COMPILER_7_UP}
  procedure Tnt_WideFmtStr(var Result: WideString; const FormatStr: WideString;
    const Args: array of const; const FormatSettings: TFormatSettings);
  begin
    _Tnt_WideFmtStr(Result, FormatStr, Args, @FormatSettings);
  end;
  {$ENDIF}

  {----------------------------------------------------------------------------------------
    Without the FormatSettings parameter, Tnt_WideFormat is *NOT* necessary...
      TntSystem.InstallTntSystemUpdates([tsFixWideFormat]);
        will fix WideFormat as well as WideFmtStr.
  ----------------------------------------------------------------------------------------}
  function Tnt_WideFormat(const FormatStr: WideString; const Args: array of const): WideString;
  begin
    Tnt_WideFmtStr(Result, FormatStr, Args);
  end;

  {$IFDEF COMPILER_7_UP}
  function Tnt_WideFormat(const FormatStr: WideString; const Args: array of const;
    const FormatSettings: TFormatSettings): WideString;
  begin
    Tnt_WideFmtStr(Result, FormatStr, Args, FormatSettings);
  end;
  {$ENDIF}

{$ENDIF}

type
  PWStrData = ^TWStrData;
  TWStrData = record
    Ident: Integer;
    Str: WideString;
  end;

function EnumStringModules(Instance: Longint; Data: Pointer): Boolean;
var
  ResStringRec: TResStringRec;
begin
  with PWStrData(Data)^ do
  begin
    ResStringRec.Module^ := Instance;
    ResStringRec.Identifier := Ident;
    Str := WideLoadResString(@ResStringRec);
    Result := Str = '';
  end;
end;

function WideFindStringResource(Ident: Integer): WideString;
var
  StrData: TWStrData;
begin
  StrData.Ident := Ident;
  StrData.Str := '';
  EnumResourceModules(EnumStringModules, @StrData);
  Result := StrData.Str;
end;

function WideLoadStr(Ident: Integer): WideString;
begin
  Result := WideFindStringResource(Ident);
end;

function WideFmtLoadStr(Ident: Integer; const Args: array of const): WideString;
begin
  WideFmtStr(Result, WideFindStringResource(Ident), Args);
end;

function Tnt_WideUpperCase(const S: WideString): WideString;
begin
  {$IFNDEF COMPILER_10_UP}
  { SysUtils.WideUpperCase is broken for Win9x. }
  Result := S;
  if Length(Result) > 0 then
    Tnt_CharUpperBuffW(PWideChar(Result), Length(Result));
  {$ELSE}
  Result := SysUtils.WideUpperCase{TNT-ALLOW WideUpperCase}(S);
  {$ENDIF}
end;

function Tnt_WideLowerCase(const S: WideString): WideString;
begin
  {$IFNDEF COMPILER_10_UP}
  { SysUtils.WideLowerCase is broken for Win9x. }
  Result := S;
  if Length(Result) > 0 then
    Tnt_CharLowerBuffW(PWideChar(Result), Length(Result));
  {$ELSE}
  Result := SysUtils.WideLowerCase{TNT-ALLOW WideLowerCase}(S);
  {$ENDIF}
end;

function TntWideLastChar(const S: WideString): WideChar;
var
  P: PWideChar;
begin
  P := WideLastChar(S);
  if P = nil then
    Result := #0
  else
    Result := P^;
end;

function Tnt_WideStringReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags; WholeWord: Boolean = False): WideString;

  function IsWordSeparator(WC: WideChar): Boolean;
  begin
    Result := (WC = WideChar(#0))
           or IsWideCharSpace(WC)
           or IsWideCharPunct(WC);
  end;

var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
  PrevChar, NextChar: WideChar;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := Tnt_WideUpperCase(S);
    Patt := Tnt_WideUpperCase(OldPattern);
  end else
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
    end; // done

    if (WholeWord) then
    begin
      if (Offset = 1) then
        PrevChar := TntWideLastChar(Result)
      else
        PrevChar := NewStr[Offset - 1];

      if Offset + Length(OldPattern) <= Length(NewStr) then
        NextChar := NewStr[Offset + Length(OldPattern)]
      else
        NextChar := WideChar(#0);

      if (not IsWordSeparator(PrevChar))
      or (not IsWordSeparator(NextChar)) then
      begin
        Result := Result + Copy(NewStr, 1, Offset + Length(OldPattern) - 1);
        NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
        SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
        continue;
      end;
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

function TntAdjustLineBreaksLength(const S: WideString; Style: TTntTextLineBreakStyle = tlbsCRLF): Integer;
var
  Source, SourceEnd: PWideChar;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);
  Result := Length(S);
  while Source < SourceEnd do
  begin
    case Source^ of
      #10, WideLineSeparator:
        if Style = tlbsCRLF then
          Inc(Result);
      #13:
        if Style = tlbsCRLF then
          if Source[1] = #10 then
            Inc(Source)
          else
            Inc(Result)
        else
          if Source[1] = #10 then
            Dec(Result);
    end;
    Inc(Source);
  end;
end;

function TntAdjustLineBreaks(const S: WideString; Style: TTntTextLineBreakStyle = tlbsCRLF): WideString;
var
  Source, SourceEnd, Dest: PWideChar;
  DestLen: Integer;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);
  DestLen := TntAdjustLineBreaksLength(S, Style);
  SetString(Result, nil, DestLen);
  Dest := Pointer(Result);
  while Source < SourceEnd do begin
    case Source^ of
      #10, WideLineSeparator:
        begin
          if Style in [tlbsCRLF, tlbsCR] then
          begin
            Dest^ := #13;
            Inc(Dest);
          end;
          if Style in [tlbsCRLF, tlbsLF] then
          begin
            Dest^ := #10;
            Inc(Dest);
          end;
          Inc(Source);
        end;
      #13:
        begin
          if Style in [tlbsCRLF, tlbsCR] then
          begin
            Dest^ := #13;
            Inc(Dest);
          end;
          if Style in [tlbsCRLF, tlbsLF] then
          begin
            Dest^ := #10;
            Inc(Dest);
          end;
          Inc(Source);
          if Source^ = #10 then Inc(Source);
        end;
    else
      Dest^ := Source^;
      Inc(Dest);
      Inc(Source);
    end;
  end;
end;

function WideWrapText(const Line, BreakStr: WideString; const BreakChars: TSysCharSet;
  MaxCol: Integer): WideString;

  function WideCharIn(C: WideChar; SysCharSet: TSysCharSet): Boolean;
  begin
    Result := (C <= High(AnsiChar)) and (AnsiChar(C) in SysCharSet);
  end;

const
  QuoteChars = ['''', '"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: WideChar;
  ExistingBreak: Boolean;
begin
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := ' ';
  ExistingBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';
  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if CurChar = BreakStr[1] then
    begin
      if QuoteChar = ' ' then
      begin
        ExistingBreak := WideSameText(BreakStr, Copy(Line, Pos, BreakLen));
        if ExistingBreak then
        begin
          Inc(Pos, BreakLen-1);
          BreakPos := Pos;
        end;
      end
    end
    else if WideCharIn(CurChar, BreakChars) then
    begin
      if QuoteChar = ' ' then BreakPos := Pos
    end
    else if WideCharIn(CurChar, QuoteChars) then
    begin
      if CurChar = QuoteChar then
        QuoteChar := ' '
      else if QuoteChar = ' ' then
        QuoteChar := CurChar;
    end;
    Inc(Pos);
    Inc(Col);
    if not (WideCharIn(QuoteChar, QuoteChars)) and (ExistingBreak or
      ((Col > MaxCol) and (BreakPos > LinePos))) then
    begin
      Col := Pos - BreakPos;
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      if not (WideCharIn(CurChar, QuoteChars)) then
        while Pos <= LineLen do
        begin
          if WideCharIn(Line[Pos], BreakChars) then
            Inc(Pos)
          else if Copy(Line, Pos, Length(sLineBreak)) = sLineBreak then
            Inc(Pos, Length(sLineBreak))
          else
            break;
        end;
      if not ExistingBreak and (Pos < LineLen) then
        Result := Result + BreakStr;
      Inc(BreakPos);
      LinePos := BreakPos;
      ExistingBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

function WideWrapText(const Line: WideString; MaxCol: Integer): WideString;
begin
  Result := WideWrapText(Line, sLineBreak, [' ', '-', #9], MaxCol); { do not localize }
end;

function WideIncludeTrailingBackslash(const S: WideString): WideString;
begin
  Result := WideIncludeTrailingPathDelimiter(S);
end;

function WideIncludeTrailingPathDelimiter(const S: WideString): WideString;
begin
  Result := S;
  if not WideIsPathDelimiter(Result, Length(Result)) then Result := Result + PathDelim;
end;

function WideExcludeTrailingBackslash(const S: WideString): WideString;
begin
  Result := WideExcludeTrailingPathDelimiter(S);
end;

function WideExcludeTrailingPathDelimiter(const S: WideString): WideString;
begin
  Result := S;
  if WideIsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function WideIsDelimiter(const Delimiters, S: WideString; Index: Integer): Boolean;
begin
  Result := False;
  if (Index <= 0) or (Index > Length(S)) then exit;
  Result := WStrScan(PWideChar(Delimiters), S[Index]) <> nil;
end;

function WideIsPathDelimiter(const S: WideString; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = PathDelim);
end;

function WideLastDelimiter(const Delimiters, S: WideString): Integer;
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

function WideChangeFileExt(const FileName, Extension: WideString): WideString;
var
  I: Integer;
begin
  I := WideLastDelimiter('.\:',Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function WideExtractFilePath(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := WideLastDelimiter('\:', FileName);
  Result := Copy(FileName, 1, I);
end;

function WideExtractFileDir(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := WideLastDelimiter(DriveDelim + PathDelim,Filename);
  if (I > 1) and (FileName[I] = PathDelim) and
    (not (FileName[I - 1] in [WideChar(PathDelim), WideChar(DriveDelim)])) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function WideExtractFileDrive(const FileName: WideString): WideString;
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = DriveDelim) then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = PathDelim) and
    (FileName[2] = PathDelim) then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = PathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = PathDelim then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;

function WideExtractFileName(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := WideLastDelimiter('\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function WideExtractFileExt(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := WideLastDelimiter('.\:', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function WideExtractRelativePath(const BaseName, DestName: WideString): WideString;
var
  BasePath, DestPath: WideString;
  BaseLead, DestLead: PWideChar;
  BasePtr, DestPtr: PWideChar;

  function WideExtractFilePathNoDrive(const FileName: WideString): WideString;
  begin
    Result := WideExtractFilePath(FileName);
    Delete(Result, 1, Length(WideExtractFileDrive(FileName)));
  end;

  function Next(var Lead: PWideChar): PWideChar;
  begin
    Result := Lead;
    if Result = nil then Exit;
    Lead := WStrScan(Lead, PathDelim);
    if Lead <> nil then
    begin
      Lead^ := #0;
      Inc(Lead);
    end;
  end;

begin
  if WideSameText(WideExtractFileDrive(BaseName), WideExtractFileDrive(DestName)) then
  begin
    BasePath := WideExtractFilePathNoDrive(BaseName);
    DestPath := WideExtractFilePathNoDrive(DestName);
    BaseLead := Pointer(BasePath);
    BasePtr := Next(BaseLead);
    DestLead := Pointer(DestPath);
    DestPtr := Next(DestLead);
    while (BasePtr <> nil) and (DestPtr <> nil) and WideSameText(BasePtr, DestPtr) do
    begin
      BasePtr := Next(BaseLead);
      DestPtr := Next(DestLead);
    end;
    Result := '';
    while BaseLead <> nil do
    begin
      Result := Result + '..' + PathDelim;             { Do not localize }
      Next(BaseLead);
    end;
    if (DestPtr <> nil) and (DestPtr^ <> #0) then
      Result := Result + DestPtr + PathDelim;
    if DestLead <> nil then
      Result := Result + DestLead;     // destlead already has a trailing backslash
    Result := Result + WideExtractFileName(DestName);
  end
  else
    Result := DestName;
end;

function WideExpandFileName(const FileName: WideString): WideString;
var
  FName: PWideChar;
  Buffer: array[0..MAX_PATH - 1] of WideChar;
begin
  SetString(Result, Buffer, Tnt_GetFullPathNameW(PWideChar(FileName), MAX_PATH, Buffer, FName));
end;

function WideExtractShortPathName(const FileName: WideString): WideString;
var
  Buffer: array[0..MAX_PATH - 1] of WideChar;
begin
  SetString(Result, Buffer, Tnt_GetShortPathNameW(PWideChar(FileName), Buffer, MAX_PATH));
end;

function WideFileCreate(const FileName: WideString): Integer;
begin
  Result := Integer(Tnt_CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0))
end;

function WideFileOpen(const FileName: WideString; Mode: LongWord): Integer;
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
  Result := Integer(Tnt_CreateFileW(PWideChar(FileName), AccessMode[Mode and 3],
    ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0));
end;

function WideFileAge(const FileName: WideString): Integer;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
  LocalFileTime: TFileTime;
begin
  Handle := Tnt_FindFirstFileW(PWideChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi, LongRec(Result).Lo) then
        Exit
    end;
  end;
  Result := -1;
end;

function WideFileAge(const FileName: WideString; out FileDateTime: TDateTime): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
  LSystemTime: TSystemTime;
  LocalFileTime: TFileTime;
begin
  Result := False;
  Handle := Tnt_FindFirstFileW(PWideChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Result := True;
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      FileTimeToSystemTime(LocalFileTime, LSystemTime);
      with LSystemTime do
        FileDateTime := EncodeDate(wYear, wMonth, wDay) +
          EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
    end;
  end;
end;

function WideDirectoryExists(const Name: WideString): Boolean;
var
  Code: Cardinal;
begin
  Code := WideFileGetAttr(Name);
  Result := (Code <> INVALID_FILE_ATTRIBUTES) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function WideFileExists(const Name: WideString): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
begin
  Result := False;
  Handle := Tnt_FindFirstFileW(PWideChar(Name), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      Result := True;
  end;
end;

function WideFileGetAttr(const FileName: WideString): Cardinal;
begin
  Result := Tnt_GetFileAttributesW(PWideChar(FileName));
end;

function WideFileSetAttr(const FileName: WideString; Attr: Integer): Boolean;
begin
  Result := Tnt_SetFileAttributesW(PWideChar(FileName), Attr)
end;

function WideFileIsReadOnly(const FileName: WideString): Boolean;
begin
  Result := (Tnt_GetFileAttributesW(PWideChar(FileName)) and faReadOnly) <> 0;
end;

function WideFileSetReadOnly(const FileName: WideString; ReadOnly: Boolean): Boolean;
var
  Flags: Integer;
begin
  Result := False;
  Flags := Tnt_GetFileAttributesW(PWideChar(FileName));
  if Flags = -1 then Exit;
  if ReadOnly then
    Flags := Flags or faReadOnly
  else
    Flags := Flags and not faReadOnly;
  Result := Tnt_SetFileAttributesW(PWideChar(FileName), Flags);
end;

function WideForceDirectories(Dir: WideString): Boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    raise ETntGeneralError.Create(SCannotCreateDir);
  Dir := WideExcludeTrailingBackslash(Dir);
  if (Length(Dir) < 3) or WideDirectoryExists(Dir)
    or (WideExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  Result := WideForceDirectories(WideExtractFilePath(Dir));
  if Result then
    Result := Tnt_CreateDirectoryW(PWideChar(Dir), nil)
end;

function WideFileSearch(const Name, DirList: WideString): WideString;
var
  I, P, L: Integer;
  C: WideChar;
begin
  Result := Name;
  P := 1;
  L := Length(DirList);
  while True do
  begin
    if WideFileExists(Result) then Exit;
    while (P <= L) and (DirList[P] = PathSep) do Inc(P);
    if P > L then Break;
    I := P;
    while (P <= L) and (DirList[P] <> PathSep) do
      Inc(P);
    Result := Copy(DirList, I, P - I);
    C := TntWideLastChar(Result);
    if (C <> DriveDelim) and (C <> PathDelim) then
      Result := Result + PathDelim;
    Result := Result + Name;
  end;
  Result := '';
end;

function WideRenameFile(const OldName, NewName: WideString): Boolean;
begin
  Result := Tnt_MoveFileW(PWideChar(OldName), PWideChar(NewName))
end;

function WideDeleteFile(const FileName: WideString): Boolean;
begin
  Result := Tnt_DeleteFileW(PWideChar(FileName))
end;

function WideCopyFile(FromFile, ToFile: WideString; FailIfExists: Boolean): Boolean;
begin
  Result := Tnt_CopyFileW(PWideChar(FromFile), PWideChar(ToFile), FailIfExists)
end;

function _WideFindMatchingFile(var F: TSearchRecW): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not Tnt_FindNextFileW(FindHandle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
    Size := (Int64(FindData.nFileSizeHigh) shl 32) + FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;

function WideFindFirst(const Path: WideString; Attr: Integer; var F: TSearchRecW): Integer;
const
  faSpecial = faHidden or faSysFile {$IFNDEF COMPILER_9_UP} or faVolumeID {$ENDIF} or faDirectory;
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := Tnt_FindFirstFileW(PWideChar(Path), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := _WideFindMatchingFile(F);
    if Result <> 0 then WideFindClose(F);
  end else
    Result := GetLastError;
end;

function WideFindNext(var F: TSearchRecW): Integer;
begin
  if Tnt_FindNextFileW(F.FindHandle, F.FindData) then
    Result := _WideFindMatchingFile(F) else
    Result := GetLastError;
end;

procedure WideFindClose(var F: TSearchRecW);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

function WideCreateDir(const Dir: WideString): Boolean;
begin
  Result := Tnt_CreateDirectoryW(PWideChar(Dir), nil);
end;

function WideRemoveDir(const Dir: WideString): Boolean;
begin
  Result := Tnt_RemoveDirectoryW(PWideChar(Dir));
end;

function WideGetCurrentDir: WideString;
begin
  SetLength(Result, MAX_PATH);
  Tnt_GetCurrentDirectoryW(MAX_PATH, PWideChar(Result));
  Result := PWideChar(Result);
end;

function WideSetCurrentDir(const Dir: WideString): Boolean;
begin
  Result := Tnt_SetCurrentDirectoryW(PWideChar(Dir));
end;

//=============================================================================================
//==  DATE/TIME STRING PARSING ================================================================
//=============================================================================================

function _IntTryStrToDateTime(Str: WideString; Flags: Integer; out DateTime: TDateTime): HResult;
begin
  Result := VarDateFromStr(Str, GetThreadLocale, Flags, Double(DateTime));
  if (not Succeeded(Result)) then begin
    if (Flags = VAR_TIMEVALUEONLY)
    and SysUtils.TryStrToTime{TNT-ALLOW TryStrToTime}(Str, DateTime) then
      Result := S_OK // SysUtils seems confident (works for date = "dd.MM.yy" and time = "H.mm.ss")
    else if (Flags = VAR_DATEVALUEONLY)
    and SysUtils.TryStrToDate{TNT-ALLOW TryStrToDate}(Str, DateTime) then
      Result := S_OK // SysUtils seems confident
    else if (Flags = 0)
    and SysUtils.TryStrToDateTime{TNT-ALLOW TryStrToDateTime}(Str, DateTime) then
      Result := S_OK // SysUtils seems confident
  end;
end;

function TntTryStrToDateTime(Str: WideString; out DateTime: TDateTime): Boolean;
begin
  Result := Succeeded(_IntTryStrToDateTime(Str, 0, DateTime));
end;

function TntTryStrToDate(Str: WideString; out DateTime: TDateTime): Boolean;
begin
  Result := Succeeded(_IntTryStrToDateTime(Str, VAR_DATEVALUEONLY, DateTime));
end;

function TntTryStrToTime(Str: WideString; out DateTime: TDateTime): Boolean;
begin
  Result := Succeeded(_IntTryStrToDateTime(Str, VAR_TIMEVALUEONLY, DateTime));
end;

function ValidDateTimeStr(Str: WideString): Boolean;
var
  Temp: TDateTime;
begin
  Result := Succeeded(_IntTryStrToDateTime(Str, 0, Temp));
end;

function ValidDateStr(Str: WideString): Boolean;
var
  Temp: TDateTime;
begin
  Result := Succeeded(_IntTryStrToDateTime(Str, VAR_DATEVALUEONLY, Temp));
end;

function ValidTimeStr(Str: WideString): Boolean;
var
  Temp: TDateTime;
begin
  Result := Succeeded(_IntTryStrToDateTime(Str, VAR_TIMEVALUEONLY, Temp));
end;

function TntStrToDateTimeDef(Str: WideString; Default: TDateTime): TDateTime;
begin
  if not TntTryStrToDateTime(Str, Result) then
    Result := Default;
end;

function TntStrToDateDef(Str: WideString; Default: TDateTime): TDateTime;
begin
  if not TntTryStrToDate(Str, Result) then
    Result := Default;
end;

function TntStrToTimeDef(Str: WideString; Default: TDateTime): TDateTime;
begin
  if not TntTryStrToTime(Str, Result) then
    Result := Default;
end;

function _IntStrToDateTime(Str: WideString; Flags: Integer; ErrorFormatStr: WideString): TDateTime;
begin
  try
    OleCheck(_IntTryStrToDateTime(Str, Flags, Result));
  except
    on E: Exception do begin
      E.Message := E.Message + CRLF + WideFormat(ErrorFormatStr, [Str]);
      raise EConvertError.Create(E.Message);
    end;
  end;
end;

function TntStrToDateTime(Str: WideString): TDateTime;
begin
  Result := _IntStrToDateTime(Str, 0, SInvalidDateTime);
end;

function TntStrToDate(Str: WideString): TDateTime;
begin
  Result := _IntStrToDateTime(Str, VAR_DATEVALUEONLY, SInvalidDate);
end;

function TntStrToTime(Str: WideString): TDateTime;
begin
  Result := _IntStrToDateTime(Str, VAR_TIMEVALUEONLY, SInvalidTime);
end;

//=============================================================================================
//==  CURRENCY STRING PARSING =================================================================
//=============================================================================================

function TntCurrToStr(Value: Currency; lpFormat: PCurrencyFmtW = nil): WideString;
const
  MAX_BUFF_SIZE = 64; // can a currency string actually be larger?
var
  ValueStr: WideString;
begin
  // format lpValue using ENG-US settings
  ValueStr := ENG_US_FloatToStr(Value);
  // get currency format
  SetLength(Result, MAX_BUFF_SIZE);
  if 0 = Tnt_GetCurrencyFormatW(GetThreadLocale, 0, PWideChar(ValueStr),
    lpFormat, PWideChar(Result), Length(Result))
  then begin
    RaiseLastOSError;
  end;
  Result := PWideChar(Result);
end;

function TntStrToCurr(const S: WideString): Currency;
begin
  try
    OleCheck(VarCyFromStr(S, GetThreadLocale, 0, Result));
  except
    on E: Exception do begin
      E.Message := E.Message + CRLF + WideFormat(SInvalidCurrency, [S]);
      raise EConvertError.Create(E.Message);
    end;
  end;
end;

function ValidCurrencyStr(const S: WideString): Boolean;
var
  Dummy: Currency;
begin
  Result := Succeeded(VarCyFromStr(S, GetThreadLocale, 0, Dummy));
end;

function TntStrToCurrDef(const S: WideString; const Default: Currency): Currency;
begin
  if not Succeeded(VarCyFromStr(S, GetThreadLocale, 0, Result)) then
    Result := Default;
end;

threadvar
  Currency_DecimalSep: WideString;
  Currency_ThousandSep: WideString;
  Currency_CurrencySymbol: WideString;

function GetDefaultCurrencyFmt: TCurrencyFmtW;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.NumDigits := StrToIntDef(WideGetLocaleStr(GetThreadLocale, LOCALE_ICURRDIGITS, '2'), 2);
  Result.LeadingZero := StrToIntDef(WideGetLocaleStr(GetThreadLocale, LOCALE_ILZERO, '1'), 1);
  Result.Grouping := StrToIntDef(Copy(WideGetLocaleStr(GetThreadLocale, LOCALE_SMONGROUPING, '3;0'), 1, 1), 3);
  Currency_DecimalSep := WideGetLocaleStr(GetThreadLocale, LOCALE_SMONDECIMALSEP, '.');
  Result.lpDecimalSep := PWideChar(Currency_DecimalSep);
  Currency_ThousandSep := WideGetLocaleStr(GetThreadLocale, LOCALE_SMONTHOUSANDSEP, ',');
  Result.lpThousandSep := PWideChar(Currency_ThousandSep);
  Result.NegativeOrder := StrToIntDef(WideGetLocaleStr(GetThreadLocale, LOCALE_INEGCURR, '0'), 0);
  Result.PositiveOrder := StrToIntDef(WideGetLocaleStr(GetThreadLocale, LOCALE_ICURRENCY, '0'), 0);
  Currency_CurrencySymbol := WideGetLocaleStr(GetThreadLocale, LOCALE_SCURRENCY, '');
  Result.lpCurrencySymbol := PWideChar(Currency_CurrencySymbol);
end;

//=============================================================================================

function WideGetLocaleStr(LocaleID: LCID; LocaleType: Integer; const Default: WideString): WideString;
var
  L: Integer;
begin
  if (not Win32PlatformIsUnicode) then
    Result := GetLocaleStr{TNT-ALLOW GetLocaleStr}(LocaleID, LocaleType, Default)
  else begin
    SetLength(Result, 255);
    L := GetLocaleInfoW(LocaleID, LocaleType, PWideChar(Result), Length(Result));
    if L > 0 then
      SetLength(Result, L - 1)
    else
      Result := Default;
  end;
end;

function WideSysErrorMessage(ErrorCode: Integer): WideString;
begin
  Result := WideLibraryErrorMessage('system', 0, ErrorCode);
end;

procedure WideRaiseLastOSError;
var
  LastError: Integer;
  Error: EWideOSError;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    Error := EWideOSError.Create(WideSysErrorMessage(LastError))
  else
    Error := EWideOSError.CreateRes(PResStringRec(@SUnkOSError));
  Error.ErrorCode := LastError;
  raise Error;
end;

function WideLibraryErrorMessage(const LibName: WideString; Dll: THandle; ErrorCode: Integer): WideString;
var
  Len: Integer;
  AnsiResult: AnsiString;
  Flags: Cardinal;
begin
  Flags := FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY;
  if Dll <> 0 then
    Flags := Flags or FORMAT_MESSAGE_FROM_HMODULE;
  if Win32PlatformIsUnicode then begin
    SetLength(Result, 256);
    Len := FormatMessageW(Flags, Pointer(Dll), ErrorCode, 0, PWideChar(Result), Length(Result), nil);
    SetLength(Result, Len);
  end else begin
    SetLength(AnsiResult, 256);
    Len := FormatMessageA(Flags, Pointer(Dll), ErrorCode, 0, PAnsiChar(AnsiResult), Length(AnsiResult), nil);
    SetLength(AnsiResult, Len);
    Result := AnsiResult;
  end;
  if Trim(Result) = '' then
    Result := WideFormat('Unspecified error (%d) from %s.', [ErrorCode, LibName]);
end;

{$IFNDEF COMPILER_7_UP}
function CheckWin32Version(AMajor: Integer; AMinor: Integer = 0): Boolean;
begin
  Result := (Win32MajorVersion > AMajor) or
            ((Win32MajorVersion = AMajor) and
             (Win32MinorVersion >= AMinor));
end;
{$ENDIF}

function WinCheckH(RetVal: Cardinal): Cardinal;
begin
  if RetVal = 0 then RaiseLastOSError;
  Result := RetVal;
end;

function WinCheckFileH(RetVal: Cardinal): Cardinal;
begin
  if RetVal = INVALID_HANDLE_VALUE then RaiseLastOSError;
  Result := RetVal;
end;

function WinCheckP(RetVal: Pointer): Pointer;
begin
  if RetVal = nil then RaiseLastOSError;
  Result := RetVal;
end;

function WideGetModuleFileName(Instance: HModule): WideString;
begin
  SetLength(Result, MAX_PATH);
  WinCheckH(Tnt_GetModuleFileNameW(Instance, PWideChar(Result), Length(Result)));
  Result := PWideChar(Result)
end;

function WideSafeLoadLibrary(const Filename: Widestring; ErrorMode: UINT): HMODULE;
var
  OldMode: UINT;
  FPUControlWord: Word;
begin
  OldMode := SetErrorMode(ErrorMode);
  try
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Result := Tnt_LoadLibraryW(PWideChar(Filename));
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    SetErrorMode(OldMode);
  end;
end;

function WideLoadPackage(const Name: Widestring): HMODULE;
begin
  Result := WideSafeLoadLibrary(Name);
  if Result = 0 then
  begin
    raise EPackageError.CreateFmt(sErrorLoadingPackage, [Name, WideSysErrorMessage(GetLastError)]);
  end;
  try
    InitializePackage(Result);
  except
    FreeLibrary(Result);
    raise;
  end;
end;

function _WideCharType(WC: WideChar; dwInfoType: Cardinal): Word;
begin
  Win32Check(Tnt_GetStringTypeExW(GetThreadLocale, dwInfoType, PWideChar(@WC), 1, Result))
end;

function IsWideCharUpper(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_UPPER) <> 0;
end;

function IsWideCharLower(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_LOWER) <> 0;
end;

function IsWideCharDigit(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_DIGIT) <> 0;
end;

function IsWideCharSpace(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_SPACE) <> 0;
end;

function IsWideCharPunct(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_PUNCT) <> 0;
end;

function IsWideCharCntrl(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_CNTRL) <> 0;
end;

function IsWideCharBlank(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_BLANK) <> 0;
end;

function IsWideCharXDigit(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_XDIGIT) <> 0;
end;

function IsWideCharAlpha(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_ALPHA) <> 0;
end;

function IsWideCharAlphaNumeric(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and (C1_ALPHA + C1_DIGIT)) <> 0;
end;

function WideTextPos(const SubStr, S: WideString): Integer;
begin
  Result := Pos(Tnt_WideUpperCase(SubStr), Tnt_WideUpperCase(S));
end;

function FindDoubleTerminator(P: PWideChar): PWideChar;
begin
  Result := P;
  while True do begin
    Result := WStrScan(Result, #0);
    Inc(Result);
    if Result^ = #0 then begin
      Dec(Result);
      break;
    end;
  end;
end;

function ExtractStringArrayStr(P: PWideChar): WideString;
var
  PEnd: PWideChar;
begin
  PEnd := FindDoubleTerminator(P);
  Inc(PEnd, 2); // move past #0#0
  SetString(Result, P, PEnd - P);
end;

function ExtractStringFromStringArray(var P: PWideChar; Separator: WideChar = #0): WideString;
var
  Start: PWideChar;
begin
  Start := P;
  P := WStrScan(Start, Separator);
  if P = nil then begin
    Result := Start;
    P := WStrEnd(Start);
  end else begin
    SetString(Result, Start, P - Start);
    Inc(P);
  end;
end;

function ExtractStringsFromStringArray(P: PWideChar; Separator: WideChar = #0): TWideStringDynArray;
const
  GROW_COUNT = 256;
var
  Count: Integer;
  Item: WideString;
begin
  Count := 0;
  SetLength(Result, GROW_COUNT);
  Item := ExtractStringFromStringArray(P, Separator);
  While Item <> '' do begin
    if Count > High(Result) then
      SetLength(Result, Length(Result) + GROW_COUNT);
    Result[Count] := Item;
    Inc(Count);
    Item := ExtractStringFromStringArray(P, Separator);
  end;
  SetLength(Result, Count);
end;

function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(@WC), 1, nil, 0, nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

function IsWideStringMappableToAnsi(const WS: WideString): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(WS), Length(WS), nil, 0, nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

function IsRTF(const Value: WideString): Boolean;
const
  RTF_BEGIN_1  = WideString('{\RTF');
  RTF_BEGIN_2  = WideString('{URTF');
begin
  Result := (WideTextPos(RTF_BEGIN_1, Value) = 1)
         or (WideTextPos(RTF_BEGIN_2, Value) = 1);
end;

{$IFDEF COMPILER_7_UP}
var
  Cached_ENG_US_FormatSettings: TFormatSettings;
  Cached_ENG_US_FormatSettings_Time: Cardinal;

function ENG_US_FormatSettings: TFormatSettings;
begin
  if Cached_ENG_US_FormatSettings_Time = _SettingChangeTime then
    Result := Cached_ENG_US_FormatSettings
  else begin
    GetLocaleFormatSettings(MAKELCID(MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US)), Result);
    Result.DecimalSeparator := '.'; // ignore overrides
    Cached_ENG_US_FormatSettings := Result;
    Cached_ENG_US_FormatSettings_Time := _SettingChangeTime;
  end;
 end;

function ENG_US_FloatToStr(Value: Extended): WideString;
begin
  Result := FloatToStr(Value, ENG_US_FormatSettings);
end;

function ENG_US_StrToFloat(const S: WideString): Extended;
begin
  if not TextToFloat(PAnsiChar(AnsiString(S)), Result, fvExtended, ENG_US_FormatSettings) then
    Result := StrToFloat(S); // try using native format
end;

{$ELSE}

function ENG_US_FloatToStr(Value: Extended): WideString;
var
  SaveDecimalSep: AnsiChar;
begin
  SaveDecimalSep := SysUtils.DecimalSeparator;
  try
    SysUtils.DecimalSeparator := '.';
    Result := FloatToStr(Value);
  finally
    SysUtils.DecimalSeparator := SaveDecimalSep;
  end;
end;

function ENG_US_StrToFloat(const S: WideString): Extended;
var
  SaveDecimalSep: AnsiChar;
begin
  try
    SaveDecimalSep := SysUtils.DecimalSeparator;
    try
      SysUtils.DecimalSeparator := '.';
      Result := StrToFloat(S);
    finally
      SysUtils.DecimalSeparator := SaveDecimalSep;
    end;
  except
    if SysUtils.DecimalSeparator <> '.' then
      Result := StrToFloat(S) // try using native format
    else
      raise;
  end;
end;
{$ENDIF}

//---------------------------------------------------------------------------------------------
//                                 Tnt - Variants
//---------------------------------------------------------------------------------------------

initialization
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);
  Win32PlatformIsXP := ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))
                    or  (Win32MajorVersion > 5);
  Win32PlatformIs2003 := ((Win32MajorVersion = 5) and (Win32MinorVersion >= 2))
                    or  (Win32MajorVersion > 5);
  Win32PlatformIsVista := (Win32MajorVersion >= 6);

finalization
  Currency_DecimalSep := ''; {make memory sleuth happy}
  Currency_ThousandSep := ''; {make memory sleuth happy}
  Currency_CurrencySymbol := ''; {make memory sleuth happy}

end.
