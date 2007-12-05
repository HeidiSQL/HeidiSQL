
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntDialogs;

{$INCLUDE compilers.inc}

interface

{ TODO: TFindDialog and TReplaceDialog. }
{ TODO: Property editor for TTntOpenDialog.Filter }

uses
  Classes, Messages, CommDlg, Windows, Dialogs,
  TntClasses, TntForms, TntSysUtils;

type
{TNT-WARN TIncludeItemEvent}
  TIncludeItemEventW = procedure (const OFN: TOFNotifyExW; var Include: Boolean) of object;

{TNT-WARN TOpenDialog}
  TTntOpenDialog = class(TOpenDialog{TNT-ALLOW TOpenDialog})
  private
    FDefaultExt: WideString;
    FFileName: TWideFileName;
    FFilter: WideString;
    FInitialDir: WideString;
    FTitle: WideString;
    FFiles: TTntStrings;
    FOnIncludeItem: TIncludeItemEventW;
    function GetDefaultExt: WideString;
    procedure SetInheritedDefaultExt(const Value: AnsiString);
    procedure SetDefaultExt(const Value: WideString);
    function GetFileName: TWideFileName;
    procedure SetFileName(const Value: TWideFileName);
    function GetFilter: WideString;
    procedure SetInheritedFilter(const Value: AnsiString);
    procedure SetFilter(const Value: WideString);
    function GetInitialDir: WideString;
    procedure SetInheritedInitialDir(const Value: AnsiString);
    procedure SetInitialDir(const Value: WideString);
    function GetTitle: WideString;
    procedure SetInheritedTitle(const Value: AnsiString);
    procedure SetTitle(const Value: WideString);
    function GetFiles: TTntStrings;
  private
    FProxiedOpenFilenameA: TOpenFilenameA;
  protected
    FAllowDoCanClose: Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    function CanCloseW(var OpenFileName: TOpenFileNameW): Boolean;
    function DoCanClose: Boolean; override;
    procedure GetFileNamesW(var OpenFileName: TOpenFileNameW);
    procedure DoIncludeItem(const OFN: TOFNotifyEx; var Include: Boolean); override;
    procedure WndProc(var Message: TMessage); override;
    function DoExecuteW(Func: Pointer; ParentWnd: HWND): Bool; overload;
    function DoExecuteW(Func: Pointer): Bool; overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    {$IFDEF COMPILER_9_UP}
    function Execute(ParentWnd: HWND): Boolean; override;
    {$ENDIF}
    property Files: TTntStrings read GetFiles;
  published
    property DefaultExt: WideString read GetDefaultExt write SetDefaultExt;
    property FileName: TWideFileName read GetFileName write SetFileName;
    property Filter: WideString read GetFilter write SetFilter;
    property InitialDir: WideString read GetInitialDir write SetInitialDir;
    property Title: WideString read GetTitle write SetTitle;
    property OnIncludeItem: TIncludeItemEventW read FOnIncludeItem write FOnIncludeItem;
  end;

{TNT-WARN TSaveDialog}
  TTntSaveDialog = class(TTntOpenDialog)
  public
    function Execute: Boolean; override;
    {$IFDEF COMPILER_9_UP}
    function Execute(ParentWnd: HWND): Boolean; override;
    {$ENDIF}
  end;

{ Message dialog }

{TNT-WARN CreateMessageDialog}
function WideCreateMessageDialog(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TTntForm;overload;
function WideCreateMessageDialog(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn): TTntForm; overload;

{TNT-WARN MessageDlg}
function WideMessageDlg(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
function WideMessageDlg(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

{TNT-WARN MessageDlgPos}
function WideMessageDlgPos(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; overload;
function WideMessageDlgPos(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; DefaultButton: TMsgDlgBtn): Integer; overload;

{TNT-WARN MessageDlgPosHelp}
function WideMessageDlgPosHelp(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: WideString): Integer; overload;
function WideMessageDlgPosHelp(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: WideString; DefaultButton: TMsgDlgBtn): Integer; overload;

{TNT-WARN ShowMessage}
procedure WideShowMessage(const Msg: WideString);
{TNT-WARN ShowMessageFmt}
procedure WideShowMessageFmt(const Msg: WideString; Params: array of const);
{TNT-WARN ShowMessagePos}
procedure WideShowMessagePos(const Msg: WideString; X, Y: Integer);

{ Input dialog }

{TNT-WARN InputQuery}
function WideInputQuery(const ACaption, APrompt: WideString;
   var Value: WideString): Boolean;
{TNT-WARN InputBox}
function WideInputBox(const ACaption, APrompt, ADefault: WideString): WideString;

{TNT-WARN PromptForFileName}
function WidePromptForFileName(var AFileName: WideString; const AFilter: WideString = '';
  const ADefaultExt: WideString = ''; const ATitle: WideString = '';
  const AInitialDir: WideString = ''; SaveDialog: Boolean = False): Boolean;

function GetModalParentWnd: HWND;

implementation

uses
  Controls, Forms, Types, SysUtils, Graphics, Consts, Math,
  TntWindows, TntStdCtrls, TntClipBrd, TntExtCtrls,
  {$IFDEF COMPILER_9_UP} WideStrUtils, {$ENDIF} TntWideStrUtils;

function GetModalParentWnd: HWND;
begin
  {$IFDEF COMPILER_9}
  Result := Application.ActiveFormHandle;
  {$ELSE}
  Result := 0;
  {$ENDIF}
  {$IFDEF COMPILER_10_UP}
  if Application.ModalPopupMode <> pmNone then
  begin
    Result := Application.ActiveFormHandle;
  end;
  {$ENDIF}
  if Result = 0 then begin
    Result := Application.Handle;
  end;
end;

var
  ProxyExecuteDialog: TTntOpenDialog;

function ProxyGetOpenFileNameA(var OpenFile: TOpenFilename): Bool; stdcall;
begin
  ProxyExecuteDialog.FProxiedOpenFilenameA := OpenFile;
  Result := False; { as if user hit "Cancel". }
end;

{ TTntOpenDialog }

constructor TTntOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FFiles := TTntStringList.Create;
end;

destructor TTntOpenDialog.Destroy;
begin
  FreeAndNil(FFiles);
  inherited;
end;

procedure TTntOpenDialog.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntOpenDialog.GetDefaultExt: WideString;
begin
  Result := GetSyncedWideString(FDefaultExt, inherited DefaultExt);
end;

procedure TTntOpenDialog.SetInheritedDefaultExt(const Value: AnsiString);
begin
  inherited DefaultExt := Value;
end;

procedure TTntOpenDialog.SetDefaultExt(const Value: WideString);
begin
  SetSyncedWideString(Value, FDefaultExt, inherited DefaultExt, SetInheritedDefaultExt);
end;

function TTntOpenDialog.GetFileName: TWideFileName;
var
  Path: array[0..MAX_PATH] of WideChar;
begin
  if Win32PlatformIsUnicode and NewStyleControls and (Handle <> 0) then begin
    // get filename from handle
    SendMessageW(GetParent(Handle), CDM_GETFILEPATH, SizeOf(Path), Integer(@Path));
    Result := Path;
  end else
    Result := GetSyncedWideString(WideString(FFileName), inherited FileName);
end;

procedure TTntOpenDialog.SetFileName(const Value: TWideFileName);
begin
  FFileName := Value;
  inherited FileName := Value;
end;

function TTntOpenDialog.GetFilter: WideString;
begin
  Result := GetSyncedWideString(FFilter, inherited Filter);
end;

procedure TTntOpenDialog.SetInheritedFilter(const Value: AnsiString);
begin
  inherited Filter := Value;
end;

procedure TTntOpenDialog.SetFilter(const Value: WideString);
begin
  SetSyncedWideString(Value, FFilter, inherited Filter, SetInheritedFilter);
end;

function TTntOpenDialog.GetInitialDir: WideString;
begin
  Result := GetSyncedWideString(FInitialDir, inherited InitialDir);
end;

procedure TTntOpenDialog.SetInheritedInitialDir(const Value: AnsiString);
begin
  inherited InitialDir := Value;
end;

procedure TTntOpenDialog.SetInitialDir(const Value: WideString);

  function RemoveTrailingPathDelimiter(const Value: WideString): WideString;
  var
    L: Integer;
  begin
    // remove trailing path delimiter (except 'C:\')
    L := Length(Value);
    if (L > 1) and WideIsPathDelimiter(Value, L) and not WideIsDelimiter(':', Value, L - 1) then
      Dec(L);
    Result := Copy(Value, 1, L);
  end;

begin
  SetSyncedWideString(RemoveTrailingPathDelimiter(Value), FInitialDir,
    inherited InitialDir, SetInheritedInitialDir);
end;

function TTntOpenDialog.GetTitle: WideString;
begin
  Result := GetSyncedWideString(FTitle, inherited Title)
end;

procedure TTntOpenDialog.SetInheritedTitle(const Value: AnsiString);
begin
  inherited Title := Value;
end;

procedure TTntOpenDialog.SetTitle(const Value: WideString);
begin
  SetSyncedWideString(Value, FTitle, inherited Title, SetInheritedTitle);
end;

function TTntOpenDialog.GetFiles: TTntStrings;
begin
  if (not Win32PlatformIsUnicode) then
    FFiles.Assign(inherited Files);
  Result := FFiles;
end;

function TTntOpenDialog.DoCanClose: Boolean;
begin
  if FAllowDoCanClose then
    Result := inherited DoCanClose
  else
    Result := True;
end;

function TTntOpenDialog.CanCloseW(var OpenFileName: TOpenFileNameW): Boolean;
begin
  GetFileNamesW(OpenFileName);
  FAllowDoCanClose := True;
  try
    Result := DoCanClose;
  finally
    FAllowDoCanClose := False;
  end;
  FFiles.Clear;
  inherited Files.Clear;
end;

procedure TTntOpenDialog.DoIncludeItem(const OFN: TOFNotifyEx; var Include: Boolean);
begin
  // CDN_INCLUDEITEM -> DoIncludeItem() is only be available on Windows 2000 +
  // Therefore, just cast OFN as a TOFNotifyExW, since that's what it really is.
  if Win32PlatformIsUnicode and Assigned(FOnIncludeItem) then
    FOnIncludeItem(TOFNotifyExW(OFN), Include)
end;

procedure TTntOpenDialog.WndProc(var Message: TMessage);
begin
  Message.Result := 0;
  if (Message.Msg = WM_INITDIALOG) and not (ofOldStyleDialog in Options) then begin
    { If not ofOldStyleDialog then DoShow on CDN_INITDONE, not WM_INITDIALOG }
    Exit;
  end;
  if Win32PlatformIsUnicode
  and (Message.Msg = WM_NOTIFY) then begin
    case (POFNotify(Message.LParam)^.hdr.code) of
      CDN_FILEOK:
        if not CanCloseW(POFNotifyW(Message.LParam)^.lpOFN^) then
        begin
          Message.Result := 1;
          SetWindowLong(Handle, DWL_MSGRESULT, Message.Result);
          Exit;
        end;
    end;
  end;
  inherited WndProc(Message);
end;

function TTntOpenDialog.DoExecuteW(Func: Pointer): Bool;
begin
  Result := DoExecuteW(Func, GetModalParentWnd);
end;

function TTntOpenDialog.DoExecuteW(Func: Pointer; ParentWnd: HWND): Bool;
var
  OpenFilename: TOpenFilenameW;

  function GetResNamePtr(var ScopedStringStorage: WideString; lpszName: PAnsiChar): PWideChar;
  // duplicated from TntTrxResourceUtils.pas
  begin
    if Tnt_Is_IntResource(PWideChar(lpszName)) then
      Result := PWideChar(lpszName)
    else begin
      ScopedStringStorage := lpszName;
      Result := PWideChar(ScopedStringStorage);
    end;
  end;

  function AllocFilterStr(const S: WideString): WideString;
  var
    P: PWideChar;
  begin
    Result := '';
    if S <> '' then
    begin
      Result := S + #0#0;  // double null terminators (an additional zero added in case Description/Filter pair not even.)
      P := WStrScan(PWideChar(Result), '|');
      while P <> nil do
      begin
        P^ := #0;
        Inc(P);
        P := WStrScan(P, '|');
      end;
    end;
  end;

var
  TempTemplate, TempFilter, TempFilename, TempExt: WideString;
begin
  FFiles.Clear;

  // 1. Init inherited dialog defaults.
  // 2. Populate OpenFileName record with ansi defaults
  ProxyExecuteDialog := Self;
  try
    DoExecute(@ProxyGetOpenFileNameA);
  finally
    ProxyExecuteDialog := nil;
  end;
  OpenFileName := TOpenFilenameW(FProxiedOpenFilenameA);

  with OpenFilename do
  begin
    if not IsWindow(hWndOwner) then begin
      hWndOwner := ParentWnd;
    end;
    // Filter (PChar -> PWideChar)
    TempFilter := AllocFilterStr(Filter);
    lpstrFilter := PWideChar(TempFilter);
    // FileName (PChar -> PWideChar)
    SetLength(TempFilename, nMaxFile + 2);
    lpstrFile := PWideChar(TempFilename);
    FillChar(lpstrFile^, (nMaxFile + 2) * SizeOf(WideChar), 0);
    WStrLCopy(lpstrFile, PWideChar(FileName), nMaxFile);
    // InitialDir (PChar -> PWideChar)
    if (InitialDir = '') and ForceCurrentDirectory then
      lpstrInitialDir := '.'
    else
      lpstrInitialDir := PWideChar(InitialDir);
    // Title (PChar -> PWideChar)
    lpstrTitle := PWideChar(Title);
    // DefaultExt (PChar -> PWideChar)
    TempExt := DefaultExt;
    if (TempExt = '') and (Flags and OFN_EXPLORER = 0) then
    begin
      TempExt := WideExtractFileExt(Filename);
      Delete(TempExt, 1, 1);
    end;
    if TempExt <> '' then
      lpstrDefExt := PWideChar(TempExt);
    // resource template (PChar -> PWideChar)
    lpTemplateName := GetResNamePtr(TempTemplate, Template);
    // start modal dialog
    Result := TaskModalDialog(Func, OpenFileName);
    if Result then
    begin
      GetFileNamesW(OpenFilename);
      if (Flags and OFN_EXTENSIONDIFFERENT) <> 0 then
        Options := Options + [ofExtensionDifferent]
      else
        Options := Options - [ofExtensionDifferent];
      if (Flags and OFN_READONLY) <> 0 then
        Options := Options + [ofReadOnly]
      else
        Options := Options - [ofReadOnly];
      FilterIndex := nFilterIndex;
    end;
  end;
end;

procedure TTntOpenDialog.GetFileNamesW(var OpenFileName: TOpenFileNameW);
var
  Separator: WideChar;

  procedure ExtractFileNamesW(P: PWideChar);
  var
    DirName, FileName: TWideFileName;
    FileList: TWideStringDynArray;
    i: integer;
  begin
    FileList := ExtractStringsFromStringArray(P, Separator);
    if Length(FileList) = 0 then 
      FFiles.Add('')
    else begin
      DirName := FileList[0];
      if Length(FileList) = 1 then
        FFiles.Add(DirName)
      else begin
        // prepare DirName
        if WideLastChar(DirName) <> WideString(PathDelim) then
          DirName := DirName + PathDelim;
        // add files
        for i := 1 {second item} to High(FileList) do begin
          FileName := FileList[i];
          // prepare FileName
          if (FileName[1] <> PathDelim)
          and ((Length(FileName) <= 3) or (FileName[2] <> DriveDelim) or (FileName[3] <> PathDelim))
          then
            FileName := DirName + FileName;
          // add to list
          FFiles.Add(FileName);
        end;
      end;
    end;
  end;

var
  P: PWideChar;
begin
  Separator := #0;
  if (ofAllowMultiSelect in Options) and
    ((ofOldStyleDialog in Options) or not NewStyleControls) then
    Separator := ' ';
  with OpenFileName do
  begin
    if ofAllowMultiSelect in Options then
    begin
      ExtractFileNamesW(lpstrFile);
      FileName := FFiles[0];
    end else
    begin
      P := lpstrFile;
      FileName := ExtractStringFromStringArray(P, Separator);
      FFiles.Add(FileName);
    end;
  end;

  // Sync inherited Files
  inherited Files.Assign(FFiles);
end;

function TTntOpenDialog.Execute: Boolean;
begin
  if (not Win32PlatformIsUnicode) then
    Result := DoExecute(@GetOpenFileNameA)
  else
    Result := DoExecuteW(@GetOpenFileNameW);
end;

{$IFDEF COMPILER_9_UP}
function TTntOpenDialog.Execute(ParentWnd: HWND): Boolean;
begin
  if (not Win32PlatformIsUnicode) then
    Result := DoExecute(@GetOpenFileNameA, ParentWnd)
  else
    Result := DoExecuteW(@GetOpenFileNameW, ParentWnd);
end;
{$ENDIF}

{ TTntSaveDialog }

function TTntSaveDialog.Execute: Boolean;
begin
  if (not Win32PlatformIsUnicode) then
    Result := DoExecute(@GetSaveFileNameA)
  else
    Result := DoExecuteW(@GetSaveFileNameW);
end;

{$IFDEF COMPILER_9_UP}
function TTntSaveDialog.Execute(ParentWnd: HWND): Boolean;
begin
  if (not Win32PlatformIsUnicode) then
    Result := DoExecute(@GetSaveFileNameA, ParentWnd)
  else
    Result := DoExecuteW(@GetSaveFileNameW, ParentWnd);
end;
{$ENDIF}

{ Message dialog }

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of WideChar;
  tm: TTextMetric;
begin
  for I := 0 to 25 do Buffer[I] := WideChar(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := WideChar(I + Ord('a'));
  GetTextMetrics(Canvas.Handle, tm);
  GetTextExtentPointW(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := (Result.X div 26 + 1) div 2;
  Result.Y := tm.tmHeight;
end;

type
  TTntMessageForm = class(TTntForm)
  private
    Message: TTntLabel;
    procedure HelpButtonClick(Sender: TObject);
  protected
    procedure CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetFormText: WideString;
  public
    constructor CreateNew(AOwner: TComponent); reintroduce;
  end;

constructor TTntMessageForm.CreateNew(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner);
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
end;

procedure TTntMessageForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TTntMessageForm.CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Word('C')) then
  begin
    Beep;
    TntClipboard.AsWideText := GetFormText;
  end;
end;

function TTntMessageForm.GetFormText: WideString;
var
  DividerLine, ButtonCaptions: WideString;
  I: integer;
begin
  DividerLine := StringOfChar('-', 27) + sLineBreak;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TTntButton then
      ButtonCaptions := ButtonCaptions + TTntButton(Components[I]).Caption +
        StringOfChar(' ', 3);
  ButtonCaptions := Tnt_WideStringReplace(ButtonCaptions,'&','', [rfReplaceAll]);
  Result := DividerLine + Caption + sLineBreak + DividerLine + Message.Caption + sLineBreak
          + DividerLine + ButtonCaptions + sLineBreak + DividerLine;
end;

function GetMessageCaption(MsgType: TMsgDlgType): WideString;
begin
  case MsgType of
    mtWarning:      Result := SMsgDlgWarning;
    mtError:        Result := SMsgDlgError;
    mtInformation:  Result := SMsgDlgInformation;
    mtConfirmation: Result := SMsgDlgConfirm;
    mtCustom:       Result := '';
    else
      raise ETntInternalError.Create('Unexpected MsgType in GetMessageCaption.');
  end;
end;

function GetButtonCaption(MsgDlgBtn: TMsgDlgBtn): WideString;
begin
  case MsgDlgBtn of
    mbYes:         Result := SMsgDlgYes;
    mbNo:          Result := SMsgDlgNo;
    mbOK:          Result := SMsgDlgOK;
    mbCancel:      Result := SMsgDlgCancel;
    mbAbort:       Result := SMsgDlgAbort;
    mbRetry:       Result := SMsgDlgRetry;
    mbIgnore:      Result := SMsgDlgIgnore;
    mbAll:         Result := SMsgDlgAll;
    mbNoToAll:     Result := SMsgDlgNoToAll;
    mbYesToAll:    Result := SMsgDlgYesToAll;
    mbHelp:        Result := SMsgDlgHelp;
    else
      raise ETntInternalError.Create('Unexpected MsgDlgBtn in GetButtonCaption.');
  end;
end;

var
  IconIDs: array[TMsgDlgType] of PAnsiChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonNames: array[TMsgDlgBtn] of WideString = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help');
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0);

function WideCreateMessageDialog(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn): TTntForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, ALeft: Integer;
  B, CancelButton: TMsgDlgBtn;
  IconID: PAnsiChar;
  ATextRect: TRect;
  ThisButtonWidth: integer;
  LButton: TTntButton;
begin
  Result := TTntMessageForm.CreateNew(Application);
  with Result do
  begin
    BorderStyle := bsDialog; // By doing this first, it will work on WINE.
    BiDiMode := Application.BiDiMode;
    Canvas.Font := Font;
    KeyPreview := True;
    Position := poDesigned;
    OnKeyDown := TTntMessageForm(Result).CustomKeyDown;
    DialogUnits := GetAveCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if B in Buttons then
      begin
        ATextRect := Rect(0,0,0,0);
        Tnt_DrawTextW(Canvas.Handle,
          PWideChar(GetButtonCaption(B)), -1,
          ATextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
          DrawTextBiDiModeFlagsReadingOnly);
        with ATextRect do ThisButtonWidth := Right - Left + 8;
        if ThisButtonWidth > ButtonWidth then
          ButtonWidth := ThisButtonWidth;
      end;
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
    SetRect(ATextRect, 0, 0, Screen.Width div 2, 0);
    Tnt_DrawTextW(Canvas.Handle, PWideChar(Msg), Length(Msg) + 1, ATextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
      DrawTextBiDiModeFlagsReadingOnly);
    IconID := IconIDs[DlgType];
    IconTextWidth := ATextRect.Right;
    IconTextHeight := ATextRect.Bottom;
    if IconID <> nil then
    begin
      Inc(IconTextWidth, 32 + HorzSpacing);
      if IconTextHeight < 32 then IconTextHeight := 32;
    end;
    ButtonCount := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then Inc(ButtonCount);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
    ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
      VertMargin * 2;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
    if DlgType <> mtCustom then
      Caption := GetMessageCaption(DlgType)
    else
      Caption := TntApplication.Title;
    if IconID <> nil then
      with TTntImage.Create(Result) do
      begin
        Name := 'Image';
        Parent := Result;
        Picture.Icon.Handle := LoadIcon(0, IconID);
        SetBounds(HorzMargin, VertMargin, 32, 32);
      end;
    TTntMessageForm(Result).Message := TTntLabel.Create(Result);
    with TTntMessageForm(Result).Message do
    begin
      Name := 'Message';
      Parent := Result;
      WordWrap := True;
      Caption := Msg;
      BoundsRect := ATextRect;
      BiDiMode := Result.BiDiMode;
      ALeft := IconTextWidth - ATextRect.Right + HorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Result.ClientWidth - ALeft - Width;
      SetBounds(ALeft, VertMargin,
        ATextRect.Right, ATextRect.Bottom);
    end;
    if mbCancel in Buttons then CancelButton := mbCancel else
      if mbNo in Buttons then CancelButton := mbNo else
        CancelButton := mbOk;
    X := (ClientWidth - ButtonGroupWidth) div 2;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then
      begin
        LButton := TTntButton.Create(Result);
        with LButton do
        begin
          Name := ButtonNames[B];
          Parent := Result;
          Caption := GetButtonCaption(B);
          ModalResult := ModalResults[B];
          if B = DefaultButton then
          begin
            Default := True;
            ActiveControl := LButton;
          end;
          if B = CancelButton then
            Cancel := True;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing,
            ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if B = mbHelp then
            OnClick := TTntMessageForm(Result).HelpButtonClick;
        end;
      end;
  end;
end;

function WideCreateMessageDialog(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TTntForm;
var
  DefaultButton: TMsgDlgBtn;
begin
  if mbOk in Buttons then DefaultButton := mbOk else
    if mbYes in Buttons then DefaultButton := mbYes else
      DefaultButton := mbRetry;
  Result := WideCreateMessageDialog(Msg, DlgType, Buttons, DefaultButton);
end;

function WideMessageDlg(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer;
begin
  Result := WideMessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, -1, -1, '', DefaultButton);
end;

function WideMessageDlg(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := WideMessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, -1, -1, '');
end;

function WideMessageDlgPos(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; DefaultButton: TMsgDlgBtn): Integer;
begin
  Result := WideMessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, X, Y, '', DefaultButton);
end;

function WideMessageDlgPos(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
begin
  Result := WideMessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, X, Y, '');
end;

function _Internal_WideMessageDlgPosHelp(Dlg: TTntForm; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: WideString): Integer;
begin
  with Dlg do
    try
      HelpContext := HelpCtx;
      HelpFile := HelpFileName;
      if X >= 0 then Left := X;
      if Y >= 0 then Top := Y;
      if (Y < 0) and (X < 0) then Position := poScreenCenter;
      Result := ShowModal;
    finally
      Free;
    end;
end;

function WideMessageDlgPosHelp(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: WideString; DefaultButton: TMsgDlgBtn): Integer;
begin
  Result := _Internal_WideMessageDlgPosHelp(
    WideCreateMessageDialog(Msg, DlgType, Buttons, DefaultButton), HelpCtx, X, Y, HelpFileName);
end;

function WideMessageDlgPosHelp(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: WideString): Integer;
begin
  Result := _Internal_WideMessageDlgPosHelp(
    WideCreateMessageDialog(Msg, DlgType, Buttons), HelpCtx, X, Y, HelpFileName);
end;

procedure WideShowMessage(const Msg: WideString);
begin
  WideShowMessagePos(Msg, -1, -1);
end;

procedure WideShowMessageFmt(const Msg: WideString; Params: array of const);
begin
  WideShowMessage(WideFormat(Msg, Params));
end;

procedure WideShowMessagePos(const Msg: WideString; X, Y: Integer);
begin
  WideMessageDlgPos(Msg, mtCustom, [mbOK], 0, X, Y);
end;

{ Input dialog }

function WideInputQuery(const ACaption, APrompt: WideString; var Value: WideString): Boolean;
var
  Form: TTntForm;
  Prompt: TTntLabel;
  Edit: TTntEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TTntForm.Create(Application);
  with Form do begin
    try
      BorderStyle := bsDialog; // By doing this first, it will work on WINE.
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TTntLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TTntEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
      end;
      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TTntButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TTntButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15, ButtonWidth,
          ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
  end;
end;

function WideInputBox(const ACaption, APrompt, ADefault: WideString): WideString;
begin
  Result := ADefault;
  WideInputQuery(ACaption, APrompt, Result);
end;

function WidePromptForFileName(var AFileName: WideString; const AFilter: WideString = '';
  const ADefaultExt: WideString = ''; const ATitle: WideString = '';
  const AInitialDir: WideString = ''; SaveDialog: Boolean = False): Boolean;
var
  Dialog: TTntOpenDialog;
begin
  if SaveDialog then
  begin
    Dialog := TTntSaveDialog.Create(nil);
    Dialog.Options := Dialog.Options + [ofOverwritePrompt];
  end
  else
    Dialog := TTntOpenDialog.Create(nil);
  with Dialog do
  try
    Title := ATitle;
    DefaultExt := ADefaultExt;
    if AFilter = '' then
      Filter := SDefaultFilter else
      Filter := AFilter;
    InitialDir := AInitialDir;
    FileName := AFileName;
    Result := Execute;
    if Result then
      AFileName := FileName;
  finally
    Free;
  end;
end;

end.
