unit uVistaFuncs;

interface

uses Forms, Windows, Graphics;

function IsWindowsVista: Boolean;  
procedure SetVistaFonts(const AFont: TFont);
procedure SetVistaContentFonts(const AFont: TFont);     
procedure SetDesktopIconFonts(const AFont: TFont);
procedure ExtendGlass(const AHandle: THandle; const AMargins: TRect);  
function CompositingEnabled: Boolean;    
function TaskDialog(const AHandle: THandle; const ATitle, ADescription,
  AContent: string; const Icon, Buttons: integer): Integer; 
procedure SetVistaTreeView(const AHandle: THandle);
  
const
  VistaFont = 'Segoe UI'; 
  VistaContentFont = 'Calibri';
  XPContentFont = 'Verdana';
  XPFont = 'Tahoma';     

  TD_ICON_BLANK = 0;
  TD_ICON_WARNING = 84;
  TD_ICON_QUESTION = 99;
  TD_ICON_ERROR = 98;
  TD_ICON_INFORMATION = 81;
  TD_ICON_SHIELD_QUESTION = 104;
  TD_ICON_SHIELD_ERROR = 105;
  TD_ICON_SHIELD_OK = 106;        
  TD_ICON_SHIELD_WARNING = 107;                     

  TD_BUTTON_OK = 1;
  TD_BUTTON_YES = 2;
  TD_BUTTON_NO = 4;
  TD_BUTTON_CANCEL = 8;
  TD_BUTTON_RETRY = 16;
  TD_BUTTON_CLOSE = 32;

  TD_RESULT_OK = 1;
  TD_RESULT_CANCEL = 2;
  TD_RESULT_RETRY = 4;
  TD_RESULT_YES = 6;
  TD_RESULT_NO = 7;
  TD_RESULT_CLOSE = 8;    

var
  CheckOSVerForFonts: Boolean = True;

implementation

uses SysUtils, Dialogs, Controls, UxTheme;   

procedure SetVistaTreeView(const AHandle: THandle);
begin
  if IsWindowsVista then
    SetWindowTheme(AHandle, 'explorer', nil);
end;

procedure SetVistaFonts(const AFont: TFont);
begin
  if (IsWindowsVista or not CheckOSVerForFonts)
    and not SameText(AFont.Name, VistaFont)
    and (Screen.Fonts.IndexOf(VistaFont) >= 0) then
  begin
    AFont.Size := AFont.Size + 1;
    AFont.Name := VistaFont;
  end;
end;

procedure SetVistaContentFonts(const AFont: TFont);
begin
  if (IsWindowsVista or not CheckOSVerForFonts)
    and not SameText(AFont.Name, VistaContentFont)
    and (Screen.Fonts.IndexOf(VistaContentFont) >= 0) then
  begin
    AFont.Size := AFont.Size + 2;
    AFont.Name := VistaContentFont;
  end;
end;

procedure SetDefaultFonts(const AFont: TFont);
begin
  AFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
end;

procedure SetDesktopIconFonts(const AFont: TFont);
var
  LogFont: TLogFont;
begin
  if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont),
    @LogFont, 0) then
    AFont.Handle := CreateFontIndirect(LogFont)
  else
    SetDefaultFonts(AFont);
end;

function IsWindowsVista: Boolean;   
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);        
  Result := VerInfo.dwMajorVersion >= 6;
end;  

const
  dwmapi = 'dwmapi.dll';
  DwmIsCompositionEnabledSig = 'DwmIsCompositionEnabled';
  DwmExtendFrameIntoClientAreaSig = 'DwmExtendFrameIntoClientArea';
  TaskDialogSig = 'TaskDialog';

function CompositingEnabled: Boolean;
var
  DLLHandle: THandle;
  DwmIsCompositionEnabledProc: function(pfEnabled: PBoolean): HRESULT; stdcall;
  Enabled: Boolean;
begin
  Result := False;
  if IsWindowsVista then
  begin
    DLLHandle := LoadLibrary(dwmapi);

    if DLLHandle <> 0 then 
    begin
      @DwmIsCompositionEnabledProc := GetProcAddress(DLLHandle,
        DwmIsCompositionEnabledSig);

      if (@DwmIsCompositionEnabledProc <> nil) then
      begin
        DwmIsCompositionEnabledProc(@Enabled);
        Result := Enabled;
      end;

      FreeLibrary(DLLHandle);
    end;
  end;
end;

//from http://www.delphipraxis.net/topic93221,next.html
procedure ExtendGlass(const AHandle: THandle; const AMargins: TRect);
type
  _MARGINS = packed record 
    cxLeftWidth: Integer; 
    cxRightWidth: Integer; 
    cyTopHeight: Integer; 
    cyBottomHeight: Integer; 
  end; 
  PMargins = ^_MARGINS; 
  TMargins = _MARGINS; 
var 
  DLLHandle: THandle;
  DwmExtendFrameIntoClientAreaProc: function(destWnd: HWND; const pMarInset: 
    PMargins): HRESULT; stdcall;
  Margins: TMargins;
begin
  if IsWindowsVista and CompositingEnabled then
  begin
    DLLHandle := LoadLibrary(dwmapi);

    if DLLHandle <> 0 then
    begin
      @DwmExtendFrameIntoClientAreaProc := GetProcAddress(DLLHandle,
        DwmExtendFrameIntoClientAreaSig);

      if (@DwmExtendFrameIntoClientAreaProc <> nil) then
      begin
        ZeroMemory(@Margins, SizeOf(Margins));
        Margins.cxLeftWidth := AMargins.Left;
        Margins.cxRightWidth := AMargins.Right;
        Margins.cyTopHeight := AMargins.Top;
        Margins.cyBottomHeight := AMargins.Bottom;

        DwmExtendFrameIntoClientAreaProc(AHandle, @Margins);
      end;

      FreeLibrary(DLLHandle);
    end;
  end;
end;

//from http://www.tmssoftware.com/atbdev5.htm
function TaskDialog(const AHandle: THandle; const ATitle, ADescription,
  AContent: string; const Icon, Buttons: Integer): Integer;
var
  DLLHandle: THandle;
  res: integer;
  S: string;
  wTitle, wDescription, wContent: array[0..1024] of widechar;
  Btns: TMsgDlgButtons;
  DlgType: TMsgDlgType;
  TaskDialogProc: function(HWND: THandle; hInstance: THandle; cTitle,
    cDescription, cContent: pwidechar; Buttons: Integer; Icon: integer;
    ResButton: pinteger): integer; cdecl stdcall;       
begin                          
  Result := 0;
  if IsWindowsVista then
  begin
    DLLHandle := LoadLibrary(comctl32);
    if DLLHandle >= 32 then
    begin
      @TaskDialogProc := GetProcAddress(DLLHandle, TaskDialogSig);

      if Assigned(TaskDialogProc) then
      begin
        StringToWideChar(ATitle, wTitle, SizeOf(wTitle));
        StringToWideChar(ADescription, wDescription, SizeOf(wDescription));

        //Get rid of line breaks, may be here for backwards compat but not
        //needed with Task Dialogs
        S := StringReplace(AContent, #10, '', [rfReplaceAll]);
        S := StringReplace(S, #13, '', [rfReplaceAll]);
        StringToWideChar(S, wContent, SizeOf(wContent));

        TaskDialogProc(AHandle, 0, wTitle, wDescription, wContent, Buttons,
          Icon, @res);

        Result := mrOK;

        case res of
          TD_RESULT_CANCEL : Result := mrCancel;
          TD_RESULT_RETRY : Result := mrRetry;
          TD_RESULT_YES : Result := mrYes;
          TD_RESULT_NO : Result := mrNo;
          TD_RESULT_CLOSE : Result := mrAbort;
        end;
      end;
      FreeLibrary(DLLHandle);
    end;
  end else
  begin
    Btns := [];
    if Buttons and TD_BUTTON_OK = TD_BUTTON_OK then
      Btns := Btns + [MBOK];
 
    if Buttons and TD_BUTTON_YES = TD_BUTTON_YES then
      Btns := Btns + [MBYES];

    if Buttons and TD_BUTTON_NO = TD_BUTTON_NO then
      Btns := Btns + [MBNO];

    if Buttons and TD_BUTTON_CANCEL = TD_BUTTON_CANCEL then
      Btns := Btns + [MBCANCEL];

    if Buttons and TD_BUTTON_RETRY = TD_BUTTON_RETRY then
      Btns := Btns + [MBRETRY];

    if Buttons and TD_BUTTON_CLOSE = TD_BUTTON_CLOSE then
      Btns := Btns + [MBABORT];

    DlgType := mtCustom;

    case Icon of
      TD_ICON_WARNING : DlgType := mtWarning;
      TD_ICON_QUESTION : DlgType := mtConfirmation;
      TD_ICON_ERROR : DlgType := mtError;
      TD_ICON_INFORMATION: DlgType := mtInformation;
    end;

    Result := MessageDlg(AContent, DlgType, Btns, 0);
  end;
end;

end.
