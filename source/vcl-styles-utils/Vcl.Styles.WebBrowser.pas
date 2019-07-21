// **************************************************************************************************
//
// Unit Vcl.Styles.WebBrowser
// unit for the VCL Styles Utils
// https://github.com/RRUZ/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.WebBrowser.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.WebBrowser;

interface

// Uncomment this option if you want which the TVclStylesWebBrowser class hook the dialogs messages directly.
{ .$DEFINE HOOKDialogs }
uses
  System.Classes,
  WinApi.Windows,
  WinApi.Messages,
  WinApi.Activex,
  Vcl.Forms,
  Vcl.OleServer,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  SHDocVw;

type

  TDocHostUIInfo = record
    cbSize: ULONG;
    dwFlags: DWORD;
    dwDoubleClick: DWORD;
    pchHostCss: PWChar;
    pchHostNS: PWChar;
  end;

{$IFDEF HOOKDialogs}

  // http://msdn.microsoft.com/en-us/library/aa753269%28v=vs.85%29.aspx
  IDocHostShowUI = interface(IUnknown)
    ['{c4d244b0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowMessage(hwnd: THandle; lpstrText: POLESTR; lpstrCaption: POLESTR; dwType: longint;
      lpstrHelpFile: POLESTR; dwHelpContext: longint; var plResult: LRESULT): HRESULT; stdcall;
    function ShowHelp(hwnd: THandle; pszHelpFile: POLESTR; uCommand: integer; dwData: longint; ptMouse: TPoint;
      var pDispachObjectHit: IDispatch): HRESULT; stdcall;
  end; // IDocHostShowUI
{$ENDIF}

  // http://msdn.microsoft.com/en-us/library/aa753260%28v=vs.85%29.aspx
  IDocHostUIHandler = interface(IUnknown)
    ['{BD3F23C0-D43E-11CF-893B-00AA00BDCE1A}']
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT; const pcmdtReserved: IUnknown;
      const pdispReserved: IDispatch): HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDocHostUIInfo): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame; const pDoc: IOleInPlaceUIWindow)
      : HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT; const pUIWindow: IOleInPlaceUIWindow; const FrameWindow: BOOL)
      : HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID; const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD): HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget; out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT; stdcall;
  end;

  TVclStylesWebBrowser = class(SHDocVw.TWebBrowser, IDocHostUIHandler{$IFDEF HOOKDialogs}, IDocHostShowUI{$ENDIF},
    IOleCommandTarget)
  strict private
  type
    TWinContainer = class(TWinControl)
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    end;

  var
    FLSM_CXHTHUMB: integer;
    FLSM_CYVTHUMB: integer;
    FVScrollBar: TScrollBar;
    FHScrollBar: TScrollBar;
    FVScrollBarContainer: TWinContainer;
    FHScrollBarContainer: TWinContainer;
    FScrollCornerContainer: TWinContainer;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure ResizeScrollBars;
    procedure VScrollChange(Sender: TObject);
    procedure HScrollChange(Sender: TObject);
    function GetIEHandle: hwnd;

    procedure DoDocumentComplete(Sender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure DoNavigateComplete2(Sender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure DoBeforeNavigate2(Sender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure DoCommandStateChange(Sender: TObject; Command: integer; Enable: WordBool);
    procedure DoProgressChange(Sender: TObject; Progress, ProgressMax: integer);
  private
    FCustomizeJSErrorDialog: Boolean;
    FCustomizeStdDialogs: Boolean;
    FUseVClStyleBackGroundColor: Boolean;
    // IDocHostUIHandler
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT; const pcmdtReserved: IUnknown;
      const pdispReserved: IDispatch): HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDocHostUIInfo): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame; const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT; const pUIWindow: IOleInPlaceUIWindow; const FrameWindow: BOOL): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID; const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD): HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget; out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT; stdcall;
{$IFDEF HOOKDialogs}
    // IDocHostShowUI
    function ShowMessage(hwnd: THandle; lpstrText: POLESTR; lpstrCaption: POLESTR; dwType: longint;
      lpstrHelpFile: POLESTR; dwHelpContext: longint; var plResult: LRESULT): HRESULT; stdcall;
    function ShowHelp(hwnd: THandle; pszHelpFile: POLESTR; uCommand: integer; dwData: longint; ptMouse: TPoint;
      var pDispachObjectHit: IDispatch): HRESULT; stdcall;
    // IOleCommandTarget
{$ENDIF}
    function QueryStatus(CmdGroup: PGUID; cCmds: Cardinal; prgCmds: POleCmd; CmdText: POleCmdText): HRESULT; stdcall;
    function Exec(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD; const vaIn: OleVariant; var vaOut: OleVariant): HRESULT; stdcall;
    // procedure SetZOrder(TopMost: Boolean); override;
  protected
    procedure InvokeEvent(DispID: TDispID; var Params: TDispParams); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    property CustomizeStdDialogs: Boolean read FCustomizeStdDialogs write FCustomizeStdDialogs;
    property CustomizeJSErrorDialog: Boolean read FCustomizeJSErrorDialog write FCustomizeJSErrorDialog;
    property UseVClStyleBackGroundColor: Boolean read FUseVClStyleBackGroundColor write FUseVClStyleBackGroundColor;
  end;

implementation

uses
  MSHTML,
  System.UITypes,
  System.Sysutils,
  System.Win.ComObj,
  Vcl.Dialogs,
  Vcl.Themes,
  Vcl.Styles;

const
  // About Scroll Bars
  // http://msdn.microsoft.com/en-us/library/windows/desktop/bb787527%28v=vs.85%29.aspx

  // MSDN WebBrowser Customization
  // http://msdn.microsoft.com/en-us/library/aa770041%28v=vs.85%29.aspx
  // MSDN WebBrowser Customization (Part 2)
  // http://msdn.microsoft.com/en-us/library/aa770042%28v=vs.85%29.aspx

  // How to customize the TWebBrowser user interface
  // http://www.delphidabbler.com/articles?article=18&part=1
  // TEmbeddedWB OnGetHostInfo
  // http://www.bsalsa.com/ewb_on_get_host.html

  // http://msdn.microsoft.com/en-us/library/aa753277%28v=vs.85%29.aspx
  DOCHOSTUIFLAG_FLAT_SCROLLBAR = $00000080;
  DOCHOSTUIFLAG_SCROLL_NO = $00000008;
  DOCHOSTUIFLAG_NO3DBORDER = $00000004;
  DOCHOSTUIFLAG_DIALOG = $00000001;
  DOCHOSTUIFLAG_THEME = $00040000;
  DOCHOSTUIFLAG_NOTHEME = $00080000;

  // Set background to vcl styles windows color.
procedure TVclStylesWebBrowser.TWinContainer.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  Details: TThemedElementDetails;
  LCanvas: TCanvas;
begin
  LCanvas := TCanvas.Create;
  try
    LCanvas.Handle := Msg.DC;
    Details.Element := teWindow;
    Details.Part := 0;
    StyleServices.DrawElement(LCanvas.Handle, Details, ClientRect);
  finally
    LCanvas.Free;
  end;
end;

constructor TVclStylesWebBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FLSM_CXHTHUMB := GetSystemMetrics(SM_CXHTHUMB);
  FLSM_CYVTHUMB := GetSystemMetrics(SM_CYVTHUMB);

  FVScrollBarContainer := nil;
  FHScrollBarContainer := nil;

  FScrollCornerContainer := TWinContainer.Create(Self);
  FScrollCornerContainer.Visible := False;

  FVScrollBarContainer := TWinContainer.Create(Self);
  FVScrollBarContainer.Visible := True;
  FVScrollBar := TScrollBar.Create(Self);
  FVScrollBar.Parent := FVScrollBarContainer;
  FVScrollBar.Kind := sbVertical;
  FVScrollBar.Visible := True;
  FVScrollBar.Align := alClient;
  FVScrollBar.OnChange := VScrollChange;
  FVScrollBar.Enabled := False;

  FHScrollBarContainer := TWinContainer.Create(Self);
  FHScrollBarContainer.Visible := False;
  FHScrollBar := TScrollBar.Create(Self);
  FHScrollBar.Parent := FHScrollBarContainer;
  FHScrollBar.Visible := True;
  FHScrollBar.Align := alClient;
  FHScrollBar.OnChange := HScrollChange;

  FCustomizeJSErrorDialog := True;
  FCustomizeStdDialogs := True;
  FUseVClStyleBackGroundColor := False;
end;

// check flicker issue;
procedure TVclStylesWebBrowser.WMSIZE(var Message: TWMSIZE);
begin
  if Document <> nil then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);

  inherited;
  ResizeScrollBars;

  if Document <> nil then
  begin
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE + RDW_ALLCHILDREN + RDW_UPDATENOW);
  end;
end;

function TVclStylesWebBrowser.GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD): HRESULT;
begin
  Result := E_FAIL;
end;

function TVclStylesWebBrowser.TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
  const nCmdID: DWORD): HRESULT;
begin
  Result := S_FALSE;
end;

function TVclStylesWebBrowser.TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR;
  var ppchURLOut: POLESTR): HRESULT;
begin
  Result := E_FAIL;
end;

function TVclStylesWebBrowser.EnableModeless(const fEnable: BOOL): HRESULT;
begin
  Result := S_OK;
end;

function TVclStylesWebBrowser.FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT;
begin
  ppDORet := nil;
  Result := S_FALSE;
end;

function TVclStylesWebBrowser.GetDropTarget(const pDropTarget: IDropTarget; out ppDropTarget: IDropTarget): HRESULT;
begin
  ppDropTarget := nil;
  Result := E_FAIL;
end;

function TVclStylesWebBrowser.GetExternal(out ppDispatch: IDispatch): HRESULT;
begin
  ppDispatch := nil;
  Result := E_FAIL;
end;

function TVclStylesWebBrowser.UpdateUI: HRESULT;
begin
  Result := S_OK;
end;

function TVclStylesWebBrowser.HideUI: HRESULT;
begin
  Result := S_OK;
end;

function TVclStylesWebBrowser.OnDocWindowActivate(const fActivate: BOOL): HRESULT;
begin
  Result := S_OK;
end;

function TVclStylesWebBrowser.OnFrameWindowActivate(const fActivate: BOOL): HRESULT;
begin
  Result := S_OK;
end;

// How to handle script errors as a WebBrowser control host
// http://support.microsoft.com/kb/261003
function TVclStylesWebBrowser.Exec(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD; const vaIn: OleVariant;
  var vaOut: OleVariant): HRESULT;
const
  CGID_DocHostCommandHandler: TGUID = (D1: $F38BC242; D2: $B950; D3: $11D1;
    D4: ($89, $18, $00, $C0, $4F, $C2, $C8, $36));
var
  LHTMLEventObj: IHTMLEventObj;
  LHTMLWindow2: IHTMLWindow2;
  LHTMLDocument2: IHTMLDocument2;
  LUnknown: IUnknown;
  Msg: string;

  function GetPropertyValue(const PropName: WideString): OleVariant;
  var
    LParams: TDispParams;
    LDispIDs: integer;
    Status: integer;
    ExcepInfo: TExcepInfo;
    LName: PWideChar;
  begin
    ZeroMemory(@LParams, SizeOf(LParams));
    LName := PWideChar(PropName);
    Status := LHTMLEventObj.GetIDsOfNames(GUID_NULL, @LName, 1, LOCALE_SYSTEM_DEFAULT, @LDispIDs);
    if Status = 0 then
    begin
      Status := LHTMLEventObj.Invoke(LDispIDs, GUID_NULL, LOCALE_SYSTEM_DEFAULT, DISPATCH_PROPERTYGET, LParams, @Result,
        @ExcepInfo, nil);
      if Status <> 0 then
        DispatchInvokeError(Status, ExcepInfo);
    end
    else if Status = DISP_E_UNKNOWNNAME then
      raise EOleError.CreateFmt('Property "%s" is not supported.', [PropName])
    else
      OleCheck(Status);
  end;

begin
  Result := S_OK;
  {
    //to do -> prompt box [000214D0-0000-0000-C000-000000000046] + OLECMDID_UPDATETRAVELENTRY_DATARECOVERY
  }
  if (CmdGroup <> nil) and IsEqualGuid(CmdGroup^, CGID_DocHostCommandHandler) then
    case nCmdID of
      OLECMDID_SHOWSCRIPTERROR:
        begin
          if not FCustomizeJSErrorDialog then
            exit;
          LUnknown := IUnknown(TVarData(vaIn).VUnknown);
          if Succeeded(LUnknown.QueryInterface(IID_IHTMLDocument2, LHTMLDocument2)) then
          begin
            LHTMLWindow2 := LHTMLDocument2.Get_parentWindow;
            if LHTMLWindow2 <> nil then
            begin
              LHTMLEventObj := LHTMLWindow2.Get_event;
              if LHTMLEventObj <> nil then
              begin
                Msg := 'An error has ocurred in the script in this page' + sLineBreak + 'Line  %s' + sLineBreak +
                  'Char  %s' + sLineBreak + 'Error %s' + sLineBreak + 'Code  %s' + sLineBreak + 'URL   %s' + sLineBreak
                  + 'Do you want to continue running scripts on this page?';
                Msg := Format(Msg, [GetPropertyValue('errorline'), GetPropertyValue('errorCharacter'),
                  GetPropertyValue('errorMessage'), GetPropertyValue('errorCode'), GetPropertyValue('errorUrl')]);
                if MessageDlg(Msg, mtWarning, [mbYes, mbNo], 0) = mrYes then
                  vaOut := True
                else
                  vaOut := False;

                Result := S_OK;
              end;
            end;
          end;
        end;
    else
      Result := OLECMDERR_E_NOTSUPPORTED;
    end
  else
    Result := OLECMDERR_E_UNKNOWNGROUP;
end;

function TVclStylesWebBrowser.QueryStatus(CmdGroup: PGUID; cCmds: Cardinal; prgCmds: POleCmd;
  CmdText: POleCmdText): HRESULT;
begin
  Result := S_FALSE;
end;

function TVclStylesWebBrowser.ResizeBorder(const prcBorder: PRECT; const pUIWindow: IOleInPlaceUIWindow;
  const FrameWindow: BOOL): HRESULT;
begin
  Result := S_FALSE;
end;

function TVclStylesWebBrowser.ShowUI(const dwID: DWORD; const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame; const pDoc: IOleInPlaceUIWindow): HRESULT;
begin
  Result := S_OK;
end;

function TVclStylesWebBrowser.ShowContextMenu(const dwID: DWORD; const ppt: PPOINT; const pcmdtReserved: IUnknown;
  const pdispReserved: IDispatch): HRESULT;
begin
  Result := S_FALSE;
end;

{$IFDEF HOOKDialogs}

function TVclStylesWebBrowser.ShowHelp(hwnd: THandle; pszHelpFile: POLESTR; uCommand, dwData: integer; ptMouse: TPoint;
  var pDispachObjectHit: IDispatch): HRESULT;
begin
  Result := S_FALSE;
end;

// http://msdn.microsoft.com/en-us/library/aa753271%28v=vs.85%29.aspx
function TVclStylesWebBrowser.ShowMessage(hwnd: THandle; lpstrText, lpstrCaption: POLESTR; dwType: integer;
  lpstrHelpFile: POLESTR; dwHelpContext: integer; var plResult: LRESULT): HRESULT;
var
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
begin
  Result := E_NOTIMPL;
  if not FCustomizeStdDialogs then
    exit;

  DlgType := mtInformation;
  if ((dwType and MB_ICONMASK) = MB_ICONHAND) or ((dwType and MB_ICONMASK) = MB_USERICON) then
    DlgType := mtCustom
  else if (dwType and MB_ICONMASK) = MB_ICONWARNING then
    DlgType := mtWarning
  else if (dwType and MB_ICONMASK) = MB_ICONQUESTION then
    DlgType := mtConfirmation
  else if (dwType and MB_ICONMASK) = MB_ICONEXCLAMATION then
    DlgType := mtInformation;

  case dwType and MB_TYPEMASK of
    MB_OK:
      Buttons := [mbOK];
    MB_OKCANCEL:
      Buttons := [mbOK, mbCancel];
    MB_ABORTRETRYIGNORE:
      Buttons := [mbAbort, mbRetry, mbIgnore];
    MB_YESNOCANCEL:
      Buttons := [mbYes, mbNo, mbCancel];
    MB_YESNO:
      Buttons := [mbYes, mbNo];
    MB_RETRYCANCEL:
      Buttons := [mbRetry, mbCancel];
  else
    Buttons := [mbOK];
  end;

  plResult := MessageDlg(lpstrText, DlgType, Buttons, dwHelpContext);
  Result := S_OK;
end;
{$ENDIF}

function TVclStylesWebBrowser.GetHostInfo(var pInfo: TDocHostUIInfo): HRESULT;
var
  BodyCss: string;
  ColorHtml: string;
  LColor: TColor;
begin
  LColor := StyleServices.GetSystemColor(clWindow);
  ColorHtml := Format('#%.2x%.2x%.2x', [GetRValue(LColor), GetGValue(LColor), GetBValue(LColor)]);
  BodyCss := Format('BODY {background-color:%s}', [ColorHtml]);

  pInfo.cbSize := SizeOf(pInfo);
  pInfo.dwFlags := 0;
  pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_NO3DBORDER; // disable 3d border
  pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_THEME;
  if FUseVClStyleBackGroundColor then
    pInfo.pchHostCss := PWideChar(BodyCss);
  Result := S_OK;
  ResizeScrollBars;
end;

function TVclStylesWebBrowser.GetIEHandle: hwnd;
var
  ChildHWND: WinApi.Windows.hwnd;
  TempHWND: WinApi.Windows.hwnd;
  lpClassName: Array [0 .. 255] of Char;
begin
  TempHWND := Self.Handle;
  if TempHWND <> 0 then
    while True do
    begin
      ChildHWND := GetWindow(TempHWND, GW_CHILD);
      if ChildHWND = 0 then
        break;
      GetClassName(ChildHWND, lpClassName, SizeOf(lpClassName));
      if SameText(string(lpClassName), 'Internet Explorer_Server') then
      begin
        Result := ChildHWND;
        exit;
      end;
      TempHWND := ChildHWND;
    end;
  Result := 0;
end;

procedure TVclStylesWebBrowser.SetParent(AParent: TWinControl);
begin
  inherited;
  if not(csDestroying in ComponentState) then
  begin
    FVScrollBarContainer.Parent := AParent;
    FHScrollBarContainer.Parent := AParent;
    FScrollCornerContainer.Parent := AParent;
    ResizeScrollBars;
  end;
end;

// procedure TVclStylesWebBrowser.SetZOrder(TopMost: Boolean);
// begin
//   inherited;
//   ResizeScrollBars;
// end;

procedure TVclStylesWebBrowser.ResizeScrollBars;
var
  StateVisible: Boolean;
  DocClientWidth: integer;
  ScrollWidth: integer;
  ScrollHeight: integer;
  HPageSize: integer;
  VPageSize: integer;
  LRect: TRect;
  IEHWND: WinApi.Windows.hwnd;

  procedure UpdateContainers;
  begin
    if FVScrollBarContainer.Visible then
    begin
      LRect := BoundsRect;
      // OutputDebugString(PChar(Format('Original VScrollBarContainer Left %d Top %d Width %d Height %d',[LRect.Left, LRect.Top, LRect.Width, LRect.Height]) ));
      LRect.Left := LRect.Right - FLSM_CXHTHUMB;
      if FHScrollBarContainer.Visible then
        LRect.Bottom := LRect.Bottom - FLSM_CYVTHUMB;

      // LRect.Width:=2;
      FVScrollBarContainer.BoundsRect := LRect;
    end;

    if FHScrollBarContainer.Visible then
    begin
      LRect := BoundsRect;
      // OutputDebugString(PChar(Format('Original HScrollBarContainer Left %d Top %d Width %d Height %d',[LRect.Left, LRect.Top, LRect.Width, LRect.Height]) ));
      LRect.Top := LRect.Bottom - FLSM_CYVTHUMB;
      if FVScrollBarContainer.Visible then
        LRect.Right := LRect.Right - FLSM_CXHTHUMB;

      // LRect.Height:=2;
      FHScrollBarContainer.BoundsRect := LRect;
      // OutputDebugString(PChar(Format('ScrollBar Left %d Top %d Width %d Height %d',[LRect.Left, LRect.Top, LRect.Width, LRect.Height]) ));
    end;

    StateVisible := FScrollCornerContainer.Visible;
    FScrollCornerContainer.Visible := FHScrollBarContainer.Visible and FVScrollBarContainer.Visible;

    if FScrollCornerContainer.Visible then
    begin
      LRect := BoundsRect;
      LRect.Left := LRect.Right - FLSM_CXHTHUMB;
      LRect.Top := LRect.Bottom - FLSM_CYVTHUMB;
      FScrollCornerContainer.BoundsRect := LRect;
      if not StateVisible then
        FScrollCornerContainer.BringToFront;
    end;
  end;

begin
  IEHWND := GetIEHandle;

  if (IEHWND = 0) or (FVScrollBarContainer = nil) or (FHScrollBarContainer = nil) then
    exit;

  FVScrollBarContainer.Visible := True;

  if (Document <> nil) and (IHTMLDocument2(Document).Body <> nil) then
  begin
    DocClientWidth := OleVariant(Document).documentElement.ClientWidth;
    if (DocClientWidth > 0) then
    begin
      ScrollWidth := OleVariant(Document).documentElement.ScrollWidth;
      // OutputDebugString(PChar(Format('ScrollWidth %s',[inttoStr(ScrollWidth)])));

      if (FHScrollBar.Max <> ScrollWidth) and (ScrollWidth >= FHScrollBar.PageSize) and (ScrollWidth >= FHScrollBar.Min)
      then
        FHScrollBar.Max := ScrollWidth;

      ScrollHeight := OleVariant(Document).documentElement.ScrollHeight;
      // OutputDebugString(PChar(Format('ScrollHeight %s',[inttoStr(ScrollHeight)])));

      if (FVScrollBar.Max <> ScrollHeight) and (ScrollHeight >= FVScrollBar.PageSize) and
        (ScrollHeight >= FVScrollBar.Min) then
        FVScrollBar.Max := ScrollHeight;
    end
    else
    begin
      ScrollWidth := IHTMLDocument2(Document).Body.getAttribute('ScrollWidth', 0);
      if (FHScrollBar.Max <> ScrollWidth) and (ScrollWidth >= FHScrollBar.PageSize) and (ScrollWidth >= FHScrollBar.Min)
      then
        FHScrollBar.Max := ScrollWidth;

      ScrollHeight := IHTMLDocument2(Document).Body.getAttribute('ScrollHeight', 0);
      if (FVScrollBar.Max <> ScrollHeight) and (ScrollHeight >= FVScrollBar.PageSize) and
        (ScrollHeight >= FVScrollBar.Min) then
        FVScrollBar.Max := ScrollHeight;
    end;

    if (FHScrollBar.Max > Self.Width - FLSM_CXHTHUMB) and (FHScrollBar.Max > 0) and (FHScrollBar.Max <> Self.Width) then
      VPageSize := Self.Height - FLSM_CYVTHUMB
    else
      VPageSize := Self.Height;

    FVScrollBar.PageSize := VPageSize;
    FVScrollBar.SetParams(FVScrollBar.Position, 0, FVScrollBar.Max);
    FVScrollBar.LargeChange := FVScrollBar.PageSize;

    HPageSize := Self.Width - FLSM_CXHTHUMB;
    FHScrollBar.PageSize := HPageSize;
    FHScrollBar.SetParams(FHScrollBar.Position, 0, FHScrollBar.Max);
    FHScrollBar.LargeChange := FHScrollBar.PageSize;

    FVScrollBar.Enabled := (VPageSize < FVScrollBar.Max) and (FVScrollBar.PageSize > 0) and (FVScrollBar.Max > 0) and
      (FVScrollBar.Max <> Self.Height);

    StateVisible := FHScrollBarContainer.Visible;

    if IsWindow(FHScrollBarContainer.Handle) then
      FHScrollBarContainer.Visible := (HPageSize < FHScrollBar.Max) and (FHScrollBar.PageSize < FHScrollBar.Max) and
        (FHScrollBar.Max > 0) and (FHScrollBar.Max <> Self.Width);

    if not StateVisible and FHScrollBarContainer.Visible then
      FHScrollBarContainer.BringToFront;

    FVScrollBarContainer.BringToFront;
  end;

  UpdateContainers;
end;

procedure TVclStylesWebBrowser.DoProgressChange(Sender: TObject; Progress, ProgressMax: integer);
begin
  ResizeScrollBars;
end;

procedure TVclStylesWebBrowser.DoDocumentComplete(Sender: TObject; const pDisp: IDispatch; const URL: OleVariant);
begin
  ResizeScrollBars;
end;

procedure TVclStylesWebBrowser.DoNavigateComplete2(Sender: TObject; const pDisp: IDispatch; const URL: OleVariant);
begin
  ResizeScrollBars;
end;

procedure TVclStylesWebBrowser.DoCommandStateChange(Sender: TObject; Command: integer; Enable: WordBool);
begin
  if (Document <> nil) and (IHTMLDocument2(Document).Body <> nil) then
  begin
    if (OleVariant(Document).documentElement.scrollTop = 0) then
      FVScrollBar.Position := IHTMLDocument2(Document).Body.getAttribute('ScrollTop', 0)
    else
      FVScrollBar.Position := OleVariant(Document).documentElement.scrollTop;

    if (OleVariant(Document).documentElement.scrollLeft = 0) then
      FHScrollBar.Position := IHTMLDocument2(Document).Body.getAttribute('ScrollLeft', 0)
    else
      FHScrollBar.Position := OleVariant(Document).documentElement.scrollLeft
  end;
  ResizeScrollBars;
end;

procedure TVclStylesWebBrowser.DoBeforeNavigate2(Sender: TObject; const pDisp: IDispatch;
  const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
begin
  ResizeScrollBars;
end;

procedure TVclStylesWebBrowser.VScrollChange(Sender: TObject);
begin
  if (Document <> nil) and (IHTMLDocument2(Document).ParentWindow <> nil) then
    IHTMLWindow2(IHTMLDocument2(Document).ParentWindow).Scroll(FHScrollBar.Position, FVScrollBar.Position);
end;

procedure TVclStylesWebBrowser.HScrollChange(Sender: TObject);
begin
  if (Document <> nil) and (IHTMLDocument2(Document).ParentWindow <> nil) then
    IHTMLWindow2(IHTMLDocument2(Document).ParentWindow).Scroll(FHScrollBar.Position, FVScrollBar.Position);
end;

procedure TVclStylesWebBrowser.InvokeEvent(DispID: TDispID; var Params: TDispParams);
var
  ArgCount: integer;
  LVarArray: Array of OleVariant;
  LIndex: integer;
begin
  inherited;
  ArgCount := Params.cArgs;
  SetLength(LVarArray, ArgCount);
  for LIndex := Low(LVarArray) to High(LVarArray) do
    LVarArray[High(LVarArray) - LIndex] := OleVariant(TDispParams(Params).rgvarg^[LIndex]);

  case DispID of
    252:
      DoNavigateComplete2(Self, LVarArray[0] { const IDispatch } , LVarArray[1] { const OleVariant } );

    259:
      DoDocumentComplete(Self, LVarArray[0] { const IDispatch } , LVarArray[1] { const OleVariant } );

    250:
      DoBeforeNavigate2(Self, LVarArray[0] { const IDispatch } , LVarArray[1] { const OleVariant } ,
        LVarArray[2] { const OleVariant } , LVarArray[3] { const OleVariant } , LVarArray[4] { const OleVariant } ,
        LVarArray[5] { const OleVariant } , WordBool((TVarData(LVarArray[6]).VPointer)^) { var WordBool } );

    105:
      DoCommandStateChange(Self, LVarArray[0] { Integer } , LVarArray[1] { WordBool } );

    108:
      DoProgressChange(Self, LVarArray[0] { Integer } , LVarArray[1] { Integer } );

  end;

  SetLength(LVarArray, 0);
end;

procedure TVclStylesWebBrowser.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  FVScrollBarContainer.Visible := Self.Visible;
  FHScrollBarContainer.Visible := Self.Visible;
  FScrollCornerContainer.Visible := Self.Visible;
end;

procedure TVclStylesWebBrowser.Loaded;
begin
  inherited;
  ResizeScrollBars;
end;

end.
