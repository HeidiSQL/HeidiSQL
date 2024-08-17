unit VirtualTrees.BaseAncestorVCL;

{$SCOPEDENUMS ON}

{****************************************************************************************************************}
{ Project          : VirtualTrees                                                                                }
{                                                                                                                }
{ author           : Karol Bieniaszewski, look at VirtualTrees.pas as some code moved from there                 }
{ year             : 2022                                                                                        }
{ contibutors      :                                                                                             }
{****************************************************************************************************************}

interface
uses
  Winapi.Windows,
  Winapi.oleacc,
  Winapi.ActiveX,
  Winapi.Messages,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.StdCtrls,
  VirtualTrees.Types;

type
  TVTBaseAncestorVcl = class abstract(TCustomControl)
  private
    // MSAA support
    FAccessible: IAccessible;                    // The IAccessible interface to the window itself.
    FAccessibleItem: IAccessible;                // The IAccessible to the item that currently has focus.
    FAccessibleName: string;                     // The name the window is given for screen readers.
    FDottedBrushTreeLines: TBrush;               // used to paint dotted lines without special pens

    procedure WMGetObject(var Message: TMessage); message WM_GETOBJECT;
  protected // methods
    function DoRenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HRESULT; virtual; abstract;
    function RenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HResult; virtual;
    procedure NotifyAccessibleEvent(pEvent: DWord = EVENT_OBJECT_STATECHANGE);
    function PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush; virtual;
    function CreateSystemImageSet(): TImageList;
    procedure SetWindowTheme(const Theme: string); virtual;
    //// Abtract method that are implemented in TBaseVirtualTree, keep in sync with TVTBaseAncestorFMX
    function GetSelectedCount(): Integer; virtual; abstract;
    procedure MarkCutCopyNodes; virtual; abstract;
    procedure DoStateChange(Enter: TVirtualTreeStates; Leave: TVirtualTreeStates = []); virtual; abstract;
    function GetSortedCutCopySet(Resolve: Boolean): TNodeArray; virtual; abstract;
    function GetSortedSelection(Resolve: Boolean): TNodeArray; virtual; abstract;
    procedure WriteNode(Stream: TStream; Node: PVirtualNode);  virtual; abstract;
    procedure Sort(Node: PVirtualNode; Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean = True); virtual; abstract;
    procedure DoMouseEnter(); virtual; abstract;
    procedure DoMouseLeave(); virtual; abstract;
  protected //properties
    property DottedBrushTreeLines: TBrush read FDottedBrushTreeLines write FDottedBrushTreeLines;
  public // methods
    destructor Destroy; override;
    procedure CopyToClipboard(); virtual;
    procedure CutToClipboard(); virtual;
    function PasteFromClipboard: Boolean; virtual; abstract;

    /// <summary>
    /// Handle less alias for WinApi.Windows.InvalidateRect
    /// </summary>
    function InvalidateRect(lpRect: PRect; bErase: BOOL): BOOL; inline;
    /// <summary>
    /// Handle less alias for WinApi.Windows.UpdateWindow
    /// </summary>
    function UpdateWindow(): BOOL; inline;
    /// <summary>
    /// Handle less alias for WinApi.Windows.RedrawWindow
    /// </summary>
    function RedrawWindow(lprcUpdate: PRect; hrgnUpdate: HRGN; flags: UINT): BOOL; overload; inline;
    /// <summary>
    /// Handle less alias for WinApi.Windows.RedrawWindow
    /// </summary>
    function RedrawWindow(const lprcUpdate: TRect; hrgnUpdate: HRGN; flags: UINT): BOOL; overload; inline;

    /// <summary>
    /// Handle less and with limited parameters version
    /// </summary>
    function SendWM_SETREDRAW(Updating: Boolean): LRESULT; inline;

    /// <summary>
    /// Handle less alias for WinApi.Windows.ShowScrollBar
    /// </summary>
    procedure ShowScrollBar(Bar: Integer; AShow: Boolean);
    /// <summary>
    /// Handle less alias for WinApi.Windows.SetScrollInfo
    /// </summary>
    function SetScrollInfo(Bar: Integer; const ScrollInfo: TScrollInfo; Redraw: Boolean): TDimension;
    /// <summary>
    /// Handle less alias for WinApi.Windows.GetScrollInfo
    /// </summary>
    function GetScrollInfo(Bar: Integer; var ScrollInfo: TScrollInfo): Boolean;
    /// <summary>
    /// Handle less alias for WinApi.Windows.GetScrollPos
    /// </summary>
    function GetScrollPos(Bar: Integer): TDimension;
    /// <summary>
    /// Canvas based without HDC alias for WinApi.Windows.GetTextMetrics
    /// </summary>
    function GetTextMetrics(Canvas: TCanvas; var TM: TTextMetric): BOOL; overload; inline;
  public //properties
    property Accessible: IAccessible read FAccessible write FAccessible;
    property AccessibleItem: IAccessible read FAccessibleItem write FAccessibleItem;
    property AccessibleName: string read FAccessibleName write FAccessibleName;
  end;

implementation

uses
  System.SyncObjs,
  System.SysUtils,
  Vcl.AxCtrls,
  Vcl.Forms,
  Vcl.Themes,
  Winapi.CommCtrl,
  Winapi.ShlObj,
  Winapi.UxTheme,
  VirtualTrees.DataObject,
  VirtualTrees.Clipboard,
  VirtualTrees.AccessibilityFactory,
  VirtualTrees.StyleHooks;

//----------------------------------------------------------------------------------------------------------------------

const
  Grays: array[0..3] of TColor = (clWhite, clSilver, clGray, clBlack);
  SysGrays: array[0..3] of TColor = (clWindow, clBtnFace, clBtnShadow, clBtnText);

//not used curently anywhere, moved to VCL, to remove ifdef (gWatcher is declared in VirtualTrees.BaseTree)
procedure ConvertImageList(gWatcher: TCriticalSection; BaseVirtualTreeClass: TClass; IL: TImageList; const ImageName: string; ColorRemapping: Boolean = True);

// Loads a bunch of images given by ImageName into IL. If ColorRemapping = True then a mapping of gray values to
// system colors is performed.

var
  lImages,
  lOneImage: TBitmap;
  I: Integer;
  MaskColor: TColor;
  Source,
  Dest: TRect;

begin
  gWatcher.Enter();
  try
    // Since we want the image list appearing in the correct system colors, we have to remap its colors.
    lImages := TBitmap.Create;
    lOneImage := TBitmap.Create;
    if ColorRemapping then
      lImages.Handle := CreateMappedRes(FindClassHInstance(BaseVirtualTreeClass), PChar(ImageName), Grays, SysGrays)
    else
      lImages.Handle := LoadBitmap(FindClassHInstance(BaseVirtualTreeClass), PChar(ImageName));

    try
      Assert(lImages.Height > 0, 'Internal image "' + ImageName + '" is missing or corrupt.');
      if lImages.Height = 0 then
        Exit;// This should never happen, it prevents a division by zero exception below in the for loop, which we have seen in a few cases
      // It is assumed that the image height determines also the width of one entry in the image list.
      IL.Clear;
      IL.Height := lImages.Height;
      IL.Width := lImages.Height;
      lOneImage.Width := IL.Width;
      lOneImage.Height := IL.Height;
      MaskColor := lImages.Canvas.Pixels[0, 0]; // this is usually clFuchsia
      Dest := Rect(0, 0, IL.Width, IL.Height);
      for I := 0 to (lImages.Width div lImages.Height) - 1 do
      begin
        Source := Rect(I * IL.Width, 0, (I + 1) * IL.Width, IL.Height);
        lOneImage.Canvas.CopyRect(Dest, lImages.Canvas, Source);
        IL.AddMasked(lOneImage, MaskColor);
      end;
    finally
      lImages.Free;
      lOneImage.Free;
    end;
  finally
    gWatcher.Leave();
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.RenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HResult;

// Returns a memory expression of all currently selected nodes in the Medium structure.
// Note: The memory requirement of this method might be very high. This depends however on the requested storage format.
//       For HGlobal (a global memory block) we need to render first all nodes to local memory and copy this then to
//       the global memory in Medium. This is necessary because we have first to determine how much
//       memory is needed before we can allocate it. Hence for a short moment we need twice the space as used by the
//       nodes alone (plus the amount the nodes need in the tree anyway)!
//       With IStream this does not happen. We directly stream out the nodes and pass the constructed stream along.

  //--------------- local function --------------------------------------------

  procedure WriteNodes(Stream: TStream);

  var
    Selection: TNodeArray;
    I: Integer;

  begin
    if ForClipboard then
      Selection := GetSortedCutCopySet(True)
    else
      Selection := GetSortedSelection(True);
    for I := 0 to High(Selection) do
      WriteNode(Stream, Selection[I]);
  end;

  //--------------- end local function ----------------------------------------

var
  Data: PCardinal;
  ResPointer: Pointer;
  ResSize: Integer;
  OLEStream: IStream;
  VCLStream: TStream;

begin
  ZeroMemory (@Medium, SizeOf(Medium));

  // We can render the native clipboard format in two different storage media.
  if (FormatEtcIn.cfFormat = CF_VIRTUALTREE) and (FormatEtcIn.tymed and (TYMED_HGLOBAL or TYMED_ISTREAM) <> 0) then
  begin
    VCLStream := nil;
    try
      Medium.unkForRelease := nil;
      // Return data in one of the supported storage formats, prefer IStream.
      if FormatEtcIn.tymed and TYMED_ISTREAM <> 0 then
      begin
        // Create an IStream on a memory handle (here it is 0 which indicates to implicitely allocated a handle).
        // Do not use TStreamAdapter as it is not compatible with OLE (when flushing the clipboard OLE wants the HGlobal
        // back which is not supported by TStreamAdapater).
        CreateStreamOnHGlobal(0, True, OLEStream);
        VCLStream := TOLEStream.Create(OLEStream);
        WriteNodes(VCLStream);
        // Rewind stream.
        VCLStream.Position := 0;
        Medium.tymed := TYMED_ISTREAM;
        IUnknown(Medium.stm) := OLEStream;
        Result := S_OK;
      end
      else
      begin
        VCLStream := TMemoryStream.Create;
        WriteNodes(VCLStream);
        ResPointer := TMemoryStream(VCLStream).Memory;
        ResSize := VCLStream.Position;

        // Allocate memory to hold the string.
        if ResSize > 0 then
        begin
          Medium.hGlobal := GlobalAlloc(GHND or GMEM_SHARE, ResSize + SizeOf(Cardinal));
          Data := GlobalLock(Medium.hGlobal);
          // Store the size of the data too, for easy retrival.
          Data^ := ResSize;
          Inc(Data);
          Move(ResPointer^, Data^, ResSize);
          GlobalUnlock(Medium.hGlobal);
          Medium.tymed := TYMED_HGLOBAL;

          Result := S_OK;
        end
        else
          Result := E_FAIL;
      end;
    finally
      // We can free the VCL stream here since it was either a pure memory stream or only a wrapper around
      // the OLEStream which exists independently.
      VCLStream.Free;
    end;
  end
  else // Ask application descendants to render self defined formats.
    Result := DoRenderOLEData(FormatEtcIn, Medium, ForClipboard);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorVcl.CopyToClipboard;

var
  lDataObject: IDataObject;

begin
  if GetSelectedCount > 0 then
  begin
    lDataObject := TVTDataObject.Create(Self, True);
    if OleSetClipboard(lDataObject) = S_OK then
    begin
      MarkCutCopyNodes;
      DoStateChange([tsCopyPending]);
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.CreateSystemImageSet: TImageList;

// Creates a system check image set.
// Note: the DarkCheckImages and FlatImages image lists must already be filled, as some images from them are copied here.

const
  MaskColor: TColor = clRed;
  cFlags = ILC_COLOR32 or ILC_MASK;

var
  BM: TBitmap;
  Theme: HTHEME;
  Details: TThemedElementDetails;

  //---------------------------------------------------------------------------

  // Mitigator function to use the correct style service for this context (either the style assigned to the control for Delphi > 10.4 or the application style)
  function StyleServices: TCustomStyleServices;
  begin
    Result := VTStyleServices(Self);
  end;

  procedure AddSystemImage(IL: TImageList; Index: Integer);
  const
    States: array [0..19] of Integer = (
      RBS_UNCHECKEDNORMAL, RBS_UNCHECKEDHOT, RBS_UNCHECKEDPRESSED, RBS_UNCHECKEDDISABLED,
      RBS_CHECKEDNORMAL, RBS_CHECKEDHOT, RBS_CHECKEDPRESSED, RBS_CHECKEDDISABLED,
      CBS_UNCHECKEDNORMAL, CBS_UNCHECKEDHOT, CBS_UNCHECKEDPRESSED, CBS_UNCHECKEDDISABLED,
      CBS_CHECKEDNORMAL, CBS_CHECKEDHOT, CBS_CHECKEDPRESSED, CBS_CHECKEDDISABLED,
      CBS_MIXEDNORMAL, CBS_MIXEDHOT, CBS_MIXEDPRESSED, CBS_MIXEDDISABLED);
  var
    ButtonState: Cardinal;
    ButtonType: Cardinal;

  begin
    BM.Canvas.FillRect(Rect(0, 0, BM.Width, BM.Height));
    if StyleServices.Enabled and StyleServices.IsSystemStyle then
    begin
      if Index < 8 then
        Details.Part := BP_RADIOBUTTON
      else
        Details.Part := BP_CHECKBOX;
      Details.State := States[Index];
      DrawThemeBackground(Theme, BM.Canvas.Handle, Details.Part, Details.State, Rect(0, 0, BM.Width, BM.Height), nil);
    end
    else
    begin
      if Index < 8 then
        ButtonType := DFCS_BUTTONRADIO
      else
        ButtonType := DFCS_BUTTONCHECK;
      if Index >= 16 then
        ButtonType := ButtonType or DFCS_BUTTON3STATE;

      case Index mod 4 of
        0:
          ButtonState := 0;
        1:
          ButtonState := DFCS_HOT;
        2:
          ButtonState := DFCS_PUSHED;
        else
          ButtonState := DFCS_INACTIVE;
      end;
      if Index in [4..7, 12..19] then
        ButtonState := ButtonState or DFCS_CHECKED;
//      if Flat then
//        ButtonState := ButtonState or DFCS_FLAT;
      DrawFrameControl(BM.Canvas.Handle, Rect(0, 0, BM.Width, BM.Height), DFC_BUTTON, ButtonType or ButtonState);
    end;
    IL.AddMasked(BM, MaskColor);
  end;

  //--------------- end local functions ---------------------------------------

const
  cDefaultCheckboxSize = 13;// Used when no other value is available
var
  I: Integer;
  lSize: TSize;
  Res: Boolean;
begin
  BM := TBitmap.Create; // Create a temporary bitmap, which holds the intermediate images.
  try
    Res := False;
    // Retrieve the checkbox image size, prefer theme if available, fall back to GetSystemMetrics() otherwise, but this returns odd results on Windows 8 and higher in high-dpi scenarios.
    if StyleServices.Enabled then
      if StyleServices.IsSystemStyle then
      begin
        {$if CompilerVersion >= 33}
        if TOSVersion.Check(10) and (TOSVersion.Build >= 15063)  then
          Theme := OpenThemeDataForDPI(Handle, 'BUTTON', CurrentPPI)
        else
        {$ifend}
          Theme := OpenThemeData(Self.Handle, 'BUTTON');
        Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
        Res := GetThemePartSize(Theme, BM.Canvas.Handle, Details.Part, Details.State, nil, TS_TRUE, lSize) = S_OK;
      end
      else
        Res := StyleServices.GetElementSize(BM.Canvas.Handle, StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal), TElementSize.esActual, lSize {$IF CompilerVersion >= 34}, Self.CurrentPPI{$IFEND});
    if not Res then begin
      lSize := TSize.Create(GetSystemMetrics(SM_CXMENUCHECK), GetSystemMetrics(SM_CYMENUCHECK));
      if lSize.cx = 0 then begin // error? (Should happen rarely only)
        lSize.cx := MulDiv(cDefaultCheckboxSize, Screen.PixelsPerInch, USER_DEFAULT_SCREEN_DPI);
        lSize.cy := lSize.cx;
      end;// if
    end;//if

    Result := TImageList.CreateSize(lSize.cx, lSize.cy);
    Result.Handle := ImageList_Create(Result.Width, Result.Height, cFlags, 0, Result.AllocBy);
    Result.Masked := True;
    Result.BkColor := clWhite;

    // Make the bitmap the same size as the image list is to avoid problems when adding.
    BM.SetSize(Result.Width, Result.Height);
    BM.Canvas.Brush.Color := MaskColor;
    BM.Canvas.Brush.Style := bsSolid;
    BM.Canvas.FillRect(Rect(0, 0, BM.Width, BM.Height));
    Result.AddMasked(BM, MaskColor);

    // Add the 20 system checkbox and radiobutton images.
    for I := 0 to 19 do
      AddSystemImage(Result, I);
    if StyleServices.Enabled and StyleServices.IsSystemStyle then
      CloseThemeData(Theme);

  finally
    BM.Free;
  end;
end;

procedure TVTBaseAncestorVcl.CutToClipboard;
var
  lDataObject: IDataObject;
begin
  if (GetSelectedCount > 0) then
  begin
    lDataObject := TVTDataObject.Create(Self, True);
    if OleSetClipboard(lDataObject) = S_OK then
    begin
      MarkCutCopyNodes;
      DoStateChange([tsCutPending], [tsCopyPending]);
      Invalidate;
    end;
  end;
end;

destructor TVTBaseAncestorVcl.Destroy;
begin
  // Disconnect all remote MSAA connections
  if Assigned(AccessibleItem) then begin
    CoDisconnectObject(AccessibleItem, 0);
    AccessibleItem := nil;
  end;
  if Assigned(Accessible) then begin
    CoDisconnectObject(Accessible, 0);
    Accessible := nil;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush;
begin
  if Assigned(CurrentDottedBrush) then
    begin
      Result := CurrentDottedBrush;
    end else
    begin
      Result := TBrush.Create;
      Result.Bitmap := TBitmap.Create;
    end;

  Result.Bitmap.Handle := CreateBitmap(8, 8, 1, 1, Bits);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.RedrawWindow(const lprcUpdate: TRect; hrgnUpdate: HRGN; flags: UINT): BOOL;
begin
  Result:= Winapi.Windows.RedrawWindow(Handle, lprcUpdate, hrgnUpdate, flags);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.RedrawWindow(lprcUpdate: PRect; hrgnUpdate: HRGN; flags: UINT): BOOL;
begin
  Result:= Winapi.Windows.RedrawWindow(Handle, lprcUpdate, hrgnUpdate, flags);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.InvalidateRect(lpRect: PRect; bErase: BOOL): BOOL;
begin
  Result:= WinApi.Windows.InvalidateRect(Handle, lpRect, bErase);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorVcl.NotifyAccessibleEvent(pEvent: DWord = EVENT_OBJECT_STATECHANGE);
begin
  if Assigned(AccessibleItem) then
    NotifyWinEvent(pEvent, Handle, OBJID_CLIENT, CHILDID_SELF);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.UpdateWindow(): BOOL;
begin
  Result:= WinApi.Windows.UpdateWindow(Handle);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorVcl.WMGetObject(var Message: TMessage);

begin
  if TVTAccessibilityFactory.GetAccessibilityFactory <> nil then
  begin
    // Create the IAccessibles for the tree view and tree view items, if necessary.
    if Accessible = nil then
      Accessible := TVTAccessibilityFactory.GetAccessibilityFactory.CreateIAccessible(Self);
    if AccessibleItem = nil then
      AccessibleItem := TVTAccessibilityFactory.GetAccessibilityFactory.CreateIAccessible(Self);
    if Cardinal(Message.LParam) = OBJID_CLIENT then
      if Assigned(Accessible) then
        Message.Result := LresultFromObject(IID_IAccessible, Message.WParam, Accessible)
      else
        Message.Result := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorVcl.ShowScrollBar(Bar: Integer; AShow: Boolean);
begin
  WinApi.Windows.ShowScrollBar(Handle, Bar, AShow);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.SendWM_SETREDRAW(Updating: Boolean): LRESULT;
begin
  Result:= SendMessage(Handle, WM_SETREDRAW, Ord(not Updating), 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.SetScrollInfo(Bar: Integer; const ScrollInfo: TScrollInfo; Redraw: Boolean): TDimension;
begin
  Result:= WinApi.Windows.SetScrollInfo(Handle, Bar, ScrollInfo, Redraw);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorVcl.SetWindowTheme(const Theme: string);
begin
  Winapi.UxTheme.SetWindowTheme(Handle, PWideChar(Theme), nil);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.GetScrollInfo(Bar: Integer; var ScrollInfo: TScrollInfo): Boolean;
begin
  Result:= WinApi.Windows.GetScrollInfo(Handle, Bar, ScrollInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.GetScrollPos(Bar: Integer): TDimension;
begin
  Result:= WinApi.Windows.GetScrollPos(Handle, Bar);
end;

function TVTBaseAncestorVcl.GetTextMetrics(Canvas: TCanvas; var TM: TTextMetric): BOOL;
begin
  Result:= WinApi.Windows.GetTextMetrics(Canvas.Handle, TM);
end;

end.
