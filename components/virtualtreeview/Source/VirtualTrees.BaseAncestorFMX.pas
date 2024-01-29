unit VirtualTrees.BaseAncestorFMX;

{$SCOPEDENUMS ON}

{****************************************************************************************************************}
{ Project          : VirtualTrees                                                                                }
{                                                                                                                }
{ author           : Karol Bieniaszewski                                                                         }
{ year             : 2022                                                                                        }
{ contibutors      :                                                                                             }
{****************************************************************************************************************}

interface
uses
  {$IFDEF MSWINDOWS}
  WinApi.Windows,
  {$ENDIF}
  System.Classes, System.UITypes,
  FMX.Objects, FMX.Graphics, FMX.Controls, FMX.StdCtrls, FMX.Forms, FMX.ImgList,
  VirtualTrees.Types, VirtualTrees.FMX;


type
  TVTBaseAncestorFMX = class abstract(TRectangle)
  strict private
    FFont: TFont;
    procedure SetFont(const Value: TFont);
  private
    FDottedBrushTreeLines: TStrokeBrush;                  // used to paint dotted lines without special pens
    FDottedBrushGridLines: TStrokeBrush;                  // used to paint dotted lines without special pens
    FInCreate: Boolean;

    function GetFillColor: TAlphaColor;
    procedure SetFillColor(const Value: TAlphaColor);
  protected
    FBevelEdges: TBevelEdges;
    FBevelInner: TBevelCut;
    FBevelOuter: TBevelCut;
    FBevelKind: TBevelKind;
    FBevelWidth: TBevelWidth;
    FBorderWidth: TBorderWidth;
    FHandleAllocated: Boolean;
    FBiDiMode: TBiDiMode;
    FHScrollBar: TScrollBar;
    FVScrollBar: TScrollBar;

    FUseRightToLeftAlignment: Boolean;

    procedure SetBevelCut(Index: Integer; const Value: TBevelCut);
    procedure SetBevelEdges(const Value: TBevelEdges);
    procedure SetBevelKind(const Value: TBevelKind);
    procedure SetBevelWidth(const Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBiDiMode(Value: TBiDiMode);

    function GetClientHeight: Single; virtual; abstract;
    function GetClientWidth: Single; virtual; abstract;
    function GetClientRect: TRect; virtual; abstract;
    procedure UpdateStyleElements; virtual; abstract;

    procedure DoStartDrag(var DragObject: TVTDragDataObject); virtual; abstract;
    procedure DoEndDrag(Target: TObject; X, Y: TDimension); virtual; abstract;
    procedure DragCanceled; virtual; abstract;

    procedure Resize; override;
    function CreateSystemImageSet(): TImageList;
    procedure SetWindowTheme(const Theme: string); virtual;

    procedure ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend}); virtual; abstract;
    function GetControlsAlignment: TAlignment; virtual; abstract;
    function PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush; virtual; abstract;
    function GetSelectedCount(): Integer; virtual; abstract;
    procedure MarkCutCopyNodes; virtual; abstract;
    function GetSortedCutCopySet(Resolve: Boolean): TNodeArray; virtual; abstract;
    function GetSortedSelection(Resolve: Boolean): TNodeArray; virtual; abstract;
    procedure WriteNode(Stream: TStream; Node: PVirtualNode);  virtual; abstract;
    procedure DoMouseEnter(); reintroduce; overload; virtual; abstract;
    procedure DoMouseLeave(); reintroduce; overload; virtual; abstract;
  protected //properties
    property DottedBrushTreeLines: TStrokeBrush read FDottedBrushTreeLines write FDottedBrushTreeLines;
    property DottedBrushGridLines: TStrokeBrush read FDottedBrushGridLines write FDottedBrushGridLines;
  public //methods
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ClientToScreen(P: TPoint): TPoint;
    function ScreenToClient(P: TPoint): TPoint;
    procedure RecreateWnd;
    procedure ShowScrollBar(Bar: Integer; AShow: Boolean);
    function SetScrollInfo(Bar: Integer; const ScrollInfo: TScrollInfo; Redraw: Boolean): TDimension;
    function GetScrollInfo(Bar: Integer; var ScrollInfo: TScrollInfo): Boolean;
    function GetScrollPos(Bar: Integer): TDimension;
    function GetScrollBarForBar(Bar: Integer): TScrollBar;
    procedure HScrollChangeProc(Sender: TObject); virtual; abstract;
    procedure VScrollChangeProc(Sender: TObject); virtual; abstract;

    procedure CopyToClipboard; virtual; abstract;
    procedure CutToClipboard; virtual; abstract;
    function PasteFromClipboard: Boolean; virtual; abstract;

    /// <summary>
    /// Alias for IsFocused to make same as Vcl Focused
    /// </summary>
    function Focused(): Boolean; inline;

    /// <summary>
    /// Convert mouse message to TMouseButton
    /// Created as method, to be available in whole hierarchy without specifing Unit file name (prevent circular unit ref).
    /// </summary>
    class function KeysToShiftState(Keys: LongInt): TShiftState; static;

    function GetParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;

    /// <summary>
    /// Alias for Repaint on FMX to be compatible with VCL
    /// </summary>
    procedure Invalidate(); inline;
    /// <summary>
    /// Alias for Repaint on FMX to be compatible with VCL
    /// </summary>
    function InvalidateRect(lpRect: PRect; bErase: Boolean): Boolean; inline;
    /// <summary>
    /// Alias for Repaint on FMX to be compatible with VCL
    /// </summary>
    function UpdateWindow(): Boolean; inline;
    /// <summary>
    /// Alias for Repaint on FMX to be compatible with VCL
    /// </summary>
    function RedrawWindow(lprcUpdate: PRect; hrgnUpdate: NativeUInt; flags: UINT): Boolean; overload; inline;
    /// <summary>
    /// Alias for Repaint on FMX to be compatible with VCL
    /// </summary>
    function RedrawWindow(const lprcUpdate: TRect; hrgnUpdate: NativeUInt; flags: UINT): Boolean; overload; inline;

    /// <summary>
    /// Alias for Repaint on FMX to be compatible with VCL
    /// </summary>
    function SendWM_SETREDRAW(Updating: Boolean): NativeUInt; inline;

    /// <summary>
    /// Simulate Windows GetSystemMetrics
    /// </summary>
    function GetSystemMetrics(nIndex: Integer): Integer;
    procedure Sort(Node: PVirtualNode; Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean = True); reintroduce; overload; virtual; abstract;																																					  
  public //properties
    property Font: TFont read FFont write SetFont;
    property ClientRect: TRect read GetClientRect;
    property ClientWidth: Single read GetClientWidth;
    property ClientHeight: Single read GetClientHeight;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment write FUseRightToLeftAlignment default false;
    property BevelEdges: TBevelEdges read FBevelEdges write SetBevelEdges default [TBevelEdge.beLeft, TBevelEdge.beTop, TBevelEdge.beRight, TBevelEdge.beBottom];
    property BevelInner: TBevelCut index 0 read FBevelInner write SetBevelCut default TBevelCut.bvRaised;
    property BevelOuter: TBevelCut index 1 read FBevelOuter write SetBevelCut default TBevelCut.bvLowered;
    property BevelKind: TBevelKind read FBevelKind write SetBevelKind default TBevelKind.bkNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode;
    property HScrollBar: TScrollBar read FHScrollBar;
    property VScrollBar: TScrollBar read FVScrollBar;
    property HandleAllocated: Boolean read FHandleAllocated;

    /// <summary>
    /// Alias for Fill.Color to make same use as Vcl Color property
    /// </summary>
    property Color: TAlphaColor read GetFillColor write SetFillColor;
  end;

{$IFNDEF MSWINDOWS}
const
  { GetSystemMetrics() codes }
  SM_CXVSCROLL = 2;
  SM_CYHSCROLL = 3;
{$ENDIF}

implementation
uses FMX.TextLayout, FMX.Utils
  {$IFNDEF MSWINDOWS}
  , WinApi.Windows
  {$ENDIF}
  ;

//-------- TVTBaseAncestorFMX ------------------------------------------------------------------------------------------

class function TVTBaseAncestorFMX.KeysToShiftState(Keys: LongInt): TShiftState;
begin
  Result := TShiftState(Word(Keys));
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.GetFillColor: TAlphaColor;
begin
  Result:= Fill.Color;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TVTBaseAncestorFMX.Create(AOwner: TComponent);
begin
  FInCreate:= true;				   
  inherited;

  FHandleAllocated:= true;
  FUseRightToLeftAlignment:= false;
  FBevelEdges:= [TBevelEdge.beLeft, TBevelEdge.beTop, TBevelEdge.beRight, TBevelEdge.beBottom];
  FBevelInner:= TBevelCut.bvRaised;
  FBevelOuter:= TBevelCut.bvLowered;
  FBevelKind:= TBevelKind.bkNone;
  FBevelWidth:= 1;
  FBorderWidth:= 0;
  FFont:= TFont.Create;
  DisableFocusEffect := True;
  CanFocus := True;
  AutoCapture := True;

  FHScrollBar:= TScrollBar.Create(Self);
  FHScrollBar.Parent:= Self;
  FHScrollBar.Orientation:= TOrientation.Horizontal;
  FHScrollBar.Align:= TAlignLayout.MostBottom;
  FHScrollBar.Visible:= true;
  FHScrollBar.OnChange:= HScrollChangeProc;
  FHScrollBar.Margins.Right:= FHScrollBar.Height;

  FVScrollBar:= TScrollBar.Create(Self);
  FVScrollBar.Parent:= Self;
  FVScrollBar.Orientation:= TOrientation.Vertical;
  FVScrollBar.Align:= TAlignLayout.MostRight;
  FVScrollBar.Visible:= true;
  FVScrollBar.OnChange:= VScrollChangeProc;
  //FVScrollBar.Margins.Bottom:= FVScrollBar.Width;

  SetAcceptsControls(false);

  FInCreate:= false;					
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTBaseAncestorFMX.Destroy();
begin
  inherited;

  if FDottedBrushTreeLines <> nil then
    FreeAndNil(FDottedBrushTreeLines);
  if FDottedBrushGridLines <> nil then
    FreeAndNil(FDottedBrushGridLines);
  FreeAndNil(FFont);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetBevelCut(Index: Integer; const Value: TBevelCut);
begin
  case Index of
    0: { BevelInner }
      if Value <> FBevelInner then
      begin
        FBevelInner := Value;
        Repaint;
      end;
    1: { BevelOuter }
      if Value <> FBevelOuter then
      begin
        FBevelOuter := Value;
        Repaint;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetBevelEdges(const Value: TBevelEdges);
begin
  if Value <> FBevelEdges then
    begin
      FBevelEdges := Value;
      Repaint;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetBevelKind(const Value: TBevelKind);
begin
  if Value <> FBevelKind then
    begin
      FBevelKind := Value;
      Repaint;
    end;
end;
//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetBevelWidth(const Value: TBevelWidth);
begin
  if Value <> FBevelWidth then
    begin
      FBevelWidth := Value;
      Repaint;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.ScreenToClient(P: TPoint): TPoint;

begin
  Result:= AbsoluteToLocal(P);
end;
//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.ClientToScreen(P: TPoint): TPoint;
begin
  Result:= LocalToAbsolute(P);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.Invalidate();
begin
  Repaint;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.InvalidateRect(lpRect: PRect; bErase: Boolean): Boolean;
begin
  Repaint;
  Result:= true;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.UpdateWindow(): Boolean;
begin
  Repaint;
  Result:= true;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.RedrawWindow(lprcUpdate: PRect; hrgnUpdate: NativeUInt; flags: UINT): Boolean;
begin
  Repaint;
  Result:= true;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.RedrawWindow(const lprcUpdate: TRect; hrgnUpdate: NativeUInt; flags: UINT): Boolean;
begin
  Repaint;
  Result:= true;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.RecreateWnd();
begin
  Repaint;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.ShowScrollBar(Bar: Integer; AShow: Boolean);
begin
  if (Bar=SB_HORZ) or (Bar=SB_BOTH) then
    FHScrollBar.Visible:= AShow;

  if (Bar=SB_VERT) or (Bar=SB_BOTH) then
    FVScrollBar.Visible:= AShow;

  if FHScrollBar.Visible and FVScrollBar.Visible then
    FHScrollBar.Margins.Right:= FHScrollBar.Height else
    FHScrollBar.Margins.Right:= 0;

  Repaint;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.SetScrollInfo(Bar: Integer; const ScrollInfo: TScrollInfo; Redraw: Boolean): TDimension;
Var ScrollBar: TScrollBar;
begin
  ScrollBar:= GetScrollBarForBar(Bar);
  if ScrollBar=nil then
    Exit(0); //!!!

  if ScrollInfo.fMask and SIF_PAGE<>0 then
    begin
      ScrollBar.SmallChange:= ScrollInfo.nPage;
    end;

  if ScrollInfo.fMask and SIF_RANGE<>0 then
    begin
      ScrollBar.Min:= ScrollInfo.nMin;
      ScrollBar.Max:= ScrollInfo.nMax;
    end;

  if ScrollInfo.fMask and SIF_POS<>0 then
    begin
      ScrollBar.Value:= ScrollInfo.nPos;
    end;

  Result:= ScrollBar.Value;

  Repaint;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.GetScrollInfo(Bar: Integer; var ScrollInfo: TScrollInfo): Boolean;
Var ScrollBar: TScrollBar;
begin
  ScrollBar:= GetScrollBarForBar(Bar);
  if ScrollBar=nil then
    Exit(False); //!!!

  Result:= true;

  ScrollInfo.cbSize:= SizeOf(TScrollInfo);
  ScrollInfo.fMask:= SIF_ALL;

  ScrollInfo.nMin:= ScrollBar.Min;
  ScrollInfo.nMax:= ScrollBar.Max;
  ScrollInfo.nPage:= ScrollBar.SmallChange;
  ScrollInfo.nPos:= ScrollBar.Value;
  ScrollInfo.nTrackPos:= ScrollBar.Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.GetScrollPos(Bar: Integer): TDimension;
Var ScrollInfo: TScrollInfo;
begin
  GetScrollInfo(Bar, ScrollInfo); //ignore result
  Result:= ScrollInfo.nPos;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.GetScrollBarForBar(Bar: Integer): TScrollBar;
begin
  if (Bar=SB_HORZ) then
    Result:= FHScrollBar else
  if (Bar=SB_VERT) then
    Result:= FVScrollBar else
    Result:= nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetBiDiMode(Value: TBiDiMode);
begin
  if FBiDiMode <> Value then
    begin
      FBiDiMode := Value;
      Repaint;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetBorderWidth(Value: TBorderWidth);
begin
  if FBorderWidth <> Value then
    begin
      FBorderWidth := Value;
      Repaint;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetFillColor(const Value: TAlphaColor);
begin
  Fill.Color:= Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.Focused(): Boolean;
begin
  Result:= IsFocused;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.GetParentForm(Control: TControl; TopForm: Boolean = True): TCustomForm;
begin
  Result:= Control.Root.GetObject as TCustomForm;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.SendWM_SETREDRAW(Updating: Boolean): NativeUInt;
begin
  Repaint;
  Result:= 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.GetSystemMetrics(nIndex: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result:= GetSystemMetrics(nIndex);
  {$ELSE}
  case nIndex of
    SM_CXVSCROLL: Result:= 16;
    SM_CYHSCROLL: Result:= 3;
    else
      raise Exception.Create('Unknown code for GetSystemMetrics: ' + IntToStr(nIndex));
  end;
  {$ENDIF}
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.CreateSystemImageSet(): TImageList;
begin
  Result:= TImageList.Create(Self);
  FillSystemCheckImages(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetWindowTheme(const Theme: string);
begin
  //nothing
end;
//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorFMX.CreateSystemImageSet(): TImageList;
begin
  Result:= TImageList.Create(Self);
  FillSystemCheckImages(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorFMX.SetWindowTheme(const Theme: string);
begin
  //nothing
end;

end.
