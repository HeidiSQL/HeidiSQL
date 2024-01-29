unit VirtualTrees.FMX;

{$SCOPEDENUMS ON}

{***********************************************************}
{ Project          : VirtualTrees                           }
{                                                           }
{ author           : Karol Bieniaszewski                    }
{ year             : 2018                                   }
{ contibutors      :                                        }
{***********************************************************}

interface
uses
    System.Classes
  , System.UITypes
  , System.Types
  , System.ImageList
  , System.Math.Vectors
  , FMX.ImgList
  , FMX.Graphics
  , FMX.Controls
  , FMX.Types
  , FMX.StdCtrls;

//-------- type aliasing -------------------------------------------------------------------------------------------------------------------

type
  TRect = System.Types.TRectF;
  PRect = System.Types.PRectF;
  TPoint = System.Types.TPointF;
  PPoint = System.Types.PPointF;
  PSize = System.Types.PSizeF;
  TSize = System.Types.TSizeF;
  TColor = System.UITypes.TAlphaColor;
  PAnsiChar = System.MarshaledAString;
  UINT = LongWord;
  PUINT = ^UINT;
  TCustomControl = TControl; //Alias for VCL compatibility as on FMX there is not TCustomControl

//------- color aliasing -------------------------------------------------------------------------------------------------------------------

const
  clBtnFace = TAlphaColor($FFF0F0F0); //TAlphaColorRec.Gray;
  clBtnText = TAlphaColorRec.Black;
  clBtnHighlight = TAlphaColorRec.DkGray;
  clBtnShadow = TAlphaColorRec.Darkgray;
  clHighlight = TAlphaColorRec.Lightblue;
  clWindow = TAlphaColorRec.White;
  clWindowText = TAlphaColorRec.Black;
  clHighlightText = TAlphaColorRec.White;
  clWhite = TAlphaColorRec.White;
  clSilver = TAlphaColorRec.Silver;
  clGray = TAlphaColorRec.Gray;
  clBlack = TAlphaColorRec.Black;
  clGreen = TAlphaColorRec.Green;
  clBlue =  TAlphaColorRec.Blue;
  clGrayText =  TAlphaColorRec.DkGray;
  clInactiveCaption = TAlphaColorRec.Darkblue; //TODO: color
  clInactiveCaptionText = TAlphaColorRec.Yellow; //TODO: color
  clDkGray = TAlphaColorRec.DkGray;


//------- needed for migration -------------------------------------------------------------------------------------------------------------

const
  { 3D border styles }
  BDR_RAISEDOUTER = 1;
  BDR_SUNKENOUTER = 2;
  BDR_RAISEDINNER = 4;
  BDR_SUNKENINNER = 8;

  BDR_OUTER = 3;
  BDR_INNER = 12;
  BDR_RAISED = 5;
  BDR_SUNKEN = 10;

  EDGE_RAISED = (BDR_RAISEDOUTER or BDR_RAISEDINNER);
  EDGE_SUNKEN = (BDR_SUNKENOUTER or BDR_SUNKENINNER);
  EDGE_ETCHED = (BDR_SUNKENOUTER or BDR_RAISEDINNER);
  EDGE_BUMP = (BDR_RAISEDOUTER or BDR_SUNKENINNER);

  ETO_OPAQUE = 2;
  ETO_CLIPPED = 4;
  ETO_RTLREADING = $80;

  RTLFlag: array[Boolean] of Integer = (0, ETO_RTLREADING);

  { Border flags }
  BF_LEFT = 1;
  BF_TOP = 2;
  BF_RIGHT = 4;
  BF_BOTTOM = 8;

  BF_TOPLEFT = (BF_TOP or BF_LEFT);
  BF_TOPRIGHT = (BF_TOP or BF_RIGHT);
  BF_BOTTOMLEFT = (BF_BOTTOM or BF_LEFT);
  BF_BOTTOMRIGHT = (BF_BOTTOM or BF_RIGHT);
  BF_RECT = (BF_LEFT or BF_TOP or BF_RIGHT or BF_BOTTOM);

  BF_MIDDLE = $800;   { Fill in the middle }
  BF_SOFT = $1000;    { For softer buttons }
  BF_ADJUST = $2000;  { Calculate the space left over }
  BF_FLAT = $4000;    { For flat rather than 3D borders }
  BF_MONO = $8000;    { For monochrome borders }

  { DrawText() Format Flags }
  DT_TOP = 0;
  DT_LEFT = 0;
  DT_CENTER = 1;
  DT_RIGHT = 2;
  DT_VCENTER = 4;
  DT_BOTTOM = 8;
  DT_WORDBREAK = $10;
  DT_SINGLELINE = $20;
  DT_EXPANDTABS = $40;
  DT_TABSTOP = $80;
  DT_NOCLIP = $100;
  DT_EXTERNALLEADING = $200;
  DT_CALCRECT = $400;
  DT_NOPREFIX = $800;
  DT_INTERNAL = $1000;


  DT_EDITCONTROL = $2000;
  DT_PATH_ELLIPSIS = $4000;
  DT_END_ELLIPSIS = $8000;
  DT_MODIFYSTRING = $10000;
  DT_RTLREADING = $20000;
  DT_WORD_ELLIPSIS = $40000;
  DT_NOFULLWIDTHCHARBREAK = $0080000;
  DT_HIDEPREFIX = $00100000;
  DT_PREFIXONLY = $00200000;

  MAXDWORD = DWORD($FFFFFFFF);
  WHEEL_DELTA = 120;            { Value for rolling one detent }
  WHEEL_PAGESCROLL = MAXDWORD;  { Scroll one page }

  { WM_SIZE message wParam values }
  SIZE_RESTORED = 0;
  SIZE_MINIMIZED = 1;
  SIZE_MAXIMIZED = 2;
  SIZE_MAXSHOW = 3;
  SIZE_MAXHIDE = 4;

  { Scroll Bar Constants }
  SB_HORZ = 0;
  SB_VERT = 1;
  SB_CTL =  2;
  SB_BOTH = 3;

  SIF_RANGE = 1;
  SIF_PAGE = 2;
  SIF_POS = 4;
  SIF_DISABLENOSCROLL = 8;
  SIF_TRACKPOS = $10;
  SIF_ALL = (SIF_RANGE or SIF_PAGE or SIF_POS or SIF_TRACKPOS);

  { Scroll Bar Commands }
  SB_LINEUP = 0;
  SB_LINELEFT = 0;
  SB_LINEDOWN = 1;
  SB_LINERIGHT = 1;
  SB_PAGEUP = 2;
  SB_PAGELEFT = 2;
  SB_PAGEDOWN = 3;
  SB_PAGERIGHT = 3;
  SB_THUMBPOSITION = 4;
  SB_THUMBTRACK = 5;
  SB_TOP = 6;
  SB_LEFT = 6;
  SB_BOTTOM = 7;
  SB_RIGHT = 7;
  SB_ENDSCROLL = 8;

  { RedrawWindow() flags }
  RDW_INVALIDATE = 1;
  RDW_INTERNALPAINT = 2;
  RDW_ERASE = 4;
  RDW_VALIDATE = 8;
  RDW_NOINTERNALPAINT = $10;
  RDW_NOERASE = $20;
  RDW_NOCHILDREN = $40;
  RDW_ALLCHILDREN = $80;
  RDW_UPDATENOW = $100;
  RDW_ERASENOW = $200;
  RDW_FRAME = $400;
  RDW_NOFRAME = $800;

  { GetSystemMetrics() codes }
  SM_CXVSCROLL = 2;
  SM_CYHSCROLL = 3;							  
var
  // Clipboard format IDs used in OLE drag'n drop and clipboard transfers.
  CF_VIRTUALTREE,
  CF_VTREFERENCE,
  CF_VRTF,
  CF_VRTFNOOBJS,   // Unfortunately CF_RTF* is already defined as being
                   // registration strings so I have to use different identifiers.
  CF_HTML,
  CF_CSV: Word;

type
  tagSCROLLINFO = record
    cbSize: UINT;
    fMask: UINT;
    nMin: Single;
    nMax: Single;
    nPage: Single;
    nPos: Single;
    nTrackPos: Single;
  end;
  PScrollInfo = ^TScrollInfo;
  TScrollInfo = tagSCROLLINFO;
  SCROLLINFO = tagSCROLLINFO;

  TBorderWidth = Single;
  TBevelCut = (bvNone, bvLowered, bvRaised, bvSpace);
  TBevelEdge = (beLeft, beTop, beRight, beBottom);
  TBevelEdges = set of TBevelEdge;
  TBevelKind = (bkNone, bkTile, bkSoft, bkFlat);
  TBevelWidth = 1..MaxInt;

  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin);
  TBorderStyle = TFormBorderStyle.bsNone..TFormBorderStyle.bsSingle;



  TChangeLink = class(TImageLink)
  private
    function GetSender: TCustomImageList; inline;
    procedure SetSender(const Value: TCustomImageList); inline;
  public
    constructor Create; override;
    property Sender: TCustomImageList read GetSender write SetSender;
  end;

  INT_PTR = Integer; //do not change on Int64 //System.IntPtr;    // NativeInt;
  {$EXTERNALSYM INT_PTR}
  UINT_PTR = Cardinal; //do not change on Int64 //System.UIntPtr;  // NativeUInt;

  WPARAM = UINT_PTR;
  LPARAM = INT_PTR;
  LRESULT = INT_PTR;

  TDWordFiller = record
  {$IFDEF CPUX64}
    Filler: array[1..4] of Byte; // Pad DWORD to make it 8 bytes (4+4) [x64 only]
  {$ENDIF}
  end;

//--------- Windows messages simulations ---------------------------------------------------------------------------------------------------

const
  WM_APP              = $8000;
  WM_MOUSEFIRST       = $0200;
  WM_MOUSEMOVE        = $0200;
  WM_LBUTTONDOWN      = $0201;
  WM_LBUTTONUP        = $0202;
  WM_LBUTTONDBLCLK    = $0203;
  WM_RBUTTONDOWN      = $0204;
  WM_RBUTTONUP        = $0205;
  WM_RBUTTONDBLCLK    = $0206;
  WM_MBUTTONDOWN      = $0207;
  WM_MBUTTONUP        = $0208;
  WM_MBUTTONDBLCLK    = $0209;
  WM_MOUSEWHEEL       = $020A;
  WM_SIZE             = $0005;
  WM_NCMBUTTONDOWN    = $00A7;
  WM_NCMBUTTONUP      = $00A8;
  WM_NCMBUTTONDBLCLK  = $00A9;
  WM_NCLBUTTONDBLCLK  = $00A3;
  WM_NCRBUTTONDOWN    = $00A4;
  WM_NCRBUTTONUP      = $00A5;
  WM_NCRBUTTONDBLCLK  = $00A6;
  WM_NCLBUTTONDOWN    = $00A1;
  WM_NCLBUTTONUP      = $00A2;
  WM_NCMOUSEMOVE      = $00A0;
  WM_KEYDOWN          = $0100;
  WM_KEYUP            = $0101;
  WM_SETFOCUS         = $0007;
  WM_KILLFOCUS        = $0008;
  WM_SETCURSOR        = $0020;
  WM_HSCROLL          = $0114;
  WM_VSCROLL          = $0115;
  WM_CHANGESTATE = WM_APP + 32;

  CM_BASE                   = $B000;
{$IF DEFINED(CLR)}
  CM_CLROFFSET              = $100;
{$ELSE}
  CM_CLROFFSET              = $0; // Only applicable in CLR
{$ENDIF}
  CM_ACTIVATE               = CM_BASE + 0;
  CM_DEACTIVATE             = CM_BASE + 1;
  CM_GOTFOCUS               = CM_BASE + 2;
  CM_LOSTFOCUS              = CM_BASE + 3;
  CM_CANCELMODE             = CM_BASE + CM_CLROFFSET + 4;
  CM_DIALOGKEY              = CM_BASE + 5;
  CM_DIALOGCHAR             = CM_BASE + 6;
{$IF NOT DEFINED(CLR)}
  CM_FOCUSCHANGED           = CM_BASE + 7;
{$ENDIF}
  CM_PARENTFONTCHANGED      = CM_BASE + CM_CLROFFSET + 8;
  CM_PARENTCOLORCHANGED     = CM_BASE + 9;
  CM_BIDIMODECHANGED        = CM_BASE + 60;
  CM_PARENTBIDIMODECHANGED  = CM_BASE + 61;
  CM_MOUSEWHEEL             = CM_BASE + 67;

  VK_ESCAPE = 27;

type
  PMessage = ^TMessage;
  TMessage = record
    Msg: Cardinal;                      //4
    tmp: Integer;                       //4
    case Integer of
      0: (
        WParam: WPARAM;                 //4
        LParam: LPARAM;                 //4
        Result: LRESULT                 //4
        );                              //= 12 + 4 = 16
      1: (
        WParamLo: Word;                 //2
        WParamHi: Word;                 //2
        //WParamFiller: TDWordFiller;
        LParamLo: Word;                 //2
        LParamHi: Word;                 //2
        //LParamFiller: TDWordFiller;
        ResultLo: Word;                 //2
        ResultHi: Word;                 //2
                                        //=12 + 8 = 20
        );
  end;

  TWMMouse = record
    Msg: Cardinal;                      //4
    Keys: Longint; //TShiftState;       //4
    //KeysFiller: TDWordFiller;
    case Integer of
      0: (
        XPos: Single;                   //4
        YPos: Single;                   //4
        Result: LRESULT;                //4
        );
      1: (
        Pos: TPoint;                    //8
        ResultLo: Word;                 //2
        ResultHi: Word;                 //2
        );                              //=12 + 8=20
  end;

  TWMMouseMove = TWMMouse;

  TWMNCHitTest = record
    Msg: Cardinal;
    //MsgFiller: TDWordFiller;
    Unused: WPARAM;
    case Integer of
      0: (
        XPos: Single;
        YPos: Single;
        //XYPosFiller: TDWordFiller
        );
      1: (
        Pos: TPoint;
        //PosFiller: TDWordFiller;
        Result: LRESULT);
  end;

  TWMNCHitMessage = record
    Msg: Cardinal;                       //4
    //MsgFiller: TDWordFiller;
    HitTest: Longint;                    //4
    //HitTestFiller: TDWordFiller;
    XCursor: Single;                     //4
    YCursor: Single;                     //4
    //XYCursorFiller: TDWordFiller;
    Result: LRESULT;                     //4
  end;                                   //=20

  TWMNCLButtonDblClk = TWMNCHitMessage;
  TWMNCLButtonDown   = TWMNCHitMessage;
  TWMNCLButtonUp     = TWMNCHitMessage;
  TWMNCMButtonDblClk = TWMNCHitMessage;
  TWMNCMButtonDown   = TWMNCHitMessage;
  TWMNCMButtonUp     = TWMNCHitMessage;
  TWMNCMouseMove     = TWMNCHitMessage;
  TWMNCRButtonDblClk = TWMNCHitMessage;
  TWMNCRButtonDown   = TWMNCHitMessage;
  TWMNCRButtonUp     = TWMNCHitMessage;

  TWMLButtonDblClk = TWMMouse;
  TWMLButtonDown   = TWMMouse;
  TWMLButtonUp     = TWMMouse;
  TWMMButtonDblClk = TWMMouse;
  TWMMButtonDown   = TWMMouse;
  TWMMButtonUp     = TWMMouse;


  TWMKey = record
    Msg: Cardinal;                       //4
    tmp: Integer;                        //4
    CharCode: Word;                      //4
    //Unused: Word;                        //2
    KeyData: Longint;                    //4
    Result: LRESULT;                     //4
  end;                                   //=20

  TWMKeyDown = TWMKey;
  TWMKeyUp = TWMKey;

  TWMSize = record
    Msg: Cardinal;                       //4
    //MsgFiller: TDWordFiller;
    SizeType: WPARAM;  { SIZE_MAXIMIZED, SIZE_MINIMIZED, SIZE_RESTORED,  //4
                         SIZE_MAXHIDE, SIZE_MAXSHOW }
    Width: Single;                       //4
    Height: Single;                      //4
    //WidthHeightFiller: TDWordFiller;
    Result: LRESULT;                     //4
  end;                                   //=20

  TWMScroll = record
    Msg: Cardinal;                      //4
    //MsgFiller: TDWordFiller;
    ScrollCode: {Smallint}Integer; { SB_xxxx }   //4
    Pos: Single;                        //4
    //ScrollCodePosFiller: TDWordFiller;
    ScrollBar: Integer;                 //4  nBar
    Result: LRESULT;                    //4
  end;                                  //=20

  TWMHScroll = TWMScroll;
  TWMVScroll = TWMScroll;

  TCMMouseWheel = record
    Msg: Cardinal;                      //4
    //MsgFiller: TDWordFiller;
    ShiftState: TShiftState;            //2
    WheelDelta: SmallInt;               //2
    //ShiftStateWheel: TDWordFiller;
    case Integer of
      0: (
        XPos: Single;                   //4
        YPos: Single;                   //4
        //XYPos: TDWordFiller
        );                              //=24!
      1: (
        Pos: TPoint;                    //8
        //PosFiller: TDWordFiller;
        Result: LRESULT                 //4
        );                              //=28!
  end;


procedure FillTWMMouse(Var MM: TWMMouse; Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single; IsNC: Boolean; IsUp: Boolean);

//--------- Text metrics -------------------------------------------------------------------------------------------------------------------
type
  TTextMetric = record
    tmHeight: Single;  //The height (ascent + descent) of characters.
    tmAscent: Single;  //The ascent (units above the base line) of characters.
    tmDescent: Single; //The descent (units below the base line) of characters.
    tmInternalLeading: Single; //The amount of leading (space) inside the bounds set by the tmHeight member. Accent marks and other diacritical characters may occur in this area. The designer may set this member to zero
    tmExternalLeading: Single; //The amount of extra leading (space) that the application adds between rows. Since this area is outside the font, it contains no marks and is not altered by text output calls in either OPAQUE or TRANSPARENT mode. The designer may set this member to zero.
    tmAveCharWidth: Single; //The average width of characters in the font (generally defined as the width of the letter x ). This value does not include the overhang required for bold or italic characters.
    tmMaxCharWidth: Single; //The width of the widest character in the font.
    tmWeight: Single; //The weight of the font.
    tmOverhang: Single;
    tmDigitizedAspectX: Single; //The horizontal aspect of the device for which the font was designed.
    tmDigitizedAspectY: Single; //The vertical aspect of the device for which the font was designed. The ratio of the tmDigitizedAspectX and tmDigitizedAspectY members is the aspect ratio of the device for which the font was designed.
    tmFirstChar: WideChar; //The value of the first character defined in the font.
    tmLastChar: WideChar;  //The value of the last character defined in the font.
    tmDefaultChar: WideChar; //The value of the character to be substituted for characters not in the font.
    tmBreakChar: WideChar; //The value of the character that will be used to define word breaks for text justification.
    tmItalic: Byte; //Specifies an italic font if it is nonzero.
    tmUnderlined: Byte; //Specifies an underlined font if it is nonzero.
    tmStruckOut: Byte; //A strikeout font if it is nonzero.
    tmPitchAndFamily: Byte; //Specifies information about the pitch, the technology, and the family of a physical font.  TMPF_FIXED_PITCH, TMPF_VECTOR, TMPF_TRUETYPE, TMPF_DEVICE
    tmCharSet: Byte; //The character set of the font. The character set can be one of the following values. ANSI_CHARSET, GREEK_CHARSET....
  end;
  procedure GetTextMetrics(ACanvas: TCanvas; var TM: TTextMetric);

//-------- function aliassing --------------------------------------------------------------------------------------------------------------

function Rect(ALeft, ATop, ARight, ABottom: Single): TRect; overload; inline;
function Rect(const ATopLeft, ABottomRight: TPoint): TRect; overload; inline;
function Point(AX, AY: Single): TPoint; overload; inline;

procedure Inc(Var V: Single; OIle: Single=1.0); overload;
procedure Dec(Var V: Single; OIle: Single=1.0); overload;
function MulDiv(const A, B, C: Single): Single; overload;
procedure FillMemory(Destination: Pointer; Length: NativeUInt; Fill: Byte);
procedure ZeroMemory(Destination: Pointer; Length: NativeUInt);
procedure MoveMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);

procedure DrawTextW(ACanvas: TCanvas; CaptionText: String; Len: Integer; Var Bounds: TRect; DrawFormat: Cardinal{this is windows format - must be converted to FMX});
procedure GetTextExtentPoint32W(ACanvas: TCanvas; CaptionText: String; Len: Integer; Var Size: TSize);
procedure DrawEdge(Canvas: TCanvas; R: TRect; edge, grfFlags: Cardinal);

type
  THighQualityBitmap = class(TBitmap)
    public
      constructor Create; override;
  end;

//fill system images
procedure FillSystemCheckImages(Parent: TFmxObject; List: TImageList);

type
  TCanvasHelper = class helper for TCanvas
  private
    function GetBrush: TBrush; inline;
    function GetPen: TStrokeBrush; inline;
  public
    property Brush: TBrush read GetBrush;
    property Pen: TStrokeBrush read GetPen;
    procedure FillRect(const ARect: TRectF); overload; inline;
    procedure DrawRect(const ARect: TRectF); overload; inline;
    procedure DrawFocusRect(const AFocusRect: TRect);
    procedure FrameRect(const AFocusRect: TRect);
    procedure RoundRect(X1, Y1, X2, Y2: Single; const XRadius, YRadius: Single); overload;
    procedure RoundRect(const Rect: TRect; const XRadius, YRadius: Single); overload;
    procedure Polygon(const Points: TPolygon);
    procedure Draw(const X, Y: Single; const Bitmap: TBitmap);											  
  end;

  TFontHelper = class helper for TFont
  private
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

{ Draws a solid triangular arrow that can point in any TScrollDirection }

type
  TScrollDirection = (sdLeft, sdRight, sdUp, sdDown);
  TArrowType = (atSolid, atArrows);

procedure DrawArrow(ACanvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Single);

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);

procedure OleUninitialize();

function timeGetTime: Int64;

implementation
uses
    System.SysUtils
  , FMX.TextLayout
  , FMX.MultiResBitmap
  , FMX.Objects
  , FMX.Effects
  , VirtualTrees.Utils
  ;

//----------------------------------------------------------------------------------------------------------------------

procedure DrawArrow(ACanvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Single);
begin
  //TODO: DrawArrow implementation
end;

//----------------------------------------------------------------------------------------------------------------------

{ TCanvasHelper }

procedure TCanvasHelper.Draw(const X, Y: Single; const Bitmap: TBitmap);
begin
  DrawBitmap(Bitmap
          , Rect(0, 0, Bitmap.Width, Bitmap.Height)
          , Rect(X, Y, X+Bitmap.Width, Y+ Bitmap.Height)
          , 1.0
          );
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCanvasHelper.DrawFocusRect(const AFocusRect: TRect);
begin
  DrawDashRect(AFocusRect, 0, 0, AllCorners, 1.0{?}, $A0909090);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCanvasHelper.DrawRect(const ARect: TRectF);
begin
  DrawRect(ARect, 0, 0, [], 1.0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCanvasHelper.FillRect(const ARect: TRectF);
begin
  FillRect(ARect, 0, 0, [], 1.0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCanvasHelper.FrameRect(const AFocusRect: TRect);
begin
  DrawRect(AFocusRect);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCanvasHelper.GetBrush: TBrush;
begin
  Result:= Fill;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCanvasHelper.GetPen: TStrokeBrush;
begin
  Result:= Stroke;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCanvasHelper.Polygon(const Points: TPolygon);
begin
  DrawPolygon(Points, 1.0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCanvasHelper.RoundRect(const Rect: TRect; const XRadius, YRadius: Single);
begin
  DrawRect(Rect, XRadius, YRadius, allCorners, 1.0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCanvasHelper.RoundRect(X1, Y1, X2, Y2: Single; const XRadius, YRadius: Single);
begin
  RoundRect(Rect(X1, Y1, X2, Y2), XRadius, YRadius);
end;


//----------------------------------------------------------------------------------------------------------------------

type
  TImageListHelper = class helper for TImageList
    function Add(aBitmap: TBitmap): integer;
  end;

function TImageListHelper.Add(aBitmap: TBitmap): integer;
const
  SCALE = 1;
var
  vSource: TCustomSourceItem;
  vBitmapItem: TCustomBitmapItem;
  vDest: TCustomDestinationItem;
  vLayer: TLayer;
begin
  Result := -1;
  if (aBitmap.Width = 0) or (aBitmap.Height = 0) then exit;

  // add source bitmap
  vSource := Source.Add;
  vSource.MultiResBitmap.TransparentColor := TColorRec.Fuchsia;
  vSource.MultiResBitmap.SizeKind := TSizeKind.Source;
  vSource.MultiResBitmap.Width := Round(aBitmap.Width / SCALE);
  vSource.MultiResBitmap.Height := Round(aBitmap.Height / SCALE);
  vBitmapItem := vSource.MultiResBitmap.ItemByScale(SCALE, True, True);
  if vBitmapItem = nil then
  begin
    vBitmapItem := vSource.MultiResBitmap.Add;
    vBitmapItem.Scale := Scale;
  end;
  vBitmapItem.Bitmap.Assign(aBitmap);

  vDest := Destination.Add;
  vLayer := vDest.Layers.Add;
  vLayer.SourceRect.Rect := TRectF.Create(TPoint.Zero, vSource.MultiResBitmap.Width,
      vSource.MultiResBitmap.Height);
  vLayer.Name := vSource.Name;
  Result := vDest.Index;
end;

//----------------------------------------------------------------------------------------------------------------------
//https://stackoverflow.com/questions/22813461/is-there-an-equivalent-to-floodfill-in-fmx-for-a-tbitmap
procedure Bitmap_FloodFill(fBitmap: TBitmap; StartX,StartY : Integer; FillColor: TAlphaColor);
var
  fBitmapData  : TBitmapData;
  X, Y         : Integer;
  ReplaceColor : TAlphaColor;
  Stack        : Array of System.Types.TPoint;
  fHeight      : Integer;
  fWidth       : Integer;

  procedure PutInStack(X, Y: Integer);
  begin
    SetLength(Stack, Length(Stack)+1);
    Stack[Length(Stack)-1] := Point(X, Y);
  end;

  procedure GetFromStack(var X, Y: Integer);
  begin
    X := Stack[Length(Stack)-1].X;
    Y := Stack[Length(Stack)-1].Y;
    SetLength(Stack, Length(Stack)-1);
  end;

begin
  X := StartX;
  Y := StartY;
  fHeight := fBitmap.Height;
  fWidth  := fBitmap.Width;
  if (X >= fWidth) or (Y >= fHeight) then Exit;

  if fBitmap.Map(TMapAccess.ReadWrite,fBitmapData) then
  try
    ReplaceColor := fBitmapData.GetPixel(X,Y);
    if ReplaceColor <> FillColor then
    begin
      PutInStack(X,Y);
      while Length(Stack) > 0 do
      begin
        GetFromStack(X,Y);
        while (X >      0) and (fBitmapData.GetPixel(X-1, Y) = ReplaceColor) do System.Dec(X);
        while (X < fWidth) and (fBitmapData.GetPixel(X  , Y) = ReplaceColor) do
        begin
          if Y   >       0 then If fBitmapData.GetPixel(X, Y-1) = ReplaceColor then PutInStack(X, Y-1);
          if Y+1 < fHeight then If fBitmapData.GetPixel(X, Y+1) = ReplaceColor then PutInStack(X, Y+1);
          fBitmapData.SetPixel(X,Y,FillColor);
          System.Inc(X);
        end;
      end;
    end;
  finally
    fBitmap.Canvas.Bitmap.Unmap(fBitmapData);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{
  ckEmpty                  =  0;  // an empty image used as place holder
  // radio buttons
  ckRadioUncheckedNormal   =  1;
  ckRadioUncheckedHot      =  2;
  ckRadioUncheckedPressed  =  3;
  ckRadioUncheckedDisabled =  4;
  ckRadioCheckedNormal     =  5;
  ckRadioCheckedHot        =  6;
  ckRadioCheckedPressed    =  7;
  ckRadioCheckedDisabled   =  8;
  // check boxes
  ckCheckUncheckedNormal   =  9;
  ckCheckUncheckedHot      = 10;
  ckCheckUncheckedPressed  = 11;
  ckCheckUncheckedDisabled = 12;
  ckCheckCheckedNormal     = 13;
  ckCheckCheckedHot        = 14;
  ckCheckCheckedPressed    = 15;
  ckCheckCheckedDisabled   = 16;
  ckCheckMixedNormal       = 17;
  ckCheckMixedHot          = 18;
  ckCheckMixedPressed      = 19;
  ckCheckMixedDisabled     = 20;
  // simple button
  ckButtonNormal           = 21; //???
  ckButtonHot              = 22; //???
  ckButtonPressed          = 23; //???
  ckButtonDisabled         = 24; //???
}
procedure FillSystemCheckImages(Parent: TFmxObject; List: TImageList);
Var cb: TCheckBox;
  rb: TRadioButton;
  BMP: TBitmap;
  eff: TInnerGlowEffect;
  procedure AddCtrlBmp(c: TControl);
  Var tmpBMP: TBitmap;
  begin
    tmpBMP:= c.MakeScreenshot;
    try
      BMP.SetSize(tmpBMP.Height, tmpBMP.Height);
      BMP.Clear(TAlphaColorRec.Null); //this somehow can sometimes clear  BeginSceneCount and must be before BeginScene
      if BMP.Canvas.BeginScene() then
        begin
          try
            BMP.Canvas.DrawBitmap(
                tmpBMP
                , Rect(2, 2, BMP.Width, BMP.Height)
                , Rect(0, 0, BMP.Width-2, BMP.Height-2)
                , 1.0
                , false
                );
          finally
            BMP.Canvas.EndScene;
          end;
        end;
    finally
      FreeAndNil(tmpBMP);
    end;
  end;
begin
  BMP:= TBitmap.Create;
  try
    BMP.SetSize(16, 16);
    BMP.Clear(TAlphaColorRec.Null);
    List.Add(BMP); //ckEmpty


    rb:= TRadioButton.Create(Parent);
    try
      rb.Parent:= Parent;
      rb.Text:= ' ';

      eff:= TInnerGlowEffect.Create(rb); //auto free
      eff.Parent:= rb;
      eff.GlowColor:= TAlphaColorRec.Teal;
      eff.Softness:= 8;
      eff.Opacity:= 0.7;
      eff.Enabled:= false;

      //------------------IsUnChecked--------------------------

      rb.IsChecked:= false;
      eff.Enabled:= false;
      AddCtrlBmp(rb);
      List.Add(BMP); //ckRadioUncheckedNormal
      eff.Enabled:= false;


      AddCtrlBmp(rb);
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightyellow;
      List.Add(BMP); //ckRadioUncheckedHot
      eff.Enabled:= false;


      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightblue;
      AddCtrlBmp(rb);
      List.Add(BMP); //ckRadioUncheckedPressed
      eff.Enabled:= false;


      rb.Enabled:= false;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Gray;
      AddCtrlBmp(rb);
      List.Add(BMP); //ckRadioUncheckedDisabled
      eff.Enabled:= false;

      //------------------IsChecked---------------------------

      rb.IsChecked:= true;

      //rb.IsPressed:= false;
      rb.Enabled:= true;
      eff.Enabled:= false;
      AddCtrlBmp(rb);
      List.Add(BMP); //ckRadioCheckedNormal
      eff.Enabled:= false;


      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightyellow;
      AddCtrlBmp(rb);
      List.Add(BMP); //ckRadioCheckedHot
      eff.Enabled:= false;


      rb.Enabled:= true;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightblue;
      AddCtrlBmp(rb);
      List.Add(BMP); //ckRadioCheckedPressed
      eff.Enabled:= false;


      rb.Enabled:= false;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Gray;
      AddCtrlBmp(rb);
      List.Add(BMP); //ckRadioCheckedDisabled
      eff.Enabled:= false;
    finally
      FreeAndNil(rb);
    end;

    cb:= TCheckBox.Create(Parent);
    try
      cb.Parent:= Parent;
      cb.Text:= ' ';

      eff:= TInnerGlowEffect.Create(cb); //auto free
      eff.Parent:= cb;
      eff.GlowColor:= TAlphaColorRec.Teal;
      eff.Softness:= 8;
      eff.Opacity:= 0.7;
      eff.Enabled:= false;

      //------------------IsUnChecked--------------------------

      cb.IsChecked:= false;
      eff.Enabled:= false;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckUncheckedNormal
      eff.Enabled:= false;

      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightyellow;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckUncheckedHot
      eff.Enabled:= false;


      //cb.IsPressed:= true;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightblue;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckUncheckedPressed
      eff.Enabled:= false;


      //cb.IsPressed:= false;
      cb.Enabled:= false;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Gray;
      AddCtrlBmp(cb);
      eff.Enabled:= false;
      List.Add(BMP); //ckCheckUncheckedDisabled

      //------------------IsChecked---------------------------

      cb.IsChecked:= true;

      cb.Enabled:= true;
      eff.Enabled:= false;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckCheckedNormal
      eff.Enabled:= false;


      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightyellow;
      eff.Opacity:= 0.3;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckCheckedHot
      eff.Opacity:= 0.7;
      eff.Enabled:= false;
      eff.Enabled:= false;


      cb.Enabled:= true;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightblue;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckCheckedPressed
      eff.Enabled:= false;


      cb.Enabled:= false;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Gray;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckCheckedDisabled
      eff.Enabled:= false;
      //------------------Mixed---------------------------

      //how to support mixed style?
      //maybe draw unchecked and fill in the center of bitmap???
      //i use ~teal for fill
      //changed to InnerGlowEffect

      cb.IsChecked:= true;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Green;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckMixedNormal
      eff.Enabled:= false;


      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightyellow;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckMixedHot
      eff.Enabled:= false;


      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Lightblue;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckMixedPressed
      eff.Enabled:= false;


      cb.Enabled:= false;
      eff.Enabled:= true;
      eff.GlowColor:= TAlphaColorRec.Gray;
      AddCtrlBmp(cb);
      List.Add(BMP); //ckCheckMixedDisabled
      eff.Enabled:= false;

    finally
      FreeAndNil(cb);
    end;
    eff.Enabled:= false;
    eff.Parent:= nil;

  finally
    FreeAndNil(BMP);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FillTWMMouse(Var MM: TWMMouse; Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single; IsNC: Boolean; IsUp: Boolean);
begin
  MM.Msg:= 0;
  if ssDouble in Shift then
    begin
      if ssLeft in Shift then
        begin
          if IsNC then
            MM.Msg:= WM_NCLBUTTONDBLCLK else
            MM.Msg:= WM_LBUTTONDBLCLK;
        end else
      if ssRight in Shift then
        begin
          if IsNC then
            MM.Msg:= WM_NCRBUTTONDBLCLK else
            MM.Msg:= WM_RBUTTONDBLCLK;
        end else
      if ssMiddle in Shift then
        begin
          if IsNC then
            MM.Msg:= WM_NCMBUTTONDBLCLK else
            MM.Msg:= WM_MBUTTONDBLCLK;
        end;
    end else
    begin
      if (ssLeft in Shift) or (Button=TMouseButton.mbLeft) then
        begin
          if IsUp then
            begin
              if IsNC then
                MM.Msg:= WM_NCLBUTTONUP else
                MM.Msg:= WM_LBUTTONUP;
            end else
            begin
              if IsNC then
                MM.Msg:= WM_NCLBUTTONDOWN else
                MM.Msg:= WM_LBUTTONDOWN;
            end;
        end else
      if (ssRight in Shift) or (Button=TMouseButton.mbRight) then
        begin
          if IsUp then
            begin
              if IsNC then
                MM.Msg:= WM_NCRBUTTONUP else
                MM.Msg:= WM_RBUTTONUP;
            end else
            begin
              if IsNC then
                MM.Msg:= WM_NCRBUTTONDOWN else
                MM.Msg:= WM_RBUTTONDOWN;
            end;

        end else
      if (ssMiddle in Shift) or (Button=TMouseButton.mbMiddle) then
        begin
          if IsUp then
            begin
              if IsNC then
                MM.Msg:= WM_NCMBUTTONUP else
                MM.Msg:= WM_MBUTTONUP;
            end else
            begin
              if IsNC then
                MM.Msg:= WM_NCMBUTTONDOWN else
                MM.Msg:= WM_MBUTTONDOWN;
            end;
        end;
    end;

  MM.XPos:= X;
  MM.YPos:= Y;
  MM.Keys:= LongInt(Word(Shift));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DrawTextW(ACanvas: TCanvas; CaptionText: String; Len: Integer; Var Bounds: TRect; DrawFormat: Cardinal{this is windows format - must be converted to FMX});
Var
  hAlign: TTextAlign;
  vAlign: TTextAlign;
  Flags: TFillTextFlags;
begin
  //TTextLayout. render
  //TODO: DrawFormat: Cardinal{this is windows format - must be converted to FMX}

  hAlign:= TTextAlign.Leading;
  if DrawFormat and DT_CENTER<>0 then
    hAlign:= TTextAlign.Center;
  if DrawFormat and DT_RIGHT<>0 then
    hAlign:= TTextAlign.Trailing;


  vAlign:= TTextAlign.Center;
  if DrawFormat and DT_VCENTER<>0 then
    vAlign:= TTextAlign.Center;
  if DrawFormat and DT_BOTTOM<>0 then
    vAlign:= TTextAlign.Trailing;

  Flags:= [];

  if DrawFormat and DT_RTLREADING<>0 then
    Flags:= Flags + [TFillTextFlag.RightToLeft];

  ACanvas.FillText(Bounds, CaptionText, false, 1.0, Flags, hAlign, vAlign);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DrawEdge(Canvas: TCanvas; R: TRect; edge, grfFlags: Cardinal);
Var tmpR: TRect;
  IsSoft, IsFlat, IsMono: Boolean;
begin

  if grfFlags and BF_SOFT<>0 then
    IsSoft:= true else
    IsSoft:= false;

  if grfFlags and BF_FLAT<>0 then
    IsFlat:= true else
    IsFlat:= false;

  if grfFlags and BF_MONO<>0 then
    IsMono:= true else
    IsMono:= false;

  if grfFlags and BF_MIDDLE<>0 then
    begin
      Canvas.Fill.Color:= clBtnFace;
      Canvas.FillRect(R, 0, 0, [], 1.0);
    end;
  tmpR:= R;
  if grfFlags and BF_LEFT<>0 then
    begin
      tmpR:= R;

      if edge and BDR_RAISEDOUTER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= TColors.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FF646464;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end;
          InflateRect(tmpR, -1, -1)
        end;

      if edge and BDR_SUNKENOUTER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FF696969;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FF646464;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end;
          InflateRect(tmpR, -1, -1)
        end;

      if edge and BDR_RAISEDINNER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFF0F0F0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end;
        end;

      if edge and BDR_SUNKENINNER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFF0F0F0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FF696969;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Left, tmpR.Bottom), 1.0);
            end;
        end;
    end;

  if grfFlags and BF_TOP<>0 then
    begin
      tmpR:= R;

      if edge and BDR_RAISEDOUTER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FF646464;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end;
          InflateRect(tmpR, -1, -1)
        end;

      if edge and BDR_SUNKENOUTER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FF696969;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FF646464;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end;
          InflateRect(tmpR, -1, -1)
        end;

      if edge and BDR_RAISEDINNER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFF0F0F0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end;
        end;

      if edge and BDR_SUNKENINNER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFF0F0F0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FF696969;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Top), Point(tmpR.Right, tmpR.Top), 1.0);
            end;
        end;


    end;

  if grfFlags and BF_RIGHT<>0 then
    begin
      tmpR:= R;
      if edge and BDR_RAISEDOUTER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FF696969;
              Canvas.DrawLine(Point(tmpR.Right-1, tmpR.Top), Point(tmpR.Right-1, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Right-1, tmpR.Top), Point(tmpR.Right-1, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FF646464;
              Canvas.DrawLine(Point(tmpR.Right-1, tmpR.Top), Point(tmpR.Right-1, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FF696969;
              Canvas.DrawLine(Point(tmpR.Right-1, tmpR.Top), Point(tmpR.Right-1, tmpR.Bottom), 1.0);
            end;
          InflateRect(tmpR, -1, -1)
        end;

      if edge and BDR_SUNKENOUTER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Right-1, tmpR.Top), Point(tmpR.Right-1, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Right-1, tmpR.Top), Point(tmpR.Right-1, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FF646464;
              Canvas.DrawLine(Point(tmpR.Right-1, tmpR.Top), Point(tmpR.Right-1, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Right-1, tmpR.Top), Point(tmpR.Right-1, tmpR.Bottom), 1.0);
            end;
          InflateRect(tmpR, -1, -1)
        end;

      Dec(tmpR.Right);

      if edge and BDR_RAISEDINNER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Right, tmpR.Top), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFF0F0F0;
              Canvas.DrawLine(Point(tmpR.Right, tmpR.Top), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Right, tmpR.Top), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Right, tmpR.Top), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end;
        end;

      if edge and BDR_SUNKENINNER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Right, tmpR.Top), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Right, tmpR.Top), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Right, tmpR.Top), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Right, tmpR.Top), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end;
        end;
    end;

  if grfFlags and BF_BOTTOM<>0 then
    begin
      tmpR:= R;
      Dec(tmpR.Bottom);
      if edge and BDR_RAISEDOUTER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FF696969;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FF646464;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FF696969;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end;
          InflateRect(tmpR, -1, -1)
        end;

      if edge and BDR_SUNKENOUTER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FF646464;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end;
          InflateRect(tmpR, -1, -1)
        end;

      if edge and BDR_RAISEDINNER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFF0F0F0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= TAlphaColorRec.White;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FFA0A0A0;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end;
        end;

      if edge and BDR_SUNKENINNER<>0 then
        begin
          if isSoft then
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if IsFlat then
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
          if isMono then
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end else
            begin
              Canvas.Stroke.Color:= $FFE3E3E3;
              Canvas.DrawLine(Point(tmpR.Left, tmpR.Bottom), Point(tmpR.Right, tmpR.Bottom), 1.0);
            end;
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure GetTextExtentPoint32W(ACanvas: TCanvas; CaptionText: String; Len: Integer; Var Size: TSize);
begin
  Size.cx:= ACanvas.TextWidth(Copy(CaptionText, 1, Len));
  Size.cy:= ACanvas.TextHeight(Copy(CaptionText, 1, Len));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure GetTextMetrics(ACanvas: TCanvas; var TM: TTextMetric);
Var P: TPathData;
  tx: TTextLayout;
  R: TRectF;
begin
{
    tmHeight: Single;  //The height (ascent + descent) of characters.
    tmAscent: Single;  //The ascent (units above the base line) of characters.
    tmDescent: Single; //The descent (units below the base line) of characters.
    tmInternalLeading: Single; //The amount of leading (space) inside the bounds set by the tmHeight member. Accent marks and other diacritical characters may occur in this area. The designer may set this member to zero
    tmExternalLeading: Single; //The amount of extra leading (space) that the application adds between rows. Since this area is outside the font, it contains no marks and is not altered by text output calls in either OPAQUE or TRANSPARENT mode. The designer may set this member to zero.
    tmAveCharWidth: Single; //The average width of characters in the font (generally defined as the width of the letter x ). This value does not include the overhang required for bold or italic characters.
    tmMaxCharWidth: Single; //The width of the widest character in the font.
    tmWeight: Single; //The weight of the font.
    tmOverhang: Single;
    tmDigitizedAspectX: Single; //The horizontal aspect of the device for which the font was designed.
    tmDigitizedAspectY: Single; //The vertical aspect of the device for which the font was designed. The ratio of the tmDigitizedAspectX and tmDigitizedAspectY members is the aspect ratio of the device for which the font was designed.
    tmFirstChar: WideChar; //The value of the first character defined in the font.
    tmLastChar: WideChar;  //The value of the last character defined in the font.
    tmDefaultChar: WideChar; //The value of the character to be substituted for characters not in the font.
    tmBreakChar: WideChar; //The value of the character that will be used to define word breaks for text justification.
    tmItalic: Byte; //Specifies an italic font if it is nonzero.
    tmUnderlined: Byte; //Specifies an underlined font if it is nonzero.
    tmStruckOut: Byte; //A strikeout font if it is nonzero.
    tmPitchAndFamily: Byte; //Specifies information about the pitch, the technology, and the family of a physical font.  TMPF_FIXED_PITCH, TMPF_VECTOR, TMPF_TRUETYPE, TMPF_DEVICE
    tmCharSet: Byte; //The character set of the font. The character set can be one of the following values. ANSI_CHARSET, GREEK_CHARSET....
}
  TM.tmExternalLeading:= 0;
  TM.tmWeight:= 0; //boldness???
  TM.tmOverhang:= 0;
  TM.tmDigitizedAspectX:= 0;
  TM.tmDigitizedAspectY:= 0;
  TM.tmFirstChar:= 'a'; //???
  TM.tmLastChar:= 'z'; //???
  TM.tmDefaultChar:= ' ';
  TM.tmBreakChar:= ' ';
  TM.tmItalic:= 0;
  TM.tmUnderlined:= 0;
  TM.tmStruckOut:= 0;
  TM.tmPitchAndFamily:= 0;
  TM.tmCharSet:= 0;

  tx:= TTextLayoutManager.DefaultTextLayout.Create(ACanvas);
  P:= TPathData.Create;
  try
    tx.Text:= 'W';
    tx.ConvertToPath(p);
    R:= P.GetBounds();

    TM.tmHeight:= R.Height;
    TM.tmMaxCharWidth:= R.Width;

    //------------------------------------
    tx.Text:= 'Ó';
    p.Clear;
    tx.ConvertToPath(p);
    R:= P.GetBounds();
    TM.tmInternalLeading:= R.Height - TM.tmHeight;

    //------------------------------------
    tx.Text:= 'x';
    p.Clear;
    tx.ConvertToPath(p);
    R:= P.GetBounds();
    TM.tmAscent:= R.Height - TM.tmHeight;
    TM.tmAveCharWidth:= R.Width;

    //------------------------------------
    tx.Text:= 'y';
    p.Clear;
    tx.ConvertToPath(p);
    TM.tmDescent:= P.GetBounds().Height - R.Height;
    TM.tmHeight:= TM.tmHeight + TM.tmDescent;
  finally
    FreeAndNil(P);
    FreeAndNil(tx);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function Rect(ALeft, ATop, ARight, ABottom: Single): TRect;
begin
  Result:= RectF(ALeft, ATop, ARight, ABottom);
end;

//----------------------------------------------------------------------------------------------------------------------

function Rect(const ATopLeft, ABottomRight: TPoint): TRect;
begin
  Result:= RectF(ATopLeft.X, ATopLeft.Y, ABottomRight.X, ABottomRight.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

function Point(AX, AY: Single): TPoint;
begin
  Result.X:= AX;
  Result.Y:= AY;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Inc(Var V: Single; OIle: Single=1.0);
begin
  V:= V + OIle;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Dec(Var V: Single; OIle: Single=1.0);
begin
  V:= V - OIle;
end;

//----------------------------------------------------------------------------------------------------------------------

function MulDiv(const A, B, C: Single): Single;
begin
  Result:= (A * B) / C;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FillMemory(Destination: Pointer; Length: NativeUInt; Fill: Byte);
begin
  FillChar(Destination^, Length, Fill);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ZeroMemory(Destination: Pointer; Length: NativeUInt);
begin
  FillChar(Destination^, Length, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MoveMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
begin
  Move(Source^, Destination^, Length);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
begin
  Move(Source^, Destination^, Length);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);
begin
  case Alignment of
    taLeftJustify:  Alignment := taRightJustify;
    taRightJustify: Alignment := taLeftJustify;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure OleUninitialize();
begin
 //nothing
end;

//----------------------------------------------------------------------------------------------------------------------

function timeGetTime: Int64;
begin
  Result:= TThread.GetTickCount;
end;

{ TChangeLink }

//----------------------------------------------------------------------------------------------------------------------

constructor TChangeLink.Create;
begin
  inherited;
  IgnoreIndex := True;
  IgnoreImages := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TChangeLink.GetSender: TCustomImageList;
begin
  Result := TCustomImageList(Images);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TChangeLink.SetSender(const Value: TCustomImageList);
begin
  Images := TBaseImageList(Value);
end;

{ THighQualityBitmap }

constructor THighQualityBitmap.Create;
begin

  inherited;

end;

{ TFontHelper }

function TFontHelper.GetOnChange: TNotifyEvent;
begin
  Result:= OnChanged;
end;

procedure TFontHelper.SetOnChange(const Value: TNotifyEvent);
begin
  OnChanged:= Value;
end;

end.
