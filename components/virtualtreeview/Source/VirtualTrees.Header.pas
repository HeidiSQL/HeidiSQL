unit VirtualTrees.Header;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.Controls,
  Vcl.Themes,
  Vcl.GraphUtil,
  System.UITypes, // some types moved from Vcl.* to System.UITypes
  VirtualTrees.StyleHooks,
  VirtualTrees.Utils,
  VirtualTrees.Types,
  VirtualTrees.DragImage;


{$MINENUMSIZE 1, make enumerations as small as possible}


const
  DefaultColumnOptions = [coAllowClick, coDraggable, coEnabled, coParentColor, coParentBidiMode, coResizable,
    coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor];

type
  TVTHeader = class;
  TVirtualTreeColumn = class;

  // This structure carries all important information about header painting and is used in the advanced header painting.
  THeaderPaintInfo = record
    TargetCanvas : TCanvas;
    Column : TVirtualTreeColumn;
    PaintRectangle : TRect;
    TextRectangle : TRect;
    IsHoverIndex,
      IsDownIndex,
      IsEnabled,
      ShowHeaderGlyph,
      ShowSortGlyph,
      ShowRightBorder : Boolean;
    DropMark : TVTDropMarkMode;
    GlyphPos,
      SortGlyphPos : TPoint;
    SortGlyphSize : TSize;
    procedure DrawSortArrow(pDirection : TSortDirection);
    procedure DrawDropMark();
  end;

  TVirtualTreeColumns = class;

  TVirtualTreeColumn = class(TCollectionItem)
  private
    const
    cDefaultColumnSpacing = 3;
  private
    FText,
      FHint               : string;
    FWidth                : TDimension;
    FPosition             : TColumnPosition;
    FMinWidth             : TDimension;
    FMaxWidth             : TDimension;
    FStyle                : TVirtualTreeColumnStyle;
    FImageIndex           : TImageIndex;
    FBiDiMode             : TBiDiMode;
    FLayout               : TVTHeaderColumnLayout;
    FMargin,
      FSpacing            : TDimension;
    FOptions              : TVTColumnOptions;
    FEditOptions          : TVTEditOptions;
    FEditNextColumn       : TDimension;
    FTag                  : NativeInt;
    FAlignment            : TAlignment;
    FCaptionAlignment     : TAlignment; // Alignment of the caption.
    FLastWidth            : TDimension;
    FColor                : TColor;
    FBonusPixel           : Boolean;
    FSpringRest           : Single; // Accumulator for width adjustment when auto spring option is enabled.
    FCaptionText          : string;
    FCheckBox             : Boolean;
    FCheckType            : TCheckType;
    FCheckState           : TCheckState;
    FImageRect            : TRect;
    FHasImage             : Boolean;
    FDefaultSortDirection : TSortDirection;
    function GetCaptionAlignment : TAlignment;
    function GetCaptionWidth : TDimension;
    function GetLeft : TDimension;
    function IsBiDiModeStored : Boolean;
    function IsCaptionAlignmentStored : Boolean;
    function IsColorStored : Boolean;
    procedure SetAlignment(const Value : TAlignment);
    procedure SetBiDiMode(Value : TBiDiMode);
    procedure SetCaptionAlignment(const Value : TAlignment);
    procedure SetCheckBox(Value : Boolean);
    procedure SetCheckState(Value : TCheckState);
    procedure SetCheckType(Value : TCheckType);
    procedure SetColor(const Value : TColor);
    procedure SetImageIndex(Value : TImageIndex);
    procedure SetLayout(Value : TVTHeaderColumnLayout);
    procedure SetMargin(Value : TDimension);
    procedure SetMaxWidth(Value : TDimension);
    procedure SetMinWidth(Value : TDimension);
    procedure SetOptions(Value : TVTColumnOptions);
    procedure SetPosition(Value : TColumnPosition);
    procedure SetSpacing(Value : TDimension);
    procedure SetStyle(Value : TVirtualTreeColumnStyle);

  protected
    FLeft : TDimension;
    procedure ChangeScale(M, D : TDimension); virtual;
    procedure ComputeHeaderLayout(var PaintInfo : THeaderPaintInfo; DrawFormat : Cardinal; CalculateTextRect : Boolean = False);
    procedure DefineProperties(Filer : TFiler); override;
    procedure GetAbsoluteBounds(var Left, Right : TDimension);
    function GetDisplayName : string; override;
    function GetText : string; virtual;                   // [IPK]
    procedure SetText(const Value : string); virtual;     // [IPK] private to protected & virtual
    function GetOwner : TVirtualTreeColumns; reintroduce;
    procedure InternalSetWidth(const Value : TDimension); //bypass side effects in SetWidth
    procedure ReadHint(Reader : TReader);
    procedure ReadText(Reader : TReader);
    procedure SetCollection(Value : TCollection); override;
    procedure SetWidth(Value : TDimension);
  public
    constructor Create(Collection : TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;
    function Equals(OtherColumnObj : TObject) : Boolean; override;
    function GetRect : TRect; virtual;
    property HasImage : Boolean read FHasImage;
    property ImageRect : TRect read FImageRect;
    procedure LoadFromStream(const Stream : TStream; Version : Integer);
    procedure ParentBiDiModeChanged;
    procedure ParentColorChanged;
    procedure RestoreLastWidth;
    function GetEffectiveColor() : TColor;
    procedure SaveToStream(const Stream : TStream);
    function UseRightToLeftReading : Boolean;

    property BonusPixel : Boolean read FBonusPixel write FBonusPixel;
    property CaptionText : string read FCaptionText;
    property LastWidth : TDimension read FLastWidth;
    property Left : TDimension read GetLeft;
    property Owner : TVirtualTreeColumns read GetOwner;
    property SpringRest : Single read FSpringRest write FSpringRest;
  published
    property Alignment            : TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property BiDiMode             : TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property CaptionAlignment     : TAlignment read GetCaptionAlignment write SetCaptionAlignment
      stored IsCaptionAlignmentStored default taLeftJustify;
    property CaptionWidth         : TDimension read GetCaptionWidth;
    property CheckType            : TCheckType read FCheckType write SetCheckType default ctCheckBox;
    property CheckState           : TCheckState read FCheckState write SetCheckState default csUncheckedNormal;
    property CheckBox             : Boolean read FCheckBox write SetCheckBox default False;
    property Color                : TColor read FColor write SetColor stored IsColorStored;
    property DefaultSortDirection : TSortDirection read FDefaultSortDirection write FDefaultSortDirection default sdAscending;
    property Hint                 : string read FHint write FHint;
    property ImageIndex           : TImageIndex read FImageIndex write SetImageIndex default - 1;
    property Layout               : TVTHeaderColumnLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin               : TDimension read FMargin write SetMargin default 4;
    property MaxWidth             : TDimension read FMaxWidth write SetMaxWidth default 10000;
    property MinWidth             : TDimension read FMinWidth write SetMinWidth default 10;
    property Options              : TVTColumnOptions read FOptions write SetOptions default DefaultColumnOptions;
    property EditOptions          : TVTEditOptions read FEditOptions write FEditOptions default toDefaultEdit;
    property EditNextColumn       : TDimension read FEditNextColumn write FEditNextColumn default - 1;
    property Position             : TColumnPosition read FPosition write SetPosition;
    property Spacing              : TDimension read FSpacing write SetSpacing default cDefaultColumnSpacing;
    property Style                : TVirtualTreeColumnStyle read FStyle write SetStyle default vsText;
    property Tag                  : NativeInt read FTag write FTag default 0;
    property Text                 : string read GetText write SetText;
    property Width                : TDimension read FWidth write SetWidth default 50;
  end;

  TVirtualTreeColumnClass = class of TVirtualTreeColumn;

  TColumnsArray = array of TVirtualTreeColumn;
  TCardinalArray = array of Cardinal;
  TIndexArray = array of TColumnIndex;

  TVirtualTreeColumns = class(TCollection)
  private
    FHeader           : TVTHeader;
    FHeaderBitmap     : TBitmap;      // backbuffer for drawing
    FHoverIndex,                      // currently "hot" column
    FDownIndex,                       // Column on which a mouse button is held down.
    FTrackIndex       : TColumnIndex; // Index of column which is currently being resized.
    FClickIndex       : TColumnIndex; // Index of the last clicked column.
    FCheckBoxHit      : Boolean;      // True if the last click was on a header checkbox.
    FPositionToIndex  : TIndexArray;
    FDefaultWidth     : TDimension;   // the width columns are created with
    FNeedPositionsFix : Boolean;      // True if FixPositions must still be called after DFM loading or Bidi mode change.
    FClearing         : Boolean;      // True if columns are being deleted entirely.
    FColumnPopupMenu  : TPopupMenu;   // Member for storing the TVTHeaderPopupMenu

    function GetCount : Integer;
    function GetItem(Index : TColumnIndex) : TVirtualTreeColumn;
    function GetNewIndex(P : TPoint; var OldIndex : TColumnIndex) : Boolean;
    procedure SetDefaultWidth(Value : TDimension);
    procedure SetItem(Index : TColumnIndex; Value : TVirtualTreeColumn);
    function GetTreeView: TCustomControl;
  protected
    // drag support
    FDragIndex  : TColumnIndex; // index of column currently being dragged
    FDropTarget : TColumnIndex; // current target column (index) while dragging
    FDropBefore : Boolean;      // True if drop position is in the left half of a column, False for the right
                                          // side to drop the dragged column to

    procedure AdjustAutoSize(CurrentIndex : TColumnIndex; Force : Boolean = False);
    function AdjustDownColumn(P : TPoint) : TColumnIndex;
    function AdjustHoverColumn(P : TPoint) : Boolean;
    procedure AdjustPosition(Column : TVirtualTreeColumn; Position : Cardinal);
    function CanSplitterResize(P : TPoint; Column : TColumnIndex) : Boolean;
    procedure DoCanSplitterResize(P : TPoint; Column : TColumnIndex; var Allowed : Boolean); virtual;
    procedure DrawButtonText(DC : HDC; Caption : string; Bounds : TRect; Enabled, Hot : Boolean; DrawFormat : Cardinal;
      WrapCaption : Boolean);
    procedure FixPositions;
    function GetColumnAndBounds(P : TPoint; var ColumnLeft, ColumnRight : TDimension; Relative : Boolean = True) : Integer;
    function GetOwner : TPersistent; override;
    function HandleClick(P : TPoint; Button : TMouseButton; Force, DblClick : Boolean) : Boolean; virtual;
    procedure HeaderPopupMenuAddHeaderPopupItem(const Sender : TObject; const Column : TColumnIndex; var Cmd : TAddPopupItemType);
    procedure IndexChanged(OldIndex, NewIndex : Integer);
    procedure InitializePositionArray;
    procedure Notify(Item : TCollectionItem; Action : System.Classes.TCollectionNotification); override;
    procedure ReorderColumns(RTL : Boolean);
    procedure SetHoverIndex(Index : TColumnIndex);
    procedure Update(Item : TCollectionItem); override;
    procedure UpdatePositions(Force : Boolean = False);

    property HeaderBitmap : TBitmap read FHeaderBitmap;
    property PositionToIndex : TIndexArray read FPositionToIndex;
    property HoverIndex : TColumnIndex read FHoverIndex write FHoverIndex;
    property DownIndex : TColumnIndex read FDownIndex write FDownIndex;
    property CheckBoxHit : Boolean read FCheckBoxHit write FCheckBoxHit;
    // Mitigator function to use the correct style service for this context (either the style assigned to the control for Delphi > 10.4 or the application style)
    function StyleServices(AControl : TControl = nil) : TCustomStyleServices;
  public
    constructor Create(AOwner : TVTHeader); virtual;
    destructor Destroy; override;

    function Add : TVirtualTreeColumn; virtual;
    procedure AnimatedResize(Column : TColumnIndex; NewWidth : TDimension);
    procedure Assign(Source : TPersistent); override;
    procedure Clear; virtual;
    function ColumnFromPosition(P : TPoint; Relative : Boolean = True) : TColumnIndex; overload; virtual;
    function ColumnFromPosition(PositionIndex : TColumnPosition) : TColumnIndex; overload; virtual;
    function Equals(OtherColumnsObj : TObject) : Boolean; override;
    procedure GetColumnBounds(Column : TColumnIndex; var Left, Right : TDimension);
    function GetFirstVisibleColumn(ConsiderAllowFocus : Boolean = False) : TColumnIndex;
    function GetLastVisibleColumn(ConsiderAllowFocus : Boolean = False) : TColumnIndex;
    function GetFirstColumn : TColumnIndex;
    function GetNextColumn(Column : TColumnIndex) : TColumnIndex;
    function GetNextVisibleColumn(Column : TColumnIndex; ConsiderAllowFocus : Boolean = False) : TColumnIndex;
    function GetPreviousColumn(Column : TColumnIndex) : TColumnIndex;
    function GetPreviousVisibleColumn(Column : TColumnIndex; ConsiderAllowFocus : Boolean = False) : TColumnIndex;
    function GetScrollWidth : TDimension;
    function GetVisibleColumns : TColumnsArray;
    function GetVisibleFixedWidth : TDimension;
    function IsValidColumn(Column : TColumnIndex) : Boolean;
    procedure LoadFromStream(const Stream : TStream; Version : Integer);
    procedure PaintHeader(DC : HDC; R : TRect; HOffset : TDimension); overload; virtual;
    procedure PaintHeader(TargetCanvas : TCanvas; R : TRect; const Target : TPoint;
      RTLOffset : TDimension = 0); overload; virtual;
    procedure SaveToStream(const Stream : TStream);
    procedure EndUpdate(); override;
    function TotalWidth : TDimension;

    property Count : Integer read GetCount;
    property ClickIndex : TColumnIndex read FClickIndex write FClickIndex;
    property DefaultWidth : TDimension read FDefaultWidth write SetDefaultWidth;
    property DragIndex : TColumnIndex read FDragIndex write FDragIndex;
    property DropBefore : Boolean read FDropBefore write FDropBefore;
    property DropTarget : TColumnIndex read FDropTarget write FDropTarget;
    property Items[Index : TColumnIndex] : TVirtualTreeColumn read GetItem write SetItem; default;
    property Header: TVTHeader read FHeader;
    property TrackIndex : TColumnIndex read FTrackIndex write FTrackIndex;
    property TreeView : TCustomControl read GetTreeView;
    property UpdateCount;
  end;

  TVirtualTreeColumnsClass = class of TVirtualTreeColumns;

  TVTConstraintPercent = 0 .. 100;

  TVTFixedAreaConstraints = class(TPersistent)
  private
    FHeader   : TVTHeader;
    FMaxHeightPercent, FMaxWidthPercent, FMinHeightPercent, FMinWidthPercent : TVTConstraintPercent;
    FOnChange : TNotifyEvent;
    procedure SetConstraints(Index : Integer; Value : TVTConstraintPercent);
  protected
    procedure Change;
    property Header : TVTHeader read FHeader;
  public
    constructor Create(AOwner : TVTHeader);

    procedure Assign(Source : TPersistent); override;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  published
    property MaxHeightPercent : TVTConstraintPercent index 0 read FMaxHeightPercent write SetConstraints default 0;
    property MaxWidthPercent  : TVTConstraintPercent index 1 read FMaxWidthPercent write SetConstraints default 95;
    property MinHeightPercent : TVTConstraintPercent index 2 read FMinHeightPercent write SetConstraints default 0;
    property MinWidthPercent  : TVTConstraintPercent index 3 read FMinWidthPercent write SetConstraints default 0;
  end;

  TVTHeader = class(TPersistent)
  private
    FOwner                       : TCustomControl;
    FColumns                     : TVirtualTreeColumns;
    FHeight                      : TDimension;
    FFont                        : TFont;
    FParentFont                  : Boolean;
    FOptions                     : TVTHeaderOptions;
    FStyle                       : TVTHeaderStyle; //button style
    FBackgroundColor             : TColor;
    FAutoSizeIndex               : TColumnIndex;
    FPopupMenu                   : TPopupMenu;
    FMainColumn                  : TColumnIndex; //the column which holds the tree
    FMaxHeight                   : TDimension;
    FMinHeight                   : TDimension;
    FDefaultHeight               : TDimension;
    FFixedAreaConstraints        : TVTFixedAreaConstraints; //Percentages for the fixed area (header, fixed columns).
    FImages                      : TCustomImageList;
    FImageChangeLink             : TChangeLink;             //connections to the image list to get notified about changes
    fSplitterHitTolerance        : TDimension;              //For property SplitterHitTolerance
    FSortColumn                  : TColumnIndex;
    FSortDirection               : TSortDirection;
    FDragImage                   : TVTDragImage;            //drag image management during header drag
    FLastWidth                   : TDimension;              //Used to adjust spring columns. This is the width of all visible columns, not the header rectangle.
    FRestoreSelectionColumnIndex : Integer;                 //The column that is used to implement the coRestoreSelection option
    FWasDoubleClick              : Boolean;                 // The previous mouse message was for a double click, that allows us to process mouse-up-messages differently
    function GetMainColumn : TColumnIndex;
    function GetUseColumns : Boolean;
    function IsFontStored : Boolean;
    procedure SetAutoSizeIndex(Value : TColumnIndex);
    procedure SetBackground(Value : TColor);
    procedure SetColumns(Value : TVirtualTreeColumns);
    procedure SetDefaultHeight(Value : TDimension);
    procedure SetFont(const Value : TFont);
    procedure SetHeight(Value : TDimension);
    procedure SetImages(const Value : TCustomImageList);
    procedure SetMainColumn(Value : TColumnIndex);
    procedure SetMaxHeight(Value : TDimension);
    procedure SetMinHeight(Value : TDimension);
    procedure SetOptions(Value : TVTHeaderOptions);
    procedure SetParentFont(Value : Boolean);
    procedure SetSortColumn(Value : TColumnIndex);
    procedure SetSortDirection(const Value : TSortDirection);
    procedure SetStyle(Value : TVTHeaderStyle);
    function GetRestoreSelectionColumnIndex : Integer;
    function AreColumnsStored: Boolean;
  protected
    FStates              : THeaderStates; //Used to keep track of internal states the header can enter.
    FDragStart           : TPoint;        //initial mouse drag position
    FTrackStart          : TPoint;        //client coordinates of the tracking start point
    FTrackPoint          : TPoint;        //Client coordinate where the tracking started.
    FDoingAutoFitColumns : Boolean;       //Flag to avoid using the stored width for Main column

    procedure FontChanged(Sender : TObject); virtual;
    procedure AutoScale(); virtual;
    function CanSplitterResize(P : TPoint) : Boolean;
    function CanWriteColumns : Boolean; virtual;
    procedure ChangeScale(M, D : TDimension); virtual;
    function DetermineSplitterIndex(P : TPoint) : Boolean; virtual;
    procedure DoAfterAutoFitColumn(Column : TColumnIndex); virtual;
    procedure DoAfterColumnWidthTracking(Column : TColumnIndex); virtual;
    procedure DoAfterHeightTracking; virtual;
    function DoBeforeAutoFitColumn(Column : TColumnIndex; SmartAutoFitType : TSmartAutoFitType) : Boolean; virtual;
    procedure DoBeforeColumnWidthTracking(Column : TColumnIndex; Shift : TShiftState); virtual;
    procedure DoBeforeHeightTracking(Shift : TShiftState); virtual;
    procedure DoCanSplitterResize(P : TPoint; var Allowed : Boolean); virtual;
    function DoColumnWidthDblClickResize(Column : TColumnIndex; P : TPoint; Shift : TShiftState) : Boolean; virtual;
    function DoColumnWidthTracking(Column : TColumnIndex; Shift : TShiftState; var TrackPoint : TPoint; P : TPoint) : Boolean; virtual;
    function DoGetPopupMenu(Column : TColumnIndex; Position : TPoint) : TPopupMenu; virtual;
    function DoHeightTracking(var P : TPoint; Shift : TShiftState) : Boolean; virtual;
    function DoHeightDblClickResize(var P : TPoint; Shift : TShiftState) : Boolean; virtual;
    procedure DoSetSortColumn(Value : TColumnIndex; pSortDirection : TSortDirection); virtual;
    procedure DragTo(P : TPoint); virtual;
    procedure FixedAreaConstraintsChanged(Sender : TObject);
    function GetColumnsClass : TVirtualTreeColumnsClass; virtual;
    function GetOwner : TPersistent; override;
    function GetShiftState : TShiftState;
    function HandleHeaderMouseMove(var Message : TWMMouseMove) : Boolean;
    function HandleMessage(var Message : TMessage) : Boolean; virtual;
    procedure ImageListChange(Sender : TObject);
    procedure PrepareDrag(P, Start : TPoint);
    procedure ReadColumns(Reader : TReader);
    procedure RecalculateHeader; virtual;
    procedure RescaleHeader;
    procedure UpdateMainColumn;
    procedure UpdateSpringColumns;
    procedure WriteColumns(Writer : TWriter);
    procedure InternalSetMainColumn(const Index : TColumnIndex);
    procedure InternalSetAutoSizeIndex(const Index : TColumnIndex);
    procedure InternalSetSortColumn(const Index : TColumnIndex);
  public
    constructor Create(AOwner : TCustomControl); virtual;
    destructor Destroy; override;

    function AllowFocus(ColumnIndex : TColumnIndex) : Boolean;
    procedure Assign(Source : TPersistent); override;
    procedure AutoFitColumns(); overload;
    procedure AutoFitColumns(Animated : Boolean; SmartAutoFitType : TSmartAutoFitType = smaUseColumnOption; RangeStartCol : Integer = NoColumn;  RangeEndCol : Integer = NoColumn); overload; virtual;
    function InHeader(P : TPoint) : Boolean; virtual;
    function InHeaderSplitterArea(P : TPoint) : Boolean; virtual;
    procedure Invalidate(Column : TVirtualTreeColumn; ExpandToBorder : Boolean = False; UpdateNowFlag : Boolean = False);
    procedure LoadFromStream(const Stream : TStream); virtual;
    function ResizeColumns(ChangeBy : TDimension; RangeStartCol : TColumnIndex; RangeEndCol : TColumnIndex; Options : TVTColumnOptions = [coVisible]) : TDimension;
    procedure RestoreColumns;
    procedure SaveToStream(const Stream : TStream); virtual;
    procedure StyleChanged(); virtual;
    procedure ToggleSortDirection();

    property DragImage : TVTDragImage read FDragImage;
    property RestoreSelectionColumnIndex : Integer read GetRestoreSelectionColumnIndex write FRestoreSelectionColumnIndex default NoColumn;
    property States : THeaderStates read FStates;
    property Treeview : TCustomControl read FOwner;
    property UseColumns : Boolean read GetUseColumns;
    property doingAutoFitColumns : Boolean read FDoingAutoFitColumns;
  published
    property AutoSizeIndex        : TColumnIndex read FAutoSizeIndex write SetAutoSizeIndex;
    property Background           : TColor read FBackgroundColor write SetBackground default clBtnFace;
    property Columns              : TVirtualTreeColumns read FColumns write SetColumns stored AreColumnsStored;
    property DefaultHeight        : TDimension read FDefaultHeight write SetDefaultHeight default 19;
    property Font                 : TFont read FFont write SetFont stored IsFontStored;
    property FixedAreaConstraints : TVTFixedAreaConstraints read FFixedAreaConstraints write FFixedAreaConstraints;
    property Height               : TDimension read FHeight write SetHeight default 19;
    property Images               : TCustomImageList read FImages write SetImages;
    property MainColumn           : TColumnIndex read GetMainColumn write SetMainColumn default 0;
    property MaxHeight            : TDimension read FMaxHeight write SetMaxHeight default 10000;
    property MinHeight            : TDimension read FMinHeight write SetMinHeight default 10;
    property Options              : TVTHeaderOptions read FOptions write SetOptions default [hoColumnResize, hoDrag, hoShowSortGlyphs];
    property ParentFont           : Boolean read FParentFont write SetParentFont default True;
    property PopupMenu            : TPopupMenu read FPopupMenu write FPopupMenu;
    property SortColumn           : TColumnIndex read FSortColumn write SetSortColumn default NoColumn;
    property SortDirection        : TSortDirection read FSortDirection write SetSortDirection default sdAscending;
    property SplitterHitTolerance : TDimension read fSplitterHitTolerance write fSplitterHitTolerance default 8;
    //The area in pixels around a spliter which is sensitive for resizing
    property Style                : TVTHeaderStyle read FStyle write SetStyle default hsThickButtons;
  end;

  TVTHeaderClass = class of TVTHeader;

implementation

uses
  WinApi.ShlObj,
  WinApi.UxTheme,
  System.Math,
  System.SysUtils,
  System.Generics.Defaults,
  Vcl.Forms,
  VirtualTrees.HeaderPopup,
  VirtualTrees.BaseTree,
  VirtualTrees.BaseAncestorVcl{to eliminate H2443 about inline expanding}
  ;

type
  TVirtualTreeColumnsCracker = class(TVirtualTreeColumns);
  TVirtualTreeColumnCracker = class(TVirtualTreeColumn);
  TBaseVirtualTreeCracker = class(TBaseVirtualTree);

  TVTHeaderHelper = class helper for TVTHeader
  public
    function Tree : TBaseVirtualTreeCracker;
  end;

  TVirtualTreeColumnHelper = class helper for TVirtualTreeColumn
    function TreeViewControl : TBaseVirtualTreeCracker;
    function Header : TVTHeader;
  end;

  TVirtualTreeColumnsHelper = class helper for TVirtualTreeColumns
    function TreeViewControl : TBaseVirtualTreeCracker;
  end;



  //----------------- TVTFixedAreaConstraints ----------------------------------------------------------------------------

constructor TVTFixedAreaConstraints.Create(AOwner : TVTHeader);

begin
  inherited Create;
  FMaxWidthPercent := 95;
  FHeader := AOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTFixedAreaConstraints.SetConstraints(Index : Integer; Value : TVTConstraintPercent);

begin
  case Index of
    0 :
      if Value <> FMaxHeightPercent then
      begin
        FMaxHeightPercent := Value;
        if (Value > 0) and (Value < FMinHeightPercent) then
          FMinHeightPercent := Value;
        Change;
      end;
    1 :
      if Value <> FMaxWidthPercent then
      begin
        FMaxWidthPercent := Value;
        if (Value > 0) and (Value < FMinWidthPercent) then
          FMinWidthPercent := Value;
        Change;
      end;
    2 :
      if Value <> FMinHeightPercent then
      begin
        FMinHeightPercent := Value;
        if (FMaxHeightPercent > 0) and (Value > FMaxHeightPercent) then
          FMaxHeightPercent := Value;
        Change;
      end;
    3 :
      if Value <> FMinWidthPercent then
      begin
        FMinWidthPercent := Value;
        if (FMaxWidthPercent > 0) and (Value > FMaxWidthPercent) then
          FMaxWidthPercent := Value;
        Change;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTFixedAreaConstraints.Change;

begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTFixedAreaConstraints.Assign(Source : TPersistent);

begin
  if Source is TVTFixedAreaConstraints then
  begin
    FMaxHeightPercent := TVTFixedAreaConstraints(Source).FMaxHeightPercent;
    FMaxWidthPercent := TVTFixedAreaConstraints(Source).FMaxWidthPercent;
    FMinHeightPercent := TVTFixedAreaConstraints(Source).FMinHeightPercent;
    FMinWidthPercent := TVTFixedAreaConstraints(Source).FMinWidthPercent;
    Change;
  end
  else
    inherited;
end;

//----------------- TVTHeader -----------------------------------------------------------------------------------------

constructor TVTHeader.Create(AOwner : TCustomControl);

begin
  inherited Create;
  FOwner := AOwner;
  FColumns := GetColumnsClass.Create(Self);
  FHeight := 19;
  FDefaultHeight := FHeight;
  FMinHeight := 10;
  FMaxHeight := 10000;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FParentFont := True;
  FBackgroundColor := clBtnFace;
  FOptions := [hoColumnResize, hoDrag, hoShowSortGlyphs];

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FSortColumn := NoColumn;
  FSortDirection := sdAscending;
  FMainColumn := NoColumn;

  FDragImage := TVTDragImage.Create(AOwner);
  with FDragImage do
  begin
    Fade := False;
    PreBlendBias := - 50;
    Transparency := 140;
  end;

  fSplitterHitTolerance := 8;
  FFixedAreaConstraints := TVTFixedAreaConstraints.Create(Self);
  FFixedAreaConstraints.OnChange := FixedAreaConstraintsChanged;

  FDoingAutoFitColumns := False;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTHeader.Destroy;

begin
  FDragImage.Free;
  FFixedAreaConstraints.Free;
  FImageChangeLink.Free;
  FFont.Free;
  FColumns.Clear; //TCollection's Clear method is not virtual, so we have to call our own Clear method manually.
  FColumns.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.FontChanged(Sender : TObject);
begin
  inherited;
  AutoScale();
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.AutoScale();
var
  I          : Integer;
  lMaxHeight : TDimension;
begin
  if (toAutoChangeScale in TBaseVirtualTreeCracker(Tree).TreeOptions.AutoOptions) then
  begin
    //Ensure a minimum header size based on the font, so that all text is visible.
    //First find the largest Columns[].Spacing
    lMaxHeight := 0;
    for I := 0 to Self.Columns.Count - 1 do
      lMaxHeight := Max(lMaxHeight, Columns[I].Spacing);
    //Calculate the required height based on the font, this is important as the user might just have increased the size of the system icon font.
    with TBitmap.Create do
      try
        Canvas.Font.Assign(FFont);
        lMaxHeight := lMaxHeight { top spacing } + Divide(lMaxHeight, 2) { minimum bottom spacing } + Canvas.TextHeight('Q');
      finally
        Free;
      end;
    //Set the calculated size
    Self.SetHeight(lMaxHeight);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetMainColumn : TColumnIndex;
begin
  if FColumns.Count > 0 then
    Result := FMainColumn
  else
    Result := NoColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetUseColumns : Boolean;
begin
  Result := FColumns.Count > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.IsFontStored : Boolean;
begin
  Result := not ParentFont;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetAutoSizeIndex(Value : TColumnIndex);
begin
  if FAutoSizeIndex <> Value then
  begin
    FAutoSizeIndex := Value;
    if hoAutoResize in FOptions then
      TVirtualTreeColumnsCracker(Columns).AdjustAutoSize(InvalidColumn);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetBackground(Value : TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Invalidate(nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetColumns(Value : TVirtualTreeColumns);

begin
  FColumns.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetDefaultHeight(Value : TDimension);
begin
  if Value < FMinHeight then
    Value := FMinHeight;
  if Value > FMaxHeight then
    Value := FMaxHeight;

  if FHeight = FDefaultHeight then
    SetHeight(Value);
  FDefaultHeight := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetFont(const Value : TFont);
begin
  FFont.Assign(Value);
  FParentFont := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetHeight(Value : TDimension);

var
  RelativeMaxHeight, RelativeMinHeight, EffectiveMaxHeight, EffectiveMinHeight : TDimension;
begin
  if not Tree.HandleAllocated then
  begin
    FHeight := Value;
    Include(FStates, hsNeedScaling);
  end
  else
  begin
    with FFixedAreaConstraints do
    begin
      RelativeMaxHeight := Divide((Tree.ClientHeight + FHeight) * FMaxHeightPercent, 100);
      RelativeMinHeight := Divide((Tree.ClientHeight + FHeight) * FMinHeightPercent, 100);

      EffectiveMinHeight := IfThen(FMaxHeightPercent > 0, Min(RelativeMaxHeight, FMinHeight), FMinHeight);
      EffectiveMaxHeight := IfThen(FMinHeightPercent > 0, Max(RelativeMinHeight, FMaxHeight), FMaxHeight);

      Value := Min(Max(Value, EffectiveMinHeight), EffectiveMaxHeight);
      if FMinHeightPercent > 0 then
        Value := Max(RelativeMinHeight, Value);
      if FMaxHeightPercent > 0 then
        Value := Min(RelativeMaxHeight, Value);
    end;

    if FHeight <> Value then
    begin
      FHeight := Value;
      if not (csLoading in Tree.ComponentState) and not (hsScaling in FStates) then
        RecalculateHeader;
      Tree.Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetImages(const Value : TCustomImageList);

begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(FOwner);
    end;
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(FOwner);
    end;
    if not (csLoading in Tree.ComponentState) then
      Invalidate(nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetMainColumn(Value : TColumnIndex);

begin
  if (csLoading in Tree.ComponentState) or (csDestroying in Tree.ComponentState) then
    FMainColumn := Value
  else
  begin
    if Value < 0 then
      Value := 0;
    if Value > FColumns.Count - 1 then
      Value := FColumns.Count - 1;
    if Value <> FMainColumn then
    begin
      FMainColumn := Value;
      Tree.MainColumnChanged;
      if not (toExtendedFocus in Tree.TreeOptions.SelectionOptions) then
        Tree.FocusedColumn := FMainColumn;
      Tree.Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetMaxHeight(Value : TDimension);

begin
  if Value < FMinHeight then
    Value := FMinHeight;
  FMaxHeight := Value;
  SetHeight(FHeight);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetMinHeight(Value : TDimension);

begin
  if Value < 0 then
    Value := 0;
  if Value > FMaxHeight then
    Value := FMaxHeight;
  FMinHeight := Value;
  SetHeight(FHeight);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetOptions(Value : TVTHeaderOptions);

var
  ToBeSet, ToBeCleared : TVTHeaderOptions;

begin
  ToBeSet := Value - FOptions;
  ToBeCleared := FOptions - Value;
  FOptions := Value;

  if (hoAutoResize in (ToBeSet + ToBeCleared)) and (FColumns.Count > 0) then
  begin
    TVirtualTreeColumnsCracker(FColumns).AdjustAutoSize(InvalidColumn);
    if Tree.HandleAllocated then
    begin
      Tree.UpdateHorizontalScrollBar(False);
      if hoAutoResize in ToBeSet then
        Tree.Invalidate;
    end;
  end;

  if not (csLoading in Tree.ComponentState) and Tree.HandleAllocated then
  begin
    if hoVisible in (ToBeSet + ToBeCleared) then
      RecalculateHeader;
    Invalidate(nil);
    Tree.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetParentFont(Value : Boolean);

begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    if FParentFont then
      FFont.Assign(TBaseVirtualTree(FOwner).Font);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetSortColumn(Value : TColumnIndex);

begin
  if csLoading in Tree.ComponentState then
    FSortColumn := Value
  else
    DoSetSortColumn(Value, FSortDirection);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetSortDirection(const Value : TSortDirection);

begin
  if Value <> FSortDirection then
  begin
    FSortDirection := Value;
    Invalidate(nil);
    if ((toAutoSort in Tree.TreeOptions.AutoOptions) or (hoHeaderClickAutoSort in Options)) and (Tree.UpdateCount = 0) then
      Tree.SortTree(FSortColumn, FSortDirection, True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.CanSplitterResize(P : TPoint) : Boolean;

begin
  Result := hoHeightResize in FOptions;
  DoCanSplitterResize(P, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetStyle(Value : TVTHeaderStyle);

begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if not (csLoading in Tree.ComponentState) then
      Invalidate(nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.StyleChanged();
begin
  AutoScale(); //Elements may have changed in size
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.CanWriteColumns : Boolean;

//descendants may override this to optionally prevent column writing (e.g. if they are build dynamically).

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.ChangeScale(M, D : TDimension);
var
  I : Integer;
begin
  //This method is only executed if toAutoChangeScale is set
  FMinHeight := MulDiv(FMinHeight, M, D);
  FMaxHeight := MulDiv(FMaxHeight, M, D);
  Self.Height := MulDiv(FHeight, M, D);
  //Scale the columns widths too
  for I := 0 to FColumns.Count - 1 do
    TVirtualTreeColumnCracker(Self.FColumns[I]).ChangeScale(M, D);
  if not ParentFont then
    Font.Height := MulDiv(Font.Height, M, D);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DetermineSplitterIndex(P : TPoint) : Boolean;

//Tries to find the index of that column whose right border corresponds to P.
//Result is True if column border was hit (with -3..+5 pixels tolerance).
//For continuous resizing the current track index and the column's left/right border are set.
//Note: The hit test is checking from right to left (or left to right in RTL mode) to make enlarging of zero-sized
//columns possible.

var
  VisibleFixedWidth : TDimension;
  SplitPoint        : TDimension;

  //--------------- local function --------------------------------------------

  function IsNearBy(IsFixedCol : Boolean; LeftTolerance, RightTolerance : TDimension) : Boolean;

  begin
    if IsFixedCol then
      Result := (P.X < SplitPoint + Tree.EffectiveOffsetX + RightTolerance) and (P.X > SplitPoint + Tree.EffectiveOffsetX - LeftTolerance)
    else
      Result := (P.X > VisibleFixedWidth) and (P.X < SplitPoint + RightTolerance) and (P.X > SplitPoint - LeftTolerance);
  end;

//--------------- end local function ----------------------------------------

var
  I             : Integer;
  LeftTolerance : TDimension; //The area left of the column divider which allows column resizing
begin
  Result := False;

  if FColumns.Count > 0 then
  begin
    FColumns.TrackIndex := NoColumn;
    VisibleFixedWidth := FColumns.GetVisibleFixedWidth;
    LeftTolerance := Round(SplitterHitTolerance * 0.6);
    if Tree.UseRightToLeftAlignment then
    begin
      SplitPoint := - Tree.EffectiveOffsetX;
      if FColumns.TotalWidth < Tree.ClientWidth then
        Inc(SplitPoint, Tree.ClientWidth - FColumns.TotalWidth);

      for I := 0 to FColumns.Count - 1 do
        with TVirtualTreeColumnsCracker(FColumns), Items[PositionToIndex[I]] do
          if coVisible in Options then
          begin
            if IsNearBy(coFixed in Options, LeftTolerance, SplitterHitTolerance - LeftTolerance) then
            begin
              if CanSplitterResize(P, PositionToIndex[I]) then
              begin
                Result := True;
                TrackIndex := PositionToIndex[I];

                //Keep the right border of this column. This and the current mouse position
                //directly determine the current column width.
                FTrackPoint.X := SplitPoint + IfThen(coFixed in Options, Tree.EffectiveOffsetX) + Width;
                FTrackPoint.Y := P.Y;
                Break;
              end;
            end;
            Inc(SplitPoint, Width);
          end;
    end
    else
    begin
      SplitPoint := - Tree.EffectiveOffsetX + FColumns.TotalWidth;

      for I := FColumns.Count - 1 downto 0 do
        with TVirtualTreeColumnsCracker(FColumns), Items[PositionToIndex[I]] do
          if coVisible in Options then
          begin
            if IsNearBy(coFixed in Options, SplitterHitTolerance - LeftTolerance, LeftTolerance) then
            begin
              if CanSplitterResize(P, PositionToIndex[I]) then
              begin
                Result := True;
                TrackIndex := PositionToIndex[I];

                //Keep the left border of this column. This and the current mouse position
                //directly determine the current column width.
                FTrackPoint.X := SplitPoint + IfThen(coFixed in Options, Tree.EffectiveOffsetX) - Width;
                FTrackPoint.Y := P.Y;
                Break;
              end;
            end;
            Dec(SplitPoint, Width);
          end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoAfterAutoFitColumn(Column : TColumnIndex);

begin
  if Assigned(Tree.OnAfterAutoFitColumn) then
    Tree.OnAfterAutoFitColumn(Self, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoAfterColumnWidthTracking(Column : TColumnIndex);

//Tell the application that a column width tracking operation has been finished.

begin
  if Assigned(Tree.OnAfterColumnWidthTracking) then
    Tree.OnAfterColumnWidthTracking(Self, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoAfterHeightTracking;

//Tell the application that a height tracking operation has been finished.

begin
  if Assigned(Tree.OnAfterHeaderHeightTracking) then
    Tree.OnAfterHeaderHeightTracking(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoBeforeAutoFitColumn(Column : TColumnIndex; SmartAutoFitType : TSmartAutoFitType) : Boolean;

//Query the application if we may autofit a column.

begin
  Result := True;
  if Assigned(Tree.OnBeforeAutoFitColumn) then
    Tree.OnBeforeAutoFitColumn(Self, Column, SmartAutoFitType, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoBeforeColumnWidthTracking(Column : TColumnIndex; Shift : TShiftState);

//Tell the a application that a column width tracking operation may begin.

begin
  if Assigned(Tree.OnBeforeColumnWidthTracking) then
    Tree.OnBeforeColumnWidthTracking(Self, Column, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoBeforeHeightTracking(Shift : TShiftState);

//Tell the application that a height tracking operation may begin.

begin
  if Assigned(Tree.OnBeforeHeaderHeightTracking) then
    Tree.OnBeforeHeaderHeightTracking(Self, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoCanSplitterResize(P : TPoint; var Allowed : Boolean);
begin
  if Assigned(Tree.OnCanSplitterResizeHeader) then
    Tree.OnCanSplitterResizeHeader(Self, P, Allowed);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoColumnWidthDblClickResize(Column : TColumnIndex; P : TPoint; Shift : TShiftState) : Boolean;

//Queries the application whether a double click on the column splitter should resize the column.

begin
  Result := True;
  if Assigned(Tree.OnColumnWidthDblClickResize) then
    Tree.OnColumnWidthDblClickResize(Self, Column, Shift, P, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoColumnWidthTracking(Column : TColumnIndex; Shift : TShiftState; var TrackPoint : TPoint; P : TPoint) : Boolean;

begin
  Result := True;
  if Assigned(Tree.OnColumnWidthTracking) then
    Tree.OnColumnWidthTracking(Self, Column, Shift, TrackPoint, P, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoGetPopupMenu(Column : TColumnIndex; Position : TPoint) : TPopupMenu;

//Queries the application whether there is a column specific header popup menu.

var
  AskParent : Boolean;

begin
  Result := PopupMenu;
  if Assigned(Tree.OnGetPopupMenu) then
    Tree.OnGetPopupMenu(TBaseVirtualTree(FOwner), nil, Column, Position, AskParent, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoHeightTracking(var P : TPoint; Shift : TShiftState) : Boolean;

begin
  Result := True;
  if Assigned(Tree.OnHeaderHeightTracking) then
    Tree.OnHeaderHeightTracking(Self, P, Shift, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoHeightDblClickResize(var P : TPoint; Shift : TShiftState) : Boolean;

begin
  Result := True;
  if Assigned(Tree.OnHeaderHeightDblClickResize) then
    Tree.OnHeaderHeightDblClickResize(Self, P, Shift, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoSetSortColumn(Value : TColumnIndex; pSortDirection : TSortDirection);

begin
  if Value < NoColumn then
    Value := NoColumn;
  if Value > Columns.Count - 1 then
    Value := Columns.Count - 1;
  if FSortColumn <> Value then
  begin
    if FSortColumn > NoColumn then
      Invalidate(Columns[FSortColumn]);
    FSortColumn := Value;
    FSortDirection := pSortDirection;
    if FSortColumn > NoColumn then
      Invalidate(Columns[FSortColumn]);
    if ((toAutoSort in Tree.TreeOptions.AutoOptions) or (hoHeaderClickAutoSort in Options)) and (Tree.UpdateCount = 0) then
      Tree.SortTree(FSortColumn, FSortDirection, True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DragTo(P : TPoint);

//Moves the drag image to a new position, which is determined from the passed point P and the previous
//mouse position.

var
  I, NewTarget : Integer;
  //optimized drag image move support
  ClientP      : TPoint;
  Left, Right  : TDimension;
  NeedRepaint  : Boolean; //True if the screen needs an update (changed drop target or drop side)

begin
  //Determine new drop target and which side of it is prefered.
  ClientP := Tree.ScreenToClient(P);
  //Make coordinates relative to (0, 0) of the non-client area.
  Inc(ClientP.Y, FHeight);
  NewTarget := FColumns.ColumnFromPosition(ClientP);
  NeedRepaint := (NewTarget <> InvalidColumn) and (NewTarget <> FColumns.DropTarget);
  if NewTarget >= 0 then
  begin
    FColumns.GetColumnBounds(NewTarget, Left, Right);
    if (ClientP.X < Divide((Left + Right), 2)) <> FColumns.DropBefore then
    begin
      NeedRepaint := True;
      FColumns.DropBefore := not FColumns.DropBefore;
    end;
  end;

  if NeedRepaint then
  begin
    //Invalidate columns which need a repaint.
    if FColumns.DropTarget > NoColumn then
    begin
      I := FColumns.DropTarget;
      FColumns.DropTarget := NoColumn;
      Invalidate(FColumns.Items[I]);
    end;
    if (NewTarget > NoColumn) and (NewTarget <> FColumns.DropTarget) then
    begin
      Invalidate(FColumns.Items[NewTarget]);
      FColumns.DropTarget := NewTarget;
    end;
  end;

  //Fix for various problems mentioned in issue 248.
  if NeedRepaint then
  begin
    TBaseVirtualTreeCracker(FOwner).UpdateWindow();

    //The new routine recaptures the backup image after the updatewindow
    //Note: We could have called this unconditionally but when called
    //over the tree, doesn't capture the background image. Since our
    //problems are in painting of the header, we call it only when the
    //drag image is over the header.
    if
    //determine the case when the drag image is or was on the header area
      (InHeader(FOwner.ScreenToClient(FDragImage.LastPosition)) or InHeader(FOwner.ScreenToClient(FDragImage.ImagePosition))) then
    begin
      GDIFlush;
      TBaseVirtualTreeCracker(FOwner).UpdateWindowAndDragImage(TBaseVirtualTree(FOwner), TBaseVirtualTreeCracker(FOwner).HeaderRect, True, True);
    end;
    //since we took care of UpdateWindow above, there is no need to do an
    //update window again by sending NeedRepaint. So switch off the second parameter.
    NeedRepaint := False;
  end;

  FDragImage.DragTo(P, NeedRepaint);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.FixedAreaConstraintsChanged(Sender : TObject);

//This method gets called when FFixedAreaConstraints is changed.

begin
  if Tree.HandleAllocated then
    RescaleHeader
  else
    Include(FStates, hsNeedScaling);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetColumnsClass : TVirtualTreeColumnsClass;

//Returns the class to be used for the actual column implementation. descendants may optionally override this and
//return their own class.

begin
  Result := TVirtualTreeColumns;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetOwner : TPersistent;

begin
  Result := FOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetRestoreSelectionColumnIndex : Integer;
begin
  if FRestoreSelectionColumnIndex >= 0 then
    Result := FRestoreSelectionColumnIndex
  else
    Result := MainColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetShiftState : TShiftState;

begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.HandleHeaderMouseMove(var Message : TWMMouseMove) : Boolean;

var
  P             : TPoint;
  NextColumn, I : TColumnIndex;
  NewWidth      : TDimension;
  iOffsetX      : TDimension;

begin
  Result := False;
  with Message do
  begin
    P := Point(XPos, YPos);
    if hsColumnWidthTrackPending in FStates then
    begin
      Tree.StopTimer(HeaderTimer);
      FStates := FStates - [hsColumnWidthTrackPending] + [hsColumnWidthTracking];
      HandleHeaderMouseMove := True;
      Result := 0;
    end
    else if hsHeightTrackPending in FStates then
    begin
      Tree.StopTimer(HeaderTimer);
      FStates := FStates - [hsHeightTrackPending] + [hsHeightTracking];
      HandleHeaderMouseMove := True;
      Result := 0;
    end
    else if hsColumnWidthTracking in FStates then
    begin
      if DoColumnWidthTracking(FColumns.TrackIndex, GetShiftState, FTrackPoint, P) then
      begin
        if Tree.UseRightToLeftAlignment then
        begin
          NewWidth := FTrackPoint.X - XPos;
          NextColumn := FColumns.GetPreviousVisibleColumn(FColumns.TrackIndex);
        end
        else
        begin
          NewWidth := XPos - FTrackPoint.X;
          NextColumn := FColumns.GetNextVisibleColumn(FColumns.TrackIndex);
        end;

        iOffsetX := Tree.EffectiveOffsetX;

        // The autosized column cannot be resized using the mouse normally. Instead we resize the next
        // visible column, so it look as we directly resize the autosized column.
        if (hoAutoResize in FOptions) and (FColumns.TrackIndex = FAutoSizeIndex) and
           (NextColumn > NoColumn) and (coResizable in FColumns[NextColumn].Options) and
           (FColumns[FColumns.TrackIndex].MinWidth < NewWidth) and
           (FColumns[FColumns.TrackIndex].MaxWidth > NewWidth) then
          FColumns[NextColumn].Width := FColumns[NextColumn].Width - NewWidth
                                        + FColumns[FColumns.TrackIndex].Width
        else
          FColumns[FColumns.TrackIndex].Width := NewWidth; // 1 EListError seen here (List index out of bounds (-1)) since 10/2013

         if (iOffsetX > 0) and (iOffsetX <> Tree.EffectiveOffsetX) then
           FTrackPoint.X := FTrackPoint.X + iOffsetX - Tree.EffectiveOffsetX;
      end;
      HandleHeaderMouseMove := True;
      Result := 0;
    end
    else if hsHeightTracking in FStates then
    begin
      if DoHeightTracking(P, GetShiftState) then
        SetHeight(FHeight + P.Y);
      HandleHeaderMouseMove := True;
      Result := 0;
    end
    else
    begin
      if hsDragPending in FStates then
      begin
        P := Tree.ClientToScreen(P);
        //start actual dragging if allowed
        if (hoDrag in FOptions) and Tree.DoHeaderDragging(TVirtualTreeColumnsCracker(FColumns).DownIndex) then
        begin
          if ((Abs(FDragStart.X - P.X) > Mouse.DragThreshold) or (Abs(FDragStart.Y - P.Y) > Mouse.DragThreshold)) then
          begin
            Tree.StopTimer(HeaderTimer);
            with TVirtualTreeColumnsCracker(FColumns) do
            begin
              I := DownIndex;
              DownIndex := NoColumn;
              HoverIndex := NoColumn;
              if I > NoColumn then
                Invalidate(FColumns[I]);
            end;
            PrepareDrag(P, FDragStart);
            FStates := FStates - [hsDragPending] + [hsDragging];
            HandleHeaderMouseMove := True;
            Result := 0;
          end;
        end;
      end
      else if hsDragging in FStates then
      begin
        DragTo(Tree.ClientToScreen(Point(XPos, YPos)));
        HandleHeaderMouseMove := True;
        Result := 0;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.HandleMessage(var Message : TMessage) : Boolean;

//The header gets here the opportunity to handle certain messages before they reach the tree. This is important
//because the tree needs to handle various non-client area messages for the header as well as some dragging/tracking
//events.
//By returning True the message will not be handled further, otherwise the message is then dispatched
//to the proper message handlers.

var
  P                                          : TPoint;
  R                                          : TRect;
  I                                          : TColumnIndex;
  OldPosition                                : Integer;
  HitIndex                                   : TColumnIndex;
  NewCursor                                  : TVTCursor;
  Button                                     : TMouseButton;
  IsInHeader, IsHSplitterHit, IsVSplitterHit : Boolean;

  //--------------- local function --------------------------------------------

  function HSplitterHit : Boolean;
  begin
    Result := (hoColumnResize in FOptions) and DetermineSplitterIndex(P);
    if Result and not InHeader(P) then
    begin
      // Code commented due to issue #1067. What was the orginal inention of this code? It does not make much sense unless you allow column resize outside the header.
      //NextCol := FColumns.GetNextVisibleColumn(FColumns.TrackIndex);
      //if not (coFixed in FColumns[FColumns.TrackIndex].Options) or (NextCol <= NoColumn) or
      //   (coFixed in FColumns[NextCol].Options) or (P.Y > Tree.RangeY) then
        Result := False;
    end;
  end;

//--------------- end local function ----------------------------------------

begin
  Result := False;
  case Message.Msg of
    WM_SIZE :
      begin
        if not (tsWindowCreating in TBaseVirtualTreeCracker(FOwner).TreeStates) then
          if (hoAutoResize in FOptions) and not (hsAutoSizing in FStates) then
          begin
            TVirtualTreeColumnsCracker(FColumns).AdjustAutoSize(InvalidColumn);
            Invalidate(nil);
          end
          else if not (hsScaling in FStates) then
          begin
            RescaleHeader;
            Invalidate(nil);
          end;
      end;
    CM_PARENTFONTCHANGED :
      if FParentFont then
        FFont.Assign(TBaseVirtualTreeCracker(FOwner).Font);
    CM_BIDIMODECHANGED :
      for I := 0 to FColumns.Count - 1 do
        if coParentBiDiMode in FColumns[I].Options then
          FColumns[I].ParentBiDiModeChanged;
    WM_NCMBUTTONDOWN :
      begin
        with TWMNCMButtonDown(Message) do
          P := Tree.ScreenToClient(Point(XCursor, YCursor));
        if InHeader(P) then
          TBaseVirtualTreeCracker(FOwner).DoHeaderMouseDown(mbMiddle, GetShiftState, P.X, P.Y + FHeight);
      end;
    WM_NCMBUTTONUP :
      begin
        with TWMNCMButtonUp(Message) do
          P := FOwner.ScreenToClient(Point(XCursor, YCursor));
        if InHeader(P) then
        begin
          with TVirtualTreeColumnsCracker(FColumns) do
          begin
            HandleClick(P, mbMiddle, True, False);
            TBaseVirtualTreeCracker(FOwner).DoHeaderMouseUp(TmouseButton.mbMiddle, GetShiftState, P.X, P.Y + Self.FHeight);
            DownIndex := NoColumn;
            CheckBoxHit := False;
          end;
        end;
        fWasDoubleClick := False;
      end;
    WM_LBUTTONDBLCLK, WM_NCLBUTTONDBLCLK, WM_NCMBUTTONDBLCLK, WM_NCRBUTTONDBLCLK :
      begin
        fWasDoubleClick := True;
        if Message.Msg <> WM_LBUTTONDBLCLK then
          with TWMNCLButtonDblClk(Message) do
            P := FOwner.ScreenToClient(Point(XCursor, YCursor))
        else
          with TWMLButtonDblClk(Message) do
            P := Point(XPos, YPos);

        if (hoHeightDblClickResize in FOptions) and InHeaderSplitterArea(P) and (FDefaultHeight > 0) then
        begin
          if DoHeightDblClickResize(P, GetShiftState) and (FDefaultHeight > 0) then
            SetHeight(FMinHeight);
          Result := True;
        end
        else if HSplitterHit and ((Message.Msg = WM_NCLBUTTONDBLCLK) or (Message.Msg = WM_LBUTTONDBLCLK)) and (hoDblClickResize in FOptions) and (FColumns.TrackIndex > NoColumn)
        then
        begin
          //If the click was on a splitter then resize column to smallest width.
          if DoColumnWidthDblClickResize(FColumns.TrackIndex, P, GetShiftState) then
            AutoFitColumns(True, smaUseColumnOption, FColumns[FColumns.TrackIndex].Position, FColumns[FColumns.TrackIndex].Position);
          Message.Result := 0;
          Result := True;
        end
        else if InHeader(P) and (Message.Msg <> WM_LBUTTONDBLCLK) then
        begin
          case Message.Msg of
            WM_NCMBUTTONDBLCLK :
              Button := TMouseButton.mbMiddle;
            WM_NCRBUTTONDBLCLK :
              Button := TMouseButton.mbRight;
          else
              //WM_NCLBUTTONDBLCLK
            Button := TMouseButton.mbLeft;
          end;
          if Button = TMouseButton.mbLeft then
            TVirtualTreeColumnsCracker(FColumns).AdjustDownColumn(P);
          TVirtualTreeColumnsCracker(FColumns).HandleClick(P, Button, True, True);
        end;
      end;
    //The "hot" area of the headers horizontal splitter is partly within the client area of the the tree, so we need
    //to handle WM_LBUTTONDOWN here, too.
    WM_LBUTTONDOWN, WM_NCLBUTTONDOWN :
      begin

        Application.CancelHint;

        if not (csDesigning in Tree.ComponentState) then
        begin
          with Tree do
          begin
            //make sure no auto scrolling is active...
            StopTimer(ScrollTimer);
            DoStateChange([], [tsScrollPending, tsScrolling]);
            //... pending editing is cancelled (actual editing remains active)
            StopTimer(EditTimer);
            DoStateChange([], [tsEditPending]);
          end;
        end;

        if Message.Msg = WM_LBUTTONDOWN then
          //Coordinates are already client area based.
          with TWMLButtonDown(Message) do
          begin
            P := Point(XPos, YPos);
            //#909
            FDragStart := Tree.ClientToScreen(P);
          end
        else
          with TWMNCLButtonDown(Message) do
          begin
            //want the drag start point in screen coordinates
            FDragStart := Point(XCursor, YCursor);
            P := Tree.ScreenToClient(FDragStart);
          end;

        IsInHeader := InHeader(P);
        //in design-time header columns are always resizable
        if (csDesigning in Tree.ComponentState) then
          IsVSplitterHit := InHeaderSplitterArea(P)
        else
          IsVSplitterHit := InHeaderSplitterArea(P) and CanSplitterResize(P);
        IsHSplitterHit := HSplitterHit;

        if IsVSplitterHit or IsHSplitterHit then
        begin
          FTrackStart := P;
          TVirtualTreeColumnsCracker(FColumns).HoverIndex := NoColumn;
          if IsVSplitterHit then
          begin
            if not (csDesigning in Tree.ComponentState) then
              DoBeforeHeightTracking(GetShiftState);
            Include(FStates, hsHeightTrackPending);
          end
          else
          begin
            if not (csDesigning in Tree.ComponentState) then
              DoBeforeColumnWidthTracking(FColumns.TrackIndex, GetShiftState);
            Include(FStates, hsColumnWidthTrackPending);
          end;

          SetCapture(Tree.Handle);
          Result := True;
          Message.Result := 0;
        end
        else if IsInHeader then
        begin
          HitIndex := TVirtualTreeColumnsCracker(FColumns).AdjustDownColumn(P);
          //in design-time header columns are always draggable
          if ((csDesigning in Tree.ComponentState) and (HitIndex > NoColumn)) or ((hoDrag in FOptions) and (HitIndex > NoColumn) and (coDraggable in FColumns[HitIndex].Options))
          then
          begin
            //Show potential drag operation.
            //Disabled columns do not start a drag operation because they can't be clicked.
            Include(FStates, hsDragPending);
            SetCapture(Tree.Handle);
            Result := True;
            Message.Result := 0;
          end;
        end;

        //This is a good opportunity to notify the application.
        if not (csDesigning in Tree.ComponentState) and IsInHeader then
          TBaseVirtualTreeCracker(FOwner).DoHeaderMouseDown(TMouseButton.mbLeft, GetShiftState, P.X, P.Y + FHeight);
      end;
    WM_NCRBUTTONDOWN :
      begin
        with TWMNCRButtonDown(Message) do
          P := FOwner.ScreenToClient(Point(XCursor, YCursor));
        if InHeader(P) then
          TBaseVirtualTreeCracker(FOwner).DoHeaderMouseDown(TMouseButton.mbRight, GetShiftState, P.X, P.Y + FHeight);
      end;
    WM_NCRBUTTONUP :
      if not (csDesigning in FOwner.ComponentState) then
        with TWMNCRButtonUp(Message) do
        begin
          Application.CancelHint;
          P := FOwner.ScreenToClient(Point(XCursor, YCursor));
          if InHeader(P) then
          begin
            HandleMessage := TVirtualTreeColumnsCracker(FColumns).HandleClick(P, TMouseButton.mbRight, True, False);
            TBaseVirtualTreeCracker(FOwner).DoHeaderMouseUp(TMouseButton.mbRight, GetShiftState, P.X, P.Y + FHeight);
          end;
          fWasDoubleClick := False;
        end;
    //When the tree window has an active mouse capture then we only get "client-area" messages.
    WM_LBUTTONUP, WM_NCLBUTTONUP :
      begin
        Application.CancelHint;

        if FStates <> [] then
        begin
          ReleaseCapture;
          if hsDragging in FStates then
          begin
            //successfull dragging moves columns
            with TWMLButtonUp(Message) do
              P := Tree.ClientToScreen(Point(XPos, YPos));
            GetWindowRect(Tree.Handle, R);
            with FColumns do
            begin
              FDragImage.EndDrag;

              //Problem fixed:
              //Column Header does not paint correctly after a drop in certain conditions
              // ** The conditions are, drag is across header, mouse is not moved after
              //the drop and the graphics hardware is slow in certain operations (encountered
              //on Windows 10).
              //Fix for the problem on certain systems where the dropped column header
              //does not appear in the new position if the mouse is not moved after
              //the drop. The reason is that the restore backup image operation (BitBlt)
              //in the above EndDrag is slower than the header repaint in the code below
              //and overlaps the new changed header with the older image.
              //This happens because BitBlt seems to operate in its own thread in the
              //graphics hardware and finishes later than the following code.
              //
              //To solve this problem, we introduce a small delay here so that the
              //changed header in the following code is correctly repainted after
              //the delayed BitBlt above has finished operation to restore the old
              //backup image.
              sleep(50);

              if (DropTarget > - 1) and (DropTarget <> DragIndex) and PtInRect(R, P) then
              begin
                OldPosition := FColumns[DragIndex].Position;
                if FColumns.DropBefore then
                begin
                  if FColumns[DragIndex].Position < FColumns[DropTarget].Position then
                    FColumns[DragIndex].Position := Max(0, FColumns[DropTarget].Position - 1)
                  else
                    FColumns[DragIndex].Position := FColumns[DropTarget].Position;
                end
                else
                begin
                  if FColumns[DragIndex].Position < FColumns[DropTarget].Position then
                    FColumns[DragIndex].Position := FColumns[DropTarget].Position
                  else
                    FColumns[DragIndex].Position := FColumns[DropTarget].Position + 1;
                end;
                Tree.DoHeaderDragged(DragIndex, OldPosition);
              end
              else
                Tree.DoHeaderDraggedOut(DragIndex, P);
              DropTarget := NoColumn;
            end;
            Invalidate(nil);
          end;
          Result := True;
          Message.Result := 0;
          fWasDoubleClick := False;
        end;

        case Message.Msg of
          WM_LBUTTONUP :
            with TWMLButtonUp(Message) do
            begin
              with TVirtualTreeColumnsCracker(FColumns) do
              begin
                if DownIndex > NoColumn then
                  HandleClick(Point(XPos, YPos), TMouseButton.mbLeft, False, False);
              end;
              if FStates <> [] then
                TBaseVirtualTreeCracker(FOwner).DoHeaderMouseUp(TMouseButton.mbLeft, KeysToShiftState(Keys), XPos, YPos);
              fWasDoubleClick := False;
            end;
          WM_NCLBUTTONUP :
            begin
              with TWMNCLButtonUp(Message) do
                P := FOwner.ScreenToClient(Point(XCursor, YCursor));
              if not fWasDoubleClick then
                TVirtualTreeColumnsCracker(FColumns).HandleClick(P, TMouseButton.mbLeft, True, False);
              TBaseVirtualTreeCracker(FOwner).DoHeaderMouseUp(TMouseButton.mbLeft, GetShiftState, P.X, P.Y + FHeight);
              Result := True;
              fWasDoubleClick := False;
            end;
        end;

        if FColumns.TrackIndex > NoColumn then
        begin
          if hsColumnWidthTracking in FStates then
            DoAfterColumnWidthTracking(FColumns.TrackIndex);
          Invalidate(Columns[FColumns.TrackIndex]);
          FColumns.TrackIndex := NoColumn;
        end;
        with TVirtualTreeColumnsCracker(FColumns) do
        begin
          if DownIndex > NoColumn then
          begin
            Invalidate(FColumns[DownIndex]);
            DownIndex := NoColumn;
          end;
        end;
        if hsHeightTracking in FStates then
          DoAfterHeightTracking;

        FStates := FStates - [hsDragging, hsDragPending, hsColumnWidthTracking, hsColumnWidthTrackPending, hsHeightTracking, hsHeightTrackPending];
      end; //WM_NCLBUTTONUP
    //hovering, mouse leave detection
    WM_NCMOUSEMOVE :
      with TWMNCMouseMove(Message), TVirtualTreeColumnsCracker(FColumns) do
      begin
        P := Tree.ScreenToClient(Point(XCursor, YCursor));
        Tree.DoHeaderMouseMove(GetShiftState, P.X, P.Y + FHeight);
        if InHeader(P) and ((AdjustHoverColumn(P)) or ((DownIndex >= 0) and (HoverIndex <> DownIndex))) then
        begin
          //We need a mouse leave detection from here for the non client area.
          //TODO: The best solution available would be the TrackMouseEvent API.
          //With the drop of the support of Win95 totally and WinNT4 we should replace the timer.
          Tree.StopTimer(HeaderTimer);
          SetTimer(Tree.Handle, HeaderTimer, 50, nil);
          //use Delphi's internal hint handling for header hints too
          if hoShowHint in FOptions then
          begin
            //client coordinates!
            XCursor := P.X;
            YCursor := P.Y + FHeight;
            Application.HintMouseMessage(FOwner, Message);
          end;
        end;
      end;
    WM_TIMER :
      if TWMTimer(Message).TimerID = HeaderTimer then
      begin
        //determine current mouse position to check if it left the window
        GetCursorPos(P);
        P := Tree.ScreenToClient(P);
        with TVirtualTreeColumnsCracker(FColumns) do
        begin
          if not InHeader(P) or ((DownIndex > NoColumn) and (HoverIndex <> DownIndex)) then
          begin
            Tree.StopTimer(HeaderTimer);
            HoverIndex := NoColumn;
            ClickIndex := NoColumn;
            DownIndex := NoColumn;
            CheckBoxHit := False;
            Result := True;
            Message.Result := 0;
            Invalidate(nil);
          end;
        end;
      end;
    WM_MOUSEMOVE : //mouse capture and general message redirection
      Result := HandleHeaderMouseMove(TWMMouseMove(Message));
    WM_SETCURSOR :
      //Feature: design-time header
      if (FStates = []) then
      begin
        //Retrieve last cursor position (GetMessagePos does not work here, I don't know why).
        GetCursorPos(P);

        //Is the mouse in the header rectangle and near the splitters?
        P := Tree.ScreenToClient(P);
        IsHSplitterHit := HSplitterHit;
        //in design-time header columns are always resizable
        if (csDesigning in Tree.ComponentState) then
          IsVSplitterHit := InHeaderSplitterArea(P)
        else
          IsVSplitterHit := InHeaderSplitterArea(P) and CanSplitterResize(P);

        if IsVSplitterHit or IsHSplitterHit then
        begin
          NewCursor := Screen.Cursors[Tree.Cursor];
          if IsVSplitterHit and ((hoHeightResize in FOptions) or (csDesigning in Tree.ComponentState)) then
            NewCursor := Screen.Cursors[crVSplit]
          else if IsHSplitterHit then
            NewCursor := Screen.Cursors[crHSplit];

          if not (csDesigning in Tree.ComponentState) then
            Tree.DoGetHeaderCursor(NewCursor);
          Result := NewCursor <> Screen.Cursors[crDefault];
          if Result then
          begin
            WinApi.Windows.SetCursor(NewCursor);
            Message.Result := 1;
          end;
        end;
      end
      else
      begin
        Message.Result := 1;
        Result := True;
      end;
    WM_KEYDOWN, WM_KILLFOCUS :
      if (Message.Msg = WM_KILLFOCUS) or (TWMKeyDown(Message).CharCode = VK_ESCAPE) then
      begin
        if hsDragging in FStates then
        begin
          ReleaseCapture;
          FDragImage.EndDrag;
          Exclude(FStates, hsDragging);
          FColumns.DropTarget := NoColumn;
          Invalidate(nil);
          Result := True;
          Message.Result := 0;
        end
        else
        begin
          if [hsColumnWidthTracking, hsHeightTracking] * FStates <> [] then
          begin
            ReleaseCapture;
            if hsColumnWidthTracking in FStates then
              DoAfterColumnWidthTracking(FColumns.TrackIndex);
            if hsHeightTracking in FStates then
              DoAfterHeightTracking;
            Result := True;
            Message.Result := 0;
          end;

          FStates := FStates - [hsColumnWidthTracking, hsColumnWidthTrackPending, hsHeightTracking, hsHeightTrackPending];
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.ImageListChange(Sender : TObject);

begin
  if not (csDestroying in Tree.ComponentState) then
    Invalidate(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.PrepareDrag(P, Start : TPoint);

//Initializes dragging of the header, P is the current mouse postion and Start the initial mouse position.

var
  Image      : TBitmap;
  ImagePos   : TPoint;
  DragColumn : TVirtualTreeColumn;
  RTLOffset  : TDimension;

begin
  //Determine initial position of drag image (screen coordinates).
  FColumns.DropTarget := NoColumn;
  Start := Tree.ScreenToClient(Start);
  Inc(Start.Y, FHeight);
  FColumns.DragIndex := FColumns.ColumnFromPosition(Start);
  DragColumn := FColumns[FColumns.DragIndex];

  Image := TBitmap.Create;
  with Image do
    try
      PixelFormat := pf32Bit;
      SetSize(DragColumn.Width, FHeight);

      //Erase the entire image with the color key value, for the case not everything
      //in the image is covered by the header image.
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect(0, 0, Width, Height));

      if Tree.UseRightToLeftAlignment then
        RTLOffset := Tree.ComputeRTLOffset
      else
        RTLOffset := 0;
      with DragColumn do
        FColumns.PaintHeader(Canvas, Rect(Left, 0, Left + Width, Height), Point( - RTLOffset, 0), RTLOffset);

      if Tree.UseRightToLeftAlignment then
        ImagePos := Tree.ClientToScreen(Point(DragColumn.Left + Tree.ComputeRTLOffset(True), 0))
      else
        ImagePos := Tree.ClientToScreen(Point(DragColumn.Left, 0));
      //Column rectangles are given in local window coordinates not client coordinates.
      Dec(ImagePos.Y, FHeight);

      if hoRestrictDrag in FOptions then
        FDragImage.MoveRestriction := dmrHorizontalOnly
      else
        FDragImage.MoveRestriction := dmrNone;
      FDragImage.PrepareDrag(Image, ImagePos, P, nil);
      FDragImage.ShowDragImage;
    finally
      Image.Free;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.ReadColumns(Reader : TReader);

begin
  Include(FStates, hsLoading);
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
  Exclude(FStates, hsLoading);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.RecalculateHeader;

//Initiate a recalculation of the non-client area of the owner tree.

begin
  if Tree.HandleAllocated then
  begin
    Tree.UpdateHeaderRect;
    SetWindowPos(Tree.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.RescaleHeader;

//Rescale the fixed elements (fixed columns, header itself) to FixedAreaConstraints.

var
  FixedWidth, MaxFixedWidth, MinFixedWidth : TDimension;

  //--------------- local function --------------------------------------------

  procedure ComputeConstraints;

  var
    I : TColumnIndex;

  begin
    with FColumns do
    begin
      I := GetFirstVisibleColumn;
      while I > NoColumn do
      begin
        if (coFixed in FColumns[I].Options) and (FColumns[I].Width < FColumns[I].MinWidth) then
          TVirtualTreeColumnCracker(FColumns[I]).InternalSetWidth(FColumns[I].MinWidth); //SetWidth has side effects and this bypasses them
        I := GetNextVisibleColumn(I);
      end;
      FixedWidth := GetVisibleFixedWidth;
    end;

    with FFixedAreaConstraints do
    begin
      MinFixedWidth := Divide(Tree.ClientWidth * FMinWidthPercent, 100);
      MaxFixedWidth := Divide(Tree.ClientWidth * FMaxWidthPercent, 100);
    end;
  end;

//----------- end local function --------------------------------------------

begin
  if ([csLoading, csReading, csWriting, csDestroying] * Tree.ComponentState = []) and not (hsLoading in FStates) and Tree.HandleAllocated then
  begin
    Include(FStates, hsScaling);

    SetHeight(FHeight);
    RecalculateHeader;

    with FFixedAreaConstraints do
      if (FMaxWidthPercent > 0) or (FMinWidthPercent > 0) or (FMinHeightPercent > 0) or (FMaxHeightPercent > 0) then
      begin
        ComputeConstraints;

        with FColumns do
          if (FMaxWidthPercent > 0) and (FixedWidth > MaxFixedWidth) then
            ResizeColumns(MaxFixedWidth - FixedWidth, 0, Count - 1, [coVisible, coFixed])
          else if (FMinWidthPercent > 0) and (FixedWidth < MinFixedWidth) then
            ResizeColumns(MinFixedWidth - FixedWidth, 0, Count - 1, [coVisible, coFixed]);

        TVirtualTreeColumnsCracker(FColumns).UpdatePositions;
      end;

    Exclude(FStates, hsScaling);
    Exclude(FStates, hsNeedScaling);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.UpdateMainColumn();

//Called once the load process of the owner tree is done.

begin
  if FMainColumn < 0 then
    MainColumn := 0;
  if FMainColumn > FColumns.Count - 1 then
    MainColumn := FColumns.Count - 1;
  if (FMainColumn >= 0) and not (coVisible in Self.Columns[FMainColumn].Options) then
  begin
    //Issue #946: Choose new MainColumn if current one ist not visible
    MainColumn := Self.Columns.GetFirstVisibleColumn();
  end
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.UpdateSpringColumns;

var
  I              : TColumnIndex;
  SpringCount    : Integer;
  Sign           : Integer;
  ChangeBy       : Single;
  Difference     : Single;
  NewAccumulator : Single;

begin
  with Tree do
    ChangeBy := HeaderRect.Right - HeaderRect.Left - FLastWidth;
  if (hoAutoSpring in FOptions) and (FLastWidth <> 0) and (ChangeBy <> 0) then
  begin
    //Stay positive if downsizing the control.
    if ChangeBy < 0 then
      Sign := - 1
    else
      Sign := 1;
    ChangeBy := Abs(ChangeBy);
    //Count how many columns have spring enabled.
    SpringCount := 0;
    for I := 0 to FColumns.Count - 1 do
      if [coVisible, coAutoSpring] * FColumns[I].Options = [coVisible, coAutoSpring] then
        System.Inc(SpringCount);
    if SpringCount > 0 then
    begin
      //Calculate the size to add/sub to each columns.
      Difference := ChangeBy / SpringCount;
      //Adjust the column's size accumulators and resize if the result is >= 1.
      for I := 0 to FColumns.Count - 1 do
        if [coVisible, coAutoSpring] * FColumns[I].Options = [coVisible, coAutoSpring] then
        begin
          //Sum up rest changes from previous runs and the amount from this one and store it in the
          //column. If there is at least one pixel difference then do a resize and reset the accumulator.
          NewAccumulator := FColumns[I].SpringRest + Difference;
          //Set new width if at least one pixel size difference is reached.
          if NewAccumulator >= 1 then
            TVirtualTreeColumnCracker(FColumns[I]).SetWidth(FColumns[I].Width + (Trunc(NewAccumulator) * Sign));
          FColumns[I].SpringRest := Frac(NewAccumulator);

          //Keep track of the size count.
          ChangeBy := ChangeBy - Difference;
          //Exit loop if resize count drops below freezing point.
          if ChangeBy < 0 then
            Break;
        end;
    end;
  end;
  with Tree do
    FLastWidth := HeaderRect.Right - HeaderRect.Left;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  //--- HACK WARNING!
  //This type cast is a partial rewrite of the private section of TWriter. The purpose is to have access to
  //the FPropPath member, which is otherwise not accessible. The reason why this access is needed is that
  //with nested components this member contains unneeded property path information. These information prevent
  //successful load of the stored properties later.
  //In System.Classes.pas you can see that FPropPath is reset several times to '' to prevent this case for certain properies.
  //Unfortunately, there is no clean way for us here to do the same.
{$HINTS off}
  TWriterHack = class(TFiler)
  private
    FRootAncestor : TComponent;
    FPropPath     : string;
  end;
{$HINTS on}


procedure TVTHeader.WriteColumns(Writer : TWriter);

//Write out the columns but take care for the case VT is a nested component.

var
  LastPropPath : string;

begin
  //Save last property path for restoration.
  LastPropPath := TWriterHack(Writer).FPropPath;
  try
    //If VT is a nested component then this path contains the name of the parent component at this time
    //(otherwise it is already empty). This path is then combined with the property name under which the tree
    //is defined in the parent component. Unfortunately, the load code in System.Classes.pas does not consider this case
    //is then unable to load this property.
    TWriterHack(Writer).FPropPath := '';
    Writer.WriteCollection(Columns);
  finally
    TWriterHack(Writer).FPropPath := LastPropPath;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.AllowFocus(ColumnIndex : TColumnIndex) : Boolean;
begin
  Result := False;
  if not FColumns.IsValidColumn(ColumnIndex) then
    Exit; //Just in case.

  Result := (coAllowFocus in FColumns[ColumnIndex].Options);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.AreColumnsStored: Boolean;
begin
  // The columns are stored by the owner tree to support Visual Form Inheritance
  // GnutGetText skips non-stored properties, so retur Stored True at runtime
  Result := not (csDesigning in Self.Treeview.ComponentState);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.Assign(Source : TPersistent);

begin
  if Source is TVTHeader then
  begin
    AutoSizeIndex := TVTHeader(Source).AutoSizeIndex;
    Background := TVTHeader(Source).Background;
    Columns := TVTHeader(Source).Columns;
    Font := TVTHeader(Source).Font;
    FixedAreaConstraints.Assign(TVTHeader(Source).FixedAreaConstraints);
    Height := TVTHeader(Source).Height;
    Images := TVTHeader(Source).Images;
    MainColumn := TVTHeader(Source).MainColumn;
    Options := TVTHeader(Source).Options;
    ParentFont := TVTHeader(Source).ParentFont;
    PopupMenu := TVTHeader(Source).PopupMenu;
    SortColumn := TVTHeader(Source).SortColumn;
    SortDirection := TVTHeader(Source).SortDirection;
    Style := TVTHeader(Source).Style;

    RescaleHeader;
  end
  else
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.AutoFitColumns();
begin
  AutoFitColumns(not Tree.IsUpdating);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.AutoFitColumns(Animated : Boolean; SmartAutoFitType : TSmartAutoFitType = smaUseColumnOption; RangeStartCol : Integer = NoColumn;
  RangeEndCol : Integer = NoColumn);

//--------------- local functions -------------------------------------------

  function GetUseSmartColumnWidth(ColumnIndex : TColumnIndex) : Boolean;

  begin
    case SmartAutoFitType of
      smaAllColumns :
        Result := True;
      smaUseColumnOption :
        Result := coSmartResize in FColumns.Items[ColumnIndex].Options;
    else
      Result := False;
    end;
  end;

//----------------------------------------------------------------------------

  procedure DoAutoFitColumn(Column : TColumnIndex);

  begin
    with TVirtualTreeColumnsCracker(FColumns) do
      if ([coResizable, coVisible] * Items[PositionToIndex[Column]].Options = [coResizable, coVisible]) and DoBeforeAutoFitColumn(PositionToIndex[Column], SmartAutoFitType) and
        not Tree.OperationCanceled then
      begin
        if Animated then
          AnimatedResize(PositionToIndex[Column], Tree.GetMaxColumnWidth(PositionToIndex[Column], GetUseSmartColumnWidth(PositionToIndex[Column])))
        else
          FColumns[PositionToIndex[Column]].Width := Tree.GetMaxColumnWidth(PositionToIndex[Column], GetUseSmartColumnWidth(PositionToIndex[Column]));

        DoAfterAutoFitColumn(PositionToIndex[Column]);
      end;
  end;

//--------------- end local functions ----------------------------------------

var
  I                : Integer;
  StartCol, EndCol : Integer;

begin
  StartCol := Max(NoColumn + 1, RangeStartCol);

  if RangeEndCol <= NoColumn then
    EndCol := FColumns.Count - 1
  else
    EndCol := Min(RangeEndCol, FColumns.Count - 1);

  if StartCol > EndCol then
    Exit; //nothing to do

  Tree.StartOperation(okAutoFitColumns);
  FDoingAutoFitColumns := True;
  try
    if Assigned(Tree.OnBeforeAutoFitColumns) then
      Tree.OnBeforeAutoFitColumns(Self, SmartAutoFitType);

    for I := StartCol to EndCol do
      DoAutoFitColumn(I);

    if Assigned(Tree.OnAfterAutoFitColumns) then
      Tree.OnAfterAutoFitColumns(Self);

  finally
    Tree.EndOperation(okAutoFitColumns);
    Tree.Invalidate();
    FDoingAutoFitColumns := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.InHeader(P : TPoint) : Boolean;

//Determines whether the given point (client coordinates!) is within the header rectangle (non-client coordinates).

var
  R, RW : TRect;

begin
  R := Tree.HeaderRect;

  //Current position of the owner in screen coordinates.
  GetWindowRect(Tree.Handle, RW);

  //Convert to client coordinates.
  MapWindowPoints(0, Tree.Handle, RW, 2);

  //Consider the header within this rectangle.
  OffsetRect(R, RW.Left, RW.Top);
  Result := PtInRect(R, P);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.InHeaderSplitterArea(P : TPoint) : Boolean;

//Determines whether the given point (client coordinates!) hits the horizontal splitter area of the header.

var
  R, RW : TRect;

begin
  if (P.Y > 2) or (P.Y < - 2) or not (hoVisible in FOptions) then
    Result := False
  else
  begin
    R := Tree.HeaderRect;
    Inc(R.Bottom, 2);

    //Current position of the owner in screen coordinates.
    GetWindowRect(Tree.Handle, RW);

    //Convert to client coordinates.
    MapWindowPoints(0, Tree.Handle, RW, 2);

    //Consider the header within this rectangle.
    OffsetRect(R, RW.Left, RW.Top);
    Result := PtInRect(R, P);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.InternalSetAutoSizeIndex(const Index : TColumnIndex);
begin
  FAutoSizeIndex := index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.InternalSetMainColumn(const Index : TColumnIndex);
begin
  FMainColumn := index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.InternalSetSortColumn(const Index : TColumnIndex);
begin
  FSortColumn := index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.Invalidate(Column : TVirtualTreeColumn; ExpandToBorder : Boolean = False; UpdateNowFlag : Boolean = False);

//Because the header is in the non-client area of the tree it needs some special handling in order to initiate its
//repainting.
//If ExpandToBorder is True then not only the given column but everything or (depending on hoFullRepaintOnResize) just
//everything to its right (or left, in RTL mode) will be invalidated (useful for resizing). This makes only sense when
//a column is given.

var
  R, RW : TRect;
  Flags : Cardinal;

begin
  if (hoVisible in FOptions) and Tree.HandleAllocated then
    with Tree do
    begin
      if Column = nil then
        R := HeaderRect
      else
      begin
        R := Column.GetRect;
        if not (coFixed in Column.Options) then
          OffsetRect(R, - EffectiveOffsetX, 0);
        if UseRightToLeftAlignment then
          OffsetRect(R, ComputeRTLOffset, 0);
        if ExpandToBorder then
        begin
          if (hoFullRepaintOnResize in Header.Options) then
          begin
            R.Left := HeaderRect.Left;
            R.Right := HeaderRect.Right;
          end
          else
          begin
            if UseRightToLeftAlignment then
              R.Left := HeaderRect.Left
            else
              R.Right := HeaderRect.Right;
          end;
        end;
      end;
      R.Bottom := Tree.ClientHeight; //We want to repaint the entire column to bottom, not just the header

      //Current position of the owner in screen coordinates.
      GetWindowRect(Handle, RW);

      //Consider the header within this rectangle.
      OffsetRect(R, RW.Left, RW.Top);

      //Expressed in client coordinates (because RedrawWindow wants them so, they will actually become negative).
      MapWindowPoints(0, Handle, R, 2);
      Flags := RDW_FRAME or RDW_INVALIDATE or RDW_VALIDATE or RDW_NOINTERNALPAINT or RDW_NOERASE or RDW_NOCHILDREN;
      if UpdateNowFlag then
        Flags := Flags or RDW_UPDATENOW;
      RedrawWindow(@R, 0, Flags);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.LoadFromStream(const Stream : TStream);

//restore the state of the header from the given stream

var
  Dummy, Version : Integer;
  S              : AnsiString;
  OldOptions     : TVTHeaderOptions;

begin
  Include(FStates, hsLoading);
  with Stream do
    try
      //Switch off all options which could influence loading the columns (they will be later set again).
      OldOptions := FOptions;
      FOptions := [];

      //Determine whether the stream contains data without a version number.
      ReadBuffer(Dummy, SizeOf(Dummy));
      if Dummy > - 1 then
      begin
        //Seek back to undo the read operation if this is an old stream format.
        Seek( - SizeOf(Dummy), soFromCurrent);
        Version := - 1;
      end
      else //Read version number if this is a "versionized" format.
        ReadBuffer(Version, SizeOf(Version));
      Columns.LoadFromStream(Stream, Version);

      ReadBuffer(Dummy, SizeOf(Dummy));
      AutoSizeIndex := Dummy;
      ReadBuffer(Dummy, SizeOf(Dummy));
      Background := Dummy;
      ReadBuffer(Dummy, SizeOf(Dummy));
      Height := Dummy;
      ReadBuffer(Dummy, SizeOf(Dummy));
      FOptions := OldOptions;
      Options := TVTHeaderOptions(Dummy);
      //PopupMenu is neither saved nor restored
      ReadBuffer(Dummy, SizeOf(Dummy));
      Style := TVTHeaderStyle(Dummy);
      //TFont has no own save routine so we do it manually
      with Font do
      begin
        ReadBuffer(Dummy, SizeOf(Dummy));
        Color := Dummy;
        ReadBuffer(Dummy, SizeOf(Dummy));
        Height := Dummy;
        ReadBuffer(Dummy, SizeOf(Dummy));
        SetLength(S, Dummy);
        ReadBuffer(PAnsiChar(S)^, Dummy);
        Name := UTF8ToString(S);
        ReadBuffer(Dummy, SizeOf(Dummy));
        Pitch := TFontPitch(Dummy);
        ReadBuffer(Dummy, SizeOf(Dummy));
        Style := TFontStyles(Byte(Dummy));
      end;

      //Read data introduced by stream version 1+.
      if Version > 0 then
      begin
        ReadBuffer(Dummy, SizeOf(Dummy));
        MainColumn := Dummy;
        ReadBuffer(Dummy, SizeOf(Dummy));
        SortColumn := Dummy;
        ReadBuffer(Dummy, SizeOf(Dummy));
        SortDirection := TSortDirection(Byte(Dummy));
      end;

      //Read data introduced by stream version 5+.
      if Version > 4 then
      begin
        ReadBuffer(Dummy, SizeOf(Dummy));
        ParentFont := Boolean(Dummy);
        ReadBuffer(Dummy, SizeOf(Dummy));
        FMaxHeight := Integer(Dummy);
        ReadBuffer(Dummy, SizeOf(Dummy));
        FMinHeight := Integer(Dummy);
        ReadBuffer(Dummy, SizeOf(Dummy));
        FDefaultHeight := Integer(Dummy);
        with FFixedAreaConstraints do
        begin
          ReadBuffer(Dummy, SizeOf(Dummy));
          FMaxHeightPercent := TVTConstraintPercent(Dummy);
          ReadBuffer(Dummy, SizeOf(Dummy));
          FMaxWidthPercent := TVTConstraintPercent(Dummy);
          ReadBuffer(Dummy, SizeOf(Dummy));
          FMinHeightPercent := TVTConstraintPercent(Dummy);
          ReadBuffer(Dummy, SizeOf(Dummy));
          FMinWidthPercent := TVTConstraintPercent(Dummy);
        end;
      end;
    finally
      Exclude(FStates, hsLoading);
      RecalculateHeader();
      Tree.DoColumnResize(NoColumn);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.ResizeColumns(ChangeBy : TDimension; RangeStartCol : TColumnIndex; RangeEndCol : TColumnIndex; Options : TVTColumnOptions = [coVisible]) : TDimension;

//Distribute the given width change to a range of columns. A 'fair' way is used to distribute ChangeBy to the columns,
//while ensuring that everything that can be distributed will be distributed.

var
  Start, I                                         : TColumnIndex;
  ColCount,
  Sign: Integer;
  ToGo, MaxDelta, Difference, Rest: TDimension;
  Constraints, Widths                              : array of TDimension;
  BonusPixel                                       : Boolean;

  //--------------- local functions -------------------------------------------

  function IsResizable(Column : TColumnIndex) : Boolean;

  begin
    if BonusPixel then
      Result := Widths[Column - RangeStartCol] < Constraints[Column - RangeStartCol]
    else
      Result := Widths[Column - RangeStartCol] > Constraints[Column - RangeStartCol];
  end;

//---------------------------------------------------------------------------

  procedure IncDelta(Column : TColumnIndex);

  begin
    if BonusPixel then
      Inc(MaxDelta, FColumns[Column].MaxWidth - Widths[Column - RangeStartCol])
    else
      Inc(MaxDelta, Widths[Column - RangeStartCol] - Constraints[Column - RangeStartCol]);
  end;

//---------------------------------------------------------------------------

  function ChangeWidth(Column : TColumnIndex; Delta : TDimension) : TDimension;

  begin
    if Delta > 0 then
      Delta := Min(Delta, Constraints[Column - RangeStartCol] - Widths[Column - RangeStartCol])
    else
      Delta := Max(Delta, Constraints[Column - RangeStartCol] - Widths[Column - RangeStartCol]);

    Inc(Widths[Column - RangeStartCol], Delta);
    Dec(ToGo, Abs(Delta));
    Result := Abs(Delta);
  end;

//---------------------------------------------------------------------------

  function ReduceConstraints : Boolean;

  var
    MaxWidth: TDimension;
    MaxReserveCol, Column : TColumnIndex;

  begin
    Result := True;
    if not (hsScaling in FStates) or BonusPixel then
      Exit;

    MaxWidth := 0;
    MaxReserveCol := NoColumn;
    for Column := RangeStartCol to RangeEndCol do
      if (Options * FColumns[Column].Options = Options) and (FColumns[Column].Width > MaxWidth) then
      begin
        MaxWidth := Widths[Column - RangeStartCol];
        MaxReserveCol := Column;
      end;

    if (MaxReserveCol <= NoColumn) or (Constraints[MaxReserveCol - RangeStartCol] <= 10) then
      Result := False
    else
      Dec(Constraints[MaxReserveCol - RangeStartCol], Divide(Constraints[MaxReserveCol - RangeStartCol], 10));
  end;

//----------- end local functions -------------------------------------------

begin
  Result := 0;
  if (ChangeBy <> 0) and (RangeEndCol >= 0) then // RangeEndCol == -1 means no columns, so nothing to do
  begin
    //Do some initialization here
    BonusPixel := ChangeBy > 0;
    Sign := IfThen(BonusPixel, 1, - 1);
    Start := IfThen(BonusPixel, RangeStartCol, RangeEndCol);
    ToGo := Abs(ChangeBy);
    SetLength(Widths, RangeEndCol - RangeStartCol + 1);
    SetLength(Constraints, RangeEndCol - RangeStartCol + 1);
    for I := RangeStartCol to RangeEndCol do
    begin
      Widths[I - RangeStartCol] := FColumns[I].Width;
      Constraints[I - RangeStartCol] := IfThen(BonusPixel, FColumns[I].MaxWidth, FColumns[I].MinWidth);
    end;

    repeat
      repeat
        MaxDelta := 0;
        ColCount := 0;
        for I := RangeStartCol to RangeEndCol do
          if (Options * FColumns[I].Options = Options) and IsResizable(I) then
          begin
            System.Inc(ColCount);
            IncDelta(I);
          end;
        if MaxDelta < Abs(ChangeBy) then
          if not ReduceConstraints then
            Break;
      until (MaxDelta >= Abs(ChangeBy)) or not (hsScaling in FStates);

      if ColCount = 0 then
        Break;

      ToGo := Min(ToGo, MaxDelta);
      Difference := ToGo div ColCount;
      Rest := ToGo mod ColCount;

      if Difference > 0 then
        for I := RangeStartCol to RangeEndCol do
          if (Options * FColumns[I].Options = Options) and IsResizable(I) then
            ChangeWidth(I, Difference * Sign);

      //Now distribute Rest.
      I := Start;
      while Rest > 0 do
      begin
        if (Options * FColumns[I].Options = Options) and IsResizable(I) then
          if FColumns[I].BonusPixel <> BonusPixel then
          begin
            Dec(Rest, ChangeWidth(I, Sign));
            FColumns[I].BonusPixel := BonusPixel;
          end;
        System.Inc(I, Sign);
        if (BonusPixel and (I > RangeEndCol)) or (not BonusPixel and (I < RangeStartCol)) then
        begin
          for I := RangeStartCol to RangeEndCol do
            if Options * FColumns[I].Options = Options then
              FColumns[I].BonusPixel := not FColumns[I].BonusPixel;
          I := Start;
        end;
      end;
    until ToGo <= 0;

    //Now set the computed widths. We also compute the result here.
    Include(FStates, hsResizing);
    for I := RangeStartCol to RangeEndCol do
      if (Options * FColumns[I].Options = Options) then
      begin
        Inc(Result, Widths[I - RangeStartCol] - FColumns[I].Width);
        TVirtualTreeColumnCracker(FColumns[I]).SetWidth(Widths[I - RangeStartCol]);
      end;
    Exclude(FStates, hsResizing);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.RestoreColumns;

//Restores all columns to their width which they had before they have been auto fitted.

var
  I : TColumnIndex;

begin
  with TVirtualTreeColumnsCracker(FColumns) do
    for I := Count - 1 downto 0 do
      if [coResizable, coVisible] * Items[PositionToIndex[I]].Options = [coResizable, coVisible] then
        Items[I].RestoreLastWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.ToggleSortDirection;
// Toggles the current sorting direction
begin
  if SortDirection = sdDescending then
    SortDirection := sdAscending
  else
    SortDirection := sdDescending;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SaveToStream(const Stream : TStream);

//Saves the complete state of the header into the provided stream.

var
  Dummy : Integer;
  DummyDimension: TDimension;			   
  Tmp   : AnsiString;

begin
  with Stream do
  begin
    //In previous version of VT was no header stream version defined.
    //For feature enhancements it is necessary, however, to know which stream
    //format we are trying to load.
    //In order to distict from non-version streams an indicator is inserted.
    Dummy := - 1;
    WriteBuffer(Dummy, SizeOf(Dummy));
    //Write current stream version number, nothing more is required at the time being.
    Dummy := VTHeaderStreamVersion;
    WriteBuffer(Dummy, SizeOf(Dummy));

    //Save columns in case they depend on certain options (like auto size).
    Columns.SaveToStream(Stream);

    Dummy := FAutoSizeIndex;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := FBackgroundColor;
    WriteBuffer(Dummy, SizeOf(Dummy));
    DummyDimension:= FHeight;
    WriteBuffer(DummyDimension, SizeOf(DummyDimension));
    Dummy := Integer(FOptions);
    WriteBuffer(Dummy, SizeOf(Dummy));
    //PopupMenu is neither saved nor restored
    Dummy := Ord(FStyle);
    WriteBuffer(Dummy, SizeOf(Dummy));
    //TFont has no own save routine so we do it manually
    with Font do
    begin
      Dummy := Color;
      WriteBuffer(Dummy, SizeOf(Dummy));

      //Need only to write one: size or height, I decided to write height.
      DummyDimension := Height;
      WriteBuffer(DummyDimension, SizeOf(DummyDimension));
      Tmp := UTF8Encode(Name);
      Dummy := Length(Tmp);
      WriteBuffer(Dummy, SizeOf(Dummy));
      WriteBuffer(PAnsiChar(Tmp)^, Dummy);
      Dummy := Ord(Pitch);
      WriteBuffer(Dummy, SizeOf(Dummy));
      Dummy := Byte(Style);
      WriteBuffer(Dummy, SizeOf(Dummy));
    end;

    //Data introduced by stream version 1.
    Dummy := FMainColumn;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := FSortColumn;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Byte(FSortDirection);
    WriteBuffer(Dummy, SizeOf(Dummy));

    //Data introduced by stream version 5.
    Dummy := Integer(ParentFont);
    WriteBuffer(Dummy, SizeOf(Dummy));
    DummyDimension := FMaxHeight;
    WriteBuffer(DummyDimension, SizeOf(DummyDimension));
    DummyDimension := FMinHeight;
    WriteBuffer(DummyDimension, SizeOf(DummyDimension));
    DummyDimension := FDefaultHeight;
    WriteBuffer(DummyDimension, SizeOf(DummyDimension));

    with FFixedAreaConstraints do
    begin
      Dummy := Integer(FMaxHeightPercent);
      WriteBuffer(Dummy, SizeOf(Dummy));
      Dummy := Integer(FMaxWidthPercent);
      WriteBuffer(Dummy, SizeOf(Dummy));
      Dummy := Integer(FMinHeightPercent);
      WriteBuffer(Dummy, SizeOf(Dummy));
      Dummy := Integer(FMinWidthPercent);
      WriteBuffer(Dummy, SizeOf(Dummy));
    end;
  end;
end;

{ TVTHeaderHelper }

function TVTHeaderHelper.Tree : TBaseVirtualTreeCracker;
begin
  Result := TBaseVirtualTreeCracker(Self.FOwner);
end;


//----------------- TVirtualTreeColumn ---------------------------------------------------------------------------------

constructor TVirtualTreeColumn.Create(Collection : TCollection);

begin
  FMinWidth := 10;
  FMaxWidth := 10000;
  FImageIndex := - 1;
  FMargin := 4;
  FSpacing := cDefaultColumnSpacing;
  FText := '';
  FOptions := DefaultColumnOptions;
  FAlignment := taLeftJustify;
  FBiDiMode := bdLeftToRight;
  FColor := clWindow;
  FLayout := blGlyphLeft;
  FBonusPixel := False;
  FCaptionAlignment := taLeftJustify;
  FCheckType := ctCheckBox;
  FCheckState := csUncheckedNormal;
  FCheckBox := False;
  FHasImage := False;
  FDefaultSortDirection := sdAscending;
  FEditNextColumn := - 1;

  inherited Create(Collection);

  if Assigned(Owner) then
  begin
    FWidth := Owner.DefaultWidth;
    FLastWidth := Owner.DefaultWidth;
    FPosition := Owner.Count - 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCollection(Value : TCollection);
begin
  inherited;
  // Read parent bidi mode and color values as default values.
  ParentBiDiModeChanged;
  ParentColorChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVirtualTreeColumn.Destroy;

var
  I  : Integer;
  ai : TColumnIndex;
  sc : TColumnIndex;

  //--------------- local function ---------------------------------------------

  procedure AdjustColumnIndex(var ColumnIndex : TColumnIndex);

  begin
    if Index = ColumnIndex then
      ColumnIndex := NoColumn
    else
      if Index < ColumnIndex then
      System.Dec(ColumnIndex);
  end;

  //--------------- end local function -----------------------------------------

begin
  // Check if this column is somehow referenced by its collection parent or the header.
  with Owner do
  begin
    // If the columns collection object is currently deleting all columns
    // then we don't need to check the various cached indices individually.
    if not FClearing then
    begin
      TreeViewControl.CancelEditNode;
      IndexChanged(Index, - 1);

      AdjustColumnIndex(FHoverIndex);
      AdjustColumnIndex(FDownIndex);
      AdjustColumnIndex(FTrackIndex);
      AdjustColumnIndex(FClickIndex);

      with Header do
      begin
        ai := AutoSizeIndex;
        AdjustColumnIndex(ai);
        InternalSetAutoSizeIndex(ai);
        if Index = MainColumn then
        begin
          // If the current main column is about to be destroyed then we have to find a new main column.
          InternalSetMainColumn(NoColumn); //SetColumn has side effects we want to avoid here.
          for I := 0 to Count - 1 do
            if I <> Index then
            begin
              InternalSetMainColumn(I);
              Break;
            end;
        end;
        sc := SortColumn;
        AdjustColumnIndex(sc);
        InternalSetSortColumn(sc);
      end;
    end;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetCaptionAlignment : TAlignment;

begin
  if coUseCaptionAlignment in FOptions then
    Result := FCaptionAlignment
  else
    Result := FAlignment;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetCaptionWidth : TDimension;
var
  Theme             : HTHEME;
  AdvancedOwnerDraw : Boolean;
  PaintInfo         : THeaderPaintInfo;
  RequestedElements : THeaderPaintElements;

  TextSize          : TSize;
  HeaderGlyphSize   : TPoint;
  UseText           : Boolean;
  R                 : TRect;
begin
  AdvancedOwnerDraw := (hoOwnerDraw in Header.Options) and Assigned(TreeViewControl.OnAdvancedHeaderDraw) and Assigned(TreeViewControl.OnHeaderDrawQueryElements) and
    not (csDesigning in TreeViewControl.ComponentState);

  PaintInfo.Column := Self;
  PaintInfo.TargetCanvas := Owner.HeaderBitmap.Canvas;

  with PaintInfo, Column do
  begin
    ShowHeaderGlyph := (hoShowImages in Header.Options) and ((Assigned(Header.Images) and (FImageIndex > - 1)) or FCheckBox);
    ShowSortGlyph := ((Header.SortColumn > - 1) and (Self = Owner.Items[Header.SortColumn])) and (hoShowSortGlyphs in Header.Options);

      // This path for text columns or advanced owner draw.
      // See if the application wants to draw part of the header itself.
    RequestedElements := [];
    if AdvancedOwnerDraw then
    begin
      PaintInfo.Column := Self;
      TreeViewControl.DoHeaderDrawQueryElements(PaintInfo, RequestedElements);
    end;
  end;

  UseText := Length(FText) > 0;
  // If nothing is to show then don't waste time with useless preparation.
  if not (UseText or PaintInfo.ShowHeaderGlyph or PaintInfo.ShowSortGlyph) then
    Exit(0);

  // Calculate sizes of the involved items.
  with Header do
  begin
    if PaintInfo.ShowHeaderGlyph then
      if not FCheckBox then
      begin
        if Assigned(Images) then
          HeaderGlyphSize := Point(Images.Width, Images.Height);
      end
      else
        with Self.TreeViewControl do
        begin
          if Assigned(CheckImages) then
            HeaderGlyphSize := Point(CheckImages.Width, CheckImages.Height);
        end
    else
      HeaderGlyphSize := Point(0, 0);
    if PaintInfo.ShowSortGlyph then
    begin
      if tsUseExplorerTheme in Self.TreeViewControl.TreeStates then
      begin
        R := Rect(0, 0, 100, 100);
        Theme := OpenThemeData(Self.TreeViewControl.Handle, 'HEADER');
        GetThemePartSize(Theme, PaintInfo.TargetCanvas.Handle, HP_HEADERSORTARROW, HSAS_SORTEDUP, @R, TS_TRUE, PaintInfo.SortGlyphSize);
        CloseThemeData(Theme);
      end
      else
      begin
        PaintInfo.SortGlyphSize.cx := Self.TreeViewControl.ScaledPixels(16);
        PaintInfo.SortGlyphSize.cy := Self.TreeViewControl.ScaledPixels(4);
      end;
    end
    else
    begin
      PaintInfo.SortGlyphSize.cx := 0;
      PaintInfo.SortGlyphSize.cy := 0;
    end;
  end;

  if UseText then
  begin
    GetTextExtentPoint32W(PaintInfo.TargetCanvas.Handle, PWideChar(FText), Length(FText), TextSize);
    Inc(TextSize.cx, 2);
  end
  else
  begin
    TextSize.cx := 0;
    TextSize.cy := 0;
  end;

  // if CalculateTextRect then
  Result := TextSize.cx;
  if PaintInfo.ShowHeaderGlyph then
    if Layout in [blGlyphLeft, blGlyphRight] then
      Inc(Result, HeaderGlyphSize.X + FSpacing)
    else                                                    // if Layout in [ blGlyphTop, blGlyphBottom] then
      Result := Max(Result, HeaderGlyphSize.X);
  if PaintInfo.ShowSortGlyph then
    Inc(Result, PaintInfo.SortGlyphSize.cx + FSpacing + 2); // without this +2, there is a slight movement of the sort glyph when expanding the column

end;
//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetLeft : TDimension;

begin
  Result := FLeft;
  if [coVisible, coFixed] * FOptions <> [coVisible, coFixed] then
    Dec(Result, TreeViewControl.EffectiveOffsetX);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.IsBiDiModeStored : Boolean;

begin
  Result := not (coParentBidiMode in FOptions);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.IsCaptionAlignmentStored : Boolean;

begin
  Result := coUseCaptionAlignment in FOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.IsColorStored : Boolean;

begin
  Result := not (coParentColor in FOptions);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetAlignment(const Value : TAlignment);

begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
    // Setting the alignment affects also the tree, hence invalidate it too.
    TreeViewControl.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetBiDiMode(Value : TBiDiMode);

begin
  if Value <> FBiDiMode then
  begin
    FBiDiMode := Value;
    Exclude(FOptions, coParentBidiMode);
    Changed(False);
    // Setting the alignment affects also the tree, hence invalidate it too.
    TreeViewControl.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCaptionAlignment(const Value : TAlignment);

begin
  if not (coUseCaptionAlignment in FOptions) or (FCaptionAlignment <> Value) then
  begin
    FCaptionAlignment := Value;
    Include(FOptions, coUseCaptionAlignment);
    // Setting the alignment affects also the tree, hence invalidate it too.
    Header.Invalidate(Self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetColor(const Value : TColor);

begin
  if FColor <> Value then
  begin
    FColor := Value;
    Exclude(FOptions, coParentColor);
    Exclude(FOptions, coStyleColor); // Issue #919
    Changed(False);
    TreeViewControl.Invalidate;
  end;
end;

function TVirtualTreeColumn.GetEffectiveColor() : TColor;
// Returns the color that should effectively be used as background color for this
// column considering all flags in the TVirtualTreeColumn.Options property
begin
  if (coParentColor in Options) or ((coStyleColor in Options) and TreeViewControl.VclStyleEnabled) then
    Result := TreeViewControl.Colors.BackGroundColor
  else
    Result := Self.Color;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCheckBox(Value : Boolean);

begin
  if Value <> FCheckBox then
  begin
    FCheckBox := Value;
    if Value and (csDesigning in TreeViewControl.ComponentState) then
      Header.Options := Header.Options + [hoShowImages];
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCheckState(Value : TCheckState);

begin
  if Value <> FCheckState then
  begin
    FCheckState := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCheckType(Value : TCheckType);

begin
  if Value <> FCheckType then
  begin
    FCheckType := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetImageIndex(Value : TImageIndex);

begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetLayout(Value : TVTHeaderColumnLayout);

begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetMargin(Value : TDimension);

begin
  // Compatibility setting for -1.
  if Value < 0 then
    Value := 4;
  if FMargin <> Value then
  begin
    FMargin := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetMaxWidth(Value : TDimension);

begin
  if Value < FMinWidth then
    Value := FMinWidth;
  FMaxWidth := Value;
  SetWidth(FWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetMinWidth(Value : TDimension);

begin
  if Value < 0 then
    Value := 0;
  if Value > FMaxWidth then
    Value := FMaxWidth;
  FMinWidth := Value;
  SetWidth(FWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetOptions(Value : TVTColumnOptions);

var
  ToBeSet,
    ToBeCleared     : TVTColumnOptions;
  VisibleChanged,
    lParentColorSet : Boolean;
begin
  if FOptions <> Value then
  begin
    ToBeCleared := FOptions - Value;
    ToBeSet := Value - FOptions;

    FOptions := Value;

    VisibleChanged := coVisible in (ToBeSet + ToBeCleared);
    lParentColorSet := coParentColor in ToBeSet;

    if coParentBidiMode in ToBeSet then
      ParentBiDiModeChanged;
    if lParentColorSet then
    begin
      Include(FOptions, coStyleColor); // Issue #919
      ParentColorChanged();
    end;

    if coAutoSpring in ToBeSet then
      FSpringRest := 0;

    if coVisible in ToBeCleared then
      Header.UpdateMainColumn(); // Fixes issue #946

    if ((coFixed in ToBeSet) or (coFixed in ToBeCleared)) and (coVisible in FOptions) then
      Header.RescaleHeader;

    Changed(False);
    // Need to repaint and adjust the owner tree too.
    if not (csLoading in TreeViewControl.ComponentState) and (VisibleChanged or lParentColorSet) and (Owner.UpdateCount = 0) and TreeViewControl.HandleAllocated then
    begin
      TreeViewControl.Invalidate();
      if VisibleChanged then
      begin
        TreeViewControl.DoColumnVisibilityChanged(Self.Index, coVisible in ToBeSet);
        TreeViewControl.UpdateHorizontalScrollBar(False);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetPosition(Value : TColumnPosition);

var
  Temp : TColumnIndex;

begin
  if (csLoading in TreeViewControl.ComponentState) or (Owner.UpdateCount > 0) then
    // Only cache the position for final fixup when loading from DFM.
    FPosition := Value
  else
  begin
    if Value >= TColumnPosition(Collection.Count) then
      Value := Collection.Count - 1;
    if FPosition <> Value then
    begin
      with Owner do
      begin
        InitializePositionArray;
        TreeViewControl.CancelEditNode;
        AdjustPosition(Self, Value);
        Self.Changed(False);

        // Need to repaint.
        with Self.Header do
        begin
          if (UpdateCount = 0) and TreeViewControl.HandleAllocated then
          begin
            Invalidate(Self);
            TreeViewControl.Invalidate;
          end;
        end;
      end;

      // If the moved column is now within the fixed columns then we make it fixed as well. If it's not
      // we clear the fixed state (in case that fixed column is moved outside fixed area).
      if (coFixed in FOptions) and (FPosition > 0) then
        Temp := Owner.ColumnFromPosition(FPosition - 1)
      else
        Temp := Owner.ColumnFromPosition(FPosition + 1);

      if Temp <> NoColumn then
      begin
        if coFixed in Owner[Temp].Options then
          Options := Options + [coFixed]
        else
          Options := Options - [coFixed];
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetSpacing(Value : TDimension);

begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetStyle(Value : TVirtualTreeColumnStyle);

begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetText(const Value : string);

begin
  if FText <> Value then
  begin
    FText := Value;
    FCaptionText := '';
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetWidth(Value : TDimension);

var
  EffectiveMaxWidth,
    EffectiveMinWidth,
    TotalFixedMaxWidth,
    TotalFixedMinWidth : TDimension;
  I                    : TColumnIndex;

begin
  if not (hsScaling in Header.States) then
    if ([coVisible, coFixed] * FOptions = [coVisible, coFixed]) then
    begin
      with Header, FixedAreaConstraints, TreeViewControl do
      begin
        TotalFixedMinWidth := 0;
        TotalFixedMaxWidth := 0;
        for I := 0 to Columns.Count - 1 do
          if ([coVisible, coFixed] * Columns[I].Options = [coVisible, coFixed]) then
          begin
            Inc(TotalFixedMaxWidth, Columns[I].MaxWidth);
            Inc(TotalFixedMinWidth, Columns[I].MinWidth);
          end;

        if HandleAllocated then // Prevent premature creation of window handle, see issue #1073
        begin
          // The percentage values have precedence over the pixel values.
          If MaxWidthPercent > 0 then
            TotalFixedMinWidth := Min(Divide(ClientWidth * MaxWidthPercent, 100), TotalFixedMinWidth);
          If MinWidthPercent > 0 then
            TotalFixedMaxWidth := Max(Divide(ClientWidth * MinWidthPercent, 100), TotalFixedMaxWidth);

          EffectiveMaxWidth := Min(TotalFixedMaxWidth - (Columns.GetVisibleFixedWidth - Self.FWidth), FMaxWidth);
          EffectiveMinWidth := Max(TotalFixedMinWidth - (Columns.GetVisibleFixedWidth - Self.FWidth), FMinWidth);
          Value := Min(Max(Value, EffectiveMinWidth), EffectiveMaxWidth);

          if MinWidthPercent > 0 then
            Value := Max(Divide(ClientWidth * MinWidthPercent, 100) - Columns.GetVisibleFixedWidth + Self.FWidth, Value);
          if MaxWidthPercent > 0 then
            Value := Min(Divide(ClientWidth * MaxWidthPercent, 100) - Columns.GetVisibleFixedWidth + Self.FWidth, Value);
        end;// if HandleAllocated
      end;
    end
    else
      Value := Min(Max(Value, FMinWidth), FMaxWidth);

  if FWidth <> Value then
  begin
    FLastWidth := FWidth;
    if not (hsResizing in Header.States) then
      FBonusPixel := False;
    if not (hoAutoResize in Header.Options) or (Index <> Header.AutoSizeIndex) then
    begin
      FWidth := Value;
      Owner.UpdatePositions;
    end;
    if not (csLoading in TreeViewControl.ComponentState) and (TreeViewControl.UpdateCount = 0) then
    begin
      if hoAutoResize in Header.Options then
        Owner.AdjustAutoSize(Index);
      TreeViewControl.DoColumnResize(Index);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ChangeScale(M, D : TDimension);
begin
  FMinWidth := MulDiv(FMinWidth, M, D);
  FMaxWidth := MulDiv(FMaxWidth, M, D);
  FSpacing := MulDiv(FSpacing, M, D);
  Self.Width := MulDiv(Self.Width, M, D);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ComputeHeaderLayout(var PaintInfo : THeaderPaintInfo; DrawFormat : Cardinal; CalculateTextRect : Boolean = False);

// The layout of a column header is determined by a lot of factors. This method takes them all into account and
// determines all necessary positions and bounds:
// - for the header text
// - the header glyph
// - the sort glyph

var
  TextSize          : TSize;
  TextPos,
    ClientSize,
    HeaderGlyphSize : TPoint;
  CurrentAlignment  : TAlignment;
  MinLeft,
    MaxRight,
    TextSpacing     : TDimension;
  UseText           : Boolean;
  R                 : TRect;
  Theme             : HTHEME;

begin
  UseText := Length(FText) > 0;
  // If nothing is to show then don't waste time with useless preparation.
  if not (UseText or PaintInfo.ShowHeaderGlyph or PaintInfo.ShowSortGlyph) then
    Exit;

  CurrentAlignment := CaptionAlignment;
  if FBiDiMode <> bdLeftToRight then
    ChangeBiDiModeAlignment(CurrentAlignment);

  // Calculate sizes of the involved items.
  ClientSize := Point(PaintInfo.PaintRectangle.Right - PaintInfo.PaintRectangle.Left, PaintInfo.PaintRectangle.Bottom - PaintInfo.PaintRectangle.Top);
  with Owner, Header do
  begin
    if PaintInfo.ShowHeaderGlyph then
      if not FCheckBox then
        HeaderGlyphSize := Point(Images.Width, Images.Height)
      else
        with Self.TreeViewControl do
        begin
          if Assigned(CheckImages) then
            HeaderGlyphSize := Point(CheckImages.Width, CheckImages.Height);
        end
    else
      HeaderGlyphSize := Point(0, 0);
    if PaintInfo.ShowSortGlyph then
    begin
      if tsUseExplorerTheme in Self.TreeViewControl.TreeStates then
      begin
        R := Rect(0, 0, 100, 100);
        Theme := OpenThemeData(TreeViewControl.Handle, 'HEADER');
        GetThemePartSize(Theme, PaintInfo.TargetCanvas.Handle, HP_HEADERSORTARROW, HSAS_SORTEDUP, @R, TS_TRUE, PaintInfo.SortGlyphSize);
        CloseThemeData(Theme);
      end
      else
      begin
        PaintInfo.SortGlyphSize.cx := Self.TreeViewControl.ScaledPixels(16);
        PaintInfo.SortGlyphSize.cy := Self.TreeViewControl.ScaledPixels(4);
      end;

      // In any case, the sort glyph is vertically centered.
      PaintInfo.SortGlyphPos.Y := Divide(ClientSize.Y - PaintInfo.SortGlyphSize.cy, 2);
    end
    else
    begin
      PaintInfo.SortGlyphSize.cx := 0;
      PaintInfo.SortGlyphSize.cy := 0;
    end;
  end;

  if UseText then
  begin
    if not (coWrapCaption in FOptions) then
    begin
      FCaptionText := FText;
      GetTextExtentPoint32W(PaintInfo.TargetCanvas.Handle, PWideChar(FText), Length(FText), TextSize);
      Inc(TextSize.cx, 2);
      PaintInfo.TextRectangle := Rect(0, 0, TextSize.cx, TextSize.cy);
    end
    else
    begin
      R := PaintInfo.PaintRectangle;
      if FCaptionText = '' then
        FCaptionText := WrapString(PaintInfo.TargetCanvas.Handle, FText, R, DT_RTLREADING and DrawFormat <> 0, DrawFormat);

      GetStringDrawRect(PaintInfo.TargetCanvas.Handle, FCaptionText, R, DrawFormat);
      TextSize.cx := PaintInfo.PaintRectangle.Right - PaintInfo.PaintRectangle.Left;
      TextSize.cy := R.Bottom - R.Top;
      PaintInfo.TextRectangle := Rect(0, 0, TextSize.cx, TextSize.cy);
    end;
    TextSpacing := FSpacing;
  end
  else
  begin
    TextSpacing := 0;
    TextSize.cx := 0;
    TextSize.cy := 0;
  end;

  // Check first for the special case where nothing is shown except the sort glyph.
  if PaintInfo.ShowSortGlyph and not (UseText or PaintInfo.ShowHeaderGlyph) then
  begin
    // Center the sort glyph in the available area if nothing else is there.
    PaintInfo.SortGlyphPos := Point(Divide(ClientSize.X - PaintInfo.SortGlyphSize.cx, 2), Divide(ClientSize.Y - PaintInfo.SortGlyphSize.cy, 2));
  end
  else
  begin
    // Determine extents of text and glyph and calculate positions which are clear from the layout.
    if (Layout in [blGlyphLeft, blGlyphRight]) or not PaintInfo.ShowHeaderGlyph then
    begin
      PaintInfo.GlyphPos.Y := Divide(ClientSize.Y - HeaderGlyphSize.Y, 2);
      // If the text is taller than the given height, perform no vertical centration as this
      // would make the text even less readable.
      //Using Max() fixes badly positioned text if Extra Large fonts have been activated in the Windows display options
      TextPos.Y := Max( - 5, Divide(ClientSize.Y - TextSize.cy, 2));
    end
    else
    begin
      if Layout = blGlyphTop then
      begin
        PaintInfo.GlyphPos.Y := Divide(ClientSize.Y - HeaderGlyphSize.Y - TextSize.cy - TextSpacing, 2);
        TextPos.Y := PaintInfo.GlyphPos.Y + HeaderGlyphSize.Y + TextSpacing;
      end
      else
      begin
        TextPos.Y := Divide(ClientSize.Y - HeaderGlyphSize.Y - TextSize.cy - TextSpacing, 2);
        PaintInfo.GlyphPos.Y := TextPos.Y + TextSize.cy + TextSpacing;
      end;
    end;

    // Each alignment needs special consideration.
    case CurrentAlignment of
      taLeftJustify :
        begin
          MinLeft := FMargin;
          if PaintInfo.ShowSortGlyph and (FBiDiMode <> bdLeftToRight) then
          begin
            // In RTL context is the sort glyph placed on the left hand side.
            PaintInfo.SortGlyphPos.X := MinLeft;
            Inc(MinLeft, PaintInfo.SortGlyphSize.cx + FSpacing);
          end;
          if Layout in [blGlyphTop, blGlyphBottom] then
          begin
            // Header glyph is above or below text, so both must be considered when calculating
            // the left positition of the sort glyph (if it is on the right hand side).
            TextPos.X := MinLeft;
            if PaintInfo.ShowHeaderGlyph then
            begin
              PaintInfo.GlyphPos.X := Divide(ClientSize.X - HeaderGlyphSize.X, 2);
              if PaintInfo.GlyphPos.X < MinLeft then
                PaintInfo.GlyphPos.X := MinLeft;
              MinLeft := Max(TextPos.X + TextSize.cx + TextSpacing, PaintInfo.GlyphPos.X + HeaderGlyphSize.X + FSpacing);
            end
            else
              MinLeft := TextPos.X + TextSize.cx + TextSpacing;
          end
          else
          begin
            // Everything is lined up. TextSpacing might be 0 if there is no text.
            // This simplifies the calculation because no extra tests are necessary.
            if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphLeft) then
            begin
              PaintInfo.GlyphPos.X := MinLeft;
              Inc(MinLeft, HeaderGlyphSize.X + FSpacing);
            end;
            TextPos.X := MinLeft;
            Inc(MinLeft, TextSize.cx + TextSpacing);
            if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphRight) then
            begin
              PaintInfo.GlyphPos.X := MinLeft;
              Inc(MinLeft, HeaderGlyphSize.X + FSpacing);
            end;
          end;
          if PaintInfo.ShowSortGlyph and (FBiDiMode = bdLeftToRight) then
            PaintInfo.SortGlyphPos.X := MinLeft;
        end;
      taCenter :
        begin
          if Layout in [blGlyphTop, blGlyphBottom] then
          begin
            PaintInfo.GlyphPos.X := Divide(ClientSize.X - HeaderGlyphSize.X, 2);
            TextPos.X := Divide(ClientSize.X - TextSize.cx, 2);
            if PaintInfo.ShowSortGlyph then
              Dec(TextPos.X, Divide(PaintInfo.SortGlyphSize.cx, 2));
          end
          else
          begin
            MinLeft := Divide(ClientSize.X - HeaderGlyphSize.X - TextSpacing - TextSize.cx, 2);
            if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphLeft) then
            begin
              PaintInfo.GlyphPos.X := MinLeft;
              Inc(MinLeft, HeaderGlyphSize.X + TextSpacing);
            end;
            TextPos.X := MinLeft;
            Inc(MinLeft, TextSize.cx + TextSpacing);
            if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphRight) then
              PaintInfo.GlyphPos.X := MinLeft;
          end;
          if PaintInfo.ShowHeaderGlyph then
          begin
            MinLeft := Min(PaintInfo.GlyphPos.X, TextPos.X);
            MaxRight := Max(PaintInfo.GlyphPos.X + HeaderGlyphSize.X, TextPos.X + TextSize.cx);
          end
          else
          begin
            MinLeft := TextPos.X;
            MaxRight := TextPos.X + TextSize.cx;
          end;
          // Place the sort glyph directly to the left or right of the larger item.
          if PaintInfo.ShowSortGlyph then
            if FBiDiMode = bdLeftToRight then
            begin
              // Sort glyph on the right hand side.
              PaintInfo.SortGlyphPos.X := MaxRight + FSpacing;
            end
            else
            begin
              // Sort glyph on the left hand side.
              PaintInfo.SortGlyphPos.X := MinLeft - FSpacing - PaintInfo.SortGlyphSize.cx;
            end;
        end;
    else
      // taRightJustify
      MaxRight := ClientSize.X - FMargin;
      if PaintInfo.ShowSortGlyph and (FBiDiMode = bdLeftToRight) then
      begin
        // In LTR context is the sort glyph placed on the right hand side.
        Dec(MaxRight, PaintInfo.SortGlyphSize.cx);
        PaintInfo.SortGlyphPos.X := MaxRight;
        Dec(MaxRight, FSpacing);
      end;
      if Layout in [blGlyphTop, blGlyphBottom] then
      begin
        TextPos.X := MaxRight - TextSize.cx;
        if PaintInfo.ShowHeaderGlyph then
        begin
          PaintInfo.GlyphPos.X := Divide(ClientSize.X - HeaderGlyphSize.X, 2);
          if PaintInfo.GlyphPos.X + HeaderGlyphSize.X + FSpacing > MaxRight then
            PaintInfo.GlyphPos.X := MaxRight - HeaderGlyphSize.X - FSpacing;
          MaxRight := Min(TextPos.X - TextSpacing, PaintInfo.GlyphPos.X - FSpacing);
        end
        else
          MaxRight := TextPos.X - TextSpacing;
      end
      else
      begin
        // Everything is lined up. TextSpacing might be 0 if there is no text.
        // This simplifies the calculation because no extra tests are necessary.
        if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphRight) then
        begin
          PaintInfo.GlyphPos.X := MaxRight - HeaderGlyphSize.X;
          MaxRight := PaintInfo.GlyphPos.X - FSpacing;
        end;
        TextPos.X := MaxRight - TextSize.cx;
        MaxRight := TextPos.X - TextSpacing;
        if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphLeft) then
        begin
          PaintInfo.GlyphPos.X := MaxRight - HeaderGlyphSize.X;
          MaxRight := PaintInfo.GlyphPos.X - FSpacing;
        end;
      end;
      if PaintInfo.ShowSortGlyph and (FBiDiMode <> bdLeftToRight) then
        PaintInfo.SortGlyphPos.X := MaxRight - PaintInfo.SortGlyphSize.cx;
    end;
  end;

  // Once the position of each element is determined there remains only one but important step.
  // The horizontal positions of every element must be adjusted so that it always fits into the
  // given header area. This is accomplished by shorten the text appropriately.

  // These are the maximum bounds. Nothing goes beyond them.
  MinLeft := FMargin;
  MaxRight := ClientSize.X - FMargin;
  if PaintInfo.ShowSortGlyph then
  begin
    if FBiDiMode = bdLeftToRight then
    begin
      // Sort glyph on the right hand side.
      if PaintInfo.SortGlyphPos.X + PaintInfo.SortGlyphSize.cx > MaxRight then
        PaintInfo.SortGlyphPos.X := MaxRight - PaintInfo.SortGlyphSize.cx;
      MaxRight := PaintInfo.SortGlyphPos.X - FSpacing;
    end;

    // Consider also the left side of the sort glyph regardless of the bidi mode.
    if PaintInfo.SortGlyphPos.X < MinLeft then
      PaintInfo.SortGlyphPos.X := MinLeft;
    // Left border needs only adjustment if the sort glyph marks the left border.
    if FBiDiMode <> bdLeftToRight then
      MinLeft := PaintInfo.SortGlyphPos.X + PaintInfo.SortGlyphSize.cx + FSpacing;

    // Finally transform sort glyph to its actual position.
    Inc(PaintInfo.SortGlyphPos.X, PaintInfo.PaintRectangle.Left);
    Inc(PaintInfo.SortGlyphPos.Y, PaintInfo.PaintRectangle.Top);
  end;
  if PaintInfo.ShowHeaderGlyph then
  begin
    if PaintInfo.GlyphPos.X + HeaderGlyphSize.X > MaxRight then
      PaintInfo.GlyphPos.X := MaxRight - HeaderGlyphSize.X;
    if Layout = blGlyphRight then
      MaxRight := PaintInfo.GlyphPos.X - FSpacing;
    if PaintInfo.GlyphPos.X < MinLeft then
      PaintInfo.GlyphPos.X := MinLeft;
    if Layout = blGlyphLeft then
      MinLeft := PaintInfo.GlyphPos.X + HeaderGlyphSize.X + FSpacing;
    if FCheckBox and (Header.MainColumn = Self.Index) then
      Dec(PaintInfo.GlyphPos.X, 2)
    else
      if Header.MainColumn <> Self.Index then
      Dec(PaintInfo.GlyphPos.X, 2);

    // Finally transform header glyph to its actual position.
    Inc(PaintInfo.GlyphPos.X, PaintInfo.PaintRectangle.Left);
    Inc(PaintInfo.GlyphPos.Y, PaintInfo.PaintRectangle.Top);
  end;
  if UseText then
  begin
    if TextPos.X < MinLeft then
      TextPos.X := MinLeft;
    OffsetRect(PaintInfo.TextRectangle, TextPos.X, TextPos.Y);
    if PaintInfo.TextRectangle.Right > MaxRight then
      PaintInfo.TextRectangle.Right := MaxRight;
    OffsetRect(PaintInfo.TextRectangle, PaintInfo.PaintRectangle.Left, PaintInfo.PaintRectangle.Top);

    if coWrapCaption in FOptions then
    begin
      // Wrap the column caption if necessary.
      R := PaintInfo.TextRectangle;
      FCaptionText := WrapString(PaintInfo.TargetCanvas.Handle, FText, R, DT_RTLREADING and DrawFormat <> 0, DrawFormat);
      GetStringDrawRect(PaintInfo.TargetCanvas.Handle, FCaptionText, R, DrawFormat);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.DefineProperties(Filer : TFiler);

begin
  inherited;

  // These properites are remains from non-Unicode Delphi versions, readers remain for backward compatibility.
  Filer.DefineProperty('WideText', ReadText, nil, False);
  Filer.DefineProperty('WideHint', ReadHint, nil, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.GetAbsoluteBounds(var Left, Right : TDimension);

// Returns the column's left and right bounds in header coordinates, that is, independant of the scrolling position.

begin
  Left := FLeft;
  Right := FLeft + FWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetDisplayName : string;
begin
  Result := FText; // Use column header caption as display name
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetOwner : TVirtualTreeColumns;

begin
  Result := Collection as TVirtualTreeColumns;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.InternalSetWidth(const Value : TDimension);
begin
  FWidth := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ReadText(Reader : TReader);

begin
  SetText(Reader.ReadString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ReadHint(Reader : TReader);

begin
  FHint := Reader.ReadString;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.Assign(Source : TPersistent);

var
  OldOptions : TVTColumnOptions;

begin
  if Source is TVirtualTreeColumn then
  begin
    OldOptions := FOptions;
    FOptions := [];

    BiDiMode := TVirtualTreeColumn(Source).BiDiMode;
    ImageIndex := TVirtualTreeColumn(Source).ImageIndex;
    Layout := TVirtualTreeColumn(Source).Layout;
    Margin := TVirtualTreeColumn(Source).Margin;
    MaxWidth := TVirtualTreeColumn(Source).MaxWidth;
    MinWidth := TVirtualTreeColumn(Source).MinWidth;
    Position := TVirtualTreeColumn(Source).Position;
    Spacing := TVirtualTreeColumn(Source).Spacing;
    Style := TVirtualTreeColumn(Source).Style;
    Text := TVirtualTreeColumn(Source).Text;
    Hint := TVirtualTreeColumn(Source).Hint;
    Width := TVirtualTreeColumn(Source).Width;
    Alignment := TVirtualTreeColumn(Source).Alignment;
    CaptionAlignment := TVirtualTreeColumn(Source).CaptionAlignment;
    Color := TVirtualTreeColumn(Source).Color;
    Tag := TVirtualTreeColumn(Source).Tag;
    EditOptions := TVirtualTreeColumn(Source).EditOptions;
    EditNextColumn := TVirtualTreeColumn(Source).EditNextColumn;

    // Order is important. Assign options last.
    FOptions := OldOptions;
    Options := TVirtualTreeColumn(Source).Options;

    Changed(False);
  end
  else
    inherited Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.Equals(OtherColumnObj : TObject) : Boolean;
var
  OtherColumn : TVirtualTreeColumn;
begin
  if OtherColumnObj is TVirtualTreeColumn then
  begin
    OtherColumn := TVirtualTreeColumn(OtherColumnObj);
    Result := (BiDiMode = OtherColumn.BiDiMode) and
      (ImageIndex = OtherColumn.ImageIndex) and
      (Layout = OtherColumn.Layout) and
      (Margin = OtherColumn.Margin) and
      (MaxWidth = OtherColumn.MaxWidth) and
      (MinWidth = OtherColumn.MinWidth) and
      (Position = OtherColumn.Position) and
      (Spacing = OtherColumn.Spacing) and
      (Style = OtherColumn.Style) and
      (Text = OtherColumn.Text) and
      (Hint = OtherColumn.Hint) and
      (Width = OtherColumn.Width) and
      (Alignment = OtherColumn.Alignment) and
      (CaptionAlignment = OtherColumn.CaptionAlignment) and
      (Color = OtherColumn.Color) and
      (Tag = OtherColumn.Tag) and
      (Options = OtherColumn.Options);
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetRect : TRect;

// Returns the rectangle this column occupies in the header (relative to (0, 0) of the non-client area).

begin
  with TVirtualTreeColumns(GetOwner).FHeader do
    Result := TreeViewControl.HeaderRect;
  Inc(Result.Left, FLeft);
  Result.Right := Result.Left + FWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

// [IPK]
function TVirtualTreeColumn.GetText : string;

begin
  Result := FText;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.LoadFromStream(const Stream : TStream; Version : Integer);
var
  Dummy : Integer;
  S     : string;

begin
  with Stream do
  begin
    ReadBuffer(Dummy, SizeOf(Dummy));
    SetLength(S, Dummy);
    ReadBuffer(PWideChar(S)^, 2 * Dummy);
    Text := S;
    ReadBuffer(Dummy, SizeOf(Dummy));
    SetLength(FHint, Dummy);
    ReadBuffer(PWideChar(FHint)^, 2 * Dummy);
    ReadBuffer(Dummy, SizeOf(Dummy));
    Width := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    MinWidth := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    MaxWidth := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Style := TVirtualTreeColumnStyle(Dummy);
    ReadBuffer(Dummy, SizeOf(Dummy));
    ImageIndex := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Layout := TVTHeaderColumnLayout(Dummy);
    ReadBuffer(Dummy, SizeOf(Dummy));
    Margin := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Spacing := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    BiDiMode := TBiDiMode(Dummy);

    ReadBuffer(Dummy, SizeOf(Dummy));
    if Version >= 3 then
      Options := TVTColumnOptions(Dummy);

    if Version > 0 then
    begin
      // Parts which have been introduced/changed with header stream version 1+.
      ReadBuffer(Dummy, SizeOf(Dummy));
      Tag := Dummy;
      ReadBuffer(Dummy, SizeOf(Dummy));
      Alignment := TAlignment(Dummy);

      if Version > 1 then
      begin
        ReadBuffer(Dummy, SizeOf(Dummy));
        Color := TColor(Dummy);
      end;

      if Version > 5 then
      begin
        if coUseCaptionAlignment in FOptions then
        begin
          ReadBuffer(Dummy, SizeOf(Dummy));
          CaptionAlignment := TAlignment(Dummy);
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ParentBiDiModeChanged;

var
  Columns : TVirtualTreeColumns;

begin
  if coParentBidiMode in FOptions then
  begin
    Columns := GetOwner as TVirtualTreeColumns;
    if Assigned(Columns) and (FBiDiMode <> TreeViewControl.BiDiMode) then
    begin
      FBiDiMode := TreeViewControl.BiDiMode;
      Changed(False);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ParentColorChanged;

var
  Columns : TVirtualTreeColumns;

begin
  if coParentColor in FOptions then
  begin
    Columns := GetOwner as TVirtualTreeColumns;
    if Assigned(Columns) and (FColor <> TreeViewControl.Color) then
    begin
      FColor := TreeViewControl.Color;
      Changed(False);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.RestoreLastWidth;

begin
  TVirtualTreeColumns(GetOwner).AnimatedResize(Index, FLastWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SaveToStream(const Stream : TStream);

var
  Dummy : Integer;

begin
  with Stream do
  begin
    Dummy := Length(FText);
    WriteBuffer(Dummy, SizeOf(Dummy));
    WriteBuffer(PWideChar(FText)^, 2 * Dummy);
    Dummy := Length(FHint);
    WriteBuffer(Dummy, SizeOf(Dummy));
    WriteBuffer(PWideChar(FHint)^, 2 * Dummy);
    WriteBuffer(FWidth, SizeOf(FWidth));
    WriteBuffer(FMinWidth, SizeOf(FMinWidth));
    WriteBuffer(FMaxWidth, SizeOf(FMaxWidth));
    Dummy := Ord(FStyle);
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := FImageIndex;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Ord(FLayout);
    WriteBuffer(Dummy, SizeOf(Dummy));
    WriteBuffer(FMargin, SizeOf(FMargin));
    WriteBuffer(FSpacing, SizeOf(FSpacing));
    Dummy := Ord(FBiDiMode);
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Integer(FOptions);
    WriteBuffer(Dummy, SizeOf(Dummy));

    // parts introduced with stream version 1
    WriteBuffer(FTag, SizeOf(Dummy));
    Dummy := Cardinal(FAlignment);
    WriteBuffer(Dummy, SizeOf(Dummy));

    // parts introduced with stream version 2
    Dummy := Integer(FColor);
    WriteBuffer(Dummy, SizeOf(Dummy));

    // parts introduced with stream version 6
    if coUseCaptionAlignment in FOptions then
    begin
      Dummy := Cardinal(FCaptionAlignment);
      WriteBuffer(Dummy, SizeOf(Dummy));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.UseRightToLeftReading : Boolean;

begin
  Result := FBiDiMode <> bdLeftToRight;
end;

//----------------- TVirtualTreeColumns --------------------------------------------------------------------------------

constructor TVirtualTreeColumns.Create(AOwner : TVTHeader);

var
  ColumnClass : TVirtualTreeColumnClass;

begin
  FHeader := AOwner;

  // Determine column class to be used in the header.
  ColumnClass := Self.TreeViewControl.GetColumnClass;
  // The owner tree always returns the default tree column class if not changed by application/descendants.
  inherited Create(ColumnClass);

  FHeaderBitmap := TBitmap.Create;
  FHeaderBitmap.PixelFormat := pf32Bit;

  FHoverIndex := NoColumn;
  FDownIndex := NoColumn;
  FClickIndex := NoColumn;
  FDropTarget := NoColumn;
  FTrackIndex := NoColumn;
  FDefaultWidth := 50;
  Self.FColumnPopupMenu := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVirtualTreeColumns.Destroy;

begin
  FreeAndNil(FColumnPopupMenu);
  FreeAndNil(FHeaderBitmap);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetCount : Integer;

begin
  Result := inherited Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetItem(Index : TColumnIndex) : TVirtualTreeColumn;

begin
  Result := TVirtualTreeColumn(inherited GetItem(Index));
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetNewIndex(P : TPoint; var OldIndex : TColumnIndex) : Boolean;

var
  NewIndex : Integer;

begin
  Result := False;
  // convert to local coordinates
  Inc(P.Y, Header.Height);
  NewIndex := ColumnFromPosition(P);
  if NewIndex <> OldIndex then
  begin
    if OldIndex > NoColumn then
      Header.Invalidate(Items[OldIndex], False, True);
    OldIndex := NewIndex;
    if OldIndex > NoColumn then
      Header.Invalidate(Items[OldIndex], False, True);
    Result := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.SetDefaultWidth(Value : TDimension);

begin
  FDefaultWidth := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.SetItem(Index : TColumnIndex; Value : TVirtualTreeColumn);

begin
  inherited SetItem(Index, Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.StyleServices(AControl : TControl) : TCustomStyleServices;
begin
  if AControl = nil then
    AControl := TreeView;
  Result := VTStyleServices(AControl);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.AdjustAutoSize(CurrentIndex : TColumnIndex; Force : Boolean = False);

// Called only if the header is in auto-size mode which means a column needs to be so large
// that it fills all the horizontal space not occupied by the other columns.
// CurrentIndex (if not InvalidColumn) describes which column has just been resized.

var
    AutoIndex,
    Index: Integer;
    NewValue, RestWidth : TDimension;
  WasUpdating : Boolean;
begin
  if Count > 0 then
  begin
    // Determine index to be used for auto resizing. This is usually given by the owner's AutoSizeIndex, but
    // could be different if the column whose resize caused the invokation here is either the auto column itself
    // or visually to the right of the auto size column.
    AutoIndex := Header.AutoSizeIndex;
    if (AutoIndex < 0) or (AutoIndex >= Count) then
      AutoIndex := Count - 1;

    if AutoIndex >= 0 then
    begin
      with TreeViewControl do
      begin
        if HandleAllocated then
          RestWidth := ClientWidth
        else
          RestWidth := Width;
      end;

      // Go through all columns and calculate the rest space remaining.
      for Index := 0 to Count - 1 do
        if (Index <> AutoIndex) and (coVisible in Items[Index].Options) then
          Dec(RestWidth, Items[Index].Width);

      with Items[AutoIndex] do
      begin
        NewValue := Max(MinWidth, Min(MaxWidth, RestWidth));
        if Force or (FWidth <> NewValue) then
        begin
          FWidth := NewValue;
          UpdatePositions;
          WasUpdating := csUpdating in TreeViewControl.ComponentState;
          if not WasUpdating then
            TreeViewControl.Updating(); // Fixes #398
          try
            TreeViewControl.DoColumnResize(AutoIndex);
          finally
            if not WasUpdating then
              TreeViewControl.Updated();
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.AdjustDownColumn(P : TPoint) : TColumnIndex;

// Determines the column from the given position and returns it. If this column is allowed to be clicked then
// it is also kept for later use.

begin
  // Convert to local coordinates.
  Inc(P.Y, Header.Height);
  Result := ColumnFromPosition(P);
  if (Result > NoColumn) and (Result <> FDownIndex) and (coAllowClick in Items[Result].Options) and
    (coEnabled in Items[Result].Options) then
  begin
    if FDownIndex > NoColumn then
      Header.Invalidate(Items[FDownIndex]);
    FDownIndex := Result;
    FCheckBoxHit := Items[Result].HasImage and PtInRect(Items[Result].ImageRect, P) and Items[Result].CheckBox;
    Header.Invalidate(Items[FDownIndex]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.AdjustHoverColumn(P : TPoint) : Boolean;

// Determines the new hover column index and returns True if the index actually changed else False.

begin
  Result := GetNewIndex(P, FHoverIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.AdjustPosition(Column : TVirtualTreeColumn; Position : Cardinal);

// Reorders the column position array so that the given column gets the given position.

var
  OldPosition : Cardinal;

begin
  OldPosition := Column.Position;
  if OldPosition <> Position then
  begin
    if OldPosition < Position then
    begin
      // column will be moved up so move down other entries
      Move(FPositionToIndex[OldPosition + 1], FPositionToIndex[OldPosition], (Position - OldPosition) * SizeOf(Cardinal));
    end
    else
    begin
      // column will be moved down so move up other entries
      Move(FPositionToIndex[Position], FPositionToIndex[Position + 1], (OldPosition - Position) * SizeOf(Cardinal));
    end;
    FPositionToIndex[Position] := Column.Index;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.CanSplitterResize(P : TPoint; Column : TColumnIndex) : Boolean;

begin
  Result := (Column > NoColumn) and ([coResizable, coVisible] * Items[Column].Options = [coResizable, coVisible]);
  DoCanSplitterResize(P, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.DoCanSplitterResize(P : TPoint; Column : TColumnIndex; var Allowed : Boolean);

begin
  if Assigned(TreeViewControl.OnCanSplitterResizeColumn) then
    TreeViewControl.OnCanSplitterResizeColumn(Header, P, Column, Allowed);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.DrawButtonText(DC : HDC; Caption : string; Bounds : TRect; Enabled, Hot : Boolean;
  DrawFormat : Cardinal; WrapCaption : Boolean);

var
  TextSpace : TDimension;
  Size      : TSize;

begin
  if not WrapCaption then
  begin
    // Do we need to shorten the caption due to limited space?
    GetTextExtentPoint32W(DC, PWideChar(Caption), Length(Caption), Size);
    TextSpace := Bounds.Right - Bounds.Left;
    if TextSpace < Size.cx then
      Caption := ShortenString(DC, Caption, TextSpace);
  end;

  SetBkMode(DC, TRANSPARENT);
  if not Enabled then
    if TreeViewControl.VclStyleEnabled then
    begin
      SetTextColor(DC, ColorToRGB(TreeViewControl.Colors.HeaderFontColor));
      WinApi.Windows.DrawTextW(DC, PWideChar(Caption), Length(Caption), Bounds, DrawFormat);
    end
    else
    begin
      OffsetRect(Bounds, 1, 1);
      SetTextColor(DC, ColorToRGB(clBtnHighlight));
      WinApi.Windows.DrawTextW(DC, PWideChar(Caption), Length(Caption), Bounds, DrawFormat);
      OffsetRect(Bounds, - 1, - 1);
      SetTextColor(DC, ColorToRGB(clBtnShadow));
      WinApi.Windows.DrawTextW(DC, PWideChar(Caption), Length(Caption), Bounds, DrawFormat);
    end
  else
  begin
    if Hot then
      SetTextColor(DC, ColorToRGB(TreeViewControl.Colors.HeaderHotColor))
    else
      SetTextColor(DC, ColorToRGB(TreeViewControl.Colors.HeaderFontColor));
    WinApi.Windows.DrawTextW(DC, PWideChar(Caption), Length(Caption), Bounds, DrawFormat);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.FixPositions;
// Fixes column positions after loading from DFM or Bidi mode change.
var
  LColumnsByPos: TList<TVirtualTreeColumn>;
  I: Integer;
begin
  LColumnsByPos := TList<TVirtualTreeColumn>.Create;
  try
    LColumnsByPos.Capacity := Self.Count;
    for I := 0 to Self.Count-1 do
      LColumnsByPos.Add(Items[I]);

    LColumnsByPos.Sort(
      TComparer<TVirtualTreeColumn>.Construct(
        function(const A, B: TVirtualTreeColumn): Integer
        begin
          Result := CompareValue(A.Position, B.Position);
          if Result = 0 then
            Result := CompareValue(A.Index, B.Index);
        end)
    );

    for I := 0 to LColumnsByPos.Count-1 do
    begin
      LColumnsByPos[I].FPosition := I;
      Self.FPositionToIndex[I] := LColumnsByPos[I].Index;
    end;

  finally
	  LColumnsByPos.Free;
  end;

  FNeedPositionsFix := False;
  UpdatePositions(True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetColumnAndBounds(P : TPoint; var ColumnLeft, ColumnRight : TDimension;
  Relative : Boolean = True) : Integer;

// Returns the column where the mouse is currently in as well as the left and right bound of
// this column (Left and Right are undetermined if no column is involved).

var
  I : Integer;

begin
  Result := InvalidColumn;
  if Relative and (P.X >= Header.Columns.GetVisibleFixedWidth) then
    ColumnLeft := - TreeViewControl.EffectiveOffsetX
  else
    ColumnLeft := 0;

  if TreeViewControl.UseRightToLeftAlignment then
    Inc(ColumnLeft, TreeViewControl.ComputeRTLOffset(True));

  for I := 0 to Count - 1 do
    with Items[FPositionToIndex[I]] do
      if coVisible in FOptions then
      begin
        ColumnRight := ColumnLeft + FWidth;

        //fix: in right to left alignment, X can be in the
        //area on the left of first column which is OUT.
        if (P.X < ColumnLeft) and (I = 0) then
        begin
          Result := InvalidColumn;
          Exit;
        end;
        if P.X < ColumnRight then
        begin
          Result := FPositionToIndex[I];
          Exit;
        end;
        ColumnLeft := ColumnRight;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetOwner : TPersistent;

begin
  Result := FHeader;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.HandleClick(P : TPoint; Button : TMouseButton; Force, DblClick : Boolean) : Boolean;

// Generates a click event if the mouse button has been released over the same column it was pressed first.
// Alternatively, Force might be set to True to indicate that the down index does not matter (right, middle and
// double click).
// Returns true if the click was handled, False otherwise.

var
  HitInfo       : TVTHeaderHitInfo;
  NewClickIndex : Integer;
  Menu          : TPopupMenu;
begin
  Result := False;
  if (csDesigning in TreeViewControl.ComponentState) then
    Exit;
  // Convert vertical position to local coordinates.
  Inc(P.Y, Header.Height);
  NewClickIndex := ColumnFromPosition(P);
  with HitInfo do
  begin
    X := P.X;
    Y := P.Y;
    Shift := Header.GetShiftState;
    if DblClick then
      Shift := Shift + [ssDouble];
  end;
  HitInfo.Button := Button;

  if (NewClickIndex > NoColumn) and (coAllowClick in Items[NewClickIndex].Options) and
    ((NewClickIndex = FDownIndex) or Force) then
  begin
    FClickIndex := NewClickIndex;
    HitInfo.Column := NewClickIndex;
    HitInfo.HitPosition := [hhiOnColumn];

    if Items[NewClickIndex].HasImage and PtInRect(Items[NewClickIndex].ImageRect, P) then
    begin
      Include(HitInfo.HitPosition, hhiOnIcon);
      if Items[NewClickIndex].CheckBox then
      begin
        if Button = TMouseButton.mbLeft then
          TreeViewControl.UpdateColumnCheckState(Items[NewClickIndex]);
        Include(HitInfo.HitPosition, hhiOnCheckbox);
      end;
    end;
  end
  else
  begin
    FClickIndex := NoColumn;
    HitInfo.Column := NoColumn;
    HitInfo.HitPosition := [hhiNoWhere];
  end;

  if DblClick then
    TreeViewControl.DoHeaderDblClick(HitInfo)
  else begin
    if (hoHeaderClickAutoSort in Header.Options) and (HitInfo.Button = TMouseButton.mbLeft) and not (hhiOnCheckbox in HitInfo.HitPosition) and (HitInfo.Column >= 0) then
    begin
      // handle automatic setting of SortColumn and toggling of the sort order
      if HitInfo.Column <> Header.SortColumn then
      begin
        // set sort column
        Header.DoSetSortColumn(HitInfo.Column, Self[HitInfo.Column].DefaultSortDirection);
      end//if
      else
      begin
        // toggle sort direction
        if Header.SortDirection = sdDescending then
          Header.SortDirection := sdAscending
        else
          Header.SortDirection := sdDescending;
      end; //else
      Result := True;
    end;   //if

    if (Button = TMouseButton.mbRight) then
    begin
      Dec(P.Y, Header.Height);      // popup menus at actual clicked point
      FreeAndNil(FColumnPopupMenu); // Attention: Do not free the TVTHeaderPopupMenu at the end of this method, otherwise the clikc events of the menu item will not be fired.
      Self.FDownIndex := NoColumn;
      Self.FTrackIndex := NoColumn;
      Self.FCheckBoxHit := False;
      Menu := Header.DoGetPopupMenu(Self.ColumnFromPosition(Point(P.X, P.Y + TreeViewControl.Height)), P);
      if Assigned(Menu) then
      begin
        TreeViewControl.StopTimer(ScrollTimer);
        TreeViewControl.StopTimer(HeaderTimer);
        Header.Columns.SetHoverIndex(NoColumn);
        TreeViewControl.DoStateChange([], [tsScrollPending, tsScrolling]);

        Menu.PopupComponent := TreeViewControl;
        With TreeViewControl.ClientToScreen(P) do
          Menu.Popup(X, Y);
        Result := True;
      end
      else if (hoAutoColumnPopupMenu in Header.Options) then
      begin
        FColumnPopupMenu := TVTHeaderPopupMenu.Create(TreeViewControl);
        TVTHeaderPopupMenu(FColumnPopupMenu).OnAddHeaderPopupItem := HeaderPopupMenuAddHeaderPopupItem;
        FColumnPopupMenu.PopupComponent := TreeViewControl;
        if (hoDblClickResize in Header.Options) and ((TreeViewControl.ChildCount[nil] > 0) or (hoAutoResizeInclCaption in Header.Options)) then
          TVTHeaderPopupMenu(FColumnPopupMenu).Options := TVTHeaderPopupMenu(FColumnPopupMenu).Options + [poResizeToFitItem]
        else
          TVTHeaderPopupMenu(FColumnPopupMenu).Options := TVTHeaderPopupMenu(FColumnPopupMenu).Options - [poResizeToFitItem];
        With TreeViewControl.ClientToScreen(P) do
          FColumnPopupMenu.Popup(X, Y);
        Result := True;
      end; // if  hoAutoColumnPopupMenu
    end;   //if mbRight
    TreeViewControl.DoHeaderClick(HitInfo);
  end;     //else (not DblClick)

  if not (hhiNoWhere in HitInfo.HitPosition) then
    Header.Invalidate(Items[NewClickIndex]);
  if (FClickIndex > NoColumn) and (FClickIndex <> NewClickIndex) then
    Header.Invalidate(Items[FClickIndex]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.HeaderPopupMenuAddHeaderPopupItem(const Sender : TObject; const Column : TColumnIndex; var Cmd : TAddPopupItemType);
begin
  TBaseVirtualTreeCracker(Sender).DoHeaderAddPopupItem(Column, Cmd);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.IndexChanged(OldIndex, NewIndex : Integer);

// Called by a column when its index in the collection changes. If NewIndex is -1 then the column is
// about to be removed, otherwise it is moved to a new index.
// The method will then update the position array to reflect the change.

var
  I         : Integer;
  Increment : Integer;
  Lower,
    Upper   : Integer;

begin
  if NewIndex = - 1 then
  begin
    // Find position in the array with the old index.
    Upper := High(FPositionToIndex);
    for I := 0 to Upper do
    begin
      if FPositionToIndex[I] = OldIndex then
      begin
        // Index found. Move all higher entries one step down and remove the last entry.
        if I < Upper then
          System.Move(FPositionToIndex[I + 1], FPositionToIndex[I], (Upper - I) * SizeOf(TColumnIndex));
      end;
      // Decrease all indices, which are greater than the index to be deleted.
      if FPositionToIndex[I] > OldIndex then
        System.Dec(FPositionToIndex[I]);
    end;
    SetLength(FPositionToIndex, High(FPositionToIndex));
  end
  else
  begin
    if OldIndex < NewIndex then
      Increment := - 1
    else
      Increment := 1;

    Lower := Min(OldIndex, NewIndex);
    Upper := Max(OldIndex, NewIndex);
    for I := 0 to High(FPositionToIndex) do
    begin
      if (FPositionToIndex[I] >= Lower) and (FPositionToIndex[I] < Upper) then
        System.Inc(FPositionToIndex[I], Increment)
      else
        if FPositionToIndex[I] = OldIndex then
        FPositionToIndex[I] := NewIndex;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.InitializePositionArray;

// Ensures that the column position array contains as many entries as columns are defined.
// The array is resized and initialized with default values if needed.

var
  I, OldSize : Integer;
  Changed    : Boolean;

begin
  if Count <> Length(FPositionToIndex) then
  begin
    OldSize := Length(FPositionToIndex);
    SetLength(FPositionToIndex, Count);
    if Count > OldSize then
    begin
      // New items have been added, just set their position to the same as their index.
      for I := OldSize to Count - 1 do
        FPositionToIndex[I] := I;
    end
    else
    begin
      // Items have been deleted, so reindex remaining entries by decrementing values larger than the highest
      // possible index until no entry is higher than this limit.
      repeat
        Changed := False;
        for I := 0 to Count - 1 do
          if FPositionToIndex[I] >= Count then
          begin
            System.Dec(FPositionToIndex[I]);
            Changed := True;
          end;
      until not Changed;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.Notify(Item : TCollectionItem; Action : System.Classes.TCollectionNotification);
var
  I : Integer;
  lRemovedPosition: TColumnPosition;
begin
  if Action in [cnDeleting] then
  begin
    lRemovedPosition := TVirtualTreeColumn(Item).Position;
    // Adjust all positions larger than the deleted column's position. Fixes #959, #1049
    for I := Count - 1 downto 0 do
    begin
      if Items[I].Position > lRemovedPosition then
        Items[I].Position := Items[I].Position - 1;
    end; //for I

    with TreeViewControl do
      if not (csLoading in ComponentState) and (FocusedColumn = Item.Index) then
        InternalSetFocusedColumn(NoColumn); //bypass side effects in SetFocusedColumn
  end;                                      // if cnDeleting
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.ReorderColumns(RTL : Boolean);

var
  I : Integer;

begin
  if RTL then
  begin
    for I := 0 to Count - 1 do
      FPositionToIndex[I] := Count - I - 1;
  end
  else
  begin
    for I := 0 to Count - 1 do
      FPositionToIndex[I] := I;
  end;

  UpdatePositions(True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.SetHoverIndex(Index : TColumnIndex);
begin
  FHoverIndex := index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.EndUpdate;
begin
  InitializePositionArray();
  FixPositions(); // Accept the cuurent order. See issue #753
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.Update(Item : TCollectionItem);

begin
  // This is the only place which gets notified when a new column has been added or removed
  // and we need this event to adjust the column position array.
  InitializePositionArray;
  if csLoading in TreeViewControl.ComponentState then
    FNeedPositionsFix := True
  else
    UpdatePositions;

  // The first column which is created is by definition also the main column.
  if (Count > 0) and (Header.MainColumn < 0) then
    Header.MainColumn := 0;

  if not (csLoading in TreeViewControl.ComponentState) and not (hsLoading in Header.States) then
  begin
    with Header do
    begin
      if hoAutoResize in Options then
        AdjustAutoSize(InvalidColumn);
      if Assigned(Item) then
        Invalidate(Item as TVirtualTreeColumn)
      else
        if Self.TreeViewControl.HandleAllocated then
      begin
        Self.TreeViewControl.UpdateHorizontalScrollBar(False);
        Invalidate(nil);
        TreeViewControl.Invalidate;
      end;

      if not (Self.TreeViewControl.IsUpdating) then
        // This is mainly to let the designer know when a change occurs at design time which
        // doesn't involve the object inspector (like column resizing with the mouse).
        // This does NOT include design time code as the communication is done via an interface.
        Self.TreeViewControl.UpdateDesigner;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.UpdatePositions(Force : Boolean = False);

// Recalculates the left border of every column and updates their position property according to the
// PostionToIndex array which primarily determines where each column is placed visually.

var
  I: Integer;
  RunningPos: TDimension;
begin
  if not (csDestroying in TreeViewControl.ComponentState) and not FNeedPositionsFix and (Force or (UpdateCount = 0)) then
  begin
    RunningPos := 0;
    for I := 0 to High(FPositionToIndex) do
      with Items[FPositionToIndex[I]] do
      begin
        FPosition := I;
        FLeft := RunningPos;
        if coVisible in FOptions then
          Inc(RunningPos, FWidth);
      end;
    TreeViewControl.UpdateHorizontalScrollBar(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.Add : TVirtualTreeColumn;

begin
  Assert(GetCurrentThreadId = MainThreadId, 'UI controls may only be changed in UI thread.');
  Result := TVirtualTreeColumn(inherited Add);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.AnimatedResize(Column : TColumnIndex; NewWidth : TDimension);

// Resizes the given column animated by scrolling the window DC.

var
  OldWidth    : TDimension;
  DC          : TCanvas;
  I, Steps    : Integer;
  DX          : TDimension;
  HeaderScrollRect,
    ScrollRect,
    R         : TRect;

begin
  if not IsValidColumn(Column) then
    Exit; // Just in case.

  // Make sure the width constrains are considered.
  if NewWidth < Items[Column].MinWidth then
    NewWidth := Items[Column].MinWidth;
  if NewWidth > Items[Column].MaxWidth then
    NewWidth := Items[Column].MaxWidth;

  OldWidth := Items[Column].Width;
  // Nothing to do if the width is the same.
  if OldWidth <> NewWidth then
  begin
    if not ((hoDisableAnimatedResize in Header.Options) or
      (coDisableAnimatedResize in Items[Column].Options)) then
    begin
      DC := TCanvas.Create;
      DC.Handle := GetWindowDC(TreeViewControl.Handle);
      with TreeViewControl do
        try
          Steps := 32;
          DX := Divide(NewWidth - OldWidth, Steps);

        // Determination of the scroll rectangle is a bit complicated since we neither want
        // to scroll the scrollbars nor the border of the treeview window.
          HeaderScrollRect := HeaderRect;
          ScrollRect := HeaderScrollRect;
        // Exclude the header itself from scrolling.
          ScrollRect.Top := ScrollRect.Bottom;
          ScrollRect.Bottom := ScrollRect.Top + ClientHeight;
          ScrollRect.Right := ScrollRect.Left + ClientWidth;
          with Items[Column] do
            Inc(ScrollRect.Left, FLeft + FWidth);
          HeaderScrollRect.Left := ScrollRect.Left;
          HeaderScrollRect.Right := ScrollRect.Right;

        // When the new width is larger then avoid artefacts on the left hand side
        // by deleting a small stripe
          if NewWidth > OldWidth then
          begin
            R := ScrollRect;
//            NewBrush := CreateSolidBrush(ColorToRGB(Color));
//            LastBrush := SelectObject(DC, NewBrush);
            R.Right := R.Left + DX;
//            FillRect(DC, R, NewBrush);
//            SelectObject(DC, LastBrush);
//            DeleteObject(NewBrush);
            DC.Brush.Color := Color;
            DC.FillRect(R);
          end
          else
          begin
            Inc(HeaderScrollRect.Left, DX);
            Inc(ScrollRect.Left, DX);
          end;

          for I := 0 to Steps - 1 do
          begin
            ScrollDC(DC.Handle, DX, 0, HeaderScrollRect, HeaderScrollRect, 0, nil);
            Inc(HeaderScrollRect.Left, DX);
            ScrollDC(DC.Handle, DX, 0, ScrollRect, ScrollRect, 0, nil);
            Inc(ScrollRect.Left, DX);
            Sleep(1);
          end;
        finally
          ReleaseDC(Handle, DC.Handle);
          DC.Free;
        end;
    end;
    Items[Column].Width := NewWidth;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.Assign(Source : TPersistent);

begin
  // Let the collection class assign the items.
  inherited;

  if Source is TVirtualTreeColumns then
  begin
    // Copying the position array is the only needed task here.
    FPositionToIndex := Copy(TVirtualTreeColumns(Source).FPositionToIndex, 0, MaxInt);

    // Make sure the left edges are correct after assignment.
    FNeedPositionsFix := False;
    UpdatePositions(True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.Clear;

begin
  FClearing := True;
  try
    TreeViewControl.CancelEditNode;

    // Since we're freeing all columns, the following have to be true when we're done.
    FHoverIndex := NoColumn;
    FDownIndex := NoColumn;
    FTrackIndex := NoColumn;
    FClickIndex := NoColumn;
    FCheckBoxHit := False;

    with Header do
      if not (hsLoading in States) then
      begin
        InternalSetAutoSizeIndex(NoColumn); //bypass side effects in SetAutoSizeColumn
        MainColumn := NoColumn;
        InternalSetSortColumn(NoColumn);    //bypass side effects in SetSortColumn
      end;

    with TreeViewControl do
      if not (csLoading in ComponentState) then
        InternalSetFocusedColumn(NoColumn); //bypass side effects in SetFocusedColumn

    inherited Clear;
  finally
    FClearing := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.ColumnFromPosition(P : TPoint; Relative : Boolean = True) : TColumnIndex;

// Determines the current column based on the position passed in P.

var
  I: Integer;
  Sum: TDimension;
begin
  Result := InvalidColumn;

  // The position must be within the header area, but we extend the vertical bounds to the entire treeview area.
  if (P.X >= 0) and (P.Y >= 0) and (P.Y <= TreeViewControl.Height) then
    with FHeader, TreeViewControl do
    begin
      if Relative and (P.X >= GetVisibleFixedWidth) then
        Sum := - EffectiveOffsetX
      else
        Sum := 0;

      if UseRightToLeftAlignment then
        Inc(Sum, ComputeRTLOffset(True));

      for I := 0 to Count - 1 do
        if coVisible in Items[FPositionToIndex[I]].Options then
        begin
          Inc(Sum, Items[FPositionToIndex[I]].Width);
          if P.X < Sum then
          begin
            Result := FPositionToIndex[I];
            Break;
          end;
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.ColumnFromPosition(PositionIndex : TColumnPosition) : TColumnIndex;

// Returns the index of the column at the given position.

begin
  if Integer(PositionIndex) < Length(FPositionToIndex) then
    Result := FPositionToIndex[PositionIndex]
  else
    Result := NoColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.Equals(OtherColumnsObj : TObject) : Boolean;

// Compares itself with the given set of columns and returns True if all published properties are the same
// (including column order), otherwise False is returned.

var
  I            : Integer;
  OtherColumns : TVirtualTreeColumns;

begin
  if not (OtherColumnsObj is TVirtualTreeColumns) then
  begin
    Result := False;
    Exit;
  end;

  OtherColumns := TVirtualTreeColumns(OtherColumnsObj);

  // Same number of columns?
  Result := OtherColumns.Count = Count;
  if Result then
  begin
    // Same order of columns?
    Result := CompareMem(Pointer(FPositionToIndex), Pointer(OtherColumns.FPositionToIndex),
      Length(FPositionToIndex) * SizeOf(TColumnIndex));
    if Result then
    begin
      for I := 0 to Count - 1 do
        if not Items[I].Equals(OtherColumns[I]) then
        begin
          Result := False;
          Break;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.GetColumnBounds(Column : TColumnIndex; var Left, Right : TDimension);

// Returns the left and right bound of the given column. If Column is NoColumn then the entire client width is returned.

begin
  if Column <= NoColumn then
  begin
    Left := 0;
    Right := TreeViewControl.ClientWidth;
  end
  else
  begin
    Left := Items[Column].Left;
    Right := Left + Items[Column].Width;
    if TreeViewControl.UseRightToLeftAlignment then
    begin
      Inc(Left, TreeViewControl.ComputeRTLOffset(True));
      Inc(Right, TreeViewControl.ComputeRTLOffset(True));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetScrollWidth : TDimension;

// Returns the average width of all visible, non-fixed columns. If there is no such column the indent is returned.

var
  I                 : Integer;
  ScrollColumnCount : Integer;

begin

  Result := 0;

  ScrollColumnCount := 0;
  for I := 0 to Header.Columns.Count - 1 do
  begin
    if ([coVisible, coFixed] * Header.Columns[I].Options = [coVisible]) then
    begin
      Inc(Result, Header.Columns[I].Width);
      System.Inc(ScrollColumnCount);
    end;
  end;

  if ScrollColumnCount > 0 then // use average width
    Result := Round(Result / ScrollColumnCount)
  else                          // use indent
    Result := TreeViewControl.Indent;

end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetTreeView: TCustomControl;
begin
  Result := TBaseVirtualTreeCracker(Header.GetOwner);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetFirstVisibleColumn(ConsiderAllowFocus : Boolean = False) : TColumnIndex;

// Returns the index of the first visible column or "InvalidColumn" if either no columns are defined or
// all columns are hidden.
// If ConsiderAllowFocus is True then the column has not only to be visible but also focus has to be allowed.

var
  I : Integer;

begin
  Result := InvalidColumn;
  if (UpdateCount > 0) or (csLoading in TreeViewControl.ComponentState) then
    Exit; // See issue #760
  for I := 0 to Count - 1 do
    if (coVisible in Items[FPositionToIndex[I]].Options) and
      ((not ConsiderAllowFocus) or
      (coAllowFocus in Items[FPositionToIndex[I]].Options)
      ) then
    begin
      Result := FPositionToIndex[I];
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetLastVisibleColumn(ConsiderAllowFocus : Boolean = False) : TColumnIndex;

// Returns the index of the last visible column or "InvalidColumn" if either no columns are defined or
// all columns are hidden.
// If ConsiderAllowFocus is True then the column has not only to be visible but also focus has to be allowed.

var
  I : Integer;

begin
  Result := InvalidColumn;
  if (UpdateCount > 0) or (csLoading in TreeViewControl.ComponentState) then
    Exit; // See issue #760
  for I := Count - 1 downto 0 do
    if (coVisible in Items[FPositionToIndex[I]].Options) and
      ((not ConsiderAllowFocus) or
      (coAllowFocus in Items[FPositionToIndex[I]].Options)
      ) then
    begin
      Result := FPositionToIndex[I];
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetFirstColumn : TColumnIndex;

// Returns the first column in display order.

begin
  if Count = 0 then
    Result := InvalidColumn
  else
    Result := FPositionToIndex[0];
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetNextColumn(Column : TColumnIndex) : TColumnIndex;

// Returns the next column in display order. Column is the index of an item in the collection (a column).

var
  Position : Integer;

begin
  if Column < 0 then
    Result := InvalidColumn
  else
  begin
    Position := Items[Column].Position;
    if Position < Count - 1 then
      Result := FPositionToIndex[Position + 1]
    else
      Result := InvalidColumn;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetNextVisibleColumn(Column : TColumnIndex; ConsiderAllowFocus : Boolean = False) : TColumnIndex;

// Returns the next visible column in display order, Column is an index into the columns list.
// If ConsiderAllowFocus is True then the column has not only to be visible but also focus has to be allowed.

begin
  Result := Column;
  repeat
    Result := GetNextColumn(Result);
  until (Result = InvalidColumn) or
    ((coVisible in Items[Result].Options) and
    ((not ConsiderAllowFocus) or
    (coAllowFocus in Items[Result].Options)
    )
    );
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetPreviousColumn(Column : TColumnIndex) : TColumnIndex;

// Returns the previous column in display order, Column is an index into the columns list.

var
  Position : Integer;

begin
  if Column < 0 then
    Result := InvalidColumn
  else
  begin
    Position := Items[Column].Position;
    if Position > 0 then
      Result := FPositionToIndex[Position - 1]
    else
      Result := InvalidColumn;
    Assert(Column <> Result, 'The previous column must not have the same position as the given column.');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetPreviousVisibleColumn(Column : TColumnIndex; ConsiderAllowFocus : Boolean = False) : TColumnIndex;

// Returns the previous visible column in display order, Column is an index into the columns list.
// If ConsiderAllowFocus is True then the column has not only to be visible but also focus has to be allowed.

begin
  Result := Column;
  repeat
    Result := GetPreviousColumn(Result);
  until (Result = InvalidColumn) or
    ((coVisible in Items[Result].Options) and
    ((not ConsiderAllowFocus) or
    (coAllowFocus in Items[Result].Options)
    )
    );
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetVisibleColumns : TColumnsArray;

// Returns a list of all currently visible columns in actual order.

var
  I, Counter : Integer;

begin
  SetLength(Result, Count);
  Counter := 0;

  for I := 0 to Count - 1 do
    if coVisible in Items[FPositionToIndex[I]].Options then
    begin
      Result[Counter] := Items[FPositionToIndex[I]];
      System.Inc(Counter);
    end;
  // Set result length to actual visible count.
  SetLength(Result, Counter);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetVisibleFixedWidth : TDimension;

// Determines the horizontal space all visible and fixed columns occupy.

var
  I : Integer;

begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Options * [coVisible, coFixed] = [coVisible, coFixed] then
      Inc(Result, Items[I].Width);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.IsValidColumn(Column : TColumnIndex) : Boolean;

// Determines whether the given column is valid or not, that is, whether it is one of the current columns.

begin
  Result := (Column > NoColumn) and (Column < Count);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.LoadFromStream(const Stream : TStream; Version : Integer);

var
  I,
    ItemCount : Integer;

begin
  Clear;
  Stream.ReadBuffer(ItemCount, SizeOf(ItemCount));
  // number of columns
  if ItemCount > 0 then
  begin
    BeginUpdate;
    try
      for I := 0 to ItemCount - 1 do
        Add.LoadFromStream(Stream, Version);
      SetLength(FPositionToIndex, ItemCount);
      Stream.ReadBuffer(FPositionToIndex[0], ItemCount * SizeOf(TColumnIndex));
      UpdatePositions(True);
    finally
      EndUpdate;
    end;
  end;

  // Data introduced with header stream version 5
  if Version > 4 then
    Stream.ReadBuffer(FDefaultWidth, SizeOf(FDefaultWidth));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.PaintHeader(DC : HDC; R : TRect; HOffset : TDimension);

// Backward compatible header paint method. This method takes care of visually moving floating columns

var
  VisibleFixedWidth : TDimension;
  RTLOffset         : TDimension;

  procedure PaintFixedArea;

  begin
    if VisibleFixedWidth > 0 then
      PaintHeader(FHeaderBitmap.Canvas,
        Rect(0, 0, Min(R.Right, VisibleFixedWidth), R.Bottom - R.Top),
        Point(R.Left, R.Top), RTLOffset);
  end;

begin
  // Adjust size of the header bitmap
  FHeaderBitmap.SetSize(Max(TreeViewControl.HeaderRect.Right, R.Right - R.Left), TreeViewControl.HeaderRect.Bottom);

  VisibleFixedWidth := GetVisibleFixedWidth;

  // Consider right-to-left directionality.
  if TreeViewControl.UseRightToLeftAlignment then
    RTLOffset := TreeViewControl.ComputeRTLOffset
  else
    RTLOffset := 0;

  if RTLOffset = 0 then
    PaintFixedArea;

  // Paint the floating part of the header.
  PaintHeader(FHeaderBitmap.Canvas,
    Rect(VisibleFixedWidth - HOffset, 0, R.Right + VisibleFixedWidth - HOffset, R.Bottom - R.Top),
    Point(R.Left + VisibleFixedWidth, R.Top), RTLOffset);

  // In case of right-to-left directionality we paint the fixed part last.
  if RTLOffset <> 0 then
    PaintFixedArea;

  // Blit the result to target.
  BitBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, FHeaderBitmap.Canvas.Handle, R.Left, R.Top, SRCCOPY);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.PaintHeader(TargetCanvas : TCanvas; R : TRect; const Target : TPoint;
  RTLOffset : TDimension = 0);

// Main paint method to draw the header.
// This procedure will paint the a slice (given in R) out of HeaderRect into TargetCanvas starting at position Target.
// This function does not offer the option to visually move floating columns due to scrolling. To accomplish this you
// need to call this method twice.

var
  Run : TColumnIndex;
  RightBorderFlag,
    NormalButtonStyle,
    NormalButtonFlags,
    PressedButtonStyle,
    PressedButtonFlags,
    RaisedButtonStyle,
    RaisedButtonFlags : Cardinal;
  Images              : TCustomImageList;
  OwnerDraw,
    AdvancedOwnerDraw : Boolean;
  PaintInfo           : THeaderPaintInfo;
  RequestedElements,
    ActualElements    : THeaderPaintElements;

  //--------------- local functions -------------------------------------------

  procedure PrepareButtonStyles;

  // Prepare the button styles and flags for later usage.

  begin
    RaisedButtonStyle := 0;
    RaisedButtonFlags := 0;
    case Header.Style of
      hsThickButtons :
        begin
          NormalButtonStyle := BDR_RAISEDINNER or BDR_RAISEDOUTER;
          NormalButtonFlags := BF_LEFT or BF_TOP or BF_BOTTOM or BF_MIDDLE or BF_SOFT or BF_ADJUST;
          PressedButtonStyle := BDR_RAISEDINNER or BDR_RAISEDOUTER;
          PressedButtonFlags := NormalButtonFlags or BF_RIGHT or BF_FLAT or BF_ADJUST;
        end;
      hsFlatButtons :
        begin
          NormalButtonStyle := BDR_RAISEDINNER;
          NormalButtonFlags := BF_LEFT or BF_TOP or BF_BOTTOM or BF_MIDDLE or BF_ADJUST;
          PressedButtonStyle := BDR_SUNKENOUTER;
          PressedButtonFlags := BF_RECT or BF_MIDDLE or BF_ADJUST;
        end;
    else
      // hsPlates or hsXPStyle, values are not used in the latter case
      begin
        NormalButtonStyle := BDR_RAISEDINNER;
        NormalButtonFlags := BF_RECT or BF_MIDDLE or BF_SOFT or BF_ADJUST;
        PressedButtonStyle := BDR_SUNKENOUTER;
        PressedButtonFlags := BF_RECT or BF_MIDDLE or BF_ADJUST;
        RaisedButtonStyle := BDR_RAISEDINNER;
        RaisedButtonFlags := BF_LEFT or BF_TOP or BF_BOTTOM or BF_MIDDLE or BF_ADJUST;
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure DrawBackground;

  // Draw the header background.

  var
    BackgroundRect : TRect;
    Details        : TThemedElementDetails;
    Theme          : HTHEME;
  begin
    BackgroundRect := Rect(Target.X, Target.Y, Target.X + R.Right - R.Left, Target.Y + Header.Height);

    with TargetCanvas do
    begin
      if hpeBackground in RequestedElements then
      begin
        PaintInfo.PaintRectangle := BackgroundRect;
        TreeViewControl.DoAdvancedHeaderDraw(PaintInfo, [hpeBackground]);
      end
      else
      begin
        if (TreeViewControl.VclStyleEnabled and (seClient in TreeViewControl.StyleElements)) then
        begin
          Details := StyleServices.GetElementDetails(thHeaderItemRightNormal);
          StyleServices.DrawElement(Handle, Details, BackgroundRect, @BackgroundRect {$IF CompilerVersion  >= 34}, TreeViewControl.FCurrentPPI{$IFEND});
        end
        else
          if tsUseThemes in TreeViewControl.TreeStates then
        begin
          Theme := OpenThemeData(TreeViewControl.Handle, 'HEADER');
          DrawThemeBackground(Theme, Handle, HP_HEADERITEM, HIS_NORMAL, BackgroundRect, nil);
          CloseThemeData(Theme);
        end
        else
        begin
          Brush.Color := Header.Background;
          FillRect(BackgroundRect);
        end;
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure PaintColumnHeader(AColumn : TColumnIndex; ATargetRect : TRect);

  // Draw a single column to TargetRect. The clipping rect needs to be set before
  // this procedure is called.

  var
    SavedDC        : Integer;
    ColCaptionText : string;
    ColImageInfo   : TVTImageInfo;
    Glyph          : TThemedHeader;
    Details        : TThemedElementDetails;
    WrapCaption    : Boolean;
    DrawFormat     : Cardinal;
    Pos            : TRect;
    DrawHot        : Boolean;
    ImageWidth     : Integer;
  begin
    ColImageInfo.Ghosted := False;
    PaintInfo.Column := Items[AColumn];
    with PaintInfo, Column do
    begin
      IsHoverIndex := (AColumn = FHoverIndex) and (hoHotTrack in Header.Options) and (coEnabled in Options);
      IsDownIndex := (AColumn = FDownIndex) and not FCheckBoxHit;

      if (coShowDropMark in FOptions) and (AColumn = FDropTarget) and (AColumn <> FDragIndex) then
      begin
        if FDropBefore then
          DropMark := dmmLeft
        else
          DropMark := dmmRight;
      end
      else
        DropMark := dmmNone;

      //Fix for issue 643
      //Do not show the left drop mark if the position to drop is just preceding the target which means
      //the dragged column will stay where it is
      if (DropMark = dmmLeft) and (Items[FDragIndex].Position = TColumnPosition(Max(Integer(Items[FDropTarget].Position) - 1, 0)))
      then
        DropMark := dmmNone
      else
      //Do not show the right drop mark if the position to drop is just following the target which means
      //the dragged column will stay where it is
        if (DropMark = dmmRight) and (Items[FDragIndex].Position = Items[FDropTarget].Position + 1)
        then
          DropMark := dmmNone;

      IsEnabled := (coEnabled in FOptions) and (TreeViewControl.Enabled);
      ShowHeaderGlyph := (hoShowImages in Header.Options) and ((Assigned(Images) and (FImageIndex > - 1)) or FCheckBox);
      ShowSortGlyph := (AColumn = Header.SortColumn) and (hoShowSortGlyphs in Header.Options);
      WrapCaption := coWrapCaption in FOptions;

      PaintRectangle := ATargetRect;

      // This path for text columns or advanced owner draw.
      if (Style = vsText) or not OwnerDraw or AdvancedOwnerDraw then
      begin
        // See if the application wants to draw part of the header itself.
        RequestedElements := [];
        if AdvancedOwnerDraw then
        begin
          PaintInfo.Column := Items[AColumn];
          TreeViewControl.DoHeaderDrawQueryElements(PaintInfo, RequestedElements);
        end;

        if ShowRightBorder or (AColumn < Count - 1) then
          RightBorderFlag := BF_RIGHT
        else
          RightBorderFlag := 0;

        if hpeBackground in RequestedElements then
          TreeViewControl.DoAdvancedHeaderDraw(PaintInfo, [hpeBackground])
        else
        begin
          if (tsUseThemes in TreeViewControl.TreeStates) or (TreeViewControl.VclStyleEnabled and (seClient in TreeViewControl.StyleElements)) then
          begin
            if IsDownIndex then
              Details := StyleServices.GetElementDetails(thHeaderItemPressed)
            else
              if IsHoverIndex then
              Details := StyleServices.GetElementDetails(thHeaderItemHot)
            else
              Details := StyleServices.GetElementDetails(thHeaderItemNormal);
            StyleServices.DrawElement(TargetCanvas.Handle, Details, PaintRectangle, @PaintRectangle{$IF CompilerVersion >= 34}, TreeViewControl.CurrentPPI{$IFEND});
            {$IF CompilerVersion >= 34}
            if TreeViewControl.CurrentPPI >= 144 then // Fixes issue #1172
            begin
              PaintRectangle.Right := PaintRectangle.Right - 1; // For screens with scaled at 150% or more use a splitter with two pixels width
              StyleServices.DrawElement(TargetCanvas.Handle, Details, PaintRectangle, @PaintRectangle, TreeViewControl.CurrentPPI);
            end;
            {$IFEND}
          end
          else
          begin // Windows classic mode
            if IsDownIndex then
              DrawEdge(TargetCanvas.Handle, PaintRectangle, PressedButtonStyle, PressedButtonFlags)
            else
              // Plates have the special case of raising on mouse over.
              if (Header.Style = hsPlates) and IsHoverIndex and
                (coAllowClick in FOptions) and (coEnabled in FOptions) then
                DrawEdge(TargetCanvas.Handle, PaintRectangle, RaisedButtonStyle,
                  RaisedButtonFlags or RightBorderFlag)
              else
                DrawEdge(TargetCanvas.Handle, PaintRectangle, NormalButtonStyle,
                  NormalButtonFlags or RightBorderFlag);
          end;
        end;

        PaintRectangle := ATargetRect;

        // calculate text and glyph position
        InflateRect(PaintRectangle, - 2, - 2);
        DrawFormat := DT_TOP or DT_NOPREFIX;
        case CaptionAlignment of
          taLeftJustify :
            DrawFormat := DrawFormat or DT_LEFT;
          taRightJustify :
            DrawFormat := DrawFormat or DT_RIGHT;
          taCenter :
            DrawFormat := DrawFormat or DT_CENTER;
        end;
        if UseRightToLeftReading then
          DrawFormat := DrawFormat + DT_RTLREADING;
        ComputeHeaderLayout(PaintInfo, DrawFormat);

        // Move glyph and text one pixel to the right and down to simulate a pressed button.
        if IsDownIndex then
        begin
          OffsetRect(TextRectangle, 1, 1);
          Inc(GlyphPos.X);
          Inc(GlyphPos.Y);
          Inc(SortGlyphPos.X);
          Inc(SortGlyphPos.Y);
        end;

        // Advanced owner draw allows to paint elements, which would normally not be painted (because of space
        // limitations, empty captions etc.).
        ActualElements := RequestedElements * [hpeHeaderGlyph, hpeSortGlyph, hpeDropMark, hpeText, hpeOverlay];

        // main glyph
        FHasImage := False;
        if Assigned(Images) then
          ImageWidth := Images.Width
        else
          ImageWidth := 0;

        if not (hpeHeaderGlyph in ActualElements) and ShowHeaderGlyph and
          (not ShowSortGlyph or (FBiDiMode <> bdLeftToRight) or (GlyphPos.X + ImageWidth <= SortGlyphPos.X)) then
        begin
          if not FCheckBox then
          begin
            ColImageInfo.Images := Images;
            Images.Draw(TargetCanvas, GlyphPos.X, GlyphPos.Y, FImageIndex, IsEnabled);
          end
          else
          begin
            with TreeViewControl do
            begin
              ColImageInfo.Images := CheckImages;
              ColImageInfo.Index := GetCheckImage(nil, FCheckType, FCheckState, IsEnabled);
              ColImageInfo.XPos := GlyphPos.X;
              ColImageInfo.YPos := GlyphPos.Y;
              PaintCheckImage(TargetCanvas, ColImageInfo, False);
            end;
          end;

          FHasImage := True;
          FImageRect.Left := GlyphPos.X;
          FImageRect.Top := GlyphPos.Y;
          FImageRect.Right := FImageRect.Left + ColImageInfo.Images.Width;
          FImageRect.Bottom := FImageRect.Top + ColImageInfo.Images.Height;
        end;

        // caption
        if WrapCaption then
          ColCaptionText := FCaptionText
        else
          ColCaptionText := Text;
        if IsHoverIndex and TreeViewControl.VclStyleEnabled then
          DrawHot := True
        else
          DrawHot := (IsHoverIndex and (hoHotTrack in Header.Options) and not (tsUseThemes in TreeViewControl.TreeStates));
        if not (hpeText in ActualElements) and (Length(Text) > 0) then
          DrawButtonText(TargetCanvas.Handle, ColCaptionText, TextRectangle, IsEnabled, DrawHot, DrawFormat, WrapCaption);

        // sort glyph
        if not (hpeSortGlyph in ActualElements) and ShowSortGlyph then
        begin
          if tsUseExplorerTheme in TreeViewControl.TreeStates then
          begin
            Pos.TopLeft := SortGlyphPos;
            Pos.Right := Pos.Left + SortGlyphSize.cx;
            Pos.Bottom := Pos.Top + SortGlyphSize.cy;
            if Header.SortDirection = sdAscending then
              Glyph := thHeaderSortArrowSortedUp
            else
              Glyph := thHeaderSortArrowSortedDown;
            Details := StyleServices.GetElementDetails(Glyph);
            if not StyleServices.DrawElement(TargetCanvas.Handle, Details, Pos, @Pos {$IF CompilerVersion  >= 34}, TreeViewControl.CurrentPPI {$IFEND}) then
              PaintInfo.DrawSortArrow(Header.SortDirection);
          end
          else
          begin
            PaintInfo.DrawSortArrow(Header.SortDirection);
          end;
        end;

        // Show an indication if this column is the current drop target in a header drag operation.
        if not (hpeDropMark in ActualElements) and (DropMark <> dmmNone) then
        begin
          PaintInfo.DrawDropMark();
        end;

        if ActualElements <> [] then
        begin
          SavedDC := SaveDC(TargetCanvas.Handle);
          TreeViewControl.DoAdvancedHeaderDraw(PaintInfo, ActualElements);
          RestoreDC(TargetCanvas.Handle, SavedDC);
        end;
      end
      else // Let application draw the header.
        TreeViewControl.DoHeaderDraw(TargetCanvas, Items[AColumn], PaintRectangle, IsHoverIndex, IsDownIndex,
          DropMark);
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  TargetRect : TRect;
  MaxX       : TDimension;
begin
  if IsRectEmpty(R) then
    Exit;

  // If both draw posibillities are specified then prefer the advanced way.
  AdvancedOwnerDraw := (hoOwnerDraw in Header.Options) and Assigned(TreeViewControl.OnAdvancedHeaderDraw) and
    Assigned(TreeViewControl.OnHeaderDrawQueryElements) and not (csDesigning in TreeViewControl.ComponentState);
  OwnerDraw := (hoOwnerDraw in Header.Options) and Assigned(TreeViewControl.OnHeaderDraw) and
    not (csDesigning in TreeViewControl.ComponentState) and not AdvancedOwnerDraw;

  ZeroMemory(@PaintInfo, SizeOf(PaintInfo));
  PaintInfo.TargetCanvas := TargetCanvas;

  with PaintInfo, TargetCanvas do
  begin
    // Use shortcuts for the images and the font.
    Images := Header.Images;
    Font := Header.Font;

    PrepareButtonStyles;

    // At first, query the application which parts of the header it wants to draw on its own.
    RequestedElements := [];
    if AdvancedOwnerDraw then
    begin
      PaintRectangle := R;
      Column := nil;
      TreeViewControl.DoHeaderDrawQueryElements(PaintInfo, RequestedElements);
    end;

    // Draw the background.
    DrawBackground;

    // Now that we have drawn the background, we apply the header's dimensions to R.
    R := Rect(Max(R.Left, 0), Max(R.Top, 0), Min(R.Right, TotalWidth), Min(R.Bottom, Header.Height));

    // Determine where to stop.
    MaxX := Target.X + R.Right - R.Left
    //Fixes issues #544, #427 -- MaxX should also shift on BidiMode bdRightToLeft
      + RTLOffset; //added for fix

    // Determine the start column.
    Run := ColumnFromPosition(Point(R.Left + RTLOffset, 0), False);
    if Run <= NoColumn then
      Exit;

    TargetRect.Top := Target.Y;
    TargetRect.Bottom := Target.Y + R.Bottom - R.Top;
    TargetRect.Left := Target.X - R.Left + Items[Run].FLeft + RTLOffset;
    // TargetRect.Right will be set in the loop

    ShowRightBorder := (Header.Style = hsThickButtons) or not (hoAutoResize in Header.Options) or (TreeViewControl.BevelKind = TBevelKind.bkNone);

    // Now go for each button.
    while (Run > NoColumn) and (TargetRect.Left < MaxX) do
    begin
      TargetRect.Right := TargetRect.Left + Items[Run].Width;

      // create a clipping rect to limit painting to button area
      ClipCanvas(TargetCanvas, Rect(Max(TargetRect.Left, Target.X), Target.Y + R.Top,
        Min(TargetRect.Right, MaxX), TargetRect.Bottom));

      PaintColumnHeader(Run, TargetRect);

      SelectClipRgn(Handle, 0);

      TargetRect.Left := TargetRect.Right;
      Run := GetNextVisibleColumn(Run);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.SaveToStream(const Stream : TStream);

var
  I : Integer;

begin
  I := Count;
  Stream.WriteBuffer(I, SizeOf(I));
  if I > 0 then
  begin
    for I := 0 to Count - 1 do
      TVirtualTreeColumn(Items[I]).SaveToStream(Stream);

    Stream.WriteBuffer(FPositionToIndex[0], Count * SizeOf(TColumnIndex));
  end;

  // Data introduced with header stream version 5.
  Stream.WriteBuffer(DefaultWidth, SizeOf(DefaultWidth));
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.TotalWidth : TDimension;

var
  LastColumn : TColumnIndex;

begin
  Result := 0;
  if (Count > 0) and (Length(FPositionToIndex) > 0) then
  begin
    LastColumn := FPositionToIndex[Count - 1];
    if not (coVisible in Items[LastColumn].Options) then
      LastColumn := GetPreviousVisibleColumn(LastColumn);
    if LastColumn > NoColumn then
      with Items[LastColumn] do
        Result := FLeft + FWidth;
  end;
end;

{ THeaderPaintInfo }

procedure THeaderPaintInfo.DrawDropMark();
var
  Y           : TDimension;
  lArrowWidth : TDimension;
begin
  lArrowWidth := TBaseVirtualTreeCracker(Self.Column.TreeViewControl).ScaledPixels(5);
  Y := Divide(PaintRectangle.Top + PaintRectangle.Bottom - 3 * lArrowWidth, 2);
  if DropMark = dmmLeft then
    DrawArrow(TargetCanvas, TScrollDirection.sdLeft, Point(PaintRectangle.Left, Y), lArrowWidth)
  else
    DrawArrow(TargetCanvas, TScrollDirection.sdRight, Point(PaintRectangle.Right - lArrowWidth - Divide(lArrowWidth, 2) {spacing}, Y), lArrowWidth);
end;

procedure THeaderPaintInfo.DrawSortArrow(pDirection : TSortDirection);
const
  cDirection : array [TSortDirection] of TScrollDirection = (TScrollDirection.sdUp, TScrollDirection.sdDown);
var
  lOldColor : TColor;
begin
  lOldColor := TargetCanvas.Pen.Color;
  TargetCanvas.Pen.Color := clDkGray;
  DrawArrow(TargetCanvas, cDirection[pDirection], Point(SortGlyphPos.X, SortGlyphPos.Y), SortGlyphSize.cy);
  TargetCanvas.Pen.Color := lOldColor;
end;

{ TVirtualTreeColumnHelper }

function TVirtualTreeColumnHelper.Header : TVTHeader;
begin
  Result := Owner.Header;
end;

function TVirtualTreeColumnHelper.TreeViewControl : TBaseVirtualTreeCracker;
begin
  Result := TBaseVirtualTreeCracker(Owner.Header.GetOwner);
end;

{ TVirtualTreeColumnsHelper }

function TVirtualTreeColumnsHelper.TreeViewControl : TBaseVirtualTreeCracker;
begin
  Result := TBaseVirtualTreeCracker(Header.GetOwner);
end;


end.
