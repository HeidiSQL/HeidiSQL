unit VirtualTrees.BaseTree;

interface

{$if CompilerVersion < 24}{$MESSAGE FATAL 'This version supports only RAD Studio XE3 and higher. Please use V5 from  http://www.jam-software.com/virtual-treeview/VirtualTreeViewV5.5.3.zip  or  https://github.com/Virtual-TreeView/Virtual-TreeView/archive/V5_stable.zip'}{$ifend}

{$booleval off} // Use fastest possible boolean evaluation

// For some things to work we need code, which is classified as being unsafe for .NET.
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

{$LEGACYIFEND ON}
{$WARN UNSUPPORTED_CONSTRUCT      OFF}

{$HPPEMIT '#include <objidl.h>'}
{$HPPEMIT '#include <oleidl.h>'}
{$HPPEMIT '#include <oleacc.h>'}
{$ifdef BCB}
  {$HPPEMIT '#pragma comment(lib, "VirtualTreesCR")'}
{$else}
  {$HPPEMIT '#pragma comment(lib, "VirtualTreesR")'}
{$endif}
{$HPPEMIT '#pragma comment(lib, "Shell32")'}
{$HPPEMIT '#pragma comment(lib, "uxtheme")'}
{$HPPEMIT '#pragma link "VirtualTrees.Accessibility"'}

uses
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, Winapi.CommCtrl,
  Winapi.UxTheme, Winapi.ShlObj,
  System.SysUtils, System.Classes, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ImgList, Vcl.StdCtrls,
  Vcl.Menus, Vcl.Printers, Vcl.Themes,
  System.UITypes, // some types moved from Vcl.* to System.UITypes
  VirtualTrees.Types,
  VirtualTrees.Colors,
  VirtualTrees.DragImage,
  VirtualTrees.Header
{$IFDEF VT_FMX}
  , VirtualTrees.BaseAncestorFMX
{$ELSE}
  , VirtualTrees.BaseAncestorVCL
{$ENDIF}
  ;

{$MinEnumSize 1, make enumerations as small as possible}

type
  {$IFDEF VT_FMX}
    TVTBaseAncestor        = TVTBaseAncestorFMX;
    TCanvas                = FMX.Graphics.TCanvas;
  {$ELSE}
    TVTBaseAncestor        = TVTBaseAncestorVcl;
    TCanvas                = Vcl.Graphics.TCanvas;
    TFormatEtcArray        = VirtualTrees.Types.TFormatEtcArray;											  
  {$ENDIF}

  // Alias defintions for convenience
  TImageIndex              = System.UITypes.TImageIndex;

  //these were moved, aliases are for backwards compatibility.
  //some may be removed once we sort out excactly what is needed.

  TDimension               = VirtualTrees.Types.TDimension;
  TColumnIndex             = VirtualTrees.Types.TColumnIndex;
  TColumnPosition          = VirtualTrees.Types.TColumnPosition;
  EVirtualTreeError        = VirtualTrees.Types.EVirtualTreeError;
  TAutoScrollInterval      = VirtualTrees.Types.TAutoScrollInterval;
  TVTScrollIncrement       = VirtualTrees.Types.TVTScrollIncrement;
  TFormatArray             = VirtualTrees.Types.TFormatArray;

  TVTPaintOption           = VirtualTrees.Types.TVTPaintOption;
  TVTPaintOptions          = VirtualTrees.Types.TVTPaintOptions;
  TVTAnimateOption         = VirtualTrees.Types.TVTAnimationOption;
  TVTAnimateOptions        = VirtualTrees.Types.TVTAnimationOptions;
  TVTAutoOption            = VirtualTrees.Types.TVTAutoOption;
  TVTAutoOptions           = VirtualTrees.Types.TVTAutoOptions;
  TVTSelectionOption       = VirtualTrees.Types.TVTSelectionOption;
  TVTSelectionOptions      = VirtualTrees.Types.TVTSelectionOptions;
  TVTEditOptions           = VirtualTrees.Types.TVTEditOptions;
  TVTMiscOption            = VirtualTrees.Types.TVTMiscOption;
  TVTMiscOptions           = VirtualTrees.Types.TVTMiscOptions;
  TVTExportMode            = VirtualTrees.Types.TVTExportMode;
  TVTStringOption          = VirtualTrees.Types.TVTStringOption;
  TVTStringOptions         = VirtualTrees.Types.TVTStringOptions;
  TCustomVirtualTreeOptions= VirtualTrees.Types.TCustomVirtualTreeOptions;
  TVirtualTreeOptions      = VirtualTrees.Types.TVirtualTreeOptions;
  TTreeOptionsClass        = VirtualTrees.Types.TTreeOptionsClass;
  TCustomStringTreeOptions = VirtualTrees.Types.TCustomStringTreeOptions;
  TStringTreeOptions       = VirtualTrees.Types.TStringTreeOptions;

  TScrollBarStyle          = VirtualTrees.Types.TScrollBarStyle;
  TScrollBarOptions        = VirtualTrees.Types.TScrollBarOptions;

  TVTColumnOption          = VirtualTrees.Types.TVTColumnOption;
  TVTColumnOptions         = VirtualTrees.Types.TVTColumnOptions;
  TVirtualTreeColumnStyle  = VirtualTrees.Types.TVirtualTreeColumnStyle;
  TSortDirection           = VirtualTrees.Types.TSortDirection;
  TCheckType               = VirtualTrees.Types.TCheckType;
  TCheckState              = VirtualTrees.Types.TCheckState;
  TVTDropMarkMode          = VirtualTrees.Types.TVTDropMarkMode;
  TScrollDirections        = VirtualTrees.Types.TScrollDirections;
  TVirtualTreeColumn       = VirtualTrees.Header.TVirtualTreeColumn;
  TVirtualTreeColumns      = VirtualTrees.Header.TVirtualTreeColumns;
  TVirtualTreeColumnClass  = VirtualTrees.Header.TVirtualTreeColumnClass;
  TColumnsArray            = VirtualTrees.Header.TColumnsArray;
  TCardinalArray           = VirtualTrees.Header.TCardinalArray;
  TIndexArray              = VirtualTrees.Header.TIndexArray;

  TVTColors                = VirtualTrees.Colors.TVTColors;
  //

type
  TBaseVirtualTree = class;
  TVirtualTreeClass = class of TBaseVirtualTree;


  // This record must already be defined here and not later because otherwise BCB users will not be able
  // to compile (conversion done by BCB is wrong).
  TCacheEntry = record
    Node: PVirtualNode;
    AbsoluteTop: TNodeHeight;
  end;

  TCache = array of TCacheEntry;

  // Used in the CF_VTREFERENCE clipboard format.
  PVTReference = ^TVTReference;
  TVTReference = record
    Process: Cardinal;
    Tree: TBaseVirtualTree;
  end;


  // ----- OLE drag'n drop handling

  IVTDragManager = interface(IUnknown)
    ['{C4B25559-14DA-446B-8901-0C879000EB16}']
    procedure ForceDragLeave; stdcall;
    function GetDataObject: IDataObject; stdcall;
    function GetDragSource: TBaseVirtualTree; stdcall;
    function GetIsDropTarget: Boolean; stdcall;

    property DataObject: IDataObject read GetDataObject;
    property DragSource: TBaseVirtualTree read GetDragSource;
    property IsDropTarget: Boolean read GetIsDropTarget;
  end;



  PVTHintData = ^TVTHintData;
  TVTHintData = record
    Tree: TBaseVirtualTree;
    Node: PVirtualNode;
    Column: TColumnIndex;
    HintRect: TRect;            // used for draw trees only, string trees get the size from the hint string
    HintText: string;    // set when size of the hint window is calculated
    BidiMode: TBidiMode;
    Alignment: TAlignment;
    LineBreakStyle: TVTToolTipLineBreakStyle;
  end;

  // Communication interface between a tree editor and the tree itself (declared as using stdcall in case it
  // is implemented in a (C/C++) DLL). The GUID is not nessecary in Delphi but important for BCB users
  // to allow QueryInterface and _uuidof calls.
  IVTEditLink = interface
    ['{2BE3EAFA-5ACB-45B4-9D9A-B58BCC496E17}']
    function BeginEdit: Boolean; stdcall;                  // Called when editing actually starts.
    function CancelEdit: Boolean; stdcall;                 // Called when editing has been cancelled by the tree.
    function EndEdit: Boolean; stdcall;                     // Called when editing has been finished by the tree. Returns True if successful, False if edit mode is still active.
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
                                                           // Called after creation to allow a setup.
    procedure ProcessMessage(var Message: TMessage); stdcall;
                                                           // Used to forward messages to the edit window(s)-
    procedure SetBounds(R: TRect); stdcall;                // Called to place the editor.
  end;


  TVTNodeExportEvent   = procedure (Sender: TBaseVirtualTree; aExportType: TVTExportType; Node: PVirtualNode) of object;
  TVTColumnExportEvent = procedure (Sender: TBaseVirtualTree; aExportType: TVTExportType; Column: TVirtualTreeColumn) of object;
  TVTTreeExportEvent   = procedure(Sender: TBaseVirtualTree; aExportType: TVTExportType) of object;

  TClipboardFormats = class(TStringList)
  private
    FOwner: TBaseVirtualTree;
  public
    constructor Create(AOwner: TBaseVirtualTree); virtual;

    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    property Owner: TBaseVirtualTree read FOwner;
  end;

  // ----- Event prototypes:

  // node enumeration
  TVTGetNodeProc = reference to procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
  // node events
  TVTChangingEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean) of object;
  TVTCheckChangingEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
    var Allowed: Boolean) of object;
  TVTChangeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode) of object;
  TVTStructureChangeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason) of object;
  TVTEditCancelEvent = procedure(Sender: TBaseVirtualTree; Column: TColumnIndex) of object;
  TVTEditChangingEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    var Allowed: Boolean) of object;
  TVTEditChangeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex) of object;
  TVTFreeNodeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode) of object;
  TVTFocusChangingEvent = procedure(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
    NewColumn: TColumnIndex; var Allowed: Boolean) of object;
  TVTFocusChangeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex) of object;
  TVTAddToSelectionEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode) of object;
  TVTRemoveFromSelectionEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode) of object;
  TVTGetImageEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var Ghosted: Boolean; var ImageIndex: TImageIndex) of object;
  TVTGetImageExEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var Ghosted: Boolean; var ImageIndex: TImageIndex; var ImageList: TCustomImageList) of object;
  TVTGetImageTextEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var ImageText: string) of object;
  TVTHotNodeChangeEvent = procedure(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode) of object;
  TVTInitChildrenEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal) of object;
  TVTInitNodeEvent = procedure(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
    var InitialStates: TVirtualNodeInitStates) of object;
  TVTPopupEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
    var AskParent: Boolean; var PopupMenu: TPopupMenu) of object;
  TVTHelpContextEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    var HelpContext: Integer) of object;
  TVTCreateEditorEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    out EditLink: IVTEditLink) of object;
  TVTSaveTreeEvent = procedure(Sender: TBaseVirtualTree; Stream: TStream) of object;
  TVTSaveNodeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream) of object;
  TVTBeforeGetCheckStateEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode) of object;

  // header/column events
  TVTHeaderAddPopupItemEvent = procedure(const Sender: TObject; const Column: TColumnIndex; var Cmd: TAddPopupItemType) of object;
  TVTHeaderClickEvent = procedure(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo) of object;
  TVTHeaderMouseEvent = procedure(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: TDimension) of object;
  TVTHeaderMouseMoveEvent = procedure(Sender: TVTHeader; Shift: TShiftState; X, Y: TDimension) of object;
  TVTBeforeHeaderHeightTrackingEvent = procedure(Sender: TVTHeader; Shift: TShiftState) of object;
  TVTAfterHeaderHeightTrackingEvent = procedure(Sender: TVTHeader) of object;
  TVTHeaderHeightTrackingEvent = procedure(Sender: TVTHeader; var P: TPoint; Shift: TShiftState; var Allowed: Boolean) of object;
  TVTHeaderHeightDblClickResizeEvent = procedure(Sender: TVTHeader; var P: TPoint; Shift: TShiftState; var Allowed: Boolean) of object;
  TVTHeaderNotifyEvent = procedure(Sender: TVTHeader; Column: TColumnIndex) of object;
  TVTHeaderDraggingEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; var Allowed: Boolean) of object;
  TVTHeaderDraggedEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer) of object;
  TVTHeaderDraggedOutEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; DropPosition: TPoint) of object;
  TVTHeaderPaintEvent = procedure(Sender: TVTHeader; HeaderCanvas: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover,
    Pressed: Boolean; DropMark: TVTDropMarkMode) of object;
  TVTHeaderPaintQueryElementsEvent = procedure(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
    var Elements: THeaderPaintElements) of object;
  TVTAdvancedHeaderPaintEvent = procedure(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
    const Elements: THeaderPaintElements) of object;
  TVTBeforeAutoFitColumnsEvent = procedure(Sender: TVTHeader; var SmartAutoFitType: TSmartAutoFitType) of object;
  TVTBeforeAutoFitColumnEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; var SmartAutoFitType: TSmartAutoFitType;
    var Allowed: Boolean) of object;
  TVTAfterAutoFitColumnEvent = procedure(Sender: TVTHeader; Column: TColumnIndex) of object;
  TVTAfterAutoFitColumnsEvent = procedure(Sender: TVTHeader) of object;
  TVTColumnCheckChangingEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; var NewState: TCheckState;
    var Allowed: Boolean) of object;
  TVTColumnClickEvent = procedure (Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState) of object;
  TVTColumnDblClickEvent = procedure (Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState) of object;
  TColumnChangeEvent = procedure(const Sender: TBaseVirtualTree; const Column: TColumnIndex; Visible: Boolean) of object;
  TVTColumnWidthDblClickResizeEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; Shift: TShiftState; P: TPoint;
    var Allowed: Boolean) of object;
  TVTBeforeColumnWidthTrackingEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; Shift: TShiftState) of object;
  TVTAfterColumnWidthTrackingEvent = procedure(Sender: TVTHeader; Column: TColumnIndex) of object;
  TVTColumnWidthTrackingEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; Shift: TShiftState; var TrackPoint: TPoint; P: TPoint;
    var Allowed: Boolean) of object;
  TVTGetHeaderCursorEvent = procedure(Sender: TVTHeader; var Cursor: TVTCursor) of object;
  TVTBeforeGetMaxColumnWidthEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; var UseSmartColumnWidth: Boolean) of object;
  TVTAfterGetMaxColumnWidthEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; var MaxWidth: TDimension) of object;
  TVTCanSplitterResizeColumnEvent = procedure(Sender: TVTHeader; P: TPoint; Column: TColumnIndex; var Allowed: Boolean) of object;
  TVTCanSplitterResizeHeaderEvent = procedure(Sender: TVTHeader; P: TPoint; var Allowed: Boolean) of object;

  // move, copy and node tracking events
  TVTNodeMovedEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode) of object;
  TVTNodeMovingEvent = procedure(Sender: TBaseVirtualTree; Node, Target: PVirtualNode;
    var Allowed: Boolean) of object;
  TVTNodeCopiedEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode) of object;
  TVTNodeCopyingEvent = procedure(Sender: TBaseVirtualTree; Node, Target: PVirtualNode;
    var Allowed: Boolean) of object;
  TVTNodeClickEvent = procedure(Sender: TBaseVirtualTree; const HitInfo: THitInfo) of object;
  TVTNodeHeightTrackingEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; Shift: TShiftState;
    var TrackPoint: TPoint; P: TPoint; var Allowed: Boolean) of object;
  TVTNodeHeightDblClickResizeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    Shift: TShiftState; P: TPoint; var Allowed: Boolean) of object;
  TVTCanSplitterResizeNodeEvent = procedure(Sender: TBaseVirtualTree; P: TPoint; Node: PVirtualNode;
    Column: TColumnIndex; var Allowed: Boolean) of object;

  TVTGetUserClipboardFormatsEvent = procedure(Sender: TBaseVirtualTree; var Formats: TFormatEtcArray) of object;

  // drag'n drop/OLE events
  TVTCreateDragManagerEvent = procedure(Sender: TBaseVirtualTree; out DragManager: IVTDragManager) of object;
  TVTCreateDataObjectEvent = procedure(Sender: TBaseVirtualTree; out IDataObject: TVTDragDataObject) of object;
  TVTDragAllowedEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    var Allowed: Boolean) of object;
  TVTDragOverEvent = procedure(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
    Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean) of object;
  TVTDragDropEvent = procedure(Sender: TBaseVirtualTree; Source: TObject; DataObject: TVTDragDataObject;
    Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode) of object;

  // paint events
  TVTBeforeItemEraseEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
    var ItemColor: TColor; var EraseAction: TItemEraseAction) of object;
  TVTAfterItemEraseEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    ItemRect: TRect) of object;
  TVTBeforeItemPaintEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    ItemRect: TRect; var CustomDraw: Boolean) of object;
  TVTAfterItemPaintEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    ItemRect: TRect) of object;
  TVTBeforeCellPaintEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect) of object;
  TVTAfterCellPaintEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; CellRect: TRect) of object;
  TVTPaintEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas) of object;
  TVTBackgroundPaintEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; R: TRect;
    var Handled: Boolean) of object;
  TVTGetLineStyleEvent = procedure(Sender: TBaseVirtualTree; var Bits: Pointer) of object;
  TVTMeasureItemEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    var NodeHeight: TDimension) of object;
  TVTPaintText = procedure(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType) of object;

  TVTPrepareButtonImagesEvent = procedure(Sender: TBaseVirtualTree; const APlusBM : TBitmap; const APlusHotBM :TBitmap;
                                          const APlusSelectedHotBM :TBitmap; const AMinusBM : TBitmap; const AMinusHotBM : TBitmap;
                                          const AMinusSelectedHotBM :TBitmap; var ASize : TSize) of object;

  TVTColumnHeaderSpanningEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; var Count: Integer) of object;

  // search, sort
  TVTCompareEvent = procedure(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
    var Result: Integer) of object;
  TVTIncrementalSearchEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string;
    var Result: Integer) of object;

  // operations
  TVTOperationEvent = procedure(Sender: TBaseVirtualTree; OperationKind: TVTOperationKind) of object;

  TVTHintKind = (vhkText, vhkOwnerDraw);
  TVTHintKindEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Kind: TVTHintKind) of object;
  TVTDrawHintEvent = procedure(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode; R: TRect; Column: TColumnIndex) of object;
  TVTGetHintSizeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var R: TRect) of object;

  // miscellaneous
  TVTBeforeDrawLineImageEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Level: Integer; var PosX: TDimension) of object;
  TVTGetNodeDataSizeEvent = procedure(Sender: TBaseVirtualTree; var NodeDataSize: Integer) of object;
  TVTKeyActionEvent = procedure(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState;
    var DoDefault: Boolean) of object;
  TVTScrollEvent = procedure(Sender: TBaseVirtualTree; DeltaX, DeltaY: TDimension) of object;
  TVTUpdatingEvent = procedure(Sender: TBaseVirtualTree; State: TVTUpdateState) of object;
  TVTGetCursorEvent = procedure(Sender: TBaseVirtualTree; var Cursor: TCursor) of object;
  TVTStateChangeEvent = procedure(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates) of object;
  TVTGetCellIsEmptyEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    var IsEmpty: Boolean) of object;
  TVTScrollBarShowEvent = procedure(Sender: TBaseVirtualTree; Bar: Integer; Show: Boolean) of object;

  // Helper types for node iterations.
  TGetFirstNodeProc = function: PVirtualNode of object;
  TGetNextNodeProc = function(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode of object;

  TVZVirtualNodeEnumerationMode = (
    vneAll,
    vneChecked,
    vneChild,
    vneCutCopy,
    vneInitialized,
    vneLeaf,
    vneLevel,
    vneNoInit,
    vneSelected,
    vneVisible,
    vneVisibleChild,
    vneVisibleNoInitChild,
    vneVisibleNoInit
  );

  PVTVirtualNodeEnumeration = ^TVTVirtualNodeEnumeration;

  TVTVirtualNodeEnumerator = record
  private
    FNode: PVirtualNode;
    FCanMoveNext: Boolean;
    FEnumeration: PVTVirtualNodeEnumeration;
    function GetCurrent: PVirtualNode; inline;
  public
    function MoveNext: Boolean; inline;
    property Current: PVirtualNode read GetCurrent;
  end;

  TVTVirtualNodeEnumeration = record
  private
    FMode: TVZVirtualNodeEnumerationMode;
    FTree: TBaseVirtualTree;
    // GetNextXxx parameters:
    FConsiderChildrenAbove: Boolean;
    FNode: PVirtualNode;
    FNodeLevel: Cardinal;
    FState: TCheckState;
    FIncludeFiltered: Boolean;
  public
    function GetEnumerator: TVTVirtualNodeEnumerator;
  private
    function GetNext(Node: PVirtualNode): PVirtualNode;
  end;


  // ----- TBaseVirtualTree
  TBaseVirtualTree = class abstract(TVTBaseAncestor)
  private
    FTotalInternalDataSize: Cardinal;            // Cache of the sum of the necessary internal data size for all tree
    FBorderStyle: TBorderStyle;
    FHeader: TVTHeader;
    FRoot: PVirtualNode;
    FDefaultNodeHeight,
    FIndent: TDimension;
    FOptions: TCustomVirtualTreeOptions;
    FUpdateCount: Cardinal;                      // update stopper, updates of the tree control are only done if = 0
    FSynchUpdateCount: Cardinal;                 // synchronizer, causes all events which are usually done via timers
                                                 // to happen immediately, regardless of the normal update state
    FNodeDataSize: Integer;                      // number of bytes to allocate with each node (in addition to its base
                                                 // structure and the internal data), if -1 then do callback
    FStates: TVirtualTreeStates;                 // various active/pending states the tree needs to consider
    FLastSelected,
    FFocusedNode: PVirtualNode;
    FEditColumn,                                 // column to be edited (focused node)
    FFocusedColumn: TColumnIndex;                // NoColumn if no columns are active otherwise the last hit column of
                                                 // the currently focused node
    FHeightTrackPoint: TPoint;                   // Starting point of a node's height changing operation.
    FHeightTrackNode: PVirtualNode;              // Node which height is being changed.
    FHeightTrackColumn: TColumnIndex;            // Initial column where the height changing operation takes place.
    FScrollDirections: TScrollDirections;        // directions to scroll client area into depending on mouse position
    FLastStructureChangeReason: TChangeReason;   // Used for delayed structure change event.
    FLastStructureChangeNode,                    // dito
    FLastChangedNode,                            // used for delayed change event
    FCurrentHotNode: PVirtualNode;               // Node over which the mouse is hovering.
    FCurrentHotColumn: TColumnIndex;             // Column over which the mouse is hovering.
    FHotNodeButtonHit: Boolean;                  // Indicates wether the mouse is hovering over the hot node's button.
    FLastSelRect,
    FNewSelRect: TRect;                          // used while doing draw selection
    FHotCursor: TCursor;                         // can be set to additionally indicate the current hot node
    FLastHitInfo: THitInfo;                      // The THitInfo of the last mouse-down event.
                                                 // in Win98 (slide) and Windows 2000 (fade))
    FHintMode: TVTHintMode;                      // determines the kind of the hint window
    FHintData: TVTHintData;                      // used while preparing the hint window
    FChangeDelay: Cardinal;                      // used to delay OnChange event
    FEditDelay: Cardinal;                        // determines time to elapse before a node goes into edit mode
    FPositionCache: TCache;                      // array which stores node references ordered by vertical positions
                                                 // (see also DoValidateCache for more information)
    FVisibleCount: Cardinal;                     // number of currently visible nodes
    FStartIndex: Cardinal;                       // index to start validating cache from
    FSelection: TNodeArray;                      // list of currently selected nodes
    FSelectionLocked: Boolean;                   // prevents the tree from changing the selection
    FRangeAnchor: PVirtualNode;                  // anchor node for selection with the keyboard, determines start of a
                                                 // selection range
    FCheckPropagationCount: Cardinal;            // nesting level of check propagation (WL, 05.02.2004)
    FLastSelectionLevel: Integer;                // keeps the last node level for constrained multiselection
    FDrawSelShiftState: TShiftState;             // keeps the initial shift state when the user starts selection with
                                                 // the mouse
    FEditLink: IVTEditLink;                      // used to comunicate with an application defined editor
    FTempNodeCache: TNodeArray;                  // used at various places to hold temporarily a bunch of node refs.
    FTempNodeCount: Cardinal;                    // number of nodes in FTempNodeCache
    FBackground: TVTBackground;                  // A background image loadable at design and runtime.
    FBackgroundImageTransparent: Boolean;        // By default, this is off. When switched on, will try to draw the image
                                                 // transparent by using the color of the component as transparent color

    FMargin: TDimension;                         // horizontal distance to border and columns
    FTextMargin: TDimension;                     // space between the node's text and its horizontal bounds
    FBackgroundOffsetX,
    FBackgroundOffsetY: TDimension;              // used to fine tune the position of the background image
    FAnimationDuration: Cardinal;                // specifies how long an animation shall take (expanding, hint)
    FWantTabs: Boolean;                          // If True then the tree also consumes the tab key.
    FNodeAlignment: TVTNodeAlignment;            // determines how to interpret the align member of a node
    FHeaderRect: TRect;                          // Space which the header currently uses in the control (window coords).
    FLastHintRect: TRect;                        // Area which the mouse must leave to reshow a hint.
    FUpdateRect: TRect;
    FEmptyListMessage: string;            // Optional message that will be displayed if no nodes exist in the control.

    // paint support and images
    FPlusBM,
    FMinusBM,                                    // small bitmaps used for tree buttons
    FHotPlusBM,
    FHotMinusBM,
    FSelectedHotPlusBM,
    FSelectedHotMinusBM: TBitmap;                // small bitmaps used for hot tree buttons
    FImages,                                     // normal images in the tree
    FStateImages,                                // state images in the tree
    FCustomCheckImages: TCustomImageList;        // application defined check images
    FCheckImageKind: TCheckImageKind;            // light or dark, cross marks or tick marks
    FCheckImages: TCustomImageList;              // Reference to global image list to be used for the check images.
    //TODO: Use this margin for other images as well
    FImagesMargin: TDimension;                   // The margin used left and right of the checkboxes.
    FImageChangeLink,
    FStateChangeLink,
    FCustomCheckChangeLink: TChangeLink;         // connections to the image lists
    FOldFontChange: TNotifyEvent;                // helper method pointer for tracking font changes in the off screen buffer
    FColors: TVTColors;                          // class comprising all customizable colors in the tree
    FButtonStyle: TVTButtonStyle;                // style of the tree buttons
    FButtonFillMode: TVTButtonFillMode;          // for rectangular tree buttons only: how to fill them
    FLineStyle: TVTLineStyle;                    // style of the tree lines
    FLineMode: TVTLineMode;                      // tree lines or bands etc.
    FSelectionCurveRadius: Cardinal;             // radius for rounded selection rectangles
    FSelectionBlendFactor: Byte;                 // Determines the factor by which the selection rectangle is to be
                                                 // faded if enabled.
    FDrawSelectionMode: TVTDrawSelectionMode;    // determines the paint mode for draw selection

    // alignment and directionality support
    FAlignment: TAlignment;                      // default alignment of the tree if no columns are shown

    // drag'n drop and clipboard support
    FDragImageKind: TVTDragImageKind;            // determines whether or not and what to show in the drag image
    FDragOperations: TDragOperations;            // determines which operations are allowed during drag'n drop
    FDragThreshold: Integer;                     // used to determine when to actually start a drag'n drop operation
    FDragManager: IVTDragManager;                // drag'n drop, cut'n paste
    FDropTargetNode: PVirtualNode;               // node currently selected as drop target
    FLastDropMode: TDropMode;                    // set while dragging and used to track changes
    FDragSelection: TNodeArray;                  // temporary copy of FSelection used during drag'n drop
    FLastDragEffect: Integer;                    // The last executed drag effect
    FDragType: TVTDragType;                      // used to switch between OLE and VCL drag'n drop
    FDragWidth,
    FDragHeight: Integer;                        // size of the drag image, the larger the more CPU power is needed
    FClipboardFormats: TClipboardFormats;        // a list of clipboard format descriptions enabled for this tree
    FLastVCLDragTarget: PVirtualNode;            // A node cache for VCL drag'n drop (keywords: DragLeave on DragDrop).
    FVCLDragEffect: Integer;                     // A cache for VCL drag'n drop to keep the current drop effect.

    // scroll support
    FScrollBarOptions: TScrollBarOptions;        // common properties of horizontal and vertical scrollbar
    FAutoScrollInterval: TAutoScrollInterval;    // determines speed of auto scrolling
    FAutoScrollDelay: Cardinal;                  // amount of milliseconds to wait until autoscrolling becomes active
    FAutoExpandDelay: Cardinal;                  // amount of milliseconds to wait until a node is expanded if it is the
                                                 // drop target
    FOffsetX: TDimension;
    FOffsetY: TDimension;                        // Determines left and top scroll offset.
    FEffectiveOffsetX: TDimension;               // Actual position of the horizontal scroll bar (varies depending on bidi mode).
    FRangeX,
    FRangeY: TNodeHeight;                         // current virtual width and height of the tree
    FBottomSpace: TDimension;                    // Extra space below the last node.

    FDefaultPasteMode: TVTNodeAttachMode;        // Used to determine where to add pasted nodes to.
    FDragScrollStart: Cardinal;                  // Contains the start time when a tree does auto scrolling as drop target.

    // search
    FIncrementalSearch: TVTIncrementalSearch;    // Used to determine whether and how incremental search is to be used.
    FSearchTimeout: Cardinal;                    // Number of milliseconds after which to stop incremental searching.
    FSearchBuffer: string;                 // Collects a sequence of keypresses used to do incremental searching.
    FLastSearchNode: PVirtualNode;               // Reference to node which was last found as search fit.
    FSearchDirection: TVTSearchDirection;        // Direction to incrementally search the tree.
    FSearchStart: TVTSearchStart;                // Where to start iteration on each key press.

    // miscellanous
    FPanningWindow: TForm;                       // Helper window for wheel panning
    FPanningCursor: TVTCursor;                   // Current wheel panning cursor.
    FLastClickPos: TPoint;                       // Used for retained drag start and wheel mouse scrolling.
    FOperationCount: Cardinal;                   // Counts how many nested long-running operations are in progress.
    FOperationCanceled: Boolean;                 // Used to indicate that a long-running operation should be canceled.
    FChangingTheme: Boolean;                     // Used to indicate that a theme change is goi ng on
    FNextNodeToSelect: PVirtualNode;             // Next tree node that we would like to select if the current one gets deleted or looses selection for other reasons.
    FPendingSyncProcs:Integer;                   // Counter that indicates whether we have queued anonymous calls to the min thread, see issue #1199

    // export
    FOnBeforeNodeExport: TVTNodeExportEvent;     // called before exporting a node
    FOnNodeExport: TVTNodeExportEvent;
    FOnAfterNodeExport: TVTNodeExportEvent;      // called after exporting a node
    FOnBeforeColumnExport: TVTColumnExportEvent; // called before exporting a column
    FOnColumnExport: TVTColumnExportEvent;
    FOnAfterColumnExport: TVTColumnExportEvent;  // called after  exporting a column
    FOnBeforeTreeExport: TVTTreeExportEvent;     // called before starting the export
    FOnAfterTreeExport: TVTTreeExportEvent;      // called after finishing the export
    FOnBeforeHeaderExport: TVTTreeExportEvent;   // called before exporting the header
    FOnAfterHeaderExport: TVTTreeExportEvent;    // called after exporting the header

    // common events
    FOnChange: TVTChangeEvent;                   // selection change
    FOnStructureChange: TVTStructureChangeEvent; // structural change like adding nodes etc.
    FOnInitChildren: TVTInitChildrenEvent;       // called when a node's children are needed (expanding etc.)
    FOnInitNode: TVTInitNodeEvent;               // called when a node needs to be initialized (child count etc.)
    FOnFreeNode: TVTFreeNodeEvent;               // called when a node is about to be destroyed, user data can and should
                                                 // be freed in this event
    FOnGetImage: TVTGetImageEvent;               // Used to retrieve the image index of a given node.
    FOnGetImageEx: TVTGetImageExEvent;           // Used to retrieve the image index of a given node along with a custom
                                                 // image list.
    FOnGetImageText: TVTGetImageTextEvent;       // Used to retrieve the image alternative text of a given node.
                                                 // Used by the accessibility interface to provide useful text for status images.
    FOnHotChange: TVTHotNodeChangeEvent;         // called when the current "hot" node (that is, the node under the mouse)
                                                 // changes and hot tracking is enabled
    FOnExpanding,                                // called just before a node is expanded
    FOnCollapsing: TVTChangingEvent;             // called just before a node is collapsed
    FOnChecking: TVTCheckChangingEvent;          // called just before a node's check state is changed
    FOnExpanded,                                 // called after a node has been expanded
    FOnCollapsed,                                // called after a node has been collapsed
    FOnChecked: TVTChangeEvent;                  // called after a node's check state has been changed
    FOnResetNode: TVTChangeEvent;                // called when a node is set to be uninitialized
    FOnNodeMoving: TVTNodeMovingEvent;           // called just before a node is moved from one parent node to another
                                                 // (this can be cancelled)
    FOnNodeMoved: TVTNodeMovedEvent;             // called after a node and its children have been moved to another
                                                 // parent node (probably another tree, but within the same application)
    FOnNodeCopying: TVTNodeCopyingEvent;         // called when a node is copied to another parent node (probably in
                                                 // another tree, but within the same application, can be cancelled)
    FOnNodeClick: TVTNodeClickEvent;             // called when the user clicks on a node
    FOnNodeDblClick: TVTNodeClickEvent;          // called when the user double clicks on a node
    FOnCanSplitterResizeNode: TVTCanSplitterResizeNodeEvent;       // called to query the application wether resizing a node is allowed
    FOnNodeHeightTracking: TVTNodeHeightTrackingEvent;             // called when a node's height is being changed via mouse
    FOnNodeHeightDblClickResize: TVTNodeHeightDblClickResizeEvent; // called when a node's vertical splitter is double clicked
    FOnNodeCopied: TVTNodeCopiedEvent;           // call after a node has been copied
    FOnEditing: TVTEditChangingEvent;            // called just before a node goes into edit mode
    FOnEditCancelled: TVTEditCancelEvent;        // called when editing has been cancelled
    FOnEdited: TVTEditChangeEvent;               // called when editing has successfully been finished
    FOnFocusChanging: TVTFocusChangingEvent;     // called when the focus is about to go to a new node and/or column
                                                 // (can be cancelled)
    FOnFocusChanged: TVTFocusChangeEvent;        // called when the focus goes to a new node and/or column
    FOnAddToSelection: TVTAddToSelectionEvent;           // called when a node is added to the selection
    FOnRemoveFromSelection: TVTRemoveFromSelectionEvent; // called when a node is removed from the selection
    FOnGetPopupMenu: TVTPopupEvent;              // called when the popup for a node or the header needs to be shown
    FOnGetHelpContext: TVTHelpContextEvent;      // called when a node specific help theme should be called
    FOnCreateEditor: TVTCreateEditorEvent;       // called when a node goes into edit mode, this allows applications
                                                 // to supply their own editor
    FOnLoadNode,                                 // called after a node has been loaded from a stream (file, clipboard,
                                                 // OLE drag'n drop) to allow an application to load their own data
                                                 // saved in OnSaveNode
    FOnSaveNode: TVTSaveNodeEvent;               // called when a node needs to be serialized into a stream
                                                 // (see OnLoadNode) to give the application the opportunity to save
                                                 // their node specific, persistent data (note: never save memory
                                                 // references)
    FOnLoadTree,                                 // called after the tree has been loaded from a stream to allow an
                                                 // application to load their own data saved in OnSaveTree
    FOnSaveTree: TVTSaveTreeEvent;               // called after the tree has been saved to a stream to allow an
                                                 // application to save its own data

    // header/column mouse events
    FOnAfterAutoFitColumn: TVTAfterAutoFitColumnEvent;
    FOnAfterAutoFitColumns: TVTAfterAutoFitColumnsEvent;
    FOnBeforeAutoFitColumns: TVTBeforeAutoFitColumnsEvent;
    FOnBeforeAutoFitColumn: TVTBeforeAutoFitColumnEvent;
    FOnHeaderAddPopupItem: TVTHeaderAddPopupItemEvent;
    FOnHeaderClick: TVTHeaderClickEvent;
    FOnHeaderDblClick: TVTHeaderClickEvent;
    FOnAfterHeaderHeightTracking: TVTAfterHeaderHeightTrackingEvent;
    FOnBeforeHeaderHeightTracking: TVTBeforeHeaderHeightTrackingEvent;
    FOnHeaderHeightTracking: TVTHeaderHeightTrackingEvent;
    FOnHeaderHeightDblClickResize: TVTHeaderHeightDblClickResizeEvent;
    FOnHeaderMouseDown,
    FOnHeaderMouseUp: TVTHeaderMouseEvent;
    FOnHeaderMouseMove: TVTHeaderMouseMoveEvent;
    FOnAfterGetMaxColumnWidth: TVTAfterGetMaxColumnWidthEvent;
    FOnBeforeGetMaxColumnWidth: TVTBeforeGetMaxColumnWidthEvent;
    FOnColumnChecked: TVTHeaderNotifyEvent;      // triggered when the column is about to be checked
    FOnColumnChecking: TVTColumnCheckChangingEvent;
    FOnColumnClick: TVTColumnClickEvent;
    FOnColumnDblClick: TVTColumnDblClickEvent;
    FOnColumnResize: TVTHeaderNotifyEvent;
    fOnColumnVisibilityChanged: TColumnChangeEvent;
    FOnColumnWidthDblClickResize: TVTColumnWidthDblClickResizeEvent;
    FOnAfterColumnWidthTracking: TVTAfterColumnWidthTrackingEvent;
    FOnBeforeColumnWidthTracking: TVTBeforeColumnWidthTrackingEvent;
    FOnColumnWidthTracking: TVTColumnWidthTrackingEvent;
    FOnGetHeaderCursor: TVTGetHeaderCursorEvent; // triggered to allow the app. to use customized cursors for the header
    FOnCanSplitterResizeColumn: TVTCanSplitterResizeColumnEvent;
    FOnCanSplitterResizeHeader: TVTCanSplitterResizeHeaderEvent;

    // paint events
    FOnAfterPaint,                               // triggered when the tree has entirely been painted
    FOnBeforePaint: TVTPaintEvent;               // triggered when the tree is about to be painted
    FOnAfterItemPaint: TVTAfterItemPaintEvent;   // triggered after an item has been painted
    FOnBeforeItemPaint: TVTBeforeItemPaintEvent; // triggered when an item is about to be painted
    FOnBeforeItemErase: TVTBeforeItemEraseEvent; // triggered when an item's background is about to be erased
    FOnAfterItemErase: TVTAfterItemEraseEvent;   // triggered after an item's background has been erased
    FOnAfterCellPaint: TVTAfterCellPaintEvent;   // triggered after a column of an item has been painted
    FOnBeforeCellPaint: TVTBeforeCellPaintEvent; // triggered when a column of an item is about to be painted
    FOnHeaderDraw: TVTHeaderPaintEvent;          // Used when owner draw is enabled for the header and a column is set
                                                 // to owner draw mode.
    FOnPrepareButtonImages : TVTPrepareButtonImagesEvent; //allow use to customise plus/minus bitmap images
    FOnHeaderDrawQueryElements: TVTHeaderPaintQueryElementsEvent; // Used for advanced header painting to query the
                                                 // application for the elements, which are drawn by it and which should
                                                 // be drawn by the tree.
    FOnAdvancedHeaderDraw: TVTAdvancedHeaderPaintEvent; // Used when owner draw is enabled for the header and a column
                                                 // is set to owner draw mode. But only if OnHeaderDrawQueryElements
                                                 // returns at least one element to be drawn by the application.
                                                 // In this case OnHeaderDraw is not used.
    FOnGetLineStyle: TVTGetLineStyleEvent;       // triggered when a custom line style is used and the pattern brush
                                                 // needs to be build
    FOnPaintBackground: TVTBackgroundPaintEvent; // triggered if a part of the tree's background must be erased which is
                                                 // not covered by any node
    FOnMeasureItem: TVTMeasureItemEvent;         // Triggered when a node is about to be drawn and its height was not yet
                                                 // determined by the application.
    FOnColumnHeaderSpanning: TVTColumnHeaderSpanningEvent; // triggered before the header column area been create for painting
    FOnGetUserClipboardFormats: TVTGetUserClipboardFormatsEvent; // gives application/descendants the opportunity to
                                                 // add own clipboard formats on the fly
    FOnPaintText: TVTPaintText;                  // triggered before either normal or fixed text is painted to allow
                                                 // even finer customization (kind of sub cell painting)
    // drag'n drop events
    FOnCreateDragManager: TVTCreateDragManagerEvent; // called to allow for app./descendant defined drag managers
    FOnCreateDataObject: TVTCreateDataObjectEvent; // called to allow for app./descendant defined data objects
    FOnDragAllowed: TVTDragAllowedEvent;         // used to get permission for manual drag in mouse down
    FOnDragOver: TVTDragOverEvent;               // called for every mouse move
    FOnDragDrop: TVTDragDropEvent;               // called on release of mouse button (if drop was allowed)
    FOnHeaderDragged: TVTHeaderDraggedEvent;     // header (column) drag'n drop
    FOnHeaderDraggedOut: TVTHeaderDraggedOutEvent; // header (column) drag'n drop, which did not result in a valid drop.
    FOnHeaderDragging: TVTHeaderDraggingEvent;   // header (column) drag'n drop

    // miscellanous events
    FOnGetNodeDataSize: TVTGetNodeDataSizeEvent; // Called if NodeDataSize is -1.
    FOnBeforeDrawLineImage: TVTBeforeDrawLineImageEvent; // Called to allow adjusting the indention of treelines.
    FOnKeyAction: TVTKeyActionEvent;             // Used to selectively prevent key actions (full expand on Ctrl+'+' etc.).
    FOnScroll: TVTScrollEvent;                   // Called when one or both paint offsets changed.
    FOnUpdating: TVTUpdatingEvent;               // Called from BeginUpdate, EndUpdate, BeginSynch and EndSynch.
    FOnGetCursor: TVTGetCursorEvent;             // Called to allow the app. to set individual cursors.
    FOnStateChange: TVTStateChangeEvent;         // Called whenever a state in the tree changes.
    FOnGetCellIsEmpty: TVTGetCellIsEmptyEvent;   // Called when the tree needs to know if a cell is empty.
    FOnShowScrollBar: TVTScrollBarShowEvent;     // Called when a scrollbar is changed in its visibility.
    FOnBeforeGetCheckState: TVTBeforeGetCheckStateEvent; // Called before a CheckState for a Node is obtained.
                                                         // Gives the application a chance to do special processing
                                                         // when a check state is actually required for the first time.

    // search, sort
    FOnCompareNodes: TVTCompareEvent;            // used during sort
    FOnDrawHint: TVTDrawHintEvent;
    FOnGetHintSize: TVTGetHintSizeEvent;
    FOnGetHintKind: TVTHintKindEvent;
    FOnIncrementalSearch: TVTIncrementalSearchEvent; // triggered on every key press (not key down)
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;

    // operations
    FOnStartOperation: TVTOperationEvent;        // Called when an operation starts
    FOnEndOperation: TVTOperationEvent;          // Called when an operation ends

    FVclStyleEnabled: Boolean;
    FSelectionCount: Integer;

    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMParentDoubleBufferedChange(var Message: TMessage); message CM_PARENTDOUBLEBUFFEREDCHANGED;

    procedure AdjustTotalCount(Node: PVirtualNode; Value: Integer; relative: Boolean = False);
    function CalculateCacheEntryCount: Integer;
    procedure CalculateVerticalAlignments(var PaintInfo: TVTPaintInfo; var VButtonAlign: TDimension);
    function ChangeCheckState(Node: PVirtualNode; Value: TCheckState): Boolean;
    function CollectSelectedNodesLTR(MainColumn: Integer; NodeLeft, NodeRight: TDimension; Alignment: TAlignment; OldRect,
      NewRect: TRect): Boolean;
    function CollectSelectedNodesRTL(MainColumn: Integer; NodeLeft, NodeRight: TDimension; Alignment: TAlignment; OldRect,
      NewRect: TRect): Boolean;
    procedure ClearNodeBackground(const PaintInfo: TVTPaintInfo; UseBackground, Floating: Boolean; R: TRect);
    function CompareNodePositions(Node1, Node2: PVirtualNode; ConsiderChildrenAbove: Boolean = False): Integer;
    procedure DrawLineImage(const PaintInfo: TVTPaintInfo; X, Y, H, VAlign: TDimension; Style: TVTLineType; Reverse: Boolean);
    function FindInPositionCache(Node: PVirtualNode; var CurrentPos: TNodeHeight): PVirtualNode; overload;
    function FindInPositionCache(Position: TDimension; var CurrentPos: TNodeHeight): PVirtualNode; overload;
    procedure FixupTotalCount(Node: PVirtualNode);
    procedure FixupTotalHeight(Node: PVirtualNode);
    function GetBottomNode: PVirtualNode;
    function GetCheckState(Node: PVirtualNode): TCheckState;
    function GetCheckType(Node: PVirtualNode): TCheckType;
    function GetChildCount(Node: PVirtualNode): Cardinal;
    function GetChildrenInitialized(Node: PVirtualNode): Boolean; inline;
    function GetCutCopyCount: Integer;
    function GetDisabled(Node: PVirtualNode): Boolean;
    function GetSyncCheckstateWithSelection(Node: PVirtualNode): Boolean;
    function GetDragManager: IVTDragManager;
    function GetExpanded(Node: PVirtualNode): Boolean;
    function GetFiltered(Node: PVirtualNode): Boolean;
    function GetFullyVisible(Node: PVirtualNode): Boolean;
    function GetHasChildren(Node: PVirtualNode): Boolean;
    function GetMultiline(Node: PVirtualNode): Boolean;
    function GetNodeHeight(Node: PVirtualNode): TNodeHeight;
    function GetNodeParent(Node: PVirtualNode): PVirtualNode;
    function GetOffsetXY: TPoint;
    function GetRootNodeCount: Cardinal;
    function GetSelected(Node: PVirtualNode): Boolean;
    function GetTopNode: PVirtualNode;
    function GetTotalCount: Cardinal;
    function GetVerticalAlignment(Node: PVirtualNode): Byte;
    function GetVisible(Node: PVirtualNode): Boolean;
    function GetVisiblePath(Node: PVirtualNode): Boolean;
    function HandleDrawSelection(X, Y: TDimension): Boolean;
    procedure HandleCheckboxClick(pHitNode: PVirtualNode; pKeys: LongInt);
    function HasVisibleNextSibling(Node: PVirtualNode): Boolean;
    function HasVisiblePreviousSibling(Node: PVirtualNode): Boolean;
    procedure ImageListChange(Sender: TObject);
    procedure InitializeFirstColumnValues(var PaintInfo: TVTPaintInfo);
    procedure InitRootNode(OldSize: Cardinal = 0);
    function IsFirstVisibleChild(Parent, Node: PVirtualNode): Boolean;
    function IsLastVisibleChild(Parent, Node: PVirtualNode): Boolean;
    function MakeNewNode: PVirtualNode;
    function PackArray({*}const TheArray: TNodeArray; Count: Integer): Integer;
    procedure FakeReadIdent(Reader: TReader);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAnimationDuration(const Value: Cardinal);
    procedure SetBackground(const Value: TVTBackground);
    procedure SetBackGroundImageTransparent(const Value: Boolean);
    procedure SetBackgroundOffset(const Index: Integer; const Value: TDimension);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetBottomNode(Node: PVirtualNode);
    procedure SetBottomSpace(const Value: TDimension);
    procedure SetButtonFillMode(const Value: TVTButtonFillMode);
    procedure SetButtonStyle(const Value: TVTButtonStyle);
    procedure SetCheckImageKind(Value: TCheckImageKind);
    procedure SetCheckState(Node: PVirtualNode; Value: TCheckState);
    procedure SetCheckType(Node: PVirtualNode; Value: TCheckType);
    procedure SetClipboardFormats(const Value: TClipboardFormats);
    procedure SetColors(const Value: TVTColors);
    procedure SetCustomCheckImages(const Value: TCustomImageList);
    procedure SetDefaultNodeHeight(Value: TDimension);
    procedure SetDisabled(Node: PVirtualNode; Value: Boolean);
    procedure SetEmptyListMessage(const Value: string);
    procedure SetExpanded(Node: PVirtualNode; Value: Boolean);
    procedure SetFocusedColumn(Value: TColumnIndex);
    procedure SetFocusedNode(Value: PVirtualNode);
    procedure SetFullyVisible(Node: PVirtualNode; Value: Boolean);
    procedure SetHasChildren(Node: PVirtualNode; Value: Boolean);
    procedure SetHeader(const Value: TVTHeader);
    procedure SetHotNode(Value: PVirtualNode);
    procedure SetFiltered(Node: PVirtualNode; Value: Boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIndent(Value: TDimension);
    procedure SetLineMode(const Value: TVTLineMode);
    procedure SetLineStyle(const Value: TVTLineStyle);
    procedure SetMargin(Value: TDimension);
    procedure SetMultiline(Node: PVirtualNode; const Value: Boolean);
    procedure SetNodeAlignment(const Value: TVTNodeAlignment);
    procedure SetNodeDataSize(Value: Integer);
    procedure SetNodeHeight(Node: PVirtualNode; Value: TNodeHeight);
    procedure SetNodeParent(Node: PVirtualNode; const Value: PVirtualNode);
    procedure SetOffsetX(const Value: TDimension);
    procedure SetOffsetXY(const Value: TPoint);
    procedure SetOffsetY(const Value: TDimension);
    procedure SetOptions(const Value: TCustomVirtualTreeOptions);
    procedure SetRootNodeCount(Value: Cardinal);
    procedure SetScrollBarOptions(Value: TScrollBarOptions);
    procedure SetSearchOption(const Value: TVTIncrementalSearch);
    procedure SetSelected(Node: PVirtualNode; Value: Boolean);
    procedure SetSelectionCurveRadius(const Value: Cardinal);
    procedure SetStateImages(const Value: TCustomImageList);
    procedure SetTextMargin(Value: TDimension);
    procedure SetTopNode(Node: PVirtualNode);
    procedure SetUpdateState(Updating: Boolean);
    procedure SetVerticalAlignment(Node: PVirtualNode; Value: Byte);
    procedure SetVisible(Node: PVirtualNode; Value: Boolean);
    procedure SetVisiblePath(Node: PVirtualNode; Value: Boolean);
    procedure PrepareBackGroundPicture(Source: TVTBackground; DrawingBitmap: TBitmap; DrawingBitmapWidth: TDimension; DrawingBitmapHeight: TDimension; ABkgcolor: TColor);
    procedure StaticBackground(Source: TVTBackground; Target: TCanvas; OffsetPosition: TPoint; R: TRect; aBkgColor: TColor);
    procedure TileBackground(Source: TVTBackground; Target: TCanvas; Offset: TPoint; R: TRect; aBkgColor: TColor);
    function ToggleCallback(Step, StepSize: Integer; Data: Pointer): Boolean;

    procedure CMColorChange(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMDenySubclassing(var Message: TMessage); message CM_DENYSUBCLASSING;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMHintShowPause(var Message: TCMHintShowPause); message CM_HINTSHOWPAUSE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure TVMGetItem(var Message: TMessage); message TVM_GETITEM;
    procedure TVMGetItemRect(var Message: TMessage); message TVM_GETITEMRECT;
    procedure TVMGetNextItem(var Message: TMessage); message TVM_GETNEXTITEM;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMCopy(var Message: TWMCopy); message WM_COPY;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMEnable(var Message: TWMEnable); message WM_ENABLE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMMButtonUp(var Message: TWMMButtonUp); message WM_MBUTTONUP;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMPrint(var Message: TWMPrint); message WM_PRINT;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    function GetRangeX: TDimension;
    procedure SetDoubleBuffered(const Value: Boolean);
    function GetVclStyleEnabled: Boolean; inline;
    procedure SetOnPrepareButtonImages(const Value: TVTPrepareButtonImagesEvent);
    function IsStored_BackgroundOffsetXY(const Index: Integer): Boolean;
    function IsStored_BottomSpace: Boolean;
    function IsStored_DefaultNodeHeight: Boolean;
    function IsStored_Indent: Boolean;
    function IsStored_Margin: Boolean;
    function IsStored_TextMargin: Boolean;
  protected
    FFontChanged: Boolean;                       // flag for keeping informed about font changes in the off screen buffer   // [IPK] - private to protected
    procedure AutoScale(); virtual;
    procedure AddToSelection(const NewItems: TNodeArray; NewLength: Integer; ForceInsert: Boolean = False); overload; virtual;
    procedure AdjustPaintCellRect(var PaintInfo: TVTPaintInfo; var NextNonEmpty: TColumnIndex); virtual;
    procedure AdjustPanningCursor(X, Y: TDimension); virtual;
    procedure AdjustTotalHeight(Node: PVirtualNode; Value: TNodeHeight; relative: Boolean = False);
    procedure AdviseChangeEvent(StructureChange: Boolean; Node: PVirtualNode; Reason: TChangeReason); virtual;
    function AllocateInternalDataArea(Size: Cardinal): Cardinal; virtual;
    procedure Animate(Steps, Duration: Cardinal; Callback: TVTAnimationCallback; Data: Pointer); virtual;
    function CalculateSelectionRect(X, Y: TDimension): Boolean; virtual;
    function CanAutoScroll: Boolean; virtual;
    function CanShowDragImage: Boolean; virtual;
    function CanSplitterResizeNode(P: TPoint; Node: PVirtualNode; Column: TColumnIndex): Boolean;
    procedure Change(Node: PVirtualNode); virtual;
    procedure ChangeTreeStatesAsync(EnterStates, LeaveStates: TVirtualTreeStates);
    procedure ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend}); override;
    function CheckParentCheckState(Node: PVirtualNode; NewCheckState: TCheckState): Boolean; virtual;
    procedure ClearDragManager;
    procedure ClearSelection(pFireChangeEvent: Boolean); overload; virtual;
    procedure ClearTempCache; virtual;
    function ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual;
    function ComputeRTLOffset(ExcludeScrollBar: Boolean = False): TDimension; virtual;
    function CountLevelDifference(Node1, Node2: PVirtualNode): Integer; virtual;
    function CountVisibleChildren(Node: PVirtualNode): Cardinal; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DecVisibleCount;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DeleteNode(Node: PVirtualNode; Reindex: Boolean; ParentClearing: Boolean); overload;
    function DetermineDropMode(const P: TPoint; var HitInfo: THitInfo; var NodeRect: TRect): TDropMode; virtual;
    procedure DetermineHiddenChildrenFlag(Node: PVirtualNode); virtual;
    procedure DetermineHiddenChildrenFlagAllNodes; virtual;
    procedure DetermineHitPositionLTR(var HitInfo: THitInfo; Offset, Right: TDimension; Alignment: TAlignment); virtual;
    procedure DetermineHitPositionRTL(var HitInfo: THitInfo; Offset, Right: TDimension; Alignment: TAlignment); virtual;
    function DetermineLineImageAndSelectLevel(Node: PVirtualNode; var LineImage: TLineImage): Integer; virtual;
    function DetermineNextCheckState(CheckType: TCheckType; CheckState: TCheckState): TCheckState; virtual;
    function DetermineScrollDirections(X, Y: TDimension): TScrollDirections; virtual;
    procedure DoAddToSelection(Node: PVirtualNode); virtual;
    procedure DoAdvancedHeaderDraw(var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements); virtual;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); virtual;
    procedure DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect); virtual;
    procedure DoAfterItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect); virtual;
    procedure DoAfterPaint(Canvas: TCanvas); virtual;
    procedure DoAutoScroll(X, Y: TDimension); virtual;
    function DoBeforeDrag(Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); virtual;
    procedure DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var Color: TColor;
      var EraseAction: TItemEraseAction); virtual;
    function DoBeforeItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect): Boolean; virtual;
    procedure DoBeforePaint(Canvas: TCanvas); virtual;
    function DoCancelEdit: Boolean; virtual;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); virtual;
    procedure DoCanSplitterResizeNode(P: TPoint; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean); virtual;
    procedure DoChange(Node: PVirtualNode); virtual;
    procedure DoCheckClick(Node: PVirtualNode; NewCheckState: TCheckState); virtual;
    procedure DoChecked(Node: PVirtualNode); virtual;
    function DoChecking(Node: PVirtualNode; var NewCheckState: TCheckState): Boolean; virtual;
    procedure DoCollapsed(Node: PVirtualNode); virtual;
    function DoCollapsing(Node: PVirtualNode): Boolean; virtual;
    procedure DoColumnChecked(Column: TColumnIndex); virtual;
    function DoColumnChecking(Column: TColumnIndex; var NewCheckState: TCheckState): Boolean; virtual;
    procedure DoColumnClick(Column: TColumnIndex; Shift: TShiftState); virtual;
    procedure DoColumnDblClick(Column: TColumnIndex; Shift: TShiftState); virtual;
    procedure DoColumnResize(Column: TColumnIndex); virtual;
    procedure DoColumnVisibilityChanged(const Column: TColumnIndex; Visible: Boolean); virtual;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; virtual;
    function DoCreateDataObject: IDataObject; virtual;
    function DoCreateDragManager: IVTDragManager; virtual;
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink; virtual;
    procedure DoDragging(P: TPoint); virtual;
    procedure DoDragExpand; virtual;
    procedure DoBeforeDrawLineImage(Node: PVirtualNode; Level: Integer; var XPos: TDimension); virtual;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer): Boolean; virtual;
    procedure DoDragDrop(Source: TObject; const DataObject: TVTDragDataObject; const Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
      var Effect: Integer; Mode: TDropMode); virtual;
    procedure DoDrawHint(Canvas: TCanvas; Node: PVirtualNode; R: TRect; Column:
        TColumnIndex);
    procedure DoEdit; virtual;
    procedure DoEndDrag(Target: TObject; X, Y: TDimension); override;
    function DoEndEdit(pCancel: Boolean = False): Boolean; virtual;
    procedure DoEndOperation(OperationKind: TVTOperationKind); virtual;
    procedure DoEnter(); override;
    procedure DoExpanded(Node: PVirtualNode); virtual;
    function DoExpanding(Node: PVirtualNode): Boolean; virtual;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); virtual;
    function DoFocusChanging(OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex): Boolean; virtual;
    procedure DoFocusNode(Node: PVirtualNode; Ask: Boolean); virtual;
    procedure DoFreeNode(Node: PVirtualNode); virtual;
    function DoGetCellContentMargin(Node: PVirtualNode; Column: TColumnIndex;
      CellContentMarginType: TVTCellContentMarginType = ccmtAllSides; Canvas: TCanvas = nil): TPoint; virtual;
    procedure DoGetCursor(var Cursor: TCursor); virtual;
    procedure DoGetHeaderCursor(var Cursor: TVTCursor); virtual;
    procedure DoGetHintSize(Node: PVirtualNode; Column: TColumnIndex; var R:
        TRect); virtual;
    procedure DoGetHintKind(Node: PVirtualNode; Column: TColumnIndex; var Kind:
        TVTHintKind);
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: TImageIndex): TCustomImageList; virtual;
    procedure DoGetImageText(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var ImageText: string); virtual;
    procedure DoGetLineStyle(var Bits: Pointer); virtual;
    function DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): string; virtual;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): string; virtual;
    function DoGetNodeExtraWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): TDimension; virtual;
    function DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): TDimension; virtual;
    function DoGetPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Position: TPoint): TPopupMenu; virtual;
    procedure DoGetUserClipboardFormats(var Formats: TFormatEtcArray); virtual;
    procedure DoHeaderAddPopupItem(const Column: TColumnIndex; var Cmd: TAddPopupItemType);
    procedure DoHeaderClick(const HitInfo: TVTHeaderHitInfo); virtual;
    procedure DoHeaderDblClick(const HitInfo: TVTHeaderHitInfo); virtual;
    procedure DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition); virtual;
    procedure DoHeaderDraggedOut(Column: TColumnIndex; DropPosition: TPoint); virtual;
    function DoHeaderDragging(Column: TColumnIndex): Boolean; virtual;
    procedure DoHeaderDraw(Canvas: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover, Pressed: Boolean;
      DropMark: TVTDropMarkMode); virtual;
    procedure DoHeaderDrawQueryElements(var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements); virtual;
    procedure DoHeaderMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: TDimension); virtual;
    procedure DoHeaderMouseMove(Shift: TShiftState; X, Y: TDimension); virtual;
    procedure DoHeaderMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: TDimension); virtual;
    procedure DoHotChange(Old, New: PVirtualNode); virtual;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; virtual;
    function DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean; virtual;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); virtual;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; virtual;
    procedure DoLoadUserData(Node: PVirtualNode; Stream: TStream); virtual;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension); virtual;
    procedure DoMouseEnter(); override;
    procedure DoMouseLeave(); override;
    procedure DoNodeCopied(Node: PVirtualNode); virtual;
    function DoNodeCopying(Node, NewParent: PVirtualNode): Boolean; virtual;
    procedure DoNodeClick(const HitInfo: THitInfo); virtual;
    procedure DoNodeDblClick(const HitInfo: THitInfo); virtual;
    function DoNodeHeightDblClickResize(Node: PVirtualNode; Column: TColumnIndex; Shift: TShiftState;
      P: TPoint): Boolean; virtual;
    function DoNodeHeightTracking(Node: PVirtualNode; Column: TColumnIndex;  Shift: TShiftState;
      var TrackPoint: TPoint; P: TPoint): Boolean; virtual;
    procedure DoNodeMoved(Node: PVirtualNode); virtual;
    function DoNodeMoving(Node, NewParent: PVirtualNode): Boolean; virtual;
    function DoPaintBackground(Canvas: TCanvas; R: TRect): Boolean; virtual;
    procedure DoPaintDropMark(Canvas: TCanvas; Node: PVirtualNode; R: TRect); virtual;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); virtual;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); virtual;
    procedure DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Position: TPoint); virtual;
    procedure DoRemoveFromSelection(Node: PVirtualNode); virtual;
    procedure DoReset(Node: PVirtualNode); virtual;
    procedure DoSaveUserData(Node: PVirtualNode; Stream: TStream); virtual;
    procedure DoScroll(DeltaX, DeltaY: TDimension); virtual;
    function DoSetOffsetXY(Value: TPoint; Options: TScrollUpdateOptions; ClipRect: PRect = nil): Boolean; virtual;
    procedure DoShowScrollBar(Bar: Integer; Show: Boolean); virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoStartOperation(OperationKind: TVTOperationKind); virtual;
    procedure DoStateChange(Enter: TVirtualTreeStates; Leave: TVirtualTreeStates = []); override;
    procedure DoStructureChange(Node: PVirtualNode; Reason: TChangeReason); virtual;
    procedure DoTimerScroll; virtual;
    procedure DoUpdating(State: TVTUpdateState); virtual;
    procedure DoColumnHeaderSpanning(Column: TColumnIndex; var Count: Integer); virtual;
    function DoValidateCache: Boolean; virtual;
    procedure DragAndDrop(AllowedEffects: DWord; const DataObject: TVTDragDataObject; var DragEffect: Integer); virtual;
    procedure DragCanceled; override;
    function DragDrop(const DataObject: TVTDragDataObject; KeyState: Integer; Pt: TPoint;
      var Effect: Integer): HResult; reintroduce; virtual;
    function DragEnter(KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult; virtual;
    procedure DragFinished; virtual;
    procedure DragLeave; virtual;
    function DragOver(Source: TObject; KeyState: Integer; DragState: TDragState; Pt: TPoint;
      var Effect: Integer): HResult; reintroduce; virtual;
    procedure DrawDottedHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: TDimension); virtual;
    procedure DrawDottedVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: TDimension); virtual;
    procedure DrawGridHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: TDimension); virtual;
    procedure DrawGridVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: TDimension; pFixedColumn: Boolean = False); virtual;
    procedure EndOperation(OperationKind: TVTOperationKind);
    procedure EnsureNodeFocused(); virtual;
    function FindNodeInSelection(P: PVirtualNode; var Index: Integer; LowBound, HighBound: Integer): Boolean; virtual;
    procedure FinishChunkHeader(Stream: TStream; StartPos, EndPos: Integer); virtual;
    procedure FontChanged(AFont: TObject); virtual;
    function GetBorderDimensions: TSize; virtual;
    function GetCheckedCount: Integer;
    function GetCheckImage(Node: PVirtualNode; ImgCheckType: TCheckType = ctNone;
      ImgCheckState: TCheckState = csUncheckedNormal; ImgEnabled: Boolean = True): Integer; virtual;
    function GetColumnClass: TVirtualTreeColumnClass; virtual;
    function GetDefaultHintKind: TVTHintKind; virtual;
    function GetDoubleBuffered: Boolean; {$if CompilerVersion >= 36}override;{$ifend}
    function GetHeaderClass: TVTHeaderClass; virtual;
    function GetHintWindowClass: THintWindowClass; virtual; abstract;
    procedure GetImageIndex(var Info: TVTPaintInfo; Kind: TVTImageKind; InfoIndex: TVTImageInfoIndex); virtual;
    function GetImageSize(Node: PVirtualNode; Kind: TVTImageKind = TVTImageKind.ikNormal; Column: TColumnIndex = 0; IncludePadding: Boolean = True): TSize; virtual;
    function GetNodeImageSize(Node: PVirtualNode): TSize; virtual; deprecated 'Use GetImageSize instead';
    function GetMaxRightExtend: TDimension; virtual;
    procedure GetNativeClipboardFormats(var Formats: TFormatEtcArray); virtual;
    function GetOperationCanceled: Boolean;
    function GetOptionsClass: TTreeOptionsClass; virtual;
    function GetSelectedCount(): Integer; override;
    procedure HandleHotTrack(X, Y: TDimension); virtual;
    procedure HandleIncrementalSearch(CharCode: Word); virtual;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); virtual;
    procedure HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo); virtual;
    procedure HandleMouseUp(var Message: TWMMouse; const HitInfo: THitInfo); virtual;
    procedure HandleClickSelection(LastFocused, NewNode: PVirtualNode; Shift: TShiftState; DragPending: Boolean);
    function HasImage(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex): Boolean; virtual; deprecated 'Use GetImageSize instead';
    function HasPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Pos: TPoint): Boolean; virtual;
    procedure IncVisibleCount;
    procedure InitChildren(Node: PVirtualNode); virtual;
    procedure InitNode(Node: PVirtualNode); virtual;
    procedure InternalAddFromStream(Stream: TStream; Version: Integer; Node: PVirtualNode); virtual;
    function InternalAddToSelection(Node: PVirtualNode; ForceInsert: Boolean): Boolean; overload;
    function InternalAddToSelection(const NewItems: TNodeArray; NewLength: Integer;
      ForceInsert: Boolean): Boolean; overload;
    procedure InternalCacheNode(Node: PVirtualNode); virtual;
    procedure InternalClearSelection; virtual;
    procedure InternalConnectNode(Node, Destination: PVirtualNode; Target: TBaseVirtualTree; Mode: TVTNodeAttachMode); virtual;
    function InternalData(Node: PVirtualNode): Pointer;
    procedure InternalDisconnectNode(Node: PVirtualNode; KeepFocus: Boolean; Reindex: Boolean = True; ParentClearing: Boolean = False); virtual;
    procedure InternalSetFocusedColumn(const index : TColumnIndex);
    procedure InternalRemoveFromSelection(Node: PVirtualNode); virtual;
    procedure InterruptValidation(pWaitForValidationTermination: Boolean = True);
    procedure InvalidateCache;
    function LineWidth(): TDimension;
    procedure Loaded; override;
    procedure MainColumnChanged; virtual;
    procedure MarkCutCopyNodes; override;
    procedure MouseMove(Shift: TShiftState; X, Y: TDimension); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OriginalWMNCPaint(DC: HDC); virtual;
    procedure Paint; override;
    procedure PaintCheckImage(Canvas: TCanvas; const ImageInfo: TVTImageInfo; Selected: Boolean); virtual;
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); virtual;
    procedure PaintNodeButton(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const R: TRect; ButtonX,
      ButtonY: TDimension; BidiMode: TBiDiMode); virtual;
    procedure PaintTreeLines(const PaintInfo: TVTPaintInfo; IndentSize: TDimension; const LineImage: TLineImage); virtual;
    procedure PaintSelectionRectangle(Target: TCanvas; WindowOrgX: TDimension; const SelectionRect: TRect;
      TargetRect: TRect); virtual;
    procedure PrepareBitmaps(NeedButtons, NeedLines: Boolean);
    procedure PrepareCell(var PaintInfo: TVTPaintInfo; WindowOrgX, MaxWidth: TDimension); virtual;
    function ReadChunk(Stream: TStream; Version: Integer; Node: PVirtualNode; ChunkType,
      ChunkSize: Integer): Boolean; virtual;
    procedure ReadNode(Stream: TStream; Version: Integer; Node: PVirtualNode); virtual;
    procedure RedirectFontChangeEvent(Canvas: TCanvas); virtual;
    procedure RemoveFromSelection(Node: PVirtualNode); virtual;
    procedure UpdateNextNodeToSelect(Node: PVirtualNode); virtual;
    procedure ResetRangeAnchor; virtual;
    procedure RestoreFontChangeEvent(Canvas: TCanvas); virtual;
    procedure SelectNodes(StartNode, EndNode: PVirtualNode; AddOnly: Boolean); virtual;
    procedure SetChildCount(Node: PVirtualNode; NewChildCount: Cardinal); virtual;
    procedure SetFocusedNodeAndColumn(Node: PVirtualNode; Column: TColumnIndex); virtual;
    procedure SetRangeX(value: TDimension);
    procedure SetWindowTheme(const Theme: string); override;
    procedure SetVisibleCount(value : Cardinal);
    procedure SkipNode(Stream: TStream); virtual;
    procedure StartOperation(OperationKind: TVTOperationKind);
    procedure StartWheelPanning(Position: TPoint); virtual;
    procedure StopTimer(ID: Integer);
    procedure StopWheelPanning; virtual;
    procedure StructureChange(Node: PVirtualNode; Reason: TChangeReason); virtual;
    function SuggestDropEffect(Source: TObject; Shift: TShiftState; Pt: TPoint; AllowedEffects: Integer): Integer; virtual;
    procedure ToggleSelection(StartNode, EndNode: PVirtualNode); virtual;
    procedure TrySetFocus();
    procedure UnselectNodes(StartNode, EndNode: PVirtualNode); virtual;
    procedure UpdateColumnCheckState(Col: TVirtualTreeColumn);
    procedure UpdateDesigner; virtual;
    procedure UpdateEditBounds; virtual;
    procedure UpdateHeaderRect; virtual;
    procedure UpdateStyleElements; override;
    procedure ValidateCache; virtual;
    procedure ValidateNodeDataSize(var Size: Integer); virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure WriteChunks(Stream: TStream; Node: PVirtualNode); virtual;
    procedure WriteNode(Stream: TStream; Node: PVirtualNode); override;
    class procedure RaiseVTError(const Msg: string; HelpContext: Integer); static;

    procedure VclStyleChanged; virtual;
    property VclStyleEnabled: Boolean read GetVclStyleEnabled;
    property TotalInternalDataSize: Cardinal read FTotalInternalDataSize;
    // Mitigator function to use the correct style service for this context (either the style assigned to the control for Delphi > 10.4 or the application style)
    function StyleServices(AControl: TControl = nil): TCustomStyleServices;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AnimationDuration: Cardinal read FAnimationDuration write SetAnimationDuration default 200;
    property AutoExpandDelay: Cardinal read FAutoExpandDelay write FAutoExpandDelay default 1000;
    property AutoScrollDelay: Cardinal read FAutoScrollDelay write FAutoScrollDelay default 1000;
    property AutoScrollInterval: TAutoScrollInterval read FAutoScrollInterval write FAutoScrollInterval default 1;
    property Background: TVTBackground read FBackground write SetBackground;
    property BackGroundImageTransparent: Boolean read FBackGroundImageTransparent write SetBackGroundImageTransparent default False;
    property BackgroundOffsetX: TDimension index 0 read FBackgroundOffsetX write SetBackgroundOffset stored IsStored_BackgroundOffsetXY; // default 0;
    property BackgroundOffsetY: TDimension index 1 read FBackgroundOffsetY write SetBackgroundOffset stored IsStored_BackgroundOffsetXY; // default 0;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default TFormBorderStyle.bsSingle;
    property BottomSpace: TDimension read FBottomSpace write SetBottomSpace stored IsStored_BottomSpace; //default 0;
    property ButtonFillMode: TVTButtonFillMode read FButtonFillMode write SetButtonFillMode default fmTreeColor;
    property ButtonStyle: TVTButtonStyle read FButtonStyle write SetButtonStyle default bsRectangle;
    property ChangeDelay: Cardinal read FChangeDelay write FChangeDelay default 0;
    property CheckImageKind: TCheckImageKind read FCheckImageKind write SetCheckImageKind stored False default ckSystemDefault; // deprecated, see issue #622
    property ClipboardFormats: TClipboardFormats read FClipboardFormats write SetClipboardFormats;
    property Colors: TVTColors read FColors write SetColors;
    property CustomCheckImages: TCustomImageList read FCustomCheckImages write SetCustomCheckImages;
    property DefaultHintKind: TVTHintKind read GetDefaultHintKind;
    property DefaultNodeHeight: TDimension read FDefaultNodeHeight write SetDefaultNodeHeight stored IsStored_DefaultNodeHeight;
    property DefaultPasteMode: TVTNodeAttachMode read FDefaultPasteMode write FDefaultPasteMode default amAddChildLast;
    property DragHeight: Integer read FDragHeight write FDragHeight default 350;
    property DragImageKind: TVTDragImageKind read FDragImageKind write FDragImageKind default diComplete;
    property DragOperations: TDragOperations read FDragOperations write FDragOperations default [doCopy, doMove];
    property DragSelection: TNodeArray read FDragSelection;
    property LastDragEffect: Integer read FLastDragEffect;
    property DragType: TVTDragType read FDragType write FDragType default dtOLE;
    property DragWidth: Integer read FDragWidth write FDragWidth default 200;
    property DrawSelectionMode: TVTDrawSelectionMode read FDrawSelectionMode write FDrawSelectionMode
      default smDottedRectangle;
    property EditColumn: TColumnIndex read FEditColumn write FEditColumn;
    property EditDelay: Cardinal read FEditDelay write FEditDelay default 1000;
    property EffectiveOffsetX: TDimension read FEffectiveOffsetX;
    property HeaderRect: TRect read FHeaderRect;
    property HintMode: TVTHintMode read FHintMode write FHintMode default hmDefault;
    property HintData: TVTHintData read FHintData write FHintData;
    property HotCursor: TCursor read FHotCursor write FHotCursor default crDefault;
    property Images: TCustomImageList read FImages write SetImages;
    property IncrementalSearch: TVTIncrementalSearch read FIncrementalSearch write SetSearchOption default isNone;
    property IncrementalSearchDirection: TVTSearchDirection read FSearchDirection write FSearchDirection default sdForward;
    property IncrementalSearchStart: TVTSearchStart read FSearchStart write FSearchStart default ssFocusedNode;
    property IncrementalSearchTimeout: Cardinal read FSearchTimeout write FSearchTimeout default 1000;
    property Indent: TDimension read FIndent write SetIndent stored IsStored_Indent; // default 18;
    property LastClickPos: TPoint read FLastClickPos write FLastClickPos;
    property LastDropMode: TDropMode read FLastDropMode write FLastDropMode;
    property LastHintRect: TRect read FLastHintRect write FLastHintRect;
    property LineMode: TVTLineMode read FLineMode write SetLineMode default lmNormal;
    property LineStyle: TVTLineStyle read FLineStyle write SetLineStyle default lsDotted;
    property Margin: TDimension read FMargin write SetMargin stored IsStored_Margin; // default 4;
    property NextNodeToSelect: PVirtualNode read FNextNodeToSelect; // Next tree node that we would like to select if the current one gets deleted
    property NodeAlignment: TVTNodeAlignment read FNodeAlignment write SetNodeAlignment default naProportional;
    property NodeDataSize: Integer read FNodeDataSize write SetNodeDataSize default -1;
    property OperationCanceled: Boolean read GetOperationCanceled;
    property HotMinusBM: TBitmap read FHotMinusBM;
    property HotPlusBM: TBitmap read FHotPlusBM;
    property MinusBM: TBitmap read FMinusBM;
    property PlusBM: TBitmap read FPlusBM;
    property RangeX: TDimension read GetRangeX;// Returns the width of the virtual tree in pixels, (not ClientWidth). If there are columns it returns the total width of all of them; otherwise it returns the maximum of the all the line's data widths.
    property RangeY: TNodeHeight read FRangeY;
    property RootNodeCount: Cardinal read GetRootNodeCount write SetRootNodeCount default 0;
    property ScrollBarOptions: TScrollBarOptions read FScrollBarOptions write SetScrollBarOptions;
    property SelectionBlendFactor: Byte read FSelectionBlendFactor write FSelectionBlendFactor default 128;
    property SelectionCurveRadius: Cardinal read FSelectionCurveRadius write SetSelectionCurveRadius default 0;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property TextMargin: TDimension read FTextMargin write SetTextMargin stored IsStored_TextMargin;
    property TreeOptions: TCustomVirtualTreeOptions read FOptions write SetOptions;
    property WantTabs: Boolean read FWantTabs write FWantTabs default False;
    property SyncCheckstateWithSelection[Node: PVirtualNode]: Boolean read GetSyncCheckstateWithSelection;

    property OnAddToSelection: TVTAddToSelectionEvent read FOnAddToSelection write FOnAddToSelection;
    property OnAdvancedHeaderDraw: TVTAdvancedHeaderPaintEvent read FOnAdvancedHeaderDraw write FOnAdvancedHeaderDraw;
    property OnAfterAutoFitColumn: TVTAfterAutoFitColumnEvent read FOnAfterAutoFitColumn write FOnAfterAutoFitColumn;
    property OnAfterAutoFitColumns: TVTAfterAutoFitColumnsEvent read FOnAfterAutoFitColumns write FOnAfterAutoFitColumns;
    property OnAfterCellPaint: TVTAfterCellPaintEvent read FOnAfterCellPaint write FOnAfterCellPaint;
    property OnAfterColumnExport : TVTColumnExportEvent read FOnAfterColumnExport write FOnAfterColumnExport;
    property OnAfterColumnWidthTracking: TVTAfterColumnWidthTrackingEvent read FOnAfterColumnWidthTracking write FOnAfterColumnWidthTracking;
    property OnAfterGetMaxColumnWidth: TVTAfterGetMaxColumnWidthEvent read FOnAfterGetMaxColumnWidth write FOnAfterGetMaxColumnWidth;
    property OnAfterHeaderExport: TVTTreeExportEvent read FOnAfterHeaderExport write FOnAfterHeaderExport;
    property OnAfterHeaderHeightTracking: TVTAfterHeaderHeightTrackingEvent read FOnAfterHeaderHeightTracking
      write FOnAfterHeaderHeightTracking;
    property OnAfterItemErase: TVTAfterItemEraseEvent read FOnAfterItemErase write FOnAfterItemErase;
    property OnAfterItemPaint: TVTAfterItemPaintEvent read FOnAfterItemPaint write FOnAfterItemPaint;
    property OnAfterNodeExport: TVTNodeExportEvent read FOnAfterNodeExport write FOnAfterNodeExport;
    property OnAfterPaint: TVTPaintEvent read FOnAfterPaint write FOnAfterPaint;
    property OnAfterTreeExport: TVTTreeExportEvent read FOnAfterTreeExport write FOnAfterTreeExport;
    property OnBeforeAutoFitColumn: TVTBeforeAutoFitColumnEvent read FOnBeforeAutoFitColumn write FOnBeforeAutoFitColumn;
    property OnBeforeAutoFitColumns: TVTBeforeAutoFitColumnsEvent read FOnBeforeAutoFitColumns write FOnBeforeAutoFitColumns;
    property OnBeforeCellPaint: TVTBeforeCellPaintEvent read FOnBeforeCellPaint write FOnBeforeCellPaint;
    property OnBeforeColumnExport: TVTColumnExportEvent read FOnBeforeColumnExport write FOnBeforeColumnExport;
    property OnBeforeColumnWidthTracking: TVTBeforeColumnWidthTrackingEvent read FOnBeforeColumnWidthTracking
      write FOnBeforeColumnWidthTracking;
    property OnBeforeDrawTreeLine: TVTBeforeDrawLineImageEvent read FOnBeforeDrawLineImage write FOnBeforeDrawLineImage;
    property OnBeforeGetMaxColumnWidth: TVTBeforeGetMaxColumnWidthEvent read FOnBeforeGetMaxColumnWidth write FOnBeforeGetMaxColumnWidth;
    property OnBeforeHeaderExport: TVTTreeExportEvent read FOnBeforeHeaderExport write FOnBeforeHeaderExport;
    property OnBeforeHeaderHeightTracking: TVTBeforeHeaderHeightTrackingEvent read FOnBeforeHeaderHeightTracking
      write FOnBeforeHeaderHeightTracking;
    property OnBeforeItemErase: TVTBeforeItemEraseEvent read FOnBeforeItemErase write FOnBeforeItemErase;
    property OnBeforeItemPaint: TVTBeforeItemPaintEvent read FOnBeforeItemPaint write FOnBeforeItemPaint;
    property OnBeforeNodeExport: TVTNodeExportEvent read FOnBeforeNodeExport write FOnBeforeNodeExport;
    property OnBeforePaint: TVTPaintEvent read FOnBeforePaint write FOnBeforePaint;
    property OnBeforeTreeExport: TVTTreeExportEvent read FOnBeforeTreeExport write FOnBeforeTreeExport;
    property OnCanSplitterResizeColumn: TVTCanSplitterResizeColumnEvent read FOnCanSplitterResizeColumn write FOnCanSplitterResizeColumn;
    property OnCanSplitterResizeHeader: TVTCanSplitterResizeHeaderEvent read FOnCanSplitterResizeHeader write FOnCanSplitterResizeHeader;
    property OnCanSplitterResizeNode: TVTCanSplitterResizeNodeEvent read FOnCanSplitterResizeNode write FOnCanSplitterResizeNode;
    property OnChange: TVTChangeEvent read FOnChange write FOnChange;
    property OnChecked: TVTChangeEvent read FOnChecked write FOnChecked;
    property OnChecking: TVTCheckChangingEvent read FOnChecking write FOnChecking;
    property OnCollapsed: TVTChangeEvent read FOnCollapsed write FOnCollapsed;
    property OnCollapsing: TVTChangingEvent read FOnCollapsing write FOnCollapsing;
    property OnColumnChecked: TVTHeaderNotifyEvent read FOnColumnChecked write FOnColumnChecked;
    property OnColumnChecking: TVTColumnCheckChangingEvent read FOnColumnChecking write FOnColumnChecking;
    property OnColumnClick: TVTColumnClickEvent read FOnColumnClick write FOnColumnClick;
    property OnColumnDblClick: TVTColumnDblClickEvent read FOnColumnDblClick write FOnColumnDblClick;
    property OnColumnExport : TVTColumnExportEvent read FOnColumnExport write FOnColumnExport;
    property OnColumnResize: TVTHeaderNotifyEvent read FOnColumnResize write FOnColumnResize;
    property OnColumnVisibilityChanged: TColumnChangeEvent read fOnColumnVisibilityChanged write fOnColumnVisibilityChanged;
    property OnColumnWidthDblClickResize: TVTColumnWidthDblClickResizeEvent read FOnColumnWidthDblClickResize
      write FOnColumnWidthDblClickResize;
    property OnColumnWidthTracking: TVTColumnWidthTrackingEvent read FOnColumnWidthTracking write FOnColumnWidthTracking;
    property OnCompareNodes: TVTCompareEvent read FOnCompareNodes write FOnCompareNodes;
    property OnCreateDataObject: TVTCreateDataObjectEvent read FOnCreateDataObject write FOnCreateDataObject;
    property OnCreateDragManager: TVTCreateDragManagerEvent read FOnCreateDragManager write FOnCreateDragManager;
    property OnCreateEditor: TVTCreateEditorEvent read FOnCreateEditor write FOnCreateEditor;
    property OnDragAllowed: TVTDragAllowedEvent read FOnDragAllowed write FOnDragAllowed;
    property OnDragOver: TVTDragOverEvent read FOnDragOver write FOnDragOver;
    property OnDragDrop: TVTDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDrawHint: TVTDrawHintEvent read FOnDrawHint write FOnDrawHint;
    property OnEditCancelled: TVTEditCancelEvent read FOnEditCancelled write FOnEditCancelled;
    property OnEditing: TVTEditChangingEvent read FOnEditing write FOnEditing;
    property OnEdited: TVTEditChangeEvent read FOnEdited write FOnEdited;
    property OnEndOperation: TVTOperationEvent read FOnEndOperation write FOnEndOperation;
    property OnExpanded: TVTChangeEvent read FOnExpanded write FOnExpanded;
    property OnExpanding: TVTChangingEvent read FOnExpanding write FOnExpanding;
    property OnFocusChanged: TVTFocusChangeEvent read FOnFocusChanged write FOnFocusChanged;
    property OnFocusChanging: TVTFocusChangingEvent read FOnFocusChanging write FOnFocusChanging;
    property OnFreeNode: TVTFreeNodeEvent read FOnFreeNode write FOnFreeNode;
    property OnGetCellIsEmpty: TVTGetCellIsEmptyEvent read FOnGetCellIsEmpty write FOnGetCellIsEmpty;
    property OnGetCursor: TVTGetCursorEvent read FOnGetCursor write FOnGetCursor;
    property OnGetHeaderCursor: TVTGetHeaderCursorEvent read FOnGetHeaderCursor write FOnGetHeaderCursor;
    property OnGetHelpContext: TVTHelpContextEvent read FOnGetHelpContext write FOnGetHelpContext;
    property OnGetHintSize: TVTGetHintSizeEvent read FOnGetHintSize write
        FOnGetHintSize;
    property OnGetHintKind: TVTHintKindEvent read FOnGetHintKind write
        FOnGetHintKind;
    property OnGetImageIndex: TVTGetImageEvent read FOnGetImage write FOnGetImage;
    property OnGetImageIndexEx: TVTGetImageExEvent read FOnGetImageEx write FOnGetImageEx;
    property OnGetImageText: TVTGetImageTextEvent read FOnGetImageText write FOnGetImageText;
    property OnGetLineStyle: TVTGetLineStyleEvent read FOnGetLineStyle write FOnGetLineStyle;
    property OnGetNodeDataSize: TVTGetNodeDataSizeEvent read FOnGetNodeDataSize write FOnGetNodeDataSize;
    property OnGetPopupMenu: TVTPopupEvent read FOnGetPopupMenu write FOnGetPopupMenu;
    property OnGetUserClipboardFormats: TVTGetUserClipboardFormatsEvent read FOnGetUserClipboardFormats
      write FOnGetUserClipboardFormats;
    property OnHeaderAddPopupItem: TVTHeaderAddPopupItemEvent read FOnHeaderAddPopupItem write FOnHeaderAddPopupItem;
    property OnHeaderClick: TVTHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderDblClick: TVTHeaderClickEvent read FOnHeaderDblClick write FOnHeaderDblClick;
    property OnHeaderDragged: TVTHeaderDraggedEvent read FOnHeaderDragged write FOnHeaderDragged;
    property OnHeaderDraggedOut: TVTHeaderDraggedOutEvent read FOnHeaderDraggedOut write FOnHeaderDraggedOut;
    property OnHeaderDragging: TVTHeaderDraggingEvent read FOnHeaderDragging write FOnHeaderDragging;
    property OnHeaderDraw: TVTHeaderPaintEvent read FOnHeaderDraw write FOnHeaderDraw;
    property OnHeaderDrawQueryElements: TVTHeaderPaintQueryElementsEvent read FOnHeaderDrawQueryElements
      write FOnHeaderDrawQueryElements;
    property OnHeaderHeightTracking: TVTHeaderHeightTrackingEvent read FOnHeaderHeightTracking
      write FOnHeaderHeightTracking;
    property OnHeaderHeightDblClickResize: TVTHeaderHeightDblClickResizeEvent read FOnHeaderHeightDblClickResize
      write FOnHeaderHeightDblClickResize;
    property OnHeaderMouseDown: TVTHeaderMouseEvent read FOnHeaderMouseDown write FOnHeaderMouseDown;
    property OnHeaderMouseMove: TVTHeaderMouseMoveEvent read FOnHeaderMouseMove write FOnHeaderMouseMove;
    property OnHeaderMouseUp: TVTHeaderMouseEvent read FOnHeaderMouseUp write FOnHeaderMouseUp;
    property OnHotChange: TVTHotNodeChangeEvent read FOnHotChange write FOnHotChange;
    property OnIncrementalSearch: TVTIncrementalSearchEvent read FOnIncrementalSearch write FOnIncrementalSearch;
    property OnInitChildren: TVTInitChildrenEvent read FOnInitChildren write FOnInitChildren;
    property OnInitNode: TVTInitNodeEvent read FOnInitNode write FOnInitNode;
    property OnKeyAction: TVTKeyActionEvent read FOnKeyAction write FOnKeyAction;
    property OnLoadNode: TVTSaveNodeEvent read FOnLoadNode write FOnLoadNode;
    property OnLoadTree: TVTSaveTreeEvent read FOnLoadTree write FOnLoadTree;
    property OnMeasureItem: TVTMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnNodeClick: TVTNodeClickEvent read FOnNodeClick write FOnNodeClick;
    property OnNodeCopied: TVTNodeCopiedEvent read FOnNodeCopied write FOnNodeCopied;
    property OnNodeCopying: TVTNodeCopyingEvent read FOnNodeCopying write FOnNodeCopying;
    property OnNodeDblClick: TVTNodeClickEvent read FOnNodeDblClick write FOnNodeDblClick;
    property OnNodeExport: TVTNodeExportEvent read FOnNodeExport write FOnNodeExport;
    property OnNodeHeightTracking: TVTNodeHeightTrackingEvent read FOnNodeHeightTracking write FOnNodeHeightTracking;
    property OnNodeHeightDblClickResize: TVTNodeHeightDblClickResizeEvent read FOnNodeHeightDblClickResize
      write FOnNodeHeightDblClickResize;
    property OnNodeMoved: TVTNodeMovedEvent read FOnNodeMoved write FOnNodeMoved;
    property OnNodeMoving: TVTNodeMovingEvent read FOnNodeMoving write FOnNodeMoving;
    property OnPaintBackground: TVTBackgroundPaintEvent read FOnPaintBackground write FOnPaintBackground;
    property OnPaintText: TVTPaintText read FOnPaintText write FOnPaintText;
    property OnPrepareButtonBitmaps : TVTPrepareButtonImagesEvent read FOnPrepareButtonImages write SetOnPrepareButtonImages;
    property OnRemoveFromSelection: TVTRemoveFromSelectionEvent read FOnRemoveFromSelection write FOnRemoveFromSelection;
    property OnResetNode: TVTChangeEvent read FOnResetNode write FOnResetNode;
    property OnSaveNode: TVTSaveNodeEvent read FOnSaveNode write FOnSaveNode;
    property OnSaveTree: TVTSaveTreeEvent read FOnSaveTree write FOnSaveTree;
    property OnScroll: TVTScrollEvent read FOnScroll write FOnScroll;
    property OnShowScrollBar: TVTScrollBarShowEvent read FOnShowScrollBar write FOnShowScrollBar;
    property OnBeforeGetCheckState: TVTBeforeGetCheckStateEvent read FOnBeforeGetCheckState write FOnBeforeGetCheckState;
    property OnStartOperation: TVTOperationEvent read FOnStartOperation write FOnStartOperation;
    property OnStateChange: TVTStateChangeEvent read FOnStateChange write FOnStateChange;
    property OnStructureChange: TVTStructureChangeEvent read FOnStructureChange write FOnStructureChange;
    property OnUpdating: TVTUpdatingEvent read FOnUpdating write FOnUpdating;
    property OnColumnHeaderSpanning: TVTColumnHeaderSpanningEvent read FOnColumnHeaderSpanning write FOnColumnHeaderSpanning;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AbsoluteIndex(Node: PVirtualNode): Cardinal;
    function AddChild(Parent: PVirtualNode; UserData: Pointer = nil): PVirtualNode; overload; virtual;
    function AddChild(Parent: PVirtualNode; const UserData: IInterface): PVirtualNode; overload;
    function AddChild(Parent: PVirtualNode; const UserData: TObject): PVirtualNode; overload;
    procedure AddFromStream(Stream: TStream; TargetNode: PVirtualNode);
    procedure AddToSelection(Node: PVirtualNode; NotifySynced: Boolean); overload; virtual;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginDrag(Immediate: Boolean; Threshold: Integer = -1);
    procedure BeginSynch;
    procedure BeginUpdate; virtual;
    procedure CancelCutOrCopy;
    function CancelEditNode: Boolean;
    procedure CancelOperation;
    function CanEdit(Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual;
    function CanFocus: Boolean; override;
    procedure Clear; virtual;
    procedure ClearChecked;
    procedure ClearSelection(); overload; inline;
    function CopyTo(Source: PVirtualNode; Tree: TBaseVirtualTree; Mode: TVTNodeAttachMode;
      ChildrenOnly: Boolean): PVirtualNode; overload;
    function CopyTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode;
      ChildrenOnly: Boolean): PVirtualNode; overload;
    procedure CutToClipboard(); override;
    procedure DeleteChildren(Node: PVirtualNode; ResetHasChildren: Boolean = False);
    procedure DeleteNode(Node: PVirtualNode; pReIndex: Boolean = True); overload; inline;
    procedure DeleteNodes(const pNodes: TNodeArray);
    procedure DeleteSelectedNodes; virtual;
    function Dragging: Boolean;
    procedure DrawGridLine(Canvas: TCanvas; R: TRect); virtual;
    function EditNode(Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual;
    function EndEditNode: Boolean;
    procedure EndSynch;
    procedure EndUpdate; virtual;
    procedure EnsureNodeSelected(pAfterDeletion: Boolean); virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure FinishCutOrCopy;
    procedure FlushClipboard;
    procedure FullCollapse(Node: PVirtualNode = nil);  virtual;
    procedure FullExpand(Node: PVirtualNode = nil); virtual;
    function GetControlsAlignment: TAlignment; override;
    function GetDisplayRect(Node: PVirtualNode; Column: TColumnIndex; TextOnly: Boolean; Unclipped: Boolean = False;
      ApplyCellContentMargin: Boolean = False): TRect;
    function GetEffectivelyFiltered(Node: PVirtualNode): Boolean;
    function GetEffectivelyVisible(Node: PVirtualNode): Boolean;
    function GetFirst(ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetFirstChecked(State: TCheckState = csCheckedNormal; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetFirstChild(Node: PVirtualNode): PVirtualNode;
    function GetFirstChildNoInit(Node: PVirtualNode): PVirtualNode;
    function GetFirstCutCopy(ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetFirstInitialized(ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetFirstLeaf: PVirtualNode;
    function GetFirstLevel(NodeLevel: Cardinal): PVirtualNode;
    function GetFirstNoInit(ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetFirstSelected(ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetFirstVisible(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = True;
      IncludeFiltered: Boolean = False): PVirtualNode;
    function GetFirstVisibleChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetFirstVisibleChildNoInit(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetFirstVisibleNoInit(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = True;
      IncludeFiltered: Boolean = False): PVirtualNode;
    procedure GetHitTestInfoAt(X, Y: TDimension; Relative: Boolean; var HitInfo: THitInfo; ShiftState: TShiftState=[]); virtual;
    function GetLast(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetLastInitialized(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetLastNoInit(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetLastChild(Node: PVirtualNode): PVirtualNode;
    function GetLastChildNoInit(Node: PVirtualNode): PVirtualNode;
    function GetLastSelected(ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetLastVisible(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = True;
      IncludeFiltered: Boolean = False): PVirtualNode;
    function GetLastVisibleChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetLastVisibleChildNoInit(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetLastVisibleNoInit(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = True;
      IncludeFiltered: Boolean = False): PVirtualNode;
    function GetMaxColumnWidth(Column: TColumnIndex; UseSmartColumnWidth: Boolean = False): TDimension; virtual;
    function GetNext(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetNextChecked(Node: PVirtualNode; State: TCheckState = csCheckedNormal;
      ConsiderChildrenAbove: Boolean = False): PVirtualNode; overload;
    function GetNextChecked(Node: PVirtualNode; ConsiderChildrenAbove: Boolean): PVirtualNode; overload;
    function GetNextCutCopy(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetNextInitialized(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetNextLeaf(Node: PVirtualNode): PVirtualNode;
    function GetNextLevel(Node: PVirtualNode; NodeLevel: Cardinal): PVirtualNode;
    function GetNextNoInit(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetNextSelected(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetNextSibling(Node: PVirtualNode): PVirtualNode;
    function GetNextSiblingNoInit(Node: PVirtualNode): PVirtualNode;
    function GetNextVisible(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = True): PVirtualNode;
    function GetNextVisibleNoInit(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = True): PVirtualNode;
    function GetNextVisibleSibling(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetNextVisibleSiblingNoInit(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetNodeAt(const P: TPoint): PVirtualNode; overload; inline;
    function GetNodeAt(X, Y: TDimension): PVirtualNode; overload;
    function GetNodeAt(X, Y: TDimension; Relative: Boolean; var NodeTop: TDimension): PVirtualNode; overload;
    function GetNodeData(Node: PVirtualNode): Pointer; overload;
    function GetNodeData<T>(pNode: PVirtualNode): T; overload; inline;
    function GetSelectedData<T>(): TArray<T>; overload;
    function GetInterfaceFromNodeData<T:IInterface>(pNode: PVirtualNode): T; overload; inline;
    function GetNodeDataAt<T>(pXCoord: Integer; pYCoord: Integer): T;
    function GetFirstSelectedNodeData<T>(): T;
    function GetNodeLevel(Node: PVirtualNode): Cardinal;
    function GetNodeLevelForSelectConstraint(Node: PVirtualNode): integer;
    function GetOffset(pElement: TVTElement; pNode: PVirtualNode): TDimension;
    procedure GetOffsets(pNode: PVirtualNode; out pOffsets: TVTOffsets; pElement: TVTElement = TVTElement.ofsEndOfClientArea; pColumn: Integer = NoColumn);
    function GetPrevious(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetPreviousChecked(Node: PVirtualNode; State: TCheckState = csCheckedNormal;
      ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetPreviousCutCopy(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetPreviousInitialized(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetPreviousLeaf(Node: PVirtualNode): PVirtualNode;
    function GetPreviousLevel(Node: PVirtualNode; NodeLevel: Cardinal): PVirtualNode;
    function GetPreviousNoInit(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetPreviousSelected(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;
    function GetPreviousSibling(Node: PVirtualNode): PVirtualNode;
    function GetPreviousSiblingNoInit(Node: PVirtualNode): PVirtualNode;
    function GetPreviousVisible(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = True): PVirtualNode;
    function GetPreviousVisibleNoInit(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = True): PVirtualNode;
    function GetPreviousVisibleSibling(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetPreviousVisibleSiblingNoInit(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetSortedCutCopySet(Resolve: Boolean): TNodeArray; override;
    function GetSortedSelection(Resolve: Boolean): TNodeArray; override;
    procedure GetTextInfo(Node: PVirtualNode; Column: TColumnIndex; const AFont: TFont; var R: TRect;
      var Text: string); virtual;
    function GetTreeRect: TRect;
    function GetVisibleParent(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
    function GetTopInvisibleParent(Node: PVirtualNode): PVirtualNode;
    function HasAsParent(Node, PotentialParent: PVirtualNode): Boolean;
    function InsertNode(Node: PVirtualNode; Mode: TVTNodeAttachMode; UserData: Pointer = nil): PVirtualNode;
    procedure InvalidateChildren(Node: PVirtualNode; Recursive: Boolean);
    procedure InvalidateColumn(Column: TColumnIndex);
    function InvalidateNode(Node: PVirtualNode): TRect; virtual;
    procedure InvalidateToBottom(Node: PVirtualNode);
    procedure InvertSelection(VisibleOnly: Boolean);
    function IsEditing: Boolean;
    function IsMouseSelecting: Boolean;
    function IsEmpty: Boolean; inline;
    function IsUpdating(): Boolean;
    function IterateSubtree(StartNode: PVirtualNode; Callback: TVTGetNodeProc; Data: Pointer; Filter: TVirtualNodeStates = [];
      DoInit: Boolean = False; ChildNodesOnly: Boolean = False): PVirtualNode;
    procedure LoadFromFile(const FileName: TFileName); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure MeasureItemHeight(const Canvas: TCanvas; Node: PVirtualNode); virtual;
    procedure MoveTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode; ChildrenOnly: Boolean); overload;
    procedure MoveTo(Node: PVirtualNode; Tree: TBaseVirtualTree; Mode: TVTNodeAttachMode;
      ChildrenOnly: Boolean); overload;
    procedure PaintTree(TargetCanvas: TCanvas; Window: TRect; Target: TPoint; PaintOptions: TVTInternalPaintOptions;
      PixelFormat: TPixelFormat = pfDevice); virtual;
    procedure PrepareDragImage(HotSpot: TPoint; const DataObject: TVTDragDataObject);
    procedure Print(Printer: TPrinter; PrintHeader: Boolean);
    function ProcessDrop(const DataObject: TVTDragDataObject; TargetNode: PVirtualNode; var Effect: Integer; Mode:
      TVTNodeAttachMode): Boolean;
    function ProcessOLEData(Source: TBaseVirtualTree; const DataObject: IDataObject; TargetNode: PVirtualNode;
                            Mode: TVTNodeAttachMode; Optimized: Boolean): Boolean;
    procedure RepaintNode(Node: PVirtualNode);
    procedure ReinitChildren(Node: PVirtualNode; Recursive: Boolean; ForceReinit: Boolean = False); virtual;
    procedure InitRecursive(Node: PVirtualNode; Levels: Cardinal = MaxInt; pVisibleOnly: Boolean = True);
    procedure ReinitNode(Node: PVirtualNode; Recursive: Boolean; ForceReinit: Boolean = False); virtual;
    procedure ResetNode(Node: PVirtualNode); virtual;
    procedure SaveToFile(const FileName: TFileName);
    procedure SaveToStream(Stream: TStream; Node: PVirtualNode = nil); virtual;
    function ScaledPixels(pPixels: TDimension): TDimension;
    procedure ScaleNodeHeights(M, D: TDimension);
    function ScrollIntoView(Node: PVirtualNode; Center: Boolean; Horizontally: Boolean = False): Boolean; overload;
    function ScrollIntoView(Column: TColumnIndex; Center: Boolean; Node: PVirtualNode = nil): Boolean; overload;
    procedure SelectAll(VisibleOnly: Boolean);
    procedure SetCheckStateForAll(aCheckState: TCheckState; pSelectedOnly: Boolean; pExcludeDisabled: Boolean = True);
    procedure SetNodeData(pNode: PVirtualNode; pUserData: Pointer); overload; inline;
    procedure SetNodeData(pNode: PVirtualNode; const pUserData: IInterface); overload; inline;
    procedure SetNodeData<T>(pNode: PVirtualNode; pUserData: T); overload;
    procedure Sort(Node: PVirtualNode; Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean = True); override;
    procedure SortTree(Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean = True); virtual;
    procedure ToggleNode(Node: PVirtualNode);
    procedure UpdateHorizontalRange; virtual;
    procedure UpdateHorizontalScrollBar(DoRepaint: Boolean);
    procedure UpdateRanges;
    procedure UpdateScrollBars(DoRepaint: Boolean); virtual;
    procedure UpdateVerticalRange;
    procedure UpdateVerticalScrollBar(DoRepaint: Boolean);
    function UseRightToLeftReading: Boolean;
    procedure ValidateChildren(Node: PVirtualNode; Recursive: Boolean);
    procedure ValidateNode(Node: PVirtualNode; Recursive: Boolean);

    { Enumerations }
    function Nodes(ConsiderChildrenAbove: Boolean = False): TVTVirtualNodeEnumeration;
    function CheckedNodes(State: TCheckState = csCheckedNormal; ConsiderChildrenAbove: Boolean = False): TVTVirtualNodeEnumeration;
    function ChildNodes(Node: PVirtualNode): TVTVirtualNodeEnumeration;
    function CutCopyNodes(ConsiderChildrenAbove: Boolean = False): TVTVirtualNodeEnumeration;
    function InitializedNodes(ConsiderChildrenAbove: Boolean = False): TVTVirtualNodeEnumeration;
    function LeafNodes: TVTVirtualNodeEnumeration;
    function LevelNodes(NodeLevel: Cardinal): TVTVirtualNodeEnumeration;
    function NoInitNodes(ConsiderChildrenAbove: Boolean = False): TVTVirtualNodeEnumeration;
    function SelectedNodes(ConsiderChildrenAbove: Boolean = False): TVTVirtualNodeEnumeration;
    function VisibleNodes(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = True;
      IncludeFiltered: Boolean = False): TVTVirtualNodeEnumeration;
    function VisibleChildNodes(Node: PVirtualNode; IncludeFiltered: Boolean = False): TVTVirtualNodeEnumeration;
    function VisibleChildNoInitNodes(Node: PVirtualNode; IncludeFiltered: Boolean = False): TVTVirtualNodeEnumeration;
    function VisibleNoInitNodes(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = True;
      IncludeFiltered: Boolean = False): TVTVirtualNodeEnumeration;
    property BottomNode: PVirtualNode read GetBottomNode write SetBottomNode;
    property CheckedCount: Integer read GetCheckedCount;
    property CheckImages: TCustomImageList read FCheckImages;
    property CheckState[Node: PVirtualNode]: TCheckState read GetCheckState write SetCheckState;
    property CheckType[Node: PVirtualNode]: TCheckType read GetCheckType write SetCheckType;
    property ChildCount[Node: PVirtualNode]: Cardinal read GetChildCount write SetChildCount;
    property ChildrenInitialized[Node: PVirtualNode]: Boolean read GetChildrenInitialized;
    property CutCopyCount: Integer read GetCutCopyCount;
    property DragManager: IVTDragManager read GetDragManager;
    property DropTargetNode: PVirtualNode read FDropTargetNode write FDropTargetNode;
    property EditLink: IVTEditLink read FEditLink;
    property EmptyListMessage: string read FEmptyListMessage write SetEmptyListMessage;
    property Expanded[Node: PVirtualNode]: Boolean read GetExpanded write SetExpanded;
    property FocusedColumn: TColumnIndex read FFocusedColumn write SetFocusedColumn default InvalidColumn;
    property FocusedNode: PVirtualNode read FFocusedNode write SetFocusedNode;
    property Font;
    property FullyVisible[Node: PVirtualNode]: Boolean read GetFullyVisible write SetFullyVisible;
    property HasChildren[Node: PVirtualNode]: Boolean read GetHasChildren write SetHasChildren;
    property Header: TVTHeader read FHeader write SetHeader;
    property HotNode: PVirtualNode read FCurrentHotNode write SetHotNode;
    property HotColumn: TColumnIndex read FCurrentHotColumn;
    property IsDisabled[Node: PVirtualNode]: Boolean read GetDisabled write SetDisabled;
    property IsEffectivelyFiltered[Node: PVirtualNode]: Boolean read GetEffectivelyFiltered;
    property IsEffectivelyVisible[Node: PVirtualNode]: Boolean read GetEffectivelyVisible;
    property IsFiltered[Node: PVirtualNode]: Boolean read GetFiltered write SetFiltered;
    property IsVisible[Node: PVirtualNode]: Boolean read GetVisible write SetVisible;
    property MultiLine[Node: PVirtualNode]: Boolean read GetMultiline write SetMultiline;
    property NodeHeight[Node: PVirtualNode]: TNodeHeight read GetNodeHeight write SetNodeHeight;
    property NodeParent[Node: PVirtualNode]: PVirtualNode read GetNodeParent write SetNodeParent;
    property OffsetX: TDimension read FOffsetX write SetOffsetX;
    property OffsetXY: TPoint read GetOffsetXY write SetOffsetXY;
    property OffsetY: TDimension read FOffsetY write SetOffsetY;
    property OperationCount: Cardinal read FOperationCount;
    property RootNode: PVirtualNode read FRoot;
    property SearchBuffer: string read FSearchBuffer;
    property Selected[Node: PVirtualNode]: Boolean read GetSelected write SetSelected;
    property SelectionLocked: Boolean read FSelectionLocked write FSelectionLocked;
    property TotalCount: Cardinal read GetTotalCount;
    property TreeStates: TVirtualTreeStates read FStates write FStates;
    property SelectedCount: Integer read FSelectionCount;
    property TopNode: PVirtualNode read GetTopNode write SetTopNode;
    property VerticalAlignment[Node: PVirtualNode]: Byte read GetVerticalAlignment write SetVerticalAlignment;
    property VisibleCount: Cardinal read FVisibleCount;
    property VisiblePath[Node: PVirtualNode]: Boolean read GetVisiblePath write SetVisiblePath;
    property UpdateCount: Cardinal read FUpdateCount;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default True;
  end;

  TVTDrawNodeEvent = procedure(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo) of object;
  TVTGetCellContentMarginEvent = procedure(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; CellContentMarginType: TVTCellContentMarginType; var CellContentMargin: TPoint) of object;
  TVTGetNodeWidthEvent = procedure(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; var NodeWidth: TDimension) of object;


// utility routines
function TreeFromNode(Node: PVirtualNode): TBaseVirtualTree;


//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Winapi.MMSystem,             // for animation timer (does not include further resources)
  System.Math,
  System.SyncObjs,
  System.StrUtils,
  Clipbrd,
  Vcl.Consts,
  Vcl.ExtCtrls,
  Vcl.AxCtrls,                 // TOLEStream
  Vcl.StdActns,                // for standard action support
  Vcl.GraphUtil,               // accessibility helper class
  VirtualTrees.StyleHooks,
  VirtualTrees.WorkerThread,
  VirtualTrees.ClipBoard,
  VirtualTrees.Utils,
  VirtualTrees.DragnDrop;

resourcestring
  // Localizable strings.
  SEditLinkIsNil = 'Edit link must not be nil.';
  SWrongMoveError = 'Target node cannot be a child node of the node to be moved.';
  SWrongStreamFormat = 'Unable to load tree structure, the format is wrong.';
  SWrongStreamVersion = 'Unable to load tree structure, the version is unknown.';
  SStreamTooSmall = 'Unable to load tree structure, not enough data available.';
  SCorruptStream1 = 'Stream data corrupt. A node''s anchor chunk is missing.';
  SCorruptStream2 = 'Stream data corrupt. Unexpected data after node''s end position.';

const
  ClipboardStates = [tsCopyPending, tsCutPending];
  DefaultScrollUpdateFlags = [suoRepaintHeader, suoRepaintScrollBars, suoScrollClientArea, suoUpdateNCArea];
  TreeNodeSize = (SizeOf(TVirtualNode) + (SizeOf(Pointer) - 1)) and not (SizeOf(Pointer) - 1); // used for node allocation and access to internal data
  /// Default value of the DefaultText property
  MouseButtonDown = [tsLeftButtonDown, tsMiddleButtonDown, tsRightButtonDown];

  // Do not modify the copyright in any way! Usage of this unit is prohibited without the copyright notice
  // in the compiled binary file.
  Copyright: string = 'Virtual Treeview © 1999-2021 Mike Lischke, Joachim Marder';



type
  //These allow us access to protected members in the classes
  TVirtualTreeColumnsCracker = class(TVirtualTreeColumns);
  TVTHeaderCracker = class(TVTHeader);
  TVirtualTreeColumnCracker = class(TVirtualTreeColumn);												 
  TBaseVirtualTreeCracker = class(TBaseVirtualTree);

  // streaming support
  TMagicID = array[0..5] of WideChar;

  // base information about a node
  TBaseChunkBody = packed record
    ChildCount: Cardinal;
    NodeHeight: TDimension;
    States: TVirtualNodeStates;
    Align: Byte;
    CheckState: TCheckState;
    CheckType: TCheckType;
    Reserved: Cardinal;
  end;

  TBaseChunk = packed record
    Header: TChunkHeader;
    Body: TBaseChunkBody;
  end;

  // Toggle animation modes.
  TToggleAnimationMode = (
    tamScrollUp,
    tamScrollDown,
    tamNoScroll
  );

  // Internally used data for animations.
  TToggleAnimationData = record
    Window: HWND;                 // copy of the tree's window handle
    DC: TControlCanvas;           // the DC of the window to erase uncovered parts
    Brush: TBrush;                // the brush to be used to erase uncovered parts
    R1,
    R2: TRect;                    // animation rectangles
    Mode1,
    Mode2: TToggleAnimationMode;  // animation modes
    ScaleFactor: Double;          // the factor between the missing step size when doing two animations
    MissedSteps: Double;
  end;


const
  MagicID: TMagicID = (#$2045, 'V', 'T', WideChar(VTTreeStreamVersion), ' ', #$2046);

var
  gWatcher: TCriticalSection = nil;
  gInitialized: Integer = 0;           // >0 if global structures have been initialized; otherwise 0
  NeedToUnitialize: Boolean = False;   // True if the OLE subsystem could be initialized successfully.

//----------------------------------------------------------------------------------------------------------------------

function TreeFromNode(Node: PVirtualNode): TBaseVirtualTree;

// Returns the tree the node currently belongs to or nil if the node is not attached to a tree.

begin
  Assert(Assigned(Node), 'Node must not be nil.');

  // The root node is marked by having its NextSibling (and PrevSibling) pointing to itself.
  while Assigned(Node) and (Node.NextSibling <> Node) do
    Node := Node.Parent;
  if Assigned(Node) then
    Result := TBaseVirtualTree(Node.Parent)
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure QuickSort(const TheArray: TNodeArray; L, R: Integer);

var
  I, J: Integer;
  P, T: Pointer;

begin
  repeat
    I := L;
    J := R;
    P := TheArray[(L + R) shr 1];
    repeat
      while PAnsiChar(TheArray[I]) < PAnsiChar(P) do
        System.Inc(I);
      while PAnsiChar(TheArray[J]) > PAnsiChar(P) do
        System.Dec(J);
      if I <= J then
      begin
        T := TheArray[I];
        TheArray[I] := TheArray[J];
        TheArray[J] := T;
        System.Inc(I);
        System.Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(TheArray, L, J);
    L := I;
  until I >= R;
end;

//----------------- TVTVirtualNodeEnumerator ---------------------------------------------------------------------------

function TVTVirtualNodeEnumerator.GetCurrent: PVirtualNode;

begin
  Result := FNode;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTVirtualNodeEnumerator.MoveNext: Boolean;

begin
  Result := FCanMoveNext;
  if Result then
  begin
    FNode := FEnumeration.GetNext(FNode);
    Result := FNode <> nil;
    FCanMoveNext := Result;
  end;
end;

//----------------- TVTVirtualNodeEnumeration --------------------------------------------------------------------------

function TVTVirtualNodeEnumeration.GetEnumerator: TVTVirtualNodeEnumerator;

begin
  Result.FNode := nil;
  Result.FCanMoveNext := True;
  Result.FEnumeration := @Self;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTVirtualNodeEnumeration.GetNext(Node: PVirtualNode): PVirtualNode;
begin
  case FMode of
    vneAll:
      if Node = nil then
        Result := FTree.GetFirst(FConsiderChildrenAbove)
      else
        Result := FTree.GetNext(Node, FConsiderChildrenAbove);

    vneChecked:
      if Node = nil then
        Result := FTree.GetFirstChecked(FState, FConsiderChildrenAbove)
      else
        Result := FTree.GetNextChecked(Node, FState, FConsiderChildrenAbove);

    vneChild:
      if Node = nil then
        Result := FTree.GetFirstChild(FNode)
      else
        Result := FTree.GetNextSibling(Node);

    vneCutCopy:
      if Node = nil then
        Result := FTree.GetFirstCutCopy(FConsiderChildrenAbove)
      else
        Result := FTree.GetNextCutCopy(Node, FConsiderChildrenAbove);

    vneInitialized:
      if Node = nil then
        Result := FTree.GetFirstInitialized(FConsiderChildrenAbove)
      else
        Result := FTree.GetNextInitialized(Node, FConsiderChildrenAbove);

    vneLeaf:
      if Node = nil then
        Result := FTree.GetFirstLeaf
      else
        Result := FTree.GetNextLeaf(Node);

    vneLevel:
      if Node = nil then
        Result := FTree.GetFirstLevel(FNodeLevel)
      else
        Result := FTree.GetNextLevel(Node, FNodeLevel);

    vneNoInit:
      if Node = nil then
        Result := FTree.GetFirstNoInit(FConsiderChildrenAbove)
      else
        Result := FTree.GetNextNoInit(Node, FConsiderChildrenAbove);

    vneSelected:
      if Node = nil then
        Result := FTree.GetFirstSelected(FConsiderChildrenAbove)
      else
        Result := FTree.GetNextSelected(Node, FConsiderChildrenAbove);

    vneVisible:
      begin
        if Node = nil then
        begin
          Result := FTree.GetFirstVisible(FNode, FConsiderChildrenAbove, FIncludeFiltered);
          if FIncludeFiltered or not FTree.IsEffectivelyFiltered[Result] then
            Exit;
        end;
        repeat
          Result := FTree.GetNextVisible(Node{, FConsiderChildrenAbove});
        until not Assigned(Result) or FIncludeFiltered or not FTree.IsEffectivelyFiltered[Result];
      end;

    vneVisibleChild:
      if Node = nil then
        Result := FTree.GetFirstVisibleChild(FNode, FIncludeFiltered)
      else
        Result := FTree.GetNextVisibleSibling(Node, FIncludeFiltered);

    vneVisibleNoInitChild:
      if Node = nil then
        Result := FTree.GetFirstVisibleChildNoInit(FNode, FIncludeFiltered)
      else
        Result := FTree.GetNextVisibleSiblingNoInit(Node, FIncludeFiltered);

    vneVisibleNoInit:
      begin
        if Node = nil then
        begin
          Result := FTree.GetFirstVisibleNoInit(FNode, FConsiderChildrenAbove, FIncludeFiltered);
          if FIncludeFiltered or not FTree.IsEffectivelyFiltered[Result] then
            Exit;
        end;
        repeat
          Result := FTree.GetNextVisibleNoInit(Node, FConsiderChildrenAbove);
        until not Assigned(Result) or FIncludeFiltered or not FTree.IsEffectivelyFiltered[Result];
      end;
  else
    Result := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure InitializeGlobalStructures();

// initialization of stuff global to the unit
begin
  if (gInitialized > 0) or (AtomicIncrement(gInitialized) <> 1) then // Ensure threadsafe that this code is executed only once
    exit;

  // This watcher is used whenever a global structure could be modified by more than one thread.
  gWatcher := TCriticalSection.Create();

  // Initialize OLE subsystem for drag'n drop and clipboard operations.
  NeedToUnitialize := not IsLibrary and Succeeded(OleInitialize(nil));

  // Register the tree reference clipboard format.
  CF_VTREFERENCE := RegisterClipboardFormat(CFSTR_VTREFERENCE);
  CF_VTHEADERREFERENCE := RegisterClipboardFormat(CFSTR_VTHEADERREFERENCE);

  // Clipboard format registration.
  // Native clipboard format. Needs a new identifier and has an average priority to allow other formats to take over.
  // This format is supposed to use the IStream storage format but unfortunately this does not work when
  // OLEFlushClipboard is used. Hence it is disabled until somebody finds a solution.
  CF_VIRTUALTREE := RegisterVTClipboardFormat(CFSTR_VIRTUALTREE, TBaseVirtualTree, 50, TYMED_HGLOBAL {or TYMED_ISTREAM});
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FinalizeGlobalStructures();

var
  HintWasEnabled: Boolean;

begin
  if gInitialized = 0 then
    exit; // Was not initialized

  if NeedToUnitialize then
    OleUninitialize;

  // If VT is used in a package and its special hint window was used then the last instance of this
  // window is not freed correctly (bug in the VCL). We explicitely tell the application to free it
  // otherwise an AV is raised due to access to an invalid memory area.
  if ModuleIsPackage then
  begin
    HintWasEnabled := Application.ShowHint;
    Application.ShowHint := False;
    if HintWasEnabled then
      Application.ShowHint := True;
  end;
  gWatcher.Free;
  gWatcher := nil;
end;

//----------------- TClipboardFormats ----------------------------------------------------------------------------------

constructor TClipboardFormats.Create(AOwner: TBaseVirtualTree);

begin
  FOwner := AOwner;
  Sorted := True;
  Duplicates := dupIgnore;
end;

//----------------------------------------------------------------------------------------------------------------------

function TClipboardFormats.Add(const S: string): Integer;

// Restrict additions to the clipbard formats to only those which are registered with the owner tree or one of its
// ancestors.

var
  Format: Word;
  RegisteredClass: TVirtualTreeClass;

begin
  RegisteredClass := TClipboardFormatList.FindFormat(S, Format);
  if Assigned(RegisteredClass) and FOwner.ClassType.InheritsFrom(RegisteredClass) then
    Result := inherited Add(S)
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TClipboardFormats.Insert(Index: Integer; const S: string);

// Restrict additions to the clipbard formats to only those which are registered with the owner tree or one of its
// ancestors.

var
  Format: Word;
  RegisteredClass: TVirtualTreeClass;

begin
  RegisteredClass := TClipboardFormatList.FindFormat(S, Format);
  if Assigned(RegisteredClass) and FOwner.ClassType.InheritsFrom(RegisteredClass) then
    inherited Insert(Index, S);
end;

//----------------- TBaseVirtualTree -----------------------------------------------------------------------------------

constructor TBaseVirtualTree.Create(AOwner: TComponent);

begin
  InitializeGlobalStructures();

  inherited;

  ControlStyle := ControlStyle - [csSetCaption] + [csCaptureMouse, csOpaque, csReplicatable, csDisplayDragImage,
    csReflector];
  FTotalInternalDataSize := 0;
  FNodeDataSize := -1;
  Width := 200;
  Height := 100;
  TabStop := True;
  ParentColor := False;
  FDefaultNodeHeight := cInitialDefaultNodeHeight;
  FDragOperations := [doCopy, doMove];
  FHotCursor := crDefault;
  FScrollBarOptions := TScrollBarOptions.Create(Self);
  FFocusedColumn := NoColumn;
  FDragImageKind := diComplete;
  FLastSelectionLevel := -1;
  FSelectionBlendFactor := 128;

  FIndent := 18;

  FPlusBM := TBitmap.Create;
  FHotPlusBM := TBitmap.Create;
  FMinusBM := TBitmap.Create;
  FHotMinusBM := TBitmap.Create;
  FSelectedHotPlusBM := TBitmap.Create;
  FSelectedHotMinusBM := TBitmap.Create;

  FBorderStyle := TFormBorderStyle.bsSingle;
  FButtonStyle := bsRectangle;
  FButtonFillMode := fmTreeColor;

  FHeader := GetHeaderClass.Create(Self);

  // we have an own double buffer handling
  inherited DoubleBuffered := False;

  FCheckImageKind := ckSystemDefault;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FStateChangeLink := TChangeLink.Create;
  FStateChangeLink.OnChange := ImageListChange;
  FCustomCheckChangeLink := TChangeLink.Create;
  FCustomCheckChangeLink.OnChange := ImageListChange;

  FAutoExpandDelay := 1000;
  FAutoScrollDelay := 1000;
  FAutoScrollInterval := 1;

  FBackground := TVTBackground.Create;
  // Similar to the Transparent property of TImage,
  // this flag is Off by default.
  FBackGroundImageTransparent := False;

  FDefaultPasteMode := amAddChildLast;
  FMargin := 4;
  FTextMargin := cDefaultTextMargin;
  FImagesMargin := 2;
  FLastDragEffect := DROPEFFECT_NONE;
  FDragType := dtOLE;
  FDragHeight := 350;
  FDragWidth := 200;

  FColors := TVTColors.Create(Self);
  FEditDelay := 1000;

  FAnimationDuration := 200;
  FSearchTimeout := 1000;
  FSearchStart := ssFocusedNode;
  FNodeAlignment := naProportional;
  FLineStyle := lsDotted;
  FIncrementalSearch := isNone;
  FClipboardFormats := TClipboardFormats.Create(Self);
  FOptions := GetOptionsClass.Create(Self);

  Touch.InteractiveGestures := [igPan, igPressAndTap];
  Touch.InteractiveGestureOptions := [igoPanInertia,
    igoPanSingleFingerHorizontal, igoPanSingleFingerVertical,
    igoPanGutter, igoParentPassthrough];

  if not (csDesigning in ComponentState) then //Don't create worker thread in IDE, there is no use for it
    TWorkerThread.AddThreadReference();
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TBaseVirtualTree.Destroy();
var
  WasValidating: Boolean;
begin
  WasValidating := (tsValidating in FStates) or (tsValidationNeeded in FStates); // Checking tsValidating is not enough, the TWorkerThread may be stuck in the first call to ChangeTreeStatesAsync()
  InterruptValidation(True);
  if WasValidating then
  begin
    // Make sure we dequeue the two synchronized calls from ChangeTreeStatesAsync(), fixes mem leak and AV reported in issue #1001, but is more a workaround.
    while CheckSynchronize() and (FPendingSyncProcs>0) do
      Sleep(1);
  end;// if
  FOptions.InternalSetMiscOptions(FOptions.MiscOptions - [toReadOnly]); //SetMiscOptions has side effects
  // Make sure there is no reference remaining to the releasing tree.
  TWorkerThread.ReleaseThreadReference(IsLibrary); // see issue #1245
  StopWheelPanning;
  CancelEditNode;

  // Just in case it didn't happen already release the edit link.
  FEditLink := nil;
  FClipboardFormats.Free;
  // Clear will also free the drag manager if it is still alive.
  Clear;
  FColors.Free;
  FBackground.Free;

  if CheckImageKind = ckSystemDefault then
    FCheckImages.Free;
  FScrollBarOptions.Free;

  // The window handle must be destroyed before the header is freed because it is needed in WM_NCDESTROY.
  if HandleAllocated then
    DestroyWindowHandle;

  // Release FDottedBrush in case WM_NCDESTROY hasn't been triggered.
  if Assigned(DottedBrushTreeLines) then
  begin
    DottedBrushTreeLines.Bitmap.Free();
    DottedBrushTreeLines.Free;
    DottedBrushTreeLines:= nil;
  end;

  FHeader.Free;
  FHeader := nil; // Do not use FreeAndNil() before checking issue #497
  FreeAndNil(FOptions); // WM_NCDESTROY accesses FOptions

  FreeMem(FRoot);

  FPlusBM.Free;
  FHotPlusBM.Free;
  FMinusBM.Free;
  FHotMinusBM.Free;
  FSelectedHotPlusBM.Free;
  FSelectedHotMinusBM.Free;

  // Fixes issue #1002
  Images := nil;
  StateImages := nil;
  CustomCheckImages := nil;

  FImageChangeLink.Free;
  FStateChangeLink.Free;
  FCustomCheckChangeLink.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AdjustTotalCount(Node: PVirtualNode; Value: Integer; Relative: Boolean = False);

// Sets a node's total count to the given value and recursively adjusts the parent's total count
// (actually, the adjustment is done iteratively to avoid function call overheads).

var
  Difference: Integer;
  Run: PVirtualNode;

begin
  if Relative then
    Difference := Value
  else
    Difference := Value - Integer(Node.TotalCount);
  if Difference <> 0 then
  begin
    Run := Node;
    // Root node has as parent the tree view.
    while Assigned(Run) and (Run <> Pointer(Self)) do
    begin
      System.Inc(Integer(Run.TotalCount), Difference);
      Run := Run.Parent;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AdjustTotalHeight(Node: PVirtualNode; Value: TNodeHeight; Relative: Boolean = False);

// Sets a node's total height to the given value and recursively adjusts the parent's total height.

var
  Difference: TNodeHeight;
  Run: PVirtualNode;

begin
  if Relative then
    Difference := Value
  else
    Difference := Value - Node.TotalHeight;
  if Difference <> 0 then
  begin
    Run := Node;
    repeat
      Inc(Run.TotalHeight, Difference);

      // If the node is not visible or the parent node is not expanded or we are already at the top
      // then nothing more remains to do.
      if not (vsVisible in Run.States) or (Run = FRoot) or
        (Run.Parent = nil) or not (vsExpanded in Run.Parent.States) then
        Break;

      Run := Run.Parent;
    until False;
  end;

  UpdateVerticalRange;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CalculateCacheEntryCount: Integer;

// Calculates the size of the position cache.

begin
  if FVisibleCount > 1 then
    Result := Ceil(FVisibleCount / CacheThreshold)
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CalculateVerticalAlignments(var PaintInfo: TVTPaintInfo; var VButtonAlign: TDimension);

// Calculates the vertical alignment of the given node and its associated expand/collapse button during
// a node paint cycle depending on the required node alignment style.

begin
  With PaintInfo do begin
    // For absolute alignment the calculation is trivial.
    case FNodeAlignment of
      naFromTop:
        VAlign := Node.Align;
      naFromBottom:
        VAlign := NodeHeight[Node] - Node.Align;
    else // naProportional
      // Consider button and line alignment, but make sure neither the image nor the button (whichever is taller)
      // go out of the entire node height (100% means bottom alignment to the node's bounds).
      if (ImageInfo[iiNormal].Index >= 0) or (ImageInfo[iiState].Index >= 0) then
      begin
        if (ImageInfo[iiNormal].Index >= 0) then
          VAlign := ImageInfo[iiNormal].Images.Height
        else
          VAlign := ImageInfo[iiState].Images.Height;
        VAlign := MulDiv((NodeHeight[Node] - VAlign), Node.Align, 100) + Divide(VAlign, 2);
      end
      else
        if toShowButtons in FOptions.PaintOptions then
          VAlign := MulDiv((NodeHeight[Node] - FPlusBM.Height), Node.Align, 100) + Divide(FPlusBM.Height, 2)
        else
          VAlign := MulDiv(Node.NodeHeight, Node.Align, 100);
    end;

    VButtonAlign := VAlign - FPlusBM.Height div 2 - (FPlusBM.Height and 1);
  end;// With PaintInfo
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ChangeCheckState(Node: PVirtualNode; Value: TCheckState): Boolean;

// Sets the check state of the node according to the given value and the node's check type.
// If the check state must be propagated to the parent nodes and one of them refuses to change then
// nothing happens and False is returned, otherwise True.

var
  Run: PVirtualNode;
  UncheckedCount,
  MixedCheckCount,
  CheckedCount: Cardinal;

begin
  Result := not (vsChecking in Node.States);
  with Node^ do
  if Result then
  begin
    Include(States, vsChecking);
    try
      if not (vsInitialized in States) then
        InitNode(Node)
      else if CheckState = Value then
      begin
        // Value didn't change and node was initialized, so nothing to do
        Result := False;
        Exit;
      end;//if

      // Indicate that we are going to propagate check states up and down the hierarchy.
      if FCheckPropagationCount = 0 then begin
        // Do not enter tsCheckPropagation more than once
        DoStateChange([tsCheckPropagation]);
        BeginUpdate();
      end;
      System.Inc(FCheckPropagationCount);
      try
        // Do actions which are associated with the given check state.
        case CheckType of
          // Check state change with additional consequences for check states of the children.
          ctTriStateCheckBox:
            begin
              // Propagate state down to the children.
              if toAutoTristateTracking in FOptions.AutoOptions then
                case Value of
                  csUncheckedNormal:
                    if Node.ChildCount > 0 then
                    begin
                      Run := FirstChild;
                      CheckedCount := 0;
                      MixedCheckCount := 0;
                      UncheckedCount := 0;
                      while Assigned(Run) do
                      begin
                        if Run.CheckType in [ctCheckBox, ctTriStateCheckBox] then
                        begin
                          if not Self.GetCheckState(Run).IsDisabled() then
                            SetCheckState(Run, csUncheckedNormal);
                          // Check if the new child state was set successfully, otherwise we have to adjust the
                          // node's new check state accordingly.
                          case Self.GetCheckState(Run) of
                            csCheckedNormal, csCheckedDisabled:
                              System.Inc(CheckedCount);
                            csMixedNormal:
                              System.Inc(MixedCheckCount);
                            csUncheckedNormal, csUncheckedDisabled:
                              System.Inc(UncheckedCount);
                          end;
                        end;
                        Run := Run.NextSibling;
                      end;

                      // If there is still a mixed state child node checkbox then this node must be mixed checked too.
                      if MixedCheckCount > 0 then
                        Value := csMixedNormal
                      else
                        // If nodes are normally checked child nodes then the unchecked count determines what
                        // to set for the node itself.
                        if CheckedCount > 0 then
                          if UncheckedCount > 0 then
                            Value := csMixedNormal
                          else
                            Value := csCheckedNormal;
                    end;
                  csCheckedNormal:
                    if Node.ChildCount > 0 then
                    begin
                      Run := FirstChild;
                      CheckedCount := 0;
                      MixedCheckCount := 0;
                      UncheckedCount := 0;
                      while Assigned(Run) do
                      begin
                        if Run.CheckType in [ctCheckBox, ctTriStateCheckBox] then
                        begin
                          if not Self.GetCheckState(Run).IsDisabled() then
                            SetCheckState(Run, csCheckedNormal);
                          // Check if the new child state was set successfully, otherwise we have to adjust the
                          // node's new check state accordingly.
                          case Self.GetCheckState(Run) of
                            csCheckedNormal:
                              System.Inc(CheckedCount);
                            csMixedNormal:
                              System.Inc(MixedCheckCount);
                            csUncheckedNormal:
                              System.Inc(UncheckedCount);
                          end;
                        end;
                        Run := Run.NextSibling;
                      end;

                      // If there is still a mixed state child node checkbox then this node must be mixed checked too.
                      if MixedCheckCount > 0 then
                        Value := csMixedNormal
                      else
                        // If nodes are normally checked child nodes then the unchecked count determines what
                        // to set for the node itself.
                        if CheckedCount > 0 then
                          if UncheckedCount > 0 then
                            Value := csMixedNormal
                          else
                            Value := csCheckedNormal;
                    end;
                end;
            end;
          // radio button check state change
          ctRadioButton:
            if Value = csCheckedNormal then
            begin
              Value := csCheckedNormal;
              // Make sure only this node is checked.
              Run := Parent.FirstChild;
              while Assigned(Run) do
              begin
                if Run.CheckType = ctRadioButton then
                  Run.CheckState := csUncheckedNormal;
                Run := Run.NextSibling;
              end;
              Invalidate;
            end;
        end;

        if Result then
          CheckState := Value // Set new check state
        else
          CheckState := Self.GetCheckState(Node).GetUnpressed(); // Reset dynamic check state.

        // Propagate state up to the parent.
        if not (vsInitialized in Parent.States) then
          InitNode(Parent);
        if (toAutoTristateTracking in FOptions.AutoOptions) and ([vsChecking, vsDisabled] * Parent.States = []) and
          (CheckType in [ctCheckBox, ctTriStateCheckBox]) and (Parent <> FRoot) and
          (Parent.CheckType = ctTriStateCheckBox) then
          Result := CheckParentCheckState(Node, Value)
        else
          Result := True;

        InvalidateNode(Node);
      finally
        System.Dec(FCheckPropagationCount); // WL, 05.02.2004
        if FCheckPropagationCount = 0 then begin
          // Allow state change event after all check operations finished
          DoStateChange([], [tsCheckPropagation]);
          EndUpdate();
        end;
      end;
    finally
      Exclude(States, vsChecking);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CollectSelectedNodesLTR(MainColumn: Integer; NodeLeft, NodeRight: TDimension; Alignment: TAlignment;
  OldRect, NewRect: TRect): Boolean;

// Helper routine used when a draw selection takes place. This version handles left-to-right directionality.
// In the process of adding or removing nodes the current selection is modified which requires to pack it after
// the function returns. Another side effect of this method is that a temporary list of nodes will be created
// (see also InternalCacheNode) which must be inserted into the current selection by the caller.

var
  Run,
  NextNode: PVirtualNode;
  TextRight,
  TextLeft,
  CurrentTop,
  CurrentRight,
  NextTop: TDimension;
  NextColumn,
  Dummy: Integer;
  DummyLeft: TDimension;

  MinY, MaxY: TDimension;
  LabelOffset: TDimension;
  IsInOldRect,
  IsInNewRect: Boolean;
  NodeWidth: TDimension;

  // quick check variables for various parameters
  DoSwitch,
  AutoSpan: Boolean;
  SimpleSelection: Boolean;

begin
  // A priori nothing changes.
  Result := False;

  // Determine minimum and maximum vertical coordinates to limit iteration to.
  MinY := Min(OldRect.Top, NewRect.Top);
  MaxY := Max(OldRect.Bottom, NewRect.Bottom);

  // Initialize short hand variables to speed up tests below.
  DoSwitch := ssCtrl in FDrawSelShiftState;
  AutoSpan := FHeader.UseColumns and (toAutoSpanColumns in FOptions.AutoOptions);
  SimpleSelection := toSimpleDrawSelection in FOptions.SelectionOptions;
  // This is the node to start with.
  Run := GetNodeAt(0, MinY, False, CurrentTop);

  if Assigned(Run) then
  begin
    LabelOffset := GetOffset(TVTElement.ofsLabel, Run);

    // ----- main loop
    // Change selection depending on the node's rectangle being in the selection rectangle or not, but
    // touch only those nodes which overlap either the old selection rectangle or the new one but not both.
    repeat
      // Collect offsets for check, normal and state images.
      TextLeft := NodeLeft + LabelOffset;
      NextTop := CurrentTop + NodeHeight[Run];

      // Simple selection allows to draw the selection rectangle anywhere. No intersection with node captions is
      // required. Only top and bottom bounds of the rectangle matter.
      if SimpleSelection or (toFullRowSelect in FOptions.SelectionOptions) then
      begin
        IsInOldRect := (NextTop > OldRect.Top) and (CurrentTop < OldRect.Bottom) and
          ((FHeader.Columns.Count = 0) or (FHeader.Columns.TotalWidth > OldRect.Left)) and ((NodeLeft + LabelOffset) < OldRect.Right);
        IsInNewRect := (NextTop > NewRect.Top) and (CurrentTop < NewRect.Bottom) and
          ((FHeader.Columns.Count = 0) or (FHeader.Columns.TotalWidth > NewRect.Left)) and ((NodeLeft + LabelOffset) < NewRect.Right);
      end
      else
      begin
        // The right column border might be extended if column spanning is enabled.
        if AutoSpan then
        begin
          with FHeader.Columns do
          begin
            NextColumn := MainColumn;
            repeat
              Dummy := GetNextVisibleColumn(NextColumn);
              if (Dummy = InvalidColumn) or not ColumnIsEmpty(Run, Dummy) or
                 (Items[Dummy].BidiMode <> bdLeftToRight) then
                Break;
              NextColumn := Dummy;
            until False;
            if NextColumn = MainColumn then
              CurrentRight := NodeRight
            else
              GetColumnBounds(NextColumn, DummyLeft, CurrentRight);
          end;
        end
        else
          CurrentRight := NodeRight;
          // Check if we need the node's width. This is the case when the node is not left aligned or the
          // left border of the selection rectangle is to the right of the left node border.
          if (TextLeft < OldRect.Left) or (TextLeft < NewRect.Left) or (Alignment <> taLeftJustify) then
          begin
            NodeWidth := DoGetNodeWidth(Run, MainColumn);
            if NodeWidth >= (CurrentRight - TextLeft) then
              TextRight := CurrentRight
            else
              case Alignment of
                taLeftJustify:
                  TextRight := TextLeft + NodeWidth;
                taCenter:
                  begin
                    TextLeft := Divide(TextLeft + CurrentRight - NodeWidth, 2);
                    TextRight := TextLeft + NodeWidth;
                  end;
              else
                // taRightJustify
                TextRight := CurrentRight;
                TextLeft := TextRight - NodeWidth;
              end;
          end
          else
            TextRight := CurrentRight;

        // Now determine whether we need to change the state.
        IsInOldRect := (OldRect.Left <= TextRight) and (OldRect.Right >= TextLeft) and
          (NextTop > OldRect.Top) and (CurrentTop < OldRect.Bottom);
        IsInNewRect := (NewRect.Left <= TextRight) and (NewRect.Right >= TextLeft) and
          (NextTop > NewRect.Top) and (CurrentTop < NewRect.Bottom);
      end;

      if IsInOldRect xor IsInNewRect then
      begin
        Result := True;
        if DoSwitch then
        begin
          if vsSelected in Run.States then
            InternalRemoveFromSelection(Run)
          else
            InternalCacheNode(Run);
        end
        else
        begin
          if IsInNewRect then
            InternalCacheNode(Run)
          else
            InternalRemoveFromSelection(Run);
          end;
      end;
      CurrentTop := NextTop;
      // Get next visible node and update left node position.
      NextNode := GetNextVisibleNoInit(Run, True);
      if NextNode = nil then
        Break;
      Inc(NodeLeft, CountLevelDifference(Run, NextNode) * FIndent);
      Run := NextNode;
    until CurrentTop > MaxY;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CollectSelectedNodesRTL(MainColumn: Integer; NodeLeft, NodeRight: TDimension; Alignment: TAlignment;
  OldRect, NewRect: TRect): Boolean;

// Helper routine used when a draw selection takes place. This version handles right-to-left directionality.
// See also comments in CollectSelectedNodesLTR.

var
  Run,
  NextNode: PVirtualNode;
  NextColumn,
  Dummy: Integer;

  DummyRight,
  TextRight,
  TextLeft,
  CheckOffset,
  CurrentTop,
  CurrentLeft,
  NextTop,
  NodeWidth,
  MinY, MaxY: TDimension;
  IsInOldRect,
  IsInNewRect: Boolean;

  // quick check variables for various parameters
  WithCheck,
  WithStateImages,
  DoSwitch,
  AutoSpan: Boolean;
  SimpleSelection: Boolean;

begin
  // A priori nothing changes.
  Result := False;
  // Switch the alignment to the opposite value in RTL context.
  ChangeBiDiModeAlignment(Alignment);

  // Determine minimum and maximum vertical coordinates to limit iteration to.
  MinY := Min(OldRect.Top, NewRect.Top);
  MaxY := Max(OldRect.Bottom, NewRect.Bottom);

  // Initialize short hand variables to speed up tests below.
  DoSwitch := ssCtrl in FDrawSelShiftState;
  WithCheck := (toCheckSupport in FOptions.MiscOptions) and Assigned(FCheckImages);
  // Don't check the events here as descendant trees might have overriden the DoGetImageIndex method.
  WithStateImages := Assigned(FStateImages) or Assigned(OnGetImageIndexEx);
  if WithCheck then
    CheckOffset := FCheckImages.Width + FImagesMargin
  else
    CheckOffset := 0;
  AutoSpan := FHeader.UseColumns and (toAutoSpanColumns in FOptions.AutoOptions);
  SimpleSelection := toSimpleDrawSelection in FOptions.SelectionOptions;
  // This is the node to start with.
  Run := GetNodeAt(0, MinY, False, CurrentTop);

  if Assigned(Run) then
  begin
    // The initial minimal left border is determined by the identation level of the node and is dynamically adjusted.
    if toShowRoot in FOptions.PaintOptions then
      Dec(NodeRight, (TDimension((GetNodeLevel(Run) + 1)) * FIndent) + FMargin)
    else
      Dec(NodeRight, (TDimension(GetNodeLevel(Run)) * FIndent) + FMargin);

    // ----- main loop
    // Change selection depending on the node's rectangle being in the selection rectangle or not, but
    // touch only those nodes which overlap either the old selection rectangle or the new one but not both.
    repeat
      // Collect offsets for check, normal and state images.
      TextRight := NodeRight;
      if WithCheck and (Run.CheckType <> ctNone) then
        Dec(TextRight, CheckOffset);
      Dec(TextRight, GetImageSize(Run, ikNormal, MainColumn).cx);
      if WithStateImages then
        Dec(TextRight, GetImageSize(Run, ikState, MainColumn).cx);
      NextTop := CurrentTop + NodeHeight[Run];

      // Simple selection allows to draw the selection rectangle anywhere. No intersection with node captions is
      // required. Only top and bottom bounds of the rectangle matter.
      if SimpleSelection then
      begin
        IsInOldRect := (NextTop > OldRect.Top) and (CurrentTop < OldRect.Bottom);
        IsInNewRect := (NextTop > NewRect.Top) and (CurrentTop < NewRect.Bottom);
      end
      else
      begin        // The left column border might be extended if column spanning is enabled.
        if AutoSpan then
        begin
          NextColumn := MainColumn;
          repeat
            Dummy := FHeader.Columns.GetPreviousVisibleColumn(NextColumn);
            if (Dummy = InvalidColumn) or not ColumnIsEmpty(Run, Dummy) or
               (FHeader.Columns[Dummy].BiDiMode = bdLeftToRight) then
              Break;
            NextColumn := Dummy;
          until False;
          if NextColumn = MainColumn then
            CurrentLeft := NodeLeft
          else
            FHeader.Columns.GetColumnBounds(NextColumn, CurrentLeft, DummyRight);
        end
        else
          CurrentLeft := NodeLeft;
          // Check if we need the node's width. This is the case when the node is not left aligned (in RTL context this        // means actually right aligned) or the right border of the selection rectangle is to the left
          // of the right node border.
          if (TextRight > OldRect.Right) or (TextRight > NewRect.Right) or (Alignment <> taRightJustify) then
          begin
          NodeWidth := DoGetNodeWidth(Run, MainColumn);
          if NodeWidth >= (TextRight - CurrentLeft) then
            TextLeft := CurrentLeft
          else
            case Alignment of
              taLeftJustify:
                begin
                  TextLeft := CurrentLeft;
                  TextRight := TextLeft + NodeWidth;
                end;
              taCenter:
                begin
                  TextLeft := Divide(TextRight + CurrentLeft - NodeWidth, 2);
                  TextRight := TextLeft + NodeWidth;
                end;
              else
                // taRightJustify
                TextLeft := TextRight - NodeWidth;
            end;
        end
        else
          TextLeft := CurrentLeft;

        // Now determine whether we need to change the state.
        IsInOldRect := (OldRect.Right >= TextLeft) and (OldRect.Left <= TextRight) and
          (NextTop > OldRect.Top) and (CurrentTop < OldRect.Bottom);
        IsInNewRect := (NewRect.Right >= TextLeft) and (NewRect.Left <= TextRight) and
          (NextTop > NewRect.Top) and (CurrentTop < NewRect.Bottom);
      end;

      if IsInOldRect xor IsInNewRect then
      begin
        Result := True;
        if DoSwitch then
        begin
          if vsSelected in Run.States then
            InternalRemoveFromSelection(Run)
          else
            InternalCacheNode(Run);
        end
        else
        begin
          if IsInNewRect then
            InternalCacheNode(Run)
          else
            InternalRemoveFromSelection(Run);
        end;
      end;
      CurrentTop := NextTop;
      // Get next visible node and update left node position.
      NextNode := GetNextVisibleNoInit(Run, True);
      if NextNode = nil then
        Break;
      Dec(NodeRight, CountLevelDifference(Run, NextNode) * FIndent);
      Run := NextNode;
    until CurrentTop > MaxY;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ClearNodeBackground(const PaintInfo: TVTPaintInfo; UseBackground, Floating: Boolean;
  R: TRect);

// Erases a node's background depending on what the application decides to do.
// UseBackground determines whether or not to use the background picture, while Floating indicates
// that R is given in coordinates of the small node bitmap or the superordinated target bitmap used in PaintTree.

var
  BackColor: TColor;
  EraseAction: TItemEraseAction;
  Offset: TPoint;

begin
  BackColor := FColors.BackGroundColor;
  with PaintInfo do
  begin
    EraseAction := eaDefault;

    if Floating then
    begin
      Offset := Point(-FEffectiveOffsetX, R.Top);
      OffsetRect(R, 0, -Offset.Y);
    end
    else
      Offset := Point(0, 0);

    DoBeforeItemErase(Canvas, Node, R, BackColor, EraseAction);

    with Canvas do
    begin
      case EraseAction of
        eaNone:
          ;
        eaColor:
          begin
            // User has given a new background color.
            Brush.Color := BackColor;
            FillRect(R);
          end;
      else // eaDefault
        if UseBackground then
        begin
          if toStaticBackground in TreeOptions.PaintOptions then
            StaticBackground(FBackground, Canvas, Offset, R, FColors.BackGroundColor)
          else
            TileBackground(FBackground, Canvas, Offset, R, FColors.BackGroundColor);
        end
        else
        begin
          if (poDrawSelection in PaintOptions) and (toFullRowSelect in FOptions.SelectionOptions) and
             (vsSelected in Node.States) and not (toUseBlendedSelection in FOptions.PaintOptions) and not
             (tsUseExplorerTheme in FStates) then
          begin
            if toShowHorzGridLines in FOptions.PaintOptions then
            begin
              Brush.Color := BackColor;
              FillRect(Rect(R.Left, R.Bottom - 1, R.Right, R.Bottom));
              Dec(R.Bottom);
            end;
            if Focused or (toPopupMode in FOptions.PaintOptions) then
            begin
              Brush.Color := FColors.FocusedSelectionColor;
              Pen.Color := FColors.FocusedSelectionBorderColor;
            end
            else
            begin
              Brush.Color := FColors.UnfocusedSelectionColor;
              Pen.Color := FColors.UnfocusedSelectionBorderColor;
            end;

            RoundRect(R.Left, R.Top, R.Right, R.Bottom, FSelectionCurveRadius, FSelectionCurveRadius);
          end
          else
          begin
            Brush.Color := BackColor;
            FillRect(R);
          end;
        end;
      end;
      DoAfterItemErase(Canvas, Node, R);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CompareNodePositions(Node1, Node2: PVirtualNode; ConsiderChildrenAbove: Boolean = False): Integer;

// Tries hard and smart to quickly determine whether Node1's structural position is before Node2's position.
// If ConsiderChildrenAbove is True, the nodes will be compared with their visual order in mind.
// Returns 0 if Node1 = Node2, < 0 if Node1 is located before Node2 else > 0.

var
  Run1,
  Run2: PVirtualNode;
  Level1,
  Level2: Cardinal;

begin
  Assert(Assigned(Node1) and Assigned(Node2), 'Nodes must never be nil.');

  if Node1 = Node2 then
    Result := 0
  else
  begin
    if HasAsParent(Node1, Node2) then
      Result := IfThen(ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions), -1, 1)
    else
      if HasAsParent(Node2, Node1) then
        Result := IfThen(ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions), 1, -1)
      else
      begin
        // the given nodes are neither equal nor are they parents of each other, so go up to FRoot
        // for each node and compare the child indices of the top level parents
        // Note: neither Node1 nor Node2 can be FRoot at this point as this (a bit strange) circumstance would
        //       be caught by the previous code.

        // start lookup at the same level
        Level1 := GetNodeLevel(Node1);
        Level2 := GetNodeLevel(Node2);
        Run1 := Node1;
        while Level1 > Level2 do
        begin
          Run1 := Run1.Parent;
          System.Dec(Level1);
        end;
        Run2 := Node2;
        while Level2 > Level1 do
        begin
          Run2 := Run2.Parent;
          System.Dec(Level2);
        end;

        // now go up until we find a common parent node (loop will safely stop at FRoot if the nodes
        // don't share a common parent)
        while Run1.Parent <> Run2.Parent do
        begin
          Run1 := Run1.Parent;
          Run2 := Run2.Parent;
        end;
        Result := Integer(Run1.Index) - Integer(Run2.Index);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DrawLineImage(const PaintInfo: TVTPaintInfo; X, Y, H, VAlign: TDimension; Style: TVTLineType;
  Reverse: Boolean);

// Draws (depending on Style) one of the 5 line types of the tree.
// If Reverse is True then a right-to-left column is being drawn, hence horizontal lines must be mirrored.
// X and Y describe the left upper corner of the line image rectangle, while H denotes its height (and width).

var
  HalfWidth,
  TargetX: TDimension;

begin
  HalfWidth := Divide(FIndent, 2);
  if Reverse then
    TargetX := 0
  else
    TargetX := FIndent - 1;

  with PaintInfo.Canvas do
  begin
    case Style of
      ltBottomRight:
        begin
          DrawDottedVLine(PaintInfo, Y + VAlign, Y + H, X + HalfWidth);
          DrawDottedHLine(PaintInfo, X + HalfWidth, X + TargetX, Y + VAlign);
        end;
      ltTopDown:
        DrawDottedVLine(PaintInfo, Y, Y + H, X + HalfWidth);
      ltTopDownRight:
        begin
          DrawDottedVLine(PaintInfo, Y, Y + H, X + HalfWidth);
          DrawDottedHLine(PaintInfo, X + HalfWidth, X + TargetX, Y + VAlign);
        end;
      ltRight:
        DrawDottedHLine(PaintInfo, X + HalfWidth, X + TargetX, Y + VAlign);
      ltTopRight:
        begin
          DrawDottedVLine(PaintInfo, Y, Y + VAlign, X + HalfWidth);
          DrawDottedHLine(PaintInfo, X + HalfWidth, X + TargetX, Y + VAlign);
        end;
      ltLeft: // left can also mean right for RTL context
        if Reverse then
          DrawDottedVLine(PaintInfo, Y, Y + H, X + FIndent)
        else
          DrawDottedVLine(PaintInfo, Y, Y + H, X);
      ltLeftBottom:
        if Reverse then
        begin
          DrawDottedVLine(PaintInfo, Y, Y + H, X + FIndent);
          DrawDottedHLine(PaintInfo, X, X + FIndent, Y + H);
        end
        else
        begin
          DrawDottedVLine(PaintInfo, Y, Y + H, X);
          DrawDottedHLine(PaintInfo, X, X + FIndent, Y + H);
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.FindInPositionCache(Node: PVirtualNode; var CurrentPos: TNodeHeight): PVirtualNode;

// Looks through the position cache and returns the node whose top position is the largest one which is smaller or equal
// to the position of the given node.

var
  L, H, I: Integer;

begin
  L := 0;
  H := High(FPositionCache);
  while L <= H do
  begin
    I := (L + H) shr 1;
    if CompareNodePositions(FPositionCache[I].Node, Node) <= 0 then
      L := I + 1
    else
      H := I - 1;
  end;
  if L = 0 then // High(FPositionCache) = -1
  begin
    Result := nil;
    CurrentPos := 0;
  end
  else
  begin
    Result := FPositionCache[L - 1].Node;
    CurrentPos := FPositionCache[L - 1].AbsoluteTop;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.FindInPositionCache(Position: TDimension; var CurrentPos: TNodeHeight): PVirtualNode;

// Looks through the position cache and returns the node whose top position is the largest one which is smaller or equal
// to the given vertical position.
// The returned node does not necessarily occupy the given position but is the nearest one to start
// iterating from to approach the real node for a given position. CurrentPos receives the actual position of the found
// node which is needed for further iteration.

var
  L, H, I: Integer;

begin
  L := 0;
  H := High(FPositionCache);
  while L <= H do
  begin
    I := (L + H) shr 1;
    if FPositionCache[I].AbsoluteTop <= Position then
      L := I + 1
    else
      H := I - 1;
  end;
  if L = 0 then // High(FPositionCache) = -1
  begin
    Result := nil;
    CurrentPos := 0;
  end
  else
  begin
    Result := FPositionCache[L - 1].Node;
    CurrentPos := FPositionCache[L - 1].AbsoluteTop;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FixupTotalCount(Node: PVirtualNode);

// Called after loading a subtree from stream. The child count in each node is already set but not
// their total count.

var
  Child: PVirtualNode;

begin
  // Initial total count is set to one on node creation.
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    FixupTotalCount(Child);
    System.Inc(Node.TotalCount, Child.TotalCount);
    Child := Child.NextSibling;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FixupTotalHeight(Node: PVirtualNode);

// Called after loading a subtree from stream. The individual height of each node is set already,
// but their total height needs an adjustment depending on their visibility state.

var
  Child: PVirtualNode;

begin
  // Initial total height is set to the node height on load.
  Child := Node.FirstChild;

  if vsExpanded in Node.States then
  begin
    while Assigned(Child) do
    begin
      FixupTotalHeight(Child);
      if vsVisible in Child.States then
        Inc(Node.TotalHeight, Child.TotalHeight);
      Child := Child.NextSibling;
    end;
  end
  else
  begin
    // The node is collapsed, so just update the total height of its child nodes.
    while Assigned(Child) do
    begin
      FixupTotalHeight(Child);
      Child := Child.NextSibling;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetBottomNode: PVirtualNode;

begin
  Result := GetNodeAt(0, ClientHeight - 1);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetCheckedCount: Integer;

var
  Node: PVirtualNode;

begin
  Result := 0;
  Node := GetFirstChecked;
  while Assigned(Node) do
  begin
     System.Inc(Result);
     Node := GetNextChecked(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetCheckState(Node: PVirtualNode): TCheckState;

begin
  if Assigned(FOnBeforeGetCheckState) then
     FOnBeforeGetCheckState(Self, Node);

  Result := Node.CheckState;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetCheckType(Node: PVirtualNode): TCheckType;

begin
  Result := Node.CheckType;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetChildCount(Node: PVirtualNode): Cardinal;
begin
  if (Node = nil) or (Node = FRoot) then
    Exit(FRoot.ChildCount);
  if not GetChildrenInitialized(Node) then
    InitChildren(Node);
  Exit(Node.ChildCount);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetChildrenInitialized(Node: PVirtualNode): Boolean;

begin
  Result := not (vsHasChildren in Node.States) or (Node.ChildCount > 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetCutCopyCount: Integer;

var
  Node: PVirtualNode;

begin
  Result := 0;
  Node := GetFirstCutCopy;
  while Assigned(Node) do
  begin
     System.Inc(Result);
     Node := GetNextCutCopy(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetDisabled(Node: PVirtualNode): Boolean;

begin
  Result := Assigned(Node) and (vsDisabled in Node.States);
end;

//----------------------------------------------------------------------------------------------------------------------
// whether the sync of checkbox with selection is allowed for this node
function TBaseVirtualTree.GetSyncCheckstateWithSelection(Node: PVirtualNode): Boolean;

begin
  Result := (toSyncCheckboxesWithSelection in FOptions.SelectionOptions)
            and (toCheckSupport in FOptions.MiscOptions)
            and Assigned(FCheckImages)
            and (Node.CheckType = ctCheckBox);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetDragManager: IVTDragManager;

// Returns the internal drag manager interface. If this does not yet exist then it is created here.

begin
  if FDragManager = nil then
  begin
    FDragManager := DoCreateDragManager;
    if FDragManager = nil then
      FDragManager := TVTDragManager.Create(Self) as IVTDragManager;
  end;

  Result := FDragManager;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetExpanded(Node: PVirtualNode): Boolean;

begin
  if Assigned(Node) then
    Result := vsExpanded in Node.States
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFiltered(Node: PVirtualNode): Boolean;

begin
  Result := vsFiltered in Node.States;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFullyVisible(Node: PVirtualNode): Boolean;

// Determines whether the given node has the visibility flag set as well as all its parents are expanded.

begin
  Assert(Assigned(Node), 'Invalid parameter.');
  Result := vsVisible in Node.States;
  if Result and (Node <> FRoot) then
    Result := VisiblePath[Node];
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetHasChildren(Node: PVirtualNode): Boolean;

begin
  if Assigned(Node) then
    Result := vsHasChildren in Node.States
  else
    Result := vsHasChildren in FRoot.States;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetMultiline(Node: PVirtualNode): Boolean;

begin
  Result := Assigned(Node) and (Node <> FRoot) and (vsMultiline in Node.States);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNodeHeight(Node: PVirtualNode): TNodeHeight;

begin
  if Assigned(Node) and (Node <> FRoot) then
  begin
    if (toVariableNodeHeight in FOptions.MiscOptions) and not (vsDeleting in Node.States) then
    begin
      if not (vsInitialized in Node.States) then
        InitNode(Node);

      // Ensure the node's height is determined.
      MeasureItemHeight(Self.Canvas, Node);
    end;
    Result := Node.NodeHeight;
  end
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNodeParent(Node: PVirtualNode): PVirtualNode;

begin
  if Assigned(Node) and (Node.Parent <> FRoot) then
    Result := Node.Parent
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetOffset(pElement: TVTElement; pNode: PVirtualNode): TDimension;
// Calculates the offset of the given element
var
  lOffsets: TVTOffsets;
begin
  GetOffsets(pNode, lOffsets, pElement);
  Exit(lOffsets[pElement]);
end;

procedure TBaseVirtualTree.GetOffsets(pNode: PVirtualNode; out pOffsets: TVTOffsets; pElement: TVTElement = TVTElement.ofsEndOfClientArea; pColumn: Integer = NoColumn);
// Calculates the offset up to the given element and supplies them in an array.
var
  lNodeLevel: Integer;
  lNodeIndent: TDimension;
begin
  // If no specific column was given, assume the main column
  if pColumn = -1 then
    pColumn := Header.MainColumn;

  // Left Margin
  pOffsets[TVTElement.ofsMargin] := FMargin;
  if pElement = ofsMargin then
    exit;

  pOffsets[TVTElement.ofsToggleButton] := pOffsets[TVTElement.ofsMargin];
  pOffsets[TVTElement.ofsCheckBox]     := pOffsets[TVTElement.ofsMargin];
  if (pColumn = Header.MainColumn) then
  begin
    if not (toFixedIndent in TreeOptions.PaintOptions) then
    begin
      // plus Indent
      lNodeLevel := GetNodeLevel(pNode);
      if toShowRoot in FOptions.PaintOptions then
        System.Inc(lNodeLevel);
    end
    else
      lNodeLevel := 1;
    lNodeIndent := lNodeLevel * TDimension(FIndent);
    // toggle buttons
    Inc(pOffsets[TVTElement.ofsToggleButton], lNodeIndent);
    Dec(pOffsets[TVTElement.ofsToggleButton], Divide((TDimension(FIndent) - FPlusBM.Width), 2) - 1 + FPlusBM.Width); //Compare PaintTree() relative line 107
    // checkbox
    Inc(pOffsets[TVTElement.ofsCheckBox], lNodeIndent);
  end;//if MainColumn

  // The area in which the toggle buttons are painted must have exactly the size of one indent level
  if pElement <= TVTElement.ofsToggleButton then
    exit;

  if (toCheckSupport in TreeOptions.MiscOptions) and Assigned(FCheckImages) and (pNode.CheckType <> ctNone) and (pColumn = Header.MainColumn) then
  begin
    Inc(pOffsets[TVTElement.ofsCheckBox], fImagesMargin);

    // right of checkbox, left of state image
    pOffsets[TVTElement.ofsStateImage] := pOffsets[TVTElement.ofsCheckBox] + FCheckImages.Width + fImagesMargin;
  end else
    pOffsets[TVTElement.ofsStateImage] := pOffsets[TVTElement.ofsCheckBox];
  if pElement <= TVTElement.ofsStateImage then
    exit;

  // right of left image, left of normal image
  pOffsets[TVTElement.ofsImage] := pOffsets[TVTElement.ofsStateImage] + GetImageSize(pNode, TVTImageKind.ikState, pColumn).cx;
  if pElement = TVTElement.ofsImage then
    exit;

  // label
  pOffsets[TVTElement.ofsLabel] := pOffsets[TVTElement.ofsImage] + GetImageSize(pNode, TVTImageKind.ikNormal, pColumn).cx;
  pOffsets[TVTElement.ofsText] := pOffsets[TVTElement.ofsLabel] + FTextMargin;
  Dec(pOffsets[TVTElement.ofsText]); //TODO: This should no longer be necessary once issue #369 is resolved.
  if pElement <= TVTElement.ofsText then
    exit;

  // End of text
  pOffsets[TVTElement.ofsRightOfText] := pOffsets[TVTElement.ofsText] + DoGetNodeWidth(pNode, pColumn) + DoGetNodeExtraWidth(pNode, pColumn);

  // end of client area
  pOffsets[TVTElement.ofsEndOfClientArea] := Max(FRangeX, ClientWidth) - FTextMargin;
end;

function TBaseVirtualTree.GetOffsetXY: TPoint;

begin
  Result := Point(FOffsetX, FOffsetY);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetRangeX: TDimension;
begin
  Result := Max(0, FRangeX);
end;

function TBaseVirtualTree.GetRootNodeCount: Cardinal;

begin
  Result := FRoot.ChildCount;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetSelected(Node: PVirtualNode): Boolean;

begin
  Result := Assigned(Node) and (vsSelected in Node.States);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetSelectedCount: Integer;
begin
  Exit(FSelectionCount);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetSelectedData<T>: TArray<T>;
var
  lItem: PVirtualNode;
  i: Integer;
begin
  SetLEngth(Result, Self.SelectedCount);
  i := 0;
  lItem := Self.GetFirstSelected;
  while Assigned(lItem) do
  begin
    Result[i] := Self.GetNodeData<T>(lItem);
    lItem := Self.GetNextSelected(lItem);
    System.Inc(i);
  end;
  SetLength(Result, i); // See issue #927, SelectedCount may not yet be updated.
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetTopNode: PVirtualNode;

var
  Dummy: TDimension;

begin
  Result := GetNodeAt(0, 0, True, Dummy);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetTotalCount(): Cardinal;

begin
  Assert(GetCurrentThreadId = MainThreadId, 'UI controls may only be used in UI thread.'); // FUpdateCount is not thread-safe! So do not write it in non-UI threads.
  System.Inc(FUpdateCount);
  try
    ValidateNode(FRoot, True);
  finally
    System.Dec(FUpdateCount);
  end;
  // The root node itself doesn't count as node.
  Result := FRoot.TotalCount - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetVclStyleEnabled: Boolean;
begin
  Exit(FVclStyleEnabled);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetVerticalAlignment(Node: PVirtualNode): Byte;

begin
  Result := Node.Align;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetVisible(Node: PVirtualNode): Boolean;

// Determines if the given node is marked as being visible.

begin
  if Node = nil then
    Node := FRoot;

  if not (vsInitialized in Node.States) then
    InitNode(Node);

  Result := vsVisible in Node.States;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetVisiblePath(Node: PVirtualNode): Boolean;

// Determines if all parents of the given node are expanded and have the visibility flag set.

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameters.');

  // FRoot is always expanded
  repeat
    Node := Node.Parent;
  until (Node = FRoot) or not (vsExpanded in Node.States) or not (vsVisible in Node.States);

  Result := Node = FRoot;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.HandleClickSelection(LastFocused, NewNode: PVirtualNode; Shift: TShiftState;
  DragPending: Boolean);

// Handles multi-selection with mouse click.

begin
  // Ctrl key down
  if ssCtrl in Shift then
  begin
    if ssShift in Shift then
    begin
      SelectNodes(FRangeAnchor, NewNode, True);
    end
    else
    begin
      if not (toSiblingSelectConstraint in FOptions.SelectionOptions) then
        FRangeAnchor := NewNode;
      // Delay selection change if a drag operation is pending.
      // Otherwise switch selection state here.
      if DragPending then
        DoStateChange([tsToggleFocusedSelection])
      else
        if vsSelected in NewNode.States then
          RemoveFromSelection(NewNode)
        else
          AddToSelection(NewNode, True);
    end;
  end
  else
    // Shift key down
    if ssShift in Shift then
    begin
      if FRangeAnchor = nil then
        FRangeAnchor := FRoot.FirstChild;

      // select node range
      if Assigned(FRangeAnchor) then
      begin
        SelectNodes(FRangeAnchor, NewNode, False);
        Invalidate;
      end;
    end
    else
    begin
      // any other case
      if not (vsSelected in NewNode.States) then
        AddToSelection(NewNode, True);
      // assign new reference item
      FRangeAnchor := NewNode;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.HandleDrawSelection(X, Y: TDimension): Boolean;

// Handles multi-selection with a focus rectangle.
// Result is True if something changed in selection.

var
  OldRect,
  NewRect: TRect;
  MainColumn: TColumnIndex;
  MaxValue: Integer;

  // limits of a node and its text
  NodeLeft,
  NodeRight: TDimension;

  // alignment and directionality
  CurrentBidiMode: TBidiMode;
  CurrentAlignment: TAlignment;

begin
  Result := False;

  // Selection changes are only done if the user drew a selection rectangle large
  // enough to exceed the threshold.
  if (FRoot.TotalCount > 1) and (tsDrawSelecting in FStates) then
  begin
    // Effective handling of node selection is done by using two rectangles stored in FSelectRec.
    OldRect := OrderRect(FLastSelRect);
    NewRect := OrderRect(FNewSelRect);
    ClearTempCache;

    MainColumn := FHeader.MainColumn;

    // Alignment and bidi mode determine where the node text is located within a node.
    if MainColumn <= NoColumn then
    begin
      CurrentBidiMode := BidiMode;
      CurrentAlignment := Alignment;
    end
    else
    begin
      CurrentBidiMode := FHeader.Columns[MainColumn].BidiMode;
      CurrentAlignment := FHeader.Columns[MainColumn].Alignment;
    end;

    // Determine initial left border of first node (take column reordering into account).
    if FHeader.UseColumns then
    begin
      // The mouse coordinates don't include any horizontal scrolling hence take this also
      // out from the returned column position.
      NodeLeft := FHeader.Columns[MainColumn].Left + FEffectiveOffsetX;
      NodeRight := NodeLeft + FHeader.Columns[MainColumn].Width;
    end
    else
    begin
      NodeLeft := 0 + FEffectiveOffsetX;
      NodeRight := NodeLeft + ClientWidth;
    end;
    if CurrentBidiMode = bdLeftToRight then
      Result := CollectSelectedNodesLTR(MainColumn, NodeLeft, NodeRight, CurrentAlignment, OldRect, NewRect)
    else
      Result := CollectSelectedNodesRTL(MainColumn, NodeLeft, NodeRight, CurrentAlignment, OldRect, NewRect);
  end;

  if Result then
  begin
    // Do some housekeeping if there was a change.
    MaxValue := PackArray(FSelection, FSelectionCount);
    if MaxValue > -1 then
    begin
      FSelectionCount := MaxValue;
      SetLength(FSelection, FSelectionCount);
    end;
    if FTempNodeCount > 0 then
    begin
      if tsClearOnNewSelection in fStates then
      begin
        DoStateChange([], [tsClearOnNewSelection]);
        ClearSelection(False);
      end;

      AddToSelection(FTempNodeCache, FTempNodeCount);
      ClearTempCache;
    end;

    Change(nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.HasVisibleNextSibling(Node: PVirtualNode): Boolean;

// Helper method to determine if the given node has a visible next sibling. This is needed to
// draw correct tree lines.

begin
  // Check if there is a sibling at all.
  Result := Assigned(Node.NextSibling);

  if Result then
  begin
    repeat
      Node := Node.NextSibling;
      Result := IsEffectivelyVisible[Node];
    until Result or (Node.NextSibling = nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.HasVisiblePreviousSibling(Node: PVirtualNode): Boolean;

// Helper method to determine if the given node has a visible previous sibling. This is needed to
// draw correct tree lines.

begin
  // Check if there is a sibling at all.
  Result := Assigned(Node.PrevSibling);

  if Result then
  begin
    repeat
      Node := Node.PrevSibling;
      Result := IsEffectivelyVisible[Node];
    until Result or (Node.PrevSibling = nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ImageListChange(Sender: TObject);

begin
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InitializeFirstColumnValues(var PaintInfo: TVTPaintInfo);

// Determines initial index, position and cell size of the first visible column.

begin
  PaintInfo.Column := FHeader.Columns.GetFirstVisibleColumn;
  with FHeader.Columns, PaintInfo do
  begin
    if Column > NoColumn then
    begin
      CellRect.Right := CellRect.Left + Items[Column].Width;
      Position := Items[Column].Position;
    end
    else
      Position := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InitRecursive(Node: PVirtualNode; Levels: Cardinal = MaxInt; pVisibleOnly: Boolean = True);

// Initializes a node and optionally its children up to a certain level.
// The sepcified number of levels are latrive to the givne Node.

var
  Run: PVirtualNode;
begin
  if not Assigned(Node) then
    Node := FRoot;

  if (Node <> FRoot) and not (vsInitialized in Node.States) then
    InitNode(Node);
  if (Levels = 0) or (pVisibleOnly and not (vsExpanded in Node.States))  then
    exit;
  Run := Node.FirstChild;

  while Assigned(Run) do
  begin
    InitRecursive(Run, Levels - 1, pVisibleOnly);
    Run := Run.NextSibling;
  end;
end;

procedure TBaseVirtualTree.InitRootNode(OldSize: Cardinal = 0);

// Reinitializes the root node.

var
  NewSize: Cardinal;

begin
  NewSize := TreeNodeSize + FTotalInternalDataSize;
  if FRoot = nil then
    FRoot := AllocMem(NewSize)
  else
  begin
    ReallocMem(FRoot, NewSize);
    ZeroMemory(PByte(FRoot) + OldSize, NewSize - OldSize);
  end;

  with FRoot^ do
  begin
    // Indication that this node is the root node.
    SetPrevSibling(FRoot);
    SetNextSibling(FRoot);
    SetParent(Pointer(Self));
    States := [vsInitialized, vsExpanded, vsHasChildren, vsVisible];
    TotalHeight := FDefaultNodeHeight;
    TotalCount := 1;
    SetNodeHeight(FDefaultNodeHeight);
    Align := 50;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InterruptValidation(pWaitForValidationTermination: Boolean = True);

var
  WasValidating: Boolean;
begin
  DoStateChange([tsStopValidation], [tsUseCache]);

  // Check the worker thread existance. It might already be gone (usually on destruction of the last tree).
  WasValidating := (tsValidating in FStates);
  TWorkerThread.RemoveTree(Self, pWaitForValidationTermination);
  if WasValidating then
    InvalidateCache();
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsFirstVisibleChild(Parent, Node: PVirtualNode): Boolean;

// Helper method to check if Node is the same as the first visible child of Parent.

var
  Run: PVirtualNode;

begin
  // Find first visible child.
  Run := Parent.FirstChild;
  while Assigned(Run) and not IsEffectivelyVisible[Run] do
    Run := Run.NextSibling;

  Result := Assigned(Run) and (Run = Node);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsLastVisibleChild(Parent, Node: PVirtualNode): Boolean;

// Helper method to check if Node is the same as the last visible child of Parent.

var
  Run: PVirtualNode;

begin
  // Find last visible child.
  Run := Parent.LastChild;
  while Assigned(Run) and not IsEffectivelyVisible[Run] do
    Run := Run.PrevSibling;

  Result := Assigned(Run) and (Run = Node);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.MakeNewNode: PVirtualNode;

var
  Size: Cardinal;

begin
  Size := TreeNodeSize;
  if (csDesigning in ComponentState) and (FNodeDataSize < 0) then
    System.Inc(Size, SizeOf(Pointer)) // Fixes #702
  else
  begin  // Make sure FNodeDataSize is valid.
    if FNodeDataSize < 0 then // NodeDataSize may be 0 for descendant controls that use only InternalData.
      ValidateNodeDataSize(FNodeDataSize);

    // Take record alignment into account.
    System.Inc(Size, FNodeDataSize);
  end;

  Result := AllocMem(Size + FTotalInternalDataSize);

  // Fill in some default values.
  with Result^ do
  begin
    TotalCount := 1;
    TotalHeight := FDefaultNodeHeight;
    SetNodeHeight(FDefaultNodeHeight);
    States := [vsVisible];
    Align := 50;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.PackArray({*}const TheArray: TNodeArray; Count: Integer): Integer; assembler;
// *This is an optimization to get as near as possible with the PUREPASCAL code without the
//  compiler generating a _DynArrayAddRef call. We still modify the array's content via pointers.

// Removes all entries from the selection array which are no longer in use. The selection array must be sorted for this
// algo to work. Values which must be removed are marked with bit 0 (LSB) set. This little trick works because memory
// is always allocated DWORD aligned. Since the selection array must be sorted while determining the entries to be
// removed it is much more efficient to increment the entry in question instead of setting it to nil (which would break
// the ordered appearance of the list).
//
// On enter EAX contains self reference, EDX the address to TheArray and ECX Count
// The returned value is the number of remaining entries in the array, so the caller can reallocate (shorten)
// the selection array if needed or -1 if nothing needs to be changed.

{$IF Defined(CPUX64) or Defined(VT_FMX)}
var
  Source, Dest: ^PVirtualNode;
  ConstOne: NativeInt;
begin
  Source := Pointer(TheArray);
  ConstOne := 1;
  Result := 0;
  // Do the fastest scan possible to find the first entry
  while (Count <> 0) and {not Odd(NativeInt(Source^))} (NativeInt(Source^) and ConstOne = 0) do
  begin
    System.Inc(Result);
    System.Inc(Source);
    System.Dec(Count);
  end;

  if Count <> 0 then
  begin
    Dest := Source;
    repeat
      // Skip odd entries
      if {not Odd(NativeInt(Source^))} NativeInt(Source^) and ConstOne = 0 then
      begin
        Dest^ := Source^;
        System.Inc(Result);
        System.Inc(Dest);
      end;
      System.Inc(Source); // Point to the next entry
      System.Dec(Count);
    until Count = 0;
  end;
end;
{$else}
asm
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        MOV     ESI, EDX
        MOV     EDX, -1
        JCXZ    @@Finish               // Empty list?
        INC     EDX                    // init remaining entries counter
        MOV     EDI, ESI               // source and destination point to the list memory
        MOV     EBX, 1                 // use a register instead of immediate operant to check against
@@PreScan:
        TEST    [ESI], EBX             // do the fastest scan possible to find the first entry
                                       // which must be removed
        JNZ     @@DoMainLoop
        INC     EDX
        ADD     ESI, 4
        DEC     ECX
        JNZ     @@PreScan
        JMP     @@Finish

@@DoMainLoop:
        MOV     EDI, ESI
@@MainLoop:
        TEST    [ESI], EBX             // odd entry?
        JNE     @@Skip                 // yes, so skip this one
        MOVSD                          // else move the entry to new location
        INC     EDX                    // count the moved entries
        DEC     ECX
        JNZ     @@MainLoop             // do it until all entries are processed
        JMP     @@Finish

@@Skip:
        ADD     ESI, 4                 // point to the next entry
        DEC     ECX
        JNZ     @@MainLoop             // do it until all entries are processed
@@Finish:
        MOV     EAX, EDX               // prepare return value
        POP     ESI
        POP     EDI
        POP     EBX
end;
{$IFEND}

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.PrepareBitmaps(NeedButtons, NeedLines: Boolean);

// initializes the contents of the internal bitmaps

const
  LineBitsDotted: array [0..8] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA, $55);
  LineBitsSolid: array [0..7] of Word = (0, 0, 0, 0, 0, 0, 0, 0);

var
  Bits: Pointer;
  Size: TSize;
  Theme: HTHEME;
  R: TRect;
  BitsLinesCount: Word;

  //--------------- local function --------------------------------------------

  procedure FillBitmap (ABitmap: TBitmap);
  begin
    with ABitmap, Canvas do
    begin
      SetSize(Size.cx, Size.cy);

      if (tsUseThemes in FStates) and (toUseExplorerTheme in FOptions.PaintOptions) or VclStyleEnabled then
      begin
        if (FHeader.MainColumn > NoColumn) then
          Brush.Color := FHeader.Columns[FHeader.MainColumn].GetEffectiveColor
        else
          Brush.Color := FColors.BackGroundColor;
      end
      else
        Brush.Color := clFuchsia;

      Transparent := True;
      TransparentColor := Brush.Color;

      FillRect(Rect(0, 0, Width, Height));
    end;
  end;

  //--------------- end local function ----------------------------------------

const
  cMinExpandoHeight = 11; // pixels @100%
begin
  if VclStyleEnabled and (seClient in StyleElements) then
  begin
    if NeedButtons then begin
      if StyleServices.GetElementSize(FPlusBM.Canvas.Handle, StyleServices.GetElementDetails(tcbCategoryGlyphClosed), TElementSize.esActual, Size) then
      begin
        Size.cx := Max(Size.cx, cMinExpandoHeight); // Use min size of 11, see issue #1035 / RSP-33715
        Size.cx := ScaledPixels(Size.cx) // I would have expected that the returned value is dpi-sclaed, but this is not the case in RAD Studio 10.4.1. See issue #984
      end
      else
        Size.cx := ScaledPixels(cMinExpandoHeight);
      Size.cy := Size.cx;
      FillBitmap(FPlusBM);
      FillBitmap(FHotPlusBM);
      FillBitmap(FSelectedHotPlusBM);
      FillBitmap(FMinusBM);
      FillBitmap(FHotMinusBM);
      FillBitmap(FSelectedHotMinusBM);
      R := Rect(0,0,Size. cx,Size.cy);
      // tcbCategoryGlyphClosed, tcbCategoryGlyphOpened from CategoryButtons
      StyleServices.DrawElement(FPlusBM.Canvas.Handle, StyleServices.GetElementDetails(tcbCategoryGlyphClosed), R {$IF CompilerVersion >= 34}, nil, FCurrentPPI{$IFEND});
      StyleServices.DrawElement(FMinusBM.Canvas.Handle, StyleServices.GetElementDetails(tcbCategoryGlyphOpened), R {$IF CompilerVersion >= 34}, nil, FCurrentPPI{$IFEND});
      FHotMinusBM.Canvas.Draw(0, 0, FMinusBM);
      FSelectedHotMinusBM.Canvas.Draw(0, 0, FMinusBM);
      FHotPlusBM.Canvas.Draw(0, 0, FPlusBM);
      FSelectedHotPlusBM.Canvas.Draw(0, 0, FPlusBM);
      if Assigned(FOnPrepareButtonImages) then
        FOnPrepareButtonImages(Self, FPlusBM, FHotPlusBM, FSelectedHotPlusBM, FMinusBM, FHotMinusBM, FSelectedHotMinusBM, size);
    end;//if NeedButtons
  end// if VclStyleEnabled
    else
      begin // No stlye
        Size.cx := ScaledPixels(9);
        Size.cy := ScaledPixels(9);
        if tsUseThemes in FStates then
        begin
          R := Rect(0, 0, 100, 100);
          {$if CompilerVersion >= 33}
          if TOSVersion.Check(10) and (TOSVersion.Build >= 15063)  then
            Theme := OpenThemeDataForDPI(Handle, 'TREEVIEW', Self.FCurrentPPI)
          else
            Theme := OpenThemeData(Handle, 'TREEVIEW');
          {$else}
          Theme := OpenThemeData(Handle, 'TREEVIEW');
          {$ifend}
          GetThemePartSize(Theme, FPlusBM.Canvas.Handle, TVP_GLYPH, GLPS_OPENED, @R, TS_TRUE, Size);
        end
          else
            Theme := 0;

        if NeedButtons then
        begin
          //VCL Themes do not really have ability to provide tree plus/minus images when not using the
          //windows theme. The bitmap style designer doesn't have any elements for them, and you
          //cannot name any elements you add, which makes it useless.
          //To mitigate this, Hook up the OnPrepareButtonImages and draw them yourself.
          if Assigned(FOnPrepareButtonImages) then
          begin
            FillBitmap(FPlusBM);
            FillBitmap(FHotPlusBM);
            FillBitmap(FSelectedHotPlusBM);
            FillBitmap(FMinusBM);
            FillBitmap(FHotMinusBM);
            FillBitmap(FSelectedHotMinusBM);
            FOnPrepareButtonImages(Self, FPlusBM, FHotPlusBM, FSelectedHotPlusBM, FMinusBM, FHotMinusBM, FSelectedHotMinusBM, size);
          end
            else
              begin
                with FMinusBM, Canvas do
                begin
                  // box is always of odd size
                  FillBitmap(FMinusBM);
                  FillBitmap(FHotMinusBM);
                  FillBitmap(FSelectedHotMinusBM);
                  // Weil die selbstgezeichneten Bitmaps sehen im Vcl Style scheiße aus
                  // Because the self-drawn bitmaps view Vcl Style shit
                  if Theme = 0 then
                  begin
                    if not(tsUseExplorerTheme in FStates) then
                    begin
                      if FButtonStyle = bsTriangle then
                      begin
                        FMinusBM.Canvas.Brush.Color := clBlack;
                        FMinusBM.Canvas.Pen.Color := clBlack;
                        FMinusBM.Canvas.Polygon([Point(0, 2), Point(8, 2), Point(4, 6)]);
                      end
                        else
                          begin
                            // Button style is rectangular. Now ButtonFillMode determines how to fill the interior.
                            if FButtonFillMode in [fmTreeColor, fmWindowColor, fmTransparent] then
                            begin
                              case FButtonFillMode of
                                fmTreeColor:
                                  FMinusBM.Canvas.Brush.Color := FColors.BackGroundColor;
                                fmWindowColor:
                                  FMinusBM.Canvas.Brush.Color := clWindow;
                              end;
                              Pen.Color := FColors.TreeLineColor;
                              Rectangle(0, 0, Width, Height);
                              Pen.Color := FColors.NodeFontColor;
                              MoveTo(2, Width div 2);
                              LineTo(Width - 2, Width div 2);
                            end
                          end;
                      FHotMinusBM.Canvas.Draw(0, 0, FMinusBM);
                      FSelectedHotMinusBM.Canvas.Draw(0, 0, FMinusBM);
                    end;
                  end;
                end;
                with FPlusBM, Canvas do
                begin
                  FillBitmap(FPlusBM);
                  FillBitmap(FHotPlusBM);
                  FillBitmap(FSelectedHotPlusBM);
                  if Theme = 0 then
                  begin
                    if not(tsUseExplorerTheme in FStates) then
                    begin
                      if FButtonStyle = bsTriangle then
                      begin
                        FPlusBM.Canvas.Brush.Color := clBlack;
                        FPlusBM.Canvas.Pen.Color := clBlack;
                        FPlusBM.Canvas.Polygon([Point(2, 0), Point(6, 4), Point(2, 8)]);
                      end
                        else
                          begin
                            // Button style is rectangular. Now ButtonFillMode determines how to fill the interior.
                            if FButtonFillMode in [fmTreeColor, fmWindowColor, fmTransparent] then
                            begin
                              case FButtonFillMode of
                                fmTreeColor:
                                  FPlusBM.Canvas.Brush.Color := FColors.BackGroundColor;
                                fmWindowColor:
                                  FPlusBM.Canvas.Brush.Color := clWindow;
                              end;
                              Pen.Color := FColors.TreeLineColor;
                              Rectangle(0, 0, Width, Height);
                              Pen.Color := FColors.NodeFontColor;
                              MoveTo(2, Width div 2);
                              LineTo(Width - 2, Width div 2);
                              MoveTo(Width div 2, 2);
                              LineTo(Width div 2, Width - 2);
                            end
                          end;
                       FHotPlusBM.Canvas.Draw(0, 0, FPlusBM);
                       FSelectedHotPlusBM.Canvas.Draw(0, 0, FPlusBM);
                     end;
                  end;
                end;


          // Overwrite glyph images if theme is active.
          if (tsUseThemes in FStates) and (Theme <> 0) then
          begin
            R := Rect(0, 0, Size.cx, Size.cy);
            DrawThemeBackground(Theme, FPlusBM.Canvas.Handle, TVP_GLYPH, GLPS_CLOSED, R, nil);
            DrawThemeBackground(Theme, FMinusBM.Canvas.Handle, TVP_GLYPH, GLPS_OPENED, R, nil);
            if tsUseExplorerTheme in FStates then
            begin
              DrawThemeBackground(Theme, FHotPlusBM.Canvas.Handle, TVP_HOTGLYPH, GLPS_CLOSED, R, nil);
              DrawThemeBackground(Theme, FSelectedHotPlusBM.Canvas.Handle, TVP_HOTGLYPH, GLPS_CLOSED, R, nil);
              DrawThemeBackground(Theme, FHotMinusBM.Canvas.Handle, TVP_HOTGLYPH, GLPS_OPENED, R, nil);
              DrawThemeBackground(Theme, FSelectedHotMinusBM.Canvas.Handle, TVP_HOTGLYPH, GLPS_OPENED, R, nil);
             end
               else
                 begin
                   FHotPlusBM.Canvas.Draw(0, 0, FPlusBM);
                   FSelectedHotPlusBM.Canvas.Draw(0, 0, FPlusBM);
                   FHotMinusBM.Canvas.Draw(0, 0, FMinusBM);
                   FSelectedHotMinusBM.Canvas.Draw(0, 0, FMinusBM);
                 end;
          end;
        end;
        if tsUseThemes in FStates then
          CloseThemeData(Theme);
      end;// if NeedButtons
  end;// else

  if NeedLines then
  begin
    case FLineStyle of
      lsDotted:
        begin
          Bits := @LineBitsDotted;
          BitsLinesCount:= Length(LineBitsDotted);
        end;
      lsSolid:
        begin
          Bits := @LineBitsSolid;
          BitsLinesCount:= Length(LineBitsSolid);
        end;
    else // lsCustomStyle
      Bits := @LineBitsDotted;
      DoGetLineStyle(Bits);
      BitsLinesCount:= Length(LineBitsDotted);
    end;
    DottedBrushTreeLines:= PrepareDottedBrush(DottedBrushTreeLines, Bits, BitsLinesCount);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetAlignment(const Value: TAlignment);

begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetAnimationDuration(const Value: Cardinal);

begin
  FAnimationDuration := Value;
  if FAnimationDuration = 0 then
    FOptions.AnimationOptions := FOptions.AnimationOptions - [toAnimatedToggle]
  else
    FOptions.AnimationOptions := FOptions.AnimationOptions + [toAnimatedToggle]
end;

//----------------------------------------------------------------------------------------------------------------------
{ New, Support for transparent background:
  * Image types: BMP, PNG, GIF, ICO, EMF, TIFF and WMF are automatically identified to support transparent background
  * Also detects certain third party image classes registered for PNG, GIF and other image types so that the
    transparency related code is used for them. See the code below.
  * If some other third party image class is registered that is not detected,
    set the flag BackgroundTransparentExternalType explicitly in order to properly do
    transparent painting.
}
procedure TBaseVirtualTree.SetBackground(const Value: TVTBackground);

begin
  FBackground.Assign(Value);
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetBackGroundImageTransparent(const Value: Boolean);

begin
  if Value <> FBackGroundImageTransparent then
  begin
    FBackGroundImageTransparent := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetBackgroundOffset(const Index: Integer; const Value: TDimension);

begin
  case Index of
    0:
      if FBackgroundOffsetX <> Value then
      begin
        FBackgroundOffsetX := Value;
        Invalidate;
      end;
    1:
      if FBackgroundOffsetY <> Value then
      begin
        FBackgroundOffsetY := Value;
        Invalidate;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetBorderStyle(Value: TBorderStyle);

begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetBottomNode(Node: PVirtualNode);

var
  Run: PVirtualNode;
  R: TRect;

begin
  if Assigned(Node) then
  begin
    // make sure all parents of the node are expanded
    Run := Node.Parent;
    while Run <> FRoot do
    begin
      if not (vsExpanded in Run.States) then
        ToggleNode(Run);
      Run := Run.Parent;
    end;
    R := GetDisplayRect(Node, FHeader.MainColumn, True);
    DoSetOffsetXY(Point(FOffsetX, FOffsetY + ClientHeight - R.Top - NodeHeight[Node]),
      [suoRepaintScrollBars, suoUpdateNCArea]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetBottomSpace(const Value: TDimension);

begin
  if FBottomSpace <> Value then
  begin
    FBottomSpace := Value;
    UpdateVerticalScrollBar(True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetButtonFillMode(const Value: TVTButtonFillMode);

begin
  if FButtonFillMode <> Value then
  begin
    if Value = TVTButtonFillMode.fmShaded then // no longer supported
      FButtonFillMode := TVTButtonFillMode.fmTreeColor
    else
      FButtonFillMode := Value;
    if not (csLoading in ComponentState) then
    begin
      PrepareBitmaps(True, False);
      if HandleAllocated then
        Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetButtonStyle(const Value: TVTButtonStyle);

begin
  if FButtonStyle <> Value then
  begin
    FButtonStyle := Value;
    if not (csLoading in ComponentState) then
    begin
      PrepareBitmaps(True, False);
      if HandleAllocated then
        Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetCheckState(Node: PVirtualNode; Value: TCheckState);

begin
  if (Node.CheckState <> Value) and DoChecking(Node, Value) then
    DoCheckClick(Node, Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetCheckStateForAll(aCheckState: TCheckState; pSelectedOnly: Boolean; pExcludeDisabled: Boolean = True);

// Changes the check state for all or for all seledcted nodes.
// aCheckState: The new check state.
// pSelectedOnly: If passed True, only the selected nodes will bechnaged, if passed False all nodes in the control will be changed.
// pExcludeDisabled: Optiopnal. If passed True (the default value), disabled checkboxes won't be changed, if passed False disabled checkboxes will be altered too.

var
  lItem : PVirtualNode;
begin
  With Self do begin
    Screen.Cursor := crHourGlass;
    BeginUpdate;
    try
      if pSelectedOnly then
        lItem := GetFirstSelected
      else
        lItem := GetFirst;
      //for i:=0 to List.Items.Count-1 do begin
      while Assigned(lItem) do begin
        if not pExcludeDisabled or not CheckState[lItem].IsDisabled() then
          CheckState[lItem] := aCheckState;
        if pSelectedOnly then
          lItem := GetNextSelected(lItem)
        else
          lItem := GetNext(lItem);
      end;//while
    finally
      Screen.Cursor := crDefault;
      EndUpdate;
    end;//try..finally
  end;//With
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetCheckType(Node: PVirtualNode; Value: TCheckType);

begin
  if (Node.CheckType <> Value) and not (toReadOnly in FOptions.MiscOptions) then
  begin
    Node.CheckType := Value;
    if (Value <> ctTriStateCheckBox) and (Node.CheckState in [csMixedNormal, csMixedPressed]) then
      Node.CheckState := csUncheckedNormal;// reset check state if it doesn't fit the new check type
    // For check boxes with tri-state check box parents we have to initialize differently.
    if (toAutoTriStateTracking in FOptions.AutoOptions) and (Value in [ctCheckBox, ctTriStateCheckBox]) and
      (Node.Parent <> FRoot) then
    begin
      if not (vsInitialized in Node.Parent.States) then
        InitNode(Node.Parent);
      if (Node.Parent.CheckType = ctTriStateCheckBox) then begin
        if (GetCheckState(Node.Parent) in [csUncheckedNormal, csUncheckedDisabled]) then
          CheckState[Node] := csUncheckedNormal
        else if (GetCheckState(Node.Parent) in [csCheckedNormal, csCheckedDisabled]) then
          CheckState[Node] := csCheckedNormal;
      end;//if
    end;//if
    InvalidateNode(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetChildCount(Node: PVirtualNode; NewChildCount: Cardinal);

// Changes a node's child structure to accomodate the new child count. This is used to add or delete
// child nodes to/from the end of the node's child list. To insert or delete a specific node a separate
// routine is used.

var
  Remaining: Cardinal;
  Index: Cardinal;
  Child: PVirtualNode;
  Count: Integer;
  NewHeight: TNodeHeight;
begin
  if not (toReadOnly in FOptions.MiscOptions) then
  begin
    if Node = nil then
      Node := FRoot;

    Assert(GetCurrentThreadId = MainThreadId, 'UI controls may only be changed in UI thread.');
    if NewChildCount = 0 then
      DeleteChildren(Node)
    else
    begin
      // If nothing changed then do nothing.
      if NewChildCount <> Node.ChildCount then
      begin
        InterruptValidation;

        if NewChildCount > Node.ChildCount then
        begin
          Remaining := NewChildCount - Node.ChildCount;
          Count := Remaining;
          NewHeight := Node.TotalHeight;

          // New nodes to add.
          if Assigned(Node.LastChild) then
            Index := Node.LastChild.Index + 1
          else
          begin
            Index := 0;
            Include(Node.States, vsHasChildren);
          end;
          Node.States := Node.States - [vsAllChildrenHidden, vsHeightMeasured];
          if (vsExpanded in Node.States) and FullyVisible[Node] then
            System.Inc(FVisibleCount, Count); // Do this before a possible init of the sub-nodes in DoMeasureItem()

          // New nodes are by default always visible, so we don't need to check the visibility.
          while Remaining > 0 do
          begin
            Child := MakeNewNode;
            Child.SetIndex(Index);
            Child.SetPrevSibling(Node.LastChild);
            if Assigned(Node.LastChild) then
              Node.LastChild.SetNextSibling(Child);
            Child.SetParent(Node);
            Node.SetLastChild(Child);
            if Node.FirstChild = nil then
              Node.SetFirstChild(Child);
            System.Dec(Remaining);
            System.Inc(Index);

            if (toVariableNodeHeight in FOptions.MiscOptions) then
              GetNodeHeight(Child);
            Inc(NewHeight, Child.TotalHeight);
          end;

          if vsExpanded in Node.States then
            AdjustTotalHeight(Node, NewHeight, False);

          AdjustTotalCount(Node, Count, True);
          Node.SetChildCount(NewChildCount);
          if (FUpdateCount = 0) and (toAutoSort in FOptions.AutoOptions) and (FHeader.SortColumn > InvalidColumn) then
            Sort(Node, FHeader.SortColumn, FHeader.SortDirection, True);

          InvalidateCache;
        end//if NewChildCount > Node.ChildCount
        else
        begin
          // Nodes have to be deleted.
          Remaining := Node.ChildCount - NewChildCount;
          while Remaining > 0 do
          begin
            DeleteNode(Node.LastChild);
            System.Dec(Remaining);
          end;
        end;

        if FUpdateCount = 0 then
        begin
          ValidateCache;
          UpdateScrollBars(True);
          Invalidate;
        end;

        if Node = FRoot then
          StructureChange(nil, crChildAdded)
        else
          StructureChange(Node, crChildAdded);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetClipboardFormats(const Value: TClipboardFormats);

var
  I: Integer;

begin
  // Add string by string instead doing an Assign or AddStrings because the list may return -1 for
  // invalid entries which cause trouble for the standard implementation.
  FClipboardFormats.Clear;
  for I := 0 to Value.Count - 1 do
    FClipboardFormats.Add(Value[I]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetColors(const Value: TVTColors);

begin
  FColors.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetCheckImageKind(Value: TCheckImageKind);
begin
  if (Value < Low(Value)) or (Value> High(Value)) then
    Value := ckSystemDefault;
  // property is deprecated. See issue #622
  if FCheckImageKind <> Value then
  begin
    if FCheckImageKind = ckSystemDefault then
      FreeAndNil(FCheckImages);
    FCheckImageKind := Value;
    if Value = ckCustom then
      FCheckImages := FCustomCheckImages
    else if HandleAllocated then
      FCheckImages := CreateSystemImageSet();
    if HandleAllocated and (FUpdateCount = 0) and not (csLoading in ComponentState) then
      InvalidateRect(nil, False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetCustomCheckImages(const Value: TCustomImageList);

begin
  if FCustomCheckImages <> Value then
  begin
    if Assigned(FCustomCheckImages) then
    begin
      FCustomCheckImages.UnRegisterChanges(FCustomCheckChangeLink);
      FCustomCheckImages.RemoveFreeNotification(Self);
      // Reset the internal check image list reference too, if necessary.
      if FCheckImages = FCustomCheckImages then
        FCheckImages := nil;
    end;
    FCustomCheckImages := Value;
    if Assigned(FCustomCheckImages) then
    begin
      // If custom check images are assigned, we switch the property CheckImageKind to ckCustom so that they are actually used
      CheckImageKind := ckCustom;
      FCustomCheckImages.RegisterChanges(FCustomCheckChangeLink);
      FCustomCheckImages.FreeNotification(Self);
    end
    else
      CheckImageKind := ckSystemDefault;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetDefaultNodeHeight(Value: TDimension);

begin
  if Value = 0 then
    Value := cInitialDefaultNodeHeight;
  if FDefaultNodeHeight <> Value then
  begin
    Inc(FRoot.TotalHeight, Value - FDefaultNodeHeight);
    FRoot.SetNodeHeight(FRoot.NodeHeight + Value - FDefaultNodeHeight);
    FDefaultNodeHeight := Value;
    InvalidateCache;
    if (FUpdateCount = 0) and HandleAllocated and not (csLoading in ComponentState) then
    begin
      ValidateCache;
      UpdateScrollBars(True);
      ScrollIntoView(FFocusedNode, toCenterScrollIntoView in FOptions.SelectionOptions, True);
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetDisabled(Node: PVirtualNode; Value: Boolean);

begin
  if Assigned(Node) and (Value xor (vsDisabled in Node.States)) then
  begin
    if Value then
      Include(Node.States, vsDisabled)
    else
      Exclude(Node.States, vsDisabled);

    if FUpdateCount = 0 then
      InvalidateNode(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetDoubleBuffered(const Value: Boolean);
begin
  // empty by intention, we do our own buffering
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetDoubleBuffered: Boolean;
begin
  Result := True; // we do our own buffering
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetEmptyListMessage(const Value: string);

begin
  if Value <> EmptyListMessage then
  begin
    FEmptyListMessage := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetExpanded(Node: PVirtualNode; Value: Boolean);

begin
  if Assigned(Node) and (Node <> FRoot) and (Value xor (vsExpanded in Node.States)) then
    ToggleNode(Node);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetFocusedColumn(Value: TColumnIndex);

begin
  if (FFocusedColumn <> Value) and
     DoFocusChanging(FFocusedNode, FFocusedNode, FFocusedColumn, Value) then
  begin
    CancelEditNode;
    InvalidateColumn(FFocusedColumn);
    InvalidateColumn(Value);
    FFocusedColumn := Value;
    if Assigned(FFocusedNode) and not (toDisableAutoscrollOnFocus in FOptions.AutoOptions) then
    begin
      if ScrollIntoView(FFocusedNode, toCenterScrollIntoView in FOptions.SelectionOptions, True) then
        InvalidateNode(FFocusedNode);
    end;

    if Assigned(FDropTargetNode) then
      InvalidateNode(FDropTargetNode);

    DoFocusChange(FFocusedNode, FFocusedColumn);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetFocusedNode(Value: PVirtualNode);

var
  WasDifferent: Boolean;

begin
  WasDifferent := Value <> FFocusedNode;
  DoFocusNode(Value, True);
  // Do change event only if there was actually a change.
  if WasDifferent and (FFocusedNode = Value) then
    DoFocusChange(FFocusedNode, FFocusedColumn);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetFullyVisible(Node: PVirtualNode; Value: Boolean);

// This method ensures that a node is visible and all its parent nodes are expanded and also visible
// if Value is True. Otherwise the visibility flag of the node is reset but the expand state
// of the parent nodes stays untouched.

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameter');

  IsVisible[Node] := Value;
  if Value then
  begin
    repeat
      Node := Node.Parent;
      if Node = FRoot then
        Break;
      if not (vsExpanded in Node.States) then
        ToggleNode(Node);
      if not (vsVisible in Node.States) then
        IsVisible[Node] := True;
    until False;
  end;
  ScrollIntoView(Node, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetHasChildren(Node: PVirtualNode; Value: Boolean);

begin
  if Assigned(Node) and not (toReadOnly in FOptions.MiscOptions) then
  begin
    if Value then
      Include(Node.States, vsHasChildren)
    else
    begin
      Exclude(Node.States, vsHasChildren);
      DeleteChildren(Node);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetHeader(const Value: TVTHeader);

begin
  FHeader.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetHotNode(Value: PVirtualNode);

begin
  FCurrentHotNode := Value;
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetFiltered(Node: PVirtualNode; Value: Boolean);

// Sets the 'filtered' flag of the given node according to Value and updates all dependent states.

var
  NeedUpdate: Boolean;

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameter.');

  // Initialize the node if necessary as this might change the filtered state.
  if not (vsInitialized in Node.States) then
    InitNode(Node);

  if Value <> (vsFiltered in Node.States) then
  begin
    InterruptValidation;
    NeedUpdate := False;
    if Value then
    begin
      Include(Node.States, vsFiltered);
      if not (toShowFilteredNodes in FOptions.PaintOptions) then
      begin
        if (vsInitializing in Node.States) and not (vsHasChildren in Node.States) then
          AdjustTotalHeight(Node, 0, False)
        else
          AdjustTotalHeight(Node, -NodeHeight[Node], True);
        if FullyVisible[Node] then
        begin
          System.Dec(FVisibleCount);
          NeedUpdate := True;
        end;
        if FocusedNode = Node then
          FocusedNode := nil;
      end;

      if FUpdateCount = 0 then
        DetermineHiddenChildrenFlag(Node.Parent)
      else
        Include(FStates, tsUpdateHiddenChildrenNeeded);
    end
    else
    begin
      Exclude(Node.States, vsFiltered);
      if not (toShowFilteredNodes in FOptions.PaintOptions) then
      begin
        AdjustTotalHeight(Node, NodeHeight[Node], True);
        if FullyVisible[Node] then
        begin
          System.Inc(FVisibleCount);
          NeedUpdate := True;
        end;
      end;

      if vsVisible in Node.States then
        // Update the hidden children flag of the parent.
        // Since this node is now visible we simply have to remove the flag.
        Exclude(Node.Parent.States, vsAllChildrenHidden);
    end;

    InvalidateCache;
    if NeedUpdate and (FUpdateCount = 0) then
    begin
      ValidateCache;
      UpdateScrollBars(True);
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TBaseVirtualTree.SetImages(const Value: TCustomImageList);

begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(Self);
    end;
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetIndent(Value: TDimension);

begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    if not (csLoading in ComponentState) and (FUpdateCount = 0) and HandleAllocated then
    begin
      UpdateScrollBars(True);
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetLineMode(const Value: TVTLineMode);

begin
  if FLineMode <> Value then
  begin
    FLineMode := Value;
    if HandleAllocated and not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetLineStyle(const Value: TVTLineStyle);

begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    if not (csLoading in ComponentState) then
    begin
      PrepareBitmaps(False, True);
      if HandleAllocated then
        Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetMargin(Value: TDimension);

begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    if HandleAllocated and not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetMultiline(Node: PVirtualNode; const Value: Boolean);

begin
  if Assigned(Node) and (Node <> FRoot) then
    if Value <> (vsMultiline in Node.States) then
    begin
      if Value then
        Include(Node.States, vsMultiline)
      else
        Exclude(Node.States, vsMultiline);

      if FUpdateCount = 0 then
        InvalidateNode(Node);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetNodeAlignment(const Value: TVTNodeAlignment);

begin
  if FNodeAlignment <> Value then
  begin
    FNodeAlignment := Value;
    if HandleAllocated and not (csReading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetNodeData(pNode: PVirtualNode; pUserData: Pointer);

  // Can be used to set user data of a PVirtualNode with the size of a pointer, useful for setting
  // A pointer to a record or a reference to a class instance.

var
  NodeData: PPointer;
begin
  // Check if there is initial user data and there is also enough user data space allocated.
  Assert(FNodeDataSize >= SizeOf(Pointer), Self.Classname + ': Cannot set initial user data because there is not enough user data space allocated.');
  NodeData := pNode.GetData();
  NodeData^ := pUserData;
  Include(pNode.States, vsOnFreeNodeCallRequired);
end;

procedure TBaseVirtualTree.SetNodeData<T>(pNode: PVirtualNode; pUserData: T);

  // Can be used to set user data of a PVirtualNode to a class instance.

begin
  pNode.SetData<T>(pUserData);
end;

procedure TBaseVirtualTree.SetNodeData(pNode: PVirtualNode; const pUserData: IInterface);

  // Can be used to set user data of a PVirtualNode to a class instance,
  // will take care about reference counting.

begin
  pNode.SetData(pUserData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetNodeDataSize(Value: Integer);

var
  LastRootCount: Cardinal;

begin
  if Value < -1 then
    Value := -1;
  if FNodeDataSize <> Value then
  begin
    FNodeDataSize := Value;
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    begin
      LastRootCount := FRoot.ChildCount;
      Clear;
      SetRootNodeCount(LastRootCount);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetNodeHeight(Node: PVirtualNode; Value: TNodeHeight);

var
  Difference: TDimension;

begin
  Assert(Assigned(Node), 'SetNodeHeight() cannot be called with Node = nil');
  Assert((Node <> FRoot), 'SetNodeHeight() cannot be called for the root node FRoot');
  if (Node.NodeHeight <> Value) then
  begin
    Difference := Value - Node.NodeHeight;
    Node.SetNodeHeight(Value);

    // If the node is effectively filtered out, nothing else has to be done, as it is not visible anyway.
    if not IsEffectivelyFiltered[Node] then
    begin
      AdjustTotalHeight(Node, Difference, True);

      // If an edit operation is currently active then update the editors boundaries as well.
      UpdateEditBounds;

      InvalidateCache;
      // Stay away from touching the node cache while it is being validated.
      if not (tsValidating in FStates) and FullyVisible[Node] then
      begin
        if (FUpdateCount = 0) and ([tsPainting, tsSizing] * FStates = []) then
        begin
          ValidateCache;
          InvalidateToBottom(Node);
          UpdateScrollBars(True);
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetNodeParent(Node: PVirtualNode; const Value: PVirtualNode);

begin
  if Assigned(Node) and Assigned(Value) and (Node.Parent <> Value) then
    MoveTo(Node, Value, amAddChildLast, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetOffsetX(const Value: TDimension);

begin
  DoSetOffsetXY(Point(Value, FOffsetY), DefaultScrollUpdateFlags);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetOffsetXY(const Value: TPoint);

begin
  DoSetOffsetXY(Value, DefaultScrollUpdateFlags);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetOffsetY(const Value: TDimension);

begin
  DoSetOffsetXY(Point(FOffsetX, Value), DefaultScrollUpdateFlags);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetOnPrepareButtonImages(const Value: TVTPrepareButtonImagesEvent);
begin
  FOnPrepareButtonImages := Value;
  PrepareBitmaps(True, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetOptions(const Value: TCustomVirtualTreeOptions);

begin
  FOptions.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetRangeX(value: TDimension);
begin
  FRangeX := value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetRootNodeCount(Value: Cardinal);

begin
  // Don't set the root node count until all other properties (in particular the OnInitNode event) have been set.
  if csLoading in ComponentState then
  begin
    FRoot.SetChildCount(Value);
    DoStateChange([tsNeedRootCountUpdate]);
  end
  else
    if FRoot.ChildCount <> Value then
    begin
      BeginUpdate;
      InterruptValidation;
      SetChildCount(FRoot, Value);
      EndUpdate;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetScrollBarOptions(Value: TScrollBarOptions);

begin
  FScrollBarOptions.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetSearchOption(const Value: TVTIncrementalSearch);

begin
  if FIncrementalSearch <> Value then
  begin
    FIncrementalSearch := Value;
    if FIncrementalSearch = isNone then
    begin
      StopTimer(SearchTimer);
      FSearchBuffer := '';
      FLastSearchNode := nil;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetSelected(Node: PVirtualNode; Value: Boolean);

begin
  if not FSelectionLocked and Assigned(Node) and (Node <> FRoot) and (Value xor (vsSelected in Node.States)) then
  begin
    if Value then
    begin
      if FSelectionCount = 0 then
        FRangeAnchor := Node
      else begin
        if not (toMultiSelect in FOptions.SelectionOptions) then
          ClearSelection;
        if FRangeAnchor = nil then
          FRangeAnchor := Node;
      end;

      AddToSelection(Node, True);

      if not (toMultiSelect in FOptions.SelectionOptions) then
        FocusedNode := GetFirstSelected; // if only one node can be selected, make sure the focused node changes with the selected node
      // Make sure there is a valid column selected (if there are columns at all).
      if ((FFocusedColumn < 0) or not (coVisible in FHeader.Columns[FFocusedColumn].Options)) and
        (FHeader.MainColumn > NoColumn) then
        if ([coVisible, coAllowFocus] *  FHeader.Columns[FHeader.MainColumn].Options = [coVisible, coAllowFocus]) then
          FFocusedColumn := FHeader.MainColumn
        else
          FFocusedColumn := FHeader.Columns.GetFirstVisibleColumn(True);
    end
    else
    begin
      RemoveFromSelection(Node);
      if FSelectionCount = 0 then
        ResetRangeAnchor;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetSelectionCurveRadius(const Value: Cardinal);

begin
  if FSelectionCurveRadius <> Value then
  begin
    FSelectionCurveRadius := Value;
    if HandleAllocated and not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetStateImages(const Value: TCustomImageList);

begin
  if FStateImages <> Value then
  begin
    if Assigned(FStateImages) then
    begin
      FStateImages.UnRegisterChanges(FStateChangeLink);
      FStateImages.RemoveFreeNotification(Self);
    end;
    FStateImages := Value;
    if Assigned(FStateImages) then
    begin
      FStateImages.RegisterChanges(FStateChangeLink);
      FStateImages.FreeNotification(Self);
    end;
    if HandleAllocated and not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetTextMargin(Value: TDimension);

begin
  if FTextMargin <> Value then
  begin
    FTextMargin := Value;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetTopNode(Node: PVirtualNode);

var
  R: TRect;
  Run: PVirtualNode;

begin
  if Assigned(Node) then
  begin
    // make sure all parents of the node are expanded
    Run := Node.Parent;
    while Run <> FRoot do
    begin
      if not (vsExpanded in Run.States) then
        ToggleNode(Run);
      Run := Run.Parent;
    end;
    R := GetDisplayRect(Node, FHeader.MainColumn, True);
    SetOffsetY(FOffsetY - R.Top);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetUpdateState(Updating: Boolean);

begin
  // The check for visibility is necessary otherwise the tree is automatically shown when
  // updating is allowed. As this happens internally the VCL does not get notified and
  // still assumes the control is hidden. This results in weird "cannot focus invisible control" errors.
  if Visible and HandleAllocated and (FUpdateCount = 0) then
    SendWM_SETREDRAW(Updating);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetVerticalAlignment(Node: PVirtualNode; Value: Byte);

begin
  if Value > 100 then
    Value := 100;
  if Node.Align <> Value then
  begin
    Node.Align := Value;
    if FullyVisible[Node] and not IsEffectivelyFiltered[Node] then
      InvalidateNode(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetVisible(Node: PVirtualNode; Value: Boolean);

// Sets the visibility style of the given node according to Value.

var
  NeedUpdate: Boolean;

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameter.');

  if Value <> (vsVisible in Node.States) then
  begin
    InterruptValidation;
    NeedUpdate := False;
    if Value then
    begin
      Include(Node.States, vsVisible);
      if vsExpanded in Node.Parent.States then
        AdjustTotalHeight(Node.Parent, Node.TotalHeight, True);
      if VisiblePath[Node] then
      begin
        System.Inc(FVisibleCount, CountVisibleChildren(Node) + Cardinal(IfThen(IsEffectivelyVisible[Node], 1)));
        NeedUpdate := True;
      end;

      // Update the hidden children flag of the parent.
      // Since this node is now visible we simply have to remove the flag.
      if not IsEffectivelyFiltered[Node] then
        Exclude(Node.Parent.States, vsAllChildrenHidden);
    end
    else
    begin
      if vsExpanded in Node.Parent.States then
        AdjustTotalHeight(Node.Parent, -Node.TotalHeight, True);
      if VisiblePath[Node] then
      begin
        System.Dec(FVisibleCount, CountVisibleChildren(Node) + Cardinal(IfThen(IsEffectivelyVisible[Node], 1)));
        NeedUpdate := True;
      end;
      Exclude(Node.States, vsVisible);

      if FUpdateCount = 0 then
        DetermineHiddenChildrenFlag(Node.Parent)
      else
        Include(FStates, tsUpdateHiddenChildrenNeeded);
    end;

    InvalidateCache;
    if NeedUpdate and (FUpdateCount = 0) then
    begin
      ValidateCache;
      UpdateScrollBars(True);
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetVisiblePath(Node: PVirtualNode; Value: Boolean);

// If Value is True then all parent nodes of Node are expanded.

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameter.');

  if Value then
  begin
    repeat
      Node := Node.Parent;
      if Node = FRoot then
        Break;
      if not (vsExpanded in Node.States) then
        ToggleNode(Node);
    until False;
  end;
end;

// ----------------------------------------------------------------------------------------------------------------------
procedure TBaseVirtualTree.PrepareBackGroundPicture(Source: TVTBackground;
  DrawingBitmap: TBitmap; DrawingBitmapWidth: TDimension; DrawingBitmapHeight: TDimension; ABkgcolor: TColor);
const
  DST = $00AA0029; // Ternary Raster Operation - Destination unchanged

  // fill background will work for transparent images and
  // will not disturb non-transparent ones
  procedure FillDrawBitmapWithBackGroundColor;
  begin
    DrawingBitmap.Canvas.Brush.Color := ABkgcolor;
    DrawingBitmap.Canvas.FillRect(Rect(0, 0, DrawingBitmap.Width, DrawingBitmap.Height));
  end;

begin
  DrawingBitmap.SetSize(DrawingBitmapWidth, DrawingBitmapHeight);

  if (Source.Graphic is TBitmap) and
     (FBackGroundImageTransparent or Source.Bitmap.TRANSPARENT)
  then
  begin
    FillDrawBitmapWithBackGroundColor;
    MaskBlt(DrawingBitmap.Canvas.Handle, 0, 0, Source.Width, Source.Height,
        Source.Bitmap.Canvas.Handle, 0, 0, Source.Bitmap.MaskHandle, 0, 0,
        MakeROP4(DST, SRCCOPY));
  end
  else
  begin
    // Similar to TImage's Transparent property behavior, we don't want
    // to draw transparent if the following flag is OFF.
    if FBackGroundImageTransparent then
      FillDrawBitmapWithBackGroundColor;
    DrawingBitmap.Canvas.Draw(0, 0, Source.Graphic);
  end
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.StaticBackground(Source: TVTBackground; Target: TCanvas; OffsetPosition: TPoint; R: TRect; aBkgColor: TColor);

// Draws the given source graphic so that it stays static in the given rectangle which is relative to the target bitmap.
// The graphic is aligned so that it always starts at the upper left corner of the target canvas.
// Offset gives the position of the target window as a possible superordinated surface.

const
  DST = $00AA0029; // Ternary Raster Operation - Destination unchanged

var
  PicRect: TRect;
  AreaRect: TRect;
  DrawRect: TRect;
  DrawingBitmap: TBitmap;
begin
  DrawingBitmap := TBitmap.Create;
  try
    // clear background
    Target.Brush.Color := aBkgColor;
    Target.FillRect(R);

  // Picture rect in relation to client viewscreen.
  PicRect := Rect(FBackgroundOffsetX, FBackgroundOffsetY, FBackgroundOffsetX + Source.Width, FBackgroundOffsetY + Source.Height);

  // Area to be draw in relation to client viewscreen.
  AreaRect := Rect(OffsetPosition.X + R.Left, OffsetPosition.Y + R.Top, OffsetPosition.X + R.Right, OffsetPosition.Y + R.Bottom);

  // If picture falls in AreaRect, return intersection (DrawRect).
  if IntersectRect(DrawRect, PicRect, AreaRect) then
  begin
      PrepareBackGroundPicture(Source, DrawingBitmap, Source.Width, Source.Height, aBkgColor);
      // copy image to destination
      BitBlt(Target.Handle, DrawRect.Left - OffsetPosition.X, DrawRect.Top - OffsetPosition.Y, (DrawRect.Right - OffsetPosition.X) - (DrawRect.Left - OffsetPosition.X),
      (DrawRect.Bottom - OffsetPosition.Y) - (DrawRect.Top - OffsetPosition.Y) + R.Top, DrawingBitmap.Canvas.Handle, DrawRect.Left - PicRect.Left, DrawRect.Top - PicRect.Top,
        SRCCOPY);
    end;
  finally
    DrawingBitmap.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.StopTimer(ID: Integer);

begin
  if HandleAllocated then
    KillTimer(Handle, ID);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetWindowTheme(const Theme: string);

begin
  FChangingTheme := True;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

//used by TCustomVirtualTreeOptions
procedure TBaseVirtualTree.SetVisibleCount(value : Cardinal);
begin
  FVisibleCount := value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.TileBackground(Source: TVTBackground; Target: TCanvas; Offset: TPoint; R: TRect; aBkgColor: TColor);

// Draws the given source graphic so that it tiles into the given rectangle which is relative to the target bitmap.
// The graphic is aligned so that it always starts at the upper left corner of the target canvas.
// Offset gives the position of the target window in an possible superordinated surface.

var
  SourceX,
  SourceY,
  TargetX,
  DeltaY: TDimension;
  DrawingBitmap: TBitmap;
begin
  DrawingBitmap := TBitmap.Create;
  try
    PrepareBackGroundPicture(Source, DrawingBitmap, Source.Width, Source.Height, aBkgColor);
    with Target do
    begin
      SourceY := (R.Top + Offset.Y + FBackgroundOffsetY) mod Source.Height;
      // Always wrap the source coordinates into positive range.
      if SourceY < 0 then
        SourceY := Source.Height + SourceY;

      // Tile image vertically until target rect is filled.
      while R.Top < R.Bottom do
      begin
        SourceX := (R.Left + Offset.X + FBackgroundOffsetX) mod Source.Width;
        // always wrap the source coordinates into positive range
        if SourceX < 0 then
          SourceX := Source.Width + SourceX;

        TargetX := R.Left;
        // height of strip to draw
        DeltaY := Min(R.Bottom - R.Top, Source.Height - SourceY);

        // tile the image horizontally
        while TargetX < R.Right do
        begin
          BitBlt(Handle, TargetX, R.Top, Min(R.Right - TargetX, Source.Width - SourceX), DeltaY,
            DrawingBitmap.Canvas.Handle, SourceX, SourceY, SRCCOPY);
          Inc(TargetX, Source.Width - SourceX);
          SourceX := 0;
        end;
        Inc(R.Top, Source.Height - SourceY);
        SourceY := 0;
      end;
    end;
  finally
    DrawingBitmap.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ToggleCallback(Step, StepSize: Integer; Data: Pointer): Boolean;

var
  Column: TColumnIndex;
  Run: TRect;
  SecondaryStepSize: Integer;

  //--------------- local functions -------------------------------------------

  procedure EraseLine;

  var
    LocalBrush: TBrush;

  begin
    with TToggleAnimationData(Data^), FHeader.Columns do
    begin
      // Iterate through all columns and erase background in their local color.
      // LocalBrush is a brush in the color of the particular column.
      Column := GetFirstVisibleColumn;
      while (Column > InvalidColumn) and (Run.Left < ClientWidth) do
      begin
        GetColumnBounds(Column, Run.Left, Run.Right);
        if coParentColor in Items[Column].Options then
        begin
          DC.Brush := Brush;
          DC.FillRect(Run);
        end
        else
        begin
          LocalBrush := TBrush.Create;
          if VclStyleEnabled then
            LocalBrush.Color := FColors.BackGroundColor
          else
            LocalBrush.Color := Items[Column].Color;
          DC.Brush := LocalBrush;
          DC.FillRect(Run);
          LocalBrush.Free;
        end;
        Column := GetNextVisibleColumn(Column);
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure DoScrollUp(DC: TCanvas; Brush: TBrush; Area: TRect; Steps: Integer);

  begin
    ScrollDC(DC.Handle, 0, -Steps, Area, Area, 0, nil);

    if Step = 0 then
      if not FHeader.UseColumns then
      begin
        DC.Brush := Brush;
        DC.FillRect(Rect(Area.Left, Area.Bottom - Steps - 1, Area.Right, Area.Bottom));
      end
      else
      begin
        Run := Rect(Area.Left, Area.Bottom - Steps - 1, Area.Right, Area.Bottom);
        EraseLine;
      end;
  end;

  //---------------------------------------------------------------------------

  procedure DoScrollDown(DC: TCanvas; Brush: TBrush; Area: TRect; Steps: Integer);

  begin
    ScrollDC(DC.Handle, 0, Steps, Area, Area, 0, nil);

    if Step = 0 then
      if not FHeader.UseColumns then
      begin
        DC.Brush := Brush;
        DC.FillRect(Rect(Area.Left, Area.Top, Area.Right, Area.Top + Steps + 1));
      end
      else
      begin
        Run := Rect(Area.Left, Area.Top, Area.Right, Area.Top + Steps + 1);
        EraseLine;
      end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  Result := True;
  if StepSize > 0 then
  begin
    SecondaryStepSize := 0;
    with TToggleAnimationData(Data^) do
    begin
      if Mode1 <> tamNoScroll then
      begin
        if Mode1 = tamScrollUp then
          DoScrollUp(DC, Brush, R1, StepSize)
        else
          DoScrollDown(DC, Brush, R1, StepSize);

        if (Mode2 <> tamNoScroll) and (ScaleFactor > 0) then
        begin
          // As this routine is able to scroll two independent areas at once, the missing StepSize is
          // computed in that case. To ensure the maximal accuracy the rounding error is accumulated.
          SecondaryStepSize := Round((StepSize + MissedSteps) * ScaleFactor);
          MissedSteps := MissedSteps + StepSize * ScaleFactor - SecondaryStepSize;
        end;
      end
      else
        SecondaryStepSize := StepSize;

      if Mode2 <> tamNoScroll then
        if Mode2 = tamScrollUp then
          DoScrollUp(DC, Brush, R2, SecondaryStepSize)
        else
          DoScrollDown(DC, Brush, R2, SecondaryStepSize);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMColorChange(var Message: TMessage);

begin
  if not (csLoading in ComponentState) then
  begin
    PrepareBitmaps(True, False);
    if HandleAllocated then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMCtl3DChanged(var Message: TMessage);

begin
  inherited;
  if FBorderStyle = bsSingle then
    RecreateWnd;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMBiDiModeChanged(var Message: TMessage);

begin
  inherited;

  if UseRightToLeftAlignment then
    FEffectiveOffsetX := FRangeX - ClientWidth + FOffsetX
  else
    FEffectiveOffsetX := -FOffsetX;
  if FEffectiveOffsetX < 0 then
    FEffectiveOffsetX := 0;

  if toAutoBidiColumnOrdering in FOptions.AutoOptions then
    TVirtualTreeColumnsCracker(FHeader.Columns).ReorderColumns(UseRightToLeftAlignment);
  FHeader.Invalidate(nil);
end;

procedure TBaseVirtualTree.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  if VclStyleEnabled and (seBorder in StyleElements) then
    RecreateWnd;
end;

procedure TBaseVirtualTree.CMParentDoubleBufferedChange(var Message: TMessage);
begin
  // empty by intention, we do our own buffering
end;

procedure TBaseVirtualTree.CMStyleChanged(var Message: TMessage);
begin
  VclStyleChanged;
  RecreateWnd;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMDenySubclassing(var Message: TMessage);

// If a Windows XP Theme Manager component is used in the application it will try to subclass all controls which do not
// explicitly deny this. Virtual Treeview knows how to handle XP themes so it does not need subclassing.

begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMDrag(var Message: TCMDrag);

var
  S: TObject;
  ShiftState: Integer;
  P: TPoint;
  Formats: TFormatArray;
  Effect: Integer;

begin
  with Message, DragRec^ do
  begin
    S := Source;
    Formats := nil;

    // Let the ancestor handle dock operations.
    if S is TDragDockObject then
      inherited
    else
    begin
      // We need an extra check for the control drag object as there might be other objects not derived from
      // this class (e.g. TActionDragObject).
      if not (tsUserDragObject in FStates) and (S is TBaseDragControlObject) then
        S := (S as TBaseDragControlObject).Control;
      case DragMessage of
        dmDragEnter, dmDragLeave, dmDragMove:
          begin
            if DragMessage = dmDragEnter then
              DoStateChange([tsVCLDragging]);
            if DragMessage = dmDragLeave then
              DoStateChange([tsVCLDragFinished], [tsVCLDragging]);

            if DragMessage = dmDragMove then
              with ScreenToClient(Pos) do
                DoAutoScroll(X, Y);

            ShiftState := 0;
            // Alt key will be queried by the KeysToShiftState function in DragOver.
            if GetKeyState(VK_SHIFT) < 0 then
              ShiftState := ShiftState or MK_SHIFT;
            if GetKeyState(VK_CONTROL) < 0 then
              ShiftState := ShiftState or MK_CONTROL;

            // Allowed drop effects are simulated for VCL dd.
            Effect := DROPEFFECT_MOVE or DROPEFFECT_COPY;
            DragOver(S, ShiftState, TDragState(DragMessage), Pos, Effect);
            FLastVCLDragTarget := FDropTargetNode;
            FVCLDragEffect := Effect;
            if (DragMessage = dmDragLeave) and Assigned(FDropTargetNode) then
            begin
              InvalidateNode(FDropTargetNode);
              FDropTargetNode := nil;
            end;
            Result := LRESULT(Effect);
          end;
        dmDragDrop:
          begin
            ShiftState := 0;
            // Alt key will be queried by the KeysToShiftState function in DragOver
            if GetKeyState(VK_SHIFT) < 0 then
              ShiftState := ShiftState or MK_SHIFT;
            if GetKeyState(VK_CONTROL) < 0 then
              ShiftState := ShiftState or MK_CONTROL;

            // allowed drop effects are simulated for VCL dd,
            // replace target node with cached node from other VCL dd messages
            if Assigned(FDropTargetNode) then
              InvalidateNode(FDropTargetNode);
            FDropTargetNode := FLastVCLDragTarget;
            P := Point(Pos.X, Pos.Y);
            P := ScreenToClient(P);
            try
              DoDragDrop(S, nil, Formats, KeysToShiftState(ShiftState), P, FVCLDragEffect, FLastDropMode);
            finally
              if Assigned(FDropTargetNode) then
              begin
                InvalidateNode(FDropTargetNode);
                FDropTargetNode := nil;
              end;
            end;
          end;
        dmFindTarget:
          begin
            Result := LRESULT(ControlAtPos(ScreenToClient(Pos), False));
            if Result = 0 then
              Result := LRESULT(Self);

            // This is a reliable place to check whether VCL drag has
            // really begun.
            if tsVCLDragPending in FStates then
              DoStateChange([tsVCLDragging], [tsVCLDragPending, tsEditPending, tsClearPending]);
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMEnabledChanged(var Message: TMessage);

begin
  inherited;

  // Need to invalidate the non-client area as well, since the header must be redrawn too.
  if csDesigning in ComponentState then
    RedrawWindow(nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE or RDW_NOCHILDREN);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMFontChanged(var Message: TMessage);

var
  HeaderMessage: TMessage;

begin
  inherited;
  AutoScale();

  HeaderMessage.Msg := CM_PARENTFONTCHANGED;
  HeaderMessage.WParam := 0;
  HeaderMessage.LParam := 0;
  HeaderMessage.Result := 0;
  TVTHeaderCracker(FHeader).HandleMessage(HeaderMessage);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMHintShow(var Message: TCMHintShow);

// Determines hint message (tooltip) and out-of-hint rect.
// Note: A special handling is needed here because we cannot pass wide strings back to the caller.
//       I had to introduce the hint data record anyway so we can use this to pass the hint string.
//       We still need to set a dummy hint string in the message to make the VCL showing the hint window.

var
  NodeRect: TRect;
  SpanColumn,
  Dummy,
  ColLeft,
  ColRight: Integer;
  HitInfo: THitInfo;
  ShowOwnHint: Boolean;
  IsFocusedOrEditing: Boolean;
  ParentForm: TCustomForm;
  BottomRightCellContentMargin: TPoint;
  HintKind: TVTHintKind;
begin
  with Message do
  begin
    Result := 1;

    if PtInRect(FLastHintRect, HintInfo.CursorPos) then
      Exit;

    // Determine node for which to show hint/tooltip.
    with HintInfo^ do
      GetHitTestInfoAt(CursorPos.X, CursorPos.Y, True, HitInfo, []);

    // Make sure a hint is only shown if the tree or at least its parent form is active.
    // Active editing is ok too as long as we don't want the hint for the current edit node.
    if IsEditing then
      IsFocusedOrEditing := HitInfo.HitNode <> FFocusedNode
    else
    begin
      IsFocusedOrEditing := Focused;
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) then
        IsFocusedOrEditing := ParentForm.Focused or Application.Active;
    end;

    if (GetCapture = 0) and ShowHint and not (Dragging or IsMouseSelecting) and ([tsScrolling] * FStates = []) and
      (FHeader.States = []) and IsFocusedOrEditing then
    begin
      with HintInfo^ do
      begin
        Result := 0;
        ShowOwnHint := False;

        //workaround for issue #291
        //it duplicates parts of the following code and code in TVirtualTreeHintWindow
        HintStr := '';
        if FHeader.UseColumns and (hoShowHint in FHeader.Options) and FHeader.InHeader(CursorPos) then
        begin
          CursorRect := FHeaderRect;
          // Convert the cursor rectangle into real client coordinates.
          OffsetRect(CursorRect, 0, -Integer(FHeader.Height));
          HitInfo.HitColumn :=  TVirtualTreeColumnsCracker(FHeader.Columns).GetColumnAndBounds(CursorPos, CursorRect.Left, CursorRect.Right);
          if (HitInfo.HitColumn > NoColumn) and not (csLButtonDown in ControlState) and
            (FHeader.Columns[HitInfo.HitColumn].Hint <> '') then
            HintStr := FHeader.Columns[HitInfo.HitColumn].Hint;
        end
        else
        if HintMode = hmDefault then
          HintStr := GetShortHint(Hint)
        else
        if Assigned(HitInfo.HitNode) and (HitInfo.HitColumn > InvalidColumn) then
        begin
          if HintMode = hmToolTip then
            HintStr := DoGetNodeToolTip(HitInfo.HitNode, HitInfo.HitColumn, fHintData.LineBreakStyle)
          else
            HintStr := DoGetNodeHint(HitInfo.HitNode, HitInfo.HitColumn, fHintData.LineBreakStyle);
        end;

        // First check whether there is a header hint to show.
        if FHeader.UseColumns and (hoShowHint in FHeader.Options) and FHeader.InHeader(CursorPos) then
        begin
          CursorRect := FHeaderRect;
          // Convert the cursor rectangle into real client coordinates.
          OffsetRect(CursorRect, 0, -Integer(FHeader.Height));
          HitInfo.HitColumn := TVirtualTreeColumnsCracker(FHeader.Columns).GetColumnAndBounds(CursorPos, CursorRect.Left, CursorRect.Right);
          // align the vertical hint position on the bottom bound of the header, but
          // avoid overlapping of mouse cursor and hint
          HintPos.Y := Max(HintPos.Y, ClientToScreen(Point(0, CursorRect.Bottom)).Y);
          // Note: the test for the left mouse button in ControlState might cause problems whenever the VCL does not
          //       realize when the button is released. This, for instance, happens when doing OLE drag'n drop and
          //       cancel this with ESC.
          if (HitInfo.HitColumn > NoColumn) and not (csLButtonDown in ControlState) then
          begin
            HintStr := FHeader.Columns[HitInfo.HitColumn].Hint;
            if HintStr = '' then
              with FHeader.Columns[HitInfo.HitColumn] do
              begin
                if (2 * FMargin + CaptionWidth + 1) >= Width then
                  HintStr := CaptionText;
              end;
            if HintStr <> '' then
              ShowOwnHint := True
            else
              Result := 1;
          end
          else
            Result := 1;
        end
        else
        begin
          // Default mode is handled as would the tree be a usual VCL control (no own hint window necessary).
          if FHintMode = hmDefault then
            HintStr := GetShortHint(Hint)
          else
          begin
            if Assigned(HitInfo.HitNode) and (HitInfo.HitColumn > InvalidColumn) then
            begin
              // An owner-draw tree should only display a hint when at least
              // its OnGetHintSize event handler is assigned.
              DoGetHintKind(HitInfo.HitNode, HitInfo.HitColumn, HintKind);
              FHintData.HintRect := Rect(0, 0, 0, 0);
              if (HintKind = vhkOwnerDraw) then
              begin
                DoGetHintSize(HitInfo.HitNode, HitInfo.HitColumn, FHintData.HintRect);
                ShowOwnHint := not IsRectEmpty(FHintData.HintRect);
              end
              else
                // For trees displaying text hints, a decision about showing the hint or not is based
                // on the hint string (if it is empty then no hint is shown).
                ShowOwnHint := True;

              if ShowOwnHint then
              begin
                if HitInfo.HitColumn > NoColumn then
                begin
                  FHeader.Columns.GetColumnBounds(HitInfo.HitColumn, ColLeft, ColRight);
                  // The right column border might be extended if column spanning is enabled.
                  if toAutoSpanColumns in FOptions.AutoOptions then
                  begin
                    SpanColumn := HitInfo.HitColumn;
                    repeat
                      Dummy := FHeader.Columns.GetNextVisibleColumn(SpanColumn);
                      if (Dummy = InvalidColumn) or not ColumnIsEmpty(HitInfo.HitNode, Dummy) then
                        Break;
                      SpanColumn := Dummy;
                    until False;
                    if SpanColumn <> HitInfo.HitColumn then
                      FHeader.Columns.GetColumnBounds(SpanColumn, Dummy, ColRight);
                  end;
                end
                else
                begin
                  ColLeft := 0;
                  ColRight := ClientWidth;
                end;

                if FHintMode <> hmTooltip then
                begin
                  // Node specific hint text.
                  CursorRect := GetDisplayRect(HitInfo.HitNode, HitInfo.HitColumn, False);
                  CursorRect.Left := ColLeft;
                  CursorRect.Right := ColRight;
                  // Align the vertical hint position on the bottom bound of the node, but
                  // avoid overlapping of mouse cursor and hint.
                  HintPos.Y := Max(HintPos.Y, ClientToScreen(CursorRect.BottomRight).Y) + ScaledPixels(2);
                end
                else
                begin
                  // Tool tip to show. This means the full caption of the node must be displayed.
                  if vsMultiline in HitInfo.HitNode.States then
                  begin
                    if hiOnItemLabel in HitInfo.HitPositions then
                    begin
                      ShowOwnHint := True;
                      NodeRect := GetDisplayRect(HitInfo.HitNode, HitInfo.HitColumn, True, False);
                    end
                    else
                      ShowOwnHint := False;
                  end
                  else
                  begin
                    NodeRect := GetDisplayRect(HitInfo.HitNode, HitInfo.HitColumn, True, True, True);
                    BottomRightCellContentMargin := DoGetCellContentMargin(HitInfo.HitNode, HitInfo.HitColumn, ccmtBottomRightOnly);

                    ShowOwnHint := (HitInfo.HitColumn > InvalidColumn) and PtInRect(NodeRect, CursorPos) and
                      (CursorPos.X <= ColRight) and (CursorPos.X >= ColLeft) and
                      (
                        // Show hint also if the node text is partially out of the client area.
                        // "ColRight - 1", since the right column border is not part of this cell.
                        ( (NodeRect.Right + BottomRightCellContentMargin.X) > Min(ColRight - 1, ClientWidth) ) or
                        (NodeRect.Left < Max(ColLeft, 0)) or
                        ( (NodeRect.Bottom + BottomRightCellContentMargin.Y) > ClientHeight ) or
                        (NodeRect.Top < 0)
                      );
                  end;

                  if ShowOwnHint then
                  begin
                    // Node specific hint text given will be retrieved when needed.
                    HintPos := ClientToScreen(Point(NodeRect.Left, NodeRect.Top));
                    CursorRect := NodeRect;
                  end
                  else
                    // nothing to show
                    Result := 1;
                end;
              end
              else
                Result := 1; // Avoid hint if this is a draw tree returning an empty hint rectangle.
            end
            else
            begin
              // No node so fall back to control's hint (if indicated) or show nothing.
              if FHintMode = hmHintAndDefault then
              begin
                HintStr := GetShortHint(Hint);

                // Fix for the problem: Default Hint once shown stayed even when
                // node hint was to be displayed. The reason was that CursorRect
                // was for the full client area. Now reducing it to remove the
                // columns from it.
                if BidiMode = bdLeftToRight then
                  CursorRect.Left := Header.Columns.TotalWidth
                else
                  CursorRect.right := CursorRect.right - Header.Columns.TotalWidth;

                if Length(HintStr) = 0 then
                  Result := 1
                else
                  ShowOwnHint := True;
              end
              else
                Result := 1;
            end;
          end;
        end;

        // Set our own hint window class and prepare structure to be passed to the hint window.
        if ShowOwnHint and (Result = 0) then
        begin
          HintWindowClass := GetHintWindowClass;
          FHintData.HintText := HintStr;
          FHintData.Tree := Self;
          FHintData.Column := HitInfo.HitColumn;
          FHintData.Node := HitInfo.HitNode;
          FLastHintRect := CursorRect;
          HintData := @FHintData;
        end
        else
          FLastHintRect := Rect(0, 0, 0, 0);
      end;

      // Remind that a hint is about to show.
      if Result = 0 then
        DoStateChange([tsHint])
      else
        DoStateChange([], [tsHint]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMHintShowPause(var Message: TCMHintShowPause);

// Tells the application that the tree (and only the tree) does not want a delayed tool tip.
// Normal hints / header hints use the default delay (except for the first time).

  begin
  if ShowHint and (FHintMode = hmToolTip) then
    Message.Pause^ := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMMouseEnter(var Message: TMessage);
begin
  DoMouseEnter();
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMMouseLeave(var Message: TMessage);

var
  LeaveStates: TVirtualTreeStates;

begin
  // Reset the last used hint rectangle in case the mouse enters the window within the bounds
  if Assigned(FHintData.Tree) then
    FHintData.Tree.FLastHintRect := Rect(0, 0, 0, 0);

  LeaveStates := [tsHint];
  if not (tsPanning in FStates) then
  begin
    StopTimer(ScrollTimer);
    LeaveStates := LeaveStates + [tsScrollPending, tsScrolling];
  end;
  DoStateChange([], LeaveStates);
  if Assigned(FCurrentHotNode) then
  begin
    DoHotChange(FCurrentHotNode, nil);
    if (toHotTrack in FOptions.PaintOptions) or (toCheckSupport in FOptions.MiscOptions) then
      InvalidateNode(FCurrentHotNode);
    FCurrentHotNode := nil;
  end;

  if Assigned(Header) then
  begin
    with TVirtualTreeColumnsCracker(Header.Columns) do
    begin
      DownIndex := NoColumn;
      HoverIndex := NoColumn;
      CheckBoxHit := False;
    end;
  end;
  DoMouseLeave();
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CMMouseWheel(var Message: TCMMouseWheel);

var
  ScrollAmount: TDimension;
  ScrollLines: DWORD;
  RTLFactor: Integer;
  WheelFactor: Double;

begin
  StopWheelPanning;

  inherited;

  if Message.Result = 0  then
  begin
    with Message do
    begin
      Result := 1;
      WheelFactor := WheelDelta / WHEEL_DELTA;
      if (FRangeY > ClientHeight) and (not (ssShift in ShiftState)) then
      begin
        // Scroll vertically if there's something to scroll...
        if ssCtrl in ShiftState then
          ScrollAmount := Trunc(WheelFactor * ClientHeight)
        else
        begin
          SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @ScrollLines, 0);
          if ScrollLines = WHEEL_PAGESCROLL then
            ScrollAmount := Trunc(WheelFactor * ClientHeight)
          else
            ScrollAmount := Integer(Trunc(WheelFactor * ScrollLines * FDefaultNodeHeight));
        end;
        SetOffsetY(FOffsetY + ScrollAmount);
      end
      else
      begin
        // ...else scroll horizontally if there's something to scroll.
        if UseRightToLeftAlignment then
          RTLFactor := -1
        else
          RTLFactor := 1;

        if ssCtrl in ShiftState then
          ScrollAmount := Trunc(WheelFactor * (ClientWidth - FHeader.Columns.GetVisibleFixedWidth))
        else
        begin
          SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @ScrollLines, 0);
          if ScrollLines = WHEEL_PAGESCROLL then
            ScrollAmount := Trunc(WheelFactor * (ClientWidth - FHeader.Columns.GetVisibleFixedWidth))
          else
            ScrollAmount := Trunc(WheelFactor * ScrollLines * FHeader.Columns.GetScrollWidth);
        end;
        SetOffsetX(FOffsetX + RTLFactor * ScrollAmount);
      end;
    end;

  end;

end;

//----------------------------------------------------------------------------------------------------------------------
procedure TBaseVirtualTree.CMSysColorChange(var Message: TMessage);

begin
  inherited;
  Message.Msg := WM_SYSCOLORCHANGE;
  DefaultHandler(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.TVMGetItem(var Message: TMessage);

// Screen reader support function. The method returns information about a particular node.

const
  StateMask = TVIS_STATEIMAGEMASK or TVIS_OVERLAYMASK or TVIS_EXPANDED or TVIS_DROPHILITED or TVIS_CUT or
    TVIS_SELECTED or TVIS_FOCUSED;

var
  Item: PTVItemEx;
  Node: PVirtualNode;
  Ghosted: Boolean;
  ImageIndex: TImageIndex;
  R: TRect;
  Text: string;
begin
  // We can only return valid data if a nodes reference is given.
  Item := Pointer(Message.LParam);
  Message.Result := Ord(((Item.mask and TVIF_HANDLE) <> 0) and Assigned(Item.hItem));
  if Message.Result = 1 then
  begin
    Node := Pointer(Item.hItem);
    // Child count requested?
    if (Item.mask and TVIF_CHILDREN) <> 0 then
      Item.cChildren := Node.ChildCount;
    // Index for normal image requested?
    if (Item.mask and TVIF_IMAGE) <> 0 then
    begin
      ImageIndex := -1;
      DoGetImageIndex(Node, ikNormal, -1, Ghosted, ImageIndex);
      Item.iImage := ImageIndex;
    end;
    // Index for selected image requested?
    if (Item.mask and TVIF_SELECTEDIMAGE) <> 0 then
    begin
      ImageIndex := -1;
      DoGetImageIndex(Node, ikSelected, -1, Ghosted, ImageIndex);
      Item.iSelectedImage := ImageIndex;
    end;
    // State info requested?
    if (Item.mask and TVIF_STATE) <> 0 then
    begin
      // Everything, which is possible is returned.
      Item.stateMask := StateMask;
      Item.state := 0;
      if Node = FFocusedNode then
        Item.state := Item.state or TVIS_FOCUSED;
      if vsSelected in Node.States then
        Item.state := Item.state or TVIS_SELECTED;
      if vsCutOrCopy in Node.States then
        Item.state := Item.state or TVIS_CUT;
      if Node = FDropTargetNode then
        Item.state := Item.state or TVIS_DROPHILITED;
      if vsExpanded in Node.States then
        Item.state := Item.state or TVIS_EXPANDED;

      // Construct state image and overlay image indices. They are zero based, btw.
      // and -1 means there is no image.
      ImageIndex := -1;
      DoGetImageIndex(Node, ikState, -1, Ghosted, ImageIndex);
      Item.state := Item.state or Byte(IndexToStateImageMask(ImageIndex + 1));
      ImageIndex := -1;
      DoGetImageIndex(Node, ikOverlay, -1, Ghosted, ImageIndex);
      Item.state := Item.state or Byte(IndexToOverlayMask(ImageIndex + 1));
    end;
    // Node caption requested?
    if (Item.mask and TVIF_TEXT) <> 0 then
    begin
      GetTextInfo(Node, -1, Font, R, Text);

      StrLCopy(Item.pszText, PWideChar(Text), Item.cchTextMax - 1);
      Item.pszText[Length(Text)] := #0;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.TVMGetItemRect(var Message: TMessage);

// Screen read support function. This method returns a node's display rectangle.

var
  TextOnly: Boolean;
  Node: PVirtualNode;

begin
  // The lparam member is used two-way. On enter it contains a pointer to the item (node).
  // On exit it is to be considered as pointer to a rectangle structure.
  Node := Pointer(Pointer(Message.LParam)^);
  Message.Result := Ord(IsVisible[Node]);
  if Message.Result <> 0 then
  begin
    TextOnly := Message.WParam <> 0;
    PRect(Message.LParam)^ := GetDisplayRect(Node, NoColumn, TextOnly);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.TVMGetNextItem(var Message: TMessage);

// Screen read support function. This method returns a node depending on the requested case.

var
  Node: PVirtualNode;

begin
  // Start with a nil result.
  Message.Result := 0;
  Node := Pointer(Message.LParam);
  case Message.WParam of
    TVGN_CARET:
      Message.Result := LRESULT(FFocusedNode);
    TVGN_CHILD:
      if Assigned(Node) then
        Message.Result := LRESULT(GetFirstChild(Node));
    TVGN_DROPHILITE:
      Message.Result := LRESULT(FDropTargetNode);
    TVGN_FIRSTVISIBLE:
      Message.Result := LRESULT(GetFirstVisible(nil, True));
    TVGN_LASTVISIBLE:
      Message.Result := LRESULT(GetLastVisible(nil, True));
    TVGN_NEXT:
      if Assigned(Node) then
        Message.Result := LRESULT(GetNextSibling(Node));
    TVGN_NEXTVISIBLE:
      if Assigned(Node) then
        Message.Result := LRESULT(GetNextVisible(Node, True));
    TVGN_PARENT:
      if Assigned(Node) and (Node <> FRoot) and (Node.Parent <> FRoot) then
        Message.Result := LRESULT(Node.Parent);
    TVGN_PREVIOUS:
      if Assigned(Node) then
        Message.Result := LRESULT(GetPreviousSibling(Node));
    TVGN_PREVIOUSVISIBLE:
      if Assigned(Node) then
        Message.Result := LRESULT(GetPreviousVisible(Node, True));
    TVGN_ROOT:
      Message.Result := LRESULT(GetFirst);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMCancelMode(var Message: TWMCancelMode);

begin
  // Clear any transient state.
  StopTimer(ExpandTimer);
  StopTimer(EditTimer);
  StopTimer(HeaderTimer);
  StopTimer(ScrollTimer);
  StopTimer(SearchTimer);
  StopTimer(ThemeChangedTimer);
  FSearchBuffer := '';
  FLastSearchNode := nil;

  DoStateChange([], [tsClearPending, tsEditPending, tsOLEDragPending, tsVCLDragPending, tsDrawSelecting,
    tsDrawSelPending, tsIncrementalSearching]);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMChar(var Message: TWMChar);

begin
  if tsIncrementalSearchPending in FStates then
  begin
    HandleIncrementalSearch(Message.CharCode);
    DoStateChange([], [tsIncrementalSearchPending]);
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMContextMenu(var Message: TWMContextMenu);

// This method is called when a popup menu is about to be displayed.
// We have to cancel some pending states here to avoid interferences.

var
  HitInfo: THitInfo;
  pt: TPoint;
begin
  DoStateChange([], [tsClearPending, tsEditPending, tsOLEDragPending, tsVCLDragPending, tsPopupMenuShown]);

  if not Assigned(PopupMenu) then begin
    // convert screen coordinates to client
    pt := ScreenToClient(Point(Message.XPos, Message.YPos));
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo); // ShiftState is not used anyway here
    DoPopupMenu(HitInfo.HitNode, HitInfo.HitColumn, pt);
  end;

  if not (tsPopupMenuShown in FStates) then
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMCopy(var Message: TWMCopy);

begin
  CopyToClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMCut(var Message: TWMCut);

begin
  CutToClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMEnable(var Message: TWMEnable);

begin
  inherited;
  RedrawWindow(nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE or RDW_NOCHILDREN);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMEraseBkgnd(var Message: TWMEraseBkgnd);

begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMGetDlgCode(var Message: TWMGetDlgCode);

begin
  Message.Result := DLGC_WANTCHARS or DLGC_WANTARROWS;
  if FWantTabs then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMHScroll(var Message: TWMHScroll);

  //--------------- local functions -------------------------------------------

  function GetRealScrollPosition: TDimension;

  var
    SI: TScrollInfo;
    Bar: Integer;

  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Bar := SB_HORZ;
    GetScrollInfo(Bar, SI);
    Result := SI.nTrackPos;
  end;

  //--------------- end local functions ---------------------------------------

var
  RTLFactor: Integer;

begin
  if UseRightToLeftAlignment then
    RTLFactor := -1
  else
    RTLFactor := 1;

  case Message.ScrollCode of
    SB_BOTTOM:
      SetOffsetX(-FRangeX);
    SB_ENDSCROLL:
      begin
        DoStateChange([], [tsThumbTracking]);
        // avoiding to adjust the vertical scroll position while tracking makes it much smoother
        // but we need to adjust the final position here then
        UpdateHorizontalScrollBar(False);
      end;
    SB_LINELEFT:
      SetOffsetX(FOffsetX + RTLFactor * FScrollBarOptions.HorizontalIncrement);
    SB_LINERIGHT:
      SetOffsetX(FOffsetX - RTLFactor * FScrollBarOptions.HorizontalIncrement);
    SB_PAGELEFT:
      SetOffsetX(FOffsetX + RTLFactor * (ClientWidth - FHeader.Columns.GetVisibleFixedWidth));
    SB_PAGERIGHT:
      SetOffsetX(FOffsetX - RTLFactor * (ClientWidth - FHeader.Columns.GetVisibleFixedWidth));
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        DoStateChange([tsThumbTracking]);
        if UseRightToLeftAlignment then
          SetOffsetX(-FRangeX + ClientWidth + GetRealScrollPosition)
        else
          SetOffsetX(-GetRealScrollPosition);
      end;
    SB_TOP:
      SetOffsetX(0);
  end;

  Message.Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMKeyDown(var Message: TWMKeyDown);

// Keyboard event handling for node focus, selection, node specific popup menus and help invokation.
// For a detailed description of every action done here read the help.

var
  Shift: TShiftState;
  Node, Temp,
  LastFocused: PVirtualNode;
  Offset: Integer;
  ClearPending,
  NeedInvalidate,
  DoRangeSelect,
  PerformMultiSelect: Boolean;
  Context: Integer;
  ParentControl: TWinControl;
  R: TRect;
  NewCheckState: TCheckState;
  TempColumn,
  NewColumn: TColumnIndex;
  ActAsGrid: Boolean;
  ForceSelection: Boolean;
  NewWidth,
  NewHeight: Integer;
  RTLFactor: Integer;

  // for tabulator handling
  GetStartColumn: function(ConsiderAllowFocus: Boolean = False): TColumnIndex of object;
  GetNextColumn: function(Column: TColumnIndex; ConsiderAllowFocus: Boolean = False): TColumnIndex of object;
  GetNextNode: TGetNextNodeProc;

  KeyState: TKeyboardState;
  Buffer: array[0..1] of AnsiChar;

  //--------------- local functions -------------------------------------------
  function getPreviousVisibleAutoSpanColumn(acolumn: TColumnIndex; anode: PVirtualNode): TColumnIndex;
  var
    PrevColumn: Integer;
  begin
    if (not assigned(anode))
       or (not FHeader.UseColumns)
       or (not (toAutoSpanColumns in FOptions.AutoOptions))
       or (acolumn = FHeader.MainColumn) then
    begin
      //previously existing logic
      result := FHeader.Columns.GetPreviousVisibleColumn(acolumn, True);
      exit;
    end;
    //consider auto spanning
    with FHeader.Columns do //standard loop for auto span
    begin
      PrevColumn := acolumn;
      repeat
        result := FHeader.Columns.GetPreviousVisibleColumn(PrevColumn);
        if (result = InvalidColumn) or
           (not ColumnIsEmpty(anode, result))
           //Any other BidiMode is not supported as already
           //documented by original developer
           or (Items[result].BidiMode <> bdLeftToRight) then
          Break;
        PrevColumn := result;
      until False;
    end;
  end;

  //---------------------------------------------------------------------------
  function getNextVisibleAutoSpanColumn(acolumn: TColumnIndex; anode: PVirtualNode): TColumnIndex;
  var
    NextColumn: Integer;
  begin
    if (not assigned(anode))
       or (not FHeader.UseColumns)
       or (not (toAutoSpanColumns in FOptions.AutoOptions))
       or (acolumn = FHeader.MainColumn) then
    begin
      //previously existing logic
      result := FHeader.Columns.GetNextVisibleColumn(acolumn, True);
      exit;
    end;
    //consider auto spanning
    with FHeader.Columns do //standard loop for auto span
    begin
      NextColumn := acolumn;
      repeat
        result := FHeader.Columns.GetNextVisibleColumn(NextColumn);
        if (result = InvalidColumn) or
           not ColumnIsEmpty(anode, result)
           //Any other BidiMode is not supported as already
           //documented by original developer
           or (Items[result].BidiMode <> bdLeftToRight) then
          Break;
        NextColumn := result;
      until False;
    end;
  end;

  //---------------------------------------------------------------------------
  function isEmptyAutoSpanColumn(acolumn: TColumnIndex; anode: PVirtualNode): boolean;
  var
    previousColumn: Integer;
  begin
    result := false;
    if (not assigned(anode))
       or (not FHeader.UseColumns)
       or (not (toAutoSpanColumns in FOptions.AutoOptions))
       or (acolumn = FHeader.MainColumn) then
      exit;
    with FHeader.Columns do
    begin
      previousColumn := FHeader.Columns.GetPreviousVisibleColumn(acolumn);
      if (previousColumn = InvalidColumn) //there is no previous column
         //Any other BidiMode is not supported as already
         //documented by original developer
         or (Items[acolumn].BidiMode <> bdLeftToRight) then
        exit; //returning false
      result := ColumnIsEmpty(anode, acolumn);
    end;
  end;


  //--------------- end local functions ---------------------------------------

begin
  // Make form key preview work and let application modify the key if it wants this.
  inherited;

  with Message do
  begin
    Shift := KeyDataToShiftState(KeyData);
    // Ask the application if the default key handling is desired.
    if DoKeyAction(CharCode, Shift) then
    begin
      if (CharCode in [VK_HOME, VK_END, VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_BACK, VK_TAB]) and (RootNode.FirstChild <> nil) then
      begin
        PerformMultiSelect := (ssShift in Shift) and (toMultiSelect in FOptions.SelectionOptions) and not IsEditing;

        // Flag to avoid range selection in case of single node advance.
        DoRangeSelect := (CharCode in [VK_HOME, VK_END, VK_PRIOR, VK_NEXT]) and PerformMultiSelect and not IsEditing;

        NeedInvalidate := DoRangeSelect or (FSelectionCount > 1);
        ActAsGrid := toGridExtensions in FOptions.MiscOptions;
        ClearPending := (Shift = []) or (ActAsGrid and not (ssShift in Shift)) or
          not (toMultiSelect in FOptions.SelectionOptions) or (CharCode in [VK_TAB, VK_BACK]);

        // Keep old focused node for range selection. Use a default node if none was focused until now.
        LastFocused := FFocusedNode;
        if (LastFocused = nil) and (Shift <> []) then
          LastFocused := GetFirstVisible(nil, True);

        // Set an initial range anchor if there is not yet one.
        if FRangeAnchor = nil then
          FRangeAnchor := GetFirstSelected;
        if FRangeAnchor = nil then
          FRangeAnchor := GetFirst;

        if UseRightToLeftAlignment then
          RTLFactor := -1
        else
          RTLFactor := 1;

        // Determine new focused node.
        case CharCode of
          VK_HOME, VK_END:
            begin
              if (CharCode = VK_END) xor UseRightToLeftAlignment then
              begin
                GetStartColumn := FHeader.Columns.GetLastVisibleColumn;
                GetNextColumn := FHeader.Columns.GetPreviousVisibleColumn;
                GetNextNode := GetPreviousVisible;
                Node := GetLastVisible(nil, True);
              end
              else
              begin
                GetStartColumn := FHeader.Columns.GetFirstVisibleColumn;
                GetNextColumn := FHeader.Columns.GetNextVisibleColumn;
                GetNextNode := GetNextVisible;
                Node := GetFirstVisible(nil, True);
              end;

              // Advance to next/previous visible column.
              if FHeader.UseColumns then
                NewColumn := GetStartColumn
              else
                NewColumn := NoColumn;
              // Find a column for the new/current node which can be focused.
              // Make the 'DoFocusChanging' for finding a valid column
              // identifiable from the 'DoFocusChanging' raised later on by
              // "FocusedNode := Node;"
              while (NewColumn > NoColumn) and not DoFocusChanging(FFocusedNode, FFocusedNode, FFocusedColumn, NewColumn) do
                NewColumn := GetNextColumn(NewColumn);
              if NewColumn > InvalidColumn then
              begin
                if (Shift = [ssCtrl]) and not ActAsGrid then
                begin
                  ScrollIntoView(Node, toCenterScrollIntoView in FOptions.SelectionOptions,
                    not (toDisableAutoscrollOnFocus in FOptions.AutoOptions));
                  if (CharCode = VK_HOME) and not UseRightToLeftAlignment then
                    SetOffsetX(0)
                  else
                    SetOffsetX(-MaxInt);
                end
                else
                begin
                  if not ActAsGrid or (ssCtrl in Shift) then
                    FocusedNode := Node;
                  //fix: In grid mode, if full row select option is ON,
                  //then also go to the node determined from the earlier logic
                  if ActAsGrid and (toFullRowSelect in FOptions.SelectionOptions) then
                    FocusedNode := Node;
                  if ActAsGrid and not (toFullRowSelect in FOptions.SelectionOptions) then
                  begin
                    FocusedColumn := NewColumn;
                    // fix: If auto span is ON the last column may be a merged column. So take
                    // care of selecting the whole merged column on END key.
                    if (CharCode = VK_END) and isEmptyAutoSpanColumn(NewColumn, FFocusedNode) then
                      FocusedColumn := getPreviousVisibleAutoSpanColumn(NewColumn, FFocusedNode);
                  end;
                end;
              end;
            end;
          VK_PRIOR:
            if Shift = [ssCtrl, ssShift] then
              SetOffsetX(FOffsetX + ClientWidth)
            else
              if [ssShift, ssAlt] = Shift then
              begin
                if FFocusedColumn <= NoColumn then
                  NewColumn := FHeader.Columns.GetFirstVisibleColumn
                else
                begin
                  Offset := FHeader.Columns.GetVisibleFixedWidth;
                  NewColumn := FFocusedColumn;
                  while True do
                  begin
                    TempColumn := FHeader.Columns.GetPreviousVisibleColumn(NewColumn);
                    NewWidth := FHeader.Columns[NewColumn].Width;
                    if (TempColumn <= NoColumn) or
                       (Offset + NewWidth >= ClientWidth) or
                       (coFixed in FHeader.Columns[TempColumn].Options) then
                      Break;
                    NewColumn := TempColumn;
                    Inc(Offset, NewWidth);
                  end;
                end;
                SetFocusedColumn(NewColumn);
              end
              else
                if ssCtrl in Shift then
                  SetOffsetY(FOffsetY + ClientHeight)
                else
                begin
                  Offset := 0;
                  // If there's no focused node then just take the very first visible one.
                  if FFocusedNode = nil then
                    Node := GetFirstVisible(nil, True)
                  else
                  begin
                    // Go up as many nodes as comprise together a size of ClientHeight.
                    Node := FFocusedNode;
                    while True do
                    begin
                      Temp := GetPreviousVisible(Node, True);
                      NewHeight := NodeHeight[Node];
                      if (Temp = nil) or (Offset + NewHeight >= ClientHeight) then
                        Break;
                      Node := Temp;
                      Inc(Offset, NodeHeight[Node]);
                    end;
                  end;
                  FocusedNode := Node;
                end;
          VK_NEXT:
            if Shift = [ssCtrl, ssShift] then
              SetOffsetX(FOffsetX - ClientWidth)
            else
              if [ssShift, ssAlt] = Shift then
              begin
                if FFocusedColumn <= NoColumn then
                  NewColumn := FHeader.Columns.GetFirstVisibleColumn
                else
                begin
                  Offset := FHeader.Columns.GetVisibleFixedWidth;
                  NewColumn := FFocusedColumn;
                  while True do
                  begin
                    TempColumn := FHeader.Columns.GetNextVisibleColumn(NewColumn);
                    NewWidth := FHeader.Columns[NewColumn].Width;
                    if (TempColumn <= NoColumn) or
                       (Offset + NewWidth >= ClientWidth) or
                       (coFixed in FHeader.Columns[TempColumn].Options) then
                      Break;
                    NewColumn := TempColumn;
                    Inc(Offset, NewWidth);
                  end;
                end;
                SetFocusedColumn(NewColumn);
              end
              else
                if ssCtrl in Shift then
                  SetOffsetY(FOffsetY - ClientHeight)
                else
                begin
                  Offset := 0;
                  // If there's no focused node then just take the very last one.
                  if FFocusedNode = nil then
                    Node := GetLastVisible(nil, True)
                  else
                  begin
                    // Go up as many nodes as comprise together a size of ClientHeight.
                    Node := FFocusedNode;
                    while True do
                    begin
                      Temp := GetNextVisible(Node, True);
                      NewHeight := NodeHeight[Node];
                      if (Temp = nil) or (Offset + NewHeight >= ClientHeight) then
                        Break;
                      Node := Temp;
                      Inc(Offset, NewHeight);
                    end;
                  end;
                  FocusedNode := Node;
                end;
          VK_UP:
            begin
              // scrolling without selection change
              if ssCtrl in Shift then
                SetOffsetY(FOffsetY + FDefaultNodeHeight)
              else
              begin
                if FFocusedNode = nil then
                  Node := GetLastVisible(nil, True)
                else
                  Node := GetPreviousVisible(FFocusedNode, True);

                if Assigned(Node) then
                begin
                  if not EndEditNode then
                    exit;
                  if (not PerformMultiSelect or (CompareNodePositions(LastFocused, Node) < -1)) and Assigned(FFocusedNode) then
                    ClearSelection(False);  // Clear selection only if more than one node was skipped. See issue #926
                  if FFocusedColumn <= NoColumn then
                    FFocusedColumn := FHeader.MainColumn;
                  FocusedNode := Node;
                end
                else
                  if Assigned(FFocusedNode) then
                    InvalidateNode(FFocusedNode);
              end;
            end;
          VK_DOWN:
            begin
              // scrolling without selection change
              if ssCtrl in Shift then
                SetOffsetY(FOffsetY - FDefaultNodeHeight)
              else
              begin
                if FFocusedNode = nil then
                  Node := GetFirstVisible(nil, True)
                else
                  Node := GetNextVisible(FFocusedNode, True);

                if Assigned(Node) then
                begin
                  if not EndEditNode then
                    exit;
                  if (not PerformMultiSelect or (CompareNodePositions(LastFocused, Node) > 1)) and  Assigned(FFocusedNode) then
                    ClearSelection(False); // Clear selection only if more than one node was skipped. See issue #926
                  if FFocusedColumn <= NoColumn then
                    FFocusedColumn := FHeader.MainColumn;
                  FocusedNode := Node;
                end
                else
                  if Assigned(FFocusedNode) then
                    InvalidateNode(FFocusedNode);
              end;
            end;
          VK_LEFT:
            begin
              // special handling
              if ssCtrl in Shift then
                SetOffsetX(FOffsetX + RTLFactor * FHeader.Columns.GetScrollWidth)
              else
              begin
                // other special cases
                Context := NoColumn;
                if (toExtendedFocus in FOptions.SelectionOptions) and (toGridExtensions in FOptions.MiscOptions) then
                begin
                  Context := getPreviousVisibleAutoSpanColumn(FFocusedColumn, FFocusedNode);
                  if Context > NoColumn then
                    FocusedColumn := Context;
                end
                else
                  if Assigned(FFocusedNode) and (vsExpanded in FFocusedNode.States) and
                     (Shift = []) and (vsHasChildren in FFocusedNode.States) then
                    ToggleNode(FFocusedNode)
                  else
                  begin
                    if FFocusedNode = nil then
                      FocusedNode := GetFirstVisible(nil, True)
                    else
                    begin
                      if FFocusedNode.Parent <> FRoot then
                        Node := FFocusedNode.Parent
                      else
                        Node := nil;
                      if Assigned(Node) then
                      begin
                        if PerformMultiSelect then
                        begin
                          // and a third special case
                          if FFocusedNode.Index > 0 then
                            DoRangeSelect := True
                          else
                           if CompareNodePositions(Node, FRangeAnchor) > 0 then
                             RemoveFromSelection(FFocusedNode);
                        end;
                        FocusedNode := Node;
                      end
                      else begin
                        // If already a root node is selected, then scroll to the left as there is nothing else we could do. #691
                        SetOffsetX(FOffsetX + RTLFactor * FHeader.Columns.GetScrollWidth);
                      end;//else
                    end;
                  end;
              end;
            end;
          VK_RIGHT:
            begin
              // special handling
              if ssCtrl in Shift then
                SetOffsetX(FOffsetX - RTLFactor * FHeader.Columns.GetScrollWidth)
              else
              begin
                // other special cases
                Context := NoColumn;
                if (toExtendedFocus in FOptions.SelectionOptions) and (toGridExtensions in FOptions.MiscOptions) then
                begin
                  Context := getNextVisibleAutoSpanColumn(FFocusedColumn, FFocusedNode);
                  if Context > NoColumn then
                    FocusedColumn := Context;
                end
                else
                  if Assigned(FFocusedNode) and not (vsExpanded in FFocusedNode.States) and
                     (Shift = []) and (vsHasChildren in FFocusedNode.States) then
                    ToggleNode(FFocusedNode)
                  else
                  begin
                    if FFocusedNode = nil then
                      FocusedNode := GetFirstVisible(nil, True)
                    else
                    begin
                      Node := GetFirstVisibleChild(FFocusedNode);
                      if Assigned(Node) then
                      begin
                        if PerformMultiSelect and (CompareNodePositions(Node, FRangeAnchor) < 0) then
                          RemoveFromSelection(FFocusedNode);
                        FocusedNode := Node;
                      end
                      else begin
                        // If already a leaf node is selected, then scroll to the right as there is nothing else we could do. #691
                        SetOffsetX(FOffsetX - RTLFactor * FHeader.Columns.GetScrollWidth);
                      end;//else
                    end;
                  end;
              end;
            end;
          VK_BACK:
            if tsIncrementalSearching in FStates then
              DoStateChange([tsIncrementalSearchPending])
            else
              if Assigned(FFocusedNode) and (FFocusedNode.Parent <> FRoot) then
                FocusedNode := FocusedNode.Parent;
          VK_TAB:
            if (toExtendedFocus in FOptions.SelectionOptions) and FHeader.UseColumns then
            begin
              // In order to avoid duplicating source code just to change the direction
              // we use function variables.
              if ssShift in Shift then
              begin
                GetStartColumn := FHeader.Columns.GetLastVisibleColumn;
                GetNextColumn := FHeader.Columns.GetPreviousVisibleColumn;
                GetNextNode := GetPreviousVisible;
              end
              else
              begin
                GetStartColumn := FHeader.Columns.GetFirstVisibleColumn;
                GetNextColumn := FHeader.Columns.GetNextVisibleColumn;
                GetNextNode := GetNextVisible;
              end;

              // Advance to next/previous visible column/node.
              Node := FFocusedNode;
              NewColumn := GetNextColumn(FFocusedColumn, True);
              repeat
                // Find a column for the current node which can be focused.
                while (NewColumn > NoColumn) and not DoFocusChanging(FFocusedNode, Node, FFocusedColumn, NewColumn)
                   //Fix: for Tab Key to properly skip the empty auto span column
                   or isEmptyAutoSpanColumn(NewColumn, Node) do
                   NewColumn := GetNextColumn(NewColumn, True);

                if NewColumn > NoColumn then
                begin
                  // Set new node and column in one go.
                  SetFocusedNodeAndColumn(Node, NewColumn);
                  Break;
                end;

                // No next column was accepted for the current node. So advance to next node and try again.
                Node := GetNextNode(Node);
                NewColumn := GetStartColumn;

				// fix: From last column, the Tab key should always go to next row irrespective of auto span
				// Similarly the Shift-Tab key should go to previos row from first column
                if (Node <> nil) and (NewColumn > NoColumn) then
                  SetFocusedNodeAndColumn(Node, NewColumn);

              until Node = nil;
            end;
        end;

        // Clear old selection if required but take care to select the new focused node if it was not selected before.
        ForceSelection := False;
        if ClearPending and ((LastFocused <> FFocusedNode) or (FSelectionCount <> 1)) then
        begin
          ClearSelection(not Assigned(FFocusedNode));
          ForceSelection := True;
        end;

        // Determine new selection anchor.
        if Shift = [] then
        begin
          FRangeAnchor := FFocusedNode;
          FLastSelectionLevel := GetNodeLevelForSelectConstraint(FFocusedNode);
        end;

        if Assigned(FFocusedNode) then
        begin
          // Finally change the selection for a specific range of nodes.
          if DoRangeSelect then
            ToggleSelection(LastFocused, FFocusedNode)
          // Make sure the new focused node is also selected.
          else if (LastFocused <> FFocusedNode) then begin
            if ForceSelection then
              AddToSelection(FFocusedNode, False)
            else
              ToggleSelection(LastFocused, FFocusedNode); // See issue #926
          end;
        end;

        // If a repaint is needed then paint the entire tree because of the ClearSelection call,
        if NeedInvalidate then
          Invalidate;
      end
      else
      begin
        // Second chance for keys not directly concerned with selection changes.

        // For +, -, /, * keys on the main keyboard (not numpad) there is no virtual key code defined.
        // We have to do special processing to get them working too.
        GetKeyboardState(KeyState);
        // Avoid conversion to control characters. We have captured the control key state already in Shift.
        KeyState[VK_CONTROL] := 0;
        if ToASCII(Message.CharCode, (Message.KeyData shr 16) and 7, KeyState, PChar(@Buffer), 0) > 0 then
        begin
          case Buffer[0] of
            '*':
              CharCode := VK_MULTIPLY;
            '+':
              CharCode := VK_ADD;
            '/':
              CharCode := VK_DIVIDE;
            '-':
              CharCode := VK_SUBTRACT;
          end;
        end;

        // According to https://web.archive.org/web/20041129085958/http://www.it-faq.pl/mskb/99/337.HTM
        // there is a problem with ToASCII when used in conjunction with dead chars.
        // The article recommends to call ToASCII twice to restore a deleted flag in the key message
        // structure under certain circumstances. It turned out it is best to always call ToASCII twice.
        ToASCII(Message.CharCode, (Message.KeyData shr 16) and 7, KeyState, PChar(@Buffer), 0);

        case CharCode of
          VK_F2:
            if (Shift = []) and Assigned(FFocusedNode) and CanEdit(FFocusedNode, FFocusedColumn) then
            begin
              FEditColumn := FFocusedColumn;
              DoEdit;
            end;
          VK_ADD:
            if not (tsIncrementalSearching in FStates) then
            begin
              if ssCtrl in Shift then begin// When changing this code review issue #781
                if ((toReverseFullExpandHotKey in TreeOptions.MiscOptions) and (Shift = [ssCtrl])) xor (Shift = [ssCtrl, ssShift]) then
                  FullExpand
                else if Shift = [ssCtrl] then
                  FHeader.AutoFitColumns
              end
              else if Shift = [] then begin
                if Assigned(FFocusedNode) and not (vsExpanded in FFocusedNode.States) then
                  ToggleNode(FFocusedNode);
              end// if Shift = []
              else
                DoStateChange([tsIncrementalSearchPending]);
            end;//if  not (tsIncrementalSearching in FStates)
          VK_SUBTRACT:
            if not (tsIncrementalSearching in FStates) then
            begin
              if ssCtrl in Shift then
                if (toReverseFullExpandHotKey in TreeOptions.MiscOptions) xor (ssShift in Shift) then
                  FullCollapse
                else
                  FHeader.RestoreColumns
              else
                if Assigned(FFocusedNode) and (vsExpanded in FFocusedNode.States) then
                  ToggleNode(FFocusedNode);
            end
            else
              DoStateChange([tsIncrementalSearchPending]);
          VK_MULTIPLY:
            if not (tsIncrementalSearching in FStates) then
            begin
              if Assigned(FFocusedNode) then
                FullExpand(FFocusedNode);
            end
            else
              DoStateChange([tsIncrementalSearchPending]);
          VK_DIVIDE:
            if not (tsIncrementalSearching in FStates) then
            begin
              if Assigned(FFocusedNode) then
                FullCollapse(FFocusedNode);
            end
            else
              DoStateChange([tsIncrementalSearchPending]);
          VK_ESCAPE: // cancel actions currently in progress
            begin
              if IsMouseSelecting then
              begin
                DoStateChange([], [tsDrawSelecting, tsDrawSelPending]);
                Invalidate;
              end
              else
                if IsEditing then
                  CancelEditNode;
            end;
          VK_SPACE:
            if (toCheckSupport in FOptions.MiscOptions) and Assigned(FFocusedNode) and
              (FFocusedNode.CheckType <> ctNone) then
            begin
              NewCheckState := DetermineNextCheckState(FFocusedNode.CheckType, GetCheckState(FFocusedNode));
              if DoChecking(FFocusedNode, NewCheckState) then
              begin
                if SelectedCount > 1 then
                  SetCheckStateForAll(NewCheckState, True)
                else
                  DoCheckClick(FFocusedNode, NewCheckState);
              end;
            end
            else
              DoStateChange([tsIncrementalSearchPending]);
          VK_F1:
            if Assigned(FOnGetHelpContext) then
            begin
              Context := 0;
              if Assigned(FFocusedNode) then
              begin
                Node := FFocusedNode;
                // Traverse the tree structure up to the root.
                repeat
                  FOnGetHelpContext(Self, Node, IfThen(FFocusedColumn > NoColumn, FFocusedColumn, 0), Context);
                  Node := Node.Parent;
                until (Node = FRoot) or (Context <> 0);
              end;

              // If no help context could be found try the tree's one or its parent's contexts.
              ParentControl := Self;
              while Assigned(ParentControl) and (Context = 0) do
              begin
                Context := ParentControl.HelpContext;
                ParentControl := ParentControl.Parent;
              end;
              if Context <> 0 then
                Application.HelpContext(Context);
            end;
          VK_APPS:
            if Assigned(FFocusedNode) then
            begin
              R := GetDisplayRect(FFocusedNode, FFocusedColumn, True);
              Offset := DoGetNodeWidth(FFocusedNode, FFocusedColumn);
              if FFocusedColumn >= 0 then
              begin
                if Offset > FHeader.Columns[FFocusedColumn].Width then
                  Offset := FHeader.Columns[FFocusedColumn].Width;
              end
              else
              begin
                if Offset > ClientWidth then
                  Offset := ClientWidth;
              end;
              DoPopupMenu(FFocusedNode, FFocusedColumn, Point(R.Left + Offset div 2, (R.Top + R.Bottom) div 2));
            end
            else
              DoPopupMenu(nil, FFocusedColumn, Point(-1, -1));
          Ord('a'), Ord('A'):
            if ssCtrl in Shift then
              SelectAll(True)
            else
              DoStateChange([tsIncrementalSearchPending]);
        else
        begin
          // Use the key for incremental search.
          // Since we are dealing with Unicode all the time there should be a more sophisticated way
          // of checking for valid characters for incremental search.
          // This is available but would require to include a significant amount of Unicode character
          // properties, so we stick with the simple space check.
          if ((Shift * [ssCtrl, ssAlt] = []) or ((Shift * [ssCtrl, ssAlt] = [ssCtrl, ssAlt]))) and (CharCode >= 32) then
            DoStateChange([tsIncrementalSearchPending]);
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMKeyUp(var Message: TWMKeyUp);

begin
  inherited;

  case Message.CharCode of
    VK_TAB:
      EnsureNodeFocused(); // Always select a node if the control gets the focus via TAB key, #237
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMKillFocus(var Msg: TWMKillFocus);

var
  Form: TCustomForm;
  Control: TWinControl;
  Pos: TSmallPoint;
  Unknown: IUnknown;

begin
  inherited;

  // Remove hint if shown currently.
  if tsHint in Self.FStates then
    Application.CancelHint;

  // Stop wheel panning if active.
  StopWheelPanning;

  // Don't let any timer continue if the tree is no longer the active control (except change timers).
  StopTimer(ExpandTimer);
  StopTimer(EditTimer);
  StopTimer(HeaderTimer);
  StopTimer(ScrollTimer);
  StopTimer(SearchTimer);
  FSearchBuffer := '';
  FLastSearchNode := nil;

  DoStateChange([], [tsScrollPending, tsScrolling, tsEditPending, tsLeftButtonDown, tsRightButtonDown,
    tsMiddleButtonDown, tsOLEDragPending, tsVCLDragPending, tsIncrementalSearching, tsNodeHeightTrackPending,
    tsNodeHeightTracking]);

  if (FSelectionCount > 0) or not (toGhostedIfUnfocused in FOptions.PaintOptions) then
    Invalidate
  else
    if Assigned(FFocusedNode) then
      InvalidateNode(FFocusedNode);

  // Workaround for wrapped non-VCL controls (like TWebBrowser), which do not use VCL mechanisms and
  // leave the ActiveControl property in the wrong state, which causes trouble when the control is refocused.
  Form := GetParentForm(Self);
  if Assigned(Form) and (Form.ActiveControl = Self) then
  begin
    Cardinal(Pos) := GetMessagePos;
    Control := FindVCLWindow(SmallPointToPoint(Pos));
    // Every control derived from TOleControl has potentially the focus problem. In order to avoid including
    // the OleCtrls unit (which will, among others, include Variants), which would allow to test for the TOleControl
    // class, the IOleClientSite interface is used for the test, which is supported by TOleControl and a good indicator.
    if Assigned(Control) and Control.GetInterface(IOleClientSite, Unknown) then
      Form.ActiveControl := nil;

    // For other classes the active control should not be modified. Otherwise you need two clicks to select it.
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMLButtonDblClk(var Message: TWMLButtonDblClk);

var
  HitInfo: THitInfo;

begin
  DoStateChange([tsLeftDblClick]);
  try
    // get information about the hit, before calling inherited, is this may change the scroll postion and so the node under the mouse would chnage and would no longer be the one the user actually clicked
      GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
      HandleMouseDblClick(Message, HitInfo);
    // Call inherited after doing our standard handling, as the event handler may close the form or re-fill the control, so our clicked node would be no longer valid.
    // Our standard handling does not do that.
    inherited;
    // #909
    // if we show a modal form in the HandleMouseDblClick(), the mouse capture wont be released
      if csCaptureMouse in ControlStyle then MouseCapture := False;
  finally
    DoStateChange([], [tsLeftDblClick]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMLButtonDown(var Message: TWMLButtonDown);

var
  HitInfo: THitInfo;

begin
  DoStateChange([tsLeftButtonDown]);
  inherited;

  // get information about the hit
  GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
  HandleMouseDown(Message, HitInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMLButtonUp(var Message: TWMLButtonUp);

var
  HitInfo: THitInfo;

begin
  DoStateChange([], [tsLeftButtonDown, tsNodeHeightTracking, tsNodeHeightTrackPending]);

  // get information about the hit
  GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
  HandleMouseUp(Message, HitInfo);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMMButtonDblClk(var Message: TWMMButtonDblClk);

var
  HitInfo: THitInfo;

begin
  DoStateChange([tsMiddleDblClick]);
  inherited;

  // get information about the hit
  if toMiddleClickSelect in FOptions.SelectionOptions then
  begin
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
    HandleMouseDblClick(Message, HitInfo);
  end;
  DoStateChange([], [tsMiddleDblClick]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMMButtonDown(var Message: TWMMButtonDown);

var
  HitInfo: THitInfo;

begin
  DoStateChange([tsMiddleButtonDown]);

  if FHeader.States = [] then
  begin
    inherited;

    // Start wheel panning or scrolling if not already active, allowed and scrolling is useful at all.
    if (toWheelPanning in FOptions.MiscOptions) and not (tsPanning in FStates) and
      ((FRangeX > ClientWidth) or (FRangeY > ClientHeight)) then
    begin
      FLastClickPos := SmallPointToPoint(Message.Pos);
      StartWheelPanning(FLastClickPos);
    end
    else
    begin
      StopWheelPanning;

      // Get information about the hit.
      if toMiddleClickSelect in FOptions.SelectionOptions then
      begin
        GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
        HandleMouseDown(Message, HitInfo);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMMButtonUp(var Message: TWMMButtonUp);

var
  HitInfo: THitInfo;

begin
  DoStateChange([], [tsMiddleButtonDown]);

  if not (tsPanning in FStates) then
    if FHeader.States = [] then
    begin
      inherited;

      // get information about the hit
      if toMiddleClickSelect in FOptions.SelectionOptions then
      begin
        GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
        HandleMouseUp(Message, HitInfo);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMNCCalcSize(var Message: TWMNCCalcSize);

begin
  inherited;

  with FHeader do
    if hoVisible in FHeader.Options then
      with Message.CalcSize_Params^ do
        Inc(rgrc[0].Top, Height);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMNCDestroy(var Message: TWMNCDestroy);

// Used to release a reference of the drag manager. This is the only reliable way we get notified about
// window destruction, because of the automatic release of a window if its parent window is freed.

begin
  InterruptValidation;

  StopTimer(ChangeTimer);
  StopTimer(StructureChangeTimer);

  if not (csDesigning in ComponentState) and HandleAllocated then
    RevokeDragDrop(Handle);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMNCHitTest(var Message: TWMNCHitTest);

begin
  inherited;
  if (hoVisible in FHeader.Options) and
    FHeader.InHeader(ScreenToClient(SmallPointToPoint(Message.Pos))) then
    Message.Result := HTBORDER;
end;

//----------------------------------------------------------------------------------------------------------------------


procedure TBaseVirtualTree.WMNCPaint(var Message: TWMNCPaint);

var
  DC: HDC;
  R: TRect;
  Flags: DWORD;
  ExStyle: Integer;
  TempRgn: HRGN;
  BorderWidth,
  BorderHeight: Integer;

begin
  if tsUseThemes in FStates then
  begin
    // If theming is enabled and the client edge border is set for the window then prevent the default window proc
    // from painting the old border to avoid flickering.
    ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      GetWindowRect(Handle, R);
      // Determine width of the client edge.
      BorderWidth := GetSystemMetrics(SM_CXEDGE);
      BorderHeight := GetSystemMetrics(SM_CYEDGE);
      InflateRect(R, -BorderWidth, -BorderHeight);
      TempRgn := CreateRectRgnIndirect(R);
      // Exclude the border from the message region if there is one. Otherwise just use the inflated
      // window area region.
      if Message.Rgn <> 1 then
        CombineRgn(TempRgn, Message.Rgn, TempRgn, RGN_AND);
      DefWindowProc(Handle, Message.Msg, WPARAM(TempRgn), 0);
      DeleteObject(TempRgn);
    end
    else
      DefaultHandler(Message);
  end
  else
    DefaultHandler(Message);

  Flags := DCX_CACHE or DCX_CLIPSIBLINGS or DCX_WINDOW or DCX_VALIDATE;

  if (Message.Rgn = 1) then
    DC := GetDCEx(Handle, 0, Flags)
  else
    DC := GetDCEx(Handle, Message.Rgn, Flags or DCX_INTERSECTRGN);

  if DC <> 0 then
  try
    OriginalWMNCPaint(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
  if (((tsUseThemes in FStates) and not VclStyleEnabled) or (VclStyleEnabled and (seBorder in StyleElements))) then
      StyleServices.PaintBorder(Self, False)
  else
    if (VclStyleEnabled and not (seBorder in StyleElements)) then
      TStyleManager.SystemStyle.PaintBorder(Self, False)
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
begin
  if tsVCLDragging in FStates then
    ImageList_DragShowNolock(False);
  if csPaintCopy in ControlState then
    FUpdateRect := ClientRect
  else
    GetUpdateRect(Handle, FUpdateRect, True);

  inherited;

  if tsVCLDragging in FStates then
    ImageList_DragShowNolock(True);

  if hoVisible in FHeader.Options then
  begin
    DC := GetDCEx(Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_WINDOW or DCX_VALIDATE);
    if DC <> 0 then
      try
        FHeader.Columns.PaintHeader(DC, FHeaderRect, -FEffectiveOffsetX);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;//if header visible
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMPaste(var Message: TWMPaste);

begin
  PasteFromClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMPrint(var Message: TWMPrint);

// This message is sent to request that the tree draws itself to a given device context. This includes not only
// the client area but also the non-client area (header!).

begin
  // Draw only if the window is visible or visibility is not required.
  if ((Message.Flags and PRF_CHECKVISIBLE) = 0) or IsWindowVisible(Handle) then
    Header.Columns.PaintHeader(Message.DC, FHeaderRect, -FEffectiveOffsetX);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMRButtonDblClk(var Message: TWMRButtonDblClk);

var
  HitInfo: THitInfo;

begin
  DoStateChange([tsRightDblClick]);
  inherited;

  // get information about the hit
  if toMiddleClickSelect in FOptions.SelectionOptions then
  begin
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
    HandleMouseDblClick(Message, HitInfo);
  end;
  DoStateChange([], [tsRightDblClick]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMRButtonDown(var Message: TWMRButtonDown);

var
  HitInfo: THitInfo;
  RemoveSynchMode: Boolean; // Needed to restore tsSynchMode correctly

begin
  DoStateChange([tsRightButtonDown]);

  if FHeader.States = [] then
  begin
    inherited;

    // get information about the hit
    if toRightClickSelect in FOptions.SelectionOptions then
    begin
      GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
      // Go temporarily into sync mode to avoid a delayed change event for the node when selecting. #679
      RemoveSynchMode := not (tsSynchMode in FStates);
      Include(FStates, tsSynchMode);
      HandleMouseDown(Message, HitInfo);
      if RemoveSynchMode then
        Exclude(FStates, tsSynchMode);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMRButtonUp(var Message: TWMRButtonUp);

// handle right click selection and node specific popup menu

var
  HitInfo: THitInfo;

begin
  DoStateChange([], [tsRightButtonDown]);

  if FHeader.States = [] then
  begin
    Application.CancelHint;

    if IsMouseSelecting and Assigned(PopupMenu) then
    begin
      // Reset selection state already here, before the inherited handler opens the default menu.
      DoStateChange([], [tsDrawSelecting, tsDrawSelPending]);
      Invalidate;
    end;

    inherited;

    // get information about the hit
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));

    if toRightClickSelect in FOptions.SelectionOptions then
      HandleMouseUp(Message, HitInfo);

  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMSetCursor(var Message: TWMSetCursor);

// Sets the hot node mouse cursor for the tree. Cursor changes for the header are handled in Header.HandleMessage.

var
  NewCursor: TCursor;
  HitInfo: THitInfo;
  P: TPoint;
  Node: PVirtualNode;

begin
  with Message do
  begin
    // Feature: design-time header #415
    // Allow header to handle cursor and return control's default if it did nothing
    if (CursorWnd = Handle) and not (tsPanning in FStates) then
    begin
      if not TVTHeaderCracker(FHeader).HandleMessage(TMessage(Message)) then
      begin
        // Apply own cursors only if there is no global cursor set.
        if Screen.Cursor = crDefault then
        begin
          // node resizing and hot tracking - for run-time only
          if not (csDesigning in ComponentState) then
          begin
            NewCursor := crDefault;
            if (toNodeHeightResize in FOptions.MiscOptions) then
            begin
              GetCursorPos(P);
              P := ScreenToClient(P);
              GetHitTestInfoAt(P.X, P.Y, True, HitInfo, []);
              if (hiOnItem in HitInfo.HitPositions) and
                 ([hiUpperSplitter, hiLowerSplitter] * HitInfo.HitPositions <> []) then
              begin
                if hiUpperSplitter in HitInfo.HitPositions then
                  Node := GetPreviousVisible(HitInfo.HitNode, True)
                else
                  Node := HitInfo.HitNode;

                if CanSplitterResizeNode(P, Node, HitInfo.HitColumn) then
                  NewCursor := crVSplit;
              end;
            end;

            if (NewCursor = crDefault) then
              if (toHotTrack in FOptions.PaintOptions) and Assigned(FCurrentHotNode) and (FHotCursor <> crDefault) then
                NewCursor := FHotCursor
              else
                NewCursor := Cursor;

            DoGetCursor(NewCursor);
          end
          else
            NewCursor := Cursor;
          Winapi.Windows.SetCursor(Screen.Cursors[NewCursor]);
          Message.Result := 1;
        end
        else
          inherited;
      end;
    end
    else
      inherited;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMSetFocus(var Msg: TWMSetFocus);

begin
  inherited;
  if (FSelectionCount > 0) or not (toGhostedIfUnfocused in FOptions.PaintOptions) then
    Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMSize(var Message: TWMSize);

begin
  inherited;

  // Need to update scroll bars here. This will cause a recursion because of the change of the client area
  // when changing a scrollbar. Usually this is no problem since with the second level recursion no change of the
  // window size happens (the same values for the scrollbars are set, which shouldn't cause a window size change).
  // Appearently, this applies not to all systems, however.
  if HandleAllocated and ([tsSizing, tsWindowCreating] * FStates = []) and (ClientHeight > 0) then
  try
    DoStateChange([tsSizing]);
    // This call will invalidate the entire non-client area which needs recalculation on resize.
    TVTHeaderCracker(FHeader).RescaleHeader;
    TVTHeaderCracker(FHeader).UpdateSpringColumns;
    UpdateScrollBars(True);

    if (tsEditing in FStates) and not FHeader.UseColumns then
      UpdateEditBounds;
  finally
    DoStateChange([], [tsSizing]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMThemeChanged(var Message: TMessage);

begin
  inherited;

  if StyleServices.Enabled and (toThemeAware in TreeOptions.PaintOptions) then
    DoStateChange([tsUseThemes])
  else
    DoStateChange([], [tsUseThemes]);

  // Updating the visuals here will not work correctly. Therefore we postpone
  // the update by using a timer.
  if not FChangingTheme then
    SetTimer(Handle, ThemeChangedTimer, ThemeChangedTimerDelay, nil);
  FChangingTheme := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMTimer(var Message: TWMTimer);

// centralized timer handling happens here

begin
  with Message do
  begin
    case TimerID of
      ExpandTimer:
        DoDragExpand;
      EditTimer:
        DoEdit;
      ScrollTimer:
        begin
          if tsScrollPending in FStates then
          begin
            Application.CancelHint;
            // Scroll delay has elapsed, set to normal scroll interval now.
            SetTimer(Handle, ScrollTimer, FAutoScrollInterval, nil);
            DoStateChange([tsScrolling], [tsScrollPending]);
          end;
          DoTimerScroll;
        end;
      ChangeTimer:
        if tsChangePending in FStates then // see issue #602
          DoChange(FLastChangedNode);
      StructureChangeTimer:
        DoStructureChange(FLastStructureChangeNode, FLastStructureChangeReason);
      SearchTimer:
        begin
          // When this event triggers then the user did not pressed any key for the specified timeout period.
          // Hence incremental searching is stopped.
          DoStateChange([], [tsIncrementalSearching]);
          StopTimer(SearchTimer);
          FSearchBuffer := '';
          FLastSearchNode := nil;
        end;
      ThemeChangedTimer:
        begin
          StopTimer(ThemeChangedTimer);
          RecreateWnd;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WMVScroll(var Message: TWMVScroll);

  //--------------- local functions -------------------------------------------

  function GetRealScrollPosition: TDimension;

  var
    SI: TScrollInfo;
    Bar: Integer;

  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Bar := SB_VERT;
    GetScrollInfo(Bar, SI);
    Result := SI.nTrackPos;
  end;

  //--------------- end local functions ---------------------------------------

begin
  case Message.ScrollCode of
    SB_BOTTOM:
      SetOffsetY(-FRoot.TotalHeight);
    SB_ENDSCROLL:
      begin
        DoStateChange([], [tsThumbTracking]);
        // Avoiding to adjust the horizontal scroll position while tracking makes scrolling much smoother
        // but we need to adjust the final position here then.
        UpdateScrollBars(True);
        // Really weird invalidation needed here (and I do it only because it happens so rarely), because
        // when showing the horizontal scrollbar while scrolling down using the down arrow button,
        // the button will be repainted on mouse up (at the wrong place in the far right lower corner)...
        RedrawWindow(nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE or RDW_NOCHILDREN);
      end;
    SB_LINEUP:
      SetOffsetY(FOffsetY + FScrollBarOptions.VerticalIncrement);
    SB_LINEDOWN:
      SetOffsetY(FOffsetY - FScrollBarOptions.VerticalIncrement);
    SB_PAGEUP:
      SetOffsetY(FOffsetY + ClientHeight);
    SB_PAGEDOWN:
      SetOffsetY(FOffsetY - ClientHeight);

    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        DoStateChange([tsThumbTracking]);
        SetOffsetY(-GetRealScrollPosition);
      end;
    SB_TOP:
      SetOffsetY(0);
  end;
  Message.Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AddToSelection(Node: PVirtualNode; NotifySynced: Boolean);

var
  Changed: Boolean;
  RemoveSyncAfterChange: Boolean;
begin
  if not FSelectionLocked then
  begin
    Assert(Assigned(Node), 'Node must not be nil!');
    Changed := InternalAddToSelection(Node, False);
    if Changed then
    begin
      UpdateNextNodeToSelect(Node);
      if (SelectedCount = 1) then
        FocusedNode := Node; // if only one node is selected, make sure the focused node changes with the selected node
      InvalidateNode(Node);
      RemoveSyncAfterChange := NotifySynced and not (tsSynchMode in fStates);
      if RemoveSyncAfterChange then
        Include(FStates, tsSynchMode);
      try
        Change(Node);
      finally
        if RemoveSyncAfterChange then
          Exclude(FStates, tsSynchMode);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AddToSelection(const NewItems: TNodeArray; NewLength: Integer; ForceInsert: Boolean = False);

// Adds the given items all at once into the current selection array. NewLength is the amount of
// nodes to add (necessary to allow NewItems to be larger than the actual used entries).
// ForceInsert is True if nodes must be inserted without consideration of level select constraint or
// already set selected flags (e.g. when loading from stream).
// Note: In the case ForceInsert is True the caller is responsible for making sure the new nodes aren't already in the
//       selection array!

var
  Changed: Boolean;

begin
  Changed := InternalAddToSelection(NewItems, NewLength, ForceInsert);
  if Changed then
  begin
    if NewLength = 1 then
    begin
      InvalidateNode(NewItems[0]);
      Change(NewItems[0]);
    end
    else
    begin
      Invalidate;
      Change(nil);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AdjustPaintCellRect(var PaintInfo: TVTPaintInfo; var NextNonEmpty: TColumnIndex);

// Used in descendants to modify the paint rectangle of the current column while painting a certain node.

begin
  // Since cells are always drawn from left to right the next column index is independent of the
  // bidi mode, but not the column borders, which might change depending on the cell's content.
  NextNonEmpty := FHeader.Columns.GetNextVisibleColumn(PaintInfo.Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AdjustPanningCursor(X, Y: TDimension);

// Triggered by a mouse move when wheel panning/scrolling is active.
// Loads the proper cursor which indicates into which direction scrolling is done.

var
  NewCursor: TPanningCursor;
  NewCursorHandle: HCURSOR;
  ScrollHorizontal,
  ScrollVertical: Boolean;

begin
  ScrollHorizontal := FRangeX > ClientWidth;
  ScrollVertical := FRangeY > ClientHeight;

  if (Abs(X - FLastClickPos.X) < 8) and (Abs(Y - FLastClickPos.Y) < 8) then
  begin
    // Mouse is in the neutral zone.
    if ScrollHorizontal then
    begin
      if ScrollVertical then
        NewCursor := TPanningCursor.MOVEALL
      else
        NewCursor := TPanningCursor.MOVEEW;
    end
    else
      NewCursor := TPanningCursor.MOVENS;
  end
  else
  begin
    // One of 8 directions applies: north, north-east, east, south-east, south, south-west, west and north-west.
    // Check also if scrolling in the particular direction is possible.
    if ScrollVertical and ScrollHorizontal then
    begin
      // All directions allowed.
      if X - FLastClickPos.X < -8 then
      begin
        // Left hand side.
        if Y - FLastClickPos.Y < -8 then
          NewCursor := TPanningCursor.MOVENW
        else
          if Y - FLastClickPos.Y > 8 then
            NewCursor := TPanningCursor.MOVESW
          else
            NewCursor := TPanningCursor.MOVEW;
      end
      else
        if X - FLastClickPos.X > 8 then
        begin
          // Right hand side.
          if Y - FLastClickPos.Y < -8 then
            NewCursor := TPanningCursor.MOVENE

          else
            if Y - FLastClickPos.Y > 8 then
              NewCursor := TPanningCursor.MOVESE
            else
              NewCursor := TPanningCursor.MOVEE;
        end
        else
        begin
          // Up or down.
          if Y < FLastClickPos.Y then
            NewCursor := TPanningCursor.MOVEN
          else
            NewCursor := TPanningCursor.MOVES;
        end;
    end
    else
      if ScrollHorizontal then
      begin
        // Only horizontal movement allowed.
        if X < FLastClickPos.X then
          NewCursor := TPanningCursor.MOVEW
        else
          NewCursor := TPanningCursor.MOVEE;
      end
      else
      begin
        // Only vertical movement allowed.
        if Y < FLastClickPos.Y then
          NewCursor := TPanningCursor.MOVEN
        else
          NewCursor := TPanningCursor.MOVES;
      end;
  end;

  // Now load the cursor and apply it.
  NewCursorHandle := LoadCursor(0, MAKEINTRESOURCE(NewCursor));
  if FPanningCursor <> NewCursorHandle then
  begin
    DeleteObject(FPanningCursor);
    FPanningCursor := NewCursorHandle;
    Winapi.Windows.SetCursor(FPanningCursor);
  end
  else
    DeleteObject(NewCursorHandle);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AdviseChangeEvent(StructureChange: Boolean; Node: PVirtualNode; Reason: TChangeReason);

// Used to register a delayed change event. If StructureChange is False then we have a selection change event (without
// a specific reason) otherwise it is a structure change.

begin
  if StructureChange then
  begin
    if tsStructureChangePending in FStates then
      StopTimer(StructureChangeTimer)
    else
      DoStateChange([tsStructureChangePending]);

    FLastStructureChangeNode := Node;
    if FLastStructureChangeReason = crIgnore then
      FLastStructureChangeReason := Reason
    else
      if Reason <> crIgnore then
        FLastStructureChangeReason := crAccumulated;
  end
  else
  begin
    if tsChangePending in FStates then
      StopTimer(ChangeTimer)
    else
      DoStateChange([tsChangePending]);

    FLastChangedNode := Node;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.AllocateInternalDataArea(Size: Cardinal): Cardinal;

// Simple registration method to be called by each descendant to claim their internal data area.
// Result is the offset from the begin of the node to the internal data area of the calling tree class.

begin
  Assert((FRoot = nil) or (FRoot.ChildCount = 0), 'Internal data allocation must be done before any node is created.');
  Result := TreeNodeSize + FTotalInternalDataSize;
  System.Inc(FTotalInternalDataSize, (Size + (SizeOf(Pointer) - 1)) and not (SizeOf(Pointer) - 1));
  InitRootNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Animate(Steps, Duration: Cardinal; Callback: TVTAnimationCallback; Data: Pointer);

// This method does the calculation part of an animation as used for node toggling and hint animations.
// Steps is the maximum amount of animation steps to do and Duration determines the milliseconds the animation
// has to run. Callback is a task specific method which is called in the loop for every step and Data is simply
// something to pass on to the callback.
// The callback is called with the current step, the current step size and the Data parameter. Since the step amount
// as well as the step size are possibly adjusted during the animation, it is impossible to determine if the current
// step is the last step, even if the original step amount is known. To solve this problem the callback will be
// called after the loop has finished with a step size of 0 indicating so to execute any post processing.

var
  StepSize,
  RemainingTime,
  RemainingSteps,
  NextTimeStep,
  CurrentStep,
  StartTime: Cardinal;
  CurrentTime: Int64;

begin
  if not (tsInAnimation in FStates) and (Duration > 0) then
  begin
    DoStateChange([tsInAnimation]);
    try
      RemainingTime := Duration;
      RemainingSteps := Steps;

      // Determine the initial step size which is either 1 if the needed steps are less than the number of
      // steps possible given by the duration or > 1 otherwise.
      StepSize := Round(Max(1, RemainingSteps / Duration));
      RemainingSteps := RemainingSteps div StepSize;
      CurrentStep := 0;

      while (RemainingSteps > 0) and (RemainingTime > 0) and not Application.Terminated do
      begin
        StartTime := timeGetTime;
        NextTimeStep := StartTime + RemainingTime div RemainingSteps;
        if not Callback(CurrentStep, StepSize, Data) then
          Break;

        // Keep duration for this step for rest calculation.
        CurrentTime := timeGetTime;
        // Wait until the calculated time has been reached.
        while CurrentTime < NextTimeStep do
          CurrentTime := timeGetTime;

        // Subtract the time this step really needed.
        if RemainingTime >= CurrentTime - StartTime then
        begin
          System.Dec(RemainingTime, CurrentTime - StartTime);
          System.Dec(RemainingSteps);
        end
        else
        begin
          RemainingTime := 0;
          RemainingSteps := 0;
        end;
        // If the remaining time per step is less than one time step then we have to decrease the
        // step count and increase the step size.
        if (RemainingSteps > 0) and ((RemainingTime div RemainingSteps) < 1) then
        begin
          repeat
            System.Inc(StepSize);
            RemainingSteps := RemainingTime div StepSize;
          until (RemainingSteps <= 0) or ((RemainingTime div RemainingSteps) >= 1);
        end;
        CurrentStep := Steps - RemainingSteps;
      end;

      if not Application.Terminated then
        Callback(0, 0, Data);
    finally
      DoStateChange([], [tsInAnimation]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.StartOperation(OperationKind: TVTOperationKind);

// Called to indicate that a long-running operation has been started.

begin
  System.Inc(FOperationCount);
  if FOperationCount = 1 then
    FOperationCanceled := False;
  DoStartOperation(OperationKind);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CalculateSelectionRect(X, Y: TDimension): Boolean;

// Recalculates old and new selection rectangle given that X, Y are new mouse coordinates.
// Returns True if there was a change since the last call.

var
  MaxValue: TDimension;

begin
  if tsDrawSelecting in FStates then
    FLastSelRect := FNewSelRect;
  FNewSelRect.BottomRight := Point(X + FEffectiveOffsetX, Y - FOffsetY);
  if FNewSelRect.Right < 0 then
    FNewSelRect.Right := 0;
  if FNewSelRect.Bottom < 0 then
    FNewSelRect.Bottom := 0;
  MaxValue := ClientWidth;
  if FRangeX > MaxValue then
    MaxValue := FRangeX;
  if FNewSelRect.Right > MaxValue then
    FNewSelRect.Right := MaxValue;
  MaxValue := ClientHeight;
  if FRangeY > MaxValue then
    MaxValue := FRangeY;
  if FNewSelRect.Bottom > MaxValue then
    FNewSelRect.Bottom := MaxValue;

  Result := not CompareMem(@FLastSelRect, @FNewSelRect, SizeOf(FNewSelRect));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CanAutoScroll: Boolean;

// Determines if auto scrolling is currently allowed.

var
  IsDropTarget: Boolean;
  IsDrawSelecting: Boolean;
  IsWheelPanning: Boolean;

begin
  // Don't scroll the client area if the header is currently doing tracking or dragging.
  // Do auto scroll only if there is a draw selection in progress or the tree is the current drop target or
  // wheel panning/scrolling is active.
  IsDropTarget := Assigned(FDragManager) and DragManager.IsDropTarget;
  IsDrawSelecting := [tsDrawSelPending, tsDrawSelecting] * FStates <> [];
  IsWheelPanning := tsPanning in FStates;
  Result := ((toAutoScroll in FOptions.AutoOptions) or IsWheelPanning) and
    (FHeader.States = []) and (IsDrawSelecting or IsDropTarget or (tsVCLDragging in FStates) or IsWheelPanning);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CanShowDragImage: Boolean;

// Determines whether a drag image should be shown.

begin
  Result := FDragImageKind <> diNoImage;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CanSplitterResizeNode(P: TPoint; Node: PVirtualNode; Column: TColumnIndex): Boolean;

begin
  Result := (toNodeHeightResize in FOptions.MiscOptions) and Assigned(Node) and (Node <> FRoot) and
            (Column > NoColumn) and (coFixed in FHeader.Columns[Column].Options);
  DoCanSplitterResizeNode(P, Node, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Change(Node: PVirtualNode);

begin
  AdviseChangeEvent(False, Node, crIgnore);

  if FUpdateCount = 0 then
  begin
    if (FChangeDelay > 0) and HandleAllocated and not (tsSynchMode in FStates) then
      SetTimer(Handle, ChangeTimer, FChangeDelay, nil)
    else
      DoChange(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend});
begin
  if (M <> D) then
  begin
    BeginUpdate();
    try
      ScaleNodeHeights(M, D);
      SetDefaultNodeHeight(MulDiv(FDefaultNodeHeight, M, D));
      Indent := MulDiv(Indent, M, D);
      FTextMargin := MulDiv(FTextMargin, M, D);
      FMargin := MulDiv(FMargin, M, D);
      FImagesMargin := MulDiv(FImagesMargin, M, D);
    finally
      EndUpdate();
    end;//try..finally
  end;// if M<>D
  inherited ChangeScale(M, D{$if CompilerVersion >= 31}, isDpiChange{$ifend});
  if (M <> D) then
  begin
    // Scale header
    TVTHeaderCracker(FHeader).ChangeScale(M, D);
    // Scale utility images, #796
    if FCheckImageKind = ckSystemDefault then begin
      FreeAndNil(FCheckImages);
      if HandleAllocated then
        FCheckImages := CreateSystemImageSet();
    end;
    UpdateHeaderRect();
    PrepareBitmaps(True, False); // See issue #991
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ScaleNodeHeights(M, D: TDimension);
var
  Run: PVirtualNode;
  lNewNodeTotalHeight: Cardinal;
begin
  // Scale also node heights
  BeginUpdate();
  try
    Run := GetFirstNoInit();
    while Assigned(Run) do
    begin
      if vsInitialized in Run.States then
        SetNodeHeight(Run, MulDiv(Run.NodeHeight, M, D))
      else // prevent initialization of non-initialzed nodes
      begin
        Run.SetNodeHeight(MulDiv(Run.NodeHeight, M, D));
        // The next three lines fix issue #1000
        lNewNodeTotalHeight := MulDiv(Run.TotalHeight, M, D);
        FRoot.TotalHeight := Cardinal(Int64(FRoot.TotalHeight) + Int64(lNewNodeTotalHeight) - Int64(Run.TotalHeight)); // Avoiding EIntOverflow exception.
        Run.TotalHeight := lNewNodeTotalHeight;
      end;
      Run := GetNextNoInit(Run);
    end; // while
  finally
    EndUpdate();
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ChangeTreeStatesAsync(EnterStates, LeaveStates: TVirtualTreeStates);
begin
  //TODO: If this works reliable, move to TWorkerThread
  if not (csDestroying in ComponentState) then
  begin
    AtomicIncrement(FPendingSyncProcs);
    TThread.Synchronize(nil, procedure
      begin
        //Decrement invoke refs
        AtomicDecrement(FPendingSyncProcs);
        // Prevent invalid combination tsUseCache + tsValidationNeeded (#915)
        if not ((tsUseCache in EnterStates) and (tsValidationNeeded in FStates + LeaveStates)) then
          DoStateChange(EnterStates, LeaveStates);
        if (tsValidating in FStates) and (tsValidating in LeaveStates) then
          UpdateEditBounds();
      end);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CheckParentCheckState(Node: PVirtualNode; NewCheckState: TCheckState): Boolean;

// Checks all siblings of node to determine which check state Node's parent must get.

var
  CheckCount,
  BoxCount: Cardinal;
  PartialCheck: Boolean;
  Run: PVirtualNode;

begin
  CheckCount := 0;
  BoxCount := 0;
  PartialCheck := False;
  Run := Node.Parent.FirstChild;
  while Assigned(Run) do
  begin
    if Run = Node then
    begin
      // The given node cannot be checked because it does not yet have its new check state (as this depends
      // on the outcome of this method). Instead NewCheckState is used as this contains the new state the node
      // will get if this method returns True.
      if Run.CheckType in [ctCheckBox, ctTriStateCheckBox] then
      begin
        System.Inc(BoxCount);
        if NewCheckState.IsChecked then
          System.Inc(CheckCount);
        PartialCheck := PartialCheck or (NewCheckState = csMixedNormal);
      end;
    end
    else
      if Run.CheckType in [ctCheckBox, ctTriStateCheckBox] then
      begin
        System.Inc(BoxCount);
        if GetCheckState(Run).IsChecked then
          System.Inc(CheckCount);
        PartialCheck := PartialCheck or (GetCheckState(Run) = csMixedNormal);
      end;
    Run := Run.NextSibling;
  end;

  if (CheckCount = 0) and not PartialCheck then
    NewCheckState := csUncheckedNormal
  else
    if CheckCount < BoxCount then
      NewCheckState := csMixedNormal
    else
      NewCheckState := csCheckedNormal;

  Node := Node.Parent;
  Result := DoChecking(Node, NewCheckState);
  if Result then
  begin
    DoCheckClick(Node, NewCheckState);
    // Recursively adjust parent of parent.
    // This is already done in the function DoCheckClick() called in the above line
    // We revent unnecessary upward recursion by commenting this code.
    //    with Node^ do
    //    begin
    //      if not (vsInitialized in Parent.States) then
    //        InitNode(Parent);
    //      if ([vsChecking, vsDisabled] * Parent.States = []) and (Parent <> FRoot) and
    //        (Parent.CheckType = ctTriStateCheckBox) then
    //        Result := CheckParentCheckState(Node, NewCheckState);
    //    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ClearTempCache;

// make sure the temporary node cache is in a reliable state

begin
  FTempNodeCache := nil;
  FTempNodeCount := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean;

// Returns True if the given column is to be considered as being empty. This will usually be determined by
// descendants as the base tree implementation has not enough information to decide.

begin
  Result := True;
  if Assigned(FOnGetCellIsEmpty) then
    FOnGetCellIsEmpty(Self, Node, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ComputeRTLOffset(ExcludeScrollBar: Boolean): TDimension;

// Computes the horizontal offset needed when all columns are automatically right aligned (in RTL bidi mode).
// ExcludeScrollBar determines if the left-hand vertical scrollbar is to be included (if visible) or not.

var
  HeaderWidth: TDimension;
  ScrollBarVisible: Boolean;
begin
  ScrollBarVisible := (FRangeY > ClientHeight) and (ScrollBarOptions.ScrollBars in [TScrollStyle.ssVertical, TScrollStyle.ssBoth]);
  if ScrollBarVisible then
    Result := GetSystemMetrics(SM_CXVSCROLL)
  else
    Result := 0;

  // Make everything right aligned.
  HeaderWidth := FHeaderRect.Right - FHeaderRect.Left;
  if FRangeX + Result <= HeaderWidth then
    Result := HeaderWidth - FRangeX;
  // Otherwise take only left-hand vertical scrollbar into account.

  if ScrollBarVisible and ExcludeScrollBar then
    Dec(Result, GetSystemMetrics(SM_CXVSCROLL));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CountLevelDifference(Node1, Node2: PVirtualNode): Integer;

// This method counts how many indentation levels the given nodes are apart. If both nodes have the same parent then the
// difference is 0 otherwise the result is basically GetNodeLevel(Node2) - GetNodeLevel(Node1), but with sign.
// If the result is negative then Node2 is less intended than Node1.

var
  Level1, Level2: Integer;

begin
  Assert(Assigned(Node1) and Assigned(Node2), 'Both nodes must be Assigned.');

  Level1 := 0;
  while Node1.Parent <> FRoot do
  begin
    System.Inc(Level1);
    Node1 := Node1.Parent;
  end;

  Level2 := 0;
  while Node2.Parent <> FRoot do
  begin
    System.Inc(Level2);
    Node2 := Node2.Parent;
  end;

  Result := Level2 - Level1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CountVisibleChildren(Node: PVirtualNode): Cardinal;

// Returns the number of visible child nodes of the given node.

begin
  Result := 0;

  // The node's direct children...
  if vsExpanded in Node.States then
  begin
    // ...and their children.
    Node := Node.FirstChild;
    while Assigned(Node) do
    begin
      if vsVisible in Node.States then
        System.Inc(Result, CountVisibleChildren(Node) + Cardinal(IfThen(IsEffectivelyVisible[Node], 1)));
      Node := Node.NextSibling;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CreateParams(var Params: TCreateParams);

const
  ScrollBar: array[TScrollStyle] of Cardinal = (0, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);

begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or ScrollBar[ScrollBarOptions.ScrollBars];
    if toFullRepaintOnResize in FOptions.MiscOptions then
      WindowClass.style := WindowClass.style or CS_HREDRAW or CS_VREDRAW
    else
      WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
    if FBorderStyle = bsSingle then
    begin
      if Ctl3D then
      begin
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
        Style := Style and not WS_BORDER;
      end
      else
        Style := Style or WS_BORDER;
    end
    else
      Style := Style and not WS_BORDER;

    AddBiDiModeExStyle(ExStyle);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CreateWnd;

// Initializes data which depends on a valid window handle.

begin
  VclStyleChanged(); // Moved here due to issue #986
  DoStateChange([tsWindowCreating]);
  inherited;
  DoStateChange([], [tsWindowCreating]);

  if not Assigned(FCheckImages) then
    FCheckImages := CreateSystemImageSet();

  if ((StyleServices.Enabled ) and (toThemeAware in TreeOptions.PaintOptions)  ) then
  begin
    DoStateChange([tsUseThemes]);
    if (toUseExplorerTheme in FOptions.PaintOptions) then
    begin
      DoStateChange([tsUseExplorerTheme]);
      SetWindowTheme('explorer');
    end
    else
      DoStateChange([], [tsUseExplorerTheme]);
  end
  else
    DoStateChange([], [tsUseThemes, tsUseExplorerTheme]);

  AutoScale();
  // Because of the special recursion and update stopper when creating the window (or resizing it)
  // we have to manually trigger the auto size calculation here.
  if hsNeedScaling in FHeader.States then
    TVTHeaderCracker(FHeader).RescaleHeader;
  if hoAutoResize in FHeader.Options then
    TVirtualTreeColumnsCracker(FHeader.Columns).AdjustAutoSize(InvalidColumn);

  PrepareBitmaps(True, True);

  // Register tree as OLE drop target.
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and ((hoDrag in Header.Options) or (toAcceptOLEDrop in TreeOptions.MiscOptions)) then // will be done in Loaded after all inherited settings are loaded from the DFMs
    RegisterDragDrop(Handle, DragManager as IDropTarget);

  UpdateScrollBars(True);
  UpdateHeaderRect;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FakeReadIdent(Reader: TReader);
begin
  Assert(Reader.NextValue = vaIdent);
  Reader.ReadIdent;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DecVisibleCount;
begin
  System.Dec(FVisibleCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DefineProperties(Filer: TFiler);

// There were heavy changes in some properties during development of VT. This method helps to make migration easier
// by reading old properties manually and put them into the new properties as appropriate.
// Note: these old properties are never written again and silently disappear.
// June 2002: Meanwhile another task is done here too: working around the problem that TCollection is not streamed
//            correctly when using Visual Form Inheritance (VFI).

var
  StoreIt: Boolean;

begin
  inherited;

  // The header can prevent writing columns altogether.
  if TVTHeaderCracker(FHeader).CanWriteColumns then
  begin
    // Check if we inherit from an ancestor form (Visual Form Inheritance).
    StoreIt := Filer.Ancestor = nil;
    // If there is an ancestor then save columns only if they are different to the base set.
    if not StoreIt then
      StoreIt := not FHeader.Columns.Equals(TBaseVirtualTree(Filer.Ancestor).FHeader.Columns);
  end
  else
    StoreIt := False;

  Filer.DefineProperty('Columns', TVTHeaderCracker(FHeader).ReadColumns, TVTHeaderCracker(FHeader).WriteColumns, StoreIt);

  // #622 made old DFMs incompatible with new VTW - so the program is compiled successfully
  //    and then suddenly crashes at user site in runtime.
  Filer.DefineProperty('CheckImageKind', FakeReadIdent, nil, false);
  /// #730 removed property HintAnimation
  Filer.DefineProperty('HintAnimation', FakeReadIdent, nil, false);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DetermineDropMode(const P: TPoint; var HitInfo: THitInfo; var NodeRect: TRect): TDropMode;

// Determine the DropMode.

var
  ImageHit: Boolean;
  LabelHit: Boolean;
  ItemHit: Boolean;

begin
  ImageHit := HitInfo.HitPositions * [hiOnNormalIcon, hiOnStateIcon] <> [];
  LabelHit := hiOnItemLabel in HitInfo.HitPositions;
  ItemHit := (hiOnItem in HitInfo.HitPositions);

  // In report mode only direct hits of the node captions/images in the main column are accepted as hits.
  if (toReportMode in FOptions.MiscOptions) and not (ItemHit or ((LabelHit or ImageHit) and
    (HitInfo.HitColumn = FHeader.MainColumn))) then
    HitInfo.HitNode := nil;

  if Assigned(HitInfo.HitNode) then
  begin
    if LabelHit or ImageHit or not (toShowDropmark in FOptions.PaintOptions) then
      Result := dmOnNode
    else
      if Divide(NodeRect.Top + NodeRect.Bottom, 2) > P.Y then
        Result := dmAbove
      else
        Result := dmBelow;
  end
  else
    Result := dmNowhere;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DetermineHiddenChildrenFlag(Node: PVirtualNode);

// Update the hidden children flag of the given node.

var
  Run: PVirtualNode;

begin
  if Node.ChildCount = 0 then
  begin
    if vsHasChildren in Node.States then
      Exclude(Node.States, vsAllChildrenHidden)
    else
      Include(Node.States, vsAllChildrenHidden);
  end
  else
  begin
    // Iterate through all siblings and stop when one visible is found.
    Run := Node.FirstChild;
    while Assigned(Run) and not IsEffectivelyVisible[Run] do
      Run := Run.NextSibling;
    if Assigned(Run) then
      Exclude(Node.States, vsAllChildrenHidden)
    else
      Include(Node.States, vsAllChildrenHidden);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DetermineHiddenChildrenFlagAllNodes;

var
  Run: PVirtualNode;

begin
  Run := GetFirstNoInit(False);
  while Assigned(Run) do
  begin
    DetermineHiddenChildrenFlag(Run);
    Run := GetNextNoInit(Run);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DetermineHitPositionLTR(var HitInfo: THitInfo; Offset, Right: TDimension;
  Alignment: TAlignment);

// This method determines the hit position within a node with left-to-right orientation.

var
  MainColumnHit: Boolean;
  lIndent,
  TextWidth,
  ImageOffset: TDimension;
  lOffsets: TVTOffsets;
begin
  MainColumnHit := HitInfo.HitColumn = FHeader.MainColumn;
  GetOffsets(HitInfo.HitNode, lOffsets, ofsRightOfText, HitInfo.HitColumn);

  if (MainColumnHit and (Offset < lOffsets[ofsCheckbox])) then
  begin
    // Position is to the left of calculated indentation which can only happen for the main column.
    // Check whether it corresponds to a button/checkbox.
    if (toShowButtons in FOptions.PaintOptions) and (vsHasChildren in HitInfo.HitNode.States) then
    begin
      // Position of button is interpreted very generously to avoid forcing the user
      // to click exactly into the 9x9 pixels area. The entire node height and one full
      // indentation level is accepted as button hit.
      if Offset >= lOffsets[ofsCheckbox] - FIndent then
        Include(HitInfo.HitPositions, hiOnItemButton);
      if Offset > lOffsets[ofsToggleButton] then
        Include(HitInfo.HitPositions, hiOnItemButtonExact);
    end;
    // no button hit so position is on indent
    if HitInfo.HitPositions = [] then
      Include(HitInfo.HitPositions, hiOnItemIndent);
  end
  else
  begin
    // The next hit positions can be:
    //   - on the check box
    //   - on the state image
    //   - on the normal image
    //   - to the left of the text area
    //   - on the label or
    //   - to the right of the text area
    // (in this order).

    // In report mode no hit other than in the main column is possible.
    if MainColumnHit or not (toReportMode in FOptions.MiscOptions) then
    begin
      if MainColumnHit and (Offset < lOffsets[ofsStateImage]) then
      begin
        HitInfo.HitPositions := [hiOnItem];
        if (HitInfo.HitNode.CheckType <> ctNone) then
          Include(HitInfo.HitPositions, hiOnItemCheckBox);
      end
      else
      begin
        ImageOffset := lOffsets[ofsImage];
        if Offset < ImageOffset then
          Include(HitInfo.HitPositions, hiOnStateIcon)
        else
        begin
          ImageOffset := lOffsets[ofsLabel];
          if Offset < ImageOffset then
            Include(HitInfo.HitPositions, hiOnNormalIcon)
          else
          begin
            TextWidth := lOffsets[ofsRightOfText] - lOffsets[ofsText];
            // ImageOffset contains now the left border of the node label area. This is used to calculate the
            // correct alignment in the column.

            // Check if the text can be aligned at all. This is only possible if there is enough room
            // in the remaining text rectangle.
            if TextWidth > Right - ImageOffset then
              Include(HitInfo.HitPositions, hiOnItemLabel)
            else
            begin
              case Alignment of
                taCenter:
                  begin
                    lIndent := Divide(ImageOffset + Right - TextWidth, 2);
                    if Offset < lIndent then
                      Include(HitInfo.HitPositions, hiOnItemLeft)
                    else
                      if Offset < lIndent + TextWidth then
                        Include(HitInfo.HitPositions, hiOnItemLabel)
                      else
                        Include(HitInfo.HitPositions, hiOnItemRight);
                  end;
                taRightJustify:
                  begin
                    lIndent := Right - TextWidth;
                    if Offset < lIndent then
                      Include(HitInfo.HitPositions, hiOnItemLeft)
                    else
                      Include(HitInfo.HitPositions, hiOnItemLabel);
                  end;
              else // taLeftJustify
                if Offset < ImageOffset + TextWidth then
                  Include(HitInfo.HitPositions, hiOnItemLabel)
                else
                  Include(HitInfo.HitPositions, hiOnItemRight);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DetermineHitPositionRTL(var HitInfo: THitInfo; Offset, Right: TDimension; Alignment: TAlignment);

// This method determines the hit position within a node with right-to-left orientation.

var
  MainColumnHit: Boolean;
  Run: PVirtualNode;
  Indent,
  TextWidth,
  ImageOffset: TDimension;

begin
  MainColumnHit := HitInfo.HitColumn = FHeader.MainColumn;

  // If columns are not used or the main column is hit then the tree indentation must be considered too.
  if MainColumnHit then
  begin
    if toFixedIndent in FOptions.PaintOptions then
      Dec(Right, FIndent)
    else
    begin
      Run := HitInfo.HitNode;
      while (Run.Parent <> FRoot) do
      begin
        Dec(Right, FIndent);
        Run := Run.Parent;
      end;
      if toShowRoot in FOptions.PaintOptions then
        Dec(Right, FIndent);
    end;
  end;

  if Offset >= Right then
  begin
    // Position is to the right of calculated indentation which can only happen for the main column.
    // Check whether it corresponds to a button/checkbox.
    if (toShowButtons in FOptions.PaintOptions) and (vsHasChildren in HitInfo.HitNode.States) then
    begin
      // Position of button is interpreted very generously to avoid forcing the user
      // to click exactly into the 9x9 pixels area. The entire node height and one full
      // indentation level is accepted as button hit.
      if Offset <= Right + FIndent then
        Include(HitInfo.HitPositions, hiOnItemButton);
      if Offset <= Right + FPlusBM.Width then
        Include(HitInfo.HitPositions, hiOnItemButtonExact);
    end;
    // no button hit so position is on indent
    if HitInfo.HitPositions = [] then
      Include(HitInfo.HitPositions, hiOnItemIndent);
  end
  else
  begin
    // The next hit positions can be:
    //   - on the check box
    //   - on the state image
    //   - on the normal image
    //   - to the left of the text area
    //   - on the label or
    //   - to the right of the text area
    // (in this order).

    // In report mode no hit other than in the main column is possible.
    if MainColumnHit or not (toReportMode in FOptions.MiscOptions) then
    begin
      ImageOffset := Right - FMargin;

      // Check support is only available for the main column.
      if MainColumnHit and (toCheckSupport in FOptions.MiscOptions) and Assigned(FCheckImages) and
        (HitInfo.HitNode.CheckType <> ctNone) then
        Dec(ImageOffset, FCheckImages.Width + FImagesMargin);

      if MainColumnHit and (Offset > ImageOffset) then
      begin
        HitInfo.HitPositions := [hiOnItem];
        if (HitInfo.HitNode.CheckType <> ctNone) then
          Include(HitInfo.HitPositions, hiOnItemCheckBox);
      end
      else
      begin
        Dec(ImageOffset, GetImageSize(HitInfo.HitNode, ikState, HitInfo.HitColumn).cx);
        if Offset > ImageOffset then
          Include(HitInfo.HitPositions, hiOnStateIcon)
        else
        begin
          Dec(ImageOffset, GetImageSize(HitInfo.HitNode, ikNormal, HitInfo.HitColumn).cx);
          if Offset > ImageOffset then
            Include(HitInfo.HitPositions, hiOnNormalIcon)
          else
          begin
            // ImageOffset contains now the right border of the node label area. This is used to calculate the
            // correct alignment in the column.
            TextWidth := DoGetNodeWidth(HitInfo.HitNode, HitInfo.HitColumn);

            // Check if the text can be aligned at all. This is only possible if there is enough room
            // in the remaining text rectangle.
            if TextWidth > ImageOffset then
              Include(HitInfo.HitPositions, hiOnItemLabel)
            else
            begin
              // Consider bidi mode here. In RTL context does left alignment actually mean right alignment
              // and vice versa.
              ChangeBiDiModeAlignment(Alignment);

              case Alignment of
                taCenter:
                  begin
                    Indent := Divide(ImageOffset - TextWidth, 2);
                    if Offset < Indent then
                      Include(HitInfo.HitPositions, hiOnItemLeft)
                    else
                      if Offset < Indent + TextWidth then
                        Include(HitInfo.HitPositions, hiOnItemLabel)
                      else
                        Include(HitInfo.HitPositions, hiOnItemRight);
                  end;
                taRightJustify:
                  begin
                    Indent := ImageOffset - TextWidth;
                    if Offset < Indent then
                      Include(HitInfo.HitPositions, hiOnItemLeft)
                    else
                      Include(HitInfo.HitPositions, hiOnItemLabel);
                  end;
              else // taLeftJustify
                if Offset > TextWidth then
                  Include(HitInfo.HitPositions, hiOnItemRight)
                else
                  Include(HitInfo.HitPositions, hiOnItemLabel);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DetermineLineImageAndSelectLevel(Node: PVirtualNode; var LineImage: TLineImage): Integer;

// This method is used during paint cycles and initializes an array of line type IDs. These IDs are used to paint
// the tree lines in front of the given node.
// Additionally an initial count of selected parents is determined and returned which is used for specific painting.

var
  X: Integer;
  Indent: Integer;
  Run: PVirtualNode;

begin
  Result := 0;
  if toShowRoot in FOptions.PaintOptions then
    X := 1
  else
    X := 0;
  Run := Node;
  // Determine indentation level of top node.
  while Run.Parent <> FRoot do
  begin
    System.Inc(X);
    Run := Run.Parent;
    // Count selected nodes (FRoot is never selected).
    if vsSelected in Run.States then
      System.Inc(Result);
  end;

  // Set initial size of line index array, this will automatically initialized all entries to ltNone.
  SetLength(LineImage, X);
  Indent := X - 1;

  // Only use lines if requested.
  if (toShowTreeLines in FOptions.PaintOptions) and
     (not (toHideTreeLinesIfThemed in FOptions.PaintOptions) or not (tsUseThemes in FStates)) then
  begin
    if toChildrenAbove in FOptions.PaintOptions then
    begin
      System.Dec(X);
      if not HasVisiblePreviousSibling(Node) then
      begin
        if (Node.Parent <> FRoot) or HasVisibleNextSibling(Node) then
          LineImage[X] := ltBottomRight
        else
          LineImage[X] := ltRight;
      end
      else
        if (Node.Parent = FRoot) and (not HasVisibleNextSibling(Node)) then
          LineImage[X] := ltTopRight
        else
          LineImage[X] := ltTopDownRight;

      // Now go up to the root to determine the rest.
      Run := Node.Parent;
      while Run <> FRoot do
      begin
        System.Dec(X);
        if HasVisiblePreviousSibling(Run) then
          LineImage[X] := ltTopDown
        else
          LineImage[X] := ltNone;

        Run := Run.Parent;
      end;
    end
    else
    begin
      // Start over parent traversal if necessary.
      Run := Node;

      if Run.Parent <> FRoot then
      begin
        // The very last image (the one immediately before the item label) is different.
        if HasVisibleNextSibling(Run) then
          LineImage[X - 1] := ltTopDownRight
        else
          LineImage[X - 1] := ltTopRight;
        Run := Run.Parent;

        // Now go up all parents.
        repeat
          if Run.Parent = FRoot then
            Break;
          System.Dec(X);
          if HasVisibleNextSibling(Run) then
            LineImage[X - 1] := ltTopDown
          else
            LineImage[X - 1] := ltNone;
          Run := Run.Parent;
        until False;
      end;

      // Prepare root level. Run points at this stage to a top level node.
      if (toShowRoot in FOptions.PaintOptions) and ((toShowTreeLines in FOptions.PaintOptions) and
         (not (toHideTreeLinesIfThemed in FOptions.PaintOptions) or not (tsUseThemes in FStates))) then
      begin
        // Is the top node a root node?
        if Run = Node then
        begin
          // First child gets the bottom-right bitmap if it isn't also the only child.
          if IsFirstVisibleChild(FRoot, Run) then
            // Is it the only child?
            if IsLastVisibleChild(FRoot, Run) then
              LineImage[0] := ltRight
            else
              LineImage[0] := ltBottomRight
          else
            // real last child
            if IsLastVisibleChild(FRoot, Run) then
              LineImage[0] := ltTopRight
            else
              LineImage[0] := ltTopDownRight;
        end
        else
        begin
          // No, top node is not a top level node. So we need different painting.
          if HasVisibleNextSibling(Run) then
            LineImage[0] := ltTopDown
          else
            LineImage[0] := ltNone;
        end;
      end;
    end;
  end;

  if (tsUseExplorerTheme in FStates) and HasChildren[Node] and (Indent >= 0)
       and not ((vsAllChildrenHidden in Node.States) and (toAutoHideButtons in TreeOptions.AutoOptions)) then
    LineImage[Indent] := ltNone;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DetermineNextCheckState(CheckType: TCheckType; CheckState: TCheckState): TCheckState;

// Determines the next check state in case the user click the check image or pressed the space key.

begin
  case CheckType of
    ctTriStateCheckBox,
    ctButton,
    ctCheckBox:
    begin
      Result := CheckState.GetToggled();
    end;//ctCheckbox
    ctRadioButton:
      Result := csCheckedNormal;
  else
    Result := csMixedNormal;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DetermineScrollDirections(X, Y: TDimension): TScrollDirections;

// Determines which direction the client area must be scrolled depending on the given position.

begin
  Result:= [];

  if CanAutoScroll then
  begin
    // Calculation for wheel panning/scrolling is a bit different to normal auto scroll.
    if tsPanning in FStates then
    begin
      if (X - FLastClickPos.X) < -8 then
        Include(Result, TScrollDirection.sdLeft);
      if (X - FLastClickPos.X) > 8 then
        Include(Result, TScrollDirection.sdRight);

      if (Y - FLastClickPos.Y) < -8 then
        Include(Result, TScrollDirection.sdUp);
      if (Y - FLastClickPos.Y) > 8 then
        Include(Result, TScrollDirection.sdDown);
    end
    else
    begin
      if (X < FDefaultNodeHeight) and (FEffectiveOffsetX <> 0) then
        Include(Result, TScrollDirection.sdLeft);
      if (ClientWidth + FEffectiveOffsetX < FRangeX) and (X > ClientWidth - FDefaultNodeHeight) then
        Include(Result, TScrollDirection.sdRight);

      if (Y < FDefaultNodeHeight) and (FOffsetY <> 0) then
        Include(Result, TScrollDirection.sdUp);
      if (ClientHeight - FOffsetY < FRangeY) and (Y > ClientHeight - FDefaultNodeHeight) then
        Include(Result, TScrollDirection.sdDown);

      // Since scrolling during dragging is not handled via the timer we do a check here whether the auto
      // scroll timeout already has elapsed or not.
      if (Result <> []) and
        ((Assigned(FDragManager) and DragManager.IsDropTarget) or
        (FindDragTarget(Point(X, Y), False) = Self)) then
      begin
        if FDragScrollStart = 0 then
          FDragScrollStart := timeGetTime;
        // Reset any scroll direction to avoid scroll in the case the user is dragging and the auto scroll time has not
        // yet elapsed.
        if ((Int64(timeGetTime) - FDragScrollStart) < FAutoScrollDelay) then
          Result := [];
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoAddToSelection(Node: PVirtualNode);
begin
  if Assigned(FOnAddToSelection) then
    FOnAddToSelection(Self, Node);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoAdvancedHeaderDraw(var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);

begin
  if Assigned(FOnAdvancedHeaderDraw) then
    FOnAdvancedHeaderDraw(FHeader, PaintInfo, Elements);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);

begin
  if Assigned(FOnAfterCellPaint) then
    FOnAfterCellPaint(Self, Canvas, Node, Column, CellRect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);

begin
  if Assigned(FOnAfterItemErase) then
    FOnAfterItemErase(Self, Canvas, Node, ItemRect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoAfterItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);

begin
  if Assigned(FOnAfterItemPaint) then
    FOnAfterItemPaint(Self, Canvas, Node, ItemRect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoAfterPaint(Canvas: TCanvas);

begin
  if Assigned(FOnAfterPaint) then
    FOnAfterPaint(Self, Canvas);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoAutoScroll(X, Y: TDimension);

begin
  FScrollDirections := DetermineScrollDirections(X, Y);

  if  not (tsPanning in FStates) then
  begin
    if FScrollDirections = [] then
    begin
      if ((FStates * [tsScrollPending, tsScrolling]) <> []) then
      begin
        StopTimer(ScrollTimer);
        DoStateChange([], [tsScrollPending, tsScrolling]);
      end;
    end
    else
    begin
      // start auto scroll if not yet done
      if (FStates * [tsScrollPending, tsScrolling]) = [] then
      begin
        DoStateChange([tsScrollPending]);
        SetTimer(Handle, ScrollTimer, FAutoScrollDelay, nil);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoBeforeDrag(Node: PVirtualNode; Column: TColumnIndex): Boolean;

begin
  Result := False;
  if Assigned(FOnDragAllowed) then
    FOnDragAllowed(Self, Node, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

var
  UpdateRect: TRect;

begin
  if Assigned(FOnBeforeCellPaint) then
  begin
    if CellPaintMode = cpmGetContentMargin then
    begin
      // Prevent drawing if we are only about to get the margin. As this also clears the update rect we need to save it.
      GetUpdateRect(Handle, UpdateRect, False);
      SetUpdateState(True);
    end;

    Canvas.Font.Assign(Self.Font);  // Fixes issue #298
    FOnBeforeCellPaint(Self, Canvas, Node, Column, CellPaintMode, CellRect, ContentRect);

    if CellPaintMode = cpmGetContentMargin then
      SetUpdateState(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var Color: TColor;
  var EraseAction: TItemEraseAction);

begin
  if Assigned(FOnBeforeItemErase) then
    FOnBeforeItemErase(Self, Canvas, Node, ItemRect, Color, EraseAction);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoBeforeItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect): Boolean;

begin
  // By default custom draw will not be used, so the tree handles drawing the node.
  Result := False;
  if Assigned(FOnBeforeItemPaint) then
    FOnBeforeItemPaint(Self, Canvas, Node, ItemRect, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoBeforePaint(Canvas: TCanvas);

begin
  if Assigned(FOnBeforePaint) then
    FOnBeforePaint(Self, Canvas);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoCancelEdit(): Boolean;

// Called when the current edit action or a pending edit must be cancelled.

begin
  Result := DoEndEdit(True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoEndEdit(pCancel: Boolean = False): Boolean;

// Called to finish a current edit action or stop the edit timer if an edit operation is pending.
// Pass True if the edit should be cancelled, pass False if the new text should be used and saved.
// Returns True if editing was successfully ended/canceled or the control was not in edit mode.
// Returns False if the control could not leave the edit mode e.g. due to an invalid value that was entered.

begin
  StopTimer(EditTimer);
  DoStateChange([], [tsEditPending]);
  if not (tsEditing in FStates) then
    Exit(True);
  if pCancel then
    Result := FEditLink.CancelEdit
  else
    Result := FEditLink.EndEdit;
  if Result then
  begin
    DoStateChange([], [tsEditing]);
    FEditLink := nil;
    if Assigned(FOnEdited) then
      FOnEdited(Self, FFocusedNode, FEditColumn);
  end;
  TrySetFocus();
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

begin
  if Assigned(FOnEditing) then
    FOnEditing(Self, Node, Column, Allowed);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoCanSplitterResizeNode(P: TPoint; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);

begin
  if Assigned(FOnCanSplitterResizeNode) then
    FOnCanSplitterResizeNode(Self, P, Node, Column, Allowed);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoChange(Node: PVirtualNode);

begin
  StopTimer(ChangeTimer);
  if Assigned(FOnChange) then
    FOnChange(Self, Node);

  // This is a good place to reset the cached node. This is the same as the node passed in here.
  // This is necessary to allow descendants to override this method and get the node then.
  DoStateChange([], [tsChangePending]);
  FLastChangedNode := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoCheckClick(Node: PVirtualNode; NewCheckState: TCheckState);

begin
  if ChangeCheckState(Node, NewCheckState) then
  begin
    DoChecked(Node);
    if SyncCheckstateWithSelection[Node] then
    begin
      // selection should follow check state
      if (NewCheckState = csCheckedNormal) then
        Selected[node] := true
      else
        Selected[node] := false;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoChecked(Node: PVirtualNode);

begin
  if Assigned(FOnChecked) then
    FOnChecked(Self, Node);
  if (Self.UpdateCount = 0) then // See issue #1174
    NotifyAccessibleEvent();
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoChecking(Node: PVirtualNode; var NewCheckState: TCheckState): Boolean;

// Determines if a node is allowed to change its check state to NewCheckState.

begin
  if (toReadOnly in FOptions.MiscOptions) or (vsDisabled in Node.States) then
    Result := False
  else
  begin
    Result := True;
    if Assigned(FOnChecking) then
      FOnChecking(Self, Node, NewCheckState, Result);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoCollapsed(Node: PVirtualNode);
var
  lFirstSelected: PVirtualNode;
  lParent: PVirtualNode;
begin
  if Assigned(FOnCollapsed) then
    FOnCollapsed(Self, Node);

  if (Self.UpdateCount = 0) then // See issue #1174
    NotifyAccessibleEvent();

  if (toAlwaysSelectNode in TreeOptions.SelectionOptions) then
  begin
    // Select the next visible parent if the currently selected node gets invisible due to a collapse
    // This makes the VT behave more like the Win32 custom TreeView control
    // This makes only sense no no multi selection is allowed and if there is a selected node at all
    lFirstSelected := GetFirstSelected();
    if Assigned(lFirstSelected) and not FullyVisible[lFirstSelected] then
    begin
      lParent := GetVisibleParent(lFirstSelected);
      Selected[lFirstSelected] := False;
      Selected[lParent] := True;
    end;//if
    //if there is (still) no selected node, then use FNextNodeToSelect to select one
    if SelectedCount = 0 then
      EnsureNodeSelected(False);
  end;//if
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoCollapsing(Node: PVirtualNode): Boolean;

begin
  Result := True;
  if Assigned(FOnCollapsing) then
    FOnCollapsing(Self, Node, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoColumnChecked(Column: TColumnIndex);
begin
  if Assigned(FOnColumnChecked) then
    FOnColumnChecked(Self.FHeader, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoColumnChecking(Column: TColumnIndex; var NewCheckState: TCheckState): Boolean;

// Determines if a column is allowed to change its check state to NewCheckState.

begin
  Result := True;
  if Assigned(FOnColumnChecking) then
    FOnColumnChecking(Self.Header, Column, NewCheckState, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoColumnClick(Column: TColumnIndex; Shift: TShiftState);

begin
  if Assigned(FOnColumnClick) then
    FOnColumnClick(Self, Column, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoColumnDblClick(Column: TColumnIndex; Shift: TShiftState);

begin
  if Assigned(FOnColumnDblClick) then
    FOnColumnDblClick(Self, Column, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoColumnHeaderSpanning(Column: TColumnIndex; var Count: Integer);
begin
  if Assigned(FOnColumnHeaderSpanning) then
    FOnColumnHeaderSpanning(Self.Header, Column, Count);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoColumnResize(Column: TColumnIndex);

var
  R: TRect;
  Run: PVirtualNode;

begin
  if not (csLoading in ComponentState) and HandleAllocated then
  begin
    // Reset all vsHeightMeasured flags if we are in multiline mode.
    Run := GetFirstInitialized;
    while Assigned(Run) do
    begin
        if vsMultiline in Run.States then
        Exclude(Run.States, vsHeightMeasured);
      Run := GetNextInitialized(Run);
    end;
    if Header.Columns.UpdateCount = 0 then
      UpdateHorizontalScrollBar(True);
    if Column > NoColumn then
    begin
      // Invalidate client area from the current column all to the right (or left in RTL mode).
      R := ClientRect;
      if not (toAutoSpanColumns in FOptions.AutoOptions) then
        if UseRightToLeftAlignment then
          R.Right := FHeader.Columns[Column].Left + FHeader.Columns[Column].Width + ComputeRTLOffset
        else
          R.Left := FHeader.Columns[Column].Left;
      InvalidateRect(@R, False);
      FHeader.Invalidate(FHeader.Columns[Column], True);
    end;
    if [hsColumnWidthTracking, hsResizing] * FHeader.States = [hsColumnWidthTracking] then
      UpdateWindow();

    if not (IsUpdating) then
      UpdateDesigner; // design time only

    if Assigned(FOnColumnResize) and not (hsResizing in FHeader.States) then
      FOnColumnResize(FHeader, Column);

    // If the tree is currently in edit state then notify edit link.
    if tsEditing in FStates then
      UpdateEditBounds;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoColumnVisibilityChanged(const Column: TColumnIndex; Visible: Boolean);
  // Triggers the OnColumnVisibilityChanged event.</summary>
begin
  if Assigned(OnColumnVisibilityChanged) then
    OnColumnVisibilityChanged(Self, Column, Visible);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;

begin
  Result := 0;
  if Assigned(FOnCompareNodes) then
    FOnCompareNodes(Self, Node1, Node2, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoCreateDataObject: IDataObject;

begin
  Result := nil;
  if Assigned(FOnCreateDataObject) then
    FOnCreateDataObject(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoCreateDragManager: IVTDragManager;

begin
  Result := nil;
  if Assigned(FOnCreateDragManager) then
    FOnCreateDragManager(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;

begin
  Result := nil;
  if Assigned(FOnCreateEditor) then
    FOnCreateEditor(Self, Node, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoDragging(P: TPoint);

// Initiates finally the drag'n drop operation and returns after DD is finished.

  //--------------- local function --------------------------------------------

  function GetDragOperations: Integer;

  begin
    if FDragOperations = [] then
      Result := DROPEFFECT_COPY or DROPEFFECT_MOVE or DROPEFFECT_LINK
    else
    begin
      Result := 0;
      if doCopy in FDragOperations then
        Result := Result or DROPEFFECT_COPY;
      if doLink in FDragOperations then
        Result := Result or DROPEFFECT_LINK;
      if doMove in FDragOperations then
        Result := Result or DROPEFFECT_MOVE;
    end;
  end;

  //--------------- end local function ----------------------------------------

var
  AllowedEffects: Integer;
  DragObject: TDragObject;

  DataObject: IDataObject;

begin
  DataObject := nil;
  // Dragging is dragging, nothing else.
  DoCancelEdit;

  if Assigned(FCurrentHotNode) then
  begin
    InvalidateNode(FCurrentHotNode);
    FCurrentHotNode := nil;
  end;
  // Select the focused node if not already done.
  if Assigned(FFocusedNode) and not (vsSelected in FFocusedNode.States) then
  begin
    InternalAddToSelection(FFocusedNode, False);
    InvalidateNode(FFocusedNode);
  end;

  UpdateWindow();

  // Keep a list of all currently selected nodes as this list might change,
  // but we have probably to delete currently selected nodes.
  FDragSelection := GetSortedSelection(True);
  try
    DoStateChange([tsOLEDragging], [tsOLEDragPending, tsClearPending]);

    // An application might create a drag object like used during VCL dd. This is not required for OLE dd but
    // required as parameter.
    DragObject := nil;
    DoStartDrag(DragObject);
    DragObject.Free;

    DataObject := DragManager.DataObject;
    PrepareDragImage(P, DataObject);

    FLastDropMode := dmOnNode;
    // Don't forget to initialize the result. It might never be touched.
    FLastDragEffect := DROPEFFECT_NONE;
    AllowedEffects := GetDragOperations;
    try
      DragAndDrop(AllowedEffects, DataObject, FLastDragEffect);
      DragManager.ForceDragLeave;
    finally
      GetCursorPos(P);
      P := ScreenToClient(P);
      DoEndDrag(Self, P.X, P.Y);

      // Finish the operation.
      if (FLastDragEffect = DROPEFFECT_MOVE) and (toAutoDeleteMovedNodes in TreeOptions.AutoOptions) then
      begin
        // The operation was a move so delete the previously selected nodes.
        DeleteSelectedNodes;
      end;

      DoStateChange([], [tsOLEDragging]);
    end;
  finally
    FDragSelection := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoDragExpand;
begin
  StopTimer(ExpandTimer);
  if Assigned(FDropTargetNode) and (vsHasChildren in FDropTargetNode.States) and
    not (vsExpanded in FDropTargetNode.States) then
  begin
    ToggleNode(FDropTargetNode);
    UpdateWindow();
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer): Boolean;

begin
  Result := False;
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, Source, Shift, State, Pt, Mode, Effect, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoDragDrop(Source: TObject; const DataObject: TVTDragDataObject; const Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);

begin
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, Source, DataObject, Formats, Shift, Pt, Effect, Mode);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoBeforeDrawLineImage(Node: PVirtualNode; Level: Integer; var XPos: TDimension);

begin
  if Assigned(FOnBeforeDrawLineImage) then
    FOnBeforeDrawLineImage(Self, Node, Level, XPos);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoEdit;

begin
  Application.CancelHint;
  StopTimer(ScrollTimer);
  StopTimer(EditTimer);
  DoStateChange([], [tsEditPending]);
  if Assigned(FFocusedNode) and not (vsDisabled in FFocusedNode.States) and
    not (toReadOnly in FOptions.MiscOptions) and (FEditLink = nil) then
  begin
    ScrollIntoView(FFocusedNode, toCenterScrollIntoView in FOptions.SelectionOptions, not (toDisableAutoscrollOnEdit in FOptions.AutoOptions));
    FEditLink := DoCreateEditor(FFocusedNode, FEditColumn);
    if Assigned(FEditLink) then
    begin
      DoStateChange([tsEditing], [tsDrawSelecting, tsDrawSelPending, tsToggleFocusedSelection, tsOLEDragPending,
        tsOLEDragging, tsClearPending, tsDrawSelPending, tsScrollPending, tsScrolling]);
      if FEditLink.PrepareEdit(Self, FFocusedNode, FEditColumn) then
      begin
        UpdateEditBounds;
        // Node needs repaint because the selection rectangle and static text must disappear.
        InvalidateNode(FFocusedNode);
        if not FEditLink.BeginEdit then
          DoStateChange([], [tsEditing]);
      end
      else
        DoStateChange([], [tsEditing]);
      if not (tsEditing in FStates) then
        FEditLink := nil;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoEndDrag(Target: TObject; X, Y: TDimension);

// Does some housekeeping for VCL drag'n drop;

begin
  inherited;

  DragFinished;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoEndOperation(OperationKind: TVTOperationKind);

begin
  if Assigned(FOnEndOperation) then
    FOnEndOperation(Self, OperationKind);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoEnter();
begin
  inherited;
  EnsureNodeSelected(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoExpanded(Node: PVirtualNode);

begin
  if Assigned(FOnExpanded) then
    FOnExpanded(Self, Node);
  if (Self.UpdateCount = 0) then // See issue #1174
    NotifyAccessibleEvent();
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoExpanding(Node: PVirtualNode): Boolean;

begin
  Result := True;
  if Assigned(FOnExpanding) then
    FOnExpanding(Self, Node, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);

begin
  if Assigned(FOnFocusChanged) then
    FOnFocusChanged(Self, Node, Column);
  NotifyAccessibleEvent(EVENT_OBJECT_LOCATIONCHANGE);
  NotifyAccessibleEvent(EVENT_OBJECT_NAMECHANGE);
  NotifyAccessibleEvent(EVENT_OBJECT_VALUECHANGE);
  NotifyAccessibleEvent(EVENT_OBJECT_STATECHANGE);
  NotifyAccessibleEvent(EVENT_OBJECT_SELECTION);
  NotifyAccessibleEvent(EVENT_OBJECT_FOCUS);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoFocusChanging(OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex): Boolean;

begin
  Result := (OldColumn = NewColumn) or FHeader.AllowFocus(NewColumn);
  if Assigned(FOnFocusChanging) then
    FOnFocusChanging(Self, OldNode, NewNode, OldColumn, NewColumn, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoFocusNode(Node: PVirtualNode; Ask: Boolean);

begin
  if not (tsEditing in FStates) or EndEditNode then
  begin
    if Node = FRoot then
      Node := nil;
    if (FFocusedNode <> Node) and (not Ask or DoFocusChanging(FFocusedNode, Node, FFocusedColumn, FFocusedColumn)) then
    begin
      if Assigned(FFocusedNode) then
      begin
        // Do automatic collapsing of last focused node if enabled. This is however only done if
        // old and new focused node have a common parent node.
        if (toAutoExpand in FOptions.AutoOptions) and Assigned(Node) and (Node.Parent = FFocusedNode.Parent) and
          (vsExpanded in FFocusedNode.States) then
          ToggleNode(FFocusedNode)
        else
          InvalidateNode(FFocusedNode);
      end;
      FFocusedNode := Node;
    end;

    // Have to scroll the node into view, even it is the same node as before.
    if Assigned(FFocusedNode) then
    begin
      // Make sure a valid column is set if columns are used and no column has currently the focus.
      // We should also check if the maincolumn is allowfocus
      if FHeader.UseColumns and (not FHeader.Columns.IsValidColumn(FFocusedColumn))
      and FHeader.AllowFocus(FHeader.MainColumn) then
        FFocusedColumn := FHeader.MainColumn;
      // Do automatic expansion of the newly focused node if enabled.
      if (toAutoExpand in FOptions.AutoOptions) and not (vsExpanded in FFocusedNode.States) then
        ToggleNode(FFocusedNode);
      InvalidateNode(FFocusedNode);
      if (FUpdateCount = 0) and not (toDisableAutoscrollOnFocus in FOptions.AutoOptions) then
        ScrollIntoView(FFocusedNode, (toCenterScrollIntoView in FOptions.SelectionOptions) and
          (MouseButtonDown * FStates = []), not (toFullRowSelect in FOptions.SelectionOptions) );
    end;

    // Reset range anchor if necessary.
    if FSelectionCount = 0 then
      ResetRangeAnchor;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoFreeNode(Node: PVirtualNode);

var
  IntfData: IInterface;
begin
  // Prevent invalid references
  if Node = FLastChangedNode then
    FLastChangedNode := nil;
  if Node = FCurrentHotNode then
    FCurrentHotNode := nil;
  if Node = FDropTargetNode then
    FDropTargetNode := nil;
  if Node = FLastStructureChangeNode then
    FLastStructureChangeNode := nil;
  if Node = FFocusedNode then
    FFocusedNode := nil;
  if Node = FNextNodeToSelect then
    UpdateNextNodeToSelect(Node);
  if Node = FLastHitInfo.HitNode then
    FLastHitInfo.HitNode := nil;
  // fire event
  if Assigned(FOnFreeNode) and ([vsInitialized, vsOnFreeNodeCallRequired] * Node.States <> []) then
    FOnFreeNode(Self, Node);

  if vsReleaseCallOnUserDataRequired in Node.States then
  begin
    // Data may have been set to nil, in which case we can't call _Release on it
    IntfData := GetInterfaceFromNodeData<IInterface>(Node);
    if Assigned(IntfData) then
      IntfData._Release();
  end;

  FreeMem(Node);
  if Self.UpdateCount = 0 then
    EnsureNodeSelected(True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoGetCellContentMargin(Node: PVirtualNode; Column: TColumnIndex;
  CellContentMarginType: TVTCellContentMarginType = ccmtAllSides; Canvas: TCanvas = nil): TPoint;

// Determines the margins of the content rectangle caused by DoBeforeCellPaint.
// Note that shrinking the content rectangle results in positive margins whereas enlarging the content rectangle results
// in negative margins.

var
  CellRect,
  ContentRect: TRect;

begin
  Result := Point(0, 0);

  if Assigned(FOnBeforeCellPaint) then // Otherwise DoBeforeCellPaint has no effect.
  begin
    if Canvas = nil then
      Canvas := Self.Canvas;

    // Determine then node's cell rectangle and content rectangle before calling DoBeforeCellPaint.
    CellRect := GetDisplayRect(Node, Column, True);
    ContentRect := CellRect;
    DoBeforeCellPaint(Canvas, Node, Column, cpmGetContentMargin, CellRect, ContentRect);

    // Calculate the changes caused by DoBeforeCellPaint.
    case CellContentMarginType of
      ccmtAllSides:
        // Calculate the width difference and high difference.
        Result := Point((CellRect.Right - CellRect.Left) - (ContentRect.Right - ContentRect.Left),
                        (CellRect.Bottom - CellRect.Top) - (ContentRect.Bottom - ContentRect.Top));
      ccmtTopLeftOnly:
        // Calculate the left margin and top margin only.
        Result := Point(ContentRect.Left - CellRect.Left, ContentRect.Top - CellRect.Top);
      ccmtBottomRightOnly:
        // Calculate the right margin and bottom margin only.
        Result := Point(CellRect.Right - ContentRect.Right, CellRect.Bottom - ContentRect.Bottom);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoGetCursor(var Cursor: TCursor);

begin
  if Assigned(FOnGetCursor) then
    FOnGetCursor(Self, Cursor);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoGetHeaderCursor(var Cursor: TVTCursor);

begin
  if Assigned(FOnGetHeaderCursor) then
    FOnGetHeaderCursor(FHeader, Cursor);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var Index: TImageIndex): TCustomImageList;

// Queries the application/descendant about certain image properties for a node.
// Returns a custom image list if given by the callee, otherwise nil.
const
  cTVTImageKind2String: Array [TVTImageKind] of string = ('ikNormal', 'ikSelected', 'ikState', 'ikOverlay');
begin
  if (Kind = ikState) and Assigned(StateImages) then
    Result := Self.StateImages
  else
    Result := Self.Images;
  // First try the enhanced event to allow for custom image lists.
  if Assigned(FOnGetImageEx) then
    FOnGetImageEx(Self, Node, Kind, Column, Ghosted, Index, Result)
  else if Assigned(FOnGetImage) then
    FOnGetImage(Self, Node, Kind, Column, Ghosted, Index);

  Assert((Index < 0) or Assigned(Result), 'An image index was supplied for TVTImageKind.' + cTVTImageKind2String[Kind] + ' but no image list was supplied.');
  if not Assigned(Result) then
    Index := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoGetImageText(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var ImageText: string);

// Queries the application/descendant about alternative image text for a node.

begin
  if Assigned(FOnGetImageText) then
     FOnGetImageText(Self, Node, Kind, Column, ImageText);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoGetLineStyle(var Bits: Pointer);

begin
  if Assigned(FOnGetLineStyle) then
    FOnGetLineStyle(Self, Bits);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle): string;

begin
  Result := Hint;
  LineBreakStyle := hlbDefault;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle): string;

begin
  Result := Hint;
  LineBreakStyle := hlbDefault;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoGetNodeExtraWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): TDimension;

// Returns the pixel width of extra space occupied by node contents (for example, static text).

begin
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): TDimension;

// Returns the pixel width of a node.

begin
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoGetPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Position: TPoint): TPopupMenu;

// Queries the application whether there is a node specific popup menu.

var
  Run: PVirtualNode;
  AskParent: Boolean;

begin
  Result := nil;
  if Assigned(FOnGetPopupMenu) then
  begin
    Run := Node;

    if Assigned(Run) then
    begin
      AskParent := True;
      repeat
        FOnGetPopupMenu(Self, Run, Column, Position, AskParent, Result);
        Run := Run.Parent;
      until (Run = FRoot) or Assigned(Result) or not AskParent;
    end
    else
      FOnGetPopupMenu(Self, nil, NoColumn, Position, AskParent, Result);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoGetUserClipboardFormats(var Formats: TFormatEtcArray);

begin
  if Assigned(FOnGetUserClipboardFormats) then
    FOnGetUserClipboardFormats(Self, Formats);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderAddPopupItem(const Column: TColumnIndex; var Cmd: TAddPopupItemType);

begin
  if Assigned(FOnHeaderAddPopupItem) then
    FOnHeaderAddPopupItem(Self, Column, Cmd);

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderClick(const HitInfo: TVTHeaderHitInfo);

begin
  if Assigned(FOnHeaderClick) then
    FOnHeaderClick(FHeader, HitInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderDblClick(const HitInfo: TVTHeaderHitInfo);

begin
  if Assigned(FOnHeaderDblClick) then
    FOnHeaderDblClick(FHeader, HitInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition);

begin
  if Assigned(FOnHeaderDragged) then
    FOnHeaderDragged(FHeader, Column, OldPosition);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderDraggedOut(Column: TColumnIndex; DropPosition: TPoint);

begin
  if Assigned(FOnHeaderDraggedOut) then
    FOnHeaderDraggedOut(FHeader, Column, DropPosition);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoHeaderDragging(Column: TColumnIndex): Boolean;

begin
  Result := True;
  if Assigned(FOnHeaderDragging) then
    FOnHeaderDragging(FHeader, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderDraw(Canvas: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover, Pressed: Boolean;
  DropMark: TVTDropMarkMode);

begin
  if Assigned(FOnHeaderDraw) then
    FOnHeaderDraw(FHeader, Canvas, Column, R, Hover, Pressed, DropMark);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderDrawQueryElements(var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);

begin
  if Assigned(FOnHeaderDrawQueryElements) then
    FOnHeaderDrawQueryElements(FHeader, PaintInfo, Elements);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: TDimension);

begin
  if Assigned(FOnHeaderMouseDown) then
    FOnHeaderMouseDown(FHeader, Button, Shift, X, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderMouseMove(Shift: TShiftState; X, Y: TDimension);

begin
  if Assigned(FOnHeaderMouseMove) then
    FOnHeaderMouseMove(FHeader, Shift, X, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHeaderMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: TDimension);

begin
  if Assigned(FOnHeaderMouseUp) then
    FOnHeaderMouseUp(FHeader, Button, Shift, X, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoHotChange(Old, New: PVirtualNode);

begin
  if Assigned(FOnHotChange) then
    FOnHotChange(Self, Old, New);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer;

begin
  Result := 0;
  if Assigned(FOnIncrementalSearch) then
    FOnIncrementalSearch(Self, Node, Text, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean;
/// The function calls the OnInitChildren and returns True if the event was called; it returns False if the caller can expect that no changes have been made to ChildCount
begin
  if Assigned(FOnInitChildren) then
  begin
    FOnInitChildren(Self, Node, ChildCount);
    Result := True;
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates);

begin
  if Assigned(FOnInitNode) then
    FOnInitNode(Self, Parent, Node, InitStates);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean;

begin
  Result := True;
  if Assigned(FOnKeyAction) then
    FOnKeyAction(Self, CharCode, Shift, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoLoadUserData(Node: PVirtualNode; Stream: TStream);

begin
  if Assigned(FOnLoadNode) then
    if Node = FRoot then
      FOnLoadNode(Self, nil, Stream)
    else
      FOnLoadNode(Self, Node, Stream);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: TDimension);

begin
  if not (vsInitialized in Node.States) then
    InitNode(Node);
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, TargetCanvas, Node, NodeHeight);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoMouseEnter();

begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoMouseLeave;

begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoNodeCopied(Node: PVirtualNode);

begin
  if Assigned(FOnNodeCopied) then
    FOnNodeCopied(Self, Node);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoNodeCopying(Node, NewParent: PVirtualNode): Boolean;

begin
  Result := True;
  if Assigned(FOnNodeCopying) then
    FOnNodeCopying(Self, Node, NewParent, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoNodeClick(const HitInfo: THitInfo);

begin
  if Assigned(FOnNodeClick) then
    FOnNodeClick(Self, HitInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoNodeDblClick(const HitInfo: THitInfo);

begin
  if Assigned(FOnNodeDblClick) then
    FOnNodeDblClick(Self, HitInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoNodeHeightDblClickResize(Node: PVirtualNode; Column: TColumnIndex; Shift: TShiftState;
  P: TPoint): Boolean;

begin
  Result := True;
  if Assigned(FOnNodeHeightDblClickResize) then
    FOnNodeHeightDblClickResize(Self, Node, Column, Shift, P, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoNodeHeightTracking(Node: PVirtualNode; Column: TColumnIndex; Shift: TShiftState;
  var TrackPoint: TPoint; P: TPoint): Boolean;

begin
  Result := True;
  if Assigned(FOnNodeHeightTracking) then
    FOnNodeHeightTracking(Self, Node, Column, Shift, TrackPoint, P, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoNodeMoved(Node: PVirtualNode);

begin
  if Assigned(FOnNodeMoved) then
    FOnNodeMoved(Self, Node);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoNodeMoving(Node, NewParent: PVirtualNode): Boolean;

begin
  Result := True;
  if Assigned(FOnNodeMoving) then
    FOnNodeMoving(Self, Node, NewParent, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoPaintBackground(Canvas: TCanvas; R: TRect): Boolean;

begin
  Result := False;
  if Assigned(FOnPaintBackground) then
    FOnPaintBackground(Self, Canvas, R, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoPaintDropMark(Canvas: TCanvas; Node: PVirtualNode; R: TRect);

// draws the drop mark into the given rectangle
// Note: Changed properties of the given canvas should be reset to their previous values.

var
  SaveBrushColor: TColor;
  SavePenStyle: TPenStyle;

begin
  if FLastDropMode in [dmAbove, dmBelow] then
    with Canvas do
    begin
      SavePenStyle := Pen.Style;
      Pen.Style := psClear;
      SaveBrushColor := Brush.Color;
      Brush.Color := FColors.DropMarkColor;

      if FLastDropMode = dmAbove then
      begin
        Polygon([Point(R.Left + 2, R.Top),
                 Point(R.Right - 2, R.Top),
                 Point(R.Right - 2, R.Top + 6),
                 Point(R.Right - 6, R.Top + 2),
                 Point(R.Left + 6 , R.Top + 2),
                 Point(R.Left + 2, R.Top + 6)
        ]);
      end
      else
        Polygon([Point(R.Left + 2, R.Bottom - 1),
                 Point(R.Right - 2, R.Bottom - 1),
                 Point(R.Right - 2, R.Bottom - 8),
                 Point(R.Right - 7, R.Bottom - 3),
                 Point(R.Left + 7 , R.Bottom - 3),
                 Point(R.Left + 2, R.Bottom - 8)
        ]);
      Brush.Color := SaveBrushColor;
      Pen.Style := SavePenStyle;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoPaintNode(var PaintInfo: TVTPaintInfo);

begin
end;

procedure TBaseVirtualTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  if Assigned(FOnPaintText) then
    FOnPaintText(Self, Canvas, Node, Column, TextType);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Position: TPoint);

// Support for node dependent popup menus.

var
  Menu: TPopupMenu;

begin
  Menu := DoGetPopupMenu(Node, Column, Position);

  if Assigned(Menu) then
  begin
    DoStateChange([tsPopupMenuShown]);
    StopTimer(EditTimer);
    Menu.PopupComponent := Self;
    with ClientToScreen(Position) do
      Menu.Popup(X, Y);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoRemoveFromSelection(Node: PVirtualNode);

begin
  if Assigned(FOnRemoveFromSelection) then
    FOnRemoveFromSelection(Self, Node);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoReset(Node: PVirtualNode);

begin
  if Assigned(FOnResetNode) then
    FOnResetNode(Self, Node);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoSaveUserData(Node: PVirtualNode; Stream: TStream);

begin
  if Assigned(FOnSaveNode) then
    if Node = FRoot then
      FOnSaveNode(Self, nil, Stream)
    else
      FOnSaveNode(Self, Node, Stream);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoScroll(DeltaX, DeltaY: TDimension);

begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, DeltaX, DeltaY);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoSetOffsetXY(Value: TPoint; Options: TScrollUpdateOptions; ClipRect: PRect = nil): Boolean;

// Actual offset setter used to scroll the client area, update scroll bars and invalidating the header (all optional).
// Returns True if the offset really changed otherwise False is returned.

var
  DeltaX: TDimension;
  DeltaY: TDimension;
  DWPStructure: HDWP;
  I: Integer;
  P: TPoint;
  R: TRect;

begin
  // Range check, order is important here.
  if Value.X < (ClientWidth - FRangeX) then
    Value.X := ClientWidth - FRangeX;
  if Value.X > 0 then
    Value.X := 0;
  DeltaX := Value.X - FOffsetX;
  if UseRightToLeftAlignment then
    DeltaX := -DeltaX;
  if Value.Y < (ClientHeight - FRangeY) then
    Value.Y := ClientHeight - FRangeY;
  if Value.Y > 0 then
    Value.Y := 0;
  DeltaY := Value.Y - FOffsetY;

  Result := (DeltaX <> 0) or (DeltaY <> 0);
  if Result then
  begin
    FOffsetX := Value.X;
    FOffsetY := Value.Y;
    Result := True;

    if tsHint in Self.FStates then
      Application.CancelHint;
    if FUpdateCount = 0 then
    begin
      // The drag image from VCL controls need special consideration.
      if tsVCLDragging in FStates then
        ImageList_DragShowNolock(False);

      if (suoScrollClientArea in Options) and not (tsToggling in FStates) then
      begin
        // Have to invalidate the entire window if there's a background.
        if (toShowBackground in FOptions.PaintOptions) and Assigned(FBackground.Graphic) then
        begin
          // Since we don't use ScrollWindow here we have to move all client windows ourselves.
          DWPStructure := BeginDeferWindowPos(ControlCount);
          for I := 0 to ControlCount - 1 do
            if Controls[I] is TWinControl then
            begin
              with Controls[I] as TWinControl do
                DWPStructure := DeferWindowPos(DWPStructure, Handle, 0, Left + DeltaX, Top + DeltaY, 0, 0,
                  SWP_NOZORDER or SWP_NOACTIVATE or SWP_NOSIZE);
              if DWPStructure = 0 then
                Break;
            end;
          if DWPStructure <> 0 then
            EndDeferWindowPos(DWPStructure);
          InvalidateRect(nil, False);
        end
        else
        begin
          if (DeltaX <> 0) and (Header.Columns.GetVisibleFixedWidth > 0) then
          begin
            // When fixed columns exists we have to scroll separately horizontally and vertically.
            // Horizontally is scroll only the client area not occupied by fixed columns and
            // vertically entire client area (or clipping area if one exists).
            R := ClientRect;
            R.Left := Header.Columns.GetVisibleFixedWidth;

            ScrollWindow(Handle, DeltaX, 0, @R, @R);
            if DeltaY <> 0 then
              ScrollWindow(Handle, 0, DeltaY, ClipRect, ClipRect);
          end
          else
            ScrollWindow(Handle, DeltaX, DeltaY, ClipRect, ClipRect);
        end;
      end;

      if suoUpdateNCArea in Options then
      begin
        if DeltaX <> 0 then
        begin
          UpdateHorizontalScrollBar(suoRepaintScrollBars in Options);
          if (suoRepaintHeader in Options) and (hoVisible in FHeader.Options) then
            FHeader.Invalidate(nil);
          if not (tsSizing in FStates) and (FScrollBarOptions.ScrollBars in [System.UITypes.TScrollStyle.ssHorizontal, System.UITypes.TScrollStyle.ssBoth]) then
            UpdateVerticalScrollBar(suoRepaintScrollBars in Options);
        end;

        if (DeltaY <> 0) and ([tsThumbTracking, tsSizing] * FStates = []) then
        begin
          UpdateVerticalScrollBar(suoRepaintScrollBars in Options);
          if not (FHeader.UseColumns or IsMouseSelecting) and
            (FScrollBarOptions.ScrollBars in [System.UITypes.TScrollStyle.ssHorizontal, System.UITypes.TScrollStyle.ssBoth]) then
            UpdateHorizontalScrollBar(suoRepaintScrollBars in Options);
        end;
      end;

      if tsVCLDragging in FStates then
        ImageList_DragShowNolock(True);
    end;

    // Finally update "hot" node if hot tracking is activated
    GetCursorPos(P);
    P := ScreenToClient(P);
    if PtInRect(ClientRect, P) then
      HandleHotTrack(P.X, P.Y);

    DoScroll(DeltaX, DeltaY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoShowScrollBar(Bar: Integer; Show: Boolean);

begin
  ShowScrollBar(Bar, Show);

  if Assigned(FOnShowScrollBar) then
    FOnShowScrollBar(Self, Bar, Show);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoStartDrag(var DragObject: TDragObject);

begin
  inherited;

  // Check if the application created an own drag object. This is needed to pass the correct source in
  // OnDragOver and OnDragDrop.
  if Assigned(DragObject) then
    DoStateChange([tsUserDragObject]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoStartOperation(OperationKind: TVTOperationKind);

begin
  if Assigned(FOnStartOperation) then
    FOnStartOperation(Self, OperationKind);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoStateChange(Enter: TVirtualTreeStates; Leave: TVirtualTreeStates = []);

var
  ActualEnter,
  ActualLeave: TVirtualTreeStates;

begin
  if Assigned(FOnStateChange) then
  begin
    ActualEnter := Enter - FStates;
    ActualLeave := FStates * Leave;
    if (ActualEnter + ActualLeave) <> [] then
      FOnStateChange(Self, Enter, Leave);
  end;
  FStates := FStates + Enter - Leave;
  Assert(FStates * [tsUseCache, tsValidationNeeded] <> [tsUseCache, tsValidationNeeded], 'Invalid state. tsUseCache and tsValidationNeeded are mutually exclusive and must not be set at the same time');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoStructureChange(Node: PVirtualNode; Reason: TChangeReason);

begin
  StopTimer(StructureChangeTimer);
  if Assigned(FOnStructureChange) then
    FOnStructureChange(Self, Node, Reason);

  // This is a good place to reset the cached node and reason. These are the same as the values passed in here.
  // This is necessary to allow descendants to override this method and get them.
  DoStateChange([], [tsStructureChangePending]);
  FLastStructureChangeNode := nil;
  FLastStructureChangeReason := crIgnore;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoTimerScroll;

var
  P,
  ClientP: TPoint;
  InRect,
  Panning: Boolean;
  R,
  ClipRect: TRect;
  DeltaX,
  DeltaY: Integer;

begin
  GetCursorPos(P);
  R := ClientRect;
  ClipRect := R;
  MapWindowPoints(Handle, 0, R, 2);
  InRect := PtInRect(R, P);
  ClientP := ScreenToClient(P);
  Panning := tsPanning in FStates;

  if IsMouseSelecting or InRect or Panning then
  begin
    DeltaX := 0;
    DeltaY := 0;
    if sdUp in FScrollDirections then
    begin
      if Panning then
        DeltaY := FLastClickPos.Y - ClientP.Y - 8
      else
        if InRect then
          DeltaY := Min(FScrollBarOptions.VerticalIncrement, ClientHeight)
        else
          DeltaY := Min(FScrollBarOptions.VerticalIncrement, ClientHeight) * Abs(R.Top - P.Y);
      if FOffsetY = 0 then
        Exclude(FScrollDirections, sdUp);
    end;

    if sdDown in FScrollDirections then
    begin
      if Panning then
        DeltaY := FLastClickPos.Y - ClientP.Y + 8
      else
        if InRect then
          DeltaY := -Min(FScrollBarOptions.VerticalIncrement, ClientHeight)
        else
          DeltaY := -Min(FScrollBarOptions.VerticalIncrement, ClientHeight) * Abs(P.Y - R.Bottom);
      if (ClientHeight - FOffsetY) = FRangeY then
        Exclude(FScrollDirections, sdDown);
    end;

    if sdLeft in FScrollDirections then
    begin
      if Panning then
        DeltaX := FLastClickPos.X - ClientP.X - 8
      else
        if InRect then
          DeltaX := FScrollBarOptions.HorizontalIncrement
        else
          DeltaX := FScrollBarOptions.HorizontalIncrement * Abs(R.Left - P.X);
      if FEffectiveOffsetX = 0 then
        Exclude(FScrollDirections, sdleft);
    end;

    if sdRight in FScrollDirections then
    begin
      if Panning then
        DeltaX := FLastClickPos.X - ClientP.X + 8
      else
        if InRect then
          DeltaX := -FScrollBarOptions.HorizontalIncrement
        else
          DeltaX := -FScrollBarOptions.HorizontalIncrement * Abs(P.X - R.Right);

      if (ClientWidth + FEffectiveOffsetX) = FRangeX then
        Exclude(FScrollDirections, sdRight);
    end;

    if UseRightToLeftAlignment then
      DeltaX := - DeltaX;

    if IsMouseSelecting then
    begin
      // In order to avoid scrolling the area which needs a repaint due to the changed selection rectangle
      // we limit the scroll area explicitely.
      OffsetRect(ClipRect, DeltaX, DeltaY);
      DoSetOffsetXY(Point(FOffsetX + DeltaX, FOffsetY + DeltaY), DefaultScrollUpdateFlags, @ClipRect);
      // When selecting with the mouse then either update only the parts of the window which have been uncovered
      // by the scroll operation if no change in the selection happend or invalidate and redraw the entire
      // client area otherwise (to avoid the time consuming task of determining the display rectangles of every
      // changed node).
      if CalculateSelectionRect(ClientP.X, ClientP.Y) and HandleDrawSelection(ClientP.X, ClientP.Y) then
        InvalidateRect(nil, False)
      else
      begin
        // The selection did not change so invalidate only the part of the window which really needs an update.
        // 1) Invalidate the parts uncovered by the scroll operation. Add another offset range, we have to
        //    scroll only one stripe but have to update two.
        OffsetRect(ClipRect, DeltaX, DeltaY);
        SubtractRect(ClipRect, ClientRect, ClipRect);
        InvalidateRect(@ClipRect, False);

        // 2) Invalidate the selection rectangles.
        UnionRect(ClipRect, OrderRect(FNewSelRect), OrderRect(FLastSelRect));
        OffsetRect(ClipRect, FOffsetX, FOffsetY);
        InvalidateRect(@ClipRect, False);
      end;
    end
    else
    begin
      // Scroll only if there is no drag'n drop in progress. Drag'n drop scrolling is handled in DragOver.
      if ((FDragManager = nil) or not DragManager.IsDropTarget) and ((DeltaX <> 0) or (DeltaY <> 0)) then
        DoSetOffsetXY(Point(FOffsetX + DeltaX, FOffsetY + DeltaY), DefaultScrollUpdateFlags, nil);
    end;
    UpdateWindow();

    if (FScrollDirections = []) and not (tsPanning in FStates) then
    begin
      StopTimer(ScrollTimer);
      DoStateChange([], [tsScrollPending, tsScrolling]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoUpdating(State: TVTUpdateState);

begin
  if Assigned(FOnUpdating) then
    FOnUpdating(Self, State);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DoValidateCache(): Boolean;

// This method fills the cache, which is used to speed up searching for nodes.
// The strategy is simple: Take the current number of visible nodes and distribute evenly a number of marks
// (which are stored in FPositionCache) so that iterating through the tree doesn't cost too much time.
// If there are less than 'CacheThreshold' nodes in the tree then the cache remains empty.
// Result is True if the cache was filled without interruption, otherwise False.
// Note: You can adjust the maximum number of nodes between two cache entries by changing CacheThreshold.

var
  EntryCount,
  Index: Cardinal;
  CurrentNode,
  Temp: PVirtualNode;
  CurrentTop: TNodeHeight;
begin
  EntryCount := 0;
  if not (tsStopValidation in FStates) then
  begin
    if FStartIndex = 0 then
      FPositionCache := nil;

    EntryCount := CalculateCacheEntryCount;
    SetLength(FPositionCache, EntryCount);
    if FStartIndex > EntryCount then
      FStartIndex := EntryCount;

    // Optimize validation by starting with FStartIndex if set.
    if (FStartIndex > 0) and Assigned(FPositionCache[FStartIndex - 1].Node) then
    begin
      // Index is the current entry in FPositionCache.
      Index := FStartIndex - 1;
      // Running term for absolute top value.
      CurrentTop := FPositionCache[Index].AbsoluteTop;
      // Running node pointer.
      CurrentNode := FPositionCache[Index].Node;
    end
    else
    begin
      // Index is the current entry in FPositionCache.
      Index := 0;
      // Running term for absolute top value.
      CurrentTop := 0;
      // Running node pointer.
      CurrentNode := GetFirstVisibleNoInit(nil, True);
    end;

    // EntryCount serves as counter for processed nodes here. This value can always start at 0 as
    // the validation either starts also at index 0 or an index which is always a multiple of CacheThreshold
    // and EntryCount is only used with modulo CacheThreshold.
    EntryCount := 0;
    if Assigned(CurrentNode) then
    begin
      while not (tsStopValidation in FStates) do
      begin
        // If the cache is full then stop the loop.
        if (Integer(Index) >= Length(FPositionCache)) then
          Break;
        if (EntryCount mod CacheThreshold) = 0 then
        begin
          // New cache entry to set up.
          with FPositionCache[Index] do
          begin
            Node := CurrentNode; // 2 EAccessViolation seen here in TreeSize V4.3.1, 1 in V4.4.0 (Write of address 00000000)
            AbsoluteTop := CurrentTop;
          end;
          System.Inc(Index);
        end;

        Inc(CurrentTop, NodeHeight[CurrentNode]);
        // Advance to next visible node.
        Temp := GetNextVisibleNoInit(CurrentNode, True);
        // If there is no further node then stop the loop.
        if (Temp = nil) then       // CHANGED: 17.09.2013 - Veit Zimmermann
          Break;                   // CHANGED: 17.09.2013 - Veit Zimmermann

        CurrentNode := Temp;
        System.Inc(EntryCount);
      end;
    end;
    // Finalize the position cache so no nil entry remains there.
    if not (tsStopValidation in FStates) and (Integer(Index) <= High(FPositionCache)) then
    begin
      SetLength(FPositionCache, Index + 1);
      with FPositionCache[Index] do
      begin
        Node := CurrentNode;
        AbsoluteTop := CurrentTop;
      end;
    end;
  end;

  Result := (EntryCount > 0) and not (tsStopValidation in FStates);

  // In variable node height mode it might have happend that some or all of the nodes have been adjusted in their
  // height. During validation updates of the scrollbars is disabled so let's do this here.
  if Result and (toVariableNodeHeight in FOptions.MiscOptions) then
  begin
    TThread.Queue(nil, procedure begin UpdateScrollBars(True) end);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DragAndDrop(AllowedEffects: Dword; const DataObject: TVTDragDataObject; var DragEffect: Integer);
var
  lDragEffect: DWord; // required for type compatibility with SHDoDragDrop
begin
  lDragEffect := DWord(DragEffect);
  SHDoDragDrop(Self.Handle, DataObject, nil, AllowedEffects, lDragEffect); // supports drag hints on Windows Vista and later
  DragEffect := Integer(lDragEffect);
 end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DragCanceled;

// Does some housekeeping for VCL drag'n drop;

begin
  inherited;

  DragFinished;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DragDrop(const DataObject: TVTDragDataObject; KeyState: Integer; Pt: TPoint;
  var Effect: Integer): HResult;

var
  Shift: TShiftState;
  EnumFormat: IEnumFormatEtc;
  Fetched: Integer;
  OLEFormat: TFormatEtc;
  Formats: TFormatArray;

begin
  StopTimer(ExpandTimer);
  StopTimer(ScrollTimer);
  DoStateChange([], [tsScrollPending, tsScrolling]);
  Formats := nil;

  // Ask explicitly again whether the action is allowed. Otherwise we may accept a drop which is intentionally not
  // allowed but cannot be prevented by the application because when the tree was scrolling while dropping
  // no DragOver event is created by the OLE subsystem.
  Result := DragOver(DragManager.DragSource, KeyState, dsDragMove, Pt, Effect);
  try
    if (Result <> NOERROR) or ((Effect and not DROPEFFECT_SCROLL) = DROPEFFECT_NONE) then
      Result := E_FAIL
    else
    begin
      try
        Shift := KeysToShiftState(KeyState);
        if tsRightButtonDown in FStates then
          Include(Shift, ssRight)
        else if tsMiddleButtonDown in FStates then
          Include(Shift, ssMiddle)
        else
          Include(Shift, ssLeft);
        Pt := ScreenToClient(Pt);
        // Determine which formats we can get and pass them along with the data object to the drop handler.
        Result := DataObject.EnumFormatEtc(DATADIR_GET, EnumFormat);
        if Failed(Result) then
          Abort;
        Result := EnumFormat.Reset;
        if Failed(Result) then
          Abort;
        // create a list of available formats
        while EnumFormat.Next(1, OLEFormat, @Fetched) = S_OK do
        begin
          SetLength(Formats, Length(Formats) + 1);
          Formats[High(Formats)] := OLEFormat.cfFormat;
        end;
        DoDragDrop(DragManager.DragSource, DataObject, Formats, Shift, Pt, Effect, FLastDropMode);
      except
        // An unhandled exception here leaks memory.
        Application.HandleException(Self);
        Result := E_UNEXPECTED;
      end;
    end;
  finally
    if Assigned(FDropTargetNode) then
    begin
      InvalidateNode(FDropTargetNode);
      FDropTargetNode := nil;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DragEnter(KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult;

// callback routine for the drop target interface

var
  Shift: TShiftState;
  Accept: Boolean;
  R: TRect;
  HitInfo: THitInfo;

begin
  try
    if not (toAcceptOLEDrop in TreeOptions.MiscOptions) then
    begin
      Effect := DROPEFFECT_NONE;
      Exit(NOERROR);
    end;

    // Determine acceptance of drag operation and reset scroll start time.
    FDragScrollStart := 0;

    Shift := KeysToShiftState(KeyState);
    if tsLeftButtonDown in FStates then
      Include(Shift, ssLeft);
    if tsMiddleButtonDown in FStates then
      Include(Shift, ssMiddle);
    if tsRightButtonDown in FStates then
      Include(Shift, ssRight);
    Pt := ScreenToClient(Pt);
    Effect := SuggestDropEffect(DragManager.DragSource, Shift, Pt, Effect);
    Accept := DoDragOver(DragManager.DragSource, Shift, dsDragEnter, Pt, FLastDropMode, Effect);
    if not Accept then
      Effect := DROPEFFECT_NONE
    else
    begin
      // Set initial drop target node and drop mode.
      GetHitTestInfoAt(Pt.X, Pt.Y, True, HitInfo, Shift);
      if Assigned(HitInfo.HitNode) then
      begin
        FDropTargetNode := HitInfo.HitNode;
        R := GetDisplayRect(HitInfo.HitNode, FHeader.MainColumn, False);
	//VSOFT CHANGE - changed  back to 4.8.5 behaviour
        if (hiOnItemLabel in HitInfo.HitPositions) or ((hiOnItem in HitInfo.HitPositions) and
          ((toFullRowDrag in FOptions.MiscOptions){ or (toFullRowSelect in FOptions.SelectionOptions)}))then
          FLastDropMode := dmOnNode
        else
          if ((R.Top + R.Bottom) div 2) > Pt.Y then
            FLastDropMode := dmAbove
          else
            FLastDropMode := dmBelow;
      end
      else
        FLastDropMode := dmNowhere;
    end;
    Result := NOERROR;
  except
    Result := E_UNEXPECTED;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DragFinished;

// Called by DragCancelled or EndDrag to make up for the still missing mouse button up messages.
// These are important for such important things like popup menus.

var
  P: TPoint;

begin
  if [tsOLEDragging, tsVCLDragPending, tsVCLDragging, tsVCLDragFinished] * FStates = [] then
    Exit;

  DoStateChange([], [tsVCLDragPending, tsVCLDragging, tsUserDragObject, tsVCLDragFinished]);

  GetCursorPos(P);
  P := ScreenToClient(P);
  if tsRightButtonDown in FStates then
    Perform(WM_RBUTTONUP, 0, LPARAM(Integer(PointToSmallPoint(P))))
  else
    if tsMiddleButtonDown in FStates then
      Perform(WM_MBUTTONUP, 0, LPARAM(Integer(PointToSmallPoint(P))))
    else
      Perform(WM_LBUTTONUP, 0, LPARAM(Integer(PointToSmallPoint(P))));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DragLeave;

var
  Effect: Integer;

begin
  StopTimer(ExpandTimer);

  if Assigned(FDropTargetNode) then
  begin
    InvalidateNode(FDropTargetNode);
    FDropTargetNode := nil;
  end;

  UpdateWindow();

  Effect := 0;
  DoDragOver(nil, [], TDragState.dsDragLeave, Point(0, 0), FLastDropMode, Effect);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.DragOver(Source: TObject; KeyState: Integer; DragState: TDragState; Pt: TPoint;
  var Effect: Integer): HResult;

// callback routine for the drop target interface

var
  Shift: TShiftState;
  Accept,
  WindowScrolled: Boolean;
  OldR, R: TRect;
  NewDropMode: TDropMode;
  HitInfo: THitInfo;
  DragPos: TPoint;
  LastNode: PVirtualNode;
  DeltaX,
  DeltaY: TDimension;
  ScrollOptions: TScrollUpdateOptions;

begin
  ScrollOptions := DefaultScrollUpdateFlags;

  try
    DragPos := Pt;
    Pt := ScreenToClient(Pt);

    // Check if we have to scroll the client area.
    FScrollDirections := DetermineScrollDirections(Pt.X, Pt.Y);
    DeltaX := 0;
    DeltaY := 0;
    if FScrollDirections <> [] then
    begin
      // Determine amount to scroll.
      if sdUp in FScrollDirections then
      begin
        DeltaY := Min(FScrollBarOptions.VerticalIncrement, ClientHeight);
        if FOffsetY = 0 then
          Exclude(FScrollDirections, sdUp);
      end;
      if sdDown in FScrollDirections then
      begin
        DeltaY := -Min(FScrollBarOptions.VerticalIncrement, ClientHeight);
        if (ClientHeight - FOffsetY) = FRangeY then
          Exclude(FScrollDirections, sdDown);
      end;
      if sdLeft in FScrollDirections then
      begin
        DeltaX := FScrollBarOptions.HorizontalIncrement;
        if FEffectiveOffsetX = 0 then
          Exclude(FScrollDirections, sdleft);
      end;
      if sdRight in FScrollDirections then
      begin
        DeltaX := -FScrollBarOptions.HorizontalIncrement;
        if (ClientWidth + FEffectiveOffsetX) = FRangeX then
          Exclude(FScrollDirections, sdRight);
      end;
      WindowScrolled := DoSetOffsetXY(Point(FOffsetX + DeltaX, FOffsetY + DeltaY), ScrollOptions, nil);
    end
    else
      WindowScrolled := False;

    // Determine acceptance of drag operation as well as drag target.
    Shift := KeysToShiftState(KeyState);
    if tsLeftButtonDown in FStates then
      Include(Shift, ssLeft);
    if tsMiddleButtonDown in FStates then
      Include(Shift, ssMiddle);
    if tsRightButtonDown in FStates then
      Include(Shift, ssRight);
    GetHitTestInfoAt(Pt.X, Pt.Y, True, HitInfo, Shift);

    if Assigned(HitInfo.HitNode) then
      R := GetDisplayRect(HitInfo.HitNode, NoColumn, False)
    else
      R := Rect(0, 0, 0, 0);
    NewDropMode := DetermineDropMode(Pt, HitInfo, R);

    if (HitInfo.HitNode <> FDropTargetNode) or (FLastDropMode <> NewDropMode) then
    begin
      // Something in the tree will change. This requires to update the screen and/or the drag image.
      FLastDropMode := NewDropMode;
      if HitInfo.HitNode <> FDropTargetNode then
      begin
        StopTimer(ExpandTimer);
        // The last target node is needed for the rectangle determination but must already be set for
        // the recapture call, hence it must be stored somewhere.
        LastNode := FDropTargetNode;
        FDropTargetNode := HitInfo.HitNode;
        // In order to show a selection rectangle a column must be focused.
        if FFocusedColumn <= NoColumn then
          FFocusedColumn := FHeader.MainColumn;

        if Assigned(LastNode) and Assigned(FDropTargetNode) then
        begin
          // Optimize the case that the selection moved between two nodes.
          OldR := GetDisplayRect(LastNode, NoColumn, False);
          UnionRect(R, R, OldR);
          InvalidateRect(@R, False);
        end
        else
        begin
          if Assigned(LastNode) then
          begin
            // Repaint last target node.
            OldR := GetDisplayRect(LastNode, NoColumn, False);
            InvalidateRect(@OldR, False);
          end
          else
            InvalidateRect(@R, False);
        end;

        // Start auto expand timer if necessary.
        if (toAutoDropExpand in FOptions.AutoOptions) and Assigned(FDropTargetNode) and
          (vsHasChildren in FDropTargetNode.States) then
          SetTimer(Handle, ExpandTimer, FAutoExpandDelay, nil);
      end
      else
      begin
        InvalidateRect(@R, False);
      end;
    end;

    Update;

    Effect := SuggestDropEffect(Source, Shift, Pt, Effect);
    Accept := DoDragOver(Source, Shift, DragState, Pt, FLastDropMode, Effect);
    if not Accept then
      Effect := DROPEFFECT_NONE;
    if WindowScrolled then
      Effect := Effect or Integer(DROPEFFECT_SCROLL);
    Result := NOERROR;
  except
    Result := E_UNEXPECTED;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DrawDottedHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: TDimension);
// Draws a horizontal line with alternating pixels
var
  R: TRect;
begin
  R := Rect(Min(Left, Right), Top, Max(Left, Right) + 1, Top + 1);
  PaintInfo.Canvas.Brush.Color := FColors.BackGroundColor;
  Winapi.Windows.FillRect(PaintInfo.Canvas.Handle, R, DottedBrushTreeLines.Handle);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DrawDottedVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: TDimension);
// Draws a vertical line with alternating pixels
var
  R: TRect;
begin
  R := Rect(Left, Min(Top, Bottom), Left + 1, Max(Top, Bottom) + 1);
  PaintInfo.Canvas.Brush.Color := FColors.BackGroundColor;
  Winapi.Windows.FillRect(PaintInfo.Canvas.Handle, R, DottedBrushTreeLines.Handle);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DrawGridLine(Canvas: TCanvas; R: TRect);
begin
  Canvas.Brush.Color := FColors.GridLineColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(R);
  //StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(tlGroupHeaderLineOpenSelectedNotFocused), R {$IF CompilerVersion  >= 34}, @R, CurrentPPI{$IFEND});
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DrawGridHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: TDimension);
// Draws a horizontal grid line
var
  R: TRect;
begin
  R := Rect(Min(Left, Right), Top, Max(Left, Right) + LineWidth, Top + LineWidth);
  DrawGridLine(PaintInfo.Canvas, R)
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DrawGridVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: TDimension; pFixedColumn: Boolean = False);
// Draws a vertical grid line
var
  R: TRect;
begin
  R := Rect(Left, Min(Top, Bottom), Left + LineWidth, Max(Top, Bottom) + LineWidth);
  if pFixedColumn and (TVtPaintOption.toShowVertGridLines in TreeOptions.PaintOptions) then // In case we showe grid lines, we must use a color for the fixed column that differentiates from the normal gridlines
    StyleServices.DrawElement(PaintInfo.Canvas.Handle, StyleServices.GetElementDetails(tlGroupHeaderLineOpenHot), R {$IF CompilerVersion  >= 34}, @R, CurrentPPI{$IFEND})
  else begin
    if StyleServices.IsSystemStyle then // This approach does not work well for many VCL styles, so we added an else case
    begin
      DrawGridLine(PaintInfo.Canvas, R)
      //StyleServices.DrawElement(PaintInfo.Canvas.Handle, StyleServices.GetElementDetails(tlGroupHeaderLineOpenSelectedNotFocused), R {$IF CompilerVersion  >= 34}, @R, CurrentPPI{$IFEND})
    end
    else begin
      DrawGridLine(PaintInfo.Canvas, R)
      //StyleServices.DrawElement(PaintInfo.Canvas.Handle, StyleServices.GetElementDetails(tbGroupBoxNormal), R {$IF CompilerVersion  >= 34}, @R, CurrentPPI{$IFEND});
    end;
  end;// else
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.EndOperation(OperationKind: TVTOperationKind);

// Called to indicate that a long-running operation has finished.

begin
  Assert(FOperationCount > 0, 'EndOperation must not be called when no operation in progress.');
  System.Dec(FOperationCount);
  DoEndOperation(OperationKind);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.EnsureNodeFocused();
begin
  if FocusedNode = nil then
    FocusedNode := Self.GetFirstSelected();
  if FocusedNode = nil then
    FocusedNode := Self.GetFirstVisible();
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.EnsureNodeSelected(pAfterDeletion: Boolean);
begin
  if IsEmpty then
    exit; // Nothing to do
  if (toAlwaysSelectNode in TreeOptions.SelectionOptions) or (pAfterDeletion and (toSelectNextNodeOnRemoval in TreeOptions.SelectionOptions)) then
  begin
    if (SelectedCount = 0) and not SelectionLocked then
    begin
      if not Assigned(FNextNodeToSelect) then
      begin
        FNextNodeToSelect := GetFirstVisible;
        // Avoid selecting a disabled node, see #954
        while Assigned(FNextNodeToSelect) and IsDisabled[FNextNodeToSelect] do
          FNextNodeToSelect := GetNextVisible(FNextNodeToSelect);
      end;
      Selected[FNextNodeToSelect] := True;
      Self.ScrollIntoView(Self.GetFirstSelected, False);
    end;// if nothing selected
    EnsureNodeFocused();
  end;//if toAlwaysSelectNode
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.FindNodeInSelection(P: PVirtualNode; var Index: Integer; LowBound,
  HighBound: Integer): Boolean;

// Search routine to find a specific node in the selection array.
// LowBound and HighBound determine the range in which to search the node.
// Either value can be -1 to denote the maximum range otherwise LowBound must be less or equal HighBound.

var
  L, H,
  I: Integer;

begin
  Result := False;
  L := 0;
  if LowBound >= 0 then
    L := LowBound;
  H := FSelectionCount - 1;
  if HighBound >= 0 then
    H := HighBound;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if PAnsiChar(FSelection[I]) < PAnsiChar(P) then
      L := I + 1
    else
    begin
      H := I - 1;
      if FSelection[I] = P then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FinishChunkHeader(Stream: TStream; StartPos, EndPos: Integer);

// used while streaming out a node to finally write out the size of the chunk

var
  Size: Integer;

begin
  // seek back to the second entry in the chunk header
  Stream.Position := StartPos + SizeOf(Size);
  // determine size of chunk without the chunk header
  Size := EndPos - StartPos - SizeOf(TChunkHeader);
  // write the size...
  Stream.Write(Size, SizeOf(Size));
  // ... and seek to the last endposition
  Stream.Position := EndPos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FontChanged(AFont: TObject);

// Little helper function for font changes (as they are not tracked in TBitmap/TCanvas.OnChange).

begin
  FFontChanged := True;
  if Assigned(FOldFontChange) then
    FOldFontChange(AFont);
  //if not (tsPainting in TreeStates) then AutoScale();
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetBorderDimensions: TSize;

// Returns the overall width of the current window border, depending on border styles.
// Note: these numbers represent the system's standards not special properties, which can be set for TWinControl
// (e.g. bevels, border width).

var
  Styles: Integer;

begin
  Result.cx := 0;
  Result.cy := 0;

  Styles := GetWindowLong(Handle, GWL_STYLE);
  if (Styles and WS_BORDER) <> 0 then
  begin
    Dec(Result.cx);
    Dec(Result.cy);
  end;
  if (Styles and WS_THICKFRAME) <> 0 then
  begin
    Dec(Result.cx, GetSystemMetrics(SM_CXFIXEDFRAME));
    Dec(Result.cy, GetSystemMetrics(SM_CYFIXEDFRAME));
  end;
  Styles := GetWindowLong(Handle, GWL_EXSTYLE);
  if (Styles and WS_EX_CLIENTEDGE) <> 0 then
  begin
    Dec(Result.cx, GetSystemMetrics(SM_CXEDGE));
    Dec(Result.cy, GetSystemMetrics(SM_CYEDGE));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetCheckImage(Node: PVirtualNode; ImgCheckType: TCheckType = ctNone; ImgCheckState:
  TCheckState = csUncheckedNormal; ImgEnabled: Boolean = True): Integer;

// Determines the index into the check image list for the given node depending on the check type
// and enabled state.

const
  // Four dimensional array consisting of image indices for the check type, the check state, the enabled state and the
  // hot state.
  CheckStateToCheckImage: array[ctCheckBox..ctButton, csUncheckedNormal..csMixedPressed, Boolean, Boolean] of Integer = (
    // ctCheckBox, ctTriStateCheckBox
    (
      // csUncheckedNormal (disabled [not hot, hot], enabled [not hot, hot])
      ((ckCheckUncheckedDisabled, ckCheckUncheckedDisabled), (ckCheckUncheckedNormal, ckCheckUncheckedHot)),
      // csUncheckedPressed (disabled [not hot, hot], enabled [not hot, hot])
      ((ckCheckUncheckedDisabled, ckCheckUncheckedDisabled), (ckCheckUncheckedPressed, ckCheckUncheckedPressed)),
      // csCheckedNormal
      ((ckCheckCheckedDisabled, ckCheckCheckedDisabled), (ckCheckCheckedNormal, ckCheckCheckedHot)),
      // csCheckedPressed
      ((ckCheckCheckedDisabled, ckCheckCheckedDisabled), (ckCheckCheckedPressed, ckCheckCheckedPressed)),
      // csMixedNormal
      ((ckCheckMixedDisabled, ckCheckMixedDisabled), (ckCheckMixedNormal, ckCheckMixedHot)),
      // csMixedPressed
      ((ckCheckMixedDisabled, ckCheckMixedDisabled), (ckCheckMixedPressed, ckCheckMixedPressed))
    ),
    // ctRadioButton
    (
      // csUncheckedNormal (disabled [not hot, hot], enabled [not hot, hot])
      ((ckRadioUncheckedDisabled, ckRadioUncheckedDisabled), (ckRadioUncheckedNormal, ckRadioUncheckedHot)),
      // csUncheckedPressed (disabled [not hot, hot], enabled [not hot, hot])
      ((ckRadioUncheckedDisabled, ckRadioUncheckedDisabled), (ckRadioUncheckedPressed, ckRadioUncheckedPressed)),
      // csCheckedNormal
      ((ckRadioCheckedDisabled, ckRadioCheckedDisabled), (ckRadioCheckedNormal, ckRadioCheckedHot)),
      // csCheckedPressed
      ((ckRadioCheckedDisabled, ckRadioCheckedDisabled), (ckRadioCheckedPressed, ckRadioCheckedPressed)),
      // csMixedNormal (should never appear with ctRadioButton)
      ((ckCheckMixedDisabled, ckCheckMixedDisabled), (ckCheckMixedNormal, ckCheckMixedHot)),
      // csMixedPressed (should never appear with ctRadioButton)
      ((ckCheckMixedDisabled, ckCheckMixedDisabled), (ckCheckMixedPressed, ckCheckMixedPressed))
    ),
    // ctButton
    (
      // csUncheckedNormal (disabled [not hot, hot], enabled [not hot, hot])
      ((ckButtonDisabled, ckButtonDisabled), (ckButtonNormal, ckButtonHot)),
      // csUncheckedPressed (disabled [not hot, hot], enabled [not hot, hot])
      ((ckButtonDisabled, ckButtonDisabled), (ckButtonPressed, ckButtonPressed)),
      // csCheckedNormal
      ((ckButtonDisabled, ckButtonDisabled), (ckButtonNormal, ckButtonHot)),
      // csCheckedPressed
      ((ckButtonDisabled, ckButtonDisabled), (ckButtonPressed, ckButtonPressed)),
      // csMixedNormal (should never appear with ctButton)
      ((ckCheckMixedDisabled, ckCheckMixedDisabled), (ckCheckMixedNormal, ckCheckMixedHot)),
      // csMixedPressed (should never appear with ctButton)
      ((ckCheckMixedDisabled, ckCheckMixedDisabled), (ckCheckMixedPressed, ckCheckMixedPressed))
    )
  );

var
  IsHot: Boolean;

begin
  if Assigned(Node) then
  begin
    ImgCheckType := Node.CheckType;
    ImgCheckState := GetCheckState(Node);
    ImgEnabled := not (vsDisabled in Node.States) and Self.Enabled;

    IsHot := Node = FCurrentHotNode;
  end
  else
    IsHot := False;

  if ImgCheckState.IsDisabled then begin // disabled image?
    // We need to use disabled images, so map ImgCheckState value from disabled to normal, as disabled state is expressed by ImgEnabled.
    ImgEnabled := False;
    ImgCheckState := ImgCheckState.GetEnabled();
  end;//if

  if ImgCheckType = ctTriStateCheckBox then
    ImgCheckType := ctCheckBox;
  if IsHot and (ImgCheckState in  [csCheckedNormal, csUncheckedNormal]) and (GetKeyState(VK_LBUTTON) < 0) and (hiOnItemCheckbox in FLastHitInfo.HitPositions) then
    System.Inc(ImgCheckState); // Advance to pressed state

  if ImgCheckType = ctNone then
    Result := -1
  else
    Result := CheckStateToCheckImage[ImgCheckType, ImgCheckState, ImgEnabled, IsHot];
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetColumnClass: TVirtualTreeColumnClass;

begin
  Result := TVirtualTreeColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetHeaderClass: TVTHeaderClass;

begin
  Result := TVTHeader;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.GetImageIndex(var Info: TVTPaintInfo; Kind: TVTImageKind; InfoIndex: TVTImageInfoIndex);

// Retrieves the image index and an eventual customized image list for drawing.

var
  CustomImages: TCustomImageList;

begin
  with Info do
  begin
    ImageInfo[InfoIndex].Index := -1;
    ImageInfo[InfoIndex].Ghosted := False;

    CustomImages := DoGetImageIndex(Node, Kind, Column, ImageInfo[InfoIndex].Ghosted, ImageInfo[InfoIndex].Index);
    if Assigned(CustomImages) then
      ImageInfo[InfoIndex].Images := CustomImages
  end;
end;

function TBaseVirtualTree.GetImageSize(Node: PVirtualNode; Kind: TVTImageKind = TVTImageKind.ikNormal; Column: TColumnIndex = 0; IncludePadding: Boolean = True): TSize;

// Determines whether the given node has got an image of the given kind in the given column.
// Returns the size of the image, or (0,0) if no image is available
// The given node will be implicitly initialized if needed.

var
  Ghosted: Boolean;
  Index: TImageIndex;
  lImageList: TCustomImageList;
begin
  if not Assigned(OnGetImageIndexEx) and (((Kind = TVTImageKind.ikNormal) and not Assigned(fImages))
    or ((Kind = TVTImageKind.ikState) and not Assigned(fStateImages))) then
  begin
    Result.cx := 0;
    Result.cy := 0;
  end;
  if not (vsInitialized in Node.States) then
    InitNode(Node);
  Index := -1;
  Ghosted := False;
  lImageList := DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
  if Index >= 0 then begin
    if IncludePadding then
      Result.cx := lImageList.Width + ScaledPixels(2)
    else
      Result.cx := lImageList.Width;
    Result.cy := lImageList.Height;
  end
  else begin
    Result.cx := 0;
    Result.cy := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsEmpty: Boolean;
begin
  Result := (Self.ChildCount[nil] = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNodeImageSize(Node: PVirtualNode): TSize;

  // Returns the size of an image
  // Override if you need different sized images for certain nodes.
begin
  Result := GetImageSize(Node);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetMaxRightExtend(): TDimension;

// Determines the maximum with of the currently visible part of the tree, depending on the length
// of the node texts. This method is used for determining the horizontal scroll range if no columns are used.

var
  Node,
  NextNode: PVirtualNode;
  TopPosition: TDimension;
  CurrentWidth: TDimension;

begin
  Node := GetNodeAt(0, 0, True, TopPosition);
  Result := 0;
  if not Assigned(Node) then
    exit;

  while Assigned(Node) do
  begin
    if not (vsInitialized in Node.States) then
      InitNode(Node);
    CurrentWidth := GetOffset(TVTElement.ofsRightOfText, Node);
    if Result < (CurrentWidth) then
      Result := CurrentWidth;
    Inc(TopPosition, NodeHeight[Node]);
    if TopPosition > Height then
      Break;

    // Get next visible node and update left node position.
    NextNode := GetNextVisible(Node, True);
    if NextNode = nil then
      Break;
    Node := NextNode;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.GetNativeClipboardFormats(var Formats: TFormatEtcArray);

// Returns the supported clipboard formats of the tree.

begin
  TClipboardFormatList.EnumerateFormats(TVirtualTreeClass(ClassType), Formats, FClipboardFormats);
  // Ask application/descendants for self defined formats.
  DoGetUserClipboardFormats(Formats);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetOperationCanceled;

begin
  Result := FOperationCanceled and (FOperationCount > 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetOptionsClass: TTreeOptionsClass;

begin
  Result := TCustomVirtualTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.HandleHotTrack(X, Y: TDimension);

// Updates the current "hot" node.

var
  HitInfo: THitInfo;
  CheckPositions: THitPositions;
  ButtonIsHit,
  DoInvalidate: Boolean;
  oldHotNode : PVirtualNode;
begin
  if not IsMouseCursorVisible then
  begin
    if Assigned(FCurrentHotNode) then
    begin
      InvalidateNode(FCurrentHotNode);
      FCurrentHotNode := nil;
    end;
    Exit;
  end;//if  not IsMouseCursorVisible

  DoInvalidate := False;
  oldHotNode := FCurrentHotNode;
  // Get information about the hit.
  GetHitTestInfoAt(X, Y, True, HitInfo, []);

  // Only make the new node being "hot" if its label is hit or full row selection is enabled.
  CheckPositions := [hiOnItemLabel, hiOnItemCheckbox];

  // If running under Windows Vista using the explorer theme hitting the buttons makes the node hot, too.
  if tsUseExplorerTheme in FStates then
    Include(CheckPositions, hiOnItemButtonExact);

  if (CheckPositions * HitInfo.HitPositions = []) and
    (not (toFullRowSelect in FOptions.SelectionOptions) or (hiNowhere in HitInfo.HitPositions)) then
    FCurrentHotNode := nil
  else
    FCurrentHotNode := HitInfo.HitNode;
  if (FCurrentHotNode <> oldHotNode) or (HitInfo.HitColumn <> FCurrentHotColumn) then
  begin
    DoInvalidate := (toHotTrack in FOptions.PaintOptions) or (toCheckSupport in FOptions.MiscOptions) or (oldHotNode <> FCurrentHotNode);
    DoHotChange(oldHotNode, HitInfo.HitNode);
    if Assigned(oldHotNode) and DoInvalidate then
      InvalidateNode(oldHotNode);
    FCurrentHotColumn := HitInfo.HitColumn;
  end;

    ButtonIsHit := (hiOnItemButtonExact in HitInfo.HitPositions);
  if Assigned(HitInfo.HitNode) and ((FHotNodeButtonHit <> ButtonIsHit) or (FCurrentHotNode <> oldHotNode) or DoInvalidate) then
  begin
    FHotNodeButtonHit := ButtonIsHit;
    InvalidateNode(HitInfo.HitNode);
  end
  else
    if not Assigned(HitInfo.HitNode) then
      FHotNodeButtonHit := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.HandleIncrementalSearch(CharCode: Word);

var
  Run, Stop: PVirtualNode;
  GetNextNode: TGetNextNodeProc;
  NewSearchText: string;
  SingleLetter,
  PreviousSearch: Boolean; // True if VK_BACK was sent.
  SearchDirection: TVTSearchDirection;

  //--------------- local functions -------------------------------------------

  procedure SetupNavigation;

  // If the search buffer is empty then we start searching with the next node after the last one, otherwise
  // we continue with the last one. Node navigation function is set up too here, to avoid frequent checks.

  var
    FindNextNode: Boolean;

  begin
    FindNextNode := (Length(FSearchBuffer) = 0) or (Run = nil) or SingleLetter or PreviousSearch;
    case FIncrementalSearch of
      isVisibleOnly:
        if SearchDirection = sdForward then
        begin
          GetNextNode := GetNextVisible;
          if FindNextNode then
          begin
            if Run = nil then
              Run := GetFirstVisible(nil, True)
            else
            begin
              Run := GetNextVisible(Run, True);
              // Do wrap around.
              if Run = nil then
                Run := GetFirstVisible(nil, True);
            end;
          end;
        end
        else
        begin
          GetNextNode := GetPreviousVisible;
          if FindNextNode then
          begin
            if Run = nil then
              Run := GetLastVisible(nil, True)
            else
            begin
              Run := GetPreviousVisible(Run, True);
              // Do wrap around.
              if Run = nil then
                Run := GetLastVisible(nil, True);
            end;
          end;
        end;
      isInitializedOnly:
        if SearchDirection = sdForward then
        begin
          GetNextNode := GetNextNoInit;
          if FindNextNode then
          begin
            if Run = nil then
              Run := GetFirstNoInit
            else
            begin
              Run := GetNextNoInit(Run);
              // Do wrap around.
              if Run = nil then
                Run := GetFirstNoInit;
            end;
          end;
        end
        else
        begin
          GetNextNode := GetPreviousNoInit;
          if FindNextNode then
          begin
            if Run = nil then
              Run := GetLastNoInit
            else
            begin
              Run := GetPreviousNoInit(Run);
              // Do wrap around.
              if Run = nil then
                Run := GetLastNoInit;
            end;
          end;
        end;
    else
      // isAll
      if SearchDirection = sdForward then
      begin
        GetNextNode := GetNext;
        if FindNextNode then
        begin
          if Run = nil then
            Run := GetFirst
          else
          begin
            Run := GetNext(Run);
            // Do wrap around.
            if Run = nil then
              Run := GetFirst;
          end;
        end;
      end
      else
      begin
        GetNextNode := GetPrevious;
        if FindNextNode then
        begin
          if Run = nil then
            Run := GetLast
          else
          begin
            Run := GetPrevious(Run);
            // Do wrap around.
            if Run = nil then
              Run := GetLast;
          end;
        end;
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  function CodePageFromLocale(Language: LCID): Integer;

  // Determines the code page for a given locale.
  // Unfortunately there is no easier way than this, currently.

  var
    Buf: array[0..6] of Char;

  begin
    GetLocaleInfo(Language, LOCALE_IDEFAULTANSICODEPAGE, Buf, 6);
    Result := StrToIntDef(Buf, GetACP);
  end;

  //---------------------------------------------------------------------------

  function KeyUnicode(C: Char): WideChar;
  // Converts the given character into its corresponding Unicode character
  // depending on the active keyboard layout.
  begin
    Result := C;      //!!!!!!
  end;

  //--------------- end local functions ---------------------------------------

var
  FoundMatch: Boolean;
  NewChar: WideChar;

begin
  StopTimer(SearchTimer);

  if FIncrementalSearch <> isNone then
  begin
    if CharCode <> 0 then
    begin
      DoStateChange([tsIncrementalSearching]);

      // Convert the given virtual key code into a Unicode character based on the current locale.
      NewChar := KeyUnicode(Char(CharCode));
      PreviousSearch := NewChar = WideChar(VK_BACK);
      // We cannot do a search with an empty search buffer.
      if not PreviousSearch or (FSearchBuffer <> '') then
      begin
        // Determine which method to use to advance nodes and the start node to search from.
        case FSearchStart of
          ssAlwaysStartOver:
            Run := nil;
          ssFocusedNode:
            Run := FFocusedNode;
        else // ssLastHit
          Run := FLastSearchNode;
        end;

        // Make sure the start node corresponds to the search criterion.
        if Assigned(Run) then
        begin
          case FIncrementalSearch of
            isInitializedOnly:
              if not (vsInitialized in Run.States) then
                Run := nil;
            isVisibleOnly:
              if not FullyVisible[Run] or IsEffectivelyFiltered[Run] then
                Run := nil;
          end;
        end;
        Stop := Run;

        // VK_BACK temporarily changes search direction to opposite mode.
        if PreviousSearch then
        begin
          if SearchDirection = sdBackward then
            SearchDirection := sdForward
          else
            SearchDirection := sdBackward;
        end
        else
          SearchDirection := FSearchDirection;
        // The "single letter mode" is used to advance quickly from node to node when pressing the same key several times.
        SingleLetter := (Length(FSearchBuffer) = 1) and not PreviousSearch and (FSearchBuffer[1] = NewChar);
        // However if the current hit (if there is one) would fit also with a repeated character then
        // don't use single letter mode.
        if SingleLetter and (DoIncrementalSearch(Run, FSearchBuffer + NewChar) = 0) then
          SingleLetter := False;
        SetupNavigation;
        FoundMatch := False;

        if Assigned(Run) then
        begin
          if SingleLetter then
            NewSearchText := FSearchBuffer
          else
            if PreviousSearch then
            begin
              SetLength(FSearchBuffer, Length(FSearchBuffer) - 1);
              NewSearchText := FSearchBuffer;
            end
            else
              NewSearchText := FSearchBuffer + NewChar;

          repeat
            if DoIncrementalSearch(Run, NewSearchText) = 0 then
            begin
              FoundMatch := True;
              Break;
            end;

            // Advance to next node if we have not found a match.
            Run := GetNextNode(Run);
            // Do wrap around start or end of tree.
            if (Run <> Stop) and (Run = nil) then
              SetupNavigation;
          until Run = Stop;
        end;

        if FoundMatch then
        begin
          ClearSelection;
          FSearchBuffer := NewSearchText;
          FLastSearchNode := Run;
          FocusedNode := Run;
          AddToSelection(Run, False);
          FLastSearchNode := Run;
        end
        else
          // Play an acoustic signal if nothing could be found but don't beep if only the currently
          // focused node matches.
          if Assigned(Run) and (DoIncrementalSearch(Run, NewSearchText) <> 0) then
            Beep;
      end;
    end;

    // Restart search timeout interval.
    SetTimer(Handle, SearchTimer, FSearchTimeout, nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);

var
  Node: PVirtualNode;
  MayEdit: Boolean;

begin
  MayEdit := not (tsEditing in FStates) and (toEditOnDblClick in FOptions.MiscOptions);
  if tsEditPending in FStates then
  begin
    StopTimer(EditTimer);
    DoStateChange([], [tsEditPending]);
  end;

  if not (tsEditing in FStates) or DoEndEdit then
  begin
    if HitInfo.HitColumn = FHeader.Columns.ClickIndex then
      DoColumnDblClick(HitInfo.HitColumn, KeysToShiftState(Message.Keys));

      if HitInfo.HitNode <> nil then
        DoNodeDblClick(HitInfo);

    Node := nil;
    if (hiOnItem in HitInfo.HitPositions) and (HitInfo.HitColumn > NoColumn) and
       (coFixed in FHeader.Columns[HitInfo.HitColumn].Options) then
    begin
      if hiUpperSplitter in HitInfo.HitPositions then
        Node := GetPreviousVisible(HitInfo.HitNode, True)
      else
        if  hiLowerSplitter in HitInfo.HitPositions then
          Node := HitInfo.HitNode;
    end;

    if Assigned(Node) and (Node <> FRoot) and (toNodeHeightDblClickResize in FOptions.MiscOptions) then
    begin
      if DoNodeHeightDblClickResize(Node, HitInfo.HitColumn, KeysToShiftState(Message.Keys), Point(Message.XPos, Message.YPos)) then
      begin
        SetNodeHeight(Node, FDefaultNodeHeight);
        UpdateWindow();
        MayEdit := False;
      end;
    end
    else
      if hiOnItemCheckBox in HitInfo.HitPositions then
      begin
        HandleCheckboxClick(HitInfo.HitNode, Message.Keys);
        MayEdit := False;
      end// if hiOnItemCheckBox
      else
      begin
        if hiOnItemButton in HitInfo.HitPositions then
        begin
          ToggleNode(HitInfo.HitNode);
          MayEdit := False;
        end
        else
        begin
          if toToggleOnDblClick in FOptions.MiscOptions then
          begin
            if ((([hiOnItemButton, hiOnItemLabel, hiOnNormalIcon, hiOnStateIcon] * HitInfo.HitPositions) <> []) or
              ((toFullRowSelect in FOptions.SelectionOptions) and Assigned(HitInfo.HitNode))) then
            begin
              ToggleNode(HitInfo.HitNode);
              MayEdit := False;
            end;
          end;
        end;
      end;
  end;

  if MayEdit and Assigned(FFocusedNode) and (FFocusedNode = HitInfo.HitNode) and
    (FFocusedColumn = HitInfo.HitColumn) and CanEdit(FFocusedNode, HitInfo.HitColumn) then
  begin
    DoStateChange([tsEditPending]);
    FEditColumn := FFocusedColumn;
    SetTimer(Handle, EditTimer, 0, nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.HandleCheckboxClick(pHitNode: PVirtualNode; pKeys: LongInt);
var
  NewCheckState: TCheckState;
begin
    NewCheckState := DetermineNextCheckState(pHitNode.CheckType, pHitNode.CheckState);
    if (ssLeft in KeysToShiftState(pKeys)) and DoChecking(pHitNode, NewCheckState) then
    begin
      if (Self.SelectedCount > 1) and (Selected[pHitNode]) and not (toSyncCheckboxesWithSelection in TreeOptions.SelectionOptions) then
        SetCheckStateForAll(NewCheckState, True)
      else
        DoCheckClick(pHitNode, NewCheckState);
    end;//if ssLeft
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo);

// centralized mouse button down handling

var
  LastFocused: PVirtualNode;
  Column: TColumnIndex;
  ShiftState: TShiftState;

  // helper variables to shorten boolean equations/expressions
  AutoDrag,              // automatic (or allowed) drag start
  IsLabelHit,            // the node's caption or images are hit
  IsCellHit,             // for grid extension or full row select (but not check box, button)
  IsAnyHit,              // either IsHit or IsCellHit
  IsHeightTracking,      // height tracking
  MultiSelect,           // multiselection is enabled
  ShiftEmpty,            // ShiftState = []
  NodeSelected: Boolean; // the new node (if any) is selected
  NewColumn: Boolean;    // column changed
  NewNode: Boolean;      // Node changed.
  NeedChangeEvent: Boolean;   // change event is required for selection change
  CanClear: Boolean;
  AltPressed: Boolean;   // Pressing the Alt key enables special processing for selection.
  FullRowDrag: Boolean;  // Start dragging anywhere within a node's bound.
  NodeRect: TRect;

  //--------------- local functions -------------------------------------------

  //Fix for issue: 310 whenever there is a need to invalidate a column, consider
  //auto spanned columns if applicable
  procedure invalidateWithAutoSpan(acolumn: TColumnIndex; anode: PVirtualNode);
  var
    NextColumn: Integer;
    Dummy: TColumnIndex;
  begin
    if (not FHeader.UseColumns) or (not (toAutoSpanColumns in FOptions.AutoOptions))
       or (acolumn = FHeader.MainColumn) then
    begin
      //no need to find auto spanned next columns
      InvalidateColumn(acolumn);
      exit;
    end;
    //invalidate auto spanned columns too
    with FHeader.Columns do //standard loop for auto span
    begin
      NextColumn := acolumn;
      repeat
        InvalidateColumn(NextColumn);
        Dummy := GetNextVisibleColumn(NextColumn);
        if (Dummy = InvalidColumn) or
           not ColumnIsEmpty(anode, Dummy)
           or
           (Items[Dummy].BidiMode <> bdLeftToRight) then
          Break;
        NextColumn := Dummy;
      until False;
    end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  if tsPanning in FStates then
  begin
    StopWheelPanning;
    Exit;
  end;

  if tsEditPending in FStates then
  begin
    StopTimer(EditTimer);
    DoStateChange([], [tsEditPending]);
  end;

  FLastHitInfo := HitInfo; // Save for later use in OnNodeClick event, see issue #692
  if (tsEditing in FStates) then begin
    if not DoEndEdit then
      exit;
    // Repeat the hit test as an OnEdited event might got triggered that could modify the tree.
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
  end;//if tsEditing

  // Focus change. Don't use the SetFocus method as this does not work for MDI Winapi.Windows.
  if not Focused and CanFocus then
  begin
    Winapi.Windows.SetFocus(Handle);
    // Repeat the hit test as an OnExit event might got triggered that could modify the tree.
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo, KeysToShiftState(Message.Keys));
    FLastHitInfo := HitInfo; // See issue #1297
  end;

  if IsEmpty then
    Exit; // Nothing to do

  // Keep clicked column in case the application needs it.
  FHeader.Columns.ClickIndex := HitInfo.HitColumn;

  // Change column only if we have hit the node label.
  if (hiOnItemLabel in HitInfo.HitPositions) or
    (toFullRowSelect in FOptions.SelectionOptions) or
    (toGridExtensions in FOptions.MiscOptions) then
  begin
    NewColumn := FFocusedColumn <> HitInfo.HitColumn;
    if toExtendedFocus in FOptions.SelectionOptions then
      Column := HitInfo.HitColumn
    else
      Column := FHeader.MainColumn;
  end
  else
  begin
    NewColumn := False;
    Column := FFocusedColumn;
  end;

  if NewColumn and not FHeader.AllowFocus(Column) then
  begin
    NewColumn := False;
    Column := FFocusedColumn;
  end;

  NewNode := FFocusedNode <> HitInfo.HitNode;

  // Translate keys and filter out shift and control key.
  ShiftState := KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt];
  if ssAlt in ShiftState then
  begin
    AltPressed := True;
    // Remove the Alt key from the shift state. It is not meaningful there.
    Exclude(ShiftState, ssAlt);
  end
  else
    AltPressed := False;

  // Various combinations determine what states the tree enters now.
  // We initialize shorthand variables to avoid the following expressions getting too large
  // and to avoid repeative expensive checks.
  IsLabelHit := not AltPressed and not (toSimpleDrawSelection in FOptions.SelectionOptions) and
    ((hiOnItemLabel in HitInfo.HitPositions) or (hiOnNormalIcon in HitInfo.HitPositions));

  IsCellHit := not IsLabelHit and Assigned(HitInfo.HitNode) and
    ([hiOnItemButton, hiOnItemCheckBox, hiNoWhere] * HitInfo.HitPositions = []) and
    ((toFullRowSelect in FOptions.SelectionOptions) or
    ((toGridExtensions in FOptions.MiscOptions) and (HitInfo.HitColumn > NoColumn)));

  IsAnyHit := IsLabelHit or IsCellHit;
  MultiSelect := toMultiSelect in FOptions.SelectionOptions;
  ShiftEmpty := ShiftState = [];
  NodeSelected := IsAnyHit and (vsSelected in HitInfo.HitNode.States);

  // Determine the Drag behavior.
  if MultiSelect and not (toDisableDrawSelection in FOptions.SelectionOptions) then
  begin
    // We have MultiSelect and want to draw a selection rectangle.
    // We will start a full row drag only in case a label was hit,
    // otherwise a multi selection will start.
    FullRowDrag := (toFullRowDrag in FOptions.MiscOptions) and IsCellHit and
        not (hiNowhere in HitInfo.HitPositions) and
        (NodeSelected or (hiOnItemLabel in HitInfo.HitPositions) or (hiOnNormalIcon in HitInfo.HitPositions));
  end
  else // No MultiSelect, hence we can start a drag anywhere in the row.
    FullRowDrag := toFullRowDrag in FOptions.MiscOptions;

  IsHeightTracking := (Message.Msg = WM_LBUTTONDOWN) and
                      (hiOnItem in HitInfo.HitPositions) and
                      ([hiUpperSplitter, hiLowerSplitter] * HitInfo.HitPositions <> []);

  // Dragging might be started in the inherited handler manually (which is discouraged for stability reasons)
  // the test for manual mode is done below (after the focused node is set).
  AutoDrag := ((DragMode = TDragMode.dmAutomatic) or Dragging) and (not IsCellHit or FullRowDrag);

  // Query the application to learn if dragging may start now (if set to dmManual).
  if Assigned(HitInfo.HitNode) and not AutoDrag and (DragMode = TDragMode.dmManual) then
    AutoDrag := DoBeforeDrag(HitInfo.HitNode, Column) and (FullRowDrag or IsLabelHit);

  // handle node height tracking
  if IsHeightTracking then
  begin
    if hiUpperSplitter in HitInfo.HitPositions then
      FHeightTrackNode := GetPreviousVisible(HitInfo.HitNode, True)
    else
      FHeightTrackNode := HitInfo.HitNode;

    if CanSplitterResizeNode(Point(Message.XPos, Message.YPos), FHeightTrackNode, HitInfo.HitColumn) then
    begin
      FHeightTrackColumn := HitInfo.HitColumn;
      NodeRect := GetDisplayRect(FHeightTrackNode, FHeightTrackColumn, False);
      FHeightTrackPoint := Point(NodeRect.Left, NodeRect.Top);
      DoStateChange([tsNodeHeightTrackPending]);
      Exit;
    end;
  end;

  // handle button clicks
  if (hiOnItemButton in HitInfo.HitPositions) and (vsHasChildren in HitInfo.HitNode.States) then
  begin
    ToggleNode(HitInfo.HitNode);
    Exit;
  end;

  // check event
  if hiOnItemCheckBox in HitInfo.HitPositions then
  begin
    HandleCheckboxClick(HitInfo.HitNode, Message.Keys);
    Exit;
  end;

  // Keep this node's level in case we need it for constraint selection.
  if (FRoot.ChildCount > 0) and ShiftEmpty or (FSelectionCount = 0) then
    if Assigned(HitInfo.HitNode) then
      FLastSelectionLevel := GetNodeLevelForSelectConstraint(HitInfo.HitNode)
    else
      FLastSelectionLevel := GetNodeLevelForSelectConstraint(GetLastVisibleNoInit(nil, True));

  // immediate clearance
  // Determine for the right mouse button if there is a popup menu. In this case and if drag'n drop is pending
  // the current selection has to stay as it is.
  with HitInfo, Message do
    CanClear := not AutoDrag and
      (not (tsRightButtonDown in FStates) or not HasPopupMenu(HitNode, HitColumn, Point(XPos, YPos)));

  // pending clearance
  if MultiSelect and ShiftEmpty and not (hiOnItemCheckbox in HitInfo.HitPositions) and IsAnyHit and AutoDrag and
    NodeSelected and not FSelectionLocked
  then
    DoStateChange([tsClearPending]);

  // User starts a selection with a selection rectangle.
  if not (toDisableDrawSelection in FOptions.SelectionOptions) and not (IsLabelHit or FullRowDrag) and MultiSelect then
  begin
    SetCapture(Handle);
    DoStateChange([tsDrawSelPending]);
    FDrawSelShiftState := ShiftState;
    FNewSelRect := Rect(Message.XPos + FEffectiveOffsetX, Message.YPos - FOffsetY, Message.XPos + FEffectiveOffsetX,
      Message.YPos - FOffsetY);
    FLastSelRect := Rect(0, 0, 0, 0);
  end;

  NeedChangeEvent := FSelectionCount >= 1;
  if not FSelectionLocked and ((not (IsAnyHit or FullRowDrag) and MultiSelect and ShiftEmpty) or
    (IsAnyHit and (not NodeSelected or (NodeSelected and CanClear)) and (ShiftEmpty or not MultiSelect or (tsRightButtonDown in FStates)))) then
  begin
    // If the currently hit node was already selected then we have to reselect it again after clearing the current
    // selection, but without a change event if it is the only selected node.
    // The same applies if the Alt key is pressed, which allows to start drawing the selection rectangle also
    // on node captions and images. Here the previous selection state does not matter, though.
    if NodeSelected or (AltPressed and Assigned(HitInfo.HitNode) and (HitInfo.HitColumn = FHeader.MainColumn)) and not (hiNowhere in HitInfo.HitPositions) then
    begin
      InternalClearSelection;
      InternalAddToSelection(HitInfo.HitNode, True);
      if NeedChangeEvent then
      begin
        Invalidate;
        Change(nil);
      end;
    end
    else if (toAlwaysSelectNode in Self.TreeOptions.SelectionOptions) then
    begin
      if not (hiNowhere in HitInfo.HitPositions) then
        ClearSelection(False)
      else
        if not (ssCtrl in ShiftState) then
          DoStateChange([tsClearOnNewSelection], []);
    end
    else
      ClearSelection(False);
    end;

  // pending node edit
  if Focused and
    ((hiOnItemLabel in HitInfo.HitPositions) or ((toGridExtensions in FOptions.MiscOptions) and
    (hiOnItem in HitInfo.HitPositions))) and NodeSelected and not NewColumn and ShiftEmpty and (SelectedCount = 1) then
  begin
    DoStateChange([tsEditPending]);
  end;

  if not (toDisableDrawSelection in FOptions.SelectionOptions)
    and not (IsLabelHit or FullRowDrag) and (MultiSelect or (hiNowhere in HitInfo.HitPositions)) then
  begin
    // The original code here was moved up to fix issue #187.
    // In order not to break the semantics of this procedure, we are leaving these if statements here
    if not IsCellHit then begin
      if NeedChangeEvent then
        Change(nil);
      Exit;
    end;
  end;

  // Keep current mouse position.
  FLastClickPos := Point(Message.XPos, Message.YPos);

  // Handle selection and node focus change.
  if (IsLabelHit or IsCellHit) and
     DoFocusChanging(FFocusedNode, HitInfo.HitNode, FFocusedColumn, Column) then
  begin
    if NewColumn then
    begin

      if not Assigned(FFocusedNode) then
        InvalidateColumn(FFocusedColumn)
      else
        invalidateWithAutoSpan(FFocusedColumn, FFocusedNode); //fix: issue 310
      if not Assigned(HitInfo.HitNode) then
        InvalidateColumn(Column)
      else
        invalidateWithAutoSpan(Column, HitInfo.HitNode); //fix: issue 310
      FFocusedColumn := Column;
    end;
    if DragKind = dkDock then
    begin
      StopTimer(ScrollTimer);
      DoStateChange([], [tsScrollPending, tsScrolling]);
    end;
    // Get the currently focused node to make multiple multi-selection blocks possible.
    LastFocused := FFocusedNode;
    if NewNode then
      DoFocusNode(HitInfo.HitNode, False);

    if MultiSelect and not ShiftEmpty and not (tsRightButtonDown in FStates) then
      HandleClickSelection(LastFocused, HitInfo.HitNode, ShiftState, AutoDrag)
    else
    begin
      if ShiftEmpty then
        FRangeAnchor := HitInfo.HitNode;

      // If the hit node is not yet selected then do it now.
      if not NodeSelected then
        AddToSelection(HitInfo.HitNode, True);
    end;

    if NewNode or NewColumn then
    begin
      ScrollIntoView(FFocusedNode, False,
        not (toDisableAutoscrollOnFocus in FOptions.AutoOptions)
        and not (toFullRowSelect in FOptions.SelectionOptions));

      DoFocusChange(FFocusedNode, FFocusedColumn);
    end;
  end;

  if (SelectedCount = 0) and NeedChangeEvent then
    Change(nil);

  // Drag'n drop initiation
  // If we lost focus in the interim the button states would be cleared in WM_KILLFOCUS.
  if AutoDrag and IsAnyHit and (FStates * [tsLeftButtonDown, tsRightButtonDown, tsMiddleButtonDown] <> []) then
    BeginDrag(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.HandleMouseUp(var Message: TWMMouse; const HitInfo: THitInfo);

// Counterpart to the mouse down handler.

var
  ReselectFocusedNode: Boolean;

begin
  ReleaseCapture;

  if not (tsVCLDragPending in FStates) then
  begin
    // reset pending or persistent states
    if IsMouseSelecting then
    begin
      DoStateChange([], [tsDrawSelecting, tsDrawSelPending, tsToggleFocusedSelection, tsClearOnNewSelection]);
      Invalidate;
    end;

    if tsClearPending in FStates then
    begin
      ReselectFocusedNode := Assigned(FFocusedNode) and (vsSelected in FFocusedNode.States);
      ClearSelection;
      if ReselectFocusedNode then
        AddToSelection(FFocusedNode, False);
    end;

    if (tsToggleFocusedSelection in FStates) and (HitInfo.HitNode = FFocusedNode) and Assigned(HitInfo.HitNode) then //Prevent AV when dereferencing HitInfo.HitNode below, see bug #100
    begin
      if vsSelected in HitInfo.HitNode.States then
      begin
        if not (toAlwaysSelectNode in TreeOptions.SelectionOptions) or (Self.SelectedCount > 1) then
          RemoveFromSelection(HitInfo.HitNode);
      end
      else
        AddToSelection(HitInfo.HitNode, False);
    end;

    DoStateChange([], [tsOLEDragPending, tsOLEDragging, tsClearPending, tsDrawSelPending, tsToggleFocusedSelection,
      tsScrollPending, tsScrolling]);
    StopTimer(ScrollTimer);

    if (FHeader.Columns.ClickIndex > NoColumn) and (FHeader.Columns.ClickIndex = HitInfo.HitColumn) then
      DoColumnClick(HitInfo.HitColumn, KeysToShiftState(Message.Keys));

    if FLastHitInfo.HitNode <> nil then begin // Use THitInfo of mouse down here, see issue #692
      DoNodeClick(FLastHitInfo);
      if Assigned(FLastHitInfo.HitNode) then begin
        InvalidateNode(FLastHitInfo.HitNode);
        FLastHitInfo.HitNode := nil; // prevent firing the event again
      end;//if
    end;

    // handle a pending edit event
    if tsEditPending in FStates then
    begin
      // Is the mouse still over the same node?
      if (HitInfo.HitNode = FFocusedNode) and (hiOnItem in HitInfo.HitPositions) and
         (toEditOnClick in FOptions.MiscOptions) and (FFocusedColumn = HitInfo.HitColumn) and
         CanEdit(FFocusedNode, HitInfo.HitColumn) then
      begin
        FEditColumn := FFocusedColumn;
        SetTimer(Handle, EditTimer, FEditDelay, nil);
      end
      else
        DoStateChange([], [tsEditPending]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.HasImage(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex): Boolean;

// Determines whether the given node has got an image of the given kind in the given column.
// Returns True if so, otherwise False.
// The given node will be implicitly initialized if needed.

var
  Ghosted: Boolean;
  Index: TImageIndex;

begin
  if not (vsInitialized in Node.States) then
    InitNode(Node);

  Index := -1;
  Ghosted := False;
  DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
  Result := Index > -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.HasPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Pos: TPoint): Boolean;

// Determines whether the tree got a popup menu, either in its PopupMenu property, via the OnGetPopupMenu event or
// through inheritance. The latter case must be checked by the descendant which must override this method.

begin
  Result := Assigned(PopupMenu) or Assigned(DoGetPopupMenu(Node, Column, Pos));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.IncVisibleCount;
begin
  System.Inc(FVisibleCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InitChildren(Node: PVirtualNode);

// Initiates the initialization of the child number of the given node.

var
  Count: Cardinal;

begin
  if Assigned(Node) and (Node <> FRoot) and (vsHasChildren in Node.States) then
  begin
    Count := Node.ChildCount;
    if DoInitChildren(Node, Count) then
    begin
      SetChildCount(Node, Count);
      if Count = 0 then
        Exclude(Node.States, vsHasChildren);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InitNode(Node: PVirtualNode);

// Initiates the initialization of the given node to allow the application to load needed data for it.

var
  InitStates: TVirtualNodeInitStates;
  MustAdjustInternalVariables: Boolean;
  ParentCheckState, SelfCheckState: TCheckState;
begin
  with Node^ do
  begin
    Include(States, vsInitializing);
    try
      InitStates := [];
      if vsInitialized in States then
        Include(InitStates, ivsReInit);
      Include(States, vsInitialized);
      if Parent = FRoot then
        DoInitNode(nil, Node, InitStates)
      else
        DoInitNode(Parent, Node, InitStates);

      // Fix: Any parent check state must be propagated here.
	    // Because the CheckType is normally set in DoInitNode
      // by the App.
      if (Node.CheckType = ctTriStateCheckBox) and (toAutoTristateTracking in FOptions.AutoOptions)  then
      begin
        ParentCheckState := Self.GetCheckState(Node.Parent);
        SelfCheckState := Self.GetCheckState(Node);
        if ((ParentCheckState = csCheckedNormal)
             or (ParentCheckState = csUncheckedNormal))
            and (not SelfCheckState.IsDisabled())
            and (SelfCheckState <> ParentCheckState)
            and (Parent <> FRoot)
        then
          SetCheckState(Node, Node.Parent.CheckState);
      end
      else if (toSyncCheckboxesWithSelection in TreeOptions.SelectionOptions) then
        Node.CheckType := TCheckType.ctCheckBox;

      if ivsDisabled in InitStates then
        Include(States, vsDisabled);
      if ivsHasChildren in InitStates then
        Include(States, vsHasChildren);
      if ivsSelected in InitStates then
        InternalAddToSelection(Node, False);
      if ivsMultiline in InitStates then
        Include(States, vsMultiline);
      if ivsFiltered in InitStates then
      begin
        MustAdjustInternalVariables := not ((ivsReInit in InitStates) and (vsFiltered in States));

        Include(States, vsFiltered);

        if not (toShowFilteredNodes in FOptions.PaintOptions) and MustAdjustInternalVariables then
        begin
          AdjustTotalHeight(Node, -NodeHeight, True);
          if FullyVisible[Node] then
            System.Dec(FVisibleCount);
          if FUpdateCount = 0 then
            UpdateScrollBars(True);
        end;
      end;

      // Expanded may already be set (when called from ReinitNode) or be set in DoInitNode, allow both.
      if (vsExpanded in Node.States) xor (ivsExpanded in InitStates) then
      begin
        // Expand node if not yet done (this will automatically initialize child nodes).
        if ivsExpanded in InitStates then
          ToggleNode(Node)
        else
          // If the node already was expanded then explicitly trigger child initialization.
          if vsHasChildren in Node.States then
            InitChildren(Node);
      end;
    finally
      Exclude(States, vsInitializing);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InternalAddFromStream(Stream: TStream; Version: Integer; Node: PVirtualNode);

// Loads all details for Node (including its children) from the given stream.
// Because the new nodes might be selected this method also fixes the selection array.

var
  Stop: PVirtualNode;
  Index: Integer;
  LastTotalHeight: TDimension;
  WasFullyVisible: Boolean;

begin
  Assert(Node <> FRoot, 'The root node cannot be loaded from stream.');

  // Keep the current total height value of Node as it has already been applied
  // but might change in the load and fixup code. We have to adjust that afterwards.
  LastTotalHeight := Node.TotalHeight;
  WasFullyVisible := FullyVisible[Node] and not IsEffectivelyFiltered[Node];

  // Read in the new nodes.
  ReadNode(Stream, Version, Node);

  // One time update of node-internal states and the global visibility counter.
  // This is located here to ease and speed up the loading process.
  FixupTotalCount(Node);
  AdjustTotalCount(Node.Parent, Node.TotalCount - 1, True); // -1 because Node itself was already set.
  FixupTotalHeight(Node);
  AdjustTotalHeight(Node.Parent, Node.TotalHeight - LastTotalHeight, True);

  // New nodes are always visible, so the visible node count has been increased already.
  // If Node is now invisible we have to take back this increment and don't need to add any visible child node.
  if not FullyVisible[Node] or IsEffectivelyFiltered[Node] then
  begin
    if WasFullyVisible then
      System.Dec(FVisibleCount);
  end
  else
    // It can never happen that the node is now fully visible but was not before as this would require
    // that the visibility state of one of its parents has changed, which cannot happen during loading.
    System.Inc(FVisibleCount, CountVisibleChildren(Node));

  // Fix selection array.
  ClearTempCache;
  if Node = FRoot then
    Stop := nil
  else
    Stop := Node.NextSibling;

  if toMultiSelect in FOptions.SelectionOptions then
  begin
    // Add all nodes which were selected before to the current selection (unless they are already there).
    while Node <> Stop do
    begin
      if (vsSelected in Node.States) and not FindNodeInSelection(Node, Index, 0, High(FSelection)) then
        InternalCacheNode(Node);
      Node := GetNextNoInit(Node);
    end;
    if FTempNodeCount > 0 then
      AddToSelection(FTempNodeCache, FTempNodeCount, True);
    ClearTempCache;
  end
  else // No further selected nodes allowed so delete the corresponding flag in all new nodes.
    while Node <> Stop do
    begin
      Exclude(Node.States, vsSelected);
      Node := GetNextNoInit(Node);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.InternalAddToSelection(Node: PVirtualNode; ForceInsert: Boolean): Boolean;
var
  lSingletonNodeArray: TNodeArray;
begin
  Assert(Assigned(Node), 'Node must not be nil!');
  SetLength(lSingletonNodeArray, 1);
  lSingletonNodeArray[0] := Node;
  Result := InternalAddToSelection(lSingletonNodeArray, 1, ForceInsert);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.InternalAddToSelection(const NewItems: TNodeArray; NewLength: Integer;
  ForceInsert: Boolean): Boolean;

// Internal version of method AddToSelection which does not trigger OnChange events

var
  I, J: Integer;
  CurrentEnd: Integer;
  Constrained,
  SiblingConstrained: Boolean;
  lPreviousSelectedCount: Integer;
  AddedNodesSize: Integer;
  PTmpNode: PVirtualNode;

begin
  lPreviousSelectedCount := FSelectionCount;
  // The idea behind this code is to use a kind of reverse merge sort. QuickSort is quite fast
  // and would do the job here too but has a serious problem with already sorted lists like FSelection.

  // current number of valid entries
  AddedNodesSize := 0;

  // 1) Remove already selected items, mark all other as being selected.
  if ForceInsert then
  begin
    //Fix: For already selected node when selected, this path
    //is used that didn't contain the Constraint logic. Added.
    Constrained := toLevelSelectConstraint in FOptions.SelectionOptions;
    if Constrained and (FLastSelectionLevel = -1) then
      FLastSelectionLevel := GetNodeLevelForSelectConstraint(NewItems[0]);
    AddedNodesSize := NewLength;
  end
  else
  begin
    Constrained := toLevelSelectConstraint in FOptions.SelectionOptions;
    if Constrained and (FLastSelectionLevel = -1) then
      FLastSelectionLevel := GetNodeLevelForSelectConstraint(NewItems[0]);
    SiblingConstrained := toSiblingSelectConstraint in FOptions.SelectionOptions;
    if SiblingConstrained and (FRangeAnchor = nil) then
      FRangeAnchor := NewItems[0];

    for I := 0 to NewLength - 1 do
      if ([vsSelected, vsDisabled] * NewItems[I].States <> []) or
         (Constrained and (Cardinal(FLastSelectionLevel) <> GetNodeLevel(NewItems[I]))) or
         (SiblingConstrained and (FRangeAnchor.Parent <> NewItems[I].Parent))
      then
        System.Inc(PAnsiChar(NewItems[I])) // mark as invalid by setting the LSB
      else
        System.Inc(AddedNodesSize);
  end;

  I := PackArray(NewItems, NewLength);
  if I > -1 then
    NewLength := I;

  Result := NewLength > 0;
  if Result then
  begin
    // 2) Sort the new item list so we can easily traverse it.
    if NewLength > 1 then
      QuickSort(NewItems, 0, NewLength - 1);
    // 3) Make room in FSelection for the new items.
    if lPreviousSelectedCount + NewLength >= Length(FSelection) then
      SetLength(FSelection, lPreviousSelectedCount + NewLength);

    // 4) Merge in new items
    J := NewLength - 1;
    CurrentEnd := lPreviousSelectedCount - 1;

    while J >= 0 do
    begin
      // First insert all new entries which are greater than the greatest entry in the old list.
      // If the current end marker is < 0 then there's nothing more to move in the selection
      // array and only the remaining new items must be inserted.
      if CurrentEnd >= 0 then
      begin
        while (J >= 0) and (PAnsiChar(NewItems[J]) > PAnsiChar(FSelection[CurrentEnd])) do
        begin
          FSelection[CurrentEnd + J + 1] := NewItems[J];
          System.Dec(J);
        end;
        // early out if nothing more needs to be copied
        if J < 0 then
          Break;
      end
      else
      begin
        // insert remaining new entries at position 0
        System.Move(NewItems[0], FSelection[0], (J + 1) * SizeOf(Pointer));
        // nothing more to do so exit main loop
        Break;
      end;

      // find the last entry in the remaining selection list which is smaller then the largest
      // entry in the remaining new items list
      FindNodeInSelection(NewItems[J], I, 0, CurrentEnd);
      System.Dec(I);
      // move all entries which are greater than the greatest entry in the new items list up
      // so the remaining gap travels down to where new items must be inserted
      System.Move(FSelection[I + 1], FSelection[I + J + 2], (CurrentEnd - I) * SizeOf(Pointer));
      CurrentEnd := I;
    end;

    // update selection count
    System.Inc(FSelectionCount, AddedNodesSize);

    // post process added nodes
    // First set vsSelected flag for all newly selected nodes, then fire event
    for I := 0 to AddedNodesSize - 1 do
      Include(NewItems[I].States, vsSelected);

    for I := 0 to AddedNodesSize - 1 do
    begin
      PTmpNode := NewItems[I];
      // call on add event callbackevent
      DoAddToSelection(PTmpNode);
      if SyncCheckstateWithSelection[PTmpNode] then
        checkstate[PTmpNode] := csCheckedNormal;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InternalCacheNode(Node: PVirtualNode);

// Adds the given node to the temporary node cache (used when collecting possibly large amounts of nodes).

var
  Len: Cardinal;

begin
  Len := Length(FTempNodeCache);
  if FTempNodeCount = Len then
  begin
    if Len < 100 then
      Len := 100
    else
      Len := Len + Len div 10;
    SetLength(FTempNodeCache, Len);
  end;
  FTempNodeCache[FTempNodeCount] := Node;
  System.Inc(FTempNodeCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InternalClearSelection();

var
  Count: Integer;
  lNode: PVirtualNode;
begin
  // It is possible that there are invalid node references in the selection array
  // if the tree update is locked and changes in the structure were made.
  // Handle this potentially dangerous situation by packing the selection array explicitely.
  if IsUpdating then
  begin
    Count := PackArray(FSelection, FSelectionCount);
    if Count > -1 then
    begin
      FSelectionCount := Count;
      SetLength(FSelection, FSelectionCount);
    end;
  end;

  while FSelectionCount > 0 do
  begin
    System.Dec(FSelectionCount);
    lNode := FSelection[FSelectionCount];
    //sync path note: deselect when click on another or on outside area
    Exclude(lNode.States, vsSelected);
    if SyncCheckstateWithSelection[lNode] then
      CheckState[lNode] := csUncheckedNormal;
    DoRemoveFromSelection(lNode);
  end;
  ResetRangeAnchor;
  FSelection := nil;
  DoStateChange([], [tsClearPending]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InternalConnectNode(Node, Destination: PVirtualNode; Target: TBaseVirtualTree;
  Mode: TVTNodeAttachMode);

// Connects Node with Destination depending on Mode.
// No error checking takes place. Node as well as Destination must be valid. Node must never be a root node and
// Destination must not be a root node if Mode is amInsertBefore or amInsertAfter.

var
  Run: PVirtualNode;

begin
  // Keep in mind that the destination node might belong to another tree.
  with Target do
  begin
    case Mode of
      amInsertBefore:
        begin
          Node.SetPrevSibling(Destination.PrevSibling);
          Destination.SetPrevSibling(Node);
          Node.SetNextSibling(Destination);
          Node.SetParent(Destination.Parent);
          Node.SetIndex(Destination.Index);
          if Node.PrevSibling = nil then
            Node.Parent.SetFirstChild(Node)
          else
            Node.PrevSibling.SetNextSibling(Node);

          // reindex all following nodes
          Run := Destination;
          while Assigned(Run) do
          begin
            Run.SetIndex(Run.Index + 1);
            Run := Run.NextSibling;
          end;
        end;
      amInsertAfter:
        begin
          Node.SetNextSibling(Destination.NextSibling);
          Destination.SetNextSibling(Node);
          Node.SetPrevSibling(Destination);
          Node.SetParent(Destination.Parent);
          if Node.NextSibling = nil then
            Node.Parent.SetLastChild(Node)
          else
            Node.NextSibling.SetPrevSibling(Node);
          Node.SetIndex(Destination.Index);

          // reindex all following nodes
          Run := Node;
          while Assigned(Run) do
          begin
            Run.SetIndex(Run.Index + 1);
            Run := Run.NextSibling;
          end;
        end;
      amAddChildFirst:
        begin
          if Assigned(Destination.FirstChild) then
          begin
            // If there's a first child then there must also be a last child.
            Destination.FirstChild.SetPrevSibling(Node);
            Node.SetNextSibling(Destination.FirstChild);
            Destination.SetFirstChild(Node);
          end
          else
          begin
            // First child node at this location.
            Destination.SetFirstChild(Node);
            Destination.SetLastChild(Node);
            Node.SetNextSibling(nil);
          end;
          Node.SetPrevSibling(nil);
          Node.SetParent(Destination);
          Node.SetIndex(0);
          // reindex all following nodes
          Run := Node.NextSibling;
          while Assigned(Run) do
          begin
            Run.SetIndex(Run.Index + 1);
            Run := Run.NextSibling;
          end;
        end;
      amAddChildLast:
        begin
          if Assigned(Destination.LastChild) then
          begin
            // If there's a last child then there must also be a first child.
            Destination.LastChild.SetNextSibling(Node);
            Node.SetPrevSibling(Destination.LastChild);
            Destination.SetLastChild(Node);
          end
          else
          begin
            // first child node at this location
            Destination.SetFirstChild(Node);
            Destination.SetLastChild(Node);
            Node.SetPrevSibling(nil);
          end;
          Node.SetNextSibling(nil);
          Node.SetParent(Destination);
          if Assigned(Node.PrevSibling) then
            Node.SetIndex(Node.PrevSibling.Index + 1)
          else
            Node.SetIndex(0);
        end;
    else
      // amNoWhere: do nothing
    end;
    // Remove temporary states.
    Node.States := Node.States - [vsChecking, vsCutOrCopy, vsDeleting];

    if (Mode <> amNoWhere) then begin
      Node.Parent.SetChildCount(Node.Parent.ChildCount + 1);
      Include(Node.Parent.States, vsHasChildren);
      AdjustTotalCount(Node.Parent, Node.TotalCount, True);

      // Add the new node's height only if its parent is expanded.
      if (vsExpanded in Node.Parent.States) and (vsVisible in Node.States) then begin
        AdjustTotalHeight(Node.Parent, Node.TotalHeight, True);
        System.Inc(FVisibleCount, CountVisibleChildren(Node) + Cardinal(IfThen(IsEffectivelyVisible[Node], 1)));
      end;//if

      // Update the hidden children flag of the parent.
      if (Node.Parent <> FRoot) then
      begin
        // If we have added a visible node then simply remove the all-children-hidden flag.
        if IsEffectivelyVisible[Node] then
          Exclude(Node.Parent.States, vsAllChildrenHidden)
        else begin
          // If we have added an invisible node and this is the only child node then
          // make sure the all-children-hidden flag is in a determined state.
          // If there were child nodes before then no action is needed.
          if Node.Parent.ChildCount = 1 then
            Include(Node.Parent.States, vsAllChildrenHidden);
        end;//else
      end; //if Node.Parent <> FRoot
    end;//if Mode <> amNoWhere
  end;//With
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.InternalData(Node: PVirtualNode): Pointer;

begin
  Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InternalDisconnectNode(Node: PVirtualNode; KeepFocus: Boolean; Reindex: Boolean = True; ParentClearing: Boolean = False);

// Disconnects the given node from its parent and siblings. The node's pointer are not reset so they can still be used
// after return from this method (probably a very short time only!).
// If KeepFocus is True then the focused node is not reset. This is useful if the given node is reconnected to the tree
// immediately after return of this method and should stay being the focused node if it was it before.
// Note: Node must not be nil or the root node.

var
  Parent,
  Run: PVirtualNode;
  Index: Integer;
  AdjustHeight: Boolean;

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Node must neither be nil nor the root node.');

  if (Node = FFocusedNode) and not KeepFocus then
  begin
    DoFocusNode(nil, False);
    DoFocusChange(FFocusedNode, FFocusedColumn);
  end;

  if Node = FRangeAnchor then
    ResetRangeAnchor;

  // Update the hidden children flag of the parent.
  if (Node.Parent <> FRoot) and not (ParentClearing) then
    if FUpdateCount = 0 then
      DetermineHiddenChildrenFlag(Node.Parent)
    else
      Include(FStates, tsUpdateHiddenChildrenNeeded);

  if not (vsDeleting in Node.States) then
  begin
    // Some states are only temporary so take them out.
    Node.States := Node.States - [vsChecking];
    Parent := Node.Parent;
    Parent.SetChildCount(Parent.ChildCount - 1);
    AdjustHeight := (vsExpanded in Parent.States) and (vsVisible in Node.States);
    if Parent.ChildCount = 0 then
    begin
      Parent.States := Parent.States - [vsAllChildrenHidden, vsHasChildren];
      if (Parent <> FRoot) and (vsExpanded in Parent.States) then
        Exclude(Parent.States, vsExpanded);
    end;
    AdjustTotalCount(Parent, -Integer(Node.TotalCount), True);
    if AdjustHeight then
      AdjustTotalHeight(Parent, -Node.TotalHeight, True);
    if FullyVisible[Node] then
      System.Dec(FVisibleCount, CountVisibleChildren(Node) + Cardinal(IfThen(IsEffectivelyVisible[Node], 1)));

    if Assigned(Node.PrevSibling) then
      Node.PrevSibling.SetNextSibling(Node.NextSibling)
    else
      Parent.SetFirstChild(Node.NextSibling);

    if Assigned(Node.NextSibling) then
    begin
      Node.NextSibling.SetPrevSibling(Node.PrevSibling);
      // Reindex all following nodes.
      if Reindex then
      begin
        Run := Node.NextSibling;
        Index := Node.Index;
        while Assigned(Run) do
        begin
          Run.SetIndex(Index);
          System.Inc(Index);
          Run := Run.NextSibling;
        end;
      end;
    end
    else
      Parent.SetLastChild(Node.PrevSibling);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InternalRemoveFromSelection(Node: PVirtualNode);

// Special version to mark a node to be no longer in the current selection. PackArray must
// be used to remove finally those entries.

var
  Index: Integer;

begin
  // Because pointers are always DWORD aligned we can simply increment all those
  // which we want to have removed (see also PackArray) and still have the
  // order in the list preserved.
  if FindNodeInSelection(Node, Index, -1, -1) then
  begin
    //sync path note: deselect when overlapping drawselection is made
    Exclude(Node.States, vsSelected);
    if SyncCheckstateWithSelection[Node] then
      Node.CheckState := csUncheckedNormal; // Avoid using SetCheckState() as it handles toSyncCheckboxesWithSelection as well.
    System.Inc(PAnsiChar(FSelection[Index]));
    DoRemoveFromSelection(Node);
    Change(Node); // Calling Change() here fixes issue #1047
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InternalSetFocusedColumn(const index: TColumnIndex);
begin
  FFocusedColumn := index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InvalidateCache;

// Marks the cache as invalid.

begin
  DoStateChange([tsValidationNeeded], [tsUseCache]);
  //ChangeTreeStatesAsync([csValidationNeeded], [csUseCache]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.MarkCutCopyNodes;

// Sets the vsCutOrCopy style in every currently selected but not disabled node to indicate it is
// now part of a clipboard operation.

var
  Nodes: TNodeArray;
  I: Integer;

begin
  Nodes := nil;
  if FSelectionCount > 0 then
  begin
    // need the current selection sorted to exclude selected nodes which are children, grandchildren etc. of
    // already selected nodes
    Nodes := GetSortedSelection(False);
    for I := 0 to High(Nodes) do
      with Nodes[I]^ do
        if not (vsDisabled in States) then
          Include(States, vsCutOrCopy);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Loaded;

var
  LastRootCount: Cardinal;
  IsReadOnly: Boolean;

begin
  inherited;

  // Call RegisterDragDrop after all visual inheritance changes to MiscOptions have been applied.
  if not (csDesigning in ComponentState) and HandleAllocated and ((hoDrag in Header.Options) or (toAcceptOLEDrop in TreeOptions.MiscOptions)) then
    RegisterDragDrop(Handle, DragManager as IDropTarget);

  // If a root node count has been set during load of the tree then update its child structure now
  // as this hasn't been done yet in this case.
  if (tsNeedRootCountUpdate in FStates) and (FRoot.ChildCount > 0) then
  begin
    DoStateChange([], [tsNeedRootCountUpdate]);
    IsReadOnly := toReadOnly in FOptions.MiscOptions;
    FOptions.InternalSetMiscOptions(FOptions.MiscOptions - [toReadOnly]);
    LastRootCount := FRoot.ChildCount;
    FRoot.SetChildCount(0);
    BeginUpdate;
    SetChildCount(FRoot, LastRootCount);
    EndUpdate;
    if IsReadOnly then
      FOptions.InternalSetMiscOptions(FOptions.MiscOptions + [toReadOnly]);
  end;

  // Prevent the object inspector at design time from marking the header as being modified
  // when auto resize is enabled.
  Updating;
  try
    TVTHeaderCracker(FHeader).UpdateMainColumn;
    TVirtualTreeColumnsCracker(FHeader.Columns).FixPositions;
    if toAutoBidiColumnOrdering in FOptions.AutoOptions then
      TVirtualTreeColumnsCracker(FHeader.Columns).ReorderColumns(UseRightToLeftAlignment);
    // Because of the special recursion and update stopper when creating the window (or resizing it)
    // we have to manually trigger the auto size calculation here.
    if hsNeedScaling in FHeader.States then
      TVTHeaderCracker(FHeader).RescaleHeader
    else
      TVTHeaderCracker(FHeader).RecalculateHeader;
    if hoAutoResize in FHeader.Options then
      TVirtualTreeColumnsCracker(FHeader.Columns).AdjustAutoSize(InvalidColumn, True);
  finally
    Updated;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.MainColumnChanged;

begin
  DoCancelEdit;
  NotifyAccessibleEvent();
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.MouseMove(Shift: TShiftState; X, Y: TDimension);

var
  R: TRect;

begin
  if tsNodeHeightTrackPending in FStates then
  begin
    // Remove hint if shown currently.
    Application.CancelHint;

    // Stop wheel panning if active.
    StopWheelPanning;

    // Stop timers
    StopTimer(ExpandTimer);
    StopTimer(EditTimer);
    StopTimer(HeaderTimer);
    StopTimer(ScrollTimer);
    StopTimer(SearchTimer);
    FSearchBuffer := '';
    FLastSearchNode := nil;

    DoStateChange([tsNodeHeightTracking], [tsScrollPending, tsScrolling, tsEditPending, tsOLEDragPending, tsVCLDragPending,
      tsIncrementalSearching, tsNodeHeightTrackPending]);
  end;

  if tsDrawSelPending in FStates then
  begin
    // Remove current selection in case the user clicked somewhere in the window (but not a node)
    // and moved the mouse.
    if CalculateSelectionRect(X, Y) then
    begin
      InvalidateRect(@FNewSelRect, False);
      UpdateWindow();
      if (Abs(FNewSelRect.Right - FNewSelRect.Left) > Mouse.DragThreshold) or
         (Abs(FNewSelRect.Bottom - FNewSelRect.Top) > Mouse.DragThreshold) then
      begin
        if tsClearPending in FStates then
        begin
          DoStateChange([], [tsClearPending]);
          ClearSelection;
        end;
        DoStateChange([tsDrawSelecting], [tsDrawSelPending]);

        // Reset to main column for multiselection.
        FocusedColumn := FHeader.MainColumn;

        // The current rectangle may already include some node captions. Handle this.
        if HandleDrawSelection(X, Y) then
          InvalidateRect(nil, False);
      end;
    end;
  end
  else
  begin
    if tsNodeHeightTracking in FStates then
    begin
      // Handle height tracking.
      if DoNodeHeightTracking(FHeightTrackNode, FHeightTrackColumn, TVTHeaderCracker(FHeader).GetShiftState, FHeightTrackPoint, Point(X, Y)) then
      begin
        // Avoid negative (or zero) node heights.
        if FHeightTrackPoint.Y >= Y then
          Y := FHeightTrackPoint.Y + 1;
        SetNodeHeight(FHeightTrackNode, Y - FHeightTrackPoint.Y);
        UpdateWindow();
        Exit;
      end;
    end;

    // Really start dragging if the mouse has been moved more than the threshold.
    if (tsOLEDragPending in FStates) and
      (
       ((Abs(FLastClickPos.X - X) >= FDragThreshold) and (X > 0)) or  // Check >0 to fix issue #833
       ((Abs(FLastClickPos.Y - Y) >= FDragThreshold) and (Y > 0))
      )
    then
      DoDragging(FLastClickPos)
    else
    begin
      if CanAutoScroll then
        DoAutoScroll(X, Y);
      if tsPanning in FStates then
        AdjustPanningCursor(X, Y);
      if not IsMouseSelecting then
      begin
        HandleHotTrack(X, Y);
        inherited MouseMove(Shift, X, Y);
      end
      else
      begin
        // Handle draw selection if required, but don't do the work twice if the
        // auto scrolling code already cares about the selection.
        if not (tsScrolling in FStates) and CalculateSelectionRect(X, Y) then
        begin
          // If something in the selection changed then invalidate the entire
          // tree instead trying to figure out the display rects of all changed nodes.
          if HandleDrawSelection(X, Y) then
            InvalidateRect(nil, False)
          else
          begin
            UnionRect(R, OrderRect(FNewSelRect), OrderRect(FLastSelRect));
            OffsetRect(R, -FEffectiveOffsetX, FOffsetY);
            InvalidateRect(@R, False);
          end;
          UpdateWindow();
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Notification(AComponent: TComponent; Operation: TOperation);

begin
  if (AComponent <> Self) and (Operation = opRemove) then
  begin
    // Check for components linked to the tree.
    if AComponent = FImages then
    begin
      Images := nil;
      if not (csDestroying in ComponentState) then
        Invalidate;
    end
    else
      if AComponent = FStateImages then
      begin
        StateImages := nil;
        if not (csDestroying in ComponentState) then
          Invalidate;
      end
      else
        if AComponent = FCustomCheckImages then
        begin
          CustomCheckImages := nil;
          FCheckImageKind := ckSystemDefault;
          if not (csDestroying in ComponentState) then
            Invalidate;
        end
        else
          if AComponent = PopupMenu then
            PopupMenu := nil
          else
            // Check for components linked to the header.
            if Assigned(FHeader) then
            begin
              if AComponent = FHeader.Images then
                FHeader.Images := nil
              else
                if AComponent = FHeader.PopupMenu then
                  FHeader.PopupMenu := nil;
            end;
  end;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.OriginalWMNCPaint(DC: HDC);

// Unfortunately, the painting for the non-client area in TControl is not always correct and does also not consider
// existing clipping regions, so it has been modified here to take this into account.

const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);

var
  RC, RW: TRect;
  EdgeSize: Integer;
  Size: TSize;

begin
  if (BevelKind <> bkNone) or (BorderWidth > 0) then
  begin
    RC := Rect(0, 0, Width, Height);
    Size := GetBorderDimensions;
    InflateRect(RC, Size.cx, Size.cy);

    RW := RC;

    if BevelKind <> bkNone then
    begin
      DrawEdge(DC, RC, InnerStyles[BevelInner] or OuterStyles[BevelOuter], Byte(BevelEdges) or EdgeStyles[BevelKind] or
        Ctl3DStyles[Ctl3D]);

      EdgeSize := 0;
      if BevelInner <> bvNone then
        Inc(EdgeSize, BevelWidth);
      if BevelOuter <> bvNone then
        Inc(EdgeSize, BevelWidth);
      if beLeft in BevelEdges then
        Inc(RC.Left, EdgeSize);
      if beTop in BevelEdges then
        Inc(RC.Top, EdgeSize);
      if beRight in BevelEdges then
        Dec(RC.Right, EdgeSize);
      if beBottom in BevelEdges then
        Dec(RC.Bottom, EdgeSize);
    end;

    // Repaint only the part in the original clipping region and not yet drawn parts.
    IntersectClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);

    // Determine inner rectangle to exclude (RC corresponds then to the client area).
    InflateRect(RC, -Integer(BorderWidth), -Integer(BorderWidth));

    // Remove the inner rectangle.
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);

    // Erase parts not drawn.
    Brush.Color := FColors.BorderColor;
    Winapi.Windows.FillRect(DC, RW, Brush.Handle);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Paint;

// Window paint routine. Used when the tree window needs to be updated.

var
  Window: TRect;
  Target: TPoint;
  Temp: TDimension;
  Options: TVTInternalPaintOptions;
  RTLOffset: TDimension;

begin

  Options := [poBackground, poColumnColor, poDrawFocusRect, poDrawDropMark, poDrawSelection, poGridLines];
  if UseRightToLeftAlignment and FHeader.UseColumns then
    RTLOffset := ComputeRTLOffset(True)
  else
    RTLOffset := 0;

  // The update rect has already been filled in WMPaint, as it is the window's update rect, which gets
  // reset when BeginPaint is called (in the ancestor).
  // The difference to the DC's clipbox is that it is also valid with internal paint operations used
  // e.g. by the Explorer while dragging, but show window content while dragging is disabled.
  if not IsRectEmpty(FUpdateRect) then
  begin
    Temp := Header.Columns.GetVisibleFixedWidth;
    if Temp = 0 then
    begin
      Window := FUpdateRect;
      Target := Window.TopLeft;

      // The clipping rectangle is given in client coordinates of the window. We have to convert it into
      // a sliding window of the tree image.
      OffsetRect(Window, FEffectiveOffsetX - RTLOffset, -FOffsetY);
      PaintTree(Canvas, Window, Target, Options);
    end
    else
    begin
      // First part, fixed columns
      Window := ClientRect;
      Window.Right := Temp;
      Target := Window.TopLeft;

      OffsetRect(Window,  -RTLOffset, -FOffsetY);
      PaintTree(Canvas, Window, Target, Options);

      // Second part, other columns
      Window := GetClientRect;

      if Temp > Window.Right then
        Exit;

      Window.Left := Temp;
      Target := Window.TopLeft;

      OffsetRect(Window, FEffectiveOffsetX - RTLOffset, -FOffsetY);
      PaintTree(Canvas, Window, Target, Options);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.PaintCheckImage(Canvas: TCanvas; const ImageInfo: TVTImageInfo; Selected: Boolean);

var
  ForegroundColor: COLORREF;
  R: TRect;
  Details, lSizeDetails: TThemedElementDetails;
  lSize: TSize;
  Theme: HTHEME;
  lCheckImages: TCustomImageList;
begin
  with ImageInfo do
  begin
    if (tsUseThemes in FStates) and (FCheckImageKind = ckSystemDefault) then
    begin
      Details.Element := teButton;
      case Index of
        // ctRadioButton
        1 : Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
        2 : Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
        3 : Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
        4 : Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
        5 : Details := StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
        6 : Details := StyleServices.GetElementDetails(tbRadioButtonCheckedHot);
        7 : Details := StyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
        8 : Details := StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
       // ct(TriState)CheckBox
        9 : Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
       10 : Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
       11 : Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
       12 : Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
       13 : Details := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
       14 : Details := StyleServices.GetElementDetails(tbCheckBoxCheckedHot);
       15 : Details := StyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
       16 : Details := StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
       17 : Details := StyleServices.GetElementDetails(tbCheckBoxMixedNormal);
       18 : Details := StyleServices.GetElementDetails(tbCheckBoxMixedHot);
       19 : Details := StyleServices.GetElementDetails(tbCheckBoxMixedPressed);
       20 : Details := StyleServices.GetElementDetails(tbCheckBoxMixedDisabled);
       // ctButton
       ckButtonNormal: Details := StyleServices.GetElementDetails(tbPushButtonNormal);
       ckButtonHot: Details := StyleServices.GetElementDetails(tbPushButtonHot);
       ckButtonPressed: Details := StyleServices.GetElementDetails(tbPushButtonPressed);
       ckButtonDisabled: Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
      else
        Details := StyleServices.GetElementDetails(tbButtonRoot);
      end;
      if StyleServices.IsSystemStyle {and not (Index in [ckButtonNormal..ckButtonDisabled])} then
      begin
        Theme := OpenThemeData(Handle, 'BUTTON');
        GetThemePartSize(Theme, Canvas.Handle, Details.Part, Details.State, nil, TS_TRUE, lSize);
        if (Index in [ckButtonNormal..ckButtonDisabled]) then begin
           lSizeDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal); // Size of dropdown button should be based on size of checkboxes
           GetThemePartSize(Theme, Canvas.Handle, lSizeDetails.Part, lSizeDetails.State, nil, TS_TRUE, lSize);
          // dropdown buttons should be slightly larger than checkboxes, see issue #887
          lSize.cx := Round(lSize.cx * 1.15);
          lSize.cy := Round(lSize.cy * 1.1);
        end;
        R := Rect(XPos, YPos, XPos + lSize.cx, YPos + lSize.cy);
        if (Index in [ckButtonNormal..ckButtonDisabled]) then
          R.Offset(-1, 0); // Eliminate 1 pixel border around Windows themed button
        DrawThemeBackground(Theme, Canvas.Handle, Details.Part, Details.State, R, nil);
        CloseThemeData(Theme);
      end
      else
      begin
        if (Index in [ckButtonNormal..ckButtonDisabled]) or not StyleServices.GetElementSize(Canvas.Handle, Details, TElementSize.esActual, lSize{$IF CompilerVersion >= 34}, CurrentPPI{$IFEND}) then begin
          // radio buttons fail in RAD Studio 10 Seattle and lower, fallback to checkbox images. See issue #615
          if not StyleServices.GetElementSize(Canvas.Handle, StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal), TElementSize.esActual, lSize{$IF CompilerVersion >= 34}, CurrentPPI{$IFEND}) then
            lSize := TSize.Create(GetSystemMetrics(SM_CXMENUCHECK), GetSystemMetrics(SM_CYMENUCHECK));
        end;//if
        R := Rect(XPos, YPos, XPos + lSize.cx, YPos + lSize.cy);
        StyleServices.DrawElement(Canvas.Handle, Details, R {$IF CompilerVersion  >= 34}, nil, FCurrentPPI{$IFEND});
        Canvas.Refresh; // Every time you give a Canvas.Handle away to some other code you can't control you have to call Canvas.Refresh afterwards because the Canvas object and the HDC can be out of sync.
      end;
      if (Index in [ckButtonNormal..ckButtonDisabled]) then begin
        Canvas.Pen.Color := clGray;
        // These constants have been determined by test using various themes and dpi-scalings
        DrawArrow(Canvas, TScrollDirection.sdDown, Point(R.Left + Round(lSize.cx * 0.22), R.Top + Round(lSize.cy * 0.33)), Round(lSize.cx *0.28));
      end;//if
    end
    else begin
      if Assigned(FCheckImages) then
        lCheckImages := FCheckImages
      else
        lCheckImages := FCustomCheckImages;
      with lCheckImages do
      begin
        if Selected and not Ghosted then
        begin
          if Focused or (TVTPaintOption.toPopupMode in FOptions.PaintOptions) then
            ForegroundColor := ColorToRGB(FColors.FocusedSelectionColor)
          else
            ForegroundColor := ColorToRGB(FColors.UnfocusedSelectionColor);
        end
        else
          ForegroundColor := GetRGBColor(BlendColor);

          ImageList_DrawEx(Handle, Index, Canvas.Handle, XPos, YPos, 0, 0, GetRGBColor(BkColor), ForegroundColor,
            ILD_TRANSPARENT);
      end;
    end; //else
  end;
end;

//----------------------------------------------------------------------------------------------------------------------


procedure TBaseVirtualTree.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
const
  Style: array[TImageType] of Cardinal = (0, ILD_MASK);
var
  ExtraStyle: Cardinal;
  CutNode: Boolean;
  PaintFocused: Boolean;
  DrawEnabled: Boolean;
  CustomOverlayDrawing: Boolean; // False if the built-in overloay drawing of TImageList should be used, True if custom drawing should take place.
begin
  with PaintInfo do
  begin
    CutNode := (vsCutOrCopy in Node.States) and (tsCutPending in FStates);
    PaintFocused := Focused or (toGhostedIfUnfocused in FOptions.PaintOptions);

    // Since the overlay image must be specified together with the image to draw
    // it is meaningfull to retrieve it in advance.
    if DoOverlay then
      GetImageIndex(PaintInfo, ikOverlay, iiOverlay)
    else
      PaintInfo.ImageInfo[iiOverlay].Index := -1;

    DrawEnabled := not (vsDisabled in Node.States) and Enabled;
    with ImageInfo[ImageInfoIndex] do
    begin
       if (vsSelected in Node.States) and not(Ghosted or CutNode) then
      begin
        if PaintFocused or (toPopupMode in FOptions.PaintOptions) then
          Images.BlendColor := FColors.FocusedSelectionColor
        else
          Images.BlendColor := FColors.UnfocusedSelectionColor;
      end
      else
        Images.BlendColor := Color;

      ExtraStyle := ILD_TRANSPARENT;
      // If the user returned an index >= 15 then we cannot use the built-in overlay image drawing.
      // Instead we do it manually. Also of the image list of the normal and the overlay icon is different,
      // we can't use the built-in drawing. See issue #779.
      if (ImageInfo[iiOverlay].Index > -1) then begin
        CustomOverlayDrawing := (ImageInfo[iiOverlay].Index >= 15) or (ImageInfo[iiOverlay].Images <> ImageInfo[iiNormal].Images);
        if not CustomOverlayDrawing then
          ExtraStyle := ILD_TRANSPARENT or ILD_OVERLAYMASK and IndexToOverlayMask(ImageInfo[iiOverlay].Index + 1);
      end
      else
        CustomOverlayDrawing := False;

      // Blend image if enabled and the tree has the focus (or ghosted images must be drawn also if unfocused) ...
      if (toUseBlendedImages in FOptions.PaintOptions) and PaintFocused
        // ... and the image is ghosted...
        and (Ghosted or
        // ... or it is not the check image and the node is selected (but selection is not for the entire row)...
        ((vsSelected in Node.States) and
        not (toFullRowSelect in FOptions.SelectionOptions) and
        not (toGridExtensions in FOptions.MiscOptions)) or
        // ... or the node must be shown in cut mode.
        CutNode) then
        ExtraStyle := ExtraStyle or ILD_BLEND50;

      if (vsSelected in Node.States) and not Ghosted then
        Images.BlendColor := clDefault;

      DrawImage(Images, Index, Canvas, XPos, YPos, Style[Images.ImageType] or ExtraStyle, DrawEnabled);

      // Now, draw the overlay. This circumnavigates limitations in the overlay mask index (it has to be 4 bits in size,
      // anything larger will be truncated by the ILD_OVERLAYMASK).
      // However this will only be done if the overlay image index is > 15, to avoid breaking code that relies
      // on overlay image indices (e.g. when using system image lists).
      if CustomOverlayDrawing then begin
        ExtraStyle := ExtraStyle and not ILD_BLEND50; // Fixes issue #551
        // Note: XPos and YPos are those of the normal images.
        DrawImage(ImageInfo[iiOverlay].Images, ImageInfo[iiOverlay].Index, Canvas, XPos, YPos,
          Style[ImageInfo[iiOverlay].Images.ImageType] or ExtraStyle, DrawEnabled);
      end;//if
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.PaintNodeButton(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const R: TRect;
  ButtonX, ButtonY: TDimension; BidiMode: TBiDiMode);

var
  Bitmap: TBitmap;
  XPos: TDimension;
  IsHot: Boolean;
  IsSelected : boolean;
  Theme: HTHEME;
  Glyph: Integer;
  State: Integer;
  Pos: TRect;

begin
  IsHot := (FCurrentHotNode = Node) and FHotNodeButtonHit;
  IsSelected := (vsSelected in Node.States);

  // Draw the node's plus/minus button according to the directionality.
    if BidiMode = bdLeftToRight then
    XPos := R.Left + ButtonX
  else
    XPos := R.Right - ButtonX - FPlusBM.Width;

  if (tsUseExplorerTheme in FStates) and not VclStyleEnabled then
  begin
    Glyph := IfThen(IsHot, TVP_HOTGLYPH, TVP_GLYPH);
    State := IfThen(vsExpanded in Node.States, GLPS_OPENED, GLPS_CLOSED);
    Pos := Rect(XPos, R.Top + ButtonY, XPos + FPlusBM.Width, R.Top + ButtonY + FPlusBM.Height);
    Theme := OpenThemeData(Handle, 'TREEVIEW');
    DrawThemeBackground(Theme, Canvas.Handle, Glyph, State, Pos, nil);
    CloseThemeData(Theme);
  end
  else
  begin
    if vsExpanded in Node.States then
    begin
      if IsHot then
      begin
        if IsSelected then
          BitMap := FSelectedHotMinusBM
        else
          Bitmap := FHotMinusBM;
      end
      else
        Bitmap := FMinusBM;
    end
    else
    begin
      if IsHot then
      begin
        if IsSelected then
          BitMap := FSelectedHotPlusBM
        else
          Bitmap := FHotPlusBM;
      end
      else
        Bitmap := FPlusBM;
    end;
    // Need to draw this masked.
    Canvas.Draw(XPos, R.Top + ButtonY, Bitmap);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.PaintTreeLines(const PaintInfo: TVTPaintInfo; IndentSize: TDimension; const LineImage: TLineImage);

var
  I: Integer;
  XPos,
  Offset: TDimension;
  NewStyles: TLineImage;

begin
  NewStyles := nil;

  with PaintInfo do
  begin
    if BidiMode = bdLeftToRight then
    begin
      XPos := CellRect.Left + PaintInfo.Offsets[ofsMargin];
      Offset := FIndent;
    end
    else
    begin
      Offset := -FIndent;
      XPos := CellRect.Right - PaintInfo.Offsets[ofsMargin] + Offset;
    end;

    case FLineMode of
      lmBands:
        if poGridLines in PaintInfo.PaintOptions then
        begin
          // Convert the line images in correct bands.
          SetLength(NewStyles, Length(LineImage));
          for I := IndentSize - 1 downto 0 do
          begin
            if (vsExpanded in Node.States) and not (vsAllChildrenHidden in Node.States) then
              NewStyles[I] := ltLeft
            else
              case LineImage[I] of
                ltRight,
                ltBottomRight,
                ltTopDownRight,
                ltTopRight:
                  NewStyles[I] := ltLeftBottom;
                ltNone:
                  // Have to take over the image to the right of this one. A no line entry can never appear as
                  // last entry so I don't need an end check here.
                  if LineImage[I + 1] in [ltNone, ltTopRight] then
                    NewStyles[I] := NewStyles[I + 1]
                  else
                    NewStyles[I] := ltLeft;
                ltTopDown:
                  // Have to check the image to the right of this one. A top down line can never appear as
                  // last entry so I don't need an end check here.
                  if LineImage[I + 1] in [ltNone, ltTopRight] then
                    NewStyles[I] := NewStyles[I + 1]
                  else
                    NewStyles[I] := ltLeft;
              end;
          end;

          PaintInfo.Canvas.Font.Color := FColors.GridLineColor;
          for I := 0 to IndentSize - 1 do
          begin
            DoBeforeDrawLineImage(PaintInfo.Node, I + Ord(not (toShowRoot in TreeOptions.PaintOptions)), XPos);
            DrawLineImage(PaintInfo, XPos, CellRect.Top, NodeHeight[Node] - 1, VAlign - 1, NewStyles[I],
              BidiMode <> bdLeftToRight);
            Inc(XPos, Offset);
          end;
        end;
    else // lmNormal
      PaintInfo.Canvas.Font.Color := FColors.TreeLineColor;
      for I := 0 to IndentSize - 1 do
      begin
        DoBeforeDrawLineImage(PaintInfo.Node, I + Ord(not (toShowRoot in TreeOptions.PaintOptions)), XPos);
        DrawLineImage(PaintInfo, XPos, CellRect.Top, NodeHeight[Node], VAlign - 1, LineImage[I],
          BidiMode <> bdLeftToRight);
        Inc(XPos, Offset);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.PaintSelectionRectangle(Target: TCanvas; WindowOrgX: TDimension; const SelectionRect: TRect;
  TargetRect: TRect);

// Helper routine to draw a selection rectangle in the mode determined by DrawSelectionMode.

var
  BlendRect: TRect;
  TextColorBackup,
  BackColorBackup: COLORREF;   // used to restore forground and background colors when drawing a selection rectangle

begin
  if ((FDrawSelectionMode = smDottedRectangle) and not (tsUseThemes in FStates)) then
  begin
    // Classical selection rectangle using dotted borderlines.
    TextColorBackup := GetTextColor(Target.Handle);
    SetTextColor(Target.Handle, $FFFFFF);
    BackColorBackup := GetBkColor(Target.Handle);
    SetBkColor(Target.Handle, 0);
    Target.DrawFocusRect(SelectionRect);
    SetTextColor(Target.Handle, TextColorBackup);
    SetBkColor(Target.Handle, BackColorBackup);
  end
  else
  begin
    // Modern alpha blended style.
    OffsetRect(TargetRect, WindowOrgX, 0);
    if IntersectRect(BlendRect, OrderRect(SelectionRect), TargetRect) then
    begin
      OffsetRect(BlendRect, -WindowOrgX, 0);
      AlphaBlend(0, Target.Handle, BlendRect, Point(0, 0), bmConstantAlphaAndColor, FSelectionBlendFactor,
        ColorToRGB(FColors.SelectionRectangleBlendColor));

      Target.Brush.Color := FColors.SelectionRectangleBorderColor;
      Target.FrameRect(SelectionRect);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.PrepareCell(var PaintInfo: TVTPaintInfo; WindowOrgX, MaxWidth: TDimension);

// This method is called immediately before a cell's content is drawn und is responsible to paint selection colors etc.

var
  TextColorBackup,
  BackColorBackup: COLORREF;
  FocusRect,
  InnerRect: TRect;
  RowRect: TRect;
  Theme: HTHEME;
const
  TREIS_HOTSELECTED = 6;

  //--------------- local functions -------------------------------------------

  procedure AlphaBlendSelection(Color: TColor);

  var
    R: TRect;

  begin
    // Take into account any window offset and size limitations in the target bitmap, as this is only as large
    // as necessary and might not cover the whole node. For normal painting this does not matter (because of
    // clipping) but for the MMX code there is no such check and it will crash badly when bitmap boundaries are
    // crossed.
    R := InnerRect;
    OffsetRect(R, -WindowOrgX, 0);
    if R.Left < 0 then
      R.Left := 0;
    if R.Right > MaxWidth then
      R.Right := MaxWidth;
    AlphaBlend(0, PaintInfo.Canvas.Handle, R, Point(0, 0), bmConstantAlphaAndColor,
      FSelectionBlendFactor, ColorToRGB(Color));
  end;

  //---------------------------------------------------------------------------

  procedure DrawBackground(State: Integer);
  begin
    // if the full row selection is disabled or toGridExtensions is in the MiscOptions, draw the selection
    // into the InnerRect, otherwise into the RowRect
    if not (toFullRowSelect in FOptions.SelectionOptions) or (toGridExtensions in FOptions.MiscOptions) then
      DrawThemeBackground(Theme, PaintInfo.Canvas.Handle, TVP_TREEITEM, State, InnerRect, nil)
    else
      DrawThemeBackground(Theme, PaintInfo.Canvas.Handle, TVP_TREEITEM, State, RowRect, nil);
  end;

  procedure DrawThemedFocusRect(State: Integer);
  var
    Theme: HTHEME;
  begin
    Theme := OpenThemeData(Application.ActiveFormHandle, 'Explorer::ItemsView');
    if not (toFullRowSelect in FOptions.SelectionOptions) or (toGridExtensions in FOptions.MiscOptions) then
      DrawThemeBackground(Theme, PaintInfo.Canvas.Handle, LVP_LISTDETAIL, State, InnerRect, nil)
    else
      DrawThemeBackground(Theme, PaintInfo.Canvas.Handle, LVP_LISTDETAIL, State, RowRect, nil);
    CloseThemeData(Theme);
  end;

  //--------------- end local functions ---------------------------------------

begin
  if tsUseExplorerTheme in FStates then
  begin
    Theme := OpenThemeData(Application.ActiveFormHandle, 'Explorer::TreeView');
    RowRect := Rect(0, PaintInfo.CellRect.Top, FRangeX, PaintInfo.CellRect.Bottom);
    if (Header.Columns.Count = 0) and (toFullRowSelect in TreeOptions.SelectionOptions) then
      RowRect.Right := Max(ClientWidth, RowRect.Right);
    if toShowVertGridLines in FOptions.PaintOptions then
      Dec(RowRect.Right);
  end;

  with PaintInfo, Canvas do
  begin
    // Fill cell background if its color differs from tree background.
    with FHeader.Columns do
    if poColumnColor in PaintOptions then
    begin
      Brush.Color := Items[Column].GetEffectiveColor;
      FillRect(CellRect);
     end;

    // Let the application customize the cell background and the content rectangle.
    DoBeforeCellPaint(Canvas, Node, Column, cpmPaint, CellRect, ContentRect);

    InnerRect := ContentRect;

    // The selection rectangle depends on alignment.
    if not (toGridExtensions in FOptions.MiscOptions) then
    begin
      case Alignment of
        taLeftJustify:
          if InnerRect.Left + NodeWidth < InnerRect.Right then
            InnerRect.Right := InnerRect.Left + NodeWidth;
        taCenter:
          if (InnerRect.Right - InnerRect.Left) > NodeWidth then
          begin
            InnerRect.Left := Divide(InnerRect.Left + InnerRect.Right - NodeWidth, 2);
            InnerRect.Right := InnerRect.Left + NodeWidth;
          end;
        taRightJustify:
          if (InnerRect.Right - InnerRect.Left) > NodeWidth then
            InnerRect.Left := InnerRect.Right - NodeWidth;
      end;
    end;

    if (Column = FFocusedColumn) or (toFullRowSelect in FOptions.SelectionOptions) then
    begin
      // Fill the selection rectangle.
      if poDrawSelection in PaintOptions then
      begin
        if Node = FDropTargetNode then
        begin
          if (FLastDropMode = dmOnNode) or (vsSelected in Node.States) then
          begin
            Brush.Color := FColors.DropTargetColor;
            Pen.Color := FColors.DropTargetBorderColor;

            if (toGridExtensions in FOptions.MiscOptions) or
              (toFullRowSelect in FOptions.SelectionOptions) then
              InnerRect := CellRect;
            if not IsRectEmpty(InnerRect) then
              if tsUseExplorerTheme in FStates then
                DrawBackground(TREIS_SELECTED)
              else
                if (toUseBlendedSelection in FOptions.PaintOptions) then
                  AlphaBlendSelection(Brush.Color)
                else
                  RoundRect(InnerRect.Left, InnerRect.Top, InnerRect.Right, InnerRect.Bottom, FSelectionCurveRadius, FSelectionCurveRadius);
          end
          else
          begin
            Brush.Style := bsClear;
          end;
        end
        else
          if vsSelected in Node.States then
          begin
             if Focused or (toPopupMode in FOptions.PaintOptions) then
             begin
              Brush.Color := FColors.FocusedSelectionColor;
              Pen.Color := FColors.FocusedSelectionBorderColor;
            end
            else
            begin
              Brush.Color := FColors.UnfocusedSelectionColor;
              Pen.Color := FColors.UnfocusedSelectionBorderColor;
            end;
            if (toGridExtensions in FOptions.MiscOptions) or (toFullRowSelect in FOptions.SelectionOptions) then
              InnerRect := CellRect;
            if not IsRectEmpty(InnerRect) then
              if tsUseExplorerTheme in FStates then
              begin
                // If the node is also hot, its background will be drawn later.
                if not (toHotTrack in FOptions.PaintOptions) or (Node <> FCurrentHotNode) or
                   ((Column <> FCurrentHotColumn) and not (toFullRowSelect in FOptions.SelectionOptions)) then
                  DrawBackground(IfThen(Self.Focused, TREIS_SELECTED, TREIS_SELECTEDNOTFOCUS));
              end
              else
                if (toUseBlendedSelection in FOptions.PaintOptions) then
                  AlphaBlendSelection(Brush.Color)
                else
                  RoundRect(InnerRect.Left, InnerRect.Top, InnerRect.Right, InnerRect.Bottom, FSelectionCurveRadius, FSelectionCurveRadius);
          end;
      end;
    end;

    if (tsUseExplorerTheme in FStates) and (toHotTrack in FOptions.PaintOptions) and (Node = FCurrentHotNode) and
       ((Column = FCurrentHotColumn) or (toFullRowSelect in FOptions.SelectionOptions)) then
      DrawBackground(IfThen((vsSelected in Node.States) and not (toAlwaysHideSelection in FOptions.PaintOptions),
                            TREIS_HOTSELECTED, TREIS_HOT));

    if (Column = FFocusedColumn) or (toFullRowSelect in FOptions.SelectionOptions) then
    begin
      // draw focus rect
      if (poDrawFocusRect in PaintOptions) and
         (Focused or (toPopupMode in FOptions.PaintOptions)) and (FFocusedNode = Node) and
         ( (Column = FFocusedColumn) or
             (not (toExtendedFocus in FOptions.SelectionOptions) and
             (toFullRowSelect in FOptions.SelectionOptions) and
             (tsUseExplorerTheme in FStates) ) ) then
      begin
        TextColorBackup := GetTextColor(Handle);
        SetTextColor(Handle, $FFFFFF);
        BackColorBackup := GetBkColor(Handle);
        SetBkColor(Handle, 0);

        if not (toExtendedFocus in FOptions.SelectionOptions) and (toFullRowSelect in FOptions.SelectionOptions) and
          (tsUseExplorerTheme in FStates) then
          FocusRect := RowRect
        else
          if toGridExtensions in FOptions.MiscOptions then
            FocusRect := CellRect
          else
            FocusRect := InnerRect;

        if tsUseExplorerTheme in FStates then
          InflateRect(FocusRect, -1, -1);

        if (tsUseExplorerTheme in FStates) then
        begin
          //Draw focused unselected style like Windows 7 Explorer
          if not (vsSelected in Node.States) then
            DrawThemedFocusRect(LIS_NORMAL)
          else
            DrawBackground(TREIS_HOTSELECTED);
        end
        else
          Winapi.Windows.DrawFocusRect(Handle, FocusRect);
        SetTextColor(Handle, TextColorBackup);
        SetBkColor(Handle, BackColorBackup);
      end;
    end;
  end;

  if tsUseExplorerTheme in FStates then
    CloseThemeData(Theme);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TBaseVirtualTree.RaiseVTError(const Msg: string; HelpContext: Integer);

begin
  raise EVirtualTreeError.CreateHelp(Msg, HelpContext);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ReadChunk(Stream: TStream; Version: Integer; Node: PVirtualNode; ChunkType,
  ChunkSize: Integer): Boolean;

// Called while loading a tree structure, Node is already valid (allocated) at this point.
// The function handles the base and user chunks, any other chunk is marked as being unknown (result becomes False)
// and skipped. descendants may handle them by overriding this method.
// Returns True if the chunk could be handled, otherwise False.
type
  TAdvancedVersion2Identifier = packed record
    ChildCount: Cardinal;
    NodeHeight: TDimension;
    States: Word;
    Align: Byte;
    CheckState: TCheckState;
    CheckType: TCheckType;
    Reserved: Cardinal;
  end;

var
  IdBody: TAdvancedVersion2Identifier;
  ChunkBody: TBaseChunkBody;
  Run: PVirtualNode;
  LastPosition: Integer;

begin
  case ChunkType of
    BaseChunk:
      begin
        // Load base chunk's body (chunk header has already been consumed).
        case Version of
          1:
            begin
              with ChunkBody do
              begin
                // In version prior to 2 there was a smaller chunk body. Hence we have to read it entry by entry now.
                Stream.Read(ChildCount, SizeOf(ChildCount));
                Stream.Read(NodeHeight, SizeOf(NodeHeight));
                // TVirtualNodeStates was a byte sized type in version 1.
                States := [];
                Stream.Read(States, SizeOf(Byte));
                // vsVisible is now in the place where vsSelected was before, but every node was visible in the old version
                // so we need to fix this too.
                if vsVisible in States then
                  //sync path note: prior version stream reading, ignored for syncing
                  Include(States, vsSelected)
                else
                  Include(States, vsVisible);
                Stream.Read(Align, SizeOf(Align));
                Stream.Read(CheckState, SizeOf(CheckState));
                Stream.Read(CheckType, SizeOf(CheckType));
              end;
            end;
          2:
            begin
              ZeroMemory(@IdBody, SizeOf(IdBody));
              Stream.Read(IdBody, SizeOf(IdBody));
              // If Align is greater than zero, we have a stream prior to VT version 6.2
              if IdBody.Align > 0 then
                with ChunkBody do
                begin
                  ChildCount := IdBody.ChildCount;
                  NodeHeight := IdBody.NodeHeight;
                  States := [];
                  System.Move(IdBody.States, States, SizeOf(IdBody.States));
                  CheckState := IdBody.CheckState;
                  CheckType := IdBody.CheckType;
                  Reserved := IdBody.Reserved;
                end
              else
                begin
                  // Stream is compatible with current size of TBaseChunkBody
                  Stream.Position := Stream.Position - SizeOf(IdBody);
                  Stream.Read(ChunkBody, SizeOf(ChunkBody));
                end;
            end;
          3:
            Stream.Read(ChunkBody, SizeOf(ChunkBody));
        end;

        with Node^ do
        begin
          // Set states first, in case the node is invisible.
          States := ChunkBody.States;
          SetNodeHeight(ChunkBody.NodeHeight);
          TotalHeight := NodeHeight;
          Align := ChunkBody.Align;
          CheckState := ChunkBody.CheckState;
          CheckType := ChunkBody.CheckType;
          SetChildCount(ChunkBody.ChildCount);

          // Create and read child nodes.
          while ChunkBody.ChildCount > 0 do
          begin
            Run := MakeNewNode;

            Run.SetPrevSibling(Node.LastChild);
            if Assigned(Run.PrevSibling) then
              Run.SetIndex(Run.PrevSibling.Index + 1);
            if Assigned(Node.LastChild) then
              Node.LastChild.SetNextSibling(Run)
            else
              Node.SetFirstChild(Run);
            Node.SetLastChild(Run);
            Run.SetParent(Node);

            ReadNode(Stream, Version, Run);
            System.Dec(ChunkBody.ChildCount);
          end;
        end;
        Result := True;
      end;
    UserChunk:
      if ChunkSize > 0 then
      begin
        // need to know whether the data was read
        LastPosition := Stream.Position;
        DoLoadUserData(Node, Stream);
        // compare stream position to learn whether the data was read
        Result := Stream.Position > LastPosition;
        // Improve stability by advancing the stream to the chunk's real end if
        // the application did not read what has been written.
        if not Result or (Stream.Position <> (LastPosition + ChunkSize)) then
          Stream.Position := LastPosition + ChunkSize;
      end
      else
        Result := True;
  else
    // unknown chunk, skip it
    Stream.Position := Stream.Position + ChunkSize;
    Result := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ReadNode(Stream: TStream; Version: Integer; Node: PVirtualNode);

// Reads the anchor chunk of each node and initiates reading the sub chunks for this node

var
  Header: TChunkHeader;
  EndPosition: Integer;

begin
  with Stream do
  begin
    // Read anchor chunk of the node.
    Stream.Read(Header, SizeOf(Header));
    if Header.ChunkType = NodeChunk then
    begin
      EndPosition := Stream.Position + Header.ChunkSize;
      // Read all subchunks until the indicated chunk end position is reached in the stream.
      while Position < EndPosition do
      begin
        // Read new chunk header.
        Stream.Read(Header, SizeOf(Header));
        ReadChunk(Stream, Version, Node, Header.ChunkType, Header.ChunkSize);
      end;
      // If the last chunk does not end at the given end position then there is something wrong.
      if Position <> EndPosition then
        RaiseVTError(SCorruptStream2, hcTFCorruptStream2);
    end
    else
      RaiseVTError(SCorruptStream1, hcTFCorruptStream1);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.RedirectFontChangeEvent(Canvas: TCanvas);

begin
  if @Canvas.Font.OnChange <> @FOldFontChange then
  begin
    FOldFontChange := Canvas.Font.OnChange;
    Canvas.Font.OnChange := FontChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.RemoveFromSelection(Node: PVirtualNode);

var
  Index: Integer;

begin
  if not FSelectionLocked then
  begin
    Assert(Assigned(Node), 'Node must not be nil!');
    Assert(GetCurrentThreadId = MainThreadId, Self.Classname + '.RemoveFromSelection() must only be called from UI thread.');
    if vsSelected in Node.States then
    begin
      Assert(FSelectionCount > 0, 'if one node has set the vsSelected flag, SelectionCount must be >0.');
      //sync path note: deselect when a ctrl click removes a selection
      Exclude(Node.States, vsSelected);
      if SyncCheckstateWithSelection[Node] then
        Node.CheckState := csUncheckedNormal; // Avoid using SetCheckState() as it handles toSyncCheckboxesWithSelection as well.

      if FindNodeInSelection(Node, Index, -1, -1) and (Index < FSelectionCount - 1) then
        System.Move(FSelection[Index + 1], FSelection[Index], (FSelectionCount - Index - 1) * SizeOf(Pointer));
      if FSelectionCount > 0 then
        System.Dec(FSelectionCount);
      SetLength(FSelection, FSelectionCount);

      if FSelectionCount = 0 then
        ResetRangeAnchor;

      if FSelectionCount <= 1 then
        UpdateNextNodeToSelect(Node);

      DoRemoveFromSelection(Node);
      InvalidateNode(Node);
      Change(Node);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateNextNodeToSelect(Node: PVirtualNode);

// save a potential node to select after the currently selected node will be deleted.
// This will make the VT to behave more like the Win32 TreeView, which always selecta a new node if the currently
// selected one gets deleted.

begin
  if ([toAlwaysSelectNode, toSelectNextNodeOnRemoval] * TreeOptions.SelectionOptions) = [] then
    Exit;
  if GetNextSibling(Node) <> nil then
    FNextNodeToSelect := GetNextSibling(Node)
  else if GetPreviousSibling(Node) <> nil then
    FNextNodeToSelect := GetPreviousSibling(Node)
  else if Node.Parent <> FRoot then
    FNextNodeToSelect := Node.Parent
  else
    FNextNodeToSelect := nil;
end;//if Assigned(Node);

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ResetRangeAnchor;

// Called when there is no selected node anymore and the selection range anchor needs a new value.

begin
  FRangeAnchor := FFocusedNode;
  FLastSelectionLevel := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.RestoreFontChangeEvent(Canvas: TCanvas);

begin
  Canvas.Font.OnChange := FOldFontChange;
  FOldFontChange := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SelectNodes(StartNode, EndNode: PVirtualNode; AddOnly: Boolean);

// Selects a range of nodes and unselects all other eventually selected nodes which are not in this range if
// AddOnly is False.
// EndNode must be visible while StartNode does not necessarily as in the case where the last focused node is the start
// node but it is a child of a node which has been collapsed previously. In this case the first visible parent node
// is used as start node. StartNode can be nil in which case the very first node in the tree is used.

var
  NodeFrom,
  NodeTo,
  LastAnchor: PVirtualNode;
  Index: Integer;

begin
  Assert(Assigned(EndNode), 'EndNode must not be nil!');
  if not FSelectionLocked then
  begin
    ClearTempCache;
    if StartNode = nil then
      StartNode := GetFirstVisibleNoInit(nil, True)
    else
      if not FullyVisible[StartNode] then
      begin
        StartNode := GetPreviousVisible(StartNode, True);
        if StartNode = nil then
          StartNode := GetFirstVisibleNoInit(nil, True);
      end;

    if CompareNodePositions(StartNode, EndNode, True) < 0 then
    begin
      NodeFrom := StartNode;
      NodeTo := EndNode;
    end
    else
    begin
      NodeFrom := EndNode;
      NodeTo := StartNode;
    end;

    // The range anchor will be reset by the following call.
    LastAnchor := FRangeAnchor;
    if not AddOnly then
      InternalClearSelection;

    while NodeFrom <> NodeTo do
    begin
      InternalCacheNode(NodeFrom);
      NodeFrom := GetNextVisible(NodeFrom, True);
    end;
    // select last node too
    InternalCacheNode(NodeFrom);
    // now add them all in "one" step
    AddToSelection(FTempNodeCache, FTempNodeCount);
    ClearTempCache;
    if Assigned(LastAnchor) and FindNodeInSelection(LastAnchor, Index, -1, -1) then
     FRangeAnchor := LastAnchor;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SetFocusedNodeAndColumn(Node: PVirtualNode; Column: TColumnIndex);

var
  OldColumn: TColumnIndex;
  WasDifferent: Boolean;

begin
  if not FHeader.AllowFocus(Column) then
    Column := FFocusedColumn;

  WasDifferent := (Node <> FFocusedNode) or (Column <> FFocusedColumn);

  OldColumn := FFocusedColumn;
  FFocusedColumn := Column;

  DoFocusNode(Node, True);

  // Check if the change was accepted.
  if FFocusedNode = Node then
  begin
    CancelEditNode;
    if WasDifferent then
      DoFocusChange(FFocusedNode, FFocusedColumn);
  end
  else
    // If the user did not accept the new cell to focus then set also the focused column back
    // to its original state.
    FFocusedColumn := OldColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SkipNode(Stream: TStream);

// Skips the data for the next node in the given stream (including the child nodes).

var
  Header: TChunkHeader;

begin
  with Stream do
  begin
    // read achor chunk of the node
    Stream.Read(Header, SizeOf(Header));
    if Header.ChunkType = NodeChunk then
      Stream.Position := Stream.Position + Header.ChunkSize
    else
      RaiseVTError(SCorruptStream1, hcTFCorruptStream1);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.StartWheelPanning(Position: TPoint);

// Called when wheel panning should start. A little helper window is created to indicate the reference position,
// which determines in which direction and how far wheel panning/scrolling will happen.

  //--------------- local function --------------------------------------------
  function CreatePanningWindow(const ImageName: TPanningCursor; const Pos: TPoint): TForm;
  var
    Form: TForm;
    Image: TImage;
    PanningImage: TIcon;
  begin
    Form := TForm.Create(Self);
    Form.PopupMode := pmExplicit;
    Form.PopupParent := GetParentForm(Self);
    Form.TransparentColor := True;
    Form.TransparentColorValue := clBtnFace;
    Form.Width := ScaledPixels(32);
    Form.Height := Form.Width;
    Form.BorderStyle := bsNone;
    Form.StyleElements := [];
    Image := TImage.Create(Form);
    Image.Left := 0;
    Image.Top := 0;
    Image.Parent := Form;
    Image.Align := TAlign.alClient;

    PanningImage := TIcon.Create;
    try
      PanningImage.Handle := LoadImage(0, MAKEINTRESOURCE(ImageName), IMAGE_CURSOR, Form.Width, Form.Height, LR_DEFAULTCOLOR or LR_LOADTRANSPARENT);
      Image.Picture.Assign(PanningImage);
      Form.Left := Pos.X - (PanningImage.Width div 2);
      Form.Top := Pos.Y - (PanningImage.Height div 2);
    finally
      PanningImage.Free;
    end;
    Form.Position := poDesigned;
    // This prevents a focus chnage compare to using TForm.Show()
    ShowWindow(Form.Handle, SW_SHOWNOACTIVATE);
    Form.Visible := True;
    Exit(Form);
  end;
  //--------------- end local function ----------------------------------------

var
  ImageName: TPanningCursor;
  Pt: TPoint;

begin
  StopTimer(ScrollTimer);
  DoStateChange([tsPanning]);

  // Determine correct cursor
  if FRangeX > ClientWidth then
  begin
    if FRangeY > ClientHeight then
      ImageName := TPanningCursor.MOVEALL
    else
      ImageName := TPanningCursor.MOVEEW;
  end
  else
    ImageName := TPanningCursor.MOVENS;

  // Create the helper window and show it at the given position without activating it.
  Pt := ClientToScreen(Position);
  FPanningWindow := CreatePanningWindow(ImageName, Pt);

  // Setup the panscroll timer and capture all mouse input.
  TrySetFocus();
  SetCapture(Handle);
  SetTimer(Handle, ScrollTimer, 20, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.StopWheelPanning;

// Stops panning if currently active and destroys the helper window.

begin
  if tsPanning in FStates then
  begin
    // Release the mouse capture and stop the panscroll timer.
    StopTimer(ScrollTimer);
    ReleaseCapture;
    DoStateChange([], [tsPanning]);

    // Destroy the helper window.
    if Assigned(FPanningWindow) then
      FPanningWindow.Release;
    DeleteObject(FPanningCursor);
    FPanningCursor := 0;
    Winapi.Windows.SetCursor(Screen.Cursors[Cursor]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.StructureChange(Node: PVirtualNode; Reason: TChangeReason);

begin
  AdviseChangeEvent(True, Node, Reason);

  if FUpdateCount = 0 then
  begin
    if (FChangeDelay > 0) and HandleAllocated and not (tsSynchMode in FStates) then
      SetTimer(Handle, StructureChangeTimer, FChangeDelay, nil)
    else
      DoStructureChange(Node, Reason);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.StyleServices(AControl: TControl): TCustomStyleServices;
begin
  if AControl = nil then
    AControl := Self;
  Result := VTStyleServices(AControl);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.SuggestDropEffect(Source: TObject; Shift: TShiftState; Pt: TPoint;
  AllowedEffects: Integer): Integer;

// determines the drop action to take if the drag'n drop operation ends on this tree
// Note: Source can be any Delphi object not just a virtual tree

begin
  Result := AllowedEffects;

  // prefer MOVE if source and target are the same control, otherwise whatever is allowed as initial value
  if Assigned(Source) and (Source = Self) then
    if (AllowedEffects and DROPEFFECT_MOVE) <> 0 then
      Result := DROPEFFECT_MOVE
    else // no change
  else
    // drag between different applicatons
    if (AllowedEffects and DROPEFFECT_COPY) <> 0 then
      Result := DROPEFFECT_COPY;

  // consider modifier keys and what is allowed at the moment, if none of the following conditions apply then
  // the initial value just set is used
  if ssCtrl in Shift then
  begin
    // copy or link
    if ssShift in Shift then
    begin
      // link
      if (AllowedEffects and DROPEFFECT_LINK) <> 0 then
        Result := DROPEFFECT_LINK;
    end
    else
    begin
      // copy
      if (AllowedEffects and DROPEFFECT_COPY) <> 0 then
        Result := DROPEFFECT_COPY;
    end;
  end
  else
  begin
    // move, link or default
    if ssShift in Shift then
    begin
      // move
      if (AllowedEffects and DROPEFFECT_MOVE) <> 0 then
        Result := DROPEFFECT_MOVE;
    end
    else
    begin
      // link or default
      if ssAlt in Shift then
      begin
        // link
        if (AllowedEffects and DROPEFFECT_LINK) <> 0 then
          Result := DROPEFFECT_LINK;
      end;
      // else default
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ToggleSelection(StartNode, EndNode: PVirtualNode);

// Switchs the selection state of a range of nodes.
// Note: This method is specifically designed to help selecting ranges with the keyboard and considers therefore
//       the range anchor.

var
  NodeFrom,
  NodeTo: PVirtualNode;
  NewSize: Integer;
  Position: Integer;

begin
  if not FSelectionLocked then
  begin
    Assert(Assigned(EndNode), 'EndNode must not be nil!');
    if StartNode = nil then
      StartNode := FRoot.FirstChild
    else
      if not FullyVisible[StartNode] then
        StartNode := GetPreviousVisible(StartNode, True);

    Position := CompareNodePositions(StartNode, EndNode);
    // nothing to do if start and end node are the same
    if Position <> 0 then
    begin
      if Position < 0 then
      begin
        NodeFrom := StartNode;
        NodeTo := EndNode;
      end
      else
      begin
        NodeFrom := EndNode;
        NodeTo := StartNode;
      end;

      ClearTempCache;

      // 1) toggle the start node if it is before the range anchor
      if CompareNodePositions(NodeFrom, FRangeAnchor) < 0 then
        if not (vsSelected in NodeFrom.States) then
          InternalCacheNode(NodeFrom)
        else
          InternalRemoveFromSelection(NodeFrom);

      // 2) toggle all nodes within the range
      NodeFrom := GetNextVisible(NodeFrom, True);
      while NodeFrom <> NodeTo do
      begin
        if not (vsSelected in NodeFrom.States) then
          InternalCacheNode(NodeFrom)
        else
          InternalRemoveFromSelection(NodeFrom);
        NodeFrom := GetNextVisible(NodeFrom, True);
      end;

      // 3) toggle end node if it is after the range anchor
      if CompareNodePositions(NodeFrom, FRangeAnchor) > 0 then
        if not (vsSelected in NodeFrom.States) then
          InternalCacheNode(NodeFrom)
        else
          InternalRemoveFromSelection(NodeFrom);

      // Do some housekeeping if there was a change.
      NewSize := PackArray(FSelection, FSelectionCount);
      if NewSize > -1 then
      begin
        FSelectionCount := NewSize;
        SetLength(FSelection, FSelectionCount);
      end;
      // If the range went over the anchor then we need to reselect it.
      if not (vsSelected in FRangeAnchor.States) then
        InternalCacheNode(FRangeAnchor);
      if FTempNodeCount > 0 then
        AddToSelection(FTempNodeCache, FTempNodeCount);
      ClearTempCache;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.TrySetFocus();
begin
  if Visible and CanFocus then
  begin
    try
      Self.SetFocus();
    except
      on EInvalidOperation do
        Exit;
    end;
  end;//if
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UnselectNodes(StartNode, EndNode: PVirtualNode);

// Deselects a range of nodes.
// EndNode must be visible while StartNode must not as in the case where the last focused node is the start node
// but it is a child of a node which has been collapsed previously. In this case the first visible parent node
// is used as start node. StartNode can be nil in which case the very first node in the tree is used.

var
  NodeFrom,
  NodeTo: PVirtualNode;
  NewSize: Integer;

begin
  if not FSelectionLocked then
  begin
    Assert(Assigned(EndNode), 'EndNode must not be nil!');

    if StartNode = nil then
      StartNode := FRoot.FirstChild
    else
      if not FullyVisible[StartNode] then
      begin
        StartNode := GetPreviousVisible(StartNode, True);
        if StartNode = nil then
          StartNode := FRoot.FirstChild;
      end;

    if CompareNodePositions(StartNode, EndNode) < 0 then
    begin
      NodeFrom := StartNode;
      NodeTo := EndNode;
    end
    else
    begin
      NodeFrom := EndNode;
      NodeTo := StartNode;
    end;

    while NodeFrom <> NodeTo do
    begin
      InternalRemoveFromSelection(NodeFrom);
      NodeFrom := GetNextVisible(NodeFrom, True);
    end;
    // Deselect last node too.
    InternalRemoveFromSelection(NodeFrom);

    // Do some housekeeping.
    NewSize := PackArray(FSelection, FSelectionCount);
    if NewSize > -1 then
    begin
      FSelectionCount := NewSize;
      SetLength(FSelection, FSelectionCount);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateColumnCheckState(Col: TVirtualTreeColumn);
var
  NewCheckState: TCheckState;
begin
  NewCheckState := DetermineNextCheckState(Col.CheckType, Col.CheckState);
  if (Col.CheckState <> NewCheckState) and DoColumnChecking(Col.Index, NewCheckState) then
  begin
    Col.CheckState := NewCheckState;
    DoColumnChecked(Col.Index);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateDesigner;

var
  ParentForm: TCustomForm;

begin
  if (csDesigning in ComponentState) and not (csUpdating in ComponentState) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
      ParentForm.Designer.Modified;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateHeaderRect();

// Calculates the rectangle the header occupies in non-client area.
// These coordinates are in window rectangle.

var
  OffsetX,
  OffsetY: TDimension;
  EdgeSize: TDimension;
  Size: TSize;

begin
  FHeaderRect := Rect(0, 0, Width, Height);

  // Consider borders...
  if HandleAllocated then begin // Prevent preliminary creation of window handle, see issue #933
    Size := GetBorderDimensions();
    InflateRect(FHeaderRect, Size.cx, Size.cy);
  end;

  // ... and bevels.
  OffsetX := BorderWidth;
  OffsetY := BorderWidth;
  if BevelKind <> TBevelKind.bkNone then
  begin
    EdgeSize := 0;
    if BevelInner <> TBevelCut.bvNone then
      Inc(EdgeSize, BevelWidth);
    if BevelOuter <> TBevelCut.bvNone then
      Inc(EdgeSize, BevelWidth);
    if TBevelEdge.beLeft in BevelEdges then
      Inc(OffsetX, EdgeSize);
    if TBevelEdge.beTop in BevelEdges then
      Inc(OffsetY, EdgeSize);
  end;

  InflateRect(FHeaderRect, -OffsetX, -OffsetY);

  if hoVisible in FHeader.Options then
  begin
    if FHeaderRect.Left <= FHeaderRect.Right then
      FHeaderRect.Bottom := FHeaderRect.Top + FHeader.Height
    else
      FHeaderRect := Rect(0, 0, 0, 0);
  end
  else
    FHeaderRect.Bottom := FHeaderRect.Top;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateEditBounds;

// Used to update the bounds of the current node editor if editing is currently active.

var
  R: TRect;
  CurrentAlignment: TAlignment;
  CurrentBidiMode: TBidiMode;
  offsets : TVTOffsets;
  offset : TDimension;

begin
  if (tsEditing in FStates) and Assigned(FFocusedNode) and
     (FEditColumn < FHeader.Columns.Count) then // prevent EArgumentOutOfRangeException
  begin
    if (GetCurrentThreadId <> MainThreadID) then
    begin
      // UpdateEditBounds() will be called at the end of the thread
      Exit;
    end;
    if vsMultiline in FFocusedNode.States then
      R := GetDisplayRect(FFocusedNode, FEditColumn, True, False)
    else if not (toGridExtensions in FOptions.MiscOptions) then
      R := GetDisplayRect(FFocusedNode, FEditColumn, True, True);

    if (toGridExtensions in FOptions.MiscOptions) then
    begin
      // Use the whole cell when grid extensions are on.
      R := GetDisplayRect(FFocusedNode, FEditColumn, False, False);
      if FEditColumn = FHeader.MainColumn then
      begin
        // Calculate an offset for the main column.
        GetOffsets(FFocusedNode, offsets, ofsLabel, FEditColumn);
        offset := offsets[ofsLabel];
//        if offsets[ofsToggleButton] < 0 then
//          Inc(offset, offsets[ofsToggleButton]);
      end
      else
        offset := 0;

      // Adjust edit bounds depending on alignment and bidi mode.
      if FEditColumn <= NoColumn then
      begin
        CurrentAlignment := Alignment;
        CurrentBidiMode := BiDiMode;
      end
      else
      begin
        CurrentAlignment := FHeader.Columns[FEditColumn].Alignment;
        CurrentBidiMode := FHeader.Columns[FEditColumn].BiDiMode;
      end;
      // Consider bidi mode here. In RTL context does left alignment actually mean right alignment and vice versa.
      if CurrentBidiMode <> bdLeftToRight then
        ChangeBiDiModeAlignment(CurrentAlignment);
      if CurrentAlignment = taLeftJustify then
      begin
        if CurrentBiDiMode = bdLeftToRight then
          Inc(R.Left, offset)
        else
          Dec(R.Right, offset);
      end
      else
      begin
        if CurrentBiDiMode = bdLeftToRight then
          Inc(R.Left, offset)
        else
          Dec(R.Right, offset);
      end;
    end;
    if toShowHorzGridLines in TreeOptions.PaintOptions then
      Dec(R.Bottom);
    R.Bottom := R.Top + R.Bottom - R.Top;
    FEditLink.SetBounds(R);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  ScrollMasks: array[Boolean] of Cardinal = (0, SIF_DISABLENOSCROLL);

const // Region identifiers for GetRandomRgn
  CLIPRGN = 1;
  METARGN = 2;
  APIRGN = 3;
  SYSRGN = 4;

function GetRandomRgn(DC: HDC; Rgn: HRGN; iNum: Integer): Integer; stdcall; external 'GDI32.DLL';

procedure TBaseVirtualTree.ValidateCache();

// Starts cache validation if not already done by adding this instance to the worker thread's waiter list
// (if not already there) and signalling the thread it can start validating.

begin
  // stop validation if it is currently validating this tree's cache.
  InterruptValidation();

  FStartIndex := 0;
  if (tsValidationNeeded in FStates) and (FVisibleCount > CacheThreshold) then
  begin
    // Tell the thread this tree needs actually something to do.
    TWorkerThread.AddTree(Self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ValidateNodeDataSize(var Size: Integer);

begin
  Size := SizeOf(Pointer);
  if Assigned(FOnGetNodeDataSize) then
    FOnGetNodeDataSize(Self, Size);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.VclStyleChanged();

  // Updates the member FVclStyleEnabled, should be called initially and when the VCL style changes

begin
  FVclStyleEnabled := StyleServices.Enabled and not StyleServices.IsSystemStyle {$IF CompilerVersion < 35} and not (csDesigning in ComponentState) {$ifend};
  Header.StyleChanged();
end;

//----------------------------------------------------------------------------------------------------------------------

//PROFILE-NO
procedure TBaseVirtualTree.WndProc(var Message: TMessage);

var
  Handled: Boolean;

begin
  Handled := False;

  // Try the header whether it needs to take this message.
  if Assigned(FHeader) and (FHeader.States <> []) then
    Handled := TVTHeaderCracker(FHeader).HandleMessage(Message);
  if not Handled then
  begin
    // For auto drag mode, let tree handle itself, instead of TControl.
    if not (csDesigning in ComponentState) and
       ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) then
    begin
      if (DragMode = dmAutomatic) and (DragKind = dkDrag) then
      begin
        if IsControlMouseMsg(TWMMouse(Message)) then
          Handled := True;
        if not Handled then
        begin
          ControlState := ControlState + [csLButtonDown];
          Dispatch(Message);  // overrides TControl's BeginDrag
          Handled := True;
        end;
      end;
    end;

    if not Handled and Assigned(FHeader) then
      Handled := TVTHeaderCracker(FHeader).HandleMessage(Message);

    if not Handled then
    begin
      if (Message.Msg in [WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN]) and not Focused then
        TrySetFocus;
      inherited;
    end;
  end;
end;
//PROFILE-YES

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WriteChunks(Stream: TStream; Node: PVirtualNode);

// Writes the core chunks for Node into the stream.
// Note: descendants can optionally override this method to add other node specific chunks.
//       Keep in mind that this method is also called for the root node. Using this fact in descendants you can
//       create a kind of "global" chunks not directly bound to a specific node.

var
  Header: TChunkHeader;
  LastPosition,
  ChunkSize: Integer;
  Chunk: TBaseChunk;
  Run: PVirtualNode;

begin
  with Stream do
  begin
    // 1. The base chunk...
    LastPosition := Position;
    Chunk.Header.ChunkType := BaseChunk;
    with Node^, Chunk do
    begin
      Body.ChildCount := ChildCount;
      Body.NodeHeight := NodeHeight;
      // Some states are only temporary so take them out as they make no sense at the new location.
      Body.States := States - [vsChecking, vsCutOrCopy, vsDeleting, vsOnFreeNodeCallRequired, vsHeightMeasured];
      Body.Align := Align;
      Body.CheckState := GetCheckState(Node);
      Body.CheckType := CheckType;
      Body.Reserved := 0;
    end;
    // write the base chunk
    Write(Chunk, SizeOf(Chunk));

    // 2. ... directly followed by the child node chunks (actually they are child chunks of
    //   the base chunk)
    if vsInitialized in Node.States then
    begin
      Run := Node.FirstChild;
      while Assigned(Run) do
      begin
        WriteNode(Stream, Run);
        Run := Run.NextSibling;
      end;
    end;

    FinishChunkHeader(Stream, LastPosition, Position);

    // 3. write user data
    LastPosition := Position;
    Header.ChunkType := UserChunk;
    Write(Header, SizeOf(Header));
    DoSaveUserData(Node, Stream);
    // check if the application actually wrote data
    ChunkSize := Position - LastPosition - SizeOf(TChunkHeader);
    // seek back to start of chunk if nothing has been written
    if ChunkSize = 0 then
    begin
      Position := LastPosition;
      Size := Size - SizeOf(Header);
    end
    else
      FinishChunkHeader(Stream, LastPosition, Position);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.WriteNode(Stream: TStream; Node: PVirtualNode);

// Writes the "cover" chunk for Node to Stream and initiates writing child nodes and chunks.

var
  LastPosition: Integer;
  Header: TChunkHeader;

begin
  // Initialize the node first if necessary and wanted.
  if toInitOnSave in FOptions.MiscOptions then
  begin
    if not (vsInitialized in Node.States) then
      InitNode(Node);
    if (vsHasChildren in Node.States) and (Node.ChildCount = 0) then
      InitChildren(Node);
  end;

  with Stream do
  begin
    LastPosition := Position;
    // Emit the anchor chunk.
    Header.ChunkType := NodeChunk;
    Write(Header, SizeOf(Header));
    // Write other chunks to stream taking their size into this chunk's size.
    WriteChunks(Stream, Node);

    // Update chunk size.
    FinishChunkHeader(Stream, LastPosition, Position);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.AbsoluteIndex(Node: PVirtualNode): Cardinal;

begin
  Result := 0;
  while Assigned(Node) and (Node <> FRoot) do
  begin
    if not (vsInitialized in Node.States) then
      InitNode(Node);
    if Assigned(Node.PrevSibling) then
    begin
      // if there's a previous sibling then add its total count to the result
      Node := Node.PrevSibling;
      System.Inc(Result, Node.TotalCount);
    end
    else
    begin
      Node := Node.Parent;
      if Node <> FRoot then
        System.Inc(Result);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.AddChild(Parent: PVirtualNode; UserData: Pointer = nil): PVirtualNode;

// Adds a new node to the given parent node. This is simply done by increasing the child count of the
// parent node. If Parent is nil then the new node is added as (last) top level node.
// UserData can be used to set the first SizeOf(Pointer) bytes of the user data area to an initial value which can be used
// in OnInitNode and will also cause to trigger the OnFreeNode event (if <> nil) even if the node is not yet
// "officially" initialized.
// AddChild is a compatibility method and will implicitly validate the parent node. This is however
// against the virtual paradigm and hence I dissuade from its usage.

begin
  if not (toReadOnly in FOptions.MiscOptions) then
    Result := InsertNode(Parent, TVTNodeAttachMode.amAddChildLast, UserData)
  else
    Result := nil;
end;

function TBaseVirtualTree.AddChild(Parent: PVirtualNode; const UserData: IInterface): PVirtualNode;
begin
  UserData._AddRef();
  Result := AddChild(Parent, Pointer(UserData));
  Include(Result.States, vsReleaseCallOnUserDataRequired);
end;

function TBaseVirtualTree.AddChild(Parent: PVirtualNode; const UserData: TObject): PVirtualNode;
begin
  Result := AddChild(Parent, Pointer(UserData));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AddFromStream(Stream: TStream; TargetNode: PVirtualNode);

// loads nodes from the given stream and adds them to TargetNode
// the current content is not cleared before the load process starts (see also LoadFromStream)

var
  ThisID: TMagicID;
  Version,
  Count: Cardinal;
  Node: PVirtualNode;

begin
  if not (toReadOnly in FOptions.MiscOptions) then
  begin
    // check first whether this is a stream we can read
    Stream.ReadBuffer(ThisID, SizeOf(TMagicID));
    if (ThisID[0] = MagicID[0]) and
       (ThisID[1] = MagicID[1]) and
       (ThisID[2] = MagicID[2]) and
       (ThisID[5] = MagicID[5]) then
    begin
      Version := Word(ThisID[3]);
      if Version <= VTTreeStreamVersion  then
      begin
        BeginUpdate;
        try
          if Version < 2 then
            Count := MaxInt
          else
            Stream.ReadBuffer(Count, SizeOf(Count));

          while (Stream.Position < Stream.Size) and (Count > 0) do
          begin
            System.Dec(Count);
            Node := MakeNewNode;
            InternalConnectNode(Node, TargetNode, Self, amAddChildLast);
            InternalAddFromStream(Stream, Version, Node);
          end;
          if TargetNode = FRoot then
            DoNodeCopied(nil)
          else
            DoNodeCopied(TargetNode);
        finally
          EndUpdate;
        end;
      end
      else
        RaiseVTError(SWrongStreamVersion, hcTFWrongStreamVersion);
    end
    else
      RaiseVTError(SWrongStreamVersion, hcTFWrongStreamVersion);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AfterConstruction;

begin
  inherited;

  if FRoot = nil then
    InitRootNode;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Assign(Source: TPersistent);

begin
  if (Source is TBaseVirtualTree) and not (toReadOnly in FOptions.MiscOptions) then
    with Source as TBaseVirtualTree do
    begin
      Self.Align := Align;
      Self.Anchors := Anchors;
      Self.AutoScrollDelay := AutoScrollDelay;
      Self.AutoScrollInterval := AutoScrollInterval;
      Self.AutoSize := AutoSize;
      Self.Background := Background;
      Self.BevelEdges := BevelEdges;
      Self.BevelInner := BevelInner;
      Self.BevelKind := BevelKind;
      Self.BevelOuter := BevelOuter;
      Self.BevelWidth := BevelWidth;
      Self.BiDiMode := BiDiMode;
      Self.BorderStyle := BorderStyle;
      Self.BorderWidth := BorderWidth;
      Self.ChangeDelay := ChangeDelay;
      Self.CheckImageKind := CheckImageKind;
      Self.Color := Color;
      Self.Colors.Assign(Colors);
      Self.Constraints.Assign(Constraints);
      Self.Ctl3D := Ctl3D;
      Self.DefaultNodeHeight := DefaultNodeHeight;
      Self.DefaultPasteMode := DefaultPasteMode;
      Self.DragCursor := DragCursor;
      Self.DragImageKind := DragImageKind;
      Self.DragKind := DragKind;
      Self.DragMode := DragMode;
      Self.Enabled := Enabled;
      Self.Font := Font;
      Self.Header := Header;
      Self.HintMode := HintMode;
      Self.HotCursor := HotCursor;
      Self.Images := Images;
      Self.ImeMode := ImeMode;
      Self.ImeName := ImeName;
      Self.Indent := Indent;
      Self.Margin := Margin;
      Self.NodeAlignment := NodeAlignment;
      Self.NodeDataSize := NodeDataSize;
      Self.TreeOptions := TreeOptions;
      Self.ParentBiDiMode := ParentBiDiMode;
      Self.ParentColor := ParentColor;
      Self.ParentCtl3D := ParentCtl3D;
      Self.ParentFont := ParentFont;
      Self.ParentShowHint := ParentShowHint;
      Self.PopupMenu := PopupMenu;
      Self.RootNodeCount := RootNodeCount;
      Self.ScrollBarOptions := ScrollBarOptions;
      Self.ShowHint := ShowHint;
      Self.StateImages := StateImages;
      Self.StyleElements := StyleElements;
      Self.TabOrder := TabOrder;
      Self.TabStop := TabStop;
      Self.Visible := Visible;
      Self.SelectionCurveRadius := SelectionCurveRadius;
      Self.SelectionBlendFactor := SelectionBlendFactor;
      Self.EmptyListMessage := EmptyListMessage;
    end
    else
      inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.AutoScale();

// If toAutoChangeScale is set, this method ensures that the default node height is set correctly.

var
  lTextHeight: TDimension;
begin
  if HandleAllocated and (toAutoChangeScale in TreeOptions.AutoOptions) then
  begin
    Canvas.Font.Assign(Self.Font);
    lTextHeight := Canvas.TextHeight('Tg') + TextMargin;
    if Assigned(Images) then
      lTextHeight := Max(lTextHeight, Images.Height + IfThen(fImagesMargin > 1, fImagesMargin div 2, fImagesMargin)); // ImagesMargin is the distance between two Images / checboxes. Don't count it twice vertically => div 2
    // By default, we only ensure that DefaultNodeHeight is large enough.
    // If the form's dpi has changed, we scale up and down the DefaultNodeHeight, See issue #677.
    if (lTextHeight <> Self.DefaultNodeHeight) then begin
      ScaleNodeHeights(lTextHeight, DefaultNodeHeight);
      Self.DefaultNodeHeight := lTextHeight;
    end;// if
  end;// if HandelAllocated
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.BeginDrag(Immediate: Boolean; Threshold: Integer);

// Reintroduced method to allow to start OLE drag'n drop as well as VCL drag'n drop.

begin
  if FDragType = dtVCL then
  begin
    DoStateChange([tsVCLDragPending]);
    inherited;
  end
  else
    if (FStates * [tsOLEDragPending, tsOLEDragging]) = [] then
    begin
      // Drag start position has already been recorded in WMMouseDown.
      if Threshold < 0 then
        FDragThreshold := Mouse.DragThreshold
      else
        FDragThreshold := Threshold;
      if Immediate then
        DoDragging(FLastClickPos)
      else
        DoStateChange([tsOLEDragPending]);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.BeginSynch;

// Starts the synchronous update mode (if not already active).

begin
  if not (csDestroying in ComponentState) then
  begin
    if FSynchUpdateCount = 0 then
    begin
      DoUpdating(usBeginSynch);

      // Stop all timers...
      StopTimer(ChangeTimer);
      StopTimer(StructureChangeTimer);
      StopTimer(ExpandTimer);
      StopTimer(EditTimer);
      StopTimer(HeaderTimer);
      StopTimer(ScrollTimer);
      StopTimer(SearchTimer);
      FSearchBuffer := '';
      FLastSearchNode := nil;
      DoStateChange([], [tsEditPending, tsScrollPending, tsScrolling, tsIncrementalSearching]);

      // ...and trigger pending update states.
      if tsStructureChangePending in FStates then
        DoStructureChange(FLastStructureChangeNode, FLastStructureChangeReason);
      if tsChangePending in FStates then
        DoChange(FLastChangedNode);
    end
    else
      DoUpdating(usSynch);
  end;
  System.Inc(FSynchUpdateCount);
  DoStateChange([tsSynchMode]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.BeginUpdate;

begin
  Assert(GetCurrentThreadId = MainThreadId, 'UI controls like ' + Classname + ' should only be manipulated through the main thread.');
  if not (csDestroying in ComponentState) then
  begin
    if FUpdateCount = 0 then
    begin
      DoUpdating(usBegin);
      SetUpdateState(True);
    end
    else
      DoUpdating(usUpdate);
  end;
  System.Inc(FUpdateCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CancelCutOrCopy;

// Resets nodes which are marked as being cut.

var
  Run: PVirtualNode;

begin
  if ([tsCutPending, tsCopyPending] * FStates) <> [] then
  begin
    Run := FRoot.FirstChild;
    while Assigned(Run) do
    begin
      if vsCutOrCopy in Run.States then
        Exclude(Run.States, vsCutOrCopy);
      Run := GetNextNoInit(Run);
    end;
  end;
  DoStateChange([], [tsCutPending, tsCopyPending]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CancelEditNode: Boolean;

// Called by the application or the current edit link to cancel the edit action.

begin
  if HandleAllocated and ([tsEditing, tsEditPending] * FStates <> []) then
    Result := DoCancelEdit
  else
    Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CancelOperation;

// Called by the application to cancel a long-running operation.

begin
  if FOperationCount > 0 then
    FOperationCanceled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CanEdit(Node: PVirtualNode; Column: TColumnIndex): Boolean;

// Returns True if the given node can be edited.

begin
  Result := (toEditable in FOptions.MiscOptions) and Enabled and not (toReadOnly in FOptions.MiscOptions)
    and ((Column < 0) or (coEditable in FHeader.Columns[Column].Options));
  DoCanEdit(Node, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CanFocus: Boolean;

var
  Form: TCustomForm;

begin
  Result := inherited CanFocus;

  if Result and not (csDesigning in ComponentState) then
  begin
    Form := GetParentForm(Self);
    Result := (Form = nil) or (Form.Enabled and Form.Visible);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Clear;

begin
  if (not IsEmpty and not (toReadOnly in FOptions.MiscOptions)) or (csDestroying in ComponentState) then
  begin
    BeginUpdate;
    try
      InterruptValidation;
      if IsEditing then
        CancelEditNode;

      if ClipboardStates * FStates <> [] then
      begin
        OleSetClipboard(nil);
        DoStateChange([], ClipboardStates);
      end;
      ClearSelection;
      FFocusedNode := nil;
      FLastSelected := nil;
      FCurrentHotNode := nil;
      FDropTargetNode := nil;
      FLastChangedNode := nil;
      FRangeAnchor := nil;
      FLastVCLDragTarget := nil;
      FLastSearchNode := nil;
      DeleteChildren(FRoot, True);
      FOffsetX := 0;
      FOffsetY := 0;

    finally
      EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ClearChecked;

var
  Node: PVirtualNode;

begin
  Node := RootNode.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CheckState <> csUncheckedNormal then
      CheckState[Node] := csUncheckedNormal;
    Node := GetNextNoInit(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ClearSelection();
begin
  ClearSelection(True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ClearDragManager;
begin
  Pointer(FDragManager) := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ClearSelection(pFireChangeEvent: Boolean);

var
  Node: PVirtualNode;
  Dummy: TDimension;
  R: TRect;
  Counter: Integer;

begin
  Assert(GetCurrentThreadId = MainThreadId, Self.Classname + '.ClearSelection() must only be called from UI thread.');
  if not FSelectionLocked and (FSelectionCount > 0) and not (csDestroying in ComponentState) then
  begin
    if (FUpdateCount = 0) and HandleAllocated and (FVisibleCount > 0) then
    begin
      // Iterate through nodes currently visible in the client area and invalidate them.
      Node := GetNodeAt(0, 0, True, Dummy);
      if Assigned(Node) then
        R := GetDisplayRect(Node, NoColumn, False);
      Counter := FSelectionCount;

      while Assigned(Node) do
      begin
        R.Bottom := R.Top + NodeHeight[Node];
        if vsSelected in Node.States then
        begin
          InvalidateRect(@R, False);
          System.Dec(Counter);
          // Only try as many nodes as are selected.
          if Counter = 0 then
            Break;
        end;
        R.Top := R.Bottom;
        if R.Top > ClientHeight then
          Break;
        Node := GetNextVisibleNoInit(Node, True);
      end;
    end;

    InternalClearSelection;
    if pFireChangeEvent then
      Change(nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CopyTo(Source: PVirtualNode; Tree: TBaseVirtualTree; Mode: TVTNodeAttachMode;
  ChildrenOnly: Boolean): PVirtualNode;

// A simplified CopyTo method to allow to copy nodes to the root of another tree.

begin
  Result := CopyTo(Source, Tree.FRoot, Mode, ChildrenOnly);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CopyTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode;
  ChildrenOnly: Boolean): PVirtualNode;

// Copies Source and all its child nodes to Target.
// Mode is used to specify further where to add the new node actually (as sibling of Target or as child of Target).
// Result is the newly created node to which source has been copied if ChildrenOnly is False or just contains Target
// in the other case.
// ChildrenOnly determines whether to copy also the source node or only its child nodes.

var
  TargetTree: TBaseVirtualTree;
  Stream: TMemoryStream;

begin
  Assert(TreeFromNode(Source) = Self, 'The source tree must contain the source node.');

  Result := nil;
  if (Mode <> amNoWhere) and Assigned(Source) and (Source <> FRoot) then
  begin
    // Assume that an empty destination means the root in this (the source) tree.
    if Target = nil then
    begin
      TargetTree := Self;
      Target := FRoot;
      Mode := amAddChildFirst;
    end
    else
      TargetTree := TreeFromNode(Target);

    if not (toReadOnly in TargetTree.TreeOptions.MiscOptions) then
    begin
      if Target = TargetTree.FRoot then
      begin
        case Mode of
          amInsertBefore:
            Mode := amAddChildFirst;
          amInsertAfter:
            Mode := amAddChildLast;
        end;
      end;

      Stream := TMemoryStream.Create;
      try
        // Write all nodes into a temprary stream depending on the ChildrenOnly flag.
        if not ChildrenOnly then
          WriteNode(Stream, Source)
        else
        begin
          Source := Source.FirstChild;
          while Assigned(Source) do
          begin
            WriteNode(Stream, Source);
            Source := Source.NextSibling;
          end;
        end;
        // Now load the serialized nodes into the target node (tree).
        TargetTree.BeginUpdate;
        try
          Stream.Position := 0;
          while Stream.Position < Stream.Size do
          begin
            Result := TargetTree.MakeNewNode;
            InternalConnectNode(Result, Target, TargetTree, Mode);
            TargetTree.InternalAddFromStream(Stream, VTTreeStreamVersion, Result);
            if not DoNodeCopying(Result, Target) then
            begin
              TargetTree.DeleteNode(Result);
              Result := nil;
            end
            else
              DoNodeCopied(Result);
          end;
          if ChildrenOnly then
            Result := Target;
        finally
          TargetTree.EndUpdate;
        end;
      finally
        Stream.Free;
      end;

      with TargetTree do
      begin
        InvalidateCache;
        if FUpdateCount = 0 then
        begin
          ValidateCache;
          UpdateScrollBars(True);
          Invalidate;
        end;
        StructureChange(Source, crNodeCopied);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DeleteChildren(Node: PVirtualNode; ResetHasChildren: Boolean = False);

// Removes all children and their children from memory without changing the vsHasChildren style by default.

var
  Run,
  Mark: PVirtualNode;
  LastTop,
  LastLeft: TDimension;
  NewSize: Integer;
  ParentVisible: Boolean;

begin
  if Assigned(Node) and (Node.ChildCount > 0) and not (toReadOnly in FOptions.MiscOptions) then
  begin
    Assert(not (tsIterating in FStates), 'Deleting nodes during tree iteration leads to invalid pointers.');

    // The code below uses some flags for speed improvements which may cause invalid pointers if updates of
    // the tree happen. Hence switch updates off until we have finished the operation.
    System.Inc(FUpdateCount);
    try
      InterruptValidation;
      LastLeft := -FEffectiveOffsetX;
      LastTop := FOffsetY;

      // Make a local copy of the visibility state of this node to speed up
      // adjusting the visible nodes count.
      ParentVisible := Node = FRoot;
      if not ParentVisible then
        ParentVisible := FullyVisible[Node] and (vsExpanded in Node.States);

      // Show that we are clearing the child list, to avoid registering structure change events.
      Run := Node.LastChild;
      while Assigned(Run) do
      begin
        if ParentVisible and IsEffectivelyVisible[Run] then
          System.Dec(FVisibleCount);

        Include(Run.States, vsDeleting);
        Mark := Run;
        Run := Run.PrevSibling;
        // Important, to avoid exchange of invalid pointers while disconnecting the node.
        if Assigned(Run) then
          Run.SetNextSibling(nil);
        DeleteNode(Mark, False, True);
      end;
      if ResetHasChildren then
        Exclude(Node.States, vsHasChildren);
      if Node <> FRoot then
        Exclude(Node.States, vsExpanded);
      Node.SetChildCount(0);
      if (Node = FRoot) or (vsDeleting in Node.States) then
      begin
        Node.TotalHeight := FDefaultNodeHeight + NodeHeight[Node];
        Node.TotalCount := 1;
      end
      else
      begin
        AdjustTotalHeight(Node, NodeHeight[Node]);
        AdjustTotalCount(Node, 1);
      end;
      Node.SetFirstChild(nil);
      Node.SetLastChild(nil);
    finally
      System.Dec(FUpdateCount);
    end;

    InvalidateCache;
    if FUpdateCount = 0 then
    begin
      NewSize := PackArray(FSelection, FSelectionCount);
      if NewSize > -1 then
      begin
        FSelectionCount := NewSize;
        SetLength(FSelection, FSelectionCount);
      end;

      ValidateCache;
      UpdateScrollBars(True);
      // Invalidate entire tree if it scrolled e.g. to make the last node also the
      // bottom node in the treeview.
      if (LastLeft <> FOffsetX) or (LastTop <> FOffsetY) then
        Invalidate
      else
        InvalidateToBottom(Node);
      if tsChangePending in FStates then begin
        DoChange(FLastChangedNode);
        EnsureNodeSelected(True);
      end;
    end;
    StructureChange(Node, crChildDeleted);
  end
  else if ResetHasChildren then
    Exclude(Node.States, vsHasChildren);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DeleteNode(Node: PVirtualNode; Reindex: Boolean; ParentClearing: Boolean);

var
  LastTop,
  LastLeft: TDimension;
  LastParent: PVirtualNode;
  WasInSynchMode: Boolean;

begin
  if Assigned(Node) and (Node <> FRoot) and not (toReadOnly in FOptions.MiscOptions) then
  begin
    Assert(not (tsIterating in FStates), 'Deleting nodes during tree iteration leads to invalid pointers.');

    // Determine parent node for structure change notification.
    LastParent := Node.Parent;

    if not ParentClearing then
    begin
      if LastParent = FRoot then
        StructureChange(nil, crChildDeleted)
      else
        StructureChange(LastParent, crChildDeleted);
      if Node = FNextNodeToSelect then
        FNextNodeToSelect := nil;
    end;

    LastLeft := -FEffectiveOffsetX;
    LastTop := FOffsetY;

    if tsHint in FStates then
    begin
      Application.CancelHint;
      DoStateChange([], [tsHint]);
    end;

    if not ParentClearing then
      InterruptValidation;

    DeleteChildren(Node);

    if vsSelected in Node.States then
    begin
      if FUpdateCount = 0 then
      begin
        // Go temporarily into sync mode to avoid a delayed change event for the node
        // when unselecting.
        WasInSynchMode := tsSynchMode in FStates;
        Include(FStates, tsSynchMode);
        RemoveFromSelection(Node);
        //EnsureNodeSelected(); // also done in  DoFreeNode()
        if not WasInSynchMode then
          Exclude(FStates, tsSynchMode);
        InvalidateToBottom(LastParent);
      end
      else
        InternalRemoveFromSelection(Node);
    end
    else
      InvalidateToBottom(LastParent);

    InternalDisconnectNode(Node, False, Reindex);
    DoFreeNode(Node);

    if not ParentClearing then
    begin
      if FUpdateCount = 0 then
        DetermineHiddenChildrenFlag(LastParent)
      else
        Include(FStates, tsUpdateHiddenChildrenNeeded);
      InvalidateCache;
      if FUpdateCount = 0 then
      begin
        ValidateCache;
        UpdateScrollBars(True);
        // Invalidate entire tree if it scrolled e.g. to make the last node also the
        // bottom node in the treeview.
        if (LastLeft <> FOffsetX) or (LastTop <> FOffsetY) then
          Invalidate;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DeleteNode(Node: PVirtualNode; pReIndex: Boolean = True);
begin
  DeleteNode(Node, pReIndex, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DeleteNodes(const pNodes: TNodeArray);

   // Deletes all given nodes.
   // Best performance is achieved if nodes are sorted by parent

var
  I: Integer;
  LevelChange: Boolean;
begin
  if Length(pNodes) = 0 then
    exit; // Prevent range error below when empty array is passen. See issue #1288
  BeginUpdate;
  try
    for I := High(pNodes) downto 1 do
    begin
      LevelChange := pNodes[I].Parent <> pNodes[I - 1].Parent;
      DeleteNode(pNodes[I], LevelChange, False);
    end;
    DeleteNode(pNodes[0]);
  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DeleteSelectedNodes;

// Deletes all currently selected nodes (including their child nodes).

var
  lNodes: TNodeArray;
begin
  lNodes := nil;
  if (FSelectionCount > 0) and not (toReadOnly in FOptions.MiscOptions) then
  begin
    lNodes := GetSortedSelection(True);
    DeleteNodes(lNodes);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.Dragging: Boolean;

begin
  // Check for both OLE drag'n drop as well as VCL drag'n drop.
  Result := ([tsOLEDragPending, tsOLEDragging] * FStates <> []) or inherited Dragging;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.EditNode(Node: PVirtualNode; Column: TColumnIndex): Boolean;

// Application triggered edit event for the given node.
// Returns True if the tree started editing otherwise False.

begin
  Assert(Assigned(Node), 'Node must not be nil.');
  Assert((Column > InvalidColumn) and (Column < FHeader.Columns.Count),
    'Column must be a valid column index (-1 if no header is shown).');

  Result := tsEditing in FStates;
  // If the tree is already editing then we don't disrupt this.
  if not Result and not (toReadOnly in FOptions.MiscOptions) then
  begin
    FocusedNode := Node;
    if Assigned(FFocusedNode) and (Node = FFocusedNode) and CanEdit(FFocusedNode, Column) then
    begin
      FEditColumn := Column;
      if not (vsInitialized in Node.States) then
        InitNode(Node);
      DoEdit;
      Result := tsEditing in FStates;
    end
    else
      Result := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.EndEditNode: Boolean;

// Called to finish a current edit action or stop the edit timer if an edit operation is pending.
// Returns True if editing was successfully ended or the control was not in edit mode
// Returns False if the control could not leave the edit mode e.g. due to an invalid value that was entered.

begin
  if [tsEditing, tsEditPending] * FStates <> [] then
    Result := DoEndEdit
  else
    Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.EndSynch;

begin
  if FSynchUpdateCount > 0 then
    System.Dec(FSynchUpdateCount);

  if not (csDestroying in ComponentState) then
  begin
    if FSynchUpdateCount = 0 then
    begin
      DoStateChange([], [tsSynchMode]);
      DoUpdating(usEndSynch);
    end
    else
      DoUpdating(usSynch);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.EndUpdate;

var
  NewSize: Integer;

begin
  if FUpdateCount = 0 then
    exit;
  System.Dec(FUpdateCount);

  if not (csDestroying in ComponentState) then
  begin
    if (FUpdateCount = 0) then
    begin
      if tsUpdateHiddenChildrenNeeded in FStates then
      begin
        DetermineHiddenChildrenFlagAllNodes;
        Exclude(FStates, tsUpdateHiddenChildrenNeeded);
      end;

      NewSize := PackArray(FSelection, FSelectionCount);
      if NewSize > -1 then
      begin
        FSelectionCount := NewSize;
        SetLength(FSelection, FSelectionCount);
      end;

      InvalidateCache;
      ValidateCache;
      if HandleAllocated then
        UpdateScrollBars(False);

      if tsStructureChangePending in FStates then
        DoStructureChange(FLastStructureChangeNode, FLastStructureChangeReason);
      try
        if tsChangePending in FStates then
          DoChange(FLastChangedNode);
      finally
        if (toAutoSort in FOptions.AutoOptions) then
          SortTree(FHeader.SortColumn, FHeader.SortDirection, True);

        SetUpdateState(False);
        if HandleAllocated then
          Invalidate;
        UpdateDesigner;
      end;
      NotifyAccessibleEvent(); // See issue #1174

      DoUpdating(usEnd);
      EnsureNodeSelected(False);
    end
    else
      DoUpdating(usUpdate);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ExecuteAction(Action: TBasicAction): Boolean;

// Some support for standard actions.

begin
  Result := inherited ExecuteAction(Action);

  if not Result then
  begin
    Result := Action is TEditSelectAll;
    if Result then
      SelectAll(False)
    else
    begin
      Result := Action is TEditCopy;
      if Result then
        CopyToClipboard
      else
        if not (toReadOnly in FOptions.MiscOptions) then
        begin
          Result := Action is TEditCut;
          if Result then
            CutToClipboard
          else
          begin
            Result := Action is TEditPaste;
            if Result then
              PasteFromClipboard
              else
              begin
                Result := Action is TEditDelete;
                if Result then
                  DeleteSelectedNodes;
              end;
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FinishCutOrCopy;

// Deletes nodes which are marked as being cutted.

var
  Run: PVirtualNode;

begin
  if tsCutPending in FStates then
  begin
    Run := FRoot.FirstChild;
    while Assigned(Run) do
    begin
      if vsCutOrCopy in Run.States then
        DeleteNode(Run);
      Run := GetNextNoInit(Run);
    end;
    DoStateChange([], [tsCutPending]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FlushClipboard;

// Used to render the data which is currently on the clipboard (finishes delayed rendering).

begin
  if ClipboardStates * FStates <> [] then
  begin
    DoStateChange([tsClipboardFlushing]);
    OleFlushClipboard;
    CancelCutOrCopy;
    DoStateChange([], [tsClipboardFlushing]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FullCollapse(Node: PVirtualNode = nil);

// This routine collapses all expanded nodes in the subtree given by Node or the whole tree if Node is FRoot or nil.
// Only nodes which are expanded will be collapsed. This excludes uninitialized nodes but nodes marked as visible
// will still be collapsed if they are expanded.

var
  Stop: PVirtualNode;

begin
  if FRoot.TotalCount > 1 then
  begin
    if Node = FRoot then
      Node := nil;

    DoStateChange([tsCollapsing]);
    BeginUpdate;
    try
      Stop := Node;
      Node := GetLastVisibleNoInit(Node, True);

      if Assigned(Node) then
      begin
        repeat
          if [vsHasChildren, vsExpanded] * Node.States = [vsHasChildren, vsExpanded] then
            ToggleNode(Node);
          Node := GetPreviousNoInit(Node, True);
        until (Node = Stop) or not Assigned(Node);

        // Collapse the start node too.
        if Assigned(Stop) and ([vsHasChildren, vsExpanded] * Stop.States = [vsHasChildren, vsExpanded]) then
          ToggleNode(Stop);
      end;
    finally
      EndUpdate;
      DoStateChange([], [tsCollapsing]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.FullExpand(Node: PVirtualNode = nil);

// This routine expands all collapsed nodes in the subtree given by Node or the whole tree if Node is FRoot or nil.
// All nodes on the way down are initialized so this procedure might take a long time.
// Since all nodes are validated, the tree cannot make use of optimatizations. Hence it is counter productive and you
// should consider avoiding its use.

var
  Stop: PVirtualNode;

begin
  if FRoot.TotalCount > 1 then
  begin
    DoStateChange([tsExpanding]);
    StartOperation(TVTOperationKind.okExpand);
    BeginUpdate;
    try
      if Node = nil then
      begin
        Node := FRoot.FirstChild;
        Stop := nil;
      end
      else
      begin
        Stop := Node.NextSibling;
        if Stop = nil then
        begin
          Stop := Node;
          repeat
            Stop := Stop.Parent;
          until (Stop = FRoot) or Assigned(Stop.NextSibling);
          if Stop = FRoot then
            Stop := nil
          else
            Stop := Stop.NextSibling;
        end;
      end;

      // Initialize the start node. Others will be initialized in GetNext.
      if not (vsInitialized in Node.States) then
        InitNode(Node);

      repeat
        if not (vsExpanded in Node.States) then
          ToggleNode(Node);
        Node := GetNext(Node);
      until (Node = Stop) or OperationCanceled;
    finally
      EndOperation(TVTOperationKind.okExpand);
      EndUpdate;
      DoStateChange([], [tsExpanding]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetControlsAlignment: TAlignment;

begin
  Result := FAlignment;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetDisplayRect(Node: PVirtualNode; Column: TColumnIndex; TextOnly: Boolean;
  Unclipped: Boolean = False; ApplyCellContentMargin: Boolean = False): TRect;

// Determines the client coordinates the given node covers, depending on scrolling, expand state etc.
// If the given node cannot be found (because one of its parents is collapsed or it is invisible) then an empty
// rectangle is returned.
// If TextOnly is True then only the text bounds are returned, that is, the resulting rectangle's left and right border
// are updated according to bidi mode, alignment and text width of the node.
// If Unclipped is True (which only makes sense if also TextOnly is True) then the calculated text rectangle is
// not clipped if the text does not entirely fit into the text space. This is special handling needed for hints.
// If ApplyCellContentMargin is True (which only makes sense if also TextOnly is True) then the calculated text
// rectangle respects the cell content margin.
// If Column is -1 then the entire client width is used before determining the node's width otherwise the bounds of the
// particular column are used.
// Note: Column must be a valid column and is used independent of whether the header is visible or not.

var
  Temp: PVirtualNode;
  LeftOffset: TDimension;
  TopOffset: TNodeHeight;
  CacheIsAvailable: Boolean;
  TextWidth: TDimension;
  CurrentBidiMode: TBidiMode;
  CurrentAlignment: TAlignment;
  MaxUnclippedHeight: TDimension;
  TM: TTextMetric;
  ExtraVerticalMargin: TDimension;
  lOffsets: TVTOffsets;
begin
  Assert(Assigned(Node), 'Node must not be nil.');
  Assert(Node <> FRoot, 'Node must not be the hidden root node.');

  if not (vsInitialized in Node.States) then
    InitNode(Node);

  Result := Rect(0, 0, 0, 0);

  // Check whether the node is visible (determine indentation level btw.).
  if not IsEffectivelyVisible[Node] then
    Exit;

  // Here we know the node is visible.
  TopOffset := 0;
  CacheIsAvailable := False;
  if tsUseCache in FStates then
  begin
    // If we can use the position cache then do a binary search to find a cached node which is as close as possible
    // to the current node. Iterate then through all following and visible nodes and sum up their heights.
    Temp := FindInPositionCache(Node, TopOffset);
    CacheIsAvailable := Assigned(Temp);
    while Assigned(Temp) and (Temp <> Node) do
    begin
      Inc(TopOffset, NodeHeight[Temp]);
      Temp := GetNextVisibleNoInit(Temp, True);
    end;
  end;
  if not CacheIsAvailable then
  begin
    // If the cache is not available then go straight through all nodes up to the root and sum up their heights.
    Temp := Node;
    repeat
      Temp := GetPreviousVisibleNoInit(Temp, True);
      if Temp = nil then
        Break;
      Inc(TopOffset, NodeHeight[Temp]);
    until False;
  end;

  Result := Rect(0, TopOffset, Max(FRangeX, ClientWidth), TopOffset + NodeHeight[Node]);

  // Limit left and right bounds to the given column (if any) and move bounds according to current scroll state.
  if Column > NoColumn then
  begin
    FHeader.Columns.GetColumnBounds(Column, Result.Left, Result.Right);
    // The right column border is not part of this cell.
    Dec(Result.Right);
    OffsetRect(Result, 0, FOffsetY);
  end
  else
    OffsetRect(Result, -FEffectiveOffsetX, FOffsetY);

  // Limit left and right bounds further if only the text area is required.
  if TextOnly then
  begin
    // If the text of a node is involved then we have to consider directionality and alignment too.
    if Column <= NoColumn then
    begin
      CurrentBidiMode := BidiMode;
      CurrentAlignment := Alignment;
    end
    else
    begin
      CurrentBidiMode := FHeader.Columns[Column].BidiMode;
      CurrentAlignment := FHeader.Columns[Column].Alignment;
    end;

    GetOffsets(Node, lOffsets, TVTElement.ofsLabel, Column);
    LeftOffset := lOffSets[TVTElement.ofsLabel];
    // Offset contains now the distance from the left or right border of the rectangle (depending on bidi mode).
    // Now consider the alignment too and calculate the final result.
    if CurrentBidiMode = bdLeftToRight then
    begin
      Inc(Result.Left, LeftOffset);
      // Left-to-right reading does not need any special adjustment of the alignment.
    end
    else
    begin
      Dec(Result.Right, LeftOffset);

      // Consider bidi mode here. In RTL context does left alignment actually mean right alignment and vice versa.
      ChangeBiDiModeAlignment(CurrentAlignment);
    end;

    TextWidth := DoGetNodeWidth(Node, Column);

    // Keep cell height before applying cell content margin in order to increase cell height if text does not fit
    // and Unclipped it true (see below).
    MaxUnclippedHeight := Result.Bottom - Result.Top;

    if ApplyCellContentMargin then
      DoBeforeCellPaint(Self.Canvas, Node, Column, cpmGetContentMargin, Result, Result);

    if Unclipped then
    begin
      // The caller requested the text coordinates unclipped. This means they must be calculated so as would
      // there be enough space, regardless of column bounds etc.
      // The layout still depends on the available space too, because this determines the position
      // of the unclipped text rectangle.
      if Result.Right - Result.Left < TextWidth - 1 then
        if CurrentBidiMode = bdLeftToRight then
          CurrentAlignment := taLeftJustify
        else
          CurrentAlignment := taRightJustify;

      // Increase cell height (up to MaxUnclippedHeight determined above) if text does not fit.
      GetTextMetrics(Self.Canvas, TM);
      ExtraVerticalMargin := System.Math.Min(TM.tmHeight, MaxUnclippedHeight) - (Result.Bottom - Result.Top);
      if ExtraVerticalMargin > 0 then
        InflateRect(Result, 0, Divide(ExtraVerticalMargin + 1, 2));

      case CurrentAlignment of
        taCenter:
          begin
            Result.Left := Divide(Result.Left + Result.Right - TextWidth, 2);
            Result.Right := Result.Left + TextWidth;
          end;
        taRightJustify:
          Result.Left := Result.Right - TextWidth;
      else // taLeftJustify
        Result.Right := Result.Left + TextWidth - 1;
      end;
    end
    else
      // Modify rectangle only if the text fits entirely into the given room.
      if Result.Right - Result.Left > TextWidth then
        case CurrentAlignment of
          taCenter:
            begin
              Result.Left := Divide(Result.Left + Result.Right - TextWidth, 2);
              Result.Right := Result.Left + TextWidth;
            end;
          taRightJustify:
            Result.Left := Result.Right - TextWidth;
        else // taLeftJustify
          Result.Right := Result.Left + TextWidth;
        end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetEffectivelyFiltered(Node: PVirtualNode): Boolean;

// Checks if a node is effectively filtered out. This depends on the nodes state and the paint options.

begin
  if Assigned(Node) then
    Result := (vsFiltered in Node.States) and not (toShowFilteredNodes in FOptions.PaintOptions)
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetEffectivelyVisible(Node: PVirtualNode): Boolean;

begin
  Result := (vsVisible in Node.States) and not IsEffectivelyFiltered[Node];
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirst(ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the first node in the tree while optionally considering toChildrenAbove.

begin
  if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
  begin
    if vsHasChildren in FRoot.States then
    begin
      Result := FRoot;

      // Child nodes are the first choice if possible.
      if Assigned(Result.FirstChild) then
      begin
        while Assigned(Result.FirstChild) do
        begin
          Result := Result.FirstChild;
          if not (vsInitialized in Result.States) then
            InitNode(Result);

          if (vsHasChildren in Result.States) and (Result.ChildCount = 0) then
            InitChildren(Result);
        end;
      end
      else
        Result := nil;
    end
    else
      Result := nil;
  end
  else
    Result := FRoot.FirstChild;

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstChecked(State: TCheckState = csCheckedNormal;
  ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the first node in the tree with the given check state.

begin
  Result := GetNextChecked(nil, State, ConsiderChildrenAbove);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstChild(Node: PVirtualNode): PVirtualNode;

// Returns the first child of the given node. The result node is initialized before exit.

begin
  if (Node = nil) or (Node = FRoot) then
    Result := FRoot.FirstChild
  else
  begin
    if not (vsInitialized in Node.States) then
      InitNode(Node);
    if vsHasChildren in Node.States then
    begin
      if Node.ChildCount = 0 then
        InitChildren(Node);
      Result := Node.FirstChild;
    end
    else
      Result := nil;
  end;

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstChildNoInit(Node: PVirtualNode): PVirtualNode;
// Determines the first child of the given node but does not initialize it.

begin
  if (Node = nil) or (Node = FRoot) then
    Result := FRoot.FirstChild
  else
  begin
    if vsHasChildren in Node.States then
      Result := Node.FirstChild
    else
      Result := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstCutCopy(ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the first node in the tree which is currently marked for a clipboard operation.
// See also GetNextCutCopy for comments on initialization.

begin
  Result := GetNextCutCopy(nil, ConsiderChildrenAbove);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstInitialized(ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the first node which is already initialized.

begin
  Result := GetFirstNoInit(ConsiderChildrenAbove);
  if Assigned(Result) and not (vsInitialized in Result.States) then
    Result := GetNextInitialized(Result, ConsiderChildrenAbove);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstLeaf: PVirtualNode;

// Returns the first node in the tree which has currently no children.
// The result is initialized if necessary.

begin
  Result := GetNextLeaf(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstLevel(NodeLevel: Cardinal): PVirtualNode;

// Returns the first node in the tree on a specific level.
// The result is initialized if necessary.

begin
  Result := GetFirstNoInit(True);
  while Assigned(Result) and (GetNodeLevel(Result) <> NodeLevel) do
    Result := GetNextNoInit(Result, True);

  if Assigned(Result) and (GetNodeLevel(Result) <> NodeLevel) then // i.e. there is no node with the desired level in the tree
    Result := nil;

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstNoInit(ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the first node in the tree while optionally considering toChildrenAbove.
// No initialization is performed.

begin
  if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
  begin
    if vsHasChildren in FRoot.States then
    begin
      Result := FRoot;

      // Child nodes are the first choice if possible.
      if Assigned(Result.FirstChild) then
      begin
        while Assigned(Result.FirstChild) do
          Result := Result.FirstChild;
      end
      else
        Result := nil;
    end
    else
      Result := nil;
  end
  else
    Result := FRoot.FirstChild;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstSelected(ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the first node in the current selection while optionally considering toChildrenAbove.

begin
  Result := GetNextSelected(nil, ConsiderChildrenAbove);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstVisible(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = True;
  IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the first visible node in the tree while optionally considering toChildrenAbove.
// If necessary nodes are initialized on demand.

begin
  Result := Node;
  if not Assigned(Result) then
    Result := FRoot;

  if vsHasChildren in Result.States then
  begin
    if Result.ChildCount = 0 then
      InitChildren(Result);

    // Child nodes are the first choice if possible.
    if Assigned(Result.FirstChild) then
    begin
      Result := GetFirstChild(Result);

      if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
      begin
        repeat
          // Search the first visible sibling.
          while Assigned(Result.NextSibling) and not (vsVisible in Result.States) do
          begin
            Result := Result.NextSibling;
            // Init node on demand as this might change the visibility.
            if not (vsInitialized in Result.States) then
              InitNode(Result);
          end;

          // If there are no visible siblings take the parent.
          if not (vsVisible in Result.States) then
          begin
            Result := Result.Parent;
            if Result = FRoot then
              Result := nil;
            Break;
          end
          else
          begin
            if (vsHasChildren in Result.States) and (Result.ChildCount = 0) then
              InitChildren(Result);
            if (not Assigned(Result.FirstChild)) or (not (vsExpanded in Result.States)) then
              Break;
          end;

          Result := Result.FirstChild;
          if not (vsInitialized in Result.States) then
            InitNode(Result);
        until False;
      end
      else
      begin
        // If there are no children or the first child is not visible then search the sibling nodes or traverse parents.
        if not (vsVisible in Result.States) then
        begin
          repeat
            // Is there a next sibling?
            if Assigned(Result.NextSibling) then
            begin
              Result := Result.NextSibling;
              // The visible state can be removed during initialization so init the node first.
              if not (vsInitialized in Result.States) then
                InitNode(Result);
              if vsVisible in Result.States then
                Break;
            end
            else
            begin
              // No sibling anymore, so use the parent's next sibling.
              if Result.Parent <> FRoot then
                Result := Result.Parent
              else
              begin
                // There are no further nodes to examine, hence there is no further visible node.
                Result := nil;
                Break;
              end;
            end;
          until False;
        end;
      end;
    end
    else
      Result := nil;
  end
  else
    Result := nil;

  if Assigned(Result) and not IncludeFiltered and IsEffectivelyFiltered[Result] then
    Result := GetNextVisible(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstVisibleChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the first visible child node of Node. If necessary nodes are initialized on demand.

begin
  if Node = nil then
    Node := FRoot;
  Result := GetFirstChild(Node);

  if Assigned(Result) and (not (vsVisible in Result.States) or
     (not IncludeFiltered and IsEffectivelyFiltered[Result])) then
    Result := GetNextVisibleSibling(Result, IncludeFiltered);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstVisibleChildNoInit(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the first visible child node of Node.

begin
  if Node = nil then
    Node := FRoot;
  Result := Node.FirstChild;
  if Assigned(Result) and (not (vsVisible in Result.States) or
     (not IncludeFiltered and IsEffectivelyFiltered[Result])) then
    Result := GetNextVisibleSiblingNoInit(Result, IncludeFiltered);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstVisibleNoInit(Node: PVirtualNode = nil;
  ConsiderChildrenAbove: Boolean = True; IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the first visible node in the tree or given subtree while optionally considering toChildrenAbove.
// No initialization is performed.

begin
  Result := Node;
  if not Assigned(Result) then
    Result := FRoot;

  if vsHasChildren in Result.States then
  begin
    // Child nodes are the first choice if possible.
    if Assigned(Result.FirstChild) then
    begin
      Result := Result.FirstChild;

      if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
      begin
        repeat
          // Search the first visible sibling.
          while Assigned(Result.NextSibling) and not (vsVisible in Result.States) do
            Result := Result.NextSibling;

          // If there a no visible siblings take the parent.
          if not (vsVisible in Result.States) then
          begin
            Result := Result.Parent;
            if Result = FRoot then
              Result := nil;
            Break;
          end
          else
            if (not Assigned(Result.FirstChild)) or (not (vsExpanded in Result.States))then
              Break;

          Result := Result.FirstChild;
        until False;
      end
      else
      begin
        // If there are no children or the first child is not visible then search the sibling nodes or traverse parents.
        if not (vsVisible in Result.States) then
        begin
          repeat
            // Is there a next sibling?
            if Assigned(Result.NextSibling) then
            begin
              Result := Result.NextSibling;
              if vsVisible in Result.States then
                Break;
            end
            else
            begin
              // No sibling anymore, so use the parent's next sibling.
              if Result.Parent <> FRoot then
                Result := Result.Parent
              else
              begin
                // There are no further nodes to examine, hence there is no further visible node.
                Result := nil;
                Break;
              end;
            end;
          until False;
        end;
      end;
    end
    else
      Result := nil;
  end
  else
    Result := nil;

  if Assigned(Result) and not IncludeFiltered and IsEffectivelyFiltered[Result] then
    Result := GetNextVisibleNoInit(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.GetHitTestInfoAt(X, Y: TDimension; Relative: Boolean; var HitInfo: THitInfo; ShiftState: TShiftState=[]);

// Determines the node that occupies the specified point or nil if there's none. The parameter Relative determines
// whether to consider X and Y as being client coordinates (if True) or as being absolute tree coordinates.
// HitInfo is filled with flags describing the hit further.

var
  ColLeft,
  ColRight: TDimension;
  NodeTop: TDimension;
  InitialColumn,
  NextColumn: TColumnIndex;
  CurrentBidiMode: TBidiMode;
  CurrentAlignment: TAlignment;
  NodeRect: TRect;

begin
  HitInfo.HitNode := nil;
  HitInfo.HitPositions := [];
  HitInfo.HitColumn := NoColumn;

  if ShiftState=[] then
    ShiftState:= KeyboardStateToShiftState();
  HitInfo.ShiftState:= ShiftState;

  // Determine if point lies in the tree's client area.
  if X < 0 then
    Include(HitInfo.HitPositions, hiToLeft)
  else
    if X > Max(FRangeX, ClientWidth) then
      Include(HitInfo.HitPositions, hiToRight);

  if Y < 0 then
    Include(HitInfo.HitPositions, hiAbove)
  else
    if Y > Max(FRangeY, ClientHeight) then
      Include(HitInfo.HitPositions, hiBelow);

  // Convert position into absolute coordinate if necessary.
  if Relative then
  begin
    if X >= Header.Columns.GetVisibleFixedWidth then
      Inc(X, FEffectiveOffsetX);
    Inc(Y, -FOffsetY);
  end;
  HitInfo.HitPoint.X := X;
  HitInfo.HitPoint.Y := Y;

  // If the point is in the tree area then check the nodes.
  if HitInfo.HitPositions = [] then
  begin
    HitInfo.HitNode := GetNodeAt(X, Y, False, NodeTop);
    if HitInfo.HitNode = nil then
      Include(HitInfo.HitPositions, hiNowhere)
    else
    begin
      // At this point we need some info about the node, so it must be initialized.
      if not (vsInitialized in HitInfo.HitNode.States) then
        InitNode(HitInfo.HitNode);

      if FHeader.UseColumns then
      begin
        HitInfo.HitColumn := TVirtualTreeColumnsCracker(FHeader.Columns).GetColumnAndBounds(Point(X, Y), ColLeft, ColRight, False);
        // If auto column spanning is enabled then look for the last non empty column.
        if toAutoSpanColumns in FOptions.AutoOptions then
        begin
          InitialColumn := HitInfo.HitColumn;
          // Search to the left of the hit column for empty columns.
          while (HitInfo.HitColumn > NoColumn) and ColumnIsEmpty(HitInfo.HitNode, HitInfo.HitColumn) do
          begin
            NextColumn := FHeader.Columns.GetPreviousVisibleColumn(HitInfo.HitColumn);
            if NextColumn = InvalidColumn then
              Break;
            HitInfo.HitColumn := NextColumn;
            Dec(ColLeft, FHeader.Columns[NextColumn].Width);
          end;
          // Search to the right of the hit column for empty columns.
          repeat
            InitialColumn := FHeader.Columns.GetNextVisibleColumn(InitialColumn);
            if (InitialColumn = InvalidColumn) or not ColumnIsEmpty(HitInfo.HitNode, InitialColumn) then
              Break;
            Inc(ColRight, FHeader.Columns[InitialColumn].Width);
          until False;
        end;
        // Make the X position and the right border relative to the start of the column.
        Dec(X, ColLeft);
        Dec(ColRight, ColLeft);
      end
      else
      begin
        HitInfo.HitColumn := NoColumn;
        ColRight := Max(FRangeX, ClientWidth);
      end;
      ColLeft := 0;

      if HitInfo.HitColumn = InvalidColumn then
        Include(HitInfo.HitPositions, hiNowhere)
      else
      begin
        // From now on X is in "column" coordinates (relative to the left column border).
        HitInfo.HitPositions := [hiOnItem];

        // Avoid getting the display rect if this is not necessary.
        if toNodeHeightResize in FOptions.MiscOptions then
        begin
          NodeRect := GetDisplayRect(HitInfo.HitNode, HitInfo.HitColumn, False);
          if Y <= (NodeRect.Top - FOffsetY + 1) then
            Include(HitInfo.HitPositions, hiUpperSplitter)
          else
          if Y >= (NodeRect.Bottom - FOffsetY - 3) then
            Include(HitInfo.HitPositions, hiLowerSplitter);
        end;

        if HitInfo.HitColumn <= NoColumn then
        begin
          CurrentBidiMode := BidiMode;
          CurrentAlignment := Alignment;
        end
        else
        begin
          CurrentBidiMode := FHeader.Columns[HitInfo.HitColumn].BidiMode;
          CurrentAlignment := FHeader.Columns[HitInfo.HitColumn].Alignment;
        end;

        if CurrentBidiMode = bdLeftToRight then
          DetermineHitPositionLTR(HitInfo, X, ColRight, CurrentAlignment)
        else
          DetermineHitPositionRTL(HitInfo, X, ColRight, CurrentAlignment);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLast(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the very last node in the tree branch given by Node and initializes the nodes all the way down including the
// result. toChildrenAbove is optionally considered. By using Node = nil the very last node in the tree is returned.

var
  Next: PVirtualNode;

begin
  Result := GetLastChild(Node);
  if not ConsiderChildrenAbove or not (toChildrenAbove in FOptions.PaintOptions) then
    while Assigned(Result) do
    begin
      // Test if there is a next last child. If not keep the node from the last run.
      // Otherwise use the next last child.
      Next := GetLastChild(Result);
      if Next = nil then
        Break;
      Result := Next;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastInitialized(Node: PVirtualNode = nil;
  ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the very last initialized child node in the tree branch given by Node.

begin
  Result := GetLastNoInit(Node, ConsiderChildrenAbove);
  if Assigned(Result) and not (vsInitialized in Result.States) then
    Result := GetPreviousInitialized(Result, ConsiderChildrenAbove);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastNoInit(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the very last node in the tree branch given by Node without initialization.

var
  Next: PVirtualNode;

begin
  Result := GetLastChildNoInit(Node);
  if not ConsiderChildrenAbove or not (toChildrenAbove in FOptions.PaintOptions) then
    while Assigned(Result) do
    begin
      // Test if there is a next last child. If not keep the node from the last run.
      // Otherwise use the next last child.
      Next := GetLastChildNoInit(Result);
      if Next = nil then
        Break;
      Result := Next;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastChild(Node: PVirtualNode): PVirtualNode;

// Determines the last child of the given node and initializes it if there is one.

begin
  if (Node = nil) or (Node = FRoot) then
    Result := FRoot.LastChild
  else
  begin
    if not (vsInitialized in Node.States) then
      InitNode(Node);
    if vsHasChildren in Node.States then
    begin
      if Node.ChildCount = 0 then
        InitChildren(Node);
      Result := Node.LastChild;
    end
    else
      Result := nil;
  end;

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastChildNoInit(Node: PVirtualNode): PVirtualNode;

// Determines the last child of the given node but does not initialize it.

begin
  if (Node = nil) or (Node = FRoot) then
    Result := FRoot.LastChild
  else
  begin
    if vsHasChildren in Node.States then
      Result := Node.LastChild
    else
      Result := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastSelected(ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the last node in the current selection while optionally considering toChildrenAbove.

begin
  Result := GetPreviousSelected(nil, ConsiderChildrenAbove);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastVisible(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = True;
  IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the very last visible node in the tree while optionally considering toChildrenAbove.
// The nodes are intialized all the way up including the result node.

var
  Run: PVirtualNode;

begin
  Result := GetLastVisibleNoInit(Node, ConsiderChildrenAbove);

  Run := Result;
  while Assigned(Run) and (Run <> Node)  and (Run <> RootNode) do
  begin
    if not (vsInitialized in Run.States) then
      InitNode(Run);
    Run := Run.Parent;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastVisibleChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

// Determines the last visible child of the given node and initializes it if necessary.

begin
  if (Node = nil) or (Node = FRoot) then
    Result := GetLastChild(FRoot)
  else
    if FullyVisible[Node] and (vsExpanded in Node.States) then
      Result := GetLastChild(Node)
    else
      Result := nil;

  if Assigned(Result) and (not (vsVisible in Result.States) or
     (not IncludeFiltered and IsEffectivelyFiltered[Result])) then
    Result := GetPreviousVisibleSibling(Result, IncludeFiltered);

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastVisibleChildNoInit(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

// Determines the last visible child of the given node without initialization.

begin
  if (Node = nil) or (Node = FRoot) then
    Result := GetLastChildNoInit(FRoot)
  else
    if FullyVisible[Node] and (vsExpanded in Node.States) then
      Result := GetLastChildNoInit(Node)
    else
      Result := nil;

  if Assigned(Result) and (not (vsVisible in Result.States) or
     (not IncludeFiltered and IsEffectivelyFiltered[Result])) then
    Result := GetPreviousVisibleSiblingNoInit(Result, IncludeFiltered);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetLastVisibleNoInit(Node: PVirtualNode = nil;
  ConsiderChildrenAbove: Boolean = True; IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the very last visible node in the tree while optionally considering toChildrenAbove.
// Note that the visibility of all ancestor nodes of the resulting node must not be considered.
// No initialization is performed.

  //--------------- local functions -------------------------------------------

  function GetNodeIsVisible(ChildNode: PVirtualNode): Boolean;
  begin
    Result := (vsVisible in ChildNode.States) and
              (IncludeFiltered or not IsEffectivelyFiltered[ChildNode]);
  end;

  function GetNodeHasVisibleChildren(ChildNode: PVirtualNode): Boolean;
  begin
    Result := (vsHasChildren in ChildNode.States) and
              (vsExpanded in ChildNode.States) and
              not (vsAllChildrenHidden in ChildNode.States);
  end;

  function IterateChildren(ParentNode: PVirtualNode): PVirtualNode;
  var
    Run: PVirtualNode;
  begin
    Result := nil;

    Run := GetLastChildNoInit(ParentNode); // Do not use 'GetLastVisibleChildNoInit' here (see above).
    while Assigned(Run) do
    begin
      if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
      begin
        if GetNodeIsVisible(Run) then
          Result := Run
        else if GetNodeHasVisibleChildren(Run) then
          Result := IterateChildren(Run);
      end else
      begin
        if GetNodeHasVisibleChildren(Run) then
          Result := IterateChildren(Run)
        else if GetNodeIsVisible(Run) then
          Result := Run;
      end;

      if Assigned(Result) then
        break;

      Run := GetPreviousSiblingNoInit(Run);
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  Run: PVirtualNode;

begin
  Result := nil;

  // First, check wether the given node and all its parents are expanded.
  // If not, there can not be any visible child node.
  Run := Node;
  while Assigned(Run) and (Run <> RootNode) do
  begin
    if not (vsExpanded in Run.States) then
      exit;
    Run := Run.Parent;
  end;

  Result := IterateChildren(Node);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetMaxColumnWidth(Column: TColumnIndex; UseSmartColumnWidth: Boolean = False): TDimension;

// This method determines the width of the largest node in the given column.
// If UseSmartColumnWidth is True then only the visible nodes which are in view will be considered
// Note: If UseSmartColumnWidth is False then every visible node in the tree will be initialized contradicting so
//       the virtual paradigm.

var
  Run,
  LastNode,
  NextNode: PVirtualNode;
  TextLeft,
  CurrentWidth: TDimension;
  lOffsets: TVTOffsets;
begin
  if OperationCanceled then
  begin
    // Behave non-destructive.
    Result := FHeader.Columns[Column].Width;
    Exit;
  end
  else
    Result := 0;

  StartOperation(okGetMaxColumnWidth);
  try
    if Assigned(FOnBeforeGetMaxColumnWidth) then
      FOnBeforeGetMaxColumnWidth(FHeader, Column, UseSmartColumnWidth);

    if UseSmartColumnWidth then // Get first visible node which is in view.
      Run := GetTopNode
    else
      Run := GetFirstVisible(nil, True);

    // Decide where to stop.
    if UseSmartColumnWidth then
      LastNode := GetNextVisible(BottomNode)
    else
      LastNode := nil;

    if hoAutoResizeInclCaption in FHeader.Options then
      Result := Result + (2 * Header.Columns[Column].Margin + Header.Columns[Column].CaptionWidth + 2);

    while Assigned(Run) and not OperationCanceled do
    begin
      GetOffsets(Run, lOffsets, TVTElement.ofsLabel, Column);
      TextLeft := lOffsets[TVTElement.ofsLabel];
      CurrentWidth := DoGetNodeWidth(Run, Column);
      Inc(CurrentWidth, DoGetNodeExtraWidth(Run, Column));
      Inc(CurrentWidth, DoGetCellContentMargin(Run, Column).X);

      // Background for fix:
      // DoGetNodeWidth works correctly to return just the
      // headerwidth in vsMultiline state of the node. But the
      // following code was adding TextLeft unnecessarily. This
      // caused a width increase each time a column splitter
      // was double-clicked for the option hoDblClickResize that
      // really does not apply for vsMultiline case.
      // Fix: If the node is multiline, leave the current width as
      // it is as returned by DoGetNodeWidth logic above.
      if (Column > NoColumn) and (vsMultiline in Run.States) then
        Result := CurrentWidth
      else
      if Result < (TextLeft + CurrentWidth) then
        Result := TextLeft + CurrentWidth;

      // Get next visible node and update left node position if needed.
      NextNode := GetNextVisible(Run, True);
      if NextNode = LastNode then
        Break;
      Run := NextNode;
    end;
    if toShowVertGridLines in FOptions.PaintOptions then
      Inc(Result);

    if Assigned(FOnAfterGetMaxColumnWidth) then
      FOnAfterGetMaxColumnWidth(FHeader, Column, Result);

  finally
    EndOperation(okGetMaxColumnWidth);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNext(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns next node in tree while optionally considering toChildrenAbove. The Result will be initialized if needed.

begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the hidden root node.');

    if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
    begin
      // If this node has no siblings use the parent.
      if not Assigned(Result.NextSibling) then
      begin
        Result := Result.Parent;
        if Result = FRoot then
        begin
          Result := nil;
        end;
      end
      else
      begin
        // There is at least one sibling so take it.
        Result := Result.NextSibling;

        // Has this node got children? Initialize them if necessary.
        if (vsHasChildren in Result.States) and (Result.ChildCount = 0) then
          InitChildren(Result);

        // Now take a look at the children.
        while Assigned(Result.FirstChild) do
        begin
          Result := Result.FirstChild;
          if (vsHasChildren in Result.States) and (Result.ChildCount = 0) then
            InitChildren(Result);
        end;
      end;
    end
    else
    begin
      // Has this node got children?
      if vsHasChildren in Result.States then
      begin
        // Yes, there are child nodes. Initialize them if necessary.
        if Result.ChildCount = 0 then
          InitChildren(Result);
      end;

      // if there is no child node try siblings
      if Assigned(Result.FirstChild) then
        Result := Result.FirstChild
      else
      begin
        repeat
          // Is there a next sibling?
          if Assigned(Result.NextSibling) then
          begin
            Result := Result.NextSibling;
            Break;
          end
          else
          begin
            // No sibling anymore, so use the parent's next sibling.
            if Result.Parent <> FRoot then
              Result := Result.Parent
            else
            begin
              // There are no further nodes to examine, hence there is no further visible node.
              Result := nil;
              Break;
            end;
          end;
        until False;
      end;
    end;
  end;

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextChecked(Node: PVirtualNode; State: TCheckState = csCheckedNormal;
  ConsiderChildrenAbove: Boolean = False): PVirtualNode;

begin
  if (Node = nil) or (Node = FRoot) then
    Result := GetFirstNoInit(ConsiderChildrenAbove)
  else
    Result := GetNextNoInit(Node, ConsiderChildrenAbove);

  while Assigned(Result) and (GetCheckState(Result) <> State) do
    Result := GetNextNoInit(Result, ConsiderChildrenAbove);

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextChecked(Node: PVirtualNode; ConsiderChildrenAbove: Boolean): PVirtualNode;
begin
  Result := Self.GetNextChecked(Node, csCheckedNormal, ConsiderChildrenAbove);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextCutCopy(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the next node in the tree which is currently marked for a clipboard operation. Since only visible nodes can
// be marked (or they are hidden after they have been marked) it is not necessary to initialize nodes to check for
// child nodes. The result, however, is initialized if necessary.

begin
  if ClipboardStates * FStates <> [] then
  begin
    if (Node = nil) or (Node = FRoot) then
      Result := GetFirstNoInit(ConsiderChildrenAbove)
    else
      Result := GetNextNoInit(Node, ConsiderChildrenAbove);
    while Assigned(Result) and not (vsCutOrCopy in Result.States) do
      Result := GetNextNoInit(Result, ConsiderChildrenAbove);
    if Assigned(Result) and not (vsInitialized in Result.States) then
      InitNode(Result);
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextInitialized(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the next node in tree which is initialized.

begin
  Result := Node;
  repeat
    Result := GetNextNoInit(Result, ConsiderChildrenAbove);
  until (Result = nil) or (vsInitialized in Result.States);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextLeaf(Node: PVirtualNode): PVirtualNode;

// Returns the next node in the tree which has currently no children.
// The result is initialized if necessary.

begin
  if (Node = nil) or (Node = FRoot) then
    Result := FRoot.FirstChild
  else
    Result := GetNext(Node);
  while Assigned(Result) and (vsHasChildren in Result.States) do
    Result := GetNext(Result);
  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextLevel(Node: PVirtualNode; NodeLevel: Cardinal): PVirtualNode;

// Returns the next node in the tree on a specific level.
// The result is initialized if necessary.

var
  StartNodeLevel: Cardinal;

begin
  Result := nil;

  if Assigned(Node) and (Node <> FRoot) then
  begin
    StartNodeLevel := GetNodeLevel(Node);

    if StartNodeLevel < NodeLevel then
    begin
      Result := GetNext(Node);
      if Assigned(Result) and (GetNodeLevel(Result) <> NodeLevel) then
        Result := GetNextLevel(Result, NodeLevel);
    end
    else
      if StartNodeLevel = NodeLevel then
      begin
        Result := Node.NextSibling;
        if not Assigned(Result) then // i.e. start node was a last sibling
        begin
          Result := Node.Parent;
          if Assigned(Result) then
          begin
            // go to next anchestor of the start node which has a next sibling (if exists)
            while Assigned(Result) and not Assigned(Result.NextSibling) do
              Result := Result.Parent;
            if Assigned(Result) then
              Result := GetNextLevel(Result.NextSibling, NodeLevel);
          end;
        end;
      end
      else
        // i.e. StartNodeLevel > NodeLevel
        Result := GetNextLevel(Node.Parent, NodeLevel);
  end;

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextNoInit(Node: PVirtualNode; ConsiderChildrenAbove: Boolean): PVirtualNode;

// Optimized version of GetNext performing no initialization, but optionally considering toChildrenAbove.

begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the hidden root node.');

    if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
    begin
      // If this node has no siblings use the parent.
      if not Assigned(Result.NextSibling) then
      begin
        Result := Result.Parent;
        if Result = FRoot then
        begin
          Result := nil;
        end;
      end
      else
      begin
        // There is at least one sibling so take it.
        Result := Result.NextSibling;

        // Now take a look at the children.
        while Assigned(Result.FirstChild) do
        begin
          Result := Result.FirstChild;
        end;
      end;
    end
    else
    begin
      // If there is no child node try siblings.
      if Assigned(Result.FirstChild) then
        Result := Result.FirstChild
      else
      begin
        repeat
          // Is there a next sibling?
          if Assigned(Result.NextSibling) then
          begin
            Result := Result.NextSibling;
            Break;
          end
          else
          begin
            // No sibling anymore, so use the parent's next sibling.
            if Result.Parent <> FRoot then
              Result := Result.Parent
            else
            begin
              // There are no further nodes to examine, hence there is no further visible node.
              Result := nil;
              Break;
            end;
          end;
        until False;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextSelected(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the next node in the tree which is currently selected. Since children of unitialized nodes cannot be
// in the current selection (because they simply do not exist yet) it is not necessary to initialize nodes here.
// The result however is initialized if necessary.

begin
  if FSelectionCount > 0 then
  begin
    if (Node = nil) or (Node = FRoot) then
      Result := GetFirstNoInit(ConsiderChildrenAbove)
    else
      Result := GetNextNoInit(Node, ConsiderChildrenAbove);
    while Assigned(Result) and not (vsSelected in Result.States) do
      Result := GetNextNoInit(Result, ConsiderChildrenAbove);
    if Assigned(Result) and not (vsInitialized in Result.States) then
      InitNode(Result);
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextSibling(Node: PVirtualNode): PVirtualNode;

// Returns the next sibling of Node and initializes it if necessary.

begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the hidden root node.');

    Result := Result.NextSibling;
    if Assigned(Result) and not (vsInitialized in Result.States) then
      InitNode(Result);
  end;
end;

function TBaseVirtualTree.GetNextSiblingNoInit(Node: PVirtualNode): PVirtualNode;

// Returns the next sibling of Node.

begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the hidden root node.');

    Result := Result.NextSibling;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextVisible(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = True): PVirtualNode;

// Returns next node in tree, with regard to Node, which is visible.
// Nodes which need an initialization (including the result) are initialized.
// toChildrenAbove is optionally considered which is the default here.

var
  TopInvisibleParent: PVirtualNode;
  ForceSearch: Boolean;

begin
  Result := Node;
  if not Assigned(Result) then Exit;

  Assert(Result <> FRoot, 'Node must not be the hidden root node.');

  repeat
    // If any ancestor is invisible, then find the last (furthest) parent node
    // which is invisible to skip invisible subtrees. Otherwise we will
    // likely go unnecessarily through a whole bunch of invisible nodes.
    TopInvisibleParent := GetTopInvisibleParent(Result);
    if Assigned(TopInvisibleParent) then
      Result := TopInvisibleParent;

    if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
    begin
      repeat
        // If there a no siblings anymore, go up one level.
        if not Assigned(Result.NextSibling) then
        begin
          Result := Result.Parent;
          if Result = FRoot then
          begin
            Result := nil;
            Break;
          end;

          if not (vsInitialized in Result.States) then
            InitNode(Result);
        end
        else
        begin
          // There is at least one sibling so take it.
          Result := Result.NextSibling;
          if not (vsInitialized in Result.States) then
            InitNode(Result);
          if not (vsVisible in Result.States) then
            Continue;

          // Now take a look at the children. As the children are initialized
          // while toggling, we don't need to call 'InitChildren' beforehand here.
          while (vsExpanded in Result.States) and Assigned(Result.FirstChild) do
          begin
            Result := Result.FirstChild;
            if not (vsInitialized in Result.States) then
              InitNode(Result);
            if not (vsVisible in Result.States) then
              Break;
          end;
        end;

        // If we found a visible node we don't need to search any longer.
        // As it has already been initialized above, we don't need to call 'InitNode' here.
        if vsVisible in Result.States then
          Break;
      until False;
    end
    else
    begin
      ForceSearch := True;
      // If we found an invisible ancestor, we must not check its children.
      // Remember, that TopInvisibleParent can be effectively invisible merely due to
      // its own parent's expansion state despite being visible itself.
      if Result <> TopInvisibleParent then
      begin
        if not (vsInitialized in Result.States) then
          InitNode(Result);

        // Child nodes are the first choice if the current node is known to be visible.
        if (vsVisible in Result.States) and (vsExpanded in Result.States) then
        begin
          // Initialize the node's children if necessary.
          if (vsHasChildren in Result.States) and (Result.ChildCount = 0) then
              InitChildren(Result);

          if Assigned(Result.FirstChild) then
          begin
            Result := Result.FirstChild;
            if not (vsInitialized in Result.States) then
              InitNode(Result);
            ForceSearch := False;
          end;
        end;
      end;

      // If there are no children or the first child is not visible then search the sibling nodes or traverse parents.
      if ForceSearch or not (vsVisible in Result.States) then
      begin
        repeat
          // Is there a next sibling?
          if Assigned(Result.NextSibling) then
          begin
            Result := Result.NextSibling;
            if not (vsInitialized in Result.States) then
              InitNode(Result);
            if vsVisible in Result.States then
              Break;
          end
          // No sibling anymore, so use the parent's next sibling.
          else if Result.Parent <> FRoot then
            Result := Result.Parent
          else
          begin
            // There are no further nodes to examine, hence there is no further visible node.
            Result := nil;
            Break;
          end;
        until False;
      end;
    end;
  until not Assigned(Result) or IsEffectivelyVisible[Result];

  Assert(Result <> Node, 'Node cannot be its own visible successor.');
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextVisibleNoInit(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = True): PVirtualNode;

// Returns the next node in tree, with regard to Node, which is visible.
// No initialization is done.
// toChildrenAbove is optionally considered which is the default here.

var
  TopInvisibleParent: PVirtualNode;
  ForceSearch: Boolean;

begin
  Result := Node;
  if not Assigned(Result) then Exit;

  Assert(Result <> FRoot, 'Node must not be the hidden root node.');

  repeat
    // If any ancestor is invisible, then find the last (furthest) parent node
    // which is invisible to skip invisible subtrees. Otherwise we will
    // likely go unnecessarily through a whole bunch of invisible nodes.
    TopInvisibleParent := GetTopInvisibleParent(Result);
    if Assigned(TopInvisibleParent) then
      Result := TopInvisibleParent;

    if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
    begin
      repeat
        // If there are no siblings anymore, go up one level.
        if not Assigned(Result.NextSibling) then
        begin
          Result := Result.Parent;
          if Result = FRoot then
          begin
            Result := nil;
            Break;
          end;
        end
        else
        begin
          // There is at least one sibling so take it.
          Result := Result.NextSibling;
          if not (vsVisible in Result.States) then
            Continue;

          // Now take a look at the children.
          while (vsExpanded in Result.States) and Assigned(Result.FirstChild) do
          begin
            Result := Result.FirstChild;
            if not (vsVisible in Result.States) then
              Break;
          end;
        end;

        // If we found a visible node we don't need to search any longer.
        if vsVisible in Result.States then
          Break;
      until False;
    end
    else
    begin
      // Child nodes are the first choice if the current node is known to be visible.
      // Remember, that TopInvisibleParent can be effectively invisible merely due to
      // its own parent's expansion state despite being visible itself.
      if (vsVisible in Result.States) and (vsExpanded in Result.States) and
         (Result <> TopInvisibleParent) and Assigned(Result.FirstChild) then
      begin
        Result := Result.FirstChild;
        ForceSearch := False;
      end else
        ForceSearch := True;

      // If there are no children or the first child is not visible then search the sibling nodes or traverse parents.
      if ForceSearch or not (vsVisible in Result.States) then
      begin
        repeat
          // Is there a next sibling?
          if Assigned(Result.NextSibling) then
          begin
            Result := Result.NextSibling;
            if vsVisible in Result.States then
              Break;
          end
          // No sibling anymore, so use the parent's next sibling.
          else if Result.Parent <> FRoot then
            Result := Result.Parent
          else
          begin
            // There are no further nodes to examine, hence there is no further visible node.
            Result := nil;
            Break;
          end;
        until False;
      end;
    end;
  until not Assigned(Result) or IsEffectivelyVisible[Result];

  Assert(Result <> Node, 'Node cannot be its own visible successor.');
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextVisibleSibling(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the next visible sibling after Node. Initialization is done implicitly.

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := GetNextSibling(Result);
  until not Assigned(Result) or ((vsVisible in Result.States) and
        (IncludeFiltered or not IsEffectivelyFiltered[Result]));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNextVisibleSiblingNoInit(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the next visible sibling after Node.

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := Result.NextSibling;
  until not Assigned(Result) or ((vsVisible in Result.States) and
       (IncludeFiltered or not IsEffectivelyFiltered[Result]));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNodeAt(X, Y: TDimension): PVirtualNode;

// Overloaded variant of GetNodeAt to easy life of application developers which do not need to have the exact
// top position returned and always use client coordinates.

var
  Dummy: TDimension;

begin
  Result := GetNodeAt(X, Y, True, Dummy);
end;

function TBaseVirtualTree.GetNodeAt(const P: TPoint): PVirtualNode;
begin
  Result := GetNodeAt(P.X, P.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNodeAt(X, Y: TDimension; Relative: Boolean; var NodeTop: TDimension): PVirtualNode;

// This method returns the node that occupies the specified point, or nil if there's none.
// If Releative is True then X and Y are given in client coordinates otherwise they are considered as being
// absolute values into the virtual tree image (regardless of the current offsets in the tree window).
// NodeTop gets the absolute or relative top position of the node returned or is untouched if no node
// could be found.

var
  AbsolutePos,
  CurrentPos: TNodeHeight;

begin
  if Y < 0 then
    Y := 0;

  AbsolutePos := Y;
  if Relative then
    Inc(AbsolutePos, -FOffsetY);

  // CurrentPos tracks a running term of the current position to test for.
  // It corresponds always to the top position of the currently considered node.
  CurrentPos := 0;

  // If the cache is available then use it.
  if tsUseCache in FStates then
    Result := FindInPositionCache(AbsolutePos, CurrentPos)
  else
    Result := GetFirstVisibleNoInit(nil, True);

  // Determine node, of which position and height corresponds to the scroll position most closely.
  while Assigned(Result) and (Result <> FRoot) do
  begin
    if AbsolutePos < (CurrentPos + NodeHeight[Result]) then
      Break;
    Inc(CurrentPos, NodeHeight[Result]);
    Result := GetNextVisibleNoInit(Result, True);
  end;

  if Result = FRoot then
    Result := nil;

  // Since the given vertical position is likely not the same as the top position
  // of the found node this top position is returned.
  if Assigned(Result) then
  begin
    NodeTop := CurrentPos;
    if Relative then
      Inc(NodeTop, FOffsetY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------


function TBaseVirtualTree.GetNodeData(Node: PVirtualNode): Pointer;

// Returns the address of the user defined data area in the node.

begin
  Assert((FNodeDataSize > 0) or not Assigned(Node), 'NodeDataSize not initialized.');
  if (FNodeDataSize <= 0) or (Node = nil) or (Node = FRoot) then
    Result := nil
  else
  begin
    Result := Node.GetData();
    Include(Node.States, vsOnFreeNodeCallRequired); // We now need to call OnFreeNode, see bug #323
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNodeData<T>(pNode: PVirtualNode): T;

// Returns the associated data converted to the class given in the generic part of the function.

var
  P: Pointer;
begin
  P := Self.GetNodeData(pNode);
  if Assigned(P) then
    Exit(T(P^))
  else
    Exit(Default(T));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetInterfaceFromNodeData<T>(pNode: PVirtualNode): T;
begin
  if Assigned(pNode) then
    Result := T(Self.GetNodeData(pNode)^)
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNodeDataAt<T>(pXCoord, pYCoord: Integer): T;

// Returns the associated data at the specified coordinates converted to the type given in the generic part of the function.

var
  lNode: PVirtualNode;
begin
  lNode := GetNodeAt(pXCoord, pYCoord);
  Result := Self.GetNodeData<T>(lNode);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetFirstSelectedNodeData<T>(): T;

// Returns of the first selected node associated data converted to the type given in the generic part of the function.

begin
  Result := Self.GetNodeData<T>(GetFirstSelected());
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetNodeLevel(Node: PVirtualNode): Cardinal;

// returns the level of the given node

var
  Run: PVirtualNode;

begin
  Result := 0;
  if Assigned(Node) and (Node <> FRoot) then
  begin
    Run := Node.Parent;
    while Run <> FRoot do
    begin
      Run := Run.Parent;
      System.Inc(Result);
    end;
  end;
end;


//----------------------------------------------------------------------------------------------------------------------
// Function introduced to avoid spaghetti code to fix setting of FLastSelectionLevel
// at various places that now needs to avoid setting it for a disabled node
function TBaseVirtualTree.GetNodeLevelForSelectConstraint(Node: PVirtualNode): integer;
begin
  if Assigned(Node) and not (vsDisabled in Node.States) then
    result := GetNodeLevel(Node)
  else
    result := -1;
end;


//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPrevious(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns previous node in tree. If ConsiderChildrenAbove is True the function considers
// whether toChildrenAbove is currently set, otherwise the result will always be the previous
// node in top-down order regardless of the current PaintOptions.
// The Result will be initialized if needed.

var
  Run: PVirtualNode;

begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the hidden root node.');

    if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
    begin
      // Has this node got children? Initialize them if necessary.
      if (vsHasChildren in Result.States) and (Result.ChildCount = 0) then
        InitChildren(Result);

      // If there is a last child, take it; if not try the previous sibling.
      if Assigned(Result.LastChild) then
        Result := Result.LastChild
      else
        if Assigned(Result.PrevSibling) then
           Result := Result.PrevSibling
      else
      begin
        // If neither a last child nor a previous sibling exist, go the tree upwards and
        // look, wether one of the parent nodes have a previous sibling. If not the result
        // will ne nil.
        repeat
          Result := Result.Parent;
          Run := nil;
          if Result <> FRoot then
            Run := Result.PrevSibling
          else
            Result := nil;
        until Assigned(Run) or (Result = nil);

        if Assigned(Run) then
          Result := Run;
      end;
    end
    else
    begin
      // Is there a previous sibling?
      if Assigned(Node.PrevSibling) then
      begin
        // Go down and find the last child node.
        Result := GetLast(Node.PrevSibling);
        if Result = nil then
          Result := Node.PrevSibling;
      end
      else
        // no previous sibling so the parent of the node is the previous visible node
        if Node.Parent <> FRoot then
          Result := Node.Parent
        else
          Result := nil;
    end;
  end;

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousChecked(Node: PVirtualNode; State: TCheckState = csCheckedNormal;
  ConsiderChildrenAbove: Boolean = False): PVirtualNode;

begin
  if (Node = nil) or (Node = FRoot) then
    Result := GetLastNoInit(nil, ConsiderChildrenAbove)
  else
    Result := GetPreviousNoInit(Node, ConsiderChildrenAbove);

  while Assigned(Result) and (GetCheckState(Result) <> State) do
    Result := GetPreviousNoInit(Result, ConsiderChildrenAbove);

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousCutCopy(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the previous node in the tree which is currently marked for a clipboard operation. Since only visible nodes can
// be marked (or they are hidden after they have been marked) it is not necessary to initialize nodes to check for
// child nodes. The result, however, is initialized if necessary.

begin
  if ClipboardStates * FStates <> [] then
  begin
    if (Node = nil) or (Node = FRoot) then
      Result := GetLastNoInit(nil, ConsiderChildrenAbove)
    else
      Result := GetPreviousNoInit(Node, ConsiderChildrenAbove);
    while Assigned(Result) and not (vsCutOrCopy in Result.States) do
      Result := GetPreviousNoInit(Result, ConsiderChildrenAbove);
    if Assigned(Result) and not (vsInitialized in Result.States) then
      InitNode(Result);
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousInitialized(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the previous node in tree which is initialized.

begin
  Result := Node;
  repeat
    Result := GetPreviousNoInit(Result, ConsiderChildrenAbove);
  until (Result = nil) or (vsInitialized in Result.States);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousLeaf(Node: PVirtualNode): PVirtualNode;

// Returns the previous node in the tree which has currently no children.
// The result is initialized if necessary.

begin
  if (Node = nil) or (Node = FRoot) then
    Result := FRoot.LastChild
  else
    Result := GetPrevious(Node);
  while Assigned(Result) and (vsHasChildren in Result.States) do
    Result := GetPrevious(Result);
  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousLevel(Node: PVirtualNode; NodeLevel: Cardinal): PVirtualNode;

// Returns the previous node in the tree on a specific level.
// The result is initialized if necessary.

var
  StartNodeLevel: Cardinal;
  Run: PVirtualNode;

begin
  Result := nil;

  if Assigned(Node) and (Node <> FRoot) then
  begin
    StartNodeLevel := GetNodeLevel(Node);

    if StartNodeLevel < NodeLevel then
    begin
      Result := Node.PrevSibling;
      if Assigned(Result) then
      begin
        // go to last descendant of previous sibling with desired node level (if exists)
        Run := Result;
        while Assigned(Run) and (GetNodeLevel(Run) < NodeLevel) do
        begin
          Result := Run;
          Run := GetLastChild(Run);
        end;
        if Assigned(Run) and (GetNodeLevel(Run) = NodeLevel) then
          Result := Run
        else
        begin
          if Assigned(Result.PrevSibling) then
            Result := GetPreviousLevel(Result, NodeLevel)
          else
            if Assigned(Result) and (Result.Parent <> FRoot) then
              Result := GetPreviousLevel(Result.Parent, NodeLevel)
          else
            Result := nil;
        end;
      end
      else
        Result := GetPreviousLevel(Node.Parent, NodeLevel);
    end
    else
      if StartNodeLevel = NodeLevel then
      begin
        Result := Node.PrevSibling;
        if not Assigned(Result) then // i.e. start node was a first sibling
        begin
          Result := Node.Parent;
          if Assigned(Result) then
            Result := GetPreviousLevel(Result, NodeLevel);
        end;
      end
      else // i.e. StartNodeLevel > NodeLevel
        Result := GetPreviousLevel(Node.Parent, NodeLevel);
  end;

  if Assigned(Result) and not (vsInitialized in Result.States) then
    InitNode(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousNoInit(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns previous node in tree, optionally considering toChildrenAbove. No initialization is performed.

var
  Run: PVirtualNode;

begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the hidden root node.');

    if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
    begin
      // If there is a last child, take it; if not try the previous sibling.
      if Assigned(Result.LastChild) then
        Result := Result.LastChild
      else
        if Assigned(Result.PrevSibling) then
          Result := Result.PrevSibling
        else
        begin
          // If neither a last child nor a previous sibling exist, go the tree upwards and
          // look, wether one of the parent nodes have a previous sibling. If not the result
          // will ne nil.
          repeat
            Result := Result.Parent;
            Run := nil;
            if Result <> FRoot then
              Run := Result.PrevSibling
            else
              Result := nil;
          until Assigned(Run) or (Result = nil);

          if Assigned(Run) then
            Result := Run;
        end;
    end
    else
    begin
      // Is there a previous sibling?
      if Assigned(Node.PrevSibling) then
      begin
        // Go down and find the last child node.
        Result := GetLastNoInit(Node.PrevSibling);
        if Result = nil then
          Result := Node.PrevSibling;
      end
      else
        // No previous sibling so the parent of the node is the previous node.
        if Node.Parent <> FRoot then
          Result := Node.Parent
        else
          Result := nil;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousSelected(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = False): PVirtualNode;

// Returns the previous node in the tree which is currently selected. Since children of unitialized nodes cannot be
// in the current selection (because they simply do not exist yet) it is not necessary to initialize nodes here.
// The result however is initialized if necessary.

begin
  if FSelectionCount > 0 then
  begin
    if (Node = nil) or (Node = FRoot) then
      Result := GetLastNoInit(nil, ConsiderChildrenAbove)
    else
      Result := GetPreviousNoInit(Node, ConsiderChildrenAbove);
    while Assigned(Result) and not (vsSelected in Result.States) do
      Result := GetPreviousNoInit(Result, ConsiderChildrenAbove);
    if Assigned(Result) and not (vsInitialized in Result.States) then
      InitNode(Result);
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousSibling(Node: PVirtualNode): PVirtualNode;

// Returns the previous sibling of Node and initializes it if necessary.

begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the hidden root node.');

    Result := Result.PrevSibling;
    if Assigned(Result) and not (vsInitialized in Result.States) then
      InitNode(Result);
  end;
end;

function TBaseVirtualTree.GetPreviousSiblingNoInit(Node: PVirtualNode): PVirtualNode;

// Returns the previous sibling of Node

begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the hidden root node.');

    Result := Result.PrevSibling;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousVisible(Node: PVirtualNode; ConsiderChildrenAbove: Boolean = True): PVirtualNode;

// Returns the previous node in tree, with regard to Node, which is visible.
// Nodes which need an initialization (including the result) are initialized.
// toChildrenAbove is optionally considered which is the default here.

var
  TopInvisibleParent: PVirtualNode;
  ForceSearch: Boolean;

begin
  Result := Node;
  if not Assigned(Result) then Exit;

  Assert(Result <> FRoot, 'Node must not be the hidden root node.');

  repeat
    // If any ancestor is invisible, then find the last (furthest) parent node
    // which is invisible to skip invisible subtrees. Otherwise we will
    // likely go unnecessarily through a whole bunch of invisible nodes.
    TopInvisibleParent := GetTopInvisibleParent(Result);
    if Assigned(TopInvisibleParent) then
      Result := TopInvisibleParent;

    if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
    begin
      ForceSearch := True;
      // If we found an invisible ancestor, we must not check its children.
      // Remember, that TopInvisibleParent can be effectively invisible merely due to
      // its own parent's expansion state despite being visible itself.
      if Result <> TopInvisibleParent then
      begin
        if not (vsInitialized in Result.States) then
          InitNode(Result);

        if (vsVisible in Result.States) and (vsExpanded in Result.States) then
        begin
          // Initialiue the node's children if necessary.
          if (vsHasChildren in Result.States) and (Result.ChildCount = 0) then
            InitChildren(Result);

          // Child nodes are the first choice if the current node is known to be visible.
          if Assigned(Result.LastChild) then
          begin
            Result := Result.LastChild;
            if not (vsInitialized in Result.States) then
              InitNode(Result);
            ForceSearch := False;
          end;
        end;
      end;

      if ForceSearch or not (vsVisible in Result.States) then
      begin
        repeat
          // Is there a previous sibling?
          if Assigned(Result.PrevSibling) then
          begin
            Result := Result.PrevSibling;
            if not (vsInitialized in Result.States) then
              InitNode(Result);
            if vsVisible in Result.States then
              Break;
          end
          // No sibling anymore, so use the parent's previous sibling.
          else if Result.Parent <> FRoot then
            Result := Result.Parent
          // There are no further nodes to examine, hence there is no further visible node.
          else
          begin
            Result := nil;
            Break;
          end;
        until False;
      end;
    end
    else
    begin
      repeat
        // If there are no sibling anymore, go up one level.
        if not Assigned(Result.PrevSibling) then
        begin
          Result := Result.Parent;
          if Result = FRoot then
          begin
            Result := nil;
            Break;
          end;
          if not (vsInitialized in Result.States) then
            InitNode(Result);
        end else
        begin
          Result := Result.PrevSibling;
          if not (vsInitialized in Result.States) then
            InitNode(Result);
          if not (vsVisible in Result.States) then
            Continue;

          // Now take a look at the children. As the children are initialized
          // while toggling, we don't need to call 'InitChildren' beforehand here.
          while (vsExpanded in Result.States) and Assigned(Result.LastChild) do
          begin
            Result := Result.LastChild;
            if not (vsInitialized in Result.States) then
              InitNode(Result);
            if not (vsVisible in Result.States) then
              Break;
          end;
        end;

        // If we found a visible node we don't need to search any longer.
        if vsVisible in Result.States then
          Break;
      until False;
    end;

    if Assigned(Result) and not (vsInitialized in Result.States) then
      InitNode(Result);
  until not Assigned(Result) or IsEffectivelyVisible[Result];

  Assert(Result <> Node, 'Node cannot be its own visible predecessor.');
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousVisibleNoInit(Node: PVirtualNode;
  ConsiderChildrenAbove: Boolean = True): PVirtualNode;

// Returns the previous node in tree, with regard to Node, which is visible.
// No initialization is done.
// toChildrenAbove is optionally considered which is the default here.

var
  TopInvisibleParent: PVirtualNode;
  ForceSearch: Boolean;

begin
  Result := Node;
  if not Assigned(Result) then Exit;

  Assert(Result <> FRoot, 'Node must not be the hidden root node.');

  repeat
    // If any ancestor is invisible, then find the last (furthest) parent node
    // which is invisible to skip invisible subtrees. Otherwise we will
    // likely go unnecessarily through a whole bunch of invisible nodes.
    TopInvisibleParent := GetTopInvisibleParent(Result);
    if Assigned(TopInvisibleParent) then
      Result := TopInvisibleParent;

    if ConsiderChildrenAbove and (toChildrenAbove in FOptions.PaintOptions) then
    begin
      // Child nodes are the first choice if the current node is known to be visible.
      // Remember, that TopInvisibleParent can be effectively invisible merely due to
      // its own parent's expansion state despite being visible itself.
      if (vsVisible in Result.States) and (vsExpanded in Result.States) and
         (Result <> TopInvisibleParent) and Assigned(Result.LastChild) then
      begin
        Result := Result.LastChild;
        ForceSearch := False;
      end else
        ForceSearch := True;

      if ForceSearch or not (vsVisible in Result.States) then
      begin
        repeat
          // Is there a previous sibling?
          if Assigned(Result.PrevSibling) then
          begin
            Result := Result.PrevSibling;
            if vsVisible in Result.States then
              Break;
          end
          // No sibling anymore, so use the parent's previous sibling.
          else if Result.Parent <> FRoot then
            Result := Result.Parent
          // There are no further nodes to examine, hence there is no further visible node.
          else
          begin
            Result := nil;
            Break;
          end;
        until False;
      end;
    end
    else
    begin
      repeat
        // If there are no siblings anymore, go up one level.
        if not Assigned(Result.PrevSibling) then
        begin
          Result := Result.Parent;
          if Result = FRoot then
          begin
            Result := nil;
            Break;
          end;
        end
        else
        begin
          // There is at least one sibling so take it.
          Result := Result.PrevSibling;
          if not (vsVisible in Result.States) then
            Continue;

          // Now take a look at the children.
          while (vsExpanded in Result.States) and Assigned(Result.LastChild) do
          begin
            Result := Result.LastChild;
            if not (vsVisible in Result.States) then
              Break;
          end;
        end;

        // If we found a visible node we don't need to search any longer.
        if vsVisible in Result.States then
          Break;
      until False;
    end;
  until not Assigned(Result) or IsEffectivelyVisible[Result];

  Assert(Result <> Node, 'Node cannot be its own visible predecessor.');
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousVisibleSibling(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the previous visible sibling before Node. Initialization is done implicitly.

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := GetPreviousSibling(Result);
  until not Assigned(Result) or ((vsVisible in Result.States) and
        (IncludeFiltered or not IsEffectivelyFiltered[Result]));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetPreviousVisibleSiblingNoInit(Node: PVirtualNode;
  IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the previous visible sibling before Node.

begin
  Assert(Assigned(Node) and (Node <> FRoot), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := Result.PrevSibling;
  until not Assigned(Result) or ((vsVisible in Result.States) and
        (IncludeFiltered or not IsEffectivelyFiltered[Result]));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.Nodes(ConsiderChildrenAbove: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for all nodes

begin
  Result.FMode := vneAll;
  Result.FTree := Self;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CheckedNodes(State: TCheckState; ConsiderChildrenAbove: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for all checked nodes

begin
  Result.FMode := vneChecked;
  Result.FTree := Self;
  Result.FState := State;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ChildNodes(Node: PVirtualNode): TVTVirtualNodeEnumeration;

// Enumeration for child nodes

begin
  Result.FMode := vneChild;
  Result.FTree := Self;
  Result.FNode := Node;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.CutCopyNodes(ConsiderChildrenAbove: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for cut copy node

begin
  Result.FMode := vneCutCopy;
  Result.FTree := Self;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.CutToClipboard;
begin
  if (toReadOnly in TreeOptions.MiscOptions)  then
    exit;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.InitializedNodes(ConsiderChildrenAbove: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for initialized nodes

begin
  Result.FMode := vneInitialized;
  Result.FTree := Self;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.LeafNodes: TVTVirtualNodeEnumeration;

// Enumeration for leaf nodes

begin
  Result.FMode := vneLeaf;
  Result.FTree := Self;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.LevelNodes(NodeLevel: Cardinal): TVTVirtualNodeEnumeration;

// Enumeration for level nodes

begin
  Result.FMode := vneLevel;
  Result.FTree := Self;
  Result.FNodeLevel := NodeLevel;
end;

function TBaseVirtualTree.LineWidth: TDimension;
// Returns the width in pixels that should be used to draw grid lines, see issue #1203
begin
  // Always use line width of 1 for older Delphi versions.
  {$if CompilerVersion < 31}
  Exit(1);
  {$else}
  if FCurrentPPI < 200 then
    Exit(1) // Always use 1 pixel is scaled <=200%
  else
    Exit(MulDiv(1, Self.FCurrentPPI, 132)); // Use 132 dpi instead of the typical 96 so that line width increase slightly slower than the actual scaling, so we have a 3px line at 400%
  {$ifend}
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.NoInitNodes(ConsiderChildrenAbove: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for no init nodes
begin
  Result.FMode := vneNoInit;
  Result.FTree := Self;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.SelectedNodes(ConsiderChildrenAbove: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for selected nodes

begin
  Result.FMode := vneSelected;
  Result.FTree := Self;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.VisibleNodes(Node: PVirtualNode; ConsiderChildrenAbove: Boolean;
  IncludeFiltered: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for visible nodes

begin
  Result.FMode := vneVisible;
  Result.FTree := Self;
  Result.FNode := Node;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
  Result.FIncludeFiltered := IncludeFiltered;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.VisibleChildNodes(Node: PVirtualNode; IncludeFiltered: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for visible child nodes

begin
  Result.FMode := vneVisibleChild;
  Result.FTree := Self;
  Result.FNode := Node;
  Result.FIncludeFiltered := IncludeFiltered;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.VisibleChildNoInitNodes(Node: PVirtualNode; IncludeFiltered: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for visible child no init nodes

begin
  Result.FMode := vneVisibleNoInitChild;
  Result.FTree := Self;
  Result.FNode := Node;
  Result.FIncludeFiltered := IncludeFiltered;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.VisibleNoInitNodes(Node: PVirtualNode; ConsiderChildrenAbove: Boolean;
  IncludeFiltered: Boolean): TVTVirtualNodeEnumeration;

// Enumeration for visible no init nodes

begin
  Result.FMode := vneVisibleNoInit;
  Result.FTree := Self;
  Result.FNode := Node;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
  Result.FIncludeFiltered := IncludeFiltered;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetSortedCutCopySet(Resolve: Boolean): TNodeArray;

// Same as GetSortedSelection but with nodes marked as being part in the current cut/copy set (e.g. for clipboard).

var
  Run: PVirtualNode;
  Counter: Cardinal;

  //--------------- local function --------------------------------------------

  procedure IncludeThisNode(Node: PVirtualNode);

  // adds the given node to the result

  var
    Len: Cardinal;

  begin
    Len := Length(Result);
    if Counter = Len then
    begin
      if Len < 100 then
        Len := 100
      else
        Len := Len + Len div 10;
      SetLength(Result, Len);
    end;
    Result[Counter] := Node;
    System.Inc(Counter);
  end;

  //--------------- end local function ----------------------------------------

begin
  Run := FRoot.FirstChild;
  Counter := 0;
  if Resolve then
  begin
    // Resolving is actually easy: just find the first cutted node in logical order
    // and then never go deeper in level than this node as long as there's a sibling node.
    // Restart the search for a cutted node (at any level) if there are no further siblings.
    while Assigned(Run) do
    begin
      if vsCutOrCopy in Run.States then
      begin
        IncludeThisNode(Run);
        if Assigned(Run.NextSibling) then
          Run := Run.NextSibling
        else
        begin
          // If there are no further siblings then go up one or more levels until a node is
          // found or all nodes have been processed. Although we consider here only initialized
          // nodes we don't need to make any special checks as only initialized nodes can also be selected.
          repeat
            Run := Run.Parent;
          until (Run = FRoot) or Assigned(Run.NextSibling);
          if Run = FRoot then
            Break
          else
            Run := Run.NextSibling;
        end;
      end
      else
        Run := GetNextNoInit(Run);
    end;
  end
  else
    while Assigned(Run) do
    begin
      if vsCutOrCopy in Run.States then
        IncludeThisNode(Run);
      Run := GetNextNoInit(Run);
    end;

  // set the resulting array to its real length
  SetLength(Result, Counter);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetSortedSelection(Resolve: Boolean): TNodeArray;

// Returns a list of selected nodes sorted in logical order, that is, as they appear in the tree.
// If Resolve is True then nodes which are children of other selected nodes are not put into the new array.
// This feature is in particuar important when doing drag'n drop as in this case all selected node plus their children
// need to be considered. A selected node which is child (grand child etc.) of another selected node is then
// automatically included and doesn't need to be explicitely mentioned in the returned selection array.
//
// Note: The caller is responsible for freeing the array. Allocation is done here. Usually, though, freeing the array
//       doesn't need additional attention as it is automatically freed by Delphi when it gets out of scope.

var
  Run: PVirtualNode;
  Counter: Cardinal;

begin
  SetLength(Result, FSelectionCount);
  if FSelectionCount > 0 then
  begin
    Run := FRoot.FirstChild;
    Counter := 0;
    if Resolve then
    begin
      // Resolving is actually easy: just find the first selected node in logical order
      // and then never go deeper in level than this node as long as there's a sibling node.
      // Restart the search for a selected node (at any level) if there are no further siblings.
      while Assigned(Run) do
      begin
        if vsSelected in Run.States then
        begin
          Result[Counter] := Run;
          System.Inc(Counter);
          if Assigned(Run.NextSibling) then
            Run := Run.NextSibling
          else
          begin
            // If there are no further siblings then go up one or more levels until a node is
            // found or all nodes have been processed. Although we consider here only initialized
            // nodes we don't need to make any special checks as only initialized nodes can also be selected.
            repeat
              Run := Run.Parent;
            until (Run = FRoot) or Assigned(Run.NextSibling);
            if Run = FRoot then
              Break
            else
              Run := Run.NextSibling;
          end;
        end
        else
          Run := GetNextNoInit(Run);
      end;
    end
    else
      while Assigned(Run) do
      begin
        if vsSelected in Run.States then
        begin
          Result[Counter] := Run;
          System.Inc(Counter);
        end;
        Run := GetNextNoInit(Run);
      end;

    // Since we may have skipped some nodes the result array is likely to be smaller than the
    // selection array, hence shorten the result to true length.
    if Integer(Counter) < Length(Result) then
      SetLength(Result, Counter);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.GetTextInfo(Node: PVirtualNode; Column: TColumnIndex; const AFont: TFont; var R: TRect;
  var Text: string);

// Generic base method for editors, hint windows etc. to get some info about a node.

begin
  R := Rect(0, 0, 0, 0);
  Text := '';
  if Assigned(Font) then // 1 EConvertError due to Font being nil seen here in 01/2019, See issue #878
    AFont.Assign(Font);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetTreeRect: TRect;

// Returns the true size of the tree in pixels. This size is at least ClientHeight x ClientWidth and depends on
// the expand state, header size etc.
// Note: if no columns are used then the width of the tree is determined by the largest node which is currently in the
//       client area. This might however not be the largest node in the entire tree.

begin
  Result := Rect(0, 0, Max(FRangeX, ClientWidth), Max(FRangeY, ClientHeight));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetVisibleParent(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

// Returns the first (nearest) parent node of Node which is visible.
// This method is one of the seldom cases where the hidden root node could be returned.

begin
  Assert(Assigned(Node), 'Node must not be nil.');
  Assert(Node <> FRoot, 'Node must not be the hidden root node.');

  Result := Node.Parent;
  while (Result <> FRoot) and (not FullyVisible[Result] or (not IncludeFiltered and IsEffectivelyFiltered[Result])) do
    Result := Result.Parent;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetTopInvisibleParent(Node: PVirtualNode): PVirtualNode;

// Returns the last (furthest) parent node of Node which is invisible.

var
  Run: PVirtualNode;

begin
  Assert(Assigned(Node), 'Node must not be nil.');
  Assert(Node <> FRoot, 'Node must not be the hidden root node.');

  Result := nil;

  Run := Node.Parent;
  while (Run <> FRoot) do
  begin
    if not ( (vsVisible in Run.States) and (vsExpanded in Run.Parent.States) ) then
      Result := Run;
    Run := Run.Parent;
  end;

end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.HasAsParent(Node, PotentialParent: PVirtualNode): Boolean;

// Determines whether Node has got PotentialParent as one of its parents.

var
  Run: PVirtualNode;

begin
  Result := Assigned(Node) and Assigned(PotentialParent) and (Node <> PotentialParent);
  if Result then
  begin
    Run := Node;
    while (Run <> FRoot) and (Run <> PotentialParent) do
      Run := Run.Parent;
    Result := Run = PotentialParent;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.InsertNode(Node: PVirtualNode; Mode: TVTNodeAttachMode; UserData: Pointer = nil): PVirtualNode;

// Adds a new node relative to Node. The final position is determined by Mode.
// UserData can be used to set the first SizeOf(Pointer) bytes of the user data area to an initial value which can be used
// in OnInitNode and will also cause to trigger the OnFreeNode event (if <> nil) even if the node is not yet
// "officially" initialized.
// InsertNode is a compatibility method and will implicitly validate the given node if the new node
// is to be added as child node. This is however against the virtual paradigm and hence I dissuade from its usage.

begin
  if Mode <> amNoWhere then
  begin
    CancelEditNode;

    if Node = nil then
      Node := FRoot;
    // we need a new node...
    Result := MakeNewNode;
    // avoid erronous attach modes
    if Node = FRoot then
    begin
      case Mode of
        amInsertBefore:
          Mode := amAddChildFirst;
        amInsertAfter:
          Mode := amAddChildLast;
      end;
    end;

    // Validate given node in case the new node becomes its child.
    if (Mode in [amAddChildFirst, amAddChildLast]) and not (vsInitialized in Node.States) then
      InitNode(Node);
    InternalConnectNode(Result, Node, Self, Mode);

    // Check if there is initial user data and there is also enough user data space allocated.
    if Assigned(UserData) then
      SetNodeData(Result, UserData);

    if FUpdateCount = 0 then
    begin
      case Mode of
        amInsertBefore,
        amInsertAfter:
        begin
          // Here no initialization is necessary because *if* a node has already got children then it
          // must also be initialized.
          // Note: Node can never be FRoot at this point.
          StructureChange(Result, crNodeAdded);
          // If auto sort is enabled then sort the node or its parent (depending on the insert mode).
          if (toAutoSort in FOptions.AutoOptions) and (FHeader.SortColumn > InvalidColumn) then
            Sort(Node.Parent, FHeader.SortColumn, FHeader.SortDirection, True);
          InvalidateToBottom(Result)
        end;
        amAddChildFirst,
        amAddChildLast:
        begin
          StructureChange(Node, crChildAdded);
          // If auto sort is enabled then sort the node or its parent (depending on the insert mode).
          if (toAutoSort in FOptions.AutoOptions) and (FHeader.SortColumn > InvalidColumn) then
            Sort(Node, FHeader.SortColumn, FHeader.SortDirection, True);
          InvalidateToBottom(Node);
        end;
      end;
      InvalidateCache();
      UpdateScrollBars(True);
    end;
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InvalidateChildren(Node: PVirtualNode; Recursive: Boolean);

// Invalidates Node and its immediate children.
// If Recursive is True then all grandchildren are invalidated as well.
// The node itself is initialized if necessary and its child nodes are created (and initialized too if
// Recursive is True).

var
  Run: PVirtualNode;

begin
  if Assigned(Node) then
  begin
    if not (vsInitialized in Node.States) then
      InitNode(Node);
    InvalidateNode(Node);
    if (vsHasChildren in Node.States) and (Node.ChildCount = 0) then
      InitChildren(Node);
    Run := Node.FirstChild;
  end
  else
    Run := FRoot.FirstChild;

  while Assigned(Run) do
  begin
    InvalidateNode(Run);
    if Recursive then
      InvalidateChildren(Run, True);
    Run := Run.NextSibling;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InvalidateColumn(Column: TColumnIndex);

// Invalidates the client area part of a column.

var
  R: TRect;

begin
  if (FUpdateCount = 0) and HandleAllocated and FHeader.Columns.IsValidColumn(Column) then
  begin
    R := ClientRect;
    FHeader.Columns.GetColumnBounds(Column, R.Left, R.Right);
    InvalidateRect(@R, False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.InvalidateNode(Node: PVirtualNode): TRect;

// Initiates repaint of the given node and returns the just invalidated rectangle.

begin
  Assert(Assigned(Node), 'Node must not be nil.');
  Assert(GetCurrentThreadId = MainThreadId, 'UI controls may only be chnaged in UI thread.');
  // Reset height measured flag too to cause a re-issue of the OnMeasureItem event.
  Exclude(Node.States, vsHeightMeasured);
  if (FUpdateCount = 0) and HandleAllocated then
  begin
    Result := GetDisplayRect(Node, NoColumn, False);
    InvalidateRect(@Result, False);
  end
  else
    result := Rect(-1,-1,-1,-1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InvalidateToBottom(Node: PVirtualNode);

// Initiates repaint of client area starting at given node. If this node is not visible or not yet initialized
// then nothing happens.

var
  R: TRect;

begin
  if (FUpdateCount = 0) and HandleAllocated then
  begin
    if (Node = nil) or (Node = FRoot) then
      Invalidate
    else
      if (vsInitialized in Node.States) and IsEffectivelyVisible[Node] then
      begin
        R := GetDisplayRect(Node, NoColumn, False);
        if R.Top < ClientHeight then
        begin
          if (toChildrenAbove in FOptions.PaintOptions) and (vsExpanded in Node.States) then
            Dec(R.Top, Node.TotalHeight + NodeHeight[Node]);
          R.Bottom := ClientHeight;
          InvalidateRect(@R, False);
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.InvertSelection(VisibleOnly: Boolean);

// Inverts the current selection (so nodes which are selected become unselected and vice versa).
// If VisibleOnly is True then only visible nodes are considered.

var
  Run: PVirtualNode;
  NewSize: Integer;
  NextFunction: TGetNextNodeProc;
  TriggerChange: Boolean;

begin
  if not FSelectionLocked and (toMultiSelect in FOptions.SelectionOptions) then
  begin
    Run := FRoot.FirstChild;
    ClearTempCache;
    if VisibleOnly then
      NextFunction := GetNextVisibleNoInit
    else
      NextFunction := GetNextNoInit;
    while Assigned(Run) do
    begin
      if vsSelected in Run.States then
        InternalRemoveFromSelection(Run)
      else
        InternalCacheNode(Run);
      Run := NextFunction(Run);
    end;

    // do some housekeeping
    // Need to trigger the OnChange event from here if nodes were only deleted but not added.
    TriggerChange := False;
    NewSize := PackArray(FSelection, FSelectionCount);
    if NewSize > -1 then
    begin
      FSelectionCount := NewSize;
      SetLength(FSelection, FSelectionCount);
      TriggerChange := True;
    end;
    if FTempNodeCount > 0 then
    begin
      AddToSelection(FTempNodeCache, FTempNodeCount);
      ClearTempCache;
      TriggerChange := False;
    end;
    Invalidate;
    if TriggerChange then
      Change(nil);
    if Self.SelectedCount = 0 then
      FNextNodeToSelect := nil;//Ensure that no other node is selected now
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsEditing: Boolean;

begin
  Result := tsEditing in FStates;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsMouseSelecting: Boolean;

begin
  Result := (tsDrawSelPending in FStates) or (tsDrawSelecting in FStates);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsStored_BackgroundOffsetXY(const Index: Integer): Boolean;
begin
  case Index of
    0:
      Result:= CompareValue(FBackgroundOffsetX, 0)<>EqualsValue;
    1:
      Result:= CompareValue(FBackgroundOffsetY, 0)<>EqualsValue;
    else
      // Clear warning only
      Result:= false;
      RaiseVTError('Unknown index in TBaseVirtualTree.IsStored_BackgroundOffsetXY', 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsStored_BottomSpace: Boolean;
begin
  Result:= CompareValue(FBottomSpace, 0)<>EqualsValue;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsStored_DefaultNodeHeight: Boolean;
begin
  Result:= CompareValue(FDefaultNodeHeight, cInitialDefaultNodeHeight)<>EqualsValue;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsStored_Indent: Boolean;
begin
  Result:= CompareValue(FIndent, 18)<>EqualsValue;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsStored_Margin: Boolean;
begin
  Result:= CompareValue(FMargin, 4)<>EqualsValue;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsStored_TextMargin: Boolean;
begin
  Result:= CompareValue(FTextMargin, cDefaultTextMargin) <> EqualsValue;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IsUpdating: Boolean;
// The tree does currently not update its window because a BeginUpdate has not yet ended.
begin
  Exit(UpdateCount > 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.IterateSubtree(StartNode: PVirtualNode; Callback: TVTGetNodeProc; Data: Pointer;
  Filter: TVirtualNodeStates = []; DoInit: Boolean = False; ChildNodesOnly: Boolean = False): PVirtualNode;

// Iterates through the all children and grandchildren etc. of Node (or the entire tree if Node = nil)
// and calls for each node the provided callback method (which must not be empty).
// Filter determines which nodes to consider (an empty set denotes all nodes).
// If DoInit is True then nodes which aren't initialized yet will be initialized.
// Note: During execution of the callback the application can set Abort to True. In this case the iteration is stopped
//       and the last accessed node (the one on which the callback set Abort to True) is returned to the caller.
//       Otherwise (no abort) nil is returned.

var
  Stop: PVirtualNode;
  Abort: Boolean;
  GetNextNode: TGetNextNodeProc;
  WasIterating: Boolean;

begin
  Assert(StartNode <> FRoot, 'Node must not be the hidden root node.');

  WasIterating := tsIterating in FStates;
  DoStateChange([tsIterating]);
  try
    // prepare function to be used when advancing
    if DoInit then
      GetNextNode := GetNext
    else
      GetNextNode := GetNextNoInit;

    Abort := False;
    Result := StartNode;
    if Result = nil then
      Stop := nil
    else
    begin
      if not (vsInitialized in Result.States) and DoInit then
        InitNode(Result);

      // The stopper does not need to be initialized since it is not taken into the enumeration.
      Stop := Result.NextSibling;
      if Stop = nil then
      begin
        Stop := Result;
        repeat
          Stop := Stop.Parent;
        until (Stop = FRoot) or Assigned(Stop.NextSibling);
        if Stop = FRoot then
          Stop := nil
        else
          Stop := Stop.NextSibling;
      end;
    end;

    // Use first node if we start with the root.
    if Result = nil then
      Result := GetFirstNoInit;

    if Assigned(Result) then
    begin
      if not (vsInitialized in Result.States) and DoInit then
        InitNode(Result);

      // Skip given node if only the child nodes are requested.
      if ChildNodesOnly then
      begin
        if Result.ChildCount = 0 then
          Result := nil
        else if StartNode <> nil then
          Result := GetNextNode(Result);
      end;

      if Filter = [] then
      begin
        // unfiltered loop
        while Assigned(Result) and (Result <> Stop) do
        begin
          Callback(Self, Result, Data, Abort);
          if Abort then
            Break;
          Result := GetNextNode(Result);
        end;
      end
      else
      begin
        // filtered loop
        while Assigned(Result) and (Result <> Stop) do
        begin
          if Result.States * Filter = Filter then
            Callback(Self, Result, Data, Abort);
          if Abort then
            Break;
          Result := GetNextNode(Result);
        end;
      end;
    end;

    if not Abort then
      Result := nil;
  finally
    if not WasIterating then
      DoStateChange([], [tsIterating]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.LoadFromFile(const FileName: TFileName);

var
  FileStream: TFileStream;

begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.LoadFromStream(Stream: TStream);

// Clears the current content of the tree and loads a new structure from the given stream.

var
  ThisID: TMagicID;
  Version,
  Count: Cardinal;
  Node: PVirtualNode;

begin
  if not (toReadOnly in FOptions.MiscOptions) then
  begin
    Clear;
    // Check first whether this is a stream we can read.
    if Stream.Read(ThisID, SizeOf(TMagicID)) < SizeOf(TMagicID) then
      RaiseVTError(SStreamTooSmall, hcTFStreamTooSmall);

    if (ThisID[0] = MagicID[0]) and
       (ThisID[1] = MagicID[1]) and
       (ThisID[2] = MagicID[2]) and
       (ThisID[5] = MagicID[5]) then
    begin
      Version := Word(ThisID[3]);
      if Version <= VTTreeStreamVersion then
      begin
        BeginUpdate;
        try
          if Version < 2 then
            Count := MaxInt
          else
            Stream.ReadBuffer(Count, SizeOf(Count));

          while (Stream.Position < Stream.Size) and (Count > 0) do
          begin
            System.Dec(Count);
            Node := MakeNewNode;
            InternalConnectNode(Node, FRoot, Self, amAddChildLast);
            InternalAddFromStream(Stream, Version, Node);
          end;
          DoNodeCopied(nil);
          if Assigned(FOnLoadTree) then
            FOnLoadTree(Self, Stream);
        finally
          EndUpdate;
        end;
      end
      else
        RaiseVTError(SWrongStreamVersion, hcTFWrongStreamVersion);
    end
    else
      RaiseVTError(SWrongStreamFormat, hcTFWrongStreamFormat);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.MeasureItemHeight(const Canvas: TCanvas; Node: PVirtualNode);

// If the height of the given node has not yet been measured then do it now.

var
  NewNodeHeight: TDimension;

begin
  if not (vsHeightMeasured in Node.States) then
  begin
    Include(Node.States, vsHeightMeasured);
    if (toVariableNodeHeight in FOptions.MiscOptions) then
    begin
      NewNodeHeight := Node.NodeHeight;
      // Anonymous methods help to make this thread safe easily.
      if (MainThreadId <> GetCurrentThreadId) then
        begin
        AtomicIncrement(FPendingSyncProcs);
        TThread.Synchronize(nil,
          procedure
          begin
            //swish:Decrement invoke refs
            AtomicDecrement(FPendingSyncProcs);
            DoMeasureItem(Canvas, Node, NewNodeHeight);
            SetNodeHeight(Node, NewNodeHeight);
          end
        )
        end
      else
      begin
        DoMeasureItem(Canvas, Node, NewNodeHeight);
        SetNodeHeight(Node, NewNodeHeight);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.MoveTo(Node: PVirtualNode; Tree: TBaseVirtualTree; Mode: TVTNodeAttachMode;
  ChildrenOnly: Boolean);

// A simplified method to allow to move nodes to the root of another tree.

begin
  MoveTo(Node, Tree.FRoot, Mode, ChildrenOnly);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.MoveTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode; ChildrenOnly: Boolean);

// Moves the given node (and all its children) to Target. Source must belong to the tree instance which calls this
// MoveTo method. Mode determines how to connect Source to Target.
// This method might involve a change of the tree if Target belongs to a different tree than Source.

var
  TargetTree: TBaseVirtualTree;
  Allowed: Boolean;
  NewNode: PVirtualNode;
  Stream: TMemoryStream;

begin
  Assert(TreeFromNode(Source) = Self, 'The source tree must contain the source node.');

  // When moving nodes then source and target must not be the same node unless only the source's children are
  // moved and they are inserted before or after the node itself.
  Allowed := (Source <> Target) or ((Mode in [amInsertBefore, amInsertAfter]) and ChildrenOnly);

  if Allowed and (Mode <> amNoWhere) and Assigned(Source) and (Source <> FRoot) and
    not (toReadOnly in FOptions.MiscOptions) then
  begin
    // Assume that an empty destination means the root in this (the source) tree.
    if Target = nil then
    begin
      TargetTree := Self;
      Target := FRoot;
      Mode := amAddChildFirst;
    end
    else
      TargetTree := TreeFromNode(Target);

    if Target = TargetTree.FRoot then
    begin
      case Mode of
        amInsertBefore:
          Mode := amAddChildFirst;
        amInsertAfter:
          Mode := amAddChildLast;
      end;
    end;

    // Make sure the target node is initialized.
    if not (vsInitialized in Target.States) then
      TargetTree.InitNode(Target)
    else
      if (vsHasChildren in Target.States) and (Target.ChildCount = 0) then
        TargetTree.InitChildren(Target);

    if TargetTree = Self then
    begin
      // Simple case: move node(s) within the same tree.
      if Target = FRoot then
        Allowed := DoNodeMoving(Source, nil)
      else
        Allowed := DoNodeMoving(Source, Target);
      if Allowed then
      begin
        // Check first that Source is not added as new child to a target node which
        // is already a child of Source.
        // Consider the case Source and Target are the same node, but only child nodes are moved.
        if (Source <> Target) and HasAsParent(Target, Source) then
          RaiseVTError(SWrongMoveError, hcTFWrongMoveError);

        if not ChildrenOnly then
        begin
          // Disconnect from old location.
          InternalDisconnectNode(Source, True);
          // Connect to new location.
          InternalConnectNode(Source, Target, Self, Mode);
          DoNodeMoved(Source);
        end
        else
        begin
          // Only child nodes should be moved. Insertion order depends on move mode.
          if Mode = amAddChildFirst then
          begin
            Source := Source.LastChild;
            while Assigned(Source) do
            begin
              NewNode := Source.PrevSibling;
              // Disconnect from old location.
              InternalDisconnectNode(Source, True, False);
              // Connect to new location.
              InternalConnectNode(Source, Target, Self, Mode);
              DoNodeMoved(Source);
              Source := NewNode;
            end;
          end
          else
          begin
            Source := Source.FirstChild;
            while Assigned(Source) do
            begin
              NewNode := Source.NextSibling;
              // Disconnect from old location.
              InternalDisconnectNode(Source, True, False);
              // Connect to new location.
              InternalConnectNode(Source, Target, Self, Mode);
              DoNodeMoved(Source);
              Source := NewNode;
            end;
          end;
        end;
      end;
    end
    else
    begin
      // Difficult case: move node(s) to another tree.
      // In opposition to node copying we ask only once if moving is allowed because
      // we cannot take back a move once done.
      if Target = TargetTree.FRoot then
        Allowed := DoNodeMoving(Source, nil)
      else
        Allowed := DoNodeMoving(Source, Target);

      if Allowed then
      begin
        Stream := TMemoryStream.Create;
        try
          // Write all nodes into a temporary stream depending on the ChildrenOnly flag.
          if not ChildrenOnly then
            WriteNode(Stream, Source)
          else
          begin
            Source := Source.FirstChild;
            while Assigned(Source) do
            begin
              WriteNode(Stream, Source);
              Source := Source.NextSibling;
            end;
          end;
          // Now load the serialized nodes into the target node (tree).
          TargetTree.BeginUpdate;
          try
            Stream.Position := 0;
            while Stream.Position < Stream.Size do
            begin
              NewNode := TargetTree.MakeNewNode;
              InternalConnectNode(NewNode, Target, TargetTree, Mode);
              TargetTree.InternalAddFromStream(Stream, VTTreeStreamVersion, NewNode);
              DoNodeMoved(NewNode);
            end;
          finally
            TargetTree.EndUpdate;
          end;
        finally
          Stream.Free;
        end;
        // finally delete original nodes
        BeginUpdate;
        try
          if ChildrenOnly then
            DeleteChildren(Source)
          else
            DeleteNode(Source);
        finally
          EndUpdate;
        end;
      end;
    end;

    InvalidateCache;
    if (FUpdateCount = 0) and Allowed then
    begin
      ValidateCache;
      UpdateScrollBars(True);
      Invalidate;
      if TargetTree <> Self then
        TargetTree.Invalidate;
    end;
    StructureChange(Source, crNodeMoved);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.PaintTree(TargetCanvas: TCanvas; Window: TRect; Target: TPoint;
  PaintOptions: TVTInternalPaintOptions; PixelFormat: TPixelFormat);

// This is the core paint routine of the tree. It is responsible for maintaining the paint cycles per node as well
// as coordinating drawing of the various parts of the tree image.
// TargetCanvas is the canvas to which to draw the tree image. This is usually the tree window itself but could well
// be a bitmap or printer canvas.
// Window determines which part of the entire tree image to draw. The full size of the virtual image is determined
// by GetTreeRect.
// Target is the position in TargetCanvas where to draw the tree part specified by Window.
// PaintOptions determines what of the tree to draw. For different tasks usually different parts need to be drawn, with
// a full image in the window, selected only nodes for a drag image etc.

const
  ImageKind: array[Boolean] of TVTImageKind = (ikNormal, ikSelected);

var
  DrawSelectionRect,
  UseBackground,
  ShowCheckImages,
  UseColumns,
  IsMainColumn: Boolean;

  IndentSize,
  ButtonY: TDimension;            // Y position of toggle button within the node's rect
  LineImage: TLineImage;
  PaintInfo: TVTPaintInfo;     // all necessary information about a node to pass to the paint routines

  R,                           // the area of an entire node in its local coordinate
  TargetRect,                  // the area of a node (part) in the target canvas
  SelectionRect,               // ordered rectangle used for drawing the selection focus rect
  ClipRect: TRect;             // area to which the canvas will be clipped when painting a node's content
  NextColumn: TColumnIndex;
  BaseOffset: TDimension;         // top position of the top node to draw given in absolute tree coordinates
  NodeBitmap: TBitmap;         // small buffer to draw flicker free
  MaximumRight,                // maximum horizontal target position
  MaximumBottom: TDimension;      // maximum vertical target position
  SelectLevel: Integer;        // > 0 if current node is selected or child/grandchild etc. of a selected node
  FirstColumn: TColumnIndex;   // index of first column which is at least partially visible in the given window

  MaxRight,
  ColLeft,
  ColRight: TDimension;

  SavedTargetDC: Integer;
  PaintWidth: TDimension;
  CurrentNodeHeight: TDimension;
  lEmptyListTextMargin: TDimension;

  CellIsTouchingClientRight: Boolean;
  CellIsInLastColumn: Boolean;
  ColumnIsFixed: Boolean;

begin
  if not (tsPainting in FStates) then
  begin
    DoStateChange([tsPainting]);
    try
      DoBeforePaint(TargetCanvas);

      if poUnbuffered in PaintOptions then
        SavedTargetDC := SaveDC(TargetCanvas.Handle)
      else
        SavedTargetDC := 0;

      // Prepare paint info structure.
      ZeroMemory(@PaintInfo, SizeOf(PaintInfo));

      PaintWidth := Window.Right - Window.Left;

      if not (poUnbuffered in PaintOptions) then
      begin
        // Create small bitmaps and initialize default values.
        // The bitmaps are used to paint one node at a time and to draw the result to the target (e.g. screen) in one step,
        // to prevent flickering.
        NodeBitmap := TBitmap.Create;
        // For alpha blending we need the 32 bit pixel format. For other targets there might be a need for a certain
        // pixel format (e.g. printing).
        if ((FDrawSelectionMode = smBlendedRectangle) or (tsUseThemes in FStates) or
          (toUseBlendedSelection in FOptions.PaintOptions)) then
          NodeBitmap.PixelFormat := pf32Bit
        else
          NodeBitmap.PixelFormat := PixelFormat;

        NodeBitmap.Width := PaintWidth;

        // Make sure the buffer bitmap and target bitmap use the same transformation mode.
        SetMapMode(NodeBitmap.Canvas.Handle, GetMapMode(TargetCanvas.Handle));
        PaintInfo.Canvas := NodeBitmap.Canvas;
      end
      else
      begin
        PaintInfo.Canvas := TargetCanvas;
        NodeBitmap := nil;
      end;

      // Lock the canvas to avoid that it gets freed on the way.
      PaintInfo.Canvas.Lock;
      try
        // Prepare the current selection rectangle once. The corner points are absolute tree coordinates.
        SelectionRect := OrderRect(FNewSelRect);
        DrawSelectionRect := IsMouseSelecting and not IsRectEmpty(SelectionRect) and (GetKeyState(VK_LBUTTON) < 0);

        // R represents an entire node (all columns), but is a bit unprecise when it comes to
        // trees without any column defined, because FRangeX only represents the maximum width of all
        // nodes in the client area (not all defined nodes). There might be, however, wider nodes somewhere. Without full
        // validation I cannot better determine the width, though. By using at least the control's width it is ensured
        // that the tree is fully displayed on screen.
        R := Rect(0, 0, Max(FRangeX, ClientWidth), 0);

        // For quick checks some intermediate variables are used.
        UseBackground := (toShowBackground in FOptions.PaintOptions) and Assigned(FBackground.Graphic) and
          (poBackground in PaintOptions);
        ShowCheckImages := Assigned(FCheckImages) and (toCheckSupport in FOptions.MiscOptions);
        UseColumns := FHeader.UseColumns;

        // Adjust paint options to tree settings. Hide selection if told so or the tree is unfocused.
        if (toAlwaysHideSelection in FOptions.PaintOptions) or
          (not Focused and (toHideSelection in FOptions.PaintOptions)) then
          Exclude(PaintOptions, poDrawSelection);
        if toHideFocusRect in FOptions.PaintOptions then
          Exclude(PaintOptions, poDrawFocusRect);

        // Determine node to start drawing with.
        BaseOffset := 0;
        PaintInfo.Node := GetNodeAt(0, Window.Top, False, BaseOffset);
        if PaintInfo.Node = nil then
          BaseOffset := Window.Top;

        // Transform selection rectangle into node bitmap coordinates.
        if DrawSelectionRect then
          OffsetRect(SelectionRect, 0, -BaseOffset);

        // The target rectangle holds the coordinates of the exact area to blit in target canvas coordinates.
        // It is usually smaller than an entire node and wanders while the paint loop advances.
        MaximumRight := Target.X + (Window.Right - Window.Left);
        MaximumBottom := Target.Y + (Window.Bottom - Window.Top);

        TargetRect := Rect(Target.X, Target.Y - (Window.Top - BaseOffset), MaximumRight, 0);
        TargetRect.Bottom := TargetRect.Top;
        TargetCanvas.Font := Self.Font;

        // This marker gets the index of the first column which is visible in the given window.
        // This is needed for column based background colors.
        FirstColumn := InvalidColumn;

        if Assigned(PaintInfo.Node) then
        begin

          // ----- main node paint loop
          while Assigned(PaintInfo.Node) do
          begin
            // Determine LineImage, SelectionLevel and IndentSize
            SelectLevel := DetermineLineImageAndSelectLevel(PaintInfo.Node, LineImage);
            IndentSize := Length(LineImage);

            // Initialize node if not already done.
            if not (vsInitialized in PaintInfo.Node.States) then
              InitNode(PaintInfo.Node);
            if (vsSelected in PaintInfo.Node.States) and not (toChildrenAbove in FOptions.PaintOptions) then
              System.Inc(SelectLevel);

            // Ensure the node's height is determined.
            MeasureItemHeight(PaintInfo.Canvas, PaintInfo.Node);

            // Adjust the brush origin for dotted lines depending on the current source position.
            // It is applied some lines later, as the canvas might get reallocated, when changing the node bitmap.
            PaintInfo.BrushOrigin := Point(Window.Left and 1, BaseOffset and 1);
            Inc(BaseOffset, PaintInfo.Node.NodeHeight);

            TargetRect.Bottom := TargetRect.Top + PaintInfo.Node.NodeHeight;

            // If poSelectedOnly is active then do the following stuff only for selected nodes or nodes
            // which are children of selected nodes.
            if (SelectLevel > 0) or not (poSelectedOnly in PaintOptions) then
            begin
              if not (poUnbuffered in PaintOptions) then
              begin
                // Adjust height of temporary node bitmap.
                with NodeBitmap do
                begin
                  if Height <> PaintInfo.Node.NodeHeight then
                  begin
                    // Avoid that the VCL copies the bitmap while changing its height.
                    Height := 0;
                    Height := PaintInfo.Node.NodeHeight;
                    SetCanvasOrigin(Canvas, Window.Left, 0);
                  end;
                end;
              end
              else
              begin
                SetCanvasOrigin(PaintInfo.Canvas, -TargetRect.Left + Window.Left, -TargetRect.Top);
                ClipCanvas(PaintInfo.Canvas, Rect(0, 0, TargetRect.Right - TargetRect.Left,
                                                  Min(TargetRect.Bottom - TargetRect.Top, MaximumBottom - TargetRect.Top))); // See issue #579
              end;

              // Set the origin of the canvas' brush. This depends on the node heights.
              with PaintInfo do
                SetBrushOrigin(Canvas, BrushOrigin.X, BrushOrigin.Y);

              CurrentNodeHeight := PaintInfo.Node.NodeHeight;
              R.Bottom := CurrentNodeHeight;

              // Let application decide whether the node should normally be drawn or by the application itself.
              if not DoBeforeItemPaint(PaintInfo.Canvas, PaintInfo.Node, R) then
              begin
                // Init paint options for the background painting.
                PaintInfo.PaintOptions := PaintOptions;

                // The node background can contain a single color, a bitmap or can be drawn by the application.
                ClearNodeBackground(PaintInfo, UseBackground, True, Rect(Window.Left, TargetRect.Top, Window.Right,
                  TargetRect.Bottom));

                // Prepare column, position and node clipping rectangle.
                PaintInfo.CellRect := R;
                if UseColumns then
                  InitializeFirstColumnValues(PaintInfo);

                // Now go through all visible columns (there's still one run if columns aren't used).
                with TVirtualTreeColumnsCracker(FHeader.Columns) do
                begin
                  while ((PaintInfo.Column > InvalidColumn) or not UseColumns)
                    and (PaintInfo.CellRect.Left < Window.Right) do
                  begin
                    if UseColumns then
                    begin
                      PaintInfo.Column := PositionToIndex[PaintInfo.Position];
                      if FirstColumn = InvalidColumn then
                        FirstColumn := PaintInfo.Column;
                      PaintInfo.BidiMode := Items[PaintInfo.Column].BiDiMode;
                      PaintInfo.Alignment := Items[PaintInfo.Column].Alignment;
                    end
                    else
                    begin
                      PaintInfo.Column := NoColumn;
                      PaintInfo.BidiMode := BidiMode;
                      PaintInfo.Alignment := FAlignment;
                    end;
                    GetOffSets(PaintInfo.Node, PaintInfo.Offsets, TVTElement.ofsText, PaintInfo.Column);

                    PaintInfo.PaintOptions := PaintOptions;
                    with PaintInfo do
                    begin
                      if (tsEditing in FStates) and (Node = FFocusedNode) and
                        ((Column = FEditColumn) or not UseColumns) then
                        Exclude(PaintOptions, poDrawSelection);
                      if not UseColumns or
                        ((vsSelected in Node.States) and (toFullRowSelect in FOptions.SelectionOptions) and
                         (poDrawSelection in PaintOptions)) or
                        (coParentColor in Items[PaintInfo.Column].Options) or
                        ((coStyleColor in Items[PaintInfo.Column].Options) and VclStyleEnabled)
                      then
                        Exclude(PaintOptions, poColumnColor);
                    end;
                    IsMainColumn := PaintInfo.Column = FHeader.MainColumn;

                    // Consider bidi mode here. In RTL context means left alignment actually right alignment and vice versa.
                    if PaintInfo.BidiMode <> bdLeftToRight then
                      ChangeBiDiModeAlignment(PaintInfo.Alignment);

                    // Paint the current cell if it is marked as being visible or columns aren't used and
                    // if this cell belongs to the main column if only the main column should be drawn.
                    if (not UseColumns or (coVisible in Items[PaintInfo.Column].Options)) and
                      (not (poMainOnly in PaintOptions) or IsMainColumn) then
                    begin
                      AdjustPaintCellRect(PaintInfo, NextColumn);

                      // Paint the cell only if it is in the current window.
                      if PaintInfo.CellRect.Right > Window.Left then
                      begin
                        with PaintInfo do
                        begin
                          // Fill in remaining values in the paint info structure.
                          NodeWidth := DoGetNodeWidth(Node, Column, Canvas);

                          if ShowCheckImages and IsMainColumn then
                          begin
                            ImageInfo[iiCheck].Index := GetCheckImage(Node);
                            ImageInfo[iiCheck].Images := FCheckImages;
                            ImageInfo[iiCheck].Ghosted := False;
                          end
                          else
                            ImageInfo[iiCheck].Index := -1;
                          GetImageIndex(PaintInfo, ikState, iiState);
                          GetImageIndex(PaintInfo, ImageKind[vsSelected in Node.States], iiNormal);

                          CalculateVerticalAlignments(PaintInfo, ButtonY);
                          // Take the space for the tree lines into account.
                          PaintInfo.AdjustImageCoordinates();
                          if UseColumns then
                          begin
                            ClipRect := CellRect;
                            if poUnbuffered in PaintOptions then
                            begin
                              ClipRect.Left := Max(ClipRect.Left, Window.Left);
                              ClipRect.Right := Min(ClipRect.Right, Window.Right);
                              ClipRect.Top := Max(ClipRect.Top, Window.Top - (BaseOffset - CurrentNodeHeight));
                              ClipRect.Bottom := ClipRect.Bottom - Max(TargetRect.Bottom - MaximumBottom, 0);
                            end;
                            ClipCanvas(Canvas, ClipRect);
                          end;

                          // Paint the horizontal grid line.
                          if (poGridLines in PaintOptions) and (toShowHorzGridLines in FOptions.PaintOptions) then
                          begin
                            Canvas.Font.Color := FColors.GridLineColor;
                            if IsMainColumn and (FLineMode = lmBands) then
                            begin
                              if BidiMode = bdLeftToRight then
                              begin
                                DrawGridHLine(PaintInfo, CellRect.Left + PaintInfo.Offsets[ofsCheckBox] - fImagesMargin, CellRect.Right - LineWidth, CellRect.Bottom - LineWidth);
                              end
                              else
                              begin
                                DrawGridHLine(PaintInfo, CellRect.Left, CellRect.Right - IfThen(toFixedIndent in FOptions.PaintOptions, LineWidth, IndentSize) * FIndent - 1, CellRect.Bottom - LineWidth);
                              end;
                            end
                            else
                              DrawGridHLine(PaintInfo, CellRect.Left, CellRect.Right, CellRect.Bottom - LineWidth);

                            Dec(CellRect.Bottom);
                            Dec(ContentRect.Bottom);
                          end;

                          if UseColumns then
                          begin
                            ColumnIsFixed := coFixed in FHeader.Columns[Column].Options;
                            // Paint vertical grid line.
                            if (poGridLines in PaintOptions) and (toShowVertGridLines in FOptions.PaintOptions) then
                            begin
                              // These variables and the nested if conditions shall make the logic
                              // easier to understand.
                              CellIsTouchingClientRight := PaintInfo.CellRect.Right = ClientRect.Right;
                              CellIsInLastColumn := Position = TColumnPosition(Count - 1);

                              // Don't draw if this is the last column and the header is in autosize mode.
                              if not ((hoAutoResize in FHeader.Options) and CellIsInLastColumn) then
                              begin
                                // We have to take spanned cells into account which we determine
                                // by checking if CellRect.Right equals the Window.Right.
                                // But since the PaintTree procedure is called twice in
                                // TBaseVirtualTree.Paint (i.e. for fixed columns and other columns.
                                // CellIsTouchingClientRight does not work for fixed columns.)
                                // we have to paint fixed column grid line anyway.
                                if not CellIsTouchingClientRight or ColumnIsFixed then
                                begin
                                  if (BidiMode = bdLeftToRight) or not ColumnIsEmpty(Node, Column) then
                                  begin
                                    DrawGridVLine(PaintInfo, CellRect.Top, CellRect.Bottom, CellRect.Right - LineWidth, ColumnIsFixed and (NextColumn >= 0));
                                  end;

                                  Dec(CellRect.Right);
                                end;
                              end;
                              // Reduce the content rect size nonetheless to retain correct alignment
                              // relative to header content (especially if "PaintInfo.Alignment = alRightJustify").
                              Dec(ContentRect.Right);
                            end// if poGridLines
                            else
                            begin
                                if ColumnIsFixed then
                                begin
                                  if (BidiMode = bdLeftToRight) or not ColumnIsEmpty(Node, Column) then
                                  begin
                                    DrawGridVLine(PaintInfo, CellRect.Top, CellRect.Bottom, CellRect.Right - LineWidth, ColumnIsFixed and (NextColumn >= 0));
                                  end;
                                  Dec(CellRect.Right);
                                end;
                            end//else
                          end;

                          // Prepare background and focus rect for the current cell.
                          PrepareCell(PaintInfo, Window.Left, PaintWidth);

                          // Some parts are only drawn for the main column.
                          if IsMainColumn then
                          begin
                            if (toShowTreeLines in FOptions.PaintOptions) and
                               (not (toHideTreeLinesIfThemed in FOptions.PaintOptions) or
                                not (tsUseThemes in FStates)) then
                              PaintTreeLines(PaintInfo, IfThen(toFixedIndent in FOptions.PaintOptions, 1,
                                             IndentSize), LineImage);
                            // Show node button if allowed, if there child nodes and at least one of the child
                            // nodes is visible or auto button hiding is disabled.
                            if (toShowButtons in FOptions.PaintOptions) and (vsHasChildren in Node.States) and
                              not ((vsAllChildrenHidden in Node.States) and
                              (toAutoHideButtons in TreeOptions.AutoOptions)) and
                              ((toShowRoot in TreeOptions.PaintOptions) or (GetNodeLevel(Node) > 0))
                            then
                              PaintNodeButton(Canvas, Node, Column, CellRect, Offsets[ofsToggleButton], ButtonY, BidiMode); // Relative X position of toggle button is needed for proper BiDi calculation

                            if ImageInfo[iiCheck].Index > -1 then
                              PaintCheckImage(Canvas, PaintInfo.ImageInfo[iiCheck], vsSelected in PaintInfo.Node.States);
                          end;

                          if ImageInfo[iiState].Index > -1 then
                            PaintImage(PaintInfo, iiState, False);
                          if ImageInfo[iiNormal].Index > -1 then
                            PaintImage(PaintInfo, iiNormal, True);

                          // Now let descendants or applications draw whatever they want,
                          // but don't draw the node if it is currently being edited.
                          if not ((tsEditing in FStates) and (Node = FFocusedNode) and
                            ((Column = FEditColumn) or not UseColumns)) then
                            DoPaintNode(PaintInfo);

                          Canvas.Brush.Color := FColors.BackGroundColor; // Set useful background color, see issue #1264
                          DoAfterCellPaint(Canvas, Node, Column, CellRect);
                        end;
                      end;

                      // leave after first run if columns aren't used
                      if not UseColumns then
                        Break;
                    end
                    else
                      NextColumn := GetNextVisibleColumn(PaintInfo.Column);

                    SelectClipRgn(PaintInfo.Canvas.Handle, 0);
                    // Stop column loop if there are no further columns in the given window.
                    if (PaintInfo.CellRect.Left >= Window.Right) or (NextColumn = InvalidColumn) then
                      Break;

                    // Move on to next column which might not be the one immediately following the current one
                    // because of auto span feature.
                    PaintInfo.Position := Items[NextColumn].Position;

                    // Move clip rectangle and continue.
                    if coVisible in Items[NextColumn].Options then
                      with PaintInfo do
                      begin
                        TVirtualTreeColumnCracker(Items[NextColumn]).GetAbsoluteBounds(CellRect.Left, CellRect.Right);
                        CellRect.Bottom := Node.NodeHeight;
                        ContentRect.Bottom := Node.NodeHeight;
                      end;
                  end;
                end;

                // This node is finished, notify descendants/application.
                with PaintInfo do
                begin
                  DoAfterItemPaint(Canvas, Node, R);

                  // Final touch for this node: mark it if it is the current drop target node.
                  if (Node = FDropTargetNode) and (toShowDropmark in FOptions.PaintOptions) and
                    (poDrawDropMark in PaintOptions) then
                    DoPaintDropMark(Canvas, Node, R);
                end;
              end; // if not DoBeforeItemPaint()  (no custom drawing)


              with PaintInfo.Canvas do
              begin
                if DrawSelectionRect then
                begin
                  PaintSelectionRectangle(PaintInfo.Canvas, Window.Left, SelectionRect, Rect(0, 0, PaintWidth,
                    CurrentNodeHeight));
                end;

                // Put the constructed node image onto the target canvas.
                if not (poUnbuffered in PaintOptions) then
                  with NodeBitmap do
                    BitBlt(TargetCanvas.Handle, TargetRect.Left, TargetRect.Top, TargetRect.Width, TargetRect.Height, Canvas.Handle, Window.Left, 0, SRCCOPY);
              end;
            end;

            Inc(TargetRect.Top, PaintInfo.Node.NodeHeight);
            if TargetRect.Top >= MaximumBottom then
              Break;

            // Keep selection rectangle coordinates in sync.
            if DrawSelectionRect then
              OffsetRect(SelectionRect, 0, -PaintInfo.Node.NodeHeight);

            // Advance to next visible node.
            PaintInfo.Node := GetNextVisible(PaintInfo.Node, True);
          end;
        end;

        // Erase rest of window not covered by a node.
        if TargetRect.Top < MaximumBottom then
        begin
          // Keep the horizontal target position to determine the selection rectangle offset later (if necessary).
          BaseOffset := Target.X;
          Target := TargetRect.TopLeft;
          R := Rect(TargetRect.Left, 0, TargetRect.Left, MaximumBottom - Target.Y);
          TargetRect := Rect(0, 0, MaximumRight - Target.X, MaximumBottom - Target.Y);

          if not (poUnbuffered in PaintOptions) then
          begin
            // Avoid unnecessary copying of bitmap content. This will destroy the DC handle too.
            NodeBitmap.Height := 0;
            NodeBitmap.PixelFormat := pf32Bit;
            NodeBitmap.SetSize(TargetRect.Right - TargetRect.Left, TargetRect.Bottom - TargetRect.Top);
          end;

          // Call back application/descendants whether they want to erase this area.
          if not DoPaintBackground(PaintInfo.Canvas, TargetRect) then
          begin
            if UseBackground then
            begin
              SetCanvasOrigin(PaintInfo.Canvas, 0, 0);
              if toStaticBackground in TreeOptions.PaintOptions then
                StaticBackground(FBackground, PaintInfo.Canvas, Target, TargetRect, FColors.BackGroundColor)
              else
                TileBackground(FBackground, PaintInfo.Canvas, Target, TargetRect, FColors.BackGroundColor);
            end
            else
            begin
              // Consider here also colors of the columns.
              SetCanvasOrigin(PaintInfo.Canvas, Target.X, 0); // This line caused issue #313 when it was placed above the if-statement
              if UseColumns then
              begin
                with FHeader.Columns do
                begin
                  // If there is no content in the tree then the first column has not yet been determined.
                  if FirstColumn = InvalidColumn then
                  begin
                    FirstColumn := GetFirstVisibleColumn;
                    repeat
                      if FirstColumn <> InvalidColumn then
                      begin
                        R.Left := Items[FirstColumn].Left;
                        R.Right := R.Left +  Items[FirstColumn].Width;
                        if R.Right > TargetRect.Left then
                          Break;
                        FirstColumn := GetNextVisibleColumn(FirstColumn);
                      end;
                    until FirstColumn = InvalidColumn;
                  end
                  else
                  begin
                    R.Left := Items[FirstColumn].Left;
                    R.Right := R.Left +  Items[FirstColumn].Width;
                  end;

                  // Initialize MaxRight.
                  MaxRight := Target.X - 1;

                  PaintInfo.Canvas.Font.Color := FColors.GridLineColor;
                  while (FirstColumn <> InvalidColumn) and (MaxRight < TargetRect.Right + Target.X) do
                  begin
                    // Determine left and right coordinate of the current column
                    ColLeft := Items[FirstColumn].Left;
                    ColRight := (ColLeft + Items[FirstColumn].Width);

                    // Check wether this column needs to be painted at all.
                    if (ColRight >= MaxRight) then
                    begin
                      R.Left := MaxRight;     // Continue where we left off
                      R.Right := ColRight;    // Paint to the right of the column
                      MaxRight := ColRight;   // And record were to start the next column.

                      if (poGridLines in PaintOptions) and
                         (toFullVertGridLines in FOptions.PaintOptions) and
                         (toShowVertGridLines in FOptions.PaintOptions) and
                         (not (hoAutoResize in FHeader.Options) or (Cardinal(FirstColumn) < TColumnPosition(Count - 1))) then
                      begin
                        DrawGridVLine(PaintInfo, R.Top, R.Bottom, R.Right - 1);
                        Dec(R.Right);
                      end;

                      if not (coParentColor in Items[FirstColumn].Options) then
                        PaintInfo.Canvas.Brush.Color := Items[FirstColumn].Color
                      else
                        PaintInfo.Canvas.Brush.Color := FColors.BackGroundColor;
                      PaintInfo.Canvas.FillRect(R);
                    end;
                    FirstColumn := GetNextVisibleColumn(FirstColumn);
                  end;

                  // Erase also the part of the tree not covert by a column.
                  if R.Right < TargetRect.Right + Target.X then
                  begin
                    R.Left := R.Right;
                    R.Right := TargetRect.Right + Target.X;
                    // Prevent erasing the last vertical grid line.
                    if (poGridLines in PaintOptions) and
                       (toFullVertGridLines in FOptions.PaintOptions) and (toShowVertGridLines in FOptions.PaintOptions) and
                       (not (hoAutoResize in FHeader.Options)) then
                      Inc(R.Left);
                    PaintInfo.Canvas.Brush.Color := FColors.BackGroundColor;
                    PaintInfo.Canvas.FillRect(R);
                  end;
                end;
                SetCanvasOrigin(PaintInfo.Canvas, 0, 0);
              end
              else
              begin
                // No columns nor bitmap background. Simply erase it with the tree color.
                SetCanvasOrigin(PaintInfo.Canvas, 0, 0);
                PaintInfo.Canvas.Brush.Color := FColors.BackGroundColor;
                PaintInfo.Canvas.FillRect(TargetRect);
              end;
            end;
          end;
          SetCanvasOrigin(PaintInfo.Canvas, 0, 0);

          if DrawSelectionRect then
          begin
            R := OrderRect(FNewSelRect);
            // Remap the selection rectangle to the current window of the tree.
            // Since Target has been used for other tasks BaseOffset got the left extent of the target position here.
            OffsetRect(R, -Target.X + BaseOffset - Window.Left, -Target.Y + FOffsetY);
            SetBrushOrigin(PaintInfo.Canvas, 0, Target.X and 1);
            PaintSelectionRectangle(PaintInfo.Canvas, 0, R, TargetRect);
          end;

          if not (poUnBuffered in PaintOptions) then
            with Target, NodeBitmap do
              BitBlt(TargetCanvas.Handle, X, Y, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
        end;
      finally
        PaintInfo.Canvas.Unlock;
        if poUnbuffered in PaintOptions then
          RestoreDC(TargetCanvas.Handle, SavedTargetDC)
        else
          NodeBitmap.Free;
      end;//try..finally

      if (FEmptyListMessage <> '') and  ((ChildCount[nil] = 0) or (GetFirstVisible = nil)) then
      begin
        // output a message if no items are to display
        Canvas.Font := Self.Font;
        Canvas.Font.Size := Round(Canvas.Font.Size * 1.25); // Use slightly larger font to attract awareness of user, there is enough space ince the list is empty.
        SetBkMode(TargetCanvas.Handle, TRANSPARENT);
        lEmptyListTextMargin := ScaledPixels(Max(cDefaultTextMargin, Self.TextMargin) * 2); // Since the list is empty and the font is slightly larger make sure text id not too close at the edges so that it looks good.
        R.Left := OffSetX + lEmptyListTextMargin;
        R.Top := lEmptyListTextMargin;
        R.Right := R.Left + Width - lEmptyListTextMargin;
        R.Bottom := Height - lEmptyListTextMargin;
        TargetCanvas.Font.Color := StyleServices.GetStyleFontColor(TStyleFont.sfTreeItemTextDisabled);//clGrayText;
        TargetCanvas.TextRect(R, FEmptyListMessage, [tfNoClip, tfLeft, tfWordBreak, tfExpandTabs]);
      end;

      DoAfterPaint(TargetCanvas);
    finally
      DoStateChange([], [tsPainting]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.PrepareDragImage(HotSpot: TPoint; const DataObject: TVTDragDataObject);

// Initiates an image drag operation. HotSpot is the position of the mouse in client coordinates.

var
  PaintOptions: TVTInternalPaintOptions;
  TreeRect,
  PaintRect: TRect;
  LocalSpot,
  PaintTarget: TPoint;
  lDragImage: TVTDragImage;                    // drag image management
  Image: TBitmap;

begin
  if CanShowDragImage then
  begin
    lDragImage := TVTDragImage.Create(Self);
    try
      // Determine the drag rectangle which is a square around the hot spot. Operate in virtual tree space.
      LocalSpot := HotSpot;
      Dec(LocalSpot.X, -FEffectiveOffsetX);
      Dec(LocalSpot.Y, FOffsetY);
      TreeRect := Rect(LocalSpot.X - FDragWidth div 2, LocalSpot.Y - FDragHeight div 2, LocalSpot.X + FDragWidth div 2,
        LocalSpot.Y + FDragHeight div 2);

      // Check that we have a valid rectangle.
      PaintRect := TreeRect;
      if TreeRect.Left < 0 then
      begin
        PaintTarget.X := -TreeRect.Left;
        PaintRect.Left := 0;
      end
      else
        PaintTarget.X := 0;
      if TreeRect.Top < 0 then
      begin
        PaintTarget.Y := -TreeRect.Top;
        PaintRect.Top := 0;
      end
      else
        PaintTarget.Y := 0;

      Image := TBitmap.Create;
      with Image do
      try
        PixelFormat := pf32Bit;
        SetSize(TreeRect.Right - TreeRect.Left, TreeRect.Bottom - TreeRect.Top);
        // Erase the entire image with the color key value, for the case not everything
        // in the image is covered by the tree image.
        Canvas.Brush.Color := FColors.BackGroundColor;
        Canvas.FillRect(Rect(0, 0, Width, Height));

        PaintOptions := [poDrawSelection, poSelectedOnly];
        if FDragImageKind = diMainColumnOnly then
          Include(PaintOptions, poMainOnly);
        PaintTree(Image.Canvas, PaintRect, PaintTarget, PaintOptions);

        // Once we have got the drag image we can convert all necessary coordinates into screen space.
        OffsetRect(TreeRect, -FEffectiveOffsetX, FOffsetY);
        HotSpot.X := Width div 2;
        HotSpot.Y := Height div 2;

        lDragImage.PrepareDrag(Image, HotSpot, DataObject, FColors.BackGroundColor);
      finally
        Image.Free;
      end;
    finally
      lDragImage.Free;
    end; // try..finally
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Print(Printer: TPrinter; PrintHeader: Boolean);

var
  SaveTreeFont: TFont;                 // Remembers the tree's current font.
  SaveHeaderFont: TFont;               // Remembers the header's current font.
  ImgRect,                             // Describes the dimensions of Image.
  TreeRect,                            // The total VTree dimensions.
  DestRect,                            // Dimensions of PrinterImage.
  SrcRect: TRect;                      // Clip dimensions from Image -> PrinterImage
  P: TPoint;                           // Used by PaintTree.
  Options: TVTInternalPaintOptions;    // Used by PaintTree.
  Image,                               // Complete Tree is drawn to this image.
  PrinterImage: TBitmap;               // This is the image that gets printed.
  SaveColor: TColor;                   // Remembers the VTree Color.
  pTxtHeight,                          // Height of font in the TPrinter.Canvas
  vTxtHeight,                          // Height of font in the VTree Canvas
  vPageWidth,
  vPageHeight,                         // Printer height in VTree resolution
  xPageNum, yPageNum,                  // # of pages (except the occasional last one)
  xPage, yPage: Integer;               // Loop counter
  Scale: Extended;                     // Scale factor between Printer Canvas and VTree Canvas
  LogFont: TLogFont;

begin
  if Assigned(Printer) then
  begin
    BeginUpdate;

    // Grid lines are the only parts which are desirable when printing.
    Options := [poGridLines];

    // Remember the tree font.
    SaveTreeFont := TFont.Create;
    SaveTreeFont.Assign(Font);
    // Create a new font for printing which does not use clear type output (but is antialiased, if possible)
    // and which has the highest possible quality.
    GetObject(Font.Handle, SizeOf(TLogFont), @LogFont);
    LogFont.lfQuality := ANTIALIASED_QUALITY;
    Font.Handle := CreateFontIndirect(LogFont);

    // Create an image that will hold the complete VTree
    Image := TBitmap.Create;
    Image.PixelFormat := pf32Bit;
    PrinterImage := nil;
    try
      TreeRect := GetTreeRect;

      Image.Width := TreeRect.Right - TreeRect.Left;
      P := Point(0, 0);
      if (hoVisible in FHeader.Options) and PrintHeader then
      begin
        Inc(TreeRect.Bottom, FHeader.Height);
        Inc(P.Y, FHeader.Height);
      end;
      Image.Height := TreeRect.Bottom - TreeRect.Top;

      ImgRect.Left := 0;
      ImgRect.Top := 0;
      ImgRect.Right := Image.Width;

      // Force the background to white color during the rendering.
      SaveColor := FColors.BackGroundColor;
      Color := clWhite;
      // Print header if it is visible.
      if (hoVisible in FHeader.Options) and PrintHeader then
      begin
        SaveHeaderFont := TFont.Create;
        try
          SaveHeaderFont.Assign(FHeader.Font);
          // Create a new font for printing which does not use clear type output (but is antialiased, if possible)
          // and which has the highest possible quality.
          GetObject(FHeader.Font.Handle, SizeOf(TLogFont), @LogFont);
          LogFont.lfQuality := ANTIALIASED_QUALITY;
          FHeader.Font.Handle := CreateFontIndirect(LogFont);
          ImgRect.Bottom := FHeader.Height;
          FHeader.Columns.PaintHeader(Image.Canvas.Handle, ImgRect, 0);
          FHeader.Font := SaveHeaderFont;
        finally
          SaveHeaderFont.Free;
        end;
      end;
      // The image's height is already adjusted for the header if it is visible.
      ImgRect.Bottom := Image.Height;

      PaintTree(Image.Canvas, ImgRect, P, Options, pf32Bit);
      Color := SaveColor;

      // Activate the printer
      Printer.BeginDoc;
      Printer.Canvas.Font := Font;

      // Now we can calculate the scaling :
      pTxtHeight := Printer.Canvas.TextHeight('Tj');
      vTxtHeight := Canvas.TextHeight('Tj');

      Scale := pTxtHeight / vTxtHeight;

      // Create an Image that has the same dimensions as the printer canvas but
      // scaled to the VTree resolution:
      PrinterImage := TBitmap.Create;

      vPageHeight := Round(Printer.PageHeight / Scale);
      vPageWidth := Round(Printer.PageWidth / Scale);

      // We do a minumum of one page.
      xPageNum := Trunc(Image.Width / vPageWidth);
      yPageNum := Trunc(Image.Height / vPageHeight);

      PrinterImage.SetSize(vPageWidth, vPageHeight);

      // Split vertically:
      for yPage := 0 to yPageNum do
      begin
        DestRect.Left := 0;
        DestRect.Top := 0;
        DestRect.Right := PrinterImage.Width;
        DestRect.Bottom := PrinterImage.Height;

        // Split horizontally:
        for xPage := 0 to xPageNum do
          begin
            SrcRect.Left := vPageWidth * xPage;
            SrcRect.Top := vPageHeight * yPage;
            SrcRect.Right := vPageWidth * xPage + PrinterImage.Width;
            SrcRect.Bottom := SrcRect.Top + vPageHeight;

            // Clear the image
            PrinterImage.Canvas.Brush.Color := clWhite;
            PrinterImage.Canvas.FillRect(Rect(0, 0, PrinterImage.Width, PrinterImage.Height));
            PrinterImage.Canvas.CopyRect(DestRect, Image.Canvas, SrcRect);
            PrtStretchDrawDIB(Printer.Canvas, Rect(0, 0, Printer.PageWidth, Printer.PageHeight - 1), PrinterImage);
            if xPage <> xPageNum then
              Printer.NewPage;
          end;
        if yPage <> yPageNum then
          Printer.NewPage;
      end;

      // Restore tree font.
      Font := SaveTreeFont;
      SaveTreeFont.Free;
      Printer.EndDoc;
    finally
      PrinterImage.Free;
      Image.Free;
      EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ProcessDrop(const DataObject: TVTDragDataObject; TargetNode: PVirtualNode; var Effect: Integer;
  Mode: TVTNodeAttachMode): Boolean;

// Recreates the (sub) tree structure serialized into memory and provided by DataObject. The new nodes are attached to
// the passed node or FRoot if TargetNode is nil.
// Returns True on success, i.e. the CF_VIRTUALTREE format is supported by the data object and the structure could be
// recreated, otherwise False.

var
  Source: TBaseVirtualTree;

begin
  Result := False;
  if Mode = amNoWhere then
    Effect := DROPEFFECT_NONE
  else
  begin
    BeginUpdate;
    // try to get the source tree of the operation
    Source := TVTDragManager.GetTreeFromDataObject(DataObject);
    if Assigned(Source) then
      Source.BeginUpdate;
    try
      try
        // Before adding the new nodes try to optimize the operation if source and target tree reside in
        // the same application and operation is a move.
        if ((Effect and DROPEFFECT_MOVE) <> 0) and Assigned(Source) then
        begin
          // If both copy and move are specified then prefer a copy because this is not destructing.
          Result := ProcessOLEData(Source, DataObject, TargetNode, Mode, (Effect and DROPEFFECT_COPY) = 0);
          // Since we made an optimized move or a copy there's no reason to act further after DoDragging returns.
          Effect := DROPEFFECT_NONE;
        end
        else
          // Act only if move or copy operation is requested.
          if (Effect and (DROPEFFECT_MOVE or DROPEFFECT_COPY)) <> 0 then
            Result := ProcessOLEData(Source, DataObject, TargetNode, Mode, False)
          else
            Result := False;
      except
        Effect := DROPEFFECT_NONE;
      end;
    finally
      if Assigned(Source) then
        Source.EndUpdate;
      EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // needed to handle OLE global memory objects
  TOLEMemoryStream = class(TCustomMemoryStream)
  public
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

function TOLEMemoryStream.Write(const Buffer; Count: Integer): Integer;

begin
  raise EStreamError.CreateRes(PResStringRec(@SCantWriteResourceStreamError));
end;

//----------------- TBaseVirtualTree -----------------------------------------------------------------------------

procedure TBaseVirtualTree.DoDrawHint(Canvas: TCanvas; Node: PVirtualNode; R:
    TRect; Column: TColumnIndex);

begin
  if Assigned(FOnDrawHint) then
    FOnDrawHint(Self, Canvas, Node, R, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoGetHintSize(Node: PVirtualNode; Column:
    TColumnIndex; var R: TRect);

begin
  if Assigned(FOnGetHintSize) then
    FOnGetHintSize(Self, Node, Column, R);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.DoGetHintKind(Node: PVirtualNode; Column:
    TColumnIndex; var Kind: TVTHintKind);

begin
  if Assigned(FOnGetHintKind) then
    FOnGetHintKind(Self, Node, Column, Kind)
  else
    Kind := DefaultHintKind;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.GetDefaultHintKind: TVTHintKind;

begin
  Result := vhkText;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ProcessOLEData(Source: TBaseVirtualTree; const DataObject: IDataObject; TargetNode: PVirtualNode;
  Mode: TVTNodeAttachMode; Optimized: Boolean): Boolean;

// Recreates the (sub) tree structure serialized into memory and provided by DataObject. The new nodes are attached to
// the passed node or FRoot if TargetNode is nil according to Mode. Optimized can be set to True if the entire operation
// happens within the same process (i.e. sender and receiver of the OLE operation are located in the same process).
// Optimize = True makes only sense if the operation to carry out is a move hence it is also the indication of the
// operation to be done here. Source is the source of the OLE data and only of use (and usually assigned) when
// an OLE operation takes place in the same application.
// Returns True on success, i.e. the CF_VIRTUALTREE format is supported by the data object and the structure could be
// recreated, otherwise False.

var
  Medium: TStgMedium;
  Stream: TStream;
  Data: Pointer;
  Node: PVirtualNode;
  Nodes: TNodeArray;
  I: Integer;
  Res: HRESULT;
  ChangeReason: TChangeReason;

begin
  Nodes := nil;
  // Check the data format available by the data object.
  with StandardOLEFormat do
  begin
    // Read best format.
    cfFormat := CF_VIRTUALTREE;
  end;
  Result := DataObject.QueryGetData(StandardOLEFormat) = S_OK;
  if Result and not (toReadOnly in FOptions.MiscOptions) then
  begin
    BeginUpdate;
    Result := False;
    try
      if TargetNode = nil then
        TargetNode := FRoot;
      if TargetNode = FRoot then
      begin
        case Mode of
          amInsertBefore:
            Mode := amAddChildFirst;
          amInsertAfter:
            Mode := amAddChildLast;
        end;
      end;

      // Optimized means source is known and in the same process so we can access its pointers, which avoids duplicating
      // the data while doing a serialization. Can only be used with cut'n paste and drag'n drop with move effect.
      if Optimized then
      begin
        if tsOLEDragging in Source.FStates then
          Nodes := Source.FDragSelection
        else
          Nodes := Source.GetSortedCutCopySet(True);

        if Mode in [amInsertBefore,amAddChildLast] then
        begin
          for I := 0 to High(Nodes) do
            if not HasAsParent(TargetNode, Nodes[I]) then
              Source.MoveTo(Nodes[I], TargetNode, Mode, False);
        end
        else
        begin
          for I := High(Nodes) downto 0 do
            if not HasAsParent(TargetNode, Nodes[I]) then
              Source.MoveTo(Nodes[I], TargetNode, Mode, False);
        end;
        Result := True;
      end
      else
      begin
        if Source = Self then
          ChangeReason := crNodeCopied
        else
          ChangeReason := crNodeAdded;
        Res := DataObject.GetData(StandardOLEFormat, Medium);
        if Res = S_OK then
        begin
          case Medium.tymed of
            TYMED_ISTREAM, // IStream interface
            TYMED_HGLOBAL: // global memory block
              begin
                Stream := nil;
                if Medium.tymed = TYMED_ISTREAM then
                  Stream := TOLEStream.Create(IUnknown(Medium.stm) as IStream)
                else
                begin
                  Data := GlobalLock(Medium.hGlobal);
                  if Assigned(Data) then
                  begin
                    // Get the total size of data to retrieve.
                    I := PCardinal(Data)^;
                    Inc(PCardinal(Data));
                    Stream := TOLEMemoryStream.Create;
                    TOLEMemoryStream(Stream).SetPointer(Data, I);
                  end;
                end;

                if Assigned(Stream) then
                try
                  while Stream.Position < Stream.Size do
                  begin
                    Node := MakeNewNode;
                    InternalConnectNode(Node, TargetNode, Self, Mode);
                    InternalAddFromStream(Stream, VTTreeStreamVersion, Node);
                    // This seems a bit strange because of the callback for granting to add the node
                    // which actually comes after the node has been added. The reason is that the node must
                    // contain valid data otherwise I don't see how the application can make a funded decision.
                    if not DoNodeCopying(Node, TargetNode) then
                    begin
                      DeleteNode(Node);
                    end
                    else
                    begin
                      DoNodeCopied(Node);
                      StructureChange(Node, ChangeReason);
                      // In order to maintain the same node order when restoring nodes in the case of amInsertAfter
                      // we have to move the reference node continously. Othwise we would end up with reversed node order.
                      if Mode = amInsertAfter then
                        TargetNode := Node;
                    end;
                  end;
                  Result := True;
                finally
                  Stream.Free;
                  if Medium.tymed = TYMED_HGLOBAL then
                    GlobalUnlock(Medium.hGlobal);
                end;
              end;
          end;
          ReleaseStgMedium(Medium);
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ReinitChildren(Node: PVirtualNode; Recursive:
    Boolean; ForceReinit: Boolean = False);

// Forces all child nodes of Node to be reinitialized.
// If Recursive is True then also the grandchildren are reinitialized.

var
  Run: PVirtualNode;

begin
  if Assigned(Node) then
  begin
    InitChildren(Node);
    Run := Node.FirstChild;
  end
  else
  begin
    InitChildren(FRoot);
    Run := FRoot.FirstChild;
  end;

  while Assigned(Run) do
  begin
    ReinitNode(Run, Recursive, ForceReinit);
    Run := Run.NextSibling;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ReinitNode(Node: PVirtualNode; Recursive: Boolean;
    ForceReinit: Boolean = False);

// Reinitializes Node if it has previously been initialized
// If Recursive, initialized children are also re-initialized recursively
// If ForceReinit, Node is always initialized and if Recursive children
// are always re-initialized as well
// InitNode is called with ivsReInit in InitialStates, if the Node has already
// been initialized.

begin
  if Assigned(Node) and (Node <> FRoot) then
  begin
    // Remove dynamic styles.
    Node.States := Node.States - [vsChecking, vsCutOrCopy, vsDeleting, vsHeightMeasured];
    if (vsInitialized in Node.States) or ForceReinit then
      InitNode(Node);
  end
  else if not Assigned(Node) then
    Node := FRoot;

  // Prevent previoulsy uninitilaized children from being initialized
  // unless ForceReinit is True. Issue #1145
  if Recursive and (ForceReinit or (Node.ChildCount > 0)) then
    ReinitChildren(Node, True, ForceReinit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.RepaintNode(Node: PVirtualNode);

// Causes an immediate repaint of the given node.

var
  R: Trect;

begin
  if Assigned(Node) and (Node <> FRoot) then
  begin
    R := GetDisplayRect(Node, NoColumn, False);
    RedrawWindow(@R, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_NOERASE or RDW_VALIDATE or RDW_NOCHILDREN);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ResetNode(Node: PVirtualNode);

// Deletes all children of the given node and marks it as being uninitialized.

begin
  DoCancelEdit;
  if (Node = nil) or (Node = FRoot) then
    Clear
  else
  begin
    DoReset(Node);
    DeleteChildren(Node);
    // Remove initialized and other dynamic styles, keep persistent styles.
    Node.States := Node.States - [vsInitialized, vsChecking, vsCutOrCopy, vsDeleting, vsHasChildren, vsExpanded,
      vsHeightMeasured];
    InvalidateNode(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SaveToFile(const FileName: TFileName);

// Saves the entire content of the tree into a file (see further notes in SaveToStream).

var
  FileStream: TFileStream;

begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SaveToStream(Stream: TStream; Node: PVirtualNode = nil);

// Saves Node and all its children to Stream. If Node is nil then all top level nodes will be stored.
// Note: You should be careful about assuming what is actually saved. The problem here is that we are dealing with
//       virtual data. The tree can so not know what it has to save. The only fact we reliably know is the tree's
//       structure. To be flexible for future enhancements as well as unknown content (unknown to the tree class which
//       is saving/loading the stream) a chunk based approach is used here. Every tree class handles only those
//       chunks which are not handled by an anchestor class and are known by the class.
//
// The base tree class saves only the structure of the tree along with application provided data. descendants may
// optionally add their own chunks to store additional information. See: WriteChunks.

var
  Count: Cardinal;

begin
  Stream.Write(MagicID, SizeOf(MagicID));
  if Node = nil then
  begin
    // Keep number of top level nodes for easy restauration.
    Count := FRoot.ChildCount;
    Stream.WriteBuffer(Count, SizeOf(Count));

    // Save entire tree here.
    Node := FRoot.FirstChild;
    while Assigned(Node) do
    begin
      WriteNode(Stream, Node);
      Node := Node.NextSibling;
    end;
  end
  else
  begin
    Count := 1;
    Stream.WriteBuffer(Count, SizeOf(Count));
    WriteNode(Stream, Node);
  end;
  if Assigned(FOnSaveTree) then
    FOnSaveTree(Self, Stream);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ScrollIntoView(Node: PVirtualNode; Center: Boolean; Horizontally: Boolean = False): Boolean;

// Scrolls the tree so that the given node is in the client area and returns True if the tree really has been
// scrolled (e.g. to avoid further updates) else returns False. If extened focus is enabled then the tree will also
// be horizontally scrolled if needed.
// Note: All collapsed parents of the node are expanded.

var
  R: TRect;
  Run: PVirtualNode;
  UseColumns,
  HScrollBarVisible: Boolean;
  OldOffsetY: TDimension;
  ScrolledVertically,
  ScrolledHorizontally: Boolean;

begin
  ScrolledVertically := False;
  ScrolledHorizontally := False;

  if Assigned(Node) and (Node <> FRoot) and HandleAllocated then // We don't want to create the handle if it has not yet been created, see issue #897
  begin
    // Make sure all parents of the node are expanded.
    Run := Node.Parent;
    while Run <> FRoot do
    begin
      if not (vsExpanded in Run.States) then
        ToggleNode(Run);
      Run := Run.Parent;
    end;
    UseColumns := FHeader.UseColumns;
    if UseColumns and FHeader.Columns.IsValidColumn(FFocusedColumn) then
      R := GetDisplayRect(Node, FFocusedColumn, not (toGridExtensions in FOptions.MiscOptions))
    else
      R := GetDisplayRect(Node, NoColumn, not (toGridExtensions in FOptions.MiscOptions));

    // The returned rectangle can never be empty after the expand code above.
    // 1) scroll vertically
    OldOffsetY := FOffsetY;
    if R.Top < 0 then
    begin
      if Center then
        SetOffsetY(FOffsetY - R.Top + Divide(ClientHeight, 2))
      else
        SetOffsetY(FOffsetY - R.Top);
    end
    else
      if (R.Bottom > ClientHeight) or Center then
      begin
        HScrollBarVisible := (ScrollBarOptions.ScrollBars in [System.UITypes.TScrollStyle.ssBoth, System.UITypes.TScrollStyle.ssHorizontal]) and
          (ScrollBarOptions.AlwaysVisible or (FRangeX > ClientWidth));
        if Center then
          SetOffsetY(FOffsetY - R.Bottom + Divide(ClientHeight, 2))
        else
          SetOffsetY(FOffsetY - R.Bottom + ClientHeight);
        // When scrolling up and the horizontal scroll appears because of the operation
        // then we have to move up the node the horizontal scrollbar's height too
        // in order to avoid that the scroll bar hides the node which we wanted to have in view.
        if not UseColumns and not HScrollBarVisible and (FRangeX > ClientWidth) then
          SetOffsetY(FOffsetY - GetSystemMetrics(SM_CYHSCROLL));
      end;
    ScrolledVertically := OldOffsetY <> FOffsetY;

    if Horizontally then
      // 2) scroll horizontally
      // Center only if there is enough space for the focused column, otherwise left align, see issue #397.
      ScrolledHorizontally := ScrollIntoView(FFocusedColumn, Center and (R.Width <= (ClientWidth - Header.Columns.GetVisibleFixedWidth)), Node);
  end;

  Result := ScrolledVertically or ScrolledHorizontally;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.ScaledPixels(pPixels: TDimension): TDimension;

   /// Returns the given pixels scaled to the current dpi assuming that we designed at 96dpi (100%)
begin
  Result := MulDiv(pPixels, {$if CompilerVersion > 31}Self.FCurrentPPI{$else}Screen.PixelsPerInch{$ifend}, 96);
end;

function TBaseVirtualTree.ScrollIntoView(Column: TColumnIndex; Center: Boolean; Node: PVirtualNode = nil): Boolean;

// Scrolls the columns so that the given column is in the client area and returns True if the columns really have been
// scrolled (e.g. to avoid further updates) else returns False.

var
  ColumnLeft,
  ColumnRight: TDimension;
  NewOffset,
  OldOffset: TDimension;
  R: TRect;

begin
  Result := False;

  if FHeader.UseColumns and FHeader.Columns.IsValidColumn(Column) then begin
    ColumnLeft := Header.Columns.Items[Column].Left;
    ColumnRight := ColumnLeft + Header.Columns.Items[Column].Width;
  end else if Assigned(Node) and (toCenterScrollIntoView in FOptions.SelectionOptions) then begin
    Center := False;
    R := GetDisplayRect(Node, NoColumn, not (toGridExtensions in FOptions.MiscOptions));
    ColumnLeft := R.Left;
    ColumnRight := R.Right;
  end else
    Exit;

  OldOffset := FOffsetX;
  NewOffset := FEffectiveOffsetX;
  if not (FHeader.UseColumns and (coFixed in Header.Columns[Column].Options)) and (not Center) then
  begin
    if ColumnRight > ClientWidth then
      NewOffset := FEffectiveOffsetX + Min(ColumnRight - ClientWidth,
       - (Header.Columns.GetVisibleFixedWidth - ColumnLeft))
    else if ColumnLeft < Header.Columns.GetVisibleFixedWidth then
      NewOffset := FEffectiveOffsetX - (Header.Columns.GetVisibleFixedWidth - ColumnLeft);
    if NewOffset <> FEffectiveOffsetX then
    begin
      if UseRightToLeftAlignment then
        SetOffsetX(-FRangeX + ClientWidth + NewOffset)
      else
        SetOffsetX(-NewOffset);
    end;
  end
  else if Center then
  begin
    NewOffset := FEffectiveOffsetX + ColumnLeft - Divide(Header.Columns.GetVisibleFixedWidth, 2) - Divide(ClientWidth, 2) + Divide(ColumnRight - ColumnLeft, 2);
    if NewOffset <> FEffectiveOffsetX then
    begin
      if UseRightToLeftAlignment then
        SetOffsetX(-FRangeX + ClientWidth + NewOffset)
      else
        SetOffsetX(-NewOffset);
    end;
  end;
  Result := OldOffset <> FOffsetX;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SelectAll(VisibleOnly: Boolean);

// Select all nodes in the tree.
// If VisibleOnly is True then only visible nodes are selected.

var
  Run: PVirtualNode;
  NextFunction: TGetNextNodeProc;
begin
  if not FSelectionLocked and (toMultiSelect in FOptions.SelectionOptions) then
  begin
    ClearTempCache;
    if VisibleOnly then
    begin
      Run := GetFirstVisible(nil, True);
      NextFunction := GetNextVisible;
    end
    else
    begin
      Run := GetFirst;
      NextFunction := GetNext;
    end;
    BeginUpdate();  // Improve performance, see issue #690
    try
      while Assigned(Run) do
      begin
        if not(vsSelected in Run.States) then
          InternalCacheNode(Run);
        Run := NextFunction(Run);
      end;//while
      if FTempNodeCount > 0 then
        AddToSelection(FTempNodeCache, FTempNodeCount);
      ClearTempCache;
    finally
      EndUpdate();
    end;//try..finally
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.Sort(Node: PVirtualNode; Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean = True);

// Sorts the given node. The application is queried about how to sort via the OnCompareNodes event.
// Column is simply passed to the the compare function so the application can also sort in a particular column.
// In order to free the application from taking care about the sort direction the parameter Direction is used.
// This way the application can always sort in increasing order, while this method reorders nodes according to this flag.

  //--------------- local functions -------------------------------------------

  function MergeAscending(A, B: PVirtualNode): PVirtualNode;

  // Merges A and B (which both must be sorted via Compare) into one list.

  var
    Dummy: TVirtualNode;
    CompareResult: Integer;
  begin
    // This avoids checking for Result = nil in the loops.
    Result := @Dummy;
    while Assigned(A) and Assigned(B) do
    begin
      if OperationCanceled then
        CompareResult := 0
      else
        CompareResult := DoCompare(A, B, Column);

      if CompareResult <= 0 then
      begin
        Result.SetNextSibling(A);
        Result := A;
        A := A.NextSibling;
      end
      else
      begin
        Result.SetNextSibling(B);
        Result := B;
        B := B.NextSibling;
      end;
    end;

    // Just append the list which is not nil (or set end of result list to nil if both lists are nil).
    if Assigned(A) then
      Result.SetNextSibling(A)
    else
      Result.SetNextSibling(B);
    // return start of the new merged list
    Result := Dummy.NextSibling;
  end;

  //---------------------------------------------------------------------------

  function MergeDescending(A, B: PVirtualNode): PVirtualNode;

  // Merges A and B (which both must be sorted via Compare) into one list.

  var
    Dummy: TVirtualNode;
    CompareResult: Integer;

  begin
    // this avoids checking for Result = nil in the loops
    Result := @Dummy;
    while Assigned(A) and Assigned(B) do
    begin
      if OperationCanceled then
        CompareResult := 0
      else
        CompareResult := DoCompare(A, B, Column);

      if CompareResult >= 0 then
      begin
        Result.SetNextSibling(A);
        Result := A;
        A := A.NextSibling;
      end
      else
      begin
        Result.SetNextSibling(B);
        Result := B;
        B := B.NextSibling;
      end;
    end;

    // Just append the list which is not nil (or set end of result list to nil if both lists are nil).
    if Assigned(A) then
      Result.SetNextSibling(A)
    else
      Result.SetNextSibling(B);
    // Return start of the newly merged list.
    Result := Dummy.NextSibling;
  end;

  //---------------------------------------------------------------------------

  function MergeSortAscending(var Node: PVirtualNode; N: Cardinal): PVirtualNode;

  // Sorts the list of nodes given by Node (which must not be nil).

  var
    A, B: PVirtualNode;

  begin
    if N > 1 then
    begin
      A := MergeSortAscending(Node, N div 2);
      B := MergeSortAscending(Node, (N + 1) div 2);
      Result := MergeAscending(A, B);
    end
    else
    begin
      Result := Node;
      Node := Node.NextSibling;
      Result.SetNextSibling(nil);
    end;
  end;

  //---------------------------------------------------------------------------

  function MergeSortDescending(var Node: PVirtualNode; N: Cardinal): PVirtualNode;

  // Sorts the list of nodes given by Node (which must not be nil).

  var
    A, B: PVirtualNode;

  begin
    if N > 1 then
    begin
      A := MergeSortDescending(Node, N div 2);
      B := MergeSortDescending(Node, (N + 1) div 2);
      Result := MergeDescending(A, B);
    end
    else
    begin
      Result := Node;
      Node := Node.NextSibling;
      Result.SetNextSibling(nil);
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  Run: PVirtualNode;
  Index: Cardinal;

begin
  InterruptValidation;
  if tsEditPending in FStates then
  begin
    StopTimer(EditTimer);
    DoStateChange([], [tsEditPending]);
  end;

  if not (tsEditing in FStates) or DoEndEdit then
  begin
    if Node = nil then
      Node := FRoot;
    if vsHasChildren in Node.States then
    begin
      if (Node.ChildCount = 0) and DoInit then
        InitChildren(Node);
      // Make sure the children are valid, so they can be sorted at all.
      if DoInit and (Node.ChildCount > 0) then
        ValidateChildren(Node, False);
      // Child count might have changed.
      if Node.ChildCount > 1 then
      begin
        StartOperation(okSortNode);
        try
          // Sort the linked list, check direction flag only once.
          if Direction = sdAscending then
            Node.SetFirstChild(MergeSortAscending(Node.FirstChild, Node.ChildCount))
          else
            Node.SetFirstChild(MergeSortDescending(Node.FirstChild, Node.ChildCount));
        finally
          EndOperation(okSortNode);
        end;
        // Consolidate the child list finally.
        Run := Node.FirstChild;
        Run.SetPrevSibling(nil);
        Index := 0;
        repeat
          Run.SetIndex(Index);
          System.Inc(Index);
          if Run.NextSibling = nil then
            Break;
          Run.NextSibling.SetPrevSibling(Run);
          Run := Run.NextSibling;
        until False;
        Node.SetLastChild(Run);

        InvalidateCache;
      end;
      if FUpdateCount = 0 then
      begin
        ValidateCache;
        Invalidate;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.SortTree(Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean = True);

  //--------------- local function --------------------------------------------

  procedure DoSort(Node: PVirtualNode);

  // Recursively sorts Node and its child nodes.

  var
    Run: PVirtualNode;

  begin
    Sort(Node, Column, Direction, DoInit);
    // Recurse to next level
    Run := Node.FirstChild;
    while Assigned(Run) and not FOperationCanceled do
    begin
      if DoInit and not (vsInitialized in Run.States) then
        InitNode(Run);
      if (vsInitialized in Run.States) and (not (toAutoSort in TreeOptions.AutoOptions) or Expanded[Run]) then // There is no need to sort collapsed branches
        DoSort(Run);
      Run := Run.NextSibling;
    end;
  end;

  //--------------- end local function ----------------------------------------

begin
  if RootNode.TotalCount <= 2 then
    Exit;//Nothing to do if there are one or zero nodes. RootNode.TotalCount is 1 if there are no nodes in the treee as the root node counts too here.

  if not Assigned(FRoot.FirstChild) then
    Exit; // Sorting should not initialize the root nodes

  // Instead of wrapping the sort using BeginUpdate/EndUpdate simply the update counter
  // is modified. Otherwise the EndUpdate call will recurse here.
  System.Inc(FUpdateCount);
  try
    if Column > InvalidColumn then
    begin
      StartOperation(okSortTree);
      try
        DoSort(FRoot);
      finally
        EndOperation(okSortTree);
      end;
    end;
    InvalidateCache;
  finally
    if FUpdateCount > 0 then
      System.Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      ValidateCache;
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ToggleNode(Node: PVirtualNode);

// Changes a node's expand state to the opposite state.

var
  Child,
  FirstVisible: PVirtualNode;
  HeightDelta,
  StepsR1,
  StepsR2: TDimension;
  Steps: Integer;
  TogglingTree,
  ChildrenInView,
  NeedFullInvalidate,
  NeedUpdate,
  NodeInView,
  PosHoldable,
  TotalFit: Boolean;
  ToggleData: TToggleAnimationData;

  //--------------- local function --------------------------------------------

  procedure PrepareAnimation;

  // Prepares ToggleData.

  var
    R: TRect;
    S: TDimension;
    M: TToggleAnimationMode;

  begin
    with ToggleData do
    begin
      Window := Handle;
      DC := TControlCanvas.Create;
      DC.Control := Self;

      if (toShowBackground in FOptions.PaintOptions) and Assigned(FBackground.Graphic) then
        Self.Brush.Style := bsClear
      else
      begin
        Self.Brush.Style := bsSolid;
        Self.Brush.Color := FColors.BackGroundColor;
      end;

      Brush := Self.Brush;

      if (Mode1 <> tamNoScroll) and (Mode2 <> tamNoScroll) then
      begin
        if StepsR1 < StepsR2 then
        begin
          // As the primary rectangle is always R1 we will get a much smoother
          // animation if R1 is the one that will be scrolled more.
          R := R2;
          R2 := R1;
          R1 := R;

          M := Mode2;
          Mode2 := Mode1;
          Mode1 := M;

          S := StepsR2;
          StepsR2 := StepsR1;
          StepsR1 := S;
        end;
        ScaleFactor := StepsR2 / StepsR1;
        MissedSteps := 0;
      end;

      if Mode1 <> tamNoScroll then
        Steps := StepsR1
      else
        Steps := StepsR2;
    end;
  end;

  //--------------- end local function ----------------------------------------

begin
  Assert(Assigned(Node), 'Node must not be nil.');

  TogglingTree := tsToggling in FStates;
  ChildrenInView := False;
  HeightDelta := 0;
  NeedFullInvalidate := False;
  NeedUpdate := False;
  NodeInView := False;
  PosHoldable := False;
  TotalFit := False;

  // We don't need to switch the expand state if the node is being deleted otherwise some
  // updates (e.g. visible node count) are done twice with disasterous results).
  if [vsDeleting, vsToggling] * Node.States = [] then
  begin
    try
      DoStateChange([tsToggling]);
      Include(Node.States, vsToggling);

      if vsExpanded in Node.States then
      begin
        if DoCollapsing(Node) then
        begin
          NeedUpdate := True;

          // Calculate the height delta right now as we need it for toChildrenAbove anyway.
          HeightDelta := -Node.TotalHeight + NodeHeight[Node];
          if (FUpdateCount = 0) and (toAnimatedToggle in FOptions.AnimationOptions) and not
             (tsCollapsing in FStates) then
          begin
            if tsHint in Self.FStates then
              Application.CancelHint;
            UpdateWindow();

            // animated collapsing
            with ToggleData do
            begin
              // Determine the animation behaviour and rectangle. If toChildrenAbove is set, the behaviour is depending
              // on the position of the node to be collapsed.
              R1 := GetDisplayRect(Node, NoColumn, False);
              Mode2 := tamNoScroll;
              if toChildrenAbove in FOptions.PaintOptions then
              begin
                PosHoldable := (FOffsetY + (Node.TotalHeight - NodeHeight[Node])) <= 0;
                NodeInView := R1.Top < ClientHeight;

                StepsR1 := 0;
                if NodeInView then
                begin
                  if PosHoldable or not (toAdvancedAnimatedToggle in FOptions.AnimationOptions) then
                  begin
                    // Scroll the child nodes down.
                    Mode1 := tamScrollDown;
                    R1.Bottom := R1.Top;
                    R1.Top := 0;
                    StepsR1 := Min(R1.Bottom - R1.Top + 1, Node.TotalHeight - NodeHeight[Node]);
                  end
                  else
                  begin
                    // The position cannot be kept. So scroll the node up to its future position.
                    Mode1 := tamScrollUp;
                    R1.Top := Max(0, R1.Top + HeightDelta);
                    R1.Bottom := ClientHeight;
                    StepsR1 := FOffsetY - HeightDelta;
                  end;
                end;
              end
              else
              begin
                if (FRangeY + FOffsetY - R1.Bottom + HeightDelta >= ClientHeight - R1.Bottom) or
                   (FRangeY <= ClientHeight) or (FOffsetY = 0) or not
                   (toAdvancedAnimatedToggle in FOptions.AnimationOptions) then
                begin
                  // Do a simple scroll up over the child nodes.
                  Mode1 := tamScrollUp;
                  Inc(R1.Top, NodeHeight[Node]);
                  R1.Bottom := ClientHeight;
                  StepsR1 := Min(R1.Bottom - R1.Top + 1, -HeightDelta);
                end
                else
                begin
                  // Scroll the node down to its future position. As FOffsetY will change we need to invalidate the
                  // whole tree.
                  Mode1 := tamScrollDown;
                  StepsR1 := Min(-FOffsetY, ClientHeight - FRangeY -FOffsetY - HeightDelta);
                  R1.Top := 0;
                  R1.Bottom := Min(ClientHeight, R1.Bottom + Steps);
                  NeedFullInvalidate := True;
                end;
              end;

              // No animation necessary if the node is below the current client height.
              if R1.Top < ClientHeight then
              begin
                PrepareAnimation;
                try
                  Animate(Steps, FAnimationDuration, ToggleCallback, @ToggleData);
                finally
                  DC.Free;
                end;
              end;
            end;
          end;

          // collapse the node
          AdjustTotalHeight(Node, IfThen(IsEffectivelyFiltered[Node], 0, NodeHeight[Node]));
          if FullyVisible[Node] then
            System.Dec(FVisibleCount, CountVisibleChildren(Node));
          Exclude(Node.States, vsExpanded);
          DoCollapsed(Node);

          // Remove child nodes now, if enabled.
          if (toAutoFreeOnCollapse in FOptions.AutoOptions) and (Node.ChildCount > 0) then
          begin
            DeleteChildren(Node);
            Include(Node.States, vsHasChildren);
          end;
        end;
      end
      else
        if DoExpanding(Node) then
        begin
          NeedUpdate := True;
          // expand the node, need to adjust the height
          if not (vsInitialized in Node.States) then
            InitNode(Node);
          if (vsHasChildren in Node.States) and (Node.ChildCount = 0) then
            InitChildren(Node);

          // Avoid setting the vsExpanded style if there are no child nodes.
          if Node.ChildCount > 0 then
          begin
            // Iterate through the child nodes without initializing them. We have to determine the entire height.
            Child := Node.FirstChild;
            repeat
              if vsVisible in Child.States then
              begin
                // Ensure the item height is measured
                MeasureItemHeight(Canvas, Child);

                Inc(HeightDelta, Child.TotalHeight);
              end;
              Child := Child.NextSibling;
            until Child = nil;

            // Getting the display rectangle is already done here as it is needed for toChildrenAbove in any case.
            if (toChildrenAbove in FOptions.PaintOptions) or (FUpdateCount = 0) then
            begin
              with ToggleData do
              begin
                R1 := GetDisplayRect(Node, NoColumn, False);
                Mode2 := tamNoScroll;
                TotalFit := HeightDelta + NodeHeight[Node] <= ClientHeight;

                if toChildrenAbove in FOptions.PaintOptions then
                begin
                  // The main goal with toChildrenAbove being set is to keep the nodes visual position so the user does
                  // not get confused. Therefore we need to scroll the view when the expanding is done.
                  PosHoldable := TotalFit and (FRangeY - ClientHeight >= 0) ;
                  ChildrenInView := (R1.Top - HeightDelta) >= 0;
                  NodeInView := R1.Bottom <= ClientHeight;
                end
                else
                begin
                  PosHoldable := TotalFit;
                  ChildrenInView := R1.Bottom + HeightDelta <= ClientHeight;
                end;

                R1.Bottom := ClientHeight;
              end;
            end;

            if FUpdateCount = 0 then
            begin
              // Do animated expanding if enabled.
              if (ToggleData.R1.Top < ClientHeight) and ([tsPainting, tsExpanding] * FStates = []) and
                (toAnimatedToggle in FOptions.AnimationOptions)then
              begin
                if tsHint in Self.FStates then
                  Application.CancelHint;
                UpdateWindow();

                // animated expanding
                with ToggleData do
                begin
                  if toChildrenAbove in FOptions.PaintOptions then
                  begin
                    // At first check if we hold the position, which is the most common case.
                    if not (toAdvancedAnimatedToggle in FOptions.AnimationOptions) or
                       (PosHoldable and ( (NodeInView and ChildrenInView) or not
                                          (toAutoScrollOnExpand in FOptions.AutoOptions) )) then
                    begin
                      Mode1 := tamScrollUp;
                      R1 := Rect(R1.Left, 0, R1.Right, R1.Top);
                      StepsR1 := Min(HeightDelta, R1.Bottom);
                    end
                    else
                    begin
                      // If we will not hold the node's visual position we mostly scroll in both directions.
                      Mode1 := tamScrollDown;
                      Mode2 := tamScrollUp;
                      R2 := Rect(R1.Left, 0, R1.Right, R1.Top);
                      if not (toAutoScrollOnExpand in FOptions.AutoOptions) then
                      begin
                        // If we shall not or cannot scroll to the desired extent we calculate the new position (with
                        // max FOffsetY applied) and animate it that way.
                        StepsR1 := -FOffsetY - Max(FRangeY + HeightDelta - ClientHeight, 0) + HeightDelta;
                        if (FRangeY + HeightDelta - ClientHeight) <= 0 then
                          Mode2 := tamNoScroll
                        else
                          StepsR2 := Min(FRangeY + HeightDelta - ClientHeight, R2.Bottom);
                      end
                      else
                      begin
                        if TotalFit and NodeInView and (FRangeY + HeightDelta > ClientHeight) then
                        begin
                          // If the whole subtree will fit into the client area and the node is currently fully visible,
                          // the first child will be made the top node if possible.
                          if HeightDelta >= R1.Top then
                            StepsR1 := Abs(R1.Top - HeightDelta)
                          else
                            StepsR1 := ClientHeight - FRangeY;
                        end
                        else
                          if FRangeY + HeightDelta <= ClientHeight then
                          begin
                            // We cannot make the first child the top node as we cannot scroll to that extent,
                            // so we do a simple scroll down.
                            Mode2 := tamNoScroll;
                            StepsR1 := HeightDelta;
                          end
                          else
                            // If the subtree does not fit into the client area at once, the expanded node will
                            // be made the bottom node.
                            StepsR1 := ClientHeight - R1.Top - NodeHeight[Node];

                        if Mode2 <> tamNoScroll then
                        begin
                          if StepsR1 > 0 then
                            StepsR2 := Min(R1.Top, HeightDelta - StepsR1)
                          else
                          begin
                            // If the node is already at the bottom scrolling is needed.
                            Mode1 := tamNoScroll;
                            StepsR2 := Min(HeightDelta, R1.Bottom);
                          end;
                        end;
                      end;
                    end;
                  end
                  else
                  begin
                    // toChildrenAbove is not set.
                    if (PosHoldable and ChildrenInView) or not (toAutoScrollOnExpand in FOptions.AutoOptions) or not
                       (toAdvancedAnimatedToggle in FOptions.AnimationOptions) or (R1.Top <= 0) then
                    begin
                      // If the node will stay at its visual position, do a simple down-scroll.
                      Mode1 := tamScrollDown;
                      Inc(R1.Top, NodeHeight[Node]);
                      StepsR1 := Min(R1.Bottom - R1.Top, HeightDelta);
                    end
                    else
                    begin
                      // We will not hold the nodes visual position so perform a double scroll.
                      Mode1 := tamScrollUp;
                      Mode2 := tamScrollDown;

                      R1.Bottom := R1.Top + NodeHeight[Node] + 1;
                      R1.Top := 0;
                      R2 := Rect(R1.Left, R1.Bottom, R1.Right, ClientHeight);

                      StepsR1 := Min(HeightDelta - (ClientHeight - R2.Top), R1.Bottom - NodeHeight[Node]);
                      StepsR2 := ClientHeight - R2.Top;
                    end;
                  end;

                  if ClientHeight >= R1.Top then
                  begin
                    PrepareAnimation;
                    try
                      Animate(Steps, FAnimationDuration, ToggleCallback, @ToggleData);
                    finally
                      DC.Free;
                    end;
                  end;
                end;
              end;
              if toAutoSort in FOptions.AutoOptions then
                Sort(Node, FHeader.SortColumn, FHeader.SortDirection, False);
            end;// if UpdateCount = 0

            Include(Node.States, vsExpanded);
            AdjustTotalHeight(Node, HeightDelta, True);
            if FullyVisible[Node] then
              System.Inc(FVisibleCount, CountVisibleChildren(Node));

            DoExpanded(Node);
          end;
        end;

      if NeedUpdate then
      begin
        InvalidateCache;
        if FUpdateCount = 0 then
        begin
          ValidateCache;
          if Node.ChildCount > 0 then
          begin
            UpdateRanges;
            UpdateScrollBars(True);
            if [tsPainting, tsExpanding] * FStates = [] then
            begin
              if (vsExpanded in Node.States) and ((toAutoScrollOnExpand in FOptions.AutoOptions) or
                 (toChildrenAbove in FOptions.PaintOptions)) then
              begin
                if toChildrenAbove in FOptions.PaintOptions then
                begin
                  NeedFullInvalidate := True;
                  if (PosHoldable and ChildrenInView and NodeInView) or not
                     (toAutoScrollOnExpand in FOptions.AutoOptions) then
                    SetOffsetY(FOffsetY - HeightDelta)
                  else
                    if TotalFit and NodeInView then
                    begin
                      FirstVisible := GetFirstVisible(Node, True);
                      if Assigned(FirstVisible) then // otherwise there is no visible child at all
                        SetOffsetY(FOffsetY - GetDisplayRect(FirstVisible, NoColumn, False).Top);
                    end
                    else
                      BottomNode := Node;
                end
                else
                begin
                  // Scroll as much child nodes into view as possible if the node has been expanded.
                  if PosHoldable then
                    NeedFullInvalidate := ScrollIntoView(GetLastVisible(Node, True), False)
                  else
                  begin
                    TopNode := Node;
                    NeedFullInvalidate := True;
                  end;
                end;
              end
              else
              begin
                // If we have collapsed the node or toAutoScrollOnExpand is not set, we try to keep the nodes
                // visual position.
                if toChildrenAbove in FOptions.PaintOptions then
                  SetOffsetY(FOffsetY - HeightDelta);
                NeedFullInvalidate := True;
              end;
            end;

            //UpdateScrollBars(True); Moved up

            // Check for automatically scrolled tree.
            if NeedFullInvalidate then
              Invalidate
            else
              InvalidateToBottom(Node);
          end
          else
            InvalidateNode(Node);
        end
        else
        begin
          UpdateRanges;
          UpdateScrollBars(True);
        end;
      end;

    finally
      Exclude(Node.States, vsToggling);
      if not TogglingTree then
        DoStateChange([], [tsToggling]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateHorizontalRange;

begin
  if FHeader.UseColumns then
    SetRangeX(FHeader.Columns.TotalWidth)
  else
    SetRangeX(GetMaxRightExtend);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateHorizontalScrollBar(DoRepaint: Boolean);

var
  ScrollInfo: TScrollInfo;

begin
  UpdateHorizontalRange;

  if IsUpdating or not HandleAllocated then
    Exit;

  // Adjust effect scroll offset depending on bidi mode.
  if UseRightToLeftAlignment then
    FEffectiveOffsetX := FRangeX - ClientWidth + FOffsetX
  else
    FEffectiveOffsetX := -FOffsetX;

  if FScrollBarOptions.ScrollBars in [System.UITypes.TScrollStyle.ssHorizontal, System.UITypes.TScrollStyle.ssBoth] then
  begin
    ZeroMemory (@ScrollInfo, SizeOf(ScrollInfo));
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    GetScrollInfo(SB_HORZ, ScrollInfo);

    if (FRangeX > ClientWidth) or FScrollBarOptions.AlwaysVisible then
    begin
      DoShowScrollBar(SB_HORZ, True);

      ScrollInfo.nMin := 0;
      ScrollInfo.nMax := FRangeX;
      ScrollInfo.nPos := FEffectiveOffsetX;
      ScrollInfo.nPage := Max(0, ClientWidth + 1);

      ScrollInfo.fMask := SIF_ALL or ScrollMasks[FScrollBarOptions.AlwaysVisible];
      SetScrollInfo(SB_HORZ, ScrollInfo, DoRepaint); // 1 app freeze seen here in TreeSize 8.1.0 after ScaleForPpi()
      if DoRepaint then
        RedrawWindow(nil, 0, RDW_FRAME or RDW_INVALIDATE); // Fixes issue #698
    end
    else
    begin
      ScrollInfo.nMin := 0;
      ScrollInfo.nMax := 0;
      ScrollInfo.nPos := 0;
      ScrollInfo.nPage := 0;
      DoShowScrollBar(SB_HORZ, False);
      SetScrollInfo(SB_HORZ, ScrollInfo, False);
    end;

    // Since the position is automatically changed if it doesn't meet the range
    // we better read the current position back to stay synchronized.
    FEffectiveOffsetX := GetScrollPos(SB_HORZ);
    if UseRightToLeftAlignment then
      SetOffsetX(-FRangeX + ClientWidth + FEffectiveOffsetX)
    else
      SetOffsetX(-FEffectiveOffsetX);
  end
  else
  begin
    DoShowScrollBar(SB_HORZ, False);

    // Reset the current horizontal offset to account for window resize etc.
    SetOffsetX(FOffsetX);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateRanges;

begin
  UpdateVerticalRange;
  UpdateHorizontalRange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateScrollBars(DoRepaint: Boolean);

// adjusts scrollbars to reflect current size and paint offset of the tree

begin
  if HandleAllocated then
  begin
    UpdateVerticalScrollBar(DoRepaint);
    UpdateHorizontalScrollBar(DoRepaint);
    Perform(CM_UPDATE_VCLSTYLE_SCROLLBARS,0,0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateStyleElements;
begin
  inherited;
  UpdateHeaderRect;
  FHeader.Columns.PaintHeader(Canvas, FHeaderRect, Point(0,0));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.UpdateVerticalRange;

begin
  // Total node height includes the height of the invisible root node.
  FRangeY := FRoot.TotalHeight - FRoot.NodeHeight + FBottomSpace;
end;

//----------------------------------------------------------------------------------------------------------------------

  procedure TBaseVirtualTree.UpdateVerticalScrollBar(DoRepaint: Boolean);

var
  ScrollInfo: TScrollInfo;

begin
  UpdateVerticalRange;

  if (IsUpdating) then
    Exit;
  Assert(GetCurrentThreadId = MainThreadId, 'UI controls like ' + Classname + ' and its scrollbars should only be manipulated through the main thread.');

  if FScrollBarOptions.ScrollBars in [TScrollStyle.ssVertical, TScrollStyle.ssBoth] then
  begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    GetScrollInfo(SB_VERT, ScrollInfo);

    if (FRangeY > ClientHeight) or FScrollBarOptions.AlwaysVisible then
    begin
      DoShowScrollBar(SB_VERT, True);

      ScrollInfo.nMin := 0;
      ScrollInfo.nMax := IfThen(FRangeY < MaxInt, FRangeY, MaxInt); // TScrollInfo values are signed 32bit only
      ScrollInfo.nPos := -FOffsetY;
      ScrollInfo.nPage := Max(0, ClientHeight + 1);

      ScrollInfo.fMask := SIF_ALL or ScrollMasks[FScrollBarOptions.AlwaysVisible];
      SetScrollInfo(SB_VERT, ScrollInfo, DoRepaint);
    end
    else
    begin
      ScrollInfo.nMin := 0;
      ScrollInfo.nMax := 0;
      ScrollInfo.nPos := 0;
      ScrollInfo.nPage := 0;
      DoShowScrollBar(SB_VERT, False);
      SetScrollInfo(SB_VERT, ScrollInfo, False);
    end;

    // Since the position is automatically changed if it doesn't meet the range
    // we better read the current position back to stay synchronized.
    SetOffsetY(-GetScrollPos(SB_VERT));
  end
  else
  begin
    DoShowScrollBar(SB_VERT, False);

    // Reset the current vertical offset to account for window resize etc.
    SetOffsetY(FOffsetY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseVirtualTree.UseRightToLeftReading: Boolean;

// The tree can handle right-to-left reading also on non-middle-east systems, so we cannot use the same function as
// it is implemented in TControl.

begin
  Result := BiDiMode <> bdLeftToRight;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ValidateChildren(Node: PVirtualNode; Recursive: Boolean);

// Ensures that the children of the given node (and all their children, if Recursive is True) are initialized.
// Node must already be initialized

var
  Child: PVirtualNode;

begin
  if Node = nil then
    Node := FRoot;

  if (vsHasChildren in Node.States) and (Node.ChildCount = 0) then
    InitChildren(Node);
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    ValidateNode(Child, Recursive);
    Child := Child.NextSibling;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseVirtualTree.ValidateNode(Node: PVirtualNode; Recursive: Boolean);

// Ensures that the given node (and all its children, if Recursive is True) are initialized.

var
  Child: PVirtualNode;

begin
  if Node = nil then
    Node := FRoot
  else
    if not (vsInitialized in Node.States) then
      InitNode(Node);

  if Recursive then
  begin
    if (vsHasChildren in Node.States) and (Node.ChildCount = 0) then
      InitChildren(Node);
    Child := Node.FirstChild;
    while Assigned(Child) do
    begin
      ValidateNode(Child, Recursive);
      Child := Child.NextSibling;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization

finalization
  FinalizeGlobalStructures();

end.
