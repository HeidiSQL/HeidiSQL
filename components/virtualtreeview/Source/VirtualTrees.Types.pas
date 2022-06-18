unit VirtualTrees.Types;

interface

uses
  WinApi.ActiveX,
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  Vcl.Controls,
  Vcl.GraphUtil,
  Vcl.Themes;

{$MINENUMSIZE 1, make enumerations as small as possible}

const
  VTTreeStreamVersion    = 3;
  VTHeaderStreamVersion  = 6;    // The header needs an own stream version to indicate changes only relevant to the header.

  CacheThreshold         = 2000; // Number of nodes a tree must at least have to start caching and at the same
                                // time the maximum number of nodes between two cache entries.
  FadeAnimationStepCount = 255;  // Number of animation steps for hint fading (0..255).
  ShadowSize             = 5;    // Size in pixels of the hint shadow. This value has no influence on Win2K and XP systems
                                // as those OSes have native shadow support.
  cDefaultTextMargin = 4;       // The default margin of text

  // Special identifiers for columns.
  NoColumn                 = - 1;
  InvalidColumn            = - 2;

  // Indices for check state images used for checking.
  ckEmpty                  = 0; // an empty image used as place holder
  // radio buttons
  ckRadioUncheckedNormal   = 1;
  ckRadioUncheckedHot      = 2;
  ckRadioUncheckedPressed  = 3;
  ckRadioUncheckedDisabled = 4;
  ckRadioCheckedNormal     = 5;
  ckRadioCheckedHot        = 6;
  ckRadioCheckedPressed    = 7;
  ckRadioCheckedDisabled   = 8;
  // check boxes
  ckCheckUncheckedNormal   = 9;
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
  ckButtonNormal           = 21;
  ckButtonHot              = 22;
  ckButtonPressed          = 23;
  ckButtonDisabled         = 24;

  // Instead using a TTimer class for each of the various events I use Windows timers with messages
  // as this is more economical.
  ExpandTimer            = 1;
  EditTimer              = 2;
  HeaderTimer            = 3;
  ScrollTimer            = 4;
  ChangeTimer            = 5;
  StructureChangeTimer   = 6;
  SearchTimer            = 7;
  ThemeChangedTimer      = 8;

  ThemeChangedTimerDelay = 500;

  // Virtual Treeview does not need to be subclassed by an eventual Theme Manager instance as it handles
  // Windows XP theme painting itself. Hence the special message is used to prevent subclassing.
  CM_DENYSUBCLASSING            = CM_BASE + 2000;

  // Decoupling message for auto-adjusting the internal edit window.
  CM_AUTOADJUST                 = CM_BASE + 2005;

  // Drag image helpers for Windows 2000 and up.
  IID_IDropTargetHelper : TGUID = (D1 : $4657278B; D2 : $411B; D3 : $11D2; D4 : ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDragSourceHelper : TGUID = (D1 : $DE5BF786; D2 : $477A; D3 : $11D2; D4 : ($83, $9D, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDropTarget : TGUID       = (D1 : $00000122; D2 : $0000; D3 : $0000; D4 : ($C0, $00, $00, $00, $00, $00, $00, $46));

  // VT's own clipboard formats,
  // Note: The reference format is used internally to allow to link to a tree reference
  //       to implement optimized moves and other back references.
  CFSTR_VIRTUALTREE      = 'Virtual Tree Data';
  CFSTR_VTREFERENCE      = 'Virtual Tree Reference';
  CFSTR_HTML             = 'HTML Format';
  CFSTR_RTF              = 'Rich Text Format';
  CFSTR_RTFNOOBJS        = 'Rich Text Format Without Objects';
  CFSTR_CSV              = 'CSV';

  // Help identifiers for exceptions. Application developers are responsible to link them with actual help topics.
  hcTFEditLinkIsNil      = 2000;
  hcTFWrongMoveError     = 2001;
  hcTFWrongStreamFormat  = 2002;
  hcTFWrongStreamVersion = 2003;
  hcTFStreamTooSmall     = 2004;
  hcTFCorruptStream1     = 2005;
  hcTFCorruptStream2     = 2006;
  hcTFClipboardFailed    = 2007;
  hcTFCannotSetUserData  = 2008;

  // Header standard split cursor.
  crHeaderSplit          = crHSplit deprecated 'Use vrHSplit instead';

  // Height changing cursor.
  crVertSplit            = crVSplit deprecated 'Use vrVSplit instead';


type
{$IFDEF VT_FMX}
  TDimension = Single;
{$ELSE}
  TDimension = Integer; // For Firemonkey support, see #841
{$ENDIF}
  TColumnIndex = type Integer;
  TColumnPosition = type Cardinal;
  PCardinal = ^Cardinal;

  // The exception used by the trees.
  EVirtualTreeError = class(Exception);

  // Limits the speed interval which can be used for auto scrolling (milliseconds).
  TAutoScrollInterval = 1 .. 1000;

  TVTScrollIncrement = 1 .. 10000;

  // OLE drag'n drop support
  TFormatEtcArray = array of TFormatEtc;
  TFormatArray = array of Word;

  TSmartAutoFitType = (smaAllColumns, //consider nodes in view only for all columns
    smaNoColumn,                      //consider nodes in view only for no column
    smaUseColumnOption                //use coSmartResize of the corresponding column
    );                                //describes the used column resize behaviour for AutoFitColumns


  TAddPopupItemType = (apNormal, apDisabled, apHidden);

  TCheckType = (
    ctNone,
    ctTriStateCheckBox,
    ctCheckBox,
    ctRadioButton,
    ctButton
    );

  // The check states include both, transient and fluent (temporary) states. The only temporary state defined so
  // far is the pressed state.
  TCheckState = (
    csUncheckedNormal,   // unchecked and not pressed
    csUncheckedPressed,  // unchecked and pressed
    csCheckedNormal,     // checked and not pressed
    csCheckedPressed,    // checked and pressed
    csMixedNormal,       // 3-state check box and not pressed
    csMixedPressed,      // 3-state check box and pressed
    csUncheckedDisabled, // disabled checkbox, not checkable
    csCheckedDisabled,   // disabled checkbox, not uncheckable
    csMixedDisabled      // disabled 3-state checkbox
    );

  /// Adds some convenience methods to type TCheckState
  TCheckStateHelper = record helper for TCheckState
  strict private
  const
    // Lookup to quickly convert a specific check state into its pressed counterpart and vice versa.
    cPressedState : array [TCheckState] of TCheckState   = (
      csUncheckedPressed, csUncheckedPressed, csCheckedPressed, csCheckedPressed, csMixedPressed, csMixedPressed, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
    cUnpressedState : array [TCheckState] of TCheckState = (
      csUncheckedNormal, csUncheckedNormal, csCheckedNormal, csCheckedNormal, csMixedNormal, csMixedNormal, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
    cEnabledState : array [TCheckState] of TCheckState   = (
      csUncheckedNormal, csUncheckedPressed, csCheckedNormal, csCheckedPressed, csMixedNormal, csMixedPressed, csUncheckedNormal, csCheckedNormal, csMixedNormal);
    cToggledState : array [TCheckState] of TCheckState   = (
      csCheckedNormal, csCheckedPressed, csUncheckedNormal, csUncheckedPressed, csCheckedNormal, csCheckedPressed, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
  public
    function GetPressed() : TCheckState; inline;
    function GetUnpressed() : TCheckState; inline;
    function GetEnabled() : TCheckState; inline;
    function GetToggled() : TCheckState; inline;
    function IsDisabled() : Boolean; inline;
    function IsChecked() : Boolean; inline;
    function IsUnChecked() : Boolean; inline;
    function IsMixed() : Boolean; inline;
  end;

type
  // Options per column.
  TVTColumnOption = (
    coAllowClick,            // Column can be clicked (must be enabled too).
    coDraggable,             // Column can be dragged.
    coEnabled,               // Column is enabled.
    coParentBidiMode,        // Column uses the parent's bidi mode.
    coParentColor,           // Column uses the parent's background color.
    coResizable,             // Column can be resized.
    coShowDropMark,          // Column shows the drop mark if it is currently the drop target.
    coVisible,               // Column is shown.
    coAutoSpring,            // Column takes part in the auto spring feature of the header (must be resizable too).
    coFixed,                 // Column is fixed and can not be selected or scrolled etc.
    coSmartResize,           // Column is resized to its largest entry which is in view (instead of its largest
                             // visible entry).
    coAllowFocus,            // Column can be focused.
    coDisableAnimatedResize, // Column resizing is not animated.
    coWrapCaption,           // Caption could be wrapped across several header lines to fit columns width.
    coUseCaptionAlignment,   // Column's caption has its own aligment.
    coEditable,              // Column can be edited
    coStyleColor             // Prefer background color of VCL style over TVirtualTreeColumn.Color
    );
  TVTColumnOptions = set of TVTColumnOption;

  TVirtualTreeColumnStyle = (
    vsText,
    vsOwnerDraw
    );

  TVTHeaderColumnLayout = (
    blGlyphLeft,
    blGlyphRight,
    blGlyphTop,
    blGlyphBottom
    );

  TSortDirection = (
    sdAscending,
    sdDescending
    );

  TSortDirectionHelper = record helper for VirtualTrees.Types.TSortDirection
  strict private
  const
    cSortDirectionToInt : Array [TSortDirection] of Integer = (1, - 1);
  public
    /// Returns +1 for ascending and -1 for descending sort order.
    function ToInt() : Integer; inline;
  end;


// Used during owner draw of the header to indicate which drop mark for the column must be drawn.
  TVTDropMarkMode = (
    dmmNone,
    dmmLeft,
    dmmRight
    );

  // auto scroll directions
  TScrollDirections = set of TScrollDirection;
//    sdLeft,
//    sdUp,
//    sdRight,
//    sdDown
//  );



  // There is a heap of switchable behavior in the tree. Since published properties may never exceed 4 bytes,
  // which limits sets to at most 32 members, and because for better overview tree options are splitted
  // in various sub-options and are held in a commom options class.
  //
  // Options to customize tree appearance:
  TVTPaintOption = (
    toHideFocusRect,         // Avoid drawing the dotted rectangle around the currently focused node.
    toHideSelection,         // Selected nodes are drawn as unselected nodes if the tree is unfocused.
    toHotTrack,              // Track which node is under the mouse cursor.
    toPopupMode,             // Paint tree as would it always have the focus (useful for tree combo boxes etc.)
    toShowBackground,        // Use the background image if there's one.
    toShowButtons,           // Display collapse/expand buttons left to a node.
    toShowDropmark,          // Show the dropmark during drag'n drop operations.
    toShowHorzGridLines,     // Display horizontal lines to simulate a grid.
    toShowRoot,              // Show lines also at top level (does not show the hidden/internal root node).
    toShowTreeLines,         // Display tree lines to show hierarchy of nodes.
    toShowVertGridLines,     // Display vertical lines (depending on columns) to simulate a grid.
    toThemeAware,            // Draw UI elements (header, tree buttons etc.) according to the current theme if enabled (Windows XP+ only, application must be themed).
    toUseBlendedImages,      // Enable alpha blending for ghosted nodes or those which are being cut/copied.
    toGhostedIfUnfocused,    // Ghosted images are still shown as ghosted if unfocused (otherwise the become non-ghosted images).
    toFullVertGridLines,     // Display vertical lines over the full client area, not only the space occupied by nodes.
                             // This option only has an effect if toShowVertGridLines is enabled too.
    toAlwaysHideSelection,   // Do not draw node selection, regardless of focused state.
    toUseBlendedSelection,   // Enable alpha blending for node selections.
    toStaticBackground,      // Show simple static background instead of a tiled one.
    toChildrenAbove,         // Display child nodes above their parent.
    toFixedIndent,           // Draw the tree with a fixed indent.
    toUseExplorerTheme,      // Use the explorer theme if run under Windows Vista (or above).
    toHideTreeLinesIfThemed, // Do not show tree lines if theming is used.
    toShowFilteredNodes      // Draw nodes even if they are filtered out.
    );
  TVTPaintOptions = set of TVTPaintOption;

  { Options to toggle animation support:
    **Do not use toAnimatedToggle when a background image is used for the tree.
   The animation does not look good as the image splits and moves with it.
  }
  TVTAnimationOption = (toAnimatedToggle, // Expanding and collapsing a node is animated (quick window scroll).
    // **See note above.
    toAdvancedAnimatedToggle              // Do some advanced animation effects when toggling a node.
    );
  TVTAnimationOptions = set of TVTAnimationOption;

  // Options which toggle automatic handling of certain situations:
  TVTAutoOption = (toAutoDropExpand, // Expand node if it is the drop target for more than a certain time.
    toAutoExpand,                    // Nodes are expanded (collapsed) when getting (losing) the focus.
    toAutoScroll,                    // Scroll if mouse is near the border while dragging or selecting.
    toAutoScrollOnExpand,            // Scroll as many child nodes in view as possible after expanding a node.
    toAutoSort,                      // Sort tree when Header.SortColumn or Header.SortDirection change or sort node if
                                     // child nodes are added. Sorting will take place also if SortColum is NoColumn (-1).

    toAutoSpanColumns,               // Large entries continue into next column(s) if there's no text in them (no clipping).
    toAutoTristateTracking,          // Checkstates are automatically propagated for tri state check boxes.
    toAutoHideButtons,               // Node buttons are hidden when there are child nodes, but all are invisible.
    toAutoDeleteMovedNodes,          // Delete nodes which where moved in a drag operation (if not directed otherwise).
    toDisableAutoscrollOnFocus,      // Disable scrolling a node or column into view if it gets focused.
    toAutoChangeScale,               // Change default node height automatically if the system's font scale is set to big fonts.
    toAutoFreeOnCollapse,            // Frees any child node after a node has been collapsed (HasChildren flag stays there).
    toDisableAutoscrollOnEdit,       // Do not center a node horizontally when it is edited.
    toAutoBidiColumnOrdering         // When set then columns (if any exist) will be reordered from lowest index to highest index
                                      // and vice versa when the tree's bidi mode is changed.
    );
  TVTAutoOptions = set of TVTAutoOption;

  // Options which determine the tree's behavior when selecting nodes:
  TVTSelectionOption = (toDisableDrawSelection, // Prevent user from selecting with the selection rectangle in multiselect mode.
    toExtendedFocus,                            // Entries other than in the main column can be selected, edited etc.
    toFullRowSelect,                            // Hit test as well as selection highlight are not constrained to the text of a node.
    toLevelSelectConstraint,                    // Constrain selection to the same level as the selection anchor.
    toMiddleClickSelect,                        // Allow selection, dragging etc. with the middle mouse button. This and toWheelPanning
                                                // are mutual exclusive.
    toMultiSelect,                              // Allow more than one node to be selected.
    toRightClickSelect,                         // Allow selection, dragging etc. with the right mouse button.
    toSiblingSelectConstraint,                  // Constrain selection to nodes with same parent.
    toCenterScrollIntoView,                     // Center nodes vertically in the client area when scrolling into view.
    toSimpleDrawSelection,                      // Simplifies draw selection, so a node's caption does not need to intersect with the
                                                // selection rectangle.
    toAlwaysSelectNode,                         // If this flag is set to true, the tree view tries to always have a node selected.
                                                // This behavior is closer to the Windows TreeView and useful in Windows Explorer style applications.
    toRestoreSelection,                         // Set to true if upon refill the previously selected nodes should be selected again.
                                                // The nodes will be identified by its caption (text in MainColumn)
                                                // You may use TVTHeader.RestoreSelectiuonColumnIndex to define an other column that should be used for indentification.
    toSyncCheckboxesWithSelection               // If checkboxes are shown, they follow the change in selections. When checkboxes are
                                                // changed, the selections follow them and vice-versa.
                                                // **Only supported for ctCheckBox type checkboxes.
    );
  TVTSelectionOptions = set of TVTSelectionOption;

  TVTEditOptions = (toDefaultEdit, // Standard behaviour for end of editing (after VK_RETURN stay on edited cell).
    toVerticalEdit,                // After VK_RETURN switch to next column.
    toHorizontalEdit               // After VK_RETURN switch to next row.
    );

  // Options which do not fit into any of the other groups:
  TVTMiscOption = (toAcceptOLEDrop, // Register tree as OLE accepting drop target
    toCheckSupport,                 // Show checkboxes/radio buttons.
    toEditable,                     // Node captions can be edited.
    toFullRepaintOnResize,          // Fully invalidate the tree when its window is resized (CS_HREDRAW/CS_VREDRAW).
    toGridExtensions,               // Use some special enhancements to simulate and support grid behavior.
    toInitOnSave,                   // Initialize nodes when saving a tree to a stream.
    toReportMode,                   // Tree behaves like TListView in report mode.
    toToggleOnDblClick,             // Toggle node expansion state when it is double clicked.
    toWheelPanning,                 // Support for mouse panning (wheel mice only). This option and toMiddleClickSelect are
                                    // mutal exclusive, where panning has precedence.
    toReadOnly,                     // The tree does not allow to be modified in any way. No action is executed and
                                    // node editing is not possible.
    toVariableNodeHeight,           // When set then GetNodeHeight will trigger OnMeasureItem to allow variable node heights.
    toFullRowDrag,                  // Start node dragging by clicking anywhere in it instead only on the caption or image.
                                    // Must be used together with toDisableDrawSelection.
    toNodeHeightResize,             // Allows changing a node's height via mouse.
    toNodeHeightDblClickResize,     // Allows to reset a node's height to FDefaultNodeHeight via a double click.
    toEditOnClick,                  // Editing mode can be entered with a single click
    toEditOnDblClick,               // Editing mode can be entered with a double click
    toReverseFullExpandHotKey       // Used to define Ctrl+'+' instead of Ctrl+Shift+'+' for full expand (and similar for collapsing)
    );
  TVTMiscOptions = set of TVTMiscOption;

  // Options to control data export
  TVTExportMode = (emAll,    // export all records (regardless checked state)
    emChecked,               // export checked records only
    emUnchecked,             // export unchecked records only
    emVisibleDueToExpansion, // Do not export nodes that are not visible because their parent is not expanded
    emSelected               // export selected nodes only
    );

  // Options regarding strings (useful only for the string tree and descendants):
  TVTStringOption = (toSaveCaptions, // If set then the caption is automatically saved with the tree node, regardless of what is
                                     // saved in the user data.
    toShowStaticText,                // Show static text in a caption which can be differently formatted than the caption
                                     // but cannot be edited.
    toAutoAcceptEditChange           // Automatically accept changes during edit if the user finishes editing other then
                                     // VK_RETURN or ESC. If not set then changes are cancelled.
    );
  TVTStringOptions = set of TVTStringOption;

const
  DefaultPaintOptions     = [toShowButtons, toShowDropmark, toShowTreeLines, toShowRoot, toThemeAware, toUseBlendedImages];
  DefaultAnimationOptions = [];
  DefaultAutoOptions      = [toAutoDropExpand, toAutoTristateTracking, toAutoScrollOnExpand, toAutoDeleteMovedNodes, toAutoChangeScale, toAutoSort, toAutoHideButtons];
  DefaultSelectionOptions = [];
  DefaultMiscOptions      = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick];

  DefaultStringOptions    = [toSaveCaptions, toAutoAcceptEditChange];

type
  TCustomVirtualTreeOptions = class(TPersistent)
  private
    FOwner            : TCustomControl;
    FPaintOptions     : TVTPaintOptions;
    FAnimationOptions : TVTAnimationOptions;
    FAutoOptions      : TVTAutoOptions;
    FSelectionOptions : TVTSelectionOptions;
    FMiscOptions      : TVTMiscOptions;
    FExportMode       : TVTExportMode;
    FEditOptions      : TVTEditOptions;
    procedure SetAnimationOptions(const Value : TVTAnimationOptions);
    procedure SetAutoOptions(const Value : TVTAutoOptions);
    procedure SetMiscOptions(const Value : TVTMiscOptions);
    procedure SetPaintOptions(const Value : TVTPaintOptions);
    procedure SetSelectionOptions(const Value : TVTSelectionOptions);
  protected
    // Mitigator function to use the correct style service for this context (either the style assigned to the control for Delphi > 10.4 or the application style)
    function StyleServices(AControl : TControl = nil) : TCustomStyleServices;
  public
    constructor Create(AOwner : TCustomControl); virtual;
    //these bypass the side effects in the regular setters.
    procedure InternalSetMiscOptions(const Value : TVTMiscOptions);

    procedure AssignTo(Dest : TPersistent); override;
    property AnimationOptions : TVTAnimationOptions read FAnimationOptions write SetAnimationOptions default DefaultAnimationOptions;
    property AutoOptions : TVTAutoOptions read FAutoOptions write SetAutoOptions default DefaultAutoOptions;
    property ExportMode : TVTExportMode read FExportMode write FExportMode default emAll;
    property MiscOptions : TVTMiscOptions read FMiscOptions write SetMiscOptions default DefaultMiscOptions;
    property PaintOptions : TVTPaintOptions read FPaintOptions write SetPaintOptions default DefaultPaintOptions;
    property SelectionOptions : TVTSelectionOptions read FSelectionOptions write SetSelectionOptions default DefaultSelectionOptions;
    property EditOptions : TVTEditOptions read FEditOptions write FEditOptions default toDefaultEdit;

    property Owner: TCustomControl read FOwner;
  end;

  TTreeOptionsClass = class of TCustomVirtualTreeOptions;

  TVirtualTreeOptions = class(TCustomVirtualTreeOptions)
  published
    property AnimationOptions;
    property AutoOptions;
    property ExportMode;
    property MiscOptions;
    property PaintOptions;
    property SelectionOptions;
  end;

  TCustomStringTreeOptions = class(TCustomVirtualTreeOptions)
  private
    FStringOptions : TVTStringOptions;
    procedure SetStringOptions(const Value : TVTStringOptions);
  protected
  public
    constructor Create(AOwner : TCustomControl); override;
    procedure AssignTo(Dest : TPersistent); override;
    property StringOptions : TVTStringOptions read FStringOptions write SetStringOptions default DefaultStringOptions;
  end;

  TStringTreeOptions = class(TCustomStringTreeOptions)
  published
    property AnimationOptions;
    property AutoOptions;
    property ExportMode;
    property MiscOptions;
    property PaintOptions;
    property SelectionOptions;
    property StringOptions;
    property EditOptions;
  end;

  TScrollBarStyle = (sbmRegular, sbm3D);

  // A class to manage scroll bar aspects.
  TScrollBarOptions = class(TPersistent)
  private
    FAlwaysVisible           : Boolean;
    FOwner                   : TCustomControl;
    FScrollBars              : TScrollStyle;       // used to hide or show vertical and/or horizontal scrollbar
    FScrollBarStyle          : TScrollBarStyle;    // kind of scrollbars to use
    FIncrementX, FIncrementY : TVTScrollIncrement; // number of pixels to scroll in one step (when auto scrolling)
    procedure SetAlwaysVisible(Value : Boolean);
    procedure SetScrollBars(Value : TScrollStyle);
    procedure SetScrollBarStyle(Value : TScrollBarStyle);
  protected
    function GetOwner : TPersistent; override;
  public
    constructor Create(AOwner : TCustomControl);

    procedure Assign(Source : TPersistent); override;
  published
    property AlwaysVisible       : Boolean read FAlwaysVisible write SetAlwaysVisible default False;
    property HorizontalIncrement : TVTScrollIncrement read FIncrementX write FIncrementX default 20;
    property ScrollBars          : TScrollStyle read FScrollBars write SetScrollBars default TScrollStyle.ssBoth;
    property ScrollBarStyle      : TScrollBarStyle read FScrollBarStyle write SetScrollBarStyle default sbmRegular;
    property VerticalIncrement   : TVTScrollIncrement read FIncrementY write FIncrementY default 20;
  end;

implementation

uses
  VirtualTrees,
  VirtualTrees.StyleHooks,
  WinApi.Windows;

type
  TVTCracker = class(TBaseVirtualTree);

  //----------------- TCustomVirtualTreeOptions --------------------------------------------------------------------------

constructor TCustomVirtualTreeOptions.Create(AOwner : TCustomControl);
begin
  FOwner := AOwner;

  FPaintOptions := DefaultPaintOptions;
  FAnimationOptions := DefaultAnimationOptions;
  FAutoOptions := DefaultAutoOptions;
  FSelectionOptions := DefaultSelectionOptions;
  FMiscOptions := DefaultMiscOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualTreeOptions.SetAnimationOptions(const Value : TVTAnimationOptions);
begin
  FAnimationOptions := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualTreeOptions.SetAutoOptions(const Value : TVTAutoOptions);
var
  ChangedOptions : TVTAutoOptions;
begin
  if FAutoOptions <> Value then
  begin
    // Exclusive ORing to get all entries wich are in either set but not in both.
    ChangedOptions := FAutoOptions + Value - (FAutoOptions * Value);
    FAutoOptions := Value;
    with FOwner do
      if (toAutoSpanColumns in ChangedOptions) and not (csLoading in ComponentState) and HandleAllocated then
        Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualTreeOptions.InternalSetMiscOptions(const Value : TVTMiscOptions);
begin
  FMiscOptions := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualTreeOptions.SetMiscOptions(const Value : TVTMiscOptions);
var
  ToBeSet, ToBeCleared : TVTMiscOptions;
begin
  if FMiscOptions <> Value then
  begin
    ToBeSet := Value - FMiscOptions;
    ToBeCleared := FMiscOptions - Value;
    FMiscOptions := Value;

    with TVTCracker(FOwner) do
      if not (csLoading in ComponentState) and HandleAllocated then
      begin
        if toCheckSupport in ToBeSet + ToBeCleared then
          Invalidate;
        if toEditOnDblClick in ToBeSet then
          FMiscOptions := FMiscOptions - [toToggleOnDblClick];
        // In order for toEditOnDblClick to take effect, we need to remove toToggleOnDblClick which is handled with priority. See issue #747

        if not (csDesigning in ComponentState) then
        begin
          if toAcceptOLEDrop in ToBeCleared then
            RevokeDragDrop(Handle);
          if toFullRepaintOnResize in ToBeSet + ToBeCleared then
            RecreateWnd;
          if toAcceptOLEDrop in ToBeSet then
            RegisterDragDrop(Handle, DragManager as IDropTarget);
          if toVariableNodeHeight in ToBeSet then
          begin
            BeginUpdate();
            try
              ReInitNode(nil, True);
            finally
              EndUpdate();
            end; //try..finally
          end;   //if toVariableNodeHeight
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualTreeOptions.SetPaintOptions(const Value : TVTPaintOptions);
var
  ToBeSet, ToBeCleared : TVTPaintOptions;
  Run                  : PVirtualNode;
  HandleWasAllocated   : Boolean;
begin
  if FPaintOptions <> Value then
  begin
    ToBeSet := Value - FPaintOptions;
    ToBeCleared := FPaintOptions - Value;
    FPaintOptions := Value;
    if (toFixedIndent in ToBeSet) then
    begin
      // Fixes issue #388
      Include(FPaintOptions, toShowRoot);
      Include(ToBeSet, toShowRoot);
    end; //if
    with TVTCracker(FOwner) do
    begin
      HandleWasAllocated := HandleAllocated;

      if not (csLoading in ComponentState) and (toShowFilteredNodes in ToBeSet + ToBeCleared) then
      begin
        if HandleWasAllocated then
          BeginUpdate;
        InterruptValidation;
        Run := GetFirstNoInit;
        while Assigned(Run) do
        begin
          if (vsFiltered in Run.States) then
          begin
            if FullyVisible[Run] then
            begin
              if toShowFilteredNodes in ToBeSet then
                IncVisibleCount
              else
                DecVisibleCount;
            end;
            if toShowFilteredNodes in ToBeSet then
              AdjustTotalHeight(Run, Run.NodeHeight, True)
            else
              AdjustTotalHeight(Run, - Run.NodeHeight, True);
          end;
          Run := GetNextNoInit(Run);
        end;
        if HandleWasAllocated then
          EndUpdate;
      end;

      if HandleAllocated then
      begin
        if IsWinVistaOrAbove and ((tsUseThemes in TreeStates) or ((toThemeAware in ToBeSet) and StyleServices.Enabled)) and (toUseExplorerTheme in (ToBeSet + ToBeCleared)) and
          not VclStyleEnabled then
        begin
          if (toUseExplorerTheme in ToBeSet) then
          begin
            SetWindowTheme('explorer');
            DoStateChange([tsUseExplorerTheme]);
          end
          else if toUseExplorerTheme in ToBeCleared then
          begin
            SetWindowTheme('');
            DoStateChange([], [tsUseExplorerTheme]);
          end;
        end;

        if not (csLoading in ComponentState) then
        begin
          if ((toThemeAware in ToBeSet + ToBeCleared) or (toUseExplorerTheme in ToBeSet + ToBeCleared) or VclStyleEnabled) then
          begin
            if ((toThemeAware in ToBeSet) and StyleServices.Enabled) then
              DoStateChange([tsUseThemes])
            else if (toThemeAware in ToBeCleared) then
              DoStateChange([], [tsUseThemes]);

            PrepareBitmaps(True, False);
            RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_VALIDATE or RDW_FRAME);
          end;

          if toChildrenAbove in ToBeSet + ToBeCleared then
          begin
            InvalidateCache;
            if UpdateCount = 0 then
            begin
              ValidateCache;
              Invalidate;
            end;
          end;

          Invalidate;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualTreeOptions.SetSelectionOptions(const Value : TVTSelectionOptions);
var
  ToBeSet, ToBeCleared : TVTSelectionOptions;
begin
  if FSelectionOptions <> Value then
  begin
    ToBeSet := Value - FSelectionOptions;
    ToBeCleared := FSelectionOptions - Value;
    FSelectionOptions := Value;

    with TVTCracker(FOwner) do
    begin
      if (toMultiSelect in (ToBeCleared + ToBeSet)) or ([toLevelSelectConstraint, toSiblingSelectConstraint] * ToBeSet <> []) then
        ClearSelection;

      if (toExtendedFocus in ToBeCleared) and (FocusedColumn > 0) and HandleAllocated then
      begin
        FocusedColumn := Header.MainColumn;
        Invalidate;
      end;

      if not (toExtendedFocus in FSelectionOptions) then
        FocusedColumn := Header.MainColumn;
    end;
  end;
end;

function TCustomVirtualTreeOptions.StyleServices(AControl : TControl) : TCustomStyleServices;
begin
  Result := VTStyleServices(FOwner);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualTreeOptions.AssignTo(Dest : TPersistent);
begin
  if Dest is TCustomVirtualTreeOptions then
  begin
    with Dest as TCustomVirtualTreeOptions do
    begin
      PaintOptions := Self.PaintOptions;
      AnimationOptions := Self.AnimationOptions;
      AutoOptions := Self.AutoOptions;
      SelectionOptions := Self.SelectionOptions;
      MiscOptions := Self.MiscOptions;
    end;
  end
  else
    inherited;
end;

//----------------- TCustomStringTreeOptions ---------------------------------------------------------------------------

constructor TCustomStringTreeOptions.Create(AOwner : TCustomControl);
begin
  inherited;
  FStringOptions := DefaultStringOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomStringTreeOptions.SetStringOptions(const Value : TVTStringOptions);
var
  ChangedOptions : TVTStringOptions;
begin
  if FStringOptions <> Value then
  begin
    // Exclusive ORing to get all entries wich are in either set but not in both.
    ChangedOptions := FStringOptions + Value - (FStringOptions * Value);
    FStringOptions := Value;
    with FOwner do
      if (toShowStaticText in ChangedOptions) and not (csLoading in ComponentState) and HandleAllocated then
        Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomStringTreeOptions.AssignTo(Dest : TPersistent);
begin
  if Dest is TCustomStringTreeOptions then
  begin
    with Dest as TCustomStringTreeOptions do
    begin
      StringOptions := Self.StringOptions;
      EditOptions := Self.EditOptions;
    end;
  end;

  // Let ancestors assign their options to the destination class.
  inherited;
end;

//----------------- TScrollBarOptions ----------------------------------------------------------------------------------

constructor TScrollBarOptions.Create(AOwner : TCustomControl);
begin
  inherited Create;

  FOwner := AOwner;
  FAlwaysVisible := False;
  FScrollBarStyle := sbmRegular;
  FScrollBars := TScrollStyle.ssBoth;
  FIncrementX := 20;
  FIncrementY := 20;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScrollBarOptions.SetAlwaysVisible(Value : Boolean);
begin
  if FAlwaysVisible <> Value then
  begin
    FAlwaysVisible := Value;
    if not (csLoading in FOwner.ComponentState) and FOwner.HandleAllocated then
      TVTCracker(FOwner).RecreateWnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScrollBarOptions.SetScrollBars(Value : TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    if not (csLoading in FOwner.ComponentState) and FOwner.HandleAllocated then
      TVTCracker(FOwner).RecreateWnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScrollBarOptions.SetScrollBarStyle(Value : TScrollBarStyle);

begin
  if FScrollBarStyle <> Value then
  begin
    FScrollBarStyle := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScrollBarOptions.GetOwner : TPersistent;

begin
  Result := FOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScrollBarOptions.Assign(Source : TPersistent);

begin
  if Source is TScrollBarOptions then
  begin
    AlwaysVisible := TScrollBarOptions(Source).AlwaysVisible;
    HorizontalIncrement := TScrollBarOptions(Source).HorizontalIncrement;
    ScrollBars := TScrollBarOptions(Source).ScrollBars;
    ScrollBarStyle := TScrollBarOptions(Source).ScrollBarStyle;
    VerticalIncrement := TScrollBarOptions(Source).VerticalIncrement;
  end
  else
    inherited;
end;



{ TCheckStateHelper }

function TCheckStateHelper.IsDisabled : Boolean;
begin
  Result := Self >= TCheckState.csUncheckedDisabled;
end;

function TCheckStateHelper.IsChecked : Boolean;
begin
  Result := Self in [csCheckedNormal, csCheckedPressed, csCheckedDisabled];
end;

function TCheckStateHelper.IsUnChecked : Boolean;
begin
  Result := Self in [csUncheckedNormal, csUncheckedPressed, csUncheckedDisabled];
end;

function TCheckStateHelper.IsMixed : Boolean;
begin
  Result := Self in [csMixedNormal, csMixedPressed, csMixedDisabled];
end;

function TCheckStateHelper.GetEnabled : TCheckState;
begin
  Result := cEnabledState[Self];
end;

function TCheckStateHelper.GetPressed() : TCheckState;
begin
  Result := cPressedState[Self];
end;

function TCheckStateHelper.GetUnpressed() : TCheckState;
begin
  Result := cUnpressedState[Self];
end;

function TCheckStateHelper.GetToggled() : TCheckState;
begin
  Result := cToggledState[Self];
end;

{ TSortDirectionHelper }

function TSortDirectionHelper.ToInt() : Integer;
begin
  Result := cSortDirectionToInt[Self];
end;


end.
