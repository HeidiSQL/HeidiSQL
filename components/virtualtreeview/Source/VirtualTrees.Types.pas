unit VirtualTrees.Types;

interface

uses
  WinApi.ActiveX,
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Vcl.GraphUtil,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.ImgList,
  System.UITypes; // some types moved from Vcl.* to System.UITypes

{$MINENUMSIZE 1, make enumerations as small as possible}

const
  VTTreeStreamVersion      = 3;
  VTHeaderStreamVersion    = 6;    // The header needs an own stream version to indicate changes only relevant to the header.

  CacheThreshold           = 2000; // Number of nodes a tree must at least have to start caching and at the same
                                   // time the maximum number of nodes between two cache entries.
  FadeAnimationStepCount   = 255;  // Number of animation steps for hint fading (0..255).
  ShadowSize               = 5;    // Size in pixels of the hint shadow. This value has no influence on Win2K and XP systems
                                   // as those OSes have native shadow support.
  cDefaultTextMargin       = 4;    // The default margin of text
  cInitialDefaultNodeHeight= 18;   // the default value of the DefualtNodeHeight property

  // Special identifiers for columns.
  NoColumn                 = - 1;
  InvalidColumn            = - 2;

  // Indices for check state images used for checking.
  ckEmpty                  = 0;    // an empty image used as place holder
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
  ExpandTimer              = 1;
  EditTimer                = 2;
  HeaderTimer              = 3;
  ScrollTimer              = 4;
  ChangeTimer              = 5;
  StructureChangeTimer     = 6;
  SearchTimer              = 7;
  ThemeChangedTimer        = 8;

  ThemeChangedTimerDelay   = 500;

  // Virtual Treeview does not need to be subclassed by an eventual Theme Manager instance as it handles
  // Windows XP theme painting itself. Hence the special message is used to prevent subclassing.
  CM_DENYSUBCLASSING       = CM_BASE + 2000;

  // Decoupling message for auto-adjusting the internal edit window.
  CM_AUTOADJUST            = CM_BASE + 2005;

  // Drag image helpers for Windows 2000 and up.
  IID_IDropTargetHelper : TGUID = (D1 : $4657278B; D2 : $411B; D3 : $11D2; D4 : ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDragSourceHelper : TGUID = (D1 : $DE5BF786; D2 : $477A; D3 : $11D2; D4 : ($83, $9D, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDropTarget : TGUID       = (D1 : $00000122; D2 : $0000; D3 : $0000; D4 : ($C0, $00, $00, $00, $00, $00, $00, $46));

  // VT's own clipboard formats,
  // Note: The reference format is used internally to allow to link to a tree reference
  //       to implement optimized moves and other back references.
  CFSTR_VIRTUALTREE        = 'Virtual Tree Data';
  CFSTR_VTREFERENCE        = 'Virtual Tree Reference';
  CFSTR_HTML               = 'HTML Format';
  CFSTR_RTF                = 'Rich Text Format';
  CFSTR_RTFNOOBJS          = 'Rich Text Format Without Objects';
  CFSTR_CSV                = 'CSV';

  // Help identifiers for exceptions. Application developers are responsible to link them with actual help topics.
  hcTFEditLinkIsNil        = 2000;
  hcTFWrongMoveError       = 2001;
  hcTFWrongStreamFormat    = 2002;
  hcTFWrongStreamVersion   = 2003;
  hcTFStreamTooSmall       = 2004;
  hcTFCorruptStream1       = 2005;
  hcTFCorruptStream2       = 2006;
  hcTFClipboardFailed      = 2007;
  hcTFCannotSetUserData    = 2008;

  // Header standard split cursor.
  crHeaderSplit            = crHSplit deprecated 'Use vrHSplit instead';

  // Height changing cursor.
  crVertSplit              = crVSplit deprecated 'Use vrVSplit instead';

  // chunk IDs
  NodeChunk = 1;
  BaseChunk = 2;        // chunk containing node state, check state, child node count etc.
                        // this chunk is immediately followed by all child nodes
  CaptionChunk = 3;     // used by the string tree to store a node's caption
  UserChunk = 4;        // used for data supplied by the application

type
{$IFDEF VT_FMX}
  TDimension = Single;
  PDimension = ^Single;
  TVTCursor = TCursor;
  TVTDragDataObject = TDragObject;
  TVTBackground = TBitmap;
  TVTPaintContext = TCanvas;
  TVTBrush = TBrush;
{$ELSE}
  TDimension = Integer; // For Firemonkey support, see #841
  PDimension = ^Integer;
  TVTCursor = HCURSOR;
  IDataObject= WinApi.ActiveX.IDataObject;
  TVTDragDataObject = IDataObject;
  TVTBackground = TPicture;
  TVTPaintContext = HDC;
  TVTBrush = HBRUSH;
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

  TSmartAutoFitType = (
    smaAllColumns,       // consider nodes in view only for all columns
    smaNoColumn,         // consider nodes in view only for no column
    smaUseColumnOption   // use coSmartResize of the corresponding column
  );  // describes the used column resize behaviour for AutoFitColumns

  TAddPopupItemType = (
    apNormal,
    apDisabled,
    apHidden
  );

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
  TVTAnimationOption = (
    toAnimatedToggle,                // Expanding and collapsing a node is animated (quick window scroll).
    // **See note above.
    toAdvancedAnimatedToggle         // Do some advanced animation effects when toggling a node.
    );
  TVTAnimationOptions = set of TVTAnimationOption;

  // Options which toggle automatic handling of certain situations:
  TVTAutoOption = (
    toAutoDropExpand,                // Expand node if it is the drop target for more than a certain time.
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
    toAutoChangeScale,               // Change default node height and header height automatically according to the height of the used font.
    toAutoFreeOnCollapse,            // Frees any child node after a node has been collapsed (HasChildren flag stays there).
    toDisableAutoscrollOnEdit,       // Do not center a node horizontally when it is edited.
    toAutoBidiColumnOrdering         // When set then columns (if any exist) will be reordered from lowest index to highest index
                                     // and vice versa when the tree's bidi mode is changed.
    );
  TVTAutoOptions = set of TVTAutoOption;

  // Options which determine the tree's behavior when selecting nodes:
  TVTSelectionOption = (
	toDisableDrawSelection,          // Prevent user from selecting with the selection rectangle in multiselect mode.
    toExtendedFocus,                 // Entries other than in the main column can be selected, edited etc.
    toFullRowSelect,                 // Hit test as well as selection highlight are not constrained to the text of a node.
    toLevelSelectConstraint,         // Constrain selection to the same level as the selection anchor.
    toMiddleClickSelect,             // Allow selection, dragging etc. with the middle mouse button. This and toWheelPanning
                                     // are mutual exclusive.
    toMultiSelect,                   // Allow more than one node to be selected.
    toRightClickSelect,              // Allow selection, dragging etc. with the right mouse button.
    toSiblingSelectConstraint,       // Constrain selection to nodes with same parent.
    toCenterScrollIntoView,          // Center nodes vertically in the client area when scrolling into view.
    toSimpleDrawSelection,           // Simplifies draw selection, so a node's caption does not need to intersect with the
                                     // selection rectangle.
    toAlwaysSelectNode,              // If this flag is set to true, the tree view tries to always have a node selected.
                                     // This behavior is closer to the Windows TreeView and useful in Windows Explorer style applications.
    toRestoreSelection,              // Set to true if upon refill the previously selected nodes should be selected again.
                                     // The nodes will be identified by its caption (text in MainColumn)
                                     // You may use TVTHeader.RestoreSelectiuonColumnIndex to define an other column that should be used for indentification.
    toSyncCheckboxesWithSelection,   // If checkboxes are shown, they follow the change in selections. When checkboxes are
                                     // changed, the selections follow them and vice-versa.
                                     // **Only supported for ctCheckBox type checkboxes.
    toSelectNextNodeOnRemoval        // If the selected node gets deleted, automatically select the next node.
    );
  TVTSelectionOptions = set of TVTSelectionOption;

  TVTEditOptions = (
    toDefaultEdit,                   // Standard behaviour for end of editing (after VK_RETURN stay on edited cell).
    toVerticalEdit,                  // After VK_RETURN switch to next column.
    toHorizontalEdit                 // After VK_RETURN switch to next row.
    );

  // Options which do not fit into any of the other groups:
  TVTMiscOption = (
    toAcceptOLEDrop,                 // Register tree as OLE accepting drop target
    toCheckSupport,                  // Show checkboxes/radio buttons.
    toEditable,                      // Node captions can be edited.
    toFullRepaintOnResize,           // Fully invalidate the tree when its window is resized (CS_HREDRAW/CS_VREDRAW).
    toGridExtensions,                // Use some special enhancements to simulate and support grid behavior.
    toInitOnSave,                    // Initialize nodes when saving a tree to a stream.
    toReportMode,                    // Tree behaves like TListView in report mode.
    toToggleOnDblClick,              // Toggle node expansion state when it is double clicked.
    toWheelPanning,                  // Support for mouse panning (wheel mice only). This option and toMiddleClickSelect are
                                     // mutal exclusive, where panning has precedence.
    toReadOnly,                      // The tree does not allow to be modified in any way. No action is executed and
                                     // node editing is not possible.
    toVariableNodeHeight,            // When set then GetNodeHeight will trigger OnMeasureItem to allow variable node heights.
    toFullRowDrag,                   // Start node dragging by clicking anywhere in it instead only on the caption or image.
                                     // Must be used together with toDisableDrawSelection.
    toNodeHeightResize,              // Allows changing a node's height via mouse.
    toNodeHeightDblClickResize,      // Allows to reset a node's height to FDefaultNodeHeight via a double click.
    toEditOnClick,                   // Editing mode can be entered with a single click
    toEditOnDblClick,                // Editing mode can be entered with a double click
    toReverseFullExpandHotKey        // Used to define Ctrl+'+' instead of Ctrl+Shift+'+' for full expand (and similar for collapsing)
    );
  TVTMiscOptions = set of TVTMiscOption;

  // Options to control data export
  TVTExportMode = (
    emAll,                           // export all records (regardless checked state)
    emChecked,                       // export checked records only
    emUnchecked,                     // export unchecked records only
    emVisibleDueToExpansion,         // Do not export nodes that are not visible because their parent is not expanded
    emSelected                       // export selected nodes only
    );

  // Describes the type of text to return in the text and draw info retrival events.
  TVSTTextType = (
    ttNormal,      // normal label of the node, this is also the text which can be edited
    ttStatic       // static (non-editable) text after the normal text
  );

  // Options regarding strings (useful only for the string tree and descendants):
  TVTStringOption = (
    toSaveCaptions,                  // If set then the caption is automatically saved with the tree node, regardless of what is
                                     // saved in the user data.
    toShowStaticText,                // Show static text in a caption which can be differently formatted than the caption
                                     // but cannot be edited.
    toAutoAcceptEditChange           // Automatically accept changes during edit if the user finishes editing other then
                                     // VK_RETURN or ESC. If not set then changes are cancelled.
    );
  TVTStringOptions = set of TVTStringOption;

  // Be careful when adding new states as this might change the size of the type which in turn
  // changes the alignment in the node record as well as the stream chunks.
  // Do not reorder the states and always add new states at the end of this enumeration in order to avoid
  // breaking existing code.
  TVirtualNodeState = (
    vsInitialized,       // Set after the node has been initialized.
    vsChecking,          // Node's check state is changing, avoid propagation.
    vsCutOrCopy,         // Node is selected as cut or copy and paste source.
    vsDisabled,          // Set if node is disabled.
    vsDeleting,          // Set when the node is about to be freed.
    vsExpanded,          // Set if the node is expanded.
    vsHasChildren,       // Indicates the presence of child nodes without actually setting them.
    vsVisible,           // Indicate whether the node is visible or not (independant of the expand states of its parents).
    vsSelected,          // Set if the node is in the current selection.
    vsOnFreeNodeCallRequired,   // Set if user data has been set which requires OnFreeNode.
    vsAllChildrenHidden, // Set if vsHasChildren is set and no child node has the vsVisible flag set.
    vsReleaseCallOnUserDataRequired, // Indicates that the user data is a reference to an interface which should be released.
    vsMultiline,         // Node text is wrapped at the cell boundaries instead of being shorted.
    vsHeightMeasured,    // Node height has been determined and does not need a recalculation.
    vsToggling,          // Set when a node is expanded/collapsed to prevent recursive calls.
    vsFiltered,          // Indicates that the node should not be painted (without effecting its children).
    vsInitializing       // Set when the node is being initialized
  );
  TVirtualNodeStates = set of TVirtualNodeState;

  // States used in InitNode to indicate states a node shall initially have.
  TVirtualNodeInitState = (
    ivsDisabled,
    ivsExpanded,
    ivsHasChildren,
    ivsMultiline,
    ivsSelected,
    ivsFiltered,
    ivsReInit
  );
  TVirtualNodeInitStates = set of TVirtualNodeInitState;

  // Various events must be handled at different places than they were initiated or need
  // a persistent storage until they are reset.
  TVirtualTreeStates = set of (
    tsChangePending,          // A selection change is pending.
    tsCheckPropagation,       // Set during automatic check state propagation.
    tsCollapsing,             // A full collapse operation is in progress.
    tsToggleFocusedSelection, // Node selection was modifed using Ctrl-click. Change selection state on next mouse up.
    tsClearPending,           // Need to clear the current selection on next mouse move.
    tsClearOnNewSelection,    // Need to clear the current selection before selecting a new node
    tsClipboardFlushing,      // Set during flushing the clipboard to avoid freeing the content.
    tsCopyPending,            // Indicates a pending copy operation which needs to be finished.
    tsCutPending,             // Indicates a pending cut operation which needs to be finished.
    tsDrawSelPending,         // Multiselection only. User held down the left mouse button on a free
                              // area and might want to start draw selection.
    tsDrawSelecting,          // Multiselection only. Draw selection has actually started.
    tsEditing,                // Indicates that an edit operation is currently in progress.
    tsEditPending,            // An mouse up start edit if dragging has not started.
    tsExpanding,              // A full expand operation is in progress.
    tsNodeHeightTracking,     // A node height changing operation is in progress.
    tsNodeHeightTrackPending, // left button is down, user might want to start changing a node's height.
    tsHint,                   // Set when our hint is visible or soon will be.
    tsInAnimation,            // Set if the tree is currently in an animation loop.
    tsIncrementalSearching,   // Set when the user starts incremental search.
    tsIncrementalSearchPending, // Set in WM_KEYDOWN to tell to use the char in WM_CHAR for incremental search.
    tsIterating,              // Set when IterateSubtree is currently in progress.
    tsLeftButtonDown,         // Set when the left mouse button is down.
    tsLeftDblClick,           // Set when the left mouse button was doubly clicked.
    tsMiddleButtonDown,       // Set when the middle mouse button is down.
    tsMiddleDblClick,         // Set when the middle mouse button was doubly clicked.
    tsNeedRootCountUpdate,    // Set if while loading a root node count is set.
    tsOLEDragging,            // OLE dragging in progress.
    tsOLEDragPending,         // User has requested to start delayed dragging.
    tsPainting,               // The tree is currently painting itself.
    tsRightButtonDown,        // Set when the right mouse button is down.
    tsRightDblClick,          // Set when the right mouse button was doubly clicked.
    tsPopupMenuShown,         // The user clicked the right mouse button, which might cause a popup menu to appear.
    tsScrolling,              // Set when autoscrolling is active.
    tsScrollPending,          // Set when waiting for the scroll delay time to elapse.
    tsSizing,                 // Set when the tree window is being resized. This is used to prevent recursive calls
                              // due to setting the scrollbars when sizing.
    tsStopValidation,         // Cache validation can be stopped (usually because a change has occured meanwhile).
    tsStructureChangePending, // The structure of the tree has been changed while the update was locked.
    tsSynchMode,              // Set when the tree is in synch mode, where no timer events are triggered.
    tsThumbTracking,          // Stop updating the horizontal scroll bar while dragging the vertical thumb and vice versa.
    tsToggling,               // A toggle operation (for some node) is in progress.
    tsUpdateHiddenChildrenNeeded, // Pending update for the hidden children flag after massive visibility changes.
    tsUseCache,               // The tree's node caches are validated and non-empty.
    tsUserDragObject,         // Signals that the application created an own drag object in OnStartDrag.
    tsUseThemes,              // The tree runs under WinXP+, is theme aware and themes are enabled.
    tsValidating,             // The tree's node caches are currently validated.
    tsPreviouslySelectedLocked,// The member FPreviouslySelected should not be changed
    tsValidationNeeded,       // Something in the structure of the tree has changed. The cache needs validation.
    tsVCLDragging,            // VCL drag'n drop in progress.
    tsVCLDragPending,         // One-shot flag to avoid clearing the current selection on implicit mouse up for VCL drag.
    tsVCLDragFinished,        // Flag to avoid triggering the OnColumnClick event twice
    tsWheelPanning,           // Wheel mouse panning is active or soon will be.
    tsWheelScrolling,         // Wheel mouse scrolling is active or soon will be.
    tsWindowCreating,         // Set during window handle creation to avoid frequent unnecessary updates.
    tsUseExplorerTheme        // The tree runs under WinVista+ and is using the explorer theme
  );


  TCheckImageKind = (
    ckCustom,         // application defined check images
    ckSystemDefault   // Uses the system check images, theme aware.
  );

  // mode to describe a move action
  TVTNodeAttachMode = (
    amNoWhere,        // just for simplified tests, means to ignore the Add/Insert command
    amInsertBefore,   // insert node just before destination (as sibling of destination)
    amInsertAfter,    // insert node just after destionation (as sibling of destination)
    amAddChildFirst,  // add node as first child of destination
    amAddChildLast    // add node as last child of destination
  );

  // modes to determine drop position further
  TDropMode = (
    dmNowhere,
    dmAbove,
    dmOnNode,
    dmBelow
  );

  // operations basically allowed during drag'n drop
  TDragOperation = (
    doCopy,
    doMove,
    doLink
  );
  TDragOperations = set of TDragOperation;

  TVTImageKind = (
    ikNormal,
    ikSelected,
    ikState,
    ikOverlay
  );

  {
    Fine points: Observed when fixing issue #623
    -- hmHint allows multiline hints automatically if provided through OnGetHint event.
       This is irresptive of whether node itself is multi-line or not.

    -- hmToolTip shows a hint only when node text is not fully shown. It's meant to
       fully show node text when not visible. It will show multi-line hint only if
       the node itself is multi-line. If you provide a custom multi-line hint then
       you must force linebreak style to hlbForceMultiLine in the OnGetHint event
       in order to show the complete hint.
  }
  TVTHintMode = (
    hmDefault,            // show the hint of the control
    hmHint,               // show node specific hint string returned by the application
    hmHintAndDefault,     // same as hmHint but show the control's hint if no node is concerned
    hmTooltip             // show the text of the node if it isn't already fully shown
  );

  // Indicates how to format a tooltip.
  TVTTooltipLineBreakStyle = (
    hlbDefault,           // Use multi-line style of the node.
    hlbForceSingleLine,   // Use single line hint.
    hlbForceMultiLine     // Use multi line hint.
  );

  TMouseButtons = set of TMouseButton;

  // Used to describe the action to do when using the OnBeforeItemErase event.
  TItemEraseAction = (
    eaColor,   // Use the provided color to erase the background instead the one of the tree.
    eaDefault, // The tree should erase the item's background (bitmap or solid).
    eaNone     // Do nothing. Let the application paint the background.
  );


  // Kinds of operations
  TVTOperationKind = (
    okAutoFitColumns,
    okGetMaxColumnWidth,
    okSortNode,
    okSortTree,
    okExport,
    okExpand
  );
  TVTOperationKinds = set of TVTOperationKind;

  // Indicates in the OnUpdating event what state the tree is currently in.
  TVTUpdateState = (
    usBegin,       // The tree just entered the update state (BeginUpdate call for the first time).
    usBeginSynch,  // The tree just entered the synch update state (BeginSynch call for the first time).
    usSynch,       // Begin/EndSynch has been called but the tree did not change the update state.
    usUpdate,      // Begin/EndUpdate has been called but the tree did not change the update state.
    usEnd,         // The tree just left the update state (EndUpdate called for the last level).
    usEndSynch     // The tree just left the synch update state (EndSynch called for the last level).
  );

  // These elements are used both to query the application, which of them it wants to draw itself and to tell it during
  // painting, which elements must be drawn during the advanced custom draw events.
  THeaderPaintElements = set of (
    hpeBackground,
    hpeDropMark,
    hpeHeaderGlyph,
    hpeSortGlyph,
    hpeText,
    // New in 7.0: Use this in FOnHeaderDrawQueryElements and OnAdvancedHeaderDraw
    // for additional custom header drawing while keeping the default drawing
    hpeOverlay
  );

  // determines whether and how the drag image is to show
  TVTDragImageKind = (
    diComplete,       // show a complete drag image with all columns, only visible columns are shown
    diMainColumnOnly, // show only the main column (the tree column)
    diNoImage         // don't show a drag image at all
  );

  // Switch for OLE and VCL drag'n drop. Because it is not possible to have both simultanously.
  TVTDragType = (
    dtOLE,
    dtVCL
  );

  // Determines the look of a tree's lines that show the hierarchy
  TVTLineStyle = (
    lsCustomStyle,           // application provides a line pattern
    lsDotted,                // usual dotted lines (default)
    lsSolid                  // simple solid lines
  );

  // TVTLineType is used during painting a tree for its tree lines that show the hierarchy
  TVTLineType = (
    ltNone,          // no line at all
    ltBottomRight,   // a line from bottom to the center and from there to the right
    ltTopDown,       // a line from top to bottom
    ltTopDownRight,  // a line from top to bottom and from center to the right
    ltRight,         // a line from center to the right
    ltTopRight,      // a line from bottom to center and from there to the right
    // special styles for alternative drawings of tree lines
    ltLeft,          // a line from top to bottom at the left
    ltLeftBottom     // a combination of ltLeft and a line at the bottom from left to right
  );

  // Determines how to draw tree lines.
  TVTLineMode = (
    lmNormal,        // usual tree lines (as in TTreeview)
    lmBands          // looks similar to a Nassi-Schneidermann diagram
  );

  // A collection of line type IDs which is used while painting a node.
  TLineImage = array of TVTLineType;


  // Export type
  TVTExportType = (
    etNone,   // No export, normal displaying on the screen
    etRTF,    // contentToRTF
    etHTML,   // contentToHTML
    etText,   // contentToText
    etExcel,  // supported by external tools
    etWord,   // supported by external tools
    etPDF,    // supported by external tools
    etPrinter,// supported by external tools
    etCSV,    // supported by external tools
    etCustom  // supported by external tools
  );

  // Options which are used when modifying the scroll offsets.
  TScrollUpdateOptions = set of (
    suoRepaintHeader,        // if suoUpdateNCArea is also set then invalidate the header
    suoRepaintScrollBars,    // if suoUpdateNCArea is also set then repaint both scrollbars after updating them
    suoScrollClientArea,     // scroll and invalidate the proper part of the client area
    suoUpdateNCArea          // update non-client area (scrollbars, header)
  );

  // Determines the look of a tree's buttons.
  TVTButtonStyle = (
    bsRectangle,             // traditional Windows look (plus/minus buttons)
    bsTriangle               // traditional Macintosh look
  );

  // TButtonFillMode is only used when the button style is bsRectangle and determines how to fill the interior.
  TVTButtonFillMode = (
    fmTreeColor,             // solid color, uses the tree's background color
    fmWindowColor,           // solid color, uses clWindow
    fmShaded,                // color gradient, Windows XP style (legacy code, use toThemeAware on Windows XP instead)
    fmTransparent            // transparent color, use the item's background color
  );

  // Method called by the Animate routine for each animation step.
  TVTAnimationCallback = function(Step, StepSize: Integer; Data: Pointer): Boolean of object;

  TVTIncrementalSearch = (
    isAll,                   // search every node in tree, initialize if necessary
    isNone,                  // disable incremental search
    isInitializedOnly,       // search only initialized nodes, skip others
    isVisibleOnly            // search only visible nodes, initialize if necessary
  );

  // Determines which direction to use when advancing nodes during an incremental search.
  TVTSearchDirection = (
    sdForward,
    sdBackward
  );

  // Determines where to start incremental searching for each key press.
  TVTSearchStart = (
    ssAlwaysStartOver,       // always use the first/last node (depending on direction) to search from
    ssLastHit,               // use the last found node
    ssFocusedNode            // use the currently focused node
  );

  // Determines how to use the align member of a node.
  TVTNodeAlignment = (
    naFromBottom,            // the align member specifies amount of units (usually pixels) from top border of the node
    naFromTop,               // align is to be measured from bottom
    naProportional           // align is to be measure in percent of the entire node height and relative to top
  );

  // Determines how to draw the selection rectangle used for draw selection.
  TVTDrawSelectionMode = (
    smDottedRectangle,       // same as DrawFocusRect
    smBlendedRectangle       // alpha blending, uses special colors (see TVTColors)
  );

  // Determines for which purpose the cell paint event is called.
  TVTCellPaintMode = (
    cpmPaint,                // painting the cell
    cpmGetContentMargin      // getting cell content margin
  );

  // Determines which sides of the cell content margin should be considered.
  TVTCellContentMarginType = (
    ccmtAllSides,            // consider all sides
    ccmtTopLeftOnly,         // consider top margin and left margin only
    ccmtBottomRightOnly      // consider bottom margin and right margin only
  );

  TChangeReason = (
    crIgnore,       // used as placeholder
    crAccumulated,  // used for delayed changes
    crChildAdded,   // one or more child nodes have been added
    crChildDeleted, // one or more child nodes have been deleted
    crNodeAdded,    // a node has been added
    crNodeCopied,   // a node has been duplicated
    crNodeMoved     // a node has been moved to a new place
  ); // desribes what made a structure change event happen

  TChunkHeader = record
    ChunkType,
    ChunkSize: Integer;      // contains the size of the chunk excluding the header
  end;

const
  DefaultPaintOptions     = [toShowButtons, toShowDropmark, toShowTreeLines, toShowRoot, toThemeAware, toUseBlendedImages, toFullVertGridLines];
  DefaultAnimationOptions = [];
  DefaultAutoOptions      = [toAutoDropExpand, toAutoTristateTracking, toAutoScrollOnExpand, toAutoDeleteMovedNodes, toAutoChangeScale, toAutoSort, toAutoHideButtons];
  DefaultSelectionOptions = [toSelectNextNodeOnRemoval];
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

  TScrollBarStyle = (
    sbmRegular,
    sbm3D
  );

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

  PVirtualNode = ^TVirtualNode;

  TVirtualNode = packed record
  private
    fIndex: Cardinal;         // index of node with regard to its parent
    fChildCount: Cardinal;    // number of child nodes
    fNodeHeight: TDimension;  // height in pixels
  public
    States: TVirtualNodeStates; // states describing various properties of the node (expanded, initialized etc.)
    Align: Byte;             // line/button alignment
    CheckState: TCheckState; // indicates the current check state (e.g. checked, pressed etc.)
    CheckType: TCheckType;   // indicates which check type shall be used for this node
    Dummy: Byte;             // dummy value to fill DWORD boundary
    TotalCount: Cardinal;    // sum of this node, all of its child nodes and their child nodes etc.
    TotalHeight: TDimension; // height in pixels this node covers on screen including the height of all of its
                             // children
    _Filler: TDWordFiller;   // Ensure 8 Byte alignment of following pointers for 64bit builds. Issue #1136
    // Note: Some copy routines require that all pointers (as well as the data area) in a node are
    //       located at the end of the node! Hence if you want to add new member fields (except pointers to internal
    //       data) then put them before field Parent.
  private
    fParent:  PVirtualNode;     // reference to the node's parent (for the root this contains the treeview)
    fPrevSibling: PVirtualNode; // link to the node's previous sibling or nil if it is the first node
    fNextSibling: PVirtualNode; // link to the node's next sibling or nil if it is the last node
  public // still public as it is used as var parameter in MergeSortAscending()
    FirstChild: PVirtualNode;  // link to the node's first child...
  private
    fLastChild: PVirtualNode;   // link to the node's last child...
  public
    procedure SetParent(const pParent: PVirtualNode); inline; //internal method, do not call directly but use Parent[Node] := x on tree control.
    procedure SetPrevSibling(const pPrevSibling: PVirtualNode); inline; //internal method, do not call directly
    procedure SetNextSibling(const pNextSibling: PVirtualNode); inline; //internal method, do not call directly
    procedure SetFirstChild(const pFirstChild: PVirtualNode); inline; //internal method, do not call directly
    procedure SetLastChild(const pLastChild: PVirtualNode); inline; //internal method, do not call directly
    procedure SetIndex(const pIndex: Cardinal); inline;       //internal method, do not call directly.
    procedure SetChildCount(const pCount: Cardinal); inline; //internal method, do not call directly.
    procedure SetNodeHeight(const pNodeHeight: TDimension); inline; //internal method, do not call directly.
    property Index: Cardinal read fIndex;
    property ChildCount: Cardinal read fChildCount;
    property Parent: PVirtualNode read fParent;
    property PrevSibling: PVirtualNode read fPrevSibling;
    property NextSibling: PVirtualNode read fNextSibling;
    property LastChild: PVirtualNode read fLastChild;
    property NodeHeight: TDimension read fNodeHeight;
  private
    Data: record end;        // this is a placeholder, each node gets extra data determined by NodeDataSize
  public
    function IsAssigned(): Boolean; inline;
    function GetData(): Pointer; overload; inline;
    function GetData<T>(): T; overload; inline;
    procedure SetData(pUserData: Pointer); overload;
    procedure SetData<T>(pUserData: T); overload;
    procedure SetData(const pUserData: IInterface); overload;
  end;


  TVTHeaderColumnLayout = (
    blGlyphLeft,
    blGlyphRight,
    blGlyphTop,
    blGlyphBottom
    );

  // These flags are used to indicate where a click in the header happened.
  TVTHeaderHitPosition = (
    hhiNoWhere,         // No column is involved (possible only if the tree is smaller than the client area).
    hhiOnColumn,        // On a column.
    hhiOnIcon,          // On the bitmap associated with a column.
    hhiOnCheckbox       // On the checkbox if enabled.
  );
  TVTHeaderHitPositions = set of TVTHeaderHitPosition;

  // These flags are returned by the hit test method.
  THitPosition = (
    hiAbove,             // above the client area (if relative) or the absolute tree area
    hiBelow,             // below the client area (if relative) or the absolute tree area
    hiNowhere,           // no node is involved (possible only if the tree is not as tall as the client area)
    hiOnItem,            // on the bitmaps/buttons or label associated with an item
    hiOnItemButton,      // on the button associated with an item
    hiOnItemButtonExact, // exactly on the button associated with an item
    hiOnItemCheckbox,    // on the checkbox if enabled
    hiOnItemIndent,      // in the indentation area in front of a node
    hiOnItemLabel,       // on the normal text area associated with an item
    hiOnItemLeft,        // in the area to the left of a node's text area (e.g. when right aligned or centered)
    hiOnItemRight,       // in the area to the right of a node's text area (e.g. if left aligned or centered)
    hiOnNormalIcon,      // on the "normal" image
    hiOnStateIcon,       // on the state image
    hiToLeft,            // to the left of the client area (if relative) or the absolute tree area
    hiToRight,           // to the right of the client area (if relative) or the absolute tree area
    hiUpperSplitter,     // in the upper splitter area of a node
    hiLowerSplitter      // in the lower splitter area of a node
  );
  THitPositions = set of THitPosition;

  // Structure used when info about a certain position in the header is needed.
  TVTHeaderHitInfo = record
    X,
    Y: TDimension;
    Button: TMouseButton;
    Shift: TShiftState;
    Column: TColumnIndex;
    HitPosition: TVTHeaderHitPositions;
  end;

  // Structure used when info about a certain position in the tree is needed.
  THitInfo = record
    HitNode: PVirtualNode;
    HitPositions: THitPositions;
    HitColumn: TColumnIndex;
    HitPoint: TPoint;
    ShiftState: TShiftState;
  end;

  TVTHeaderStyle = (
    hsThickButtons,                 //TButton look and feel
    hsFlatButtons,                  //flatter look than hsThickButton, like an always raised flat TToolButton
    hsPlates                        //flat TToolButton look and feel (raise on hover etc.)
    );

  TVTHeaderOption = (
    hoAutoResize,                   //Adjust a column so that the header never exceeds the client width of the owner control.
    hoColumnResize,                 //Resizing columns with the mouse is allowed.
    hoDblClickResize,               //Allows a column to resize itself to its largest entry.
    hoDrag,                         //Dragging columns is allowed.
    hoHotTrack,                     //Header captions are highlighted when mouse is over a particular column.
    hoOwnerDraw,                    //Header items with the owner draw style can be drawn by the application via event.
    hoRestrictDrag,                 //Header can only be dragged horizontally.
    hoShowHint,                     //Show application defined header hint.
    hoShowImages,                   //Show header images.
    hoShowSortGlyphs,               //Allow visible sort glyphs.
    hoVisible,                      //Header is visible.
    hoAutoSpring,                   //Distribute size changes of the header to all columns, which are sizable and have the coAutoSpring option enabled.
    hoFullRepaintOnResize,          //Fully invalidate the header (instead of subsequent columns only) when a column is resized.
    hoDisableAnimatedResize,        //Disable animated resize for all columns.
    hoHeightResize,                 //Allow resizing header height via mouse.
    hoHeightDblClickResize,         //Allow the header to resize itself to its default height.
    hoHeaderClickAutoSort,          //Clicks on the header will make the clicked column the SortColumn or toggle sort direction if it already was the sort column
    hoAutoColumnPopupMenu,          //Show a context menu for activating and deactivating columns on right click
    hoAutoResizeInclCaption         //Includes the header caption for the auto resizing
    );
  TVTHeaderOptions = set of TVTHeaderOption;

  THeaderState = (
    hsAutoSizing,                   //auto size chain is in progess, do not trigger again on WM_SIZE
    hsDragging,                     //header dragging is in progress (only if enabled)
    hsDragPending,                  //left button is down, user might want to start dragging a column
    hsLoading,                      //The header currently loads from stream, so updates are not necessary.
    hsColumnWidthTracking,          //column resizing is in progress
    hsColumnWidthTrackPending,      //left button is down, user might want to start resize a column
    hsHeightTracking,               //height resizing is in progress
    hsHeightTrackPending,           //left button is down, user might want to start changing height
    hsResizing,                     //multi column resizing in progress
    hsScaling,                      //the header is scaled after a change of FixedAreaConstraints or client size
    hsNeedScaling                   //the header needs to be scaled
    );
  THeaderStates = set of THeaderState;

  // content elements of the control from left to right, used when calculatin left margins.
  TVTElement = (
    ofsMargin, // right of the margin
    ofsToggleButton, // the exact x-postition of the toggle button
    ofsCheckBox,
    ofsStateImage,
    ofsImage,
    ofsLabel, // where drawing a selection begins
    ofsText, // includes TextMargin
    ofsRightOfText, // Includes NodeWidth and ExtraNodeWidth
    ofsEndOfClientArea // The end of the paint area
  );

  /// An array that can be used to calculate the offsets ofthe elements in the tree.
  TVTOffsets = array [TVTElement] of TDimension;

  // For painting a node and its columns/cells a lot of information must be passed frequently around.
  TVTImageInfo = record
    Index: TImageIndex;       // Index in the associated image list.
    XPos,                     // Horizontal position in the current target canvas.
    YPos: TDimension;         // Vertical position in the current target canvas.
    Ghosted: Boolean;         // Flag to indicate that the image must be drawn slightly lighter.
    Images: TCustomImageList; // The image list to be used for painting.
    function Equals(const pImageInfo2: TVTImageInfo): Boolean;
  end;

  TVTImageInfoIndex = (
    iiNormal,
    iiState,
    iiCheck,
    iiOverlay
  );

  // options which determine what to draw in PaintTree
  TVTInternalPaintOption = (
    poBackground,       // draw background image if there is any and it is enabled
    poColumnColor,      // erase node's background with the column's color
    poDrawFocusRect,    // draw focus rectangle around the focused node
    poDrawSelection,    // draw selected nodes with the normal selection color
    poDrawDropMark,     // draw drop mark if a node is currently the drop target
    poGridLines,        // draw grid lines if enabled
    poMainOnly,         // draw only the main column
    poSelectedOnly,     // draw only selected nodes
    poUnbuffered        // draw directly onto the target canvas; especially useful when printing
  );
  TVTInternalPaintOptions = set of TVTInternalPaintOption;

  TVTPaintInfo = record
    Canvas: TCanvas;              // the canvas to paint on
    PaintOptions: TVTInternalPaintOptions;  // a copy of the paint options passed to PaintTree
    Node: PVirtualNode;           // the node to paint
    Column: TColumnIndex;         // the node's column index to paint
    Position: TColumnPosition;    // the column position of the node
    CellRect: TRect;              // the node cell
    ContentRect: TRect;           // the area of the cell used for the node's content
    NodeWidth: TDimension;        // the actual node width
    Alignment: TAlignment;        // how to align within the node rectangle
    CaptionAlignment: TAlignment; // how to align text within the caption rectangle
    BidiMode: TBidiMode;          // directionality to be used for painting
    BrushOrigin: TPoint;          // the alignment for the brush used to draw dotted lines
    ImageInfo: array[TVTImageInfoIndex] of TVTImageInfo; // info about each possible node image
    Offsets: TVTOffsets;          // The offsets of the various elements of a tree node
    VAlign: TDimension;
    procedure AdjustImageCoordinates();
  end;

  TNodeArray = array of PVirtualNode;

implementation

uses
  System.TypInfo,
  VirtualTrees.StyleHooks,
  VirtualTrees.BaseTree,
  VirtualTrees.BaseAncestorVcl{to eliminate H2443 about inline expanding}
  ;

type
  TVTCracker = class(TBaseVirtualTree);


{ TVirtualNode }

function TVirtualNode.GetData(): Pointer;

// Returns the associated data converted to the class given in the generic part of the function.

begin
  Result := @Self.Data;
  Include(States, vsOnFreeNodeCallRequired);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualNode.GetData<T>: T;

// Returns the associated data converted to the class given in the generic part of the function.

begin
  Result := T(Pointer((PByte(@(Self.Data))))^);
  Include(States, vsOnFreeNodeCallRequired);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualNode.IsAssigned: Boolean;

// Returns False if this node is nil, True otherwise

begin
  Exit(@Self <> nil);
end;

procedure TVirtualNode.SetNodeHeight(const pNodeHeight: TDimension);
begin
  fNodeHeight := pNodeHeight;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualNode.SetData(pUserData: Pointer);


  // Can be used to set user data of a PVirtualNode with the size of a pointer, useful for setting
  // A pointer to a record or a reference to a class instance.
var
  NodeData: PPointer;
begin
  NodeData := PPointer(@Self.Data);
  NodeData^ := pUserData;
  Include(Self.States, vsOnFreeNodeCallRequired);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualNode.SetChildCount(const pCount: Cardinal);
begin
  fChildCount := pCount;
end;

procedure TVirtualNode.SetData(const pUserData: IInterface);


  // Can be used to set user data of a PVirtualNode to a class instance,
  // will take care about reference counting.

begin
  pUserData._AddRef();
  SetData(Pointer(pUserData));
  Include(Self.States, vsReleaseCallOnUserDataRequired);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualNode.SetData<T>(pUserData: T);

begin
  T(Pointer((PByte(@(Self.Data))))^) := pUserData;
  if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
    Include(Self.States, vsReleaseCallOnUserDataRequired);
  Include(Self.States, vsOnFreeNodeCallRequired);
end;

procedure TVirtualNode.SetFirstChild(const pFirstChild: PVirtualNode);
begin
  FirstChild := pFirstChild;
end;

procedure TVirtualNode.SetLastChild(const pLastChild: PVirtualNode);
begin
  fLastChild := pLastChild;
end;

procedure TVirtualNode.SetIndex(const pIndex: Cardinal);
begin
  fIndex := pIndex;
end;

procedure TVirtualNode.SetParent(const pParent: PVirtualNode);
begin
  fParent := pParent;
end;

procedure TVirtualNode.SetPrevSibling(const pPrevSibling: PVirtualNode);
begin
  fPrevSibling := pPrevSibling;
end;

procedure TVirtualNode.SetNextSibling(const pNextSibling: PVirtualNode);
begin
  fNextSibling := pNextSibling;
end;

//----------------------------------------------------------------------------------------------------------------------


{ TVTImageInfo }

function TVTImageInfo.Equals(const pImageInfo2: TVTImageInfo): Boolean;

  // Returns true if both images are the same, does not regard Ghosted and position.

begin
  Result := (Self.Index = pImageInfo2.Index) and (Self.Images = pImageInfo2.Images);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TVTPaintInfo }

procedure TVTPaintInfo.AdjustImageCoordinates();
// During painting of the main column some coordinates must be adjusted due to the tree lines.
begin
  ContentRect := CellRect;
  if BidiMode = bdLeftToRight then
  begin
    ContentRect.Left := CellRect.Left + Offsets[TVTElement.ofsLabel];
    ImageInfo[iiNormal].XPos := CellRect.Left + Offsets[TVTElement.ofsImage];
    ImageInfo[iiState].XPos := CellRect.Left + Offsets[TVTElement.ofsStateImage];
    ImageInfo[iiCheck].XPos := CellRect.Left + Offsets[TVTElement.ofsCheckBox];
  end
  else
  begin
    /// Since images are still drawn from left to right, we need to substract the image sze as well.
    ImageInfo[iiNormal].XPos := CellRect.Right - Offsets[TVTElement.ofsImage] - (Offsets[TVTElement.ofsLabel] - Offsets[TVTElement.ofsImage]);
    ImageInfo[iiState].XPos := CellRect.Right - Offsets[TVTElement.ofsStateImage] - (Offsets[TVTElement.ofsImage] - Offsets[TVTElement.ofsStateImage]);
    ImageInfo[iiCheck].XPos := CellRect.Right - Offsets[TVTElement.ofsCheckBox] - (Offsets[TVTElement.ofsStateImage] - Offsets[TVTElement.ofsCheckBox]);
    ContentRect.Right := CellRect.Right - Offsets[TVTElement.ofsLabel];
  end;
  if ImageInfo[iiNormal].Index > -1 then
    ImageInfo[iiNormal].YPos := CellRect.Top + VAlign - ImageInfo[iiNormal].Images.Height div 2;
  if ImageInfo[iiState].Index > -1 then
    ImageInfo[iiState].YPos := CellRect.Top + VAlign - ImageInfo[iiState].Images.Height div 2;
  if ImageInfo[iiCheck].Index > -1 then
    ImageInfo[iiCheck].YPos := CellRect.Top + VAlign - ImageInfo[iiCheck].Images.Height div 2;
end;


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
        if ((tsUseThemes in TreeStates) or ((toThemeAware in ToBeSet) and StyleServices.Enabled)) and (toUseExplorerTheme in (ToBeSet + ToBeCleared)) and
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
            RedrawWindow(nil, 0, RDW_INVALIDATE or RDW_VALIDATE or RDW_FRAME);
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

//----------------------------------------------------------------------------------------------------------------------

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

//----------------------------------------------------------------------------------------------------------------------

{ TCheckStateHelper }

function TCheckStateHelper.IsDisabled: Boolean;
begin
  Result := Self >= TCheckState.csUncheckedDisabled;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCheckStateHelper.IsChecked: Boolean;
begin
  Result := Self in [csCheckedNormal, csCheckedPressed, csCheckedDisabled];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCheckStateHelper.IsUnChecked: Boolean;
begin
  Result := Self in [csUncheckedNormal, csUncheckedPressed, csUncheckedDisabled];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCheckStateHelper.IsMixed: Boolean;
begin
  Result := Self in [csMixedNormal, csMixedPressed, csMixedDisabled];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCheckStateHelper.GetEnabled: TCheckState;
begin
  Result := cEnabledState[Self];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCheckStateHelper.GetPressed(): TCheckState;
begin
  Result := cPressedState[Self];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCheckStateHelper.GetUnpressed(): TCheckState;
begin
  Result := cUnpressedState[Self];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCheckStateHelper.GetToggled(): TCheckState;
begin
  Result := cToggledState[Self];
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSortDirectionHelper }

function TSortDirectionHelper.ToInt() : Integer;
begin
  Result := cSortDirectionToInt[Self];
end;


end.
