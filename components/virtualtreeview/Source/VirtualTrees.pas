unit VirtualTrees;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is VirtualTrees.pas, released September 30, 2000.
//
// The initial developer of the original code is digital publishing AG (Munich, Germany, www.digitalpublishing.de),
// most code was written by Mike Lischke 2000-2009 (public@soft-gems.net, www.soft-gems.net)
//
// Portions created by digital publishing AG are Copyright
// (C) 1999-2001 digital publishing AG. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// For a list of recent changes please see file CHANGES.TXT
//
// Credits for their valuable assistance and code donations go to:
//   Freddy Ertl, Marian Aldenhoevel, Thomas Bogenrieder, Jim Kuenemann, Werner Lehmann, Jens Treichler,
//   Paul Gallagher (IBO tree), Ondrej Kelle, Ronaldo Melo Ferraz, Heri Bender, Roland Beduerftig (BCB)
//   Anthony Mills, Alexander Egorushkin (BCB), Mathias Torell (BCB), Frank van den Bergh, Vadim Sedulin, Peter Evans,
//   Milan Vandrovec (BCB), Steve Moss, Joe White, David Clark, Anders Thomsen, Igor Afanasyev, Eugene Programmer,
//   Corbin Dunn, Richard Pringle, Uli Gerhardt, Azza, Igor Savkic, Daniel Bauten, Timo Tegtmeier, Dmitry Zegebart,
//   Andreas Hausladen, Joachim Marder, Roman Kassebaum, Vincent Parrett, Dietmar Roesler, Sanjay Kanade,
//   and everyone that sent pull requests: https://github.com/Virtual-TreeView/Virtual-TreeView/pulls?q=
// Beta testers:
//   Freddy Ertl, Hans-Juergen Schnorrenberg, Werner Lehmann, Jim Kueneman, Vadim Sedulin, Moritz Franckenstein,
//   Wim van der Vegt, Franc v/d Westelaken
// Indirect contribution (via publicly accessible work of those persons):
//   Alex Denissov, Hiroyuki Hori (MMXAsm expert)
// Documentation:
//   Markus Spoettl and toolsfactory GbR (http://www.doc-o-matic.com/, sponsoring Virtual TreeView development
//   with a free copy of the Doc-O-Matic help authoring system), Sven H. (Step by step tutorial)
// Source repository:
//   https://github.com/Virtual-TreeView/Virtual-TreeView
// Accessability implementation:
//   Marco Zehe (with help from Sebastian Modersohn)
// Port to Firemonkey:
//   Karol Bieniaszewski (github user livius2)
//----------------------------------------------------------------------------------------------------------------------

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
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX,
  System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.ImgList, Vcl.Menus, Vcl.Themes,
  VirtualTrees.Types,
  VirtualTrees.Header,
  VirtualTrees.BaseTree,
{$IFDEF VT_FMX}
  VirtualTrees.AncestorFMX,
{$ELSE}
  VirtualTrees.AncestorVCL
{$ENDIF}
  ;
 

  {$MinEnumSize 1, make enumerations as small as possible}

type
  // Some aliases for backward compatiblity
  PVirtualNode             = VirtualTrees.Types.PVirtualNode;
  TVirtualNode             = VirtualTrees.Types.TVirtualNode;
  TVTHeaderColumnLayout    = VirtualTrees.Types.TVTHeaderColumnLayout;
  TSmartAutoFitType        = VirtualTrees.Types.TSmartAutoFitType;
  TVirtualTreeStates       = VirtualTrees.Types.TVirtualTreeStates;
  TCheckState              = VirtualTrees.Types.TCheckState;
  TCheckType               = VirtualTrees.Types.TCheckType;
  TSortDirection           = VirtualTrees.Types.TSortDirection;
  TColumnIndex             = VirtualTrees.Types.TColumnIndex;
  TVTColumnOption          = VirtualTrees.Types.TVTColumnOption;
  TVTHeaderHitInfo         = VirtualTrees.Types.TVTHeaderHitInfo;
  TVTHeaderHitPosition     = VirtualTrees.Types.TVTHeaderHitPosition;
  TVTHeaderHitPositions    = VirtualTrees.Types.TVTHeaderHitPositions;
  THeaderState             = VirtualTrees.Types.THeaderState;
  THeaderStates            = VirtualTrees.Types.THeaderStates;
  TDropMode                = VirtualTrees.Types.TDropMode;
  TFormatArray             = VirtualTrees.Types.TFormatArray;
  TVTHeaderOption          = VirtualTrees.Types.TVTHeaderOption;
  TVTHeaderOptions         = VirtualTrees.Types.TVTHeaderOptions;
  TVTHeaderStyle           = VirtualTrees.Types.TVTHeaderStyle;
  TVTExportType            = VirtualTrees.Types.TVTExportType;
  TVTImageKind             = VirtualTrees.Types.TVTImageKind;
  TVTExportMode            = VirtualTrees.Types.TVTExportMode;
  TVTOperationKind         = VirtualTrees.Types.TVTOperationKind;
  TVTUpdateState           = VirtualTrees.Types.TVTUpdateState;
  TVTCellPaintMode         = VirtualTrees.Types.TVTCellPaintMode;
  TVirtualNodeState        = VirtualTrees.Types.TVirtualNodeState;
  TVirtualNodeInitState    = VirtualTrees.Types.TVirtualNodeInitState;
  TVirtualNodeInitStates   = VirtualTrees.Types.TVirtualNodeInitStates;
  TVTTooltipLineBreakStyle = VirtualTrees.Types.TVTTooltipLineBreakStyle;
  TVTNodeAttachMode        = VirtualTrees.Types.TVTNodeAttachMode;
  TNodeArray               = VirtualTrees.Types.TNodeArray;
  THitInfo                 = VirtualTrees.Types.THitInfo;
  THitPosition             = VirtualTrees.Types.THitPosition;
  TVTPaintOption           = VirtualTrees.Types.TVTPaintOption;
  TVTAutoOption            = VirtualTrees.Types.TVTAutoOption;
  TVTAutoOptions           = VirtualTrees.Types.TVTAutoOptions;
  TVTSelectionOption       = VirtualTrees.Types.TVTSelectionOption;
  TVstTextType             = VirtualTrees.Types.TVstTextType;
  TVTHintMode              = VirtualTrees.Types.TVTHintMode;
  TBaseVirtualTree         = VirtualTrees.BaseTree.TBaseVirtualTree;
  IVTEditLink              = VirtualTrees.BaseTree.IVTEditLink;
  TVTHeaderNotifyEvent     = VirtualTrees.BaseTree.TVTHeaderNotifyEvent;
  TVTCompareEvent          = VirtualTrees.BaseTree.TVTCompareEvent;
  TVirtualTreeColumn       = VirtualTrees.Header.TVirtualTreeColumn;
  TVirtualTreeColumns      = VirtualTrees.Header.TVirtualTreeColumns;
  TVTHeader                = VirtualTrees.Header.TVTHeader;
  TVTHeaderClass           = VirtualTrees.Header.TVTHeaderClass;
  THeaderPaintInfo         = VirtualTrees.Header.THeaderPaintInfo;
  TVTConstraintPercent     = VirtualTrees.Header.TVTConstraintPercent;
  TVTFixedAreaConstraints  = VirtualTrees.Header.TVTFixedAreaConstraints;
  TColumnsArray            = VirtualTrees.Header.TColumnsArray;
  TCanvas                  = Vcl.Graphics.TCanvas;

const
  // Aliases for increased compatibility with V7, feel free to extend by pull requests
  NoColumn                 = VirtualTrees.Types.NoColumn;
  InvalidColumn            = VirtualTrees.Types.InvalidColumn;
  sdAscending              = VirtualTrees.Types.TSortDirection.sdAscending;
  sdDescending             = VirtualTrees.Types.TSortDirection.sdDescending;
  toAutoSort               = VirtualTrees.Types.TVTAutoOption.toAutoSort;
  toCheckSupport           = VirtualTrees.Types.TVTMiscOption.toCheckSupport;
  toEditable               = VirtualTrees.Types.TVTMiscOption.toEditable;
  toShowRoot               = VirtualTrees.Types.TVTPaintOption.toShowRoot;
  ctNone                   = VirtualTrees.Types.TCheckType.ctNone;
  ctTriStateCheckBox       = VirtualTrees.Types.TCheckType.ctTriStateCheckBox;
  ctCheckBox               = VirtualTrees.Types.TCheckType.ctCheckBox;
  ctRadioButton            = VirtualTrees.Types.TCheckType.ctRadioButton;
  ctButton                 = VirtualTrees.Types.TCheckType.ctButton;

  csUncheckedNormal        = VirtualTrees.Types.TCheckState.csUncheckedNormal;
  csUncheckedPressed       = VirtualTrees.Types.TCheckState.csUncheckedPressed;
  csCheckedNormal          = VirtualTrees.Types.TCheckState.csCheckedNormal;
  csCheckedPressed         = VirtualTrees.Types.TCheckState.csCheckedPressed;
  csMixedNormal            = VirtualTrees.Types.TCheckState.csMixedNormal;
  csMixedPressed           = VirtualTrees.Types.TCheckState.csMixedPressed;
  csUncheckedDisabled      = VirtualTrees.Types.TCheckState.csUncheckedDisabled;
  csCheckedDisabled        = VirtualTrees.Types.TCheckState.csCheckedDisabled;
  csMixedDisable           = VirtualTrees.Types.TCheckState.csMixedDisabled;

  coVisible                = VirtualTrees.Types.TVTColumnOption.coVisible;
  vsDisabled               = VirtualTrees.Types.TVirtualNodeState.vsDisabled;
  etHTML                   = VirtualTrees.Types.TVTExportType.etHTML;
  hiOnItemButton           = VirtualTrees.Types.THitPosition.hiOnItemButton;
  dmOnNode                 = VirtualTrees.Types.TDropMode.dmOnNode;
  hlbForceMultiLine        = VirtualTrees.Types.TVTTooltipLineBreakStyle.hlbForceMultiLine;
  hmHintAndDefault         = VirtualTrees.Types.TVTHintMode.hmHintAndDefault;
  hmTooltip                = VirtualTrees.Types.TVTHintMode.hmTooltip;

type
  TCustomVirtualStringTree = class;

{$IFDEF VT_FMX}
  TVTAncestor = TVTAncestorFMX;
{$ELSE}
  TVTAncestor = TVTAncestorVcl;
{$ENDIF}

  // Describes the source to use when converting a string tree into a string for clipboard etc.
  TVSTTextSourceType = (
    tstAll,             // All nodes are rendered. Initialization is done on the fly.
    tstInitialized,     // Only initialized nodes are rendered.
    tstSelected,        // Only selected nodes are rendered.
    tstCutCopySet,      // Only nodes currently marked as being in the cut/copy clipboard set are rendered.
    tstVisible,         // Only visible nodes are rendered.
    tstChecked          // Only checked nodes are rendered
  );

  TVSTGetTextEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string) of object;
  TVSTGetHintEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string) of object;
  // New text can only be set for variable caption.
  TVSTNewTextEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    NewText: string) of object;
  /// <summary>String tree event for custom handling of string abbreviations.</summary>
  /// <param name="Sender">The instance that fired the event.</param>
  /// <param name="TargetCanvas">Teh canvas on that the sending control will paint.</param>
  /// <param name="Node">The Node that is going to be painted.</param>
  /// <param name="Column">The column index that is going to be painted.</param>
  /// <param name="Result">Var parameter that contains the caption or string that should be used.</param>
  /// <param name="Done">Boolean var paramter: Assign True if a string is passed in the Result parameter. Leave the default value False if no shorting is need or the control shuld do it. </param>
  /// <remarks>
  ///  If the text of a node does not fit into its cell (in grid mode) or is too wide for the width of the tree view it is being abbreviated with an ellipsis (...). By default the ellipsis is added to the end of the node text.
  ///  Occasionally you may want to shorten the node text at a different position, for example if the node text is a path string and not the last folder or filename should be cut off but rather some mid level folders if possible.
  /// </remarks>
  TVSTShortenStringEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; const S: string; TextSpace: TDimension; var Result: string;
    var Done: Boolean) of object;
  TVTMeasureTextEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; const Text: string; var Extent: TDimension) of object;
  TVTDrawTextEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean) of object;

  /// Event arguments of the OnGetCellText event
  TVSTGetCellTextEventArgs = record
    Node: PVirtualNode;
    Column: TColumnIndex;
    CellText: string;
    StaticText: string;
    StaticTextAlignment: TAlignment;
    ExportType: TVTExportType;
    constructor Create(pNode: PVirtualNode; pColumn: TColumnIndex; pExportType: TVTExportType = TVTExportType.etNone);
  end;

  /// Event signature which is called when text is painted on the canvas or needed for the export.
  TVSTGetCellTextEvent = procedure(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs) of object;

  TCustomVirtualStringTree = class(TVTAncestor)
  private
    FInternalDataOffset: Cardinal;        // offset to the internal data of the string tree
    FDefaultText: string;                   // text to show if there's no OnGetText event handler (e.g. at design time)
    FTextHeight: Integer;                          // true size of the font
    FEllipsisWidth: Integer;                       // width of '...' for the current font

    FOnGetText: TVSTGetTextEvent;                  // used to retrieve the string to be displayed for a specific node
    fOnGetCellText: TVSTGetCellTextEvent;             // used to retrieve the normal and static text of a tree node
    FOnGetHint: TVSTGetHintEvent;                  // used to retrieve the hint to be displayed for a specific node
    FOnNewText: TVSTNewTextEvent;                  // used to notify the application about an edited node caption
    FOnShortenString: TVSTShortenStringEvent;      // used to allow the application a customized string shortage
    FOnMeasureTextWidth: TVTMeasureTextEvent;      // used to adjust the width of the cells
    FOnMeasureTextHeight: TVTMeasureTextEvent;
    FOnDrawText: TVTDrawTextEvent;                 // used to custom draw the node text
    /// Returns True if the property DefaultText has a value that differs from the default value, False otherwise.
    function IsDefaultTextStored(): Boolean;
    function GetImageText(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex): string;
    function GetOptions: TCustomStringTreeOptions;
    function GetStaticText(Node: PVirtualNode; Column: TColumnIndex): string;
    function GetText(Node: PVirtualNode; Column: TColumnIndex): string;
    procedure ReadText(Reader: TReader);
    procedure WriteText(Writer: TWriter);
    procedure ResetInternalData(Node: PVirtualNode; Recursive: Boolean);
    procedure SetDefaultText(const Value: string);
    procedure SetOptions(const Value: TCustomStringTreeOptions);
    procedure SetText(Node: PVirtualNode; Column: TColumnIndex; const Value: string);
    procedure WMSetFont(var Msg: TWMSetFont); message WM_SETFONT;
    procedure GetDataFromGrid(const AStrings : TStringList; const IncludeHeading : Boolean = True);
  protected
    /// <summary>Contains the name of the string that should be restored as selection</summary>
    /// <seealso cref="TVTSelectionOption.toRestoreSelection">
    FPreviouslySelected: TStringList;
    procedure InitializeTextProperties(var PaintInfo: TVTPaintInfo);
    procedure PaintNormalText(var PaintInfo: TVTPaintInfo; TextOutFlags: Integer; Text: string); virtual;
    procedure PaintStaticText(const PaintInfo: TVTPaintInfo; pStaticTextAlignment: TAlignment; const Text: string); virtual; // [IPK] - private to protected
    procedure AdjustPaintCellRect(var PaintInfo: TVTPaintInfo; var NextNonEmpty: TColumnIndex); override;
    function CanExportNode(Node: PVirtualNode): Boolean;
    function CalculateStaticTextWidth(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string): TDimension; virtual;
    function CalculateTextWidth(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string): TDimension; virtual;
    function ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink; override;
    procedure DoAddToSelection(Node: PVirtualNode); override;
    function DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): string; override;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): string; override;
    function DoGetNodeExtraWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): TDimension; override;
    function DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): TDimension; override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); virtual;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string); virtual;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    function DoShortenString(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const S: string; Width: TDimension;
      EllipsisWidth: TDimension = 0): string; virtual;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal); virtual;
    function DoTextMeasuring(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string): TSize; virtual;
    function GetOptionsClass: TTreeOptionsClass; override;
    procedure GetRenderStartValues(Source: TVSTTextSourceType; var Node: PVirtualNode;
      var NextNodeProc: TGetNextNodeProc);
    function InternalData(Node: PVirtualNode): Pointer;
    procedure MainColumnChanged; override;
    function ReadChunk(Stream: TStream; Version: Integer; Node: PVirtualNode; ChunkType,
      ChunkSize: Integer): Boolean; override;
    procedure ReadOldStringOptions(Reader: TReader);
    function RenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HResult; override;
    procedure SetChildCount(Node: PVirtualNode; NewChildCount: Cardinal); override;
    procedure WriteChunks(Stream: TStream; Node: PVirtualNode); override;

    property DefaultText: string read FDefaultText write SetDefaultText stored False;// Stored via own writer
    property EllipsisWidth: Integer read FEllipsisWidth;
    property TreeOptions: TCustomStringTreeOptions read GetOptions write SetOptions;

    property OnGetHint: TVSTGetHintEvent read FOnGetHint write FOnGetHint;
    property OnGetText: TVSTGetTextEvent read FOnGetText write FOnGetText;
    property OnGetCellText: TVSTGetCellTextEvent read fOnGetCellText write fOnGetCellText;
    property OnNewText: TVSTNewTextEvent read FOnNewText write FOnNewText;
    property OnShortenString: TVSTShortenStringEvent read FOnShortenString write FOnShortenString;
    property OnMeasureTextWidth: TVTMeasureTextEvent read FOnMeasureTextWidth write FOnMeasureTextWidth;
    property OnMeasureTextHeight: TVTMeasureTextEvent read FOnMeasureTextHeight write FOnMeasureTextHeight;
    property OnDrawText: TVTDrawTextEvent read FOnDrawText write FOnDrawText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function AddChild(Parent: PVirtualNode; UserData: Pointer = nil): PVirtualNode; override;
    function ComputeNodeHeight(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; S: string = ''): TDimension; virtual;
    function ContentToClipboard(Format: Word; Source: TVSTTextSourceType): HGLOBAL;
    procedure ContentToCustom(Source: TVSTTextSourceType);
    function ContentToHTML(Source: TVSTTextSourceType; const Caption: string = ''): String;
    function ContentToRTF(Source: TVSTTextSourceType): RawByteString;
    function ContentToText(Source: TVSTTextSourceType; Separator: Char): String; overload;
    function ContentToUnicode(Source: TVSTTextSourceType; Separator: WideChar): string; overload; deprecated 'Use ContentToText instead';
    function ContentToText(Source: TVSTTextSourceType; const Separator: string): string; overload;
    procedure GetTextInfo(Node: PVirtualNode; Column: TColumnIndex; const AFont: TFont; var R: TRect;
      var Text: string); override;
    function InvalidateNode(Node: PVirtualNode): TRect; override;
    function Path(Node: PVirtualNode; Column: TColumnIndex; Delimiter: Char): string;
    procedure ReinitNode(Node: PVirtualNode; Recursive: Boolean; ForceReinit:
        Boolean = False); override;
    procedure RemoveFromSelection(Node: PVirtualNode); override;
    function SaveToCSVFile(const FileNameWithPath : TFileName; const IncludeHeading : Boolean) : Boolean;
    /// Alternate text for images used in Accessibility.
    property ImageText[Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex]: string read GetImageText;
    property StaticText[Node: PVirtualNode; Column: TColumnIndex]: string read GetStaticText;
    property Text[Node: PVirtualNode; Column: TColumnIndex]: string read GetText write SetText;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TVirtualStringTree = class(TCustomVirtualStringTree)
  private
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
  protected
    function GetOptionsClass: TTreeOptionsClass; override;
  public
    property Canvas;
    property RangeX;
    property LastDragEffect;
    property CheckImageKind; // should no more be published to make #622 fix working
  published
    property AccessibleName;
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackGroundImageTransparent;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderStyle;
    property BottomSpace;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property Ctl3D;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DefaultText;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property EmptyListMessage;
    property Enabled;
    property Font;
    property Header;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property OperationCanceled;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootNodeCount;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property StyleElements;
    {$if CompilerVersion >= 34}property StyleName;{$ifend}
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
    property Visible;
    property WantTabs;

    property OnAddToSelection;
    property OnAdvancedHeaderDraw;
    property OnAfterAutoFitColumn;
    property OnAfterAutoFitColumns;
    property OnAfterCellPaint;
    property OnAfterColumnExport;
    property OnAfterColumnWidthTracking;
    property OnAfterGetMaxColumnWidth;
    property OnAfterHeaderExport;
    property OnAfterHeaderHeightTracking;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterNodeExport;
    property OnAfterPaint;
    property OnAfterTreeExport;
    property OnBeforeAutoFitColumn;
    property OnBeforeAutoFitColumns;
    property OnBeforeCellPaint;
    property OnBeforeColumnExport;
    property OnBeforeColumnWidthTracking;
    property OnBeforeDrawTreeLine;
    property OnBeforeGetMaxColumnWidth;
    property OnBeforeHeaderExport;
    property OnBeforeHeaderHeightTracking;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforeNodeExport;
    property OnBeforePaint;
    property OnBeforeTreeExport;
    property OnCanSplitterResizeColumn;
    property OnCanSplitterResizeHeader;
    property OnCanSplitterResizeNode;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnChecked;
    property OnColumnChecking;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnExport;
    property OnColumnResize;
    property OnColumnVisibilityChanged;
    property OnColumnWidthDblClickResize;
    property OnColumnWidthTracking;
    property OnCompareNodes;
    property OnContextPopup;
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnDrawHint;
    property OnDrawText;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEndOperation;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellText;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetText;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetHintKind;
    property OnGetHintSize;
    property OnGetImageIndex;
    property OnGetImageIndexEx;
    property OnGetImageText;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderAddPopupItem;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderHeightDblClickResize;
    property OnHeaderHeightTracking;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnLoadTree;
    property OnMeasureItem;
    property OnMeasureTextWidth;
    property OnMeasureTextHeight;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnNewText;
    property OnNodeClick;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeDblClick;
    property OnNodeExport;
    property OnNodeHeightDblClickResize;
    property OnNodeHeightTracking;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnPrepareButtonBitmaps;
    property OnRemoveFromSelection;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnSaveTree;
    property OnScroll;
    property OnShortenString;
    property OnShowScrollBar;
    property OnBeforeGetCheckState;
    property OnStartDock;
    property OnStartDrag;
    property OnStartOperation;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
    property OnCanResize;
    property OnGesture;
    property Touch;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation
uses
  System.TypInfo,              // for migration stuff
  System.StrUtils,
  System.Types,                // prevent inline compiler warning
  System.UITypes,              // prevent inline compiler warning
  VirtualTrees.StyleHooks,
  VirtualTrees.ClipBoard,
  VirtualTrees.Utils,
  VirtualTrees.Export,
  VirtualTrees.EditLink,
  VirtualTrees.BaseAncestorVcl{to eliminate H2443 about inline expanding}
  ;

const
  cDefaultText = 'Node';
  RTLFlag: array[Boolean] of Integer = (0, ETO_RTLREADING);
  AlignmentToDrawFlag: array[TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
  gInitialized: Integer = 0;           // >0 if global structures have been initialized; otherwise 0

//// initialization of stuff global to the unit
procedure InitializeGlobalStructures();
begin
  if (gInitialized > 0) or (AtomicIncrement(gInitialized) <> 1) then // Ensure threadsafe that this code is executed only once
    exit;

  // Clipboard format registration.
  // Specialized string tree formats.
  CF_HTML := RegisterVTClipboardFormat(CFSTR_HTML, TCustomVirtualStringTree, 80);
  CF_VRTFNOOBJS := RegisterVTClipboardFormat(CFSTR_RTFNOOBJS, TCustomVirtualStringTree, 84);
  CF_VRTF := RegisterVTClipboardFormat(CFSTR_RTF, TCustomVirtualStringTree, 85);
  CF_CSV := RegisterVTClipboardFormat(CFSTR_CSV, TCustomVirtualStringTree, 90);
  // Predefined clipboard formats. Just add them to the internal list.
  RegisterVTClipboardFormat(CF_TEXT, TCustomVirtualStringTree, 100);
  RegisterVTClipboardFormat(CF_UNICODETEXT, TCustomVirtualStringTree, 95);
end;


//----------------- TCustomVirtualString -------------------------------------------------------------------------------

constructor TCustomVirtualStringTree.Create(AOwner: TComponent);

begin
  InitializeGlobalStructures();
  inherited;
  FPreviouslySelected := nil;
  FDefaultText := cDefaultText;
  FInternalDataOffset := AllocateInternalDataArea(SizeOf(Cardinal));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.GetRenderStartValues(Source: TVSTTextSourceType; var Node: PVirtualNode;
  var NextNodeProc: TGetNextNodeProc);

begin
  case Source of
    tstInitialized:
      begin
        Node := GetFirstInitialized;
        NextNodeProc := GetNextInitialized;
      end;
    tstSelected:
      begin
        Node := GetFirstSelected;
        NextNodeProc := GetNextSelected;
      end;
    tstCutCopySet:
      begin
        Node := GetFirstCutCopy;
        NextNodeProc := GetNextCutCopy;
      end;
    tstVisible:
      begin
        Node := GetFirstVisible(nil, True);
        NextNodeProc := GetNextVisible;
      end;
    tstChecked:
      begin
        Node := GetFirstChecked;
        NextNodeProc := GetNextChecked;
      end;
  else // tstAll
    Node := GetFirst;
    NextNodeProc := GetNext;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.GetDataFromGrid(const AStrings: TStringList;
  const IncludeHeading: Boolean);
var
  LColIndex   : Integer;
  LStartIndex : Integer;
  LAddString  : string;
  LCellText   : string;
  LChildNode  : PVirtualNode;
begin
  { Start from the First column. }
  LStartIndex := 0;

  { Do it for Header first }
  if IncludeHeading then
  begin
    LAddString := EmptyStr;
    for LColIndex := LStartIndex to Pred(Header.Columns.Count) do
    begin
      if (LColIndex > LStartIndex) then
        LAddString := LAddString + ',';
      LAddString := LAddString + AnsiQuotedStr(Header.Columns.Items[LColIndex].Text, '"');
    end;//for
    AStrings.Add(LAddString);
  end;//if

  { Loop thru the virtual tree for Data }
  LChildNode := GetFirst;
  while Assigned(LChildNode) do
  begin
    LAddString := EmptyStr;

    { Read for each column and then populate the text }
    for LColIndex := LStartIndex to Pred(Header.Columns.Count) do
    begin
      LCellText := Text[LChildNode, LColIndex];
      if (LCellText = EmptyStr) then
        LCellText := ' ';
      if (LColIndex > LStartIndex) then
        LAddString := LAddString + ',';
      LAddString := LAddString + AnsiQuotedStr(LCellText, '"');
    end;//for - Header.Columns.Count

    AStrings.Add(LAddString);
    LChildNode := LChildNode.NextSibling;
  end;//while Assigned(LChildNode);
end;

function TCustomVirtualStringTree.GetImageText(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex): string;
begin
  Assert(Assigned(Node), 'Node must not be nil.');

  if not (vsInitialized in Node.States) then
    InitNode(Node);
  Result := '';

  DoGetImageText(Node, Kind, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.GetOptions: TCustomStringTreeOptions;

begin
  Result := inherited TreeOptions as TCustomStringTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.GetStaticText(Node: PVirtualNode; Column: TColumnIndex): string;

var
  lEventArgs: TVSTGetCellTextEventArgs;

begin
  Assert(Assigned(Node), 'Node must not be nil.');
  lEventArgs := TVSTGetCellTextEventArgs.Create(Node, Column);
  DoGetText(lEventArgs);
  Exit(lEventArgs.StaticText);
end;


//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.GetText(Node: PVirtualNode; Column: TColumnIndex): string;

var
  lEventArgs: TVSTGetCellTextEventArgs;

begin
  Assert(Assigned(Node), 'Node must not be nil.');
  lEventArgs := TVSTGetCellTextEventArgs.Create(Node, Column);
  lEventArgs.CellText := FDefaultText;
  DoGetText(lEventArgs);
  Exit(lEventArgs.CellText)
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.InitializeTextProperties(var PaintInfo: TVTPaintInfo);

// Initializes default values for customization in PaintNormalText.

begin
  with PaintInfo do
  begin
    // Set default font values first.
    Canvas.Font.Assign(Font);
    if Enabled then // Otherwise only those colors are used, which are passed from Font to Canvas.Font.
      Canvas.Font.Color := Colors.NodeFontColor
    else
      Canvas.Font.Color := Colors.DisabledColor;

    if (toHotTrack in TreeOptions.PaintOptions) and (Node = HotNode) then
    begin
      if not (tsUseExplorerTheme in TreeStates) then
      begin
        Canvas.Font.Style := Canvas.Font.Style + [TFontStyle.fsUnderline];
        Canvas.Font.Color := Colors.HotColor;
      end;
    end;

    // Change the font color only if the node also is drawn in selected style.
    if poDrawSelection in PaintOptions then
    begin
      if (Column = FocusedColumn) or (toFullRowSelect in TreeOptions.SelectionOptions) then
      begin
        if Node = DropTargetNode then
        begin
          if ((LastDropMode = dmOnNode) or (vsSelected in Node.States)) then
            Canvas.Font.Color := Colors.GetSelectedNodeFontColor(True); // See #1083, since drop highlight color is chosen independent of the focus state, we need to choose Font color also independent of it.
        end
        else
          if vsSelected in Node.States then
          begin
            Canvas.Font.Color := Colors.GetSelectedNodeFontColor(Focused or (toPopupMode in TreeOptions.PaintOptions));
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.PaintNormalText(var PaintInfo: TVTPaintInfo; TextOutFlags: Integer;
  Text: string);

// This method is responsible for painting the given text to target canvas (under consideration of the given rectangles).
// The text drawn here is considered as the normal text in a node.
// Note: NodeWidth is the actual width of the text to be drawn. This does not necessarily correspond to the width of
//       the node rectangle. The clipping rectangle comprises the entire node (including tree lines, buttons etc.).

var
  TripleWidth: TDimension;
  R: TRect;
  DrawFormat: Cardinal;
  Height: TDimension;
  lNewNodeWidth: TDimension;
begin
  InitializeTextProperties(PaintInfo);
  with PaintInfo do
  begin
    R := ContentRect;
    Canvas.TextFlags := 0;
    InflateRect(R, -TextMargin, 0);

    if (vsDisabled in Node.States) or not Enabled then
      Canvas.Font.Color := Colors.DisabledColor;
    // Multiline nodes don't need special font handling or text manipulation.
    // Note: multiline support requires the Unicode version of DrawText, which is able to do word breaking.
    //       The emulation in this unit does not support this so we have to use the OS version. However
    //       DrawTextW is only available on NT/2000/XP and up. Hence there is only partial multiline support
    //       for 9x/Me.
    if vsMultiline in Node.States then
    begin
      DoPaintText(Node, Canvas, Column, ttNormal);
      Height := ComputeNodeHeight(Canvas, Node, Column);

      // The edit control flag will ensure that no partial line is displayed, that is, only lines
      // which are (vertically) fully visible are drawn.
      DrawFormat := DT_NOPREFIX or DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL or AlignmentToDrawFlag[Alignment];
      if BidiMode <> bdLeftToRight then
        DrawFormat := DrawFormat or DT_RTLREADING;

      // Center the text vertically if it fits entirely into the content rect.
      if R.Bottom - R.Top > Height then
        InflateRect(R, 0, Divide(Height - R.Bottom - R.Top, 2));
    end
    else
    begin
      FFontChanged := False;
      TripleWidth := FEllipsisWidth;
      DoPaintText(Node, Canvas, Column, ttNormal);
      if FFontChanged then
      begin
        // If the font has been changed then the ellipsis width must be recalculated.
        TripleWidth := 0;
        // Recalculate also the width of the normal text.
        lNewNodeWidth := DoTextMeasuring(Canvas, Node, Column, Text).cx + 2 * TextMargin;
        if lNewNodeWidth <> NodeWidth then
        begin
          NodeWidth := lNewNodeWidth;
          InvalidateNode(Node); // repaint node and selection as the font chnaged, see #1084
        end;//if
      end;// if FFontChanged

      DrawFormat := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE;
      if BidiMode <> bdLeftToRight then
        DrawFormat := DrawFormat or DT_RTLREADING;
      // Check if the text must be shortend.
      if (Column > NoColumn) and ((NodeWidth - 2 * TextMargin) > R.Width) then
      begin
        Text := DoShortenString(Canvas, Node, Column, Text, R.Right - R.Left, TripleWidth);
        if Alignment = taRightJustify then
          DrawFormat := DrawFormat or DT_RIGHT
        else
          DrawFormat := DrawFormat or DT_LEFT;
      end
      else
        DrawFormat := DrawFormat or AlignmentToDrawFlag[Alignment];
    end;

    if Canvas.TextFlags and ETO_OPAQUE = 0 then
      SetBkMode(Canvas.Handle, TRANSPARENT)
    else
      SetBkMode(Canvas.Handle, OPAQUE);

    DoTextDrawing(PaintInfo, Text, R, DrawFormat);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.PaintStaticText(const PaintInfo: TVTPaintInfo; pStaticTextAlignment: TAlignment; const Text: string);

// This method retrives and draws the static text bound to a particular node.

var
  R: TRect;
  DrawFormat: Cardinal;

begin
  with PaintInfo do
  begin
    Canvas.Font.Assign(Font);
    if toFullRowSelect in TreeOptions.SelectionOptions then
    begin
      if Node = DropTargetNode then
      begin
        if (LastDropMode = dmOnNode) or (vsSelected in Node.States) then
          Canvas.Font.Color := Colors.GetSelectedNodeFontColor(Focused or (toPopupMode in TreeOptions.PaintOptions))
        else
          Canvas.Font.Color := Colors.NodeFontColor;
      end
      else
        if vsSelected in Node.States then
        begin
          if Focused or (toPopupMode in TreeOptions.PaintOptions) then
            Canvas.Font.Color := Colors.GetSelectedNodeFontColor(Focused or (toPopupMode in TreeOptions.PaintOptions))
          else
            Canvas.Font.Color := Colors.NodeFontColor;
        end;
    end;

    DrawFormat := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE;
    Canvas.TextFlags := 0;
    DoPaintText(Node, Canvas, Column, ttStatic);

    // Disabled node color overrides all other variants.
    if (vsDisabled in Node.States) or not Enabled then
      Canvas.Font.Color := Colors.DisabledColor;

    R := ContentRect;
    if pStaticTextAlignment = taRightJustify then begin
      DrawFormat := DrawFormat or DT_RIGHT;
      Dec(R.Right, TextMargin);
      if PaintInfo.Alignment = taRightJustify then
         Dec(R.Right, NodeWidth); // room for node text
    end
    else begin
      Inc(R.Left, TextMargin);
      if PaintInfo.Alignment = taLeftJustify then
        Inc(R.Left, NodeWidth); // room for node text
    end;

    if Canvas.TextFlags and ETO_OPAQUE = 0 then
      SetBkMode(Canvas.Handle, TRANSPARENT)
    else
      SetBkMode(Canvas.Handle, OPAQUE);
    Winapi.Windows.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, DrawFormat);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.ReadText(Reader: TReader);
begin
  SetDefaultText(Reader.ReadString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.SaveToCSVFile(
  const FileNameWithPath: TFileName; const IncludeHeading: Boolean): Boolean;
var
  LResultList : TStringList;
begin
  Result := False;
  if (FileNameWithPath = '') then
    Exit;

  LResultList := TStringList.Create;
  try
    { Get the data from grid. }
    GetDataFromGrid(LResultList, IncludeHeading);
    { Save File to Disk }
    LResultList.SaveToFile(FileNameWithPath);
    Result := True;
  finally
    FreeAndNil(LResultList);
  end;//try-finally
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.SetDefaultText(const Value: string);

begin
  if FDefaultText <> Value then
  begin
    FDefaultText := Value;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.SetOptions(const Value: TCustomStringTreeOptions);

begin
  inherited TreeOptions.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.SetText(Node: PVirtualNode; Column: TColumnIndex; const Value: string);

begin
  DoNewText(Node, Column, Value);
  InvalidateNode(Node);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.WMSetFont(var Msg: TWMSetFont);

// Whenever a new font is applied to the tree some default values are determined to avoid frequent
// determination of the same value.

var
  MemDC: HDC;
  Run: PVirtualNode;
  TM: TTextMetric;
  Size: TSize;
  Data: PInteger;

begin
  inherited;

  MemDC := CreateCompatibleDC(0);
  try
    SelectObject(MemDC, Msg.Font);
    WinApi.Windows.GetTextMetrics(MemDC, TM);
    FTextHeight := TM.tmHeight;

    GetTextExtentPoint32W(MemDC, '...', 3, Size);
    FEllipsisWidth := Size.cx;
  finally
    DeleteDC(MemDC);
  end;

  // Have to reset all node widths.
  Run := RootNode.FirstChild;
  while Assigned(Run) do
  begin
    Data := InternalData(Run);
    if Assigned(Data) then
      Data^ := 0;
    Run := GetNextNoInit(Run);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.AddChild(Parent: PVirtualNode; UserData: Pointer): PVirtualNode;
var
  NewNodeText: string;
begin
  Result := inherited AddChild(Parent, UserData);
  // Restore the prviously restored node if the caption of this node is knwon and no other node was selected
  if (toRestoreSelection in TreeOptions.SelectionOptions) and Assigned(FPreviouslySelected) and Assigned(OnGetText) then
  begin
    // See if this was the previously selected node and restore it in this case
    Self.OnGetText(Self, Result, Header.RestoreSelectionColumnIndex, ttNormal, NewNodeText);
    if FPreviouslySelected.IndexOf(NewNodeText) >= 0 then
    begin
      // Select this node and make sure that the parent node is expanded
      TreeStates:= TreeStates + [tsPreviouslySelectedLocked];
      try
        Self.Selected[Result] := True;
      finally
        TreeStates:= TreeStates - [tsPreviouslySelectedLocked];
      end;
      // if a there is a selected node now, then make sure that it is visible
      if (Self.GetFirstSelected <> nil) then
        Self.FullyVisible[Self.GetFirstSelected]:= True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.AdjustPaintCellRect(var PaintInfo: TVTPaintInfo; var NextNonEmpty: TColumnIndex);

// In the case a node spans several columns (if enabled) we need to determine how many columns.
// Note: the autospan feature can only be used with left-to-right layout.

begin
  if (toAutoSpanColumns in TreeOptions.AutoOptions) and Header.UseColumns and (PaintInfo.BidiMode = bdLeftToRight) then
    with Header.Columns, PaintInfo do
    begin
      // Start with the directly following column.
      NextNonEmpty := GetNextVisibleColumn(Column);

      // Auto spanning columns can only be used for left-to-right directionality because the tree is drawn
      // from left to right. For RTL directionality it would be necessary to draw it from right to left.
      // While this could be managed, it becomes impossible when directionality is mixed.
      repeat
        if (NextNonEmpty = InvalidColumn) or not ColumnIsEmpty(Node, NextNonEmpty) or
          (Items[NextNonEmpty].BidiMode <> bdLeftToRight) then
          Break;
        Inc(CellRect.Right, Items[NextNonEmpty].Width);
        NextNonEmpty := GetNextVisibleColumn(NextNonEmpty);
      until False;
    end
    else
      inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.CalculateStaticTextWidth(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string): TDimension;

begin
  Result := 0;
  if (Length(Text) > 0) and (Alignment <> taCenter) and not (vsMultiline in Node.States) then
  begin
    DoPaintText(Node, Canvas, Column, ttStatic);

    Inc(Result, DoTextMeasuring(Canvas, Node, Column, Text).cx);
    Inc(Result, TextMargin);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.CalculateTextWidth(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string): TDimension;

// Determines the width of the given text.

begin
  Result := 2 * TextMargin;
  if Length(Text) > 0 then
  begin
    Canvas.Font.Assign(Font);
    DoPaintText(Node, Canvas, Column, ttNormal);

    Inc(Result, DoTextMeasuring(Canvas, Node, Column, Text).cx);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean;

// For hit tests it is necessary to consider cases where columns are empty and automatic column spanning is enabled.
// This method simply checks the given column's text and if this is empty then the column is considered as being empty.

begin
  Result := Length(Text[Node, Column]) = 0;
  // If there is no text then let the ancestor decide if the column is to be considered as being empty
  // (e.g. by asking the application). If there is text then the column is never be considered as being empty.
  if Result then
    Result := inherited ColumnIsEmpty(Node, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DefineProperties(Filer: TFiler);

begin
  inherited;

  // For backwards compatiblity
  Filer.DefineProperty('WideDefaultText', ReadText, nil, False);
  // Delphi does never store an empty string unless we define the property in code.
  Filer.DefineProperty('DefaultText', ReadText, WriteText, IsDefaultTextStored);
  Filer.DefineProperty('StringOptions', ReadOldStringOptions, nil, False);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TCustomVirtualStringTree.Destroy;
begin
  FreeAndNil(FPreviouslySelected);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoAddToSelection(Node: PVirtualNode);
var
  lSelectedNodeCaption: string;
begin
  inherited;
  if (toRestoreSelection in TreeOptions.SelectionOptions) and Assigned(Self.OnGetText) and not (tsPreviouslySelectedLocked in TreeStates) then
  begin
    if not Assigned(FPreviouslySelected) then
    begin
      FPreviouslySelected := TStringList.Create();
      FPreviouslySelected.Duplicates := dupIgnore;
      FPreviouslySelected.Sorted := True; //Improves performance, required to use Find()
      FPreviouslySelected.CaseSensitive := False;
    end;
    if Self.SelectedCount = 1 then
      FPreviouslySelected.Clear();
    Self.OnGetText(Self, Node, Header.RestoreSelectionColumnIndex, ttNormal, lSelectedNodeCaption);
    FPreviouslySelected.Add(lSelectedNodeCaption);
  end;//if
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;
begin
  Result := inherited DoCreateEditor(Node, Column);
  // Enable generic label editing support if the application does not have own editors.
  if Result = nil then
    Result := TStringEditLink.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle): string;

begin
  Result := inherited DoGetNodeHint(Node, Column, LineBreakStyle);
  if Assigned(FOnGetHint) then
    FOnGetHint(Self, Node, Column, LineBreakStyle, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle): string;

begin
  Result := inherited DoGetNodeToolTip(Node, Column, LineBreakStyle);
  if Assigned(FOnGetHint) then
    FOnGetHint(Self, Node, Column, LineBreakStyle, Result)
  else
    Result := Text[Node, Column];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoGetNodeExtraWidth(Node: PVirtualNode; Column: TColumnIndex;
  Canvas: TCanvas = nil): TDimension;

begin
  if not (toShowStaticText in TreeOptions.StringOptions) then
    Exit(0);
  if Canvas = nil then
    Canvas := Self.Canvas;
  Result := CalculateStaticTextWidth(Canvas, Node, Column, StaticText[Node, Column]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): TDimension;

// Returns the text width of the given node in pixels.
// This width is stored in the node's data member to increase access speed.

var
  Data: PDimension;

begin
  if (Column > NoColumn) and (vsMultiline in Node.States) then
    Result := Header.Columns[Column].Width
  else
  begin
    if Canvas = nil then
      Canvas := Self.Canvas;

    if (Column = Header.MainColumn) or (Column = NoColumn) then
    begin
      // Primary column or no columns.
      Data := InternalData(Node);
      if Assigned(Data) then
      begin
        Result := Data^;
        if (Result = 0)
           or Header.doingAutoFitColumns then
        begin
          Data^ := CalculateTextWidth(Canvas, Node, Column, Text[Node, Column]);
          Result := Data^;
        end;
      end
      else
        Result := 0;
    end
    else
      // any other column
      Result := CalculateTextWidth(Canvas, Node, Column, Text[Node, Column]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);

begin
  if not (vsInitialized in pEventArgs.Node.States) then
    InitNode(pEventArgs.Node);
  if Assigned(OnGetCellText) then
  begin
    OnGetCellText(Self, pEventArgs);
  end
  else if Assigned(FOnGetText) then begin
    FOnGetText(Self, pEventArgs.Node, pEventArgs.Column, TVSTTextType.ttNormal, pEventArgs.CellText);
    if toShowStaticText in TreeOptions.StringOptions then
      FOnGetText(Self, pEventArgs.Node, pEventArgs.Column, TVSTTextType.ttStatic, pEventArgs.StaticText);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer;

// Since the string tree has access to node text it can do incremental search on its own. Use the event to
// override the default behavior.

begin
  Result := 0;
  if Assigned(OnIncrementalSearch) then
    OnIncrementalSearch(Self, Node, Text, Result)
  else
    // Default behavior is to match the search string with the start of the node text.
    if not StartsText(Text, GetText(Node, FocusedColumn)) then
      Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string);

begin
  if Assigned(FOnNewText) then
    FOnNewText(Self, Node, Column, Text);

  // The width might have changed, so update the scrollbar.
  if UpdateCount = 0 then
    UpdateHorizontalScrollBar(True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoPaintNode(var PaintInfo: TVTPaintInfo);

// Main output routine to print the text of the given node using the space provided in PaintInfo.ContentRect.

var
  lEventArgs: TVSTGetCellTextEventArgs;
  TextOutFlags: Integer;

begin
  // Set a new OnChange event for the canvas' font so we know if the application changes it in the callbacks.
  // This long winded procedure is necessary because font changes (as well as brush and pen changes) are
  // unfortunately not announced via the Canvas.OnChange event.
  RedirectFontChangeEvent(PaintInfo.Canvas);
  try

    // Determine main text direction as well as other text properties.
    TextOutFlags := ETO_CLIPPED or RTLFlag[PaintInfo.BidiMode <> bdLeftToRight];
    lEventArgs := TVSTGetCellTextEventArgs.Create(PaintInfo.Node, PaintInfo.Column);

    lEventArgs.CellText := FDefaultText;
    lEventArgs.StaticTextAlignment := PaintInfo.Alignment;
    DoGetText(lEventArgs);

    // Paint the normal text first...
    if not lEventArgs.CellText.IsEmpty then
      PaintNormalText(PaintInfo, TextOutFlags, lEventArgs.CellText);

    // ... and afterwards the static text if not centered and the node is not multiline enabled.
    if (Alignment <> taCenter) and not (vsMultiline in PaintInfo.Node.States) and (toShowStaticText in TreeOptions.StringOptions) and not lEventArgs.StaticText.IsEmpty then
      PaintStaticText(PaintInfo, lEventArgs.StaticTextAlignment, lEventArgs.StaticText);
  finally
    RestoreFontChangeEvent(PaintInfo.Canvas);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoShortenString(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const S: string; Width: TDimension; EllipsisWidth: TDimension = 0): string;

var
  Done: Boolean;

begin
  Done := False;
  if Assigned(FOnShortenString) then
    FOnShortenString(Self, Canvas, Node, Column, S, Width, Result, Done);
  if not Done then
    Result := ShortenString(Canvas.Handle, S, Width, EllipsisWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect;
  DrawFormat: Cardinal);

var
  DefaultDraw: Boolean;
  lText: string;
begin
  DefaultDraw := True;
  if Assigned(FOnDrawText) then
    FOnDrawText(Self, PaintInfo.Canvas, PaintInfo.Node, PaintInfo.Column, Text, CellRect, DefaultDraw);
  if ((DrawFormat and DT_RIGHT) > 0) and (TFontStyle.fsItalic in PaintInfo.Canvas.Font.Style) then
    lText := Text + ' '
  else
    lText := Text;
  if DefaultDraw then
    Winapi.Windows.DrawTextW(PaintInfo.Canvas.Handle, PWideChar(lText), Length(lText), CellRect, DrawFormat);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoTextMeasuring(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string): TSize;

var
  R: TRect;
  DrawFormat: Integer;

begin
  GetTextExtentPoint32W(Canvas.Handle, PWideChar(Text), Length(Text), Result);
  if vsMultiLine in Node.States then
  begin
    DrawFormat := DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL or AlignmentToDrawFlag[Alignment];
    if BidiMode <> bdLeftToRight then
      DrawFormat := DrawFormat or DT_RTLREADING;

    R := Rect(0, 0, Result.cx, MaxInt);
    Winapi.Windows.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, DrawFormat);
    Result.cx := R.Right - R.Left;
  end;
  if Assigned(FOnMeasureTextWidth) then
    FOnMeasureTextWidth(Self, Canvas, Node, Column, Text, Result.cx);
  if Assigned(FOnMeasureTextHeight) then
    FOnMeasureTextHeight(Self, Canvas, Node, Column, Text, Result.cy);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.GetOptionsClass: TTreeOptionsClass;

begin
  Result := TCustomStringTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.InternalData(Node: PVirtualNode): Pointer;

begin
  if (Node = nil) or (FInternalDataOffset = 0) then
    Result := nil
  else if Node = RootNode then
    Result := PByte(Node) + FInternalDataOffset
  else
    Result := PByte(Node) + Self.NodeDataSize + FInternalDataOffset;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.MainColumnChanged;

var
  Run: PVirtualNode;
  Data: PInteger;

begin
  inherited;

  // Have to reset all node widths.
  Run := RootNode.FirstChild;
  while Assigned(Run) do
  begin
    Data := InternalData(Run);
    if Assigned(Data) then
      Data^ := 0;
    Run := GetNextNoInit(Run);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ReadChunk(Stream: TStream; Version: Integer; Node: PVirtualNode; ChunkType,
  ChunkSize: Integer): Boolean;

// read in the caption chunk if there is one

var
  NewText: string;

begin
  case ChunkType of
    CaptionChunk:
      begin
        NewText := '';
        if ChunkSize > 0 then
        begin
          SetLength(NewText, ChunkSize div 2);
          Stream.Read(PWideChar(NewText)^, ChunkSize);
        end;
        // Do a new text event regardless of the caption content to allow removing the default string.
        Text[Node, Header.MainColumn] := NewText;
        Result := True;
      end;
  else
    Result := inherited ReadChunk(Stream, Version, Node, ChunkType, ChunkSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TOldVTStringOption = (soSaveCaptions, soShowStaticText);

procedure TCustomVirtualStringTree.ReadOldStringOptions(Reader: TReader);

// Migration helper routine to silently convert forms containing the old tree options member into the new
// sub-options structure.

var
  OldOption: TOldVTStringOption;
  EnumName: string;

begin
  // If we are at design time currently then let the designer know we changed something.
  UpdateDesigner;

  // It should never happen at this place that there is something different than the old set.
  if Reader.ReadValue = vaSet then
    with TreeOptions do
    begin
      // Remove all default values set by the constructor.
      StringOptions := [];

      while True do
      begin
        // Sets are stored with their members as simple strings. Read them one by one and map them to the new option
        // in the correct sub-option set.
        EnumName := Reader.ReadStr;
        if EnumName = '' then
          Break;
        OldOption := TOldVTStringOption(GetEnumValue(TypeInfo(TOldVTStringOption), EnumName));
        case OldOption of
          soSaveCaptions:
            StringOptions := StringOptions + [toSaveCaptions];
          soShowStaticText:
            StringOptions := StringOptions + [toShowStaticText];
        end;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.RenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium;
  ForClipboard: Boolean): HResult;

// Returns string expressions of all currently selected nodes in the Medium structure.

begin
  Result := inherited RenderOLEData(FormatEtcIn, Medium, ForClipboard);
  if Failed(Result) then
  try
    if ForClipboard then
      Medium.hGlobal := ContentToClipboard(FormatEtcIn.cfFormat, tstCutCopySet)
    else
      Medium.hGlobal := ContentToClipboard(FormatEtcIn.cfFormat, tstSelected);

    // Fill rest of the Medium structure if rendering went fine.
    if Medium.hGlobal <> 0 then
    begin
      Medium.tymed := TYMED_HGLOBAL;
      Medium.unkForRelease := nil;

      Result := S_OK;
    end;
  except
    Result := E_FAIL;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.WriteChunks(Stream: TStream; Node: PVirtualNode);

// Adds another sibling chunk for Node storing the label if the node is initialized.
// Note: If the application stores a node's caption in the node's data member (which will be quite common) and needs to
//       store more node specific data then it should use the OnSaveNode event rather than the caption autosave function
//       (take out soSaveCaption from StringOptions). Otherwise the caption is unnecessarily stored twice.

var
  ChunkHeader: TChunkHeader;
  S: string;
  Len: Integer;

begin
  inherited;
  if (toSaveCaptions in TreeOptions.StringOptions) and (Node <> RootNode) and
    (vsInitialized in Node.States) then
    with Stream do
    begin
      // Read the node's caption (primary column only).
      S := Text[Node, Header.MainColumn];
      Len := 2 * Length(S);
      if Len > 0 then
      begin
        // Write a new sub chunk.
        ChunkHeader.ChunkType := CaptionChunk;
        ChunkHeader.ChunkSize := Len;
        Write(ChunkHeader, SizeOf(ChunkHeader));
        Write(PWideChar(S)^, Len);
      end;
    end;
end;

procedure TCustomVirtualStringTree.WriteText(Writer: TWriter);
begin
  Writer.WriteString(DefaultText);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ComputeNodeHeight(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
 S: string): TDimension;

// Default node height calculation for multi line nodes. This method can be used by the application to delegate the
// computation to the string tree.
// Canvas is used to compute that value by using its current font settings.
// Node and Column describe the cell to be used for the computation.
// S is the string for which the height must be computed. If this string is empty the cell text is used instead.

var
  DrawFormat: Cardinal;
  BidiMode: TBidiMode;
  Alignment: TAlignment;
  PaintInfo: TVTPaintInfo;
  Dummy: TColumnIndex;
  lOffsets: TVTOffsets;
begin
  if Length(S) = 0 then
    S := Text[Node, Column];

  if Column <= NoColumn then
  begin
    BidiMode := Self.BidiMode;
    Alignment := Self.Alignment;
  end
  else
  begin
    BidiMode := Header.Columns[Column].BidiMode;
    Alignment := Header.Columns[Column].Alignment;
  end;

  if BidiMode <> bdLeftToRight then
    ChangeBidiModeAlignment(Alignment);

  if vsMultiline in Node.States then
    DrawFormat := DT_NOPREFIX or DT_TOP or DT_WORDBREAK or DT_EDITCONTROL
  else
    DrawFormat := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE;
  DrawFormat := DrawFormat or DT_CALCRECT;

  // Allow for autospanning.
  PaintInfo.Node := Node;
  PaintInfo.BidiMode := BidiMode;
  PaintInfo.Column := Column;
  PaintInfo.CellRect := Rect(0, 0, 0, 0);
  GetOffsets(Node, lOffsets, TVTElement.ofsEndOfClientArea, Column);
  if Column > NoColumn then
  begin
    PaintInfo.CellRect.Right := Header.Columns[Column].Width - 2 * TextMargin;
    PaintInfo.CellRect.Left := lOffsets[TVTElement.ofsLabel];
  end
  else
    PaintInfo.CellRect.Right := ClientWidth;
  AdjustPaintCellRect(PaintInfo, Dummy);

  if BidiMode <> bdLeftToRight then
    DrawFormat := DrawFormat or DT_RIGHT or DT_RTLREADING
  else
    DrawFormat := DrawFormat or DT_LEFT;
  Winapi.Windows.DrawTextW(Canvas.Handle, PWideChar(S), Length(S), PaintInfo.CellRect, DrawFormat);
  Result := PaintInfo.CellRect.Bottom - PaintInfo.CellRect.Top;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToClipboard(Format: Word; Source: TVSTTextSourceType): HGLOBAL;

// This method constructs a shareable memory object filled with string data in the required format. Supported are:
// CF_TEXT - plain ANSI text (Unicode text is converted using the user's current locale)
// CF_UNICODETEXT - plain Unicode text
// CF_CSV - comma separated plain ANSI text
// CF_VRTF + CF_RTFNOOBS - rich text (plain ANSI)
// CF_HTML - HTML text encoded using UTF-8
//
// Result is the handle to a globally allocated memory block which can directly be used for clipboard and drag'n drop
// transfers. The caller is responsible for freeing the memory. If for some reason the content could not be rendered
// the Result is 0.

begin
  Result := VirtualTrees.Export.ContentToClipboard(Self, Format, Source);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToHTML(Source: TVSTTextSourceType; const Caption: string = ''): String;

// Renders the current tree content (depending on Source) as HTML text encoded in UTF-8.
// If Caption is not empty then it is used to create and fill the header for the table built here.
// Based on ideas and code from Frank van den Bergh and Andreas Hörstemeier.

begin
  Result := VirtualTrees.Export.ContentToHTML(Self, Source, Caption);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.CanExportNode(Node: PVirtualNode): Boolean;

begin
  case TreeOptions.ExportMode of
    emChecked:
      Result := CheckState[Node] = csCheckedNormal;
    emUnchecked:
      Result := CheckState[Node] = csUncheckedNormal;
    emVisibleDueToExpansion: //Do not export nodes that are not visible because their parent is not expanded
      Result := not Assigned(Node.Parent) or Self.Expanded[Node.Parent];
    emSelected: // export selected nodes only
      Result := Selected[Node];
    else
      Result := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.RemoveFromSelection(Node: PVirtualNode);
var
  lSelectedNodeCaption: string;
  lIndex: Integer;
begin
  inherited;
  if (toRestoreSelection in TreeOptions.SelectionOptions) and Assigned(FPreviouslySelected) and not Self.Selected[Node] then
  begin
    if Self.SelectedCount = 0 then
      FPreviouslySelected.Clear()
    else
    begin
      Self.OnGetText(Self, Node, Header.RestoreSelectionColumnIndex, ttNormal, lSelectedNodeCaption);
      if FPreviouslySelected.Find(lSelectedNodeCaption, lIndex) then
        FPreviouslySelected.Delete(lIndex);
    end;//else
  end;//if
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToRTF(Source: TVSTTextSourceType): RawByteString;

// Renders the current tree content (depending on Source) as RTF (rich text).
// Based on ideas and code from Frank van den Bergh and Andreas Hörstemeier.

begin
  Result := VirtualTrees.Export.ContentToRTF(Self, Source);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.ContentToCustom(Source: TVSTTextSourceType);

// Generic export procedure which polls the application at every stage of the export.

begin
  VirtualTrees.Export.ContentToCustom(Self, Source);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToText(Source: TVSTTextSourceType; Separator: Char): String;

begin
  Result := ContentToText(Source, string(Separator));
end;

//----------------------------------------------------------------------------------------------------------------------


function TCustomVirtualStringTree.ContentToUnicode(Source: TVSTTextSourceType; Separator: Char): string;

begin
  Result := Self.ContentToText(Source, string(Separator));
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToText(Source: TVSTTextSourceType; const Separator: string): string;

// Renders the current tree content (depending on Source) as Unicode text.
// If an entry contains the separator char then it is wrapped with double quotation marks.

begin
  Result := VirtualTrees.Export.ContentToUnicodeString(Self, Source, Separator);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.GetTextInfo(Node: PVirtualNode; Column: TColumnIndex; const AFont: TFont; var R: TRect;
  var Text: string);

// Returns the font, the text and its bounding rectangle to the caller. R is returned as the closest
// bounding rectangle around Text.

var
  NewHeight: TDimension;
  TM: TTextMetric;

begin
  // Get default font and initialize the other parameters.
  inherited GetTextInfo(Node, Column, AFont, R, Text);

  Canvas.Font.Assign(AFont);

  FFontChanged := False;
  RedirectFontChangeEvent(Canvas);
  DoPaintText(Node, Canvas, Column, ttNormal);
  if FFontChanged then
  begin
    AFont.Assign(Canvas.Font);
    GetTextMetrics(Canvas, TM);
    NewHeight := TM.tmHeight;
  end
  else // Otherwise the correct font is already there and we only need to set the correct height.
    NewHeight := FTextHeight;
  RestoreFontChangeEvent(Canvas);

  // Alignment to the actual text.
  Text := Self.Text[Node, Column];
  R := GetDisplayRect(Node, Column, True, not (vsMultiline in Node.States));
  if toShowHorzGridLines in TreeOptions.PaintOptions then
    Dec(R.Bottom);
  InflateRect(R, 0, -Divide(R.Bottom - R.Top - NewHeight, 2));
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.InvalidateNode(Node: PVirtualNode): TRect;

var
  Data: PInteger;

begin
  Result := inherited InvalidateNode(Node);
  // Reset node width so changed text attributes are applied correctly.
  Data := InternalData(Node);
  if Assigned(Data) then
    Data^ := 0;
end;

function TCustomVirtualStringTree.IsDefaultTextStored: Boolean;
begin
  Exit(DefaultText <> cDefaultText);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.Path(Node: PVirtualNode; Column: TColumnIndex; Delimiter: Char): string;

// Constructs a string containing the node and all its parents. The last character in the returned path is always the
// given delimiter.

begin
  if (Node = nil) or (Node = RootNode) then
    Result := Delimiter
  else
  begin
    Result := '';
    while Node <> RootNode do
    begin
      Result := Text[Node, Column] + Delimiter + Result;
      Node := Node.Parent;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.ResetInternalData(Node: PVirtualNode; Recursive: Boolean);

var
  Data: PInteger;
  Run: PVirtualNode;

begin
  // Reset node width so changed text attributes are applied correctly.
  if Assigned(Node) and (Node <> RootNode) then
  begin
    Data := InternalData(Node);
    if Assigned(Data) then
      Data^ := 0;
  end;

  if Recursive then
  begin
    if Assigned(Node) then
      Run := Node.FirstChild
    else
      Run := RootNode.FirstChild;

    while Assigned(Run) do
    begin
      ResetInternalData(Run, Recursive);
      Run := Run.NextSibling;
    end;
  end;//if Recursive
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.ReinitNode(Node: PVirtualNode; Recursive: Boolean; ForceReinit: Boolean = False);

begin
  inherited;

  ResetInternalData(Node, False);  // False because we are already in a loop inside ReinitNode
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.SetChildCount(Node: PVirtualNode; NewChildCount: Cardinal);

begin
  inherited;
  ResetInternalData(Node, False);
end;

//----------------- TVirtualStringTree ---------------------------------------------------------------------------------


function TVirtualStringTree.GetOptions: TStringTreeOptions;

begin
  Result := inherited TreeOptions as TStringTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualStringTree.SetOptions(const Value: TStringTreeOptions);

begin
  inherited TreeOptions.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualStringTree.GetOptionsClass: TTreeOptionsClass;

begin
  Result := TStringTreeOptions;
end;

{ TVSTGetCellTextEventArgs }

//----------------------------------------------------------------------------------------------------------------------

constructor TVSTGetCellTextEventArgs.Create(pNode: PVirtualNode; pColumn: TColumnIndex; pExportType: TVTExportType);
begin
  Self.Node := pNode;
  Self.Column := pColumn;
  Self.ExportType := pExportType;
end;

initialization
  TCustomStyleEngine.RegisterStyleHook(TVirtualStringTree, TVclStyleScrollBarsHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TVirtualStringTree, TVclStyleScrollBarsHook);

end.
