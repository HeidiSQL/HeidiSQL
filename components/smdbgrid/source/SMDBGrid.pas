{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}

{ Copyright (C) 1998-2007, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
  WEB: http://www.scalabium.com

English:
  The successor TDBGrid with the extended features.
  Is able to display multiline wordwrap column titles,
  checkboxs for boolean fields, checkboxes for record selecting,
  fixed columns, row numeration, the convenient select of records
  from the keyboard, stretch drawing of the graphic fields in the cells,
  possibility to exclude insert and delete of records in the DBGrid,
  own standard PopupMenu, save/restore of a column states, processing of
  additional events etc.

  1. movement from column to column by ENTER key (like TAB)
  2. multiline wordwrap column titles (original idea found
     in TBitDBGrid - Ilya Andreev, ilya_andreev@geocities.com
     FIDONet: 2:5030/55.28 AKA 2:5030/402.17)
  3. display opportunity of selected record mark (like checkbox)
  4. editing of boolean fields like checkbox
  5. a convenient select of records from keyboard (is derived from TRXDBGrid, RXLibrary)
  6. an opportunity to exclude insert and delete of records in the SMDBGrid
  7. save and restore of the column order and column width in the INI-file
  8. own PopUp-menu with standard items (Add/Edit/Delete record, Print/Export
     data, Save/Cancel changes, Refresh data, Select/UnSelect records,
     Save/Restore layout)
  9. fixing of the few columns in horizontal scrolling
 10. delete of the all selected records
 11. Refresh of the data in SMDBGrid (useful for TQuery because Refresh
     correctly works only for TTable)
 12. processing of events by pressing on column title (is derived
     from TRXDBGrid, RXLibrary)
 13. ability of display of the MEMO/BLOB/PICTURE-fields as Bitmap (is
     derived from TRXDBGrid, RXLibrary)
 14. display hints for each cells if cell text is cutted by column width
     (original idea found in TBitDBGrid - Ilya Andreev, ilya_andreev@geocities.com
     FIDONet: 2:5030/55.28 AKA 2:5030/402.17)
 15. opportunity to assign of events: OnAppendRecord, OnEditRecord,
     OnDeleteRecord, OnPrintData, OnExportData
 16. lowered draw of the current selected column (like grid in
     1C-accounting)
 17. standard Popup menu like window system menu:
       "Add record",
       "Insert record",
       "Edit record",
       "Delete record",
       "-",
       "Print ...",
       "Export ...",
       "-",
       "Save changes",
       "Cancel changes",
       "Refresh data",
       "-",
       "Select/Unselect records",
       "-",
       "Save layout",
       "Restore layout",
       "-",
       "Setup..."
  18. different styles (for example, like PriceList: odd/even row color)
  19. different inplace editors:
       - Date Edit
       - Calculator Edit
       - Spin Edit
       - Incremental Lookup Combo Box
       - Pick List Combo Box
       - BLOB Edit
       - Memo Edit
       - Hyperlink Edit Image Pick List Combo Box
       - Button
       - Checkbox
       - Graphics
       - ImageList
       - ...
  20. footers for every column (automatical calculations will be added later)
  21. bands for columns
  22. Windows XP themes support
  23. autofilter row (implemented by Heiner Morales Morera <moralesmh@ucimed.com>)

PS: in archive there are translated resources (file SMCnst.PAS).
If anybody want to send new native resources, then I'll include in the next build with your copyright.

if you have some bug report and/or suggestions, contact me.
}

unit SMDBGrid;

interface

//{$DEFINE USE_BDE}
//{$DEFINE VER_ENTERPRISE}
//{$DEFINE VER_KBMMEMTABLE} {added by G.J. Doornink" <dkg@xs4all.nl>}
//{$DEFINE FLATSCROLLBARS}

{$I compilers.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Grids, DBGrids, DB, StdCtrls, SMCnst,
  RXUtils {ex-VCLUtils from RX-Lib};

type
  TExOptions = set of (eoAutoWidth, eoBooleanAsCheckBox,
                       eoCheckBoxSelect, eoCellHint,
                       eoDisableDelete, eoDisableInsert, eoDrawGraphicField,
                       eoENTERlikeTAB, eoFixedLikeColumn,
                       eoKeepSelection, eoLayout,
                       eoRowNumber, eoRowSizing,
                       eoSelectedTitle, eoShowFooter, eoShowGlyphs, eoShowLookup, eoShowRecNo, eoStandardPopup,
                       eoTitleButtons, eoAutoSave, eoAutoLoad,
                       eoDrawBands, eoHotTrack, eoBLOBEditor, eoBandsOverTitles,
                       eoRowHeightAutofit, eoTitleWordWrap,
                       eoShowFilterBar, eoAnyKeyFilter, eoFilterAutoApply);

  {start cutting from TRxDBGrid}
  TCheckTitleBtnEvent = procedure (Sender: TObject; ACol: Longint; Field: TField; var Enabled: Boolean) of object;
  TGetCellParamsEvent = procedure (Sender: TObject; Field: TField; AFont: TFont; var Background: TColor; Highlight: Boolean) of object;
  TGetBtnParamsEvent = procedure (Sender: TObject; Field: TField; AFont: TFont; var Background: TColor; IsDown: Boolean) of object;
  {end cutting from TRxDBGrid}

  TGetGlyphEvent = procedure (Sender: TObject; var Bitmap: TBitmap) of object;
  TGetCellHint = procedure (Sender: TObject; Column: TColumn; var HintStr: string;
                           var HintInfo: THintInfo) of object;

  TGetRecordCountEvent = procedure (Sender: TObject; var RecordCount: Integer) of object;
  TSMSortChangedEvent = procedure (Sender: TObject; Column: TColumn) of object;

type
  TSMSortType = (stNone, stAscending, stDescending);
  TSMInplaceEditorType = (ieStandard, ieImageList, ieProgressbar, iePassword, ieHyperlink, ieButton, ieCheckbox);

  TSMDBGrid = class;

  TSMFooterType = (ftCustom, ftSum, ftCount, ftAverage, ftMin, ftMax);
  TSMTextEllipsis = (teNone, teEnd, teMiddle);

  TSMDBColumn = class(TColumn)
  private
    FBandIndex: Integer;

    FSortCaption: string;
    FSortType: TSMSortType;
    FInplaceEditor: TSMInplaceEditorType;

    FFooterValue: Variant;
    FFooterType: TSMFooterType;
    FFooterText: string;

    FTag: Longint;
    FTextEllipsis: TSMTextEllipsis;

    FFilterValue: string;
    FFilterList: TStrings;

    procedure SetFooterValue(Value: Variant);
    procedure SetFooterType(Value: TSMFooterType);
    procedure SetSortCaption(Value: string);
    procedure SetSortType(Value: TSMSortType);
    procedure SetInplaceEditor(Value: TSMInplaceEditorType);
    procedure SetFilterValue(const Value: string);
    function GetFilterList: TStrings;
    procedure SetFilterList(const Value: TStrings);
  protected
    function GetGrid: TSMDBGrid;
  public
    intInternalCount: Integer; {required for Avg calculation}

    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;

    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean); virtual;

    property Grid: TSMDBGrid read GetGrid;
  published
    property BandIndex: Integer read FBandIndex write FBandIndex default -1;

    property SortCaption: string read FSortCaption write SetSortCaption;
    property SortType: TSMSortType read FSortType write SetSortType default stNone;
    property InplaceEditor: TSMInplaceEditorType read FInplaceEditor write SetInplaceEditor default ieStandard;

    property FooterValue: Variant read FFooterValue write SetFooterValue;
    property FooterType: TSMFooterType read FFooterType write SetFooterType default ftCustom;
    property FooterText: string read FFooterText write FFooterText;

    property Tag: Longint read FTag write FTag default 0;
    property TextEllipsis: TSMTextEllipsis read FTextEllipsis write FTextEllipsis default teNone;

    property FilterValue: string read FFilterValue write SetFilterValue;
    property FilterList: TStrings read GetFilterList write SetFilterList;
  end;

  TSMDBImagesColumn = class(TSMDBColumn)
  private
    FImages: TImageList;

    procedure SetImages(Value: TImageList);
  protected
  public
    constructor Create(Collection: TCollection); override;
    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean); override;
  published
    property Images: TImageList read FImages write SetImages;
  end;

  TSMDBProgressColumn = class(TSMDBColumn)
  private
    FMin: Integer;
    FMax: Integer;
    FColor: TColor;
  protected
  public
    constructor Create(Collection: TCollection); override;
    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean); override;
  published
    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;

    property Color: TColor read FColor write FColor;
  end;

  TSMDBPasswordColumn = class(TSMDBColumn)
  private
    FPasswordChar: Char;
  protected
  public
    constructor Create(Collection: TCollection); override;
    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean); override;
  published
    property PasswordChar: Char read FPasswordChar write FPasswordChar default '*';
  end;

  TSMDBHyperlinkColumn = class(TSMDBColumn)
  private
  protected
  public
    constructor Create(Collection: TCollection); override;
    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean); override;
  published
  end;

  TSMDBButtonColumn = class(TSMDBColumn)
  private
  protected
  public
    constructor Create(Collection: TCollection); override;
    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean); override;
  published
  end;

  TSMDBCheckboxColumn = class(TSMDBColumn)
  private
    FValueChecked: string;
    FValueUnChecked: string;
  protected
    procedure SetValueChecked(const AValue: string);
    procedure SetValueUnChecked(const AValue: string);
  public
    constructor Create(Collection: TCollection); override;
    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean); override;
  published
    property ValueChecked: string read FValueChecked write FValueChecked;
    property ValueUnChecked: string read FValueUnChecked write FValueUnChecked;
  end;

  TSMDBGridColumns = class(TDBGridColumns)
  private
    function GetColumn(Index: Integer): TSMDBColumn;
    procedure SetColumn(Index: Integer; Value: TSMDBColumn);
  protected
  public
    function Add: TSMDBColumn;
    property Items[Index: Integer]: TSMDBColumn read GetColumn write SetColumn; default;
  end;


  TSMGradientDraw = class(TPersistent)
  private
    FGrid: TSMDBGrid;
    FDirection: TSMGradientDirection;
    FStartColor: TColor;
    FEndColor: TColor;

    procedure SetDirection(Value: TSMGradientDirection);
    procedure SetStartColor(AValue: TColor);
    procedure SetEndColor(AValue: TColor);
  public
    constructor Create(AGrid: TSMDBGrid); virtual;
  published
    property Direction: TSMGradientDirection read FDirection write SetDirection default fdNone;
    property StartColor: TColor read FStartColor write SetStartColor default clWhite;
    property EndColor: TColor read FEndColor write SetEndColor default $00DDDDDD;
  end;

  TSMGridStyle = (gsNormal, gsCustom, gsPriceList, gsMSMoney, gsBrick, gsDesert, gsEggplant, gsLilac, gsMaple, gsMarine, gsRose, gsSpruce, gsWheat, gsSoftWheat, gsSoftRose, gsAquaBlue, gsSoftMaple, gsSoftLilac, gsSoftDesert, gsSoftEggPlant, gsSoftBrick, gsSoftSpruce, gsSoftYellowGreen, gsSoftGray);
  TSMDBGridStyle = class(TPersistent)
  private
    FGrid: TSMDBGrid;
    FStyle: TSMGridStyle;
    FOddColor: TColor;
    FEvenColor: TColor;
    FFilterColor: TColor;

    FBackground: TBitmap;

    {custom gradient draw styles}
    FBands: TSMGradientDraw;
    FTitle: TSMGradientDraw;
    FFooter: TSMGradientDraw;
    FWallpaper: TSMGradientDraw;
    FGrouping: TSMGradientDraw;

    procedure SetBackground(Value: TBitmap);
    procedure SetStyle(Value: TSMGridStyle);
    procedure SetOddColor(AValue: TColor);
    procedure SetEvenColor(AValue: TColor);
    procedure SetFilterColor(const Value: TColor);

    function GetGradient(Index: Integer): TSMGradientDraw;
    procedure SetGradient(Index: Integer; Value: TSMGradientDraw);
  public
    constructor Create(AGrid: TSMDBGrid); virtual;
    destructor Destroy; override;
  published
    property Style: TSMGridStyle read FStyle write SetStyle;
    property OddColor: TColor read FOddColor write SetOddColor;
    property EvenColor: TColor read FEvenColor write SetEvenColor;
    property Background: TBitmap read FBackground write SetBackground;

    property Bands: TSMGradientDraw index 0 read GetGradient write SetGradient;
    property Title: TSMGradientDraw index 1 read GetGradient write SetGradient;
    property Footer: TSMGradientDraw index 2 read GetGradient write SetGradient;
    property Wallpaper: TSMGradientDraw index 3 read GetGradient write SetGradient;
    property Grouping: TSMGradientDraw index 4 read GetGradient write SetGradient;

    property FilterColor: TColor read FFilterColor write SetFilterColor default clInfoBk;
  end;


  {Grouping collection definition}
  TSMGrouping = class(TCollectionItem)
  private
    { Private declarations }
    FExpression: string;
    FFont: TFont;
    FColor: TColor;

    procedure SetFont(Value: TFont);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Expression: string read FExpression write FExpression;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write FColor;
  end;

  TSMGroupings = class(TCollection)
  private
    FGrid: TSMDBGrid;

    function GetGrouping(Index: Integer): TSMGrouping;
    procedure SetGrouping(Index: Integer; Value: TSMGrouping);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AGrid: TSMDBGrid);
    function Add: TSMGrouping;

    property Grid: TSMDBGrid read FGrid;
    property Items[Index: Integer]: TSMGrouping read GetGrouping write SetGrouping; default;
  end;

  TDrawFooterCellEvent = procedure(Sender: TObject; Canvas: TCanvas;
                                   FooterCellRect: TRect; Field: TField;
                                   var FooterText: string; var DefaultDrawing: Boolean) of object;

  TGetParsedExpression = procedure(Sender: TObject; Expression: string; var Text: string; var Value: Boolean) of object;
  TDrawGroupingCellEvent = procedure(Sender: TObject; ACanvas: TCanvas; CellRect: TRect;
                                     Group: TSMGrouping; Text: string; var DefaultDrawing: Boolean) of object;

  TSMDBGTitleHeightKind = (hkAuto, hkLineCount, hkPixelCount);
  TSMDBGTitleHeight = class(TPersistent)
  private
    { Private declarations }
    FGrid: TSMDBGrid;

    FKind: TSMDBGTitleHeightKind;
    FLineCount: Integer;
    FPixelCount: Integer;

    procedure SetKind(Value: TSMDBGTitleHeightKind);
    procedure SetLineCount(Value: Integer);
    procedure SetPixelCount(Value: Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AGrid: TSMDBGrid); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Kind: TSMDBGTitleHeightKind read FKind write SetKind default hkAuto;
    property LineCount: Integer read FLineCount write SetLineCount default 1;
    property PixelCount: Integer read FPixelCount write SetPixelCount default 0;
  end;

  TFilterComboBox = class(TComboBox)
  public
    Column: TSMDBColumn;
  end;

  TSMDBGrid = class(TDBGrid)
  private
    { Private declarations }
    FBands: TStrings;
    FBandsFont: TFont;

    FFlat: Boolean;
    FExOptions: TExOptions;
    FGridStyle: TSMDBGridStyle;
    FTitleHeight: TSMDBGTitleHeight;

    FFooterColor: TColor;
    FFooterHeight: Integer;
    FOnUpdateFooter: TNotifyEvent;
    FOnDrawFooterCell: TDrawFooterCellEvent;
    FOnCellHint: TGetCellHint;
    FOnExpression: TGetParsedExpression;
    FOnDrawGroupingCell: TDrawGroupingCellEvent;

    FOnGetRecordCount: TGetRecordCountEvent;
    FOnFilterChanged: TNotifyEvent;

    FOnSortChanged: TSMSortChangedEvent;

    FHintField: string;

    {$IFDEF DELPHI_4_UP}
    FImages: TImageList;
    {$ENDIF}

    {selection: from TRxDBGrid}
    FMultiSelect: Boolean;
    FSelecting: Boolean;
    FMsIndicators: TImageList;
    FSelectionAnchor: TBookmarkStr;
    FDisableCount: Integer;
    FFixedCols: Integer;
    FSwapButtons: Boolean;
    FOnCheckButton: TCheckTitleBtnEvent;
    FTracking: Boolean;
    FPressedCol: Longint;
    FPressed: Boolean;
    FOnGetCellParams: TGetCellParamsEvent;
    FOnGetBtnParams: TGetBtnParamsEvent;

    {Registry}
    FRegistryKey: string;
    FRegistrySection: string;

    {popup menu with standard operations}
    FDBPopUpMenu: TPopUpMenu;
    FOnAppendRecord: TNotifyEvent;
    FOnInsertRecord: TNotifyEvent;
    FOnEditRecord: TNotifyEvent;
    FOnDeleteRecord: TNotifyEvent;
    FOnPostData: TNotifyEvent;
    FOnCancelData: TNotifyEvent;
    FOnRefreshData: TNotifyEvent;
    FOnPrintData: TNotifyEvent;
    FOnExportData: TNotifyEvent;
    FOnSetupGrid: TNotifyEvent;
    FOnFilterData: TNotifyEvent;
    FOnSearchData: TNotifyEvent;

    FOnChangeSelection: TNotifyEvent;
    FOnEditChange: TNotifyEvent;
    FOnTopLeftChanged: TNotifyEvent;

    FOnDrawColumnTitle: TDrawColumnCellEvent;
    FOnGetGlyph: TGetGlyphEvent;
    FWidthOfIndicator: Integer;

    FAutoFitIsLocked: Boolean;

    StartOfSelect: TBookmark;

    FGroupings: TSMGroupings;

    FScrollHintWnd: THintWindow;
    FLastRect: TRect;
    FLastRow, FLastCol: Integer;
    FLastXPDrawn: Boolean;

    HScrollbar: Boolean;
    VScrollbar: Boolean;

    CmbFilter: TFilterComboBox;

    FOnColWidthsChanged: TNotifyEvent;
    FOnDrawBackground: TNotifyEvent;

    FIsFlagAlwaysShowEditor: Boolean;
    
    function GetScrollBars: TScrollStyle;
    procedure SetScrollBars(Value: TScrollStyle);

    function GetDefaultRowHeight: Integer;
    procedure SetDefaultRowHeight(Value: Integer);
    procedure SetRowHeight;

    procedure SetBandsFont(Value: TFont);
    procedure SetBands(Value: TStrings);
    function GetBandRect(ACol: Integer): TRect;

    procedure FreeStartOfSelect;

    procedure SetGroupings(Value: TSMGroupings);

    procedure SetGridStyle(Value: TSMDBGridStyle);
    procedure SetTitleHeight(Value: TSMDBGTitleHeight);
    procedure SetFooterColor(Value: TColor);
    procedure SetFooterHeight(Value: Integer);

    function ColumnIsCheckbox(AColumn: TColumn): Boolean;

    {events for BLOB-field's (memo and graphics) inplace editor}
    procedure frmBlobEditResize(Sender: TObject);
    procedure LoadImage(Sender: TObject);
    procedure SaveImage(Sender: TObject);
    procedure ImageToClipboard(Sender: TObject);
    procedure ClipboardToImage(Sender: TObject);
    procedure ClearImage(Sender: TObject);

    procedure SetIndicatorWidth(Value: Integer);

    procedure AppendClick(Sender: TObject);
    procedure InsertClick(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure PrintClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure PostClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure RefreshClick(Sender: TObject);
    procedure SetupGridClick(Sender: TObject);
    procedure SearchClick(Sender: TObject);
    procedure FilterClick(Sender: TObject);

    {start cutting from TRxDBGrid}
    procedure SetFixedCols(Value: Integer);
    function GetFixedCols: Integer;
    function GetTitleOffset: Byte;
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    function AcquireFocus: Boolean;
    function ActiveRowSelected: Boolean;
    function GetOptions: TDBGridOptions;
    procedure SetOptions(Value: TDBGridOptions);
    {end cutting from TRxDBGrid}

    function GetImageIndex(Field: TField): Integer;
    procedure SetExOptions(Val: TExOptions);

    {original idea foundTBitDBGrid:
    Ilya Andreev, ilya_andreev@geocities.com
    FIDONet: 2:5030/55.28 AKA 2:5030/402.17}
    procedure SetTitlesHeight;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    {end of derived}

    function GetSortImageWidth: Integer;

    procedure WMNCCalcSize(var msg: TMessage); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPaint;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    procedure SetFlat(Value: Boolean);
    function IsMouseInRect(ARect: TRect): Boolean;

    procedure CmbFilterChange(Sender: TObject);
    procedure CmbFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CmbFilterExit(Sender: TObject);
    procedure CmbFilterClick(Sender: TObject);

    function UpperAccentCase(const S: string): string;

    {$IFDEF DELPHI_5_UP}
    function GetColumns: TSMDBGridColumns;
    procedure SetColumns(Value: TSMDBGridColumns);
    {$ENDIF}
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure DblClick; override;
    procedure Loaded; override;

    procedure DrawGridBackground; virtual;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CreateColumns: TDBGridColumns; override;
    function CreateEditor: TInplaceEdit; override;
    procedure EditChanged(Sender: TObject); dynamic;
    procedure TitleClick(Column: TColumn); override;

    {AutoFit of column width support}
    procedure UpdateColWidths1; virtual;
    procedure UpdateColWidths; virtual;

    function CalcTitleRect1(Col: TColumn; ARow: Integer; var MasterCol: TColumn): TRect;

    {for footer support}
    function GetClientRect: TRect; override;
    procedure DoUpdateFooter; virtual;
    function GetFooterRect: TRect; virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure LinkActive(Value: Boolean); override;

    {start cutting from TRxDBGrid}
    function HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TGridDrawState): Boolean; override;
{$IFDEF DELPHI_4_UP}
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
{$ENDIF}
    procedure Scroll(Distance: Integer); override;

    procedure LayoutChanged; override;
    procedure ColWidthsChanged; override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
    procedure RowHeightsChanged; override;
    procedure SetColumnAttributes; override;
    procedure TopLeftChanged; override;
    function CanEditShow: Boolean; override;

    procedure CheckTitleButton(ACol: Longint; var Enabled: Boolean); dynamic;
    procedure GetCellProps(ACol: Integer; Field: TField; AFont: TFont; var Background: TColor;
      Highlight: Boolean); dynamic;
    {end cutting from TRxDBGrid}

    procedure CellClick(Column: TColumn); override;
    function CellRectForDraw(R: TRect; ACol: Longint): TRect;

    function DrawGroupings(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState): Boolean;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    function GetGlyph: TBitmap; virtual;
    procedure DrawComboArrow(R: TRect);
    procedure DrawCheckBox(R: TRect; AState: TCheckBoxState; al: TAlignment); virtual;
    procedure DrawGraphic(R: TRect; Field: TField); virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DefDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMHScroll(var Message: TWMHSCroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVSCroll); message WM_VSCROLL;

    procedure OnFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    function GetRecordCount: Integer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteData;
    procedure RefreshData;

    procedure InvalidateFooter;
    procedure CalculateTotals(OnlySelected: Boolean {$IFDEF DELPHI_5_UP}=False{$ENDIF});

    procedure SelectOneClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure UnSelectOneClick(Sender: TObject);
    procedure UnSelectAllClick(Sender: TObject);

    procedure SaveLayoutClick(Sender: TObject);
    procedure RestoreLayoutClick(Sender: TObject);

    procedure ToggleRowSelection;
    procedure GotoSelection(Index: Longint);

    procedure DisableScroll;
    procedure EnableScroll;
    function ScrollDisabled: Boolean;

    procedure ShowBlobEditor;

    procedure ClearSort;
    procedure SetSortField(AField: TField; ASortType: TSMSortType);

    procedure ClearFilter;
    procedure ApplyFilter;

    property IndicatorOffset;
    property TitleOffset: Byte read GetTitleOffset;
    property DBPopUpMenu: TPopUpMenu read FDBPopUpMenu write FDBPopUpMenu;
    {$IFDEF DELPHI_5_UP}
    property Columns: TSMDBGridColumns read GetColumns write SetColumns;
    {$ENDIF}

    property Col;
    property Row;

  published
    { Published declarations }
    property Flat: Boolean read FFlat write SetFlat;
    property Bands: TStrings read FBands write SetBands;
    property BandsFont: TFont read FBandsFont write SetBandsFont;
    property Groupings: TSMGroupings read FGroupings write SetGroupings;

    property GridStyle: TSMDBGridStyle read FGridStyle write SetGridStyle;
    property TitleHeight: TSMDBGTitleHeight read FTitleHeight write SetTitleHeight;
    property FooterColor: TColor read FFooterColor write SetFooterColor;
    property FooterHeight: Integer read FFooterHeight write SetFooterHeight default 0;

    property GridLineWidth;
    property ExOptions: TExOptions read FExOptions write SetExOptions;
    property HintField: string read FHintField write FHintField;

    {$IFDEF DELPHI_4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;

    property Images: TImageList read FImages write FImages;
    property ParentBiDiMode;

    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}

    {selection}
    property Options: TDBGridOptions read GetOptions write SetOptions;
    property FixedCols: Integer read GetFixedCols write SetFixedCols default 0;
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;

    {Registry}
    property RegistryKey: string read FRegistryKey write FRegistryKey;
    property RegistrySection: string read FRegistrySection write FRegistrySection;

    property OnAppendRecord: TNotifyEvent read FOnAppendRecord write FOnAppendRecord;
    property OnInsertRecord: TNotifyEvent read FOnInsertRecord write FOnInsertRecord;
    property OnEditRecord: TNotifyEvent read FOnEditRecord write FOnEditRecord;
    property OnDeleteRecord: TNotifyEvent read FOnDeleteRecord write FOnDeleteRecord;
    property OnPostData: TNotifyEvent read FOnPostData write FOnPostData;
    property OnCancelData: TNotifyEvent read FOnCancelData write FOnCancelData;
    property OnRefreshData: TNotifyEvent read FOnRefreshData write FOnRefreshData;
    property OnPrintData: TNotifyEvent read FOnPrintData write FOnPrintData;
    property OnExportData: TNotifyEvent read FOnExportData write FOnExportData;
    property OnFilterData: TNotifyEvent read FOnFilterData write FOnFilterData;
    property OnSearchData: TNotifyEvent read FOnSearchData write FOnSearchData;
    property OnCheckButton: TCheckTitleBtnEvent read FOnCheckButton write FOnCheckButton;

    property OnCellHint: TGetCellHint read FOnCellHint write FOnCellHint;
    property OnExpression: TGetParsedExpression read FOnExpression write FOnExpression;
    property OnDrawGroupingCell: TDrawGroupingCellEvent read FOnDrawGroupingCell write FOnDrawGroupingCell;
    property OnGetRecordCount: TGetRecordCountEvent read FOnGetRecordCount write FOnGetRecordCount;

    property OnEditChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;

    property OnChangeSelection: TNotifyEvent read FOnChangeSelection write FOnChangeSelection;
    property OnSetupGrid: TNotifyEvent read FOnSetupGrid write FOnSetupGrid;
    property OnDrawColumnTitle: TDrawColumnCellEvent read FOnDrawColumnTitle write FOnDrawColumnTitle;
    property OnGetGlyph: TGetGlyphEvent read FOnGetGlyph write FOnGetGlyph;
    property WidthOfIndicator: Integer read FWidthOfIndicator write SetIndicatorWidth;

    property OnDrawFooterCell: TDrawFooterCellEvent read FOnDrawFooterCell write FOnDrawFooterCell;
    property OnUpdateFooter: TNotifyEvent read FOnUpdateFooter write FOnUpdateFooter;
    property OnFilterChanged: TNotifyEvent read FOnFilterChanged write FOnFilterChanged;

    property OnSortChanged: TSMSortChangedEvent read FOnSortChanged write FOnSortChanged;

    property OnColWidthsChanged: TNotifyEvent read FOnColWidthsChanged write FOnColWidthsChanged;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;

    property DefaultRowHeight: Integer read GetDefaultRowHeight write SetDefaultRowHeight;

    property ScrollBars read GetScrollBars write SetScrollBars{ default ssBoth};
    property ColCount;
    property RowCount;
    property VisibleColCount;
    property VisibleRowCount;

    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
  end;

procedure SaveGridToRegistry(grid: TDBGrid; const RegistryKey, RegistrySection: string);
procedure LoadGridFromRegistry(grid: TDBGrid; const RegistryKey, RegistrySection: string);

procedure SaveGridToIni(grid: TDBGrid; const FileName: string; const SectionName: string {$IFDEF DELPHI_5_UP} = '' {$ENDIF} );
procedure LoadGridFromIni(grid: TDBGrid; const FileName: string; const SectionName: string {$IFDEF DELPHI_5_UP} = '' {$ENDIF} );

{$IFDEF FLATSCROLLBARS}
function FlatSB_SetScrollPos(hWnd: HWND; nBar, nPos: Integer; bRedraw: BOOL): Integer; stdcall;
function FlatSB_SetScrollInfo(hWnd: HWND; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
function FlatSB_SetScrollProp(p1: HWND; index: Integer; newValue: Integer; p4: Bool): Bool; stdcall;
function InitializeFlatSB(hWnd: HWND): Bool; stdcall;
procedure UninitializeFlatSB(hWnd: HWND); stdcall;
{$ENDIF}

procedure Register;

implementation
uses Registry, IniFiles,
     ExtCtrls, Clipbrd, ExtDlgs, TypInfo
     {$IFDEF USE_BDE}, DBTables {$ENDIF}
     {$IFDEF VER_ENTERPRISE}, DBClient {$ENDIF}
     {$IFDEF VER_KBMMEMTABLE}, kbmMemTable {$ENDIF}
     {$IFDEF DELPHI_6_UP} , Variants {$ENDIF}
     ;

{$R *.RES}
var
  FCheckWidth, FCheckHeight: Integer;
  frmBlobEdit: TForm;

const
  FILTERBAR_HEIGHT = 19;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBGrid]);
end;

type
  HTHEME = THandle;

var
  IsThemeActive: function: BOOL stdcall;
  IsAppThemed: function: BOOL stdcall;

  OpenThemeData: function(hWnd: THandle; pszClassList: LPCWSTR): HTHEME stdcall;
  CloseThemeData: function(hTheme: HTHEME): HResult stdcall;
  DrawThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId: Integer; iStateId: Integer; const pRect: PRect; const pClipRect: PRect): HResult stdcall;

function IsXPThemesEnabled: Boolean; forward;

{$IFDEF FLATSCROLLBARS}
function FlatSB_SetScrollPos; external 'comctl32.dll' name 'FlatSB_SetScrollPos';
function FlatSB_SetScrollInfo; external 'comctl32.dll' name 'FlatSB_SetScrollInfo';
function FlatSB_SetScrollProp; external 'comctl32.dll' name 'FlatSB_SetScrollProp';
function InitializeFlatSB; external 'comctl32.dll' name 'InitializeFlatSB';
procedure UninitializeFlatSB; external 'comctl32.dll' name 'UninitializeFlatSB';
{$ENDIF}

procedure ListIntersect(OldList, NewList: TStrings);
var
  i: Integer;
begin
  i := OldList.Count-1;
  while i > -1 do
  begin
    if NewList.IndexOf(OldList[i]) < 0 then
      OldList.Delete(i);
    Dec(i);
  end;
end;

procedure ListDifference(OldList, NewList: TStrings);
var
  i: Integer;
begin
  i := OldList.Count-1;
  while i > -1 do
  begin
    if NewList.IndexOf(OldList[i]) >= 0 then
      OldList.Delete(i);
    Dec(i);
  end;
end;

function GetIndexDefs(ds: TDataSet): TIndexDefs;
var
  i: Integer;
  PropInfoIndices: PPropInfo;
begin
  Result := nil;

  PropInfoIndices := GetPropInfo(ds.ClassInfo, 'IndexDefs');
  {if such property exists}
  if Assigned(PropInfoIndices) and
    (PropInfoIndices.PropType^.Kind = tkClass) then
  begin
    i := GetOrdProp(ds, PropInfoIndices);
    if TObject(i) is TIndexDefs then
      Result := TIndexDefs(i)
  end;
end;

function GetIndexDef(IndexDefs: TIndexDefs; const sName: string): TIndexDef;
var
  i: Integer;
begin
  Result := nil;

  if Assigned(IndexDefs) then
  begin
    i := IndexDefs.IndexOf(sName);
    if i <> -1 then
      Result := IndexDefs[i]
  end;
end;

procedure SetIndexName(ds: TDataSet; const strValue: string);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(ds.ClassInfo, 'IndexName');

  {if such properties exists}
  if Assigned(PropInfo) then
  begin
    case PropInfo^.PropType^^.Kind of
      tkString,
      tkLString,
      tkWString: SetStrProp(ds, PropInfo, strValue);
    end;
  end
end;

function GetCommaText(const List: TStrings; ch: Char): string;
var
  i, j: integer;
begin
  j := List.Count;

  if j > 0 then
  begin
    Result := List[0];
    for i := 1 to j-1 do
      Result := Result + ch + List[i];
  end
  else
    Result := '';
end;

procedure SortGrid(Column: TColumn);
//var
//  cds: TClientDataSet;

  procedure ListFieldNames(List: TStrings; const FieldNames: string);
  var
    i: Integer;
  begin
    i := 1;
    while i <= Length(FieldNames) do
     List.Add(ExtractFieldName(FieldNames, i));
  end;

  procedure SetIndexField(const FieldNames: string; IsAdd: Boolean);
  const
    SMIndexName = 'SMIndex';
  var
    grid: TSMDBGrid;
    i, j, AscField: Integer;
    DescList, AllFieldList, MasterDetailList, NewList: TStrings;

    IndexDefs: TIndexDefs;
    IndexDef: TIndexDef;

    FieldName, AllFields, DescFields: string;
    IdxOpts: TIndexOptions;
  begin
    DescList := TStringList.Create;
    MasterDetailList := TStringList.Create;
    AllFieldList := TStringList.Create;
    NewList := TStringList.Create;

    if Column.Grid is TSMDBGrid then
      grid := TSMDBGrid(Column.Grid)
    else
      grid := nil;

    try
      ListFieldNames(NewList, FieldNames);

      IndexDefs := GetIndexDefs(Column.Field.Dataset);
      IndexDef := GetIndexDef(IndexDefs, SMIndexName);
      IdxOpts := [];

      if Assigned(IndexDef) then
      begin
        ListFieldNames(AllFieldList, IndexDef.Fields);

        if (ixDescending in IndexDef.Options) then
          DescList.Assign(AllFieldList)
{$IFDEF DELPHI_4_UP}
        else
        if (IndexDef.DescFields <> '') then
          ListFieldNames(DescList, IndexDef.DescFields)
{$ENDIF};

        if IsAdd then
          IdxOpts := IndexDef.Options
        else
        begin
          if Assigned(grid) then
            grid.ClearSort;

          ListIntersect(AllFieldList, NewList);
          ListIntersect(DescList, NewList);
        end;

      {$IFDEF USE_BDE}
        if Column.Field.Dataset is TTable then
        begin
          with TTable(Column.Field.Dataset) do
            if Exclusive then
              DeleteIndex(SMIndexName)
        end
      {$ENDIF}
      {$IFDEF VER_ENTERPRISE}
        else
        if Column.Field.Dataset is TClientDataset then
        begin
          with TClientDataset(Column.Field.Dataset) do
          begin
            if Assigned(MasterSource) and (MasterFields <> EmptyStr) then
              IndexFieldNames := MasterFields;
            DeleteIndex(SMIndexName)
          end;
        end
      {$ENDIF}
      {$IFDEF USE_KBMMEMTABLE}
        else
        if Column.Field.Dataset is TkbmMemTable then
        begin
          with TkbmMemTable(Column.Field.Dataset) do
            DeleteIndex(SMIndexName);
        end
      {$ENDIF}
        ;
        IndexDefs.Update;
      end;

      if False then
    {$IFDEF VER_ENTERPRISE}
      else
      if Column.Field.Dataset is TClientDataset then
      begin
        with TClientDataset(Column.Field.Dataset) do
        begin
          if Assigned(MasterSource) and (MasterFields <> EmptyStr) then
          begin
            ListFieldNames(MasterDetailList, MasterFields);
            ListDifference(DescList, MasterDetailList);
            ListDifference(AllFieldList, MasterDetailList);
            for i := MasterDetailList.Count-1 downto 0 do
              AllFieldList.Insert(0, MasterDetailList[i]);
          end;
        end;
      end
    {$ENDIF}
    {$IFDEF USE_KBMMEMTABLE}
      else
      if Column.Field.Dataset is TkbmMemTable then
      begin
        with TkbmMemTable(Column.Field.Dataset) do
        begin
          if Assigned(MasterSource) and (MasterFields <> EmptyStr) then
          begin
            ListFieldNames(MasterDetailList, MasterFields);
            ListDifference(DescList, MasterDetailList);
            ListDifference(AllFieldList, MasterDetailList);
            for i := MasterDetailList.Count-1 downto 0 do
              AllFieldList.Insert(0, MasterDetailList[i]);
          end;
        end;
      end
    {$ENDIF}
      ;

      Include(IdxOpts, ixCaseInsensitive);

      for j := NewList.Count-1 downto 0 do
      begin
        FieldName := NewList[j];

        AscField := AllFieldList.IndexOf(FieldName);
        if AscField <> -1 then
        begin
          { Field already exists in index }
          i := DescList.IndexOf(FieldName);
          if i <> -1 then
          begin
            DescList.Delete(i);

            if IsAdd then
              AllFieldList.Delete(AscField);
          end
          else
            DescList.Add(FieldName);
        end
        else
          AllFieldList.Add(FieldName);
      end;

      if DescList.Count > 0 then
        Include(IdxOpts, ixDescending)
      else
        Exclude(IdxOpts, ixDescending);

      DescFields := GetCommaText(DescList, ';');
      AllFields := GetCommaText(AllFieldList, ';');
{$IFDEF DELPHI_4_UP}
      if (Column.ParentColumn <> nil) and (DescFields <> '') then
        Exit
      else
{$ENDIF}
    {$IFDEF USE_BDE}
      if (Column.Field.Dataset is TTable) then
      begin
        with TTable(Column.Field.Dataset) do
          if Exclusive then
            AddIndex(SMIndexName, AllFields, IdxOpts{$IFDEF DELPHI_4_UP}, DescFields {$ENDIF})
          else
            exit
      end
    {$ENDIF}
    {$IFDEF VER_ENTERPRISE}
      else
      if Column.Field.Dataset is TClientDataset then
        TClientDataset(Column.Field.Dataset).AddIndex(SMIndexName, AllFields, IdxOpts{$IFDEF DELPHI_4_UP}, DescFields {$ENDIF})
    {$ENDIF}
    {$IFDEF USE_KBMMEMTABLE}
      else
      if Column.Field.Dataset is TkbmMemTable then
      begin
        IndexDef := IndexDefs.AddIndexDef;
        IndexDef.Name := SMIndexName;
        IndexDef.DescFields := DescFields;
        IndexDef.Fields := AllFields;
        IndexDef.Options := IdxOpts;
        TkbmMemTable(Column.Field.Dataset).Indexes.Add(IndexDef);
      end
    {$ENDIF}
      ;

      IndexDefs.Update;
      SetIndexName(Column.Field.Dataset, SMIndexName);

      if (Column is TSMDBColumn) then
      begin
        with TSMDBColumn(Column) do
          if (Pos(FieldNames, DescFields) > 0) then
            SortType := stDescending
          else
          if (Pos(FieldNames, AllFields) > 0) then
            SortType := stAscending
          else
            SortType := stNone;
      end
    finally
      DescList.Free;
      AllFieldList.Free;
      MasterDetailList.Free;
      NewList.Free;
    end;
  end;

var
  IsShiftPressed: Boolean;
  KeyState: TKeyboardState;
begin
  if False
  {$IFDEF USE_BDE}
    or (Column.Field.Dataset is TTable)
  {$ENDIF}
  {$IFDEF VER_ENTERPRISE}
     or (Column.Field.Dataset is TClientDataset)
  {$ENDIF}
  {$IFDEF VER_KBMMEMTABLE}
     or (Column.Field.Dataset is TkbmMemTable)
  {$ENDIF}
    then
  begin
    if Assigned(Column) and
       Assigned(Column.Field) and
       Assigned(Column.Field.DataSet) then
    begin
      GetKeyboardState(KeyState);
      IsShiftPressed := (KeyState[VK_SHIFT] and $80 <> 0);

//      cds := TClientDataset(Column.Field.Dataset);

      case Column.Field.FieldKind of
        fkData,
        fkInternalCalc: if not Column.Field.IsBlob
                           {$IFDEF DELPHI_4_UP}
                           and not (Column.Field.DataType in [ftADT, ftArray, ftDataSet])
                           {$ENDIF}
                           then
                          SetIndexField(Column.FieldName, IsShiftPressed);

        fkLookup: SetIndexField(Column.Field.KeyFields, IsShiftPressed);
          //fkCalculated, fkAggregate: //can't do anything with these
      end;
    end;
  end
end;

function Font2String(smFont: TFont; cl: TColor; al: TAlignment): string;
begin
  Result := smFont.Name + ',' +
            IntToStr(smFont.CharSet) + ',' +
            IntToStr(smFont.Color) + ',' +
            IntToStr(smFont.Size) + ',' +
            IntToStr(Byte(smFont.Style)) + ',' +
            IntToStr(cl) + ','+
            IntToStr(Ord(al));
end;

procedure String2Font(Value: string; smFont: TFont; var Background: TColor; var al: TAlignment);
var
  Data: string;
  i: Integer;
begin
  try
    i := Pos(',', Value);
    if i > 0 then
    begin
      {Name}
      Data := Trim(Copy(Value, 1, i-1));
      if Data <> '' then
        smFont.Name := Data;
      Delete(Value, 1, i);
      i := Pos(',', Value);
      if i > 0 then
      begin
        {CharSet}
        Data := Trim(Copy(Value, 1, i-1));
        if Data <> '' then
          smFont.Charset := TFontCharSet(StrToIntDef(Data, smFont.Charset));
        Delete(Value, 1, i);
        i := Pos(',', Value);
        if i > 0 then
        begin
          {Color}
          Data := Trim(Copy(Value, 1, i-1));
          if Data <> '' then
            smFont.Color := TColor(StrToIntDef(Data, smFont.Color));
          Delete(Value, 1, i);
          i := Pos(',', Value);
          if i > 0 then
          begin
            {Size}
            Data := Trim(Copy(Value, 1, i-1));
            if Data <> '' then
              smFont.Size := StrToIntDef(Data, smFont.Size);
            Delete(Value, 1, i);
            i := Pos(',', Value);
            if i > 0 then
            begin
              {Style}
              Data := Trim(Copy(Value, 1, i-1));
              if Data <> '' then
                smFont.Style := TFontStyles(Byte(StrToIntDef(Data, Byte(smFont.Style))));
              Delete(Value, 1, i);
              i := Pos(',', Value);
              if i > 0 then
              begin
                {Background}
                Data := Trim(Copy(Value, 1, i-1));
                if Data <> '' then
                  Background := TColor(StrToIntDef(Data, Background));
                Delete(Value, 1, i);
                {alignment}
                Data := Trim(Value);
                if Data <> '' then
                  al := TAlignment(StrToIntDef(Data, Ord(al)));
              end
            end
          end
        end
      end
    end;
  except
  end;
end;

function NormilizedText(const s: string): string;
begin
  if (Pos(',', s) > 0) then
    Result := '"' + s + '"'
  else
    Result := s
end;

procedure SaveGridToRegistry(grid: TDBGrid; const RegistryKey, RegistrySection: string);
var
  RegIniFile: TRegIniFile;
  i: Integer;
  strAttr: string;
begin
  RegIniFile := TRegIniFile.Create(RegistryKey);
  try
    RegIniFile.WriteInteger(RegistrySection, 'Count', grid.Columns.Count);
    for i := 0 to (grid.Columns.Count-1) do
    begin
      with grid.Columns.Items[i] do
      begin
        if ReadOnly then
          strAttr := 'R'
        else
          strAttr := '';
        RegIniFile.WriteString(RegistrySection, IntToStr(i),
                               Format('%s,%d,%s,%s', [FieldName, Width, NormilizedText(Title.Caption), strAttr]));
        RegIniFile.WriteString(RegistrySection, IntToStr(i) + '_Title', Font2String(Title.Font, Title.Color, Title.Alignment));
        RegIniFile.WriteString(RegistrySection, IntToStr(i) + '_Data', Font2String(Font, Color, Alignment));
      end;

      if (grid is TSMDBGrid) and (grid.Columns.Items[i] is TSMDBColumn) then
        with TSMDBColumn(grid.Columns.Items[i]) do
        begin
          RegIniFile.WriteString(RegistrySection, IntToStr(i) + '_Sort',
                                 Format('%d,%s,%d,%d,%s,%d,%d', [Ord(SortType), NormilizedText(SortCaption), BandIndex, Ord(InplaceEditor), NormilizedText(VarToStr(FooterValue)), Ord(FooterType), Tag]));
          if FFooterText<>'' then
            RegIniFile.WriteString(RegistrySection, IntToStr(i) + '_Footer',Format('%s',[NormilizedText(FFooterText)]));
        end;
    end;
  finally
    RegIniFile.Free;
  end
end;

function GetValueFromKey(var strValues: string): string;
var
//  j: Integer;

  P: PChar;
  intStart, intEnd: Integer;
  IsBody: Boolean;
begin
  if (strValues = '') then
  begin
    Result := '';
    exit;
  end;

  P := PChar(strValues);
  intStart := 1;
  intEnd := 1;
  IsBody := (P^ = '"');
  if IsBody then
    Inc(intStart);
  while (P <> nil) and (P^ <> #0) do
  begin
    if (P^ = ',') and not IsBody then
      break
    else
    if IsBody and (P^ = '"') then
      IsBody := False;

    Inc(P);
    Inc(intEnd);
  end;
  Result := Copy(strValues, intStart, intEnd-intStart);
  Delete(strValues, 1, intEnd);

  intEnd := Length(Result);
  if (intEnd > 0) then
    if (Result[intEnd] = '"') then
      Delete(Result, intEnd, 1);

{  j := Pos(',', strValues);
  if (j = 0) then
    Result := strValues
  else
  begin
    Result := Copy(strValues, 1, j-1);
    Delete(strValues, 1, j);
  end;
}end;

type
  THackDBGrid = class(TDBGrid);
  
procedure LoadGridFromRegistry(grid: TDBGrid; const RegistryKey, RegistrySection: string);
var
  RegIniFile: TRegIniFile;
  i, Count, aWidth: Integer;
  s, strAttr: string;
  cl: TColor;
  col: TColumn;
  al: TAlignment;
begin
{ disable DBGrid-repaint while not will executed EndLayout
  Because I donn't want to repaint of the grid after each
  addition and after Columns.Clear }
  THackDBGrid(grid).BeginLayout;
  if grid is TSMDBGrid then
    TSMDBGrid(grid).FAutoFitIsLocked := True;

  RegIniFile := TRegIniFile.Create(RegistryKey);
  try
    Count := RegIniFile.ReadInteger(RegistrySection, 'Count', 0);
    if (Count > 0) then
    begin
      grid.Columns.Clear;
      for i := 0 to (Count-1) do
      begin
        s := RegIniFile.ReadString(RegistrySection, IntToStr(i), '');
        if (s <> '') then
        begin
          col := grid.Columns.Add;
          col.FieldName := GetValueFromKey(s);
          aWidth := StrToIntDef(GetValueFromKey(s), 64);
          if (aWidth > -1) then
            col.Width := aWidth
      {$IFDEF DELPHI_4_UP}
          else
            col.Visible := False
      {$ENDIF}
          ;
          col.Title.Caption := GetValueFromKey(s);
          strAttr := GetValueFromKey(s);
          col.ReadOnly := (Pos('R', strAttr) > 0);

          s := RegIniFile.ReadString(RegistrySection, IntToStr(i) + '_Title', '');
          if (s <> '') then
          begin
            cl := col.Title.Color;
            al := col.Title.Alignment;
            String2Font(s, col.Title.Font, cl, al);
            col.Title.Color := cl;
            col.Title.Alignment := al;
          end;
          s := RegIniFile.ReadString(RegistrySection, IntToStr(i) + '_Data', '');
          if (s <> '') then
          begin
            cl := col.Color;
            al := col.Alignment;
            String2Font(s, col.Font, cl, al);
            col.Color := cl;
            col.Alignment := al;
          end;

          if col is TSMDBColumn then
          begin
            s := RegIniFile.ReadString(RegistrySection, IntToStr(i) + '_Sort', '');
            if (s <> '') then
              with TSMDBColumn(col) do
              begin
                SortType := TSMSortType(StrToIntDef(GetValueFromKey(s), 0));
                SortCaption := GetValueFromKey(s);

                BandIndex := StrToIntDef(GetValueFromKey(s), 0);
                FInplaceEditor := TSMInplaceEditorType(StrToIntDef(GetValueFromKey(s), 0));
                FooterValue := GetValueFromKey(s);
                FooterType := TSMFooterType(StrToIntDef(GetValueFromKey(s), 0));
                Tag := StrToIntDef(GetValueFromKey(s), 0);
              end;
            s := RegIniFile.ReadString(RegistrySection, IntToStr(i) + '_Footer', '');
            if (s <> '') then
              with TSMDBColumn(col) do
              begin
                 FFooterText := GetValueFromKey(s);
              end;
          end;
        end;
      end;
    end;

  finally
    RegIniFile.Free;

    if grid is TSMDBGrid then
      TSMDBGrid(grid).FAutoFitIsLocked := False;
    THackDBGrid(grid).EndLayout;
  end;
end;

procedure SaveGridToIni(grid: TDBGrid; const FileName: string; const SectionName: string {$IFDEF DELPHI_5_UP} = '' {$ENDIF} );
var
  i: Integer;
  strSection, strAttr: string;
begin
  if (SectionName = '') then
    strSection := grid.Name
  else
    strSection := SectionName;
  with TIniFile.Create(FileName) do
    try
      WriteInteger(strSection, 'Count', grid.Columns.Count);
      for i := 0 to (grid.Columns.Count-1) do
      begin
        with grid.Columns.Items[i] do
        begin
          if ReadOnly then
            strAttr := 'R'
          else
            strAttr := '';
          WriteString(strSection, IntToStr(i),
                      Format('%s,%d,%s,%s', [FieldName, Width, NormilizedText(Title.Caption), strAttr]));
          WriteString(strSection, IntToStr(i) + '_Title', Font2String(Title.Font, Title.Color, Title.Alignment));
          WriteString(strSection, IntToStr(i) + '_Data', Font2String(Font, Color, Alignment));
        end;
        if (grid is TSMDBGrid) and (grid.Columns.Items[i] is TSMDBColumn) then
          with TSMDBColumn(grid.Columns.Items[i]) do
            WriteString(strSection, IntToStr(i) + '_Sort',
                        Format('%d,%s,%d,%d,%s,%d,%d', [Ord(SortType), NormilizedText(SortCaption), BandIndex, Ord(InplaceEditor), NormilizedText(VarToStr(FooterValue)), Ord(FooterType), Tag]));
      end;
    finally
      Free;
    end
end;

procedure LoadGridFromIni(grid: TDBGrid; const FileName: string; const SectionName: string {$IFDEF DELPHI_5_UP} = '' {$ENDIF} );
var
  i, Count, aWidth: Integer;
  s, strSection, strAttr: string;
  cl: TColor;
  col: TColumn;
  al: TAlignment;
begin
{ disable DBGrid-repaint while not will executed EndLayout
  Because I donn't want to repaint of the grid after each
  addition and after Columns.Clear }
  THackDBGrid(grid).BeginLayout;
  if grid is TSMDBGrid then
    TSMDBGrid(grid).FAutoFitIsLocked := True;

  if (SectionName = '') then
    strSection := grid.Name
  else
    strSection := SectionName;

  with TIniFile.Create(FileName) do
    try
      Count := ReadInteger(strSection, 'Count', 0);
      if (Count > 0) then
      begin
        grid.Columns.Clear;
        for i := 0 to (Count-1) do
        begin
          s := ReadString(strSection, IntToStr(i), '');
          if (s <> '') then
          begin
            col := grid.Columns.Add;
            col.FieldName := GetValueFromKey(s);
            aWidth := StrToIntDef(GetValueFromKey(s), 64);
            if (aWidth > -1) then
              col.Width := aWidth
        {$IFDEF DELPHI_4_UP}
            else
              col.Visible := False
        {$ENDIF}
            ;
            col.Title.Caption := GetValueFromKey(s);
            strAttr := GetValueFromKey(s);
            col.ReadOnly := (Pos('R', strAttr) > 0);

            s := ReadString(strSection, IntToStr(i) + '_Title', '');
            if (s <> '') then
            begin
              cl := col.Title.Color;
              al := col.Title.Alignment;
              String2Font(s, col.Title.Font, cl, al);
              col.Title.Color := cl;
              col.Title.Alignment := al;
            end;
            s := ReadString(strSection, IntToStr(i) + '_Data', '');
            if (s <> '') then
            begin
              cl := col.Color;
              al := col.Alignment;
              String2Font(s, col.Font, cl, al);
              col.Color := cl;
              col.Alignment := al;
            end;

            if col is TSMDBColumn then
            begin
              s := ReadString(strSection, IntToStr(i) + '_Sort', '');
              if (s <> '') then
                with TSMDBColumn(col) do
                begin
                  SortType := TSMSortType(StrToIntDef(GetValueFromKey(s), 0));
                  SortCaption := GetValueFromKey(s);

                  BandIndex := StrToIntDef(GetValueFromKey(s), 0);
                  FInplaceEditor := TSMInplaceEditorType(StrToIntDef(GetValueFromKey(s), 0));
                  FooterValue := GetValueFromKey(s);
                  FooterType := TSMFooterType(StrToIntDef(GetValueFromKey(s), 0));
                  Tag := StrToIntDef(GetValueFromKey(s), 0);
                end
            end;
          end;
        end;
      end;

    finally
      Free;

      if grid is TSMDBGrid then
        TSMDBGrid(grid).FAutoFitIsLocked := False;
      THackDBGrid(grid).EndLayout;
    end;
end;


type
  TBookmarks = class(TBookmarkList);
  TGridPicture = (gpBlob, gpMemo, gpPicture, gpOle, gpSortAsc, gpSortDesc);

const
  GridBmpNames: array[TGridPicture] of PChar = ('SM_BLOB', 'SM_MEMO', 'SM_PICT', 'SM_OLE',
                                                'SM_ARROWASC', 'SM_ARROWDESC');
  GridBitmaps: array[TGridPicture] of TBitmap = (nil, nil, nil, nil, nil, nil);
  bmArrow = 'SM_DBGARROW';
  bmEdit = 'SM_DBGEDIT';
  bmInsert = 'SM_DBGINSERT';
  bmMultiDot = 'SM_MSDOT';
  bmMultiArrow = 'SM_MSARROW';

//  bmMultiDot = 'SM_MSDOT';
//  bmMultiArrow = 'SM_MSARROW';
//  bmMultiCheckBox = 'SM_MSCHECKBOX';

function GetGridBitmap(BmpType: TGridPicture): TBitmap;
begin
  if GridBitmaps[BmpType] = nil then
  begin
    GridBitmaps[BmpType] := TBitmap.Create;
    GridBitmaps[BmpType].Handle := LoadBitmap(HInstance, GridBmpNames[BmpType]);
  end;
  Result := GridBitmaps[BmpType];
end;

procedure DestroyLocals; far;
var
  i: TGridPicture;
begin
  for i := Low(TGridPicture) to High(TGridPicture) do
    GridBitmaps[i].Free;
end;

procedure GridInvalidateRow(Grid: TSMDBGrid; Row: Longint);
var
  i: Longint;
begin
  for i := 0 to Grid.ColCount - 1 do
    Grid.InvalidateCell(i, Row);
end;

procedure GetCheckBoxSize;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES{32759}));
      FCheckWidth := Width div 4;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
end;

procedure WriteTitleText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; TextEllipsis: TSMTextEllipsis; IsWordBreak: Boolean {$IFDEF DELPHI_4_UP} ; ARightToLeft: Boolean {$ENDIF});
const
  EllipsisFlags: array [TSMTextEllipsis] of Integer = (0, DT_END_ELLIPSIS, DT_PATH_ELLIPSIS);
  WordBreaks: array [Boolean] of Integer = (0, DT_WORDBREAK);
  AlignFlags: array [TAlignment] of Integer =
     (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX or DT_VCENTER,
      DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX or DT_VCENTER,
      DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX or DT_VCENTER);
{$IFDEF DELPHI_4_UP}
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
{$ENDIF}
var
  B, R: TRect;
{$IFDEF DELPHI_4_UP}
  Hold: Integer;
  Left: Integer;
  I: TColorRef;
{$ELSE}
  I: Integer;
{$ENDIF}

  DrawBitmap: TBitmap;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if (TextEllipsis = teNone) and (GetNearestColor(ACanvas.Handle, I) = I) then
  begin
    { Use ExtTextOut for solid colors }

    { In BiDi, because we changed the window origin, the text that does not
      change alignment, actually gets its alignment changed. }
{$IFDEF DELPHI_4_UP}
    if (ACanvas.CanvasOrientation = coRightToLeft) and
       (not ARightToLeft) then
      ChangeBiDiModeAlignment(Alignment);
{$ENDIF}

{
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else //taCenter
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;
//    ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
}
    ACanvas.Brush.Style := bsClear;
{$IFDEF DELPHI_4_UP}
    if (ACanvas.CanvasOrientation = coRightToLeft) and
       ARightToLeft then
    begin
      case Alignment of
        taLeftJustify:
          Left := ARect.Left + DX;
        taRightJustify:
          Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
      else //taCenter
        Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
          - (ACanvas.TextWidth(Text) shr 1);
      end;

      ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text)
    end
    else
{$ENDIF}
    begin
      Inc(ARect.Left, DX);
      Dec(ARect.Right, DX);
      Inc(ARect.Top, DY);
      Dec(ARect.Bottom, DY);

      DrawText(ACanvas.Handle, PChar(Text), Length(Text), ARect,
               EllipsisFlags[TextEllipsis] or
               AlignFlags[Alignment] or
               WordBreaks[IsWordBreak]
               {$IFDEF DELPHI_4_UP} or RTL[ARightToLeft] {$ENDIF} );
    end
  end
  else
  begin
    DrawBitmap := TBitmap.Create;
    try
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
      begin                     { brush origin tics in painting / scrolling.    }
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);

{$IFDEF DELPHI_4_UP}
        if (ACanvas.CanvasOrientation = coRightToLeft) then
          ChangeBiDiModeAlignment(Alignment);
{$ENDIF}
        DrawText(Handle, PChar(Text), Length(Text), R,
          EllipsisFlags[TextEllipsis] or AlignFlags[Alignment] {$IFDEF DELPHI_4_UP} or RTL[ARightToLeft] {$ENDIF} );
      end;
{$IFDEF DELPHI_4_UP}
      if (ACanvas.CanvasOrientation = coRightToLeft) then
      begin
        Hold := ARect.Left;
        ARect.Left := ARect.Right;
        ARect.Right := Hold;
      end;
{$ENDIF}
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
//      DrawBitmap.Canvas.Unlock;
      DrawBitmap.Free;
    end;
  end;
end;

{ TSMDBColumn }
function GetColumnEllipsis(col: TColumn): TSMTextEllipsis;
begin
  if (col is TSMDBColumn) then
    Result := TSMDBColumn(col).TextEllipsis
  else
    Result := teNone
end;

constructor TSMDBColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FSortCaption := '';
  FSortType := stNone;
  FInplaceEditor := ieStandard;
  FFooterType := ftCustom;
  FFooterText := '';

  FTag := 0;
  FBandIndex := -1;
  FTextEllipsis := teNone;
end;

destructor TSMDBColumn.Destroy;
begin
  if Assigned(FFilterList) then begin
    FFilterList.Free;
    FFilterList := nil;
  end;

  inherited;
end;

procedure TSMDBColumn.Assign(Source: TPersistent);
var
  colSource: TSMDBColumn;
begin
  inherited Assign(Source);

  if Source is TSMDBColumn then
  begin
    colSource := TSMDBColumn(Source);
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      SortCaption := colSource.SortCaption;
      SortType := colSource.SortType;
      FInplaceEditor := colSource.InplaceEditor;
      Tag := colSource.Tag;
      BandIndex := colSource.BandIndex;
      TextEllipsis := colSource.TextEllipsis;
      FilterValue := colSource.FilterValue;
      FilterList.Assign(colSource.FilterList);

      FooterType := colSource.FooterType;
      FooterText := colSource.FooterText;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end;
end;

procedure TSMDBColumn.RestoreDefaults;
begin
  inherited RestoreDefaults;

  FFilterList.Free;
  FFilterList := nil;
end;

procedure TSMDBColumn.DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean);
begin
end;

procedure TSMDBColumn.SetSortCaption(Value: string);
begin
  if FSortCaption <> Value then
  begin
    FSortCaption := Value;
    Changed(False);
    GridInvalidateRow(Grid, 0)// Grid.Invalidate
  end;
end;

procedure TSMDBColumn.SetInplaceEditor(Value: TSMInplaceEditorType);
begin
  if FInplaceEditor <> Value then
  begin
    FInplaceEditor := Value;
    Changed(False)
  end;
end;

procedure TSMDBColumn.SetSortType(Value: TSMSortType);
begin
  if FSortType <> Value then
  begin
    FSortType := Value;
    Changed(False);

    if Assigned(Grid) and Assigned(Grid.OnSortChanged) then
      Grid.OnSortChanged(Grid, Self);

    GridInvalidateRow(Grid, 0); //Grid.Invalidate
  end;
end;

procedure TSMDBColumn.SetFooterValue(Value: Variant);
begin
//  if (FFooterValue <> Value) then
  begin
    FFooterValue := Value;
    Changed(False);
    Grid.InvalidateFooter;
  end;
end;

procedure TSMDBColumn.SetFooterType(Value: TSMFooterType);
begin
  if (FFooterType <> Value) then
  begin
    FFooterType := Value;
    Changed(False);
    Grid.InvalidateFooter;
  end;
end;

function TSMDBColumn.GetGrid: TSMDBGrid;
begin
  if Assigned(Collection) and (Collection is TSMDBGridColumns) then
    Result := TSMDBGrid(inherited Grid)
  else
    Result := nil;
end;

procedure TSMDBColumn.SetFilterValue(const Value: string);
begin
  if FFilterValue <> Value then
  begin
    FFilterValue := Value;
    Changed(False);
  end;
end;

function TSMDBColumn.GetFilterList : TStrings;
begin
  if FFilterList = nil then
    FFilterList := TStringList.Create;
  Result := FFilterList;
end;

procedure TSMDBColumn.SetFilterList(const Value: TStrings);
begin
  if Value = nil then
  begin
    FFilterList.Free;
    FFilterList := nil;
    exit;
  end;
  FFilterList.Assign(Value);
end;

{ TSMDBImagesColumn }
constructor TSMDBImagesColumn.Create(Collection: TCollection);
begin
  inherited;

  FInplaceEditor := ieImagelist
end;

procedure TSMDBImagesColumn.SetImages(Value: TImageList);
begin
  FImages := Value
end;

procedure TSMDBImagesColumn.DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean);
var
  intValue: Integer;
begin
  inherited;

  if Assigned(Images) then
  begin
    if VarIsNull(Value) or VarIsEmpty(Value) then
      intValue := 0
    else
      intValue := Value;

    {draw background}
    ACanvas.Brush.Color := Background;
    ACanvas.FillRect(ARect);

    Images.Draw(ACanvas, ARect.Left, ARect.Top, intValue);

    Done := True
  end;
end;

{ TSMDBProgressColumn }
constructor TSMDBProgressColumn.Create(Collection: TCollection);
begin
  inherited;

  FInplaceEditor := ieProgressbar
end;

procedure TSMDBProgressColumn.DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean);
var
  bmp: TBitmap;

  intValue, iSize: Longint;
begin
  inherited;

  if VarIsNull(Value) or VarIsEmpty(Value) then
    intValue := 0
  else
    intValue := Value;

  with ACanvas do
  begin
    bmp := TBitmap.Create;
    try
      bmp.Height := ARect.Bottom-ARect.Top;
      bmp.Width := ARect.Right-ARect.Left;
      {draw background}
      bmp.Canvas.Brush.Color := Background;
      bmp.Canvas.FillRect(bmp.Canvas.ClipRect);

      if Max-Min = 0 then
        iSize := 0
      else
        iSize := LongInt(Trunc(bmp.Width * ((intValue-Min)*100/Max-Min)*0.01));
      if iSize > bmp.Width then
        iSize := bmp.Width;
      if iSize > 0 then
      begin
        bmp.Canvas.Brush.Color := Self.Color;
        bmp.Canvas.FillRect(Rect(0, 0, iSize, bmp.Height));
      end;

      Draw(ARect.Left, ARect.Top, bmp);
      Done := True
    finally
      bmp.Free;
    end;
  end;
end;

{ TSMDBPasswordColumn }
constructor TSMDBPasswordColumn.Create(Collection: TCollection);
begin
  inherited;

  FInplaceEditor := iePassword
end;

procedure TSMDBPasswordColumn.DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean);
var
  i: Integer;
  s, strValue: string;
begin
  inherited;

  {draw background}
  ACanvas.Brush.Color := Background;
  ACanvas.FillRect(ARect);

  if VarIsNull(Value) or VarIsEmpty(Value) then
  else
  begin
    strValue := VarToStr(Value);

    s := '';
    for i := 1 to Length(strValue) do
      s := s + PasswordChar;
    WriteTitleText(ACanvas, ARect, 0, 0, s{Value}, Alignment,
                   TextEllipsis,
                   False
                   {$IFDEF DELPHI_4_UP},
                   (grid.BiDiMode <> bdLeftToRight)
//                   grid.IsRightToLeft//UseRightToLeftAlignmentForField(Columns[BCol].Field, Columns[BCol].Alignment)
                   {$ENDIF})
  end;
  Done := True
end;

{ TSMDBHyperlinkColumn }
constructor TSMDBHyperlinkColumn.Create(Collection: TCollection);
begin
  inherited;

  FInplaceEditor := ieHyperLink
end;

procedure TSMDBHyperlinkColumn.DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean);
var
  strValue: string;
begin
  inherited;

  if VarIsNull(Value) or VarIsEmpty(Value) then
  else
  begin
    strValue := VarToStr(Value);

    {draw background}
    ACanvas.Brush.Color := Background;
    ACanvas.FillRect(ARect);

    ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
    ACanvas.Font.Color := clBlue;
    WriteTitleText(ACanvas, ARect, 0, 0, strValue, Alignment,
                   TextEllipsis,
                   False
                   {$IFDEF DELPHI_4_UP},
                   (grid.BiDiMode <> bdLeftToRight)
//                   grid.IsRightToLeft//UseRightToLeftAlignmentForField(Columns[BCol].Field, Columns[BCol].Alignment)
                   {$ENDIF})
  end;
  Done := True
end;

{ TSMDBButtonColumn }
constructor TSMDBButtonColumn.Create(Collection: TCollection);
begin
  inherited;

  FInplaceEditor := ieButton
end;

procedure TSMDBButtonColumn.DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean);
const
  BP_PUSHBUTTON = 1;

  PBS_HOT       = 2;
  PBS_PRESSED   = 3;
var
  DrawState: Integer;
  hhTheme: hTHEME;
  FMouseInRect: Boolean;
  strValue: string;
begin
  inherited;

  if IsXPThemesEnabled and
     not (csDesigning in Grid.ComponentState) then
    hhTheme := OpenThemeData(0, 'Button')
  else
    hhTheme := 0;

  if (hhTheme <> 0) then
  begin
    FMouseInRect := Grid.IsMouseInRect(ARect);

    DrawState := 0;
    if FMouseInRect then
      DrawState := DrawState or PBS_HOT;

    try
      Grid.FLastXPDrawn := True;
      DrawThemeBackground(hhTheme, ACanvas.Handle, BP_PUSHBUTTON, DrawState, @ARect, nil);
    finally
      CloseThemeData(hhTheme);
    end;
  end
  else
  begin
    DrawState := DFCS_BUTTONPUSH;
    if Grid.Flat then
      DrawState := DrawState or DFCS_FLAT;

    DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DrawState);

    strValue := VarToStr(Value);
    if strValue <> '' then
    begin
      WriteTitleText(ACanvas, ARect, 0, 0, strValue, Alignment,
                     TextEllipsis,
                     False
                     {$IFDEF DELPHI_4_UP},
                     (grid.BiDiMode <> bdLeftToRight)
//                     grid.IsRightToLeft//UseRightToLeftAlignmentForField(Columns[BCol].Field, Columns[BCol].Alignment)
                     {$ENDIF})
    end;
  end;
  Done := True
end;

{ TSMDBCheckboxColumn }
constructor TSMDBCheckboxColumn.Create(Collection: TCollection);
begin
  inherited;

  FInplaceEditor := ieCheckbox
end;

procedure TSMDBCheckboxColumn.SetValueChecked(const AValue: string);
begin
  if (FValueChecked <> AValue) then
  begin
    FValueChecked := AValue;
    Changed(False)
  end
end;

procedure TSMDBCheckboxColumn.SetValueUnChecked(const AValue: string);
begin
  if (FValueUnChecked <> AValue) then
  begin
    FValueUnChecked := AValue;
    Changed(False)
  end
end;

procedure TSMDBCheckboxColumn.DrawItem(ACanvas: TCanvas; ARect: TRect; Value: Variant; Background: TColor; AState: TGridDrawState; var Done: Boolean);
var
  CheckState: TCheckBoxState;
begin
  inherited;

  CheckState := cbUnChecked;
  try
    if VarIsNull(Value) or VarIsEmpty(Value) then
      CheckState := cbGrayed
    else
    if Value then
      CheckState := cbChecked
  except
  end;

  ACanvas.FillRect(ARect);
  InflateRect(ARect, -2, -2);

  Grid.DrawCheckBox(ARect, CheckState, taCenter);
  InflateRect(ARect, 2, 2);
  Done := True
end;

{ TSMDBGridColumns }
function TSMDBGridColumns.Add: TSMDBColumn;
begin
  Result := TSMDBColumn(inherited Add);
end;

function TSMDBGridColumns.GetColumn(Index: Integer): TSMDBColumn;
begin
  Result := TSMDBColumn(inherited Items[Index]);
end;

procedure TSMDBGridColumns.SetColumn(Index: Integer; Value: TSMDBColumn);
begin
  Items[Index].Assign(Value);
end;

{ TSMGradientDraw }
constructor TSMGradientDraw.Create(AGrid: TSMDBGrid);
begin
  inherited Create;

  FDirection := fdNone;
  FStartColor := clWhite;
  FEndColor := $00DDDDDD;
  FGrid := AGrid;
end;

procedure TSMGradientDraw.SetStartColor(AValue: TColor);
begin
  if (FStartColor <> AValue) then
  begin
    FStartColor := AValue;
    if (FGrid <> nil) then
      FGrid.Invalidate;
  end;
end;

procedure TSMGradientDraw.SetEndColor(AValue: TColor);
begin
  if (FEndColor <> AValue) then
  begin
    FEndColor := AValue;
    if (FGrid <> nil) then
      FGrid.Invalidate;
  end;
end;

procedure TSMGradientDraw.SetDirection(Value: TSMGradientDirection);
begin
  if (FDirection <> Value) then
  begin
    FDirection := Value;
    if (FGrid <> nil) then
      FGrid.Invalidate;
  end;
end;

{ TSMDBGridStyle }
constructor TSMDBGridStyle.Create(AGrid: TSMDBGrid);
begin
  inherited Create;

  FBackground := TBitmap.Create;
  FBands := TSMGradientDraw.Create(AGrid);
  FTitle := TSMGradientDraw.Create(AGrid);
  FFooter := TSMGradientDraw.Create(AGrid);
  FWallpaper := TSMGradientDraw.Create(AGrid);
  FGrouping := TSMGradientDraw.Create(AGrid);

  FGrid := AGrid;
  FFilterColor := clInfoBk;
end;

destructor TSMDBGridStyle.Destroy;
begin
  FBackground.Free;

  FBands.Free;
  FTitle.Free;
  FFooter.Free;
  FWallpaper.Free;
  FGrouping.Free;

  inherited Destroy;
end;

function TSMDBGridStyle.GetGradient(Index: Integer): TSMGradientDraw;
begin
  case Index of
    0: Result := FBands;
    1: Result := FTitle;
    2: Result := FFooter;
    3: Result := FWallpaper;
    4: Result := FGrouping;
  else
    Result := nil
  end;
end;

procedure TSMDBGridStyle.SetGradient(Index: Integer; Value: TSMGradientDraw);
begin
  case Index of
    0: FBands.Assign(Value);
    1: FTitle.Assign(Value);
    2: FFooter.Assign(Value);
    3: FWallpaper.Assign(Value);
    4: FGrouping.Assign(Value);
  else
  end;
  if (FGrid <> nil) then
    FGrid.Invalidate;
end;

procedure TSMDBGridStyle.SetOddColor(AValue: TColor);
begin
  if (FOddColor <> AValue) then
  begin
    FOddColor := AValue;
    FStyle := gsCustom;
    if (FGrid <> nil) then
      FGrid.Invalidate;
  end;
end;

procedure TSMDBGridStyle.SetEvenColor(AValue: TColor);
begin
  if (FEvenColor <> AValue) then
  begin
    FEvenColor := AValue;
    FStyle := gsCustom;
    if (FGrid <> nil) then
      FGrid.Invalidate;
  end;
end;

procedure TSMDBGridStyle.SetBackground(Value: TBitmap);
begin
  FBackground.Assign(Value);

  if (FGrid <> nil) then
    FGrid.Invalidate;
end;

procedure TSMDBGridStyle.SetStyle(Value: TSMGridStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;

    case FStyle of
      gsNormal: begin
                  FOddColor := FGrid.Color;
                  FEvenColor := FGrid.Color;
                end;
      gsPriceList: begin
                     FOddColor := cl3DLight;
                     FEvenColor := clWindow;
                   end;
      gsMSMoney: begin
                   FOddColor := clSkyBlue;
                   FEvenColor := clWindow;
                 end;
      gsBrick: begin
                 FOddColor := TColor($CFDFDF);
                 FEvenColor := TColor($A0BFBF);
               end;
      gsDesert: begin
                  FOddColor := TColor($B8CCD8);
                  FEvenColor := TColor($688CA0);
                end;
      gsEggplant: begin
                    FOddColor := TColor($A8B090);
                    FEvenColor := TColor($788058);
                  end;
      gsLilac: begin
                 FOddColor := TColor($D8A8B0);
                 FEvenColor := TColor($B05058);
               end;
      gsMaple: begin
                 FOddColor := TColor($A8D8EB);
                 FEvenColor := TColor($48A8C8);
               end;
      gsMarine: begin
                  FOddColor := TColor($B8C088);
                  FEvenColor := TColor($889048);
                end;
      gsRose: begin
                FOddColor := TColor($B8B0D0);
                FEvenColor := TColor($7060A0);
              end;
      gsSpruce: begin
                  FOddColor := TColor($A8C8A0);
                  FEvenColor := TColor($689858);
                end;
      gsWheat: begin
                 FOddColor := TColor($A0E0E0);
                 FEvenColor := TColor($40BCC0);
               end;

    { Soft-color themes from Aditya F. Fadilla [afadilla@email.com] Thanks!}

      gsSoftWheat: begin
                     FOddColor := TColor($00ACE3E3);
                     FEvenColor := TColor($00C5EAEB);
                   end;

      gsSoftRose: begin
                    FOddColor := TColor($00DCD7EA);
                    FEvenColor := TColor($00E3E0ED);
                  end;

      gsAquaBlue: begin
                    FOddColor := TColor($00FFF4D9);
                    FEvenColor := TColor($00FFDFBF);
                  end;

      gsSoftMaple: begin
                     FOddColor := TColor($00C6E6F2);
                     FEvenColor := TColor($009BD0E1);
                   end;

      gsSoftLilac: begin
                     FOddColor := TColor($00E7CBD0);
                     FEvenColor := TColor($00ECD5D7);
                   end;

      gsSoftDesert: begin
                      FOddColor := TColor($00DFE9EE);
                      FEvenColor := TColor($00BCCDD6);
                    end;

      gsSoftEggPlant: begin
                        FOddColor := TColor($00D7DACB);
                        FEvenColor := TColor($00C2C7AD);
                      end;

      gsSoftBrick: begin
                     FOddColor := TColor($00DFEEEE);
                     FEvenColor := TColor($00CDE4E4);
                   end;

      gsSoftSpruce: begin
                      FOddColor := TColor($00D3E3CE);
                      FEvenColor := TColor($00BAD1B1);
                    end;

      gsSoftYellowGreen: begin
                           FOddColor := TColor($00E6FFFF);
                           FEvenColor := TColor($00DCE9D8);
                         end;

      gsSoftGray: begin
                    FOddColor := TColor($00E4E4E4);
                    FEvenColor := TColor($00F5F5F5);
                  end;

    end;
    if (FGrid <> nil) then
      FGrid.Invalidate;
  end;
end;

procedure TSMDBGridStyle.SetFilterColor(const Value: TColor);
begin
  if (FFilterColor <> Value) then
  begin
    FFilterColor := Value;
    if (FGrid <> nil) then
      FGrid.Invalidate;
  end;
end;

{ TSMGrouping }
constructor TSMGrouping.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FExpression := '';
  FFont := TFont.Create;
  with (Collection as TSMGroupings) do
    FFont.Assign(Grid.Font);
  FFont.Style := [fsBold];
  FFont.Color := clBtnText;
  FColor := clBtnFace;
end;

destructor TSMGrouping.Destroy;
begin
  FFont.Free;

  inherited Destroy;
end;

procedure TSMGrouping.SetFont(Value: TFont);
begin
  FFont.Assign(Value)
end;

procedure TSMGrouping.Assign(Source: TPersistent);
begin
  if Source is TSMGrouping then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      Expression := TSMGrouping(Source).Expression;
      Font.Assign(TSMGrouping(Source).Font);
      Color := TSMGrouping(Source).Color;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;


{ TSMGroupings }
constructor TSMGroupings.Create(AGrid: TSMDBGrid);
begin
  inherited Create(TSMGrouping);

  FGrid := AGrid;
end;

function TSMGroupings.Add: TSMGrouping;
begin
  Result := TSMGrouping(inherited Add);
end;

function TSMGroupings.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TSMGroupings.GetGrouping(Index: Integer): TSMGrouping;
begin
  Result := TSMGrouping(inherited Items[Index]);
end;

procedure TSMGroupings.SetGrouping(Index: Integer; Value: TSMGrouping);
begin
  Items[Index].Assign(Value);
end;


{ TSMDBGTitleHeight }
constructor TSMDBGTitleHeight.Create(AGrid: TSMDBGrid);
begin
  inherited Create;

  FGrid := AGrid;
end;

procedure TSMDBGTitleHeight.Assign(Source: TPersistent);
var
  th: TSMDBGTitleHeight;
begin
  inherited Assign(Source);

  if (Source is TSMDBGTitleHeight) then
  begin
    th := TSMDBGTitleHeight(Source);

    Kind := th.Kind;
    LineCount := th.LineCount;
    PixelCount := th.PixelCount;
  end;
end;

procedure TSMDBGTitleHeight.SetKind(Value: TSMDBGTitleHeightKind);
begin
  if (FKind <> Value) then
  begin
    FKind := Value;
    FGrid.SetTitlesHeight
  end;
end;

procedure TSMDBGTitleHeight.SetLineCount(Value: Integer);
begin
  if (FLineCount <> Value) then
  begin
    FLineCount := Value;
    FGrid.SetTitlesHeight
  end;
end;

procedure TSMDBGTitleHeight.SetPixelCount(Value: Integer);
begin
  if (FPixelCount <> Value) then
  begin
    FPixelCount := Value;
    FGrid.SetTitlesHeight
  end;
end;

{ TSMDBGrid }
constructor TSMDBGrid.Create(AOwner: TComponent);
var
  NewItem: TMenuItem;
  j
  {$IFDEF DELPHI_4_UP}
  , h
  {$ENDIF}
  : Integer;
  Bmp: TBitmap;
begin
  inherited Create(AOwner);

  FBands := TStringList.Create;
  FBandsFont := TFont.Create;
  FBandsFont.Assign(TitleFont);

  FAutoFitIsLocked := True;
  
  FGroupings := TSMGroupings.Create(Self);

  FGridStyle := TSMDBGridStyle.Create(Self);
  FGridStyle.OddColor := Color;
  FGridStyle.EvenColor := Color;

  FTitleHeight := TSMDBGTitleHeight.Create(Self);
  FTitleHeight.Kind := hkAuto;
  FTitleHeight.LineCount := 1;
  FTitleHeight.PixelCount := DefaultRowHeight;

  FFooterColor := clBtnFace;

  FRegistryKey := 'Software\Scalabium';
  FRegistrySection := 'SMDBGrid';

  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(HInstance, bmArrow);
    FMsIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmEdit);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmInsert);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmMultiDot);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmMultiArrow);
    FMsIndicators.AddMasked(Bmp, clWhite);

{    Bmp.Handle := LoadBitmap(hInstance, bmMultiDot);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBitmap(hInstance, bmMultiArrow);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBitmap(hInstance, bmMultiCheckBox);
    FMsIndicators.AddMasked(Bmp, clWhite);
}  finally
    Bmp.Free;
  end;
  FScrollHintWnd := THintWindow.Create(Self);
  FScrollHintWnd.Color := clInfoBk;
  FPressedCol := -1;

  FDBPopUpMenu := TPopUpMenu.Create(Self {AOwner});
  if not (csDesigning in ComponentState) then
  begin
    {$IFDEF DELPHI_4_UP}
    h := 0;
    {$ENDIF}
    for j := 0 to High(PopUpCaption) do
    begin
      NewItem := TMenuItem.Create(Self);
      NewItem.Caption := PopUpCaption[j];
      {$IFDEF DELPHI_4_UP}
      if NewItem.Caption = '-' then
        NewItem.ImageIndex := -1
      else
      begin
        NewItem.ImageIndex := h;
        Inc(h)
      end;
      {$ENDIF}

      case j of
        0: NewItem.OnClick := AppendClick;
        1: NewItem.OnClick := InsertClick;
        2: NewItem.OnClick := EditClick;
        3: NewItem.OnClick := DeleteClick;

        5: NewItem.OnClick := PrintClick;
        6: NewItem.OnClick := ExportClick;
        7: NewItem.OnClick := FilterClick;
        8: begin
             NewItem.OnClick := SearchClick;
             NewItem.ShortCut := ShortCut(Word('F'), [ssCtrl]);
           end;

       10: NewItem.OnClick := PostClick;
       11: NewItem.OnClick := CancelClick;
       12: begin
             NewItem.OnClick := RefreshClick;
             NewItem.ShortCut := ShortCut(VK_F5, []);
           end;

       15: NewItem.OnClick := SelectOneClick;
       16: NewItem.OnClick := SelectAllClick;
       18: NewItem.OnClick := UnSelectOneClick;
       19: NewItem.OnClick := UnSelectAllClick;

       21: NewItem.OnClick := SaveLayoutClick;
       22: NewItem.OnClick := RestoreLayoutClick;

       24: NewItem.OnClick := SetupGridClick;
      end;
      if j in [15, 16, 17, 18, 19] then
        FDBPopUpMenu.Items[14].Add(NewItem)
      else
        FDBPopUpMenu.Items.Add(NewItem);
    end;
  end;
//  PopUpMenu := FDBPopUpMenu;

  GetCheckBoxSize;
  FWidthOfIndicator := IndicatorWidth;

  FExOptions := [eoENTERlikeTAB, eoKeepSelection, eoStandardPopup, eoBLOBEditor, eoTitleWordWrap, eoFilterAutoApply];
  HScrollbar := True;
  VScrollbar := True;
//  ScrollBars := ssBoth;
//  Color := clInfoBk;

  {Filter editor}
  CmbFilter := TFilterComboBox.Create(Self);
  with CmbFilter do
  begin
    Parent := Self;
    Color  := FGridStyle.FilterColor;
    Ctl3D  := False;
    DropDownCount := 10;
    OnChange := CmbFilterChange;
    OnKeyDown := CmbFilterKeyDown;
    OnExit := CmbFilterExit;
{$IFDEF DELPHI_6_UP}
    OnSelect := CmbFilterClick;
    AutoComplete := True;
{$ENDIF}
    OnClick := CmbFilterClick;
    Visible := False;
    Width := 0;
    SendToBack;
  end;
end;

destructor TSMDBGrid.Destroy;
begin
  FreeStartOfSelect;

  if (eoAutoSave in ExOptions) then
    SaveGridToRegistry(Self, FRegistryKey, FRegistrySection);

  FScrollHintWnd.Free;
  FDBPopUpMenu.Free;
  FMsIndicators.Free;
  FGridStyle.Free;
  FTitleHeight.Free;

  FBands.Free;
  FGroupings.Free;
  FBandsFont.Free;

{$IFDEF FLATSCROLLBARS}
//  if Flat then
//    UninitializeFlatSB(Handle);
{$ENDIF}

  if (eoShowFilterBar in ExOptions) and
     Assigned(DataLink.DataSet) then
    DataLink.DataSet.Filtered := False;

  inherited Destroy;
end;

procedure TSMDBGrid.SetBands(Value: TStrings);
begin
  FBands.Assign(Value);

  if not (csLoading in ComponentState) then
    Invalidate
end;

procedure TSMDBGrid.SetBandsFont(Value: TFont);
begin
  FBandsFont.Assign(Value);

  if not (csLoading in ComponentState) then
    Invalidate
end;

procedure TSMDBGrid.SetGroupings(Value: TSMGroupings);
begin
  FGroupings.Assign(Value)
end;

procedure TSMDBGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if Assigned(DataLink) and
       (AComponent = DataLink.DataSet) then
      FreeStartOfSelect
  end;

  inherited Notification(AComponent, Operation);
end;

procedure TSMDBGrid.LinkActive(Value: Boolean);
var
  defRowHeight: Integer;
begin
  defRowHeight := DefaultRowHeight;

  inherited;

  DefaultRowHeight := defRowHeight
end;

procedure TSMDBGrid.FreeStartOfSelect;
begin
  if Assigned(StartOfSelect) and
     Assigned(DataLink.DataSet) then
  begin
    DataLink.DataSet.FreeBookmark(StartOfSelect);
    StartOfSelect := nil;
  end;
end;

procedure TSMDBGrid.SetGridStyle(Value: TSMDBGridStyle);
begin
  FGridStyle.Assign(Value);
end;

procedure TSMDBGrid.SetTitleHeight(Value: TSMDBGTitleHeight);
begin
  FTitleHeight.Assign(Value);
end;

procedure TSMDBGrid.SetFooterColor(Value: TColor);
begin
  if (FFooterColor <> Value) then
  begin
    FFooterColor := Value;
    LayoutChanged;
  end;
end;

procedure TSMDBGrid.SetFlat(Value: Boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    RecreateWnd;
//    Invalidate
  end;
end;

procedure TSMDBGrid.SetFooterHeight(Value: Integer);
begin
  if (FFooterHeight <> Value) then
  begin
    if Value < 0 then
      Value := 0;
    FFooterHeight := Value;
    LayoutChanged;
  end;
end;

function TSMDBGrid.CreateColumns: TDBGridColumns;
begin
  Result := TSMDBGridColumns.Create(Self, {$IFDEF DELPHI_5_UP} TSMDBColumn {$ELSE} TColumn {$ENDIF});
//  Result := inherited CreateColumns
end;

function TSMDBGrid.CreateEditor: TInplaceEdit;
//var
//  Background: TColor;
begin
  Result := inherited CreateEditor;

//  Background := Self.Color;
//  GetCellProps(SelectedIndex, Columns[SelectedIndex].Field, nil, Background, False);

  with TEdit(Result) do
  begin
    Color := Self.Color;
    OnChange := EditChanged;
  end;
end;

procedure TSMDBGrid.EditChanged(Sender: TObject);
begin
  if Assigned(FOnEditChange) then
    FOnEditChange(Self);
end;

procedure TSMDBGrid.TitleClick(Column: TColumn);
begin
  if (eoShowFilterBar in ExOptions) and
     CmbFilter.Visible then
    exit;

  inherited;

  if DataLink.Active and not Assigned(OnTitleClick) then
    SortGrid(Column);
end;

procedure TSMDBGrid.CalculateTotals(OnlySelected: Boolean {$IFDEF DELPHI_5_UP}=False{$ENDIF});
var
  ABookmark: TBookmark;
  i: Integer;
  ExistInfoForUpdate: Boolean;

procedure CalcRowTotal;
  var
    i:integer;
  begin
    with Datalink.Dataset do
    begin
      for i := 0 to Columns.Count - 1 do
      begin
        if (Columns[i] is TSMDBColumn) and
          Assigned(Columns[i].Field) then
        begin
          with TSMDBColumn(Columns[i]) do
          begin
            case FooterType of
              ftSum,
              ftAverage: begin
                           if (Field is TNumericField) then
                             FFooterValue := FFooterValue + TNumericField(Field).AsFloat;
                           Inc(intInternalCount);
                         end;
              ftCount: FFooterValue := FFooterValue + 1;
              ftMin: if FFooterValue > Field.Value then
                       FFooterValue := Field.Value;
              ftMax: if FFooterValue < Field.Value then
                       FFooterValue := Field.Value;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  with Datalink.Dataset do
  begin
    {initialize all footer values}
    ExistInfoForUpdate := False;
    for i := 0 to Columns.Count-1 do
    begin
      if (Columns[i] is TSMDBColumn) then
      begin
        with TSMDBColumn(Columns[i]) do
        begin
          case FooterType of
            ftSum,
            ftCount,
            ftAverage,
            ftMin,
            ftMax: begin
                     FFooterValue := 0;
                     intInternalCount := 0;
                     ExistInfoForUpdate := True;
                   end;
          end
        end
      end
    end;

    if (BOF and EOF) or not ExistInfoForUpdate then Exit;
    DisableControls;
    try
      ABookmark := GetBookmark;
      try
        First;
        if OnlySelected then
        begin
          for i:= 0 to SelectedRows.Count-1 do
          begin
            GotoBookmark(Pointer(SelectedRows[i]));
            CalcRowTotal;
          end;
        end
        else
        begin
          while not EOF do
          begin
            CalcRowTotal;
            Next;
          end
        end;
      finally
        try
          GotoBookmark(ABookmark);
        except
        end;
        FreeBookmark(ABookmark);
      end;

      {update an avg value}
      for i := 0 to Columns.Count-1 do
      begin
        if (Columns[i] is TSMDBColumn) then
        begin
          with TSMDBColumn(Columns[i]) do
          begin
            case FooterType of
              ftAverage: begin
                           if (intInternalCount > 0) then
                             FFooterValue := FFooterValue/intInternalCount;
                         end;
            end
          end
        end
      end  
    finally
      EnableControls;
    end;
  end;
  InvalidateFooter
end;

procedure TSMDBGrid.InvalidateFooter;
var
  FooterRect: TRect;
begin
  if not Showing then Exit;  // do not try to paint if it's hidden

  FooterRect := GetFooterRect;
  SendMessage(Handle, WM_NCPaint, 0, 0);
//  InvalidateRect(Handle, @FooterRect, False);
end;

function TSMDBGrid.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;

  if (eoShowFooter in ExOptions) then
  begin
    if (FooterHeight = 0) then
      Result.Bottom := Result.Bottom - (DefaultRowHeight+4)
    else
      Result.Bottom := Result.Bottom - FooterHeight;
  end
end;

function TSMDBGrid.GetFooterRect: TRect;
var
  FooterRect: TRect;
begin
//  FooterRect := CellRect(0, RowCount - 1);
  FooterRect.Left := 0;
  FooterRect.Right := ClientWidth;
  FooterRect.Top := ClientRect.Bottom;
  if (FooterHeight = 0) then
    FooterRect.Bottom := FooterRect.Top + DefaultRowHeight + 3
  else
    FooterRect.Bottom := FooterRect.Top + FooterHeight - 1;
  Result := FooterRect;
//  Result := Rect(0, ClientRect.Bottom, ClientWidth, ClientRect.Bottom+50);
end;

procedure TSMDBGrid.DoUpdateFooter;
begin
  if Assigned(FOnUpdateFooter) then
    FOnUpdateFooter(Self);
end;

function TSMDBGrid.GetDefaultRowHeight: Integer;
Begin
  Result := inherited DefaultRowHeight;
end;

procedure TSMDBGrid.SetDefaultRowHeight(Value: Integer);
var
  i: Integer;
begin
  if (Value = inherited DefaultRowHeight) then
    exit;

  i := Value;
  {save the height for header}
  if (RowCount > 0) then
    i := RowHeights[0];
  if Value = 0 then
    Value := inherited DefaultRowHeight;
  inherited DefaultRowHeight := Value;

  if (RowCount > 0) then
  begin
    if HandleAllocated and (dgTitles in Options) then
    begin
      try
        {restore the saved height for header}
        RowHeights[0] := i;
      except
      end
    end
  end
end;

{thanks for original idea and draft implementation to Stephen Schaff <stephen.schaff@aruplab.com>}
procedure TSMDBGrid.SetRowHeight;
const
  WordBreaks: array [Boolean] of Integer = (0, DT_WORDBREAK);
var
  i, j, MaxHeight: Integer;
  RRect: TRect;
  oldActiveRecord: Integer;
  s: string;
begin
  if not (csDestroying in ComponentState) and
     Assigned(Parent) and
     (eoRowHeightAutofit in ExOptions) then
  begin
    if not DataLink.Active then exit;

    {save current position}
    oldActiveRecord := DataLink.ActiveRecord;
    DataLink.ActiveRecord := TopRow;
    j := 1;
    while not DataLink.DataSet.Eof and
          (DataLink.ActiveRecord <= TopRow+VisibleColCount) do
    begin
      {recalculate the row height}
      MaxHeight := 0;
      for i := 0 to Columns.Count-1 do
      begin
      {$IFDEF DELPHI_4_UP}
        if Columns[i].Visible then
      {$ENDIF}
        begin
          RRect := CellRect(0, 0);
          RRect.Right := Columns[i].Width - 1;
          RRect.Left := 0;
          RRect := CellRectForDraw(RRect, i);

          Canvas.Font := Columns[i].Font;
          s := Columns[i].Field.AsString;
          MaxHeight := Max(MaxHeight,
                           DrawText(Canvas.Handle,
                                    PChar(s),
                                    Length(s),
                                    RRect,
                                    DT_EXPANDTABS or DT_CALCRECT or WordBreaks[eoTitleWordWrap in ExOptions]));
        end;
      end;

      if (MaxHeight <> 0) then
      begin
        if (dgRowLines in Options) then
          Inc(MaxHeight, 3)
        else
          Inc(MaxHeight, 2);

        RowHeights[j] := MaxHeight + 2
      end;
      DataLink.ActiveRecord := DataLink.ActiveRecord+1;
      Inc(j);
    end;
    {restore position}
    DataLink.ActiveRecord := oldActiveRecord
  end;
end;

procedure TSMDBGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style or WS_TABSTOP;

{    if HScrollbar then
//      Style := Style and WS_HSCROLL
    else
      Style := Style and not WS_HSCROLL;

    if VScrollbar then
//      Style := Style and WS_VSCROLL
    else
      Style := Style and not WS_VSCROLL
}  end;
end;

procedure TSMDBGrid.CreateWnd;
{$IFDEF FLATSCROLLBARS}
const
  WSB_PROP_VSTYLE = $00000100;
  WSB_PROP_HSTYLE = $00000200;

  FSB_ENCARTA_MODE = 1;
  FSB_FLAT_MODE    = 2;

  WSB_PROP_VBKGCOLOR = $00000040;
  WSB_PROP_HBKGCOLOR = $00000080;

var
  XVerScrollInfo, XHorScrollInfo: TScrollInfo;
{$ENDIF}
begin
  inherited;

{$IFDEF FLATSCROLLBARS}
  if Flat then
  begin
    InitializeFlatSB(Handle);

    GetScrollInfo(Handle, SB_VERT, XVerScrollInfo);
    GetScrollInfo(Handle, SB_HORZ, XHorScrollInfo);

    FlatSB_SetScrollInfo(Handle, SB_VERT, XVerScrollInfo, False{True});
    FlatSB_SetScrollInfo(Handle, SB_HORZ, XHorScrollInfo, False{True});

    FlatSB_SetScrollProp(Handle, WSB_PROP_VSTYLE, FSB_FLAT_MODE, False{True});
    FlatSB_SetScrollProp(Handle, WSB_PROP_HSTYLE, FSB_FLAT_MODE, False{True});
//    FlatSB_SetScrollProp(Handle, WSB_PROP_VSTYLE, FSB_ENCARTA_MODE, False{True});
//    FlatSB_SetScrollProp(Handle, WSB_PROP_HSTYLE, FSB_ENCARTA_MODE, False{True});

//    FlatSB_SetScrollProp(Handle, WSB_PROP_VBKGCOLOR, clGreen, False{True});
//    FlatSB_SetScrollProp(Handle, WSB_PROP_HBKGCOLOR, clBlue, False{True});
  end
   else
     UninitializeFlatSB(Handle);
{$ENDIF}
end;

function TSMDBGrid.GetScrollBars: TScrollStyle;
begin
  Result := inherited ScrollBars
end;

procedure TSMDBGrid.SetScrollBars(Value: TScrollStyle);
begin
  HScrollbar := (Value in [ssBoth, ssHorizontal]);
  VScrollbar := (Value in [ssBoth, ssVertical]);

  inherited ScrollBars := Value;

  RecreateWnd;
end;

procedure TSMDBGrid.WMNCCalcSize(var msg: TMessage);
var
  Style: Integer;
begin
  if eoAutoWidth in ExOptions then
  begin
    Style := GetWindowLong(Handle, GWL_STYLE) and not WS_HSCROLL;
    SetWindowLong(Handle, GWL_STYLE, Style);
  end;

inherited;
exit;

  Style := GetWindowLong(Handle, GWL_STYLE);

  if not HScrollbar and ((Style and WS_HSCROLL) <> 0) then
    Style := Style and not WS_HSCROLL
  else
  if HScrollbar and ((Style and WS_HSCROLL) = 0) then
    Style := Style and WS_HSCROLL;

  if not VScrollbar and ((Style and WS_VSCROLL) <> 0) then
    Style := Style and not WS_VSCROLL
  else
  if VScrollbar and ((Style and WS_VSCROLL) = 0) then
    Style := Style and WS_VSCROLL;

  SetWindowLong(Handle, GWL_STYLE, Style);

  inherited;
end;

procedure TSMDBGrid.WMNCPaint(var Message: TMessage);
var
  DrawBitmap: TBitmap;
  FooterRect, TempRect, FooterCellRect, LastFooterCellRect: TRect;
  ACol: Integer;
  ACanvas: TCanvas;


  function GetFooterCellRect(Col: Integer): TRect;
  var
    FooterCellRect: TRect;
  begin
    FooterCellRect := CellRect(ACol, RowCount-1);
    if FooterCellRect.Left <> FooterCellRect.Right then
    begin
      FooterCellRect.Top := FooterRect.Top;
      FooterCellRect.Bottom := FooterRect.Bottom;
    end;
    Result := FooterCellRect;
  end;


  procedure DrawFooterLines(Rect: TRect; Col, Row: Integer);
  begin
    with ACanvas do
    begin
      Pen.Color := clBtnShadow;
      MoveTo(Rect.Left + 1, Rect.Bottom - 2);
      LineTo(Rect.Left + 1, Rect.Top + 1);
      LineTo(Rect.Right - 1, Rect.Top + 1);

      Pen.Color := clBtnHighlight;
      MoveTo(Rect.Left + 2, Rect.Bottom - 2);
      LineTo(Rect.Right - 1, Rect.Bottom - 2);
      LineTo(Rect.Right - 1, Rect.Top + 1);
    end
  end;

  procedure ProcessFooterCell(ACol: Integer);
  var
    strFooter: string;
    {$IFNDEF DELPHI_4_UP}
    i: Integer;
    {$ENDIF}
    BCol: Integer;
    DefaultDrawing: Boolean;
    al: TAlignment;
  begin
    if dgIndicator in Options then
      BCol := ACol - 1
    else
      BCol := ACol;

    FooterCellRect := CellRect(ACol, RowCount-1);
    if (BCol >-1) and
       (FooterCellRect.Left <> FooterCellRect.Right) then
    begin
      if Columns[BCol] is TSMDBColumn then
        with TSMDBColumn(Columns[BCol]) do
        begin
          strFooter := VarToStr(FooterValue);
          if FFooterText <> '' then
          begin
    {$IFDEF DELPHI_4_UP}
            strFooter := StringReplace(FFooterText, '%', strFooter, [rfReplaceAll])
    {$ELSE}
            i := Pos('%', FFooterText);
            if (i > 0) then
              strFooter := Copy(FFooterText, 1, i-1) + strFooter + Copy(FFooterText, i+1, Length(FFooterText)-i);
    {$ENDIF}
          end;
          al := Alignment
        end
      else
      begin
        strFooter := '';
        al := taLeftJustify
      end;
      DefaultDrawing := True;
      ACanvas.Brush.Color := FFooterColor;
      ACanvas.Font.Assign(Font);

      FooterCellRect := GetFooterCellRect(ACol);
      InflateRect(FooterCellRect, 0, -2);

      if Assigned(FOnDrawFooterCell) then
      begin
        FOnDrawFooterCell(Self, ACanvas, FooterCellRect,
                          Columns[BCol].Field, strFooter, DefaultDrawing);
      end;

      if not DefaultDrawing or (strFooter = '') then exit;

      {Fill with brush color for cell}
      if (GridStyle.Footer.Direction = fdNone) then
        ACanvas.FillRect(FooterCellRect)
      else
        SMDrawGradient(ACanvas, FooterCellRect, GridStyle.Footer.StartColor, GridStyle.Footer.EndColor, GridStyle.Footer.Direction, 255);

      FooterCellRect.Top := FooterRect.Top + 1;
      FooterCellRect.Bottom := FooterRect.Bottom - 1;
      FooterCellRect.Left := FooterCellRect.Left - 1;
      DrawFooterLines(FooterCellRect, ACol, RowCount);

      FooterCellRect.Top := FooterCellRect.Top + 2;
      FooterCellRect.Bottom := FooterRect.Bottom - 4;
      FooterCellRect.Left := FooterCellRect.Left + 2;
      FooterCellRect.Right := FooterCellRect.Left + ColWidths[ACol] - 4;

{      FooterCellRect.Top := FooterCellRect.Top +
                            (FooterCellRect.Bottom - FooterCellRect.Top - ACanvas.TextHeight('Ag')) div 2;
      FooterCellRect.Left := FooterCellRect.Left + 5;
      FooterCellRect.Right := FooterCellRect.Left + ColWidths[ACol] - 9;
}
      WriteTitleText(ACanvas, FooterCellRect, 0, 0, strFooter, al,
                     teNone,
                     False
                     {$IFDEF DELPHI_4_UP},
                     (BiDiMode <> bdLeftToRight)
//                     IsRightToLeft//UseRightToLeftAlignmentForField(Columns[BCol].Field, Columns[BCol].Alignment)
                     {$ENDIF})
    end
  end;
  
begin
  inherited;

  if not (eoShowFooter in ExOptions) then exit;

  DrawBitmap := TBitmap.Create;
  ACanvas := DrawBitmap.Canvas;
  with DrawBitmap do
  begin
    Width := Self.Width;
    Height := Self.Height;
  end;

  for ACol := LeftCol to ColCount-1 do
  begin
    FooterCellRect := CellRect(ACol, RowCount-1);
    if FooterCellRect.Left = FooterCellRect.Right then
      break;
    LastFooterCellRect := FooterCellRect;
  end;

  FooterRect := GetFooterRect;
  FooterRect.Top := FooterRect.Top - 10;//1;
  FooterRect.Bottom := FooterRect.Bottom + 1;
  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(FooterRect);

  {Fill with Footer color, up to right-most visible cell}
  ACanvas.Brush.Color := FFooterColor;
  FooterRect := GetFooterRect;
  FooterRect.Right := LastFooterCellRect.Right + 1;
  ACanvas.Pen.Color := clBlack;
  ACanvas.MoveTo(FooterRect.Left, FooterRect.Top - 1);
  ACanvas.LineTo(FooterRect.Right, FooterRect.Top - 1);
  ACanvas.LineTo(FooterRect.Right, FooterRect.Bottom + 1);
  ACanvas.FillRect(FooterRect);

  ACanvas.Pen.Color := clWhite;
  ACanvas.MoveTo(FooterRect.Left, FooterRect.Top);
  ACanvas.LineTo(FooterRect.Right, FooterRect.Top);
  ACanvas.MoveTo(FooterRect.Left, FooterRect.Top);
  ACanvas.LineTo(FooterRect.Left, FooterRect.Bottom);
  ACanvas.Pen.Color := clBlack;
  ACanvas.MoveTo(FooterRect.Left, FooterRect.Bottom);
  ACanvas.LineTo(FooterRect.Right, FooterRect.Bottom);

  if Datalink.Active then
  begin
    for ACol := IndicatorOffset to (inherited FixedCols)-IndicatorOffset do
      ProcessFooterCell(ACol);
    for ACol := LeftCol to ColCount-1 do
      ProcessFooterCell(ACol);
  end;

  FooterRect := GetFooterRect;
  FooterRect.Top := FooterRect.Top - 10;
  FooterRect.Bottom := FooterRect.Bottom + 1;

  TempRect := FooterRect;
  Canvas.CopyRect(TempRect, ACanvas, TempRect);
//  Canvas.StretchDraw(TempRect, DrawBitmap);

  DrawBitmap.Free;
end;

procedure TSMDBGrid.WMSize(var Message: TWMSize);
begin
  inherited;

  if not (csLoading in ComponentState) then
  begin
    Invalidate;
    InvalidateFooter
  end;
end;

procedure TSMDBGrid.DrawGridBackground;
begin
  if GridStyle.Background.Empty then
  begin
    if (GridStyle.Wallpaper.Direction <> fdNone) then
      SMDrawGradient(Canvas, ClientRect, GridStyle.Wallpaper.StartColor, GridStyle.Wallpaper.EndColor, GridStyle.Wallpaper.Direction, 255)
  end
  else
  begin
    Canvas.Brush.Bitmap := FGridStyle.Background;
    Canvas.FillRect(ClientRect);
    Canvas.Brush.Bitmap := nil;
  end;

  if Assigned(OnDrawBackground) then
    OnDrawBackground(Self)
end;

procedure TSMDBGrid.Paint;
var
  ARect: TRect;
begin
{  if ScrollBars in [ssNone, ssHorizontal] then
    SetScrollRange(Self.Handle, SB_VERT, 0, 0, False);
  if ScrollBars in [ssNone, ssVertical] then
    SetScrollRange(Self.Handle, SB_HORZ, 0, 0, False);
}

  if not (csLoading in ComponentState) then
    DrawGridBackground;

  inherited Paint;
  
  if not Assigned(Datalink) or not Assigned(Datalink.Dataset) or
     not Datalink.Dataset.Active then
  begin
    ARect := ClientRect;
    DrawText(Canvas.Handle, PChar(SNoDataToDisplay), -1, ARect,
               DT_CENTER or DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE);
  end
end;

procedure TSMDBGrid.WMHScroll(var Message: TWMHSCroll);
{$IFDEF FLATSCROLLBARS}
var
  XHorScrollInfo: TScrollInfo;
{$ENDIF}
begin
  inherited;

{20050714}
  if (Groupings.Count > 0) or
     (GridStyle.Style <> gsNormal) or
     Assigned(GridStyle.Background) or
     (GridStyle.Wallpaper.Direction <> fdNone) then
    Invalidate;

{$IFDEF FLATSCROLLBARS}
  if Flat then
  begin
    GetScrollInfo(Handle, SB_HORZ, XHorScrollInfo);
    FlatSB_SetScrollInfo(Handle, SB_HORZ, XHorScrollInfo, True);
  end;
{$ENDIF}
end;

{transfered from TGXDBGrid component, gnunn@GExperts.com}
procedure TSMDBGrid.WMVScroll(var Message: TWMVSCroll);
var
  HintTxt: string;
  pt: TPoint;
  rHint: TRect;
{$IFDEF FLATSCROLLBARS}
  XVerScrollInfo: TScrollInfo;
{$ENDIF}
begin
  try
    if (Message.ScrollCode = SB_THUMBTRACK) then
      Message.ScrollCode := SB_THUMBPOSITION;
    inherited;

{$IFDEF FLATSCROLLBARS}
    if Flat then
    begin
      GetScrollInfo(Handle, SB_VERT, XVerScrollInfo);
      FlatSB_SetScrollInfo(Handle, SB_VERT, XVerScrollInfo, True);
    end;
{$ENDIF}

    if not (csDesigning in ComponentState) then
    begin
      if (eoShowRecNo in ExOptions) and
         Assigned(DataLink.DataSet) and
         (DataLink.DataSet.RecNo > 0) then
      begin
        FScrollHintWnd.ReleaseHandle;
        HintTxt := SRecNo +
          IntToStr(DataLink.DataSet.RecNo) +
          SRecOf +
          IntToStr(GetRecordCount);
        GetCursorPos(pt);
        rHint := FScrollHintWnd.CalcHintRect(100, HintTxt, nil);
        OffsetRect(rHint,
                   pt.x + GetSystemMetrics(SM_CXCURSOR) div 2,
                   pt.y + GetSystemMetrics(SM_CYCURSOR) div 2);

        {prevent from display when mouse wheel scrolling etc...}
        if (GetKeyState(VK_LBUTTON) and $80) = $80 then
          FScrollHintWnd.ActivateHint(rHint, HintTxt)
        else
          FScrollHintWnd.ReleaseHandle;
      end;
    end;
  finally
  end;
end;
{end of transfer}

{Standard popup menu events}
procedure TSMDBGrid.AppendClick(Sender: TObject);
begin
  if Assigned(FOnAppendRecord) then
    FOnAppendRecord(Self)
  else
    Datalink.DataSet.Append;
end;

procedure TSMDBGrid.InsertClick(Sender: TObject);
begin
  if Assigned(FOnInsertRecord) then
    FOnInsertRecord(Self)
  else
    Datalink.DataSet.Insert;
end;

procedure TSMDBGrid.frmBlobEditResize(Sender: TObject);
var
  intClientWidth, intClientHeight, intBtnTop, intBtnWidth: Integer;
begin
  with TForm(Sender) do
  begin
    intClientWidth := ClientWidth;
    intClientHeight := ClientHeight;

    with frmBlobEdit.Controls[0] do
    begin
      Left := intClientWidth - 2*Width - 10;
      intBtnTop := intClientHeight - Height - 5;
      Top := intBtnTop;
    end;

    with frmBlobEdit.Controls[1] do
    begin
      Left := intClientWidth - Width - 5;
      Top := intBtnTop;
    end;

    with frmBlobEdit.Controls[2] do
    begin
      Width := intClientWidth - 10;
      Height := intBtnTop - 15;
    end;

    with TPanel(frmBlobEdit.Controls[3]) do
    begin
      Width := intClientWidth - 10;
      Height := intBtnTop - 15;
      intClientWidth := ClientWidth;

      intBtnWidth := Controls[1].Width + 5;
      Controls[0].Width := intClientWidth - intBtnWidth - 10;
      Controls[0].Height := ClientHeight - 10;
      Controls[1].Left := intClientWidth - intBtnWidth;
      Controls[2].Left := intClientWidth - intBtnWidth;
      Controls[3].Left := intClientWidth - intBtnWidth;
      Controls[4].Left := intClientWidth - intBtnWidth;
      Controls[5].Left := intClientWidth - intBtnWidth;
    end;
  end;
end;

procedure TSMDBGrid.LoadImage(Sender: TObject);
begin
  with TOpenPictureDialog.Create(Application) do
    try
      if Execute then
        with TImage(TPanel(frmBlobEdit.Controls[3]).Controls[0]) do
          Picture.LoadFromFile(FileName);
    finally
      Free
    end;
end;

procedure TSMDBGrid.SaveImage(Sender: TObject);
begin
  with TSavePictureDialog.Create(Application) do
    try
      if Execute then
        with TImage(TPanel(frmBlobEdit.Controls[3]).Controls[0]) do
          Picture.SaveToFile(FileName);
    finally
      Free
    end;
end;

procedure TSMDBGrid.ImageToClipboard(Sender: TObject);
begin
  with TImage(TPanel(frmBlobEdit.Controls[3]).Controls[0]) do
    if Picture.Graphic <> nil then
      Clipboard.Assign(Picture);
end;

procedure TSMDBGrid.ClipboardToImage(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_BITMAP) then
    with TImage(TPanel(frmBlobEdit.Controls[3]).Controls[0]) do
      Picture.Bitmap.Assign(Clipboard);
end;

procedure TSMDBGrid.ClearImage(Sender: TObject);
begin
  with TImage(TPanel(frmBlobEdit.Controls[3]).Controls[0]) do
    Picture.Bitmap.Assign(nil);
end;

procedure TSMDBGrid.ClearSort;
var
  i: Integer;
begin
  for i := 0 to Columns.Count-1 do
    if Columns[i] is TSMDBColumn then
      TSMDBColumn(Columns[i]).SortType := stNone;
end;

procedure TSMDBGrid.SetSortField(AField: TField; ASortType: TSMSortType);
var
  i: Integer;
begin
  if not Assigned(AField) then exit;

  for i := 0 to Columns.Count-1 do
    if Columns[i] is TSMDBColumn then
      with TSMDBColumn(Columns[i]) do
      if Assigned(Field) then
      begin
        if CompareText(AField.FieldName, FieldName) = 0 then
          SortType := ASortType
        else
          SortType := stNone;
      end
end;

procedure TSMDBGrid.ShowBlobEditor;
var
  memo: TMemo;
  pnl: TPanel;
begin
  if (SelectedIndex >= FixedCols) and
     (SelectedIndex > -1) and
     Assigned(Columns[SelectedIndex]) then
  begin
    with Columns[SelectedIndex] do
      if Assigned(Field) and ((Field.IsBlob) or (Field.Size > 256)) {and
         (not ReadOnly) and Field.CanModify} then
      begin
        try
          if not Assigned(frmBlobEdit) then
          begin
            frmBlobEdit := TForm.Create(Application);

            with frmBlobEdit do
            begin
              Position := poScreenCenter;
//              BorderStyle := bsSizeToolWin;
              BorderIcons := BorderIcons - [biMinimize];

              with TButton.Create(frmBlobEdit) do
              begin
                Caption := SBtnOk;
                Default := True;
                Parent := frmBlobEdit;
                ModalResult := mrOk;
                Enabled := (not ReadOnly) and Field.CanModify;
              end;

              with TButton.Create(frmBlobEdit) do
              begin
                Caption := SBtnCancel;
                Cancel := True;
                Parent := frmBlobEdit;
                ModalResult := mrCancel;
              end;

              with TMemo.Create(frmBlobEdit) do
              begin
                Left := 5;
                Top := 5;
                ScrollBars := ssBoth;
                Parent := frmBlobEdit;
                Visible := False
              end;

              pnl := TPanel.Create(frmBlobEdit);
              with pnl do
              begin
                Left := 5;
                Top := 5;
                Parent := frmBlobEdit;
                BevelInner := bvLowered;
                BevelOuter := bvRaised;
                Visible := False
              end;

              with TImage.Create(pnl) do
              begin
                Left := 5;
                Top := 5;
                Center := True;
                Parent := pnl;
              end;

              with TButton.Create(pnl) do
              begin
                Caption := SBtnLoad;
                Top := 5;
                Parent := pnl;
                OnClick := LoadImage;
              end;

              with TButton.Create(pnl) do
              begin
                Caption := SBtnSave;
                Top := 10 + Height;
                Parent := pnl;
                OnClick := SaveImage;
              end;

              with TButton.Create(pnl) do
              begin
                Caption := SBtnCopy;
                Top := 5 + 2*(5 + Height);
                Parent := pnl;
                OnClick := ImageToClipboard;
              end;

              with TButton.Create(pnl) do
              begin
                Caption := SBtnPaste;
                Top := 5 + 3*(5 + Height);
                Parent := pnl;
                OnClick := ClipboardToImage;
              end;

              with TButton.Create(pnl) do
              begin
                Caption := SBtnClear;
                Top := 5 + 4*(5 + Height);
                Parent := pnl;
                OnClick := ClearImage;
              end;
              OnResize := frmBlobEditResize;
            end;
          end;

          memo := TMemo(frmBlobEdit.Controls[2]);
          pnl := TPanel(frmBlobEdit.Controls[3]);
          if (TBlobField(Field).BlobType = ftGraphic) then
          begin
            memo.Visible := False;
            pnl.Visible := True;

            TImage(pnl.Controls[0]).Picture.Assign(TBlobField(Field));
          end
          else
          begin
            memo.Visible := True;
            pnl.Visible := False;

            try
              memo.Font.Assign(Font);
              memo.Lines.Text := Field.AsString;
            except
              { Memo too large }
              on E: EInvalidOperation do
                memo.Lines.Text := Format('(%s)', [E.Message]);
            end;
          end;
          frmBlobEdit.Caption := FieldName;
          if (frmBlobEdit.ShowModal = mrOk) then
          begin
            if not (Field.DataSet.State in [dsEdit, dsInsert]) then
              Field.DataSet.Edit;

            if (TBlobField(Field).BlobType = ftGraphic) then
              TBlobField(Field).Assign(TImage(pnl.Controls[0]).Picture.Graphic)
            else
              Field.AsString := memo.Lines.Text;
          end;
        finally
        end;
      end;
  end
end;

procedure TSMDBGrid.DblClick;
begin
  if Assigned(OnDblClick) then
    inherited DblClick
  else
  if (eoBLOBEditor in ExOptions) then
    ShowBlobEditor;
end;

procedure TSMDBGrid.EditClick(Sender: TObject);
begin
  if Assigned(FOnEditRecord) then
    FOnEditRecord(Self)
  else
    Datalink.DataSet.Edit;
end;

procedure TSMDBGrid.DeleteClick(Sender: TObject);
begin
  if Assigned(FOnDeleteRecord) then
    FOnDeleteRecord(Self)
  else
    DeleteData;
end;

procedure TSMDBGrid.PrintClick(Sender: TObject);
begin
  if Assigned(FOnPrintData) then
    FOnPrintData(Self)
end;

procedure TSMDBGrid.ExportClick(Sender: TObject);
begin
  if Assigned(FOnExportData) then
    FOnExportData(Self)
end;

procedure TSMDBGrid.SearchClick(Sender: TObject);
begin
  if Assigned(FOnSearchData) then
    FOnSearchData(Self)
end;

procedure TSMDBGrid.FilterClick(Sender: TObject);
begin
  if Assigned(FOnFilterData) then
    FOnFilterData(Self)
end;

procedure TSMDBGrid.PostClick(Sender: TObject);
begin
  if Assigned(FOnPostData) then
    FOnPostData(Self)
  else
    Datalink.DataSet.Post;
end;

procedure TSMDBGrid.CancelClick(Sender: TObject);
begin
  if Assigned(FOnCancelData) then
    FOnCancelData(Self)
  else
    Datalink.DataSet.Cancel;
end;

procedure TSMDBGrid.RefreshClick(Sender: TObject);
begin
  if Assigned(FOnRefreshData) then
    FOnRefreshData(Self)
  else
    RefreshData;
end;

procedure TSMDBGrid.SetupGridClick(Sender: TObject);
begin
  if Assigned(FOnSetupGrid) then
    FOnSetupGrid(Self)
end;

function TSMDBGrid.GetImageIndex(Field: TField): Integer;
var
  AOnGetText: TFieldGetTextEvent;
  AOnSetText: TFieldSetTextEvent;
begin
  Result := -1;
  if (eoShowGlyphs in FExOptions) and Assigned(Field) then
  begin
    if (not ReadOnly) and Field.CanModify then
    begin
      { Allow editing of memo fields if OnSetText and OnGetText
        events are assigned }
      AOnGetText := Field.OnGetText;
      AOnSetText := Field.OnSetText;
      if Assigned(AOnSetText) and Assigned(AOnGetText) then Exit;
    end;
    case Field.DataType of
      ftBytes, ftVarBytes, ftBlob: Result := Integer(gpBlob);
      ftMemo: Result := Integer(gpMemo);
      ftGraphic: Result := Integer(gpPicture);
      ftTypedBinary: Result := Integer(gpBlob);
      ftFmtMemo: Result := Integer(gpMemo);
      ftParadoxOle, ftDBaseOle: Result := Integer(gpOle);
    end;
  end;
end;

function TSMDBGrid.ActiveRowSelected: Boolean;
var
  Index: Integer;
begin
  Result := False;
  if Datalink.Active and ((dgMultiSelect in Options) or (eoCheckBoxSelect in ExOptions)) then
    Result := SelectedRows.Find(Datalink.DataSet.Bookmark, Index);
end;

function TSMDBGrid.HighlightCell(DataCol, DataRow: Integer;
  const Value: string; AState: TGridDrawState): Boolean;
begin
  Result := ActiveRowSelected;
  if not Result then
    Result := inherited HighlightCell(DataCol, DataRow, Value, AState);
end;

procedure TSMDBGrid.ToggleRowSelection;
var
  WasSelected: Boolean;
begin
  if Datalink.Active and ((dgMultiSelect in Options) or (eoCheckBoxSelect in ExOptions)) then
  begin
    if not (dgMultiSelect in Options) then
    begin
      WasSelected := SelectedRows.CurrentRowSelected;
      SelectedRows.Clear;
      if WasSelected then
        SelectedRows.CurrentRowSelected := True;
    end;

    with SelectedRows do
      CurrentRowSelected := not CurrentRowSelected;
    if Assigned(FOnChangeSelection) then
      FOnChangeSelection(Self);
  end;
end;

procedure TSMDBGrid.GotoSelection(Index: Longint);
begin
  if (dgMultiSelect in Options) and DataLink.Active and (Index < SelectedRows.Count) and (Index >= 0) then
    Datalink.DataSet.GotoBookmark(Pointer(SelectedRows[Index]));
end;

function TSMDBGrid.CalcTitleRect1(Col: TColumn; ARow: Integer;
  var MasterCol: TColumn): TRect;
{$IFDEF DELPHI_5_UP}
{$ELSE}
var
  I{, J}: Integer;
//  InBiDiMode: Boolean;
//  DrawInfo: TGridDrawInfo;
{$ENDIF}

begin
{$IFDEF DELPHI_5_UP}
  Result := CalcTitleRect(Col, ARow, MasterCol);
{$ELSE}
  I := DataToRawColumn(MasterCol.Index);
  Result := CellRect(I, ARow);
{$ENDIF}
exit;

{  MasterCol := ColumnAtDepth(Col, ARow);
  if MasterCol = nil then Exit;

  I := DataToRawColumn(MasterCol.Index);
  if I >= LeftCol then
    J := MasterCol.Depth
  else
  begin
    I := LeftCol;
    if Col.Depth > ARow then
      J := ARow
    else
      J := Col.Depth;
  end;

  Result := CellRect(I, J);

  InBiDiMode := UseRightToLeftAlignment and
                (Canvas.CanvasOrientation = coLeftToRight);

  for I := Col.Index to Columns.Count-1 do
  begin
    if ColumnAtDepth(Columns[I], ARow) <> MasterCol then Break;
    if not InBiDiMode then
    begin
      J := CellRect(DataToRawColumn(I), ARow).Right;
      if J = 0 then Break;
      Result.Right := Max(Result.Right, J);
    end
    else
    begin
      J := CellRect(DataToRawColumn(I), ARow).Left;
      if J >= ClientWidth then Break;
      Result.Left := J;
    end;
  end;
  J := Col.Depth;
  if (J <= ARow) and (J < FixedRows-1) then
  begin
    CalcFixedInfo(DrawInfo);
    Result.Bottom := DrawInfo.Vert.FixedBoundary - DrawInfo.Vert.EffectiveLineWidth;
  end;
}end;

function TSMDBGrid.GetBandRect(ACol: Integer): TRect;
var
  MasterCol: TColumn;
  ColIndex, Shift, Count, i, intBandIndex: Integer;
  r: TRect;
begin
  if [dgColLines] * Options = [dgColLines] then
    Shift := 1
  else
    Shift := 0;

  ColIndex := ACol;
  Count := 1;
  if (Columns[ACol] is TSMDBColumn) then
    intBandIndex := TSMDBColumn(Columns[ACol]).BandIndex
  else
    intBandIndex := -1;
  if (intBandIndex > -1) then
  begin
{
    ColIndex := -1;
    Count := 0;
    for i := 0 to Columns.Count-1 do
      if (Columns[i] is TSMDBColumn) then
      begin
        with TSMDBColumn(Columns[i]) do
          if (BandIndex = intBandIndex) then
          begin
            if ColIndex < 0 then
              ColIndex := i;
            Inc(Count);
          end
      end
      else
        break;
}

    while (ColIndex > 0) and
          (Columns[ColIndex-1] is TSMDBColumn) and
          (TSMDBColumn(Columns[ColIndex-1]).BandIndex = intBandIndex) do
    begin
      Dec(ColIndex);
      Inc(Count);
    end;
    i := ACol+1;
    while (i < Columns.Count) and
          (Columns[i] is TSMDBColumn) and
          (TSMDBColumn(Columns[I]).BandIndex = intBandIndex) do
    begin
      Inc(Count);
      Inc(i);
    end;
  end;

  if ColIndex + Count > Columns.Count then
  begin
    ColIndex := ACol;
    Count := 1;
  end;

  if (ColIndex < Columns.Count) then
    Result := CalcTitleRect1(Columns[ColIndex], 0, MasterCol)
  else
    Result := Rect(0, 0, 0, 0);
  for i := ColIndex + 1 to ColIndex + Count - 1 do
    if (i < Columns.Count) then
    begin
      r := CalcTitleRect1(Columns[i], 0, MasterCol);
      Result.Right := Result.Right + r.Right - r.Left + Shift;
    end;
end;

{an original idea found in TBitDBGrid:
 Ilya Andreev, ilya_andreev@geocities.com
 FIDONet: 2:5030/55.28 AKA 2:5030/402.17}
procedure TSMDBGrid.SetTitlesHeight;
const
  WordBreaks: array [Boolean] of Integer = (0, DT_WORDBREAK);
var
  i, MaxHeight: Integer;
  RRect: TRect;
  pt: Integer;
  s: string;
begin
  if not (csDestroying in ComponentState) and
     Assigned(Parent) and (dgTitles in Options) then
  begin
    {recalculate a title height}
    MaxHeight := 0;
    if (TitleHeight.Kind = hkAuto) then
    begin
      for i := 0 to Columns.Count-1 do
      begin
        RRect := CellRect(0, 0);
        RRect.Right := Columns[i].Width - 1;
        RRect.Left := 0;
        RRect := CellRectForDraw(RRect, i);

        Canvas.Font := Columns[i].Title.Font;

        s := Columns[i].Title.Caption;
        pt := Pos('|', s);
        if pt > 0 then
        begin
          while pt <> 0 do
          begin
            s[pt] := #13;
            pt := Pos('|', s);
          end;
          Columns[i].Title.Caption := s;
        end;

        MaxHeight := Max(MaxHeight, DrawText(Canvas.Handle,
                         PChar(s),
                         Length(s),
                         RRect,
                         DT_EXPANDTABS or DT_CALCRECT or WordBreaks[eoTitleWordWrap in ExOptions]));
      end;
    end
    else
    if (TitleHeight.Kind = hkLineCount) then
    begin
      Canvas.Font := TitleFont;
      MaxHeight := Canvas.TextHeight('W')*TitleHeight.LineCount;
    end
    else
    if (TitleHeight.Kind = hkPixelCount) then
    begin
      MaxHeight := TitleHeight.FPixelCount
    end;

    if (MaxHeight <> 0) then
    begin
      if (dgRowLines in Options) then
        Inc(MaxHeight, 3)
      else
        Inc(MaxHeight, 2);
      if (eoTitleButtons in ExOptions) then
        Inc(MaxHeight, 2);

      {TODO: we must calculate a height for bands more correctly}
      if (eoDrawBands in ExOptions) and (Bands.Count > 0) and
         not (eoBandsOverTitles in ExOptions) then
        Inc(MaxHeight, DefaultRowHeight);
//        MaxHeight := 2*MaxHeight;

      if eoShowFilterBar in ExOptions then
        MaxHeight := MaxHeight + FILTERBAR_HEIGHT;

      if (RowCount > 0) then
        RowHeights[0] := MaxHeight + 2
    end;
  end;
end;

procedure TSMDBGrid.CMHintShow(var Msg: TMessage);
var
  ACol, ARow: Integer;
  OldActive: Integer;
  fld: TField;
begin
 if (eoCellHint in FExOptions) or
    (HintField <> '') then
   with PHintInfo(Msg.LParam)^  do
     try
       HintStr := Hint;

       Msg.Result := 1;
       if not DataLink.Active then Exit;
       TDrawGrid(Self).MouseToCell(CursorPos.X, CursorPos.Y, ACol, ARow);
       CursorRect := CellRect(ACol, ARow);
       ACol := ACol - IndicatorOffset;
       if (ACol < 0) then Exit;
       ARow := ARow - TitleOffset;
       HintPos := ClientToScreen(CursorRect.TopLeft);
       InflateRect(CursorRect, 1, 1);
       if (ARow = -1) then
       begin
         HintStr := Columns[ACol].Title.Caption;
         if Canvas.TextWidth(HintStr) < Columns[ACol].Width then Exit;
         Msg.Result := 0;
         Exit;
      end;
      if ARow < 0 then exit;
      OldActive := DataLink.ActiveRecord;
      DataLink.ActiveRecord := ARow;
      if HintField <> '' then
        fld := DataLink.DataSet.FindField(HintField)
      else
        fld := Columns[ACol].Field;
      if Assigned(fld) then
        if fld.IsBlob then
          HintStr := fld.AsString
        else
          HintStr := fld.DisplayText;
      if Assigned(OnCellHint) then
        OnCellHint(Self, Columns[ACol], HintStr, PHintInfo(Msg.LParam)^);
      DataLink.ActiveRecord := OldActive;
      if (((CursorRect.Right - CursorRect.Left) >=  Columns[ACol].Width) and
          (Canvas.TextWidth(HintStr) < Columns[ACol].Width)) or
         ((Canvas.TextWidth(HintStr) < (CursorRect.Right - CursorRect.Left)) and
          (Columns[ACol].Alignment = taLeftJustify)) then exit;
        Msg.Result := 0;
    except
      Msg.Result := 1;
    end;
end;
{end of derived}

procedure TSMDBGrid.UpdateColWidths1;
var
  i, intTotalWidth, intClientWidth{, FOffset}: Integer;
  coef: Double;
begin
  if (eoAutoWidth in ExOptions) and
     (ColCount > 0) and
     not (csDestroying in ComponentState) and
     not (csReading in ComponentState) and
     not (csLoading in ComponentState) and
    (not FAutoFitIsLocked) and
    (UpdateLock = 0) and (LayoutLock = 0) then
  begin
    FAutoFitIsLocked := True;
    try
      intTotalWidth := 0;
      intClientWidth := ClientWidth;

      {calculate a total width of columns}
      for i := 0 to ColCount-1 do
        Inc(intTotalWidth, (ColWidths[i] {+ GridLineWidth}));

      if (intClientWidth - intTotalWidth > 2) or
         (intClientWidth - intTotalWidth < 0) then
//      if (ABS(intTotalWidth - intClientWidth) > 5)then
      begin
        {calculate a coef}
        coef := intClientWidth/intTotalWidth;

        {recalc the positions and width of each column (except last! - on the last I'll put all approximation)}
//        FOffset := 0;
        for i := 0 to ColCount-1{2} do
        begin
          ColWidths[i] := Trunc(coef*ColWidths[i]);
//          Inc(FOffset, ColWidths[i] {+ GridLineWidth});
        end;
       {the full approximation in stretch mode I assign to last column}
//        ColWidths[ColCount-1] := intClientWidth - FOffset {- 2*GridLineWidth}
      end;
    finally
      FAutoFitIsLocked := False;
    end;
  end;
end;

function TSMDBGrid.ColumnIsCheckbox(AColumn: TColumn): Boolean;
begin
  if not Assigned(AColumn) or not Assigned(AColumn.Field) then
    Result := False
  else
    Result := ((AColumn.Field.DataType = ftBoolean) and
               (eoBooleanAsCheckbox in ExOptions)) or
              ((AColumn is TSMDBColumn) and
               (TSMDBColumn(AColumn).InplaceEditor = ieCheckbox))
end;

{Implemented by Stephen T. Schaff <Stephen.Schaff@aruplab.com>}
procedure TSMDBGrid.UpdateColWidths;
var
  i, intTotalWidth, intClientWidth{, FOffset}: Integer;
  coef: Double;
  NCMet: TNonClientMetrics;
  TempLen: Integer;
  OldFont: TFont;

  ArrayOfColumnsLen: array {$IFDEF DELPHI_5_UP} {$ELSE} [0..200] {$ENDIF} of Integer;
  StartPoint: Integer;

  { Used to allow titles that wrap to not make a column too big.
    To Do: Compare based on TextWidth rather than how many chars there are.
    the way it is now WWW would be shorter than llll.  Unless the
    charset is fixed that is not true. }
  function GetBiggestWord( Title: String): String;
  var
    BiggestCount: Integer;
    Count: Integer;
    BiggestWord: String;
  begin
    BiggestCount := 0;
    while Title <> '' do
    begin
      Count := Pos(' ', Title);
      if Count = 0 then
        Count := Length(Title);

      if Count > BiggestCount then
      begin
        BiggestCount := Count;
        BiggestWord := Copy(Title, 0, Count);
      end;

      Title := Copy(Title, Count+1, 99999);
    end;

    Result := BiggestWord;
  end;

begin
  if (eoAutoWidth in ExOptions) and
     (ColCount > 0) and
     not (csDestroying in ComponentState) and
     not (csReading in ComponentState) and
     not (csLoading in ComponentState) and
    (not FAutoFitIsLocked) and
    (UpdateLock = 0) and (LayoutLock = 0) then
  begin
    FAutoFitIsLocked := True;
    try
      // Initalize Length Array
      {$IFDEF DELPHI_5_UP}
      SetLength(ArrayOfColumnsLen, ColCount);
      {$ENDIF}
      for i := 0 to ColCount-1 do
        ArrayOfColumnsLen[i] := 0;

      // Find the optimal size of each column
      // Start with the column names
      OldFont := TFont.Create;
      try
        OldFont.Assign(Font);
//        Font.Assign(TitleFont);
        for i := 0 to Columns.Count-1 do
        begin
          Font.Assign(Columns[i].Title.Font);

          // Column Headers
          if (eoTitleWordWrap in ExOptions) then
            TempLen := Canvas.TextWidth(GetBiggestWord(Columns[i].Title.Caption))
          else
            TempLen := Canvas.TextWidth(Columns[i].Title.Caption);
          ArrayOfColumnsLen[i] := TempLen;
        end;
        Font.Assign(OldFont);
      finally
        OldFont.Free;
      end;

      // Now check the current (maybe should be first?) data value to make sure that it is
      //  not way different.  Only do this if we have a valid dataset.
      if not(csDesigning in ComponentState) then
      if (not( (DataSource.DataSet = nil) or (DataSource.DataSet.Active = False) ) )then
      begin
        for i := 0 to Columns.Count-1 do
        begin
          // If this is a binary field (and checkboxes are on) then make it small
          //  otherwise get the max len.
          if ColumnIsCheckbox(Columns[i]) then
            TempLen := 2
          else
            TempLen := Canvas.TextWidth(Columns[i].Field.AsString);
          if TempLen > ArrayOfColumnsLen[i] then
            ArrayOfColumnsLen[i] := TempLen;
        end;
      end;

      // Get the current metrics so we know the system scrollbar width and Border Width.
      FillChar(NCMet, SizeOf(NCMet), 0);
      NCMet.cbSize := SizeOf(NCMet);
      SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NCMet), @NCMet, 0);

      // Get the Client Width
      intClientWidth := Width - (NCMet.iBorderWidth * 2) - (GridLineWidth* (ColCount + 1));

      // Subtract the Scrollbar's width from the total width if there is a scrollbar.
      if (GetWindowlong(Self.Handle, GWL_STYLE) and WS_VSCROLL) <> 0 then
        intClientWidth := intClientWidth - NCMet.iScrollWidth;

      // Subtract the indicator if it is being used
      if dgIndicator in Options then
      begin
        StartPoint := 1;
        intClientWidth := intClientWidth - WidthOfIndicator;
      end
      else
        StartPoint := 0;

      {calculate a total width of columns}
      intTotalWidth := 0;
      for i := 0 to Columns.Count-1 do
        Inc(intTotalWidth, ArrayOfColumnsLen[i]);

      {calculate a coef}
      if intTotalWidth = 0 then
        coef := 1
      else
        coef := intClientWidth/intTotalWidth;

      // Apply the coef to the column widths
      for i := 0 to Columns.Count - 1 do
        ColWidths[i+StartPoint] := Trunc(coef*ArrayOfColumnsLen[i]);
    finally
      FAutoFitIsLocked := False;
    end;
  end;
end;

procedure TSMDBGrid.Loaded;
begin
  inherited Loaded;

  if (eoAutoLoad in ExOptions) then
    LoadGridFromRegistry(Self, FRegistryKey, FRegistrySection);

  FAutoFitIsLocked := False;
  if (eoAutoWidth in ExOptions) then
    UpdateColWidths;

   if FIsFlagAlwaysShowEditor then
   begin
     FIsFlagAlwaysShowEditor := False;
     Options := Options + [dgAlwaysShowEditor];
   end;
end;

procedure TSMDBGrid.LayoutChanged;
var
  ACol: Longint;
begin
  ACol := Col;

  inherited LayoutChanged;

  if Datalink.Active and (FixedCols > 0) then
    Col := Min(Max(inherited FixedCols, ACol), ColCount - 1);

  if (eoAutoWidth in ExOptions) then
    UpdateColWidths;

  {recalculate a title height}
  SetTitlesHeight;
  SetRowHeight;

  if (eoShowFooter in ExOptions) then
    InvalidateFooter;
end;

procedure TSMDBGrid.ColWidthsChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited ColWidthsChanged;

  if Datalink.Active and (FixedCols > 0) then
    Col := Min(Max(inherited FixedCols, ACol), ColCount - 1);

  if (eoAutoWidth in ExOptions) then
    UpdateColWidths;

  if Assigned(OnColWidthsChanged) then
    OnColWidthsChanged(Self)
end;

procedure TSMDBGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited SizeChanged(OldColCount, OldRowCount);

  if (eoAutoWidth in ExOptions) then
    UpdateColWidths;
end;

procedure TSMDBGrid.RowHeightsChanged;
var
  i: Integer;
begin
  inherited;

  if not Assigned(Parent) then exit;

  for i := TopRow to TopRow + VisibleRowCount + 1 do
    if (RowHeights[i] <> DefaultRowHeight) then
    begin
      if RowHeights[i] < Canvas.TextHeight('Wg') + 2 then
        RowHeights[i] := Canvas.TextHeight('Wg') + 2;
      if not (eoRowHeightAutofit in ExOptions) then
        DefaultRowHeight := RowHeights[i];
      break;
    end;
end;

procedure TSMDBGrid.SetIndicatorWidth(Value: Integer);
var
  FrameOffs: Byte;
begin
  if (Value <> FWidthOfIndicator) then
  begin
    if ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
      FrameOffs := 1
    else
      FrameOffs := 2;

    if (eoCheckBoxSelect in ExOptions) and
       (Value < FCheckWidth + 4*FrameOffs + FMsIndicators.Width) then
      Value := FCheckWidth + 4*FrameOffs + FMsIndicators.Width;

    if Value < IndicatorWidth then
      Value := IndicatorWidth;
    FWidthOfIndicator := Value;

    SetColumnAttributes
  end;
end;

procedure TSMDBGrid.SetColumnAttributes;
begin
  inherited SetColumnAttributes;

  if (dgIndicator in Options) then
    if (ColCount > 0) then
      ColWidths[0] := FWidthOfIndicator;

  SetFixedCols(FFixedCols);
end;

function TSMDBGrid.GetTitleOffset: Byte;
begin
  Result := 0;
  if dgTitles in Options then
    Inc(Result);
end;

procedure TSMDBGrid.SetFixedCols(Value: Integer);
var
  FixCount, i, intRow: Integer;
begin
  FixCount := Max(Value, 0) + IndicatorOffset;
  if DataLink.Active and not (csLoading in ComponentState) and
    (ColCount > IndicatorOffset + 1) then
  begin
    FixCount := Min(FixCount, ColCount - 1);
    intRow := Row;
    inherited FixedCols := FixCount;
    for i := 1 to Min(FixedCols, ColCount - 1) do
      TabStops[i] := False;
    Row := intRow;
  end;
  FFixedCols := FixCount - IndicatorOffset;
end;

function TSMDBGrid.GetFixedCols: Integer;
begin
  if DataLink.Active then
    Result := inherited FixedCols - IndicatorOffset
  else
    Result := FFixedCols;
end;

procedure TSMDBGrid.SelectOneClick(Sender: TObject);
begin
  if (dgMultiSelect in Options) and Datalink.Active then
  begin
    SelectedRows.CurrentRowSelected := True;
    if Assigned(FOnChangeSelection) then
      FOnChangeSelection(Self);
  end
end;

procedure TSMDBGrid.SelectAllClick(Sender: TObject);
var
  ABookmark: TBookmark;
begin
  if (dgMultiSelect in Options) and DataLink.Active then
  begin
    with Datalink.Dataset do
    begin
      if (BOF and EOF) then Exit;
      DisableControls;
      try
        ABookmark := GetBookmark;
        try
          First;
          while not EOF do
          begin
            SelectedRows.CurrentRowSelected := True;
            Next;
          end;
        finally
          try
            if BookmarkValid(ABookmark) then
              GotoBookmark(ABookmark);
          except
          end;
          FreeBookmark(ABookmark);
        end;
      finally
        if Assigned(FOnChangeSelection) then
          FOnChangeSelection(Self);
        EnableControls;
      end;
    end;
  end;
end;

procedure TSMDBGrid.UnSelectOneClick(Sender: TObject);
begin
  if (dgMultiSelect in Options) and Datalink.Active then
  begin
    SelectedRows.CurrentRowSelected := False;
    if Assigned(FOnChangeSelection) then
      FOnChangeSelection(Self);
  end
end;

procedure TSMDBGrid.UnSelectAllClick(Sender: TObject);
begin
  if (dgMultiSelect in Options) then
  begin
    SelectedRows.Clear;
    FSelecting := False;
    if Assigned(FOnChangeSelection) then
      FOnChangeSelection(Self);
  end;
end;

procedure TSMDBGrid.SaveLayoutClick(Sender: TObject);
begin
  SaveGridToRegistry(Self, FRegistryKey, FRegistrySection);
end;

procedure TSMDBGrid.RestoreLayoutClick(Sender: TObject);
begin
  LoadGridFromRegistry(Self, FRegistryKey, FRegistrySection);
end;

procedure TSMDBGrid.DeleteData;

  function DeletePrompt: Boolean;
  var S: string;
  begin
    if (SelectedRows.Count > 1) then
      S := SDeleteMultipleRecordsQuestion
    else
      S := SDeleteRecordQuestion;
    Result := not (dgConfirmDelete in Options) or
      (MessageDlg(S, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  end;

begin
  if DeletePrompt then
  begin
    if SelectedRows.Count > 0 then
      SelectedRows.Delete
    else
      Datalink.DataSet.Delete;
  end;
end;

procedure TSMDBGrid.RefreshData;
var
  bookPosition: TBookMark;
  boolContinue: Boolean;
begin
  boolContinue := True;

  {if needed, then save the inputed data}
  if Assigned(Datalink.DataSet) then
  begin
     with Datalink.DataSet do
     begin
       if (State in [dsInsert, dsEdit]) and CanModify then Post;
{       if (Datalink.DataSet is TBDEDataSet) then
         with (Datalink.DataSet as TBDEDataSet) do
         begin
           if CachedUpdates and UpdatesPending then
             try
               case MessageDlg(strSaveChanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
                 mrYes: ApplyUpdates;
                 mrNo: CancelUpdates;
                 else
                   boolContinue := False;
               end;
             except
               MessageDlg(strErrSaveChanges, mtError, [mbOk], 0);
               boolContinue := False;
             end;
         end;
}
       if boolContinue then
       begin
         {save a current position}
         bookPosition := GetBookmark;

         {close and open a dataset}
         Active := False;
         Active := True;

         {restore a saved position}
         try
           GotoBookmark(bookPosition);
         except
           First;
         end;
         FreeBookmark(bookPosition);
       end;
     end;
  end;
end;

type
  THackGrid = class(TCustomGrid);

procedure TSMDBGrid.SetExOptions(Val: TExOptions);
var
  FrameOffs: Byte;
begin
  if (FExOptions <> Val) then
  begin
    FExOptions := Val;

   if (eoRowSizing in Val) then
     THackGrid(Self).Options := THackGrid(Self).Options + [goRowSizing]
   else
     THackGrid(Self).Options := THackGrid(Self).Options - [goRowSizing];

    if ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
      FrameOffs := 1
    else
      FrameOffs := 2;

    if (eoCheckBoxSelect in Val) then
    begin
      if (FWidthOfIndicator = IndicatorWidth) then
        WidthOfIndicator := FCheckWidth + 4*FrameOffs + FMsIndicators.Width;
    end
    else
    begin
      if (WidthOfIndicator = FCheckWidth + 4*FrameOffs + FMsIndicators.Width) then
        WidthOfIndicator := IndicatorWidth;
    end;

    if (eoAutoWidth in Val) then
      UpdateColWidths;

    SetTitlesHeight;
    SetRowHeight;

    InvalidateFooter;

    Invalidate;
  end;
end;

function TSMDBGrid.CanEditShow: Boolean;
var
  i: Integer;
  str: string;
  CalcExpResult: Boolean;
begin
  Result := inherited CanEditShow;

  if (FieldCount > 0) and
     (SelectedIndex > -1) and
     Assigned(Columns[SelectedIndex].Field) then
  begin
    Result := Result and not Columns[SelectedIndex].ReadOnly; // Edit in the case the column is not read-only

    if Result and
       (Datalink <> nil) and
       Datalink.Active then
      Result := (GetImageIndex(Columns[SelectedIndex].Field) < 0);
    if Result and
       ColumnIsCheckbox(Columns[SelectedIndex]) then
      Result := False;
    if Result and (SelectedIndex < FixedCols) then
      Result := False;

    if Result then
    begin
      {if grouping, then edit is disabled}
      for i := 0 to Groupings.Count-1 do
      begin
        with Groupings[i] do
        begin
          if (Expression <> '') then
          begin
            CalcExpResult := False;
            str := Columns[SelectedIndex].Field.DisplayText;
            if Assigned(OnExpression) then
              OnExpression(Self, Expression, str, CalcExpResult);
            if CalcExpResult then
            begin
              Result := False;
              break
            end
          end
        end
      end
    end
  end;
end;

function TSMDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not (csDesigning in ComponentState) then
  begin
    SetFocus;
    Result := Focused or (InplaceEditor <> nil) and InplaceEditor.Focused;
  end;
end;

function TSMDBGrid.GetOptions: TDBGridOptions;
begin
  Result := inherited Options;

  FIsFlagAlwaysShowEditor := (dgAlwaysShowEditor in Result) and (csLoading in ComponentState);
  if FIsFlagAlwaysShowEditor then
  begin
    Result := Result - [dgAlwaysShowEditor];
    inherited Options := Result;
  end;

  if FMultiSelect then
    Result := Result + [dgMultiSelect]
  else
    Result := Result - [dgMultiSelect];
end;

procedure TSMDBGrid.SetOptions(Value: TDBGridOptions);
begin
  inherited Options := Value - [dgMultiSelect];

  if FMultiSelect <> (dgMultiSelect in Value) then
  begin
    FMultiSelect := (dgMultiSelect in Value);
    if not FMultiSelect then
      SelectedRows.Clear;
  end;
end;

procedure TSMDBGrid.GetCellProps(ACol: Integer; Field: TField; AFont: TFont;
  var Background: TColor; Highlight: Boolean);
begin
  if not Highlight then
  begin

    if (eoFixedLikeColumn in ExOptions) or
       (((dgIndicator in Options) and (ACol >= FixedCols)) or
        (not (dgIndicator in Options) and (ACol > FixedCols-1))) then
      if (GridStyle.Style <> gsNormal) {and
         (Background = Color) }then
      begin
        if (DataLink.ActiveRecord mod 2 = 0) then
          Background := GridStyle.OddColor
        else
          Background := GridStyle.EvenColor;
      end
  end;

  if Assigned(FOnGetCellParams) then
    FOnGetCellParams(Self, Field, AFont, Background, Highlight)
end;

procedure TSMDBGrid.CheckTitleButton(ACol: Longint; var Enabled: Boolean);
begin
  if (ACol >= 0) and (ACol < Columns.Count) then
  begin
    if Assigned(FOnCheckButton) then
      FOnCheckButton(Self, ACol, Columns[ACol].Field, Enabled);
  end
  else
    Enabled := False;
end;

procedure TSMDBGrid.DisableScroll;
begin
  Inc(FDisableCount);
end;

type
  THackLink = class(TGridDataLink);

procedure TSMDBGrid.EnableScroll;
begin
  if FDisableCount <> 0 then
  begin
    Dec(FDisableCount);
    if FDisableCount = 0 then
      THackLink(DataLink).DataSetScrolled(0);
  end;
end;

function TSMDBGrid.ScrollDisabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TSMDBGrid.Scroll(Distance: Integer);
var
  IndicatorRect: TRect;
//  oldActiveRecord, i: Integer;
begin
  //Invalidate; {20050714}
  if (Groupings.Count > 0) or
     (GridStyle.Style <> gsNormal) or
     Assigned(GridStyle.Background) or
     (GridStyle.Wallpaper.Direction <> fdNone) then
  begin
    Invalidate
//20050929    oldActiveRecord := DataLink.ActiveRecord;
//    for i := oldActiveRecord to oldActiveRecord+Distance do
//      GridInvalidateRow(Self, i);
  end;

  if FDisableCount = 0 then
  begin
    inherited Scroll(Distance);

    if (dgIndicator in Options) and
       HandleAllocated and
       (dgMultiSelect in Options) then
    begin
      IndicatorRect := BoxRect(0, 0, 0, RowCount - 1);
      InvalidateRect(Handle, @IndicatorRect, False);
    end;
  end;
end;

{$IFDEF DELPHI_4_UP}
function TSMDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if not AcquireFocus then Exit;
    if Datalink.Active then
    begin
      Result := Datalink.DataSet.MoveBy(1) <> 0;
    end;
  end;
end;

function TSMDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if not AcquireFocus then Exit;
    if Datalink.Active then
    begin
      Result := Datalink.DataSet.MoveBy(-1) <> 0;
    end;
  end;
end;
{$ENDIF}

procedure TSMDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyDownEvent: TKeyEvent;

  function ItAddLastRecord: Boolean;
  begin
    Result := (eoDisableInsert in FExOptions) and
              (Datalink.ActiveRecord >= GetRecordCount-1);
  end;

  procedure ClearSelections;
  begin
    if (dgMultiSelect in Options) then
    begin
      if not (eoKeepSelection in ExOptions) then
        SelectedRows.Clear;
      FSelecting := False;
    end;
  end;

  procedure DoSelection(Select: Boolean; Direction: Integer);
  var
    AddAfter: Boolean;
  begin
    AddAfter := False;
    BeginUpdate;
    try
      if (dgMultiSelect in Options) and DataLink.Active then
        if Select and (ssShift in Shift) then
        begin
          if not FSelecting then
          begin
            FSelectionAnchor := TBookmarks(SelectedRows).CurrentRow;
            SelectedRows.CurrentRowSelected := True;
            FSelecting := True;
            AddAfter := True;
          end
          else
            with TBookmarks(SelectedRows) do
            begin
              AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
              if not AddAfter then
                CurrentRowSelected := False;
            end
        end
        else
          ClearSelections;
      if Direction <> 0 then
        if Datalink.DataSet.State = dsInsert then
          Datalink.DataSet.MoveBy(0)
        else
          Datalink.DataSet.MoveBy(Direction);
      if AddAfter then
        SelectedRows.CurrentRowSelected := True;
    finally
      EndUpdate;
    end;
  end;

  procedure NextRow(Select: Boolean);
  begin
    with Datalink.Dataset do
    begin
      DoSelection(Select, 1);
      if EOF and CanModify and (not ReadOnly) and (dgEditing in Options) then
        if not ItAddLastRecord then
          AppendClick(Self)
        else
          Key := 0;
    end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
    DoSelection(Select, -1);
  end;

  {implemented by Joao Clobocar <joao@clobocar.com.br>
  skip the readonly fields from navigation (for fast entry)}
  procedure CheckTab0(GoForward: Boolean);
  var
    ACol, Original: Integer;
  begin
    if DataLink.Active then
    begin
      SelectedIndex := Col - 1; // pseudo-move column to force field update. (i couldn't find a better way)
      ACol := Col;
      Original := ACol;
      BeginUpdate;    { Prevent highlight flicker on tab to next/prior row }
      try
        while True do
        begin
          if GoForward then
            Inc(ACol)
          else
            Dec(ACol);
          if ACol >= ColCount then
          begin
            ClearSelections;
            NextRow(False);
            if Key = 0 then
              ACol := Original
            else
              ACol := IndicatorOffset;
          end
          else
          if ACol < IndicatorOffset then
          begin
            ClearSelections;
            PriorRow(False);
            if not DataLink.DataSet.Bof then
              ACol := ColCount - 1
            else
              ACol := Original;
          end;
          if (ACol = Original) or (not Columns[ACol-1].ReadOnly) then
          begin
            Col := ACol;
            Key := 0;
            break;
          end;
        end;
      finally
        EndUpdate;
      end
    end
  end;

  procedure CheckTab(GoForward: Boolean);
  var
    ACol, Original: Integer;
  begin
    if (dgMultiSelect in Options) and DataLink.Active then
    begin
      ACol := Col;
      Original := ACol;
      BeginUpdate;    { Prevent highlight flicker on tab to next/prior row }
      try
        while True do
        begin
          if GoForward then
            Inc(ACol)
          else
            Dec(ACol);
          if ACol >= ColCount then
          begin
            ClearSelections;
            NextRow(False);
{20041229start
            if Key = 0 then
              ACol := Original
            else
20041229end}
            ACol := IndicatorOffset;
          end
          else
            if ACol < IndicatorOffset then
            begin
              ClearSelections;
              PriorRow(False);
{20041229}              ACol := ColCount;
{20041229start
              if not DataLink.DataSet.Bof then
                ACol := ColCount - 1
              else
                ACol := Original;
20041229end}
            end;
{20041229}        if ACol = Original then Exit;
{20041229start
          if (ACol = Original) or (not Columns[ACol-1].ReadOnly) then
          begin
            Col := ACol;
            Key := 0;
            break;
          end;
20041229end}
          if TabStops[ACol] then
          begin
//            MoveCol(ACol);
            Exit;
          end;
        end;
      finally
        EndUpdate;
      end
    end
{    else
    begin
      ACol := Col;
      if GoForward then
        Inc(ACol)
      else
        Dec(ACol);
      if ACol >= ColCount then
        NextRow(False);
    end
}  end;

const
  RowMovementKeys = [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END];

var
  oldSelectedRowCount: Integer;
begin
//  inherited KeyDown(Key, Shift);
//  exit;
  
  if (dgMultiSelect in Options) and Assigned(OnChangeSelection) then
    oldSelectedRowCount := SelectedRows.Count
  else
    oldSelectedRowCount := 0;

  KeyDownEvent := OnKeyDown;
  if Assigned(KeyDownEvent) then
    KeyDownEvent(Self, Key, Shift);
  if not Datalink.Active or not CanGridAcceptKey(Key, Shift) then Exit;

{20050714}  if (Key in RowMovementKeys) then
    Invalidate;
  with Datalink.DataSet do
    if (ssCtrl in Shift) then
    begin
      if (Key in RowMovementKeys) then
        ClearSelections;

      case Key of
        VK_LEFT: if FixedCols > 0 then
                 begin
                   SelectedIndex := FixedCols;
                   Key := 0;
                 end;
        VK_DELETE: begin
                     Key := 0;
                     if not (eoDisableDelete in FExOptions) then
                       if not ReadOnly and CanModify then
                         DeleteClick(nil);
                   end;
      end
    end
    else
    begin
      case Key of
        VK_LEFT: if (FixedCols > 0) and not (dgRowSelect in Options) then
                 begin
                   if SelectedIndex <= FFixedCols then Key := 0;
                 end;
        VK_HOME: if (FixedCols > 0) and (ColCount <> IndicatorOffset + 1) and
                    not (dgRowSelect in Options) then
                 begin
                   SelectedIndex := FixedCols;
                   Key := 0;
                 end;
        VK_SPACE: if (Datalink <> nil) and Datalink.Active and
                     (SelectedIndex > -1) and
                     ColumnIsCheckbox(Columns[SelectedIndex]) then
                  begin
                    CellClick(Columns[SelectedIndex]);
                  end;
        VK_DOWN: begin
                   NextRow(True);
                   if (eoDisableInsert in FExOptions) and
                      (DataLink.Dataset.Eof) then
                     DataLink.Dataset.Cancel;
                   Key := 0;
                 end;
        VK_INSERT: if (eoDisableInsert in FExOptions) then Key := 0;
      end;
      if (Datalink.DataSet.State = dsBrowse) then
      begin
        case Key of
          VK_UP: begin
                   PriorRow(True);
                   Key := 0;
                 end;
          VK_RETURN: if (eoENTERlikeTAB in FExOptions)  then
                     {going on next column}
                     begin
//                       Key := 0;
//                       PostMessage(Self.Handle, WM_KeyDown, VK_Tab, 0);
                       if (SelectedIndex < Columns.Count-1) then
                         SelectedIndex := SelectedIndex + 1
                       else
                       begin
                         if not (eoDisableInsert in ExOptions) and
                            Datalink.DataSet.CanModify then
                           Datalink.DataSet.Append;

                         SelectedIndex := FixedCols {0};
                       end;
                     end;
        end;
      end;
      if ((Key in [VK_LEFT, VK_RIGHT]) and (dgRowSelect in Options)) or
         ((Key in [VK_HOME, VK_END]) and ((ColCount = IndicatorOffset + 1)
          or (dgRowSelect in Options))) or (Key in [VK_ESCAPE, VK_NEXT,
          VK_PRIOR]) or ((Key = VK_INSERT) and (CanModify and
          (not ReadOnly) and (dgEditing in Options))) then
        ClearSelections
      else
        if ((Key = VK_TAB) and not (ssAlt in Shift)) then
          CheckTab(not (ssShift in Shift));
    end;
  OnKeyDown := nil;
//  try
    inherited KeyDown(Key, Shift);
//  except
//  end;
  OnKeyDown := KeyDownEvent;

  if (dgMultiSelect in Options) and Assigned(OnChangeSelection) then
  begin
    if (oldSelectedRowCount <> SelectedRows.Count) then
      OnChangeSelection(Self)
  end
end;

procedure TSMDBGrid.TopLeftChanged;
begin
  if (dgRowSelect in Options) and DefaultDrawing then
    GridInvalidateRow(Self, Self.Row);

  inherited TopLeftChanged;
  if FTracking then StopTracking;

  if (eoAutoWidth in ExOptions) then
    UpdateColWidths;

  if (eoShowFooter in ExOptions) then
    InvalidateFooter;

  if Assigned(FOnTopLeftChanged) then
    FOnTopLeftChanged(Self);
end;

procedure TSMDBGrid.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TSMDBGrid.TrackButton(X, Y: Integer);
var
  Cell: TGridCoord;
  NewPressed: Boolean;
begin
  Cell := MouseCoord(X, Y);
  NewPressed := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
    and (FPressedCol = Cell.X) and (Cell.Y = 0);
  if FPressed <> NewPressed then
  begin
    FPressed := NewPressed;
    GridInvalidateRow(Self, 0);
  end;
end;

procedure TSMDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
  gc: TGridCoord;
begin
  inherited;

  if (csDesigning in ComponentState) or
     not Assigned(DataLink.DataSet) or
     not Assigned(DataSource.DataSet) or
    DataSource.DataSet.ControlsDisabled then exit;

  if (eoHotTrack in ExOptions) and
     Assigned(DataLink.DataSet) then
  begin
    gc := MouseCoord(X, Y);

    if (gc.X > 0) and (gc.Y > 0) then
      DataSource.DataSet.MoveBy(gc.Y - Row);
  end;

  TDrawGrid(Self).MouseToCell(X, Y, ACol, ARow);
  if ((ACol = FLastCol) and (ARow = FLastRow)) or
     not IsXPThemesEnabled or
     (Groupings.Count > 0) then
    exit;

  FLastRect := CellRect(ACol, ARow);

  {update previous cell}
  if FLastXPDrawn then
    InvalidateCell(FLastCol, FLastRow);

  {repaint current cell (if title)}
  InvalidateCell(ACol, ARow);
  FLastRow := ARow;
  FLastCol := ACol;

  FLastXPDrawn := False;
end;

procedure TSMDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure SetEnabledItems;
  begin
    with FDBPopUpMenu do
    begin
      {$IFDEF DELPHI_4_UP}
      Images := FImages;
      {$ENDIF}

      {Append}
      if Assigned(Datalink.DataSet) then
        Items[0].Enabled := not (eoDisableInsert in FExOptions) and
                            (not ReadOnly) and
                            Datalink.DataSet.CanModify and
                            (Datalink.DataSet.State = dsBrowse)
      else
        Items[0].Enabled := False;

      {Insert}
      Items[1].Enabled := Items[0].Enabled;

      {Edit}
      if Assigned(Datalink.DataSet) then
        Items[2].Enabled := not ReadOnly and
                            Datalink.DataSet.CanModify and
                            (Datalink.DataSet.State = dsBrowse)
//                          and (GetRecordCount > 0)
      else
        Items[2].Enabled := False;

      {Delete}
      if Assigned(Datalink.DataSet) then
        Items[3].Enabled := (not (eoDisableDelete in FExOptions)) and
                            (not ReadOnly) and
                            Datalink.DataSet.CanModify and
                            (Datalink.DataSet.State = dsBrowse)
//                             and (GetRecordCount > 0)
      else
        Items[3].Enabled := False;

      {Print}
      Items[5].Enabled := True;
      Items[5].Visible := Assigned(FOnPrintData);
      if Assigned(Datalink.DataSet) then
        Items[5].Enabled := (Datalink.DataSet.State = dsBrowse) and Assigned(FOnPrintData)
      else
        Items[5].Enabled := False;

      {Export}
      Items[6].Enabled := True;
      Items[6].Visible := Assigned(FOnExportData);
      if Assigned(Datalink.DataSet) then
        Items[6].Enabled := (Datalink.DataSet.State = dsBrowse) and Assigned(FOnExportData)
      else
        Items[6].Enabled := False;

      {Filter}
      Items[7].Enabled := True;
      Items[7].Visible := Assigned(FOnFilterData);
      if Assigned(Datalink.DataSet) then
        Items[7].Enabled := (Datalink.DataSet.State = dsBrowse) and Assigned(FOnFilterData)
      else
        Items[7].Enabled := False;

      {Search}
      Items[8].Enabled := True;
      Items[8].Visible := Assigned(FOnSearchData);
      if Assigned(Datalink.DataSet) then
        Items[8].Enabled := (Datalink.DataSet.State = dsBrowse) and Assigned(FOnSearchData)
      else
        Items[8].Enabled := False;

      Items[9].Visible := Items[5].Visible or Items[6].Visible or Items[7].Visible or Items[8].Visible;

      {Post}
      if Assigned(Datalink.DataSet) then
        Items[10].Enabled := (not ReadOnly) and
                            (Datalink.DataSet.State in [dsInsert, dsEdit]) and
                            Datalink.DataSet.CanModify
      else
        Items[10].Enabled := False;

      {Cancel}
      if Assigned(Datalink.DataSet) then
        Items[11].Enabled := (not ReadOnly) and
                            (Datalink.DataSet.State in [dsInsert, dsEdit])
      else
        Items[11].Enabled := False;

      {Refresh}
      if Assigned(Datalink.DataSet) then
        Items[12].Enabled := (Datalink.DataSet.State = dsBrowse)
      else
        Items[12].Enabled := False;

      {select/unselect}
      Items[14].Enabled := Assigned(Datalink.DataSet) and
                           Datalink.DataSet.Active and
                           (dgMultiSelect in Options);

      {save/restore layout}
      Items[16].Enabled := True;
      Items[17].Enabled := True;

      Items[15].Visible := (eoLayout in ExOptions);
      Items[16].Visible := (eoLayout in ExOptions);
      Items[17].Visible := (eoLayout in ExOptions);
      Items[16].Enabled := (eoLayout in ExOptions);
      Items[17].Enabled := (eoLayout in ExOptions);

      {setup of the grid}
      Items[19].Enabled := True;
      Items[19].Visible := Assigned(FOnSetupGrid);
      if Assigned(Datalink.DataSet) then
        Items[19].Enabled := Assigned(FOnSetupGrid)
      else
        Items[19].Enabled := False;
      Items[18].Visible := Items[19].Visible;
    end;
  end;

var
  Cell: TGridCoord;
  MouseDownEvent: TMouseEvent;
  EnableClick: Boolean;
  PopCoord: TPoint;
  cr: TRect;
begin
  if not Assigned(DataLink.Dataset) or
     DataLink.Dataset.ControlsDisabled then exit;

  if not AcquireFocus then Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    DblClick;
    Exit;
  end;

  if (dgMultiSelect in Options) then
  begin
    FreeStartOfSelect;

    StartOfSelect := DataLink.DataSet.GetBookmark;
  end;

  if Sizing(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else
  begin
    Cell := MouseCoord(X, Y);

    if not (csDesigning in ComponentState) and
       (eoStandardPopup in FExOptions) and
       Datalink.Active and
       ((dgIndicator in Options) and
        (Cell.Y > -1) and (Cell.Y < TitleOffset) and
        (Cell.X > -1) and (Cell.X < IndicatorOffset) or
        ((Button = mbRight) and (Cell.X >= IndicatorOffset) and not Assigned(PopupMenu))) then
    begin
      SetEnabledItems;
      PopCoord := ClientToScreen(Point(X, Y));
      FDBPopUpMenu.Popup(PopCoord.X, PopCoord.Y);
    end
    else
      if (eoTitleButtons in ExOptions) and
         (Datalink <> nil) and Datalink.Active and
         (Cell.Y < TitleOffset) and (Cell.X >= IndicatorOffset) and
         not (csDesigning in ComponentState) then
      begin
        if (dgColumnResize in Options) and (Button = mbRight) then
        begin
          Button := mbLeft;
          FSwapButtons := True;
          MouseCapture := True;
        end
        else
          if (Button = mbLeft) then
          begin
            EnableClick := True;
            CheckTitleButton(Cell.X - IndicatorOffset, EnableClick);
            if EnableClick then
            begin
              MouseCapture := True;
              FTracking := True;
              FPressedCol := Cell.X;
              TrackButton(X, Y);
            end
            else
              Beep;
            Exit;
          end;
      end;

    cr := CellRect(Cell.X, Cell.Y);
    if (eoShowFilterBar in ExOptions) and
       (Cell.Y > -1) and (Cell.Y < TitleOffset) and (Y > cr.Bottom - FILTERBAR_HEIGHT) and
       (Cell.X >= ({FixedCols +} IndicatorOffset)) then
    begin
      with CmbFilter do
      begin
        Left := cr.Left;
        Top := cr.Bottom - FILTERBAR_HEIGHT;
        Height := FILTERBAR_HEIGHT;
        if (Columns[Cell.X - ({FixedCols +} IndicatorOffset)] is TSMDBColumn) then
        begin
          Column := TSMDBColumn(Columns[Cell.X - ({FixedCols +} IndicatorOffset)]);
          if Column.FilterList.Count > 0 then
          begin
            try
              Items.BeginUpdate;
              CmbFilter.Items.Assign(Column.FilterList);
              Items.Insert(0, '');
              Style := csDropDown;
              ItemIndex := Items.IndexOf(Column.FilterValue);
              Height := DropDownCount * ItemHeight;
              Sorted := True;
              Width := cr.Right - cr.Left + 3;
              Items.EndUpdate;
            except
            end;
           end
           else
           begin
             Items.Clear;
             Style := csSimple;
             Text := Column.FilterValue;
             Width := cr.Right - cr.Left;
           end;
           Visible := True;
           SetFocus;
           Exit;
         end;
      end;
    end;

    if (Cell.X < FixedCols + IndicatorOffset) and Datalink.Active then
    begin
      if (dgIndicator in Options) then
        inherited MouseDown(Button, Shift, 1, Y)
      else
        if Cell.Y >= TitleOffset then
          if Cell.Y - Row <> 0 then
            Datalink.Dataset.MoveBy(Cell.Y - Row);
    end
    else
      inherited MouseDown(Button, Shift, X, Y);
    MouseDownEvent := OnMouseDown;
    if Assigned(MouseDownEvent) and
       (Cell.Y >= TitleOffset) then
      MouseDownEvent(Self, Button, Shift, X, Y);
    if not (((csDesigning in ComponentState) or (dgColumnResize in Options)) and
      (Cell.Y < TitleOffset)) and (Button = mbLeft) then
    begin
      if (dgMultiSelect in Options) and Datalink.Active then
      begin
        with SelectedRows do
        begin
          FSelecting := False;
          if (ssCtrl in Shift) then
          begin
            CurrentRowSelected := not CurrentRowSelected;
            if Assigned(FOnChangeSelection) then
              FOnChangeSelection(Self);
          end
          else
          if not (eoKeepSelection in ExOptions) then
          begin
            Clear;
            CurrentRowSelected := True;
            if Assigned(FOnChangeSelection) then
              FOnChangeSelection(Self);
          end
        end
      end
    end;
  end;
end;

procedure TSMDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cell: TGridCoord;
  ACol: Longint;
  DoClick: Boolean;

  EndOfSelect, Test: TBookmark;
  SelectForwards: Boolean;
  CompareStart: Integer;
begin
  {for shoft+click selection
  Thanks to Roy Lambert [roy.lambert@ntlworld.com]}
  if (dgMultiSelect in Options) and
     (ssShift in Shift) and
     Assigned(StartOfSelect) and
     Assigned(DataLink.DataSet) then
  begin
    DataLink.DataSet.DisableControls;
    EndOfSelect := DataLink.DataSet.GetBookmark;
    DataLink.DataSet.GotoBookmark(StartOfSelect);
    CompareStart := DataLink.DataSet.CompareBookmarks(EndOfSelect, StartOfSelect);
    Test := nil;
    if CompareStart <> 0 then
    begin
      SelectForwards := (CompareStart > 0);
      repeat
        SelectedRows.CurrentRowSelected := True;
        if Assigned(Test) then
          DataLink.DataSet.FreeBookmark(Test);
        Test := DataLink.DataSet.GetBookmark;
        if SelectForwards then
          DataLink.DataSet.Next
        else
          DataLink.DataSet.Prior;
      until (DataLink.DataSet.CompareBookmarks(EndOfSelect, Test) = 0) or DataLink.DataSet.Eof or DataLink.DataSet.Bof;
    end;
    DataLink.DataSet.GotoBookmark(EndOfSelect);
    if Assigned(EndOfSelect) then
      DataLink.DataSet.FreeBookmark(EndOfSelect);
    if Assigned(Test) then
      DataLink.DataSet.FreeBookmark(Test);
    DataLink.DataSet.EnableControls;
  end;

  Cell := MouseCoord(X, Y);
  ACol := Cell.X;
  if (dgIndicator in Options) then
    Dec(ACol);

  if FTracking and (FPressedCol >= 0) then
  begin
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
      and (Cell.Y = 0) and (Cell.X = FPressedCol);
    StopTracking;
    if DoClick then
    begin
      if (DataLink <> nil) and
         DataLink.Active and
         (ACol >= 0) and
         (ACol < Columns.Count) then
      else
        CellClick(Columns[ACol]);
    end;
  end
  else
    if FSwapButtons then
    begin
      FSwapButtons := False;
      MouseCapture := False;
      if Button = mbRight then
        Button := mbLeft;
    end;

  if (eoCheckBoxSelect in ExOptions) and
//     (dgMultiSelect in Options) and
     (Cell.X < IndicatorOffset) and
     (Cell.Y >= 0) then
    ToggleRowSelection;

  if (Button = mbLeft) and
     (Cell.X >= IndicatorOffset) and
     (ACol <= FixedCols) and
     (Cell.Y >= TitleOffset) then
    CellClick(Columns[ACol])
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

function ValueMatch(const ValueList, Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length(ValueList) do
    if AnsiCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then
    begin
      Result := True;
      break;
    end;
end;

procedure TSMDBGrid.CellClick(Column: TColumn);
var
  R: TRect;
  BCol, APos: Integer;
begin
  inherited CellClick(Column);

  if (Datalink <> nil) and
     Datalink.Active and
     Assigned(Column.Field) and
     ColumnIsCheckbox(Column) and
     CanEditModify then
  begin
    try
      if (Column.Field.DataType = ftBoolean) then
        Column.Field.AsBoolean := not Column.Field.AsBoolean
      else
      if (Column is TSMDBCheckboxColumn) then
      begin
        with TSMDBCheckboxColumn(Column) do
        begin
          APos := -1;
          if ValueMatch(ValueChecked, Field.AsString) then
            Field.AsString := ExtractFieldName(ValueUnchecked, APos)
          else
            Field.AsString := ExtractFieldName(ValueChecked, APos);
        end
      end
    except
      Column.Field.Value := NULL;
    end;

    if (dgIndicator in Options) then
      BCol := Column.Index + 1
    else
      BCol := Column.Index;
    GetEditText(BCol, Row);

    R := CellRect(BCol, Row);
    DrawCell(BCol, Row, R, [{gdSelected, gdFocused}]);
  end
  else
    if (eoShowLookup in ExOptions) and
       (not ReadOnly) and
       (dgEditing in Options) and
       (not Column.ReadOnly) and
       Assigned(Column.Field) and
       (not Column.Field.ReadOnly) then
    begin
      if (Column.Field.FieldKind = fkLookup) or
          (Column.PickList.Count > 0) then
      begin {Open combobox quickly when lookup field}
        keybd_event(VK_F2, 0, 0, 0);
        keybd_event(VK_F2, 0, KEYEVENTF_KEYUP, 0);
        keybd_event(VK_MENU, 0, 0, 0);
        keybd_event(VK_DOWN, 0, 0, 0);
        keybd_event(VK_DOWN, 0, KEYEVENTF_KEYUP, 0);
        keybd_event(VK_MENU, 0, KEYEVENTF_KEYUP, 0);
      end
      else
        if (Column.ButtonStyle = cbsEllipsis) then
        begin {Click quickly when ButtonStyle property is cbsEllipsis}
          if not EditorMode then
            EditorMode := True;
          EditButtonClick;
        end;
    end;
end;

function TSMDBGrid.GetSortImageWidth: Integer;
begin
  Result := Max(GetGridBitmap(gpSortAsc).Width, GetGridBitmap(gpSortDesc).Width);
end;

function TSMDBGrid.CellRectForDraw(R: TRect; ACol: Longint): TRect;
var
  i: Integer;
begin
  Result := R;

  i := GetSortImageWidth;
  if (Result.Right-Result.Left > i+4) then
  begin
    if (ACol > -1) and
       (Columns[ACol] is TSMDBColumn) and
       (TSMDBColumn(Columns[ACol]).SortType <> stNone) then
      Result.Right := Result.Right-i-4;
  end;
  i := 2*(GridLineWidth+1)+1;
  Result.Right := Result.Right-i
end;

function TSMDBGrid.GetGlyph: TBitmap;
begin
  Result := nil;
  if Assigned(FOnGetGlyph) then
    FOnGetGlyph(Self, Result);
end;

function TSMDBGrid.IsMouseInRect(ARect: TRect): Boolean;
var
  r: TRect;
begin
  Result := IntersectRect(r, ARect, FLastRect);
end;

procedure TSMDBGrid.DrawComboArrow(R: TRect);
const
  CBXS_NORMAL = 1;
  CBXS_HOT = 2;

  CP_DROPDOWNBUTTON = 1;

var
  DrawState: Integer;

  hhTheme: hTHEME;
  FMouseInRect: Boolean;
begin
  if IsXPThemesEnabled and
     not (csDesigning in ComponentState) then
    hhTheme := OpenThemeData(0, 'Combobox')
  else
    hhTheme := 0;

  if (hhTheme <> 0) then
  begin
    FMouseInRect := IsMouseInRect(R);

    if FMouseInRect then
      DrawState := CBXS_HOT
    else
      DrawState := CBXS_NORMAL;

    try
      FLastXPDrawn := FMouseInRect;//True;
      DrawThemeBackground(hhTheme, Canvas.Handle, CP_DROPDOWNBUTTON, DrawState, @R, nil);
    finally
      CloseThemeData(hhTheme);
    end;
  end
  else
    DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, DFCS_SCROLLCOMBOBOX);
end;

procedure TSMDBGrid.DrawCheckBox(R: TRect; AState: TCheckBoxState; al: TAlignment);
const
  CBS_UNCHECKEDNORMAL = 1;
  CBS_UNCHECKEDHOT = 2;
  CBS_CHECKEDNORMAL = 5;
  CBS_CHECKEDHOT = 6;
  CBS_MIXEDNORMAL = 9;
  CBS_MIXEDHOT = 10;

  BP_CHECKBOX = 3;

var
  DrawState: Integer;
  DrawRect: TRect;

  hhTheme: hTHEME;
  FMouseInRect: Boolean;
begin
  {draw CheckBox instead Bitmap indicator}
{        Canvas.Brush.Color := FixedColor;
        Canvas.Font.Name := 'Symbol';
        Canvas.Font.Color := clWindowText;
        Canvas.Font.Style := [fsBold];
        WriteTitleText(Canvas, FixRect, 0, 0, 'Ö', taCenter);
}

  case AState of
    cbChecked: DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
    cbUnchecked: DrawState := DFCS_BUTTONCHECK;
  else // cbGrayed
    DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
  end;
  if Flat then
    DrawState := DrawState or DFCS_FLAT;

  case al of
    taRightJustify: begin
                      DrawRect.Left := R.Right - FCheckWidth;
                      DrawRect.Right := R.Right;
                    end;
    taCenter: begin
                DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
                DrawRect.Right := DrawRect.Left + FCheckWidth;
              end;
  else // taLeftJustify
    DrawRect.Left := R.Left;
    DrawRect.Right := DrawRect.Left + FCheckWidth;
  end;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckWidth) div 2;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;

  if IsXPThemesEnabled and
     not (csDesigning in ComponentState) then
    hhTheme := OpenThemeData(0, 'Button')
  else
    hhTheme := 0;

  if (hhTheme <> 0) then
  begin
    FMouseInRect := IsMouseInRect(R);

    case AState of
      cbChecked: if FMouseInRect then
                   DrawState := CBS_CHECKEDHOT
                 else
                   DrawState := CBS_CHECKEDNORMAL;
      cbUnchecked: if FMouseInRect then
                     DrawState := CBS_UNCHECKEDHOT
                   else
                     DrawState := CBS_UNCHECKEDNORMAL;
      else // cbGrayed
        if FMouseInRect then
          DrawState := CBS_MIXEDHOT
        else
          DrawState := CBS_MIXEDNORMAL
    end;

    try
      FLastXPDrawn := FMouseInRect;//True;
      DrawThemeBackground(hhTheme, Canvas.Handle, BP_CHECKBOX, DrawState, @DrawRect, nil);
    finally
      CloseThemeData(hhTheme);
    end;
  end
  else
    DrawFrameControl(Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
end;

procedure TSMDBGrid.DrawGraphic(R: TRect; Field: TField);
var
  pic: TPicture;
  W, H: Integer;
  x: Double;

  BRect: TRect;
begin
  if Field.IsNull then exit;

  pic := TPicture.Create;
  try
    try
      pic.{Graphic.}Assign(Field);
    except
    end;
    if (pic.Width <> 0) and (pic.Height <> 0) then
    begin
      BRect := R;
      x := (pic.Width/pic.Height); {aspect ratio}
      H := BRect.Bottom - BRect.Top;
      W := Trunc(H*x);
      if W > (R.Right - R.Left) then  {re-proportion pic}
      begin
        W := (R.Right-R.Left);
        H := Trunc(W/x);
      end;
      BRect.Left := (R.Right + R.Left - W) shr 1;
      BRect.Right := BRect.Left + W;
      BRect.Top := (R.Bottom + R.Top - H) shr 1;
      BRect.Bottom := BRect.Top + H;
      InflateRect(BRect, -1, -1);

      Canvas.StretchDraw(BRect, pic.Graphic);
    end;
  finally
    pic.Free;
  end
end;

procedure TSMDBGrid.DefDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  OldActive: Integer;
  Highlight: Boolean;
  Value: string;
  DrawColumn: TColumn;
  FrameOffs: Byte;
begin
  if csLoading in ComponentState then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    exit;
  end;

  Dec(ARow, TitleOffset);
  Dec(ACol, IndicatorOffset);

  if IsXPThemesEnabled and
     (gdFixed in AState) and ([dgRowLines, dgColLines] * Options =
    [dgRowLines, dgColLines]) then
  begin
    InflateRect(ARect, -1, -1);
    FrameOffs := 1;
  end
  else
    FrameOffs := 0;//2;

  with Canvas do
  begin
    DrawColumn := Columns[ACol];
    if gdFixed in AState then
    begin
      Font := DrawColumn.Title.Font;
      Brush.Color := DrawColumn.Title.Color;
    end
    else
    begin
      Font := DrawColumn.Font;
      Brush.Color := DrawColumn.Color;
    end;
    if ARow < 0 then
      with DrawColumn.Title do
        WriteTitleText(Canvas, ARect, FrameOffs, FrameOffs, Caption, Alignment,
                       teNone,
                       eoTitleWordWrap in ExOptions
                       {$IFDEF DELPHI_4_UP},
                       (BiDiMode <> bdLeftToRight)
//                       IsRightToLeft//UseRightToLeftAlignmentForField(DrawColumn.Field, Alignment)
                       {$ENDIF})
    else
      if (DataLink = nil) or not DataLink.Active then
        FillRect(ARect)
      else
      begin
        Value := '';
        OldActive := DataLink.ActiveRecord;
        try
          DataLink.ActiveRecord := ARow;
          if Assigned(DrawColumn.Field) then
            Value := DrawColumn.Field.DisplayText;
          Highlight := HighlightCell(ACol, ARow, Value, AState);
          if Highlight then
          begin
            Brush.Color := clHighlight;
            Font.Color := clHighlightText;
          end;

          if DefaultDrawing {and not Highlight }then
            WriteTitleText(Canvas, ARect, 2, 2, Value, DrawColumn.Alignment,
                           GetColumnEllipsis(DrawColumn),
                           False
                           {$IFDEF DELPHI_4_UP},
                           (BiDiMode <> bdLeftToRight)
//                           IsRightToLeft//UseRightToLeftAlignmentForField(DrawColumn.Field, DrawColumn.Alignment)
                           {$ENDIF});

{20050201}          if Columns.State = csDefault then
            DrawDataCell(ARect, DrawColumn.Field, AState);
          DrawColumnCell(ARect, ACol, DrawColumn, AState);
{}        finally
          DataLink.ActiveRecord := OldActive;
        end;
        if DefaultDrawing and (gdSelected in AState) and
           ((dgAlwaysShowSelection in Options) or Focused) and
           not (csDesigning in ComponentState) and
           not (dgRowSelect in Options) and
           (UpdateLock = 0) and
           (ValidParentForm(Self).ActiveControl = Self) then
          Windows.DrawFocusRect(Handle, ARect);
      end;
  end;

  if IsXPThemesEnabled and
     (gdFixed in AState) and ([dgRowLines, dgColLines] * Options =
     [dgRowLines, dgColLines]) then
  begin
    InflateRect(ARect, 1, 1);
    if Flat then
      DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_FLAT)
    else
    begin
      DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
      DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
    end
  end;
end;

procedure TSMDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  TitleText: string;
  i, j, idxSort, BCol: LongInt;
  bmp: TBitmap;
  BRect: TRect;

{  function RowIsMultiSelected: Boolean;
  var
    Index: Integer;
  begin
    Result := (dgMultiSelect in Options) and Datalink.Active and
      SelectedRows.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  end;
}
  procedure DrawTitleColumn(DrawColumn: TColumn);
  var
    HRect: TRect;
    smeColumn: TSMDBColumn;
  begin
    if (eoDrawBands in ExOptions) and (DrawColumn is TSMDBColumn) then
    begin
      smeColumn := TSMDBColumn(DrawColumn);
      if (smeColumn.BandIndex > -1) and (smeColumn.BandIndex < Bands.Count) then
      begin
        HRect := GetBandRect(DrawColumn.Index);
        {TODO: calculate a height for band!}
        HRect.Bottom := HRect.Top + DefaultRowHeight;//(HRect.Bottom - HRect.Top - 1) div 2;
        if (GridStyle.Bands.Direction = fdNone) then
          Canvas.FillRect(HRect)
        else
          SMDrawGradient(Canvas, HRect, GridStyle.Bands.StartColor, GridStyle.Bands.EndColor, GridStyle.Bands.Direction, 255);

        Canvas.Font.Assign(fBandsFont);
        InflateRect(HRect, -1, -1);
        WriteTitleText(Canvas, HRect, 2, 2, Bands[smeColumn.BandIndex], taCenter,
                       teNone,
                       eoTitleWordWrap in ExOptions
                       {$IFDEF DELPHI_4_UP},
                       (BiDiMode <> bdLeftToRight)
//                       IsRightToLeft//UseRightToLeftAlignmentForField(DrawColumn.Field, DrawColumn.Alignment)
                       {$ENDIF});

        InflateRect(HRect, 1, 1);
        HRect.Bottom := HRect.Bottom+1;
        if Flat then
//          DrawEdge(Canvas.Handle, HRect, BDR_RAISEDINNER, BF_FLAT)
          DrawEdge(Canvas.Handle, HRect, BDR_RAISEDINNER, BF_BOTTOM)
        else
        begin
          DrawEdge(Canvas.Handle, HRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
          DrawEdge(Canvas.Handle, HRect, BDR_RAISEDINNER, BF_TOPLEFT);
        end;

//        DrawEdge(Canvas.Handle, HRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
//        DrawEdge(Canvas.Handle, HRect, BDR_RAISEDINNER, BF_TOPLEFT);
      end;

      ARect.Top := ARect.Top + DefaultRowHeight + 1{3}//(ARect.Bottom - ARect.Top + 1) div 2;
    end;

    TitleText := DrawColumn.Title.Caption;

    {draw a column sorted image}
    //look: whether there is a sorting according this column
    if (ARect.Right-ARect.Left > j) then
    begin
      if (DrawColumn is TSMDBColumn) and (TSMDBColumn(DrawColumn).SortType <> stNone) then
        ARect.Right := ARect.Right-j;
    end;

    //draw title.caption
    if DefaultDrawing  and (TitleText <> '') then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font := DrawColumn.Title.Font;
//      Canvas.Brush.Color := DrawColumn.Title.Color;
      if not (eoTitleButtons in ExOptions) then
        WriteTitleText(Canvas, ARect, 2, 2, TitleText, DrawColumn.Title.Alignment,
                       teNone,
                       eoTitleWordWrap in ExOptions
                       {$IFDEF DELPHI_4_UP},
                       (BiDiMode <> bdLeftToRight)
//                       IsRightToLeft//UseRightToLeftAlignmentForField(DrawColumn.Field, DrawColumn.Alignment)
                       {$ENDIF});

      if (DrawColumn is TSMDBColumn) and (TSMDBColumn(DrawColumn).SortType <> stNone) then
      begin
        ARect.Right := ARect.Right+j;

        i := (ARect.Bottom - ARect.Top - j);
        if (eoShowFilterBar in ExOptions) then
          i := i - FILTERBAR_HEIGHT;
        i := i div 2;

        if (TSMDBColumn(DrawColumn).SortType = stAscending) then
          Bmp := GetGridBitmap(gpSortAsc)
        else
          Bmp := GetGridBitmap(gpSortDesc);
        BRect := Bounds(ARect.Right - 4 - j, ARect.Top+i, j, j);
        Canvas.FillRect(BRect);
        DrawBitmapTransparent(Canvas, (BRect.Left + BRect.Right - Bmp.Width) div 2,
                                      (BRect.Top + BRect.Bottom - Bmp.Height) div 2, Bmp, clSilver);
        TitleText := TSMDBColumn(DrawColumn).SortCaption;
        if (TitleText <> '') then
        begin
          BRect.Right := ARect.Right - 4;
          BRect.Left := BRect.Right - j;
          BRect.Top := ARect.Top + i;
          BRect.Bottom := ARect.Bottom;
          with Canvas.Font do
          begin
            Name := 'Small Fonts';
            Size := 5;
            Style := [];
          end;
          Canvas.Brush.Style := bsClear;
          DrawText(Canvas.Handle, PChar(TitleText), Length(TitleText),
                   BRect,
                   DT_EXPANDTABS or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
        end;
      end;
    end
  end;

var
  CheckState: TCheckBoxState;

  Down: Boolean;
  SavePen, BackColor: TColor;
  AField: TField;
  OldActive: Longint;
  FrameOffs: Byte;
  Indicator, ALeft: Integer;
  MultiSelected: Boolean;
  FixRect: TRect;
  DrawColumn: TColumn;
  BState: TGridDrawState;

  hhTheme: hTHEME;
  ColDrawn: Boolean;

const
  EdgeFlag: array[Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);

  HP_HEADERITEM = 1;
  HIS_NORMAL = 1;
  HIS_HOT = 2;

begin
  if (dgIndicator in Options) then
    BCol := ACol - 1
  else
    BCol := ACol;

  if BCol > -1 then
    DrawColumn := Columns[BCol]
  else
    DrawColumn := nil;

  if Assigned(DrawColumn) then
    Canvas.Font := DrawColumn.Font;

  if (gdFixed in AState) and
     (((ARow = 0) and (dgTitles in Options)) or
      ((ACol > -1) or ((ACol = 0) and (dgIndicator in Options) {and (eoStandardPopup in FExOptions)}))) then
  begin
{
inherited;
exit;
}
    if (ACol = 0) and (dgIndicator in Options) then
    begin
      Down := False;
      Canvas.Brush.Color := FixedColor
    end
    else
    begin
      Down := (eoSelectedTitle in FExOptions) and (BCol = SelectedIndex);
      if Assigned(DrawColumn) then
        Canvas.Brush.Color := DrawColumn.Title.Color;
    end;
    if Flat then
      DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_FLAT)
    else
    begin
      DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_BOTTOMLEFT);
      DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_TOPRIGHT);
    end;

    if ((ACol > 0) or not (dgIndicator in Options)) and
//       ((ARow > 0) or not (dgTitles in Options) or (Assigned(DrawColumn) and (DrawColumn.Title.Color = FixedColor))) and
       IsXPThemesEnabled and
       not (csDesigning in ComponentState) and
       (GridStyle.Title.Direction = fdNone) then
      hhTheme := OpenThemeData(0, 'Header')
    else
      hhTheme := 0;

    if (hhTheme <> 0) then
    begin
      FixRect := ARect;
//      if (dgColLines in Options) then
//        InflateRect(ARect, 1, 1);
      try
        if IsMouseInRect(FixRect) then
        begin
          FLastXPDrawn := True;
          DrawThemeBackground(hhTheme, Canvas.Handle, HP_HEADERITEM, HIS_HOT, @ARect, nil)
        end
        else
          DrawThemeBackground(hhTheme, Canvas.Handle, HP_HEADERITEM, HIS_NORMAL, @ARect, nil);
      finally
        CloseThemeData(hhTheme);
      end;
//      if (dgColLines in Options) then
//        InflateRect(ARect, -1, -1);
    end
    else
    begin
      if (dgColLines in Options) then
      begin
        if not Flat then
          InflateRect(ARect, -1, -1);
      end;
      if (GridStyle.Title.Direction = fdNone) then
        Canvas.FillRect(ARect)
      else
        SMDrawGradient(Canvas, ARect, GridStyle.Title.StartColor, GridStyle.Title.EndColor, GridStyle.Title.Direction, 255);
    end
  end;

  // draw border
  if DefaultDrawing then
  begin
    j := GetSortImageWidth;
    if (ARow = 0) and (dgTitles in Options) and
       (ACol = 0) and
       (dgIndicator in Options) and
       (eoStandardPopup in FExOptions) then
    begin
      Canvas.Brush.Color := clBlack;
      i := (ARect.Bottom - ARect.Top - 7) div 2;
      idxSort := (ARect.Right - ARect.Left - 7) div 2;
      Canvas.Polygon([Point(ARect.Left + idxSort, ARect.Top + i),
                      Point(ARect.Left + idxSort + 7, ARect.Top + i),
                      Point(ARect.Left + idxSort + (7 div 2), ARect.Bottom - i)]);
    end
    else
    if Assigned(DrawColumn) then
    begin
      SMDrawGradient(Canvas, ARect, GridStyle.Title.StartColor, GridStyle.Title.EndColor, GridStyle.Title.Direction, 255);
      if (ARow = 0) and (dgTitles in Options) then
        DrawTitleColumn(DrawColumn)
      else
      begin
        BState := AState;
        if (eoFixedLikeColumn in ExOptions) and
           (ACol > 0) and
           (ACol <= FixedCols) then
          AState := AState - [gdFixed];
{20051115        if DefaultDrawing  and
           (gdFixed in AState) and
           (GridStyle.Title.Direction = fdNone) then
        begin
          Canvas.Brush.Color := FixedColor;
          Canvas.FillRect(ARect)
        end;
}
{2006        if ColumnIsCheckbox(DrawColumn) then
        else
}          DefDrawCell(ACol, ARow, ARect, AState);
//        inherited DrawCell(ACol, ARow, ARect, AState);
        AState := BState;
      end
    end;

    if Assigned(FOnDrawColumnTitle) then
      FOnDrawColumnTitle(Self, ARect, ACol, DrawColumn{Columns[BCol]}, AState);
  end;
//  else
  begin
    ColDrawn := False;
    if DefaultDrawing and
       (Datalink <> nil) and
       Datalink.Active and
       Assigned(DrawColumn) and
       Assigned(DrawColumn.Field) and
       (DrawColumn is TSMDBColumn) and
       (((ARow > 0) and (dgTitles in Options)) or (not (dgTitles in Options))) then
    begin
      OldActive := DataLink.ActiveRecord;
      try
        DataLink.ActiveRecord := ARow - TitleOffset;

        try
          if (FixedCols > 0) and (ACol <= FixedCols) then
            BackColor := FixedColor
          else
          begin
            if HighlightCell(ACol, ARow, '', AState) then
              BackColor := clHighlight
            else
              BackColor := DrawColumn.Color;
            GetCellProps(ACol, DrawColumn.Field, Canvas.Font, BackColor, HighlightCell(ACol, ARow, TitleText, AState));
          end;

          TSMDBColumn(DrawColumn).DrawItem(Canvas, ARect, DrawColumn.Field.Value, BackColor, AState, ColDrawn)
        except
        end
      finally
        DataLink.ActiveRecord := OldActive;
      end;
    end;

    if not ColDrawn then
    begin
      if (gdFixed in AState) then
      begin
        if Assigned(OnDrawColumnCell) and Assigned(DrawColumn) and
           not ((ARow = 0) and (dgTitles in Options)) then
        begin
          OldActive := DataLink.ActiveRecord;
          try
            DataLink.ActiveRecord := ARow - TitleOffset;

            OnDrawColumnCell(Self, ARect, ACol, DrawColumn, AState);
          finally
            DataLink.ActiveRecord := OldActive;
          end;
        end
      end
      else
      begin
        if GridStyle.Background.Empty and
           (GridStyle.Wallpaper.Direction = fdNone) and
           (not Assigned(DrawColumn) or ((DrawColumn is TSMDBColumn) and (TSMDBColumn(DrawColumn).TextEllipsis = teNone))) then
          inherited DrawCell(ACol, ARow, ARect, AState)
        else
          DefDrawCell(ACol, ARow, ARect, AState);
      end;
    end;
  end;

  if Assigned(OnDrawDataCell) and
     Assigned(DrawColumn) and
     Assigned(DrawColumn.Field) then
    OnDrawDataCell(Self, ARect, DrawColumn.Field, AState);

  if (dgIndicator in Options) and (ACol = 0) and (ARow - TitleOffset >= 0) and
{     ((dgMultiSelect in Options) or
      Assigned(OnGetGlyph) or
      (eoCheckBoxSelect in ExOptions)) and
}      (DataLink <> nil) and DataLink.Active {and
    (Datalink.DataSet.State = dsBrowse) }then
  begin
{    if IsXPThemesEnabled and
       not (csDesigning in ComponentState) then
      hhTheme := OpenThemeData(0, 'Header')
    else
      hhTheme := 0;

    if (hhTheme <> 0) then
    begin
      try
        DrawThemeBackground(hhTheme, Canvas.Handle, HP_HEADERITEM, HIS_NORMAL, @ARect, nil);
      finally
        CloseThemeData(hhTheme);
      end;
      //draw indicator bitmap
      if Assigned(DataLink) and DataLink.Active  then
      begin
        MultiSelected := False;
        if ARow >= 0 then
        begin
          OldActive := DataLink.ActiveRecord;
          try
            Datalink.ActiveRecord := ARow;
            MultiSelected := RowIsMultiselected;
          finally
            Datalink.ActiveRecord := OldActive;
          end;
        end;
        if (ARow = DataLink.ActiveRecord) or MultiSelected then
        begin
          Indicator := 0;
          if DataLink.DataSet <> nil then
            case DataLink.DataSet.State of
              dsEdit: Indicator := 1;
              dsInsert: Indicator := 2;
              dsBrowse:
                if MultiSelected then
                  if (ARow <> Datalink.ActiveRecord) then
                    Indicator := 3
                  else
                    Indicator := 4;  // multiselected and current row
            end;
          FMsIndicators.BkColor := FixedColor;
          ALeft := ARect.Right - FMsIndicators.Width - FrameOffs;
          FMsIndicators.Draw(Canvas,
                             ALeft,
                             (ARect.Top + ARect.Bottom - FMsIndicators.Height) shr 1,
                             Indicator);
        end;
      end;
    end;
}

    { draw multiselect indicators if needed }
    FixRect := ARect;
    if ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
    begin
      InflateRect(FixRect, -1, -1);
      FrameOffs := 1;
    end
    else
      FrameOffs := 2;
    CheckState := cbUnChecked;
    OldActive := DataLink.ActiveRecord;
    try
      Datalink.ActiveRecord := ARow - TitleOffset;
      MultiSelected := ActiveRowSelected;
      if ActiveRowSelected then
        CheckState := cbChecked;

      Bmp := GetGlyph;
    finally
      Datalink.ActiveRecord := OldActive;
    end;

    if (eoCheckBoxSelect in ExOptions) then
    begin
      BRect := FixRect;
      BRect.Right := BRect.Right - 2*FrameOffs - FMsIndicators.Width;
      DrawCheckBox(BRect, CheckState, taRightJustify);
    end;

    if (eoRowNumber in ExOptions) then
    begin
      BRect := FixRect;
      BRect.Right := BRect.Right - 3*FrameOffs - FMsIndicators.Width;
      if (eoCheckBoxSelect in ExOptions) then
        BRect.Right := BRect.Right - FCheckWidth;
      TitleText := IntToStr(ARow - TitleOffset + 1);
      DrawText(Canvas.Handle, PChar(TitleText), Length(TitleText), BRect, DT_EXPANDTABS or DT_RIGHT);
    end;

    if (ARow - TitleOffset = DataLink.ActiveRecord) or MultiSelected then
    begin
      Indicator := 0;
      if DataLink.DataSet <> nil then
        case DataLink.DataSet.State of
          dsEdit: Indicator := 1;
          dsInsert: Indicator := 2;
          dsBrowse:
            if MultiSelected then
              if (ARow - TitleOffset <> Datalink.ActiveRecord) then
                Indicator := 3
              else
                Indicator := 4;  // multiselected and current row
        end;
      FMsIndicators.BkColor := FixedColor;
      ALeft := ARect.Right - FMsIndicators.Width - FrameOffs;
{$IFDEF DELPHI_4_UP}
      if Canvas.CanvasOrientation = coRightToLeft then Inc(ALeft);
{$ENDIF}
      FMsIndicators.Draw(Canvas, ALeft,
        (ARect.Top + ARect.Bottom - FMsIndicators.Height) shr 1, Indicator{, True});
//      if ARow = FDatalink.ActiveRecord then
//        FSelRow := ARow + FTitleOffset;
    end;
{    if MultiSelected then
    begin
      if (ARow - TitleOffset <> Datalink.ActiveRecord) then
        Indicator := 0
      else //multiselected and current row
        Indicator := 1;

      FMsIndicators.BkColor := FixedColor;
      FMsIndicators.Draw(Canvas, FixRect.Right - FMsIndicators.Width -
          FrameOffs, (FixRect.Top + FixRect.Bottom - FMsIndicators.Height)
          shr 1, Indicator);
    end;
}
    if (Bmp <> nil) then
    begin
      BRect.Left := FixRect.Left + FrameOffs;
      BRect.Top := FixRect.Top + FrameOffs;
      if (bmp.Width < FixRect.Right - FixRect.Left) then
        BRect.Right := BRect.Left + bmp.Width
      else
        if (eoCheckBoxSelect in ExOptions) then
          BRect.Right := FixRect.Right - FCheckWidth - FrameOffs
        else
          BRect.Right := FixRect.Right - FMsIndicators.Width - FrameOffs;
      BRect.Bottom := FixRect.Bottom - FrameOffs;
      Canvas.StretchDraw(BRect, bmp);
      bmp.Free;
    end;
  end;
  if (eoTitleButtons in ExOptions) and
     not (csLoading in ComponentState) and
     (gdFixed in AState) and
     (dgTitles in Options) and (ARow = 0) then
  begin
    SavePen := Canvas.Pen.Color;
    try
      Down := (FPressedCol = ACol) and FPressed;
      Canvas.Pen.Color := clWindowFrame;
      if not (dgColLines in Options) then
      begin
        Canvas.MoveTo(ARect.Right - 1, ARect.Top);
        Canvas.LineTo(ARect.Right - 1, ARect.Bottom);
        Dec(ARect.Right);
      end;
      if not (dgRowLines in Options) then
      begin
        Canvas.MoveTo(ARect.Left, ARect.Bottom - 1);
        Canvas.LineTo(ARect.Right, ARect.Bottom - 1);
        Dec(ARect.Bottom);
      end;
      if (dgIndicator in Options) then Dec(ACol);
      AField := nil;
      if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
        (ACol < Columns.Count) then
      begin
        DrawColumn := Columns[ACol];
        AField := DrawColumn.Field;
      end
      else
        DrawColumn := nil;

      if Flat then
        DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_FLAT)
      else
      begin
        DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_BOTTOMRIGHT);
        DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_TOPLEFT);
      end;
      InflateRect(ARect, -1, -1);
      if Down then
      begin
        Inc(ARect.Left);
        Inc(ARect.Top);
      end;
      Canvas.Font := TitleFont;
      Canvas.Brush.Color := FixedColor;
      if (DrawColumn <> nil) then
      begin
        Canvas.Font := DrawColumn.Title.Font;
        Canvas.Brush.Color := DrawColumn.Title.Color;
      end;
      if (AField <> nil) and Assigned(FOnGetBtnParams) then
      begin
        BackColor := Canvas.Brush.Color;
        FOnGetBtnParams(Self, AField, Canvas.Font, BackColor, Down);
        Canvas.Brush.Color := BackColor;
      end;
      if (DataLink = nil) or not DataLink.Active then
        Canvas.FillRect(ARect)
      else
        if (DrawColumn <> nil) then
          WriteTitleText(Canvas, ARect, 2, 2, DrawColumn.Title.Caption, Columns[BCol].Title.Alignment,
                         teNone,
                         eoTitleWordWrap in ExOptions
                         {$IFDEF DELPHI_4_UP},
                         (BiDiMode <> bdLeftToRight)
//                         IsRightToLeft//UseRightToLeftAlignmentForField(DrawColumn.Field, DrawColumn.Alignment)
                         {$ENDIF})
        else
          WriteTitleText(Canvas, ARect, 2, 2, '', taLeftJustify,
                         teNone,
                         False
                         {$IFDEF DELPHI_4_UP},
                         (BiDiMode <> bdLeftToRight)
//                         IsRightToLeft//UseRightToLeftAlignmentForField(nil, taLeftJustify)
                         {$ENDIF});
    finally
      Canvas.Pen.Color := SavePen;
    end;
  end;
//  if (Bmp <> nil) then
//    bmp.Free

//  DrawGroupings(ARect, ACol, DrawColumn, AState);

  if (eoShowFilterBar in ExOptions) and
     (ARow = 0) and (DrawColumn <> nil) then
  begin
    FixRect := ARect;
    FixRect.Top := FixRect.Bottom - FILTERBAR_HEIGHT;
    Canvas.Brush.Color := FGridStyle.FilterColor;
    Canvas.FillRect(FixRect);

    if (DrawColumn is TSMDBColumn) then
    begin
      Canvas.Font := DrawColumn.Title.Font;
      Canvas.TextRect(FixRect, FixRect.Left + 1, FixRect.Top + 1, TSMDBColumn(DrawColumn).FilterValue);
    end;

    Canvas.Pen.Color := clDkGray;
    Canvas.MoveTo(FixRect.Left, FixRect.Top);
    Canvas.LineTo(FixRect.Right, FixRect.Top);
  end;
end;

function TSMDBGrid.DrawGroupings(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState): Boolean;
var
  i: Integer;
//  intGroupIndex: Integer;

  {the TRect for drawing simulated combobox}
  RectLookup: TRect;
  W: Integer;
  CalcExpResult: Boolean;
  str: string;
  MasterCol: TColumn;
begin
  Result := False;
  if not Assigned(Column) then exit;
//    if (DataCol > LeftCol) then exit;

  {iterate thru groups and check if we must draw a group row}
//  intGroupIndex := -1;
  for i := 0 to Groupings.Count-1 do
  begin
    with Groupings[i] do
    begin
      if (Expression <> '') then
      begin
        CalcExpResult := False;
        str := Column.Field.DisplayText;
        if Assigned(OnExpression) then
          OnExpression(Self, Expression, str, CalcExpResult);
        if CalcExpResult then
        begin
//          intGroupIndex := i;

          RectLookup := Rect;
          if (dgIndicator in Options) then
            RectLookup.Left := WidthOfIndicator + 1
          else
            RectLookup.Left := 1;
          MasterCol := Column;
          if Columns.Count > 0 then
            W := CalcTitleRect1(Columns[Columns.Count-1], 0, MasterCol).Right
          else
            W := 0;
          if (W <> 0) then
            RectLookup.Right := W
          else
            RectLookup.Right := ClientWidth;

          CalcExpResult := True;
          if Assigned(OnDrawGroupingCell) then
            FOnDrawGroupingCell(Self, Canvas, RectLookup, Groupings[i], str, CalcExpResult);

          if CalcExpResult then
          begin
            Result := True;

            {clear a canvas}
            Canvas.Brush.Color := Color;
            Canvas.Font.Assign(Font);
            if (GridStyle.Grouping.Direction = fdNone) then
              Canvas.FillRect(RectLookup)
            else
              SMDrawGradient(Canvas, RectLookup, GridStyle.Grouping.StartColor, GridStyle.Grouping.EndColor, GridStyle.Grouping.Direction, 255);

            {write the group text}
            WriteTitleText(Canvas, RectLookup, -2 + Ord(gdFixed in State), 2, str, Column.Alignment,
                           teNone,
                           eoTitleWordWrap in ExOptions
                           {$IFDEF DELPHI_4_UP},
                           (BiDiMode <> bdLeftToRight)
//                           IsRightToLeft//UseRightToLeftAlignmentForField(Column.Field, Column.Alignment)
                           {$ENDIF});
          end;

          break
        end
      end
    end
  end;
//  if intGroupIndex < 0 then
//    DefaultDrawColumnCell(Rect, DataCol, Column, State)
end;

procedure TSMDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  i: Integer;
  NewBackground: TColor;
  Highlight, ColumnDrawn: Boolean;
  Bmp: TBitmap;
  Field: TField;

  {the TRect for drawing simulated combobox}
  RectLookup: TRect;
  W, intMidX: Integer;
  CheckState: TCheckBoxState;
begin
  ColumnDrawn := False;
  with RectLookup do
  begin
    Left := Rect.Right - (Rect.Bottom - Rect.Top)+1;
    Top := Rect.Top+1;
    Right := Rect.Right-1;
    Bottom := Rect.Bottom-1;
  end;

  Field := Column.Field;
  Highlight := (gdSelected in State) and
               ((dgAlwaysShowSelection in Options) or Focused);
  if (gdFixed in State) then
    NewBackground := FixedColor
  else
  begin
    if Highlight or HighlightCell(DataCol, DataLink.ActiveRecord, Field.AsString, State) then
    begin
      NewBackground := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end
    else
      NewBackground := {Column.Color;// }Canvas.Brush.Color;
  end;
  GetCellProps(Column.Index, Field, Canvas.Font, NewBackground, Highlight or ActiveRowSelected);
  Canvas.Brush.Color := NewBackground;

  if DefaultDrawing then
  begin
    i := GetImageIndex(Field);
    if i >= 0 then
    begin
      Bmp := GetGridBitmap(TGridPicture(i));
      Canvas.FillRect(Rect);
      DrawBitmapTransparent(Canvas, (Rect.Left + Rect.Right - Bmp.Width) div 2,
        (Rect.Top + Rect.Bottom - Bmp.Height) div 2, Bmp, clOlive);
//      if Assigned(bmp) then
//        bmp.Free;
    end
    else
      if DrawGroupings(Rect, DataCol, Column, State) then
        exit;
  end;

  if Columns.State = csDefault then
    inherited DrawDataCell(Rect, Field, State);

  if DefaultDrawing and
//20051027     (GridStyle.Style <> gsNormal) and
     not (eoShowLookup in ExOptions) and
     (Column is TSMDBColumn) and (TSMDBColumn(Column).TextEllipsis = teNone) then
    DefaultDrawColumnCell(Rect, DataCol, Column, State);
  inherited DrawColumnCell(Rect, DataCol, Column, State);

  if DefaultDrawing and Highlight and not (csDesigning in ComponentState)
     and not (dgRowSelect in Options)
     and (ValidParentForm(Self).ActiveControl = Self) then
    Canvas.DrawFocusRect(Rect);

  if DefaultDrawing and
     (eoDrawGraphicField in FExOptions) and
     (Datalink <> nil) and
     Datalink.Active and
     Assigned(Column.Field) and
     (Column.Field is TBlobField) and
     (Column.Field.DataType in [ftBlob,ftGraphic,ftTypedBinary,ftParadoxOle,ftDBaseOle]) then
  begin
    Canvas.FillRect(Rect);
    DrawGraphic(Rect, Column.Field);
    ColumnDrawn := True;
  end;

  if DefaultDrawing and
     (Datalink <> nil) and
     Datalink.Active and
     ColumnIsCheckbox(Column) then
  begin
    CheckState := cbUnChecked;
    try
      if Column.Field.IsNull then
        CheckState := cbUnChecked
      else
      if Column.Field.Value then
        CheckState := cbChecked
    except
    end;
    Canvas.FillRect(Rect);
    DrawCheckBox(Rect, CheckState, taCenter);
    ColumnDrawn := True;
  end;

  if (eoShowLookup in ExOptions) then
  begin
    if (Column.Field.FieldKind = fkLookup) or
       (Column.PickList.Count > 0) then
    begin //Drawing combobox if FieldKind is lookup
      Canvas.FillRect(Rect);
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
      {Drawing combobox-area }
      DrawComboArrow(RectLookup);
    end
    else
      if (Column.ButtonStyle = cbsEllipsis) then
      begin
        {Show "?" when ButtonStyle Property is cbsEllipsis }
//        DrawFrameControl(Canvas.Handle, RectLookup, DFC_CAPTION, DFCS_CAPTIONHELP)

        Canvas.FillRect(RectLookup);
        DrawEdge(Canvas.Handle, RectLookup, EDGE_RAISED, BF_RECT or BF_MIDDLE);
        intMidX := (RectLookup.Right - RectLookup.Left) shr 1;
        W := (RectLookup.Bottom - RectLookup.Top) shr 3;
        if W = 0 then W := 1;
        PatBlt(Canvas.Handle, RectLookup.Left + intMidX, RectLookup.Top + intMidX, W, W, BLACKNESS);
        PatBlt(Canvas.Handle, RectLookup.Left + intMidX - (W * 2), RectLookup.Top + intMidX, W, W, BLACKNESS);
        PatBlt(Canvas.Handle, RectLookup.Left + intMidX + (W * 2), RectLookup.Top + intMidX, W, W, BLACKNESS);

      end
      else
      if not ColumnDrawn then
      begin
        {Draw in default except above conditions}
        DefaultDrawColumnCell(Rect, DataCol, Column, State);
      end
  end;

  {draw title}
//  DrawCell(SelectedIndex+1, 0, Rect, [gdFixed]);
end;

{thanks for idea to Giovanni Premuda <gpremuda@softwerk.it>}
function TSMDBGrid.GetRecordCount: Integer;
var
  i: Integer;
begin
  i := DataLink.DataSet.RecordCount;
  if Assigned(OnGetRecordCount) then
     OnGetRecordCount(Self, i);
  Result := i;
end;

procedure TSMDBGrid.CmbFilterChange(Sender: TObject);
begin
  if eoAnyKeyFilter in ExOptions then
    CmbFilterClick(Self);
end;

procedure TSMDBGrid.CmbFilterClick(Sender : TObject);
begin
  with CmbFilter do
  begin
    if Assigned(Column) and
       (Column.FilterValue <> Text) then
    begin
      Column.FilterValue := Text;
      ApplyFilter;
    end;
  end;
end;

procedure TSMDBGrid.CmbFilterExit(Sender: TObject);
begin
  with CmbFilter do
  begin
    Visible := False;
    if eoAnyKeyFilter in ExOptions then
      Exit;

    if Assigned(Column) and (Column.FilterValue <> Text) then
    begin
      Column.FilterValue := Text;
      ApplyFilter;
      Column := nil;
    end;
    Items.Clear;
    Text := '';
  end;
end;

procedure TSMDBGrid.CmbFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    with CmbFilter do
    begin
      if Assigned(Column) and (Column.FilterValue <> Text) then
      begin
        Column.FilterValue := Text;
        ApplyFilter;
      end;
    end;
  end;
end;

procedure TSMDBGrid.OnFilterRecord(DataSet: TDataSet; var Accept: Boolean);
var
  i, intPos: Integer;
  IsWildCard: Boolean;
  strFValue: string;
begin
  Accept := True;
  for i := 0 to Columns.Count - 1 do
  begin
    if (Columns[i] is TSMDBColumn) then
    begin
      with TSMDBColumn(Columns[i]) do
      begin
        intPos := Pos('*', FilterValue);
        IsWildCard := (intPos > 0);
        strFValue := FilterValue;
        Delete(strFValue, intPos, 1);
        Accept := (strFValue = '') or
                  ((not IsWildCard) and (Field is TNumericField) and (Field.AsString = strFValue)) or
                  ((not (Field is TNumericField) or IsWildCard) and (Pos(UpperAccentCase(strFValue), UpperAccentCase(Field.AsString)) > 0));
      end;
      if not Accept then
        break;
    end;
  end;
end;

procedure TSMDBGrid.ClearFilter;
var
  i: Integer;
begin
  for i := 0 to Columns.Count - 1 do
    if (Columns[i] is TSMDBColumn) then
      TSMDBColumn(Columns[i]).FilterValue := '';

  ApplyFilter;
end;

procedure TSMDBGrid.ApplyFilter;
var
  TempCol: Integer;
begin
  if (eoFilterAutoApply in ExOptions) then
  begin
    TempCol := LeftCol;
    BeginUpdate;
    try
      if (DataLink.Active) then
      begin
        DataLink.DataSet.Filtered := False;
        DataLink.DataSet.OnFilterRecord := OnFilterRecord;
        DataLink.DataSet.Filtered := True;
      end;
    finally
      LeftCol := TempCol;
      EndUpdate;
    end;
  end;

  if Assigned(OnFilterChanged) then
    OnFilterChanged(Self);
end;

function TSMDBGrid.UpperAccentCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then
      Dec(Ch, 32)
    else
      case Ch of
        'á', 'Á': Ch := 'A';
        'é', 'É': Ch := 'E';
        'í', 'Í': Ch := 'I';
        'ó', 'Ó' : Ch := 'O';
        'ú', 'Ú' : Ch := 'U';
        'ñ' : Ch := 'Ñ';
      end;
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

{$IFDEF DELPHI_5_UP}
function TSMDBGrid.GetColumns: TSMDBGridColumns;
begin
  Result := TSMDBGridColumns(inherited Columns)
end;

procedure TSMDBGrid.SetColumns(Value: TSMDBGridColumns);
begin
  TSMDBGridColumns(Columns).Assign(Value)
end;
{$ENDIF}

const
  THEME_DLL = 'uxtheme.dll';

  ComCtlVersionIE3 = $00040046;
  ComCtlVersionIE4 = $00040047;
  ComCtlVersionIE401 = $00040048;
  ComCtlVersionIE5 = $00050050;
  ComCtlVersionIE501 = $00050051;
  ComCtlVersionIE6 = $00060000;

var
  FThemeAPILoaded: Boolean;
  hThemeAPI: THandle;

function IsXPThemesEnabled: Boolean;
begin
  Result := FThemeAPILoaded and IsThemeActive and IsAppThemed
end;

{$IFNDEF SMForDelphi7}
function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := Cardinal(-1);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result:= FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;
{$ENDIF}

procedure LoadXPThemeAPI;
var
  iErrorMode: Integer;
begin
  FThemeAPILoaded := False;

  if (GetFileVersion(comctl32) < ComCtlVersionIE6) then exit;

  iErrorMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  hThemeAPI := LoadLibrary(THEME_DLL);
  SetErrorMode(iErrorMode);

  if (hThemeAPI <> 0) then
  begin
    @OpenThemeData := GetProcAddress(hThemeAPI, 'OpenThemeData');
    @CloseThemeData := GetProcAddress(hThemeAPI, 'CloseThemeData');

    @DrawThemeBackground := GetProcAddress(hThemeAPI, 'DrawThemeBackground');

    @IsThemeActive := GetProcAddress(hThemeAPI, 'IsThemeActive');
    @IsAppThemed := GetProcAddress(hThemeAPI, 'IsAppThemed');

    FThemeAPILoaded := True;
  end;
end;

procedure UnloadXPThemeAPI;
begin
  if FThemeAPILoaded then
  begin
    FThemeAPILoaded := False;
    FreeLibrary(hThemeAPI);
  end;
end;

initialization
  LoadXPThemeAPI;

  RegisterClass(TSMDBColumn);
  RegisterClass(TSMDBGridColumns);

finalization
  DestroyLocals;

  UnRegisterClass(TSMDBGridColumns);
  UnRegisterClass(TSMDBColumn);

  UnloadXPThemeAPI;

end.
