{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}

{ Copyright (C) 1998-2000, written by Shkolnik Mike
  FIDOnet: 2:463/106.14
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

English:
  The successor TDBGrid with the extended features.
  Is able to display multiline wordwrap column titles,
  checkboxs for boolean fields, checkboxs for record selecting,
  fixing of columns, a convenient select of records from the keyboard,
  stretch drawing of the graphic fields in the cells,
  possibility to exclude insert and delete of records in the DBGrid,
  own standard PopupMenu, save/restore of a column states, processing of
  additional events etc.

  1. movement from column to column by ENTER key (like TAB)
  2. multiline wordwrap column titles (partly is transfered
     from TBitDBGrid - Ilya Andreev, ilya_andreev@geocities.com
     FIDONet: 2:5030/55.28 AKA 2:5030/402.17)
  3. display opportunity of selected record mark (like checkbox)
  4. editing of boolean fields like checkbox
  5. a convenient select of records from keyboard (is transfered from TRXDBGrid, RXLibrary)
  6. an opportunity to exclude insert and delete of records in the SMDBGrid
  7. save and restore of the column order and column width in the INI-file
  8. own PopUp-menu with standard items (Add/Edit/Delete record, Print/Export
     data, Save/Cancel changes, Refresh data, Select/UnSelect records,
     Save/Restore layout)
  9. fixing of the few columns in horizontal scrolling
 10. delete of the all selected records
 11. Refresh of the data in SMDBGrid (useful for TQuery because Refresh
     correctly works only for TTable)
 12. processing of events by pressing on column title (is transfered
     from TRXDBGrid, RXLibrary)
 13. ability of display of the MEMO/BLOB/PICTURE-fields as Bitmap (is
     transfered from TRXDBGrid, RXLibrary)
 14. display hints for each cells if cell text is cutted by column width
     (transfered from TBitDBGrid - Ilya Andreev, ilya_andreev@geocities.com
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

PS: in archive there are English, French, German, Italian, Dutch,
    Brazilian Portuguese, Russian, Ukrainian and Japan resources
    (view a file SMCnst.PAS in Resourse directory).
If anybody want to send a native resources, then I shall include it in next build.


Thanks to native tranclators:
- Remy (walloon@euronet.be) for French resources
- Thomas Grimm (tgrimm@allegro-itc.de) for German resources
- Naohiro Fukuda (nao@nagoya.terracom.co.jp) for Japan resources
- Julian (gzorzi@misam.it) for Italian resources
- Rodrigo Hjort (rodrigo_hjort@excite.com) for Brazilian Portuguese resources
- sam francke (s.j.francke@hccnet.nl) for Dutch resources
- Daniel Ramirez Jaime (rdaniel2000@hotmail.com) for Spanish Mexican resources

I want to thank Naohiro Fukuda (nao@nagoya.terracom.co.jp)
and Remy (walloon@euronet.be), due to which in TSMDBGrid
there was much less errors and bugs and for their sentences
on improverment a component.
}

unit SMDBGrid;

interface

{$I compilers.inc}

uses
{$ifdef COMPILER_9_UP}
  Variants,
{$endif}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Grids, DBGrids, DB, StdCtrls, SMCnst;

type
  TExOptions = set of (eoBooleanAsCheckBox,
                       eoCheckBoxSelect, eoCellHint,
                       eoDisableDelete, eoDisableInsert, eoDrawGraphicField,
                       eoENTERlikeTAB, eoFixedLikeColumn,
                       eoKeepSelection, eoLayout,
                       eoSelectedTitle, eoShowGlyphs, eoShowLookup, eoStandardPopup,
                       eoTitleButtons);

type
  {start cutting from TRxDBGrid}
  TCheckTitleBtnEvent = procedure (Sender: TObject; ACol: Longint; Field: TField; var Enabled: Boolean) of object;
  TGetCellParamsEvent = procedure (Sender: TObject; Field: TField; AFont: TFont; var Background: TColor; Highlight: Boolean) of object;
  TGetBtnParamsEvent = procedure (Sender: TObject; Field: TField; AFont: TFont; var Background: TColor; IsDown: Boolean) of object;
  {end cutting from TRxDBGrid}

  TGetGlyphEvent = procedure (Sender: TObject; var Bitmap: TBitmap) of object;

type
  TSMSortType = (stNone, stAscending, stDescending);

  TSMSortColumn = class
    FieldName: string;
    SortCaption: string;
    SortType: TSMSortType;
  end;

  TSMDBGrid = class;

  TSMListSortColumns = class(TList)
  private
    function GetColumn(Index: Integer): TSMSortColumn;
    procedure SetColumn(Index: Integer; Value: TSMSortColumn);
  public
    function Add: TSMSortColumn;
    procedure RebuildColumns(Grid: TSMDBGrid);

    property Items[Index: Integer]: TSMSortColumn read GetColumn write SetColumn; default;
  end;


  TSMDBGrid = class(TDBGrid)
  private
    { Private declarations }
    FExOptions: TExOptions;

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
    FOnChangeSelection: TNotifyEvent;

    FOnDrawColumnTitle: TDrawColumnCellEvent;
    FOnGetGlyph: TGetGlyphEvent;
    FWidthOfIndicator: Integer;

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

    procedure SaveLayoutClick(Sender: TObject);
    procedure RestoreLayoutClick(Sender: TObject);

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

   {partly is transfered from TBitDBGrid:
    Ilya Andreev, ilya_andreev@geocities.com
    FIDONet: 2:5030/55.28 AKA 2:5030/402.17}
    procedure SetTitlesHeight;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    {end of transfered}

    function GetSortImageWidth: Integer;
  protected
    { Protected declarations }
//    procedure Paint; override;

    {start cutting from TRxDBGrid}
    function HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TGridDrawState): Boolean; override;
    procedure Scroll(Distance: Integer); override;

    procedure LayoutChanged; override;
    procedure ColWidthsChanged; override;
    procedure SetColumnAttributes; override;
    procedure TopLeftChanged; override;
    function CanEditShow: Boolean; override;

    procedure CheckTitleButton(ACol: Longint; var Enabled: Boolean); dynamic;
    procedure GetCellProps(Field: TField; AFont: TFont; var Background: TColor;
      Highlight: Boolean); dynamic;
    {end cutting from TRxDBGrid}

    procedure CellClick(Column: TColumn); override;
    function CellRectForDraw(R: TRect; ACol: Longint): TRect;

    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    function GetGlyph: TBitmap; virtual;
    procedure DrawCheckBox(R: TRect; AState: TCheckBoxState; al: TAlignment); virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    { added by anse 07.08.02 }
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; Override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; Override;
  public
    { Public declarations }
    SortColumns: TSMListSortColumns;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteData;
    procedure RefreshData;

    procedure SelectOneClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure UnSelectOneClick(Sender: TObject);
    procedure UnSelectAllClick(Sender: TObject);

    procedure SaveLayoutToRegistry;
    procedure RestoreLayoutFromRegistry;

    procedure ToggleRowSelection;
    procedure GotoSelection(Index: Longint);

    procedure DisableScroll;
    procedure EnableScroll;
    function ScrollDisabled: Boolean;

    property IndicatorOffset;
    property TitleOffset: Byte read GetTitleOffset;
  published
    { Published declarations }

    property GridLineWidth;
    property ExOptions: TExOptions read FExOptions write SetExOptions;

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
    property OnCheckButton: TCheckTitleBtnEvent read FOnCheckButton write FOnCheckButton;
    property OnChangeSelection: TNotifyEvent read FOnChangeSelection write FOnChangeSelection;

    property OnSetupGrid: TNotifyEvent read FOnSetupGrid write FOnSetupGrid;
    property OnDrawColumnTitle: TDrawColumnCellEvent read FOnDrawColumnTitle write FOnDrawColumnTitle;
    property OnGetGlyph: TGetGlyphEvent read FOnGetGlyph write FOnGetGlyph;
    property WidthOfIndicator: Integer read FWidthOfIndicator write SetIndicatorWidth;

    property ScrollBars;
    property ColCount;
    property RowCount;
    property VisibleColCount;
    property VisibleRowCount;
    property Col;
    property Row;

    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
  end;

procedure Register;

implementation
uses RXUtils {ex VCLUtils from RX-Lib}, TypInfo, Registry, DBTables
     {$IFDEF VER140} , Variants {$ENDIF};

{$R *.RES}
var
  FCheckWidth, FCheckHeight: Integer;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBGrid]);
end;



function TSMDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
   result:=false;
   if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
   if not result then
    if (DataSource<>nil) and (DataSource.DataSet<>nil) then
     begin
      DataSource.DataSet.Next;
      result:=true;
     end;
end;

function TSMDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  result:=false;
   if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not result then
    if (DataSource<>nil) and (DataSource.DataSet<>nil) then
     begin
      DataSource.DataSet.Prior;
      result:=true;
     end;
end;


{ TSMListSortColumns }
function TSMListSortColumns.Add: TSMSortColumn;
begin
  Result := TSMSortColumn.Create;

  inherited Add(Result);
end;

function TSMListSortColumns.GetColumn(Index: Integer): TSMSortColumn;
begin
  Result := TSMSortColumn(inherited Items[Index]);
end;

procedure TSMListSortColumns.SetColumn(Index: Integer; Value: TSMSortColumn);
begin
  Items[Index] := Value;
end;

procedure TSMListSortColumns.RebuildColumns(Grid: TSMDBGrid);
var i: Integer;
begin
  if Assigned(Grid) and Assigned(Grid.DataSource) and
    Assigned(Grid.DataSource.DataSet) then
  begin
    Grid.BeginLayout;
    try
      Clear;
      with Grid.DataSource.DataSet do
        for i := 0 to FieldCount-1 do
          Add.FieldName := Fields[i].FieldName
    finally
      Grid.EndLayout;
    end
  end
  else
    Clear;
end;





type
  TBookmarks = class(TBookmarkList);
  TGridPicture = (gpBlob, gpMemo, gpPicture, gpOle, gpSortAsc, gpSortDesc);

const
  GridBmpNames: array[TGridPicture] of PChar = ('SM_BLOB', 'SM_MEMO', 'SM_PICT', 'SM_OLE',
                                                'SM_ARROWASC', 'SM_ARROWDESC');
  GridBitmaps: array[TGridPicture] of TBitmap = (nil, nil, nil, nil, nil, nil);
  bmMultiDot = 'SM_MSDOT';
  bmMultiArrow = 'SM_MSARROW';
  bmMultiCheckBox = 'SM_MSCHECKBOX';

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
var I: TGridPicture;
begin
  for I := Low(TGridPicture) to High(TGridPicture) do
    GridBitmaps[I].Free;
end;

procedure GridInvalidateRow(Grid: TSMDBGrid; Row: Longint);
var I: Longint;
begin
  for I := 0 to Grid.ColCount - 1 do Grid.InvalidateCell(I, Row);
end;

procedure GetCheckBoxSize;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(32759));
      FCheckWidth := Width div 4;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
end;

constructor TSMDBGrid.Create(AOwner: TComponent);
var NewItem: TMenuItem;
    j: Integer;
    Bmp: TBitmap;
begin
  inherited Create(AOwner);

  SortColumns := TSMListSortColumns.Create;

  FRegistryKey := 'Software\MikeSoft';
  FRegistrySection := 'SMDBGrid';

  Bmp := TBitmap.Create;
  try
    Bmp.Handle := LoadBitmap(hInstance, bmMultiDot);
    FMsIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBitmap(hInstance, bmMultiArrow);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBitmap(hInstance, bmMultiCheckBox);
    FMsIndicators.AddMasked(Bmp, clWhite);

  finally
    Bmp.Free;
  end;
  FPressedCol := -1;

  FDBPopUpMenu := TPopUpMenu.Create(Self {AOwner});
  if not (csDesigning in ComponentState) then
  begin

    for j := 0 to High(PopUpCaption) do
    begin
      NewItem := TMenuItem.Create(Self);
      NewItem.Caption := PopUpCaption[j];
      case j of
        0: NewItem.OnClick := AppendClick;
        1: NewItem.OnClick := InsertClick;
        2: NewItem.OnClick := EditClick;
        3: NewItem.OnClick := DeleteClick;

        5: NewItem.OnClick := PrintClick;
        6: NewItem.OnClick := ExportClick;

        8: NewItem.OnClick := PostClick;
        9: NewItem.OnClick := CancelClick;
       10: NewItem.OnClick := RefreshClick;

       13: NewItem.OnClick := SelectOneClick;
       14: NewItem.OnClick := SelectAllClick;
       16: NewItem.OnClick := UnSelectOneClick;
       17: NewItem.OnClick := UnSelectAllClick;

       19: NewItem.OnClick := SaveLayoutClick;
       20: NewItem.OnClick := RestoreLayoutClick;

       22: NewItem.OnClick := SetupGridClick;
      end;
      if j in [13, 14, 15, 16, 17] then
        FDBPopUpMenu.Items[12].Add(NewItem)
      else
        FDBPopUpMenu.Items.Add(NewItem);
    end;
  end;
//  PopUpMenu := FDBPopUpMenu;

  GetCheckBoxSize;
  FWidthOfIndicator := IndicatorWidth;

  FExOptions := [eoENTERlikeTAB, eoKeepSelection, eoStandardPopup];
//  ScrollBars := ssBoth;
//  Color := clInfoBk;
end;

destructor TSMDBGrid.Destroy;
begin
  SortColumns.Free;
  FDBPopUpMenu.Free;

  FMsIndicators.Free;

  inherited Destroy;
end;

{procedure TSMDBGrid.Paint;
begin
  if ScrollBars in [ssNone, ssHorizontal] then
    SetScrollRange(Self.Handle, SB_VERT, 0, 0, False);
  if ScrollBars in [ssNone, ssVertical] then
    SetScrollRange(Self.Handle, SB_HORZ, 0, 0, False);

  inherited Paint;
end;
}

{Standard popup menu events}
procedure TSMDBGrid.AppendClick(Sender: TObject);
begin
  if Assigned(FOnAppendRecord) then
    FOnAppendRecord(Sender)
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

procedure TSMDBGrid.EditClick(Sender: TObject);
begin
  if Assigned(FOnEditRecord) then
    FOnEditRecord(Sender)
  else
    Datalink.DataSet.Edit;
end;

procedure TSMDBGrid.DeleteClick(Sender: TObject);
begin
  if Assigned(FOnDeleteRecord) then
    FOnDeleteRecord(Sender)
  else
    DeleteData;
end;

procedure TSMDBGrid.PrintClick(Sender: TObject);
begin
  if Assigned(FOnPrintData) then
    FOnPrintData(Sender)
end;

procedure TSMDBGrid.ExportClick(Sender: TObject);
begin
  if Assigned(FOnexportData) then
    FOnExportData(Sender)
end;

procedure TSMDBGrid.PostClick(Sender: TObject);
begin
  if Assigned(FOnPostData) then
    FOnPostData(Sender)
  else
    Datalink.DataSet.Post;
end;

procedure TSMDBGrid.CancelClick(Sender: TObject);
begin
  if Assigned(FOnCancelData) then
    FOnCancelData(Sender)
  else
    Datalink.DataSet.Cancel;
end;

procedure TSMDBGrid.RefreshClick(Sender: TObject);
begin
  if Assigned(FOnRefreshData) then
    FOnRefreshData(Sender)
  else
    RefreshData;
end;

procedure TSMDBGrid.SetupGridClick(Sender: TObject);
begin
  if Assigned(FOnSetupGrid) then
    FOnSetupGrid(Sender)
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
var Index: Integer;
begin
  Result := False;
  if (dgMultiSelect in Options) and Datalink.Active then
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
begin
  if (dgMultiSelect in Options) and Datalink.Active then
  begin
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

{partly is transfered from TBitDBGrid:
 Ilya Andreev, ilya_andreev@geocities.com
 FIDONet: 2:5030/55.28 AKA 2:5030/402.17}
procedure TSMDBGrid.SetTitlesHeight;
var
  i, MaxHeight: Integer;
  RRect: TRect;
  pt: Integer;
  s: string;
begin
  if (dgTitles in Options) then
  begin
    {recalculate a title height}
    MaxHeight := 0;
    for i := 0 to Columns.Count - 1 do
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
                       DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK));
    end;

    if (MaxHeight <> 0) then
    begin
      if (dgRowLines in Options) then
        Inc(MaxHeight, 3)
      else
        Inc(MaxHeight, 2);
      if (eoTitleButtons in ExOptions) then
        Inc(MaxHeight, 2);
      RowHeights[0] := MaxHeight+4
    end;
  end;
end;
{end of transfered}

procedure TSMDBGrid.LayoutChanged;
var ACol: Longint;
begin
  ACol := Col;
  inherited LayoutChanged;
  if Datalink.Active and (FixedCols > 0) then
    Col := Min(Max(inherited FixedCols, ACol), ColCount - 1);

  {recalculate a title height}
  SetTitlesHeight;
end;

procedure TSMDBGrid.ColWidthsChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited ColWidthsChanged;
  if Datalink.Active and (FixedCols > 0) then
    Col := Min(Max(inherited FixedCols, ACol), ColCount - 1);
end;

procedure TSMDBGrid.SetIndicatorWidth(Value: Integer);
var FrameOffs: Byte;
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
    ColWidths[0] := WidthOfIndicator;

  SetFixedCols(FFixedCols);
end;

function TSMDBGrid.GetTitleOffset: Byte;
begin
  Result := 0;
  if dgTitles in Options then
    Inc(Result);
end;

procedure TSMDBGrid.SetFixedCols(Value: Integer);
var FixCount, i: Integer;
begin
  FixCount := Max(Value, 0) + IndicatorOffset;
  if DataLink.Active and not (csLoading in ComponentState) and
    (ColCount > IndicatorOffset + 1) then
  begin
    FixCount := Min(FixCount, ColCount - 1);
    inherited FixedCols := FixCount;
    for i := 1 to Min(FixedCols, ColCount - 1) do
      TabStops[i] := False;
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
var ABookmark: TBookmark;
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
  SaveLayoutToRegistry;
end;

procedure TSMDBGrid.RestoreLayoutClick(Sender: TObject);
begin
  RestoreLayoutFromRegistry;
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
var bookPosition: TBookMark;
    boolContinue: Boolean;
begin
  boolContinue := True;

  {if needs, save the changed data}
  if Assigned(Datalink.DataSet) then
  begin
     with Datalink.DataSet do
     begin
       if (State in [dsInsert, dsEdit]) and CanModify then Post;
       if (Datalink.DataSet is TBDEDataSet) then
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

procedure TSMDBGrid.SetExOptions(Val: TExOptions);
var FrameOffs: Byte;
begin
  if (FExOptions <> Val) then
  begin
    FExOptions := Val;


    if ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
      FrameOffs := 1
    else
      FrameOffs := 2;

    if (eoCheckBoxSelect in Val) then
    begin
      if (WidthOfIndicator = IndicatorWidth) then
        WidthOfIndicator := FCheckWidth + 4*FrameOffs + FMsIndicators.Width;
    end
    else
    begin
      if (WidthOfIndicator = FCheckWidth + 4*FrameOffs + FMsIndicators.Width) then
        WidthOfIndicator := IndicatorWidth;
    end;
    Invalidate;
  end;
end;

function TSMDBGrid.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow;

  if Result and
     (Datalink <> nil) and
     Datalink.Active and
     (FieldCount > 0) and
     (SelectedIndex < FieldCount) and
     (SelectedIndex >= 0) and
     (FieldCount <= DataSource.DataSet.FieldCount) and
     (Fields[SelectedIndex] <> nil) then
    Result := GetImageIndex(Fields[SelectedIndex]) < 0;
  if Result and
     (eoBooleanAsCheckBox in FExOptions) and
     Assigned(Fields[SelectedIndex]) and
     (Fields[SelectedIndex].DataType = ftBoolean) then
    Result := False
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

procedure TSMDBGrid.GetCellProps(Field: TField; AFont: TFont;
  var Background: TColor; Highlight: Boolean);
begin
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
var IndicatorRect: TRect;
begin
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

procedure TSMDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyDownEvent: TKeyEvent;

  function ItAddLastRecord: Boolean;
  begin
    Result := (eoDisableInsert in FExOptions) and
              (Datalink.ActiveRecord >= Datalink.RecordCount-1);
  end;

  procedure ClearSelections;
  begin
    if (dgMultiSelect in Options) then
    begin
      if not (eoKeepSelection in ExOptions) then
      begin
        SelectedRows.Clear;
        if Assigned(FOnChangeSelection) then
          FOnChangeSelection(Self);
      end;
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
            if Assigned(FOnChangeSelection) then
              FOnChangeSelection(Self);
            FSelecting := True;
            AddAfter := True;
          end
          else
            with TBookmarks(SelectedRows) do
            begin
              AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
              if not AddAfter then
              begin
                CurrentRowSelected := False;
                if Assigned(FOnChangeSelection) then
                  FOnChangeSelection(Self);
              end
            end
        end
        else
          ClearSelections;
      if Direction <> 0 then
        Datalink.DataSet.MoveBy(Direction);
      if AddAfter then
      begin
        SelectedRows.CurrentRowSelected := True;
        if Assigned(FOnChangeSelection) then
          FOnChangeSelection(Self);
      end;
    finally
      EndUpdate;
    end;
  end;

  procedure NextRow(Select: Boolean);
  begin
    with Datalink.Dataset do begin
      DoSelection(Select, 1);
      if EOF and CanModify and (not ReadOnly) and (dgEditing in Options) and
         not ItAddLastRecord then
        AppendClick(Self);
    end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
    DoSelection(Select, -1);
  end;

  procedure CheckTab(GoForward: Boolean);
  var ACol, Original: Integer;
  begin
    ACol := Col;
    Original := ACol;
    if (dgMultiSelect in Options) and DataLink.Active then
      while True do
      begin
        if GoForward then
          Inc(ACol)
        else
          Dec(ACol);
        if ACol >= ColCount then
        begin
          ClearSelections;
          ACol := IndicatorOffset;
        end
        else
          if ACol < IndicatorOffset then
          begin
            ClearSelections;
            ACol := ColCount;
          end;
        if ACol = Original then Exit;
        if TabStops[ACol] then Exit;
      end;
  end;

const
  RowMovementKeys = [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END];

begin
  KeyDownEvent := OnKeyDown;
  if Assigned(KeyDownEvent) then
    KeyDownEvent(Self, Key, Shift);
  if not Datalink.Active or not CanGridAcceptKey(Key, Shift) then Exit;
  with Datalink.DataSet do
    if ssCtrl in Shift then
    begin
      if (Key in RowMovementKeys) then
        ClearSelections;

      case Key of
        VK_LEFT: if FixedCols > 0 then
                 begin
                   SelectedIndex := FixedCols;
                   Exit;
                 end;
        VK_DELETE: begin
                     if (eoDisableDelete in FExOptions) then Exit;
                     if not ReadOnly and CanModify then
                     begin
                       DeleteClick(nil);
                       Exit;
                     end;
                   end;
      end
    end
    else
    begin
      case Key of
        VK_LEFT: if (FixedCols > 0) and not (dgRowSelect in Options) then
                 begin
                   if SelectedIndex <= FFixedCols then Exit;
                 end;
        VK_HOME: if (FixedCols > 0) and (ColCount <> IndicatorOffset + 1) and
                    not (dgRowSelect in Options) then
                 begin
                   SelectedIndex := FixedCols;
                   Exit;
                 end;
        VK_SPACE: if (eoBooleanAsCheckbox in FExOptions) and
                     (Datalink <> nil) and Datalink.Active and
                     (Columns[SelectedIndex].Field.DataType = ftBoolean) then
                    CellClick(Columns[SelectedIndex]);
      end;
      case Key of
        VK_DOWN: begin
                   NextRow(True);
                   Exit;
                 end;
        VK_INSERT: if (eoDisableInsert in FExOptions) then Exit;
        VK_UP: begin
                 PriorRow(True);
                 Exit;
               end;
        13:  if (eoENTERlikeTAB in FExOptions)  then
                 {going on next column}
               if (SelectedIndex < Columns.Count-1) then
                 SelectedIndex := SelectedIndex + 1
               else
                 SelectedIndex := 0;
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
end;

procedure TSMDBGrid.TopLeftChanged;
begin
  if (dgRowSelect in Options) and DefaultDrawing then
    GridInvalidateRow(Self, Self.Row);

  inherited TopLeftChanged;
  if FTracking then StopTracking;
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

procedure TSMDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure SetEnabledItems;
  begin
    with FDBPopUpMenu do
    begin
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
//                          and (Datalink.DataSet.RecordCount > 0)
      else
        Items[2].Enabled := False;

      {Delete}
      if Assigned(Datalink.DataSet) then
        Items[3].Enabled := (not (eoDisableDelete in FExOptions)) and
                            (not ReadOnly) and
                            Datalink.DataSet.CanModify and
                            (Datalink.DataSet.State = dsBrowse)
//                             and (Datalink.DataSet.RecordCount > 0)
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

      Items[7].Visible := Items[5].Visible or Items[6].Visible;

      {Post}
      if Assigned(Datalink.DataSet) then
        Items[8].Enabled := (not ReadOnly) and
                            (Datalink.DataSet.State in [dsInsert, dsEdit]) and
                            Datalink.DataSet.CanModify
      else
        Items[8].Enabled := False;

      {Cancel}
      if Assigned(Datalink.DataSet) then
        Items[9].Enabled := (not ReadOnly) and
                            (Datalink.DataSet.State in [dsInsert, dsEdit])
      else
        Items[9].Enabled := False;

      {Refresh}
      if Assigned(Datalink.DataSet) then
        Items[10].Enabled := (Datalink.DataSet.State = dsBrowse)
      else
        Items[10].Enabled := False;

      {select/unselect}
      Items[12].Enabled := Assigned(Datalink.DataSet) and
                           Datalink.DataSet.Active and
                           (dgMultiSelect in Options);

      {save/restore layout}
      Items[14].Enabled := True;
      Items[15].Enabled := True;

      Items[13].Visible := (eoLayout in ExOptions);
      Items[14].Visible := (eoLayout in ExOptions);
      Items[15].Visible := (eoLayout in ExOptions);
      Items[14].Enabled := (eoLayout in ExOptions);
      Items[15].Enabled := (eoLayout in ExOptions);

      {setup of the grid}
      Items[17].Enabled := True;
      Items[17].Visible := Assigned(FOnSetupGrid);
      if Assigned(Datalink.DataSet) then
        Items[17].Enabled := Assigned(FOnSetupGrid)
      else
        Items[17].Enabled := False;
      Items[16].Visible := Items[17].Visible;
    end;
  end;

var
  Cell: TGridCoord;
  MouseDownEvent: TMouseEvent;
  EnableClick: Boolean;
  PopCoord: TPoint;
begin
  if not AcquireFocus then Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    DblClick;
    Exit;
  end;
  if Sizing(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else
  begin
    Cell := MouseCoord(X, Y);

    if not (csDesigning in ComponentState) and
       (eoStandardPopup in FExOptions) and
       ((dgIndicator in Options) and
        (Cell.Y < TitleOffset) and
        (Cell.X < IndicatorOffset) or
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
    if Assigned(MouseDownEvent) then
      MouseDownEvent(Self, Button, Shift, X, Y);
    if not (((csDesigning in ComponentState) or (dgColumnResize in Options)) and
      (Cell.Y < TitleOffset)) and (Button = mbLeft) then
    begin
      if (dgMultiSelect in Options) and Datalink.Active then
        with SelectedRows do
        begin
          FSelecting := False;
          if ssCtrl in Shift then
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
        end;
    end;
  end;
end;

procedure TSMDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cell: TGridCoord;
  ACol: Longint;
  DoClick: Boolean;
begin
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
     (dgMultiSelect in Options) and
     (Cell.X < IndicatorOffset) and
     (Cell.Y >= 0) then
    ToggleRowSelection;

  if (Button = mbLeft) and
     (Cell.X >= IndicatorOffset) and
     (ACol <= FixedCols) and
     (Cell.Y > TitleOffset) then
    CellClick(Columns[ACol])
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

{from Borland sources}
procedure WriteTitleText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment);
const
  AlignFlags: array [TAlignment] of Integer =
     (DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX);
var
  B, R, rect1: TRect;
  txth: Integer;
  {$IFDEF COMPILER_4_UP}
  I: TColorRef;
  {$ELSE}
  I: Integer;
  {$ENDIF}

  lpDTP: TDrawTextParams;
  DrawBitmap: TBitmap;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if GetNearestColor(ACanvas.Handle, I) = I then
  begin
    ACanvas.FillRect(ARect);

    rect1.Left := 0;
    rect1.Top := 0;
    rect1.Right := 0;
    rect1.Bottom := 0;
    rect1 := ARect;

    lpDTP.cbSize := SizeOf(lpDTP);
    lpDTP.uiLengthDrawn := Length(Text);
    lpDTP.iLeftMargin := 0;
    lpDTP.iRightMargin := 0;

    InflateRect(rect1, -DX, -DY);

    txth := DrawTextEx(ACanvas.Handle,PChar(Text), Length(Text),
                       rect1, DT_WORDBREAK or DT_CALCRECT, @lpDTP);

    rect1 := ARect;
    InflateRect(rect1, -DX, -DY);

    rect1.top := rect1.top + ((rect1.Bottom-rect1.top) div 2) - (txth div 2);
    DrawTextEx(ACanvas.Handle, PChar(Text), Length(Text),
               rect1, AlignFlags[Alignment], @lpDTP);
  end
  else
  begin
    DrawBitmap := TBitmap.Create;
    DrawBitmap.Canvas.Lock;
    try
      with DrawBitmap, ARect do
      begin
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
        DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment]);
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
      DrawBitmap.Free;
    end;
  end;
end;

procedure TSMDBGrid.CellClick(Column: TColumn);
var R: TRect;
    BCol: Integer;
begin
  inherited CellClick(Column);

  if (Datalink <> nil) and
     Datalink.Active and
     Assigned(Column.Field) and
     (Column.Field.DataType = ftBoolean) and
     (eoBooleanAsCheckBox in FExOptions) and
     CanEditModify then
  begin
    try
      Column.Field.AsBoolean := not Column.Field.AsBoolean;
//      Column.Field.Value := not Column.Field.Value;
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
var i, j: Integer;
begin
  Result := R;

  j := GetSortImageWidth;
  if (Result.Right-Result.Left > j+4) then
  begin
    for i := 0 to SortColumns.Count-1 do
      if (SortColumns[i].FieldName = Columns[ACol].FieldName) and
         (SortColumns[i].SortType <> stNone) then
        break;
    if (i < SortColumns.Count) then
      Result.Right := Result.Right-j-4;
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

procedure TSMDBGrid.DrawCheckBox(R: TRect; AState: TCheckBoxState; al: TAlignment);
var
  DrawState: Integer;
  DrawRect: TRect;
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

  DrawFrameControl(Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
end;

procedure TSMDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  TitleText: string;
  i, j, idxSort, BCol: LongInt;
  CheckState: TCheckBoxState;

  Down: Boolean;
  SavePen, BackColor: TColor;
  AField: TField;
  OldActive: Longint;
  FrameOffs: Byte;
//  Indicator: Integer;
//  MultiSelected: Boolean;
  BRect, FixRect: TRect;
  DrawColumn: TColumn;
  bmp: TBitmap;
const
  EdgeFlag: array[Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);

begin
  if (dgIndicator in Options) then
    BCol := ACol - 1
  else
    BCol := ACol;

  if (gdFixed in AState) and (ARow = 0) and (dgTitles in Options) and
     ((ACol <> 0) or ((ACol = 0) and (dgIndicator in Options) and (eoStandardPopup in FExOptions))) then
  begin
    // draw border
    if DefaultDrawing then
    begin
      if (ACol = 0) and (dgIndicator in Options) then
      begin
        Down := False;
        Canvas.Brush.Color := FixedColor
      end
      else
      begin
        Down := (eoSelectedTitle in FExOptions) and (BCol = SelectedIndex);
        if Assigned(Columns[BCol]) then
          Canvas.Brush.Color := Columns[BCol].Title.Color;
      end;
      DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_BOTTOMLEFT);
      DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_TOPRIGHT);

      InflateRect(ARect, -1, -1);
      Canvas.FillRect(ARect);
    end;

    j := GetSortImageWidth;
    if (ACol = 0) and
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
      if Assigned(Columns[BCol]) then
      begin
        TitleText := Columns[BCol].Title.Caption;

        {draw a column sorted image}
        //look: whether there is a sorting according this column
        idxSort := -1;
        if (ARect.Right-ARect.Left > j) then
        begin
          for i := 0 to SortColumns.Count-1 do
            if (SortColumns[i].FieldName = Columns[BCol].FieldName) and
               (SortColumns[i].SortType <> stNone) then
            begin
              idxSort := i;
              break
            end;
          if idxSort > -1 then
            ARect.Right := ARect.Right-j;
        end;

        //draw title.caption
        if DefaultDrawing  and (TitleText <> '') then
        begin
          Canvas.Brush.Style := bsClear;
          Canvas.Font := Columns[BCol].Title.Font;
          Canvas.Brush.Color := Columns[BCol].Title.Color;
          WriteTitleText(Canvas, ARect, 2, 2, TitleText, Columns[BCol].Title.Alignment);

          if idxSort > -1 then
          begin
            ARect.Right := ARect.Right+j;

            i := (ARect.Bottom - ARect.Top - j) div 2;
            if (SortColumns[idxSort].SortType = stAscending) then
            begin
              Bmp := GetGridBitmap(gpSortAsc);
{              Canvas.Pen.Color := clBtnShadow;
              Canvas.MoveTo(ARect.Right - 4, ARect.Top + i);
              Canvas.LineTo(ARect.Right - 4 - j, ARect.Top + i);
              Canvas.LineTo(ARect.Right - 4 - (j div 2), ARect.Bottom - i);

              Canvas.Pen.Color := clBtnHighlight;
              Canvas.LineTo(ARect.Right - 4, ARect.Top + i);
}
            end
            else
            begin
              Bmp := GetGridBitmap(gpSortDesc);
{              Canvas.Pen.Color := clBtnHighlight;
              Canvas.MoveTo(ARect.Right - 4 - (j div 2), ARect.Top + i);
              Canvas.LineTo(ARect.Right - 4, ARect.Bottom - i);
              Canvas.LineTo(ARect.Right - 4 - j, ARect.Bottom - i);

              Canvas.Pen.Color := clBtnShadow;
              Canvas.LineTo(ARect.Right - 4 - (j div 2), ARect.Top + i);
}
            end;
            BRect := Bounds(ARect.Right - 4 - j, ARect.Top+i, j, j);
            Canvas.FillRect(BRect);
            DrawBitmapTransparent(Canvas, (BRect.Left + BRect.Right - Bmp.Width) div 2,
                                 (BRect.Top + BRect.Bottom - Bmp.Height) div 2, Bmp, clSilver);

            if (SortColumns[idxSort].SortCaption <> '') then
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
              DrawText(Canvas.Handle,
                       PChar(SortColumns[idxSort].SortCaption),
                       Length(SortColumns[idxSort].SortCaption),
                       BRect,
                       DT_EXPANDTABS or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
            end;
          end;
        end
      end;

    if Assigned(FOnDrawColumnTitle) then
      FOnDrawColumnTitle(Self, ARect, ACol, Columns[BCol], AState);
  end
  else
  begin
    if ((ACol > 0) or (not (dgIndicator in Options) and (ACol = 0))) and DefaultDrawing and
       (eoBooleanAsCheckBox in FExOptions) and
       (Datalink <> nil) and
       Datalink.Active and
       Assigned(Columns[BCol]) and
       Assigned(Columns[BCol].Field) and
       (Columns[BCol].Field.DataType = ftBoolean) and
       (((ARow > 0) and (dgTitles in Options)) or (not (dgTitles in Options))) then
    begin
      DrawColumn := Columns[BCol];

      if Assigned(DrawColumn.Field) then
        TitleText := DrawColumn.Field.DisplayText
      else
        TitleText := '';
      if (BCol <= FixedCols) and (FixedCols > 0) then
        Canvas.Brush.Color := FixedColor
      else
        if HighlightCell(ACol, ARow, TitleText, AState) then
          Canvas.Brush.Color := clHighlight
        else
          Canvas.Brush.Color := DrawColumn.Color;
      Canvas.FillRect(ARect);
      InflateRect(ARect, -2, -2);

      OldActive := DataLink.ActiveRecord;
      CheckState := cbUnChecked;
      try
        DataLink.ActiveRecord := ARow - TitleOffset;

        try
          if DrawColumn.Field.IsNull then
            CheckState := cbUnChecked
          else
            if DrawColumn.Field.Value then
              CheckState := cbChecked
//          TCheckBoxState(DrawColumn.Field.Value);
        except
        end
      finally
        DataLink.ActiveRecord := OldActive;
      end;

      DrawCheckBox(ARect, CheckState, taCenter);
      InflateRect(ARect, 2, 2);
    end
    else
    begin
      if (eoFixedLikeColumn in ExOptions) and
         (ACol > 0) and
         (ACol <= FixedCols) then
        AState := AState - [gdFixed];
      inherited DrawCell(ACol, ARow, ARect, AState)
    end;
  end;

  if (dgIndicator in Options) and (ACol = 0) and (ARow - TitleOffset >= 0) and
     (dgMultiSelect in Options) and (DataLink <> nil) and DataLink.Active {and
    (Datalink.DataSet.State = dsBrowse) }then
  begin
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
//      MultiSelected := ActiveRowSelected;
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

{    if MultiSelected then
    begin
      if (ARow - TitleOffset <> Datalink.ActiveRecord) then
        Indicator := 0
      else //multiselected and current row
        Indicator := 1;

      FMsIndicators.BkColor := FixedColor;
      FMsIndicators.Draw(Self.Canvas, FixRect.Right - FMsIndicators.Width -
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

      DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_BOTTOMRIGHT);
      DrawEdge(Canvas.Handle, ARect, EdgeFlag[Down], BF_TOPLEFT);
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
          WriteTitleText(Canvas, ARect, 2, 2, DrawColumn.Title.Caption, Columns[BCol].Title.Alignment)
        else
          WriteTitleText(Canvas, ARect, 2, 2, '', taLeftJustify);
    finally
      Canvas.Pen.Color := SavePen;
    end;
  end;
end;

procedure TSMDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  i: Integer;
  NewBackgrnd: TColor;
  Highlight: Boolean;
  Bmp: TBitmap;
  Field: TField;

  {the TRect for drawing simulated combobox}
  RectLookup: TRect;
  W, intMidX: Integer;
begin
  with RectLookup do
  begin
    Left := Rect.Right - (Rect.Bottom - Rect.Top)+1;
    Top := Rect.Top+1;
    Right := Rect.Right-1;
    Bottom := Rect.Bottom-1;
  end;

  Field := Column.Field;
  NewBackgrnd := Canvas.Brush.Color;
  Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or Focused);
  GetCellProps(Field, Canvas.Font, NewBackgrnd, Highlight or ActiveRowSelected);
  Canvas.Brush.Color := NewBackgrnd;
  if DefaultDrawing then
  begin
    i := GetImageIndex(Field);
    if i >= 0 then
    begin
      Bmp := GetGridBitmap(TGridPicture(i));
      Canvas.FillRect(Rect);
      DrawBitmapTransparent(Canvas, (Rect.Left + Rect.Right - Bmp.Width) div 2,
        (Rect.Top + Rect.Bottom - Bmp.Height) div 2, Bmp, clOlive);
    end
    else
      DefaultDrawColumnCell(Rect, DataCol, Column, State);

    if (eoDrawGraphicField in FExOptions) and
       (Column.Field is TBlobField) and
       (Column.Field.DataType = ftGraphic) then
    begin
      bmp := TBitmap.Create;
      try
        bmp.Assign(Field);
        Canvas.StretchDraw(Rect, bmp);
      finally
        bmp.Free;
      end;
    end;
  end;

  if Columns.State = csDefault then
    inherited DrawDataCell(Rect, Field, State);

  inherited DrawColumnCell(Rect, DataCol, Column, State);

  if DefaultDrawing and Highlight and not (csDesigning in ComponentState)
     and not (dgRowSelect in Options)
     and (ValidParentForm(Self).ActiveControl = Self) then
    Canvas.DrawFocusRect(Rect);


  if (eoShowLookup in ExOptions) then
  begin
    if (Column.Field.FieldKind = fkLookup) or
       (Column.PickList.Count > 0) then
    begin //Drawing combobox if FieldKind is lookup
      Canvas.FillRect(Rect);
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
      {Drawing combobox-area }
      DrawFrameControl(Canvas.Handle, RectLookup, DFC_SCROLL, DFCS_SCROLLCOMBOBOX);
    end
    else
      if Column.ButtonStyle = cbsEllipsis then
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
        {Draw in default except above conditions}
        DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;

  {draw title}
//  DrawCell(SelectedIndex+1, 0, Rect, [gdFixed]);
end;

{is transferred from TBitDBGrid:
  Ilya Andreev, ilya_andreev@geocities.com
  FIDONet: 2:5030/55.28 AKA 2:5030/402.17}
procedure TSMDBGrid.CMHintShow(var Msg: TMessage);
var ACol, ARow: Integer;
    OldActive: Integer;
begin
 if eoCellHint in FExOptions then
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
      if Columns[ACol].Field <> nil then
        HintStr := Columns[ACol].Field.DisplayText;
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
{end of transfered}

procedure TSMDBGrid.SaveLayoutToRegistry;
var RegIniFile: TRegIniFile;
    i: Integer;
begin
  RegIniFile := TRegIniFile.Create(FRegistryKey);
  RegIniFile.WriteInteger(FRegistrySection, 'Count', Columns.Count);
  for i := 0 to (Columns.Count-1) do
  begin
    with Columns.Items[i] do
      RegIniFile.WriteString(FRegistrySection, IntToStr(i),
                             Format('%s,%d,%s', [FieldName, Width, Title.Caption]));
  end;
  RegIniFile.Free;
end;

procedure TSMDBGrid.RestoreLayoutFromRegistry;

  function GetValueFromKey(var strValues: string): string;
  var j: Integer;
  begin
    j := Pos(',', strValues);
    Result := Copy(strValues, 1, j-1);
    Delete(strValues, 1, j);
  end;

var RegIniFile: TRegIniFile;
    i, Count: Integer;
    s: string;
begin
{ disable DBGrid-repaint while not will executed EndLayout
  Because I donn't want to repaint of the grid after each
  addition and after Columns.Clear }
  BeginLayout;

  RegIniFile := TRegIniFile.Create(FRegistryKey);
  Count := RegIniFile.ReadInteger(FRegistrySection, 'Count', 0);
  if (Count > 0) then
  begin
    Columns.Clear;
    for i := 0 to (Count-1) do
    begin
      S := RegIniFile.ReadString(FRegistrySection, IntToStr(i), '');
      if (S <> '') then
      begin
        with Columns.Add do
        begin
          FieldName := GetValueFromKey(S);
          Width := StrToIntDef(GetValueFromKey(S), 64);
          Title.Caption := S;
        end;
      end;
    end;
  end;

  RegIniFile.Free;
  EndLayout;
end;

initialization
finalization
  DestroyLocals;

end.
