unit table_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, ToolWin, VirtualTrees, WideStrings,
  SynRegExpr, ActiveX, DB, ExtCtrls, ImgList, SynEdit, SynMemo, Menus;

type
  TfrmTableEditor = class(TFrame)
    btnSave: TButton;
    btnDiscard: TButton;
    btnHelp: TButton;
    listColumns: TVirtualStringTree;
    PageControlMain: TPageControl;
    tabBasic: TTabSheet;
    tabIndexes: TTabSheet;
    tabOptions: TTabSheet;
    lblName: TLabel;
    editName: TTntEdit;
    memoComment: TTntMemo;
    lblComment: TLabel;
    lblAutoinc: TLabel;
    lblAvgRowLen: TLabel;
    lblInsertMethod: TLabel;
    lblUnion: TLabel;
    lblMaxRows: TLabel;
    lblRowFormat: TLabel;
    editAutoInc: TEdit;
    editAvgRowLen: TEdit;
    editMaxRows: TEdit;
    chkChecksum: TCheckBox;
    comboRowFormat: TComboBox;
    memoUnionTables: TTntMemo;
    comboInsertMethod: TComboBox;
    lblCollation: TLabel;
    comboCollation: TTNTComboBox;
    lblEngine: TLabel;
    comboEngine: TComboBox;
    treeIndexes: TVirtualStringTree;
    tlbIndexes: TToolBar;
    btnAddIndex: TToolButton;
    btnRemoveIndex: TToolButton;
    btnClearIndexes: TToolButton;
    btnMoveUpIndex: TToolButton;
    btnMoveDownIndex: TToolButton;
    StaticText1: TStaticText;
    pnlColumnsTop: TPanel;
    tlbColumns: TToolBar;
    btnAddColumn: TToolButton;
    btnRemoveColumn: TToolButton;
    btnClearColumns: TToolButton;
    btnMoveUpColumn: TToolButton;
    btnMoveDownColumn: TToolButton;
    SplitterTopBottom: TSplitter;
    tabCREATEcode: TTabSheet;
    tabALTERCode: TTabSheet;
    SynMemoCREATEcode: TSynMemo;
    SynMemoALTERcode: TSynMemo;
    popupIndexes: TPopupMenu;
    menuAddIndex: TMenuItem;
    menuAddIndexColumn: TMenuItem;
    menuRemoveIndex: TMenuItem;
    menuMoveUpIndex: TMenuItem;
    menuMoveDownIndex: TMenuItem;
    menuClearIndexes: TMenuItem;
    lblStatus: TLabel;
    popupColumns: TPopupMenu;
    menuAddColumn: TMenuItem;
    menuRemoveColumn: TMenuItem;
    menuClearColumns: TMenuItem;
    menuMoveUpColumn: TMenuItem;
    menuMoveDownColumn: TMenuItem;
    chkCharsetConvert: TCheckBox;
    procedure editNameChange(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure btnAddColumnClick(Sender: TObject);
    procedure btnRemoveColumnClick(Sender: TObject);
    procedure btnClearColumnsClick(Sender: TObject);
    procedure listColumnsBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure listColumnsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure btnHelpClick(Sender: TObject);
    procedure listColumnsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure listColumnsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure listColumnsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure btnMoveUpColumnClick(Sender: TObject);
    procedure btnMoveDownColumnClick(Sender: TObject);
    procedure listColumnsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
		  Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure listColumnsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
		  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure listColumnsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
		  Column: TColumnIndex; TextType: TVSTTextType);
    procedure listColumnsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure treeIndexesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure treeIndexesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
		  var CellText: WideString);
    procedure treeIndexesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure treeIndexesInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure treeIndexesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure btnClearIndexesClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure editNumEditChange(Sender: TObject);
    procedure comboEngineSelect(Sender: TObject);
    procedure listColumnsClick(Sender: TObject);
    procedure listColumnsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure listColumnsAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure btnAddIndexClick(Sender: TObject);
    procedure treeIndexesDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure treeIndexesDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure treeIndexesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure treeIndexesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure treeIndexesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure treeIndexesCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure btnMoveUpIndexClick(Sender: TObject);
    procedure btnMoveDownIndexClick(Sender: TObject);
    procedure btnRemoveIndexClick(Sender: TObject);
    procedure menuAddIndexColumnClick(Sender: TObject);
    procedure PageControlMainChange(Sender: TObject);
    procedure chkCharsetConvertClick(Sender: TObject);
    procedure treeIndexesClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
  private
    { Private declarations }
    FModified: Boolean;
    FLoaded: Boolean;
    FAlterTableName: WideString;
    CreateCodeValid, AlterCodeValid: Boolean;
    Columns, ColumnNames, ColumnsChanges,
    Indexes, OldIndexes: TWideStringList;
    procedure ColumnsChange(Sender: TObject);
    procedure IndexesChange(Sender: TObject);
    procedure ValidateColumnControls;
    procedure ValidateIndexControls;
    procedure SelectNode(VT: TVirtualStringTree; idx: Cardinal; ParentNode: PVirtualNode=nil);
    procedure MoveFocusedColumn(NewIdx: Cardinal);
    procedure MoveFocusedIndexPart(NewIdx: Cardinal);
    procedure SetModified(Value: Boolean);
    procedure ResetModificationFlags;
    function ComposeCreateStatement: WideString;
    function ComposeAlterStatement: WideString;
    function GetIndexSQL(IndexList: TWideStringlist; idx: Integer): WideString;
    property Modified: Boolean read FModified write SetModified;
    procedure SetStatus(msg: WideString = '');
    procedure UpdateSQLcode;
    function CellEditingAllowed(Node: PVirtualNode; Column: TColumnIndex): Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(AlterTableName: WideString='');
  end;


implementation

uses main, helpers, grideditlinks, mysql_structures;

const
  PKEY = 'PRIMARY';
  KEY = 'KEY';
  UKEY = 'UNIQUE';
  FKEY = 'FULLTEXT';
  SKEY = 'SPATIAL';
  NotModifiedFlag : Byte = 0;
  ModifiedFlag : Byte = 1;


{$R *.dfm}


constructor TfrmTableEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Do not set alClient via DFM! In conjunction with ExplicitXXX properties that
  // repeatedly breaks the GUI layout when you reload the project
  Align := alClient;
  PageControlMain.Height := GetRegValue(REGNAME_TABLEEDITOR_TABSHEIGHT, PageControlMain.Height);
  InheritFont(Font);
  FixVT(listColumns);
  FixVT(treeIndexes);
  Mainform.RestoreListSetup(listColumns);
  Mainform.RestoreListSetup(treeIndexes);
  comboRowFormat.Items.CommaText := 'DEFAULT,DYNAMIC,FIXED,COMPRESSED,REDUNDANT,COMPACT';
  comboInsertMethod.Items.CommaText := 'NO,FIRST,LAST';
  SynMemoALTERcode.TabWidth := Mainform.SynMemoQuery.TabWidth;
  SynMemoALTERcode.Font.Name := Mainform.SynMemoQuery.Font.Name;
  SynMemoALTERcode.Font.Size := Mainform.SynMemoQuery.Font.Size;
  SynMemoCREATEcode.TabWidth := Mainform.SynMemoQuery.TabWidth;
  SynMemoCREATEcode.Font.Name := Mainform.SynMemoQuery.Font.Name;
  SynMemoCREATEcode.Font.Size := Mainform.SynMemoQuery.Font.Size;
  Columns := TWideStringList.Create;
  Columns.OnChange := ColumnsChange;
  ColumnNames := TWideStringList.Create;
  ColumnsChanges := TWideStringList.Create;
  Indexes := TWideStringList.Create;
  Indexes.OnChange := IndexesChange;
  OldIndexes := TWideStringList.Create;
end;


destructor TfrmTableEditor.Destroy;
begin
  // Store GUI setup
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_TABLEEDITOR_TABSHEIGHT, PageControlMain.Height);
  Mainform.SaveListSetup(listColumns);
  Mainform.SaveListSetup(treeIndexes);
  Inherited;
end;


procedure TfrmTableEditor.Init(AlterTableName: WideString='');
var
  ds: TDataset;
  Props: TWideStringlist;
  IndexType: String;
  LastKeyName, CreateTable, engine: WideString;
  rx: TRegExpr;
begin
  SetStatus('Initializing ...');
  FLoaded := False;
  // Start with "basic" tab activated when just called
  if FAlterTableName <> AlterTableName then
    PageControlMain.ActivePage := tabBasic;
  Mainform.TableEnginesCombo(comboEngine);
  comboCollation.Items.Clear;
  Mainform.GetCollations(comboCollation.Items);
  FAlterTableName := AlterTableName;
  btnClearColumnsClick(Self);
  btnClearIndexesClick(Self);
  tabALTERcode.TabVisible := FAlterTableName <> '';

  if FAlterTableName = '' then begin
    // Creating new table
    editName.Clear;
    Mainform.SetEditorTabCaption(Self, '');
    memoComment.Text := '';
    editAutoInc.Text := '';
    editAvgRowLen.Text := '';
    editMaxRows.Text := '';
    chkChecksum.Checked := False;
    comboRowFormat.ItemIndex := 0;
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(Mainform.GetVar('SHOW VARIABLES LIKE ''collation_database''', 1));
    memoUnionTables.Clear;
    comboInsertMethod.ItemIndex := -1;

    PageControlMain.ActivePage := tabBasic;

  end else begin
    // Editing existing table
    editName.Text := FAlterTableName;
    Mainform.SetEditorTabCaption(Self, FAlterTableName);
    ds := Mainform.GetResults('SHOW TABLE STATUS LIKE '+esc(FAlterTableName));
    memoComment.Text := ds.FieldByName(DBO_COMMENT).AsWideString;
    if ds.FindField(DBO_ENGINE) <> nil then
      engine := ds.FieldByName(DBO_ENGINE).AsString
    else
      engine := ds.FieldByName(DBO_TYPE).AsString;
    comboEngine.ItemIndex := comboEngine.Items.IndexOf(engine);
    if ds.FindField(DBO_COLLATION) <> nil then
      comboCollation.ItemIndex := comboCollation.Items.IndexOf(ds.FieldByName(DBO_COLLATION).AsWideString);
    editAutoInc.Text := ds.FieldByName(DBO_AUTOINC).AsString;
    editAvgRowLen.Text := ds.FieldByName(DBO_AVGROWLEN).AsString;
    comboRowFormat.ItemIndex := comboRowFormat.Items.IndexOf(ds.FieldByName(DBO_ROWFORMAT).AsString);
    FreeAndNil(ds);
    CreateTable := Mainform.GetVar('SHOW CREATE TABLE '+Mainform.mask(FAlterTableName), 1);
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.Expression := '\bUNION=\((.+)\)';
    if rx.Exec(CreateTable) then
      memoUnionTables.Text := rx.Match[1]
    else
      memoUnionTables.Clear;
    FreeAndNil(rx);

    ds := Mainform.GetResults('SHOW FULL COLUMNS FROM '+Mainform.mask(FAlterTableName));
    listColumns.BeginUpdate;
    while not ds.Eof do begin
      Props := TWideStringlist.Create;
      Props.OnChange := ColumnsChange;
      Props.Add(UpperCase(GetFirstWord(ds.FieldByName('Type').AsString)));
      Props.Add(getEnumValues(ds.FieldByName('Type').AsWideString));
      Props.Add(BoolToStr(Pos('unsigned', ds.FieldByName('Type').AsWideString) > 0));
      Props.Add(BoolToStr(ds.FieldByName('Null').AsString = 'YES'));
      if ds.FieldByName('Extra').AsWideString = 'auto_increment' then
        Props.Add(IntToStr(Integer(cdtAutoInc))+'AUTO_INCREMENT')
      else if ds.FieldByName('Default').IsNull then
        Props.Add(IntToStr(Integer(cdtNull))+'NULL')
      else if ds.FieldByName('Default').AsWideString = 'CURRENT_TIMESTAMP' then
        Props.Add(IntToStr(Integer(cdtCurTS))+ds.FieldByName('Default').AsWideString)
      else
        Props.Add(IntToStr(Integer(cdtText))+ds.FieldByName('Default').AsWideString);
      if ds.FindField('Comment') <> nil then
        Props.Add(ds.FieldByName('Comment').AsWideString)
      else
        Props.Add('');
      if ds.FindField('Collation') <> nil then
        Props.Add(ds.FieldByName('Collation').AsString)
      else
        Props.Add('');
      Columns.AddObject(ds.FieldByName('Field').AsWideString, Props);
      ds.Next;
    end;
    FreeAndNil(ds);
    listColumns.EndUpdate;

    ds := Mainform.GetResults('SHOW KEYS FROM '+Mainform.mask(FAlterTableName));
    LastKeyName := '';
    Props := nil;
    while not ds.Eof do begin
      if LastKeyName <> ds.FieldByName('Key_name').AsWideString then begin
        Props := TWideStringlist.Create;
        Props.OnChange := IndexesChange;
        IndexType := ds.FieldByName('Key_name').AsString;
        if (ds.FindField('Index_type') <> nil) and (
          (ds.FieldByName('Index_type').AsString = FKEY) or (ds.FieldByName('Index_type').AsString = SKEY)) then
          IndexType := ds.FieldByName('Index_type').AsString
        else if (IndexType <> PKEY) and (ds.FieldByName('Non_unique').AsInteger = 0) then
          IndexType := UKEY
        else if ds.FieldByName('Non_unique').AsInteger = 1 then
          IndexType := KEY;
        Indexes.AddObject(ds.FieldByName('Key_name').AsWideString+REGDELIM+IndexType, Props);
      end;
      Props.Add(ds.FieldByName('Column_name').AsWideString);
      if ds.FieldByName('Sub_part').AsString <> '' then
        Props[Props.Count-1] := Props[Props.Count-1] + '('+ds.FieldByName('Sub_part').AsString+')';
      LastKeyName := ds.FieldByName('Key_name').AsWideString;
      ds.Next;
    end;
    FreeAndNil(ds);

  end;
  // Validate controls
  ColumnsChange(Self);
  comboEngineSelect(comboEngine);
  ValidateColumnControls;
  ValidateIndexControls;
  ResetModificationFlags;
  // Indicate change mechanisms can call their events now. See Modification().
  FLoaded := True;
  // Empty status label
  SetStatus;
end;


procedure TfrmTableEditor.btnDiscardClick(Sender: TObject);
begin
  // Reinit GUI, discarding changes
  Init(FAlterTableName);
end;


procedure TfrmTableEditor.btnSaveClick(Sender: TObject);
var
  sql: WideString;
  i: Integer;
  Props: TWideStringlist;
begin
  // Create or alter table
  if FAlterTableName = '' then
    sql := ComposeCreateStatement
  else
    sql := ComposeAlterStatement;
  Mainform.ExecUpdateQuery(sql);
  // Set table name for altering if Apply was clicked
  FAlterTableName := editName.Text;
  tabALTERcode.TabVisible := FAlterTableName <> '';
  Mainform.SetEditorTabCaption(Self, FAlterTableName);
  Mainform.tabData.TabVisible := True;
  if chkCharsetConvert.Checked then begin
    // Autoadjust column collations
    for i:=0 to Columns.Count-1 do begin
      Props := TWideStringlist(Columns.Objects[i]);
      if Props[6] <> '' then begin
        Props.OnChange := nil;
        Props[6] := comboCollation.Text;
        Props.OnChange := ColumnsChange;
      end;
    end;
  end;
  ResetModificationFlags;
  Mainform.ResetSelectedTableStuff;
end;


procedure TfrmTableEditor.ResetModificationFlags;
var
  i: Integer;
  Parts: TWideStringlist;
begin
  // Reset modification flags of TEdits and TMemos
  for i:=0 to ComponentCount-1 do
    Components[i].Tag := NotModifiedFlag;
  // Reset name value pairs for column changes
  ColumnNames.Clear;
  ColumnsChanges.Clear;
  for i:=0 to Columns.Count-1 do
    ColumnNames.Add(Columns[i] + ColumnNames.NameValueSeparator + Columns[i]);
  // Copy index list so we have one to work with and one which can later be used to detect changes
  OldIndexes.Clear;
  for i:=0 to Indexes.Count-1 do begin
    Parts := TWideStringlist.Create;
    Parts.Assign(TWideStringlist(Indexes.Objects[i]));
    OldIndexes.AddObject(Indexes[i], Parts);
  end;
  // Enable converting data for an existing table
  chkCharsetConvertClick(comboCollation);
  // Assist the user in auto unchecking this checkbox so data doesn't get converted more than once accidently
  chkCharsetConvert.Checked := False;
  Modified := False;
end;


function TfrmTableEditor.ComposeAlterStatement: WideString;
var
  Specs, Props, IndexesComposed, OldIndexesComposed: TWideStringlist;
  ColSpec, IndexSQL, DefaultText: WideString;
  i, j: Integer;
  AddIt, DropIt: Boolean;
  dt: TMySQLDataTypeRecord;
  DefaultType: TColumnDefaultType;
  ds: TDataset;
begin
  // Compose ALTER query, called by buttons and for SQL code tab
  SetStatus('Composing ALTER statement ...');
  Specs := TWideStringlist.Create;
  if editName.Text <> FAlterTableName then
    Specs.Add('RENAME TO ' + Mainform.mask(editName.Text));
  if memoComment.Tag = ModifiedFlag then
    Specs.Add('COMMENT = ' + esc(memoComment.Text));
  if (comboCollation.Tag = ModifiedFlag) or (chkCharsetConvert.Checked) then
    Specs.Add('COLLATE = ' + comboCollation.Text);
  if comboEngine.Tag = ModifiedFlag then
    Specs.Add('ENGINE = ' + comboEngine.Text);
  if comboRowFormat.Tag = ModifiedFlag then
    Specs.Add('ROW_FORMAT = ' + comboRowFormat.Text);
  if chkChecksum.Tag = ModifiedFlag then
    Specs.Add('CHECKSUM = ' + IntToStr(Integer(chkChecksum.Checked)));
  if editAutoInc.Tag = ModifiedFlag then
    Specs.Add('AUTO_INCREMENT = ' + IntToStr(MakeInt(editAutoInc.Text)));
  if editAvgRowLen.Tag = ModifiedFlag then
    Specs.Add('AVG_ROW_LENGTH = ' + IntToStr(MakeInt(editAvgRowLen.Text)));
  if editMaxRows.Tag = ModifiedFlag then
    Specs.Add('MAX_ROWS = ' + IntToStr(MakeInt(editMaxRows.Text)));
  if memoUnionTables.Enabled and (memoUnionTables.Tag = ModifiedFlag) and (memoUnionTables.Text <> '') then
    Specs.Add('UNION = ('+memoUnionTables.Text+')');
  if comboInsertMethod.Enabled and (comboInsertMethod.Tag = ModifiedFlag) and (comboInsertMethod.Text <> '') then
    Specs.Add('INSERT_METHOD = '+comboInsertMethod.Text);
  if chkCharsetConvert.Checked then begin
    ds := Mainform.GetCollations;
    while not ds.Eof do begin
      if ds.FieldByName('Collation').AsWideString = comboCollation.Text then begin
        Specs.Add('CONVERT TO CHARSET '+ds.FieldByName('Charset').AsWideString);
        break;
      end;
      ds.Next;
    end;
  end;

  // Update columns
  for i:=0 to Columns.Count - 1 do begin
    AddIt := True;
    ColSpec := ' ' + Mainform.mask(Columns[i]);
    Props := TWideStringlist(Columns.Objects[i]);
    ColSpec := ColSpec + ' ' +Props[0];
    if Props[1] <> '' then
      ColSpec := ColSpec + '(' + Props[1] + ')';
    dt := GetDatatypeByName(Props[0]);
    if dt.HasUnsigned and StrToBool(Props[2]) then
      ColSpec := ColSpec + ' UNSIGNED';
    if not StrToBool(Props[3]) then
      ColSpec := ColSpec + ' NOT';
    ColSpec := ColSpec + ' NULL';
    if Props[4] <> '' then begin
      DefaultText := Props[4];
      DefaultType := GetColumnDefaultType(DefaultText);
      case DefaultType of
        cdtText:     ColSpec := ColSpec + ' DEFAULT '+esc(DefaultText);
        cdtNull:     ColSpec := ColSpec + ' DEFAULT NULL';
        cdtCurTS:    ColSpec := ColSpec + ' DEFAULT CURRENT_TIMESTAMP';
        cdtAutoInc:  ColSpec := ColSpec + ' AUTO_INCREMENT';
      end;
    end;
    if Props[5] <> '' then
      ColSpec := ColSpec + ' COMMENT '+esc(Props[5]);
    if Props[6] <> '' then begin
      ColSpec := ColSpec + ' COLLATE ';
      if chkCharsetConvert.Checked then
        ColSpec := ColSpec + comboCollation.Text
      else
        ColSpec := ColSpec + Props[6];
    end;
    // Server version requirement, see http://dev.mysql.com/doc/refman/4.1/en/alter-table.html
    if Mainform.mysql_version >= 40001 then begin
      if i = 0 then
        ColSpec := ColSpec + ' FIRST'
      else
        ColSpec := ColSpec + ' AFTER '+Mainform.mask(Columns[i-1]);
    end;
    for j:=0 to ColumnNames.Count - 1 do begin
      if ColumnNames.ValueFromIndex[j] = Columns[i] then begin
        AddIt := False;
        // Only touch column if it was changed
        if ColumnsChanges.IndexOf(Columns[i]) > -1 then
          Specs.Add('CHANGE COLUMN '+Mainform.mask(ColumnNames.Names[j]) + ColSpec);
        break;
      end;
    end;
    if AddIt then
      Specs.Add('ADD COLUMN '+ColSpec);
  end;
  // Delete columns
  for i:=0 to ColumnNames.Count-1 do begin
    if ColumnNames.ValueFromIndex[i] = '' then
      Specs.Add('DROP COLUMN '+Mainform.mask(ColumnNames.Names[i]));
  end;

  // Prepare ADD INDEX ... clauses once so we don't call GetIndexSQL() too often
  IndexesComposed := TWideStringlist.Create;
  for i:=0 to Indexes.Count-1 do
    IndexesComposed.Add(GetIndexSQL(Indexes, i));
  OldIndexesComposed := TWideStringlist.Create;
  for i:=0 to OldIndexes.Count-1 do
    OldIndexesComposed.Add(GetIndexSQL(OldIndexes, i));
  // Drop indexes
  for i:=0 to OldIndexesComposed.Count-1 do begin
    DropIt := IndexesComposed.IndexOf(OldIndexesComposed[i]) = -1;
    if not DropIt then
      Continue;
    IndexSQL := Copy(OldIndexes[i], LastPos(REGDELIM, OldIndexes[i])+1, Length(OldIndexes[i]));
    if IndexSQL = PKEY then
      IndexSQL := 'PRIMARY KEY'
    else
      IndexSQL := 'INDEX ' + Mainform.Mask(Copy(OldIndexes[i], 1, LastPos(REGDELIM, OldIndexes[i])-1));
    Specs.Add('DROP '+IndexSQL);
  end;
  // Add changed or added indexes
  for i:=0 to IndexesComposed.Count-1 do begin
    if OldIndexesComposed.IndexOf(IndexesComposed[i]) = -1 then
      Specs.Add('ADD '+IndexesComposed[i]);
  end;

  Result := 'ALTER TABLE '+Mainform.mask(FAlterTableName) + CRLF + #9 + ImplodeStr(',' + CRLF + #9, Specs);
  FreeAndNil(Specs);
  FreeAndNil(IndexesComposed);
  FreeAndNil(OldIndexesComposed);
  SetStatus;
  Screen.Cursor := crDefault;
end;


function TfrmTableEditor.ComposeCreateStatement: WideString;
var
  i: Integer;
  ColProps: TWideStringlist;
  dt: TMySQLDataTypeRecord;
  DefaultType: TColumnDefaultType;
  DefaultText: WideString;
begin
  // Compose CREATE query, called by buttons and for SQL code tab
  Result := 'CREATE TABLE '+Mainform.mask(editName.Text)+' ('+CRLF;
  for i:=0 to Columns.Count - 1 do begin
    ColProps := TWideStringlist(Columns.Objects[i]);
    Result := Result + #9 + Mainform.mask(Columns[i]) + ' ' +ColProps[0];
    if ColProps[1] <> '' then
      Result := Result + '(' + ColProps[1] + ')';
    dt := GetDatatypeByName(ColProps[0]);
    if dt.HasUnsigned and StrToBool(ColProps[2]) then
      Result := Result + ' UNSIGNED';
    if not StrToBool(ColProps[3]) then
      Result := Result + ' NOT';
    Result := Result + ' NULL';
    if ColProps[4] <> '' then begin
      DefaultText := ColProps[4];
      DefaultType := GetColumnDefaultType(DefaultText);
      case DefaultType of
        cdtText:     Result := Result + ' DEFAULT '+esc(DefaultText);
        cdtNull:     Result := Result + ' DEFAULT NULL';
        cdtCurTS:    Result := Result + ' DEFAULT CURRENT_TIMESTAMP';
        cdtAutoInc:  Result := Result + ' AUTO_INCREMENT';
      end;
    end;
    if ColProps[5] <> '' then
      Result := Result + ' COMMENT '+esc(ColProps[5]);
    if ColProps[6] <> '' then
      Result := Result + ' COLLATE '+ColProps[6];
    Result := Result + ','+CRLF;
  end;

  for i:=0 to Indexes.Count - 1 do begin
    Result := Result + #9 + GetIndexSQL(Indexes, i) + ','+CRLF;
  end;
  if Columns.Count + Indexes.Count > 0 then
    Delete(Result, Length(Result)-2, 3);

  Result := Result + CRLF +
    ')' + CRLF +
    'COMMENT = '+esc(memoComment.Text) + CRLF +
    'COLLATE = '+comboCollation.Text + CRLF +
    'ENGINE = '+comboEngine.Text + CRLF +
    'ROW_FORMAT = '+comboRowFormat.Text + CRLF +
    'CHECKSUM = '+IntToStr(Integer(chkChecksum.Checked)) + CRLF;
  if editAutoInc.Text <> '' then
    Result := Result + 'AUTO_INCREMENT = '+editAutoInc.Text + CRLF;
  if editAvgRowLen.Text <> '' then
    Result := Result + 'AVG_ROW_LENGTH = '+editAvgRowLen.Text + CRLF;
  if editMaxRows.Text <> '' then
    Result := Result + 'MAX_ROWS = '+editMaxRows.Text + CRLF;
  if memoUnionTables.Enabled and (memoUnionTables.Text <> '') then
    Result := Result + 'UNION = ('+memoUnionTables.Text+')' + CRLF;
  if comboInsertMethod.Enabled and (comboInsertMethod.Text <> '') then
    Result := Result + 'INSERT_METHOD = '+comboInsertMethod.Text + CRLF;

end;


function TfrmTableEditor.GetIndexSQL(IndexList: TWideStringlist; idx: Integer): WideString;
var
  IndexName, IndexType, ColName: WideString;
  Parts: TWideStringlist;
  i, p: Integer;
begin
  IndexName := Copy(IndexList[idx], 1, LastPos(REGDELIM, IndexList[idx])-1);
  IndexType := Copy(IndexList[idx], LastPos(REGDELIM, IndexList[idx])+1, Length(IndexList[idx]));
  Result := '';
  if IndexType = PKEY then
    Result := Result + 'PRIMARY KEY '
  else begin
    if IndexType <> KEY then
      Result := Result + IndexType + ' ';
    Result := Result + 'INDEX ' + Mainform.Mask(IndexName) + ' ';
  end;
  Result := Result + '(';
  Parts := TWideStringlist(IndexList.Objects[idx]);
  for i:=0 to Parts.Count - 1 do begin
    ColName := Parts[i];
    p := LastPos('(', ColName);
    if p > 1 then
      Result := Result + Mainform.Mask(Copy(ColName, 1, p-1)) + Copy(ColName, p, Length(ColName)-p+1)
    else
      Result := Result + Mainform.Mask(ColName);
    Result := Result + ', ';
  end;
  if Parts.Count > 0 then
    Delete(Result, Length(Result)-1, 2);

  Result := Result + ')';
end;


procedure TfrmTableEditor.editNameChange(Sender: TObject);
begin
  // Name edited
  editName.Font.Color := clWindowText;
  editName.Color := clWindow;
  try
    ensureValidIdentifier( editName.Text );
  except
    // Invalid name
    if editName.Text <> '' then begin
      editName.Font.Color := clRed;
      editName.Color := clYellow;
    end;
  end;
  Modification(Sender);
end;


procedure TfrmTableEditor.Modification(Sender: TObject);
begin
  // Memorize modified status
  if FLoaded then begin
    if Sender is TComponent then
      TComponent(Sender).Tag := ModifiedFlag;
    Modified := True;
  end;
end;


procedure TfrmTableEditor.btnAddColumnClick(Sender: TObject);
var
  DefProperties, Properties: TWideStringList;
  fn: PVirtualNode;
  idx: Integer;
begin
  // Add new column after selected one
  fn := listColumns.FocusedNode;
  Properties := TWideStringList.Create;
  Properties.OnChange := ColumnsChange;
  if Assigned(fn) then begin
    idx := fn.Index+1;
    // Copy properties from focused node
    DefProperties := TWideStringlist(Columns.Objects[fn.Index]);
    Properties.Assign(DefProperties);
  end else begin
    idx := Columns.Count;
    Properties.CommaText := 'INT,10,'+BoolToStr(False)+','+BoolToStr(True)+','+IntToStr(Integer(cdtNull))+'NULL,,';
  end;
  Columns.InsertObject(idx, 'Column '+IntToStr(idx+1), Properties);
  SelectNode(listColumns, idx);
  listColumns.EditNode(listColumns.FocusedNode, 1);
end;


procedure TfrmTableEditor.btnRemoveColumnClick(Sender: TObject);
var
  i: Integer;
  n: PVirtualNode;
begin
  // Remove selected column
  n := listColumns.FocusedNode;
  for i:=0 to ColumnNames.Count-1 do begin
    if ColumnNames.ValueFromIndex[i] = Columns[n.Index] then begin
      ColumnNames[i] := ColumnNames.Names[i] + ColumnNames.NameValueSeparator;
      break;
    end;
  end;
  Columns.Delete(n.Index);
  if (not Assigned(n)) and (Columns.Count > 0) then
    SelectNode(listColumns, Columns.Count-1);
end;


procedure TfrmTableEditor.btnClearColumnsClick(Sender: TObject);
var i: Integer;
begin
  // Set empty values in changelist
  for i:=0 to ColumnNames.Count - 1 do
    ColumnNames[i] := ColumnNames.Names[i] + ColumnNames.NameValueSeparator;
  // Column data gets freed below - end any editor which could cause AV's
  if listColumns.IsEditing then
    listColumns.CancelEditNode;
  // I suspect Columns.Clear to be silly enough and leave its objects in memory,
  // so we'll free them explicitely
  for i := 0 to Columns.Count - 1 do
    Columns.Objects[i].Free;
  Columns.Clear;
end;


procedure TfrmTableEditor.btnMoveUpColumnClick(Sender: TObject);
begin
  // Move column up
  MoveFocusedColumn(listColumns.FocusedNode.Index-1);
end;


procedure TfrmTableEditor.btnMoveDownColumnClick(Sender: TObject);
begin
  // Move column down
  MoveFocusedColumn(listColumns.FocusedNode.Index+1);
end;


procedure TfrmTableEditor.listColumnsDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := (Source = Sender) and (Mode <> dmNowhere);
  // Not sure what this effect does, probably show a specific mouse cursor?
  Effect := DROPEFFECT_MOVE;
end;


procedure TfrmTableEditor.listColumnsDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  Node: PVirtualNode;
begin
  Node := Sender.GetNodeAt(Pt.X, Pt.Y);
  if Assigned(Node) then
    MoveFocusedColumn(Node.Index);
end;


procedure TfrmTableEditor.listColumnsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  // Darken cell background to signalize it doesn't allow length/set
  // Exclude non editable checkbox columns - grey looks ugly there.
  if (not CellEditingAllowed(Node, Column)) and (not (Column in [4, 5])) then begin
    TargetCanvas.Brush.Color := clBtnFace;
    TargetCanvas.FillRect(CellRect);
  end;
end;


procedure TfrmTableEditor.listColumnsAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  Props: TWideStringlist;
  ImageIndex, X, Y: Integer;
  VT: TVirtualStringTree;
begin
  // Paint checkbox image in certain columns
  // while restricting "Allow NULL" checkbox to numeric datatypes
  if (Column in [4, 5]) and CellEditingAllowed(Node, Column) then begin
    Props := TWideStringlist(Columns.Objects[Node.Index]);
    if StrToBool(Props[Column-2]) then ImageIndex := 128
    else ImageIndex := 127;
    VT := TVirtualStringTree(Sender);
    X := CellRect.Left + (VT.Header.Columns[Column].Width div 2) - (VT.Images.Width div 2);
    Y := CellRect.Top + Integer(VT.NodeHeight[Node] div 2) - (VT.Images.Height div 2);
    VT.Images.Draw(TargetCanvas, X, Y, ImageIndex);
  end;
end;


procedure TfrmTableEditor.listColumnsBeforePaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
var
  VT: TVirtualStringTree;
  OldNodeCount: Cardinal;
begin
  // (Re)paint column list. Adjust number of nodes and autofit leftmost counter column.
  VT := Sender as TVirtualStringTree;
  OldNodeCount := VT.RootNodeCount;
  VT.RootNodeCount := Columns.Count;
  if OldNodeCount <> VT.RootNodeCount then
    VT.Header.AutoFitColumns(False, smaUseColumnOption, 0, 0);
end;


procedure TfrmTableEditor.listColumnsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  // Column focus changed
  ValidateColumnControls;
end;


procedure TfrmTableEditor.ValidateColumnControls;
var Node: PVirtualNode;
begin
  Node := listColumns.FocusedNode;
  btnRemoveColumn.Enabled := Assigned(Node);
  btnClearColumns.Enabled := Columns.Count > 0;
  btnMoveUpColumn.Enabled := Assigned(Node) and (Node <> listColumns.GetFirst);
  btnMoveDownColumn.Enabled := Assigned(Node) and (Node <> listColumns.GetLast);

  menuRemoveColumn.Enabled := btnRemoveColumn.Enabled;
  menuClearColumns.Enabled := btnClearColumns.Enabled;
  menuMoveUpColumn.Enabled := btnMoveUpColumn.Enabled;
  menuMoveDownColumn.Enabled := btnMoveDownColumn.Enabled;
end;


procedure TfrmTableEditor.listColumnsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  // Allow text editing? Explicitely block that in checkbox columns 
  Allowed := CellEditingAllowed(Node, Column) and (not (Column in [4,5]));
end;


function TfrmTableEditor.CellEditingAllowed(Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  Props: TWideStringlist;
  dt: TMysqlDataTypeRecord;
begin
  Props := TWideStringlist(Columns.Objects[Node.Index]);
  dt := GetDatatypeByName(Props[0]);
  case Column of
    // No editor for very first column and checkbox columns
    0: Result := False;
    3: Result := dt.HasLength;
    4: Result := dt.HasUnsigned;
    // No editing of collation allowed if "Convert data" was checked
    8: Result := not chkCharsetConvert.Checked;
    else Result := True;
  end;
end;


procedure TfrmTableEditor.listColumnsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Properties: TWideStringList;
begin
  // Display column text
  case Column of
    0: CellText := IntToStr(Node.Index+1);
    1: CellText := Columns[Node.Index];
    4, 5: CellText := ''; // Checkbox
    else begin
      Properties := TWideStringList(Columns.Objects[Node.Index]);
      CellText := Properties[Column-2];
      if (Column = 8) and (CellText <> '') and (chkCharsetConvert.Checked) then
        CellText := comboCollation.Text;
    end;
  end;
  if Column = 6 then
    CellText := Copy(CellText, 2, Length(CellText)-1);
end;


procedure TfrmTableEditor.listColumnsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Datatype: String;
  TextColor: TColor;
  dt: TMySQLDatatypeRecord;
begin
  // Give datatype column specific color, as set in preferences
  if vsSelected in Node.States then
    Exit;

  case Column of
    2: begin
      Datatype := TWideStringlist(Columns.Objects[Node.Index])[0];
      dt := GetDatatypeByName(Datatype);

      case dt.Category of
        catInteger, catReal: TextColor := Mainform.prefFieldColorNumeric;
        catTemporal:         TextColor := Mainform.prefFieldColorDateTime;
        catText:             TextColor := Mainform.prefFieldColorText;
        catBinary:           TextColor := Mainform.prefFieldColorBinary;
        catIntegerNamed:     TextColor := Mainform.prefFieldColorEnum;
        catSet, catSetNamed: TextColor := Mainform.prefFieldColorSet;
        // TODO: catSpatial
        else                 TextColor := TargetCanvas.Font.Color;
      end;
    end;

    6: begin
      // TODO: text black, special values blue?
      TextColor := clBlue;
    end;
    else Exit;
  end;

  TargetCanvas.Font.Color := TextColor;
end;


procedure TfrmTableEditor.listColumnsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  Properties: TWideStringList;
  idx, i: Integer;
begin
  // Column property edited
  if Column = 1 then begin
    idx := Columns.IndexOf(NewText);
    if (idx > -1) and (idx <> Integer(Node.Index)) then begin
      MessageDlg('Column "'+NewText+'" already exists.', mtError, [mbOk], 0);
      Sender.EditNode(Node, Column);
      Exit;
    end;

    for i:=0 to ColumnNames.Count -1 do begin
      if ColumnNames.ValueFromIndex[i] = Columns[Node.Index] then begin
        ColumnNames.ValueFromIndex[i] := NewText;
        break;
      end;
    end;

    ColumnsChanges.Add(NewText);
    Columns[Node.Index] := NewText;
  end else if Column > 1 then begin
    ColumnsChanges.Add(Columns[Node.Index]);
    Properties := TWideStringList(Columns.Objects[Node.Index]);
    Properties[Column-2] := NewText;
    if (Column = 2) and (not CellEditingAllowed(Node, 3)) then begin
      // Reset length/set for column types which don't support that
      Properties[1] := '';
    end;
  end;
end;


procedure TfrmTableEditor.listColumnsClick(Sender: TObject);
var
  VT: TVirtualStringTree;
  Props: TWideStringlist;
  Click: THitInfo;
begin
  // Handle click event
  VT := Sender as TVirtualStringTree;
  VT.GetHitTestInfoAt(Mouse.CursorPos.X-VT.ClientOrigin.X, Mouse.CursorPos.Y-VT.ClientOrigin.Y, True, Click);
  if not Assigned(Click.HitNode) then
    Exit;
  if Click.HitColumn in [4, 5] then begin
    // For checkboxes, cell editors are disabled, instead toggle their state
    Props := TWideStringList(Columns.Objects[Click.HitNode.Index]);
    if CellEditingAllowed(Click.HitNode, Click.HitColumn) then begin
      ColumnsChanges.Add(Columns[Click.HitNode.Index]);
      Props[Click.HitColumn-2] := BoolToStr(not StrToBool(Props[Click.HitColumn-2]));
      VT.InvalidateNode(Click.HitNode);
    end;
  end else begin
    // All other cells go into edit mode please
    // Explicitely done on OnClick, not in OnFocusChanged which seemed annoying for keyboard users
    if Assigned(Click.HitNode) and (Click.HitColumn > NoColumn) then
      VT.EditNode(Click.HitNode, Click.HitColumn);
  end;
end;


procedure TfrmTableEditor.listColumnsCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  EnumEditor: TEnumEditorLink;
  DefaultEditor: TColumnDefaultEditorLink;
  i: Integer;
  Props: TWideStringlist;
begin
  // Start cell editor
  case Column of
    2: begin // Datatype pulldown
      EnumEditor := TEnumEditorLink.Create;
      EnumEditor.ValueList := TWideStringList.Create;
      for i:=Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do
        EnumEditor.ValueList.Add(MySqlDataTypeArray[i].Name);
      EditLink := EnumEditor;
      end;
    8: begin // Collation pulldown
      EnumEditor := TEnumEditorLink.Create;
      EnumEditor.ValueList := TWideStringList.Create;
      Mainform.GetCollations(EnumEditor.ValueList);
      EditLink := EnumEditor;
      end;
    6: begin
      DefaultEditor := TColumnDefaultEditorLink.Create;
      Props := TWideStringlist(Columns.Objects[Node.Index]);
      DefaultEditor.DefaultText := Props[Column-2];
      DefaultEditor.DefaultType := GetColumnDefaultType(DefaultEditor.DefaultText);
      EditLink := DefaultEditor;
    end
    else
      EditLink := TStringEditLink.Create
  end;
end;


procedure TfrmTableEditor.SetModified(Value: Boolean);
begin
  // Some value has changed
  FModified := Value;
  btnSave.Enabled := FModified;
  btnDiscard.Enabled := FModified;
  CreateCodeValid := False;
  AlterCodeValid := False;
  UpdateSQLcode;
end;


procedure TfrmTableEditor.ColumnsChange(Sender: TObject);
begin
  Modification(Sender);
  listColumns.Repaint;
end;


procedure TfrmTableEditor.SelectNode(VT: TVirtualStringTree; idx: Cardinal; ParentNode: PVirtualNode=nil);
var Node: PVirtualNode;
begin
  // Helper to focus and highlight a node by its index
  if Assigned(ParentNode) then
    Node := VT.GetFirstChild(ParentNode)
  else
    Node := VT.GetFirst;
  while Assigned(Node) do begin
    if Node.Index = idx then begin
      VT.FocusedNode := Node;
      VT.Selected[Node] := True;
      break;
    end;
    Node := VT.GetNextSibling(Node);
  end;
end;


procedure TfrmTableEditor.MoveFocusedColumn(NewIdx: Cardinal);
begin
  if listColumns.IsEditing then
    listColumns.EndEditNode;
  listColumns.FocusedColumn := listColumns.Header.MainColumn;
  ColumnsChanges.Add(Columns[listColumns.FocusedNode.Index]);
  Columns.Move(listColumns.FocusedNode.Index, NewIdx);
  SelectNode(listColumns, NewIdx);
end;


procedure TfrmTableEditor.editNumEditChange(Sender: TObject);
var
  ed: TEdit;
  ShouldBe: String;
  CursorPos: Integer;
begin
  // Only numbers allowed in this TEdit
  Modification(Sender);
  ed := Sender as TEdit;
  ShouldBe := IntToStr(Makeint(ed.Text));
  if (ed.Text = ShouldBe) or (ed.Text = '') then Exit;
  MessageBeep(MB_ICONEXCLAMATION);
  CursorPos := ed.SelStart;
  ed.OnChange := nil;
  ed.Text := ShouldBe;
  ed.SelStart := CursorPos;
  ed.OnChange := editNumEditChange;
end;


procedure TfrmTableEditor.comboEngineSelect(Sender: TObject);
var
  IsMerge: Boolean;
begin
  // Enable/disable engine specific option controls
  IsMerge := (Sender as TComboBox).Text = 'MRG_MYISAM';
  memoUnionTables.Enabled := IsMerge;
  comboInsertMethod.Enabled := IsMerge;
  Modification(Sender);
end;


procedure TfrmTableEditor.IndexesChange(Sender: TObject);
begin
  Modification(Sender);
  treeIndexes.Invalidate;
end;


procedure TfrmTableEditor.btnAddIndexClick(Sender: TObject);
var
  IndexParts: TWideStringlist;
  Name: WideString;
begin
  // Add new index
  IndexParts := TWideStringlist.Create;
  IndexParts.OnChange := IndexesChange;
  Name := 'Index '+IntToStr(Indexes.Count+1);
  Indexes.AddObject(Name+REGDELIM+KEY, IndexParts);
  SelectNode(treeIndexes, Indexes.Count-1);
end;


procedure TfrmTableEditor.menuAddIndexColumnClick(Sender: TObject);
var
  IndexParts: TWideStringlist;
  Node: PVirtualNode;
  i, j, p: Integer;
  Col, NewCol: WideString;
  ColExists: Boolean;
begin
  // Add column to index
  Node := treeIndexes.FocusedNode;
  if not Assigned(Node) then
    Exit;
  if treeIndexes.GetNodeLevel(Node) = 1 then
    Node := Node.Parent;
  IndexParts := TWideStringlist(Indexes.Objects[Node.Index]);
  ColExists := False;
  NewCol := '';
  for i:=0 to Columns.Count - 1 do begin
    for j:=0 to IndexParts.Count - 1 do begin
      Col := IndexParts[j];
      p := LastPos('(', Col);
      if p > 0 then
        Col := Copy(Col, 0, p-1);
      ColExists := Col = Columns[i];
      if ColExists then
        break;
    end;
    if not ColExists then begin
      NewCol := Columns[i];
      break;
    end;
  end;
  treeIndexes.AddChild(Node);
  IndexParts.Add(NewCol);
  SelectNode(treeIndexes, IndexParts.Count-1, Node);
end;


procedure TfrmTableEditor.btnRemoveIndexClick(Sender: TObject);
var
  IndexParts: TWideStringlist;
  idx: Integer;
  NewSelectNode: PVirtualNode;
begin
  // Remove index or part
  case treeIndexes.GetNodeLevel(treeIndexes.FocusedNode) of
    0: begin
      idx := treeIndexes.FocusedNode.Index;
      Indexes.Objects[idx].Free;
      Indexes.Delete(idx);
    end;
    1: begin
      idx := treeIndexes.FocusedNode.Parent.Index;
      IndexParts := TWideStringlist(Indexes.Objects[idx]);
      IndexParts.Delete(treeIndexes.FocusedNode.Index);
      treeIndexes.DeleteNode(treeIndexes.FocusedNode);
    end;
  end;
  NewSelectNode := treeIndexes.GetPreviousVisible(treeIndexes.FocusedNode);
  if Assigned(NewSelectNode) then
    SelectNode(treeIndexes, NewSelectNode.Index, NewSelectNode.Parent);
end;


procedure TfrmTableEditor.btnClearIndexesClick(Sender: TObject);
var i: Integer;
begin
  // Clear all indexes
  // Column data gets freed below - end any editor which could cause AV's
  if treeIndexes.IsEditing then
    treeIndexes.CancelEditNode;
  for i := 0 to Indexes.Count - 1 do
    Indexes.Objects[i].Free;
  Indexes.Clear;
  treeIndexes.Clear;
end;


procedure TfrmTableEditor.treeIndexesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  IndexType: String;
  VT: TVirtualStringTree;
begin
  // Icon image showing type of index
  VT := Sender as TVirtualStringTree;
  if Column <> 0 then
    Exit;
  case VT.GetNodeLevel(Node) of
    0: begin
      IndexType := VT.Text[Node, 1];
      if IndexType = PKEY then ImageIndex := ICONINDEX_PRIMARYKEY
      else if IndexType = KEY then ImageIndex := ICONINDEX_INDEXKEY
      else if IndexType = UKEY then ImageIndex := ICONINDEX_UNIQUEKEY
      else if IndexType = FKEY then ImageIndex := ICONINDEX_FULLTEXTKEY
      else if IndexType = SKEY then ImageIndex := ICONINDEX_SPATIALKEY;
    end;
    1: ImageIndex := 42;
  end;
end;


procedure TfrmTableEditor.treeIndexesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  IndexProps: TWideStringlist;
  IndexType: WideString;
begin
  // Index tree showing cell text
  case Sender.GetNodeLevel(Node) of
    0: begin
      IndexType := Copy(Indexes[Node.Index], LastPos(REGDELIM, Indexes[Node.Index])+1, Length(Indexes[Node.Index]));
      case Column of
        0: if IndexType = PKEY then
             CellText := IndexType + ' KEY' // Fixed name "PRIMARY KEY", cannot be changed
           else
             CellText := Copy(Indexes[Node.Index], 1, LastPos(REGDELIM, Indexes[Node.Index])-1);
        1: CellText := IndexType;
      end;
    end;
    1: case Column of
      0: begin
           IndexProps := TWideStringlist(Indexes.Objects[Node.Parent.Index]);
           CellText := IndexProps[Node.Index];
         end;
      else CellText := '';
    end;
  end;
end;


procedure TfrmTableEditor.treeIndexesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  IndexParts: TWideStringlist;
begin
  // Tell number of columns contained in index
  IndexParts := TWideStringlist(Indexes.Objects[Node.Index]);
  ChildCount := IndexParts.Count;
end;


procedure TfrmTableEditor.treeIndexesInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  // Show plus sign on first level nodes
  if Sender.GetNodeLevel(Node) = 0 then
    Include( InitialStates, ivsHasChildren);
end;


procedure TfrmTableEditor.treeIndexesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  ValidateIndexControls;
end;


procedure TfrmTableEditor.treeIndexesClick(Sender: TObject);
var
  VT: TVirtualStringTree;
  Click: THitInfo;
begin
  // Handle click event
  VT := Sender as TVirtualStringTree;
  VT.GetHitTestInfoAt(Mouse.CursorPos.X-VT.ClientOrigin.X, Mouse.CursorPos.Y-VT.ClientOrigin.Y, True, Click);
  if Assigned(Click.HitNode) then
    VT.EditNode(Click.HitNode, Click.HitColumn);
end;


procedure TfrmTableEditor.ValidateIndexControls;
var
  Node: PVirtualNode;
  HasNode: Boolean;
  Level: Integer;
begin
  // Enable/disable buttons
  Node := treeIndexes.FocusedNode;
  HasNode := Assigned(Node);
  if HasNode then Level := treeIndexes.GetNodeLevel(Node)
  else Level := -1;

  btnRemoveIndex.Enabled := HasNode;
  btnClearIndexes.Enabled := Indexes.Count > 0;
  btnMoveUpIndex.Enabled := HasNode and (Level = 1) and (Node <> treeIndexes.GetFirstChild(Node.Parent));
  btnMoveDownIndex.Enabled := HasNode and (Level = 1) and (Node <> treeIndexes.GetLastChild(Node.Parent));

  menuAddIndexColumn.Enabled := HasNode;
  menuRemoveIndex.Enabled := btnRemoveIndex.Enabled;
  menuClearIndexes.Enabled := btnClearIndexes.Enabled;
  menuMoveUpIndex.Enabled := btnMoveUpIndex.Enabled;
  menuMoveDownIndex.Enabled := btnMoveDownIndex.Enabled;
end;


procedure TfrmTableEditor.treeIndexesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  VT: TVirtualStringtree;
begin
  // Disallow renaming primary key
  VT := Sender as TVirtualStringtree;
  if (VT.GetNodeLevel(Node) = 0) and (Column = VT.Header.MainColumn) and (VT.Text[Node, Column+1] = PKEY) then
    Allowed := False
  else if (VT.GetNodeLevel(Node) = 1) and (Column = 1) then
    Allowed := False
  else
    Allowed := True;
end;


procedure TfrmTableEditor.treeIndexesCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  EnumEditor: TEnumEditorLink;
  Level: Cardinal;
begin
  // Start cell editor
  Level := (Sender as TVirtualStringtree).GetNodeLevel(Node);
  if (Level = 0) and (Column = 1) then begin
    // Index type pulldown
    EnumEditor := TEnumEditorLink.Create;
    EnumEditor.ValueList := TWideStringList.Create;
    EnumEditor.ValueList.CommaText := PKEY +','+ KEY +','+ UKEY +','+ FKEY +','+ SKEY;
    EditLink := EnumEditor;
  end else if (Level = 1) and (Column = 0) then begin
    // Column names pulldown
    EnumEditor := TEnumEditorLink.Create;
    EnumEditor.ValueList := Columns;
    EnumEditor.AllowCustomText := True; // Allows adding a subpart in index parts: "TextCol(20)"
    EditLink := EnumEditor;
  end else
    EditLink := TStringEditLink.Create
end;


procedure TfrmTableEditor.treeIndexesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  VT: TVirtualStringtree;
  IndexParts: TWideStringlist;
begin
  // Rename index of column
  VT := Sender as TVirtualStringtree;
  case VT.GetNodeLevel(Node) of
    0: case Column of
         0: Indexes[Node.Index] := NewText + REGDELIM + VT.Text[Node, Column+1];
         1: Indexes[Node.Index] := VT.Text[Node, Column-1] + REGDELIM + NewText;
       end;
    1: begin
      IndexParts := TWideStringlist(Indexes.Objects[Node.Parent.Index]);
      IndexParts[Node.Index] := NewText;
    end;
  end;
  VT.RepaintNode(Node);
end;


procedure TfrmTableEditor.treeIndexesBeforePaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
begin
  // (Re)paint index list
  (Sender as TVirtualStringTree).RootNodeCount := Indexes.Count;
end;


procedure TfrmTableEditor.treeIndexesDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  Node: PVirtualNode;
begin
  // Accept nodes from the column list and allow column moving
  if Source = listColumns then begin
    Accept := True;
    Exit;
  end else if Source = Sender then begin
    Node := Sender.GetNodeAt(Pt.X, Pt.Y);
    Accept := Assigned(Node) and (Sender.GetNodeLevel(Node) = 1) and
      (Node <> Sender.FocusedNode) and (Node.Parent = Sender.FocusedNode.Parent);
  end;
end;


procedure TfrmTableEditor.treeIndexesDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  Node: PVirtualNode;
  IndexParts: TWideStringlist;
  ColName: WideString;
  ColPos: Integer;
  VT: TVirtualStringtree;
begin
  // Column node dropped here
  VT := Sender as TVirtualStringtree;
  Node := VT.GetNodeAt(Pt.X, Pt.Y);
  if not Assigned(Node) then begin
    MessageBeep(MB_ICONEXCLAMATION);
    Exit;
  end;
  if VT.GetNodeLevel(Node) = 1 then begin
    ColPos := Node.Index;
    if Mode = dmBelow then Inc(ColPos);
    Node := Node.Parent;
  end else
    ColPos := MaxInt;

  if Source = Sender then
    MoveFocusedIndexPart(ColPos)
  else begin
    IndexParts := TWideStringlist(Indexes.Objects[Node.Index]);
    ColName := Columns[(Source as TVirtualStringTree).FocusedNode.Index];
    if IndexParts.IndexOf(ColName) > -1 then begin
      if MessageDlg('Index "'+VT.Text[Node, 0]+'" already contains the column "'+ColName+'". It is possible to add a column twice into a index, but total nonsense in practice.'+CRLF+CRLF+'Add anyway?',
        mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;
    end;

    if ColPos >= IndexParts.Count then
      IndexParts.Add(ColName)
    else
      IndexParts.Insert(ColPos, ColName);
    Node.States := Node.States + [vsHasChildren, vsExpanded];
  end;
  // Finally tell parent node to update its children
  VT.ReinitChildren(Node, False);
end;


procedure TfrmTableEditor.btnMoveUpIndexClick(Sender: TObject);
begin
  // Move index part up
  MoveFocusedIndexPart(treeIndexes.FocusedNode.Index-1);
end;


procedure TfrmTableEditor.btnMoveDownIndexClick(Sender: TObject);
begin
  // Move index part down
  MoveFocusedIndexPart(treeIndexes.FocusedNode.Index+1);
end;


procedure TfrmTableEditor.MoveFocusedIndexPart(NewIdx: Cardinal);
var
  IndexParts: TWideStringlist;
begin
  // Move focused index or index part
  if treeIndexes.IsEditing then
    treeIndexes.EndEditNode;
  IndexParts := TWideStringlist(Indexes.Objects[treeIndexes.FocusedNode.Parent.Index]);
  IndexParts.Move(treeIndexes.FocusedNode.Index, NewIdx);
  SelectNode(treeIndexes, NewIdx, treeIndexes.FocusedNode.Parent);
end;


procedure TfrmTableEditor.SetStatus(msg: WideString);
begin
  if msg <> '' then
    Screen.Cursor := crHourglass
  else
    Screen.Cursor := crDefault;
  lblStatus.Caption := msg;
  lblStatus.Repaint;
end;


procedure TfrmTableEditor.PageControlMainChange(Sender: TObject);
begin
  UpdateSQLcode;
end;


procedure TfrmTableEditor.UpdateSQLcode;
var
  OldTopLine: Integer;
begin
  if (PageControlMain.ActivePage = tabALTERCode) and (not AlterCodeValid) then begin
    SynMemoALTERcode.BeginUpdate;
    OldTopLine := SynMemoALTERcode.TopLine;
    SynMemoALTERcode.Text := ComposeAlterStatement;
    SynMemoALTERcode.TopLine := OldTopLine;
    SynMemoALTERcode.EndUpdate;
    AlterCodeValid := True;
  end else if (PageControlMain.ActivePage = tabCREATECode) and (not CreateCodeValid) then begin
    SynMemoCREATEcode.BeginUpdate;
    OldTopLine := SynMemoCREATEcode.TopLine;
    SynMemoCREATEcode.Text := ComposeCreateStatement;
    SynMemoCREATEcode.TopLine := OldTopLine;
    SynMemoCREATEcode.EndUpdate;
    CreateCodeValid := True;
  end;
end;


procedure TfrmTableEditor.chkCharsetConvertClick(Sender: TObject);
begin
  chkCharsetConvert.Enabled := (FAlterTablename <> '') and (comboCollation.ItemIndex > -1);
  listColumns.Repaint;
  Modification(Sender);
end;


procedure TfrmTableEditor.btnHelpClick(Sender: TObject);
begin
  // Help button
  Mainform.CallSQLHelpWithKeyword('CREATE TABLE');
end;


end.
