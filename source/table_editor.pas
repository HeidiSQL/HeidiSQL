unit table_editor;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ToolWin, VirtualTrees, SynRegExpr, ActiveX, ExtCtrls, SynEdit,
  SynMemo, Menus, Contnrs,
  grideditlinks, mysql_structures, mysql_connection, helpers, mysql_api;

type
  TFrame = TDBObjectEditor;
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
    editName: TEdit;
    memoComment: TMemo;
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
    memoUnionTables: TMemo;
    comboInsertMethod: TComboBox;
    lblCollation: TLabel;
    comboCollation: TComboBox;
    lblEngine: TLabel;
    comboEngine: TComboBox;
    treeIndexes: TVirtualStringTree;
    tlbIndexes: TToolBar;
    btnAddIndex: TToolButton;
    btnRemoveIndex: TToolButton;
    btnClearIndexes: TToolButton;
    btnMoveUpIndex: TToolButton;
    btnMoveDownIndex: TToolButton;
    pnlColumnsTop: TPanel;
    tlbColumns: TToolBar;
    btnAddColumn: TToolButton;
    btnRemoveColumn: TToolButton;
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
    popupColumns: TPopupMenu;
    menuAddColumn: TMenuItem;
    menuRemoveColumn: TMenuItem;
    menuMoveUpColumn: TMenuItem;
    menuMoveDownColumn: TMenuItem;
    chkCharsetConvert: TCheckBox;
    N1: TMenuItem;
    menuCreateIndex: TMenuItem;
    menuAddToIndex: TMenuItem;
    tabForeignKeys: TTabSheet;
    tlbForeignKeys: TToolBar;
    btnAddForeignKey: TToolButton;
    btnRemoveForeignKey: TToolButton;
    btnClearForeignKeys: TToolButton;
    menuCopyColumnCell: TMenuItem;
    N2: TMenuItem;
    popupSQLmemo: TPopupMenu;
    menuSQLCopy: TMenuItem;
    menuSQLSelectAll: TMenuItem;
    pnlNoForeignKeys: TPanel;
    listForeignKeys: TVirtualStringTree;
    procedure editNameChange(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure btnAddColumnClick(Sender: TObject);
    procedure btnRemoveColumnClick(Sender: TObject);
    procedure listColumnsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure btnHelpClick(Sender: TObject);
    procedure listColumnsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure listColumnsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure listColumnsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: String);
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
		  var CellText: String);
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
    procedure treeIndexesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: String);
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
    procedure popupColumnsPopup(Sender: TObject);
    procedure AddIndexByColumn(Sender: TObject);
    procedure listColumnsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure listForeignKeysBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure listForeignKeysCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure listForeignKeysFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure listForeignKeysGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure listForeignKeysGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure listForeignKeysNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    procedure btnClearForeignKeysClick(Sender: TObject);
    procedure btnAddForeignKeyClick(Sender: TObject);
    procedure btnRemoveForeignKeyClick(Sender: TObject);
    procedure listForeignKeysEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure listColumnsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure listColumnsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure listColumnsNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure popupSQLmemoPopup(Sender: TObject);
    procedure listColumnsKeyPress(Sender: TObject; var Key: Char);
    procedure vtHandleClickOrKeyPress(Sender: TVirtualStringTree;
      Node: PVirtualNode; Column: TColumnIndex; HitPositions: THitPositions);
    procedure listColumnsStructureChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Reason: TChangeReason);
  private
    { Private declarations }
    FLoaded: Boolean;
    CreateCodeValid, AlterCodeValid: Boolean;
    FColumns: TTableColumnList;
    FKeys: TTableKeyList;
    FForeignKeys: TForeignKeyList;
    DeletedKeys, DeletedForeignKeys: TStringList;
    procedure ValidateColumnControls;
    procedure ValidateIndexControls;
    procedure MoveFocusedIndexPart(NewIdx: Cardinal);
    procedure ResetModificationFlags;
    function ComposeCreateStatement: String;
    function ComposeAlterStatement: String;
    function GetIndexSQL(idx: Integer): String;
    function GetForeignKeySQL(idx: Integer): String;
    procedure UpdateSQLcode;
    function CellEditingAllowed(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    function GetIndexIcon(idx: Integer): Integer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(ObjectName: String=''; ObjectType: TListNodeType=lntNone); override;
    function ApplyModifications: TModalResult; override;
  end;


implementation

uses main;

const
  NotModifiedFlag : Byte = 0;
  ModifiedFlag : Byte = 1;


{$R *.dfm}


constructor TfrmTableEditor.Create(AOwner: TComponent);
begin
  inherited;
  ScaleControls(Screen.PixelsPerInch, FORMS_DPI);
  PageControlMain.Height := GetRegValue(REGNAME_TABLEEDITOR_TABSHEIGHT, PageControlMain.Height);
  FixVT(listColumns);
  FixVT(treeIndexes);
  FixVT(listForeignKeys);
  // Try the best to auto fit various column widths, respecting a custom DPI setting and a pulldown arrow
  listColumns.Header.Columns[2].Width := Mainform.Canvas.TextWidth('GEOMETRYCOLLECTION') + 6*listColumns.TextMargin;
  listColumns.Header.Columns[6].Width := Mainform.Canvas.TextWidth('AUTO_INCREMENT') + 4*listColumns.TextMargin;
  listColumns.Header.Columns[8].Width := Mainform.Canvas.TextWidth('macroman_general_ci') + 6*listColumns.TextMargin;
  // Overide column widths by custom values
  Mainform.RestoreListSetup(listColumns);
  Mainform.RestoreListSetup(treeIndexes);
  Mainform.RestoreListSetup(listForeignKeys);
  comboRowFormat.Items.CommaText := 'DEFAULT,DYNAMIC,FIXED,COMPRESSED,REDUNDANT,COMPACT';
  comboInsertMethod.Items.CommaText := 'NO,FIRST,LAST';
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
  DeletedKeys := TStringList.Create;
  DeletedForeignKeys := TStringList.Create;
  editName.MaxLength := NAME_LEN;
end;


destructor TfrmTableEditor.Destroy;
begin
  // Store GUI setup
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_TABLEEDITOR_TABSHEIGHT, PageControlMain.Height);
  Mainform.SaveListSetup(listColumns);
  Mainform.SaveListSetup(treeIndexes);
  Mainform.SaveListSetup(listForeignKeys);
  inherited;
end;


procedure TfrmTableEditor.Init(ObjectName: String=''; ObjectType: TListNodeType=lntNone);
var
  CreateTable, AttrName, AttrValue: String;
  rx: TRegExpr;
begin
  inherited;
  FLoaded := False;
  comboEngine.Items := Mainform.Connection.TableEngines;
  comboEngine.ItemIndex := comboEngine.Items.IndexOf(Mainform.Connection.TableEngineDefault);
  comboCollation.Items := Mainform.Connection.CollationList;
  listColumns.BeginUpdate;
  FColumns.Clear;
  btnClearIndexesClick(Self);
  btnClearForeignKeysClick(Self);
  tabALTERcode.TabVisible := FEditObjectName <> '';
  // Clear value editors
  memoComment.Text := '';
  editAutoInc.Text := '';
  editAvgRowLen.Text := '';
  editMaxRows.Text := '';
  chkChecksum.Checked := False;
  comboRowFormat.ItemIndex := 0;
  comboCollation.ItemIndex := -1;
  memoUnionTables.Clear;
  comboInsertMethod.ItemIndex := -1;

  if FEditObjectName = '' then begin
    // Creating new table
    editName.Text := '';
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(Mainform.Connection.GetVar('SHOW VARIABLES LIKE ''collation_database''', 1));
    PageControlMain.ActivePage := tabBasic;
  end else begin
    // Editing existing table
    editName.Text := FEditObjectName;
    CreateTable := Mainform.SelectedTableCreateStatement;
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.Expression := '\s(\S+)\s*=\s*(\S+)';
    if rx.Exec(CreateTable) then while true do begin
      AttrName := UpperCase(rx.Match[1]);
      AttrValue := rx.Match[2];
      if (AttrName='ENGINE') or (AttrName='TYPE') then
        comboEngine.ItemIndex := comboEngine.Items.IndexOf(AttrValue)
      else if AttrName='COLLATE' then
        comboCollation.ItemIndex := comboCollation.Items.IndexOf(AttrValue)
      else if AttrName='AVG_ROW_LENGTH' then
        editAvgRowLen.Text := AttrValue
      else if AttrName='AUTO_INCREMENT' then
        editAutoInc.Text := AttrValue
      else if AttrName='ROW_FORMAT' then
        comboRowFormat.ItemIndex := comboRowFormat.Items.IndexOf(AttrValue)
      else if AttrName='CHECKSUM' then
        chkChecksum.Checked := AttrValue='1'
      else if AttrName='MAX_ROWS' then
        editMaxRows.Text := AttrValue
      else if AttrName='INSERT_METHOD' then
        comboInsertMethod.ItemIndex := comboInsertMethod.Items.IndexOf(AttrValue);
      if not rx.ExecNext then
        break;
    end;

    rx.Expression := '\bUNION=\((.+)\)';
    if rx.Exec(CreateTable) then
      memoUnionTables.Lines.Text := rx.Match[1]
    else
      memoUnionTables.Lines.Clear;
    rx.Expression := '\bCOMMENT=''((.+)[^''])''';
    if rx.Exec(CreateTable) then
      memoComment.Lines.Text := StringReplace(rx.Match[1], '''''', '''', [rfReplaceAll])
    else
      memoComment.Lines.Clear;
    ParseTableStructure(CreateTable, FColumns, FKeys, FForeignKeys);
  end;
  listColumns.RootNodeCount := FColumns.Count;
  DeInitializeVTNodes(listColumns);
  listColumns.EndUpdate;
  // Validate controls
  comboEngineSelect(comboEngine);
  ValidateColumnControls;
  ValidateIndexControls;
  ResetModificationFlags;
  CreateCodeValid := False;
  AlterCodeValid := False;
  PageControlMainChange(Self); // Foreign key editor needs a hit
  UpdateSQLCode;
  // Indicate change mechanisms can call their events now. See Modification().
  FLoaded := True;
  // Empty status panel
  Mainform.showstatus;
  Screen.Cursor := crDefault;
end;


procedure TfrmTableEditor.btnDiscardClick(Sender: TObject);
begin
  // Reinit GUI, discarding changes
  Modified := False;
  Init(FEditObjectName);
end;


procedure TfrmTableEditor.btnSaveClick(Sender: TObject);
begin
  ApplyModifications;
end;


function TfrmTableEditor.ApplyModifications: TModalResult;
var
  sql: String;
  i: Integer;
  Specs: TStringList;
begin
  // Create or alter table
  Result := mrOk;
  Specs := TStringList.Create;
  if FEditObjectName = '' then
    sql := ComposeCreateStatement
  else begin
    sql := ComposeAlterStatement;
    // Special case for altered foreign keys: These have to be dropped in a seperate query
    // otherwise the server would return error 121 "Duplicate key on write or update"
    // See also http://dev.mysql.com/doc/refman/5.1/en/innodb-foreign-key-constraints.html :
    //   "You cannot add a foreign key and drop a foreign key in separate clauses of a single
    //   ALTER TABLE  statement. Separate statements are required."
    for i:=0 to FForeignKeys.Count-1 do begin
      if FForeignKeys[i].Modified and (not FForeignKeys[i].Added) then
        Specs.Add('DROP FOREIGN KEY '+Mainform.mask(FForeignKeys[i].KeyName));
    end;
  end;
  try
    if Specs.Count > 0 then
      Mainform.Connection.Query('ALTER TABLE '+Mainform.mask(FEditObjectName)+' '+ImplodeStr(', ', Specs));
    Mainform.Connection.Query(sql);
    tabALTERcode.TabVisible := FEditObjectName <> '';
    if chkCharsetConvert.Checked then begin
      // Autoadjust column collations
      for i:=0 to FColumns.Count-1 do begin
        if FColumns[i].Collation <> '' then
          FColumns[i].Collation := comboCollation.Text;
      end;
    end;
    // Set table name for altering if Apply was clicked
    FEditObjectName := editName.Text;
    tabALTERcode.TabVisible := FEditObjectName <> '';
    Mainform.SetEditorTabCaption(Self, FEditObjectName);
    Mainform.RefreshTreeDB(Mainform.ActiveDatabase);
    Mainform.ParseSelectedTableStructure;
    ResetModificationFlags;
  except
    on E:Exception do begin
      MessageDlg(E.Message, mtError, [mbOk], 0);
      Result := mrAbort;
    end;
  end;
end;


procedure TfrmTableEditor.ResetModificationFlags;
var
  i: Integer;
begin
  // Enable converting data for an existing table
  chkCharsetConvertClick(comboCollation);
  // Assist the user in auto unchecking this checkbox so data doesn't get converted more than once accidently
  chkCharsetConvert.Checked := False;
  // Reset modification flags of TEdits and TMemos
  for i:=0 to ComponentCount-1 do
    Components[i].Tag := NotModifiedFlag;
  // Reset column changes
  for i:=FColumns.Count-1 downto 0 do begin
    if FColumns[i].Status = esDeleted then
      FColumns.Delete(i)
    else begin
      FColumns[i].Status := esUntouched;
      FColumns[i].OldName := '';
    end;
  end;
  DeletedKeys.Clear;
  for i:=0 to FKeys.Count-1 do begin
    FKeys[i].Added := False;
    FKeys[i].Modified := False;
  end;
  DeletedForeignKeys.Clear;
  for i:=0 to FForeignKeys.Count-1 do begin
    FForeignKeys[i].Added := False;
    FForeignKeys[i].Modified := False;
  end;
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
end;


function TfrmTableEditor.ComposeAlterStatement: String;
var
  Specs: TStringList;
  ColSpec, OldColName, IndexSQL: String;
  i: Integer;
  Results: TMySQLQuery;
  Col, PreviousCol: PTableColumn;
  Node: PVirtualNode;
begin
  // Compose ALTER query, called by buttons and for SQL code tab
  Mainform.showstatus('Composing ALTER statement ...');
  Screen.Cursor := crHourglass;
  Specs := TStringList.Create;
  if editName.Text <> FEditObjectName then
    Specs.Add('RENAME TO ' + Mainform.mask(editName.Text));
  if memoComment.Tag = ModifiedFlag then
    Specs.Add('COMMENT=' + esc(memoComment.Text));
  if (comboCollation.Tag = ModifiedFlag) or (chkCharsetConvert.Checked) then
    Specs.Add('COLLATE=' + comboCollation.Text);
  if comboEngine.Tag = ModifiedFlag then begin
    if Mainform.Connection.ServerVersionInt < 40018 then
      Specs.Add('TYPE=' + comboEngine.Text)
    else
      Specs.Add('ENGINE=' + comboEngine.Text);
  end;
  if comboRowFormat.Tag = ModifiedFlag then
    Specs.Add('ROW_FORMAT=' + comboRowFormat.Text);
  if chkChecksum.Tag = ModifiedFlag then
    Specs.Add('CHECKSUM=' + IntToStr(Integer(chkChecksum.Checked)));
  if editAutoInc.Tag = ModifiedFlag then
    Specs.Add('AUTO_INCREMENT=' + IntToStr(MakeInt(editAutoInc.Text)));
  if editAvgRowLen.Tag = ModifiedFlag then
    Specs.Add('AVG_ROW_LENGTH=' + IntToStr(MakeInt(editAvgRowLen.Text)));
  if editMaxRows.Tag = ModifiedFlag then
    Specs.Add('MAX_ROWS=' + IntToStr(MakeInt(editMaxRows.Text)));
  if memoUnionTables.Enabled and (memoUnionTables.Tag = ModifiedFlag) and (memoUnionTables.Text <> '') then
    Specs.Add('UNION=('+memoUnionTables.Text+')');
  if comboInsertMethod.Enabled and (comboInsertMethod.Tag = ModifiedFlag) and (comboInsertMethod.Text <> '') then
    Specs.Add('INSERT_METHOD='+comboInsertMethod.Text);
  if chkCharsetConvert.Checked then begin
    Results := Mainform.Connection.CollationTable;
    if Assigned(Results) then while not Results.Eof do begin
      if Results.Col('Collation') = comboCollation.Text then begin
        Specs.Add('CONVERT TO CHARSET '+Results.Col('Charset'));
        break;
      end;
      Results.Next;
    end;
  end;

  // Update columns
  EnableProgressBar(FColumns.Count + DeletedKeys.Count + FKeys.Count);
  Node := listColumns.GetFirst;
  PreviousCol := nil;
  while Assigned(Node) do begin
    Mainform.ProgressBarStatus.StepIt;
    Col := listColumns.GetNodeData(Node);
    if Col.Status <> esUntouched then begin
      ColSpec := Mainform.mask(Col.Name);
      ColSpec := ColSpec + ' ' + Col.DataType.Name;
      if Col.LengthSet <> '' then
        ColSpec := ColSpec + '(' + Col.LengthSet + ')';
      if Col.DataType.HasUnsigned and Col.Unsigned then
        ColSpec := ColSpec + ' UNSIGNED';
      if not Col.AllowNull then
        ColSpec := ColSpec + ' NOT';
      ColSpec := ColSpec + ' NULL';
      if Col.DefaultType <> cdtNothing then begin
        ColSpec := ColSpec + ' ' + GetColumnDefaultClause(Col.DefaultType, Col.DefaultText);
        ColSpec := TrimRight(ColSpec); // Remove whitespace for columns without default value
      end;
      if Col.Comment <> '' then
        ColSpec := ColSpec + ' COMMENT '+esc(Col.Comment);
      if Col.Collation <> '' then begin
        ColSpec := ColSpec + ' COLLATE ';
        if chkCharsetConvert.Checked then
          ColSpec := ColSpec + comboCollation.Text
        else
          ColSpec := ColSpec + Col.Collation;
      end;
      // Server version requirement, see http://dev.mysql.com/doc/refman/4.1/en/alter-table.html
      if Mainform.Connection.ServerVersionInt >= 40001 then begin
        if PreviousCol = nil then
          ColSpec := ColSpec + ' FIRST'
        else
          ColSpec := ColSpec + ' AFTER '+Mainform.mask(PreviousCol.Name);
      end;
      if Col.Status = esModified then begin
        OldColName := Col.OldName;
        if OldColName = '' then
          OldColName := Col.Name;
        Specs.Add('CHANGE COLUMN '+Mainform.mask(OldColName) + ' ' + ColSpec);
      end else if Col.Status in [esAddedUntouched, esAddedModified] then
        Specs.Add('ADD COLUMN ' + ColSpec);
    end;
    PreviousCol := Col;
    Node := listColumns.GetNextSibling(Node);
  end;

  // Deleted columns, not available as Node in above loop
  for i:=0 to FColumns.Count-1 do begin
    if FColumns[i].Status = esDeleted then begin
      OldColName := FColumns[i].OldName;
      if OldColName = '' then
        OldColName := FColumns[i].Name;
      Specs.Add('DROP COLUMN '+Mainform.mask(OldColName));
    end;
  end;

  // Drop indexes, also changed indexes, which will be readded below
  for i:=0 to DeletedKeys.Count-1 do begin
    Mainform.ProgressBarStatus.StepIt;
    if DeletedKeys[i] = PKEY then
      IndexSQL := 'PRIMARY KEY'
    else
      IndexSQL := 'INDEX ' + Mainform.Mask(DeletedKeys[i]);
    Specs.Add('DROP '+IndexSQL);
  end;
  // Add changed or added indexes
  for i:=0 to FKeys.Count-1 do begin
    Mainform.ProgressBarStatus.StepIt;
    if FKeys[i].Modified and (not FKeys[i].Added) then begin
      if FKeys[i].IndexType = PKEY then
        IndexSQL := 'PRIMARY KEY'
      else
        IndexSQL := 'INDEX ' + Mainform.Mask(FKeys[i].OldName);
      Specs.Add('DROP '+IndexSQL);
    end;
    if FKeys[i].Added or FKeys[i].Modified then
      Specs.Add('ADD '+GetIndexSQL(i));
  end;

  for i:=0 to DeletedForeignKeys.Count-1 do
    Specs.Add('DROP FOREIGN KEY '+Mainform.mask(DeletedForeignKeys[i]));
  for i:=0 to FForeignKeys.Count-1 do begin
    if FForeignKeys[i].Added or FForeignKeys[i].Modified then
      Specs.Add('ADD '+GetForeignKeySQL(i));
  end;

  Result := 'ALTER TABLE '+Mainform.mask(FEditObjectName) + CRLF + #9 + ImplodeStr(',' + CRLF + #9, Specs);
  Result := Trim(Result);
  FreeAndNil(Specs);
  Mainform.showstatus;
  Mainform.ProgressBarStatus.Hide;
  Screen.Cursor := crDefault;
end;


function TfrmTableEditor.ComposeCreateStatement: String;
var
  i, IndexCount: Integer;
  Col: PTableColumn;
  Node: PVirtualNode;
  tmp: String;
begin
  // Compose CREATE query, called by buttons and for SQL code tab
  Result := 'CREATE TABLE '+Mainform.mask(editName.Text)+' ('+CRLF;
  Node := listColumns.GetFirst;
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    Result := Result + #9 + Mainform.mask(Col.Name) + ' ' +Col.DataType.Name;
    if Col.LengthSet <> '' then
      Result := Result + '(' + Col.LengthSet + ')';
    if Col.DataType.HasUnsigned and Col.Unsigned then
      Result := Result + ' UNSIGNED';
    if not Col.AllowNull then
      Result := Result + ' NOT';
    Result := Result + ' NULL';
    if Col.DefaultType <> cdtNothing then begin
      Result := Result + ' ' + GetColumnDefaultClause(Col.DefaultType, Col.DefaultText);
      Result := TrimRight(Result); // Remove whitespace for columns without default value
    end;
    if Col.Comment <> '' then
      Result := Result + ' COMMENT '+esc(Col.Comment);
    if Col.Collation <> '' then
      Result := Result + ' COLLATE '+Col.Collation;
    Result := Result + ','+CRLF;
    Node := listColumns.GetNextSibling(Node);
  end;

  IndexCount := 0;
  for i:=0 to FKeys.Count-1 do begin
    tmp := GetIndexSQL(i);
    if tmp <> '' then begin
      Result := Result + #9 + tmp + ','+CRLF;
      Inc(IndexCount);
    end;
  end;

  for i:=0 to FForeignKeys.Count-1 do
    Result := Result + #9 + GetForeignKeySQL(i) + ','+CRLF;

  if Integer(listColumns.RootNodeCount) + IndexCount + FForeignKeys.Count > 0 then
    Delete(Result, Length(Result)-2, 3);

  Result := Result + CRLF + ')' + CRLF;
  if memoComment.Text <> '' then
    Result := Result + 'COMMENT='+esc(memoComment.Text) + CRLF;
  if comboCollation.Text <> '' then
    Result := Result + 'COLLATE='+comboCollation.Text + CRLF;
  if comboEngine.Text <> '' then begin
    if Mainform.Connection.ServerVersionInt < 40018 then
      Result := Result + 'TYPE='+comboEngine.Text + CRLF
    else
      Result := Result + 'ENGINE='+comboEngine.Text + CRLF;
  end;
  if comboRowFormat.Text <> '' then
    Result := Result + 'ROW_FORMAT='+comboRowFormat.Text + CRLF;
  if chkChecksum.Checked then
    Result := Result + 'CHECKSUM='+IntToStr(Integer(chkChecksum.Checked)) + CRLF;
  if editAutoInc.Text <> '' then
    Result := Result + 'AUTO_INCREMENT='+editAutoInc.Text + CRLF;
  if editAvgRowLen.Text <> '' then
    Result := Result + 'AVG_ROW_LENGTH='+editAvgRowLen.Text + CRLF;
  if editMaxRows.Text <> '' then
    Result := Result + 'MAX_ROWS='+editMaxRows.Text + CRLF;
  if memoUnionTables.Enabled and (memoUnionTables.Text <> '') then
    Result := Result + 'UNION=('+memoUnionTables.Text+')' + CRLF;
  if comboInsertMethod.Enabled and (comboInsertMethod.Text <> '') then
    Result := Result + 'INSERT_METHOD='+comboInsertMethod.Text + CRLF;
  Result := Trim(Result);
end;


function TfrmTableEditor.GetIndexSQL(idx: Integer): String;
var
  i: Integer;
begin
  Result := '';
  // Supress SQL error  trying index creation with 0 column
  if FKeys[idx].Columns.Count = 0 then
    Exit;
  if FKeys[idx].IndexType = PKEY then
    Result := Result + 'PRIMARY KEY '
  else begin
    if FKeys[idx].IndexType <> KEY then
      Result := Result + FKeys[idx].IndexType + ' ';
    Result := Result + 'INDEX ' + Mainform.Mask(FKeys[idx].Name) + ' ';
  end;
  Result := Result + '(';
  for i:=0 to FKeys[idx].Columns.Count-1 do begin
    Result := Result + Mainform.Mask(FKeys[idx].Columns[i]);
    if FKeys[idx].SubParts[i] <> '' then
      Result := Result + '(' + FKeys[idx].SubParts[i] + ')';
    Result := Result + ', ';
  end;
  if FKeys[idx].Columns.Count > 0 then
    Delete(Result, Length(Result)-1, 2);

  Result := Result + ')';

  if FKeys[idx].Algorithm <> '' then
    Result := Result + ' USING ' + FKeys[idx].Algorithm;
end;


function TfrmTableEditor.GetForeignKeySQL(idx: Integer): String;
var
  Key: TForeignKey;
  i: Integer;
begin
  Key := FForeignKeys[idx];
  Result := 'CONSTRAINT '+Mainform.mask(Key.KeyName)+' FOREIGN KEY (';
  for i:=0 to Key.Columns.Count-1 do
    Result := Result + Mainform.mask(Key.Columns[i]) + ', ';
  if Key.Columns.Count > 0 then Delete(Result, Length(Result)-1, 2);
  Result := Result + ') REFERENCES ' + Mainform.MaskMulti(Key.ReferenceTable) + ' (';
  for i:=0 to Key.ForeignColumns.Count-1 do
    Result := Result + Mainform.mask(Key.ForeignColumns[i]) + ', ';
  if Key.ForeignColumns.Count > 0 then Delete(Result, Length(Result)-1, 2);
  Result := Result + ')';
  if Key.OnUpdate <> '' then
    Result := Result + ' ON UPDATE ' + Key.OnUpdate;
  if Key.OnDelete <> '' then
    Result := Result + ' ON DELETE ' + Key.OnDelete;
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
    btnSave.Enabled := Modified;
    btnDiscard.Enabled := Modified;
    CreateCodeValid := False;
    AlterCodeValid := False;
    UpdateSQLcode;
  end;
end;


procedure TfrmTableEditor.btnAddColumnClick(Sender: TObject);
var
  NewCol: TTableColumn;
  FocusedCol: PTableColumn;
  fn, NewNode: PVirtualNode;
  idx: Integer;
begin
  // Add new column after selected one
  if listColumns.IsEditing then
    listColumns.EndEditNode;
  fn := listColumns.FocusedNode;
  NewCol := TTableColumn.Create;
  if Assigned(fn) then begin
    idx := fn.Index+1;
    // Copy properties from focused node
    FocusedCol := listColumns.GetNodeData(fn);
    NewCol.DataType := FocusedCol.DataType;
    NewCol.LengthSet := FocusedCol.LengthSet;
    NewCol.Unsigned := FocusedCol.Unsigned;
    NewCol.AllowNull := FocusedCol.AllowNull;
    // There can be only one
    if FocusedCol.DefaultType = cdtAutoInc then begin
      NewCol.DefaultType := cdtText;
      NewCol.DefaultText := '0';
    end else begin
      NewCol.DefaultType := FocusedCol.DefaultType;
      NewCol.DefaultText := FocusedCol.DefaultText;
    end;
    NewCol.Comment := FocusedCol.Comment;
    NewCol.Collation := '';
  end else begin
    idx := listColumns.RootNodeCount;
    NewCol.DataType := GetDatatypeByName('INT');
    NewCol.LengthSet := '10';
    NewCol.Unsigned := False;
    NewCol.AllowNull := True;
    NewCol.DefaultType := cdtNothing;
    NewCol.DefaultText := '';
    NewCol.Comment := '';
    NewCol.Collation := '';
  end;
  NewCol.Name := 'Column '+IntToStr(idx+1);
  FColumns.Insert(idx, NewCol);
  NewNode := listColumns.InsertNode(fn, amInsertAfter, @NewCol);
  NewCol.Status := esAddedUntouched;
  SelectNode(listColumns, NewNode);
  ValidateColumnControls;
  listColumns.EditNode(NewNode, 1);
end;


procedure TfrmTableEditor.btnRemoveColumnClick(Sender: TObject);
var
  Node, NodeFocus: PVirtualNode;
  Col: PTableColumn;
begin
  // Remove selected column(s)
  Node := listColumns.GetFirstSelected;
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    if Col.OldName <> '' then begin
      Col.Name := Col.OldName;
      Col.OldName := '';
    end;
    Col.Status := esDeleted;
    Node := listColumns.GetNextSelected(Node);
  end;
  // Find next unselected node for new focus
  Node := listColumns.FocusedNode;
  NodeFocus := nil;
  while Assigned(Node) do begin
    if not (vsSelected in Node.States) then begin
      NodeFocus := Node;
      break;
    end;
    Node := listColumns.GetNextSibling(Node);
  end;

  listColumns.DeleteSelectedNodes;

  if not Assigned(NodeFocus) then
    NodeFocus := listColumns.GetLast;
  if Assigned(NodeFocus) then
    SelectNode(listColumns, NodeFocus.Index);
  ValidateColumnControls;
end;


procedure TfrmTableEditor.btnMoveUpColumnClick(Sender: TObject);
begin
  // Move column up
  listColumns.MoveTo(listColumns.FocusedNode, listColumns.GetPreviousSibling(listColumns.FocusedNode), amInsertBefore, False);
  ValidateColumnControls;
end;


procedure TfrmTableEditor.btnMoveDownColumnClick(Sender: TObject);
begin
  // Move column down
  listColumns.MoveTo(listColumns.FocusedNode, listColumns.GetNextSibling(listColumns.FocusedNode), amInsertAfter, False);
  ValidateColumnControls;
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
  AttachMode: TVTNodeAttachMode;
begin
  Node := Sender.GetNodeAt(Pt.X, Pt.Y);
  if Assigned(Node) then begin
    case Mode of
      dmAbove, dmOnNode: AttachMode := amInsertBefore;
      else AttachMode := amInsertAfter;
    end;
    listColumns.MoveTo(listColumns.FocusedNode, Node, AttachMode, False);
    ValidateColumnControls;
  end;
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
  Col: PTableColumn;
  ImageIndex, X, Y: Integer;
  VT: TVirtualStringTree;
begin
  // Paint checkbox image in certain columns
  // while restricting "Allow NULL" checkbox to numeric datatypes
  if (Column in [4, 5]) and CellEditingAllowed(Node, Column) then begin
    Col := Sender.GetNodeData(Node);
    if (Col.Unsigned and (Column=4)) or (Col.AllowNull and (Column=5)) then ImageIndex := 128
    else ImageIndex := 127;
    VT := TVirtualStringTree(Sender);
    X := CellRect.Left + (VT.Header.Columns[Column].Width div 2) - (VT.Images.Width div 2);
    Y := CellRect.Top + Integer(VT.NodeHeight[Node] div 2) - (VT.Images.Height div 2);
    VT.Images.Draw(TargetCanvas, X, Y, ImageIndex);
  end;
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
  btnRemoveColumn.Enabled := listColumns.SelectedCount > 0;
  btnMoveUpColumn.Enabled := Assigned(Node) and (Node <> listColumns.GetFirst);
  btnMoveDownColumn.Enabled := Assigned(Node) and (Node <> listColumns.GetLast);

  menuRemoveColumn.Enabled := btnRemoveColumn.Enabled;
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
  Col: PTableColumn;
begin
  Col := listColumns.GetNodeData(Node);
  case Column of
    // No editor for very first column and checkbox columns
    0: Result := False;
    3: Result := Col.DataType.HasLength;
    4: Result := Col.DataType.HasUnsigned;
    // No editing of collation allowed if "Convert data" was checked
    8: Result := not chkCharsetConvert.Checked;
    else Result := True;
  end;
end;


procedure TfrmTableEditor.listColumnsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Col: PTableColumn;
begin
  // Display column text
  Col := Sender.GetNodeData(Node);
  case Column of
    0: CellText := IntToStr(Node.Index+1);
    1: CellText := Col.Name;
    2: CellText := Col.DataType.Name;
    3: CellText := Col.LengthSet;
    4, 5: CellText := ''; // Checkbox
    6: begin
      case Col.DefaultType of
        cdtNothing:                 CellText := 'No default';
        cdtText, cdtTextUpdateTS: begin
          if Col.DataType.Category = dtcInteger then
            CellText := Col.DefaultText
          else
            CellText := esc(Col.DefaultText);
        end;
        cdtNull, cdtNullUpdateTS:   CellText := 'NULL';
        cdtCurTS, cdtCurTSUpdateTS: CellText := 'CURRENT_TIMESTAMP';
        cdtAutoInc:                 CellText := 'AUTO_INCREMENT';
      end;
      if Col.DefaultType in [cdtTextUpdateTS, cdtNullUpdateTS, cdtCurTSUpdateTS] then
        CellText := CellText + ' ON UPDATE CURRENT_TIMESTAMP';
    end;
    7: CellText := Col.Comment;
    8: begin
      CellText := Col.Collation;
      if (CellText <> '') and (chkCharsetConvert.Checked) then
        CellText := comboCollation.Text;
    end;
  end;
end;


procedure TfrmTableEditor.listColumnsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Col: PTableColumn;
begin
  // Bind data to node
  Col := Sender.GetNodeData(Node);
  Col^ := FColumns[Node.Index];
end;


procedure TfrmTableEditor.listColumnsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  i: Integer;
  Col: PTableColumn;
begin
  // Primary key icon
  if Column <> 0 then Exit;
  Col := Sender.GetNodeData(Node);

  for i:=0 to FKeys.Count-1 do begin
    if FKeys[i].Columns.IndexOf(Col.Name) > -1 then
      ImageIndex := GetIndexIcon(i);
    // Priority for PK columns. We cannot display more than one icon anyway.
    if ImageIndex > -1 then
      break;
  end;
end;


procedure TfrmTableEditor.listColumnsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTableColumn);
end;


procedure TfrmTableEditor.listColumnsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  TextColor: TColor;
  i: Integer;
  Col: PTableColumn;
begin
  Col := Sender.GetNodeData(Node);
  // Bold font for primary key columns
  for i:=0 to FKeys.Count-1 do begin
    if (FKeys[i].IndexType = PKEY) and (FKeys[i].Columns.IndexOf(Col.Name) > -1) then begin
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      break;
    end;
  end;

  // No specific colors for selected nodes, would interfere with blue selection background
  if vsSelected in Node.States then Exit;
  // Break early if nothing to do
  if not (Column in [2, 6]) then Exit;

  // Give datatype column specific color, as set in preferences
  TextColor := TargetCanvas.Font.Color;
  case Column of
    2: TextColor := DatatypeCategories[Integer(Col.DataType.Category)].Color;

    6: case Col.DefaultType of
      cdtNothing, cdtNull, cdtNullUpdateTS:
        TextColor := clGray;
      cdtCurTS, cdtCurTSUpdateTS:
        TextColor := DatatypeCategories[Integer(dtcTemporal)].Color;
      cdtAutoInc:
        TextColor := clNavy;
    end;
  end;
  TargetCanvas.Font.Color := TextColor;
end;


procedure TfrmTableEditor.listColumnsStructureChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Reason: TChangeReason);
begin
  // Auto resize first column to optimal width
  listColumns.Header.AutoFitColumns(False, smaUseColumnOption, 0, 0);
end;


procedure TfrmTableEditor.listColumnsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  i: Integer;
  Col: PTableColumn;
begin
  // Column property edited
  Col := Sender.GetNodeData(Node);
  case Column of
    1: begin // Name of column
      for i:=0 to FColumns.Count-1 do begin
        if FColumns[i].Name = NewText then begin
          MessageDlg('Column "'+NewText+'" already exists.', mtError, [mbOk], 0);
          Exit;
        end;
      end;
      Col.OldName := Col.Name;
      Col.Name := NewText;
    end;
    2: begin // Data type
      Col.DataType := GetDatatypeByName(NewText);
      // Reset length/set for column types which don't support that
      if not Col.DataType.HasLength then
        Col.LengthSet := '';
      // Suggest length/set if required
      if Col.DataType.RequiresLength and (Col.LengthSet = '') then
        Col.LengthSet := Col.DataType.DefLengthSet;
      // Auto-fix user selected default type which can be invalid now
      case Col.DataType.Category of
        dtcInteger, dtcReal: begin
          if Col.DefaultType in [cdtCurTS, cdtCurTSUpdateTS] then
            Col.DefaultType := cdtNothing;
          if Col.DefaultType = cdtTextUpdateTS then
            Col.DefaultType := cdtText;
          if Col.DefaultType = cdtNullUpdateTS then
            Col.DefaultType := cdtNull;
        end;
        dtcText, dtcIntegerNamed, dtcBinary, dtcSpatial, dtcSet, dtcSetNamed: begin
          if Col.DefaultType in [cdtCurTS, cdtCurTSUpdateTS, cdtAutoInc] then
            Col.DefaultType := cdtNothing;
          if Col.DefaultType = cdtNullUpdateTS then
            Col.DefaultType := cdtNull;
        end;
        dtcTemporal: begin
          if Col.DefaultType = cdtAutoinc then
            Col.DefaultType := cdtNothing;
        end;
      end;

    end; // Length / Set
    3: Col.LengthSet := NewText;
    // 4 + 5 are checkboxes - handled in OnClick
    6: begin // Default value
      Col.DefaultText := NewText;
      Col.DefaultType := GetColumnDefaultType(Col.DefaultText);
    end;
    7: Col.Comment := NewText;
    8: Col.Collation := NewText;
  end;
  Col.Status := esModified;
end;


procedure TfrmTableEditor.listColumnsNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Col: PTableColumn;
begin
  Col := Sender.GetNodeData(Node);
  Col.Status := esModified;
end;


procedure TfrmTableEditor.listColumnsClick(Sender: TObject);
var
  VT: TVirtualStringTree;
  Click: THitInfo;
begin
  // Handle click event
  VT := Sender as TVirtualStringTree;
  VT.GetHitTestInfoAt(Mouse.CursorPos.X-VT.ClientOrigin.X, Mouse.CursorPos.Y-VT.ClientOrigin.Y, True, Click);
  vtHandleClickOrKeyPress(VT, Click.HitNode, Click.HitColumn, Click.HitPositions);
end;



procedure TfrmTableEditor.listColumnsKeyPress(Sender: TObject; var Key: Char);
var
  VT: TVirtualStringTree;
begin
  // Space/click on checkbox column
  VT := Sender as TVirtualStringTree;
  if (Ord(Key) = VK_SPACE) and (VT.FocusedColumn in [4, 5]) then
    vtHandleClickOrKeyPress(VT, VT.FocusedNode, VT.FocusedColumn, []);
end;


procedure TfrmTableEditor.vtHandleClickOrKeyPress(Sender: TVirtualStringTree;
  Node: PVirtualNode; Column: TColumnIndex; HitPositions: THitPositions);
var
  Col: PTableColumn;
  VT: TVirtualStringTree;
begin
  if (not Assigned(Node)) or (Column = NoColumn) then
    Exit;
  VT := Sender as TVirtualStringTree;
  // For checkboxes, cell editors are disabled, instead toggle their state
  if CellEditingAllowed(Node, Column) then begin
    Col := VT.GetNodeData(Node);
    case Column of
      4: begin
        Col.Unsigned := not Col.Unsigned;
        Col.Status := esModified;
        VT.InvalidateNode(Node);
      end;
      5: begin
        Col.AllowNull := not Col.AllowNull;
        // Switch default value from NULL to Text if Allow Null is off
        if (not Col.AllowNull) and (Col.DefaultType in [cdtNull, cdtNullUpdateTS]) then begin
          Col.DefaultType := cdtNothing;
          Col.DefaultText := '';
        end;
        Col.Status := esModified;
        VT.InvalidateNode(Node);
      end;
      else begin
        // All other cells go into edit mode please
        // Explicitely done on OnClick, not in OnFocusChanged which seemed annoying for keyboard users
        if hiOnItemLabel in HitPositions then
          VT.EditNode(Node, Column);
      end;
    end;
  end;
end;

procedure TfrmTableEditor.listColumnsCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
  DefaultEditor: TColumnDefaultEditorLink;
  DatatypeEditor: TDatatypeEditorLink;
  Col: PTableColumn;
begin
  // Start cell editor
  VT := Sender as TVirtualStringTree;
  Col := Sender.GetNodeData(Node);
  case Column of
    2: begin // Datatype pulldown
      DatatypeEditor := TDatatypeEditorLink.Create(VT);
      DatatypeEditor.Datatype := Col.DataType.Index;
      EditLink := DataTypeEditor;
      end;
    8: begin // Collation pulldown
      EnumEditor := TEnumEditorLink.Create(VT);
      EnumEditor.ValueList := TStringList.Create;
      EnumEditor.ValueList.Text := Mainform.Connection.CollationList.Text;
      EnumEditor.ValueList.Insert(0, '');
      EditLink := EnumEditor;
      end;
    6: begin
      DefaultEditor := TColumnDefaultEditorLink.Create(VT);
      DefaultEditor.DefaultText := Col.DefaultText;
      DefaultEditor.DefaultType := Col.DefaultType;
      DefaultEditor.DataType := Col.DataType.Index;
      EditLink := DefaultEditor;
    end
    else
      EditLink := TInplaceEditorLink.Create(VT);
  end;
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
  ShouldBe := CleanupNumber(ed.Text);
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


procedure TfrmTableEditor.btnAddIndexClick(Sender: TObject);
var
  TblKey: TTableKey;
begin
  // Add new index
  TblKey := TTableKey.Create;
  TblKey.Name := 'Index '+IntToStr(FKeys.Count+1);
  TblKey.OldName := TblKey.Name;
  TblKey.IndexType := KEY;
  TblKey.Added := True;
  FKeys.Add(TblKey);
  Modification(Sender);
  treeIndexes.Invalidate;
  SelectNode(treeIndexes, FKeys.Count-1);
end;


procedure TfrmTableEditor.menuAddIndexColumnClick(Sender: TObject);
var
  Node: PVirtualNode;
  i, j: Integer;
  NewCol, PartLength: String;
  ColExists: Boolean;
  Column: TTableColumn;
  TblKey: TTableKey;
begin
  // Add column to index
  Node := treeIndexes.FocusedNode;
  if not Assigned(Node) then
    Exit;
  if treeIndexes.GetNodeLevel(Node) = 1 then
    Node := Node.Parent;
  TblKey := FKeys[Node.Index];
  // Find the first unused column for that index as default
  ColExists := False;
  NewCol := '';
  PartLength := '';
  for i:=0 to FColumns.Count-1 do begin
    Column := FColumns[i];
    if Column.Status = esDeleted then
      Continue;
    for j:=0 to TblKey.Columns.Count - 1 do begin
      ColExists := TblKey.Columns[j] = Column.Name;
      if ColExists then
        break;
    end;
    if not ColExists then begin
      NewCol := Column.Name;
      if (TblKey.IndexType <> FKEY) and (Column.DataType.Index in [dtTinyText, dtText, dtMediumText, dtLongText, dtTinyBlob, dtBlob, dtMediumBlob, dtLongBlob]) then
        PartLength := '100';
      break;
    end;
  end;
  treeIndexes.AddChild(Node);
  TblKey.Columns.Add(NewCol);
  TblKey.SubParts.Add(PartLength);
  Modification(Sender);
  treeIndexes.Invalidate;
  SelectNode(treeIndexes, FKeys.Count-1, Node);
end;


procedure TfrmTableEditor.btnRemoveIndexClick(Sender: TObject);
var
  idx: Integer;
  NewSelectNode: PVirtualNode;
begin
  // Remove index or part
  case treeIndexes.GetNodeLevel(treeIndexes.FocusedNode) of
    0: begin
      idx := treeIndexes.FocusedNode.Index;
      if not FKeys[idx].Added then
        DeletedKeys.Add(FKeys[idx].OldName);
      FKeys.Delete(idx);
    end;
    1: begin
      idx := treeIndexes.FocusedNode.Parent.Index;
      FKeys[idx].Columns.Delete(treeIndexes.FocusedNode.Index);
      FKeys[idx].SubParts.Delete(treeIndexes.FocusedNode.Index);
      treeIndexes.DeleteNode(treeIndexes.FocusedNode);
    end;
  end;
  Modification(Sender);
  treeIndexes.Invalidate;
  NewSelectNode := treeIndexes.GetPreviousVisible(treeIndexes.FocusedNode);
  if Assigned(NewSelectNode) then
    SelectNode(treeIndexes, NewSelectNode.Index, NewSelectNode.Parent);
end;


procedure TfrmTableEditor.btnClearIndexesClick(Sender: TObject);
var
  i: Integer;
  TblKey: TTableKey;
begin
  // Clear all indexes
  // Column data gets freed below - end any editor which could cause AV's
  if treeIndexes.IsEditing then
    treeIndexes.CancelEditNode;
  for i:=0 to FKeys.Count-1 do begin
    TblKey := TTableKey(FKeys[i]);
    if not TblKey.Added then
      DeletedKeys.Add(TblKey.OldName);
  end;
  FKeys.Clear;
  Modification(Sender);
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
      ImageIndex := GetIndexIcon(Node.Index);
    end;
    1: ImageIndex := 42;
  end;
end;


procedure TfrmTableEditor.treeIndexesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  TblKey: TTableKey;
begin
  // Index tree showing cell text
  case Sender.GetNodeLevel(Node) of
    0: begin
      TblKey := FKeys[Node.Index];
      case Column of
        0: if TblKey.IndexType = PKEY then
             CellText := TblKey.IndexType + ' KEY' // Fixed name "PRIMARY KEY", cannot be changed
           else
             CellText := TblKey.Name;
        1: CellText := TblKey.IndexType;
        2: CellText := TblKey.Algorithm;
      end;
    end;
    1: begin
      TblKey := FKeys[Node.Parent.Index];
      case Column of
        0: CellText := TblKey.Columns[Node.Index];
        1: CellText := TblKey.SubParts[Node.Index];
        2: CellText := '';
      end;
    end;
  end;
end;


procedure TfrmTableEditor.treeIndexesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  // Tell number of columns contained in index
  ChildCount := FKeys[Node.Index].Columns.Count;
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
  if Assigned(Click.HitNode) and (Click.HitColumn > NoColumn) and (hiOnItemLabel in Click.HitPositions) then
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
  btnClearIndexes.Enabled := FKeys.Count > 0;
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
  IndexedColName: String;
  i: Integer;
begin
  VT := Sender as TVirtualStringtree;
  Allowed := False;
  if VT.GetNodeLevel(Node) = 0 then begin
    // Disallow renaming primary key
    if (Column <> 0) or (VT.Text[Node, 1] <> PKEY) then
      Allowed := True
  end else begin
    // Column length is allowed for (var)char/text types only, even mandantory for text and blobs
    IndexedColName := VT.Text[Node, 0];
    for i:=0 to FColumns.Count-1 do begin
      if FColumns[i].Name = IndexedColName then begin
        Allowed := FColumns[i].DataType.Category = dtcText;
        break;
      end;
    end;
  end;
end;


procedure TfrmTableEditor.treeIndexesCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
  Level: Cardinal;
  ColNode: PVirtualNode;
  Col: PTableColumn;
begin
  // Start cell editor
  VT := Sender as TVirtualStringTree;
  Level := (Sender as TVirtualStringtree).GetNodeLevel(Node);
  if (Level = 0) and (Column = 1) then begin
    // Index type pulldown
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.ValueList := TStringList.Create;
    EnumEditor.ValueList.CommaText := PKEY +','+ KEY +','+ UKEY +','+ FKEY +','+ SKEY;
    EditLink := EnumEditor;
  end else if (Level = 0) and (Column = 2) then begin
    // Algorithm pulldown
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.ValueList := Explode(',', ',BTREE,HASH,RTREE');
    EditLink := EnumEditor;
  end else if (Level = 1) and (Column = 0) then begin
    // Column names pulldown
    EnumEditor := TEnumEditorLink.Create(VT);
    ColNode := listColumns.GetFirst;
    while Assigned(ColNode) do begin
      Col := listColumns.GetNodeData(ColNode);
      EnumEditor.ValueList.Add(Col.Name);
      ColNode := listColumns.GetNext(ColNode);
    end;
    EnumEditor.AllowCustomText := True; // Allows adding a subpart in index parts: "TextCol(20)"
    EditLink := EnumEditor;
  end else
    EditLink := TInplaceEditorLink.Create(VT);
end;


procedure TfrmTableEditor.treeIndexesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  VT: TVirtualStringtree;
  TblKey: TTableKey;
  rx: TRegExpr;
begin
  // Rename index of column
  VT := Sender as TVirtualStringtree;
  case VT.GetNodeLevel(Node) of
    0: begin
       TblKey := FKeys[Node.Index];
       case Column of
         0: TblKey.Name := NewText;
         1: TblKey.IndexType := NewText;
         2: TblKey.Algorithm := NewText;
       end;
       // Needs to be called manually for Name and IndexType properties:
       TblKey.Modification(Sender);
    end;
    1: begin
       TblKey := FKeys[Node.Parent.Index];
       case Column of
         0: begin
           // Detect input of "col(123)" and move "123" into subpart
           rx := TRegExpr.Create;
           rx.Expression := '.+\((\d+)\)';
           if rx.Exec(NewText) then begin
             TblKey.Columns[Node.Index] := Copy(NewText, 1, Length(NewText)-rx.MatchLen[1]-2);
             TblKey.Subparts[Node.Index] := rx.Match[1];
           end else
             TblKey.Columns[Node.Index] := NewText;
         end;
         1: TblKey.SubParts[Node.Index] := NewText;
       end;
    end;
  end;

  Modification(Sender);
  VT.RepaintNode(Node);
end;


procedure TfrmTableEditor.treeIndexesBeforePaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
begin
  // (Re)paint index list
  (Sender as TVirtualStringTree).RootNodeCount := FKeys.Count;
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
  ColName, PartLength: String;
  ColPos: Integer;
  VT, SourceVT: TVirtualStringtree;
  Col: PTableColumn;
  TblKey: TTableKey;
begin
  // Column node dropped here
  VT := Sender as TVirtualStringtree;
  SourceVT := Source as TVirtualStringtree;
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
    ColPos := Node.ChildCount;

  if Source = Sender then
    MoveFocusedIndexPart(ColPos)
  else begin
    TblKey := FKeys[Node.Index];
    Col := SourceVT.GetNodeData(SourceVT.FocusedNode);
    ColName := Col.Name;
    if TblKey.Columns.IndexOf(ColName) > -1 then begin
      if MessageDlg('Index "'+VT.Text[Node, 0]+'" already contains the column "'+ColName+'". It is possible to add a column twice into a index, but total nonsense in practice.'+CRLF+CRLF+'Add anyway?',
        mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;
    end;

    TblKey.Columns.Insert(ColPos, ColName);
    PartLength := '';
    if (TblKey.IndexType <> FKEY) and (Col.DataType.Index in [dtTinyText, dtText, dtMediumText, dtLongText, dtTinyBlob, dtBlob, dtMediumBlob, dtLongBlob]) then
      PartLength := '100';
    TblKey.Subparts.Insert(ColPos, PartLength);
    Node.States := Node.States + [vsHasChildren, vsExpanded];
  end;
  Modification(Sender);
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
  TblKey: TTableKey;
begin
  // Move focused index or index part
  if treeIndexes.IsEditing then
    treeIndexes.EndEditNode;
  TblKey := FKeys[treeIndexes.FocusedNode.Parent.Index];
  TblKey.Columns.Move(treeIndexes.FocusedNode.Index, NewIdx);
  TblKey.SubParts.Move(treeIndexes.FocusedNode.Index, NewIdx);
  Modification(treeIndexes);
  SelectNode(treeIndexes, NewIdx, treeIndexes.FocusedNode.Parent);
end;


procedure TfrmTableEditor.PageControlMainChange(Sender: TObject);
var
  SupportsForeignKeys: Boolean;
begin
  if treeIndexes.IsEditing then
    treeIndexes.EndEditNode;
  if PageControlMain.ActivePage = tabForeignKeys then begin
    SupportsForeignKeys := LowerCase(comboEngine.Text) = 'innodb';
    ListForeignKeys.Enabled := SupportsForeignKeys;
    tlbForeignKeys.Enabled := SupportsForeignKeys;
    pnlNoForeignKeys.Caption := 'The selected table engine ('+comboEngine.Text+') does not support foreign keys.';
    if SupportsForeignKeys then
      ListForeignKeys.Margins.Bottom := 0
    else
      ListForeignKeys.Margins.Bottom := GetTextHeight(pnlNoForeignKeys.Font)+4;
    ListForeignKeys.Repaint;
  end;
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
  chkCharsetConvert.Enabled := (FEditObjectName <> '') and (comboCollation.ItemIndex > -1);
  listColumns.Repaint;
  Modification(Sender);
end;


procedure TfrmTableEditor.popupColumnsPopup(Sender: TObject);
  function AddItem(Parent: TMenuItem; Caption: String; ImageIndex: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Parent.GetParentMenu);
    Result.Caption := Caption;
    Result.ImageIndex := ImageIndex;
    Result.OnClick := AddIndexByColumn;
    Parent.Add(Result);
  end;
var
  i: Integer;
  Item: TMenuItem;
  PrimaryKeyExists,
  ColumnsSelected: Boolean;
  IndexName: String;
  Node: PVirtualNode;
  Col: PTableColumn;
begin
  ColumnsSelected := ListColumns.SelectedCount > 0;
  menuAddToIndex.Clear;
  menuCreateIndex.Clear;
  menuAddToIndex.Enabled := ColumnsSelected;
  menuCreateIndex.Enabled := ColumnsSelected;
  if not ColumnsSelected then
    Exit;

  // Auto create submenu items for "Add to index" ...
  PrimaryKeyExists := False;
  for i:=0 to FKeys.Count-1 do begin
    if FKeys[i].IndexType = PKEY then begin
      PrimaryKeyExists := True;
      IndexName := FKeys[i].Name;
    end else
      IndexName := FKeys[i].Name + ' ('+FKeys[i].IndexType+')';
    Item := AddItem(menuAddToIndex, IndexName, GetIndexIcon(i));
    // Disable menuitem if all selected columns are already part of this index,
    // enable it if one or more selected columns are not.
    Item.Enabled := False;
    Node := listColumns.GetFirstSelected;
    while Assigned(Node) do begin
      Col := listColumns.GetNodeData(Node);
      if FKeys[i].Columns.IndexOf(Col.Name) = -1 then begin
        Item.Enabled := True;
        Break;
      end;
      Node := listColumns.GetNextSelected(Node);
    end;
  end;
  menuAddToIndex.Enabled := menuAddToIndex.Count > 0;

  // ... and for item "Create index"
  Item := AddItem(menuCreateIndex, PKEY, ICONINDEX_PRIMARYKEY);
  Item.Enabled := not PrimaryKeyExists;
  AddItem(menuCreateIndex, KEY, ICONINDEX_INDEXKEY);
  AddItem(menuCreateIndex, UKEY, ICONINDEX_UNIQUEKEY);
  AddItem(menuCreateIndex, FKEY, ICONINDEX_FULLTEXTKEY);
  AddItem(menuCreateIndex, SKEY, ICONINDEX_SPATIALKEY);
end;


procedure TfrmTableEditor.AddIndexByColumn(Sender: TObject);
var
  Item: TMenuItem;
  i: Integer;
  NewType: String;
  NewParts: TStringList;
  TblKey: TTableKey;
begin
  // Auto create index or add columns to existing one by rightclicking a column
  Item := (Sender as TMenuItem);
  NewParts := GetVTCaptions(listColumns, True, 1);
  if Item.Parent = menuCreateIndex then begin
    // Remove auto hotkeys
    NewType := StringReplace(Item.Caption, '&', '', [rfReplaceAll]);
    // Avoid creating a second key with the same columns
    for i:=0 to FKeys.Count-1 do begin
      TblKey := FKeys[i];
      if (TblKey.IndexType = NewType) and (TblKey.Columns.Text = NewParts.Text) then begin
        if MessageDlg('Key already exists. Really create another identical one?'+CRLF+CRLF+
          'This will increase disk usage and probably slow down queries on this table.',
          mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit;
        break;
      end;
    end;
    TblKey := TTableKey.Create;
    TblKey.Name := ImplodeStr('_', NewParts);
    TblKey.IndexType := NewType;
    TblKey.Added := True;
    TblKey.Columns := NewParts;
    for i:=0 to TblKey.Columns.Count do
      TblKey.SubParts.Add('');
    FKeys.Add(TblKey);
    PageControlMain.ActivePage := tabIndexes;
    treeIndexes.Repaint;
    SelectNode(treeIndexes, FKeys.Count-1);
    SelectNode(treeIndexes, 0, treeIndexes.FocusedNode);
  end else begin
    PageControlMain.ActivePage := tabIndexes;
    TblKey := FKeys[Item.MenuIndex];
    for i:=0 to NewParts.Count-1 do begin
      if TblKey.Columns.IndexOf(NewParts[i]) = -1 then begin
        TblKey.Columns.Add(NewParts[i]);
        TblKey.Subparts.Add('');
      end;
    end;
    SelectNode(treeIndexes, Item.MenuIndex);
    treeIndexes.ReinitChildren(treeIndexes.FocusedNode, False);
    SelectNode(treeIndexes, TblKey.Columns.Count-1, treeIndexes.FocusedNode);
  end;
  Modification(Sender);
end;


function TfrmTableEditor.GetIndexIcon(idx: Integer): Integer;
begin
  // Detect key icon index for specified index
  if FKeys[idx].IndexType = PKEY then Result := ICONINDEX_PRIMARYKEY
  else if FKeys[idx].IndexType = KEY then Result := ICONINDEX_INDEXKEY
  else if FKeys[idx].IndexType = UKEY then Result := ICONINDEX_UNIQUEKEY
  else if FKeys[idx].IndexType = FKEY then Result := ICONINDEX_FULLTEXTKEY
  else if FKeys[idx].IndexType = SKEY then Result := ICONINDEX_SPATIALKEY
  else Result := -1;
end;


procedure TfrmTableEditor.btnAddForeignKeyClick(Sender: TObject);
var
  Key: TForeignKey;
  idx: Integer;
begin
  // Add new foreign key
  Key := TForeignKey.Create;
  idx := FForeignKeys.Add(Key);
  Key.KeyName := 'FK'+IntToStr(idx+1);
  Key.OnUpdate := '';
  Key.OnDelete := '';
  Key.Added := True;
  Modification(Sender);
  listForeignKeys.Repaint;
  SelectNode(listForeignKeys, idx);
  listForeignKeys.EditNode(listForeignKeys.FocusedNode, listForeignKeys.Header.MainColumn);
end;


procedure TfrmTableEditor.btnRemoveForeignKeyClick(Sender: TObject);
var
  Key: TForeignKey;
begin
  // Remove a foreign key
  Key := FForeignKeys[listForeignKeys.FocusedNode.Index];
  if not Key.Added then
    DeletedForeignKeys.Add(Key.KeyName);
  FForeignKeys.Delete(listForeignKeys.FocusedNode.Index);
  Modification(Sender);
  listForeignKeys.Repaint;
end;


procedure TfrmTableEditor.btnClearForeignKeysClick(Sender: TObject);
var
  i: Integer;
begin
  // Clear all foreign keys
  for i:=FForeignKeys.Count-1 downto 0 do begin
    if not FForeignKeys[i].Added then
      DeletedForeignKeys.Add(FForeignKeys[i].KeyName);
    FForeignKeys.Delete(i);
  end;
  Modification(Sender);
  listForeignKeys.Repaint;
end;


procedure TfrmTableEditor.listForeignKeysBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  VT: TVirtualStringTree;
begin
  // Set RootNodeCount
  VT := Sender as TVirtualStringTree;
  VT.RootNodeCount := FForeignKeys.Count;
  btnClearForeignKeys.Enabled := VT.RootNodeCount > 0;
end;


procedure TfrmTableEditor.listForeignKeysEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  Key: TForeignKey;
begin
  // Disallow editing foreign columns when no reference table was selected.
  // Also, check for existance of reference table and warn if it's missing.
  if Column = 3 then begin
    Key := FForeignKeys[Node.Index];
    Allowed := False;
    if Key.ReferenceTable = '' then
      MessageDlg('Please select a reference table before selecting foreign columns.', mtError, [mbOk], 0)
    else begin
      try
        Mainform.Connection.GetVar('SELECT 1 FROM '+Mainform.MaskMulti(Key.ReferenceTable));
        Allowed := True;
      except
        // Leave Allowed = False
        MessageDlg('Reference table "'+Key.ReferenceTable+'" seems to be missing, broken or non-accessible.', mtError, [mbOk], 0)
      end;
    end;
  end else
    Allowed := True;
end;


procedure TfrmTableEditor.listForeignKeysCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
  SetEditor: TSetEditorLink;
  DBObjects: TDBObjectList;
  Key: TForeignKey;
  ColNode: PVirtualNode;
  Col: PTableColumn;
  i: Integer;
begin
  // Init grid editor in foreign key list
  VT := Sender as TVirtualStringTree;
  case Column of
    0: EditLink := TInplaceEditorLink.Create(VT);
    1: begin
        SetEditor := TSetEditorLink.Create(VT);
        ColNode := listColumns.GetFirst;
        while Assigned(ColNode) do begin
          Col := listColumns.GetNodeData(ColNode);
          SetEditor.ValueList.Add(Col.Name);
          ColNode := listColumns.GetNextSibling(ColNode);
        end;
        EditLink := SetEditor;
      end;
    2: begin
        EnumEditor := TEnumEditorLink.Create(VT);
        EnumEditor.AllowCustomText := True;
        DBObjects := Mainform.Connection.GetDBObjects(Mainform.ActiveDatabase);
        for i:=0 to DBObjects.Count-1 do
          EnumEditor.ValueList.Add(DBObjects[i].Name);
        EditLink := EnumEditor;
      end;
    3: begin
        Key := FForeignKeys[Node.Index];
        SetEditor := TSetEditorLink.Create(VT);
        SetEditor.ValueList := Mainform.Connection.GetCol('SHOW COLUMNS FROM '+Mainform.MaskMulti(Key.ReferenceTable));
        EditLink := SetEditor;
      end;
    4, 5: begin
        EnumEditor := TEnumEditorLink.Create(VT);
        EnumEditor.ValueList.Text := 'RESTRICT'+CRLF+'CASCADE'+CRLF+'SET NULL'+CRLF+'NO ACTION';
        EditLink := EnumEditor;
      end;
  end;
end;


procedure TfrmTableEditor.listForeignKeysFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  // Focus on foreign key list changed
  btnRemoveForeignKey.Enabled := Assigned(Sender.FocusedNode);
end;


procedure TfrmTableEditor.listForeignKeysGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  // Return image index for node cell in foreign key list
  case Column of
    0: ImageIndex := 136;
    else ImageIndex := -1;
  end;
end;


procedure TfrmTableEditor.listForeignKeysGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Key: TForeignKey;
begin
  // Return cell text in foreign key list
  Key := FForeignKeys[Node.Index];
  case Column of
    0: CellText := Key.KeyName;
    1: CellText := ImplodeStr(',', Key.Columns);
    2: CellText := Key.ReferenceTable;
    3: CellText := ImplodeStr(',', Key.ForeignColumns);
    4: begin
        CellText := Key.OnUpdate;
        // Both ON UPDATE + DELETE default to "RESTRICT", see http://dev.mysql.com/doc/refman/5.1/en/innodb-foreign-key-constraints.html
        if CellText = '' then
          CellText := 'RESTRICT';
      end;
    5: begin
        CellText := Key.OnDelete;
        if CellText = '' then
          CellText := 'RESTRICT';
      end;
  end;
end;


procedure TfrmTableEditor.listForeignKeysNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  Key: TForeignKey;
begin
  // Cell text in foreign key list edited
  Key := FForeignKeys[Node.Index];
  Key.Modified := True;
  Modification(Sender);
  case Column of
    0: Key.KeyName := NewText;
    1: Key.Columns := Explode(',', NewText);
    2: Key.ReferenceTable := NewText;
    3: Key.ForeignColumns := Explode(',', NewText);
    4: Key.OnUpdate := NewText;
    5: Key.OnDelete := NewText;
  end;
end;


procedure TfrmTableEditor.btnHelpClick(Sender: TObject);
begin
  // Help button
  Mainform.CallSQLHelpWithKeyword('CREATE TABLE');
end;


procedure TfrmTableEditor.popupSQLmemoPopup(Sender: TObject);
begin
  // Ensure SynMemo's have focus, otherwise Select-All and Copy actions may fail
  if PageControlMain.ActivePage = tabCREATEcode then
    SynMemoCreateCode.SetFocus
  else if PageControlMain.ActivePage = tabALTERcode then
    SynMemoAlterCode.SetFocus;
end;


end.
