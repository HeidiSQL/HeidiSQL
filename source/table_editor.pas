unit table_editor;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ToolWin, VirtualTrees, VirtualTrees.Types, SynRegExpr, Winapi.ActiveX, Vcl.ExtCtrls, SynEdit,
  SynMemo, Vcl.Menus, Vcl.Clipbrd, System.Math, System.UITypes, System.Generics.Collections,
  grideditlinks, dbstructures, dbstructures.mysql, dbconnection, apphelpers, gnugettext, System.StrUtils, extra_controls,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL;

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
    popupProperties: TPopupMenu;
    menuAddProperty: TMenuItem;
    menuAddIndexColumn: TMenuItem;
    menuRemoveProperty: TMenuItem;
    menuMoveUpIndex: TMenuItem;
    menuMoveDownIndex: TMenuItem;
    menuClearProperties: TMenuItem;
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
    listForeignKeys: TVirtualStringTree;
    menuCopyColumns: TMenuItem;
    menuPasteColumns: TMenuItem;
    tabPartitions: TTabSheet;
    SynMemoPartitions: TSynMemo;
    tabCheckConstraints: TTabSheet;
    tlbCheckConstraints: TToolBar;
    btnAddCheckConstraint: TToolButton;
    btnRemoveCheckConstraint: TToolButton;
    btnClearCheckConstraints: TToolButton;
    listCheckConstraints: TVirtualStringTree;
    Copy1: TMenuItem;
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
    procedure listColumnsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: TVTDragDataObject; Formats: TFormatArray;
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
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
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
      DataObject: TVTDragDataObject; Formats: TFormatArray; Shift: TShiftState;
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
    procedure AnyTreeClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure popupColumnsPopup(Sender: TObject);
    procedure AddIndexByColumn(Sender: TObject);
    procedure listForeignKeysBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure listForeignKeysCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure listForeignKeysFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure listForeignKeysGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
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
    procedure listColumnsKeyPress(Sender: TObject; var Key: Char);
    procedure vtHandleClickOrKeyPress(Sender: TVirtualStringTree;
      Node: PVirtualNode; Column: TColumnIndex; HitPositions: THitPositions);
    procedure menuCopyColumnsClick(Sender: TObject);
    procedure menuPasteColumnsClick(Sender: TObject);
    procedure listColumnsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure AnyTreeStructureChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Reason: TChangeReason);
    procedure listCheckConstraintsBeforePaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure btnAddCheckConstraintClick(Sender: TObject);
    procedure listCheckConstraintsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure listCheckConstraintsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure listCheckConstraintsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure listCheckConstraintsCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure listCheckConstraintsNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure btnClearCheckConstraintsClick(Sender: TObject);
    procedure btnRemoveCheckConstraintClick(Sender: TObject);
    procedure popupPropertiesPopup(Sender: TObject);
    procedure menuRemovePropertyClick(Sender: TObject);
    procedure menuClearPropertiesClick(Sender: TObject);
    procedure menuAddPropertyClick(Sender: TObject);
  private
    { Private declarations }
    FLoaded: Boolean;
    CreateCodeValid, AlterCodeValid: Boolean;
    FColumns: TTableColumnList;
    FKeys, FDeletedKeys: TTableKeyList;
    FForeignKeys: TForeignKeyList;
    FCheckConstraints: TCheckConstraintList;
    FDeletedForeignKeys,
    FDeletedCheckConstraints: TStringList;
    FAlterRestrictedMessageDisplayed: Boolean;
    const ColNumCounter = 0;
    const ColNumName = 1;
    const ColNumDatatype = 2;
    const ColNumLengthSet = 3;
    const ColNumUnsigned = 4;
    const ColNumAllownull = 5;
    const ColNumZerofill = 6;
    const ColNumDefault = 7;
    const ColNumComment = 8;
    const ColNumCollation = 9;
    const ColNumExpression = 10;
    const ColNumVirtuality = 11;
    const ColNumSrid = 12;
    const ColNumInvisible = 13;
    const ColNumsCheckboxes = [ColNumUnsigned, ColNumAllownull, ColNumZerofill, ColNumInvisible];
    procedure ValidateColumnControls;
    procedure ValidateIndexControls;
    procedure MoveFocusedIndexPart(NewIdx: Cardinal);
    procedure ResetModificationFlags;
    function ComposeCreateStatement: TSQLBatch;
    function ComposeAlterStatement: TSQLBatch;
    procedure UpdateSQLcode;
    function CellEditingAllowed(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    function GetKeyImageIndexes(Col: TTableColumn): TList<Integer>;
    procedure CalcMinColWidth;
    procedure UpdateTabCaptions;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Init(Obj: TDBObject); override;
    function DeInit: TModalResult; override;
    function ApplyModifications: TModalResult; override;
  end;


implementation

uses main;


{$R *.dfm}


constructor TfrmTableEditor.Create(AOwner: TComponent);
begin
  inherited;
  comboRowFormat.Items.CommaText := 'DEFAULT,DYNAMIC,FIXED,COMPRESSED,REDUNDANT,COMPACT';
  comboInsertMethod.Items.CommaText := 'NO,FIRST,LAST';
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
  FDeletedKeys := TTableKeyList.Create;
  FDeletedForeignKeys := TStringList.Create;
  FDeletedCheckConstraints := TStringList.Create;
  FDeletedCheckConstraints.Duplicates := dupIgnore;
  editName.MaxLength := NAME_LEN;
  FAlterRestrictedMessageDisplayed := False;
  btnSave.Hint := ShortCutToText(MainForm.actSaveSQL.ShortCut);
end;


procedure TfrmTableEditor.Init(Obj: TDBObject);
var
  AttrName, AttrValue: String;
  rx: TRegExpr;
begin
  inherited;
  FLoaded := False;

  FixVT(listColumns);
  FixVT(treeIndexes);
  FixVT(listForeignKeys);
  FixVT(listCheckConstraints);
  // Try the best to auto fit various column widths, respecting a custom DPI setting and a pulldown arrow
  listColumns.Header.Columns[2].Width := Mainform.Canvas.TextWidth('GEOMETRYCOLLECTION') + 6*listColumns.TextMargin;
  listColumns.Header.Columns[7].Width := Mainform.Canvas.TextWidth('AUTO_INCREMENT') + 4*listColumns.TextMargin;
  listColumns.Header.Columns[9].Width := Mainform.Canvas.TextWidth('macroman_general_ci') + 6*listColumns.TextMargin;
  // Overide column widths by custom values
  TExtForm.RestoreListSetup(listColumns);
  TExtForm.RestoreListSetup(treeIndexes);
  TExtForm.RestoreListSetup(listForeignKeys);
  TExtForm.RestoreListSetup(listCheckConstraints);
  // Fix control width and position, broken when opening a second table. See issue #1959
  comboCollation.Left := lblCollation.Left + TExtForm.ScaleSize(150, Self);
  comboCollation.Width := comboRowFormat.Width;
  comboCollation.Items := DBObject.Connection.CollationList;
  chkCharsetConvert.Left := comboCollation.Left + comboCollation.Width + 10;
  comboEngine.Left := comboCollation.Left;
  comboEngine.Width := comboCollation.Width;
  comboEngine.Items := DBObject.Connection.TableEngines;
  comboEngine.Items.Insert(0, '<'+_('Server default')+'>');
  comboEngine.ItemIndex := 0;
  memoUnionTables.Left := comboCollation.Left;
  memoUnionTables.Width := comboCollation.Width;
  comboInsertMethod.Left := comboCollation.Left;
  comboInsertMethod.Width := comboCollation.Width;
  if DBObject.Connection.Parameters.IsMariaDB then begin
    with listColumns.Header do begin
      Columns[10].Options := Columns[10].Options + [coVisible];
      Columns[11].Options := Columns[11].Options + [coVisible];
    end;
  end;
  listColumns.BeginUpdate;
  listColumns.Clear;
  treeIndexes.Clear;
  listForeignKeys.Clear;
  listCheckConstraints.Clear;
  tabALTERcode.TabVisible := ObjectExists;
  // Clear value editors
  memoComment.Text := '';
  if Obj.Connection.ServerVersionInt < 50503 then
    memoComment.MaxLength := 60
  else
    memoComment.MaxLength := 2048;
  editAutoInc.Text := '';
  editAvgRowLen.Text := '';
  editMaxRows.Text := '';
  chkChecksum.Checked := False;
  comboRowFormat.ItemIndex := 0;
  comboCollation.ItemIndex := -1;
  memoUnionTables.Clear;
  comboInsertMethod.ItemIndex := -1;
  SynMemoPartitions.Clear;

  if not ObjectExists then begin
    // Creating new table
    editName.Text := '';
    if DBObject.Connection.Parameters.IsAnyMySQL then
      comboCollation.ItemIndex := comboCollation.Items.IndexOf(DBObject.Connection.GetSessionVariable('collation_database'));
    PageControlMain.ActivePage := tabBasic;
    FColumns := TTableColumnList.Create;
    FKeys := TTableKeyList.Create;
    FForeignKeys := TForeignKeyList.Create;
    FCheckConstraints := TCheckConstraintList.Create;
  end else begin
    // Editing existing table
    editName.Text := DBObject.Name;
    // Try collation from SHOW TABLE STATUS, sometimes missing in SHOW CREATE TABLE result
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(DBObject.Collation);
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.Expression := '\s(\S+)\s*=\s*(\S+)';
    if rx.Exec(DBObject.CreateCode) then while true do begin
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
    if rx.Exec(DBObject.CreateCode) then
      memoUnionTables.Lines.Text := rx.Match[1]
    else
      memoUnionTables.Lines.Clear;

    // Prefer to take comment from SHOW TABLE STATUS result, to support single quotes without including some create option
    // See issue #196
    memoComment.Text := DBObject.Comment;

    rx.Expression := '\b(PARTITION\s+BY\s+.+)$';
    if rx.Exec(DBObject.CreateCode) then begin
      SynMemoPartitions.Text := Trim(rx.Match[1]);
      SynMemoPartitions.Text := ReplaceRegExpr('\*/$', SynMemoPartitions.Text, '');
    end else
      SynMemoPartitions.Clear;

    FColumns := DBObject.TableColumns;
    FKeys := DBObject.TableKeys;
    FForeignKeys := DBObject.TableForeignKeys;
    FCheckConstraints := DBObject.TableCheckConstraints;
  end;
  listColumns.RootNodeCount := FColumns.Count;
  DeInitializeVTNodes(listColumns);
  listColumns.EndUpdate;
  // Init all nodes, so they keep their FColumn data after click on Remove button, see #245
  listColumns.ValidateNode(nil, True);

  // Set root nodes per BeforePaint event:
  treeIndexes.Invalidate;
  listForeignKeys.Invalidate;
  listCheckConstraints.Invalidate;

  // Validate controls
  comboEngineSelect(comboEngine);
  ValidateColumnControls;
  ValidateIndexControls;
  ResetModificationFlags;
  CreateCodeValid := False;
  AlterCodeValid := False;
  PageControlMainChange(Self); // Foreign key editor needs a hit
  // Buttons are randomly moved, since VirtualTree update, see #440
  btnSave.Top := Height - btnSave.Height - 3;
  btnHelp.Top := btnSave.Top;
  btnDiscard.Top := btnSave.Top;
  UpdateSQLCode;
  UpdateTabCaptions;
  CalcMinColWidth;
  // Indicate change mechanisms can call their events now. See Modification().
  FLoaded := True;
  // Empty status panel
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


function TfrmTableEditor.DeInit: TModalResult;
begin
  // Store GUI setup
  TExtForm.SaveListSetup(listColumns);
  TExtForm.SaveListSetup(treeIndexes);
  TExtForm.SaveListSetup(listForeignKeys);
  TExtForm.SaveListSetup(listCheckConstraints);
  Result := inherited;
end;


procedure TfrmTableEditor.btnDiscardClick(Sender: TObject);
begin
  // Reinit GUI, discarding changes
  Modified := False;
  Init(DBObject);
end;


procedure TfrmTableEditor.btnSaveClick(Sender: TObject);
begin
  // Save changes, and make it impossible to (accidentally) click the save button twice
  btnSave.Enabled := False;
  btnSave.Repaint;
  if ApplyModifications = mrOK then begin
    // Initialize all edit fields with fresh result from SHOW TABLE STATUS row
    Init(MainForm.ActiveDbObj)
  end else begin
    // Re-enable save button when something went wrong
    btnSave.Enabled := True;
  end;
end;


function TfrmTableEditor.ApplyModifications: TModalResult;
var
  Batch: TSQLBatch;
  Query: TSQLSentence;
  i: Integer;
  Rename, ErrMessage, ErrMessageAdditional, InnodbStatus: String;
begin
  // Check if all indexes have at least one column
  // If not, exit early
  for i := 0 to FKeys.Count-1 do begin
    if FKeys.Items[i].Columns.Count = 0 then begin
      ErrorDialog( f_('%s Index "%s" does not contain any column. You can add columns using drag''n drop from the columns list.', [FKeys.Items[i].IndexType, FKeys.Items[i].Name]));
      Result := mrAbort;
      Exit;
    end;
  end;

  // Create or alter table
  Result := mrOk;

  if not ObjectExists then
    Batch := ComposeCreateStatement
  else
    Batch := ComposeAlterStatement;
  try
    for Query in Batch do begin
      DBObject.Connection.Query(Query.SQL);
      DBObject.Connection.ShowWarnings;
    end;
    // Rename table
    if ObjectExists and (editName.Text <> DBObject.Name) then begin
      Rename := DBObject.Connection.GetSQLSpecifity(spRenameTable, [DBObject.QuotedName, DBObject.Connection.QuoteIdent(editName.Text)]);
      DBObject.Connection.Query(Rename);
      DBObject.Connection.ShowWarnings;
    end;
    tabALTERcode.TabVisible := ObjectExists;
    if chkCharsetConvert.Checked then begin
      // Autoadjust column collations
      for i:=0 to FColumns.Count-1 do begin
        if FColumns[i].Collation <> '' then
          FColumns[i].Collation := comboCollation.Text;
      end;
    end;
    // Set table name for altering if Apply was clicked
    DBObject.Name := editName.Text;
    DBObject.UnloadDetails;
    tabALTERcode.TabVisible := ObjectExists;
    Mainform.UpdateEditorTab;
    MainForm.tabData.TabVisible := True;
    Mainform.RefreshTree(DBObject);
    Mainform.RefreshHelperNode(TQueryTab.HelperNodeColumns);
    ResetModificationFlags;
    AlterCodeValid := False;
    CreateCodeValid := False;
  except
    on E:EDbError do begin
      ErrMessage := E.Message;
      // Help user with a cryptic error message, by getting details from INNODB STATUS
      // See https://stackoverflow.com/questions/8434518/mysql-foreign-key-constraint-is-incorrectly-formed-error/64251639
      if DBObject.Connection.Parameters.IsAnyMySQL
        and ContainsText(ErrMessage, 'constraint is incorrectly formed') then
      begin
        InnodbStatus := DBObject.Connection.GetVar('SHOW ENGINE INNODB STATUS', 'Status');
        ErrMessageAdditional := RegExprGetMatch('\n([^\n]+ constraint failed\.[^\n]+)\n', InnodbStatus, 1, False, True);
        if not ErrMessageAdditional.IsEmpty then
          ErrMessage := ErrMessage + sLineBreak + sLineBreak + 'INNODB STATUS:' + sLineBreak + ErrMessageAdditional;
      end;
      ErrorDialog(ErrMessage);
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
    Components[i].Tag := 0;
  // Reset column changes
  for i:=FColumns.Count-1 downto 0 do begin
    if FColumns[i].Status = esDeleted then
      FColumns.Delete(i)
    else begin
      FColumns[i].OldName := FColumns[i].Name;
      FColumns[i].Status := esUntouched;
    end;
  end;
  FDeletedKeys.Clear;
  for i:=0 to FKeys.Count-1 do begin
    FKeys[i].OldName := FKeys[i].Name;
    FKeys[i].OldIndexType := FKeys[i].IndexType;
    FKeys[i].Added := False;
    FKeys[i].Modified := False;
  end;
  FDeletedForeignKeys.Clear;
  for i:=0 to FForeignKeys.Count-1 do begin
    FForeignKeys[i].OldKeyName := FForeignKeys[i].KeyName;
    FForeignKeys[i].Added := False;
    FForeignKeys[i].Modified := False;
  end;
  FDeletedCheckConstraints.Clear;
  for i:=0 to FCheckConstraints.Count-1 do begin
    FCheckConstraints[i].Added := False;
    FCheckConstraints[i].Modified := False;
  end;

  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
end;


function TfrmTableEditor.ComposeAlterStatement: TSQLBatch;
var
  Specs: TStringList;
  ColSpec, IndexSQL, SQL, OverrideCollation,
  AlterColBase, AddColBase: String;
  i: Integer;
  Results: TDBQuery;
  Col, PreviousCol: PTableColumn;
  TblKey: TTableKey;
  Constraint: TCheckConstraint;
  Node: PVirtualNode;
  Conn: TDBConnection;

  procedure FinishSpecs;
  begin
    if Specs.Count > 0 then begin
      SQL := SQL + Trim('ALTER TABLE '+DBObject.QuotedName + sLineBreak +
        CodeIndent + Implode(',' + sLineBreak + CodeIndent, Specs)) + ';' + sLineBreak;
      Specs.Clear;
    end;
  end;

  procedure AddQuery(Query: String);
  begin
    FinishSpecs;
    SQL := SQL + Format(Query, [DBObject.QuotedName]) + ';' + CRLF;
  end;
begin
  // Compose ALTER query, called by buttons and for SQL code tab
  Mainform.ShowStatusMsg(_('Composing ALTER statement ...'));
  Screen.Cursor := crHourglass;
  Specs := TStringList.Create;
  SQL := '';
  Conn := DBObject.Connection;

  // Special case for altered foreign keys: These have to be dropped in a seperate query
  // otherwise the server would return error 121 "Duplicate key on write or update"
  // See also http://dev.mysql.com/doc/refman/5.1/en/innodb-foreign-key-constraints.html :
  //   "You cannot add a foreign key and drop a foreign key in separate clauses of a single
  //   ALTER TABLE  statement. Separate statements are required."
  for i:=0 to FForeignKeys.Count-1 do begin
    if FForeignKeys[i].Modified and (not FForeignKeys[i].Added) then
      Specs.Add(Conn.GetSQLSpecifity(spForeignKeyDrop, [Conn.QuoteIdent(FForeignKeys[i].OldKeyName)]));
  end;
  FinishSpecs;

  // Special case for removed default values on columns, which can neither be done in
  // ALTER TABLE ... CHANGE COLUMN query, as there is no "no default" clause, nor by
  // appending an ALTER COLUMN ... DROP DEFAULT, without getting an "unknown column" error.
  // Also, do this after the data type was altered, if from TEXT > VARCHAR e.g.
  for i:=0 to FColumns.Count-1 do begin
    if (Conn.Parameters.IsAnyMySQL or Conn.Parameters.IsAnyPostgreSQL)
      and (FColumns[i].Status = esModified)
      and (FColumns[i].DefaultType = cdtNothing)
      and (FColumns[i].OldDataType.HasDefault)
      then
      Specs.Add('ALTER '+Conn.QuoteIdent(FColumns[i].OldName)+' DROP DEFAULT');
  end;
  FinishSpecs;

  if memoComment.Tag = MODIFIEDFLAG then begin
    case Conn.Parameters.NetTypeGroup of
      ngMySQL, ngMSSQL: begin
        Specs.Add('COMMENT=' + Conn.EscapeString(memoComment.Text));
      end;
      ngPgSQL: begin
        AddQuery('COMMENT ON TABLE '+DBObject.QuotedName+' IS '+Conn.EscapeString(memoComment.Text));
      end;
    end;
  end;
  if (comboCollation.Tag = MODIFIEDFLAG) or (chkCharsetConvert.Checked) then
    Specs.Add('COLLATE=' + Conn.EscapeString(comboCollation.Text));
  if (comboEngine.Tag = MODIFIEDFLAG) and (comboEngine.ItemIndex > 0) then begin
    if Conn.ServerVersionInt < 40018 then
      Specs.Add('TYPE=' + comboEngine.Text)
    else
      Specs.Add('ENGINE=' + comboEngine.Text);
  end;
  if comboRowFormat.Tag = MODIFIEDFLAG then
    Specs.Add('ROW_FORMAT=' + comboRowFormat.Text);
  if chkChecksum.Tag = MODIFIEDFLAG then
    Specs.Add('CHECKSUM=' + IntToStr(Integer(chkChecksum.Checked)));
  if editAutoInc.Tag = MODIFIEDFLAG then
    Specs.Add('AUTO_INCREMENT=' + IntToStr(MakeInt(editAutoInc.Text)));
  if editAvgRowLen.Tag = MODIFIEDFLAG then
    Specs.Add('AVG_ROW_LENGTH=' + IntToStr(MakeInt(editAvgRowLen.Text)));
  if editMaxRows.Tag = MODIFIEDFLAG then
    Specs.Add('MAX_ROWS=' + IntToStr(MakeInt(editMaxRows.Text)));
  if memoUnionTables.Enabled and (memoUnionTables.Tag = MODIFIEDFLAG) and (memoUnionTables.Text <> '') then
    Specs.Add('UNION=('+memoUnionTables.Text+')');
  if comboInsertMethod.Enabled and (comboInsertMethod.Tag = MODIFIEDFLAG) and (comboInsertMethod.Text <> '') then
    Specs.Add('INSERT_METHOD='+comboInsertMethod.Text);
  if chkCharsetConvert.Checked then begin
    Results := Conn.CollationTable;
    if Assigned(Results) then while not Results.Eof do begin
      if Results.Col('Collation') = comboCollation.Text then begin
        Specs.Add('CONVERT TO CHARSET '+Results.Col('Charset')+' COLLATE '+Conn.EscapeString(comboCollation.Text));
        break;
      end;
      Results.Next;
    end;
  end;

  // Update columns
  Node := listColumns.GetFirst;
  PreviousCol := nil;
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    if Col.Status <> esUntouched then begin
      OverrideCollation := IfThen(chkCharsetConvert.Checked, comboCollation.Text);
      AlterColBase := Conn.GetSQLSpecifity(spChangeColumn);
      AddColBase := Conn.GetSQLSpecifity(spAddColumn);

      case Conn.Parameters.NetTypeGroup of

        ngMySQL: begin
          ColSpec := Col.SQLCode(OverrideCollation);
          // Server version requirement, see http://dev.mysql.com/doc/refman/4.1/en/alter-table.html
          if Conn.ServerVersionInt >= 40001 then begin
            if PreviousCol = nil then
              ColSpec := ColSpec + ' FIRST'
            else
              ColSpec := ColSpec + ' AFTER '+Conn.QuoteIdent(PreviousCol.Name);
          end;
          case Col.Status of
            esModified: begin
              Specs.Add(Format(AlterColBase, [Conn.QuoteIdent(Col.OldName), ColSpec]));
            end;
            esAddedUntouched, esAddedModified: begin
              Specs.Add(Format(AddColBase, [ColSpec]));
            end;
          end;
        end;

        ngMSSQL: begin
          ColSpec := Col.SQLCode(OverrideCollation);
          case Col.Status of
            esModified: begin
              Specs.Add(Format(AlterColBase, ['', ColSpec]));
            end;
            esAddedUntouched, esAddedModified: begin
              Specs.Add(Format(AddColBase, [ColSpec]));
            end;
          end;
          AddQuery('EXECUTE sp_addextendedproperty '+Conn.EscapeString('MS_Description')+', '+
            Conn.EscapeString(Col.Comment)+', '+
            Conn.EscapeString('Schema')+', '+Conn.EscapeString(DBObject.Schema)+', '+
            Conn.EscapeString('table')+', '+Conn.EscapeString(DBObject.Name)+', '+
            Conn.EscapeString('column')+', '+Conn.EscapeString(Col.Name)
            );
        end;

        ngPgSQL: begin
          // https://www.postgresql.org/docs/current/sql-altertable.html
          // All the forms of ALTER TABLE that act on a single table, except RENAME, SET SCHEMA, ATTACH PARTITION,
          // and DETACH PARTITION can be combined into a list of multiple alterations to be applied together.
          case Col.Status of
            esModified: begin
              // Rename
              if Col.Name <> Col.OldName then begin
                FinishSpecs;
                Specs.Add(
                  Conn.GetSQLSpecifity(spRenameColumn, [Conn.QuoteIdent(Col.OldName), Conn.QuoteIdent(Col.Name)])
                  );
                FinishSpecs;
              end;
              // Type
              ColSpec := 'TYPE ' + Col.SQLCode(OverrideCollation, [cpType]);
              Specs.Add(Format(AlterColBase, [Conn.QuoteIdent(Col.Name), ColSpec]));
              // NULL allowed?
              ColSpec := IfThen(Col.AllowNull, 'DROP NOT NULL', 'SET NOT NULL');
              Specs.Add(Format(AlterColBase, [Conn.QuoteIdent(Col.Name), ColSpec]));
              // Default
              if Col.DefaultType=cdtNothing then
                ColSpec := 'DROP DEFAULT'
              else
                ColSpec := 'SET ' + Col.SQLCode(OverrideCollation, [cpDefault]);
              Specs.Add(Format(AlterColBase, [Conn.QuoteIdent(Col.Name), ColSpec]));
              // Collation
              ColSpec := Col.SQLCode(OverrideCollation, [cpCollation]);
              if not ColSpec.IsEmpty then
                Specs.Add(Format(AlterColBase, [Conn.QuoteIdent(Col.Name), ColSpec]));
            end;
            esAddedUntouched, esAddedModified: begin
              ColSpec := Col.SQLCode(OverrideCollation);
              Specs.Add(Format(AddColBase, [ColSpec]));
            end;
          end;
          AddQuery('COMMENT ON COLUMN %s.'+Conn.QuoteIdent(Col.Name)+' IS '+Conn.EscapeString(Col.Comment));
        end;

        ngSQLite: begin
          ColSpec := Col.SQLCode;
          case Col.Status of
            esModified: begin
              // Rename
              if Col.Name <> Col.OldName then begin
                Specs.Add(
                  Conn.GetSQLSpecifity(spRenameColumn, [Conn.QuoteIdent(Col.OldName), Conn.QuoteIdent(Col.Name)])
                  );
              end;
            end;
            esAddedUntouched, esAddedModified: begin
              Specs.Add(Format(AddColBase, [ColSpec]));
            end;
          end;
          FinishSpecs;
        end;

      end;

    end;
    PreviousCol := Col;
    Node := listColumns.GetNextSibling(Node);
  end;

  // Deleted columns, not available as Node in above loop
  for i:=0 to FColumns.Count-1 do begin
    if FColumns[i].Status = esDeleted then begin
      Specs.Add('DROP COLUMN '+Conn.QuoteIdent(FColumns[i].OldName));
      // MSSQL + SQLite want one ALTER TABLE query per DROP COLUMN
      if Conn.Parameters.NetTypeGroup in [ngMSSQL, ngSQLite] then
        FinishSpecs;
    end;
  end;

  // Drop indexes
  for TblKey in FDeletedKeys do begin
    if not TblKey.InsideCreateCode then
      Continue;
    if Conn.Parameters.IsAnyPostgreSQL then begin
      if TblKey.IsPrimary or TblKey.IsUnique then
        IndexSQL := 'CONSTRAINT ' + TblKey.OldName
      else // wrong:
        IndexSQL := 'INDEX ' + Conn.QuoteIdent(TblKey.OldName);
    end
    else begin
      if TblKey.IsPrimary then
        IndexSQL := 'PRIMARY KEY'
      else
        IndexSQL := 'INDEX ' + Conn.QuoteIdent(TblKey.OldName);
    end;
    Specs.Add('DROP '+IndexSQL);
  end;

  // Drop changed indexes, and add changed or added indexes
  for TblKey in FKeys do begin
    if not TblKey.InsideCreateCode then
      Continue;
    if TblKey.Modified and (not TblKey.Added) then begin
      if Conn.Parameters.IsAnyPostgreSQL then begin
        if (TblKey.OldIndexType = TTableKey.PRIMARY) or (TblKey.OldIndexType = TTableKey.UNIQUE) then
          IndexSQL := 'CONSTRAINT ' + TblKey.OldName
        else // wrong:
          IndexSQL := 'INDEX ' + Conn.QuoteIdent(TblKey.OldName);
      end
      else begin
        if TblKey.OldIndexType = TTableKey.PRIMARY then
          IndexSQL := 'PRIMARY KEY'
        else
          IndexSQL := 'INDEX ' + Conn.QuoteIdent(TblKey.OldName);
      end;
      Specs.Add('DROP '+IndexSQL);
    end;
    if TblKey.Added or TblKey.Modified then
      Specs.Add('ADD '+TblKey.SQLCode);
  end;

  for i:=0 to FDeletedForeignKeys.Count-1 do begin
    Specs.Add(Conn.GetSQLSpecifity(spForeignKeyDrop, [Conn.QuoteIdent(FDeletedForeignKeys[i])]));
  end;
  for i:=0 to FForeignKeys.Count-1 do begin
    if FForeignKeys[i].Added or FForeignKeys[i].Modified then
      Specs.Add('ADD '+FForeignKeys[i].SQLCode(True));
  end;

  // Check constraints
  for i:=0 to FDeletedCheckConstraints.Count-1 do begin
    Specs.Add('DROP CONSTRAINT ' + Conn.QuoteIdent(FDeletedCheckConstraints[i]));
  end;
  for Constraint in FCheckConstraints do begin
    if Constraint.Added or Constraint.Modified then
      Specs.Add('ADD ' + Constraint.SQLCode);
  end;


  FinishSpecs;

  // Separate queries from here on

  // Drop indexes, also changed indexes, which will be readded below
  for i:=0 to FDeletedKeys.Count-1 do begin
    if FDeletedKeys[i].InsideCreateCode then
      Continue;
    if FDeletedKeys[i].IsPrimary then
      IndexSQL := 'PRIMARY KEY'
    else
      IndexSQL := 'INDEX ' + Conn.QuoteIdent(FDeletedKeys[i].OldName);
    AddQuery('DROP '+IndexSQL);
  end;
  // Add changed or added indexes
  for i:=0 to FKeys.Count-1 do begin
    if FKeys[i].InsideCreateCode then
      Continue;
    if FKeys[i].Modified and (not FKeys[i].Added) then begin
      if FKeys[i].OldIndexType = TTableKey.PRIMARY then
        IndexSQL := 'PRIMARY KEY'
      else
        IndexSQL := 'INDEX ' + Conn.QuoteIdent(FKeys[i].OldName);
      AddQuery('DROP '+IndexSQL);
    end;
    if FKeys[i].Added or FKeys[i].Modified then
      AddQuery(FKeys[i].SQLCode(DBObject.Name));
  end;

  Result := TSQLBatch.Create;
  Result.SQL := SQL;

  FreeAndNil(Specs);
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


function TfrmTableEditor.ComposeCreateStatement: TSQLBatch;
var
  i, IndexCount: Integer;
  Col: PTableColumn;
  Constraint: TCheckConstraint;
  Node: PVirtualNode;
  tmp, SQL: String;
begin
  // Compose CREATE query, called by buttons and for SQL code tab
  SQL := 'CREATE TABLE '+DBObject.Connection.QuoteIdent(editName.Text)+' ('+CRLF;
  Node := listColumns.GetFirst;
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    SQL := SQL + CodeIndent + Col.SQLCode + ',' + sLineBreak;
    Node := listColumns.GetNextSibling(Node);
  end;

  IndexCount := 0;
  for i:=0 to FKeys.Count-1 do begin
    if not FKeys[i].InsideCreateCode then
      Continue;
    tmp := FKeys[i].SQLCode;
    if tmp <> '' then begin
      SQL := SQL + CodeIndent + tmp + ',' + sLineBreak;
      Inc(IndexCount);
    end;
  end;

  for i:=0 to FForeignKeys.Count-1 do
    SQL := SQL + CodeIndent + FForeignKeys[i].SQLCode(True) + ',' + sLineBreak;

  // Check constraints
  for Constraint in FCheckConstraints do begin
    SQL := SQL + CodeIndent + Constraint.SQLCode + ',' + sLineBreak;
  end;

  if Integer(listColumns.RootNodeCount) + IndexCount + FForeignKeys.Count > 0 then
    Delete(SQL, Length(SQL)-2, 3);

  SQL := SQL + CRLF + ')' + CRLF;
  if memoComment.Text <> '' then
    SQL := SQL + 'COMMENT='+DBObject.Connection.EscapeString(memoComment.Text) + CRLF;
  if comboCollation.Text <> '' then
    SQL := SQL + 'COLLATE='+DBObject.Connection.EscapeString(comboCollation.Text) + CRLF;
  if (comboEngine.Text <> '') and (comboEngine.ItemIndex > 0) then begin
    if DBObject.Connection.ServerVersionInt < 40018 then
      SQL := SQL + 'TYPE='+comboEngine.Text + CRLF
    else
      SQL := SQL + 'ENGINE='+comboEngine.Text + CRLF;
  end;
  if comboRowFormat.Text <> 'DEFAULT' then
    SQL := SQL + 'ROW_FORMAT='+comboRowFormat.Text + CRLF;
  if chkChecksum.Checked then
    SQL := SQL + 'CHECKSUM='+IntToStr(Integer(chkChecksum.Checked)) + CRLF;
  if editAutoInc.Text <> '' then
    SQL := SQL + 'AUTO_INCREMENT='+editAutoInc.Text + CRLF;
  if editAvgRowLen.Text <> '' then
    SQL := SQL + 'AVG_ROW_LENGTH='+editAvgRowLen.Text + CRLF;
  if editMaxRows.Text <> '' then
    SQL := SQL + 'MAX_ROWS='+editMaxRows.Text + CRLF;
  if memoUnionTables.Enabled and (memoUnionTables.Text <> '') then
    SQL := SQL + 'UNION=('+memoUnionTables.Text+')' + CRLF;
  if comboInsertMethod.Enabled and (comboInsertMethod.Text <> '') then
    SQL := SQL + 'INSERT_METHOD='+comboInsertMethod.Text + CRLF;
  if SynMemoPartitions.GetTextLen > 0 then
    SQL := SQL +  '/*!50100 ' + SynMemoPartitions.Text + ' */';
  SQL := SQL + ';' + CRLF;

  // Separate queries from here on

  if DBObject.Connection.Parameters.IsAnyPostgreSQL then begin
    Node := listColumns.GetFirst;
    while Assigned(Node) do begin
      Col := listColumns.GetNodeData(Node);
      SQL := SQL + 'COMMENT ON COLUMN '+
        DBObject.Connection.QuoteIdent(editName.Text)+'.'+DBObject.Connection.QuoteIdent(Col.Name)+
        ' IS '+DBObject.Connection.EscapeString(Col.Comment) + ';' + CRLF;
      Node := listColumns.GetNextSibling(Node);
    end;
  end;

  for i:=0 to FKeys.Count-1 do begin
    if FKeys[i].InsideCreateCode then
      Continue;
    tmp := FKeys[i].SQLCode(editName.Text);
    if tmp <> '' then begin
      SQL := SQL + tmp + ';' + sLineBreak;
    end;
  end;

  Result := TSQLBatch.Create;
  Result.SQL := Trim(SQL);
end;


procedure TfrmTableEditor.Modification(Sender: TObject);
begin
  // Memorize modified status
  if FLoaded then begin
    if Sender is TComponent then
      TComponent(Sender).Tag := MODIFIEDFLAG;
    Modified := True;
    btnSave.Enabled := Modified and (editName.Text <> '') and (listColumns.RootNodeCount > 0);
    btnDiscard.Enabled := Modified;
    CreateCodeValid := False;
    AlterCodeValid := False;
    UpdateSQLcode;
    CalcMinColWidth;
  end;
end;


procedure TfrmTableEditor.btnAddColumnClick(Sender: TObject);
var
  NewCol: TTableColumn;
  FocusedCol: PTableColumn;
  fn, NewNode: PVirtualNode;
  idx: Integer;
  DefaultType: String;
begin
  // Add new column after selected one
  if listColumns.IsEditing then
    listColumns.EndEditNode;
  fn := listColumns.FocusedNode;
  NewCol := TTableColumn.Create(DBObject.Connection);
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
    NewCol.Collation := '';
  end else begin
    idx := listColumns.RootNodeCount;
    DefaultType := 'INT';
    NewCol.DataType := DBObject.Connection.GetDatatypeByName(DefaultType, False);
    NewCol.Unsigned := False;
    NewCol.AllowNull := True;
    NewCol.DefaultType := cdtNothing;
    NewCol.DefaultText := '';
    NewCol.Comment := '';
    NewCol.Collation := '';
  end;
  NewCol.Name := _('Column')+' '+IntToStr(idx+1);
  FColumns.Insert(idx, NewCol);
  NewNode := listColumns.InsertNode(fn, amInsertAfter, @NewCol);
  NewCol.Status := esAddedUntouched;
  SelectNode(listColumns, NewNode);
  Modification(Sender);
  ValidateColumnControls;
  // not sufficient, if list has minimum height, see https://www.heidisql.com/forum.php?t=35766
  // if listColumns.ScrollIntoView(NewNode, False) then
  if listColumns.Height > listColumns.Header.Height+20 then
    listColumns.EditNode(NewNode, 1);
end;


procedure TfrmTableEditor.btnRemoveColumnClick(Sender: TObject);
var
  Node, NodeFocus: PVirtualNode;
  Col: PTableColumn;
begin
  // Remove selected column(s)
  Node := GetNextNode(listColumns, nil, True);
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    Col.Name := Col.OldName;
    Col.Status := esDeleted;
    Node := GetNextNode(listColumns, Node, True);
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
  // Delete selected, including those which are invisible through filter
  listColumns.DeleteSelectedNodes;

  if not Assigned(NodeFocus) then
    NodeFocus := listColumns.GetLast;
  if Assigned(NodeFocus) then
    SelectNode(listColumns, NodeFocus.Index);
  listColumns.Repaint; // .Invalidate does not remove nodes immediately
  Modification(Sender);
  ValidateColumnControls;
end;


procedure TfrmTableEditor.btnMoveUpColumnClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  // Move up selected columns
  listColumns.EndEditNode;

  Node := GetNextNode(listColumns, nil, true);
  while Assigned(Node) do begin
    listColumns.MoveTo(Node, listColumns.GetPreviousSibling(Node), amInsertBefore, False);
    Node := GetNextNode(listColumns, Node, true);
  end;

  ValidateColumnControls;
end;


procedure TfrmTableEditor.btnMoveDownColumnClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  // Move down selected columns
  listColumns.EndEditNode;

  Node := listColumns.GetLast;
  while Assigned(Node) do begin
    if listColumns.Selected[Node] then begin
      listColumns.MoveTo(Node, listColumns.GetNextSibling(Node), amInsertAfter, False);
    end;
    Node := listColumns.GetPrevious(Node);
  end;

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
  Source: TObject; DataObject: TVTDragDataObject; Formats: TFormatArray;
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
var
  BgColor: TColor;
begin
  BgColor := MainForm.GetAlternatingRowBackground(Node);

  // Darken cell background to signalize it doesn't allow length/set
  // Exclude non editable checkbox columns - grey looks ugly there.
  if (not CellEditingAllowed(Node, Column)) and (not (Column in ColNumsCheckboxes)) then begin
    BgColor := clBtnFace;
  end;

  // Apply color
  if BgColor <> clNone then begin
    TargetCanvas.Brush.Color := BgColor;
    TargetCanvas.FillRect(CellRect);
  end;
end;


function TfrmTableEditor.GetKeyImageIndexes(Col: TTableColumn): TList<Integer>;
var
  idx, i: Integer;
begin
  Result := TList<Integer>.Create;
  for i:=0 to FKeys.Count-1 do begin
    if FKeys[i].Columns.IndexOf(Col.Name) > -1 then begin
      idx := FKeys[i].ImageIndex;
      if not Result.Contains(idx) then
        Result.Add(idx);
    end;
  end;
  for i:=0 to FForeignKeys.Count-1 do begin
    if FForeignKeys[i].Columns.IndexOf(Col.Name) > -1 then begin
      idx := ICONINDEX_FOREIGNKEY;
      if not Result.Contains(idx) then
        Result.Add(idx);
    end;
  end;
end;


procedure TfrmTableEditor.CalcMinColWidth;
var
  i, MinWidthThisCol, MinWidthAllCols: Integer;
  ImageIndexes: TList<Integer>;
begin
  // Find maximum width for first column so both the index icons and the text have enough room
  MinWidthAllCols := 0;
  for i:=0 to FColumns.Count-1 do begin
    ImageIndexes := GetKeyImageIndexes(FColumns[i]);
    MinWidthThisCol := ImageIndexes.Count * listColumns.Images.Width;
    MinWidthAllCols := Max(MinWidthAllCols, MinWidthThisCol);
  end;
  // Add room for text and extra spacing
  Inc(MinWidthAllCols, listColumns.GetMaxColumnWidth(0));
  Inc(MinWidthAllCols, listColumns.TextMargin);
  listColumns.Header.Columns[0].Width := MinWidthAllCols;
end;


procedure TfrmTableEditor.listColumnsAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  Col: PTableColumn;
  ImageIndex, X, Y, i: Integer;
  VT: TVirtualStringTree;
  Checked: Boolean;
  ImageIndexes: TList<Integer>;
begin
  VT := TVirtualStringTree(Sender);
  Col := Sender.GetNodeData(Node);
  Y := CellRect.Top + Integer(VT.NodeHeight[Node] div 2) - (VT.Images.Height div 2);

  // Paint one icon per index type of which this column is part of
  if Column = ColNumCounter then begin
    X := 0;
    ImageIndexes := GetKeyImageIndexes(Col^);
    for i in ImageIndexes do begin
      VT.Images.Draw(TargetCanvas, X, Y, i);
      Inc(X, VT.Images.Width);
    end;
    ImageIndexes.Free;
  end;

  // Paint checkbox image in certain columns
  // while restricting "Allow NULL" checkbox to numeric datatypes
  if (Column in ColNumsCheckboxes) then begin
    Checked := (Col.Unsigned and (Column=ColNumUnsigned))
      or (Col.AllowNull and (Column=ColNumAllownull))
      or (Col.ZeroFill and (Column = ColNumZerofill))
      or (Col.Invisible and (Column = ColNumInvisible));
    if CellEditingAllowed(Node, Column) then begin
      if Checked then ImageIndex := 128
      else ImageIndex := 127;
    end else begin
      if Checked then ImageIndex := 176
      else ImageIndex := 175;
    end;
    X := CellRect.Left + (VT.Header.Columns[Column].Width div 2) - (VT.Images.Width div 2);
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
var
  NextSelected, LastSelected: PVirtualNode;
begin
  btnRemoveColumn.Enabled := listColumns.SelectedCount > 0;

  LastSelected := nil;
  NextSelected := GetNextNode(listColumns, nil, True);
  while Assigned(NextSelected) do begin
    LastSelected := NextSelected;
    NextSelected := GetNextNode(listColumns, NextSelected, True);
  end;

  btnMoveUpColumn.Enabled := (listColumns.SelectedCount > 0)
    and (listColumns.GetFirstSelected <> listColumns.GetFirst)
    and (DBObject.Connection.Parameters.NetTypeGroup = ngMySQL);
  btnMoveDownColumn.Enabled := (listColumns.SelectedCount > 0)
    and (LastSelected <> listColumns.GetLast)
    and (DBObject.Connection.Parameters.NetTypeGroup = ngMySQL);

  menuRemoveColumn.Enabled := btnRemoveColumn.Enabled;
  menuMoveUpColumn.Enabled := btnMoveUpColumn.Enabled;
  menuMoveDownColumn.Enabled := btnMoveDownColumn.Enabled;
end;


procedure TfrmTableEditor.listColumnsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  // Allow text editing? Explicitely block that in checkbox columns 
  Allowed := CellEditingAllowed(Node, Column) and (not (Column in ColNumsCheckboxes));
end;


function TfrmTableEditor.CellEditingAllowed(Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  Col: PTableColumn;
  i: Integer;
begin
  Col := listColumns.GetNodeData(Node);
  case Column of
    // No editor for very first column and checkbox columns
    ColNumCounter: Result := False;

    ColNumLengthSet: Result := Col.DataType.HasLength;

    ColNumUnsigned: begin
      Result := (Col.DataType.Category in [dtcInteger, dtcReal])
        and (Col.DataType.Index <> dbdtBit)
        and (DBObject.Connection.Parameters.IsAnyMySQL);
      if (not Result) and Col.Unsigned then begin
        Col.Unsigned := False;
        Col.Status := esModified;
      end;
    end;

    ColNumAllownull: begin
      // Do not allow NULL, and force NOT NULL, on primary key columns
      Result := True;
      for i:=0 to FKeys.Count-1 do begin
        if FKeys[i].IsPrimary and (FKeys[i].Columns.IndexOf(Col.Name) > -1) then begin
          if Col.AllowNull then begin
            Col.AllowNull := False;
            Col.Status := esModified;
          end;
          Result := False;
          break;
        end;
      end;
    end;

    ColNumZerofill: begin
      Result := (Col.DataType.Category in [dtcInteger, dtcReal])
        and (Col.DataType.Index <> dbdtBit)
        and (DBObject.Connection.Parameters.IsAnyMySQL);
      if (not Result) and Col.ZeroFill then begin
        Col.ZeroFill := False;
        Col.Status := esModified;
      end;
    end;

    // No editing of collation allowed if "Convert data" was checked
    ColNumCollation: Result := not chkCharsetConvert.Checked;

    ColNumSrid: Result := (Col.DataType.Category = dtcSpatial) and DBObject.Connection.Has(frSrid);

    ColNumInvisible: Result := DBObject.Connection.Has(frInvisibleColumns);

    else Result := True;
  end;

  // SQLite does not support altering existing columns, except renaming. See issue #1256
  if ObjectExists and DBObject.Connection.Parameters.IsAnySQLite then begin
    if Col.Status in [esUntouched, esModified, esDeleted] then begin
      Result := Result and (Column = ColNumName);
      if (not Result) and (not FAlterRestrictedMessageDisplayed) then begin
        MainForm.LogSQL(
          f_('Altering tables restricted. For details see %s', ['https://www.sqlite.org/lang_altertable.html#making_other_kinds_of_table_schema_changes']),
          lcInfo
          );
        FAlterRestrictedMessageDisplayed := True;
      end;
    end;
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
  CellText := '';
  case Column of
    ColNumCounter: CellText := IntToStr(Node.Index+1);

    ColNumName: CellText := Col.Name;

    ColNumDatatype: CellText := Col.DataType.Name;

    ColNumLengthSet: CellText := Col.LengthSet;

    ColNumUnsigned,
    ColNumAllownull,
    ColNumZerofill,
    ColNumInvisible: CellText := ''; // Checkbox

    ColNumDefault: begin
      case Col.DefaultType of
        cdtNothing:      CellText := _('No default');
        cdtText:         CellText := Col.Connection.EscapeString(Col.DefaultText);
        cdtNull:         CellText := 'NULL';
        cdtExpression:   CellText := Col.DefaultText;
        cdtAutoInc:      CellText := Col.AutoIncName;
      end;
      case Col.OnUpdateType of
        // cdtNothing: leave clause away
        // cdtText: not supported
        // cdtNull: not supported
        cdtExpression:   CellText := CellText + ' ON UPDATE ' + Col.OnUpdateText;
        // cdtAutoInc: invalid here
      end;
    end;

    ColNumComment: CellText := Col.Comment;

    ColNumCollation: begin
      CellText := Col.Collation;
      if (CellText <> '') and (chkCharsetConvert.Checked) then
        CellText := comboCollation.Text;
    end;

    ColNumExpression: CellText := Col.GenerationExpression;

    ColNumVirtuality: CellText := Col.Virtuality;

    ColNumSrid: begin
      if (Col.DataType.Category = dtcSpatial) and (Col.Connection.Has(frSrid)) then
        CellText := Col.SRID.ToString;
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
    if FKeys[i].IsPrimary and (FKeys[i].Columns.IndexOf(Col.Name) > -1) then begin
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      break;
    end;
  end;

  // No specific colors for selected nodes, would interfere with blue selection background
  // Disabled in Oct 2023, probably works better than expected
  //if vsSelected in Node.States then Exit;

  // Give datatype column specific color, as set in preferences
  TextColor := TargetCanvas.Font.Color;
  case Column of
    ColNumCounter: TextColor := clGrayText;

    ColNumDatatype: TextColor := DatatypeCategories[Col.DataType.Category].Color;

    ColNumDefault: case Col.DefaultType of
      cdtNothing, cdtNull:
        TextColor := DatatypeCategories[Col.DataType.Category].NullColor;
      else
        TextColor := DatatypeCategories[Col.DataType.Category].Color;
    end;
  end;
  TargetCanvas.Font.Color := TextColor;
end;


procedure TfrmTableEditor.listColumnsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  i: Integer;
  Col: PTableColumn;
  Key: TTableKey;
  WasModified: Boolean;
begin
  // Column property edited
  Col := Sender.GetNodeData(Node);
  WasModified := True;
  case Column of
    ColNumName: begin
      for i:=0 to FColumns.Count-1 do begin
        if (FColumns[i].Name = NewText) and (not (FColumns[i].Status in [esDeleted, esAddedDeleted])) then begin
          ErrorDialog(f_('Column "%s" already exists.', [NewText]));
          Exit;
        end;
      end;
      for Key in FKeys do begin
        for i:=0 to Key.Columns.Count-1 do begin
          if Key.Columns[i] = Col.Name then
            Key.Columns[i] := NewText;
        end;
      end;
      treeIndexes.Invalidate;
      Col.Name := NewText;
    end;

    ColNumDatatype: begin
      Col.DataType := DBObject.Connection.GetDatatypeByName(NewText, False, Col.Name);
      // Reset length/set for column types which don't support that
      if not Col.DataType.HasLength then
        Col.LengthSet := '';
      // Remove subpart from indexes where this column is a part of
      if Col.DataType.Category <> dtcText then begin
        for Key in FKeys do begin
          for i:=0 to Key.Columns.Count-1 do begin
            if Key.Columns[i] = Col.Name then
              Key.SubParts[i] := '';
          end;
        end;
        treeIndexes.Invalidate;
      end;
      // Suggest length/set if required
      if (not Col.LengthCustomized) or (Col.DataType.RequiresLength and (Col.LengthSet = '')) then
        Col.LengthSet := Col.DataType.DefLengthSet;
      // Auto-change default type and text
      if not Col.DataType.HasDefault then begin
        Col.DefaultType := cdtNothing;
        Col.DefaultText := '';
      end else begin
        // Auto-fix user selected default type which can be invalid now
        case Col.DataType.Category of
          dtcInteger: begin
            Col.DefaultType := cdtExpression;
            if Col.AllowNull then
              Col.DefaultType := cdtNull
            else
              Col.DefaultText := IntToStr(MakeInt(Col.DefaultText));
          end;
          dtcReal: begin
            Col.DefaultType := cdtExpression;
            if Col.AllowNull then
              Col.DefaultType := cdtNull
            else
              Col.DefaultText := FloatToStr(MakeFloat(Col.DefaultText));
          end;
          dtcText, dtcBinary, dtcSpatial, dtcOther: begin
            Col.DefaultType := cdtText;
            if Col.AllowNull then
              Col.DefaultType := cdtNull;
          end;
          dtcTemporal: begin
            if Col.DefaultType = cdtAutoinc then
              Col.DefaultType := cdtNothing;
          end;
        end;
      end;
    end;

    ColNumLengthSet: begin
      if Col.DataType.RequiresLength and (NewText='') then begin
        WasModified := False;
        ErrorDialog(f_('Column data type %s requires a length/set', [Col.DataType.Name]));
      end else begin
        Col.LengthSet := NewText;
        Col.LengthCustomized := True;
      end;
    end;

    // 4, 5, 6, 13 are checkboxes - handled in OnClick

    ColNumDefault: begin
      // DefaultText/Type and OnUpdateText/Type are set in TColumnDefaultEditorLink.EndEdit
      if Col.DefaultType = cdtNull then
        Col.AllowNull := True;
    end;

    ColNumComment: Col.Comment := NewText;

    ColNumCollation: Col.Collation := NewText;

    ColNumExpression: Col.GenerationExpression := NewText;

    ColNumVirtuality: Col.Virtuality := NewText;

    ColNumSrid: Col.SRID := StrToUIntDef(NewText, 0);
  end;
  if WasModified then begin
    Col.Status := esModified;
    Modification(Sender);
  end;
end;


procedure TfrmTableEditor.listColumnsNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Col: PTableColumn;
begin
  Col := Sender.GetNodeData(Node);
  Col.Status := esModified;
  Modification(Sender);
end;


procedure TfrmTableEditor.listColumnsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  // Enable/disable move buttons
  ValidateColumnControls;
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
  if (Ord(Key) = VK_SPACE) and (VT.FocusedColumn in ColNumsCheckboxes) then
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
      ColNumUnsigned: begin
        Col.Unsigned := not Col.Unsigned;
        Col.Status := esModified;
        Modification(Sender);
        VT.InvalidateNode(Node);
      end;

      ColNumAllownull: begin
        Col.AllowNull := not Col.AllowNull;
        // Switch default value from NULL to Text if Allow Null is off
        if (not Col.AllowNull) and (Col.DefaultType = cdtNull) then begin
          Col.DefaultType := cdtNothing;
          Col.DefaultText := '';
        end;
        Col.Status := esModified;
        Modification(Sender);
        VT.InvalidateNode(Node);
      end;

      ColNumZerofill: begin
        Col.ZeroFill := not Col.ZeroFill;
        Col.Status := esModified;
        Modification(Sender);
        VT.InvalidateNode(Node);
      end;

      ColNumInvisible: begin
        Col.Invisible := not Col.Invisible;
        Col.Status := esModified;
        Modification(Sender);
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
  Edit: TInplaceEditorLink;
begin
  // Start cell editor
  VT := Sender as TVirtualStringTree;
  Col := Sender.GetNodeData(Node);
  case Column of
    ColNumDatatype: begin // Datatype pulldown
      DatatypeEditor := TDatatypeEditorLink.Create(VT, True, Col^);
      EditLink := DataTypeEditor;
      end;

    ColNumCollation: begin // Collation pulldown
      EnumEditor := TEnumEditorLink.Create(VT, True, Col^);
      EnumEditor.AllowCustomText := True;
      EnumEditor.ItemMustExist := True;
      EnumEditor.ValueList := TStringList.Create;
      EnumEditor.ValueList.Text := DBObject.Connection.CollationList.Text;
      EnumEditor.ValueList.Sort;
      EnumEditor.ValueList.Insert(0, '');
      EditLink := EnumEditor;
      end;

    ColNumDefault: begin
      DefaultEditor := TColumnDefaultEditorLink.Create(VT, True, Col^);
      DefaultEditor.DefaultType := Col.DefaultType;
      DefaultEditor.DefaultText := Col.DefaultText;
      DefaultEditor.OnUpdateType := Col.OnUpdateType;
      DefaultEditor.OnUpdateText := Col.OnUpdateText;
      EditLink := DefaultEditor;
    end;

    ColNumVirtuality: begin // Virtuality pulldown
      EnumEditor := TEnumEditorLink.Create(VT, True, Col^);
      EnumEditor.ValueList := TStringList.Create;
      if DBObject.Connection.Parameters.IsMariaDB then
        EnumEditor.ValueList.CommaText := ',VIRTUAL,PERSISTENT'
      else
        EnumEditor.ValueList.CommaText := ',VIRTUAL,STORED';
      EditLink := EnumEditor;
    end

    else begin
      Edit := TInplaceEditorLink.Create(VT, True, Col^);
      Edit.TitleText := VT.Header.Columns[Column].Text;
      Edit.ButtonVisible := True;
      EditLink := Edit;
    end;
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
  IsMerge := UpperCase((Sender as TComboBox).Text) = 'MRG_MYISAM';
  memoUnionTables.Enabled := IsMerge;
  comboInsertMethod.Enabled := IsMerge;
  Modification(Sender);
end;


procedure TfrmTableEditor.btnAddIndexClick(Sender: TObject);
var
  TblKey: TTableKey;
begin
  // Add new index
  TblKey := TTableKey.Create(DBObject.Connection);
  TblKey.Name := _('Index')+' '+IntToStr(FKeys.Count+1);
  TblKey.OldName := TblKey.Name;
  TblKey.IndexType := TTableKey.KEY;
  TblKey.OldIndexType := TblKey.IndexType;
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
      if (not TblKey.IsFulltext) and (Column.DataType.Index in [dbdtTinyText, dbdtText, dbdtMediumText, dbdtLongText, dbdtTinyBlob, dbdtBlob, dbdtMediumBlob, dbdtLongBlob]) then
        PartLength := '100';
      break;
    end;
  end;
  treeIndexes.AddChild(Node);
  TblKey.Columns.Add(NewCol);
  TblKey.SubParts.Add(PartLength);
  TblKey.Collations.Add('A');
  Modification(Sender);
  treeIndexes.Invalidate;
  SelectNode(treeIndexes, FKeys.Count-1, Node);
end;


procedure TfrmTableEditor.btnRemoveIndexClick(Sender: TObject);
var
  idx: Integer;
  NewSelectNode: PVirtualNode;
  DeleteTblKey: TTableKey;
begin
  // Remove index or part
  if treeIndexes.IsEditing then
    treeIndexes.CancelEditNode;
  case treeIndexes.GetNodeLevel(treeIndexes.FocusedNode) of
    0: begin
      idx := treeIndexes.FocusedNode.Index;
      if not FKeys[idx].Added then begin
        DeleteTblKey := TTableKey.Create(DBObject.Connection);
        DeleteTblKey.Assign(FKeys[idx]);
        FDeletedKeys.Add(DeleteTblKey);
      end;
      FKeys.Delete(idx);
      // Delete node although ReinitChildren would do the same, but the Repaint before
      // creates AVs in certain cases. See issue #2557
      treeIndexes.DeleteNode(treeIndexes.FocusedNode);
    end;
    1: begin
      idx := treeIndexes.FocusedNode.Parent.Index;
      FKeys[idx].Columns.Delete(treeIndexes.FocusedNode.Index);
      FKeys[idx].SubParts.Delete(treeIndexes.FocusedNode.Index);
      FKeys[idx].Collations.Delete(treeIndexes.FocusedNode.Index);
      treeIndexes.DeleteNode(treeIndexes.FocusedNode);
    end;
  end;
  Modification(Sender);
  treeIndexes.Repaint;
  treeIndexes.ReinitChildren(nil, True);
  NewSelectNode := treeIndexes.GetPreviousVisible(treeIndexes.FocusedNode);
  if Assigned(NewSelectNode) then
    SelectNode(treeIndexes, NewSelectNode.Index, NewSelectNode.Parent);
end;


procedure TfrmTableEditor.btnClearIndexesClick(Sender: TObject);
var
  TblKey, DeleteTblKey: TTableKey;
begin
  // Clear all indexes
  // Column data gets freed below - end any editor which could cause AV's
  if treeIndexes.IsEditing then
    treeIndexes.CancelEditNode;
  // Trigger ValidateIndexControls
  SelectNode(treeIndexes, nil);
  for TblKey in FKeys do begin
    if not TblKey.Added then begin
      DeleteTblKey := TTableKey.Create(DBObject.Connection);
      DeleteTblKey.Assign(TblKey);
      FDeletedKeys.Add(DeleteTblKey);
    end;
  end;
  FKeys.Clear;
  Modification(Sender);
  treeIndexes.Clear;
end;


procedure TfrmTableEditor.treeIndexesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  VT: TVirtualStringTree;
  TblKey: TTableKey;
begin
  // Icon image showing type of index
  VT := Sender as TVirtualStringTree;
  if Column <> 0 then Exit;
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  case VT.GetNodeLevel(Node) of
    0: ImageIndex := FKeys[Node.Index].ImageIndex;
    1: begin
      TblKey := FKeys[Node.Parent.Index];
      if TblKey.IsExpression(Node.Index) then
        ImageIndex := 13
      else
        ImageIndex := 42;
    end;
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
        0: if TblKey.IsPrimary then
             CellText := TblKey.IndexType + ' KEY' // Fixed name "PRIMARY KEY", cannot be changed
           else
             CellText := TblKey.Name;
        1: CellText := TblKey.IndexType;
        2: CellText := TblKey.Algorithm;
        3: CellText := TblKey.Comment;
        4: CellText := ''; // Column collation
      end;
    end;
    1: begin
      TblKey := FKeys[Node.Parent.Index];
      case Column of
        0: CellText := TblKey.Columns[Node.Index];
        1: CellText := TblKey.SubParts[Node.Index];
        2: CellText := ''; // Index algorithm
        3: CellText := ''; // Index comment
        4: begin
          CellText := TblKey.Collations[Node.Index];
          CellText := IfThen(CellText.ToLower = 'a', 'ASC', 'DESC');
        end;
      end;
    end;
  end;
end;


procedure TfrmTableEditor.treeIndexesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  // Tell number of columns contained in index
  ChildCount := FKeys[Node.Index].Columns.Count;
  ListColumns.Invalidate;
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


procedure TfrmTableEditor.AnyTreeClick(Sender: TObject);
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

  menuMoveUpIndex.Enabled := btnMoveUpIndex.Enabled;
  menuMoveDownIndex.Enabled := btnMoveDownIndex.Enabled;
end;


procedure TfrmTableEditor.btnAddCheckConstraintClick(Sender: TObject);
var
  CheckConstraint: TCheckConstraint;
  idx: Integer;
begin
  // Add new check constraint
  CheckConstraint := TCheckConstraint.Create(DBObject.Connection);
  idx := FCheckConstraints.Add(CheckConstraint);
  CheckConstraint.Name := 'CC'+IntToStr(idx+1);
  CheckConstraint.CheckClause := '';
  CheckConstraint.Added := True;
  Modification(Sender);
  listCheckConstraints.Repaint;
  SelectNode(listCheckConstraints, idx);
  listCheckConstraints.EditNode(listCheckConstraints.FocusedNode, listCheckConstraints.Header.MainColumn);
end;


procedure TfrmTableEditor.btnRemoveCheckConstraintClick(Sender: TObject);
var
  Constraint: TCheckConstraint;
begin
  // Remove a foreign key
  listCheckConstraints.CancelEditNode;
  Constraint := FCheckConstraints[listCheckConstraints.FocusedNode.Index];
  if (not Constraint.Added) and (not Constraint.Modified) then
    FDeletedCheckConstraints.Add(Constraint.Name);
  FCheckConstraints.Delete(listCheckConstraints.FocusedNode.Index);
  Modification(Sender);
  listCheckConstraints.Repaint;
end;


procedure TfrmTableEditor.btnClearCheckConstraintsClick(Sender: TObject);
var
  i: Integer;
begin
  // Clear all check constraints
  listCheckConstraints.CancelEditNode;
  for i:=FCheckConstraints.Count-1 downto 0 do begin
    if (not FCheckConstraints[i].Added) and (not FCheckConstraints[i].Modified) then
      FDeletedCheckConstraints.Add(FCheckConstraints[i].Name);
    FCheckConstraints.Delete(i);
  end;
  Modification(Sender);
  listCheckConstraints.Repaint;
end;


procedure TfrmTableEditor.listCheckConstraintsBeforePaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  // Set RootNodeCount
  listCheckConstraints.RootNodeCount := FCheckConstraints.Count;
  btnClearCheckConstraints.Enabled := listCheckConstraints.RootNodeCount > 0;
end;


procedure TfrmTableEditor.listCheckConstraintsCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  Edit: TInplaceEditorLink;
  EnumEditor: TEnumEditorLink;
  SQLFunc: TSQLFunction;
begin
  // Edit check constraint
  VT := Sender as TVirtualStringTree;
  case Column of
    0: begin
      Edit := TInplaceEditorLink.Create(VT, True, nil);
      Edit.TitleText := VT.Header.Columns[Column].Text;
      Edit.ButtonVisible := True;
      EditLink := Edit;
    end;
    1: begin
      EnumEditor := TEnumEditorLink.Create(VT, True, nil);
      for SQLFunc in DBObject.Connection.SQLFunctions do
        EnumEditor.ValueList.Add(SQLFunc.Name + SQLFunc.Declaration);
      EnumEditor.AllowCustomText := True;
      EditLink := EnumEditor;
    end;
  end;
end;


procedure TfrmTableEditor.listCheckConstraintsFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  // Focus on list changed
  btnRemoveCheckConstraint.Enabled := Assigned(Node);
end;


procedure TfrmTableEditor.listCheckConstraintsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  // Return image index for node cell in list
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  case Column of
    0: ImageIndex := tabCheckConstraints.ImageIndex;
    else ImageIndex := -1;
  end;
end;


procedure TfrmTableEditor.listCheckConstraintsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  CheckConstraint: TCheckConstraint;
begin
  // Return cell text in list
  CheckConstraint := FCheckConstraints[Node.Index];
  case Column of
    0: CellText := CheckConstraint.Name;
    1: CellText := CheckConstraint.CheckClause;
  end;
end;


procedure TfrmTableEditor.listCheckConstraintsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Constraint: TCheckConstraint;
begin
  // Check constraint edited
  Constraint := FCheckConstraints[Node.Index];
  if (not Constraint.Added) and (not Constraint.Modified) then
    FDeletedCheckConstraints.Add(Constraint.Name);
  case Column of
    0: Constraint.Name := NewText;
    1: Constraint.CheckClause := NewText;
  end;
  Constraint.Modified := True;
  Modification(Sender);
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
    // Disallow renaming primary key, and direction/collation of key node level
    if (Column = 0) and (VT.Text[Node, 1] <> TTableKey.PRIMARY) then
      Allowed := True
    else
      Allowed := Column in [1,2,3];
  end else case Column of
    0: Allowed := True;
    1: begin
      // Column length is allowed for (var)char/text types only, even mandantory for text and blobs
      IndexedColName := VT.Text[Node, 0];
      for i:=0 to FColumns.Count-1 do begin
        if FColumns[i].Name = IndexedColName then begin
          Allowed := FColumns[i].DataType.Category = dtcText;
          break;
        end;
      end;
    end;
    4: Allowed := True; // Collation
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
    EnumEditor := TEnumEditorLink.Create(VT, True, nil);
    EnumEditor.ValueList := TStringList.Create;
    EnumEditor.ValueList.CommaText := TTableKey.PRIMARY +','+ TTableKey.KEY +','+ TTableKey.UNIQUE +','+ TTableKey.FULLTEXT +','+ TTableKey.SPATIAL;
    EditLink := EnumEditor;
  end else if (Level = 0) and (Column = 2) then begin
    // Algorithm pulldown
    EnumEditor := TEnumEditorLink.Create(VT, True, nil);
    EnumEditor.ValueList := Explode(',', ',BTREE,HASH,RTREE');
    EditLink := EnumEditor;
  end else if (Level = 1) and (Column = 0) then begin
    // Column names pulldown
    EnumEditor := TEnumEditorLink.Create(VT, True, nil);
    ColNode := listColumns.GetFirst;
    while Assigned(ColNode) do begin
      Col := listColumns.GetNodeData(ColNode);
      EnumEditor.ValueList.Add(Col.Name);
      ColNode := listColumns.GetNext(ColNode);
    end;
    EnumEditor.AllowCustomText := True; // Allows adding a subpart in index parts: "TextCol(20)"
    EditLink := EnumEditor;
  end else if (Level = 1) and (Column = 4) then begin
    EnumEditor := TEnumEditorLink.Create(VT, True, nil);
    EnumEditor.ValueList := Explode(',', ',ASC,DESC');
    EditLink := EnumEditor;
  end else
    EditLink := TInplaceEditorLink.Create(VT, True, nil);
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
         1: begin
             TblKey.IndexType := NewText;
             if NewText = TTableKey.PRIMARY then
               TblKey.Name := TTableKey.PRIMARY;
           end;
         2: TblKey.Algorithm := NewText;
         3: TblKey.Comment := NewText;
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
         4: begin
           if NewText.ToLower = 'asc' then
             TblKey.Collations[Node.Index] := 'A'
           else
             TblKey.Collations[Node.Index] := 'D';
         end;
       end;
       TblKey.Modified := True;
    end;
  end;

  Modification(Sender);
  VT.RepaintNode(Node);
end;


procedure TfrmTableEditor.treeIndexesBeforePaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
begin
  // (Re)paint index list
  treeIndexes.RootNodeCount := FKeys.Count;
end;


procedure TfrmTableEditor.treeIndexesDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  TargetNode: PVirtualNode;
  VT: TVirtualStringtree;
begin
  // Accept nodes from the column list and allow column moving
  VT := Sender as TVirtualStringtree;
  TargetNode := VT.GetNodeAt(Pt.X, Pt.Y);

  if Source = listColumns then begin
    // Do not accept above or below a root level (index) node
    Accept := (VT.GetNodeLevel(TargetNode) = 1) or (Mode = dmOnNode);

  end else if Source = Sender then begin
    Accept := Assigned(TargetNode) and (Sender.GetNodeLevel(TargetNode) = 1) and
      (TargetNode <> Sender.FocusedNode) and (TargetNode.Parent = Sender.FocusedNode.Parent);
  end;
end;


procedure TfrmTableEditor.treeIndexesDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: TVTDragDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  FocusedNode, TargetNode, IndexNode: PVirtualNode;
  ColName, PartLength: String;
  ColPos: Cardinal;
  VT, SourceVT: TVirtualStringtree;
  Col: PTableColumn;
  TblKey: TTableKey;
begin
  // Column node dropped here
  VT := Sender as TVirtualStringtree;
  SourceVT := Source as TVirtualStringtree;
  TargetNode := VT.GetNodeAt(Pt.X, Pt.Y);
  FocusedNode := VT.FocusedNode;
  IndexNode := nil;
  ColPos := 0;
  if not Assigned(TargetNode) then begin
    MessageBeep(MB_ICONEXCLAMATION);
    Exit;
  end;
  Mainform.LogSQL('TargetNode.Index: '+TargetNode.Index.ToString, lcDebug);

  case VT.GetNodeLevel(TargetNode) of
    0: begin
      // DragOver only accepts dmOnNode in root tree level
      IndexNode := TargetNode;
      ColPos := IndexNode.ChildCount;
    end;

    1: begin
      IndexNode := TargetNode.Parent;
      // Find the right new position for the dropped column
      ColPos := TargetNode.Index;
      if Source = Sender then begin
        // Drop within index tree: Take care if user dragged from above or from below the target node
        if FocusedNode <> nil then begin
          if (FocusedNode.Index < TargetNode.Index) and (Mode = dmAbove) and (ColPos > 0) then
            Dec(ColPos);
          if (FocusedNode.Index > TargetNode.Index) and (Mode = dmBelow) and (ColPos < IndexNode.ChildCount-1) then
            Inc(ColPos);
        end;
      end else begin
        // Drop from columns list
        if Mode = dmBelow then
          Inc(ColPos);
      end;
    end;

  end;

  if Source = Sender then
    MoveFocusedIndexPart(ColPos)
  else begin
    TblKey := FKeys[IndexNode.Index];
    Col := SourceVT.GetNodeData(SourceVT.FocusedNode);
    ColName := Col.Name;
    if TblKey.Columns.IndexOf(ColName) > -1 then begin
      if MessageDialog(_('Add duplicated column to index?'),
        f_('Index "%s" already contains the column "%s". It is possible to add a column twice into a index, but total nonsense in practice.', [VT.Text[IndexNode, 0], ColName]),
        mtConfirmation, [mbYes, mbNo]) = mrNo then
        Exit;
    end;

    TblKey.Columns.Insert(ColPos, ColName);
    PartLength := '';
    if (not TblKey.IsFulltext) and (Col.DataType.Index in [dbdtTinyText, dbdtText, dbdtMediumText, dbdtLongText, dbdtTinyBlob, dbdtBlob, dbdtMediumBlob, dbdtLongBlob]) then
      PartLength := '100';
    TblKey.Subparts.Insert(ColPos, PartLength);
    TblKey.Collations.Insert(ColPos, 'A');
    IndexNode.States := IndexNode.States + [vsHasChildren, vsExpanded];
  end;
  Modification(Sender);
  // Finally tell parent node to update its children
  VT.ReinitChildren(IndexNode, False);
  VT.Repaint;
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
  if NewIdx >= Cardinal(TblKey.Columns.Count) then begin
    MessageBeep(MB_ICONEXCLAMATION);
    Exit;
  end;
  TblKey.Columns.Move(treeIndexes.FocusedNode.Index, NewIdx);
  TblKey.SubParts.Move(treeIndexes.FocusedNode.Index, NewIdx);
  TblKey.Collations.Move(treeIndexes.FocusedNode.Index, NewIdx);
  Modification(treeIndexes);
  SelectNode(treeIndexes, NewIdx, treeIndexes.FocusedNode.Parent);
end;


procedure TfrmTableEditor.PageControlMainChange(Sender: TObject);
begin
  treeIndexes.EndEditNode;
  listForeignKeys.EndEditNode;
  listCheckConstraints.EndEditNode;
  // Ensure SynMemo's have focus, otherwise Select-All and Copy actions may fail
  if PageControlMain.ActivePage = tabCREATEcode then begin
    SynMemoCreateCode.TrySetFocus;
  end
  else if PageControlMain.ActivePage = tabALTERcode then begin
    SynMemoAlterCode.TrySetFocus;
  end;
  UpdateSQLcode;
  TExtForm.PageControlTabHighlight(PageControlMain);
end;


procedure TfrmTableEditor.UpdateSQLcode;
var
  OldTopLine: Integer;
  Query: TSQLSentence;
begin
  if (PageControlMain.ActivePage = tabALTERCode) and (not AlterCodeValid) then begin
    SynMemoALTERcode.BeginUpdate;
    OldTopLine := SynMemoALTERcode.TopLine;
    SynMemoALTERcode.Clear;
    for Query in ComposeAlterStatement do
      SynMemoALTERcode.Text := SynMemoALTERcode.Text + Query.SQL + ';' + CRLF;
    SynMemoALTERcode.TopLine := OldTopLine;
    SynMemoALTERcode.EndUpdate;
    AlterCodeValid := True;
  end else if (PageControlMain.ActivePage = tabCREATECode) and (not CreateCodeValid) then begin
    SynMemoCREATEcode.BeginUpdate;
    OldTopLine := SynMemoCREATEcode.TopLine;
    SynMemoCREATEcode.Clear;
    for Query in ComposeCreateStatement do
      SynMemoCREATEcode.Text := SynMemoCREATEcode.Text + Query.SQL + ';' + CRLF;
    SynMemoCREATEcode.TopLine := OldTopLine;
    SynMemoCREATEcode.EndUpdate;
    CreateCodeValid := True;
  end;
end;


procedure TfrmTableEditor.chkCharsetConvertClick(Sender: TObject);
begin
  chkCharsetConvert.Enabled := ObjectExists and (comboCollation.ItemIndex > -1);
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
  menuCopyColumns.Enabled := ColumnsSelected;
  menuPasteColumns.Enabled := Clipboard.HasFormat(CF_TEXT);
  menuAddToIndex.Clear;
  menuCreateIndex.Clear;
  menuAddToIndex.Enabled := ColumnsSelected;
  menuCreateIndex.Enabled := ColumnsSelected;
  if not ColumnsSelected then
    Exit;

  // Auto create submenu items for "Add to index" ...
  PrimaryKeyExists := False;
  for i:=0 to FKeys.Count-1 do begin
    if FKeys[i].IsPrimary then begin
      PrimaryKeyExists := True;
      IndexName := TTableKey.PRIMARY;
    end else
      IndexName := FKeys[i].Name + ' ('+FKeys[i].IndexType+')';
    Item := AddItem(menuAddToIndex, IndexName, FKeys[i].ImageIndex);
    // Disable menuitem if all selected columns are already part of this index,
    // enable it if one or more selected columns are not.
    Item.Enabled := False;
    Node := GetNextNode(listColumns, nil, True);
    while Assigned(Node) do begin
      Col := listColumns.GetNodeData(Node);
      if FKeys[i].Columns.IndexOf(Col.Name) = -1 then begin
        Item.Enabled := True;
        Break;
      end;
      Node := GetNextNode(listColumns, Node, True);
    end;
  end;
  menuAddToIndex.Enabled := menuAddToIndex.Count > 0;

  // ... and for item "Create index"
  Item := AddItem(menuCreateIndex, TTableKey.PRIMARY, ICONINDEX_PRIMARYKEY);
  Item.Enabled := not PrimaryKeyExists;
  AddItem(menuCreateIndex, TTableKey.KEY, ICONINDEX_INDEXKEY);
  AddItem(menuCreateIndex, TTableKey.UNIQUE, ICONINDEX_UNIQUEKEY);
  AddItem(menuCreateIndex, TTableKey.FULLTEXT, ICONINDEX_FULLTEXTKEY);
  AddItem(menuCreateIndex, TTableKey.SPATIAL, ICONINDEX_SPATIALKEY);
end;


procedure TfrmTableEditor.menuAddPropertyClick(Sender: TObject);
var
  Comp: TComponent;
begin
  Comp := PopupComponent(Sender);
  if Comp = treeIndexes then
    btnAddIndex.OnClick(Sender)
  else if Comp = listForeignKeys then
    btnAddForeignKey.OnClick(Sender)
  else if Comp = listCheckConstraints then
    btnAddCheckConstraint.OnClick(Sender);
end;


procedure TfrmTableEditor.menuRemovePropertyClick(Sender: TObject);
var
  Comp: TComponent;
begin
  Comp := PopupComponent(Sender);
  if Comp = treeIndexes then
    btnRemoveIndex.OnClick(Sender)
  else if Comp = listForeignKeys then
    btnRemoveForeignKey.OnClick(Sender)
  else if Comp = listCheckConstraints then
    btnRemoveCheckConstraint.OnClick(Sender);
end;


procedure TfrmTableEditor.menuClearPropertiesClick(Sender: TObject);
var
  Comp: TComponent;
begin
  Comp := PopupComponent(Sender);
  if Comp = treeIndexes then
    btnClearIndexes.OnClick(Sender)
  else if Comp = listForeignKeys then
    btnClearForeignKeys.OnClick(Sender)
  else if Comp = listCheckConstraints then
    btnClearCheckConstraints.OnClick(Sender);
end;


procedure TfrmTableEditor.popupPropertiesPopup(Sender: TObject);
var
  Comp: TComponent;
begin
  Comp := PopupComponent(Sender);
  if Comp = treeIndexes then begin
    menuRemoveProperty.Enabled := btnRemoveIndex.Enabled;
    menuClearProperties.Enabled := btnClearIndexes.Enabled;
    menuAddIndexColumn.Enabled := Assigned(treeIndexes.FocusedNode);
  end else if Comp = listForeignKeys then begin
    menuRemoveProperty.Enabled := btnRemoveForeignKey.Enabled;
    menuClearProperties.Enabled := btnClearForeignKeys.Enabled;
    menuAddIndexColumn.Enabled := False;
  end else if Comp = listCheckConstraints then begin
    menuRemoveProperty.Enabled := btnRemoveCheckConstraint.Enabled;
    menuClearProperties.Enabled := btnClearCheckConstraints.Enabled;
    menuAddIndexColumn.Enabled := False;
  end;
end;


procedure TfrmTableEditor.AddIndexByColumn(Sender: TObject);
var
  Item: TMenuItem;
  i: Integer;
  NewType: String;
  NewParts: TStringList;
  TblKey: TTableKey;
  Node: PVirtualNode;
begin
  // Auto create index or add columns to existing one by rightclicking a column
  Item := (Sender as TMenuItem);
  NewParts := TStringList.Create;
  Node := GetNextNode(listColumns, nil, True);
  while Assigned(Node) do begin
    NewParts.Add(listColumns.Text[Node, 1]);
    Node := GetNextNode(listColumns, Node, True);
  end;
  if Item.Parent = menuCreateIndex then begin
    NewType := StripHotkey(Item.Caption);
    // Avoid creating a second key with the same columns
    for i:=0 to FKeys.Count-1 do begin
      TblKey := FKeys[i];
      if (TblKey.IndexType = NewType) and (TblKey.Columns.Text = NewParts.Text) then begin
        if MessageDialog(_('Key already exists. Really create another identical one?'),
          _('This will increase disk usage and probably slow down queries on this table.'),
          mtConfirmation, [mbYes, mbNo]) = mrNo then
          Exit;
        break;
      end;
    end;
    TblKey := TTableKey.Create(DBObject.Connection);
    TblKey.Name := Implode('_', NewParts);
    TblKey.IndexType := NewType;
    TblKey.Added := True;
    TblKey.Columns := NewParts;
    for i:=0 to TblKey.Columns.Count-1 do begin
      TblKey.SubParts.Add('');
      TblKey.Collations.Add('A');
    end;
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
        TblKey.Collations.Add('A');
      end;
    end;
    SelectNode(treeIndexes, Item.MenuIndex);
    treeIndexes.ReinitNode(treeIndexes.FocusedNode, False);
    SelectNode(treeIndexes, TblKey.Columns.Count-1, treeIndexes.FocusedNode);
  end;
  Modification(Sender);
end;


procedure TfrmTableEditor.btnAddForeignKeyClick(Sender: TObject);
var
  Key: TForeignKey;
  idx: Integer;
begin
  // Add new foreign key
  Key := TForeignKey.Create(DBObject.Connection);
  idx := FForeignKeys.Add(Key);
  Key.KeyName := 'FK'+IntToStr(idx+1);
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
  if listForeignKeys.IsEditing then
    listForeignKeys.CancelEditNode;
  Key := FForeignKeys[listForeignKeys.FocusedNode.Index];
  if not Key.Added then
    FDeletedForeignKeys.Add(Key.OldKeyName);
  FForeignKeys.Delete(listForeignKeys.FocusedNode.Index);
  Modification(Sender);
  listForeignKeys.Repaint;
end;


procedure TfrmTableEditor.btnClearForeignKeysClick(Sender: TObject);
var
  i: Integer;
begin
  // Clear all foreign keys
  if listForeignKeys.IsEditing then
    listForeignKeys.CancelEditNode;
  for i:=FForeignKeys.Count-1 downto 0 do begin
    if not FForeignKeys[i].Added then
      FDeletedForeignKeys.Add(FForeignKeys[i].OldKeyName);
    FForeignKeys.Delete(i);
  end;
  Modification(Sender);
  listForeignKeys.Repaint;
end;


procedure TfrmTableEditor.listForeignKeysBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  // Set RootNodeCount
  listForeignKeys.RootNodeCount := FForeignKeys.Count;
  btnClearForeignKeys.Enabled := listForeignKeys.RootNodeCount > 0;
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
      ErrorDialog(_('Please select a reference table before selecting foreign columns.'))
    else begin
      if Key.ReferenceTableObj = nil then begin
        // Leave Allowed = False
        ErrorDialog(f_('Reference table "%s" seems to be missing, broken or non-accessible.', [Key.ReferenceTable]));
      end else begin
        Allowed := True;
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
  PCol: PTableColumn;
  Col: TTableColumn;
  Obj: TDBObject;
  Columns: TTableColumnList;
begin
  // Init grid editor in foreign key list
  VT := Sender as TVirtualStringTree;
  case Column of
    0: EditLink := TInplaceEditorLink.Create(VT, True, nil);
    1: begin
        SetEditor := TSetEditorLink.Create(VT, True, nil);
        ColNode := listColumns.GetFirst;
        while Assigned(ColNode) do begin
          PCol := listColumns.GetNodeData(ColNode);
          SetEditor.ValueList.Add(PCol.Name);
          ColNode := listColumns.GetNextSibling(ColNode);
        end;
        EditLink := SetEditor;
      end;
    2: begin
        EnumEditor := TEnumEditorLink.Create(VT, True, nil);
        EnumEditor.AllowCustomText := True;
        DBObjects := DBObject.Connection.GetDBObjects(DBObject.Connection.Database);
        for Obj in DBObjects do begin
          if (Obj.NodeType = lntTable) then
            EnumEditor.ValueList.Add(Obj.Name);
        end;
        EditLink := EnumEditor;
      end;
    3: begin
        Key := FForeignKeys[Node.Index];
        SetEditor := TSetEditorLink.Create(VT, True, nil);
        Obj := Key.ReferenceTableObj;
        if Obj <> nil then begin
          Columns := Obj.TableColumns;
          for Col in Columns do begin
            SetEditor.ValueList.Add(Col.Name);
          end;
        end;
        EditLink := SetEditor;
      end;
    4, 5: begin
        EnumEditor := TEnumEditorLink.Create(VT, True, nil);
        EnumEditor.ValueList := Explode(',', DBObject.Connection.GetSQLSpecifity(spForeignKeyEventAction));
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
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  // Return image index for node cell in foreign key list
  if not (Kind in [ikNormal, ikSelected]) then Exit;
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
    1: CellText := Implode(',', Key.Columns);
    2: CellText := Key.ReferenceTable;
    3: CellText := Implode(',', Key.ForeignColumns);
    4: begin
        CellText := Key.OnUpdate;
        // Both ON UPDATE + DELETE default to "RESTRICT", see http://dev.mysql.com/doc/refman/5.1/en/innodb-foreign-key-constraints.html
        // MySQL 8 has a "NO ACTION" default here, which makes any fallback wrong here
      end;
    5: CellText := Key.OnDelete;
  end;
end;


procedure TfrmTableEditor.listForeignKeysNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  Key, OtherKey: TForeignKey;
  i, j, k: Integer;
  NameInUse: Boolean;
  RefDatabase, RefTable: String;
  KeyColumnsSQLCode, RefColumnsSQLCode: String;
  Err: String;
  RefColumns: TTableColumnList;
  TypesMatch: Boolean;
  RefObj: TDBObject;
begin
  // Cell text in foreign key list edited
  Key := FForeignKeys[Node.Index];
  Key.Modified := True;
  Modification(Sender);
  case Column of
    0: begin
      Key.KeyName := NewText;
      Key.KeyNameWasCustomized := True;
    end;
    1: Key.Columns := Explode(',', NewText);
    2: begin
      Key.ReferenceTable := NewText;
      if not Key.KeyNameWasCustomized then begin
        Key.KeyName := 'FK_'+DBObject.Name+'_'+Key.ReferenceTable;
        i := 1;
        NameInUse := True;
        while NameInUse do begin
          for OtherKey in FForeignKeys do begin
            NameInUse := (Key <> OtherKey) and (Key.KeyName = OtherKey.KeyName);
            if NameInUse then break;
          end;
          if NameInUse then begin
            Inc(i);
            Key.KeyName := 'FK_'+DBObject.Name+'_'+Key.ReferenceTable+'_'+IntToStr(i);
          end;
        end;

      end;
    end;
    3: begin
      Key.ForeignColumns := Explode(',', NewText);

      // Compare data types and unsigned flags of source and reference columns. See issue #400
      Err := '';
      if Key.Columns.Count <> Key.ForeignColumns.Count then begin
        Err := _('Foreign column count does not match source column count.');
      end else begin
        RefDatabase := DBObject.Database;
        RefTable := Key.ReferenceTable;
        i := Pos('.', RefTable);
        if i > 0 then begin
          RefDatabase := Copy(RefTable, 1, i-1);
          RefTable := Copy(RefTable, i+1, MaxInt);
        end;
        RefObj := TDBObject.Create(DBObject.Connection);
        RefObj.Name := RefTable;
        RefObj.Database := RefDatabase;
        RefObj.NodeType := lntTable;
        RefColumns := RefObj.TableColumns;
        TypesMatch := True;
        KeyColumnsSQLCode := '';
        RefColumnsSQLCode := '';
        for i:=0 to Key.Columns.Count-1 do begin
          for j:=0 to FColumns.Count-1 do begin
            if FColumns[j].Name = Key.Columns[i] then begin
              KeyColumnsSQLCode := KeyColumnsSQLCode + FColumns[j].SQLCode + CRLF;
              for k:=0 to RefColumns.Count-1 do begin
                if RefColumns[k].Name = Key.ForeignColumns[i] then begin
                  RefColumnsSQLCode := RefColumnsSQLCode + RefColumns[k].SQLCode + CRLF;
                  TypesMatch := TypesMatch
                    and (RefColumns[k].DataType.Index = FColumns[j].DataType.Index)
                    and (RefColumns[k].Unsigned = FColumns[j].Unsigned);
                end;
              end;
            end;
          end;
        end;
        if not TypesMatch then begin
          Err := _('The selected foreign column do not match the source columns data type and unsigned flag. This will give you an error message when trying to save this change. Please compare yourself:');
          Err := Err + CRLF + CRLF + KeyColumnsSQLCode + CRLF + Trim(RefColumnsSQLCode);
        end;
      end;
      if Err <> '' then
        ErrorDialog(_('Foreign key mismatch'), Err);

    end;
    4: Key.OnUpdate := NewText;
    5: Key.OnDelete := NewText;
  end;
end;


procedure TfrmTableEditor.btnHelpClick(Sender: TObject);
begin
  // Help button
  Help(Self, 'createtable');
end;


procedure TfrmTableEditor.menuCopyColumnsClick(Sender: TObject);
var
  Node: PVirtualNode;
  Col: PTableColumn;
  Cols: TStringList;
begin
  // Copy selected columns in a text format to clipboard
  Node := GetNextNode(listColumns, nil, True);
  Cols := TStringList.Create;
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    Cols.Add(Col.Serialize);
    Node := GetNextNode(listColumns, Node, True);
  end;
  Clipboard.TryAsText := Cols.Text;
  Cols.Free;
end;


procedure TfrmTableEditor.menuPasteColumnsClick(Sender: TObject);
var
  Node: PVirtualNode;
  Col: TTableColumn;
  ColsFromClp: TStringList;
  ColSerialized: String;
begin
  // Complement to "copy columns"
  ColsFromClp := TStringList.Create;
  ColsFromClp.Text := Clipboard.TryAsText;
  Node := listColumns.FocusedNode;
  if not Assigned(Node) then
    Node := listColumns.GetLast;
  listcolumns.BeginUpdate;
  try
    for ColSerialized in ColsFromClp do begin
      try
        Col := TTableColumn.Create(DBObject.Connection, ColSerialized);
        Col.Status := esAddedUntouched;
        // Create new node, insert column structure into list, and let OnInitNode bind its pointer
        Node := listColumns.InsertNode(Node, amInsertAfter, nil);
        FColumns.Insert(Node.Index, Col);
      except
        on E:Exception do begin
          MainForm.LogSQL(E.ClassName+' exception when creating column from text: "'+ColSerialized+'"', lcError);
        end;
      end;
    end;
  finally
    listcolumns.EndUpdate;
  end;
  listColumns.Repaint;
  Modification(Sender);
  ColsFromClp.Free;
end;


procedure TfrmTableEditor.AnyTreeStructureChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Reason: TChangeReason);
begin
  UpdateTabCaptions;
end;


procedure TfrmTableEditor.UpdateTabCaptions;
begin
  // Append number of listed keys (or whatever) to the tab caption
  tabIndexes.Caption := _('Indexes') + ' (' + FKeys.Count.ToString + ')';
  tabForeignKeys.Caption := _('Foreign keys') + ' (' + FForeignKeys.Count.ToString + ')';
  tabCheckConstraints.Caption := _('Check constraints') + ' (' + FCheckConstraints.Count.ToString + ')';
end;


end.
