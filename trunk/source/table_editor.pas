unit table_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, ToolWin, VirtualTrees, WideStrings,
  SynRegExpr, ActiveX, ExtCtrls, ImgList, SynEdit, SynMemo, Menus, WideStrUtils,
  Contnrs, grideditlinks, mysql_structures, mysql_connection, helpers;

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
    StaticText1: TStaticText;
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
    listForeignKeys: TVirtualStringTree;
    lblNoForeignKeys: TLabel;
    menuCopyColumnCell: TMenuItem;
    N2: TMenuItem;
    procedure editNameChange(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure btnAddColumnClick(Sender: TObject);
    procedure btnRemoveColumnClick(Sender: TObject);
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
      var CellText: WideString);
    procedure listForeignKeysNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure btnClearForeignKeysClick(Sender: TObject);
    procedure btnAddForeignKeyClick(Sender: TObject);
    procedure btnRemoveForeignKeyClick(Sender: TObject);
    procedure listForeignKeysEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure listColumnsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure listColumnsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure listColumnsNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    { Private declarations }
    FModified: Boolean;
    FLoaded: Boolean;
    FAlterTableName: WideString;
    CreateCodeValid, AlterCodeValid: Boolean;
    FColumns: TObjectList;
    Indexes, OldIndexes,
    DeletedForeignKeys: TWideStringList;
    ForeignKeys: TObjectList;
    procedure IndexesChange(Sender: TObject);
    procedure ValidateColumnControls;
    procedure ValidateIndexControls;
    procedure MoveFocusedIndexPart(NewIdx: Cardinal);
    procedure SetModified(Value: Boolean);
    procedure ResetModificationFlags;
    function ComposeCreateStatement: WideString;
    function ComposeAlterStatement: WideString;
    function GetIndexSQL(IndexList: TWideStringlist; idx: Integer): WideString;
    function GetForeignKeySQL(idx: Integer): WideString;
    property Modified: Boolean read FModified write SetModified;
    procedure UpdateSQLcode;
    function CellEditingAllowed(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    function GetIndexIcon(idx: Integer): Integer;
    procedure GetIndexInfo(idx: Integer; var IndexName, IndexType: WideString);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(AlterTableName: WideString='');
  end;

  // Column object, many of them in a TObjectList
  TColumn = class(TObject)
    Name, OldName: WideString;
    DataType: TDatatype;
    LengthSet: WideString;
    Unsigned, AllowNull: Boolean;
    DefaultType: TColumnDefaultType;
    DefaultText: WideString;
    Comment, Collation: WideString;
    FStatus: TEditingStatus;
    private
      procedure SetStatus(Value: TEditingStatus);
    public
      property Status: TEditingStatus read FStatus write SetStatus;
  end;
  PColumn = ^TColumn;

  // Helper object to manage foreign keys in a TObjectList
  TForeignKey = class(TObject)
    KeyName, ReferenceTable, OnUpdate, OnDelete: WideString;
    Columns, ForeignColumns: TWideStringList;
    Modified, Added: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses main;

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
  FColumns := TObjectList.Create;
  Indexes := TWideStringList.Create;
  Indexes.OnChange := IndexesChange;
  OldIndexes := TWideStringList.Create;
  ForeignKeys := TObjectList.Create;
  DeletedForeignKeys := TWideStringList.Create;
end;


destructor TfrmTableEditor.Destroy;
begin
  // Store GUI setup
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_TABLEEDITOR_TABSHEIGHT, PageControlMain.Height);
  Mainform.SaveListSetup(listColumns);
  Mainform.SaveListSetup(treeIndexes);
  Mainform.SaveListSetup(listForeignKeys);
  Inherited;
end;


procedure TfrmTableEditor.Init(AlterTableName: WideString='');
var
  Results: TMySQLQuery;
  Props: TWideStringlist;
  Col: TColumn;
  IndexType: String;
  LastKeyName, CreateTable, engine, ColSpec: WideString;
  rx, rxCol: TRegExpr;
  i: Integer;
  InLiteral: Boolean;
  ForeignKey: TForeignKey;
begin
  Mainform.showstatus('Initializing editor ...');
  Screen.Cursor := crHourglass;
  FLoaded := False;
  // Start with "basic" tab activated when just called
  if FAlterTableName <> AlterTableName then
    PageControlMain.ActivePage := tabBasic;
  comboEngine.Items := Mainform.Connection.TableEngines;
  comboEngine.ItemIndex := comboEngine.Items.IndexOf(Mainform.Connection.TableEngineDefault);
  comboCollation.Items := Mainform.Connection.CollationList;
  FAlterTableName := AlterTableName;
  listColumns.BeginUpdate;
  FColumns.Clear;
  btnClearIndexesClick(Self);
  btnClearForeignKeysClick(Self);
  tabALTERcode.TabVisible := FAlterTableName <> '';
  MainForm.SetupSynEditors;

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
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(Mainform.Connection.GetVar('SHOW VARIABLES LIKE ''collation_database''', 1));
    memoUnionTables.Clear;
    comboInsertMethod.ItemIndex := -1;

    PageControlMain.ActivePage := tabBasic;

  end else begin
    // Editing existing table
    editName.Text := FAlterTableName;
    Mainform.SetEditorTabCaption(Self, FAlterTableName);
    Results := Mainform.Connection.GetResults('SHOW TABLE STATUS LIKE '+esc(FAlterTableName));
    if Results.ColExists(DBO_ENGINE) then
      engine := Results.Col(DBO_ENGINE)
    else
      engine := Results.Col(DBO_TYPE);
    comboEngine.ItemIndex := comboEngine.Items.IndexOf(engine);
    if Results.ColExists(DBO_COLLATION) then
      comboCollation.ItemIndex := comboCollation.Items.IndexOf(Results.Col(DBO_COLLATION));
    editAutoInc.Text := Results.Col(DBO_AUTOINC);
    editAvgRowLen.Text := Results.Col(DBO_AVGROWLEN);
    chkChecksum.Checked := Pos('checksum=1', LowerCase(Results.Col(DBO_CROPTIONS))) > 0;
    comboRowFormat.ItemIndex := comboRowFormat.Items.IndexOf(Results.Col(DBO_ROWFORMAT));
    FreeAndNil(Results);
    CreateTable := Mainform.Connection.GetVar('SHOW CREATE TABLE '+Mainform.mask(FAlterTableName), 1);
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.Expression := '\bUNION=\((.+)\)';
    if rx.Exec(CreateTable) then
      memoUnionTables.Text := rx.Match[1]
    else
      memoUnionTables.Clear;
    rx.Expression := '\bCOMMENT=''((.+)[^''])''';
    if rx.Exec(CreateTable) then
      memoComment.Text := WideStringReplace(rx.Match[1], '''''', '''', [rfReplaceAll])
    else
      memoComment.Clear;

    rx.ModifierS := False;
    rx.ModifierM := True;
    rx.Expression := '^\s+[`"]([^`"]+)[`"]\s(\w+)(.*)';
    rxCol := TRegExpr.Create;
    rxCol.ModifierI := True;
    if rx.Exec(CreateTable) then while true do begin
      ColSpec := rx.Match[3];

      // Strip trailing comma
      if (ColSpec <> '') and (ColSpec[Length(ColSpec)] = ',') then
        Delete(ColSpec, Length(ColSpec), 1);

      Col := TColumn.Create;
      FColumns.Add(Col);
      Col.Name := rx.Match[1];
      Col.Status := esUntouched;

      // Datatype
      Col.DataType := GetDatatypeByName(UpperCase(rx.Match[2]));

      // Length / Set
      // Various datatypes, e.g. BLOBs, don't have any length property
      InLiteral := False;
      if (ColSpec <> '') and (ColSpec[1] = '(') then begin
        for i:=2 to Length(ColSpec) do begin
          if (ColSpec[i] = ')') and (not InLiteral) then
            break;
          if ColSpec[i] = '''' then
            InLiteral := not InLiteral;
        end;
        Col.LengthSet := Copy(ColSpec, 2, i-2);
        Delete(ColSpec, 1, i);
      end;
      ColSpec := Trim(ColSpec);

      // Unsigned
      if UpperCase(Copy(ColSpec, 1, 8)) = 'UNSIGNED' then begin
        Col.Unsigned := True;
        Delete(ColSpec, 1, 9);
      end else
        Col.Unsigned := False;

      // Collation
      rxCol.Expression := '^(CHARACTER SET \w+\s+)?COLLATE (\w+)\b';
      if rxCol.Exec(ColSpec) then begin
        Col.Collation := rxCol.Match[2];
        Delete(ColSpec, 1, rxCol.MatchLen[0]+1);
      end;

      // Allow NULL
      if UpperCase(Copy(ColSpec, 1, 8)) = 'NOT NULL' then begin
        Col.AllowNull := False;
        Delete(ColSpec, 1, 9);
      end else begin
        Col.AllowNull := True;
        // Sporadically there is a "NULL" found at this position.
        if UpperCase(Copy(ColSpec, 1, 4)) = 'NULL' then
          Delete(ColSpec, 1, 5);
      end;

      // Default value
      Col.DefaultType := cdtNothing;
      Col.DefaultText := '';
      if UpperCase(Copy(ColSpec, 1, 14)) = 'AUTO_INCREMENT' then begin
        Col.DefaultType := cdtAutoInc;
        Col.DefaultText := 'AUTO_INCREMENT';
        Delete(ColSpec, 1, 15);
      end else if UpperCase(Copy(ColSpec, 1, 8)) = 'DEFAULT ' then begin
        Delete(ColSpec, 1, 8);
        if UpperCase(Copy(ColSpec, 1, 4)) = 'NULL' then begin
          Col.DefaultType := cdtNull;
          Col.DefaultText := 'NULL';
          Delete(ColSpec, 1, 5);
        end else if UpperCase(Copy(ColSpec, 1, 17)) = 'CURRENT_TIMESTAMP' then begin
          Col.DefaultType := cdtCurTS;
          Col.DefaultText := 'CURRENT_TIMESTAMP';
          Delete(ColSpec, 1, 18);
        end else if ColSpec[1] = '''' then begin
          InLiteral := True;
          for i:=2 to Length(ColSpec) do begin
            if ColSpec[i] = '''' then
              InLiteral := not InLiteral
            else if not InLiteral then
              break;
          end;
          Col.DefaultType := cdtText;
          Col.DefaultText := Copy(ColSpec, 2, i-3);
          // A single quote gets escaped by single quote - remove the escape char - escaping is done in Save action afterwards
          Col.DefaultText := WideStringReplace(Col.DefaultText, '''''', '''', [rfReplaceAll]);
          Delete(ColSpec, 1, i);
        end;
      end;
      if UpperCase(Copy(ColSpec, 1, 27)) = 'ON UPDATE CURRENT_TIMESTAMP' then begin
        // Adjust default type
        case Col.DefaultType of
          cdtText: Col.DefaultType := cdtTextUpdateTS;
          cdtNull: Col.DefaultType := cdtNullUpdateTS;
          cdtCurTS: Col.DefaultType := cdtCurTSUpdateTS;
        end;
        Delete(ColSpec, 1, 28);
      end;

      // Comment
      if UpperCase(Copy(ColSpec, 1, 9)) = 'COMMENT ''' then begin
        InLiteral := True;
        for i:=10 to Length(ColSpec) do begin
          if ColSpec[i] = '''' then
            InLiteral := not InLiteral
          else if not InLiteral then
            break;
        end;
        Col.Comment := Copy(ColSpec, 10, i-11);
        Col.Comment := WideStringReplace(Col.Comment, '''''', '''', [rfReplaceAll]);
        Delete(ColSpec, 1, i);
      end;

      if not rx.ExecNext then
        break;
    end;
    FreeAndNil(rxCol);

    // Detect foreign keys
    // CONSTRAINT `FK1` FOREIGN KEY (`which`) REFERENCES `fk1` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
    rx.Expression := '\s+CONSTRAINT\s+[`"]([^`"]+)[`"]\sFOREIGN KEY\s+\(([^\)]+)\)\s+REFERENCES\s+[`"]([^\(]+)[`"]\s\(([^\)]+)\)(\s+ON DELETE (RESTRICT|CASCADE|SET NULL|NO ACTION))?(\s+ON UPDATE (RESTRICT|CASCADE|SET NULL|NO ACTION))?';
    if rx.Exec(CreateTable) then while true do begin
      ForeignKey := TForeignKey.Create;
      ForeignKeys.Add(ForeignKey);
      ForeignKey.KeyName := rx.Match[1];
      ForeignKey.ReferenceTable := WideStringReplace(rx.Match[3], '`', '', [rfReplaceAll]);
      ForeignKey.ReferenceTable := WideStringReplace(ForeignKey.ReferenceTable, '"', '', [rfReplaceAll]);
      ExplodeQuotedList(rx.Match[2], ForeignKey.Columns);
      ExplodeQuotedList(rx.Match[4], ForeignKey.ForeignColumns);
      if rx.Match[6] <> '' then
        ForeignKey.OnDelete := rx.Match[6];
      if rx.Match[8] <> '' then
        ForeignKey.OnUpdate := rx.Match[8];
      if not rx.ExecNext then
        break;
    end;
    FreeAndNil(rx);

    Results := Mainform.Connection.GetResults('SHOW KEYS FROM '+Mainform.mask(FAlterTableName));
    LastKeyName := '';
    Props := nil;
    while not Results.Eof do begin
      if LastKeyName <> Results.Col('Key_name') then begin
        Props := TWideStringlist.Create;
        Props.OnChange := IndexesChange;
        IndexType := Results.Col('Key_name');
        if Results.ColExists('Index_type') and (
          (Results.Col('Index_type') = FKEY) or (Results.Col('Index_type') = SKEY)) then
          IndexType := Results.Col('Index_type')
        else if (IndexType <> PKEY) and (Results.Col('Non_unique') = '0') then
          IndexType := UKEY
        else if Results.Col('Non_unique') = '1' then
          IndexType := KEY;
        Indexes.AddObject(Results.Col('Key_name')+REGDELIM+IndexType, Props);
      end;
      Props.Add(Results.Col('Column_name'));
      if Results.Col('Sub_part') <> '' then
        Props[Props.Count-1] := Props[Props.Count-1] + '('+Results.Col('Sub_part')+')';
      LastKeyName := Results.Col('Key_name');
      Results.Next;
    end;
    FreeAndNil(Results);

  end;
  listColumns.RootNodeCount := FColumns.Count;
  DeInitializeVTNodes(listColumns);
  listColumns.EndUpdate;
  // Validate controls
  comboEngineSelect(comboEngine);
  ValidateColumnControls;
  ValidateIndexControls;
  ResetModificationFlags;
  // Indicate change mechanisms can call their events now. See Modification().
  FLoaded := True;
  // Empty status panel
  Mainform.showstatus;
  Screen.Cursor := crDefault;
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
  Specs: TWideStringlist;
  Key: TForeignKey;
  Col: TColumn;
  FocusChangeEvent: procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex) of object;
begin
  // Create or alter table
  try
    if FAlterTableName = '' then
      sql := ComposeCreateStatement
    else begin
      sql := ComposeAlterStatement;
      // Special case for altered foreign keys: These have to be dropped in a seperate query
      // otherwise the server would return error 121 "Duplicate key on write or update"
      // See also http://dev.mysql.com/doc/refman/5.1/en/innodb-foreign-key-constraints.html :
      //   "You cannot add a foreign key and drop a foreign key in separate clauses of a single
      //   ALTER TABLE  statement. Separate statements are required."
      Specs := TWideStringList.Create;
      for i:=0 to ForeignKeys.Count-1 do begin
        Key := ForeignKeys[i] as TForeignKey;
        if Key.Modified and (not Key.Added) then
          Specs.Add('DROP FOREIGN KEY '+Mainform.mask(Key.KeyName));
      end;
      if Specs.Count > 0 then
        Mainform.Connection.Query('ALTER TABLE '+Mainform.mask(FAlterTableName)+' '+ImplodeStr(', ', Specs));
    end;
    Mainform.Connection.Query(sql);
    // Set table name for altering if Apply was clicked
    FAlterTableName := editName.Text;
    tabALTERcode.TabVisible := FAlterTableName <> '';
    if chkCharsetConvert.Checked then begin
      // Autoadjust column collations
      for i:=0 to FColumns.Count-1 do begin
        Col := FColumns[i] as TColumn;
        if Col.Collation <> '' then
          Col.Collation := comboCollation.Text;
      end;
    end;
    ResetModificationFlags;

    // Refresh db tree and reselect edited table node.
    // Supress tab jumping, implicitely invoked by RefreshTreeDB.
    FocusChangeEvent := Mainform.DBtree.OnFocusChanged;
    Mainform.DBtree.OnFocusChanged := nil;
    Mainform.RefreshTreeDB(Mainform.ActiveDatabase);
    Mainform.DBtree.OnFocusChanged := FocusChangeEvent;
    Mainform.ResetSelectedTableStuff;
    Mainform.SelectDBObject(FAlterTableName, lntTable);
  except
    on E:Exception do
      MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;


procedure TfrmTableEditor.ResetModificationFlags;
var
  i: Integer;
  Parts: TWideStringlist;
  Key: TForeignKey;
  Col: TColumn;
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
    Col := FColumns[i] as TColumn;
    if Col.Status = esDeleted then
      FColumns.Delete(i)
    else begin
      Col.Status := esUntouched;
      Col.OldName := '';
    end;
  end;
  // Copy index list so we have one to work with and one which can later be used to detect changes
  OldIndexes.Clear;
  for i:=0 to Indexes.Count-1 do begin
    Parts := TWideStringlist.Create;
    Parts.Assign(TWideStringlist(Indexes.Objects[i]));
    OldIndexes.AddObject(Indexes[i], Parts);
  end;
  DeletedForeignKeys.Clear;
  for i:=0 to ForeignKeys.Count-1 do begin
    Key := ForeignKeys[i] as TForeignKey;
    Key.Added := False;
    Key.Modified := False;
  end;
  Modified := False;
end;


function TfrmTableEditor.ComposeAlterStatement: WideString;
var
  Specs, IndexesComposed, OldIndexesComposed: TWideStringlist;
  ColSpec, OldColName, IndexSQL: WideString;
  i: Integer;
  DropIt: Boolean;
  Results: TMySQLQuery;
  Key: TForeignKey;
  Col, PreviousCol: PColumn;
  ColObj: TColumn;
  Node: PVirtualNode;
begin
  // Compose ALTER query, called by buttons and for SQL code tab
  Mainform.showstatus('Composing ALTER statement ...');
  Screen.Cursor := crHourglass;
  Specs := TWideStringlist.Create;
  if editName.Text <> FAlterTableName then
    Specs.Add('RENAME TO ' + Mainform.mask(editName.Text));
  if memoComment.Tag = ModifiedFlag then
    Specs.Add('COMMENT=' + esc(memoComment.Text));
  if (comboCollation.Tag = ModifiedFlag) or (chkCharsetConvert.Checked) then
    Specs.Add('COLLATE=' + comboCollation.Text);
  if comboEngine.Tag = ModifiedFlag then
    Specs.Add('ENGINE=' + comboEngine.Text);
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
  EnableProgressBar(FColumns.Count + OldIndexes.Count + Indexes.Count);
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
    ColObj := FColumns[i] as TColumn;
    if ColObj.Status = esDeleted then begin
      OldColName := ColObj.OldName;
      if OldColName = '' then
        OldColName := ColObj.Name;
      Specs.Add('DROP COLUMN '+Mainform.mask(OldColName));
    end;
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
    Mainform.ProgressBarStatus.StepIt;
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
    Mainform.ProgressBarStatus.StepIt;
    if (OldIndexesComposed.IndexOf(IndexesComposed[i]) = -1) and (IndexesComposed[i] <> '') then
      Specs.Add('ADD '+IndexesComposed[i]);
  end;

  for i:=0 to DeletedForeignKeys.Count-1 do
    Specs.Add('DROP FOREIGN KEY '+Mainform.mask(DeletedForeignKeys[i]));

  for i:=0 to ForeignKeys.Count-1 do begin
    Key := ForeignKeys[i] as TForeignKey;
    if Key.Added or Key.Modified then
      Specs.Add('ADD '+GetForeignKeySQL(i));
  end;

  Result := 'ALTER TABLE '+Mainform.mask(FAlterTableName) + CRLF + #9 + ImplodeStr(',' + CRLF + #9, Specs);
  Result := Trim(Result);
  FreeAndNil(Specs);
  FreeAndNil(IndexesComposed);
  FreeAndNil(OldIndexesComposed);
  Mainform.showstatus;
  Mainform.ProgressBarStatus.Hide;
  Screen.Cursor := crDefault;
end;


function TfrmTableEditor.ComposeCreateStatement: WideString;
var
  i, IndexCount: Integer;
  Col: PColumn;
  Node: PVirtualNode;
  tmp: WideString;
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
  for i:=0 to Indexes.Count - 1 do begin
    tmp := GetIndexSQL(Indexes, i);
    if tmp <> '' then begin
      Result := Result + #9 + GetIndexSQL(Indexes, i) + ','+CRLF;
      Inc(IndexCount);
    end;
  end;

  for i:=0 to ForeignKeys.Count-1 do
    Result := Result + #9 + GetForeignKeySQL(i) + ','+CRLF;

  if Integer(listColumns.RootNodeCount) + IndexCount + ForeignKeys.Count > 0 then
    Delete(Result, Length(Result)-2, 3);

  Result := Result + CRLF + ')' + CRLF;
  if memoComment.Text <> '' then
    Result := Result + 'COMMENT='+esc(memoComment.Text) + CRLF;
  if comboCollation.Text <> '' then
    Result := Result + 'COLLATE='+comboCollation.Text + CRLF;
  if comboEngine.Text <> '' then
    Result := Result + 'ENGINE='+comboEngine.Text + CRLF;
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


function TfrmTableEditor.GetIndexSQL(IndexList: TWideStringlist; idx: Integer): WideString;
var
  IndexName, IndexType, ColName: WideString;
  Parts: TWideStringlist;
  i, p: Integer;
begin
  Result := '';
  Parts := TWideStringlist(IndexList.Objects[idx]);
  // Supress SQL error  trying index creation with 0 column
  if Parts.Count = 0 then
    Exit;
  IndexName := Copy(IndexList[idx], 1, LastPos(REGDELIM, IndexList[idx])-1);
  IndexType := Copy(IndexList[idx], LastPos(REGDELIM, IndexList[idx])+1, Length(IndexList[idx]));
  if IndexType = PKEY then
    Result := Result + 'PRIMARY KEY '
  else begin
    if IndexType <> KEY then
      Result := Result + IndexType + ' ';
    Result := Result + 'INDEX ' + Mainform.Mask(IndexName) + ' ';
  end;
  Result := Result + '(';
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


function TfrmTableEditor.GetForeignKeySQL(idx: Integer): WideString;
var
  Key: TForeignKey;
  i: Integer;
begin
  Key := ForeignKeys[idx] as TForeignKey;
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
  end;
end;


procedure TfrmTableEditor.btnAddColumnClick(Sender: TObject);
var
  NewCol: TColumn;
  FocusedCol: PColumn;
  fn, NewNode: PVirtualNode;
  idx: Integer;
begin
  // Add new column after selected one
  fn := listColumns.FocusedNode;
  NewCol := TColumn.Create;
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
  Col: PColumn;
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
  Col: PColumn;
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
  Col: PColumn;
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
  var CellText: WideString);
var
  Col: PColumn;
begin
  // Display column text
  Col := Sender.GetNodeData(Node);
  case Column of
    0: CellText := IntToStr(Node.Index+1);
    1: CellText := Col.Name;
    2: CellText := Col.DataType.Name;
    3: CellText := Col.LengthSet;
    4, 5: CellText := ''; // Checkbox
    6: case Col.DefaultType of
      cdtNothing:                 CellText := '';
      cdtText, cdtTextUpdateTS:   CellText := Col.DefaultText;
      cdtNull, cdtNullUpdateTS:   CellText := 'NULL';
      cdtCurTS, cdtCurTSUpdateTS: CellText := 'CURRENT_TIMESTAMP';
      cdtAutoInc:                 CellText := 'AUTO_INCREMENT';
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
  Col: PColumn;
begin
  // Bind data to node
  Col := Sender.GetNodeData(Node);
  Col^ := FColumns[Node.Index] as TColumn;
end;


procedure TfrmTableEditor.listColumnsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  IndexName, IndexType: WideString;
  Props: TWideStringlist;
  i: Integer;
  Col: PColumn;
begin
  // Primary key icon
  if Column <> 0 then Exit;
  Col := Sender.GetNodeData(Node);

  for i:=0 to Indexes.Count-1 do begin
    GetIndexInfo(i, IndexName, IndexType);
    Props := TWideStringlist(Indexes.Objects[i]);
    if Props.IndexOf(Col.Name) > -1 then begin
      if IndexType = PKEY then ImageIndex := ICONINDEX_PRIMARYKEY
      else if IndexType = UKEY then ImageIndex := ICONINDEX_UNIQUEKEY
      else if IndexType = KEY then ImageIndex := ICONINDEX_INDEXKEY
      else if IndexType = FKEY then ImageIndex := ICONINDEX_FULLTEXTKEY
      else if IndexType = SKEY then ImageIndex := ICONINDEX_SPATIALKEY;
    end;
    // Priority for PK columns. We cannot display more than one icon anyway.
    if ImageIndex > -1 then
      break;
  end;
end;


procedure TfrmTableEditor.listColumnsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TColumn);
end;


procedure TfrmTableEditor.listColumnsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  TextColor: TColor;
  i: Integer;
  IndexName, IndexType: WideString;
  Col: PColumn;
begin
  Col := Sender.GetNodeData(Node);
  // Bold font for primary key columns
  for i:=0 to Indexes.Count-1 do begin
    GetIndexInfo(i, IndexName, IndexType);
    if IndexType = PKEY then begin
      if TWideStringlist(Indexes.Objects[i]).IndexOf(Col.Name) > -1 then
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
      cdtNull, cdtNullUpdateTS:
        TextColor := clGray;
      cdtCurTS, cdtCurTSUpdateTS:
        TextColor := DatatypeCategories[Integer(dtcTemporal)].Color;
      cdtAutoInc:
        TextColor := clNavy;
    end;
  end;
  TargetCanvas.Font.Color := TextColor;
end;


procedure TfrmTableEditor.listColumnsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  i: Integer;
  Col: PColumn;
begin
  // Column property edited
  Col := Sender.GetNodeData(Node);
  case Column of
    1: begin
      for i:=0 to FColumns.Count-1 do begin
        if TColumn(FColumns[i]).Name = NewText then begin
          MessageDlg('Column "'+NewText+'" already exists.', mtError, [mbOk], 0);
          Exit;
        end;
      end;
      Col.OldName := Col.Name;
      Col.Name := NewText;
    end;
    2: begin
      Col.DataType := GetDatatypeByName(NewText);
      // Reset length/set for column types which don't support that
      if not Col.DataType.HasLength then
        Col.LengthSet := '';
      // Suggest length/set if required
      if Col.DataType.RequiresLength and (Col.LengthSet = '') then
        Col.LengthSet := Col.DataType.DefLengthSet;
    end;
    3: Col.LengthSet := NewText;
    // 4 + 5 are checkboxes - handled in OnClick
    6: begin
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
  Col: PColumn;
begin
  Col := Sender.GetNodeData(Node);
  Col.Status := esModified;
end;


procedure TfrmTableEditor.listColumnsClick(Sender: TObject);
var
  VT: TVirtualStringTree;
  Col: PColumn;
  Click: THitInfo;
begin
  // Handle click event
  VT := Sender as TVirtualStringTree;
  VT.GetHitTestInfoAt(Mouse.CursorPos.X-VT.ClientOrigin.X, Mouse.CursorPos.Y-VT.ClientOrigin.Y, True, Click);
  if not Assigned(Click.HitNode) then
    Exit;
  // For checkboxes, cell editors are disabled, instead toggle their state
  if CellEditingAllowed(Click.HitNode, Click.HitColumn) then begin
    Col := VT.GetNodeData(Click.HitNode);
    case Click.HitColumn of
      4: begin
        Col.Unsigned := not Col.Unsigned;
        Col.Status := esModified;
        VT.InvalidateNode(Click.HitNode);
      end;
      5: begin
        Col.AllowNull := not Col.AllowNull;
        // Switch default value from NULL to Text if Allow Null is off
        if (not Col.AllowNull) and (Col.DefaultType in [cdtNull, cdtNullUpdateTS]) then
          Col.DefaultType := cdtText;
        Col.Status := esModified;
        VT.InvalidateNode(Click.HitNode);
      end;
      else begin
        // All other cells go into edit mode please
        // Explicitely done on OnClick, not in OnFocusChanged which seemed annoying for keyboard users
        if Assigned(Click.HitNode) and (Click.HitColumn > NoColumn) and (hiOnItemLabel in Click.HitPositions) then
          VT.EditNode(Click.HitNode, Click.HitColumn);
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
  Col: PColumn;
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
      EnumEditor.ValueList := TWideStringList.Create;
      EnumEditor.ValueList.Text := Mainform.Connection.CollationList.Text;
      EnumEditor.ValueList.Insert(0, '');
      EditLink := EnumEditor;
      end;
    6: begin
      DefaultEditor := TColumnDefaultEditorLink.Create(VT);
      DefaultEditor.DefaultText := Col.DefaultText;
      DefaultEditor.DefaultType := Col.DefaultType;
      EditLink := DefaultEditor;
    end
    else
      EditLink := TInplaceEditorLink.Create(VT);
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
  Column: TColumn;
begin
  // Add column to index
  Node := treeIndexes.FocusedNode;
  if not Assigned(Node) then
    Exit;
  if treeIndexes.GetNodeLevel(Node) = 1 then
    Node := Node.Parent;
  IndexParts := TWideStringlist(Indexes.Objects[Node.Index]);
  // Find the first unused column for that index as default
  ColExists := False;
  NewCol := '';
  for i:=0 to FColumns.Count-1 do begin
    Column := FColumns[i] as TColumn;
    if Column.Status = esDeleted then
      Continue;
    for j:=0 to IndexParts.Count - 1 do begin
      Col := IndexParts[j];
      p := LastPos('(', Col);
      if p > 0 then
        Col := Copy(Col, 0, p-1);
      ColExists := Col = Column.Name;
      if ColExists then
        break;
    end;
    if not ColExists then begin
      NewCol := Column.Name;
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
      ImageIndex := GetIndexIcon(Node.Index);
    end;
    1: ImageIndex := 42;
  end;
end;


procedure TfrmTableEditor.treeIndexesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  IndexProps: TWideStringlist;
  IndexName, IndexType: WideString;
begin
  // Index tree showing cell text
  case Sender.GetNodeLevel(Node) of
    0: begin
      GetIndexInfo(Node.Index, IndexName, IndexType);
      case Column of
        0: if IndexType = PKEY then
             CellText := IndexType + ' KEY' // Fixed name "PRIMARY KEY", cannot be changed
           else
             CellText := IndexName;
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
  VT: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
  Level: Cardinal;
  ColNode: PVirtualNode;
  Col: PColumn;
begin
  // Start cell editor
  VT := Sender as TVirtualStringTree;
  Level := (Sender as TVirtualStringtree).GetNodeLevel(Node);
  if (Level = 0) and (Column = 1) then begin
    // Index type pulldown
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.ValueList := TWideStringList.Create;
    EnumEditor.ValueList.CommaText := PKEY +','+ KEY +','+ UKEY +','+ FKEY +','+ SKEY;
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
  VT, SourceVT: TVirtualStringtree;
  Col: PColumn;
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
    ColPos := MaxInt;

  if Source = Sender then
    MoveFocusedIndexPart(ColPos)
  else begin
    IndexParts := TWideStringlist(Indexes.Objects[Node.Index]);
    Col := SourceVT.GetNodeData(SourceVT.FocusedNode);
    ColName := Col.Name;
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
    lblNoForeignKeys.Caption := 'The selected table engine ('+comboEngine.Text+') does not support foreign keys. '+
      'To enable foreign keys you have to change the table engine in the "Options" tab to "InnoDB".';
    if SupportsForeignKeys then
      ListForeignKeys.Height := lblNoForeignKeys.Top - ListForeignKeys.Top + lblNoForeignKeys.Height
    else
      ListForeignKeys.Height := lblNoForeignKeys.Top - ListForeignKeys.Top - 4;
    lblNoForeignKeys.Visible := not SupportsForeignKeys;
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
  chkCharsetConvert.Enabled := (FAlterTablename <> '') and (comboCollation.ItemIndex > -1);
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
  IndexName, IndexType: WideString;
  Item: TMenuItem;
  PrimaryKeyExists,
  ColumnsSelected: Boolean;
  IndexParts: TWideStringList;
  Node: PVirtualNode;
  Col: PColumn;
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
  for i:=0 to Indexes.Count-1 do begin
    GetIndexInfo(i, IndexName, IndexType);
    if IndexType = PKEY then
      PrimaryKeyExists := True
    else
      IndexName := IndexName + ' ('+IndexType+')';
    Item := AddItem(menuAddToIndex, IndexName, GetIndexIcon(i));
    // Disable menuitem if all selected columns are already part of this index,
    // enable it if one or more selected columns are not.
    Item.Enabled := False;
    IndexParts := TWideStringList(Indexes.Objects[i]);
    Node := listColumns.GetFirstSelected;
    while Assigned(Node) do begin
      Col := listColumns.GetNodeData(Node);
      if IndexParts.IndexOf(Col.Name) = -1 then begin
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
  IndexName, IndexType,
  NewName, NewType: WideString;
  IndexParts, NewParts: TWideStringlist;
begin
  // Auto create index or add columns to existing one by rightclicking a column
  Item := (Sender as TMenuItem);
  NewParts := GetVTCaptions(listColumns, True, 1);
  if Item.Parent = menuCreateIndex then begin
    // Remove auto hotkeys
    NewType := StringReplace(Item.Caption, '&', '', [rfReplaceAll]);
    // Avoid creating a second key with the same columns
    for i:=0 to Indexes.Count-1 do begin
      IndexParts := TWideStringList(Indexes.Objects[i]);
      GetIndexInfo(i, IndexName, IndexType);
      if (IndexType = NewType) and (IndexParts.Text = NewParts.Text) then begin
        if MessageDlg('Key already exists. Really create another identical one?'+CRLF+CRLF+
          'This will increase disk usage and probably slow down queries on this table.',
          mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit;
        break;
      end;
    end;
    NewParts.OnChange := IndexesChange;
    NewName := ImplodeStr('_', NewParts);
    Indexes.AddObject(NewName+REGDELIM+NewType, NewParts);
    PageControlMain.ActivePage := tabIndexes;
    treeIndexes.Repaint;
    SelectNode(treeIndexes, Indexes.Count-1);
    SelectNode(treeIndexes, 0, treeIndexes.FocusedNode);
  end else begin
    PageControlMain.ActivePage := tabIndexes;
    IndexParts := TWideStringlist(Indexes.Objects[Item.MenuIndex]);
    for i:=0 to NewParts.Count-1 do begin
      if IndexParts.IndexOf(NewParts[i]) = -1 then
        IndexParts.Add(NewParts[i]);
    end;
    SelectNode(treeIndexes, Item.MenuIndex);
    treeIndexes.ReinitChildren(treeIndexes.FocusedNode, False);
    SelectNode(treeIndexes, IndexParts.Count-1, treeIndexes.FocusedNode);
  end;
end;


function TfrmTableEditor.GetIndexIcon(idx: Integer): Integer;
var
  IndexType, IndexName: WideString;
begin
  // Detect key icon index for specified index
  GetIndexInfo(idx, IndexName, IndexType);
  if IndexType = PKEY then Result := ICONINDEX_PRIMARYKEY
  else if IndexType = KEY then Result := ICONINDEX_INDEXKEY
  else if IndexType = UKEY then Result := ICONINDEX_UNIQUEKEY
  else if IndexType = FKEY then Result := ICONINDEX_FULLTEXTKEY
  else if IndexType = SKEY then Result := ICONINDEX_SPATIALKEY
  else Result := -1;
end;


procedure TfrmTableEditor.GetIndexInfo(idx: Integer; var IndexName, IndexType: WideString);
begin
  IndexName := Copy(Indexes[idx], 1, LastPos(REGDELIM, Indexes[idx])-1);
  IndexType := Copy(Indexes[idx], LastPos(REGDELIM, Indexes[idx])+1, Length(Indexes[idx]));
end;


procedure TfrmTableEditor.btnAddForeignKeyClick(Sender: TObject);
var
  Key: TForeignKey;
  idx: Integer;
begin
  // Add new foreign key
  Key := TForeignKey.Create;
  idx := ForeignKeys.Add(Key);
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
  Key := ForeignKeys[listForeignKeys.FocusedNode.Index] as TForeignKey;
  if not Key.Added then
    DeletedForeignKeys.Add(Key.KeyName);
  ForeignKeys.Delete(listForeignKeys.FocusedNode.Index);
  Modification(Sender);
  listForeignKeys.Repaint;
end;


procedure TfrmTableEditor.btnClearForeignKeysClick(Sender: TObject);
var
  i: Integer;
  Key: TForeignKey;
begin
  // Clear all foreign keys
  for i:=ForeignKeys.Count-1 downto 0 do begin
    Key := ForeignKeys[i] as TForeignKey;
    if not Key.Added then
      DeletedForeignKeys.Add(Key.KeyName);
    ForeignKeys.Delete(i);
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
  VT.RootNodeCount := ForeignKeys.Count;
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
    Key := ForeignKeys[Node.Index] as TForeignKey;
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
  Results: TMySQLQuery;
  Key: TForeignKey;
  ColNode: PVirtualNode;
  Col: PColumn;
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
        Results := Mainform.FetchActiveDbTableList;
        while not Results.Eof do begin
          EnumEditor.ValueList.Add(Results.Col(DBO_NAME));
          Results.Next;
        end;
        EditLink := EnumEditor;
      end;
    3: begin
        Key := ForeignKeys[Node.Index] as TForeignKey;
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
  var CellText: WideString);
var
  Key: TForeignKey;
begin
  // Return cell text in foreign key list
  Key := ForeignKeys[Node.Index] as TForeignKey;
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
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  Key: TForeignKey;
begin
  // Cell text in foreign key list edited
  Key := ForeignKeys[Node.Index] as TForeignKey;
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



{ *** TForeignKey structure }

constructor TForeignKey.Create;
begin
  inherited Create;
  Columns := TWideStringlist.Create;
  ForeignColumns := TWideStringlist.Create;
end;

destructor TForeignKey.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Columns);
  FreeAndNil(ForeignColumns);
end;

procedure TColumn.SetStatus(Value: TEditingStatus);
begin
  // Set editing flag and enable "Save" button
  if (FStatus in [esAddedUntouched, esAddedModified]) and (Value = esModified) then
    Value := esAddedModified
  else if (FStatus in [esAddedUntouched, esAddedModified]) and (Value = esDeleted) then
    Value := esAddedDeleted;
  FStatus := Value;
  if Value <> esUntouched then
    Mainform.TableEditor.Modification(Self);
end;

end.
