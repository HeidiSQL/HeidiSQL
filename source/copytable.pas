unit copytable;


// -------------------------------------
// Copy table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CheckLst, ComCtrls, WideStrings,
  TntStdCtrls, TntCheckLst, mysql_connection;

type
  TCopyTableForm = class(TForm)
    editNewTablename: TTntEdit;
    lblNewTablename: TLabel;
    radioStructure: TRadioButton;
    radioStructureAndData: TRadioButton;
    CheckListBoxFields: TTntCheckListBox;
    CheckBoxWithAllFields: TCheckBox;
    ButtonCancel: TButton;
    CheckBoxWithIndexes: TCheckBox;
    lblTargetDB: TLabel;
    ComboSelectDatabase: TTntComboBox;
    ButtonOK: TButton;
    chkSelectAll: TCheckBox;
    procedure radioStructureClick(Sender: TObject);
    procedure radioStructureAndDataClick(Sender: TObject);
    procedure CheckBoxWithAllFieldsClick(Sender: TObject);
    procedure editNewTablenameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckListBoxFieldsClickCheck(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
  private
    { Private declarations }
    oldTableName : WideString;
  public
    { Public declarations }
  end;

  function CopyTableWindow(AOwner: TComponent): Boolean;


implementation

uses helpers, main, db;

{$R *.DFM}

{**
  Create form on demand
  @param TComponent Owner of form (should be calling form)
  @return Boolean Form closed using modalresult mrOK
}
function CopyTableWindow(AOwner: TComponent): Boolean;
var
  f : TCopyTableForm;
begin
  f := TCopyTableForm.Create(AOwner);
  Result := (f.ShowModal=mrOK);
  FreeAndNil(f);
end;


procedure TCopyTableForm.radioStructureClick(Sender: TObject);
begin
  radioStructureAndData.Checked := not radioStructure.Checked;
end;

procedure TCopyTableForm.radioStructureAndDataClick(Sender: TObject);
begin
  radioStructure.Checked := not radioStructureAndData.Checked;
end;

procedure TCopyTableForm.CheckBoxWithAllFieldsClick(Sender: TObject);
begin
  CheckListBoxFields.Enabled := not CheckBoxWithAllFields.Checked;
  chkSelectAll.Enabled := CheckListBoxFields.Enabled;
end;


procedure TCopyTableForm.editNewTablenameChange(Sender: TObject);
begin
  // validate tablename
  try
    ensureValidIdentifier( editNewTablename.Text );
    editNewTablename.Font.Color := clWindowText;
    editNewTablename.Color := clWindow;
    // Enable "OK"-Button if we have a valid name
    ButtonOK.Enabled := True;
  except
    editNewTablename.Font.Color := clRed;
    editNewTablename.Color := clYellow;
    ButtonOK.Enabled := False;
  end;
end;


procedure TCopyTableForm.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
end;


procedure TCopyTableForm.FormShow(Sender: TObject);
var
  i : Integer;
  struc_data : Byte;
  NodeData: PVTreeData;
begin
  if Mainform.DBtree.Focused then
    oldTableName := Mainform.SelectedTable.Text
  else begin
    NodeData := Mainform.ListTables.GetNodeData(Mainform.ListTables.FocusedNode);
    oldTableName := NodeData.Captions[0];
  end;
  editNewTablename.Text := oldTableName + '_copy';
  editNewTablename.SetFocus;
  lblNewTablename.Caption := 'Copy ''' + oldTableName + ''' to new table:';

	// Select TargetDatabase
  ComboSelectDatabase.Items.Clear;
  ComboSelectDatabase.Items.Assign(Mainform.Databases);
  ComboSelectDatabase.ItemIndex := ComboSelectDatabase.Items.IndexOf( Mainform.ActiveDatabase );
  if comboSelectDatabase.ItemIndex = -1 then
    comboSelectDatabase.ItemIndex := 0;

  // fill columns:
  CheckListBoxFields.Items.Text := Mainform.Connection.GetCol('SHOW FIELDS FROM ' + mainform.mask(oldTableName)).Text;

  // select all:
  for i:=0 to CheckListBoxFields.Items.Count-1 do
    CheckListBoxFields.checked[i] := true;

  {***
    restore last settings
    @see feature #1647058
  }
  struc_data := GetRegValue( REGNAME_COPYTABLE_STRUCDATA, DEFAULT_COPYTABLE_STRUCDATA );
  case struc_data of
    REGVAL_COPYTABLE_STRUCTURE:
      radioStructure.Checked := true;
    REGVAL_COPYTABLE_STRUCTURE_AND_DATA:
      radioStructureAndData.Checked := true;
  end;
  CheckBoxWithIndexes.Checked := GetRegValue( REGNAME_COPYTABLE_INDEXES, CheckBoxWithIndexes.Checked );
  CheckBoxWithAllFields.Checked := GetRegValue( REGNAME_COPYTABLE_ALLFIELDS, CheckBoxWithAllFields.Checked );
  // Ensure CheckListBoxFields + chkSelectAll are en/disabled
  CheckBoxWithAllFieldsClick(Sender);
  // Ensure chkSelectAll shows its correct state
  CheckListBoxFieldsClickCheck(Sender);
end;


procedure TCopyTableForm.ButtonOKClick(Sender: TObject);
var
  strquery     : WideString;
  i,which,k    : Integer;
  keylist      : Array of TMyKey;
  keystr       : WideString;
  notnull,
  default      : WideString;
  Results      : TMySQLQuery;
  isFulltext   : Boolean;
  struc_data   : Byte;
  Fixes        : TWideStringlist;
begin
  // copy table!

  // store settings
  if radioStructure.Checked then struc_data := REGVAL_COPYTABLE_STRUCTURE
  else struc_data := REGVAL_COPYTABLE_STRUCTURE_AND_DATA;

  OpenRegistry;
  MainReg.WriteInteger( REGNAME_COPYTABLE_STRUCDATA, struc_data );
  MainReg.WriteBool( REGNAME_COPYTABLE_INDEXES, CheckBoxWithIndexes.Checked );
  MainReg.WriteBool( REGNAME_COPYTABLE_ALLFIELDS, CheckBoxWithAllFields.Checked );

  strquery := 'CREATE TABLE ' + mainform.mask(ComboSelectDatabase.Text) + '.' + mainform.mask(editNewTablename.Text) + ' ';

  // keys >
  if CheckBoxWithIndexes.Checked then begin
    Results := Mainform.Connection.GetResults('SHOW KEYS FROM ' + mainform.mask(oldtablename));
    setLength(keylist, 0);
    keystr := '';

    for i:=1 to Results.RecordCount do
    begin
      which := -1;

      for k:=0 to length(keylist)-1 do
      begin
        if keylist[k].Name = Results.Col(2) then // keyname exists!
          which := k;
      end;
      if which = -1 then
      begin
        setlength(keylist, length(keylist)+1);
        which := high(keylist);
        keylist[which].Columns := TWideStringList.Create;
        keylist[which].SubParts := TWideStringList.Create;
        // set properties for new key
        if Mainform.Connection.ServerVersionInt < 40002 then
          isFulltext := Results.Col('Comment') = 'FULLTEXT'
        else
          isFulltext := Results.Col('Index_type') = 'FULLTEXT';
        keylist[which].Name := Results.Col(2);
        if Results.Col(2) = 'PRIMARY' then
          keylist[which]._type := 'PRIMARY'
        else if isFulltext then
          keylist[which]._type := 'FULLTEXT'
        else if Results.Col(1) = '1' then
          keylist[which]._type := ''
        else if Results.Col(1) = '0' then
          keylist[which]._type := 'UNIQUE';
      end;
      // add column
      keylist[which].Columns.add(Results.Col('Column_Name'));
      keylist[which].SubParts.add(Results.Col('Sub_part'));
      Results.Next;
    end;
    FreeAndNil(Results);
    for k:=0 to high(keylist) do
    begin
      if k > 0 then
        keystr := keystr + ',';
      if keylist[k].Name = 'PRIMARY' then
        keystr := keystr + '  PRIMARY KEY ('
      else
        keystr := keystr + '  ' + keylist[k]._type + ' KEY ' + Mainform.Mask(keylist[k].Name) + ' (';
      for i := 0 to keylist[k].Columns.count - 1 do
      begin
        if i > 0 then
          keystr := keystr + ', ';
        keystr := keystr + mainform.mask(keylist[k].Columns[i]);
        if keylist[k].SubParts[i] <> '' then
          keystr := keystr + '(' + keylist[k].SubParts[i] + ')';
      end;
      keystr := keystr + ')';
    end;
    if keystr<> '' then
      strquery := strquery + '(' + keystr + ')'
  end;
  // < keys

  // Add collation and engine clauses
  Results := Mainform.FetchActiveDbTableList;
  while not Results.Eof do begin
    if Results.Col(DBO_NAME) = oldTableName then begin
      if Results.ColExists(DBO_COLLATION) and (Results.Col(DBO_COLLATION) <> '') then
        strquery := strquery + ' COLLATE ' + Results.Col(DBO_COLLATION);
      if Results.ColExists(DBO_ENGINE) and (Results.Col(DBO_ENGINE) <> '') then
        strquery := strquery + ' ENGINE=' + Results.Col(DBO_ENGINE);
      break;
    end;
    Results.Next;
  end;

  strquery := strquery + ' SELECT';

  // which fields?
  if CheckBoxWithAllFields.Checked then
    strquery := strquery + ' *'
  else begin
    for i:=0 to CheckListBoxFields.Items.Count-1 do
      if CheckListBoxFields.Checked[i] then
        strquery := strquery + ' ' + mainform.mask(CheckListBoxFields.Items[i]) + ',';
    delete(strquery, length(strquery), 1);
  end;

  strquery := strquery + ' FROM ' + mainform.mask(oldTableName);

  // what?
  if radioStructure.Checked then
    strquery := strquery + ' WHERE 1 = 0';

  Mainform.Connection.Query(strquery, False);

  // Fix missing auto_increment property and CURRENT_TIMESTAMP defaults in new table
  Results := Mainform.Connection.GetResults('SHOW FIELDS FROM ' + mainform.mask(oldtablename));
  Fixes := TWideStringlist.Create;
  while not Results.Eof do begin
    notnull := '';
    if Results.Col('Null') = '' then
      notnull := 'NOT NULL';
    default := '';
    if Results.Col('Default') <> '' then begin
      default := 'DEFAULT ';
      if Results.Col('Default') = 'CURRENT_TIMESTAMP' then
        default := default + Results.Col('Default')
      else
        default := default + esc(Results.Col('Default'));
    end;

    if (CheckBoxWithIndexes.Checked and (Results.Col('Extra') = 'auto_increment'))
      or (Results.Col('Default') = 'CURRENT_TIMESTAMP') then begin
      Fixes.Add('CHANGE '+Mainform.mask(Results.Col('Field'))+' '+
        Mainform.mask(Results.Col('Field'))+' '+
        Results.Col('Type')+' '+default+' '+notnull+' '+Results.Col('Extra'));
    end;

    Results.Next;
  end;
  if Fixes.Count > 0 then begin
    Mainform.Connection.Query('ALTER TABLE '+Mainform.mask(ComboSelectDatabase.Text) + '.'+Mainform.mask(editNewTablename.Text)+ ' '+
      ImplodeStr(', ', Fixes)
      );
  end;
  Results.Free;
  FreeAndNil(Fixes);

  Mainform.actRefresh.Execute;
  close;

end;

procedure TCopyTableForm.CheckListBoxFieldsClickCheck(Sender: TObject);
var
  i : Integer;
  allSelected, noneSelected : Boolean;
begin
  allselected := True;
  noneSelected := True;
  for i := 0 to CheckListBoxFields.Items.Count - 1 do begin
    if CheckListBoxFields.Checked[i] then
      noneSelected := False
    else
      allSelected := False;
  end;
  if noneSelected then
    chkSelectAll.State := cbUnchecked
  else if allSelected then
    chkSelectAll.State := cbChecked
  else
    chkSelectAll.State := cbGrayed;
end;


procedure TCopyTableForm.chkSelectAllClick(Sender: TObject);
begin
  // Avoid executing when checkbox was toggled by code (see proc below)
  if (Sender as TCheckBox).Focused then
    ToggleCheckListBox( CheckListBoxFields, (Sender as TCheckBox).Checked );
end;

end.
