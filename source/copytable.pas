unit copytable;


// -------------------------------------
// Copy table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CheckLst, ZDataSet, ComCtrls, WideStrings,
  TntStdCtrls, TntCheckLst;

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
  ds: TDataSet;
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
  CheckListBoxFields.Items.Clear;
  ds := Mainform.GetResults( 'SHOW FIELDS FROM ' + mainform.mask(oldTableName) );
  for i:=1 to ds.RecordCount do
  begin
    CheckListBoxFields.Items.Add( ds.Fields[0].AsWideString );
    ds.Next;
  end;
  ds.Close;
  FreeAndNil(ds);

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
  zq           : TDataSet;
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
    zq := Mainform.GetResults( 'SHOW KEYS FROM ' + mainform.mask(oldtablename) );
    setLength(keylist, 0);
    keystr := '';

    for i:=1 to zq.RecordCount do
    begin
      which := -1;

      for k:=0 to length(keylist)-1 do
      begin
        if keylist[k].Name = zq.Fields[2].AsString then // keyname exists!
          which := k;
      end;
      if which = -1 then
      begin
        setlength(keylist, length(keylist)+1);
        which := high(keylist);
        keylist[which].Columns := TWideStringList.Create;
        keylist[which].SubParts := TWideStringList.Create;
        with keylist[which] do // set properties for new key
        begin
          if Mainform.mysql_version < 40002 then
            isFulltext := (zq.FieldByName('Comment').AsString = 'FULLTEXT')
          else
            isFulltext := (zq.FieldByName('Index_type').AsString = 'FULLTEXT');
          Name := zq.Fields[2].AsString;
          if zq.Fields[2].AsString = 'PRIMARY' then
            _type := 'PRIMARY'
          else if isFulltext then
            _type := 'FULLTEXT'
          else if zq.Fields[1].AsString = '1' then
            _type := ''
          else if zq.Fields[1].AsString = '0' then
            _type := 'UNIQUE';
        end;
      end;
      // add column
      keylist[which].Columns.add( zq.FieldByName('Column_Name').AsWideString );
      keylist[which].SubParts.add( zq.FieldByName('Sub_part').AsWideString );
      zq.Next;
    end;
    zq.Close;
    FreeAndNil(zq);
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
  zq := Mainform.FetchActiveDbTableList;
  while not zq.Eof do begin
    if zq.FieldByName(DBO_NAME).AsWideString = oldTableName then begin
      if (zq.FindField(DBO_COLLATION) <> nil) and (zq.FieldByName(DBO_COLLATION).AsString <> '') then
        strquery := strquery + ' COLLATE ' + zq.FieldByName(DBO_COLLATION).AsString;
      if (zq.FindField(DBO_ENGINE) <> nil) and (zq.FieldByName(DBO_ENGINE).AsString <> '') then
        strquery := strquery + ' ENGINE=' + zq.FieldByName(DBO_ENGINE).AsString;
      break;
    end;
    zq.Next;
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

  Mainform.ExecUpdateQuery(strquery);

  // Fix missing auto_increment property and CURRENT_TIMESTAMP defaults in new table
  zq := Mainform.GetResults('SHOW FIELDS FROM ' + mainform.mask(oldtablename));
  Fixes := TWideStringlist.Create;
  while not zq.Eof do begin
    notnull := '';
    if zq.FieldByName('Null').AsString = '' then
      notnull := 'NOT NULL';
    default := '';
    if zq.FieldByName('Default').AsWideString <> '' then begin
      default := 'DEFAULT ';
      if zq.FieldByName('Default').AsWideString = 'CURRENT_TIMESTAMP' then
        default := default + zq.FieldByName('Default').AsWideString
      else
        default := default + esc(zq.FieldByName('Default').AsWideString);
    end;

    if (CheckBoxWithIndexes.Checked and (zq.FieldByName('Extra').AsString = 'auto_increment'))
      or (zq.FieldByName('Default').AsString = 'CURRENT_TIMESTAMP') then begin
      Fixes.Add('CHANGE '+Mainform.mask(zq.FieldByName('Field').AsWideString)+' '+
        Mainform.mask(zq.FieldByName('Field').AsWideString)+' '+
        zq.FieldByName('Type').AsWideString+' '+default+' '+notnull+' '+zq.FieldByName('Extra').AsString);
    end;

    zq.Next;
  end;
  if Fixes.Count > 0 then begin
    Mainform.ExecUpdateQuery('ALTER TABLE '+Mainform.mask(ComboSelectDatabase.Text) + '.'+Mainform.mask(editNewTablename.Text)+ ' '+
      ImplodeStr(', ', Fixes)
      );
  end;
  zq.Close;
  FreeAndNil(zq);
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
