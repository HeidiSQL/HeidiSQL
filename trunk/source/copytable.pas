unit copytable;


// -------------------------------------
// Copy table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CheckLst, ZDataSet, ComCtrls, Registry, WideStrings,
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
    procedure radioStructureClick(Sender: TObject);
    procedure radioStructureAndDataClick(Sender: TObject);
    procedure CheckBoxWithAllFieldsClick(Sender: TObject);
    procedure editNewTablenameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    { Private declarations }
    oldTableName : WideString;
  public
    { Public declarations }
  end;

  function CopyTableWindow(AOwner: TComponent): Boolean;

const
  OPTION_UNDEFINED = 255;
  OPTION_STRUCTURE = 0;
  OPTION_STRUCTURE_AND_DATA = 1;
  OPTION_REGNAME_STRUC_DATA = 'CopyTable_Option_StructureData';
  OPTION_REGNAME_WITH_INDEXES = 'CopyTable_Option_WithIndexes';
  OPTION_REGNAME_WITH_ALL_FIELDS = 'CopyTable_Option_WithAllFields';

implementation

uses helpers, main, childwin, db;

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


procedure TCopyTableForm.FormShow(Sender: TObject);
var
  i : Integer;
  struc_data : Byte;
  ds: TDataSet;
  NodeData: PVTreeData;
begin
  NodeData := Mainform.ChildWin.ListTables.GetNodeData(Mainform.ChildWin.ListTables.FocusedNode);
  oldTableName := NodeData.Captions[0];
  editNewTablename.Text := oldTableName + '_copy';
  editNewTablename.SetFocus;
  lblNewTablename.Caption := 'Copy ''' + oldTableName + ''' to new Table:';

	// Select TargetDatabase
  ComboSelectDatabase.Items.Clear;
  ComboSelectDatabase.Items.Assign(Mainform.ChildWin.Databases);
  ComboSelectDatabase.ItemIndex := ComboSelectDatabase.Items.IndexOf( Mainform.ChildWin.ActiveDatabase );
  if comboSelectDatabase.ItemIndex = -1 then
    comboSelectDatabase.ItemIndex := 0;

  // fill columns:
  CheckListBoxFields.Items.Clear;
  ds := Mainform.ChildWin.GetResults( 'SHOW FIELDS FROM ' + mainform.mask(oldTableName) );
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
  struc_data := mainform.GetRegValue( OPTION_REGNAME_STRUC_DATA, OPTION_STRUCTURE_AND_DATA );
  case struc_data of
    OPTION_STRUCTURE:
      radioStructure.Checked := true;
    OPTION_STRUCTURE_AND_DATA:
      radioStructureAndData.Checked := true;
  end;
  CheckBoxWithIndexes.Checked := mainform.GetRegValue( OPTION_REGNAME_WITH_INDEXES, CheckBoxWithIndexes.Checked );
  CheckBoxWithAllFields.Checked := mainform.GetRegValue( OPTION_REGNAME_WITH_ALL_FIELDS, CheckBoxWithAllFields.Checked );

end;


procedure TCopyTableForm.ButtonOKClick(Sender: TObject);
var
  strquery     : WideString;
  i,which,k    : Integer;
  keylist      : Array of TMyKey;
  keystr       : WideString;
  ai_q, notnull, default    : WideString;
  zq           : TDataSet;
  isFulltext   : Boolean;
  struc_data   : Byte;
  reg          : TRegistry;
begin
  // copy table!

  // store settings
  struc_data := OPTION_UNDEFINED;
  if radioStructure.Checked then struc_data := OPTION_STRUCTURE;
  if radioStructureAndData.Checked then struc_data := OPTION_STRUCTURE_AND_DATA;

  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, False) then begin
    reg.WriteInteger( OPTION_REGNAME_STRUC_DATA, struc_data );
    reg.WriteBool( OPTION_REGNAME_WITH_INDEXES, CheckBoxWithIndexes.Checked );
    reg.WriteBool( OPTION_REGNAME_WITH_ALL_FIELDS, CheckBoxWithAllFields.Checked );
    reg.CloseKey;
  end;
  reg.Free;

  strquery := 'CREATE TABLE ' + mainform.mask(ComboSelectDatabase.Text) + '.' + mainform.mask(editNewTablename.Text) + ' ';

  // keys >
  if CheckBoxWithIndexes.Checked then begin
    zq := Mainform.ChildWin.GetResults( 'SHOW KEYS FROM ' + mainform.mask(oldtablename) );
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
          if Mainform.ChildWin.mysql_version < 40002 then
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
        keystr := keystr + '  ' + keylist[k]._type + ' KEY ' + keylist[k].Name + ' (';
      for i := 0 to keylist[k].Columns.count - 1 do
      begin
        if i > 0 then
          keystr := keystr + ', ';
        keystr := keystr + keylist[k].Columns[i];
        if keylist[k].SubParts[i] <> '' then
          keystr := keystr + '(' + keylist[k].SubParts[i] + ')';
      end;
      keystr := keystr + ')';
    end;
    if keystr<> '' then
      strquery := strquery + '(' + keystr + ')'
  end;
  // < keys

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

  Mainform.ChildWin.ExecUpdateQuery(strquery);

  // Find a auto_increment-column
  zq := Mainform.ChildWin.GetResults('SHOW FIELDS FROM ' + mainform.mask(oldtablename));
  for i:=1 to zq.RecordCount do
  begin
    if zq.Fields[5].AsString = 'auto_increment' then begin
      if zq.Fields[2].AsString = '' then notnull := 'NOT NULL' else notnull := '';
      if zq.Fields[4].AsWideString <> '' then default := 'DEFAULT "'+zq.Fields[4].AsWideString+'"' else default := '';
      ai_q := 'ALTER TABLE ' + mainform.mask(ComboSelectDatabase.Text) + '.'+mainform.mask(editNewTablename.Text)+' CHANGE '+mainform.mask(zq.Fields[0].AsWideString)+' '+mainform.mask(zq.Fields[0].AsWideString)+' '+zq.Fields[1].AsWideString+' '+default+' '+notnull+' AUTO_INCREMENT';
      Mainform.ChildWin.ExecUpdateQuery(ai_q);
    end;
    zq.Next;
  end;
  zq.Close;
  FreeAndNil(zq);

  Mainform.Childwin.MenuRefreshClick(Self);
  close;

end;

end.
