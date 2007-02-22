unit copytable;


// -------------------------------------
// Copy table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CheckLst, ZDataSet, ComCtrls;

type
  TCopyTableForm = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckListBoxFields: TCheckListBox;
    CheckBoxWithAllFields: TCheckBox;
    ButtonCancel: TButton;
    CheckBoxWithIndexes: TCheckBox;
    Label3: TLabel;
    ComboSelectDatabase: TComboBox;
    ButtonOK: TButton;
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure CheckBoxWithAllFieldsClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
    oldTableName : String;
  public
    { Public declarations }
  end;

var
  CopyTableForm: TCopyTableForm;

implementation

uses helpers, main, childwin;

{$R *.DFM}

procedure TCopyTableForm.RadioButton1Click(Sender: TObject);
begin
  RadioButton2.Checked := not RadioButton1.Checked;
end;

procedure TCopyTableForm.RadioButton2Click(Sender: TObject);
begin
  RadioButton1.Checked := not RadioButton2.Checked;
end;

procedure TCopyTableForm.CheckBoxWithAllFieldsClick(Sender: TObject);
begin
  CheckListBoxFields.Enabled := not CheckBoxWithAllFields.Checked;
end;


procedure TCopyTableForm.Edit1Change(Sender: TObject);
begin
  // validate tablename
  ButtonOK.Enabled := (Edit1.text <> '');
end;


procedure TCopyTableForm.FormShow(Sender: TObject);
var
  i : Integer;
  tn : TTreeNode;
begin
  oldTableName := TMDIChild(Mainform.ActiveMDIChild).ListTables.Selected.Caption;
  Edit1.Text := oldTableName + '_copy';
  Edit1.SetFocus;
  Label1.Caption := 'Copy ''' + oldTableName + ''' to new Table:';

	// Select TargetDatabase
  ComboSelectDatabase.Items.Clear;
  with TMDIChild(Mainform.ActiveMDIChild) do
  begin
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Level = 1 then
        comboSelectDatabase.Items.Add(tn.Text);
    end;

    for i:=0 to comboSelectDatabase.Items.Count-1 do
    begin
      if (comboSelectDatabase.Items[i] = ActualDatabase) then
      begin
        comboSelectDatabase.ItemIndex := i;
        break;
      end;
    end;
    if comboSelectDatabase.ItemIndex = -1 then
      comboSelectDatabase.ItemIndex := 0;
  end;


  // fill columns:
  CheckListBoxFields.Items.Clear;
  with TMDIChild(Mainform.ActiveMDIChild) do
  begin
    GetResults( 'SHOW FIELDS FROM ' + mainform.mask(oldTableName), ZQuery3 );
    for i:=1 to ZQuery3.RecordCount do
    begin
      CheckListBoxFields.Items.Add( ZQuery3.Fields[0].AsString );
      ZQuery3.Next;
    end;
  end;

  // select all:
  for i:=0 to CheckListBoxFields.Items.Count-1 do
    CheckListBoxFields.checked[i] := true;

end;


procedure TCopyTableForm.ButtonOKClick(Sender: TObject);
var
  strquery     : String;
  i,which,k    : Integer;
  keylist      : Array of TMyKey;
  keystr       : String;
  ai_q, notnull, default    : String;
  zq           : TZReadOnlyQuery;
  isFulltext   : Boolean;
begin
  // copy table!
  strquery := 'CREATE TABLE ' + mainform.mask(ComboSelectDatabase.Text) + '.' + mainform.mask(Edit1.Text) + ' ';
  zq := TMDIChild(Mainform.ActiveMDIChild).ZQuery3;

  // keys >
  if CheckBoxWithIndexes.Checked then begin
    TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SHOW KEYS FROM ' + mainform.mask(oldtablename), zq );
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
        keylist[which].Columns := TStringList.Create;
        keylist[which].SubParts := TStringList.Create;
        with keylist[which] do // set properties for new key
        begin
          if TMDIChild(Mainform.ActiveMDIChild).mysql_version < 40002 then
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
      keylist[which].Columns.add( zq.FieldByName('Column_Name').AsString );
      keylist[which].SubParts.add( zq.FieldByName('Sub_part').AsString );
      zq.Next;
    end;
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
  if RadioButton1.Checked then
    strquery := strquery + ' WHERE 1 = 0';

  TMDIChild(Mainform.ActiveMDIChild).ExecUpdateQuery(strquery);

  // Find a auto_increment-column
  zq.SQL.Clear();
  zq.SQL.Add( 'SHOW FIELDS FROM ' + mainform.mask(oldtablename) );
  zq.Open;
  zq.First;
  for i:=1 to zq.RecordCount do
  begin
    if zq.Fields[5].AsString = 'auto_increment' then begin
      if zq.Fields[2].AsString = '' then notnull := 'NOT NULL' else notnull := '';
      if zq.Fields[4].AsString <> '' then default := 'DEFAULT "'+zq.Fields[4].AsString+'"' else default := '';
      ai_q := 'ALTER TABLE ' + mainform.mask(ComboSelectDatabase.Text) + '.'+mainform.mask(Edit1.Text)+' CHANGE '+mainform.mask(zq.Fields[0].AsString)+' '+mainform.mask(zq.Fields[0].AsString)+' '+zq.Fields[1].AsString+' '+default+' '+notnull+' AUTO_INCREMENT';
      TMDIChild(Mainform.ActiveMDIChild).ExecUpdateQuery(ai_q);
    end;
    zq.Next;
  end;

  TMDIChild(Mainform.ActiveMDIChild).ShowDBProperties(self);
  close;

end;

procedure TCopyTableForm.ButtonCancelClick(Sender: TObject);
begin
  close;
end;

end.
