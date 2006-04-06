unit copytable;


// -------------------------------------
// HeidiSQL
// Copy table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CheckLst;

type
  TCopyTableForm = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckListBoxFields: TCheckListBox;
    CheckBoxWithAllFields: TCheckBox;
    ButtonOK: TBitBtn;
    ButtonCancel: TButton;
    Label2: TLabel;
    CheckBoxWithIndexes: TCheckBox;
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

uses helpers, mysql, main, childwin;

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
  r : PMYSQL_ROW;
begin
  oldTableName := TMDIChild(Mainform.ActiveMDIChild).TabellenListe.Selected.Caption;
  Edit1.Text := oldTableName + '_copy';
  Edit1.SetFocus;
  Label1.Caption := 'Copy ''' + oldTableName + ''' to new Table:';

  // fill columns:
  CheckListBoxFields.Items.Clear;
  with TMDIChild(Mainform.ActiveMDIChild) do
  begin
    myresult := q('SHOW FIELDS FROM ' + mainform.mask(oldTableName));
    for i:=0 to myresult.row_count - 1 do
    begin
      r := mysql_fetch_row(myresult);
      CheckListBoxFields.Items.Add(r[0]);
    end;
  end;

  // select all:
  for i:=0 to CheckListBoxFields.Items.Count-1 do
    CheckListBoxFields.checked[i] := true;
end;


procedure TCopyTableForm.ButtonOKClick(Sender: TObject);
var
  strquery : String;
  i,j,which,k        : Integer;
  keylist  : Array of TMyKey;
  keystr   : String;
  MyResult : PMYSQL_RES;
  row : PMYSQL_ROW;
  ai_q, notnull, default    : String;
begin
  // copy table!
  strquery := 'CREATE TABLE ' + mainform.mask(Edit1.Text) + ' ';

  // keys >
  if CheckBoxWithIndexes.Checked then begin

    MyResult := TMDIChild(Mainform.ActiveMDIChild).q('SHOW KEYS FROM ' + mainform.mask(oldtablename));
    setLength(keylist, 0);
    keystr := '';

    for j := 1 to mysql_num_rows(MyResult) do
    begin
      row := mysql_fetch_row(MyResult);
      which := -1;

      for k:=0 to length(keylist)-1 do
      begin
        if keylist[k].Name = row[2] then // keyname exists!
          which := k;
      end;
      if which = -1 then
      begin
        setlength(keylist, length(keylist)+1);
        which := high(keylist);
        keylist[which].Columns := TStringList.Create;
        with keylist[which] do // set properties for new key
        begin
          Name := row[2];
          if row[2] = 'PRIMARY' then
            _type := 'PRIMARY'
          else if mysql_num_fields(MyResult) >= 10 then if row[9] = 'FULLTEXT' then
            _type := 'FULLTEXT'
          else if row[1] = '1' then
            _type := ''
          else if row[1] = '0' then
            _type := 'UNIQUE';
        end;
      end;
      keylist[which].Columns.add(row[4]); // add column(s)
    end;
    for k:=0 to high(keylist) do
    begin
      if k > 0 then
        keystr := keystr + ',';
      if keylist[k].Name = 'PRIMARY' then
        keystr := keystr + '  PRIMARY KEY ('
      else
        keystr := keystr + '  ' + keylist[k]._type + ' KEY ' + keylist[k].Name + ' (';
      keystr := keystr + implodestr(',', keylist[k].Columns) + ')';
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
        strquery := strquery + ' ' + CheckListBoxFields.Items[i] + ',';
    delete(strquery, length(strquery), 1);
  end;

  strquery := strquery + ' FROM ' + mainform.mask(oldTableName);

  // what?
  if RadioButton1.Checked then
    strquery := strquery + ' WHERE 1 = 0';

  TMDIChild(Mainform.ActiveMDIChild).q(strquery);

  // Find a auto_increment-column
  MyResult := TMDIChild(Mainform.ActiveMDIChild).q('SHOW FIELDS FROM ' + mainform.mask(oldtablename));
  for j := 0 to mysql_num_rows(MyResult)-1 do begin
    row := mysql_fetch_row(MyResult);
    if row[5] = 'auto_increment' then begin
      if row[2] = '' then notnull := 'NOT NULL' else notnull := '';
      if row[4] <> '' then default := 'DEFAULT "'+row[4]+'"' else default := '';
      ai_q := 'ALTER TABLE '+mainform.mask(Edit1.Text)+' CHANGE '+mainform.mask(row[0])+' '+mainform.mask(row[0])+' '+row[1]+' '+default+' '+notnull+' AUTO_INCREMENT';
      TMDIChild(Mainform.ActiveMDIChild).q(ai_q);
    end;
  end;
  mysql_free_result(MyResult);



  TMDIChild(Mainform.ActiveMDIChild).ShowDBProperties(self);
  close;

end;

procedure TCopyTableForm.ButtonCancelClick(Sender: TObject);
begin
  close;
end;

end.
