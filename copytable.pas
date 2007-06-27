unit copytable;


// -------------------------------------
// HeidiSQL
// Copy table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CheckLst, ZDataSet;

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
begin
  oldTableName := TMDIChild(Mainform.ActiveMDIChild).TabellenListe.Selected.Caption;
  Edit1.Text := oldTableName + '_copy';
  Edit1.SetFocus;
  Label1.Caption := 'Copy ''' + oldTableName + ''' to new Table:';

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
  strquery : String;
  i,which,k        : Integer;
  keylist  : Array of TMyKey;
  keystr   : String;
  ai_q, notnull, default    : String;
  zq : TZReadOnlyQuery;
begin
  // copy table!
  strquery := 'CREATE TABLE ' + mainform.mask(Edit1.Text) + ' ';
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
        with keylist[which] do // set properties for new key
        begin
          Name := zq.Fields[2].AsString;
          if zq.Fields[2].AsString = 'PRIMARY' then
            _type := 'PRIMARY'
          else if zq.FieldCount >= 10 then if zq.Fields[9].AsString = 'FULLTEXT' then
            _type := 'FULLTEXT'
          else if zq.Fields[1].AsString = '1' then
            _type := ''
          else if zq.Fields[1].AsString = '0' then
            _type := 'UNIQUE';
        end;
        zq.Next;
      end;
      keylist[which].Columns.add(zq.Fields[4].AsString); // add column(s)
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
        strquery := strquery + ' ' + mainform.mask(CheckListBoxFields.Items[i]) + ',';
    delete(strquery, length(strquery), 1);
  end;

  strquery := strquery + ' FROM ' + mainform.mask(oldTableName);

  // what?
  if RadioButton1.Checked then
    strquery := strquery + ' WHERE 1 = 0';

  TMDIChild(Mainform.ActiveMDIChild).ExecQuery(strquery);

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
      ai_q := 'ALTER TABLE '+mainform.mask(Edit1.Text)+' CHANGE '+mainform.mask(zq.Fields[0].AsString)+' '+mainform.mask(zq.Fields[0].AsString)+' '+zq.Fields[1].AsString+' '+default+' '+notnull+' AUTO_INCREMENT';
      TMDIChild(Mainform.ActiveMDIChild).ExecQuery(ai_q);
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
