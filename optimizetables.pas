unit optimizetables;


// -------------------------------------
// HeidiSQL
// Table-diagnostics
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, CheckLst, comctrls, Buttons, mysql, ToolWin;

type
  Toptimize = class(TForm)
    TablesCheckListBox: TCheckListBox;
    DBComboBox: TComboBox;
    Label1: TLabel;
    Button3: TButton;
    CheckBoxQuickRepair: TCheckBox;
    CheckBoxQuickCheck: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Label3: TLabel;
    ListViewResults: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DBComboBoxChange(Sender: TObject);
    procedure CheckListToggle(Sender: TObject);
    procedure Optimze(Sender: TObject);
    procedure Check(Sender: TObject);
    procedure Analyze(Sender: TObject);
    procedure Repair(Sender: TObject);
    procedure showresult(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TablesCheckListBoxClickCheck(Sender: TObject);
  private
    { Private declarations }
    MyResult  : PMYSQL_RES;
  public
    { Public declarations }
  end;

var
  optimize: Toptimize;

implementation
uses childwin, helpers, main;
{$R *.DFM}

procedure Toptimize.Button3Click(Sender: TObject);
begin
  close;
end;

procedure Toptimize.FormShow(Sender: TObject);
var
  i : Integer;
  tn : TTreeNode;
begin
  // FormShow!

  // read dbs and Tables from treeview
  DBComboBox.Items.Clear;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Level = 1 then
        DBComboBox.Items.Add(tn.Text);
    end;
    DBComboBox.ItemIndex := 0;
    for i:=0 to DBComboBox.Items.Count-1 do
    begin
      if DBComboBox.Items[i] = ActualDatabase then
        DBComboBox.ItemIndex := i;
    end;
  end;
  if DBComboBox.ItemIndex = -1 then
    DBComboBox.ItemIndex := 0;
  DBComboBox.OnChange(self);
end;

procedure Toptimize.DBComboBoxChange(Sender: TObject);
var
  tn, child : TTreeNode;
  i,j : Integer;
begin
  // read tables from db
  TablesCheckListBox.Items.Clear;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Text = DBComboBox.Text then
      begin
        child := tn.getFirstChild;
        for j:=0 to tn.Count-1 do
        begin
          TablesCheckListBox.Items.Add(child.Text);
          child := tn.getNextChild(child);
        end;
      end;
    end;
  end;
  // select all:
  for i:=0 to TablesCheckListBox.Items.Count-1 do
    TablesCheckListBox.checked[i] := true;
end;


procedure Toptimize.CheckListToggle(Sender: TObject);
begin
  // select all/none
  ToggleCheckListBox(TablesCheckListBox, ((Sender as TControl).Tag = 1));
  TablesCheckListBox.OnClickCheck(self);
end;


procedure Toptimize.Optimze(Sender: TObject);
var i : Integer;
begin
  screen.Cursor := crSQLWait;
  ListViewResults.Columns.BeginUpdate();
  ListViewResults.Columns.Clear;
  ListViewResults.Items.BeginUpdate();
  ListViewResults.Items.Clear;
  ListViewResults.Columns.EndUpdate();
  ListViewResults.Items.EndUpdate();
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    mysql_select_db(mysql, pchar(self.DBComboBox.Text));
    for i:=0 to self.TablesCheckListBox.Items.Count - 1 do
    begin
      if TablesCheckListBox.Checked[i] then
        q('OPTIMIZE TABLE ' + TablesCheckListBox.Items[i]);
    end;
  end;
  screen.Cursor := crDefault;
end;

procedure Toptimize.Check(Sender: TObject);
var
  i : Integer;
  checkedtables : TStrings;
  querystr  : String;
begin
  screen.Cursor := crSQLWait;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    checkedtables := TStringList.Create;
    with self.TablesCheckListBox do
      for i:=0 to Items.Count - 1 do
        if Checked[i] then
          checkedtables.Add(Items[i]);
    querystr := 'CHECK TABLE ' + implodestrs(',', checkedtables);
    if CheckBoxQuickCheck.Checked then
      querystr := querystr + ' TYPE = QUICK';
    mysql_select_db(mysql, pchar(self.DBComboBox.Text));
    self.myresult := q(querystr);
    showresult(self);
  end;
  screen.Cursor := crDefault;
end;

procedure Toptimize.Analyze(Sender: TObject);
var
  i : Integer;
  checkedtables : TStrings;
  querystr  : String;
begin
  screen.Cursor := crSQLWait;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    checkedtables := TStringList.Create;
    with self.TablesCheckListBox do
      for i:=0 to Items.Count - 1 do
        if Checked[i] then
          checkedtables.Add(Items[i]);
    querystr := 'ANALYZE TABLE ' + implodestrs(',', checkedtables);
    mysql_select_db(mysql, pchar(self.DBComboBox.Text));
    self.myresult := q(querystr);
    showresult(self);
  end;
  screen.Cursor := crDefault;
end;

procedure Toptimize.Repair(Sender: TObject);
var
  i : Integer;
  checkedtables : TStrings;
  querystr  : String;
begin
  screen.Cursor := crSQLWait;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    checkedtables := TStringList.Create;
    with self.TablesCheckListBox do
      for i:=0 to Items.Count - 1 do
        if Checked[i] then
          checkedtables.Add(Items[i]);
    querystr := 'REPAIR TABLE ' + implodestrs(',', checkedtables);
    if CheckBoxQuickRepair.Checked then
      querystr := querystr + ' TYPE = QUICK';
    mysql_select_db(mysql, pchar(self.DBComboBox.Text));
    self.myresult := q(querystr);
    showresult(self);
  end;
  screen.Cursor := crDefault;
end;

procedure Toptimize.showresult(Sender: TObject);
var
  i,j,fieldcount : Integer;
  Field        : PMYSQL_FIELD;
  row          : PMYSQL_ROW;
  li           : TListItem;
  lc           : TListColumn;
begin
  ListViewResults.Columns.BeginUpdate();
  ListViewResults.Columns.Clear;
  ListViewResults.Items.BeginUpdate();
  ListViewResults.Items.Clear;
  if MyResult <> nil then
  begin
    fieldcount := mysql_num_fields(MyResult);
    for i := 0 to fieldcount -1 do
    begin
      Field  := mysql_fetch_field(self.MyResult);
      lc := ListViewResults.Columns.Add;
      lc.Caption := Field.name;
    end;

    for i := 1 to mysql_num_rows(MyResult) do
    begin
      row := mysql_fetch_row(MyResult);
      li := ListViewResults.Items.Add;
      li.Caption := row[0];
      for j := 1 to fieldcount -1 do // fill cells
        li.SubItems.Add(row[j]);
    end;

    for i := 0 to ListViewResults.Columns.Count-1 do
      ListViewResults.Columns[i].Width := -2;
  end;
  ListViewResults.Columns.EndUpdate();
  ListViewResults.Items.EndUpdate();
end;

procedure Toptimize.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
    mysql_select_db(mysql, pchar(ActualDatabase));
end;

procedure Toptimize.TablesCheckListBoxClickCheck(Sender: TObject);
var
   i : Integer;
   somechecked : Boolean;
begin
  somechecked := false;
  for i:=0 to TablesCheckListBox.Items.Count-1 do begin
    if TablesCheckListBox.Checked[i] then begin
      somechecked := true;
      break;
    end;
  end;
  // en-/disable buttons/checkboxes:
  BitBtn1.Enabled := somechecked;
  BitBtn2.Enabled := somechecked;
  BitBtn3.Enabled := somechecked;
  BitBtn4.Enabled := somechecked;
  CheckBoxQuickRepair.Enabled := somechecked;
  CheckBoxQuickCheck.Enabled := somechecked;

end;

end.
