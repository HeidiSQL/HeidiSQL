unit optimizetables;


// -------------------------------------
// Table-diagnostics
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, CheckLst, comctrls, Buttons, ToolWin, Db;

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
    procedure showresult(Sender: TObject; ds: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TablesCheckListBoxClickCheck(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function TableDiagnosticsWindow (AOwner : TComponent; Flags : String = '') : Boolean;


implementation
uses childwin, helpers, main;
{$R *.DFM}

function TableDiagnosticsWindow (AOwner : TComponent; Flags : String = '') : Boolean;
var
  f : Toptimize;
begin
  f := Toptimize.Create(AOwner);
  f.ShowModal;
  Result := True;
  FreeAndNil (f);
end;

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
  for i:=0 to Mainform.ChildWin.DBTree.Items.Count-1 do
  begin
    tn := Mainform.ChildWin.DBTree.Items[i];
    if tn.Level = 1 then
      DBComboBox.Items.Add(tn.Text);
  end;
  DBComboBox.ItemIndex := 0;
  for i:=0 to DBComboBox.Items.Count-1 do
  begin
    if DBComboBox.Items[i] = Mainform.ChildWin.ActualDatabase then
      DBComboBox.ItemIndex := i;
  end;
  if DBComboBox.ItemIndex = -1 then
    DBComboBox.ItemIndex := 0;
  DBComboBox.OnChange(self);
end;

procedure Toptimize.DBComboBoxChange(Sender: TObject);
begin
  // read tables from db
  TablesCheckListBox.Items := Mainform.ChildWin.GetCol( 'SHOW TABLES FROM ' + MainForm.mask(DBComboBox.Text) );
  // Check all
  ToggleCheckListBox( TablesCheckListBox, True );
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
  Mainform.ChildWin.ExecUseQuery( self.DBComboBox.Text );
  for i:=0 to TablesCheckListBox.Items.Count - 1 do
  begin
    if TablesCheckListBox.Checked[i] then
      Mainform.ChildWin.ExecUpdateQuery('OPTIMIZE TABLE ' + mainform.mask(TablesCheckListBox.Items[i]));
  end;
  screen.Cursor := crDefault;
end;

procedure Toptimize.Check(Sender: TObject);
var
  i : Integer;
  checkedtables : TStringList;
  querystr  : String;
  ds: TDataSet;
begin
  screen.Cursor := crSQLWait;
  checkedtables := TStringList.Create;
  with TablesCheckListBox do
    for i:=0 to Items.Count - 1 do
      if Checked[i] then
        checkedtables.Add(mainform.mask(Items[i]));
  querystr := 'CHECK TABLE ' + implodestr(',', checkedtables);
  if CheckBoxQuickCheck.Checked then
    querystr := querystr + ' QUICK';
  Mainform.ChildWin.ExecUseQuery( self.DBComboBox.Text );
  ds := Mainform.ChildWin.GetResults( querystr );
  showresult(self, ds);
  screen.Cursor := crDefault;
end;

procedure Toptimize.Analyze(Sender: TObject);
var
  i : Integer;
  checkedtables : TStringList;
  querystr  : String;
  ds: TDataset;
begin
  screen.Cursor := crSQLWait;
  checkedtables := TStringList.Create;
  with TablesCheckListBox do
    for i:=0 to Items.Count - 1 do
      if Checked[i] then
        checkedtables.Add(mainform.mask(Items[i]));
  querystr := 'ANALYZE TABLE ' + implodestr(',', checkedtables);
  Mainform.ChildWin.ExecUseQuery( self.DBComboBox.Text );
  ds := Mainform.ChildWin.GetResults( querystr );
  showresult(self, ds);
  screen.Cursor := crDefault;
end;

procedure Toptimize.Repair(Sender: TObject);
var
  i : Integer;
  checkedtables : TStringList;
  querystr  : String;
  ds: TDataSet;
begin
  screen.Cursor := crSQLWait;
  checkedtables := TStringList.Create;
  with TablesCheckListBox do
    for i:=0 to Items.Count - 1 do
      if Checked[i] then
        checkedtables.Add(mainform.mask(Items[i]));
  querystr := 'REPAIR TABLE ' + implodestr(',', checkedtables);
  if CheckBoxQuickRepair.Checked then
    querystr := querystr + ' QUICK';
  Mainform.ChildWin.ExecUseQuery( self.DBComboBox.Text );
  ds := Mainform.ChildWin.GetResults( querystr );
  showresult(self, ds);
  screen.Cursor := crDefault;
end;

procedure Toptimize.showresult(Sender: TObject; ds: TDataSet);
var
  i,j,fieldcount : Integer;
  li           : TListItem;
  lc           : TListColumn;
begin
  ListViewResults.Columns.BeginUpdate();
  ListViewResults.Columns.Clear;
  ListViewResults.Items.BeginUpdate();
  ListViewResults.Items.Clear;
  fieldcount := ds.FieldCount;
  for i := 0 to fieldcount -1 do
  begin
    lc := ListViewResults.Columns.Add;
    lc.Caption := ds.Fields[i].Fieldname;
  end;

  for i:=1 to ds.RecordCount do
  begin
    li := ListViewResults.Items.Add;
    li.Caption := ds.Fields[0].AsString;
    for j := 1 to fieldcount -1 do // fill cells
      li.SubItems.Add(ds.Fields[j].AsString);
    ds.Next;
  end;

  for i := 0 to ListViewResults.Columns.Count-1 do
    ListViewResults.Columns[i].Width := -2;

  ListViewResults.Columns.EndUpdate();
  ListViewResults.Items.EndUpdate();
end;

procedure Toptimize.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // catch exception to be sure window can close
  try
    if Mainform.ChildWin.ActualDatabase <> '' then
      Mainform.ChildWin.ExecUseQuery( Mainform.ChildWin.ActualDatabase );
  except
  end;

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
