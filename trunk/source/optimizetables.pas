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
    btnOptimize: TButton;
    btnCheck: TButton;
    btnAnalyze: TButton;
    btnRepair: TButton;
    Label3: TLabel;
    ListViewResults: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    cbxExtendedCheck: TCheckBox;
    cbxExtendedRepair: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure DBComboBoxChange(Sender: TObject);
    procedure CheckListToggle(Sender: TObject);
    procedure Optimze(Sender: TObject);
    procedure Check(Sender: TObject);
    procedure Analyze(Sender: TObject);
    procedure Repair(Sender: TObject);
    procedure ClearResults;
    procedure AddResults(ds: TDataSet);
    procedure TablesCheckListBoxClickCheck(Sender: TObject);
    procedure RunIterated(pseudoSql: string);
    procedure ValidateControls;
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

procedure Toptimize.FormShow(Sender: TObject);
var
  i : Integer;
  tn : TTreeNode;
  Selected: TStringList;
begin
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
    if DBComboBox.Items[i] = Mainform.ChildWin.ActiveDatabase then
      DBComboBox.ItemIndex := i;
  end;
  if DBComboBox.ItemIndex = -1 then
    DBComboBox.ItemIndex := 0;
  DBComboBox.OnChange(self);

  // Fetch selected tables in list and preselect them in the checkboxlist
  Selected := GetVTCaptions( Mainform.ChildWin.ListTables, True );
  if Selected.Count > 0 then
    ToggleCheckListBox(TablesCheckListBox, True, Selected)
  else // Select all in checkboxlist if no table is preselected 
    ToggleCheckListBox(TablesCheckListBox, True);
end;

procedure Toptimize.DBComboBoxChange(Sender: TObject);
begin
  // read tables from db
  TablesCheckListBox.Items := Mainform.ChildWin.GetCol( 'SHOW TABLES FROM ' + MainForm.mask(DBComboBox.Text) );
  // Check all
  ToggleCheckListBox( TablesCheckListBox, True );
  // Enable controls if there are tables in the database.
  ValidateControls;
end;


procedure Toptimize.CheckListToggle(Sender: TObject);
begin
  // select all/none
  ToggleCheckListBox(TablesCheckListBox, ((Sender as TControl).Tag = 1));
  TablesCheckListBox.OnClickCheck(self);
end;

procedure Toptimize.RunIterated(pseudoSql: string);
var
  i: integer;
  ds: TDataSet;
  sql: string;
begin
  Screen.Cursor := crSQLWait;
  Mainform.ChildWin.TemporaryDatabase := self.DBComboBox.Text;
  ClearResults;
  try
    for i := 0 to TablesCheckListBox.Items.Count - 1 do begin
      if TablesCheckListBox.Checked[i] then begin
        sql := StringReplace(pseudoSql, '$table', mainform.mask(TablesCheckListBox.Items[i]), [rfReplaceAll]);
        ds := Mainform.ChildWin.GetResults(sql);
        AddResults(ds);
        ds.Close;
        FreeAndNil(ds);
        TablesCheckListBox.Checked[i] := false;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    Mainform.ChildWin.TemporaryDatabase := '';
    ValidateControls;
  end;
end;

procedure Toptimize.Optimze(Sender: TObject);
begin
  RunIterated('OPTIMIZE TABLE $table');
end;

procedure Toptimize.Check(Sender: TObject);
var
  querystr  : String;
begin
  querystr := 'CHECK TABLE $table';
  if CheckBoxQuickCheck.Checked then
    querystr := querystr + ' QUICK';
  if cbxExtendedCheck.Checked then
    querystr := querystr + ' EXTENDED';
  RunIterated(querystr);
end;

procedure Toptimize.Analyze(Sender: TObject);
begin
  RunIterated('ANALYZE TABLE $table');
end;

procedure Toptimize.Repair(Sender: TObject);
var
  querystr  : String;
begin
  querystr := 'REPAIR TABLE $table';
  if CheckBoxQuickRepair.Checked then
    querystr := querystr + ' QUICK';
  if cbxExtendedRepair.Checked then
    querystr := querystr + ' EXTENDED';
  RunIterated(querystr);
end;

procedure Toptimize.ClearResults;
begin
  ListViewResults.Columns.BeginUpdate();
  ListViewResults.Columns.Clear;
  ListViewResults.Items.BeginUpdate();
  ListViewResults.Items.Clear;
  ListViewResults.Columns.EndUpdate();
  ListViewResults.Items.EndUpdate();
end;

procedure Toptimize.AddResults(ds: TDataSet);
var
  i,j,fieldcount : Integer;
  li           : TListItem;
  lc           : TListColumn;
begin
  fieldcount := ds.FieldCount;
  if fieldcount > ListViewResults.Columns.Count then begin
    ListViewResults.Columns.BeginUpdate();
    for i := ListViewResults.Columns.Count to fieldcount - 1 do begin
      lc := ListViewResults.Columns.Add;
      lc.Caption := ds.Fields[i].Fieldname;
    end;
    ListViewResults.Columns.EndUpdate();
  end;

  ListViewResults.Items.BeginUpdate();
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

  ListViewResults.Items[ListViewResults.Items.Count - 1].MakeVisible(false);
  ListViewResults.Items.EndUpdate();
end;

procedure Toptimize.TablesCheckListBoxClickCheck(Sender: TObject);
begin
  ValidateControls;
end;

procedure Toptimize.ValidateControls;
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
  btnOptimize.Enabled := somechecked;
  btnCheck.Enabled := somechecked;
  btnAnalyze.Enabled := somechecked;
  btnRepair.Enabled := somechecked;
end;

end.
