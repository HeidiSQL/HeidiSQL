unit optimizetables;


// -------------------------------------
// Table-diagnostics
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, CheckLst, comctrls, Buttons, ToolWin, Db, Registry,
  WideStrings, TntCheckLst, TntStdCtrls, WideStrUtils;

type
  Toptimize = class(TForm)
    TablesCheckListBox: TTNTCheckListBox;
    DBComboBox: TTnTComboBox;
    lblSelect: TLabel;
    btnClose: TButton;
    cbxQuickRepair: TCheckBox;
    cbxQuickCheck: TCheckBox;
    btnOptimize: TButton;
    btnCheck: TButton;
    btnAnalyze: TButton;
    btnRepair: TButton;
    lblResults: TLabel;
    ListResults: TListView;
    tlbCheckToggle: TToolBar;
    tlbCheckNone: TToolButton;
    tlbCheckAll: TToolButton;
    cbxExtendedCheck: TCheckBox;
    cbxExtendedRepair: TCheckBox;
    btnHelp: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DBComboBoxChange(Sender: TObject);
    procedure CheckListToggle(Sender: TObject);
    procedure Optimize(Sender: TObject);
    procedure Check(Sender: TObject);
    procedure Analyze(Sender: TObject);
    procedure Repair(Sender: TObject);
    procedure ClearResults;
    procedure AddResults(ds: TDataSet);
    procedure btnHelpClick(Sender: TObject);
    procedure TablesCheckListBoxClickCheck(Sender: TObject);
    procedure RunIterated(pseudoSql: WideString);
    procedure ValidateControls;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses childwin, helpers, main;

{$R *.DFM}


{**
  FormCreate: Restore GUI setup
}
procedure Toptimize.FormCreate(Sender: TObject);
begin
  Width := Mainform.GetRegValue(REGNAME_MAINTWINWIDTH, Width);
  Height := Mainform.GetRegValue(REGNAME_MAINTWINHEIGHT, Height);
  SetWindowSizeGrip( Self.Handle, True );
end;


{**
  FormDestroy: Save GUI setup
}
procedure Toptimize.FormDestroy(Sender: TObject);
var
  reg : TRegistry;
begin
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, False) then begin
    reg.WriteInteger( REGNAME_MAINTWINWIDTH, Width );
    reg.WriteInteger( REGNAME_MAINTWINHEIGHT, Height );
    reg.CloseKey;
  end;
  reg.Free;
  Close;
end;


{**
  FormShow: Fill DB combobox and tables list
}
procedure Toptimize.FormShow(Sender: TObject);
var
  Selected: TWideStringList;
begin
  // read dbs and Tables from treeview
  DBComboBox.Items.Clear;
  DBComboBox.Items.Assign(Mainform.ChildWin.Databases);
  DBComboBox.ItemIndex := DBComboBox.Items.IndexOf( Mainform.ChildWin.ActiveDatabase );
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


{**
  DB selected in pulldown
}
procedure Toptimize.DBComboBoxChange(Sender: TObject);
var
  ds: TDataset;
begin
  // read tables from db
  ds := Mainform.ChildWin.FetchDbTableList(DBComboBox.Text);
  TablesCheckListBox.Items.Clear;
  while not ds.Eof do begin
    TablesCheckListBox.Items.Add(ds.Fields[0].AsWideString);
    ds.Next;
  end;
  // Check all
  ToggleCheckListBox( TablesCheckListBox, True );
  // Enable controls if there are tables in the database.
  ValidateControls;
end;


{**
  Check all/none
}
procedure Toptimize.CheckListToggle(Sender: TObject);
begin
  // select all/none
  ToggleCheckListBox(TablesCheckListBox, ((Sender as TControl).Tag = 1));
  TablesCheckListBox.OnClickCheck(self);
end;


{**
  Parse and run SQL template for all maintenance actions
}
procedure Toptimize.RunIterated(pseudoSql: WideString);
var
  i: integer;
  ds: TDataSet;
  sql: WideString;
begin
  Screen.Cursor := crSQLWait;
  Mainform.ChildWin.TemporaryDatabase := self.DBComboBox.Text;
  ClearResults;
  try
    for i := 0 to TablesCheckListBox.Items.Count - 1 do begin
      if TablesCheckListBox.Checked[i] then begin
        sql := WideStringReplace(pseudoSql, '$table', mainform.mask(TablesCheckListBox.Items[i]), [rfReplaceAll]);
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


{**
  Optimize tables
}
procedure Toptimize.Optimize(Sender: TObject);
begin
  RunIterated('OPTIMIZE TABLE $table');
end;


{**
  Check tables
}
procedure Toptimize.Check(Sender: TObject);
var
  querystr  : WideString;
begin
  querystr := 'CHECK TABLE $table';
  if cbxQuickCheck.Checked then
    querystr := querystr + ' QUICK';
  if cbxExtendedCheck.Checked then
    querystr := querystr + ' EXTENDED';
  RunIterated(querystr);
end;


{**
  Analyze tables
}
procedure Toptimize.Analyze(Sender: TObject);
begin
  RunIterated('ANALYZE TABLE $table');
end;


{**
  Repair tables
}
procedure Toptimize.Repair(Sender: TObject);
var
  querystr  : WideString;
begin
  querystr := 'REPAIR TABLE $table';
  if cbxQuickRepair.Checked then
    querystr := querystr + ' QUICK';
  if cbxExtendedRepair.Checked then
    querystr := querystr + ' EXTENDED';
  RunIterated(querystr);
end;


{**
  Clear ListResults
}
procedure Toptimize.ClearResults;
begin
  ListResults.Columns.BeginUpdate;
  ListResults.Columns.Clear;
  ListResults.Items.BeginUpdate;
  ListResults.Items.Clear;
  ListResults.Columns.EndUpdate;
  ListResults.Items.EndUpdate;
end;


{**
  Add results from maintenance action
}
procedure Toptimize.AddResults(ds: TDataSet);
var
  i,j,fieldcount : Integer;
  li           : TListItem;
  lc           : TListColumn;
begin
  fieldcount := ds.FieldCount;
  if fieldcount > ListResults.Columns.Count then begin
    ListResults.Columns.BeginUpdate();
    for i := ListResults.Columns.Count to fieldcount - 1 do begin
      lc := ListResults.Columns.Add;
      lc.Caption := ds.Fields[i].Fieldname;
    end;
    ListResults.Columns.EndUpdate;
  end;

  ListResults.Items.BeginUpdate;
  for i:=1 to ds.RecordCount do
  begin
    li := ListResults.Items.Add;
    // Todo: TListView is not unicode safe, switch to VTree
    li.Caption := ds.Fields[0].AsString;
    for j := 1 to fieldcount -1 do // fill cells
      li.SubItems.Add(ds.Fields[j].AsString);
    ds.Next;
  end;

  for i := 0 to ListResults.Columns.Count-1 do
    ListResults.Columns[i].Width := -2;

  ListResults.Items[ListResults.Items.Count - 1].MakeVisible(false);
  ListResults.Items.EndUpdate;
end;

procedure Toptimize.btnHelpClick(Sender: TObject);
begin
  Mainform.CallSQLHelpWithKeyword('OPTIMIZE');
end;


{**
  Table was (un-)checked: ensure correct state of buttons
}
procedure Toptimize.TablesCheckListBoxClickCheck(Sender: TObject);
begin
  ValidateControls;
end;


{**
  Check if at least one table is checked and dis/enable the buttons
}
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
