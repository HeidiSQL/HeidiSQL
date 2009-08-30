unit column_selection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, TntCheckLst, WideStrings, DB;

type
  TColumnSelectionForm = class(TForm)
    pnlBevel: TPanel; // This panel has the only duty to display a normal border at the edges of the form 
    btnCancel: TButton;
    btnOK: TButton;
    chkSelectAll: TCheckBox;
    chklistColumns: TTNTCheckListBox;
    chkSort: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chklistColumnsClickCheck(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkSortClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  reg_name : String;

implementation

uses MAIN, helpers;


{$R *.dfm}



procedure TColumnSelectionForm.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
end;


{**
  FormShow
}
procedure TColumnSelectionForm.FormShow(Sender: TObject);
var
  ds: TDataSet;
begin
  ds := Mainform.SelectedTableColumns;
  ds.First;
  while not ds.Eof do begin
    chklistColumns.Items.Add(ds.Fields[0].AsWideString);
    ds.Next;
  end;

  // Check items!
  if Mainform.FDataGridSelect.Count = 0 then // Simply check all items
    ToggleCheckListBox( chklistColumns, True )
  else // Only check selected items
    ToggleCheckListBox( chklistColumns, True, Mainform.FDataGridSelect );

  // Call check-event to update state of "Select / Deselect all" checkbox
  chklistColumnsClickCheck( Sender );

  // Restore last used sorting state from registry
  chkSort.Checked := GetRegValue(REGNAME_SORTDISPLAYEDCOLUMNS, chkSort.Checked);
end;


{**
  OK clicked
}
procedure TColumnSelectionForm.btnOKClick(Sender: TObject);
var
  i : Integer;
begin
  // Prepare string for storing in registry.
  // Use quote-character as separator to ensure columnnames can
  // be extracted safely later
  Mainform.FDataGridSelect.Clear;
  for i := 0 to chklistColumns.Items.Count - 1 do
  begin
    if chklistColumns.Checked[i] then
      Mainform.FDataGridSelect.Add(chklistColumns.Items[i]);
  end;

  // If all columns were selected, delete existing superflous reg-value
  // Otherwise, save value
  if Mainform.FDataGridSelect.Count = chklistColumns.Items.Count then
    Mainform.FDataGridSelect.Clear;

  Mainform.viewdata(Sender);

  btnCancel.OnClick(Sender);
end;


{**
  Select / Deselect all
}
procedure TColumnSelectionForm.chkSelectAllClick(Sender: TObject);
begin
  // Avoid executing when checkbox was toggled by code (see proc below)
  if (Sender as TCheckBox).Focused then
    ToggleCheckListBox( chklistColumns, (Sender as TCheckBox).Checked );
end;


{**
  Click within column list
  Updates state of "Select / deselect all" checkbox
}
procedure TColumnSelectionForm.chklistColumnsClickCheck(Sender: TObject);
var
  i : Integer;
  allSelected, noneSelected : Boolean;
begin
  allselected := True;
  noneSelected := True;
  for i := 0 to chklistColumns.Items.Count - 1 do
  begin
    if chklistColumns.Checked[i] then
    begin
      noneSelected := False;
    end
    else
    begin
      allSelected := False;
    end;
  end;
  if noneSelected then
    chkSelectAll.State := cbUnchecked
  else if allSelected then
    chkSelectAll.State := cbChecked
  else
    chkSelectAll.State := cbGrayed;

end;


{**
  Sort / Unsort the list with fields
}
procedure TColumnSelectionForm.chkSortClick(Sender: TObject);
var
  checkedfields : TStringList;
  i: Integer;
  ds: TDataSet;
begin
  // Memorize checked items in a list
  checkedfields := TStringList.Create;
  for i := 0 to chklistColumns.Items.Count - 1 do begin
    if chklistColumns.Checked[i] then
      checkedfields.Add(chklistColumns.Items[i]);
  end;

  chklistColumns.Sorted := TCheckBox(Sender).Checked;

  // Setting Sorted to false doesn't resort anything in the list.
  // So we have to add all items again in original order
  if not chklistColumns.Sorted then begin
    // Add all fieldnames again
    chklistColumns.Items.BeginUpdate;
    chklistColumns.Clear;
    ds := Mainform.SelectedTableColumns;
    ds.First;
    while not ds.Eof do begin
      chklistColumns.Items.Add(ds.Fields[0].AsWideString);
      ds.Next;
    end;
    chklistColumns.Items.EndUpdate;
  end;

  // check those which are in the checkedfields list
  for i := 0 to chklistColumns.Items.Count-1 do begin
    chklistColumns.Checked[i] := checkedfields.IndexOf( chklistColumns.Items[i] ) > -1;
  end;
end;


procedure TColumnSelectionForm.btnCancelClick(Sender: TObject);
begin
  Mainform.tbtnDataColumns.Down := False;
  Close;
end;


{**
  Cancel this dialog if the user clicks elsewhere on mainform
}
procedure TColumnSelectionForm.FormDeactivate(Sender: TObject);
begin
  btnCancel.OnClick(Sender);
end;


{**
  Be sure the form is destroyed after closing.
}
procedure TColumnSelectionForm.FormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  Action := caFree;
end;


end.
