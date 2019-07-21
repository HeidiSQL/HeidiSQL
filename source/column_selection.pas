unit column_selection;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, CheckLst, ExtCtrls, SysUtils,
  apphelpers, gnugettext, extra_controls;

type
  TColumnSelectionForm = class(TExtForm)
    btnCancel: TButton;
    btnOK: TButton;
    chkSelectAll: TCheckBox;
    chklistColumns: TCheckListBox;
    chkSort: TCheckBox;
    editFilter: TButtonedEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chklistColumnsClickCheck(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure PopulateList(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure editFilterLeftButtonClick(Sender: TObject);
  private
    { Private declarations }
    FCheckedColumns: TStringList;
    FLastFilter: String;
  public
    { Public declarations }
  end;


implementation

uses main;


{$R *.dfm}



procedure TColumnSelectionForm.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
  Width := AppSettings.ReadInt(asColumnSelectorWidth);
  Height := AppSettings.ReadInt(asColumnSelectorHeight);
  FCheckedColumns := TStringList.Create;
end;


procedure TColumnSelectionForm.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asColumnSelectorWidth, Width);
  AppSettings.WriteInt(asColumnSelectorHeight, Height);
  FCheckedColumns.Free;
end;


{**
  FormShow
}
procedure TColumnSelectionForm.FormShow(Sender: TObject);
var
  i: Integer;
  Col: String;
begin
  FCheckedColumns.Clear;
  for i:=0 to Mainform.SelectedTableColumns.Count-1 do begin
    Col := Mainform.SelectedTableColumns[i].Name;
    chklistColumns.Items.Add(Col);
    if (Mainform.DataGridHiddenColumns.Count = 0) or
      (Mainform.DataGridHiddenColumns.IndexOf(chklistColumns.Items[i]) = -1)
      then begin
      FCheckedColumns.Add(Col);
      chklistColumns.Checked[i] := True;
    end;
  end;

  // Call check-event to update state of "Select / Deselect all" checkbox
  chklistColumnsClickCheck( Sender );

  // Restore last used sorting state from registry
  chkSort.Checked := AppSettings.ReadBool(asDisplayedColumnsSorted);
end;


{**
  OK clicked
}
procedure TColumnSelectionForm.btnOKClick(Sender: TObject);
var
  i: Integer;
  Col: String;
begin
  // Prepare string for storing in registry.
  // Use quote-character as separator to ensure columnnames can
  // be extracted safely later
  Mainform.DataGridHiddenColumns.Clear;
  for i:=0 to Mainform.SelectedTableColumns.Count-1 do begin
    Col := Mainform.SelectedTableColumns[i].Name;
    if FCheckedColumns.IndexOf(Col) = -1 then
      Mainform.DataGridHiddenColumns.Add(Col);
  end;
  InvalidateVT(Mainform.DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
  btnCancel.OnClick(Sender);
end;


{**
  Select / Deselect all
}
procedure TColumnSelectionForm.chkSelectAllClick(Sender: TObject);
var
  cb: TCheckBox;
  i: Integer;
begin
  // Avoid executing when checkbox was toggled by code (see proc below)
  cb := Sender as TCheckBox;
  if cb.Focused then begin
    chklistColumns.CheckAll(cb.State);
    for i:=0 to chklistColumns.Items.Count-1 do begin
      if (FCheckedColumns.IndexOf(chklistColumns.Items[i]) = -1) and (cb.State = cbChecked) then
        FCheckedColumns.Add(chklistColumns.Items[i]);
      if (FCheckedColumns.IndexOf(chklistColumns.Items[i]) > -1) and (cb.State = cbUnchecked) then
        FCheckedColumns.Delete(FCheckedColumns.IndexOf(chklistColumns.Items[i]));
    end;
  end;
end;


procedure TColumnSelectionForm.editFilterLeftButtonClick(Sender: TObject);
begin
  if IsNotEmpty(editFilter.Text) then begin
    FLastFilter := editFilter.Text;
    editFilter.Text := '';
  end else if IsNotEmpty(FLastFilter) then begin
    editFilter.Text := FLastFilter;
    FLastFilter := '';
  end;
end;


{**
  Click within column list
  Updates state of "Select / deselect all" checkbox
}
procedure TColumnSelectionForm.chklistColumnsClickCheck(Sender: TObject);
var
  i : Integer;
  AllSelected, NoneSelected : Boolean;
  FocusedItem: String;
  FocusedItemIndex: Integer;
begin
  // Add or remove clicked item from list
  if chklistColumns.ItemIndex > -1 then begin
    FocusedItem := chklistColumns.Items[chklistColumns.ItemIndex];
    if chklistColumns.Checked[chklistColumns.ItemIndex] then begin
      FCheckedColumns.Add(FocusedItem)
    end else begin
      FocusedItemIndex := FCheckedColumns.IndexOf(FocusedItem);
      if FocusedItemIndex > -1 then
        FCheckedColumns.Delete(FocusedItemIndex);
    end;
  end;

  Allselected := True;
  NoneSelected := True;
  for i:=0 to chklistColumns.Items.Count-1 do begin
    if chklistColumns.Checked[i] then
      NoneSelected := False
    else
      AllSelected := False;
  end;
  if NoneSelected then
    chkSelectAll.State := cbUnchecked
  else if AllSelected then
    chkSelectAll.State := cbChecked
  else
    chkSelectAll.State := cbGrayed;
end;


{**
  Sort / Unsort the list with fields
}
procedure TColumnSelectionForm.PopulateList(Sender: TObject);
var
  i: Integer;
  Col: String;
begin
  // Setting Sorted to false doesn't resort anything in the list.
  // So we have to add all items again in original order
  chklistColumns.Sorted := chkSort.Checked;
  // Add all fieldnames again
  chklistColumns.Items.BeginUpdate;
  chklistColumns.Clear;
  for i:=0 to Mainform.SelectedTableColumns.Count-1 do begin
    Col := Mainform.SelectedTableColumns[i].Name;
    if IsEmpty(editFilter.Text) or (Pos(LowerCase(editFilter.Text), LowerCase(Col)) > 0) then
      chklistColumns.Items.Add(Col);
  end;
  chklistColumns.Items.EndUpdate;

  // check those which remembered as checked
  for i:=0 to chklistColumns.Items.Count-1 do begin
    chklistColumns.Checked[i] := FCheckedColumns.IndexOf(chklistColumns.Items[i]) > -1;
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
