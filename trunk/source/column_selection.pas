unit column_selection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, Registry, TntCheckLst, WideStrings;

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
  reg_columns : TWideStringList;
begin
  // Take column names from listColumns and add here
  chklistColumns.Items.Text := GetVTCaptions(Mainform.Childwin.ListColumns).Text;

  // Set global reg_name (also used in btnOKClick)
  reg_name := REGNAME_DISPLAYEDCOLUMNS + '_' + Mainform.Childwin.ActiveDatabase + '.' + Mainform.Childwin.SelectedTable;

  // Read reg value and check items!
  reg_columns := explode( '`', Mainform.GetRegValue( reg_name, '', Mainform.Childwin.Conn.Description ) );
  if reg_columns.Count = 0 then // Simply check all items
    ToggleCheckListBox( chklistColumns, True )
  else // Only check selected items
    ToggleCheckListBox( chklistColumns, True, reg_columns );

  // Call check-event to update state of "Select / Deselect all" checkbox
  chklistColumnsClickCheck( Sender );

  // Restore last used sorting state from registry
  chkSort.Checked := Mainform.GetRegValue(REGNAME_SORTDISPLAYEDCOLUMNS, chkSort.Checked);
end;


{**
  OK clicked
}
procedure TColumnSelectionForm.btnOKClick(Sender: TObject);
var
  reg_oldvalue, reg_newvalue : String;
  i : Integer;
  allSelected : Boolean;
  reg : TRegistry;
begin
  // Open registry registry key
  reg := TRegistry.Create;
  reg.OpenKey( REGPATH + REGKEY_SESSIONS + Mainform.Childwin.Conn.Description, true );

  // Set initial values
  reg_oldvalue := Mainform.GetRegValue(reg_name, '', Mainform.Childwin.Conn.Description);
  reg_newvalue := '';

  // Prepare string for storing in registry.
  // Use quote-character as separator to ensure columnnames can
  // be extracted safely later
  allSelected := True;
  for i := 0 to chklistColumns.Items.Count - 1 do
  begin
    if chklistColumns.Checked[i] then
    begin
      if reg_newvalue <> '' then
        reg_newvalue := reg_newvalue + '`';
      reg_newvalue := reg_newvalue + chklistColumns.Items[i];
    end
    else
    begin
      allSelected := False;
    end;
  end;

  // If all columns were selected, delete existing superflous reg-value
  // Otherwise, save value
  if allSelected then
  begin
    if reg.ValueExists( reg_name ) then
      reg.DeleteValue( reg_name );
    reg_newvalue := '';
  end
  else
  begin
    reg.WriteString( reg_name, reg_newvalue );
  end;

  // Store state of sort-checkbox in registry
  reg.OpenKey( REGPATH, false );
  reg.WriteBool( REGNAME_SORTDISPLAYEDCOLUMNS, chkSort.checked );

  reg.CloseKey;
  FreeAndNil(reg);

  // Signalizes childwin to refresh grid-data
  if reg_oldvalue <> reg_newvalue then
    Mainform.Childwin.viewdata(Sender);

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
begin
  chklistColumns.Sorted := TCheckBox(Sender).Checked;

  // Setting Sorted to false doesn't resort anything in the list.
  // So we have to add all items again in original order
  if( not chklistColumns.Sorted ) then
  begin

    // Memorize checked items in a list
    checkedfields := TStringList.Create;
    for i := 0 to chklistColumns.Items.Count - 1 do
    begin
      if chklistColumns.Checked[i] then
        checkedfields.Add(chklistColumns.Items[i]);
    end;

    // Add all fieldnames again and check those which are in the checkedfields list
    chklistColumns.Items.BeginUpdate;
    chklistColumns.Items.Text := GetVTCaptions(Mainform.Childwin.ListColumns).Text;
    for i := 0 to chklistColumns.Items.Count-1 do
    begin
      if checkedfields.IndexOf( chklistColumns.Items[i] ) > -1 then
        chklistColumns.Checked[i] := True;
    end;
    chklistColumns.Items.EndUpdate;
  end;
end;


procedure TColumnSelectionForm.btnCancelClick(Sender: TObject);
begin
  Mainform.Childwin.tbtnDataColumns.Down := False;
  Close;
end;


{**
  Cancel this dialog if the user clicks elsewhere on childwin
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
