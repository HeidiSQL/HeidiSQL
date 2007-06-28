unit column_selection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, Registry;

type
  TColumnSelectionForm = class(TForm)
    pnlBevel: TPanel; // This panel has the only duty to display a normal border at the edges of the form 
    btnCancel: TButton;
    btnOK: TButton;
    chkSelectAll: TCheckBox;
    chklistColumns: TCheckListBox;
    procedure FormShow(Sender: TObject);
    procedure chklistColumnsClickCheck(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ColumnSelectionForm: TColumnSelectionForm;
  reg_name : String;

implementation

uses MAIN, helpers;


{$R *.dfm}


{**
  FormShow
}
procedure TColumnSelectionForm.FormShow(Sender: TObject);
var
  i : Integer;
  reg : TRegistry;
  reg_columns : TStringList;
begin
  // Take column names from listColumns and add here
  chklistColumns.Items.Clear;
  for i := 0 to Mainform.Childwin.listColumns.Items.Count-1 do
  begin
    chklistColumns.Items.Add( Mainform.Childwin.listColumns.Items[i].Caption );
  end;

  // Set global reg_name (also used in btnOKClick)
  reg_name := REGNAME_DISPLAYEDCOLUMNS + '_' + Mainform.Childwin.ActualDatabase + '.' + Mainform.Childwin.ActualTable;

  // Read reg value and check items!
  reg := TRegistry.Create;
  reg.OpenKey( REGPATH + '\Servers\' + Mainform.Childwin.Conn.Description, true );
  reg_columns := explode( '`', reg.ReadString( reg_name ) );
  if reg_columns.Count = 0 then
  begin
    // Simply check all items
    ToggleCheckListBox( chklistColumns, True );
  end
  else
  begin
    // Only check selected items
    ToggleCheckListBox( chklistColumns, True, reg_columns );
  end;

  // Call check-event to update state of "Select / Deselect all" checkbox
  chklistColumnsClickCheck( Sender );
end;


{**
  Cancel clicked
}
procedure TColumnSelectionForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
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
  reg.OpenKey( REGPATH + '\Servers\' + Mainform.Childwin.Conn.Description, true );

  // Set initial values
  reg_oldvalue := '';
  reg_newvalue := '';
  if reg.ValueExists( reg_name ) then
  begin
    reg_oldvalue := reg.ReadString( reg_name );
  end;

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

  try
    // Reload grid-data if column selection has changed
    if reg_oldvalue <> reg_newvalue then
      Mainform.Childwin.viewdata( Sender );
  finally
    ModalResult := mrOk;
  end;

end;


{**
  Form is closing
}
procedure TColumnSelectionForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Mainform.Childwin.btnColumnSelection.Down := False;
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


end.
