unit data_sorting;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Buttons,

  apphelpers, gnugettext;


type
  TDataSortingForm = class(TForm)
    pnlBevel: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnAddCol: TButton;
    btnReset: TButton;
    procedure btnAddColClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure DisplaySortingControls(Sender: TObject);
  private
    { Private declarations }
    ColumnNames : TStringList;
    OrderColumns : TOrderColArray;
    OldOrderClause : String;
    procedure dropdownColsChange( Sender: TObject );
    procedure buttonOrderClick( Sender: TObject );
    procedure buttonDeleteClick( Sender: TObject );
    procedure Modified;
  public
    { Public declarations }
  end;



implementation

uses main;

{$R *.dfm}


procedure TDataSortingForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  TranslateComponent(Self);

  ColumnNames := TStringList.Create;
  // Take column names from listColumns and add here
  for i:=0 to Mainform.SelectedTableColumns.Count-1 do begin
    ColumnNames.Add(Mainform.SelectedTableColumns[i].Name);
  end;

  OrderColumns := Mainform.DataGridSortColumns;
  OldOrderClause := ComposeOrderClause(OrderColumns);

  // First creation of controls
  DisplaySortingControls(Sender);
end;


{**
  Create controls for order columns
}
procedure TDataSortingForm.DisplaySortingControls(Sender: TObject);
var
  labelNumber: TLabel;
  buttonDelete: TButton;
  dropdownCols: TComboBox;
  buttonOrder: TSpeedButton;
  i, xPosition, topPosition, btnWidth : Integer;
  Margin: Integer;       // Space between controls
  MarginBig: Integer;    // Space above the very first and last controls, used to separate stuff
begin
  // Remove previously created components, which all have a tag > 0
  for i := ComponentCount - 1 downto 0 do begin
    if Components[i].Tag > 0 then
      Components[i].Free;
  end;

  Margin := Round(2 * DpiScaleFactor(Self));
  MarginBig := Round(6 * DpiScaleFactor(Self));

  // Set initial width to avoid resizing form to 0
  xPosition := pnlBevel.Width;
  topPosition := MarginBig;

  // Create line with controls for each order column
  // TODO: disable repaint on every created control. Sending WM_SETREDRAW=0 message creates artefacts.
  for i:=0 to Length(OrderColumns)-1 do
  begin
    xPosition := pnlBevel.BorderWidth;

    // 1. Label with number
    labelNumber := TLabel.Create(self);
    labelNumber.Parent := pnlBevel;
    labelNumber.AutoSize := False; // Avoids automatic changes to width + height
    labelNumber.Left := xPosition;
    labelNumber.Top := topPosition;
    labelNumber.Width := Round(15 * DpiScaleFactor(Self));
    labelNumber.Alignment := taRightJustify;
    labelNumber.Layout := tlCenter;
    labelNumber.Caption := IntToStr(i+1) + '.';
    labelNumber.Tag := i+1;
    Inc( xPosition, labelNumber.Width + Margin );

    // 2. Dropdown with columnnames
    dropdownCols := TComboBox.Create(self);
    dropdownCols.Parent := pnlBevel;
    dropdownCols.Width := Round(130 * DpiScaleFactor(Self));
    dropdownCols.Left := xPosition;
    dropdownCols.Top := topPosition;
    dropdownCols.Items.Text := ColumnNames.Text;
    dropdownCols.Style := csDropDownList; // Not editable
    dropdownCols.ItemIndex := ColumnNames.IndexOf(OrderColumns[i].ColumnName);
    dropdownCols.Tag := i+1;
    dropdownCols.OnChange := dropdownColsChange;
    labelNumber.Height := dropdownCols.Height;
    Inc( xPosition, dropdownCols.Width + Margin );

    // 3. A button for selecting ASC/DESC
    buttonOrder := TSpeedButton.Create(self);
    buttonOrder.Parent := pnlBevel;
    buttonOrder.Width := Round(40 * DpiScaleFactor(Self));
    buttonOrder.Height := dropdownCols.Height;
    buttonOrder.Left := xPosition;
    buttonOrder.Top := topPosition;
    buttonOrder.AllowAllUp := True; // Enables Down = False
    buttonOrder.GroupIndex := i+1; // if > 0 enables Down = True
    buttonOrder.Caption := TXT_ASC;
    if OrderColumns[i].SortDirection = ORDER_DESC then
    begin
      buttonOrder.Caption := TXT_DESC;
      buttonOrder.Down := True;
    end;
    buttonOrder.Hint := _('Toggle the sort direction for this column.');
    buttonOrder.Tag := i+1;
    buttonOrder.OnClick := buttonOrderClick;
    Inc( xPosition, buttonOrder.Width + Margin );

    // 4. Button for deleting
    buttonDelete := TButton.Create(self);
    buttonDelete.Parent := pnlBevel;
    buttonDelete.Width := Round(30 * DpiScaleFactor(Self));
    buttonDelete.Height := dropdownCols.Height;
    buttonDelete.Left := xPosition;
    buttonDelete.Top := topPosition;
    buttonDelete.Caption := 'X';
    buttonDelete.Hint := _('Drops sorting by this column.');
    buttonDelete.Tag := i+1;
    buttonDelete.OnClick := buttonDeleteClick;
    Inc( xPosition, buttonDelete.Width + Margin );

    topPosition := dropdownCols.Top + dropdownCols.Height + Margin;
  end;

  // Auto-adjust size of form
  Height := topPosition + MarginBig +
    btnreset.Height + Margin +
    btnOK.Height + Margin +
    pnlBevel.BorderWidth;
  Width := xPosition + pnlBevel.BorderWidth;

  // Auto-adjust width and position of main buttons at bottom
  btnWidth := (pnlBevel.Width -pnlBevel.BorderWidth*2 - Margin) DIV 3 - Margin;
  btnOK.Width := btnWidth;
  btnOK.Left := pnlBevel.BorderWidth + Margin;
  btnCancel.Width := btnWidth;
  btnCancel.Left := btnOK.Left + btnWidth + Margin;
  btnAddCol.Width := btnWidth;
  btnAddCol.Left := btnCancel.Left + btnWidth + Margin;
  btnReset.Left := btnOK.Left;
  btnReset.Width := btnAddCol.Left + btnAddCol.Width - btnReset.Left;
  btnReset.Enabled := Mainform.actDataResetSorting.Enabled;
end;


{**
  Dropdown for column selection was changed
}
procedure TDataSortingForm.dropdownColsChange( Sender: TObject );
var
  combo : TComboBox;
begin
  combo := Sender as TComboBox;
  OrderColumns[combo.Tag-1].ColumnName := combo.Text;

  // Enables OK button
  Modified;
end;


{**
  Button for selecting sort-direction was clicked
}
procedure TDataSortingForm.buttonOrderClick( Sender: TObject );
var
  btn : TSpeedButton;
begin
  btn := Sender as TSpeedButton;
  if btn.Down then
  begin
    btn.Caption := TXT_DESC;
    OrderColumns[btn.Tag-1].SortDirection := ORDER_DESC;
  end
  else
  begin
    btn.Caption := TXT_ASC;
    OrderColumns[btn.Tag-1].SortDirection := ORDER_ASC;
  end;

  // Enables OK button
  Modified;
end;


{**
  Delete order column
}
procedure TDataSortingForm.buttonDeleteClick( Sender: TObject );
var
  btn : TButton;
  i : Integer;
begin
  btn := Sender as TButton;

  if Length(OrderColumns)>1 then
  begin
    // Move remaining items one up
    for i := btn.Tag-1 to Length(OrderColumns) - 2 do
    begin
      OrderColumns[i] := OrderColumns[i+1];
    end;
  end;
  // Delete last item
  SetLength(OrderColumns, Length(OrderColumns)-1);

  // Refresh controls
  DisplaySortingControls(Sender);

  // Enables OK button
  Modified;
end;


{**
  Add a new order column
}
procedure TDataSortingForm.btnAddColClick(Sender: TObject);
var
  i, new : Integer;
  UnusedColumns : TStringList;
begin
  SetLength( OrderColumns, Length(OrderColumns)+1 );
  new := Length(OrderColumns)-1;
  OrderColumns[new] := TOrderCol.Create;

  // Take first unused column as default for new sort column
  UnusedColumns := TStringList.Create;
  UnusedColumns.AddStrings( ColumnNames );
  for i := 0 to Length(OrderColumns) - 1 do
  begin
    if UnusedColumns.IndexOf(OrderColumns[i].ColumnName) > -1 then
    begin
      UnusedColumns.Delete( UnusedColumns.IndexOf(OrderColumns[i].ColumnName) );
    end;
  end;
  if UnusedColumns.Count > 0 then
    OrderColumns[new].ColumnName := UnusedColumns[0]
  else
    OrderColumns[new].ColumnName := ColumnNames[0];

  // Sort ASC by default
  OrderColumns[new].SortDirection := ORDER_ASC;

  // Refresh controls
  DisplaySortingControls(Sender);

  // Enables OK button
  Modified;
end;


{**
  Gets called when any option has changed.
  Enables the OK button if ORDER options have changed
}
procedure TDataSortingForm.Modified;
begin
  btnOk.Enabled := ComposeOrderClause(OrderColumns) <> OldOrderClause;
end;


{**
  OK clicked: Write ORDER clause to registry
}
procedure TDataSortingForm.btnOKClick(Sender: TObject);
begin
  // TODO: apply ordering
  Mainform.DataGridSortColumns := OrderColumns;
  InvalidateVT(Mainform.DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
  btnCancel.OnClick(Sender);
end;


procedure TDataSortingForm.btnCancelClick(Sender: TObject);
begin
  Mainform.tbtnDataSorting.Down := False;
  Close;
end;


{**
  Be sure the form is destroyed after closing.
}
procedure TDataSortingForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


{**
  Cancel this dialog if the user clicks elsewhere on mainform
}
procedure TDataSortingForm.FormDeactivate(Sender: TObject);
begin
  btnCancel.OnClick(Sender);
end;


procedure TDataSortingForm.btnResetClick(Sender: TObject);
begin
  Mainform.actDataResetSortingExecute(Sender);
  btnCancel.OnClick(Sender);
end;


end.
