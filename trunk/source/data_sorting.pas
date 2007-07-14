unit data_sorting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, Registry, smdbgrid;

type
  TOrderCol = record
    ColumnName: String;
    SortDirection: Byte;
  end;


type
  TDataSortingForm = class(TForm)
    pnlBevel: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnAddCol: TButton;
    procedure btnAddColClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    ColumnNames : TStringList;
    OrderColumns : Array of TOrderCol;
    reg_name, OldOrderClause : String;
    procedure DisplaySortingControls;
    procedure dropdownColsChange( Sender: TObject );
    procedure buttonOrderClick( Sender: TObject );
    procedure buttonDeleteClick( Sender: TObject );
    procedure Modified;
    function ComposeOrderClause: String;
  public
    { Public declarations }
  end;


const
  ORDER_ASC = 0;            // Used for tag-value of "Direction"-button
  ORDER_DESC = 1;           // dito
  TXT_ASC = 'ASC';          // Used for caption of "Direction"-button
  TXT_DESC = 'DESC';        // dito
  LINE_HEIGHT = 20;         // Height of automatically created controls
  MARGIN = 2;               // Space between controls
  MARGIN_BIG = 3 * MARGIN;  // Space above the very first and last controls, used to separate stuff

implementation

uses main;

{$R *.dfm}


{**
  Initialization
}
procedure TDataSortingForm.FormShow(Sender: TObject);
var
  i : Cardinal;
  grid: TSMDBGrid;
  reg : TRegistry;
begin
  // Take column names from listColumns and add here
  ColumnNames := TStringList.Create;
  for i := 0 to Mainform.Childwin.listColumns.Items.Count-1 do
  begin
    ColumnNames.Add( Mainform.Childwin.listColumns.Items[i].Caption );
  end;

  // Read original ORDER clause from registry
  reg := TRegistry.Create();
  reg.OpenKey( REGPATH + '\Servers\' + Mainform.Childwin.Description, true );
  reg_name := 'ORDERCLAUSE_' + Mainform.Childwin.ActualDatabase + '.' + Mainform.Childwin.ActualTable;
  OldOrderClause := reg.ReadString(reg_name);
  reg.CloseKey;

  // Create one OrderColumns record for each grid.SortColumn
  grid := Mainform.Childwin.gridData;
  if grid.SortColumns.Count > 0 then // Avoids AV when accessing SortColumn[0] ... ??
  begin
    for i := 0 to grid.SortColumns.Count - 1 do
    begin
      SetLength( OrderColumns, i+1 );
      OrderColumns[i].ColumnName := grid.SortColumns[i].FieldName;
      if grid.SortColumns[i].SortType = stDescending then
        OrderColumns[i].SortDirection := ORDER_ASC
      else
        OrderColumns[i].SortDirection := ORDER_DESC; // Also applies for SortType = stNone
    end;
  end;

  // First creation of controls
  DisplaySortingControls;

end;


{**
  Create controls for order columns
}
procedure TDataSortingForm.DisplaySortingControls;
var
  labelNumber: TLabel;
  buttonDelete: TButton;
  dropdownCols: TComboBox;
  buttonOrder: TSpeedButton;
  i, xPosition, topPosition, btnWidth : Integer;
begin

  // Remove previously created components
  for i := ComponentCount - 1 downto 0 do
  begin
    if Components[i].Tag > 0 then
      Components[i].Free;
  end;

  // Set initial width to avoid resizing form to 0
  xPosition := pnlBevel.Width;


  // Create line with controls for each order column
  for i:=0 to Length(OrderColumns)-1 do
  begin
    xPosition := pnlBevel.BorderWidth;
    topPosition := pnlBevel.BorderWidth + MARGIN_BIG + (i * (LINE_HEIGHT + MARGIN));

    // 1. Label with number
    labelNumber := TLabel.Create(self);
    labelNumber.Parent := pnlBevel;
    labelNumber.AutoSize := False; // Avoids automatic changes to width + height
    labelNumber.Left := xPosition;
    labelNumber.Top := topPosition;
    labelNumber.Width := 15;
    labelNumber.Height := LINE_HEIGHT;
    labelNumber.Alignment := taRightJustify;
    labelNumber.Layout := tlCenter;
    labelNumber.Caption := IntToStr(i+1) + '.';
    labelNumber.Tag := i+1;
    Inc( xPosition, labelNumber.Width + MARGIN );

    // 2. Dropdown with columnnames
    dropdownCols := TComboBox.Create(self);
    dropdownCols.Parent := pnlBevel;
    dropdownCols.Width := 120;
    dropdownCols.Height := LINE_HEIGHT;
    dropdownCols.Left := xPosition;
    dropdownCols.Top := topPosition;
    dropdownCols.Items := ColumnNames;
    dropdownCols.Style := csDropDownList; // Not editable
    dropdownCols.ItemIndex := ColumnNames.IndexOf(OrderColumns[i].ColumnName);
    dropdownCols.Tag := i+1;
    dropdownCols.OnChange := dropdownColsChange;
    Inc( xPosition, dropdownCols.Width + MARGIN );

    // 3. A button for selecting ASC/DESC
    buttonOrder := TSpeedButton.Create(self);
    buttonOrder.Parent := pnlBevel;
    buttonOrder.Width := 35;
    buttonOrder.Height := LINE_HEIGHT;
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
    buttonOrder.Hint := 'Toggle the sort direction for this column.';
    buttonOrder.Tag := i+1;
    buttonOrder.OnClick := buttonOrderClick;
    Inc( xPosition, buttonOrder.Width + MARGIN );

    // 4. Button for deleting
    buttonDelete := TButton.Create(self);
    buttonDelete.Parent := pnlBevel;
    buttonDelete.Width := 20;
    buttonDelete.Height := LINE_HEIGHT;
    buttonDelete.Left := xPosition;
    buttonDelete.Top := topPosition;
    buttonDelete.Caption := 'X';
    buttonDelete.Hint := 'Drops sorting by this column.';
    buttonDelete.Tag := i+1;
    buttonDelete.OnClick := buttonDeleteClick;
    Inc( xPosition, buttonDelete.Width + MARGIN );

  end;

  // Auto-adjust size of form
  Height := (pnlBevel.BorderWidth * 2) + // Top + Bottom border
    (MARGIN_BIG * 2) + // Separator spaces
    (Length(OrderColumns) * (LINE_HEIGHT + MARGIN)) + // Height of created controls
    (btnOK.Height + MARGIN); // Height of buttons
  Width := xPosition + pnlBevel.BorderWidth;

  // Auto-adjust width and position of main buttons at bottom
  btnWidth := (pnlBevel.Width -pnlBevel.BorderWidth*2 - MARGIN) DIV 3 - MARGIN;
  btnOK.Width := btnWidth;
  btnOK.Top := Height - pnlBevel.BorderWidth - MARGIN - btnOK.Height;
  btnOK.Left := pnlBevel.BorderWidth + MARGIN;
  btnCancel.Width := btnWidth;
  btnCancel.Left := btnOK.Left + btnWidth + MARGIN;
  btnAddCol.Width := btnWidth;
  btnAddCol.Left := btnCancel.Left + btnWidth + MARGIN;
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
  DisplaySortingControls;

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
  DisplaySortingControls;

  // Enables OK button
  Modified;
end;


{**
  Gets called when any option has changed.
  Enables the OK button if ORDER options have changed
}
procedure TDataSortingForm.Modified;
begin
  btnOk.Enabled := ComposeOrderClause <> OldOrderClause;
end;


{**
  Concat all sort options to a ORDER clause
}
function TDataSortingForm.ComposeOrderClause: String;
var
  i : Integer;
  sort : String;
begin
  result := '';
  for i := 0 to Length(OrderColumns) - 1 do
  begin
    if result <> '' then
      result := result + ', ';
    if OrderColumns[i].SortDirection = ORDER_ASC then
      sort := TXT_ASC
    else
      sort := TXT_DESC;
    result := result + Mainform.Mask( OrderColumns[i].ColumnName ) + ' ' + sort;
  end;
end;


{**
  OK clicked: Write ORDER clause to registry
}
procedure TDataSortingForm.btnOKClick(Sender: TObject);
var
  reg : TRegistry;
begin
  reg := TRegistry.Create();
  reg.OpenKey( REGPATH + '\Servers\' + Mainform.Childwin.Description, true );
  reg.WriteString( reg_name, ComposeOrderClause );
  reg.CloseKey;
end;


end.
