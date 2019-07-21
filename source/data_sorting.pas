unit data_sorting;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  Vcl.Graphics, apphelpers, gnugettext, extra_controls;


type
  TDataSortingForm = class(TExtForm)
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
    procedure DisplaySortingControls(Sender: TObject);
  private
    { Private declarations }
    ColumnNames : TStringList;
    OrderColumns : TOrderColArray;
    OldOrderClause : String;
    procedure comboColumnsChange( Sender: TObject );
    procedure btnOrderClick( Sender: TObject );
    procedure btnDeleteClick( Sender: TObject );
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
  lblNumber: TLabel;
  btnDelete: TButton;
  comboColumns: TComboBox;
  btnOrder: TSpeedButton;
  i, TopPos,
  Width1, Width2, Width3, Width4,   // Width of controls per row
  Margin,                           // Space between controls
  MarginBig: Integer;               // Space above the very first and last controls, used to separate stuff
begin
  // Remove previously created components, which all have a tag > 0
  for i := ComponentCount - 1 downto 0 do begin
    if Components[i].Tag > 0 then
      Components[i].Free;
  end;

  Margin := 3;
  MarginBig := Margin * 2;
  Width1 := 15;
  Width2 := 160;
  Width3 := 23;
  Width4 := 23;

  // Set initial width to avoid resizing form to 0
  TopPos := pnlBevel.BorderWidth + MarginBig;

  // Create line with controls for each order column
  // TODO: disable repaint on every created control. Sending WM_SETREDRAW=0 message creates artefacts.
  LockWindowUpdate(pnlBevel.Handle);
  for i:=0 to Length(OrderColumns)-1 do begin
    // 1. Label with number
    lblNumber := TLabel.Create(self);
    lblNumber.Parent := pnlBevel;
    lblNumber.AutoSize := False; // Avoids automatic changes to width + height
    lblNumber.Left := pnlBevel.BorderWidth + MarginBig;
    lblNumber.Top := TopPos;
    lblNumber.Width := Width1;
    lblNumber.Alignment := taRightJustify;
    lblNumber.Layout := tlCenter;
    lblNumber.Caption := IntToStr(i+1) + '.';
    lblNumber.Tag := i+1;

    // 2. Dropdown with columnnames
    comboColumns := TComboBox.Create(self);
    comboColumns.Parent := pnlBevel;
    comboColumns.Width := Width2;
    comboColumns.Left := lblNumber.Left + lblNumber.Width + Margin;
    comboColumns.Top := TopPos;
    comboColumns.Items.Text := ColumnNames.Text;
    comboColumns.Style := csDropDownList; // Not editable
    comboColumns.ItemIndex := ColumnNames.IndexOf(OrderColumns[i].ColumnName);
    comboColumns.Tag := i+1;
    comboColumns.OnChange := comboColumnsChange;
    lblNumber.Height := comboColumns.Height;

    // 3. A button for selecting ASC/DESC
    btnOrder := TSpeedButton.Create(self);
    btnOrder.Parent := pnlBevel;
    btnOrder.Width := Width3;
    btnOrder.Height := comboColumns.Height;
    btnOrder.Left := comboColumns.Left + comboColumns.Width + Margin;
    btnOrder.Top := TopPos;
    btnOrder.AllowAllUp := True; // Enables Down = False
    btnOrder.GroupIndex := i+1; // if > 0 enables Down = True
    btnOrder.Glyph.Transparent := True;
    btnOrder.Glyph.AlphaFormat := afDefined;
    if OrderColumns[i].SortDirection = ORDER_DESC then begin
      MainForm.VirtualImageListMain.GetBitmap(110, btnOrder.Glyph);
      btnOrder.Down := True;
    end else begin
      MainForm.VirtualImageListMain.GetBitmap(109, btnOrder.Glyph);
    end;
    btnOrder.Hint := _('Toggle the sort direction for this column.');
    btnOrder.Tag := i+1;
    btnOrder.OnClick := btnOrderClick;

    // 4. Button for deleting
    btnDelete := TButton.Create(self);
    btnDelete.Parent := pnlBevel;
    btnDelete.Width := Width4;
    btnDelete.Height := comboColumns.Height;
    btnDelete.Left := btnOrder.Left + btnOrder.Width + Margin;
    btnDelete.Top := TopPos;
    btnDelete.Images := MainForm.VirtualImageListMain;
    btnDelete.ImageIndex := 26;
    btnDelete.Hint := _('Drops sorting by this column.');
    btnDelete.Tag := i+1;
    btnDelete.OnClick := btnDeleteClick;

    TopPos := comboColumns.Top + comboColumns.Height + Margin;
  end;
  LockWindowUpdate(0);

  Inc(TopPos, MarginBig);

  // Auto-adjust size of form
  Height := TopPos +
    btnReset.Height + Margin +
    btnOK.Height + MarginBig +
    pnlBevel.BorderWidth;
  Width := pnlBevel.BorderWidth +
    MarginBig + Width1 +
    Margin + Width2 +
    Margin + Width3 +
    Margin + Width4 +
    MarginBig + pnlBevel.BorderWidth;

  // Auto-adjust width and position of main buttons at bottom
  btnReset.Left := pnlBevel.BorderWidth + MarginBig;
  btnReset.Top := TopPos;
  btnReset.Width := Width - 2 * pnlBevel.BorderWidth - 2 * MarginBig;
  btnReset.Enabled := Mainform.actDataResetSorting.Enabled;

  btnOK.Left := pnlBevel.BorderWidth + MarginBig;
  btnOK.Top := btnReset.Top + btnReset.Height + Margin;
  btnOK.Width := Round(btnReset.Width / 3) - Margin;

  btnCancel.Left := btnOK.Left + btnOK.Width + Margin;
  btnCancel.Top := btnReset.Top + btnReset.Height + Margin;
  btnCancel.Width := btnOK.Width;

  btnAddCol.Left := btnCancel.Left + btnCancel.Width + Margin;
  btnAddCol.Top := btnReset.Top + btnReset.Height + Margin;
  btnAddCol.Width := btnOK.Width;
end;


{**
  Dropdown for column selection was changed
}
procedure TDataSortingForm.comboColumnsChange( Sender: TObject );
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
procedure TDataSortingForm.btnOrderClick( Sender: TObject );
var
  btn: TSpeedButton;
begin
  btn := Sender as TSpeedButton;
  btn.Glyph := nil;
  if OrderColumns[btn.Tag-1].SortDirection = ORDER_ASC then begin
    MainForm.VirtualImageListMain.GetBitmap(110, btn.Glyph);
    OrderColumns[btn.Tag-1].SortDirection := ORDER_DESC;
  end else begin
    MainForm.VirtualImageListMain.GetBitmap(109, btn.Glyph);
    OrderColumns[btn.Tag-1].SortDirection := ORDER_ASC;
  end;

  // Enables OK button
  Modified;
end;


{**
  Delete order column
}
procedure TDataSortingForm.btnDeleteClick( Sender: TObject );
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


end.
