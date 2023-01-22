unit data_sorting;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Buttons,
  Vcl.Graphics, apphelpers, gnugettext, extra_controls;


type
  TfrmDataSorting = class(TExtForm)
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
    FColumnNames: TStringList;
    FSortItems: TSortItems;
    FOldOrderClause: String;
    procedure comboColumnsChange(Sender: TObject);
    procedure btnOrderClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure Modified;
  public
    { Public declarations }
  end;



implementation

uses main;

{$R *.dfm}


procedure TfrmDataSorting.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FColumnNames := TStringList.Create;
  // Take column names from listColumns and add here
  for i:=0 to Mainform.SelectedTableColumns.Count-1 do begin
    FColumnNames.Add(Mainform.SelectedTableColumns[i].Name);
  end;

  FSortItems := TSortItems.Create(True);
  FSortItems.Assign(MainForm.DataGridSortItems);
  FOldOrderClause := FSortItems.ComposeOrderClause;

  // First creation of controls
  DisplaySortingControls(Sender);
end;


{**
  Create controls for order columns
}
procedure TfrmDataSorting.DisplaySortingControls(Sender: TObject);
var
  SortItem: TSortItem;
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

  Margin := ScaleSize(3);
  MarginBig := ScaleSize(Margin * 2);
  Width1 := ScaleSize(15);
  Width2 := ScaleSize(160);
  Width3 := ScaleSize(23);
  Width4 := ScaleSize(23);

  // Set initial width to avoid resizing form to 0
  TopPos := pnlBevel.BorderWidth + MarginBig;

  // Create line with controls for each order column
  // TODO: disable repaint on every created control. Sending WM_SETREDRAW=0 message creates artefacts.
  LockWindowUpdate(pnlBevel.Handle);
  for i:=0 to FSortItems.Count-1 do begin
    SortItem := FSortItems[i];
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

    // 2. Dropdown with column names
    comboColumns := TComboBox.Create(self);
    comboColumns.Parent := pnlBevel;
    comboColumns.Width := Width2;
    comboColumns.Left := lblNumber.Left + lblNumber.Width + Margin;
    comboColumns.Top := TopPos;
    comboColumns.Items.Text := FColumnNames.Text;
    comboColumns.Style := csDropDownList; // Not editable
    comboColumns.ItemIndex := FColumnNames.IndexOf(SortItem.Column);
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
    if SortItem.Order = sioDescending then begin
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
procedure TfrmDataSorting.comboColumnsChange( Sender: TObject );
var
  combo : TComboBox;
begin
  combo := Sender as TComboBox;
  FSortItems[combo.Tag-1].Column := combo.Text;

  // Enables OK button
  Modified;
end;


{**
  Button for selecting sort-direction was clicked
}
procedure TfrmDataSorting.btnOrderClick( Sender: TObject );
var
  btn: TSpeedButton;
begin
  btn := Sender as TSpeedButton;
  btn.Glyph := nil;
  if FSortItems[btn.Tag-1].Order = sioAscending then begin
    MainForm.VirtualImageListMain.GetBitmap(110, btn.Glyph);
    FSortItems[btn.Tag-1].Order := sioDescending;
  end else begin
    MainForm.VirtualImageListMain.GetBitmap(109, btn.Glyph);
    FSortItems[btn.Tag-1].Order := sioAscending;
  end;

  // Enables OK button
  Modified;
end;


{**
  Delete order column
}
procedure TfrmDataSorting.btnDeleteClick(Sender: TObject);
var
  btn: TButton;
begin
  btn := Sender as TButton;
  FSortItems.Delete(btn.Tag-1);
  // Refresh controls
  DisplaySortingControls(Self);
  // Enables OK button
  Modified;
end;


{**
  Add a new order column
}
procedure TfrmDataSorting.btnAddColClick(Sender: TObject);
var
  UnusedColumns: TStringList;
  NewSortItem, SortItem: TSortItem;
begin
  NewSortItem := TSortItem.Create;
  FSortItems.Add(NewSortItem);

  // Take first unused column as default for new sort item
  UnusedColumns := TStringList.Create;
  UnusedColumns.AddStrings(FColumnNames);
  for SortItem in FSortItems do begin
    if UnusedColumns.IndexOf(SortItem.Column) > -1 then
      UnusedColumns.Delete(UnusedColumns.IndexOf(SortItem.Column));
  end;
  if UnusedColumns.Count > 0 then
    NewSortItem.Column := UnusedColumns[0]
  else
    NewSortItem.Column := FColumnNames[0];

  // Refresh controls
  DisplaySortingControls(Sender);

  // Enables OK button
  Modified;
end;


{**
  Gets called when any option has changed.
  Enables the OK button if ORDER options have changed
}
procedure TfrmDataSorting.Modified;
begin
  btnOk.Enabled := FSortItems.ComposeOrderClause <> FOldOrderClause;
end;


{**
  OK clicked: Write ORDER clause to registry
}
procedure TfrmDataSorting.btnOKClick(Sender: TObject);
begin
  // TODO: apply ordering
  MainForm.DataGridSortItems.Assign(FSortItems);
  InvalidateVT(Mainform.DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
  btnCancel.OnClick(Sender);
end;


procedure TfrmDataSorting.btnCancelClick(Sender: TObject);
begin
  Mainform.tbtnDataSorting.Down := False;
  Close;
end;


{**
  Be sure the form is destroyed after closing.
}
procedure TfrmDataSorting.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


{**
  Cancel this dialog if the user clicks elsewhere on mainform
}
procedure TfrmDataSorting.FormDeactivate(Sender: TObject);
begin
  btnCancel.OnClick(Sender);
end;


end.
