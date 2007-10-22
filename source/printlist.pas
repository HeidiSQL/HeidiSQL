unit printlist;


// -------------------------------------
// Print TListView-Content
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, printers, comctrls, ExtCtrls, VirtualTrees, Math;

type
  TprintlistForm = class(TForm)
    comboPrinters: TComboBox;
    btnConfigure: TButton;
    boxColumns: TGroupBox;
    btnCancel: TButton;
    btnPrint: TButton;
    PrinterSetup: TPrinterSetupDialog;
    chkAllColumns: TCheckBox;
    lblSelect: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure chkAllColumnsClick(Sender: TObject);
    procedure comboPrintersChange(Sender: TObject);
  private
    list : TVirtualStringTree;
    title : String;
    { Private declarations }
  public
    { Public declarations }
  end;

  function printlistWindow(AOwner: TComponent): Boolean;

implementation

uses childwin, main, helpers;

{$R *.DFM}

{**
  Create form on demand
  @param TComponent Owner of form (should be calling form)
  @return Boolean Form closed using modalresult mrOK
}
function printlistWindow(AOwner: TComponent): Boolean;
var
  f : TprintlistForm;
begin
  f := TprintlistForm.Create(AOwner);
  Result := (f.ShowModal=mrOK);
  FreeAndNil(f);
end;


procedure TprintlistForm.FormShow(Sender: TObject);
var
  i, chkTop, chkCount : Integer;
  cwin : TMDIChild;
  chk : TCheckBox;
const
  chkHeight = 23;
begin
  // show!
  Screen.Cursor := crHourGlass;
  comboPrinters.Items := Printer.printers;
  comboPrinters.ItemIndex := Printer.printerIndex;

  cwin := Mainform.ChildWin;

  // which ListView to print?
  case cwin.PageControlMain.ActivePageIndex of
    0 : case cwin.PageControlHost.ActivePageIndex of
      0 : begin list := cwin.ListVariables; title := 'Server-Variables for ' + cwin.Conn.MysqlParams.Host; end;
      1 : begin list := cwin.ListProcesses; title := 'Processlist for ' + cwin.Conn.MysqlParams.Host; end;
      2 : begin list := cwin.ListCommandStats; title := 'Command-statistics for ' + cwin.Conn.MysqlParams.Host; end;
      end;
    1 : begin list := cwin.ListTables; title := 'Tables-List for Database ' + cwin.ActiveDatabase; end;
    2 : begin list := cwin.ListColumns; title := 'Field-List for ' + cwin.ActiveDatabase + '/' + cwin.SelectedTable; end;
  end;
  caption := 'Print ' + title + '...';

  // delete all CheckBoxes
  for i:=ComponentCount-1 downto 0 do begin
    if Components[i].tag = 1 then
      Components[i].free;
  end;


  // add one CheckBox per list-column
  chkTop := 2;
  chkCount := 0;
  chkAllColumns.Checked := True;
  for i:=0 to list.Header.Columns.count-1 do
  begin
    // Skip hidden columns
    if not (coVisible in list.Header.Columns[i].Options) then
      continue;

    chk := TCheckBox.Create(self);
    chk.Parent := boxColumns;
    chk.Caption := list.Header.Columns[i].Text;
    chk.Checked := True;
    chk.Enabled := False;
    if chkCount mod 2 = 0 then begin
      inc(chkTop);
      chk.Left := 16;
    end else
      chk.Left := 130;
    chk.Top := chkTop * chkHeight;
    chk.Tag := 1;
    inc(chkCount);
  end;
  Height := boxColumns.Top
    + (3 * chkHeight) // "All columns checkbox + space
    + (Height - boxColumns.Top - boxColumns.Height) // Bottom rest
    + (Ceil(chkCount/2) * chkHeight); // Height of generated checkboxes
  Screen.Cursor := crDefault;
end;

procedure TprintlistForm.btnConfigureClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  printerSetup.Execute;
  comboPrinters.ItemIndex := Printer.PrinterIndex;
  Screen.Cursor := crDefault;
end;

procedure TprintlistForm.btnPrintClick(Sender: TObject);
var
  lspace, rspace, i, j, k, limiter, breite, hoehe, colwidth : Integer;
  Cols : TStringList;
  Node : PVirtualNode;
  NodeData : PVTreeData;
begin
  // print!
  Screen.Cursor := crHourglass;

  Cols := TStringList.Create;
  for i:=0 to list.Header.Columns.Count-1 do begin
    for j:=0 to ComponentCount-1 do if (Components[j].tag = 1) then begin
      if ((Components[j] as TCheckBox).caption = list.Header.Columns[i].Text) and
        ((Components[j] as TCheckBox).checked)
         then
        Cols.add(list.Header.Columns[i].Text);
    end;
  end;

  lspace := 200;
  rspace := 100;
  Printer.Title := APPNAME + ': ' + Title;
  Printer.BeginDoc;

  with Printer.Canvas do begin
    SetMapMode(handle, MM_LOMETRIC);
    Font.Name := 'Arial';
    SetTextAlign(handle, TA_LEFT+TA_TOP);

    // 1/10 mm
    breite := getdeviceCaps(handle, horzsize) * 10;
    hoehe := getdeviceCaps(handle, vertsize) * 10;
    colwidth := (breite - lspace - rspace) div Cols.Count;

    Font.Height := 40;
    Font.Style := [fsBold];
    TextOut(lspace, -100, APPNAME + ': ' + title);
    Font.Height := 30;

    // print columns
    Pen.Color := clWhite;
    Brush.Color := clLtGray;
    Rectangle(lspace, -300, breite - rspace, -350);
    Pen.Color := clBlack;
    MoveTo(lspace, -300);
    LineTo(breite - rspace, -300);
    for i:=0 to Cols.Count-1 do
      TextOut(lspace + i*colwidth, -310, Cols[i]);
    MoveTo(lspace, -350);
    LineTo(breite - rspace, -350);
    Brush.Color := clWhite;
    TextOut(lspace, -(hoehe-50), 'Page ' + inttostr(printer.PageNumber));

    // print lines
    Font.Style := [];
    limiter := 0;

    // Find first list node
    Node := list.GetFirstVisible;
    i := 0;
    while Assigned(Node) do
    begin
      if ((i-limiter) * 50 + 360 + 100) > hoehe then begin
        printer.NewPage;
        TextOut(lspace, -(hoehe-50), 'Page ' + inttostr(printer.PageNumber));
        limiter := i;
      end;
      MoveTo(lspace, -((i-limiter) * 50 + 365 + font.Height));
      LineTo(breite - rspace, -((i-limiter) * 50 + 365 + font.Height));

      // Fetch node data
      NodeData := list.GetNodeData( Node );
      for j:=0 to Cols.count-1 do
      begin
        for k:=0 to NodeData.Captions.count-1 do
        begin
          if list.Header.Columns[k].Text = Cols[j] then
            TextOut(lspace + j*colwidth, -((i - limiter) * 50 + 360), NodeData.Captions[k]);
        end;
      end;

      // Go to next node in list
      Node := list.GetNextVisible(Node);
      inc(i);
    end;

  end;
  Printer.EndDoc;
  Screen.Cursor := crDefault;
end;



procedure TprintlistForm.chkAllColumnsClick(Sender: TObject);
var i : Integer;
begin
  // toggle checkboxes-enabled
  for i:=ComponentCount-1 downto 0 do
  begin
    if Components[i].tag = 1 then
    begin
      (Components[i] as TCheckBox).enabled := not chkAllColumns.checked;
      if chkAllColumns.Checked then
        (Components[i] as TCheckBox).Checked := true;
    end;
  end;
end;


procedure TprintlistForm.comboPrintersChange(Sender: TObject);
begin
  // chose printer
  Screen.Cursor := crHourglass;
  Printer.PrinterIndex := comboPrinters.ItemIndex;
  Screen.Cursor := crDefault;
end;

end.
