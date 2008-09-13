unit printlist;


// -------------------------------------
// Print TListView-Content
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, printers, VirtualTrees;

type
  TprintlistForm = class(TForm)
    comboPrinters: TComboBox;
    btnConfigure: TButton;
    btnCancel: TButton;
    btnPrint: TButton;
    PrinterSetup: TPrinterSetupDialog;
    lblSelect: TLabel;
    chkPrintHeader: TCheckBox;
    procedure btnConfigureClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure comboPrintersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
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


procedure TprintlistForm.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
end;

procedure TprintlistForm.FormShow(Sender: TObject);
begin
  // show!
  Screen.Cursor := crHourGlass;
  comboPrinters.Items := Printer.printers;
  comboPrinters.ItemIndex := Printer.printerIndex;
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
  cwin: TMDIChild;
  list: TVirtualStringTree;
begin
  // print!
  Screen.Cursor := crHourglass;
  cwin := Mainform.ChildWin;
  // which ListView to print?
  case cwin.PageControlMain.ActivePageIndex of
    0: case cwin.PageControlHost.ActivePageIndex of
      0: list := cwin.ListVariables;
      1: list := cwin.ListStatus;
      2: list := cwin.ListProcesses;
      else list := cwin.ListCommandStats;
      end;
    1: list := cwin.ListTables;
    2: list := cwin.ListColumns;
    3: list := cwin.DataGrid;
    else list := cwin.QueryGrid;
  end;
  list.Print(Printer, chkPrintHeader.Checked);
  Screen.Cursor := crDefault;
end;


procedure TprintlistForm.comboPrintersChange(Sender: TObject);
begin
  // chose printer
  Screen.Cursor := crHourglass;
  Printer.PrinterIndex := comboPrinters.ItemIndex;
  Screen.Cursor := crDefault;
end;

end.
