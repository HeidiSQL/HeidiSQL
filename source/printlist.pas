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


implementation

uses main, helpers;

{$R *.DFM}


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
  list: TVirtualStringTree;
begin
  // print!
  Screen.Cursor := crHourglass;
  // which ListView to print?
  case Mainform.PageControlMain.ActivePageIndex of
    0: case Mainform.PageControlHost.ActivePageIndex of
      0: list := Mainform.ListVariables;
      1: list := Mainform.ListStatus;
      2: list := Mainform.ListProcesses;
      else list := Mainform.ListCommandStats;
      end;
    1: list := Mainform.ListTables;
    2: list := Mainform.DataGrid;
    else list := Mainform.QueryGrid;
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
