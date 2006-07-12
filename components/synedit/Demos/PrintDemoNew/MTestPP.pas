{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MTestPP.pas, released 2000-06-01.

The Original Code is part of the TestPP project, written by
Morten J. Skovrup for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: MTestPP.pas,v 1.2 2000/11/22 08:37:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit MTestPP;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynEditHighlighter, SynHighlighterPas, SynEdit, StdCtrls, SynEditPrint,
  SynEditPrintTypes, ActnList, ToolWin, ComCtrls, ImgList, Menus;

type
  TForm1 = class(TForm)
    SynEdit: TSynEdit;
    SynPasSyn: TSynPasSyn;
    OpenDialog: TOpenDialog;
    PrintDialog: TPrintDialog;
    SynEditPrint: TSynEditPrint;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    FileOpenCmd: TAction;
    FilePrinterSetupCmd: TAction;
    FilePrintCmd: TAction;
    FileExitCmd: TAction;
    FileImages: TImageList;
    FilePrintPreviewCmd: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    PrinterSetupDialog: TPrinterSetupDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileOpenCmd1: TMenuItem;
    N1: TMenuItem;
    PrinterSetup1: TMenuItem;
    PrintPreview1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    FilePageSetup: TAction;
    PageSetup1: TMenuItem;
    ToolButton8: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileOpenCmdExecute(Sender: TObject);
    procedure FilePrinterSetupCmdExecute(Sender: TObject);
    procedure FilePrintPreviewCmdExecute(Sender: TObject);
    procedure FilePrintCmdExecute(Sender: TObject);
    procedure FileExitCmdExecute(Sender: TObject);
    procedure FilePageSetupExecute(Sender: TObject);
  private
    { Private declarations }
    FCurFile: string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses DTestPrintPreview, DPageSetup;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  with SynEditPrint.Header do begin
      {First line, default font, left aligned}
    Add('This is the first line in the header', nil, taLeftJustify, 1);
      {First line, default font, right aligned}
    Add('Page: $PAGENUM$ of $PAGECOUNT$', nil, taRightJustify, 1);
      {Second line, default font, left aligned}
    Add('$TITLE$', nil, taLeftJustify, 2);
    AFont.Assign(DefaultFont);
    AFont.Size := 6;
      {Second line, small font, right aligned - note that lines can have different fonts}
    Add('Print Date: $DATE$. Time: $TIME$', AFont, taRightJustify, 2);
  end;
  with SynEditPrint.Footer do begin
    AFont.Assign(DefaultFont);
    Add('$PAGENUM$/$PAGECOUNT$', nil, taRightJustify, 1);
    AFont.Size := 6;
    Add('Printed by John Doe', AFont, taLeftJustify, 1);
  end;
  AFont.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SynEditPrint.Free;
end;

procedure TForm1.FileOpenCmdExecute(Sender: TObject);
begin
  OpenDialog.Filter := SynPasSyn.DefaultFilter;
  if OpenDialog.Execute then begin
    SynEdit.Lines.LoadFromFile(OpenDialog.FileName);
    FCurFile := OpenDialog.FileName;
  end;
end;

procedure TForm1.FilePrinterSetupCmdExecute(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TForm1.FilePrintPreviewCmdExecute(Sender: TObject);
begin
  SynEditPrint.SynEdit := SynEdit;
  SynEditPrint.Title := FCurFile;
  with TestPrintPreviewDlg do begin
    SynEditPrintPreview.SynEditPrint := SynEditPrint;
    ShowModal;
  end;
end;

procedure TForm1.FilePrintCmdExecute(Sender: TObject);
begin
  if PrintDialog.Execute then begin
    SynEditPrint.SynEdit := SynEdit;
    SynEditPrint.Title := FCurFile;
    SynEditPrint.Print;
  end;
end;

procedure TForm1.FileExitCmdExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FilePageSetupExecute(Sender: TObject);
begin
  PageSetupDlg.SetValues(SynEditPrint);
  if PageSetupDlg.ShowModal = mrOk then
    PageSetupDlg.GetValues(SynEditPrint);
end;

end.

