{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-06-23.

The Original Code is part of the PrintDemo1 project, written by Michael Hieke
for the SynEdit component suite.
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

$Id: frmMain.pas,v 1.3 2003/05/30 05:32:27 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit frmMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SynEditHighlighter, SynHighlighterPas, SynEditPrint, Buttons;

type
  TPrintDemoForm = class(TForm)
    Label1: TLabel;
    eFileName: TEdit;
    SpeedButton1: TSpeedButton;
    dlgFileOpen: TOpenDialog;
    SynPasSyn1: TSynPasSyn;
    Label2: TLabel;
    memoHeader: TMemo;
    memoFooter: TMemo;
    Label3: TLabel;
    dlgFilePrint: TPrintDialog;
    btnPrint: TButton;
    btnHeaderFont: TButton;
    cbUseHighlighter: TCheckBox;
    dlgSelectFont: TFontDialog;
    cbPrintBlackAndWhite: TCheckBox;
    cbPrintLineNumbers: TCheckBox;
    cbWordWrap: TCheckBox;
    Label4: TLabel;
    eTitle: TEdit;
    Label5: TLabel;
    ePrintDateTime: TEdit;
    btnFooterFont: TButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure eFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnHeaderFontClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnFooterFontClick(Sender: TObject);
  private
    fPrintOut: TSynEditPrint;
  end;

var
  PrintDemoForm: TPrintDemoForm;

implementation

{$R *.DFM}

procedure TPrintDemoForm.SpeedButton1Click(Sender: TObject);
begin
  dlgFileOpen.Filter := SynPasSyn1.DefaultFilter;
  if dlgFileOpen.Execute then
    eFileName.Text := dlgFileOpen.FileName;
end;

procedure TPrintDemoForm.eFileNameChange(Sender: TObject);
begin
  btnPrint.Enabled := FileExists(eFileName.Text);
end;

procedure TPrintDemoForm.FormCreate(Sender: TObject);
begin
  fPrintOut := TSynEditPrint.Create( Self );
end;

procedure TPrintDemoForm.btnHeaderFontClick(Sender: TObject);
begin
  dlgSelectFont.Font.Assign(fPrintOut.Header.DefaultFont);
  if dlgSelectFont.Execute then
    fPrintOut.Header.DefaultFont.Assign(dlgSelectFont.Font);
end;

procedure TPrintDemoForm.btnPrintClick(Sender: TObject);
begin
  // set all properties because this can affect pagination
  fPrintOut.Title := eTitle.Text;
  fPrintOut.Lines.LoadFromFile(eFileName.Text);
  if ePrintDateTime.Text <> '' then
    fPrintOut.Header.Add( ePrintDateTime.Text, nil, taRightJustify, 1 );
  fPrintOut.Footer.Add( memoFooter.Lines.Text, nil, taCenter, 1 );
  fPrintOut.Header.Add( memoHeader.Lines.Text, nil, taCenter, 1 );
  fPrintOut.Colors := not cbPrintBlackAndWhite.Checked;
  if cbUseHighlighter.Checked then
    fPrintOut.Highlighter := SynPasSyn1
  else
    fPrintOut.Highlighter := nil;
  fPrintOut.LineNumbers := cbPrintLineNumbers.Checked;
  fPrintOut.Wrap := cbWordWrap.Checked;
  // show print setup dialog and print
  with dlgFilePrint do begin
    MinPage := 1;
    FromPage := 1;
    MaxPage := fPrintOut.PageCount;
    ToPage := MaxPage;
    if Execute then begin
      fPrintOut.Copies := Copies;
      case PrintRange of
        prAllPages: fPrintOut.Print;
        prPageNums: fPrintOut.PrintRange(FromPage, ToPage);
      end;
    end;
  end;
end;

procedure TPrintDemoForm.btnFooterFontClick(Sender: TObject);
begin
  dlgSelectFont.Font.Assign(fPrintOut.Footer.DefaultFont);
  if dlgSelectFont.Execute then
    fPrintOut.Footer.DefaultFont.Assign(dlgSelectFont.Font);
end;

end.

