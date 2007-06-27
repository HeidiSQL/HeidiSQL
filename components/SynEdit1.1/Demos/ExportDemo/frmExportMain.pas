{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmExportMain.pas, released 2000-06-23.

The Original Code is part of the ExportDemo project, written by Michael Hieke
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

$Id: frmExportMain.pas,v 1.2 2000/11/22 08:37:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit frmExportMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynEdit, Menus, SynExportRTF, SynEditExport, SynExportHTML,
  SynEditHighlighter, SynHighlighterPas, ComCtrls, SynHighlighterDfm,
  SynHighlighterCpp;

type
  TForm1 = class(TForm)
    menuMain: TMainMenu;
    mFile: TMenuItem;
    miFileOpen: TMenuItem;
    N1: TMenuItem;
    miFileExit: TMenuItem;
    SynEdit1: TSynEdit;
    dlgFileOpen: TOpenDialog;
    dlgFileSaveAs: TSaveDialog;
    mExport: TMenuItem;
    miExportToFile: TMenuItem;
    Statusbar: TStatusBar;
    SynExporterHTML1: TSynExporterHTML;
    SynExporterRTF1: TSynExporterRTF;
    miExportAsHTML: TMenuItem;
    miExportAsRTF: TMenuItem;
    miExportAllFormats: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    miExportClipboardNative: TMenuItem;
    miExportClipboardText: TMenuItem;
    SynCppSyn1: TSynCppSyn;
    SynDfmSyn1: TSynDfmSyn;
    SynPasSyn1: TSynPasSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miExportToFileClick(Sender: TObject);
    procedure mExportClick(Sender: TObject);
    procedure miExportAsClicked(Sender: TObject);
    procedure miExportClipboardNativeClick(Sender: TObject);
    procedure miExportClipboardTextClick(Sender: TObject);
  private
    fExportAs: integer;
    fHighlighters: TStringList;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  uHighlighterProcs, Clipbrd;

{ TForm1 }
  
procedure TForm1.FormCreate(Sender: TObject);
begin
  fHighlighters := TStringList.Create;
  fHighlighters.Sorted := TRUE;
  GetHighlighters(Self, fHighlighters, FALSE);
  dlgFileOpen.Filter := GetHighlightersFilter(fHighlighters) + 'All files|*.*|';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fHighlighters.Free;
end;

procedure TForm1.miFileOpenClick(Sender: TObject);
begin
  if dlgFileOpen.Execute then begin
    SynEdit1.Lines.LoadFromFile(dlgFileOpen.FileName);
    SynEdit1.Highlighter := GetHighlighterFromFileExt(fHighlighters,
      ExtractFileExt(dlgFileOpen.FileName));
    if Assigned(SynEdit1.Highlighter) then
      Statusbar.SimpleText := 'Using highlighter for ' +
        SynEdit1.Highlighter.GetLanguageName
    else
      Statusbar.SimpleText := 'No highlighter assigned';
  end;
end;

procedure TForm1.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.mExportClick(Sender: TObject);
var
  HasText, IsEnabled: boolean;
  i: integer;
begin
  miExportAsHTML.Checked := fExportAs = 1;
  miExportAsRTF.Checked := fExportAs = 2;
  miExportAllFormats.Checked := fExportAs = 0;

  HasText := FALSE;
  for i := 0 to SynEdit1.Lines.Count - 1 do
    if SynEdit1.Lines[i] <> '' then begin
      HasText := TRUE;
      break;
    end;
  IsEnabled := HasText and Assigned(SynEdit1.Highlighter);
  miExportClipboardNative.Enabled := IsEnabled;
  IsEnabled := IsEnabled and (fExportAs > 0);
  miExportToFile.Enabled := IsEnabled;
  miExportClipboardText.Enabled := IsEnabled;
end;

procedure TForm1.miExportToFileClick(Sender: TObject);
var
  FileName: string;
  Exporter: TSynCustomExporter;
begin
  case fExportAs of
    1: dlgFileSaveAs.Filter := SynExporterHTML1.DefaultFilter;
    2: dlgFileSaveAs.Filter := SynExporterRTF1.DefaultFilter;
  end;
  if dlgFileSaveAs.Execute then begin
    Exporter := nil;
    FileName := dlgFileSaveAs.FileName;
    case fExportAs of
      1: begin
           if ExtractFileExt(FileName) = '' then
             FileName := FileName + '.html';
           Exporter := SynExporterHTML1;
         end;
      2: begin
           if ExtractFileExt(FileName) = '' then
             FileName := FileName + '.rtf';
           Exporter := SynExporterRTF1;
         end;
    end;
    if Assigned(Exporter) then with Exporter do begin
      Title := 'Source file exported to file';
      Highlighter := SynEdit1.Highlighter;
      ExportAsText := TRUE;
      ExportAll(SynEdit1.Lines);
      SaveToFile(FileName);
    end;
  end;
end;

procedure TForm1.miExportAsClicked(Sender: TObject);
begin
  if Sender = miExportAsHTML then
    fExportAs := 1
  else if Sender = miExportAsRTF then
    fExportAs := 2
  else
    fExportAs := 0;
end;

procedure TForm1.miExportClipboardNativeClick(Sender: TObject);
begin
  Clipboard.Open;
  try
    Clipboard.AsText := SynEdit1.Lines.Text;
    // HTML?
    if fExportAs in [0, 1] then with SynExporterHTML1 do begin
      Title := 'Source file exported to clipboard (native format)';
      ExportAsText := FALSE;
      Highlighter := SynEdit1.Highlighter;
      ExportAll(SynEdit1.Lines);
      CopyToClipboard;
    end;
    // RTF?
    if fExportAs in [0, 2] then with SynExporterRTF1 do begin
      Title := 'Source file exported to clipboard (native format)';
      ExportAsText := FALSE;
      Highlighter := SynEdit1.Highlighter;
      ExportAll(SynEdit1.Lines);
      CopyToClipboard;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TForm1.miExportClipboardTextClick(Sender: TObject);
var
  Exporter: TSynCustomExporter;
begin
  Exporter := nil;
  case fExportAs of
    1: Exporter := SynExporterHTML1;
    2: Exporter := SynExporterRTF1;
  end;
  if Assigned(Exporter) then with Exporter do begin
    Title := 'Source file exported to clipboard (as text)';
    ExportAsText := TRUE;
    Highlighter := SynEdit1.Highlighter;
    ExportAll(SynEdit1.Lines);
    CopyToClipboard;
  end;
end;

end.

