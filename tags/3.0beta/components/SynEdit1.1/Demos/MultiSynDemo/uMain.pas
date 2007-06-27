{-------------------------------------------------------------------------------
The purpose of this demo is to show how to implement the TSynMultiSyn control
allowing you to syntax highlight documents with many highlighters based on
schemes that you define.

I thought it would be a great help by setting up the AutoComplete and
Completion proposal. When running the demo use them to correctly insert the tags
that I setup the SynMultiSyn1.Schemes to look for as Start / End Expression.
--------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uMain.pas, released 2001-03-31.

The Original Code is part of the MultiSynDemo project, written by Leon Brown
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

$Id: uMain.pas,v 1.1 2001/03/31 20:34:07 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit uMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynCompletionProposal, SynHighlighterJScript, SynHighlighterHTML,
  SynHighlighterMulti, SynEditHighlighter, SynHighlighterCss, SynEdit,
  Menus, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    About1: TMenuItem;
    Exit1: TMenuItem;
    File1: TMenuItem;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Save1: TMenuItem;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletionProposal1: TSynCompletionProposal;
    SynCssSyn1: TSynCssSyn;
    SynEdit1: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynMultiSyn1: TSynMultiSyn;
    Panel1: TPanel;
    ListBox1: TListBox;
    Label1: TLabel;
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
{ Initialize the controls }
  SynEdit1.ClearAll;
{ The D4Demo uses this...
  OpenDialog1.Filter := GetHighlightersFilter(fHighlighters) +
    'All files|*.*|';
  If you set SynMultiSyn1.DefalutFilter at Design time, you get a
  repeat filter if it is the same as another highlighter. }
  OpenDialog1.Filter := SynMultiSyn1.DefaultFilter;
  SaveDialog1.Filter := SynMultiSyn1.DefaultFilter;
{ Set SynMultiSyn1 Filter to HTML because that is the example I'm
  using. }
  SynMultiSyn1.DefaultFilter := SynHTMLSyn1.DefaultFilter;

{ Load Completion and Autocomplete from user maintained text files }
  SynCompletionProposal1.ItemList.LoadFromFile(
    ExtractFilePath(ParamStr(0)) + 'SynCP.txt');
  SynAutoComplete1.AutoCompleteList.LoadFromFile(
    ExtractFilePath(ParamStr(0)) + 'SynAC.txt');
end;

procedure TfrmMain.New1Click(Sender: TObject);
begin
  SynEdit1.ClearAll;
end;

procedure TfrmMain.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
    SynEdit1.SetFocus;
  end;
end;

procedure TfrmMain.Save1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then begin
    SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
    SynEdit1.SetFocus;
  end;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  MessageDlg('MultiHighlight HTML Demo'#13#10 +
    'by'#13#10 +
    'Leon Brown - LeonBrown77@hotmail.com', mtInformation, [mbOk], 0);
  SynEdit1.SetFocus;
end;


procedure TfrmMain.ListBox1DblClick(Sender: TObject);
begin
{ Want to add text to SynEdit to be used with AutoComplete }
  SynEdit1.SelText := ListBox1.Items.Strings[ListBox1.ItemIndex];
  SynEdit1.SetFocus;
end;

procedure TfrmMain.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  // caret position has changed
  if Changes * [scAll, scCaretX, scCaretY] <> [] then begin
    Statusbar1.SimpleText := Format('Ln:%6d, Col:%3d',
      [SynEdit1.CaretY, SynEdit1.CaretX]);
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  SynEdit1.SetFocus;
end;

end.

