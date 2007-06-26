{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EditAppWorkbook.dpr, released 2000-09-08.

The Original Code is part of the EditAppDemos project, written by
Michael Hieke for the SynEdit component suite.
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

$Id: EditAppWorkbook.dpr,v 1.1 2000/09/16 07:34:43 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

program EditAppWorkbook;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  frmMainWorkbook in 'frmMainWorkbook.pas' {WorkbookMainForm},
  uEditAppIntfs in 'uEditAppIntfs.pas',
  frmEditor in 'frmEditor.pas' {EditorForm},
  dmCommands in 'dmCommands.pas' {CommandsDataModule: TDataModule},
  uHighlighterProcs in '..\uHighlighterProcs.pas',
  dlgSearchText in '..\SearchReplaceDemo\dlgSearchText.pas' {TextSearchDialog},
  dlgReplaceText in '..\SearchReplaceDemo\dlgReplaceText.pas' {TextReplaceDialog},
  dlgConfirmReplace in '..\SearchReplaceDemo\dlgConfirmReplace.pas' {ConfirmReplaceDialog};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SynEdit Demo';
  Application.CreateForm(TWorkbookMainForm, WorkbookMainForm);
  Application.Run;
end.

