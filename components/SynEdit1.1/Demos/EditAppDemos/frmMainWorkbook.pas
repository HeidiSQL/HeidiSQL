{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMainWorkbook.pas, released 2000-09-08.

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

$Id: frmMainWorkbook.pas,v 1.2 2000/11/22 08:34:14 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit frmMainWorkbook;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  frmMain, ComCtrls, ActnList, Menus, uEditAppIntfs;

type
  TWorkbookMainForm = class(TMainForm)
    pctrlMain: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  protected
    function DoCreateEditor(AFileName: string): IEditor; override;
  end;

var
  WorkbookMainForm: TWorkbookMainForm;

implementation

{$R *.DFM}

{ TWorkbookMainForm }

function TWorkbookMainForm.DoCreateEditor(AFileName: string): IEditor;
begin
  if GI_EditorFactory <> nil then
    Result := GI_EditorFactory.CreateTabSheet(pctrlMain)
  else
    Result := nil;
end;

procedure TWorkbookMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  CmdLineOpenFiles(TRUE);
end;

procedure TWorkbookMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  if GI_EditorFactory <> nil then
    CanClose := GI_EditorFactory.CanCloseAll;
end;

end.

