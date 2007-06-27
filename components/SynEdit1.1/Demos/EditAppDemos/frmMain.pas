{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-09-08.

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

$Id: frmMain.pas,v 1.2 2000/11/22 08:34:14 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit frmMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, uEditAppIntfs, ComCtrls;

type
  TMainForm = class(TForm)
    mnuMain: TMainMenu;
    mFile: TMenuItem;
    miFileExit: TMenuItem;
    miFileNew: TMenuItem;
    N1: TMenuItem;
    mEdit: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    miFileClose: TMenuItem;
    miEditUndo: TMenuItem;
    miEditRedo: TMenuItem;
    N2: TMenuItem;
    miEditCut: TMenuItem;
    miEditCopy: TMenuItem;
    miEditPaste: TMenuItem;
    miEditDelete: TMenuItem;
    miEditSelectAll: TMenuItem;
    N3: TMenuItem;
    miEditFind: TMenuItem;
    miEditFindNext: TMenuItem;
    miEditFindPrev: TMenuItem;
    miEditReplace: TMenuItem;
    StatusBar: TStatusBar;
    miViewStatusbar: TMenuItem;
    mView: TMenuItem;
    N4: TMenuItem;
    mRecentFiles: TMenuItem;
    miFileMRU5: TMenuItem;
    miFileMRU4: TMenuItem;
    miFileMRU3: TMenuItem;
    miFileMRU2: TMenuItem;
    miFileMRU1: TMenuItem;
    N5: TMenuItem;
    miFilePrint: TMenuItem;
    actlStandard: TActionList;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileExit: TAction;
    actViewStatusbar: TAction;
    actUpdateStatusBarPanels: TAction;
    actFileCloseAll: TAction;
    miFileCloseAll: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mFileClick(Sender: TObject);
    procedure actFileNewOrOpenUpdate(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure mRecentFilesClick(Sender: TObject);
    procedure actViewStatusbarUpdate(Sender: TObject);
    procedure actViewStatusbarExecute(Sender: TObject);
    procedure OnOpenMRUFile(Sender: TObject);
    procedure actUpdateStatusBarPanelsUpdate(Sender: TObject);
    procedure actFileCloseAllExecute(Sender: TObject);
    procedure actFileCloseAllUpdate(Sender: TObject);
  protected
    fMRUItems: array[1..5] of TMenuItem;
    function CanCloseAll: boolean;
    function CmdLineOpenFiles(AMultipleFiles: boolean): boolean;
    function DoCreateEditor(AFileName: string): IEditor; virtual;
    procedure DoOpenFile(AFileName: string);
    procedure ReadIniSettings;
    procedure WriteIniSettings;
  end;

implementation

{$R *.DFM}

uses
  IniFiles, dmCommands;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fMRUItems[1] := miFileMRU1;
  fMRUItems[2] := miFileMRU2;
  fMRUItems[3] := miFileMRU3;
  fMRUItems[4] := miFileMRU4;
  fMRUItems[5] := miFileMRU5;
  CommandsDataModule := TCommandsDataModule.Create(Self);
  ReadIniSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if GI_EditorFactory <> nil then
    GI_EditorFactory.CloseAll;
  WriteIniSettings;
  CommandsDataModule.Free;
end;

// implementation

function TMainForm.CanCloseAll: boolean;
begin
  Result := TRUE;
end;

function TMainForm.CmdLineOpenFiles(AMultipleFiles: boolean): boolean;
var
  i, Cnt: integer;
begin
  Cnt := ParamCount;
  if Cnt > 0 then begin
    if not AMultipleFiles and (Cnt > 1) then
      Cnt := 1;
    for i := 1 to Cnt do
      DoOpenFile(ParamStr(i));
    Result := TRUE;
  end else
    Result := FALSE;
end;

function TMainForm.DoCreateEditor(AFileName: string): IEditor;
begin
  Result := nil;
end;

procedure TMainForm.DoOpenFile(AFileName: string);
var
  i: integer;
  LEditor: IEditor;
begin
  AFileName := ExpandFileName(AFileName);
  if AFileName <> '' then begin
    CommandsDataModule.RemoveMRUEntry(AFileName);
    // activate the editor if already open
    Assert(GI_EditorFactory <> nil);
    for i := GI_EditorFactory.GetEditorCount - 1 downto 0 do begin
      LEditor := GI_EditorFactory.Editor[i];
      if CompareText(LEditor.GetFileName, AFileName) = 0 then begin
        LEditor.Activate;
        exit;
      end;
    end;
  end;
  // create a new editor, add it to the editor list, open the file
  LEditor := DoCreateEditor(AFileName);
  if LEditor <> nil then
    LEditor.OpenFile(AFileName);
end;

procedure TMainForm.ReadIniSettings;
var
  iniFile: TIniFile;
  x, y, w, h: integer;
  i: integer;
  s: string;
begin
  iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    x := iniFile.ReadInteger('Main', 'Left', 0);
    y := iniFile.ReadInteger('Main', 'Top', 0);
    w := iniFile.ReadInteger('Main', 'Width', 0);
    h := iniFile.ReadInteger('Main', 'Height', 0);
    if (w > 0) and (h > 0) then
      SetBounds(x, y, w, h);
    if iniFile.ReadInteger('Main', 'Maximized', 0) <> 0 then
      WindowState := wsMaximized;
    StatusBar.Visible := iniFile.ReadInteger('Main', 'ShowStatusbar', 1) <> 0;
    // MRU files
    for i := 5 downto 1 do begin
      s := iniFile.ReadString('MRUFiles', Format('MRUFile%d', [i]), '');
      if s <> '' then
        CommandsDataModule.AddMRUEntry(s);
    end;
  finally
    iniFile.Free;
  end;
end;

procedure TMainForm.WriteIniSettings;
var
  iniFile: TIniFile;
  wp: TWindowPlacement;
  i: integer;
  s: string;
begin
  iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    wp.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @wp);
    // form properties
    with wp.rcNormalPosition do begin
      iniFile.WriteInteger('Main', 'Left', Left);
      iniFile.WriteInteger('Main', 'Top', Top);
      iniFile.WriteInteger('Main', 'Width', Right - Left);
      iniFile.WriteInteger('Main', 'Height', Bottom - Top);
    end;
    iniFile.WriteInteger('Main', 'Maximized', Ord(WindowState = wsMaximized));
    iniFile.WriteInteger('Main', 'ShowStatusbar', Ord(Statusbar.Visible));
    // MRU files
    for i := 1 to 5 do begin
      s := CommandsDataModule.GetMRUEntry(i - 1);
      if s <> '' then
        iniFile.WriteString('MRUFiles', Format('MRUFile%d', [i]), s)
      else
        iniFile.DeleteKey('MRUFiles', Format('MRUFile%d', [i]));
    end;
  finally
    iniFile.Free;
  end;
end;

// action handler methods

procedure TMainForm.actFileNewOrOpenUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GI_EditorFactory <> nil;
end;

procedure TMainForm.actFileNewExecute(Sender: TObject);
begin
  DoOpenFile('');
end;

procedure TMainForm.actFileOpenExecute(Sender: TObject);
begin
  with CommandsDataModule.dlgFileOpen do begin
    if Execute then
      DoOpenFile(FileName);
  end;
end;

procedure TMainForm.actFileCloseAllExecute(Sender: TObject);
var
  i: integer;
begin
  if GI_EditorFactory <> nil then begin
    if not CanCloseAll then
      exit;
    i := GI_EditorFactory.GetEditorCount - 1;
    // close all editor childs
    while i >= 0 do begin
      GI_EditorFactory.GetEditor(i).Close;
      Dec(i);
    end;
  end;
end;

procedure TMainForm.actFileCloseAllUpdate(Sender: TObject);
begin
  actFileCloseAll.Enabled := (GI_EditorFactory <> nil)
    and (GI_EditorFactory.GetEditorCount > 0);
end;

procedure TMainForm.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.mRecentFilesClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  for i := Low(fMRUItems) to High(fMRUItems) do
    if fMRUItems[i] <> nil then begin
      s := CommandsDataModule.GetMRUEntry(i - Low(fMRUItems));
      fMRUItems[i].Visible := s <> '';
      fMRUItems[i].Caption := s;
    end;
end;

procedure TMainForm.mFileClick(Sender: TObject);
begin
  mRecentFiles.Enabled := CommandsDataModule.GetMRUEntries > 0;
end;

procedure TMainForm.actViewStatusbarUpdate(Sender: TObject);
begin
  actViewStatusbar.Checked := StatusBar.Visible;
end;

procedure TMainForm.actViewStatusbarExecute(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TMainForm.OnOpenMRUFile(Sender: TObject);
var
  i: integer;
  s: string;
begin
  for i := Low(fMRUItems) to High(fMRUItems) do
    if Sender = fMRUItems[i] then begin
      s := CommandsDataModule.GetMRUEntry(i - 1);
      if s <> '' then
        DoOpenFile(s);
    end;
end;

procedure TMainForm.actUpdateStatusBarPanelsUpdate(Sender: TObject);
resourcestring
  SModified = 'Modified';
var
  ptCaret: TPoint;
begin
  actUpdateStatusBarPanels.Enabled := TRUE;
  if GI_ActiveEditor <> nil then begin
    ptCaret := GI_ActiveEditor.GetCaretPos;
    if (ptCaret.X > 0) and (ptCaret.Y > 0) then
      StatusBar.Panels[0].Text := Format(' %6d:%3d ', [ptCaret.Y, ptCaret.X])
    else
      StatusBar.Panels[0].Text := '';
    if GI_ActiveEditor.GetModified then
      StatusBar.Panels[1].Text := SModified
    else
      StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := GI_ActiveEditor.GetEditorState;
  end else begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
  end;
end;

end.

