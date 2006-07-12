{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynAutoCorrectEditor.pas, released 2001-10-05.
Author of this file is Aaron Chan. All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynAutoCorrectEditor.pas,v 1.2 2002/04/08 08:38:13 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynAutoCorrectEditor;

interface

{$I SynEdit.inc}

uses
  {$IFDEF SYN_CLX}  //js 06-04-2002
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QStdCtrls, QButtons, Types,
  {$ELSE}
  Windows,  Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, Registry,

  {$ENDIF}

  SysUtils, Classes,
  SynAutoCorrect;

type
  TfrmAutoCorrectEditor = class(TForm)
    lblLabel1: TLabel;
    lblLabel2: TLabel;
    lbxItems: TListBox;
    pnlSeparator: TPanel;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnClear: TSpeedButton;
    btnEdit: TSpeedButton;
    btnExit: TSpeedButton;
    bvlSeparator: TBevel;
    procedure FormShow(Sender: TObject);
    procedure lbxItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure lbxItemsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    SynAutoCorrect: TSynAutoCorrect;
  end;

var
  frmAutoCorrectEditor: TfrmAutoCorrectEditor;
{$IFNDEF SYN_CLX}  //js 06-04-2002
  Reg: TRegIniFile;
{$ENDIF}

implementation

{$R *.DFM}

procedure TfrmAutoCorrectEditor.FormShow(Sender: TObject);
var
  i: Integer;

begin
{$IFNDEF SYN_CLX} //js 06-04-2002
  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_CURRENT_USER;
  Reg.OpenKey('Software\Aerodynamica\Components\SynAutoCorrect', True);

  i := Reg.ReadInteger('', 'Top', 0);
  if i <> 0 then Top := i;

  i := Reg.ReadInteger('', 'Left', 0);
  if i <> 0 then Left := i;

  i := Reg.ReadInteger('', 'Width', 0);
  if i <> 0 then Width := i;

  i := Reg.ReadInteger('', 'Height', 0);
  if i <> 0 then Height := i;

  lbxItems.Items.Assign(SynAutoCorrect.ReplaceItems);
{$ENDIF}
end;

procedure TfrmAutoCorrectEditor.lbxItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  CurrentText: String;

begin
  CurrentText := lbxItems.Items[Index];

  with lbxItems do
  begin
    Canvas.FillRect(Rect);

    Canvas.TextOut(Rect.Left + 2, Rect.Top, HalfString(CurrentText, True));
    Canvas.TextOut(Rect.Left + (lbxItems.ClientWidth div 2) + 2, Rect.Top, HalfString(CurrentText, False));
  end;
end;

procedure TfrmAutoCorrectEditor.btnAddClick(Sender: TObject);
var
  sReplaceFrom, sReplaceTo: String;

begin
  if InputQuery('Add...', 'Replace:', sReplaceFrom) then
    InputQuery('Add...', 'With:', sReplaceTo)
  else
    Exit;

  with SynAutoCorrect do
  begin
    if (sReplaceFrom <> '') and (sReplaceTo <> '') then
    begin
      Add(sReplaceFrom, sReplaceTo);
      lbxItems.Items.Assign(SynAutoCorrect.ReplaceItems);
    end;
  end;

  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmAutoCorrectEditor.btnDeleteClick(Sender: TObject);
begin
  if lbxItems.ItemIndex < 0 then
  begin
    {$IFDEF SYN_CLX}
    ShowMessage('Please select an item before executing this command!');
    {$ELSE}  //js 06-04-2002 no messagebox in clx
    MessageBox(0, 'Please select an item before executing this command!', 'Error', MB_APPLMODAL or MB_ICONERROR);
    {$ENDIF}

    Exit;
  end;

  SynAutoCorrect.Delete(lbxItems.ItemIndex);
  lbxItems.Items.Assign(SynAutoCorrect.ReplaceItems);

  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmAutoCorrectEditor.btnEditClick(Sender: TObject);
var
  sReplaceFrom, sReplaceTo, CurrentText: String;

begin
  if lbxItems.ItemIndex < 0 then
  begin
    {$IFDEF SYN_CLX}
    ShowMessage('Please select an item before executing this command!');
    {$ELSE}  //js 06-04-2002 no messagebox in clx
    MessageBox(0, 'Please select an item before executing this command!', 'Error', MB_APPLMODAL or MB_ICONERROR);
    {$ENDIF}

    Exit;
  end;

  CurrentText := SynAutoCorrect.ReplaceItems[lbxItems.ItemIndex];
  sReplaceFrom := HalfString(CurrentText, True);
  sReplaceTo := HalfString(CurrentText, False);

  if InputQuery('Edit...', 'Replace:', sReplaceFrom) then
    InputQuery('Edit...', 'With:', sReplaceTo)
  else
    Exit;

  with SynAutoCorrect do
  begin
    Edit(lbxItems.ItemIndex, sReplaceFrom, sReplaceTo);

    lbxItems.Items.Assign(SynAutoCorrect.ReplaceItems);
  end;

  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmAutoCorrectEditor.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAutoCorrectEditor.btnClearClick(Sender: TObject);
begin
  {$IFNDEF SYN_CLX}  //js 06-04-2002
  if MessageBox(0, 'Are you sure you want to clear the entire list?', 'Confirmation', MB_APPLMODAL or MB_YESNO or MB_ICONQUESTION) <> IDYES then Exit;
  {$ENDIF}
  SynAutoCorrect.ReplaceItems.Clear;
  lbxItems.Items.Clear;

  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmAutoCorrectEditor.lbxItemsClick(Sender: TObject);
begin
  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmAutoCorrectEditor.FormResize(Sender: TObject);
begin
  if Height < 215 then Height := 215;
  if Width < 272 then Width := 272;
  
  lbxItems.Height := ClientHeight - 66;
  lbxItems.Width := ClientWidth - 17;
  pnlSeparator.Left := (lbxItems.Width div 2) + lbxItems.Left;
  lblLabel2.Left := pnlSeparator.Left;
  pnlSeparator.Height := lbxItems.Height;
end;

procedure TfrmAutoCorrectEditor.FormDestroy(Sender: TObject);
begin
{$IFNDEF SYN_CLX}//js 07-04-2002
  Reg.WriteInteger('', 'Left', Left);
  Reg.WriteInteger('', 'Top', Top);
  Reg.WriteInteger('', 'Width', Width);
  Reg.WriteInteger('', 'Height', Height);

  Reg.Free;
{$ENDIF}
end;

end.
