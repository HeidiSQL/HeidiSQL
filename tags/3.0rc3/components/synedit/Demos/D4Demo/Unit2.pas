{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Unit2.pas, released 2000-06-23.

The Original Code is the D4Demo project of the mwEdit component suite
by Martin Waldenburg and other developers.
The Original Author of the D4Demo project is Primoz Gabrijelcic.
Portions written by Primoz Gabrijelcic are copyright 1998 Primoz Gabrijelcic.
All Rights Reserved.

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

$Id: Unit2.pas,v 1.2 2000/11/22 08:34:13 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - printing has been removed from D4Demo, since it is covered in greater
    detail in a dedicated example.
-------------------------------------------------------------------------------}

unit Unit2;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm2 = class(TForm)
    lbKeywords: TListBox;
    btnLoad: TButton;
    btnClose: TButton;
    OpenDialog1: TOpenDialog;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    procedure btnLoadClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    lbKeywords.Items.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm2.btnDeleteClick(Sender: TObject);
begin
  if lbKeywords.ItemIndex <> -1 then
    lbKeywords.Items.Delete(lbKeywords.ItemIndex);
end;

procedure TForm2.btnAddClick(Sender: TObject);
var
  val: string;
begin
  val := Inputbox('Add Reserved Word', 'Reserved Word:', '');
  if val <> '' then
    lbKeywords.Items.Add(val);
end;

procedure TForm2.btnEditClick(Sender: TObject);
var
  val: string;
begin
  if lbKeywords.ItemIndex <> -1 then begin
    val := Inputbox('Edit Reserved Word', 'Reserved Word:', lbKeywords.Items[lbKeywords.ItemIndex]);
    if val <> lbKeywords.Items[lbKeywords.ItemIndex] then
      lbKeywords.Items.Add(val);
  end;
end;

end.

  