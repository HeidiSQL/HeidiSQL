{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPythonBehaviour.pas, released 2000-06-23.
The Original Code is based on odPythonBehaviour.pas by Olivier Deckmyn, part
of the mwEdit component suite.

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

$Id: SynEditPythonBehaviour.pas,v 1.3 2001/11/09 07:48:57 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a component which implements editing rules to apply to a Python source file)
@author(Olivier Deckmyn, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1999-10-17)
@lastmod(May 19, 2000)
The  SynEditPythonBehaviour unit provides a simple component implements editing rules to apply
to a python source file. Python has a unusual way to mark blocks (like begin/end in pascal) : it
uses indentation. So the rule is after a ":" and a line break, we have to indent once.
}
unit SynEditPythonBehaviour;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_CLX}
  Qt, QGraphics, QControls, QForms, QDialogs,
  {$ELSE}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  {$ENDIF}
  SynEdit, SynEditKeyCmds;

const
  ecPythonIndent = ecUserFirst + 1974;

type
  TSynEditPythonBehaviour = class(TComponent)
  private
    FEditor: TSynEdit;
    FFormerKeyPress: TKeyPressEvent;
    FFormProcessUserCommand: TProcessCommandEvent;
    fIndent: integer;
  protected
    procedure SetEditor(Value: TSynEdit); virtual;
    procedure doKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure doProcessUserCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer); virtual;
  public
    procedure Loaded; override;
    procedure AttachFormerEvents;
    constructor Create(aOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  published
    property Editor: TSynEdit read FEditor write SetEditor;
    property Indent: integer read fIndent write fIndent default 4;
  end;

implementation

uses
  SynEditStrConst;

procedure TSynEditPythonBehaviour.SetEditor(Value: TSynEdit);
begin
  if FEditor <> Value then begin
    // First restore the former event handlers, if any
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState)
      and assigned(FEditor)
    then begin
      if assigned(FFormerKeyPress) then begin
        FEditor.OnKeypress := FFormerKeyPress;
        FFormerKeyPress := nil;
      end;
      if assigned(FFormProcessUserCommand) then begin
        FEditor.OnProcessUserCommand := FFormProcessUserCommand;
        FFormProcessUserCommand := nil;
      end;
    end;
    // Set the new editor
    FEditor := Value;
    // Attach the new event handlers
    if ComponentState * [csDesigning, csLoading] = [] then
      AttachFormerEvents;
  end;
end; // SetEditor

procedure TSynEditPythonBehaviour.doKeyPress(Sender: TObject; var Key: Char);
var
  i: integer;
  lLine: string;
begin
  if assigned(FFormerKeyPress) then FFormerKeyPress(Sender, Key);

  if assigned(FEditor) and (Key = #13) then begin
    lLine := Trim(FEditor.Lines[FEditor.CaretY - 2]);
    if Copy(lLine, Length(lLine), 1) = ':' then begin
      for i := 1 to fIndent do
        FEditor.CommandProcessor(ecPythonIndent, #0, nil);
    end;
  end;

end;

procedure TSynEditPythonBehaviour.doProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if assigned(FFormProcessUserCommand) then
    FFormProcessUserCommand(Self, Command, aChar, Data);

  if Command = ecPythonIndent then begin
    Command := ecChar;
    aChar := ' ';
  end;
end;

procedure TSynEditPythonBehaviour.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then begin
    AttachFormerEvents;
  end;
end;

procedure TSynEditPythonBehaviour.AttachFormerEvents;
begin
  if assigned(FEditor) then begin
    FFormerKeyPress := FEditor.OnKeyPress;
    FFormProcessUserCommand := FEditor.OnProcessUserCommand;
    FEditor.OnKeyPress := doKeyPress;
    FEditor.OnProcessUserCommand := doProcessUserCommand;
  end;
end;

constructor TSynEditPythonBehaviour.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  FFormerKeyPress := nil;
  FFormProcessUserCommand := nil;
  fIndent := 4;
end;

procedure TSynEditPythonBehaviour.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (aComponent = FEditor) then
    FEditor := nil;
end;

end.

