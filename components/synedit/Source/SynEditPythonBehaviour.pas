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
Unicode translation by Maël Hörz.

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

$Id: SynEditPythonBehaviour.pas,v 1.5.2.3 2008/09/14 16:24:59 maelh Exp $

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
{$IFNDEF QSYNEDITPYTHONBEHAVIOUR}
unit SynEditPythonBehaviour;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  SynEdit,
  SynEditKeyCmds,
  SynUnicode,
  SysUtils,
  Classes;

type
  TSynEditPythonBehaviour = class(TComponent)
  private
    FEditor: TSynEdit;
    FIndent: Integer;
  protected
    procedure SetEditor(Value: TSynEdit); virtual;
    procedure doProcessUserCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer; HandlerData: Pointer); virtual;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Editor: TSynEdit read FEditor write SetEditor;
    property Indent: Integer read FIndent write FIndent default 4;
  end;

implementation

uses
  SynEditStrConst;

procedure TSynEditPythonBehaviour.SetEditor(Value: TSynEdit);
begin
  if FEditor <> Value then
  begin
    if (Editor <> nil) and not (csDesigning in ComponentState) then
      Editor.UnregisterCommandHandler(doProcessUserCommand);
    // Set the new editor
    FEditor := Value;
    if (Editor <> nil) and not (csDesigning in ComponentState) then
      Editor.RegisterCommandHandler(doProcessUserCommand, nil);
  end;
end; 

procedure TSynEditPythonBehaviour.doProcessUserCommand(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer;
  HandlerData: Pointer);
var
  iEditor: TCustomSynEdit;
  iPrevLine: UnicodeString;
  cSpace: Integer;
begin
  if (Command = ecLineBreak) and AfterProcessing then
  begin
    iEditor := Sender as TCustomSynEdit;
    { CaretY should never be lesser than 2 right after ecLineBreak, so there's
    no need for a check }
    iPrevLine := WideTrimRight(iEditor.Lines[iEditor.CaretY - 2]);
    if (iPrevLine <> '') and (iPrevLine[Length(iPrevLine)] = ':') then
    begin
      iEditor.UndoList.BeginBlock;
      try
        for cSpace := 1 to Indent do
          iEditor.ExecuteCommand(ecChar, #32, nil);
      finally
        iEditor.UndoList.EndBlock;
      end;
    end;
  end;
end;

constructor TSynEditPythonBehaviour.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndent := 4;
end;

end.
