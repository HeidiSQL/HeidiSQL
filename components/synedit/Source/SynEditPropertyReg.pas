{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPropertyReg.pas, released 2000-04-07.
The Original Code is based on mwEditPropertyReg.pas, part of the
mwEdit component suite.
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

$Id: SynEditPropertyReg.pas,v 1.7 2002/04/08 18:35:30 jrx Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditPropertyReg;

{$I SynEdit.inc}

interface

{*****************}
{$IFNDEF SYN_CLX}  //js 07-04-2002 changed to SYN_CLX from SYN_KYLIX
procedure Register;
{$ENDIF}

implementation

uses
  Classes,
{$IFDEF SYN_CLX}
  QDialogs,
  QForms,
  QGraphics,
  QControls,
{$ELSE}
  Dialogs,
  Forms,
  Graphics,
  Controls,
{$IFDEF SYN_COMPILER_6_UP}
  VCLEditors,  //js 07-04-2002 don't use in clx version
{$ENDIF}
{$ENDIF}
{$IFDEF SYN_COMPILER_6_UP}
  DesignIntf,
  DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  SynEditKeyCmds,
  SynEditKeyCmdsEditor,
  SynEdit,
  SynEditPrint,
  SynEditPrintMargins,
  SynEditPrintMarginsDialog,
  SynAutoCorrect,
  SynAutoCorrectEditor;

{**************}
{$IFNDEF SYN_CLX}  //js 07-04-2002 changed to SYN_CLX from SYN_KYLIX
type
  TSynEditFontProperty = class(TFontProperty)
  public
    procedure Edit; override;
  end;

  TSynEditCommandProperty = class(TIntegerProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TSynEditKeystrokesProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TSynEditPrintMarginsProperty = class(TClassProperty)
  public
   	procedure Edit; override;
   	function GetAttributes: TPropertyAttributes; override;
  end;

  TAutoCorrectionProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue:string; override;
  end;

  TSynAutoCorrectComponentEditor = class(TDefaultEditor)
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


{ TSynEditFontProperty }

procedure TSynEditFontProperty.Edit;
const
  { context ids for the Font editor }
  hcDFontEditor = 25000;
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue);
    FontDialog.HelpContext := hcDFontEditor;
  {$IFDEF SYN_CLX}
  {$ELSE}
    FontDialog.Options := FontDialog.Options + [fdShowHelp, fdForceFontExist,
       fdFixedPitchOnly];
  {$ENDIF}
    if FontDialog.Execute then
      SetOrdValue(Longint(FontDialog.Font));
  finally
    FontDialog.Free;
  end;
end;

{ TSynEditCommandProperty }

procedure TSynEditCommandProperty.Edit;
begin
  ShowMessage('I''m thinking that this will show a dialog that has a list'#13#10+
     'of all editor commands and a description of them to choose from.');
end;

function TSynEditCommandProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TSynEditCommandProperty.GetValue: string;
begin
  Result := EditorCommandToCodeString(TSynEditorCommand(GetOrdValue));
end;

procedure TSynEditCommandProperty.GetValues(Proc: TGetStrProc);
begin
  GetEditorCommandValues(Proc);
end;

procedure TSynEditCommandProperty.SetValue(const Value: string);
var
  NewValue: longint;
begin
  if IdentToEditorCommand(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

{ TSynEditKeystrokesProperty }

procedure TSynEditKeystrokesProperty.Edit;
var
  Dlg: TSynEditKeystrokesEditorForm;
begin
  Application.CreateForm(TSynEditKeystrokesEditorForm, Dlg);
  try
    Dlg.Caption := Self.GetName;
    Dlg.Keystrokes := TSynEditKeystrokes(GetOrdValue);
    if Dlg.ShowModal = mrOk then
    begin
      { SetOrdValue will operate on all selected propertiy values }
      SetOrdValue(Longint(Dlg.Keystrokes));
      Modified;
    end;
  finally
    Dlg.Free;
  end;
end;

function TSynEditKeystrokesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TSynEditPrintMarginsProperty }

procedure TSynEditPrintMarginsProperty.Edit;
var
  SynEditPrintMarginsDlg : TSynEditPrintMarginsDlg;
begin
  SynEditPrintMarginsDlg := TSynEditPrintMarginsDlg.Create(nil);
  try
    SynEditPrintMarginsDlg.SetMargins(TSynEditPrintMargins(GetOrdValue));
    if SynEditPrintMarginsDlg.ShowModal = mrOk then
      SynEditPrintMarginsDlg.GetMargins(TSynEditPrintMargins(GetOrdValue));
  finally
    SynEditPrintMarginsDlg.Free;
  end;
end;

function TSynEditPrintMarginsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties, paReadOnly, paSortList];
end;

procedure TSynAutoCorrectComponentEditor.Edit;
begin
  frmAutoCorrectEditor := TfrmAutoCorrectEditor.Create(Application);
  frmAutoCorrectEditor.SynAutoCorrect := TSynAutoCorrect(Component);
  frmAutoCorrectEditor.ShowModal;
  frmAutoCorrectEditor.Free;

  Designer.Modified;
end;

procedure TSynAutoCorrectComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TSynAutoCorrectComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := '&Edit...';
  end;
end;

function TSynAutoCorrectComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TAutoCorrectionProperty.Edit;
begin
   frmAutoCorrectEditor := TfrmAutoCorrectEditor.Create(Application);
   frmAutoCorrectEditor.SynAutoCorrect := TSynAutoCorrect(GetComponent(0));
   frmAutoCorrectEditor.ShowModal;
   frmAutoCorrectEditor.Free;

   Designer.Modified;
end;

function TAutoCorrectionProperty.GetAttributes: TPropertyAttributes;
begin
  GetAttributes := [paDialog, paReadOnly];
end;

function TAutoCorrectionProperty.GetValue: String;
begin
  GetValue := '(AutoCorrections)';
end;


{ Register }

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TFont), TCustomSynEdit,
     'Font', TSynEditFontProperty);
  RegisterPropertyEditor(TypeInfo(TSynEditorCommand), NIL,
     'Command', TSynEditCommandProperty);
  RegisterPropertyEditor(TypeInfo(TSynEditKeystrokes), NIL, '',
     TSynEditKeystrokesProperty);
  RegisterPropertyEditor(TypeInfo(TSynEditPrintMargins), NIL, '',
     TSynEditPrintMarginsProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TSynAutoCorrect, 'ReplaceItems',
     TAutoCorrectionProperty);
  RegisterComponentEditor(TSynAutoCorrect, TSynAutoCorrectComponentEditor);
end;

{$ENDIF}

end.

