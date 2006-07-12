{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-11-11.

The Original Code is part of the SimpleIDEDemo project, written by
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

$Id: frmMain.pas,v 1.3 2000/11/22 08:37:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit frmMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ImgList, ComCtrls, ToolWin, SynEdit, SynEditHighlighter,
  SynHighlighterPas, uSimpleIDEDebugger, Menus;

type
  TSimpleIDEMainForm = class(TForm)
    tbDebug: TToolBar;
    tbtnRun: TToolButton;
    tbtnStep: TToolButton;
    tbtnGotoCursor: TToolButton;
    tbtnPause: TToolButton;
    tbtnStop: TToolButton;
    imglActions: TImageList;
    actlMain: TActionList;
    actDebugRun: TAction;
    actDebugStep: TAction;
    actDebugGotoCursor: TAction;
    actDebugPause: TAction;
    actDebugStop: TAction;
    ToolButton1: TToolButton;
    tbtnToggleBreakpoint: TToolButton;
    tbtnClearAllBreakpoints: TToolButton;
    actToggleBreakpoint: TAction;
    actClearAllBreakpoints: TAction;
    SynEditor: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    imglGutterGlyphs: TImageList;
    MainMenu1: TMainMenu;
    mDebug: TMenuItem;
    miDebugRun: TMenuItem;
    miDebugStep: TMenuItem;
    miDebugGotoCursor: TMenuItem;
    miDebugPause: TMenuItem;
    miDebugStop: TMenuItem;
    N1: TMenuItem;
    miToggleBreakpoint: TMenuItem;
    miClearBreakpoints: TMenuItem;
    Statusbar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SynEditorGutterClick(Sender: TObject; X, Y, Line: Integer;
      mark: TSynEditMark);
    procedure SynEditorSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure actDebugRunExecute(Sender: TObject);
    procedure actDebugRunUpdate(Sender: TObject);
    procedure actDebugStepExecute(Sender: TObject);
    procedure actDebugStepUpdate(Sender: TObject);
    procedure actDebugGotoCursorExecute(Sender: TObject);
    procedure actDebugGotoCursorUpdate(Sender: TObject);
    procedure actDebugPauseExecute(Sender: TObject);
    procedure actDebugPauseUpdate(Sender: TObject);
    procedure actDebugStopExecute(Sender: TObject);
    procedure actDebugStopUpdate(Sender: TObject);
    procedure actToggleBreakpointExecute(Sender: TObject);
    procedure actToggleBreakpointUpdate(Sender: TObject);
    procedure actClearAllBreakpointsExecute(Sender: TObject);
    procedure actClearAllBreakpointsUpdate(Sender: TObject);
  private
    fCurrentLine: integer;
    fDebugger: TSampleDebugger;
    procedure DebuggerBreakpointChange(Sender: TObject; ALine: integer);
    procedure DebuggerCurrentLineChange(Sender: TObject);
    procedure DebuggerStateChange(Sender: TObject; OldState,
      NewState: TDebuggerState);
    procedure DebuggerYield(Sender: TObject);
    procedure PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: integer);
    procedure SetCurrentLine(ALine: integer);
  end;

var
  SimpleIDEMainForm: TSimpleIDEMainForm;

implementation

{$R *.DFM}

{ TGutterMarkDrawPlugin }

type
  TDebugSupportPlugin = class(TSynEditPlugin)
  protected
    fForm: TSimpleIDEMainForm;
    procedure AfterPaint(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(AForm: TSimpleIDEMainForm);
  end;

constructor TDebugSupportPlugin.Create(AForm: TSimpleIDEMainForm);
begin
  inherited Create(AForm.SynEditor);
  fForm := AForm;
end;

procedure TDebugSupportPlugin.AfterPaint(ACanvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
begin
  fForm.PaintGutterGlyphs(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TDebugSupportPlugin.LinesInserted(FirstLine, Count: integer);
begin
// Note: You will need this event if you want to track the changes to
//       breakpoints in "Real World" apps, where the editor is not read-only
end;

procedure TDebugSupportPlugin.LinesDeleted(FirstLine, Count: integer);
begin
// Note: You will need this event if you want to track the changes to
//       breakpoints in "Real World" apps, where the editor is not read-only
end;

{ TSimpleIDEMainForm }

procedure TSimpleIDEMainForm.FormCreate(Sender: TObject);
var
  Settings: TStringList;
begin
  fCurrentLine := -1;
  fDebugger := TSampleDebugger.Create;
  with fDebugger do begin
    OnBreakpointChange := DebuggerBreakpointChange;
    OnCurrentLineChange := DebuggerCurrentLineChange;
    OnStateChange := DebuggerStateChange;
    OnYield := DebuggerYield;
  end;

  TDebugSupportPlugin.Create(Self);

  Settings := TStringList.Create;
  try
    SynPasSyn1.EnumUserSettings(Settings);
    if Settings.Count > 0 then
      SynPasSyn1.UseUserSettings(Settings.Count - 1);
  finally
    Settings.Free;
  end;
  SynEditor.Text := SampleSource;
end;

procedure TSimpleIDEMainForm.FormDestroy(Sender: TObject);
begin
  fDebugger.Free;
end;

procedure TSimpleIDEMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if fDebugger.IsRunning then begin
    fDebugger.Stop;
    CanClose := FALSE;
  end;
end;

procedure TSimpleIDEMainForm.SynEditorGutterClick(Sender: TObject; X, Y,
  Line: Integer; mark: TSynEditMark);
begin
  if fDebugger <> nil then
    fDebugger.ToggleBreakpoint(Line);
end;

procedure TSimpleIDEMainForm.SynEditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  LI: TDebuggerLineInfos;
begin
  if fDebugger <> nil then begin
    LI := fDebugger.GetLineInfos(Line);
    if dlCurrentLine in LI then begin
      Special := TRUE;
      FG := clWhite;
      BG := clBlue;
    end else if dlBreakpointLine in LI then begin
      Special := TRUE;
      FG := clWhite;
      if dlExecutableLine in LI then
        BG := clRed
      else
        BG := clGray;
    end;
  end;
end;

procedure TSimpleIDEMainForm.DebuggerBreakpointChange(Sender: TObject;
  ALine: integer);
begin
  if (ALine >= 1) and (ALine <= SynEditor.Lines.Count) then
    SynEditor.InvalidateLine(ALine)
  else
    SynEditor.Invalidate;
end;

procedure TSimpleIDEMainForm.DebuggerCurrentLineChange(Sender: TObject);
begin
  if (fDebugger <> nil) and not fDebugger.IsRunning then
    SetCurrentLine(fDebugger.CurrentLine)
  else
    SetCurrentLine(-1);
end;

procedure TSimpleIDEMainForm.DebuggerStateChange(Sender: TObject; OldState,
  NewState: TDebuggerState);
var
  s: string;
begin
  case NewState of
    dsRunning: s := 'Program is running';
    dsPaused: s := 'Program is paused';
  else
    s := 'Ready';
  end;
  Statusbar.SimpleText := ' ' + s;
end;

procedure TSimpleIDEMainForm.DebuggerYield(Sender: TObject);
begin
  UpdateActions;
  Application.ProcessMessages;
end;

procedure TSimpleIDEMainForm.PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  LH, X, Y: integer;
  LI: TDebuggerLineInfos;
  ImgIndex: integer;
begin
  if fDebugger <> nil then begin
    X := 14;
    LH := SynEditor.LineHeight;
    Y := (LH - imglGutterGlyphs.Height) div 2
      + LH * (FirstLine - SynEditor.TopLine);
    while FirstLine <= LastLine do begin
      LI := fDebugger.GetLineInfos(FirstLine);
      if dlCurrentLine in LI then begin
        if dlBreakpointLine in LI then
          ImgIndex := 2
        else
          ImgIndex := 1;
      end else if dlExecutableLine in LI then begin
        if dlBreakpointLine in LI then
          ImgIndex := 3
        else
          ImgIndex := 0;
      end else begin
        if dlBreakpointLine in LI then
          ImgIndex := 4
        else
          ImgIndex := -1;
      end;
      if ImgIndex >= 0 then
        imglGutterGlyphs.Draw(ACanvas, X, Y, ImgIndex);
      Inc(FirstLine);
      Inc(Y, LH);
    end;
  end;
end;

procedure TSimpleIDEMainForm.SetCurrentLine(ALine: integer);
begin
  if fCurrentLine <> ALine then begin
    SynEditor.InvalidateLine(fCurrentLine);
    fCurrentLine := ALine;
    if (fCurrentLine > 0) and (SynEditor.CaretY <> fCurrentLine) then
      SynEditor.CaretXY := Point(1, fCurrentLine);
    SynEditor.InvalidateLine(fCurrentLine);
  end;
end;

procedure TSimpleIDEMainForm.actDebugRunExecute(Sender: TObject);
begin
  fDebugger.Run;
end;

procedure TSimpleIDEMainForm.actDebugRunUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (fDebugger <> nil) and fDebugger.CanRun;
end;

procedure TSimpleIDEMainForm.actDebugStepExecute(Sender: TObject);
begin
  fDebugger.Step;
end;

procedure TSimpleIDEMainForm.actDebugStepUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (fDebugger <> nil) and fDebugger.CanStep;
end;

procedure TSimpleIDEMainForm.actDebugGotoCursorExecute(Sender: TObject);
begin
  fDebugger.GotoCursor(SynEditor.CaretY);
end;

procedure TSimpleIDEMainForm.actDebugGotoCursorUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (fDebugger <> nil)
    and fDebugger.CanGotoCursor(SynEditor.CaretY);
end;

procedure TSimpleIDEMainForm.actDebugPauseExecute(Sender: TObject);
begin
  fDebugger.Pause;
end;

procedure TSimpleIDEMainForm.actDebugPauseUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (fDebugger <> nil) and fDebugger.CanPause;
end;

procedure TSimpleIDEMainForm.actDebugStopExecute(Sender: TObject);
begin
  fDebugger.Stop;
end;

procedure TSimpleIDEMainForm.actDebugStopUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (fDebugger <> nil) and fDebugger.CanStop;
end;

procedure TSimpleIDEMainForm.actToggleBreakpointExecute(Sender: TObject);
begin
  fDebugger.ToggleBreakpoint(SynEditor.CaretY);
end;

procedure TSimpleIDEMainForm.actToggleBreakpointUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := fDebugger <> nil;
end;

procedure TSimpleIDEMainForm.actClearAllBreakpointsExecute(
  Sender: TObject);
begin
  fDebugger.ClearAllBreakpoints;
end;

procedure TSimpleIDEMainForm.actClearAllBreakpointsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (fDebugger <> nil)
    and fDebugger.HasBreakpoints;
end;

end.

  