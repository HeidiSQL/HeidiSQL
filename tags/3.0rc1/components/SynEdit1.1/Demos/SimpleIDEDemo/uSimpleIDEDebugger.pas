{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uSimpleIDEDebugger.pas, released 2000-11-11.

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

$Id: uSimpleIDEDebugger.pas,v 1.2 2000/11/11 19:12:59 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit uSimpleIDEDebugger;

{$I SynEdit.inc}
   
interface

uses
  Windows, Classes;

type
  TDebuggerState = (dsStopped, dsRunning, dsPaused);

  TDebuggerLineInfo = (dlCurrentLine, dlBreakpointLine, dlExecutableLine);
  TDebuggerLineInfos = set of TDebuggerLineInfo;
  
  TBreakpointChangeEvent = procedure(Sender: TObject; ALine: integer) of object;
  TDebuggerStateChangeEvent = procedure(Sender: TObject;
    OldState, NewState: TDebuggerState) of object;

  TSampleDebugger = class(TObject)
  private
    fBreakpoints: TList;
    fCurrentLine: integer;
    fDebuggerState: TDebuggerState;
    fLineToStop: integer;
    fNextInstruction: integer;
    fWantedState: TDebuggerState;
    fOnBreakpointChange: TBreakpointChangeEvent;
    fOnCurrentLineChange: TNotifyEvent;
    fOnStateChange: TDebuggerStateChangeEvent;
    fOnYield: TNotifyEvent;
    function CurrentLineIsBreakpoint: boolean;
    procedure DoOnBreakpointChanged(ALine: integer);
    procedure DoCurrentLineChanged;
    procedure DoStateChange;
    procedure DoYield;
  public
    constructor Create;
    destructor Destroy; override;

    function CanGotoCursor(ALine: integer): boolean;
    function CanPause: boolean;
    function CanRun: boolean;
    function CanStep: boolean;
    function CanStop: boolean;
    procedure ClearAllBreakpoints;
    function GetLineInfos(ALine: integer): TDebuggerLineInfos;
    procedure GotoCursor(ALine: integer);
    function HasBreakpoints: boolean;
    function IsBreakpointLine(ALine: integer): boolean;
    function IsExecutableLine(ALine: integer): boolean;
    function IsRunning: boolean;
    procedure Pause;
    procedure Run;
    procedure Step;
    procedure Stop;
    procedure ToggleBreakpoint(ALine: integer);
  public
    property CurrentLine: integer read fCurrentLine;
    property OnBreakpointChange: TBreakpointChangeEvent read fOnBreakpointChange
      write fOnBreakpointChange;
    property OnCurrentLineChange: TNotifyEvent read fOnCurrentLineChange
      write fOnCurrentLineChange;
    property OnStateChange: TDebuggerStateChangeEvent read fOnStateChange
      write fOnStateChange;
    property OnYield: TNotifyEvent read fOnYield write fOnYield;
  end;

const
  SampleSource =
{  1 }  'program Test;'#13#10 +
{  2 }  ''#13#10 +
{  3 }  'procedure TestProc;'#13#10 +
{  4 }  'begin'#13#10 +
{  5 }  '  DoNothing;'#13#10 +
{  6 }  'end;'#13#10 +
{  7 }  ''#13#10 +
{  8 }  'var'#13#10 +
{  9 }  '  i: integer;'#13#10 +
{ 10 }  ''#13#10 +
{ 11 }  'begin'#13#10 +
{ 12 }  '  while TRUE do'#13#10 +
{ 13 }  '    TestProc;'#13#10 +
{ 14 }  'end.';

type
  TSampleExecutableLine = record
    Line: integer;
    Delta: integer; // to change the array index
  end;

const
  SampleCode: array[0..7] of TSampleExecutableLine = (
    (Line: 11; Delta: 1), (Line: 12; Delta: 1),
    (Line: 13; Delta: 1), (Line:  4; Delta: 1),
    (Line:  5; Delta: 1), (Line:  6; Delta: -4),
    (Line: 14; Delta: 1), (Line: -1; Delta: 0));
  ExecutableLines: array[0..6] of integer = (4, 5, 6, 11, 12, 13, 14);

implementation

{ TSampleDebugger }

constructor TSampleDebugger.Create;
begin
  inherited Create;
  fBreakpoints := TList.Create;
  fCurrentLine := -1;
  fDebuggerState := dsStopped;
  fNextInstruction := Low(SampleCode);
end;

destructor TSampleDebugger.Destroy;
begin
  fBreakpoints.Free;
  inherited Destroy;
end;

function TSampleDebugger.CanGotoCursor(ALine: integer): boolean;
begin
  Result := (fDebuggerState <> dsRunning) and IsExecutableLine(ALine);
end;

function TSampleDebugger.CanPause: boolean;
begin
  Result := fDebuggerState = dsRunning;
end;

function TSampleDebugger.CanRun: boolean;
begin
  Result := fDebuggerState <> dsRunning;
end;

function TSampleDebugger.CanStep: boolean;
begin
  Result := fDebuggerState <> dsRunning;
end;

function TSampleDebugger.CanStop: boolean;
begin
  Result := fDebuggerState <> dsStopped;
end;

function TSampleDebugger.CurrentLineIsBreakpoint: boolean;
begin
  Result := (fCurrentLine = fLineToStop)
    or ((fBreakpoints.Count > 0) and IsBreakpointLine(fCurrentLine));
end;

procedure TSampleDebugger.DoOnBreakpointChanged(ALine: integer);
begin
  if Assigned(fOnBreakpointChange) then
    fOnBreakpointChange(Self, ALine);
end;

procedure TSampleDebugger.DoCurrentLineChanged;
begin
  if Assigned(fOnCurrentLineChange) then
    fOnCurrentLineChange(Self);
end;

procedure TSampleDebugger.DoStateChange;
begin
  if fDebuggerState <> fWantedState then begin
    if fWantedState = dsStopped then
      fCurrentLine := -1;
    if Assigned(fOnStateChange) then
      fOnStateChange(Self, fDebuggerState, fWantedState);
    fDebuggerState := fWantedState;
    if fWantedState <> dsRunning then
      fLineToStop := -1;
    DoCurrentLineChanged;
  end;
end;

procedure TSampleDebugger.DoYield;
begin
  if Assigned(fOnYield) then
    fOnYield(Self);
end;

procedure TSampleDebugger.ClearAllBreakpoints;
begin
  if fBreakpoints.Count > 0 then begin
    fBreakpoints.Clear;
    DoOnBreakpointChanged(-1);
  end;
end;

function TSampleDebugger.GetLineInfos(ALine: integer): TDebuggerLineInfos;
begin
  Result := [];
  if ALine > 0 then begin
    if ALine = fCurrentLine then
      Include(Result, dlCurrentLine);
    if IsExecutableLine(ALine) then
      Include(Result, dlExecutableLine);
    if IsBreakpointLine(ALine) then
      Include(Result, dlBreakpointLine);
  end;
end;

procedure TSampleDebugger.GotoCursor(ALine: integer);
begin
  fLineToStop := ALine;
  Run;
end;

function TSampleDebugger.HasBreakpoints: boolean;
begin
  Result := fBreakpoints.Count > 0;
end;

function TSampleDebugger.IsBreakpointLine(ALine: integer): boolean;
var
  i: integer;
begin
  Result := FALSE;
  if ALine > 0 then begin
    i := fBreakpoints.Count - 1;
    while i >= 0 do begin
      if integer(fBreakpoints[i]) = ALine then begin
        Result := TRUE;
        break;
      end;
      Dec(i);
    end;
  end;
end;

function TSampleDebugger.IsExecutableLine(ALine: integer): boolean;
var
  i: integer;
begin
  Result := FALSE;
  if ALine > 0 then begin
    i := High(ExecutableLines);
    while i >= Low(ExecutableLines) do begin
      if ALine = ExecutableLines[i] then begin
        Result := TRUE;
        break;
      end;
      Dec(i);
    end;
  end;
end;

function TSampleDebugger.IsRunning: boolean;
begin
  Result := fDebuggerState = dsRunning;
end;

procedure TSampleDebugger.Pause;
begin
  if fDebuggerState = dsRunning then
    fWantedState := dsPaused;
end;

procedure TSampleDebugger.Run;
var
  dwTime: DWORD;
begin
  fWantedState := dsRunning;
  DoStateChange;
  dwTime := GetTickCount + 100;
  repeat
    if GetTickCount >= dwTime then begin
      DoYield;
      dwTime := GetTickCount + 100;
    end;
    Step;
    if fWantedState <> fDebuggerState then
      DoStateChange;
  until fDebuggerState <> dsRunning;
  fLineToStop := -1;
end;

procedure TSampleDebugger.Step;
begin
  if fDebuggerState = dsStopped then begin
    fNextInstruction := Low(SampleCode);
    fCurrentLine := SampleCode[fNextInstruction].Line;
    fWantedState := dsPaused;
    DoStateChange;
  end else begin
    Sleep(50);
    fNextInstruction := fNextInstruction + SampleCode[fNextInstruction].Delta;
    fCurrentLine := SampleCode[fNextInstruction].Line;
    case fDebuggerState of
      dsRunning:
        begin
          if CurrentLineIsBreakpoint then
            fWantedState := dsPaused;
        end;
    else
      DoCurrentLineChanged;
    end;
  end;
end;

procedure TSampleDebugger.Stop;
begin
  fWantedState := dsStopped;
  DoStateChange;
end;

procedure TSampleDebugger.ToggleBreakpoint(ALine: integer);
var
  SetBP: boolean;
  i: integer;
begin
  if ALine > 0 then begin
    SetBP := TRUE;
    for i := 0 to fBreakpoints.Count - 1 do begin
      if integer(fBreakpoints[i]) = ALine then begin
        fBreakpoints.Delete(i);
        SetBP := FALSE;
        break;
      end else if integer(fBreakpoints[i]) > ALine then begin
        fBreakpoints.Insert(i, pointer(ALine));
        SetBP := FALSE;
        break;
      end;
    end;
    if SetBP then
      fBreakpoints.Add(pointer(ALine));
    DoOnBreakpointChanged(ALine);
  end;
end;

end.

