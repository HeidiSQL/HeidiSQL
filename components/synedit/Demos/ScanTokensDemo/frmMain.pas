{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-07-06.

The Original Code is part of the ScanTokensDemo project, written by
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

$Id: frmMain.pas,v 1.2 2000/11/22 08:37:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit frmMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynEditHighlighter, SynHighlighterPas, StdCtrls, ExtCtrls, SynEdit,
  ComCtrls;

type
  TScanThreadForm = class(TForm)
    SynEdit1: TSynEdit;
    Splitter1: TSplitter;
    SynPasSyn1: TSynPasSyn;
    StatusBar1: TStatusBar;
    SynEdit2: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
  private
    fWorkerThread: TThread;
  end;

var
  ScanThreadForm: TScanThreadForm;

implementation

{$R *.DFM}

{ TScanKeywordThread }

type
  TScanKeywordThread = class(TThread)
  private
    fHighlighter: TSynCustomHighlighter;
    fKeywords: TStringList;
    fLastPercent: integer;
    fScanEventHandle: THandle;
    fSource: string;
    fSourceChanged: boolean;
    procedure GetSource;
    procedure SetResults;
    procedure ShowProgress;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetModified;
    procedure Shutdown;
  end;

constructor TScanKeywordThread.Create;
begin
  inherited Create(TRUE);
  fHighlighter := TSynPasSyn.Create(nil);
  fKeywords := TStringList.Create;
  fScanEventHandle := CreateEvent(nil, FALSE, FALSE, nil);
  if (fScanEventHandle = 0) or (fScanEventHandle = INVALID_HANDLE_VALUE) then
    raise EOutOfResources.Create('Couldn''t create WIN32 event object');
  Resume;
end;

destructor TScanKeywordThread.Destroy;
begin
  fHighlighter.Free;
  fKeywords.Free;
  if (fScanEventHandle <> 0) and (fScanEventHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(fScanEventHandle);
  inherited Destroy;
end;

procedure TScanKeywordThread.Execute;
var
  i: integer;
  s: string;
  Percent: integer;
begin
  while not Terminated do begin
    WaitForSingleObject(fScanEventHandle, INFINITE);
    repeat
      if Terminated then
        break;
      // make sure the event is reset when we are still in the repeat loop
      ResetEvent(fScanEventHandle);
      // get the modified source and set fSourceChanged to 0
      Synchronize(GetSource);
      if Terminated then
        break;
      // clear keyword list
      fKeywords.Clear;
      fLastPercent := 0;
      // scan the source text for the keywords, cancel if the source in the
      // editor has been changed again
      fHighlighter.SetLine(fSource, 1);
      while not fSourceChanged and not fHighlighter.GetEol do begin
        if fHighlighter.GetTokenKind = Ord(SynHighlighterPas.tkKey) then begin
          s := fHighlighter.GetToken;
          with fKeywords do begin
            i := IndexOf(s);
            if i = -1 then
              AddObject(s, pointer(1))
            else
              Objects[i] := pointer(integer(Objects[i]) + 1);
          end;
        end;
        // show progress (and burn some cycles ;-)
        Percent := MulDiv(100, fHighlighter.GetTokenPos, Length(fSource));
        if fLastPercent <> Percent then begin
          fLastPercent := Percent;
          Sleep(10);
          Synchronize(ShowProgress);
        end;
        fHighlighter.Next;
      end;
    until not fSourceChanged;

    if Terminated then
      break;
    // source was changed while scanning
    if fSourceChanged then begin
      Sleep(100);
      continue;
    end;

    fLastPercent := 100;
    Synchronize(ShowProgress);

    fKeywords.Sort;
    for i := 0 to fKeywords.Count - 1 do begin
      fKeywords[i] := fKeywords[i] + ': ' +
        IntToStr(integer(fKeywords.Objects[i]));
    end;
    Synchronize(SetResults);
    // and go to sleep again
  end;
end;

procedure TScanKeywordThread.GetSource;
begin
  if ScanThreadForm <> nil then
    fSource := ScanThreadForm.SynEdit1.Text
  else
    fSource := '';
  fSourceChanged := FALSE;
end;

procedure TScanKeywordThread.SetModified;
begin
  fSourceChanged := TRUE;
  if (fScanEventHandle <> 0) and (fScanEventHandle <> INVALID_HANDLE_VALUE) then
    SetEvent(fScanEventHandle);
end;

procedure TScanKeywordThread.SetResults;
begin
  if ScanThreadForm <> nil then
    ScanThreadForm.SynEdit2.Lines.Assign(fKeywords);
end;

procedure TScanKeywordThread.ShowProgress;
begin
  if ScanThreadForm <> nil then
    ScanThreadForm.StatusBar1.SimpleText := Format('%d %% done', [fLastPercent]);
end;

procedure TScanKeywordThread.Shutdown;
begin
  Terminate;
  if (fScanEventHandle <> 0) and (fScanEventHandle <> INVALID_HANDLE_VALUE) then
    SetEvent(fScanEventHandle);
end;

{ TScanThreadForm }

procedure TScanThreadForm.FormCreate(Sender: TObject);
begin
  fWorkerThread := TScanKeywordThread.Create;
  if FileExists('Windows.pas') then
    SynEdit1.Lines.LoadFromFile('Windows.pas');
  TScanKeywordThread(fWorkerThread).SetModified;
end;

procedure TScanThreadForm.FormDestroy(Sender: TObject);
begin
  ScanThreadForm := nil;
  if fWorkerThread <> nil then
    TScanKeywordThread(fWorkerThread).Shutdown;
end;

procedure TScanThreadForm.SynEdit1Change(Sender: TObject);
begin
  SynEdit2.ClearAll;
  if fWorkerThread <> nil then
    TScanKeywordThread(fWorkerThread).SetModified;
end;

end.

  