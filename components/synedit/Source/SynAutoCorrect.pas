{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynAutoCorrect.pas, released 2001-10-05.
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

$Id: SynAutoCorrect.pas,v 1.6 2002/01/15 19:38:31 harmeister Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynAutoCorrect;

{$I SynEdit.inc}

interface

uses
  Classes,
  SysUtils,
{$IFDEF SYN_CLX}
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  Types,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Registry,
{$ENDIF}
  SynEditMiscProcs,
  SynEditTypes,
  SynEditHighlighter,
  SynEdit,
  IniFiles;

type
  TRecordEditorVars = record
    kd: TKeyEvent;
    md: TMouseEvent;
  end;
  PRecordEditorVars = ^TRecordEditorVars;

  TSynAutoCorrect = class(TComponent)
  private
    { Private declarations }
    FAutoCorrectOnMouseDown: Boolean;
    FBeepOnAutoCorrect: Boolean;
    FEnabled: Boolean;
    FEditor: TCustomSynEdit;
    FEditVars: PRecordEditorVars;
    FIgnoreCase: Boolean;
    FReplaceItems: TStrings;
    FOnReplaceText: TReplaceTextEvent;
    PrevLine: Integer;

    FMaintainCase: Boolean; // GN

    // GN
    function CorrectItemStart(EditLine: String; SearchString: String; StartPos: LongInt; frMatchCase, frWholeWord: Boolean): LongInt;
    Function FindAndCorrect(var EditLine: String; SearchString, ReplaceText: String; var CurrentX: Integer): Boolean;
    function PreviousToken: String;
    function GetReplaceItems: TStrings;
    procedure SetReplaceItems(const Value: TStrings);
  protected
    { Protected declarations }
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetEditor(Value: TCustomSynEdit);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(sReplaceFrom: String; sReplaceTo: String);
    procedure AutoCorrectAll;
    procedure Delete(iItemIndex: Integer);
    procedure Edit(iItemIndex: Integer; sReplaceFrom: String; sReplaceTo: String);

    procedure LoadFromIni(FileName: String; Section: String);
    procedure SaveToIni(FileName: String; Section: String);
    function LoadFromList(FileName: String): Boolean;
    function SaveToList(FileName: String): Boolean;
  {$IFDEF SYN_CLX}
  {$ELSE}
    procedure LoadFromRegistry(RegistryRoot: DWORD; RegistryKey: String);
    procedure SaveToRegistry(RegistryRoot: DWORD; RegistryKey: String);
  {$ENDIF}
  published
    { Published declarations }
    property AutoCorrectOnMouseDown: Boolean read FAutoCorrectOnMouseDown write FAutoCorrectOnMouseDown;
    property BeepOnAutoCorrect: Boolean read FBeepOnAutoCorrect write FBeepOnAutoCorrect;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;
    property ReplaceItems: TStrings read GetReplaceItems write SetReplaceItems;

    // GN
    property MaintainCase: Boolean read FMaintainCase write FMaintainCase default True;
    property OnReplaceText: TReplaceTextEvent read FOnReplaceText write FOnReplaceText;
  end;

const
  DELIMITERS: set of Byte = [0..255] - [0..32, 65..90, 96..105, 46];
  NUMBERS: set of Byte = [48..57];

  function HalfString(Str: String; FirstHalf: Boolean): String;
  function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
  function StringsToStr(const List: TStrings; Sep: AnsiString): AnsiString;
  procedure StrToStrings(S: AnsiString; Sep: AnsiString; const List: TStrings);


implementation


// utilities
function HalfString(Str: String; FirstHalf: Boolean): String;
var
  ResultArray: array[Boolean] of String;
  BreakPoint: Integer;

begin
  BreakPoint := LastDelimiter(#44, Str);

  if BreakPoint = 0 then
    BreakPoint := MaxInt-1;

  ResultArray[True] := Copy(Str, 1, BreakPoint - 1);
  ResultArray[False] := Copy(Str, BreakPoint + 1, MaxInt);
  Result := ResultArray[FirstHalf];
end;

function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, 1, Count);
end;

function StringsToStr(const List: TStrings; Sep: AnsiString): AnsiString;
var
  I, L: Integer;

begin
  Result := '';

  for I := 0 to List.Count - 1 do
  begin
    Result := Result + List[I];
    Result := Result + Sep;
  end;

  if List.Count <> 0 then
  begin
    L := Length(Sep);
    system.Delete(Result, Length(Result) - L + 1, L);
  end;
end;

procedure StrToStrings(S: AnsiString; Sep: AnsiString; const List: TStrings);
var
  I, L: Integer;
  Left: AnsiString;

begin
  Assert(List <> nil);

  List.Clear;
  L := Length(Sep);
  I := Pos(Sep, S);

  while (I > 0) do
  begin
    Left := StrLeft(S, I - 1);
    List.Add(Left);
    System.Delete(S, 1, I + L - 1);
    I := Pos(Sep, S);
  end;

  if S <> '' then
    List.Add(S);
end;

type
  TAsCustomSynEdit = class(TCustomSynEdit);

procedure TSynAutoCorrect.LoadFromIni(FileName: String; Section: String);
var
  Reg: TIniFile;

begin
  Reg := TIniFile.Create(FileName);
  try
    StrToStrings(Reg.ReadString(Section, 'ReplaceItems', ''), ';', FReplaceItems);
  finally
    Reg.Free;
  end;
end;

procedure TSynAutoCorrect.SaveToIni(FileName: String; Section: String);
var
  Reg: TIniFile;
begin
  Reg := TIniFile.Create(FileName);
  try
    Reg.WriteString(Section, 'ReplaceItems', StringsToStr(FReplaceItems, ';'));
  finally
    Reg.Free;
  end;
end;

function TSynAutoCorrect.LoadFromList(FileName: String): Boolean;
begin
  if FileExists(FileName) then
  begin
    FReplaceItems.LoadFromFile(FileName);
    Result := True;
  end
  else
    Result := False;
end;

function TSynAutoCorrect.SaveToList(FileName: String): Boolean;
begin
  if FileExists(FileName) then
  begin
    FReplaceItems.SaveToFile(FileName);
    Result := True;
  end
  else
    Result := False;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynAutoCorrect.LoadFromRegistry(RegistryRoot: DWORD; RegistryKey: String);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := RegistryRoot;
    Reg.OpenKey(RegistryKey, True);
    StrToStrings(Reg.ReadString('', 'ReplaceItems', ''), ';', FReplaceItems);
  finally
    Reg.Free;
  end;
end;

procedure TSynAutoCorrect.SaveToRegistry(RegistryRoot: DWORD; RegistryKey: String);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := RegistryRoot;
    Reg.OpenKey(RegistryKey, True);
    Reg.WriteString('', 'ReplaceItems', StringsToStr(FReplaceItems, ';'));
  finally
    Reg.Free;
  end;
end;
{$ENDIF}

procedure TSynAutoCorrect.SetEditor(Value: TCustomSynEdit);
begin
  FEditor := Value;

  if FEditor <> nil then
  begin
    New(FEditVars);
    FEditVars.kd := Editor.OnKeyDown;
    FEditVars.md := TAsCustomSynEdit(Editor).OnMouseDown;
    Editor.FreeNotification(Self);

    if not (csDesigning in ComponentState) then
    begin
      Editor.OnKeyDown := EditorKeyDown;
      TAsCustomSynEdit(Editor).OnMouseDown := EditorMouseDown;
    end;
  end;
end;

procedure TSynAutoCorrect.Add(sReplaceFrom: String; sReplaceTo: String);
begin
  FReplaceItems.Add(sReplaceFrom + #44 + sReplaceTo);
end;

procedure TSynAutoCorrect.Delete(iItemIndex: Integer);
begin
  FReplaceItems.Delete(iItemIndex);
end;

procedure TSynAutoCorrect.Edit(iItemIndex: Integer; sReplaceFrom: String; sReplaceTo: String);
var
  s: String;

begin
  if (iItemIndex < 0) then Exit;

  s := sReplaceFrom;
  s := s + #44 + sReplaceTo;

  FReplaceItems[iItemIndex] := s;
end;

procedure TSynAutoCorrect.AutoCorrectAll;
var
  i: Integer;
  EditBuf: String;
  CaretX: Integer;
  ReplaceFrom: String;
  ReplaceTo: String;
  CurrentText: String;

begin
  if Editor = nil then Exit;

  EditBuf := Editor.Lines.Text;
  CaretX := -1;

  for i := 0 to FReplaceItems.Count - 1 do
  begin
    CurrentText := FReplaceItems[i];
    ReplaceFrom := HalfString(CurrentText, True);
    ReplaceTo := HalfString(CurrentText, False);
    FindAndCorrect(EditBuf, ReplaceFrom, ReplaceTo, CaretX);
  end;

  Editor.Lines.Text := EditBuf;
end;

procedure TSynAutoCorrect.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  bSuccess: Boolean;
  EditBuf: String;
  CaretX: Integer;
  ReplaceFrom: String;
  ReplaceTo: String;
  CurrentText: String;

begin
//  if Assigned(TRecordEditorVars(FEditVars^).kd) then
//    TRecordEditorVars(FEditVars^).kd(Sender, Key, Shift);

  if Editor = nil then Exit;
  if Enabled = False then Exit;

  PrevLine := Editor.CaretY;

  if ((ssShift in Shift) and (Key <> 16) and (Key in DELIMITERS)) or ((Key in DELIMITERS) and not (Key in NUMBERS)) or (Key = 13) or (Key = 9) or (Key = 32) or (Key = 37) or (Key = 38) or (Key = 39) or (Key = 40) then
  begin
    bSuccess := False;

    EditBuf := PreviousToken;
    if EditBuf <> '' then
    begin
       CaretX := Editor.CaretX;

       for i := 0 to FReplaceItems.Count - 1 do
       begin
          CurrentText := FReplaceItems[i];
          ReplaceFrom := HalfString(CurrentText, True);
          ReplaceTo := HalfString(CurrentText, False);
          bSuccess := bSuccess or FindAndCorrect(EditBuf, ReplaceFrom, ReplaceTo, CaretX);
       end;

       if bSuccess then
       begin
       {$IFDEF SYN_CLX}
       {$ELSE}
         if FBeepOnAutoCorrect then
           MessageBeep(0);
       {$ENDIF}
       end;
    end;
  end;
end;

procedure TSynAutoCorrect.EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  bSuccess: Boolean;
  EditBuf: String;
  CaretX: Integer;
  ReplaceFrom: String;
  ReplaceTo: String;
  CurrentText: String;
  Action: TSynReplaceAction;

begin
//  if Assigned(TRecordEditorVars(FEditVars^).md) then
//    TRecordEditorVars(FEditVars^).md(Sender, Button, Shift, X, Y);

  if FAutoCorrectOnMouseDown = False then Exit;

  if Editor = nil then Exit;
  if Enabled = False then Exit;
  if PrevLine = -1 then Exit;

  bSuccess := False;
  EditBuf := Editor.Lines.Strings[PrevLine - 1];
  CaretX := -1;

  for i := 0 to FReplaceItems.Count - 1 do
  begin
     CurrentText := FReplaceItems[i];
     ReplaceFrom := HalfString(CurrentText,True);
     ReplaceTo := HalfString(CurrentText,False);
     bSuccess := bSuccess or FindAndCorrect(EditBuf, ReplaceFrom, ReplaceTo, CaretX);
  end;

  if bSuccess then
  begin
    if Assigned(fOnReplaceText) then
    begin
      Action := raReplace;
      FOnReplaceText(Self, Editor.Lines.Strings[PrevLine - 1], EditBuf, Editor.CaretY, 0, Action);

      if Action = raCancel then
         Exit;
    end;
    Editor.Lines.Strings[PrevLine - 1] := EditBuf;
  {$IFDEF SYN_CLX}
  {$ELSE}
    if FBeepOnAutoCorrect then
      MessageBeep(0);
  {$ENDIF}
  end;
end;

constructor TSynAutoCorrect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FReplaceItems := TStringList.Create;

  PrevLine := -1;
  FAutoCorrectOnMouseDown := False;
  FBeepOnAutoCorrect := True;
  FEnabled := True;
  FEditor := nil;
  FIgnoreCase := True;

  // GN
  FMaintainCase := True;
end;

destructor TSynAutoCorrect.Destroy;
begin
  FReplaceItems.Free;
  Dispose(FEditVars);
  inherited;
end;

// GN
function TSynAutoCorrect.PreviousToken: String;
var
  i: Integer;
  X: Integer;

begin
  Result := Editor.LineText;
  X := Editor.CaretX;
  i := X - 1;

  if i <= Length(Result) then begin
    while (i > 0) and (Result[i] > ' ') do
      Dec(i);

    Inc(i);
    Result := Copy(Result, i, X - i);
  end
  else
    Result := '';
end;

function TSynAutoCorrect.FindAndCorrect(var EditLine: String; SearchString, ReplaceText: String; var CurrentX: Integer): Boolean;
var
  StartPos: LongInt;
  EndPos: Integer;
  FoundText: String;
  ReplaceDefText: String;
  p: TPoint;
  Action: TSynReplaceAction;

  function FirstCapCase(S: String): String;
  begin
    if S <> '' then
    begin
      s := LowerCase(s);
      s[1] := UpCase(s[1]);
    end;

    Result := S;
  end;

begin
  Result := False;
  ReplaceDefText := ReplaceText;
  StartPos := 0;
  EndPos := Length(SearchString);

  if (Editor <> nil) and not (Editor.ReadOnly) then
  begin
    StartPos := CorrectItemStart(EditLine, SearchString, StartPos, not IgnoreCase, True);

    while StartPos > -1 do
    begin
      if fMaintainCase then
      begin
        ReplaceText := ReplaceDefText;
        FoundText := Copy(EditLine,StartPos+1,EndPos);

        if FoundText = AnsiUpperCase(FoundText) then
          ReplaceText := AnsiUpperCase(ReplaceText)
        else
        begin
          if FoundText = AnsiLowerCase(FoundText) then
            ReplaceText := AnsiLowerCase(ReplaceText)
          else
          begin
            if FoundText = FirstCapCase(FoundText) then
              ReplaceText := FirstCapCase(ReplaceText);
          end;
        end;
      end;

      if CurrentX > - 1 then
      begin
        p := Editor.CaretXY;
        if Assigned(fOnReplaceText) then
        begin
          Action := raReplace;
          FOnReplaceText(Self, SearchString, ReplaceText, P.Y, P.x, Action);

          if Action = raCancel then
            Break;
        end;

        Editor.BeginUpdate;

        try
          if p.x = 0 then
            Editor.BlockBegin := Point(p.x - 1 - EndPos, p.y)
          else
            Editor.BlockBegin := Point(p.x - EndPos, p.y);

          Editor.BlockEnd := p;
          p := Editor.BlockBegin;
          Editor.SelText := ReplaceText;
          Result := True;
        finally
          Editor.EndUpdate;
        end;

        Break;
      end
      else
      begin
        Result := True;
        EditLine := Copy(EditLine, 1, StartPos) + ReplaceText + Copy(EditLine, StartPos + EndPos + 1, MaxInt);
        Inc(StartPos, EndPos);
        StartPos := CorrectItemStart(EditLine, SearchString, StartPos, not IgnoreCase, True);
      end;
    end;
  end;
end;

function TSynAutoCorrect.CorrectItemStart(EditLine: String; SearchString: String; StartPos: LongInt; frMatchCase, frWholeWord: Boolean): LongInt;
var
  SearchCount, I: Integer;
  C: Char;
  CharMap: array [Char] of Char;
  CurBuf, Buf: PChar;
  BufLen: Integer;

const
  WordDelimiters: set of Char = [#0..#32];

  function FindNextWordStart(var BufPtr: PChar): Boolean;
  begin
//    Result := False;

    while (SearchCount > 0) and not (BufPtr^ in WordDelimiters) do
    begin
      Inc(BufPtr, 1);
      Dec(SearchCount);
    end;

    while (SearchCount > 0) and (BufPtr^ in WordDelimiters) do
    begin
      Inc(BufPtr, 1);
      Dec(SearchCount);
    end;

    Result := SearchCount >= 0;  
  end;

  function ScanText(var BufPtr: PChar): Boolean;
  var
     FirstWord: Boolean;
  begin
    Result := False;

    FirstWord := True;

    if frWholeWord then
    begin
       while (SearchCount > 0) and (BufPtr^ in WordDelimiters) do
       begin
         Inc(BufPtr, 1);
         Dec(SearchCount);
       end;
    end;

    while SearchCount >= 0 do
    begin
      if frWholeWord and (FirstWord = False) then
        if not FindNextWordStart(BufPtr) then Break;
      I := 0;
      while (CharMap[BufPtr[I]] = SearchString[I+1]) do
      begin
        Inc(I);
        if I >= Length(SearchString) then
        begin
          if (not frWholeWord) or (SearchCount = 0) or (BufPtr[I] in WordDelimiters) then
          begin
            Result := True;
            Exit;
          end;

          Break;
        end;
      end;
      FirstWord := False;
      Inc(BufPtr);
      Dec(SearchCount);
    end;
  end;

begin
  Result := -1;
  BufLen := Length(EditLine);
  Buf := PChar(EditLine);

  if BufLen > 0 then
  begin
    SearchCount := succ(BufLen - StartPos - Length(SearchString));

    if (SearchCount >= 0) and (SearchCount <= BufLen) and (StartPos + SearchCount <= BufLen) then
    begin
      CurBuf := @Buf[StartPos];
      for C := Low(CharMap) to High(CharMap) do
      {$IFDEF SYN_CLX}
      begin
        if not frMatchCase then
          CharMap[C] := UpCase(C)
        else
          CharMap[C] := C;
      end;
      {$ELSE}
        CharMap[C] := C;
      {$ENDIF}
      if not frMatchCase then
      begin
      {$IFDEF SYN_CLX}
        SearchString := UpperCase(SearchString);
      {$ELSE}
        CharUpperBuff(PChar(@CharMap), sizeof(CharMap));
        CharUpperBuff(@SearchString[1], Length(SearchString));
      {$ENDIF}
      end;
      if not ScanText(CurBuf) then
        CurBuf := nil
      else
      begin
        if CurBuf <> nil then
          Result := CurBuf - Buf;
      end;
    end;
  end;

//  Buf := nil;
  CurBuf := nil;
end;

function TSynAutoCorrect.GetReplaceItems: TStrings;
begin
  Result := FReplaceItems;
end;

procedure TSynAutoCorrect.SetReplaceItems(const Value: TStrings);
begin
  FReplaceItems.Assign(Value);
end;

end.
