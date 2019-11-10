{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditAutoComplete.pas, released 2000-06-25.

The Initial Author of the Original Code is Michael Hieke.
Portions written by Michael Hieke are Copyright 2000 Michael Hieke.
Portions written by Cyrille de Brebisson (from mwCompletionProposal.pas) are
Copyright 1999 Cyrille de Brebisson.
Unicode translation by Maël Hörz.
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

$Id: SynEditAutoComplete.pas,v 1.10.2.4 2008/09/14 16:24:58 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITAUTOCOMPLETE}
unit SynEditAutoComplete;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  Windows,
  Menus,
  SynEdit,
  SynEditKeyCmds,
  SynUnicode,
  Classes;

type
  TCustomSynAutoComplete = class(TComponent)
  protected
    FAutoCompleteList: TUnicodeStrings;
    FCompletions: TUnicodeStrings;
    FCompletionComments: TUnicodeStrings;
    FCompletionValues: TUnicodeStrings;
    FEditor: TCustomSynEdit;
    FEditors: TList;
    FEOTokenChars: UnicodeString;
    FCaseSensitive: Boolean;
    FParsed: Boolean;
    procedure CompletionListChanged(Sender: TObject);
    procedure DefineProperties(Filer: TFiler); override;    
    function GetCompletions: TUnicodeStrings;
    function GetCompletionComments: TUnicodeStrings;
    function GetCompletionValues: TUnicodeStrings;
    function GetEditorCount: Integer;
    function GetNthEditor(Index: Integer): TCustomSynEdit;
    procedure SetAutoCompleteList(Value: TUnicodeStrings); virtual;
    procedure SetEditor(Value: TCustomSynEdit);
    procedure SynEditCommandHandler(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data, HandlerData: Pointer);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddEditor(AEditor: TCustomSynEdit): Boolean;
    function RemoveEditor(AEditor: TCustomSynEdit): Boolean;

    procedure AddCompletion(const AToken, AValue, AComment: UnicodeString);
    procedure Execute(AEditor: TCustomSynEdit); virtual;
    procedure ExecuteCompletion(const AToken: UnicodeString; AEditor: TCustomSynEdit);
      virtual;
    procedure ParseCompletionList; virtual;
  public
    property AutoCompleteList: TUnicodeStrings read FAutoCompleteList
      write SetAutoCompleteList;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property Completions: TUnicodeStrings read GetCompletions;
    property CompletionComments: TUnicodeStrings read GetCompletionComments;
    property CompletionValues: TUnicodeStrings read GetCompletionValues;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property EditorCount: Integer read GetEditorCount;
    property Editors[Index: Integer]: TCustomSynEdit read GetNthEditor;
    property EndOfTokenChr: UnicodeString read FEOTokenChars write FEOTokenChars;
  end;

  TSynAutoComplete = class(TCustomSynAutoComplete)
  published
    property AutoCompleteList;
    property CaseSensitive;
    property Editor;
    property EndOfTokenChr;
  end;

implementation

uses
  SynEditTypes,
  SysUtils;

{ TCustomSynAutoComplete }

procedure TCustomSynAutoComplete.AddCompletion(const AToken, AValue, AComment: UnicodeString);
begin
  if AToken <> '' then
  begin
    if (FAutoCompleteList.Count = 0) and (FCompletions.Count = 0) then
      FParsed := True;
    FCompletions.Add(AToken);
    FCompletionComments.Add(AComment);
    FCompletionValues.Add(AValue);
  end;
end;

function TCustomSynAutoComplete.AddEditor(AEditor: TCustomSynEdit): Boolean;
var
  i: Integer;
begin
  if AEditor <> nil then
  begin
    i := FEditors.IndexOf(AEditor);
    if i = -1 then
    begin
      AEditor.FreeNotification(Self);
      FEditors.Add(AEditor);
      AEditor.RegisterCommandHandler(SynEditCommandHandler, nil);
    end;
    Result := True;
  end
  else
    Result := False;
end;

procedure TCustomSynAutoComplete.CompletionListChanged(Sender: TObject);
begin
  FParsed := False;
end;

constructor TCustomSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoCompleteList := TUnicodeStringList.Create;
  TUnicodeStringList(FAutoCompleteList).OnChange := CompletionListChanged;
  FCompletions := TUnicodeStringList.Create;
  FCompletionComments := TUnicodeStringList.Create;
  FCompletionValues := TUnicodeStringList.Create;
  FEditors := TList.Create;
  FEOTokenChars := '()[]{}.';
end;

destructor TCustomSynAutoComplete.Destroy;
begin
  Editor := nil;
  while EditorCount > 0 do
    RemoveEditor(TCustomSynEdit(FEditors.Last));

  inherited Destroy;
  FEditors.Free;
  FCompletions.Free;
  FCompletionComments.Free;
  FCompletionValues.Free;
  FAutoCompleteList.Free;
end;

procedure TCustomSynAutoComplete.DefineProperties(Filer: TFiler);
begin
  inherited;
{$IFNDEF UNICODE}
  UnicodeDefineProperties(Filer, Self);
{$ENDIF}
end;

procedure TCustomSynAutoComplete.Execute(AEditor: TCustomSynEdit);
var
  s: UnicodeString;
  i, j: Integer;
begin
  if AEditor <> nil then
  begin
    // get token
    s := AEditor.LineText;
    j := AEditor.CaretX;
    i := j - 1;
    if i <= Length(s) then
    begin
      while (i > 0) and (s[i] > ' ') and (Pos(s[i], FEOTokenChars) = 0) do
        Dec(i);
      Inc(i);
      s := Copy(s, i, j - i);
      ExecuteCompletion(s, AEditor);
    end;
  end;
end;

procedure TCustomSynAutoComplete.ExecuteCompletion(const AToken: UnicodeString;
  AEditor: TCustomSynEdit);
var
  i, j, Len, IndentLen: Integer;
  s: UnicodeString;
  IdxMaybe, NumMaybe: Integer;
  p: TBufferCoord;
  NewCaretPos: Boolean;
  Temp: TUnicodeStringList;
begin
  if not FParsed then
    ParseCompletionList;
  Len := Length(AToken);
  if (Len > 0) and (AEditor <> nil) and not AEditor.ReadOnly
    and (FCompletions.Count > 0) then
  begin
    // find completion for this token - not all chars necessary if unambiguous
    i := FCompletions.Count - 1;
    IdxMaybe := -1;
    NumMaybe := 0;
    if FCaseSensitive then
    begin
      while i > -1 do
      begin
        s := FCompletions[i];
        if WideCompareStr(s, AToken) = 0 then
          Break
        else if WideCompareStr(Copy(s, 1, Len), AToken) = 0 then
        begin
          Inc(NumMaybe);
          IdxMaybe := i;
        end;
        Dec(i);
      end;
    end
    else
    begin
      while i > -1 do
      begin
        s := FCompletions[i];
        if WideCompareText(s, AToken) = 0 then
          Break
        else if WideCompareText(Copy(s, 1, Len), AToken) = 0 then
        begin
          Inc(NumMaybe);
          IdxMaybe := i;
        end;
        Dec(i);
      end;
    end;
    if (i = -1) and (NumMaybe = 1) then
      i := IdxMaybe;
    if i > -1 then
    begin
      // select token in editor
      p := AEditor.CaretXY;
      AEditor.BeginUpdate;
      try
        AEditor.BlockBegin := BufferCoord(p.Char - Len, p.Line);
        AEditor.BlockEnd := p;
        // indent the completion string if necessary, determine the caret pos
        IndentLen := p.Char - Len - 1;
        p := AEditor.BlockBegin;
        NewCaretPos := False;
        Temp := TUnicodeStringList.Create;
        try
          Temp.Text := FCompletionValues[i];
          // indent lines
          if (IndentLen > 0) and (Temp.Count > 1) then
          begin
            s := UnicodeStringOfChar(' ', IndentLen);
            for i := 1 to Temp.Count - 1 do
              Temp[i] := s + Temp[i];
          end;
          // find first '|' and use it as caret position
          for i := 0 to Temp.Count - 1 do
          begin
            s := Temp[i];
            j := Pos('|', s);
            if j > 0 then
            begin
              Delete(s, j, 1);
              Temp[i] := s;
//              if j > 1 then
//                Dec(j);
              NewCaretPos := True;
              Inc(p.Line, i);
              if i = 0 then
//                Inc(p.x, j)
                Inc(p.Char, j - 1)
              else
                p.Char := j;
              Break;
            end;
          end;
          s := Temp.Text;
          // strip the trailing #13#10 that was appended by the stringlist
          i := Length(s);
          if (i >= 2) and (s[i - 1] = #13) and (s[i] = #10) then
            SetLength(s, i - 2);
        finally
          Temp.Free;
        end;
        // replace the selected text and position the caret
        AEditor.SelText := s;
        if NewCaretPos then
          AEditor.CaretXY := p;
      finally
        AEditor.EndUpdate;                                                
      end;
    end;
  end;
end;

function TCustomSynAutoComplete.GetCompletions: TUnicodeStrings;
begin
  if not FParsed then
    ParseCompletionList;
  Result := FCompletions;
end;

function TCustomSynAutoComplete.GetCompletionComments: TUnicodeStrings;
begin
  if not FParsed then
    ParseCompletionList;
  Result := FCompletionComments;
end;

function TCustomSynAutoComplete.GetCompletionValues: TUnicodeStrings;
begin
  if not FParsed then
    ParseCompletionList;
  Result := FCompletionValues;
end;

function TCustomSynAutoComplete.GetEditorCount: Integer;
begin
  Result := FEditors.Count;
end;

function TCustomSynAutoComplete.GetNthEditor(Index: Integer): TCustomSynEdit;
begin
  if (Index >= 0) and (Index < FEditors.Count) then
    Result := FEditors[Index]
  else
    Result := nil;
end;

procedure TCustomSynAutoComplete.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Editor then
      Editor := nil
    else if AComponent is TCustomSynEdit then
      RemoveEditor(TCustomSynEdit(AComponent));
  end;
end;

procedure TCustomSynAutoComplete.ParseCompletionList;
var
  BorlandDCI: Boolean;
  i, j, Len: Integer;
  s, sCompl, sComment, sComplValue: UnicodeString;

  procedure SaveEntry;
  begin
    FCompletions.Add(sCompl);
    sCompl := '';
    FCompletionComments.Add(sComment);
    sComment := '';
    FCompletionValues.Add(sComplValue);
    sComplValue := '';
  end;

begin
  FCompletions.Clear;
  FCompletionComments.Clear;
  FCompletionValues.Clear;

  if FAutoCompleteList.Count > 0 then
  begin
    s := FAutoCompleteList[0];
    BorlandDCI := (s <> '') and (s[1] = '[');

    sCompl := '';
    sComment := '';
    sComplValue := '';
    for i := 0 to FAutoCompleteList.Count - 1 do
    begin
      s := FAutoCompleteList[i];
      Len := Length(s);
      if BorlandDCI then
      begin
        // the style of the Delphi32.dci file
        if (Len > 0) and (s[1] = '[') then
        begin
          // save last entry
          if sCompl <> '' then
            SaveEntry;
          // new completion entry
          j := 2;
          while (j <= Len) and (s[j] > ' ') do
            Inc(j);
          sCompl := Copy(s, 2, j - 2);
          // start of comment in DCI file
          while (j <= Len) and (s[j] <= ' ') do
            Inc(j);
          if (j <= Len) and (s[j] = '|') then
            Inc(j);
          while (j <= Len) and (s[j] <= ' ') do
            Inc(j);
          sComment := Copy(s, j, Len);
          if sComment[Length(sComment)] = ']' then
            SetLength(sComment, Length(sComment) - 1);
        end
        else
        begin
          if sComplValue <> '' then
            sComplValue := sComplValue + #13#10;
          sComplValue := sComplValue + s;
        end;
      end
      else
      begin
        // the original style
        if (Len > 0) and (s[1] <> '=') then
        begin
          // save last entry
          if sCompl <> '' then
            SaveEntry;
          // new completion entry
          sCompl := s;
        end
        else if (Len > 0) and (s[1] = '=') then
        begin
          if sComplValue <> '' then
            sComplValue := sComplValue + #13#10;
          sComplValue := sComplValue + Copy(s, 2, Len);
        end;
      end;
    end;
    if sCompl <> '' then                                                        //mg 2000-11-07
      SaveEntry;
  end;
  FParsed := True;
end;

function TCustomSynAutoComplete.RemoveEditor(AEditor: TCustomSynEdit): Boolean;
var
  i: Integer;
begin
  if AEditor <> nil then
  begin
    i := FEditors.IndexOf(AEditor);
    if (i > -1) then
    begin
      if FEditor = AEditor then
        FEditor := nil;
      FEditors.Delete(i);
      AEditor.UnregisterCommandHandler(SynEditCommandHandler);
      {$IFDEF SYN_COMPILER_5_UP}
      RemoveFreeNotification(AEditor);
      {$ENDIF}
    end;
  end;
  Result := False;
end;

procedure TCustomSynAutoComplete.SetAutoCompleteList(Value: TUnicodeStrings);
begin
  FAutoCompleteList.Assign(Value);
  FParsed := False;
end;

procedure TCustomSynAutoComplete.SetEditor(Value: TCustomSynEdit);
begin
  if Value <> FEditor then
  begin
    if FEditor <> nil then
      RemoveEditor(FEditor);
    FEditor := Value;
    if (Value <> nil) then
      AddEditor(Value);
  end;
end;

procedure TCustomSynAutoComplete.SynEditCommandHandler(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data,
  HandlerData: Pointer);
begin
  if not AfterProcessing and not Handled and (Command = ecAutoCompletion) then
  begin
    Handled := True;
    Execute(Sender as TCustomSynEdit);
  end;
end;

end.
