{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditSearch.pas, released 2000-04-07.

The Original Code is based on the mwEditSearch.pas file from the mwEdit
component suite by Martin Waldenburg and other developers.
Portions created by Martin Waldenburg are Copyright 1999 Martin Waldenburg.
Unicode translation by Maël Hörz.
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

$Id: SynEditSearch.pas,v 1.12.2.6 2009/09/29 00:16:46 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITSEARCH}
unit SynEditSearch;
{$ENDIF}

{$I SynEdit.Inc}

interface

uses
  SynEditTypes,
  SynEditMiscClasses,
  SynUnicode,
  Classes;

type
  TSynEditSearch = class(TSynEditSearchCustom)
  private
    Run: PWideChar;
    FOrigin: PWideChar;
    FTheEnd: PWideChar;
    FPat, FCasedPat: UnicodeString;
    FCount: Integer;
    FTextLen: Integer;
    FLookAt: Integer;
    FPatLen, FPatLenSucc: Integer;
    FShift: array[WideChar] of Integer;
    FCaseSensitive: Boolean;
    FWhole: Boolean;
    FResults: TList;
    FShiftInitialized: Boolean;
    FTextToSearch: UnicodeString;
    function GetFinished: Boolean;
    procedure InitShiftTable;
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    function TestWholeWord: Boolean;
    procedure SetPattern(const Value: UnicodeString); override;
    function GetPattern: UnicodeString; override;
    function GetLength(Index: Integer): Integer; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function FindAll(const NewText: UnicodeString): Integer; override;
    function Replace(const aOccurrence, aReplacement: UnicodeString): UnicodeString; override;
    function FindFirst(const NewText: UnicodeString): Integer;
    procedure FixResults(First, Delta: Integer);
    function Next: Integer;

    property Count: Integer read FCount write FCount;
    property Finished: Boolean read GetFinished;
    property Pattern read FCasedPat;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property Whole: Boolean read FWhole write FWhole;
  end;

implementation

uses
  Windows,
  SysUtils;

constructor TSynEditSearch.Create(aOwner: TComponent);
begin
  inherited;
  FResults := TList.Create;
end;

function TSynEditSearch.GetFinished: Boolean;
begin
  Result := (Run >= FTheEnd) or (FPatLen >= FTextLen);
end;

function TSynEditSearch.GetResult(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < FResults.Count) then
    Result := Integer(FResults[Index]);
end;

function TSynEditSearch.GetResultCount: Integer;
begin
  Result := FResults.Count;
end;

procedure TSynEditSearch.FixResults(First, Delta: Integer);
var
  i: Integer;
begin
  if (Delta <> 0) and (FResults.Count > 0) then begin
    i := Pred(FResults.Count);
    while i >= 0 do begin
      if Integer(FResults[i]) <= First then Break;
      FResults[i] := Pointer(Integer(FResults[i]) - Delta);
      Dec(i);
    end;
  end;
end;

procedure TSynEditSearch.InitShiftTable;
var
  C: WideChar;
  I: Integer;
begin
  FPatLen := Length(FPat);
  if FPatLen = 0 then raise Exception.Create('Pattern is empty');
  FPatLenSucc := FPatLen + 1;
  FLookAt := 1;
  for C := Low(WideChar) to High(WideChar) do FShift[C] := FPatLenSucc;
  for I := 1 to FPatLen do FShift[FPat[I]] := FPatLenSucc - I;
  while FLookAt < FPatLen do
  begin
    if FPat[FPatLen] = FPat[FPatLen - FLookAt] then Break;
    Inc(FLookAt);
  end;
  FShiftInitialized := True;
end;                                

// TODO: would be more intelligent to use IsWordBreakChar for SynEdit
function IsWordBreakChar(C: WideChar): Boolean;
begin
  case C of
    #0..#32, '.', ',', ';', ':', '"', '''', WideChar(#$00B4), WideChar(#$0060),
    WideChar(#$00B0), '^', '!', '?', '&', '$', '@', WideChar(#$00A7), '%', '#',
    '~', '[', ']', '(', ')', '{', '}', '<', '>', '-', '=', '+', '*', '/',
    '\', '|':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynEditSearch.TestWholeWord: Boolean;
var
  Test: PWideChar;
begin
  Test := Run - FPatLen;

  Result := ((Test < FOrigin) or IsWordBreakChar(Test[0])) and
    ((Run >= FTheEnd) or IsWordBreakChar(Run[1]));
end;

function TSynEditSearch.Next: Integer;
var
  I: Integer;
  J: PWideChar;
begin
  Result := 0;
  Inc(Run, FPatLen);
  while Run < FTheEnd do
  begin
    if FPat[FPatLen] <> Run^ then
      Inc(Run, FShift[(Run + 1)^])
    else
    begin
      J := Run - FPatLen + 1;
      I := 1;
      while FPat[I] = J^ do
      begin
        if I = FPatLen then
        begin
          if FWhole and not TestWholeWord then Break;
          Inc(FCount);
          Result := Run - FOrigin - FPatLen + 2;
          Exit;
        end;
        Inc(I);
        Inc(J);
      end;
      Inc(Run, FLookAt);
      if Run >= FTheEnd then
        Break;
      Inc(Run, FShift[Run^] - 1);
    end;
  end;
end;

destructor TSynEditSearch.Destroy;
begin
  FResults.Free;
  inherited Destroy;
end;

procedure TSynEditSearch.SetPattern(const Value: UnicodeString);
begin
  if FPat <> Value then
  begin
    FCasedPat := Value;
    if CaseSensitive then
      FPat := FCasedPat
    else
      FPat := SynWideLowerCase(FCasedPat);
    FShiftInitialized := False;
  end;
  FCount := 0;
end;

procedure TSynEditSearch.SetCaseSensitive(const Value: Boolean);
begin
  if FCaseSensitive <> Value then
  begin
    FCaseSensitive := Value;
    if FCaseSensitive then
      FPat := FCasedPat
    else
      FPat := SynWideLowerCase(FCasedPat);
    FShiftInitialized := False;
  end;
end;

function TSynEditSearch.FindAll(const NewText: UnicodeString): Integer;
var
  Found: Integer;
begin
  // never shrink Capacity
  FResults.Count := 0;
  Found := FindFirst(NewText);
  while Found > 0 do
  begin
    FResults.Add(Pointer(Found));
    Found := Next;
  end;
  Result := FResults.Count;
end;

function TSynEditSearch.Replace(const aOccurrence, aReplacement: UnicodeString): UnicodeString;
begin
  Result := aReplacement;
end;                     

function TSynEditSearch.FindFirst(const NewText: UnicodeString): Integer;
begin
  if not FShiftInitialized then
    InitShiftTable;
  Result := 0;
  FTextLen := Length(NewText);
  if FTextLen >= FPatLen then
  begin
    if CaseSensitive then
      FTextToSearch := NewText
    else
      FTextToSearch := SynWideLowerCase(NewText);
    FOrigin := PWideChar(FTextToSearch);
    FTheEnd := FOrigin + FTextLen;
    Run := (FOrigin - 1);
    Result := Next;
  end;
end;

function TSynEditSearch.GetLength(Index: Integer): Integer;
begin
  Result := FPatLen;
end;

function TSynEditSearch.GetPattern: UnicodeString;
begin
  Result := FCasedPat;
end;

procedure TSynEditSearch.SetOptions(const Value: TSynSearchOptions);
begin
  CaseSensitive := ssoMatchCase in Value;
  Whole := ssoWholeWord in Value;
end;

end.

