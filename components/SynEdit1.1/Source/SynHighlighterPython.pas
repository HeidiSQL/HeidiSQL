{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPython.pas, released 2000-06-23.
The Original Code is based on the odPySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Olivier Deckmyn.
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

$Id: SynHighlighterPython.pas,v 1.8 2002/04/09 09:58:51 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(A Python language highlighter for SynEdit)
@author(Olivier Deckmyn, converted to SynEdit by David Muir <dhmn@dmsoftware.co.uk>)
@created(unknown, converted to SynEdit on 2000-06-23)
@lastmod(2000-06-23)
The SynHighlighterPython implements a highlighter for Python for the SynEdit projects.
}
unit SynHighlighterPython;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  {$ELSE}
  Windows, Messages, Controls, Graphics, Registry,
  {$ENDIF}
  SynEditHighlighter, SynEditTypes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown, rsMultilineString, rsMultilineString2);

  TProcTableProc = procedure of object;
  
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynPythonSyn = class(TSynCustomHighLighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..101] of TIdentFuncTableFunc;
    fStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func15: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func101: TtkTokenKind;
    procedure SymbolProc;
    procedure CRProc;
    procedure CommentProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure String2Proc;
    procedure StringEndProc(EndChar:char);
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                         //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    property IdentChars;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
    write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
    write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
    write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
    write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
    write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
    write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True:
        mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynPythonSyn.InitIdent;
var
  I: Integer;
begin
  for I := 0 to 101 do
    case I of
      15: fIdentFuncTable[I] := Func15;
      21: fIdentFuncTable[I] := Func21;
      23: fIdentFuncTable[I] := Func23;
      31: fIdentFuncTable[I] := Func31;
      32: fIdentFuncTable[I] := Func32;
      33: fIdentFuncTable[I] := Func33;
      37: fIdentFuncTable[I] := Func37;
      39: fIdentFuncTable[I] := Func39;
      41: fIdentFuncTable[I] := Func41;
      45: fIdentFuncTable[I] := Func45;
      49: fIdentFuncTable[I] := Func49;
      52: fIdentFuncTable[I] := Func52;
      54: fIdentFuncTable[I] := Func54;
      55: fIdentFuncTable[I] := Func55;
      57: fIdentFuncTable[I] := Func57;
      63: fIdentFuncTable[I] := Func63;
      73: fIdentFuncTable[I] := Func73;
      77: fIdentFuncTable[I] := Func77;
      79: fIdentFuncTable[I] := Func79;
      91: fIdentFuncTable[I] := Func91;
      96: fIdentFuncTable[I] := Func96;
      101: fIdentFuncTable[I] := Func101;
    else
      fIdentFuncTable[I] := AltFunc;
    end;
end;

function TSynPythonSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynPythonSyn.KeyComp(const aKey: string): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then begin
    Result := True;
    for i := 1 to fStringLen do begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else
    Result := False;
end; { KeyComp }

function TSynPythonSyn.Func15: TtkTokenKind;
begin
  if KeyComp('Def') then
    Result := tkKey
  else if KeyComp('If') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func21: TtkTokenKind;
begin
  if KeyComp('Del') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func23: TtkTokenKind;
begin
  if KeyComp('In') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func31: TtkTokenKind;
begin
  if KeyComp('Len') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func32: TtkTokenKind;
begin
  if KeyComp('Elif') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func33: TtkTokenKind;
begin
  if KeyComp('Lambda') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func37: TtkTokenKind;
begin
  if KeyComp('Break') then
    Result := tkKey
  else if KeyComp('Exec') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func39: TtkTokenKind;
begin
  if KeyComp('For') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func41: TtkTokenKind;
begin
  if KeyComp('Else') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func45: TtkTokenKind;
begin
  if KeyComp('Range') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func49: TtkTokenKind;
begin
  if KeyComp('Global') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func52: TtkTokenKind;
begin
  if KeyComp('Raise') then
    Result := tkKey
  else if KeyComp('From') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func54: TtkTokenKind;
begin
  if KeyComp('Class') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func55: TtkTokenKind;
begin
  if KeyComp('Pass') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func57: TtkTokenKind;
begin
  if KeyComp('While') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func63: TtkTokenKind;
begin
  if KeyComp('Try') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func73: TtkTokenKind;
begin
  if KeyComp('Except') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func77: TtkTokenKind;
begin
  if KeyComp('Print') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func79: TtkTokenKind;
begin
  if KeyComp('Finally') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func91: TtkTokenKind;
begin
  if KeyComp('Import') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func96: TtkTokenKind;
begin
  if KeyComp('Return') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.Func101: TtkTokenKind;
begin
  if KeyComp('Continue') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPythonSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPythonSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 102 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynPythonSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '@','&', '}', '{', ':', ',', ']', '[', '*',
      '^', ')', '(', ';', '/', '=', '-', '+':
        fProcTable[I] := SymbolProc;
      #13: fProcTable[I] := CRProc;
      '#': fProcTable[I] := CommentProc;
      '>': fProcTable[I] := GreaterProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      '$': fProcTable[I] := IntegerProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '.': fProcTable[I] := PointProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      #39: fProcTable[I] := StringProc;
      '"': fProcTable[I] := String2Proc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynPythonSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRange := rsUnknown;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clTeal;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterPython;
end; { Create }

procedure TSynPythonSyn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynPythonSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPythonSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else
    inc(Run);
  end;
end;

procedure TSynPythonSyn.CommentProc;
begin
  fTokenID := tkComment;
  inc(Run);
  while not (FLine[Run] in [#13, #10, #0]) do
    inc(Run);
end;

procedure TSynPythonSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    inc(Run);
end;

procedure TSynPythonSyn.IntegerProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    inc(Run);
end;

procedure TSynPythonSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPythonSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynPythonSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynPythonSyn.PointProc;
begin
  case FLine[Run + 1] of
    '.': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    ')': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

procedure TSynPythonSyn.String2Proc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then begin
    inc(Run, 3);

    fRange:=rsMultilineString2;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of
        '"':
          if (fLine[Run + 1] = '"') and (fLine[Run + 2] = '"') then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            EXIT;
          end else
            inc(Run);
        #10: EXIT;
        #13: EXIT;
        else
          inc(Run);
      end;
    end;
  end;

  repeat
    if (FLine[Run] in [#0, #10, #13]) then BREAK;
    inc(Run);
  until FLine[Run] = '"';
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPythonSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then begin
    inc(Run, 3);

    fRange:=rsMultilineString;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of
        #39:
          if (fLine[Run + 1] = #39) and (fLine[Run + 2] = #39) then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            EXIT;
          end else
            inc(Run);
        #10: EXIT;
        #13: EXIT;
        else
          inc(Run);
      end;
    end;
  end;

  repeat
    if (FLine[Run] in [#0, #10, #13]) then BREAK;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPythonSyn.StringEndProc(EndChar:char);
begin
  fTokenID := tkString;

    case FLine[Run] of
    #0:
      begin
        NullProc;
        EXIT;
      end;
    #10:
      begin
        LFProc;
        EXIT;
    end;
    #13:
      begin
        CRProc;
        EXIT;
      end;
  end;

  repeat
    if (FLine[Run]=EndChar) and (FLine[Run+1]=EndChar) and (FLine[Run+2]=EndChar) then begin
      inc(Run,3);
      fRange:=rsUnknown;
      EXIT;
    end;

    inc(Run);
  until (FLine[Run] in [#0, #10, #13]);
end;

procedure TSynPythonSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPythonSyn.Next;
begin
  fTokenPos := Run;

  case fRange of
    rsMultilineString:
      StringEndProc(#39);
    rsMultilineString2:
      StringEndProc('"');
    else
  fProcTable[fLine[Run]];
end;
end;

function TSynPythonSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynPythonSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynPythonSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynPythonSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPythonSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynPythonSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynPythonSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynPythonSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynPythonSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}                                             //mh 2000-07-14
function TSynPythonSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPython;
end;

function TSynPythonSyn.GetSampleSource: string;
begin
  Result :=
    '#!/usr/local/bin/python'#13#10 +
    'import string, sys'#13#10 +
    '# If no arguments were given, print a helpful message'#13#10 +
    'if len(sys.argv)==1:'#13#10 +
    '    print ''Usage: celsius temp1 temp2 ...'''#13#10 +
    '    sys.exit(0)';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            //mh 2000-07-14
  RegisterPlaceableHighlighter(TSynPythonSyn);
{$ENDIF}
end.

