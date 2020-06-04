{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The original code is: C:\radw3\dpr\bin\SynHighlighterZPL.pas, released 2020-05-03.
Description: Syntax Parser/Highlighter
The initial author of this file is chtilux.
Copyright (c) 2020, all rights reserved.

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

$Id: $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterZPL;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkNull, tkKey, tkText, tkComment, tkNumber);

  TSynZPLSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FKeyAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FFieldDataAttri: TSynHighlighterAttributes;
    pvFieldData: Boolean;
    pvGraphic: Boolean;
    FCommentScript: boolean;

    procedure NullProc;
    procedure KeyProc;
    procedure TextProc;
    procedure SlashProc;
    procedure NumberProc;
    procedure SetCommentScript(const Value: boolean);
  protected
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetSampleSource: UnicodeString; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetEol: Boolean; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
  published
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property TextAttri: TSynHighlighterAttributes read FTextAttri write FTextAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property FieldDataAttri: TSynHighlighterAttributes read FFieldDataAttri write FFieldDataAttri;
    (* // is not a ZPL comment, but in my needs yes (for scripting), so set
           commentScript to False to disable // comments                      *)
    property CommentScript: boolean read FCommentScript write SetCommentScript default True;
  end;

implementation

uses
  SynEditStrConst;

resourcestring
  SYNS_FilterZPL = '*.zpl;*.zplx';
  SYNS_LangZPL = 'ZPL';
  SYNS_FriendlyLangZPL = 'ZPL';

{ TMyHighlighter }

constructor TSynZPLSyn.Create(AOwner: TComponent);
begin
  inherited;
  pvFieldData := False;
  pvGraphic := False;
  FCommentScript := True;

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FKeyAttri.Foreground := clMaroon;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(FTextAttri);

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clNavy;
  AddAttribute(FNumberAttri);

  FFieldDataAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  FFieldDataAttri.Foreground := clNavy;
  FFieldDataAttri.Style := [fsBold];
  AddAttribute(FFieldDataAttri);

  SetAttributesOnChange(DefHighlightChange);

  FDefaultFilter := SYNS_FilterZPL;
end;

function TSynZPLSyn.GetDefaultAttribute(
  Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_COMMENT: Result := FCommentAttri;
  else
    Result := nil;
  end;
end;

function TSynZPLSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynZPLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangZPL;
end;

class function TSynZPLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangZPL;
end;

function TSynZPLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkKey: Result := FKeyAttri;
    tkText: begin
      if not pvFieldData then
        Result := FTextAttri
      else
        Result := FFieldDataAttri;
    end;
    tkComment: Result := FCommentAttri;
    tkNumber: begin
      if not pvGraphic then
        Result := FNumberAttri
      else
        Result := FTextAttri;
    end;
  else
    Result := nil;
  end;
end;

function TSynZPLSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynZPLSyn.KeyProc;
var
  lcComment: boolean;
begin
  pvFieldData := (Run < Length(FLineStr)-1-2) and (FLine[Run] = '^') and (UpCase(FLine[Run+1]) = 'F') and (UpCase(FLine[Run+2]) = 'D');
  pvGraphic := (Run < Length(FLineStr)-1-2) and (FLine[Run] = '~') and (UpCase(FLine[Run+1]) = 'D') and (UpCase(FLine[Run+2]) = 'G');
  lcComment := (Run < Length(FLineStr)-1-2) and (FLine[Run] = '^') and (UpCase(FLine[Run+1]) = 'F') and (UpCase(FLine[Run+2]) = 'X');
  if not lcComment then
  begin
  FTokenID := tkKey;
  if Run < Length(FLineStr)-2 then
    Inc(Run,3)
  else
    Inc(Run);
  end
  else
  begin
    FTokenID := tkComment;
    Inc(Run);
    while FLine[Run] <> #0 do
      case FLine[Run] of
        #10, #13:
          Break;
        else
          Inc(Run);
      end;
  end;
end;

procedure TSynZPLSyn.Next;
begin
  case FLine[Run] of
     #0: NullProc;
    '^': KeyProc;
    '~': KeyProc;
    '/': SlashProc;
    '0'..'9': NumberProc;
  else
    TextProc;
  end;
  inherited;
end;

procedure TSynZPLSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynZPLSyn.NumberProc;
  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;
begin
  Inc(Run);
  FTokenID := tkNumber;
  while IsNumberChar do Inc(Run);
end;

procedure TSynZPLSyn.SetCommentScript(const Value: boolean);
begin
  FCommentScript := Value;
end;

procedure TSynZPLSyn.SlashProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if (Run > 0) or not(FCommentScript) then
  begin
    FTokenID := tkText;
    Inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a comment
  FTokenID := tkComment;
  Inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10, #13:
        Break;
      else
        Inc(Run);
    end;
end;

procedure TSynZPLSyn.TextProc;

  function IsTextChar: Boolean;
  begin
    case fLine[Run] of
      'a'..'z', 'A'..'Z':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  FTokenID := tkText;
  Inc(Run);
  while FLine[Run] <> #0 do
    if IsTextChar then
      Inc(Run)
    else
      Break;
end;

function TSynZPLSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '^XA'#13#10+
    '^LH30,6161'#13#10+
    '^FO20,10'#13#10+
    '^ADN,90,50'#13#10+
    '^FDHello^FS'#13#10+
    '^XZ';
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynZPLSyn);
{$ENDIF}
end.
