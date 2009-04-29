{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterInno.pas, released 2000-05-01.
The Initial Author of this file is Satya.
Portions created by Satya are Copyright 2000 Satya.
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

$Id: SynHighlighterInno.pas,v 1.22.2.9 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an Inno script file highlighter for SynEdit)
@author(Satya)
@created(2000-05-01)
@lastmod(2001-01-23)
The SynHighlighterInno unit provides an Inno script file highlighter for SynEdit.
Check out http://www.jrsoftware.org for the free Inno Setup program,
and http://www.wintax.nl/isx/ for My Inno Setup Extensions.
}

{$IFNDEF QSYNHIGHLIGHTERINNO}
unit SynHighlighterInno;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynHighlighterHashEntries,
  QSynUnicode,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkConstant, tkIdentifier, tkKey, tkKeyOrParameter,
    tkNull, tkNumber, tkParameter, tkSection, tkSpace, tkString, tkSymbol,
    tkUnknown);

  TSynInnoSyn = class(TSynCustomHighlighter)
  private
    fTokenID: TtkTokenKind;
    fConstantAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSectionAttri: TSynHighlighterAttributes;
    fParamAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymbolProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SectionProc;
    procedure SpaceProc;
    procedure EqualProc;
    procedure ConstantProc;
    procedure SemiColonProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
  protected
    function IsCurrentToken(const Token: UnicodeString): Boolean; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property ConstantAttri: TSynHighlighterAttributes read fConstantAttri
      write fConstantAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property ParameterAttri: TSynHighlighterAttributes read fParamAttri
      write fParamAttri;
    property SectionAttri: TSynHighlighterAttributes read fSectionAttri
      write fSectionAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  {Note: new 'Section names' and the new 'Constants' need not be added
         as they are highlighted automatically}

  {Ref:  Keywords and Parameters are updated as they last appeared in
         Inno Setup / ISX version 1.3.26}

  Keywords: UnicodeString =
    'adminprivilegesrequired,allownoicons,allowrootdirectory,allowuncpath,' +
    'alwayscreateuninstallicon,alwaysrestart,alwaysshowcomponentslist,' +
    'alwaysshowdironreadypage,alwaysshowgrouponreadypage,' +
    'alwaysusepersonalgroup,appcopyright,appid,appmutex,appname,apppublisher,' +
    'apppublisherurl,appsupporturl,appupdatesurl,appvername,appversion,' +
    'attribs,backcolor,backcolor2,backcolordirection,backsolid,bits,' +
    'changesassociations,check,codefile,comment,components,compression,compresslevel,copymode,'+
    'createappdir,createuninstallregkey,defaultdirname,defaultgroupname,' +
    'description,destdir,destname,direxistswarning,disableappenddir,' +
    'disabledirexistswarning,disabledirpage,disablefinishedpage,' +
    'disableprogramgrouppage,disablereadymemo,disablereadypage,' +
    'disablestartupprompt,diskclustersize,disksize,diskspacemblabel,' +
    'diskspanning,dontmergeduplicatefiles,enabledirdoesntexistwarning,' +
    'extradiskspacerequired,filename,flags,flatcomponentslist,fontinstall,' +
    'groupdescription,hotkey,iconfilename,iconindex,infoafterfile,infobeforefile,' +
    'installmode,internalcompresslevel,key,licensefile,messagesfile,minversion,name,' +
    'onlybelowversion,outputbasefilename,outputdir,overwriteuninstregentries,' +
    'parameters,password,reservebytes,root,runonceid,section,' +
    'showcomponentsizes,source,sourcedir,statusmsg,subkey,tasks,type,types,' +
    'uninstalldisplayicon,uninstalldisplayname,uninstallfilesdir,' +
    'uninstalliconname,uninstalllogmode,uninstallstyle,uninstallable,' +
    'updateuninstalllogappname,usepreviousappdir,usepreviousgroup,' +
    'useprevioustasks,useprevioussetuptype,usesetupldr,valuedata,valuename,' +
    'valuetype,windowresizable,windowshowcaption,windowstartmaximized,' +
    'windowvisible,wizardimagebackcolor,wizardimagefile,wizardsmallimagefile,' +
    'wizardstyle,workingdir';

  Parameters: UnicodeString =
    'hkcc,hkcr,hkcu,hklm,hku,alwaysoverwrite,alwaysskipifsameorolder,append,' +
    'binary,classic,closeonexit,comparetimestampalso,confirmoverwrite,' +
    'createkeyifdoesntexist,createonlyiffileexists,createvalueifdoesntexist,' +
    'deleteafterinstall,deletekey,deletevalue,dirifempty,dontcloseonexit,' +
    'dontcreatekey,disablenouninstallwarning,dword,exclusive,expandsz,' +
    'external,files,filesandordirs,fixed,fontisnttruetype,iscustom,isreadme,' +
    'modern,multisz,new,noerror,none,normal,nowait,onlyifdestfileexists,' +
    'onlyifdoesntexist,overwrite,overwritereadonly,postinstall,' +
    'preservestringtype,regserver,regtypelib,restart,restartreplace,' +
    'runmaximized,runminimized,sharedfile,shellexec,showcheckbox,' +
    'skipifnotsilent,skipifsilent,silent,skipifdoesntexist,' +
    'skipifsourcedoesntexist,unchecked,uninsalwaysuninstall,' +
    'uninsclearvalue,uninsdeleteentry,uninsdeletekey,uninsdeletekeyifempty,' +
    'uninsdeletesection,uninsdeletesectionifempty,uninsdeletevalue,' +
    'uninsneveruninstall,useapppaths,verysilent,waituntilidle';

  KeyOrParameter: UnicodeString = 'string';

function TSynInnoSyn.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
     case Str^ of
       '_': Result := 1;
       'a'..'z': Result := 2 + Ord(Str^) - Ord('a');
       'A'..'Z': Result := 2 + Ord(Str^) - Ord('A');
       else Result := 0;
     end;
  end;

begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
{$IFOPT Q-}
    Result := 7 * Result + GetOrd;
{$ELSE}
    Result := (7 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    inc(Str);
  end;
  Result := Result and $1FF; // 511
  fStringLen := Str - fToIdent;
end;

function TSynInnoSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

function TSynInnoSyn.IsCurrentToken(const Token: UnicodeString): Boolean;
  var
  I: Integer;
  Temp: PWideChar;
begin
  Temp := fToIdent;
  if Length(Token) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if SynWideLowerCase(Temp^)[1] <> SynWideLowerCase(Token[i])[1] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else
    Result := False;
end;

constructor TSynInnoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fKeywords := TSynHashEntryList.Create;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGray;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(fInvalidAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clMaroon;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fConstantAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  fConstantAttri.Style := [fsBold, fsItalic];
  fConstantAttri.Foreground := clTeal;
  AddAttribute(fConstantAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);

  //Parameters
  fParamAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fParamAttri.Style := [fsBold];
  fParamAttri.Foreground := clOlive;
  AddAttribute(fParamAttri);

  fSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  fSectionAttri.Style := [fsBold];
  fSectionAttri.Foreground := clRed;
  AddAttribute(fSectionAttri);

  SetAttributesOnChange(DefHighlightChange);
  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkParameter), Parameters, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkKeyOrParameter), KeyOrParameter, IsIdentChar,
    DoAddKeyword);
  fDefaultFilter := SYNS_FilterInno;
end;

destructor TSynInnoSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynInnoSyn.SymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynInnoSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynInnoSyn.EqualProc;
begin
// If any word has equal (=) symbol,
// then the immediately followed text is treated as string
// (though it does not have quotes)
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = ';' then
    begin
      Inc(Run);
      break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.IdentProc;
var
  LookAhead: integer;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  if fTokenID = tkKeyOrParameter then
  begin
    LookAhead := Run;
    while CharInSet(fLine[LookAhead], [#9, ' ']) do
      Inc(LookAhead);
    if fLine[LookAhead] = ':' then
      fTokenID := tkKey
    else
      fTokenID := tkParameter;
  end;
end;

procedure TSynInnoSyn.SectionProc;
begin
  // if it is not column 0 mark as tkParameter and get out of here
  if Run > 0 then
  begin
    fTokenID := tkUnknown;
    inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a Section
  fTokenID := tkSection;
  repeat
    Inc(Run);
    if fLine[Run] = ']' then
    begin
      Inc(Run);
      break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynInnoSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynInnoSyn.NumberProc;
begin
  fTokenID := tkNumber;
  repeat
    Inc(Run);
  until not CharInSet(fLine[Run], ['0'..'9']);
end;

procedure TSynInnoSyn.ConstantProc;
var
  BraceLevel, LastOpenBrace: Integer;
begin
  { Much of this is based on code from the SkipPastConst function in IS's
    CmnFunc2 unit. [jr] }
  if fLine[Run + 1] = '{' then
  begin
    { '{{' is not a constant }
    fTokenID := tkUnknown;
    Inc(Run, 2);
    Exit;
  end;
  fTokenID := tkConstant;
  BraceLevel := 1;
  LastOpenBrace := Low(Integer);
  repeat
    Inc(Run);
    case fLine[Run] of
      '{': begin
             if LastOpenBrace <> Run - 1 then
             begin
               Inc(BraceLevel);
               LastOpenBrace := Run;
             end
             else
               { Skip over '{{' when in an embedded constant }
               Dec(BraceLevel);
           end;
      '}': begin
             Dec (BraceLevel);
             if BraceLevel = 0 then
             begin
               Inc(Run);
               Break;
             end;
           end;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynInnoSyn.SemiColonProc;
var
  I: Integer;
begin
  for I := Run-1 downto 0 do
    if fLine[I] > ' ' then begin
      // If the semicolon is not the first non-whitespace character on the
      // line, then it isn't the start of a comment.
      fTokenID := tkUnknown;
      inc(Run);
      Exit;
    end;
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = '"' then begin
      Inc(Run);
      if fLine[Run] <> '"' then // embedded "" does not end the string
        break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynInnoSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #59 {';'}: SemiColonProc;
    #61 {'='}: EqualProc;
    #34: StringProc;
    '#', ':', ',', '(', ')': SymbolProc;
    '{': ConstantProc;
    #91 {'['} : SectionProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynInnoSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynInnoSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkParameter: Result := fParamAttri;
    tkSection: Result := fSectionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkConstant: Result := fConstantAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynInnoSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynInnoSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterInno;
end;

class function TSynInnoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangInno;
end;

procedure TSynInnoSyn.DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

class function TSynInnoSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangInno;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynInnoSyn);
{$ENDIF}
end.
