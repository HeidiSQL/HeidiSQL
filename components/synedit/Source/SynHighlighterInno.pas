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

unit SynHighlighterInno;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkConstant, tkIdentifier, tkKey, tkKeyOrParameter,
    tkNull, tkNumber, tkParameter, tkSection, tkSpace, tkString, tkSymbol,
    tkUnknown);

  TSynInnoSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FConstantAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FSectionAttri: TSynHighlighterAttributes;
    FParamAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FInvalidAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FKeywords: TSynHashEntryList;
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
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
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
    function GetTokenKind: Integer; override;
    procedure Next; override;
  published
    property ConstantAttri: TSynHighlighterAttributes read FConstantAttri
      write FConstantAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read FInvalidAttri
      write FInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property ParameterAttri: TSynHighlighterAttributes read FParamAttri
      write FParamAttri;
    property SectionAttri: TSynHighlighterAttributes read FSectionAttri
      write FSectionAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

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
    Inc(Str);
  end;
  Result := Result and $1FF; // 511
  FStringLen := Str - FToIdent;
end;

function TSynInnoSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      Break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        Exit;
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
  Temp := FToIdent;
  if Length(Token) = FStringLen then
  begin
    Result := True;
    for i := 1 to FStringLen do
    begin
      if SynWideLowerCase(Temp^)[1] <> SynWideLowerCase(Token[i])[1] then
      begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end
  else
    Result := False;
end;

constructor TSynInnoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := False;

  FKeywords := TSynHashEntryList.Create;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGray;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FInvalidAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clNavy;
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FConstantAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  FConstantAttri.Style := [fsBold, fsItalic];
  FConstantAttri.Foreground := clTeal;
  AddAttribute(FConstantAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  //Parameters
  FParamAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FParamAttri.Style := [fsBold];
  FParamAttri.Foreground := clOlive;
  AddAttribute(FParamAttri);

  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  FSectionAttri.Style := [fsBold];
  FSectionAttri.Foreground := clRed;
  AddAttribute(FSectionAttri);

  SetAttributesOnChange(DefHighlightChange);
  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkParameter), Parameters, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkKeyOrParameter), KeyOrParameter, IsIdentChar,
    DoAddKeyword);
  FDefaultFilter := SYNS_FilterInno;
end;

destructor TSynInnoSyn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynInnoSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynInnoSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynInnoSyn.EqualProc;
begin
// If any word has equal (=) symbol,
// then the immediately followed text is treated as string
// (though it does not have quotes)
  FTokenID := tkString;
  repeat
    Inc(Run);
    if FLine[Run] = ';' then
    begin
      Inc(Run);
      Break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.IdentProc;
var
  LookAhead: Integer;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  if FTokenID = tkKeyOrParameter then
  begin
    LookAhead := Run;
    while CharInSet(FLine[LookAhead], [#9, ' ']) do
      Inc(LookAhead);
    if FLine[LookAhead] = ':' then
      FTokenID := tkKey
    else
      FTokenID := tkParameter;
  end;
end;

procedure TSynInnoSyn.SectionProc;
begin
  // if it is not column 0 mark as tkParameter and get out of here
  if Run > 0 then
  begin
    FTokenID := tkUnknown;
    Inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a Section
  FTokenID := tkSection;
  repeat
    Inc(Run);
    if FLine[Run] = ']' then
    begin
      Inc(Run);
      Break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynInnoSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynInnoSyn.NumberProc;
begin
  FTokenID := tkNumber;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], ['0'..'9']);
end;

procedure TSynInnoSyn.ConstantProc;
var
  BraceLevel, LastOpenBrace: Integer;
begin
  { Much of this is based on code from the SkipPastConst function in IS's
    CmnFunc2 unit. [jr] }
  if FLine[Run + 1] = '{' then
  begin
    { '{{' is not a constant }
    FTokenID := tkUnknown;
    Inc(Run, 2);
    Exit;
  end;
  FTokenID := tkConstant;
  BraceLevel := 1;
  LastOpenBrace := Low(Integer);
  repeat
    Inc(Run);
    case FLine[Run] of
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
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynInnoSyn.SemiColonProc;
var
  I: Integer;
begin
  for I := Run-1 downto 0 do
    if FLine[I] > ' ' then begin
      // If the semicolon is not the first non-whitespace character on the
      // line, then it isn't the start of a comment.
      FTokenID := tkUnknown;
      Inc(Run);
      Exit;
    end;
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    Inc(Run);
    if FLine[Run] = '"' then begin
      Inc(Run);
      if FLine[Run] <> '"' then // embedded "" does not end the string
        Break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynInnoSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
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

function TSynInnoSyn.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynInnoSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkParameter: Result := FParamAttri;
    tkSection: Result := FSectionAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkConstant: Result := FConstantAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynInnoSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynInnoSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterInno;
end;

class function TSynInnoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangInno;
end;

procedure TSynInnoSyn.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
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
