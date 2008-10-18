{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCache.pas, released 2000-04-21.
The Original Code is based on the mwCacheSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Pavel Krehula.
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

$Id: SynHighlighterCache.pas,v 1.13.2.6 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Cache object script files highlighter for SynEdit)
@author(Pavel Krehula <pavel@mas.cz>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999-12-17, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterCache unit provides SynEdit with a Cache object script files highlighter.
Thanks to Martin Waldenburg.
}

{$IFNDEF QSYNHIGHLIGHTERCACHE}
unit SynHighlighterCache;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynUnicode,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkClass, tkComment, tkFunction, tkIdentifier, tkKey, tkNull,
    tkNumber, tkDirective, tkSpace, tkString, tkSymbol, tkIndirect, tkLabel,
    tkMacro, tkUserFunction, tkEmbedSQL, tkEmbedText, tkUnknown);

  TRangeState = (rsUnKnown, rsSQL, rsHTML, rsCommand);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynCacheSyn = class(TSynCustomHighlighter)
  private
    fBrace: LongInt;
    fFirstBrace: Boolean;
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..1996] of TIdentFuncTableFunc;
    fClassAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fIndirectAttri: TSynHighlighterAttributes;
    fLabelAttri: TSynHighlighterAttributes;
    fMacroAttri: TSynHighlighterAttributes;
    fUserFunctionAttri: TSynHighlighterAttributes;
    fEmbedSQLAttri: TSynHighlighterAttributes;
    fEmbedTextAttri: TSynHighlighterAttributes;
    FCanKey: boolean;    // if true, the next token can be a keyword
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function Func38html(Index: Integer): TtkTokenKind;
    function Func38sql(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure CRProc;
    procedure CommentProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure IndirectProc;
    procedure SymbolProc;
    procedure FuncProc;
    procedure DirectiveProc;
    procedure EmbeddedProc;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property ClassAttri: TSynHighlighterAttributes read fClassAttri
      write fClassAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PreprocesorAttri: TSynHighlighterAttributes read fDirectiveAttri
      write fDirectiveAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property IndirectAttri: TSynHighlighterAttributes read fIndirectAttri
      write fIndirectAttri;
    property LabelAttri: TSynHighlighterAttributes read fLabelAttri
      write fLabelAttri;
    property MacroAttri: TSynHighlighterAttributes read fMacroAttri
      write fMacroAttri;
    property UserFunctionAttri: TSynHighlighterAttributes
      read fUserFunctionAttri write fUserFunctionAttri;
    property EmbededSQLandHTMLAttri: TSynHighlighterAttributes
      read fEmbedSQLAttri write fEmbedSQLAttri;
    property EmbededTextAttri: TSynHighlighterAttributes read fEmbedTextAttri
      write fEmbedTextAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..274] of UnicodeString = (
    '$a', '$ascii', '$c', '$char', '$d', '$data', '$device', '$e', '$ec', 
    '$ecode', '$es', '$estack', '$et', '$etrap', '$extract', '$f', '$find', 
    '$fn', '$fnumber', '$g', '$get', '$h', '$horolog', '$i', '$in', 
    '$increment', '$inumber', '$io', '$j', '$job', '$justify', '$k', '$key', 
    '$l', '$lb', '$ld', '$length', '$lf', '$lg', '$li', '$list', '$listbuild', 
    '$listdata', '$listfind', '$listget', '$listlength', '$ll', '$n', '$na', 
    '$name', '$next', '$o', '$order', '$p', '$piece', '$principal', '$q', '$ql', 
    '$qlength', '$qs', '$qsubscript', '$query', '$quit', '$r', '$random', '$re', 
    '$reverse', '$s', '$select', '$st', '$stack', '$storage', '$t', '$test', 
    '$text', '$tl', '$tlevel', '$tr', '$translate', '$vi', '$view', '$x', '$y', 
    '$za', '$zabs', '$zarccos', '$zarcsin', '$zarctan', '$zb', '$zbitand', 
    '$zbitcount', '$zbitfind', '$zbitget', '$zbitlen', '$zbitnot', '$zbitor', 
    '$zbitset', '$zbitstr', '$zbitxor', '$zboolean', '$zc', '$zchild', 
    '$zconvert', '$zcos', '$zcot', '$zcrc', '$zcsc', '$zcvt', '$zcyc', '$zdate', 
    '$zdateh', '$zdatetime', '$zdatetimeh', '$ze', '$zeof', '$zerr', '$zerror', 
    '$zexp', '$zf', '$zh', '$zhex', '$zhorolog', '$zi', '$zincr', '$zincrement', 
    '$zio', '$zis', '$ziswide', '$zjob', '$zla', '$zlascii', '$zlc', '$zlchar', 
    '$zln', '$zlog', '$zmode', '$zn', '$zname', '$znext', '$znspace', '$zo', 
    '$zorder', '$zp', '$zparent', '$zpi', '$zpos', '$zposition', '$zpower', 
    '$zprevious', '$zr', '$zreference', '$zs', '$zse', '$zsearch', '$zsec', 
    '$zseek', '$zsin', '$zsort', '$zsqr', '$zstorage', '$zstrip', '$zt', 
    '$ztan', '$zth', '$ztime', '$ztimeh', '$ztimestamp', '$ztrap', '$zts', 
    '$zu', '$zutil', '$zv', '$zversion', '$zw', '$zwa', '$zwascii', '$zwbp', 
    '$zwbpack', '$zwbunp', '$zwbunpack', '$zwc', '$zwchar', '$zwidth', '$zwp', 
    '$zwpack', '$zwunp', '$zwunpack', '$zz', '$zzdec', '$zzenkaku', '$zzhex', 
    '&html', '&sql', '^$g', '^$global', '^$j', '^$job', '^$l', '^$lock', '^$r', 
    '^$routine', 'b', 'break', 'c', 'close', 'd', 'do', 'e', 'else', 'f', 'for', 
    'g', 'goto', 'h', 'halt', 'hang', 'i', 'if', 'j', 'job', 'k', 'kill', 'l', 
    'lock', 'm', 'merge', 'n', 'new', 'o', 'open', 'p', 'print', 'q', 'quit', 
    'r', 'read', 's', 'set', 'tc', 'tcommint', 'tro', 'trollback', 'ts', 
    'tstart', 'u', 'use', 'vi', 'view', 'w', 'write', 'x', 'xecute', 'zb', 
    'zbreak', 'zi', 'zinsert', 'zk', 'zkill', 'zl', 'zload', 'zn', 'znspace', 
    'zp', 'zprint', 'zq', 'zquit', 'zr', 'zremove', 'zs', 'zsave', 'zsync', 
    'ztrap', 'zw', 'zwrite', 'zzdump' 
  );

  KeyIndices: array[0..1996] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 139, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 186, -1, -1, -1, -1, -1, -1, -1, 153, -1, 232, -1, 
    212, 74, -1, -1, -1, -1, -1, 178, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 265, -1, -1, -1, 19, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 77, -1, -1, -1, -1, -1, -1, 272, 
    259, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 234, -1, -1, -1, 
    187, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 198, 246, -1, -1, -1, 
    24, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 76, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 236, -1, 206, 210, -1, -1, 181, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 221, -1, -1, 27, -1, -1, -1, 
    9, -1, -1, -1, -1, -1, 23, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    25, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 116, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 58, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 28, -1, -1, 137, -1, -1, -1, -1, -1, -1, 183, -1, -1, -1, 18, 49, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 244, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 83, -1, -1, 
    -1, -1, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 242, 108, 31, -1, 
    -1, 93, -1, -1, -1, -1, -1, -1, 274, -1, -1, -1, -1, -1, -1, 128, -1, -1, 
    -1, -1, -1, 8, -1, -1, -1, -1, 191, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 17, -1, -1, -1, 88, -1, -1, -1, -1, 66, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 249, -1, 33, -1, -1, 185, 59, 
    -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 193, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 248, -1, -1, 
    -1, 117, -1, -1, 84, -1, -1, -1, -1, -1, 100, -1, 133, -1, -1, 245, -1, -1, 
    -1, 257, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 255, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 250, -1, -1, -1, 152, -1, -1, 
    -1, -1, 239, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 47, -1, -1, 22, 114, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 247, -1, 86, 68, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 252, -1, 80, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6, -1, -1, -1, 
    -1, 113, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 51, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 195, -1, -1, -1, -1, -1, -1, 44, -1, -1, -1, 
    -1, 65, -1, 175, -1, -1, 99, -1, -1, -1, -1, -1, -1, -1, -1, 118, -1, -1, 
    -1, -1, -1, 95, 121, -1, 92, 188, -1, -1, -1, -1, -1, -1, -1, -1, 53, -1, 
    -1, -1, -1, -1, -1, -1, -1, 156, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 164, 240, -1, -1, -1, -1, -1, 202, -1, 130, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 179, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    157, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 168, -1, -1, 56, -1, -1, -1, 
    -1, 1, -1, -1, -1, -1, 223, -1, -1, -1, -1, -1, -1, -1, -1, -1, 225, -1, -1, 
    -1, -1, -1, 197, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    125, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 64, -1, -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 55, -1, -1, 
    -1, -1, -1, 145, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    106, -1, -1, -1, -1, -1, -1, -1, 122, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 67, -1, -1, -1, -1, 85, -1, -1, -1, 261, 
    -1, 182, -1, -1, -1, -1, -1, -1, -1, 41, -1, -1, -1, 158, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 26, 154, -1, -1, -1, -1, 
    -1, 201, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 91, 69, -1, -1, 
    -1, -1, -1, -1, -1, 72, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 34, -1, 
    -1, -1, -1, 123, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 251, -1, 
    -1, -1, 176, -1, -1, -1, -1, -1, 270, -1, -1, -1, -1, -1, -1, -1, 203, -1, 
    -1, -1, -1, -1, -1, 165, -1, -1, -1, 184, -1, -1, -1, -1, -1, -1, -1, 190, 
    103, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 120, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 20, -1, -1, -1, -1, -1, -1, -1, -1, 144, -1, -1, 
    254, -1, -1, -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, -1, 205, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, -1, -1, -1, 104, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 196, -1, -1, -1, -1, 35, -1, -1, -1, -1, 
    115, -1, -1, -1, -1, -1, -1, 134, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 253, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 207, -1, -1, 166, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 231, -1, 
    -1, -1, -1, -1, 143, -1, 238, -1, -1, -1, -1, -1, 43, -1, -1, -1, -1, -1, 
    174, -1, -1, -1, -1, -1, 109, 199, -1, -1, -1, -1, -1, -1, 256, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 209, -1, -1, -1, 136, -1, -1, 
    -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 81, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 37, -1, -1, -1, -1, -1, -1, -1, -1, 267, 
    -1, 96, -1, -1, -1, -1, -1, -1, -1, 148, -1, 258, -1, -1, -1, -1, -1, 150, 
    -1, -1, -1, -1, 90, -1, -1, -1, 211, -1, -1, -1, 140, -1, -1, -1, -1, -1, 
    -1, -1, -1, 208, -1, -1, -1, -1, -1, -1, -1, -1, 13, 82, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 38, 45, -1, -1, -1, -1, -1, 180, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 213, -1, -1, -1, 142, -1, -1, -1, 189, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 273, 147, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 89, -1, 
    172, -1, 177, -1, -1, 260, 112, -1, -1, -1, -1, -1, 40, -1, -1, -1, -1, -1, 
    36, -1, 216, 61, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 241, -1, -1, -1, -1, -1, -1, -1, -1, 243, -1, -1, -1, -1, -1, -1, 110, 
    39, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 215, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 194, -1, -1, 218, 
    54, -1, -1, 149, -1, -1, -1, -1, -1, -1, -1, 167, -1, -1, 129, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 132, 
    -1, -1, -1, -1, -1, -1, -1, -1, 12, -1, 135, -1, -1, 30, -1, -1, -1, 70, 
    262, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 220, -1, -1, 
    -1, 151, -1, 170, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, 14, 
    -1, -1, 42, -1, -1, -1, -1, -1, 263, -1, -1, 0, -1, -1, 94, -1, -1, -1, -1, 
    -1, -1, 105, -1, -1, -1, -1, -1, -1, 75, -1, -1, -1, -1, -1, -1, 264, -1, 
    -1, -1, -1, 98, -1, -1, -1, -1, -1, -1, -1, -1, -1, 222, -1, -1, -1, 161, 
    -1, -1, 200, -1, -1, -1, -1, -1, -1, 71, 131, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 237, -1, 46, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 204, -1, -1, -1, -1, -1, -1, -1, 266, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 224, -1, -1, 217, 169, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 229, 235, -1, 
    233, -1, -1, -1, -1, -1, -1, 2, -1, -1, -1, -1, -1, -1, 141, -1, -1, -1, -1, 
    -1, 62, -1, -1, 155, 97, -1, -1, -1, -1, -1, -1, 268, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 159, 226, -1, 73, -1, 171, -1, -1, 271, -1, 
    107, -1, 127, -1, -1, -1, -1, -1, -1, -1, -1, 227, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 4, -1, 87, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 29, -1, -1, -1, 
    146, -1, 138, -1, -1, -1, 228, -1, -1, -1, 173, -1, -1, -1, 50, -1, -1, 78, 
    -1, -1, -1, 60, -1, 219, -1, -1, 269, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    52, -1, 7, -1, -1, -1, 57, 79, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 111, -1, -1, -1, -1, -1, -1, -1, 160, -1, 
    -1, -1, 214, -1, 230, -1, -1, -1, -1, 16, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 162, -1, -1, 163, -1, -1, 
    15, -1, -1, -1 
  );

{$Q-}
function TSynCacheSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 355 + Ord(Str^) * 71;
    inc(Str);
  end;
  Result := Result mod 1997;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynCacheSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynCacheSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[379] := Func38html;
  fIdentFuncTable[1125] := Func38sql;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
end;

function TSynCacheSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCacheSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynCacheSyn.Func38html(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkEmbedSQL;
    fRange := rsHTML;
  end
  else
    Result := tkIdentifier;
end;

function TSynCacheSyn.Func38sql(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkEmbedSQL;
    fRange := rsSQL;
  end
  else
    Result := tkIdentifier;
end;

constructor TSynCacheSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fClassAttri := TSynHighlighterAttributes.Create(SYNS_AttrClass, SYNS_FriendlyAttrClass);
  AddAttribute(fClassAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  AddAttribute(fFunctionAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDir, SYNS_FriendlyAttrDir);
  AddAttribute(fDirectiveAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fIndirectAttri := TSynHighlighterAttributes.Create(SYNS_AttrIndirect, SYNS_FriendlyAttrIndirect);
  AddAttribute(fIndirectAttri);
  fLabelAttri := TSynHighlighterAttributes.Create(SYNS_AttrLabel, SYNS_FriendlyAttrLabel);
  AddAttribute(fLabelAttri);
  fMacroAttri := TSynHighlighterAttributes.Create(SYNS_AttrMacro, SYNS_FriendlyAttrMacro);
  AddAttribute(fMacroAttri);
  fUserFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrUserFunction, SYNS_FriendlyAttrUserFunction);
  AddAttribute(fUserFunctionAttri);
  fEmbedSQLAttri := TSynHighlighterAttributes.Create(SYNS_AttrEmbedSQL, SYNS_FriendlyAttrEmbedSQL);
  AddAttribute(fEmbedSQLAttri);
  fEmbedTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrEmbedText, SYNS_FriendlyAttrEmbedText);
  AddAttribute(fEmbedTextAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterCache;
  fRange := rsUnknown;
end;

procedure TSynCacheSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
  FRange := rsUnknown;
end;

procedure TSynCacheSyn.CommentProc;
begin
  fTokenID := tkComment;
  if FLine[Run+1]=';' then fTokenID := tkEmbedText;

  while FLine[Run] <> #0 do  begin
    case FLine[Run] of
      #10, #13: break;
    end;
    inc(Run);
  end;
end;

//------------------------------------------------------------------------------
//    higlight keywords and identifiers
//------------------------------------------------------------------------------
procedure TSynCacheSyn.IdentProc;
var
  fir: WideChar;
begin
  if FTokenPos=0 then fTokenID := tkLabel
  else begin
    fir := FLine[Run];
    if fir = '^' then FCanKey := true;

    FRange := rsUnknown;
    if FCanKey then
      fTokenID := IdentKind(fLine + Run)
    else
    begin
      fTokenID := tkIdentifier;
      while IsIdentChar(fLine[Run]) do inc(Run);
      exit;
    end;
    FRange := rsCommand;
    inc(Run, fStringLen);
    if not (IsLineEnd(Run) or CharInSet(fLine[Run], [#32, ':'])) and (fir <> '^') then
    begin
      fTokenID := tkIdentifier;
    end
  end;
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynCacheSyn.LFProc;
begin
  fTokenID := tkSpace;
  FCanKey := true;
  inc(Run);
end;

procedure TSynCacheSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynCacheSyn.NumberProc;

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
  if (fTokenPos = 0) and CharInSet(FLine[Run], ['0'..'9']) then
  begin
    fTokenID := tkLabel;
    while IsIdentChar(fLine[Run]) do inc(Run);
    FCanKey := false;
    exit;
  end;

  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':  if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
  FRange := rsUnknown;
end;

procedure TSynCacheSyn.SpaceProc;
var
  x: integer;
begin
  x := Run;
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
  FCanKey := true;
  if FRange = rsCommand then
    FCanKey := (Run - x > 1);
end;

procedure TSynCacheSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
  FRange := rsUnknown;
end;

procedure TSynCacheSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynCacheSyn.Next;
begin
  fTokenPos := Run;
  if FLine[Run] = #0 then NullProc
  else
    case fRange of
      rsSQL,
      rsHTML: EmbeddedProc;
      else
        case fLine[Run] of
          #13: CRProc;
          ';': CommentProc;
          'A'..'Z', 'a'..'z', '%', '^': IdentProc;
          '$': FuncProc;
          '@': IndirectProc;
          #10: LFProc;
          #0: NullProc;
          '0'..'9': NumberProc;
          #1..#9, #11, #12, #14..#32: SpaceProc;
          #34: StringProc;
          '(',')','+','-','[',']','.','<','>','''','=',',',':','/','\',
          '?','!','_','*': SymbolProc;
          '#': DirectiveProc;
          '&': EmbeddedProc;
          else UnknownProc;
        end;
    end;
  inherited;
end;

function TSynCacheSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynCacheSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynCacheSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynCacheSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCacheSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkClass: Result := fClassAttri;
    tkComment: Result := fCommentAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkDirective: Result := fDirectiveAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkIndirect: Result := fIndirectAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkLabel: Result := fLabelAttri;
    tkMacro: Result := fMacroAttri;
    tkUserFunction: Result := fUserFunctionAttri;
    tkEmbedSQL: Result := fEmbedSQLAttri;
    tkEmbedText: Result := fEmbedTextAttri;
  else Result := nil;
  end;
end;

function TSynCacheSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynCacheSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynCacheSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynCacheSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCache;
end;

function TSynCacheSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '0'..'9', 'a'..'z', 'A'..'Z', '%', '^', '$', '&':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynCacheSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCache;
end;

//------------------------------------------------------------------------------
//   highlight indirection syntax:   @ident
//------------------------------------------------------------------------------
procedure TSynCacheSyn.IndirectProc;
begin
  fTokenID := tkIndirect;
  inc(Run);
  while IsIdentChar(FLine[Run]) do inc(Run);
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
//  highlight symbols
//------------------------------------------------------------------------------
procedure TSynCacheSyn.SymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
//  highlight user defined functions and macros
//              function:   $$ident
//              macro   :   $$$ident
//------------------------------------------------------------------------------
procedure TSynCacheSyn.FuncProc;
begin
  case FLine[Run] of
    '$': case FLine[Run + 1] of
           '$': case Fline[Run + 2] of
                  '$': fTokenID := tkMacro;
                  else fTokenID := tkUserFunction;
                end;
           else begin
                  fTokenID := IdentKind((fLine + Run));
                  inc(Run, fStringLen);
                  if fTokenID = tkKey then fTokenID := tkFunction;
                end;
         end;
    else fTokenID := tkIdentifier;
  end;
  while IsIdentChar(fLine[Run]) do inc(Run);
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
//    highlight preprocesor directives and class syntax
//              preprocesor:  #identifier
//              class      :  ##class
//------------------------------------------------------------------------------
procedure TSynCacheSyn.DirectiveProc;
var
  i: integer;
begin
  if FLine[Run + 1] = '#' then
    fTokenID := tkClass
  else
  begin
    for i := fTokenPos downto 0 do
      if not CharInSet(FLine[i], [#32, '#']) then
      begin
        fTokenID := tkSymbol;
        inc(Run);
        exit;
      end;

    fTokenID := tkDirective
  end;

  inc(Run);
  while IsIdentChar(fLine[Run]) or (FLine[Run] = '#') do inc(Run);
  FRange := rsUnknown;
end;

//------------------------------------------------------------------------------
//  highlight embeded SQL and HTML
//                SQL  :    &sql( .... )
//                HTML :    &html<   ..... >
//------------------------------------------------------------------------------
procedure TSynCacheSyn.EmbeddedProc;
begin
  case fRange of
    rsUnknown, rsCommand: begin
                 fTokenID := IdentKind( (fLine + Run) );
                 if fTokenID <> tkEmbedSQL then begin
                   fTokenID := tkSymbol;
                   inc( Run );
                 end else begin
                   fBrace := 1;
                   fFirstBrace := true;
                   inc( Run, fStringLen );
                 end;
               end;
    rsSQL: begin
             fTokenID := tkEmbedSQL;
             while (FLine[Run] <> #0) and (fBrace<>0) do begin
               case FLine[Run] of
                 '(': if not fFirstBrace then inc(fBrace)
                      else fFirstBrace := false;
                 ')': dec(fBrace);
               end;
               inc(Run);
             end;
             if fBrace=0 then fRange := rsUnknown;
           end;
    rsHTML: begin
              fTokenID := tkEmbedSQL;
              while (FLine[Run] <> #0) and (fBrace<>0) do begin
                case FLine[Run] of
                  '<': if not fFirstBrace then inc(fBrace)
                       else fFirstBrace := false;
                  '>': dec(fBrace);
                end;
                inc(Run);
              end;
              if fBrace=0 then fRange := rsUnknown;
            end;
  end;
end;

class function TSynCacheSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCache;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCacheSyn);
{$ENDIF}
end.
