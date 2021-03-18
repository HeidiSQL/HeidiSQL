{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHTML.pas, released 2000-04-10.
The Original Code is based on the hkHTMLSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Hideo Koiso.
Unicode translation by Maël Hörz.
HTML5 tags added by CodehunterWorks
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

$Id: SynHighlighterHtml.pas,v 1.24.3 2012/09/13 12:05:00 codehunterworks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an HTML highlighter for SynEdit)
@author(Hideo Koiso, converted to SynEdit by Michael Hieke)
@created(1999-11-02, converted to SynEdit 2000-04-10)
@lastmod(2012-09-13)
The SynHighlighterHTML unit provides SynEdit with an HTML highlighter.
}

unit SynHighlighterHtml;

interface

{$I SynEdit.inc}

uses
{$IFDEF UNICODE}
  WideStrUtils,
{$ENDIF}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

const
  MAX_ESCAPEAMPS = 249;

  EscapeAmps: array[0..MAX_ESCAPEAMPS - 1] of PWideChar = (
    ('&Alpha;'),         { ?        }  { greek capital alpha }
    ('&Beta;'),          { ?        }  { greek capital beta }
    ('&Gamma;'),         { G        }  { greek capital gamma }
    ('&Delta;'),         { ?        }  { greek capital delta }
    ('&Epsilon;'),       { ?        }  { greek capital epsilon }
    ('&Zeta;'),          { ?        }  { greek capital zeta }
    ('&Eta;'),           { ?        }  { greek capital eta }
    ('&Theta;'),         { T        }  { greek capital theta }
    ('&Iota;'),          { ?        }  { greek capital iota }
    ('&Kappa;'),         { ?        }  { greek capital kappa }
    ('&Lambda;'),        { ?        }  { greek capital lambda }
    ('&Mu;'),            { ?        }  { greek capital mu }
    ('&Nu;'),            { ?        }  { greek capital nu }
    ('&Xi;'),            { ?        }  { greek capital xi }
    ('&Omicron;'),       { ?        }  { greek capital omicron }
    ('&Pi;'),            { ?        }  { greek capital pi }
    ('&Rho;'),           { ?        }  { greek capital rho }
    ('&Sigma;'),         { S        }  { greek capital sigma }
    ('&Tau;'),           { ?        }  { greek capital tau }
    ('&Upsilon;'),       { ?        }  { greek capital upsilon }
    ('&Phi;'),           { F        }  { greek capital phi }
    ('&Chi;'),           { ?        }  { greek capital chi }
    ('&Psi;'),           { ?        }  { greek capital psi }
    ('&Omega;'),         { O        }  { greek capital omega }
    ('&alpha;'),         { a        }  { greek small alpha }
    ('&beta;'),          { ß        }  { greek small beta }
    ('&gamma;'),         { ?        }  { greek small gamma }
    ('&delta;'),         { d        }  { greek small delta }
    ('&epsilon;'),       { e        }  { greek small epsilon }
    ('&zeta;'),          { ?        }  { greek small zeta }
    ('&eta;'),           { ?        }  { greek small eta }
    ('&theta;'),         { ?        }  { greek small theta }
    ('&iota;'),          { ?        }  { greek small iota }
    ('&kappa;'),         { ?        }  { greek small kappa }
    ('&lambda;'),        { ?        }  { greek small lambda }
    ('&mu;'),            { µ        }  { greek small mu }
    ('&nu;'),            { ?        }  { greek small nu }
    ('&xi;'),            { ?        }  { greek small xi }
    ('&omicron;'),       { ?        }  { greek small omicron }
    ('&pi;'),            { p        }  { greek small pi }
    ('&rho;'),           { ?        }  { greek small rho }
    ('&sigmaf;'),        { ?        }  { greek small final sigma }
    ('&sigma;'),         { s        }  { greek small sigma }
    ('&tau;'),           { t        }  { greek small tau }
    ('&upsilon;'),       { ?        }  { greek small upsilon }
    ('&phi;'),           { f        }  { greek small phi }
    ('&chi;'),           { ?        }  { greek small chi }
    ('&psi;'),           { ?        }  { greek small psi }
    ('&omega;'),         { ?        }  { greek small omega }
    ('&thetasym;'),      { ?        }  { greek small theta symbol }
    ('&upsih;'),         { ?        }  { greek upsilon with hook symbol }
    ('&piv;'),           { ?        }  { greek pi symbol }
    ('&bull;'),          { •        }  { bullet }
    ('&hellip;'),        { …        }  { horizontal ellipsis }
    ('&prime;'),         { '        }  { prime }
    ('&Prime;'),         { "        }  { double prime }
    ('&oline;'),         { ?        }  { overline, = spacing overscore }
    ('&frasl;'),         { /        }  { fraction slash }
    ('&weierp;'),        { P        }  { script capital P }
    ('&image;'),         { I        }  { imaginary part }
    ('&real;'),          { R        }  { real part }
    ('&trade;'),         { ™        }  { trademark sign }
    ('&alefsym;'),       { ?        }  { first transfinite cardinal }
    ('&larr;'),          { ?        }  { leftwards arrow }
    ('&uarr;'),          { ?        }  { upwards arrow }
    ('&rarr;'),          { ?        }  { rightwards arrow }
    ('&darr;'),          { ?        }  { downwards arrow }
    ('&harr;'),          { ?        }  { left right arrow }
    ('&crarr;'),         { ?        }  { carriage return arrow }
    ('&lArr;'),          { ?        }  { leftwards double arrow }
    ('&uArr;'),          { ?        }  { upwards double arrow }
    ('&rArr;'),          { ?        }  { rightwards double arrow }
    ('&dArr;'),          { ?        }  { downwards double arrow }
    ('&hArr;'),          { ?        }  { left right double arrow }
    ('&forall;'),        { ?        }  { for all }
    ('&part;'),          { ?        }  { partial differential }
    ('&exist;'),         { ?        }  { there exists }
    ('&empty;'),         { Ø        }  { empty set }
    ('&nabla;'),         { ?        }  { backward difference }
    ('&isin;'),          { ?        }  { element of }
    ('&notin;'),         { ?        }  { not an element of }
    ('&ni;'),            { ?        }  { contains as member }
    ('&prod;'),          { ?        }  { n-ary product }
    ('&sum;'),           { ?        }  { n-ary sumation }
    ('&minus;'),         { -        }  { minus sign }
    ('&lowast;'),        { *        }  { asterisk operator }
    ('&radic;'),         { v        }  { square root }
    ('&prop;'),          { ?        }  { proportional to }
    ('&infin;'),         { 8        }  { infinity }
    ('&ang;'),           { ?        }  { angle }
    ('&and;'),           { ?        }  { logical and }
    ('&or;'),            { ?        }  { logical or }
    ('&cap;'),           { n        }  { intersection }
    ('&cup;'),           { ?        }  { union }
    ('&int;'),           { ?        }  { integral }
    ('&there4;'),        { ?        }  { therefore }
    ('&sim;'),           { ~        }  { similar to = tilde operator }
    ('&cong;'),          { ?        }  { approximately equal to }
    ('&asymp;'),         { ˜        }  { almost euqal to }
    ('&ne;'),            { ?        }  { not equal to }
    ('&equiv;'),         { =        }  { identical to }
    ('&le;'),            { =        }  { less-than or equal to }
    ('&ge;'),            { =        }  { greater-than or equal to }
    ('&sub;'),           { ?        }  { subset of }
    ('&sup;'),           { ?        }  { superset of }
    ('&nsub;'),          { ?        }  { not a subset of }
    ('&sube;'),          { ?        }  { subset of or equal to }
    ('&supe;'),          { ?        }  { superset of or equal to }
    ('&oplus;'),         { ?        }  { circled plus }
    ('&otimes;'),        { ?        }  { circled times }
    ('&perp;'),          { ?        }  { orthogonal to = perpendicular }
    ('&sdot;'),          { ·        }  { dot operator }
    ('&lceil;'),         { ?        }  { left ceiling }
    ('&rceil;'),         { ?        }  { right ceiling }
    ('&lfloor;'),        { ?        }  { left floor }
    ('&rfloor;'),        { ?        }  { right floor }
    ('&lang;'),          { <        }  { left-pointing angle bracket }
    ('&rang;'),          { >        }  { right-pointing angle bracket }
    ('&loz;'),           { ?        }  { lozenge }
    ('&spades;'),        { ?        }  { black spade suit }
    ('&clubs;'),         { ?        }  { black club suit }
    ('&hearts;'),        { ?        }  { black heart suit }
    ('&diams;'),         { ?        }  { black diamond suit }
    ('&lsquo;'),         { ‘        }  { left single quote  }
    ('&rsquo;'),         { ’        }  { right single quote }
    ('&sbquo;'),         { ‚        }  { single low-9 quote }
    ('&ldquo;'),         { “        }  { left double quote }
    ('&rdquo;'),         { ”        }  { right double quote }
    ('&bdquo;'),         { „        }  { double low-9 quote }
    ('&dagger;'),        { †        }  { dagger }
    ('&Dagger;'),        { ‡        }  { double dagger }
    ('&permil;'),        { ‰        }  { per mill sign }
    ('&lsaquo;'),        { ‹        }  { single left-pointing angle quote }
    ('&rsaquo;'),        { ›        }  { single right-pointing angle quote }
    ('&quot;'),          { &#034; " }  { double quotation mark }
    ('&amp;'),           { &#038; & }  { ampersand }
    ('&lt;'),            { &#060; < }  { less-than sign }
    ('&gt;'),            { >        }  { greater-than sign }
    ('&ndash;'),         { &#150; – }  { en dash }
    ('&mdash;'),         { &#151; — }  { em dash }
    ('&nbsp;'),          { &#160;   }  { nonbreaking space }
    ('&thinsp;'),        {          }  { thin space }
    ('&ensp;'),          {          }  { en space }
    ('&emsp;'),          {          }  { em space }
    ('&iexcl;'),         { &#161; ! }  { inverted exclamation }
    ('&cent;'),          { &#162; c }  { cent sign }
    ('&pound;'),         { &#163; L }  { pound sterling }
    ('&curren;'),        { &#164; ¤ }  { general currency sign }
    ('&yen;'),           { &#165; Y }  { yen sign }
    ('&brvbar;'),        { &#166; ¦ }  { broken vertical bar }
    ('&brkbar;'),        { &#166; ¦ }  { broken vertical bar }
    ('&sect;'),          { &#167; § }  { section sign }
    ('&uml;'),           { &#168; ¨ }  { umlaut }
    ('&die;'),           { &#168; ¨ }  { umlaut }
    ('&copy;'),          { &#169; © }  { copyright }
    ('&ordf;'),          { &#170; a }  { feminine ordinal }
    ('&laquo;'),         { &#171; « }  { left angle quote }
    ('&not;'),           { &#172; ¬ }  { not sign }
    ('&shy;'),           { &#173; ­ }  { soft hyphen }
    ('&reg;'),           { &#174; ® }  { registered trademark }
    ('&macr;'),          { &#175; — }  { macron accent }
    ('&hibar;'),         { &#175; — }  { macron accent }
    ('&deg;'),           { &#176; ° }  { degree sign }
    ('&plusmn;'),        { &#177; ± }  { plus or minus }
    ('&sup2;'),          { &#178; 2 }  { superscript two }
    ('&sup3;'),          { &#179; 3 }  { superscript three }
    ('&acute;'),         { &#180; ´ }  { acute accent }
    ('&micro;'),         { &#181; µ }  { micro sign }
    ('&para;'),          { &#182; ¶ }  { paragraph sign }
    ('&middot;'),        { &#183; · }  { middle dot }
    ('&cedil;'),         { &#184; ¸ }  { cedilla }
    ('&sup1;'),          { &#185; 1 }  { superscript one }
    ('&ordm;'),          { &#186; o }  { masculine ordinal }
    ('&raquo;'),         { &#187; » }  { right angle quote }
    ('&frac14;'),        { &#188; 1 }  { one-fourth }
    ('&frac12;'),        { &#189; 1 }  { one-half }
    ('&frac34;'),        { &#190; 3 }  { three-fourths }
    ('&iquest;'),        { &#191; ? }  { inverted question mark }
    ('&Agrave;'),        { &#192; A }  { uppercase A, grave accent }
    ('&Aacute;'),        { &#193; Á }  { uppercase A, acute accent }
    ('&Acirc;'),         { &#194; Â }  { uppercase A, circumflex accent }
    ('&Atilde;'),        { &#195; A }  { uppercase A, tilde }
    ('&Auml;'),          { &#196; Ä }  { uppercase A, umlaut }
    ('&Aring;'),         { &#197; A }  { uppercase A, ring }
    ('&AElig;'),         { &#198; A }  { uppercase AE }
    ('&Ccedil;'),        { &#199; Ç }  { uppercase C, cedilla }
    ('&Egrave;'),        { &#200; E }  { uppercase E, grave accent }
    ('&Eacute;'),        { &#201; É }  { uppercase E, acute accent }
    ('&Ecirc;'),         { &#202; E }  { uppercase E, circumflex accent }
    ('&Euml;'),          { &#203; Ë }  { uppercase E, umlaut }
    ('&Igrave;'),        { &#204; I }  { uppercase I, grave accent }
    ('&Iacute;'),        { &#205; Í }  { uppercase I, acute accent }
    ('&Icirc;'),         { &#206; Î }  { uppercase I, circumflex accent }
    ('&Iuml;'),          { &#207; I }  { uppercase I, umlaut }
    ('&ETH;'),           { &#208; ? }  { uppercase Eth, Icelandic }
    ('&Ntilde;'),        { &#209; N }  { uppercase N, tilde }
    ('&Ograve;'),        { &#210; O }  { uppercase O, grave accent }
    ('&Oacute;'),        { &#211; Ó }  { uppercase O, acute accent }
    ('&Ocirc;'),         { &#212; Ô }  { uppercase O, circumflex accent }
    ('&Otilde;'),        { &#213; O }  { uppercase O, tilde }
    ('&Ouml;'),          { &#214; Ö }  { uppercase O, umlaut }
    ('&times;'),         { &#215; × }  { multiplication sign }
    ('&Oslash;'),        { &#216; O }  { uppercase O, slash }
    ('&Ugrave;'),        { &#217; U }  { uppercase U, grave accent }
    ('&Uacute;'),        { &#218; Ú }  { uppercase U, acute accent }
    ('&Ucirc;'),         { &#219; U }  { uppercase U, circumflex accent }
    ('&Uuml;'),          { &#220; Ü }  { uppercase U, umlaut }
    ('&Yacute;'),        { &#221; Ý }  { uppercase Y, acute accent }
    ('&THORN;'),         { &#222; ? }  { uppercase THORN, Icelandic }
    ('&szlig;'),         { &#223; ß }  { lowercase sharps, German }
    ('&agrave;'),        { &#224; à }  { lowercase a, grave accent }
    ('&aacute;'),        { &#225; á }  { lowercase a, acute accent }
    ('&acirc;'),         { &#226; â }  { lowercase a, circumflex accent }
    ('&atilde;'),        { &#227; ã }  { lowercase a, tilde }
    ('&auml;'),          { &#228; ä }  { lowercase a, umlaut }
    ('&aring;'),         { &#229; å }  { lowercase a, ring }
    ('&aelig;'),         { &#230; a }  { lowercase ae }
    ('&ccedil;'),        { &#231; ç }  { lowercase c, cedilla }
    ('&egrave;'),        { &#232; e }  { lowercase e, grave accent }
    ('&eacute;'),        { &#233; é }  { lowercase e, acute accent }
    ('&ecirc;'),         { &#234; ê }  { lowercase e, circumflex accent }
    ('&euml;'),          { &#235; ë }  { lowercase e, umlaut }
    ('&igrave;'),        { &#236; i }  { lowercase i, grave accent }
    ('&iacute;'),        { &#237; í }  { lowercase i, acute accent }
    ('&icirc;'),         { &#238; î }  { lowercase i, circumflex accent }
    ('&iuml;'),          { &#239; i }  { lowercase i, umlaut }
    ('&eth;'),           { &#240; ? }  { lowercase eth, Icelandic }
    ('&ntilde;'),        { &#241; ñ }  { lowercase n, tilde }
    ('&ograve;'),        { &#242; o }  { lowercase o, grave accent }
    ('&oacute;'),        { &#243; ó }  { lowercase o, acute accent }
    ('&ocirc;'),         { &#244; ô }  { lowercase o, circumflex accent }
    ('&otilde;'),        { &#245; o }  { lowercase o, tilde }
    ('&ouml;'),          { &#246; ö }  { lowercase o, umlaut }
    ('&divide;'),        { &#247; ÷ }  { division sign }
    ('&oslash;'),        { &#248; o }  { lowercase o, slash }
    ('&ugrave;'),        { &#249; u }  { lowercase u, grave accent }
    ('&uacute;'),        { &#250; ú }  { lowercase u, acute accent }
    ('&ucirc;'),         { &#251; u }  { lowercase u, circumflex accent }
    ('&uuml;'),          { &#252; ü }  { lowercase u, umlaut }
    ('&yacute;'),        { &#253; ý }  { lowercase y, acute accent }
    ('&thorn;'),         { &#254; ? }  { lowercase thorn, Icelandic }
    ('&yuml;'),          { &#255; y }  { lowercase y, umlaut }
    ('&euro;'),          { €        }  { euro sign }
    ('&OElig;'),         { Œ        }  { capital ligature OE }
    ('&oelig;'),         { œ        }  { small ligature oe }
    ('&scaron;'),        { š        }  { small S with caron }
    ('&Scaron;'),        { Š        }  { capital S with caron }
    ('&fnof;'),          { ƒ        }  { function }
    ('&circ;')           { ˆ        }  { circumflex accent }
  );


type
  TtkTokenKind = (tkAmpersand, tkComment, tkIdentifier, tkKey, tkNull,
    tkSpace, tkSymbol, tkText, tkUndefKey, tkValue);

  TRangeState = (rsAmpersand, rsComment, rsKey, rsParam, rsText,
    rsUnknown, rsValue, rsQuoteValue, rsDoubleQuoteValue);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynHTMLSyn = class(TSynCustomHighlighter)
  private
    FAndCode: Integer;
    FRange: TRangeState;
//    FIdentFuncTable: array[0..1542] of TIdentFuncTableFunc;
    FIdentFuncTable: array[0..2178] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    FAndAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FUndefKeyAttri: TSynHighlighterAttributes;
    FValueAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure TextProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure AmpersandProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property AndAttri: TSynHighlighterAttributes read FAndAttri write FAndAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property TextAttri: TSynHighlighterAttributes read FTextAttri
      write FTextAttri;
    property UndefKeyAttri: TSynHighlighterAttributes read FUndefKeyAttri
      write FUndefKeyAttri;
    property ValueAttri: TSynHighlighterAttributes read FValueAttri
      write FValueAttri;
  end;

implementation

uses
  SynEditStrConst;

const
//  KeyWords: array[0..201] of UnicodeString = (
//    '!doctype', '/a', '/abbr', '/acronym', '/address', '/applet', '/b', '/bdo',
//    '/big', '/blink', '/blockquote', '/body', '/button', '/caption', '/center',
//    '/cite', '/code', '/colgroup', '/comment', '/dd', '/del', '/dfn', '/dir',
//    '/div', '/dl', '/dt', '/em', '/embed', '/fieldset', '/font', '/form',
//    '/frameset', '/h1', '/h2', '/h3', '/h4', '/h5', '/h6', '/head', '/html',
//    '/i', '/iframe', '/ilayer', '/ins', '/kbd', '/label', '/layer', '/legend',
//    '/li', '/listing', '/map', '/marquee', '/menu', '/multicol', '/nobr',
//    '/noembed', '/noframes', '/nolayer', '/noscript', '/object', '/ol',
//    '/optgroup', '/option', '/p', '/pre', '/q', '/s', '/samp', '/script',
//    '/select', '/server', '/small', '/span', '/strike', '/strong', '/style',
//    '/sub', '/sup', '/table', '/tbody', '/td', '/textarea', '/tfoot', '/th',
//    '/thead', '/title', '/tr', '/tt', '/u', '/ul', '/var', '/xmp', 'a', 'abbr',
//    'acronym', 'address', 'applet', 'area', 'b', 'base', 'basefont', 'bdo',
//    'bgsound', 'big', 'blink', 'blockquote', 'body', 'br', 'button', 'caption',
//    'center', 'cite', 'code', 'col', 'colgroup', 'comment', 'dd', 'del', 'dfn',
//    'dir', 'div', 'dl', 'dt', 'em', 'embed', 'fieldset', 'font', 'form',
//    'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head', 'hr',
//    'html', 'i', 'iframe', 'ilayer', 'img', 'input', 'ins', 'isindex', 'kbd',
//    'keygen', 'label', 'layer', 'legend', 'li', 'link', 'listing', 'map',
//    'marquee', 'menu', 'meta', 'multicol', 'nextid', 'nobr', 'noembed',
//    'noframes', 'nolayer', 'noscript', 'object', 'ol', 'optgroup', 'option',
//    'p', 'param', 'plaintext', 'pre', 'q', 's', 'samp', 'script', 'select',
//    'server', 'small', 'spacer', 'span', 'strike', 'strong', 'style', 'sub',
//    'sup', 'table', 'tbody', 'td', 'textarea', 'tfoot', 'th', 'thead', 'title',
//    'tr', 'tt', 'u', 'ul', 'var', 'wbr', 'xmp'
//  );
//
//  KeyIndices: array[0..1542] of Integer = (
//    -1, -1, 182, -1, -1, -1, 97, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, 33, -1, -1, -1, -1, 40, -1, -1, -1, -1, -1, 137, 189, -1, -1,
//    -1, -1, -1, -1, -1, -1, 191, -1, -1, -1, -1, -1, -1, -1, 52, 170, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, 5, 55, -1, 83, -1, -1, 34, -1, 198, -1, -1, -1,
//    -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, 74, 111, -1, 62, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, 35, 72, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 130,
//    190, -1, 117, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 36, -1, -1, 157, -1, -1, -1,
//    -1, -1, 13, 114, -1, -1, -1, -1, 131, -1, -1, -1, -1, -1, -1, 21, -1, -1,
//    -1, 161, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 139, -1,
//    -1, -1, -1, 37, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, -1, -1,
//    132, 103, -1, -1, -1, 199, -1, -1, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, 133, -1, -1, -1, -1, -1, -1, -1, -1, 54,
//    -1, -1, -1, -1, -1, 42, -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1,
//    148, -1, -1, -1, -1, -1, -1, -1, 96, -1, -1, -1, -1, -1, -1, -1, -1, 134,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 183, -1, -1, 168, -1, 45,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 179, -1, -1, 63, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, 135, -1, -1, -1, -1, -1, -1, 60, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 71, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, 65, -1, -1, -1, -1, -1, -1, -1, 125, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, 4, -1, -1, 39, -1, -1, -1, -1, 128, 20, -1, -1, 51, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 176, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, 112, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    180, -1, -1, -1, -1, -1, 172, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 107,
//    -1, -1, -1, -1, 66, -1, -1, -1, 59, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, 162, -1, 8, -1, -1, -1, -1, -1, -1, 166, -1,
//    -1, -1, 169, 141, 86, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 95, -1, -1,
//    -1, -1, -1, -1, -1, 56, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, 19, -1, -1, 41, -1, 173, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, 88, -1, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1, -1, 186,
//    -1, -1, -1, -1, -1, -1, -1, -1, 3, -1, -1, -1, -1, -1, -1, -1, -1, 200, -1,
//    -1, -1, 87, 181, -1, -1, -1, -1, 119, -1, -1, -1, 57, -1, -1, -1, 104, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 47, -1, 26, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, 174, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, 201, -1, -1, -1, 195, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    58, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, 101,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, 116, -1, -1, -1, -1, 113, 187, -1, -1,
//    -1, 94, -1, -1, -1, -1, -1, 165, -1, -1, -1, -1, -1, -1, -1, 69, -1, -1, -1,
//    -1, -1, 167, -1, -1, 163, -1, -1, 197, -1, -1, -1, -1, 78, -1, 68, -1, -1,
//    -1, -1, -1, -1, 145, -1, -1, 196, -1, -1, -1, -1, 12, -1, -1, -1, 160, -1,
//    61, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 123,
//    -1, -1, -1, -1, -1, -1, 76, 120, -1, 140, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, 10, -1, -1, -1, -1, -1, 153, -1, -1, -1, -1, -1, -1, -1, 30, -1, -1,
//    -1, -1, -1, -1, 142, -1, -1, -1, -1, -1, 99, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 152, -1, 171,
//    -1, -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, 150, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    22, -1, -1, -1, -1, -1, 138, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    24, -1, 70, -1, -1, -1, -1, -1, -1, 29, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 53, -1, -1, 177, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, -1, -1, -1, -1, -1, -1, 100, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 144, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 90, -1, -1,
//    -1, 121, 159, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, 23, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 155, 149, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, 81, 2, -1, 110, -1, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, 146, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    178, -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, -1, 143, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, 164, -1, -1, -1, 48, -1, -1, -1, -1, -1, -1, 9,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, 6, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, 188, -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, 73, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 64,
//    79, -1, -1, -1, -1, -1, -1, -1, -1, -1, 127, -1, -1, -1, 18, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, 43, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, 184, -1, -1, -1, 175, -1, -1, 193, -1, 92, 151, 154, -1, -1, -1, -1,
//    106, -1, -1, -1, -1, -1, -1, -1, -1, -1, 194, -1, -1, -1, -1, -1, -1, -1,
//    -1, 75, -1, -1, -1, -1, -1, -1, 84, -1, -1, -1, -1, -1, 28, -1, -1, -1, -1,
//    -1, -1, 98, -1, 80, -1, -1, -1, 85, -1, -1, -1, -1, 67, -1, 118, -1, -1, -1,
//    -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, 77, -1, -1, 122, 44, -1, -1,
//    -1, -1, -1, 89, -1, -1, -1, 115, 136, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    -1, -1, -1, -1, -1, -1, -1, 105, -1, -1, -1, -1, -1, -1, -1, -1, 147, -1,
//    16, 185, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//    158, -1, -1, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, 91,
//    -1, -1, 156, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
//  );

  KeyWords: array[0..256] of UnicodeString = (
    '!doctype', '/!doctype', '/a', '/abbr', '/acronym', '/address', '/applet',
    '/area', '/article', '/aside', '/audio', '/b', '/base', '/basefont', '/bb',
    '/bdo', '/big', '/blockquote', '/body', '/button', '/canvas', '/caption',
    '/center', '/cite', '/code', '/col', '/colgroup', '/command', '/datalist',
    '/dd', '/del', '/details', '/dfn', '/dialog', '/dir', '/div', '/dl', '/dt',
    '/em', '/embed', '/fieldset', '/figcaption', '/figure', '/font', '/footer',
    '/form', '/frame', '/frameset', '/h1', '/h2', '/h3', '/h4', '/h5', '/h6',
    '/head', '/header', '/hgroup', '/html', '/i', '/iframe', '/img', '/input',
    '/ins', '/kbd', '/keygen', '/label', '/layer', '/legend', '/li', '/link',
    '/map', '/mark', '/marquee', '/menu', '/meta', '/meter', '/multicol',
    '/nav', '/nobr', '/noembed', '/noframes', '/nolayer', '/noscript',
    '/object', '/ol', '/optgroup', '/option', '/output', '/p', '/param', '/pre',
    '/progress', '/q', '/rp', '/rt', '/ruby', '/s', '/samp', '/script',
    '/section', '/select', '/server', '/small', '/source', '/span', '/strike',
    '/strong', '/style', '/sub', '/summary', '/sup', '/table', '/tbody', '/td',
    '/textarea', '/tfoot', '/th', '/thead', '/time', '/title', '/tr', '/track',
    '/tt', '/u', '/ul', '/var', '/video', '/wbr', '/xmp', 'a', 'abbr',
    'acronym', 'address', 'applet', 'area', 'article', 'aside', 'audio', 'b',
    'base', 'basefont', 'bb', 'bdo', 'big', 'blockquote', 'body', 'button',
    'canvas', 'caption', 'center', 'cite', 'code', 'col', 'colgroup', 'command',
    'datalist', 'dd', 'del', 'details', 'dfn', 'dialog', 'dir', 'div', 'dl',
    'dt', 'em', 'embed', 'fieldset', 'figcaption', 'figure', 'font', 'footer',
    'form', 'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head',
    'header', 'hgroup', 'html', 'i', 'iframe', 'img', 'input', 'ins', 'kbd',
    'keygen', 'label', 'layer', 'legend', 'li', 'link', 'map', 'mark',
    'marquee', 'menu', 'meta', 'meter', 'multicol', 'nav', 'nobr', 'noembed',
    'noframes', 'nolayer', 'noscript', 'object', 'ol', 'optgroup', 'option',
    'output', 'p', 'param', 'pre', 'progress', 'q', 'rp', 'rt', 'ruby', 's',
    'samp', 'script', 'section', 'select', 'server', 'small', 'source', 'span',
    'strike', 'strong', 'style', 'sub', 'summary', 'sup', 'synedit', 'table',
    'tbody', 'td', 'textarea', 'tfoot', 'th', 'thead', 'time', 'title', 'tr',
    'track', 'tt', 'u', 'ul', 'var', 'video', 'wbr', 'xmp'
  );

  KeyIndices: array[0..2178] of Integer = (
    -1, -1, -1, 3, -1, -1, 231, 250, -1, -1, -1, 212, -1, -1, -1, -1, -1, -1,
    -1, -1, 175, -1, -1, -1, -1, -1, 128, -1, -1, -1, -1, 155, -1, -1, -1, -1,
    -1, -1, -1, 83, -1, 201, 122, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 48, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 70, -1, -1, -1, -1, -1, -1, 183, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 216, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1,
    -1, 89, -1, -1, -1, 234, -1, -1, 188, -1, -1, -1, -1, -1, -1, -1, -1, 107,
    -1, -1, 61, -1, -1, -1, -1, -1, 21, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 225, -1, -1, 150, -1, -1, 91, -1, -1, -1, 88, -1,
    -1, -1, 158, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 59, -1,
    -1, -1, 137, 12, -1, 67, -1, -1, 47, -1, -1, -1, -1, -1, 10, -1, -1, 135,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 218, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 170, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    174, -1, 7, -1, -1, -1, 142, -1, -1, -1, -1, -1, -1, -1, -1, 133, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 232, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 77, -1, -1, -1, 178, -1, -1, -1, -1, -1,
    -1, 209, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 130, -1, 162,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 51, -1, -1, -1, 237, -1, -1, -1,
    17, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 79, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 157, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    210, -1, -1, -1, 104, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 206, -1, -1, -1, -1, -1, -1, -1, -1, 165, -1, -1, -1, -1,
    -1, -1, -1, 254, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, 126, -1, -1,
    -1, -1, -1, -1, -1, 24, -1, -1, 238, -1, 96, -1, 38, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 248, -1, -1, -1, 156, -1, 103, -1, -1,
    -1, -1, -1, -1, -1, -1, 239, 211, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    111, -1, -1, -1, -1, -1, -1, -1, 120, -1, -1, -1, 29, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 71, 84, -1, -1, -1, -1, -1, 87, -1, -1, -1, -1, 186, -1,
    -1, -1, -1, -1, -1, -1, -1, 243, -1, -1, -1, -1, 20, -1, -1, -1, -1, -1, -1,
    115, -1, -1, -1, -1, -1, -1, 26, 138, -1, -1, -1, -1, -1, -1, -1, 163, -1,
    -1, 144, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 181, 22, -1, -1, -1, -1, 255, -1, -1, -1, -1, -1, -1, 36, -1, -1, 240,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 112, -1, -1, -1, -1, -1, -1,
    153, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 34, -1, -1, -1,
    -1, -1, -1, -1, 106, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 256,
    -1, 164, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, 145, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 65, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 37,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 114, -1, 197,
    63, -1, -1, -1, -1, -1, -1, 64, -1, -1, -1, -1, -1, -1, 202, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 75, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 44, -1, 200, -1, -1, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, 151,
    -1, -1, -1, -1, -1, -1, 242, -1, -1, -1, -1, -1, -1, -1, -1, 193, 176, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 66, -1, -1, -1, -1, -1, 220, -1, -1, -1,
    141, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 81, -1, -1, 93, 76, -1, -1, 14, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 58, -1, -1, -1, -1, 230, -1, 198, -1,
    -1, -1, -1, -1, -1, 69, -1, -1, -1, -1, 101, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 244, -1, -1, -1, -1, 208, -1, -1, -1,
    -1, -1, -1, -1, 100, 203, 5, -1, -1, -1, -1, -1, 41, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 116, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 25, -1, -1, -1, -1, -1, 45, 92, -1, -1, -1, -1, 80, 204, -1,
    -1, -1, -1, -1, 42, -1, -1, -1, -1, -1, 132, -1, 249, -1, -1, -1, -1, -1,
    -1, -1, 82, -1, 16, -1, 121, 86, -1, -1, -1, 224, -1, 195, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 159, -1, -1, 54,
    -1, -1, -1, -1, -1, -1, -1, -1, 207, -1, -1, 68, -1, -1, -1, -1, -1, -1,
    252, -1, 233, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 56, -1, -1,
    -1, -1, -1, -1, -1, -1, 251, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 124,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 108, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 179, 18, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 191, -1, -1, -1, -1, -1, 99, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 52, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 90,
    171, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 72,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 168, -1, -1, -1, 226, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 222, -1, -1, -1, -1, -1, 253, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 246, -1, -1, -1, -1, -1, -1, -1, -1, -1, 196,
    -1, -1, -1, -1, -1, -1, 199, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6, -1, -1,
    -1, -1, 0, -1, -1, 229, -1, -1, 228, -1, -1, -1, -1, -1, 215, -1, 125, 102,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 227, -1, 172, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 194, -1, -1,
    -1, -1, -1, -1, -1, 184, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 161, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 169, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 213, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 205, -1, -1, 190, -1, -1, -1, 97, -1, -1, -1, -1, -1, 33, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 247, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 55, -1, 19, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 148, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 223, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 187, -1, -1, -1,
    95, -1, 136, -1, 177, -1, -1, -1, -1, -1, -1, -1, -1, -1, 9, -1, -1, -1,
    118, -1, -1, -1, -1, -1, -1, 152, -1, -1, -1, -1, -1, -1, 40, -1, -1, -1,
    -1, -1, -1, 50, -1, -1, -1, -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 143, -1, -1, -1, -1, -1, -1, -1, 214, -1, 166,
    60, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 39, -1, -1, -1, -1, 147, -1, -1,
    -1, -1, -1, 15, -1, -1, 167, -1, -1, 173, -1, -1, -1, -1, -1, -1, -1, 131,
    -1, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 57, -1,
    -1, -1, -1, -1, -1, -1, 149, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 235, 35, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1,
    27, -1, -1, -1, -1, -1, -1, -1, 160, -1, -1, -1, -1, -1, 74, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, -1, 30, -1,
    217, -1, -1, -1, -1, -1, 189, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 154, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    182, -1, 146, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 139, -1, -1, 98, -1, -1, -1, -1, 129,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, -1, 180, -1, -1,
    245, -1, -1, -1, -1, -1, -1, 241, -1, -1, -1, -1, 117, -1, 221, -1, -1, -1,
    -1, -1, 23, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, 53, -1, 1, -1, -1,
    -1, -1, -1, -1, -1, 113, -1, -1, 134, -1, -1, -1, 94, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 185, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 140, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 43, -1, -1, -1, -1, -1, 109, -1, -1, -1,
    -1, -1, 105, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 85, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 127, -1, -1, -1, 28, -1, -1, -1, -1, 123, -1, -1, -1, -1, -1, -1,
    -1, 236, -1, 219, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1
  );

{$Q-}
function TSynHTMLSyn.HashKey(Str: PWideChar): Cardinal;
begin
//  Result := 0;
//  while IsIdentChar(Str^) or CharInSet(Str^, ['!', '/']) do
//  begin
//    Result := Result * 932 + Ord(Str^) * 46;
//    Inc(Str);
//  end;
//  Result := Result mod 1543;
//  FStringLen := Str - FToIdent;

  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 627 + Ord(Str^) * 829;
    Inc(Str);
  end;
  Result := Result mod 2179;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynHTMLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(FIdentFuncTable) then
    Result := FIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynHTMLSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynHTMLSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkUndefKey;
end;

function TSynHTMLSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkUndefKey;
end;

constructor TSynHTMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  FIdentifierAttri.Style := [fsBold];
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := $00ff0080;
  AddAttribute(FKeyAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsBold];
  AddAttribute(FSymbolAttri);

  FTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(FTextAttri);

  FUndefKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknownWord, SYNS_FriendlyAttrUnknownWord);
  FUndefKeyAttri.Style := [fsBold];
  FUndefKeyAttri.Foreground := clRed;
  AddAttribute(FUndefKeyAttri);

  FValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  FValueAttri.Foreground := $00ff8000;
  AddAttribute(FValueAttri);

  FAndAttri := TSynHighlighterAttributes.Create(SYNS_AttrEscapeAmpersand, SYNS_FriendlyAttrEscapeAmpersand);
  FAndAttri.Style := [fsBold];
  FAndAttri.Foreground := $0000ff00;
  AddAttribute(FAndAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  FRange := rsText;
  FDefaultFilter := SYNS_FilterHTML;
  FAndCode := -1;
end;

procedure TSynHTMLSyn.BraceCloseProc;
begin
  FRange := rsText;
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynHTMLSyn.CommentProc;
begin
  FTokenID := tkComment;

  if IsLineEnd(Run) then
  begin
    NextProcedure;
    Exit;
  end;

  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = '>') and (FLine[Run - 1] = '-') and (FLine[Run - 2] = '-') then
    begin
      FRange := rsText;
      Inc(Run);
      Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.BraceOpenProc;
begin
  Inc(Run);
  if (FLine[Run] = '!') and (FLine[Run + 1] = '-') and (FLine[Run + 2] = '-') then
  begin
    FRange := rsComment;
    FTokenID := tkComment;
    Inc(Run, 3);
  end
  else
  begin
    FRange := rsKey;
    FTokenID := tkSymbol;
  end;
end;

procedure TSynHTMLSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynHTMLSyn.EqualProc;
begin
  FRange := rsValue;
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynHTMLSyn.IdentProc;
begin
  case FRange of
  rsKey:
    begin
      FRange := rsParam;
      FTokenID := IdentKind((FLine + Run));
      Inc(Run, FStringLen);
    end;
  rsValue:
    begin
      FRange := rsParam;
      FTokenID := tkValue;
      repeat
        Inc(Run);
      until (FLine[Run] <= #32) or (FLine[Run] = '>');
    end;
  else
    FTokenID := tkIdentifier;
    repeat
      Inc(Run);
    until (FLine[Run] <= #32) or (FLine[Run] = '=') or (FLine[Run] = '"') or
      (FLine[Run] = '>');
  end;
end;

procedure TSynHTMLSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynHTMLSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynHTMLSyn.TextProc;

  function IsStopChar: Boolean;
  begin
    case FLine[Run] of
      #0..#31, '<', '&':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  i: Integer;
begin
  if CharInSet(FLine[Run], [#0..#31, '<']) then
  begin
    NextProcedure;
    Exit;
  end;

  FTokenID := tkText;

  while True do
  begin
    while not IsStopChar do Inc(Run);

    if (FLine[Run] = '&') then
    begin
      if (FLine[Run + 1] = '#') then
      begin
        FAndCode := -1;
        i := Run;
        Inc(Run, 2);
        if CharInSet(FLine[Run], ['X', 'x']) then
        begin
          Inc(Run);
          while IsNumberChar do
            Inc(Run);
        end
        else
          while CharInSet(FLine[Run], ['0'..'9']) do
            Inc(Run);
        if (FLine[Run] = ';') then
        begin
          Inc(Run);
          Run := i;
          FRange := rsAmpersand;
        end;
        Break;
      end
      else
        for i := Low(EscapeAmps) To High(EscapeAmps) do
          if (WStrLComp((FLine + Run), EscapeAmps[i], WStrLen(EscapeAmps[i])) = 0) then
          begin
            FAndCode := i;
            FRange := rsAmpersand;
            Exit;
          end;

      Inc(Run);
    end
    else
      Break;
  end;
end;

procedure TSynHTMLSyn.AmpersandProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if FRange <> rsAmpersand then
  begin
    if FRange = rsKey then
    begin
      Inc(Run);
      FRange := rsText;
      FTokenID := tkText;
    end
    else
      IdentProc;
    Exit;
  end;

  case FAndCode of
  Low(EscapeAmps)..High(EscapeAmps):
    begin
      FTokenID := tkAmpersand;
      Inc(Run, WStrLen(EscapeAmps[FAndCode]));
    end;
    else begin
      if (FLine[Run + 1] = '#') then
      begin
        FAndCode := -1;
        Inc(Run, 2);
        if CharInSet(FLine[Run], ['X', 'x']) then
        begin
          Inc(Run);
          while IsNumberChar do
            Inc(Run);
        end
        else
          while CharInSet(FLine[Run], ['0'..'9']) do
            Inc(Run);
        if (FLine[Run] = ';') then begin
          Inc(Run);
          FTokenID := tkAmpersand;
        end else
          FTokenID := tkText;
      end;
    end;
  end;
  FAndCode := -1;
  FRange := rsText;
end;

procedure TSynHTMLSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while FLine[Run] <= #32 do
  begin
    if CharInSet(FLine[Run], [#0, #9, #10, #13]) then
      Break;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.StringProc;
var
  iOpenChar: WideChar;
begin
  case FRange of
    rsQuoteValue:
      begin
        iOpenChar := #39;
        FTokenID := tkValue;
      end;
    rsDoubleQuoteValue:
      begin
        iOpenChar := '"';
        FTokenID := tkValue;
      end;
    else
    begin
      iOpenChar := FLine[Run];
      if FRange = rsValue then
      begin
        if iOpenChar = '"' then
          FRange := rsDoubleQuoteValue
        else
          FRange := rsQuoteValue;
        FTokenID := tkValue;
      end else
      begin
        IdentProc;
        Exit;
      end;
      Inc(Run); { jumps over the opening char }
    end;
  end;

  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = iOpenChar then
    begin
      Inc(Run);  { jumps over the closing char }
      if FRange in [rsDoubleQuoteValue, rsQuoteValue] then
        FRange := rsParam
      else
        FRange := rsText;
      Break;
    end;
    Inc(Run);
  end;
end;

function TSynHTMLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '/', '0'..'9', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;


procedure TSynHTMLSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsText:
      TextProc;
    rsComment:
      CommentProc;
    rsQuoteValue, rsDoubleQuoteValue:
      if IsLineEnd(Run) then
        NextProcedure
      else
        StringProc;
    else
      NextProcedure;
  end;

  // ensure that one call of Next is enough to reach next token
  if (fOldRun = Run) and not GetEol then Next;

  inherited;
end;

procedure TSynHTMLSyn.NextProcedure;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '&': AmpersandProc;
    '"', #39: StringProc;
    '<': BraceOpenProc;
    '>': BraceCloseProc;
    '=': EqualProc;
    else IdentProc;
  end;
end;

function TSynHTMLSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynHTMLSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynHTMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynHTMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkAmpersand: Result := FAndAttri;
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkSpace: Result := FSpaceAttri;
    tkSymbol: Result := FSymbolAttri;
    tkText: Result := FTextAttri;
    tkUndefKey: Result := FUndefKeyAttri;
    tkValue: Result := FValueAttri;
    else Result := nil;
  end;
end;

function TSynHTMLSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynHTMLSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynHTMLSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynHTMLSyn.ResetRange;
begin
  FRange:= rsText;
end;

function TSynHTMLSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterHTML;
end;

class function TSynHTMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangHTML;
end;

function TSynHTMLSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '<!-- Syntax highlighting -->'#13#10 +
    #13#10 +
    '<html>'#13#10 +
    '<body bgcolor="red">'#13#10 +
    '  <form name="frmLogin" action="doSomething.asp">'#13#10 +
    '    <input name="user" value=''any'#13#10 +
    '      value''>'#13#10 +
    '  </form>'#13#10 +
    '  <invalid>Sample HTML code &copy; 2001</invalid>'#13#10 +
    '</body>'#13#10 +
    '</html>';
end;

class function TSynHTMLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangHTML;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynHTMLSyn);
{$ENDIF}
end.
