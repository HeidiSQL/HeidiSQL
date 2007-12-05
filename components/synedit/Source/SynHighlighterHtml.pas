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

$Id: SynHighlighterHtml.pas,v 1.24.2.10 2006/05/21 11:59:35 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an HTML highlighter for SynEdit)
@author(Hideo Koiso, converted to SynEdit by Michael Hieke)
@created(1999-11-02, converted to SynEdit 2000-04-10)
@lastmod(2000-06-23)
The SynHighlighterHTML unit provides SynEdit with an HTML highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERHTML}
unit SynHighlighterHtml;
{$ENDIF}

interface

{$I SynEdit.inc}

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
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
    rsUnKnown, rsValue, rsQuoteValue, rsDoubleQuoteValue);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynHTMLSyn = class(TSynCustomHighlighter)
  private
    fAndCode: Integer;
    fRange: TRangeState;
    fIdentFuncTable: array[0..1542] of TIdentFuncTableFunc;
    fTokenID: TtkTokenKind;
    fAndAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fUndefKeyAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
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
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property AndAttri: TSynHighlighterAttributes read fAndAttri write fAndAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property UndefKeyAttri: TSynHighlighterAttributes read fUndefKeyAttri
      write fUndefKeyAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri
      write fValueAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynUnicode,
  QSynEditStrConst;
{$ELSE}
  SynUnicode,
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..201] of WideString = (
    '!doctype', '/a', '/abbr', '/acronym', '/address', '/applet', '/b', '/bdo', 
    '/big', '/blink', '/blockquote', '/body', '/button', '/caption', '/center', 
    '/cite', '/code', '/colgroup', '/comment', '/dd', '/del', '/dfn', '/dir', 
    '/div', '/dl', '/dt', '/em', '/embed', '/fieldset', '/font', '/form', 
    '/frameset', '/h1', '/h2', '/h3', '/h4', '/h5', '/h6', '/head', '/html', 
    '/i', '/iframe', '/ilayer', '/ins', '/kbd', '/label', '/layer', '/legend', 
    '/li', '/listing', '/map', '/marquee', '/menu', '/multicol', '/nobr', 
    '/noembed', '/noframes', '/nolayer', '/noscript', '/object', '/ol', 
    '/optgroup', '/option', '/p', '/pre', '/q', '/s', '/samp', '/script', 
    '/select', '/server', '/small', '/span', '/strike', '/strong', '/style', 
    '/sub', '/sup', '/table', '/tbody', '/td', '/textarea', '/tfoot', '/th', 
    '/thead', '/title', '/tr', '/tt', '/u', '/ul', '/var', '/xmp', 'a', 'abbr', 
    'acronym', 'address', 'applet', 'area', 'b', 'base', 'basefont', 'bdo', 
    'bgsound', 'big', 'blink', 'blockquote', 'body', 'br', 'button', 'caption', 
    'center', 'cite', 'code', 'col', 'colgroup', 'comment', 'dd', 'del', 'dfn', 
    'dir', 'div', 'dl', 'dt', 'em', 'embed', 'fieldset', 'font', 'form', 
    'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head', 'hr', 
    'html', 'i', 'iframe', 'ilayer', 'img', 'input', 'ins', 'isindex', 'kbd', 
    'keygen', 'label', 'layer', 'legend', 'li', 'link', 'listing', 'map', 
    'marquee', 'menu', 'meta', 'multicol', 'nextid', 'nobr', 'noembed', 
    'noframes', 'nolayer', 'noscript', 'object', 'ol', 'optgroup', 'option', 
    'p', 'param', 'plaintext', 'pre', 'q', 's', 'samp', 'script', 'select', 
    'server', 'small', 'spacer', 'span', 'strike', 'strong', 'style', 'sub', 
    'sup', 'table', 'tbody', 'td', 'textarea', 'tfoot', 'th', 'thead', 'title', 
    'tr', 'tt', 'u', 'ul', 'var', 'wbr', 'xmp' 
  );

  KeyIndices: array[0..1542] of Integer = (
    -1, -1, 182, -1, -1, -1, 97, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 33, -1, -1, -1, -1, 40, -1, -1, -1, -1, -1, 137, 189, -1, -1, 
    -1, -1, -1, -1, -1, -1, 191, -1, -1, -1, -1, -1, -1, -1, 52, 170, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 5, 55, -1, 83, -1, -1, 34, -1, 198, -1, -1, -1, 
    -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, 74, 111, -1, 62, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 35, 72, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 130, 
    190, -1, 117, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 36, -1, -1, 157, -1, -1, -1, 
    -1, -1, 13, 114, -1, -1, -1, -1, 131, -1, -1, -1, -1, -1, -1, 21, -1, -1, 
    -1, 161, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 139, -1, 
    -1, -1, -1, 37, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, -1, -1, 
    132, 103, -1, -1, -1, 199, -1, -1, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 133, -1, -1, -1, -1, -1, -1, -1, -1, 54, 
    -1, -1, -1, -1, -1, 42, -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, 
    148, -1, -1, -1, -1, -1, -1, -1, 96, -1, -1, -1, -1, -1, -1, -1, -1, 134, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 183, -1, -1, 168, -1, 45, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 179, -1, -1, 63, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 135, -1, -1, -1, -1, -1, -1, 60, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 71, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 65, -1, -1, -1, -1, -1, -1, -1, 125, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 4, -1, -1, 39, -1, -1, -1, -1, 128, 20, -1, -1, 51, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 176, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 112, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    180, -1, -1, -1, -1, -1, 172, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 107, 
    -1, -1, -1, -1, 66, -1, -1, -1, 59, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 162, -1, 8, -1, -1, -1, -1, -1, -1, 166, -1, 
    -1, -1, 169, 141, 86, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 95, -1, -1, 
    -1, -1, -1, -1, -1, 56, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 19, -1, -1, 41, -1, 173, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 88, -1, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1, -1, 186, 
    -1, -1, -1, -1, -1, -1, -1, -1, 3, -1, -1, -1, -1, -1, -1, -1, -1, 200, -1, 
    -1, -1, 87, 181, -1, -1, -1, -1, 119, -1, -1, -1, 57, -1, -1, -1, 104, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 47, -1, 26, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 174, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 201, -1, -1, -1, 195, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    58, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, 101, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 116, -1, -1, -1, -1, 113, 187, -1, -1, 
    -1, 94, -1, -1, -1, -1, -1, 165, -1, -1, -1, -1, -1, -1, -1, 69, -1, -1, -1, 
    -1, -1, 167, -1, -1, 163, -1, -1, 197, -1, -1, -1, -1, 78, -1, 68, -1, -1, 
    -1, -1, -1, -1, 145, -1, -1, 196, -1, -1, -1, -1, 12, -1, -1, -1, 160, -1, 
    61, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 123, 
    -1, -1, -1, -1, -1, -1, 76, 120, -1, 140, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 10, -1, -1, -1, -1, -1, 153, -1, -1, -1, -1, -1, -1, -1, 30, -1, -1, 
    -1, -1, -1, -1, 142, -1, -1, -1, -1, -1, 99, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 152, -1, 171, 
    -1, -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, 150, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    22, -1, -1, -1, -1, -1, 138, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    24, -1, 70, -1, -1, -1, -1, -1, -1, 29, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 53, -1, -1, 177, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, -1, -1, -1, -1, -1, -1, 100, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 144, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 90, -1, -1, 
    -1, 121, 159, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, 23, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 155, 149, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 81, 2, -1, 110, -1, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 146, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    178, -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, -1, 143, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 164, -1, -1, -1, 48, -1, -1, -1, -1, -1, -1, 9, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, 6, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 188, -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, 73, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 64, 
    79, -1, -1, -1, -1, -1, -1, -1, -1, -1, 127, -1, -1, -1, 18, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 43, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 184, -1, -1, -1, 175, -1, -1, 193, -1, 92, 151, 154, -1, -1, -1, -1, 
    106, -1, -1, -1, -1, -1, -1, -1, -1, -1, 194, -1, -1, -1, -1, -1, -1, -1, 
    -1, 75, -1, -1, -1, -1, -1, -1, 84, -1, -1, -1, -1, -1, 28, -1, -1, -1, -1, 
    -1, -1, 98, -1, 80, -1, -1, -1, 85, -1, -1, -1, -1, 67, -1, 118, -1, -1, -1, 
    -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, 77, -1, -1, 122, 44, -1, -1, 
    -1, -1, -1, 89, -1, -1, -1, 115, 136, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 105, -1, -1, -1, -1, -1, -1, -1, -1, 147, -1, 
    16, 185, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    158, -1, -1, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, 91, 
    -1, -1, 156, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 
  );

{$Q-}
function TSynHTMLSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) or (Str^ in [WideChar('!'), WideChar('/')]) do
  begin
    Result := Result * 932 + Ord(Str^) * 46;
    inc(Str);
  end;
  Result := Result mod 1543;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynHTMLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynHTMLSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
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

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  fIdentifierAttri.Style := [fsBold];
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := $00ff0080;
  AddAttribute(fKeyAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Style := [fsBold];
  AddAttribute(fSymbolAttri);

  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(fTextAttri);

  fUndefKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknownWord, SYNS_FriendlyAttrUnknownWord);
  fUndefKeyAttri.Style := [fsBold];
  fUndefKeyAttri.Foreground := clRed;
  AddAttribute(fUndefKeyAttri);

  fValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  fValueAttri.Foreground := $00ff8000;
  AddAttribute(fValueAttri);

  fAndAttri := TSynHighlighterAttributes.Create(SYNS_AttrEscapeAmpersand, SYNS_FriendlyAttrEscapeAmpersand);
  fAndAttri.Style := [fsBold];
  fAndAttri.Foreground := $0000ff00;
  AddAttribute(fAndAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  fRange := rsText;
  fDefaultFilter := SYNS_FilterHTML;
  fAndCode := -1;
end;

procedure TSynHTMLSyn.BraceCloseProc;
begin
  fRange := rsText;
  fTokenId := tkSymbol;
  Inc(Run);
end;

procedure TSynHTMLSyn.CommentProc;
begin
  fTokenID := tkComment;

  if IsLineEnd(Run) then
  begin
    NextProcedure;
    Exit;
  end;

  while not IsLineEnd(Run) do
  begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = '-') and (fLine[Run - 2] = '-') then
    begin
      fRange := rsText;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.BraceOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '!') and (fLine[Run + 1] = '-') and (fLine[Run + 2] = '-') then
  begin
    fRange := rsComment;
    fTokenID := tkComment;
    Inc(Run, 3);
  end
  else
  begin
    fRange := rsKey;
    fTokenID := tkSymbol;
  end;
end;

procedure TSynHTMLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynHTMLSyn.EqualProc;
begin
  fRange := rsValue;
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynHTMLSyn.IdentProc;
begin
  case fRange of
  rsKey:
    begin
      fRange := rsParam;
      fTokenID := IdentKind((fLine + Run));
      Inc(Run, fStringLen);
    end;
  rsValue:
    begin
      fRange := rsParam;
      fTokenID := tkValue;
      repeat
        Inc(Run);
      until (fLine[Run] <= #32) or (fLine[Run] = '>');
    end;
  else
    fTokenID := tkIdentifier;
    repeat
      Inc(Run);
    until (fLine[Run] <= #32) or (fLine[Run] = '=') or (fLine[Run] = '"') or
      (fLine[Run] = '>'); 
  end;
end;

procedure TSynHTMLSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynHTMLSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynHTMLSyn.TextProc;

  function IsStopChar: Boolean;
  begin
    case fLine[Run] of
      #0..#31, '<', '&':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  i: Integer;
begin
  if fLine[Run] in ([WideChar(#0)..WideChar(#31), WideChar('<')]) then
  begin
    NextProcedure;
    exit;
  end;

  fTokenID := tkText;

  while True do
  begin
    while not IsStopChar do Inc(Run);

    if (fLine[Run] = '&') then
    begin
      if (fLine[Run + 1] = '#') then
      begin
        fAndCode := -1;
        i := Run;
        inc(Run, 2);
        if fLine[Run] in [WideChar('X'), WideChar('x')] then
        begin
          inc(Run);
          while IsNumberChar do
            inc(Run);
        end
        else
          while (fLine[Run] in [WideChar('0')..WideChar('9')]) do
            inc(Run);
        if (fLine[Run] = ';') then
        begin
          inc(Run);
          Run := i;
          fRange := rsAmpersand;
        end;
        break;
      end
      else
        for i := Low(EscapeAmps) To High(EscapeAmps) do
          if (StrLCompW((fLine + Run), EscapeAmps[i], StrLenW(EscapeAmps[i])) = 0) then
          begin
            fAndCode := i;
            fRange := rsAmpersand;
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
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if fRange <> rsAmpersand then
  begin
    if fRange = rsKey then
    begin
      Inc(Run);
      fRange := rsText;
      fTokenID := tkText;
    end
    else
      IdentProc;
    Exit;
  end;
  
  case fAndCode of
  Low(EscapeAmps)..High(EscapeAmps):
    begin
      fTokenID := tkAmpersand;
      Inc(Run, StrLenW(EscapeAmps[fAndCode]));
    end;
    else begin
      if (fLine[Run + 1] = '#') then
      begin
        fAndCode := -1;
        inc(Run, 2);
        if fLine[Run] in [WideChar('X'), WideChar('x')] then
        begin
          inc(Run);
          while IsNumberChar do
            inc(Run);
        end
        else
          while (fLine[Run] in [WideChar('0')..WideChar('9')]) do
            inc(Run);
        if (fLine[Run] = ';') then begin
          inc(Run);
          fTokenID := tkAmpersand;
        end else
          fTokenID := tkText;
      end;
    end;
  end;
  fAndCode := -1;
  fRange := rsText;
end;

procedure TSynHTMLSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while fLine[Run] <= #32 do
  begin
    if fLine[Run] in [WideChar(#0), WideChar(#9), WideChar(#10), WideChar(#13)] then break;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.StringProc;
var
  iOpenChar: WideChar;
begin
  case fRange of
    rsQuoteValue:
      begin
        iOpenChar := #39;
        fTokenID := tkValue;
      end;
    rsDoubleQuoteValue:
      begin
        iOpenChar := '"';
        fTokenID := tkValue;
      end;
    else
    begin
      iOpenChar := fLine[Run];
      if fRange = rsValue then
      begin
        if iOpenChar = '"' then
          fRange := rsDoubleQuoteValue
        else
          fRange := rsQuoteValue;
        fTokenID := tkValue;
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
    if fLine[Run] = iOpenChar then
    begin
      Inc(Run);  { jumps over the closing char }
      if fRange in [rsDoubleQuoteValue, rsQuoteValue] then
        fRange := rsParam
      else
        fRange := rsText;
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
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
  case fLine[Run] of
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

function TSynHTMLSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynHTMLSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynHTMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynHTMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkAmpersand: Result := fAndAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSpace: Result := fSpaceAttri;
    tkSymbol: Result := fSymbolAttri;
    tkText: Result := fTextAttri;
    tkUndefKey: Result := fUndefKeyAttri;
    tkValue: Result := fValueAttri;
    else Result := nil;
  end;
end;

function TSynHTMLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynHTMLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynHTMLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynHTMLSyn.ResetRange;
begin
  fRange:= rsText;
end;

function TSynHTMLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterHTML;
end;

class function TSynHTMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangHTML;
end;

function TSynHTMLSyn.GetSampleSource: WideString;
begin
  Result := '<!-- Syntax highlighting -->'#13#10 +
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

class function TSynHTMLSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangHTML;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynHTMLSyn);
{$ENDIF}
end.
