{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterMPerl.pas, released 2000-12-16.
The Original Code is based on ideas of the the mwEdit component suite by
Martin Waldenburg and other developers. CRC32 codes processing is based
on works of Earl F. Glynn, Overland Park, KS.
Initial Author of SynHighlighterMPerl.pas is Murad Kakabayev.

Portions created by Willo van der Merwe are Copyright 1999 Willo van der Merwe.
Portions created by Earl F. Glynn are Copyright 1989, 1995-1996 Earl F. Glynn.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above. If you wish
to allow use of your version of this file only under the terms of the GPL and
not to allow others to use your version of this file under the MPL, indicate
your decision by deleting the provisions above and replace them with the
notice and other provisions required by the GPL. If you do not delete the
provisions above, a recipient may use your version of this file under either
the MPL or the GPL.

$Id: SynHighlighterMPerl.pas,v 1.8 2002/04/09 09:58:51 plpolak Exp $

If this file will be included into SynEdit project, you may retrieve the
latest version of this file at the SynEdit home page, located at
http://SynEdit.SourceForge.net  otherwise you can request latest version
by e-mail to murad_kakabayev@mail.ru;

Known Issues:
  From Murad Kakabayev:
  - SynHighlighterMPerl supports nested braces () <>  [] in quoted and RE
    operators up to 63 nested levels. If you want more nested levels - you're
    advanced Perl hacker (Larry Wall?) and you need not any highlighters in your
    favorite vi editor! :-). Seriously, I consider that 63 levels - it is enough.
    Increasing of levels will decrease reliability of other parsing methods
  - Spaces between braces in s()() and tr<><> marked with UnknownAttributes.
  - Highlighter currently compiled & tested with Delphi3,Delphi5,CBuilder4. I can't
    make test with other Borland Compilers. If we find any compilation errors-
    please email me about it
  - Not parsed EMPTY HEREDOC without ';' because without Perl interpreter
    impossible recognize constructions like
         = 3343 <<23;
    Now it recognized as HEREDOC;
    if you need _shift left_ operation, you must separate "<<" and next token
    by spaces :
         = 3343 << 23;
    if you need EMPTY HEREDOC, you must use ';' or empty quoted string after << :
        print <<;
      or
         = print (<<'',12345);
  - There're some troubles with recognizing slash '/'. Is it begin of pattern
    or just division? I use some strange algorithm, it works in most cases,
    but do not wait magic from highlighter ;-)
  - options poParseWithXSwitch ,poParseAfter__END__ on changing require some
    method or event to reparse all edited text from beginning, so now without
    manual changing begin of edited text these options are not especially useful.
    In next releases of SynEdit's suite, TSynHighlighter will contain this
    extremely useful method, I hope.
  - And last. My English,as you could notice, is terrible :-(. I would be very
    glad, if somebody has found time properly to document this unit and fix
    my grammar errors.

Todo:
  From Murad Kakabayev:
  - Maybe, I add some formatting symbols to highlight Perl source code inside
    POD. It is useful then documentaion containes example source code.
    Example :
    =head1
    COOL : very cool module from Vasya Pupkin!
    Syntax :
    (formating symbol here, for example = '+' on beginning of line)
    use COOL qw (bla-bla-bla);
        COOL::coolest($a,$b,$c)
    +
    ........
    =cut

New features:
  V1.1
  - parsing formats. Comments inside formats also highlighted with
    CommentAttributes.
  - nested brackes in reqular expressions and quoted operators
    See "Known Issues" above.
  - '      HEREDOC' identifiers.
    See "Known Issues" above.
  - Slightly optimized.
  - option poParseAfter__END__ is useful then your script contains Perl
    code and __END__ (__DATA__) directive, for example, most CPAN modules
    contains POD documantation after __END__
  - option poParseWithXSwitch - If you running Perl with "-x" swith, Perl
    skip all garbage before string "#!....perl", so highlighter will do
    the same. All marked with the EndOfFileAttributes.
    See "Known Issues" above.
  V1.1.1
  - Fixed bug with q() parsing
  - Checked under Borland CBuilder4
-------------------------------------------------------------------------------}
{
@abstract(Another Perl highlighter for SynEdit)
@author  (Murad Kakabayev <murad_kakabayev@mail.ru>)
@created (2000-12-16)
@lastmod (Fri Dec 22 10:41:31 2000)

- Version 1.1.1

- The SynHighlighterMPerl implements a highlighter for Perl for the SynEdit projects.

- SynHighlighterMPerl.pas was generated by <makemperl.v1.1.1.pl> script.
}

unit SynHighlighterMPerl;

interface

{$I SynEdit.inc}

uses
  SysUtils, Classes,
  {$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  {$ELSE}
  Windows, Messages, Controls, Graphics,
  {$ENDIF}
  SynEditTypes, SynEditHighlighter;

type
  TTokenKind = (tkNull,tkUnknown,tkSpace,tkComment,tkEND,tkPredefinedVar,tkPod,
                tkString,tkSingleString,tkKeyword,tkPragma,tkInternalFunction,
                tkSymbol,tkVariable,tkSpecSub,tkOperator,tkDecNumber,tkOctNumber,
                tkHexNumber,tkBackQuote,tkHeredoc,tkRe,tkFormatBody,tkFormatHeader
                );

  TPredefinedRangeStates = (
                rsUnknown,rsM,rsMBracket,rsMSlash,rsQuoted,rsQuotedBracket,
                rsQr,rsQrBracket,rsS_BetweenBrackets,rsS_First,rsS_FirstBracket,
                rsS_Second,rsS_SecondBracket,rsTr_BetweenBrackets,rsTr_First,
                rsTr_FirstBracket,rsTr_Second,rsTr_SecondBracket,rsEndOfData,
                rsPod,rsString,rsSingleString,rsBackQuote,rsEmptyHeredoc,
                rsBeforeScript,rsFormatBody);


  TRangeState = record
    case boolean of
       true  : (P: pointer);
       false : (Filler: byte; C: char; Range: byte; Step: byte);
    end;

  TBracketKind = (bkSquareOpen,bkCurlyOpen,bkRoundOpen,bkAngleOpen,
                  bkSquareClose,bkCurlyClose,bkRoundClose,bkAngleClose,bkUnknown);
  TBracketType = (bOpen,bClose,bUnknown);
  
  TProcTableProc = procedure of object;

  TSynMPerlSynOption  = (poParseAfter__END__,poParseWithXSwitch);
  TSynMPerlSynOptions = set of TSynMPerlSynOption;

  TSynMPerlSyn = class(TSynCustomHighlighter)
  private
    FCurrentRange      : TRangeState;
    FOptions           : TSynMPerlSynOptions;

    FSkipHeredocBeforeEOL : boolean;

    fToIdent           : PChar;
    FCurrentLine       : PChar;
    FCurrentLineOffset : longint;
    FStringLen         : integer;
    FTokenID           : TTokenKind;
    FTokenPos          : integer;
    FProcTable         : array [#0..#255] of TProcTableProc;

    FSpecialSUBAttributes       : TSynHighlighterAttributes;
    FUnknownAttributes          : TSynHighlighterAttributes;
    FCommentAttributes          : TSynHighlighterAttributes;
    FPodAttributes              : TSynHighlighterAttributes;
    FVariableAttributes         : TSynHighlighterAttributes;
    FQuoteAttributes            : TSynHighlighterAttributes;
    FOctNumberAttributes        : TSynHighlighterAttributes;
    FPragmaAttributes           : TSynHighlighterAttributes;
    FFormatAttributes           : TSynHighlighterAttributes;
    FDecNumberAttributes        : TSynHighlighterAttributes;
    FOperatorAttributes         : TSynHighlighterAttributes;
    FSymbolAttributes           : TSynHighlighterAttributes;
    FInternalFunctionAttributes : TSynHighlighterAttributes;
    FKeywordAttributes          : TSynHighlighterAttributes;
    FREAttributes               : TSynHighlighterAttributes;
    FHexNumberAttributes        : TSynHighlighterAttributes;
    FEndOfDataAttributes        : TSynHighlighterAttributes;
    FSpaceAttributes            : TSynHighlighterAttributes;
    FHEREDOCAttributes          : TSynHighlighterAttributes;
    FPredefinedVarAttributes    : TSynHighlighterAttributes;

    procedure MakeMethodTables;
    procedure UnknownProc;
    procedure SymbolProc;
    procedure NullProc;
    procedure LFProc;
    procedure CRProc;
    procedure SpaceProc;
    procedure CommentProc;
    procedure ProcUnderscore;
    procedure EqualProc;
    procedure ToIdent(Index : integer);
    procedure EndOfDataProc;
    procedure BeforeScriptProc;
    procedure PodProc;
    procedure LessProc;
    procedure HeredocProc(IsEmptyHeredoc : boolean);
    procedure QuoteProc;
    procedure BackQuoteProc;
    procedure SingleQuoteProc;
    procedure GenQuoteProc;
    procedure FromLetterProc;
    procedure VarProc;
    procedure ArrayProc;
    procedure HashProc;
    procedure NumberProc;
    procedure MinusProc;
    procedure QProc;
    procedure SlashProc;
    procedure MProc(Step : integer);
    procedure S_BetweenBrackets;
    procedure Tr_BetweenBrackets;
    procedure QWProc( Step : integer);
    function  CheckSharp(Offset : integer) : boolean;
    function  IsPreviousBackSlash(Offset : integer) : boolean;
    procedure FormatProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF} //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function  GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function  GetEol: Boolean; override;
    function  GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function  GetToken: String; override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  GetTokenKind: integer; override;
    function  GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetLine(NewValue: String; LineNumber:Integer); override;
  published
    property Options : TSynMPerlSynOptions read FOptions write FOptions;
    property SpecialSUBAttributes: TSynHighlighterAttributes read FSpecialSUBAttributes write FSpecialSUBAttributes;
    property UnknownAttributes: TSynHighlighterAttributes read FUnknownAttributes write FUnknownAttributes;
    property CommentAttributes: TSynHighlighterAttributes read FCommentAttributes write FCommentAttributes;
    property PodAttributes: TSynHighlighterAttributes read FPodAttributes write FPodAttributes;
    property VariableAttributes: TSynHighlighterAttributes read FVariableAttributes write FVariableAttributes;
    property QuoteAttributes: TSynHighlighterAttributes read FQuoteAttributes write FQuoteAttributes;
    property OctNumberAttributes: TSynHighlighterAttributes read FOctNumberAttributes write FOctNumberAttributes;
    property PragmaAttributes: TSynHighlighterAttributes read FPragmaAttributes write FPragmaAttributes;
    property FormatAttributes: TSynHighlighterAttributes read FFormatAttributes write FFormatAttributes;
    property DecNumberAttributes: TSynHighlighterAttributes read FDecNumberAttributes write FDecNumberAttributes;
    property OperatorAttributes: TSynHighlighterAttributes read FOperatorAttributes write FOperatorAttributes;
    property SymbolAttributes: TSynHighlighterAttributes read FSymbolAttributes write FSymbolAttributes;
    property InternalFunctionAttributes: TSynHighlighterAttributes read FInternalFunctionAttributes write FInternalFunctionAttributes;
    property KeywordAttributes: TSynHighlighterAttributes read FKeywordAttributes write FKeywordAttributes;
    property REAttributes: TSynHighlighterAttributes read FREAttributes write FREAttributes;
    property HexNumberAttributes: TSynHighlighterAttributes read FHexNumberAttributes write FHexNumberAttributes;
    property EndOfDataAttributes: TSynHighlighterAttributes read FEndOfDataAttributes write FEndOfDataAttributes;
    property SpaceAttributes: TSynHighlighterAttributes read FSpaceAttributes write FSpaceAttributes;
    property HEREDOCAttributes: TSynHighlighterAttributes read FHEREDOCAttributes write FHEREDOCAttributes;
    property PredefinedVarAttributes: TSynHighlighterAttributes read FPredefinedVarAttributes write FPredefinedVarAttributes;
  end;

implementation

uses  SynEditStrConst;

type  TSetChar = set of Char;
const IdentArray : array [2..6] of TSetChar = (
      TSynValidStringChars,
      [#0..#32,'!'..'/',':'..'?','['..'^','`','{'..'~'],
      ['0'..'9','_'],
      ['0'..'9','a'..'f','A'..'F','_'],
      ['0'..'9']
);



const
   CRCTable:  array[0..255] of
              {$IFDEF SYN_COMPILER_4_UP} cardinal {$ELSE} integer {$ENDIF} = (
   $00000000,$77073096,$EE0E612C,$990951BA,$076DC419,$706AF48F,$E963A535,$9E6495A3,
   $0EDB8832,$79DCB8A4,$E0D5E91E,$97D2D988,$09B64C2B,$7EB17CBD,$E7B82D07,$90BF1D91,
   $1DB71064,$6AB020F2,$F3B97148,$84BE41DE,$1ADAD47D,$6DDDE4EB,$F4D4B551,$83D385C7,
   $136C9856,$646BA8C0,$FD62F97A,$8A65C9EC,$14015C4F,$63066CD9,$FA0F3D63,$8D080DF5,
   $3B6E20C8,$4C69105E,$D56041E4,$A2677172,$3C03E4D1,$4B04D447,$D20D85FD,$A50AB56B,
   $35B5A8FA,$42B2986C,$DBBBC9D6,$ACBCF940,$32D86CE3,$45DF5C75,$DCD60DCF,$ABD13D59,
   $26D930AC,$51DE003A,$C8D75180,$BFD06116,$21B4F4B5,$56B3C423,$CFBA9599,$B8BDA50F,
   $2802B89E,$5F058808,$C60CD9B2,$B10BE924,$2F6F7C87,$58684C11,$C1611DAB,$B6662D3D,
   $76DC4190,$01DB7106,$98D220BC,$EFD5102A,$71B18589,$06B6B51F,$9FBFE4A5,$E8B8D433,
   $7807C9A2,$0F00F934,$9609A88E,$E10E9818,$7F6A0DBB,$086D3D2D,$91646C97,$E6635C01,
   $6B6B51F4,$1C6C6162,$856530D8,$F262004E,$6C0695ED,$1B01A57B,$8208F4C1,$F50FC457,
   $65B0D9C6,$12B7E950,$8BBEB8EA,$FCB9887C,$62DD1DDF,$15DA2D49,$8CD37CF3,$FBD44C65,
   $4DB26158,$3AB551CE,$A3BC0074,$D4BB30E2,$4ADFA541,$3DD895D7,$A4D1C46D,$D3D6F4FB,
   $4369E96A,$346ED9FC,$AD678846,$DA60B8D0,$44042D73,$33031DE5,$AA0A4C5F,$DD0D7CC9,
   $5005713C,$270241AA,$BE0B1010,$C90C2086,$5768B525,$206F85B3,$B966D409,$CE61E49F,
   $5EDEF90E,$29D9C998,$B0D09822,$C7D7A8B4,$59B33D17,$2EB40D81,$B7BD5C3B,$C0BA6CAD,
   $EDB88320,$9ABFB3B6,$03B6E20C,$74B1D29A,$EAD54739,$9DD277AF,$04DB2615,$73DC1683,
   $E3630B12,$94643B84,$0D6D6A3E,$7A6A5AA8,$E40ECF0B,$9309FF9D,$0A00AE27,$7D079EB1,
   $F00F9344,$8708A3D2,$1E01F268,$6906C2FE,$F762575D,$806567CB,$196C3671,$6E6B06E7,
   $FED41B76,$89D32BE0,$10DA7A5A,$67DD4ACC,$F9B9DF6F,$8EBEEFF9,$17B7BE43,$60B08ED5,
   $D6D6A3E8,$A1D1937E,$38D8C2C4,$4FDFF252,$D1BB67F1,$A6BC5767,$3FB506DD,$48B2364B,
   $D80D2BDA,$AF0A1B4C,$36034AF6,$41047A60,$DF60EFC3,$A867DF55,$316E8EEF,$4669BE79,
   $CB61B38C,$BC66831A,$256FD2A0,$5268E236,$CC0C7795,$BB0B4703,$220216B9,$5505262F,
   $C5BA3BBE,$B2BD0B28,$2BB45A92,$5CB36A04,$C2D7FFA7,$B5D0CF31,$2CD99E8B,$5BDEAE1D,
   $9B64C2B0,$EC63F226,$756AA39C,$026D930A,$9C0906A9,$EB0E363F,$72076785,$05005713,
   $95BF4A82,$E2B87A14,$7BB12BAE,$0CB61B38,$92D28E9B,$E5D5BE0D,$7CDCEFB7,$0BDBDF21,
   $86D3D2D4,$F1D4E242,$68DDB3F8,$1FDA836E,$81BE16CD,$F6B9265B,$6FB077E1,$18B74777,
   $88085AE6,$FF0F6A70,$66063BCA,$11010B5C,$8F659EFF,$F862AE69,$616BFFD3,$166CCF45,
   $A00AE278,$D70DD2EE,$4E048354,$3903B3C2,$A7672661,$D06016F7,$4969474D,$3E6E77DB,
   $AED16A4A,$D9D65ADC,$40DF0B66,$37D83BF0,$A9BCAE53,$DEBB9EC5,$47B2CF7F,$30B5FFE9,
   $BDBDF21C,$CABAC28A,$53B39330,$24B4A3A6,$BAD03605,$CDD70693,$54DE5729,$23D967BF,
   $B3667A2E,$C4614AB8,$5D681B02,$2A6F2B94,$B40BBE37,$C30C8EA1,$5A05DF1B,$2D02EF8D);


   KeyIdentIndex : array[0..255] of integer = (
     131072,65538,65539,65540,0,0,131077,0,131079,65545,0,0,65546,65547,131084,
     65550,196623,65554,0,0,65555,0,65556,196629,0,131096,65562,0,65563,131100,
     65566,0,131103,0,65569,0,65570,65571,131108,65574,131111,131113,0,0,131115,
     0,0,0,196653,65584,65585,0,131122,65588,0,65589,131126,131128,131130,0,65596,
     65597,65598,65599,131136,65602,0,65603,0,0,131140,65606,196679,65610,0,131147,
     131149,65615,65616,0,65617,0,0,196690,65621,0,0,0,65622,131159,65625,0,131162,
     131164,65630,0,0,65631,0,196704,131171,65637,196710,131177,65643,65644,65645,
     131182,65648,65649,0,131186,196724,65655,131192,131194,131196,0,131198,131200,
     131202,65668,65669,65670,0,131207,131209,131211,0,0,0,65677,0,65678,196751,
     131218,0,65684,0,131221,0,0,65687,196760,0,65691,0,65692,131229,131231,131233,
     65699,0,0,196772,65703,131240,0,65706,0,0,0,65707,131244,65710,65711,65712,
     196785,65716,131253,0,0,65719,0,196792,0,131259,0,131261,65727,65728,131265,
     65731,0,0,196804,65735,65736,131273,0,65739,0,0,65740,0,0,0,65741,131278,
     131280,131282,196820,131287,65753,0,0,65754,131291,65757,65758,0,131295,
     65761,65762,0,65763,131300,0,196838,131305,131307,131309,131311,65777,0,
     131314,65780,65781,196854,0,65785,0,0,131322,0,196860,0,0,0,131327,131329,
     0,65795,65796,131333,65799,65800,65801,65802,131339,131341,0,65807,0,0,65808
     );

   KeyIdentData : array[0..272] of integer = (
     $90FC74{hex},$BB3CD6{rindex},$FC203D{stat},$E24C97{accept},$5DC713{exists},
     $F6A2E0{while},$8C5E90{xor},$FE6E19{warn},$A8BACE{eof},$2507CB{join},$2A1972{msgrcv},
     $F899BE{CHECK},$DB2F72{setsockopt},$10D35D{endhostent},$B43F73{rmdir},$EE1483{if},
     $34D0EA{for},$92673F{chroot},$006FEE{bless},$5D3228{ord},$5C9EAC{return},
     $E846ED{link},$525FEE{semget},$6468B7{ge},$4ED7FC{undef},$B4ED18{STORESIZE},
     $310628{FETCHSIZE},$B0026A{closedir},$F31449{lc},$35F2A7{VERSION},$6C2C99{strict},
     $5D6054{study},$9E0EE0{getsockopt},$77B469{CLOSE},$A049CB{PRINT},$6BCF20{next},
     $733AE0{no},$6D8975{kill},$0D37D8{tell},$C61B24{telldir},$469C1D{STDERR},
     $C199D0{dbmclose},$54E216{getservbyname},$8C6FBE{import},$79FEEC{setnetent},
     $D46A3A{untie},$090D52{waitpid},$F322C9{getpwuid},$9A8A07{splice},$FCA0D8{getpeername},
     $E6C1DC{getservbyport},$B8EF13{gethostbyaddr},$04D63A{socketpair},$1B4E69{reverse},
     $0DDBE9{truncate},$010320{gethostbyname},$DA53A9{charnames},$822A68{rand},
     $B3DF88{atan2},$83CB47{gmtime},$D3FA86{crypt},$D75606{endpwent},$DEBC12{pipe},
     $88DE48{eq},$E5C309{semop},$7668D6{getprotoent},$90A7D5{GETC},$04B8EB{package},
     $A29F56{sigtrap},$51901C{index},$7F548B{getprotobynumber},$824AB8{sin},$547728{each},
     $EB24CC{PUSH},$4108AD{quotemeta},$FF7C40{msgsnd},$F3A0B8{BEGIN},$3A6891{endprotoent},
     $1D4FF2{WRITE},$F47FF5{local},$D04759{time},$EE4C74{semctl},$B6B3B6{redo},
     $1082EB{fileno},$9E9A26{scalar},$46FB08{getnetbyaddr},$FF173B{getnetbyname},
     $1A4141{overload},$BE84C6{chr},$33015C{oct},$AA5CF9{wait},$6E6778{or},$3EBA71{lstat},
     $3BD934{endservent},$59DF9F{setprotoent},$F82297{cos},$D59852{printf},$29CC58{shift},
     $CC836B{setgrent},$666446{eval},$639B32{ucfirst},$EA7173{chdir},$7B720C{elsif},
     $28AE39{dump},$D8A344{sqrt},$882C4F{bind},$8010F2{defined},$5FF5ED{rename},
     $1AA829{END},$0513B3{unlink},$9F76BC{last},$F01559{use},$EC73A9{map},$D0F1A6{syswrite},
     $197393{umask},$78D37C{format},$7EB1D7{log},$602D3E{sort},$362C8E{POP},$D1BDFC{getpgrp},
     $C96F20{autouse},$FFADAB{fcntl},$24839C{goto},$E6C65E{listen},$FF8A97{else},
     $9BE754{getgrnam},$C1F5B6{continue},$32C240{readdir},$054A61{getc},$D76D0C{split},
     $9C8DB1{recv},$8FB028{system},$A18B54{isa},$832F09{EXTEND},$281AA2{DELETE},
     $A67F62{seekdir},$D44845{gt},$7EC978{push},$23071A{getpwent},$983FBE{pos},
     $E228E6{utime},$F32A39{STDOUT},$345CB8{open},$F3BD51{uc},$37E5D1{send},$D238CB{setpwent},
     $F0B077{srand},$E763AE{symlink},$65876D{setservent},$D0DD24{delete},$65ED45{values},
     $4455B3{substr},$1F1E9D{unless},$09BE30{readlink},$8673CF{READ},$855CF4{getpwnam},
     $2D5FD4{PRINTF},$A29D57{new},$7D0EE8{SHIFT},$CA9BF0{keys},$A50537{shmwrite},
     $BF4850{reset},$25422F{lcfirst},$C6FFC1{rewinddir},$F598F1{Exporter},$2B91D9{exec},
     $AF8766{fork},$3DBCBA{getgrent},$4DBBE2{vec},$7D63C0{can},$20918E{lt},$15FC82{syscall},
     $5E52C5{binmode},$AAEE9B{getsockname},$976FFC{wantarray},$B8D56D{ops},$E58AE2{glob},
     $435BCE{sub},$20A6FF{getservent},$8D4FE3{FIRSTKEY},$7E88C5{shmctl},$FFFD02{chown},
     $B71129{TIEHASH},$39CEC2{TIESCALAR},$480AB1{constant},$93BE43{msgget},$820F7B{READLINE},
     $B9C6FF{flock},$BF3F11{length},$BDB1C0{alarm},$A11691{ioctl},$9F56EE{FETCH},
     $27C918{socket},$4D5B12{exit},$778C6A{subs},$9A813F{pack},$139E7B{read},
     $0B9D5A{grep},$1DEAC2{opendir},$22C1CB{DATA},$498D42{write},$8ED604{caller},
     $62E1EC{formline},$5552E4{sprintf},$624D81{SPLICE},$2E61A2{vmsish},$3926C5{die},
     $A6D3FE{ne},$637475{integer},$61DD56{blib},$B5582B{chmod},$6F60FC{mkdir},
     $1135B1{sleep},$B9919F{getprotobyname},$17458F{endnetent},$4FEE77{getgrgid},
     $C711AF{cmp},$77D461{unpack},$216DDA{STORE},$27422B{fields},$108D53{TIEARRAY},
     $C2A3E4{UNSHIFT},$A5D6DC{INIT},$4465A5{vars},$F48B7B{print},$C7C4AB{CLEAR},
     $2376D9{close},$D288B4{abs},$C9EDA6{endgrent},$789735{localtime},$8200D7{foreach},
     $1F85B5{tie},$4E8D04{sethostent},$F51991{times},$5417EE{getppid},$9CD26A{do},
     $4AB836{require},$37C421{chop},$55ED91{getnetent},$5329E3{dbmopen},$63A71F{DESTROY},
     $E5D59E{TIEHANDLE},$F0217D{base},$966A06{shmread},$400521{setpgrp},$DD3ABA{chomp},
     $1739EB{diagnostics},$EADD18{getpriority},$DBB935{NEXTKEY},$916E04{pop},
     $A32661{connect},$2FADD9{msgctl},$2EE7B1{ref},$1003A3{int},$B157AF{AUTOLOAD},
     $875DFE{unshift},$C29B5F{shmget},$42673B{locale},$8001AB{shutdown},$F24BC3{less},
     $A50095{EXISTS},$0BAC96{gethostent},$90B17C{le},$7C218B{seek},$CC92DB{sysread},
     $442467{not},$B4427F{and},$8ADC72{my},$FF8B09{exp},$304B63{select},$C56A51{setpriority},
     $909029{STDIN},$39B93A{getlogin});

   KeyIdentTypes : array[0..272] of byte = (
     11,11,11,11,11,9,15,9,11,11,11,14,11,11,11,9,9,11,11,11,9,11,11,15,11,14,
     14,11,11,14,10,11,11,14,14,9,10,11,11,11,5,11,11,10,11,11,11,11,11,11,11,
     11,11,11,11,11,10,11,11,11,11,11,11,15,11,11,14,10,10,11,11,11,11,14,11,
     11,14,11,14,9,11,11,9,11,11,11,11,10,11,11,11,15,11,11,11,11,11,11,11,9,
     11,11,9,11,11,11,11,11,14,11,9,10,11,11,11,23,11,11,14,11,10,11,9,11,9,11,
     9,11,11,11,11,11,14,14,14,11,15,11,11,11,11,5,11,11,11,11,11,11,11,11,11,
     11,9,11,14,11,14,14,14,11,11,11,11,11,10,11,11,11,11,14,15,11,11,11,11,10,
     11,9,11,14,11,11,14,14,10,11,14,11,11,11,11,14,11,9,10,11,11,11,11,5,11,
     11,11,11,14,10,9,15,10,10,11,11,11,11,11,11,15,11,14,10,14,14,14,10,11,14,
     11,11,11,11,9,11,11,11,11,9,10,11,11,11,14,14,10,11,11,11,10,11,14,11,11,
     11,11,11,14,11,11,10,11,10,14,11,15,11,11,15,15,9,11,11,11,5,11);

   KeyVarIndex : array[0..255] of integer = (
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65536,0,0,0,0,65537,0,0,0,0,0,0,0,0,0,
     0,0,0,65538,0,65539,0,0,0,0,0,0,0,0,0,65540,0,65541,0,0,0,0,0,0,0,65542,
     65543,0,65544,0,65545,65546,0,0,0,65547,0,0,0,0,0,65548,0,131085,65551,0,
     0,0,0,0,0,65552,0,0,0,0,0,65553,0,0,0,0,0,65554,0,0,0,0,0,0,0,65555,0,0,
     0,0,0,0,0,0,0,65556,0,65557,0,0,65558,0,65559,0,65560,0,65561,0,0,0,0,0,
     0,0,0,0,0,0,65562,65563,65564,0,0,0,65565,131102,0,0,0,0,0,0,65568,0,0,0,
     65569,0,0,0,65570,0,0,0,0,0,65571,0,0,0,0,0,0,0,0,0,131108,65574,65575,0,
     0,0,0,0,0,0,0,0,65576,0,0,0,0,65577,0,0,0,65578,0,0,65579,0,65580,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65581,0,0,0,0,131118,0,0,0,0,0,0,0,0,131120,
     0,0,0,65586,0,0,0,0,0,65587,0,0,0,0,65588,0,0,0,0,0);

   KeyVarData : array[0..52] of integer = (
     $6D0C12{ARG},$B6D444{ERRNO},$A9774B{FORMAT_LINE_BREAK_CHARACTERS},$DFEA80{GID},
     $872230{OFS},$73676B{MULTILINE_MATCHING},$A211D5{FORMAT_NAME},$7D9D9E{UID},
     $B65F75{PID},$B94203{EXECUTABLE_NAME},$B14F78{INPUT_RECORD_SEPARATOR},$D82E87{COMPILING},
     $055B4F{EXTENDED_OS_ERROR},$2DF0A8{MATCH},$E2FE60{SUBSCRIPT_SEPARATOR},$E0D854{POSTMATCH},
     $066B51{INPUT_LINE_NUMBER},$CC1299{EFFECTIVE_GROUP_ID},$E0B8E5{FORMAT_LINES_PER_PAGE},
     $81E2E6{OUTPUT_FIELD_SEPARATOR},$A0E885{ARGV},$F31095{OS_ERROR},$2B1808{OUTPUT_AUTOFLUSH},
     $CD87BC{DEBUGGING},$EF3F98{RS},$E38B7A{PROGRAM_NAME},$DEC6FE{PERLDB},$15428F{EGID},
     $BC6866{OUTPUT_RECORD_SEPARATOR},$341A98{EVAL_ERROR},$8DB8C8{REAL_USER_ID},
     $105A0A{PERL_VERSION},$B73591{EUID},$B531FE{REAL_GROUP_ID},$B11DBC{OFMT},
     $BD1F02{SYSTEM_FD_MAX},$9AC6DD{PROCESS_ID},$F452F9{FORMAT_TOP_NAME},$F0C1C9{LIST_SEPARATOR},
     $34A316{LAST_PAREN_MATCH},$E13BEB{FORMAT_FORMFEED},$E6BE7A{FORMAT_LINES_LEFT},
     $6BD009{SUBSEP},$975A7F{ACCUMULATOR},$4F410C{OSNAME},$2C037D{FORMAT_PAGE_NUMBER},
     $5F9BC1{INPLACE_EDITORS},$67BDA2{WARNING},$9F5253{NR},$88BCD0{BASETIME},
     $F338D0{CHILD_ERROR},$D4602F{PREMATCH},$207E35{EFFECTIVE_USER_ID});

{ For example, we must recognize internal function "print"
  1. Calculate CRC32 : CRC32('print') = $D3F48B7B
  2. Split CRC value into Index:D3 and Value: F48B7B
  3. return tkSymbol if KeyIdentIndex[D3] in null, else -
  4. Decode KeyIdentIndex[D3] value - it is 4-byte integer, first 2 bytes is
     Count of all precalculated CRC32 values, which started from D3 and second
     2 bytes is Offset in KeyIdentData array, where we can find these values
  5. Locate our Value(F48B7B) in KeyIdentData array from Offset next Count
     array elements
  6. If search was successful, return TokenKind of located Value from
     KeyIdentTypes array.                                                    }
procedure SplitCRC(CRC : integer; var Index : Byte; var Value : integer);
begin
    Index  := CRC shr 24;
    Value  := CRC and $FFFFFF;
end;

procedure SplitIntoOffsetAndLen(A : integer; var Offset, Len : Word);
begin
    Offset := A and $ffff;
    Len    := A shr 16;
end;

function CheckIdentCRC(CRC : integer) : TTokenKind;
var Index  : Byte;
    Value  : integer;
    Len,Offset : word;
    I      : integer ;
begin
    result := tkSymbol;
    SplitCRC(CRC,Index,Value);
    if KeyIdentIndex[Index] = 0 then exit;
    SplitIntoOffsetAndLen(KeyIdentIndex[Index],Offset,Len);

    for i:= Offset to Offset+Len-1 do
       if KeyIdentData[i] = Value then begin
          result :=  TTokenKind(KeyIdentTypes[i]);
          exit;
       end;
end;

function CheckVariableCRC(CRC : integer) : TTokenKind;
var Index  : Byte;
    Value  : integer;
    Len,Offset : word;
    I      : integer ;
begin
    result := tkVariable;
    SplitCRC(CRC,Index,Value);
    if KeyVarIndex[Index] = 0 then exit;
    SplitIntoOffsetAndLen(KeyVarIndex[Index],Offset,Len);

    for i:= Offset to Offset+Len-1 do
       if KeyVarData[i] = Value then begin
          result :=  tkPredefinedVar;
          exit;
       end;
end;

type TVarLetterType = (vltCaret,vltSpace,vltPredefined,vltUnderscore,
                       vltSharp,vltOther);
     TGenLetterType = (gltIdent,gltCRLFNULL,gltSpaces,gltOther);

var VarIdentArray : array [#0..#255] of TVarLetterType;
    GenIdentArray : array [#0..#255] of TGenLetterType;

procedure BuildIdentArray;
var C : Char;
begin
   for c:=#0 to #255 do begin
      case C of
        #0..#32 : VarIdentArray[c] := vltSpace;
        '^'     : VarIdentArray[c] := vltCaret;
       #48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#38,#39,#96,#34,#93,#91,#43,#42,#46,#47,#124,#44,#92,#59,#37,#61,#45,#126,#58,#63,#33,#64,#36,#60,#62,#40,#41
                : VarIdentArray[c] := vltPredefined;
        '_'     : VarIdentArray[c] := vltUnderscore;
        '#'     : VarIdentArray[c] := vltSharp;
       else  VarIdentArray[c] := vltOther;
      end;
      case C of
        #0,#13,#10              : GenIdentArray[c] := gltCRLFNULL;
        #1..#9,#11,#12,#14..#32 : GenIdentArray[c] := gltSpaces;
        'a'..'z','_','0'..'9',
        'A'..'Z'                : GenIdentArray[c] := gltIdent;
        else  GenIdentArray[c] := gltOther;
      end;
   end;
end;

procedure ResetRangeState(var RangeState : TRangeState);
begin
    RangeState.P := nil;
end;

function BuildSimpleRange(Range: TPredefinedRangeStates): TRangeState;
begin
   ResetRangeState(result);
   Result.Range := Byte(Range);
end;

function BuildDelimiterRange(C: char; Range: Byte; Step: byte): TRangeState;
begin
  ResetRangeState(result);
  Result.C := C;
  Result.Range := Range;
  Result.Step := Step;
end;

procedure IncRangeStep(var RangeState : TRangeState);
begin
    RangeState.Step := RangeState.Step + 1;
end;

function BuildRangePointer(CRC, Len : cardinal) : Pointer;
begin
    result := Pointer (( CRC and $FFFFFF00) + ( Len and $FF ));
end;

function ExtractStringLenFromHash(P : Pointer) : integer;
begin
    Result := Cardinal(P) and $FF;
end;

type TRangeStateKind = (rskHeredoc,rskDelimiter,rskSimple);

function RecognizeRange(RangeState : TRangeState) : TRangeStateKind;
begin
    with RangeState do begin
        if (Filler <> 0) or (Range > 31) or (Step > 3) then result := rskHeredoc else
           if (C <> #0) then result := rskDelimiter else
               if Step = 0 then result := rskSimple else
                  case TPredefinedRangeStates(Range) of
                     rsMBracket,
                     rsQuotedBracket,
                     rsS_FirstBracket,
                     rsS_SecondBracket,
                     rsS_BetweenBrackets,
                     rsTr_FirstBracket,
                     rsTr_SecondBracket,
                     rsTr_BetweenBrackets : result := rskDelimiter
                     else result :=  rskHeredoc;
                  end;
    end
end;

function IsOpenBracket(C : Char) : boolean;
begin
   result := C in ['<','(','{','['];
end;

function RecognizeBracket(C : Char) : TBracketKind;
begin
   case C of
     '[' : result := bkSquareOpen;
     '{' : result := bkCurlyOpen;
     '(' : result := bkRoundOpen;
     '<' : result := bkAngleOpen;
     ']' : result := bkSquareClose;
     '}' : result := bkCurlyClose;
     ')' : result := bkRoundClose;
     '>' : result := bkAngleClose;
      else result := bkUnknown;
   end;
end;

function BuildBracketRange(C: char; Range: Byte; Step: byte): TRangeState;
begin
  ResetRangeState(result);
  Result.C := Char(RecognizeBracket(C));
  Result.Range := Range;
  Result.Step  := Step;
end;

procedure IncBracketCounter(var RangeState: TRangeState);
var Count,BracketKind : integer;
begin
   Count := Byte(RangeState.C) shr 2; // first 6 bits
   if Count = 63 then exit;           // only 63 nested levels,unfortunately
   BracketKind  := Byte(RangeState.C) and 3; // last 2 bits
   RangeState.C := Char((Count + 1) shl 2 or BracketKind);
end;

procedure DecBracketCounter(var RangeState: TRangeState);
var Count,BracketKind : integer;
begin
   Count := Byte(RangeState.C) shr 2;
   if Count = 0 then exit;
   BracketKind  := Byte(RangeState.C) and 3;
   RangeState.C :=  Char((Count-1) shl 2 or BracketKind);
end;

function IsBracketLevelsZero(RangeState: TRangeState) : boolean;
begin
    result := (Byte(RangeState.C) shr 2) = 0;
end;

function CheckForWantedBracket(C : Char; RangeState : TRangeState ) : TBracketType;
var BK,BK2 : TBracketKind;
begin
   result := bUnknown;
   BK := RecognizeBracket(c);
   if BK = bkUnknown then exit;
   BK2 := TBracketKind(Byte(RangeState.C) and 3);
   if BK = BK2 then result := bOpen
   else
      if TBracketKind(Byte(BK2) + 4) = BK then result := bClose;
end;

function CalcCRC32(P: PChar; Len: cardinal):
         {$IFDEF SYN_COMPILER_4_UP} cardinal {$ELSE} integer {$ENDIF};
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Len - 1 do begin
    Result := (Result shr 8) xor CRCTable[byte((P + i)^)
      xor (Result and $000000FF)]
  end;
end;

function TSynMPerlSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['%','@','$','_','0'..'9','a'..'z','A'..'Z'];
end;

procedure TSynMPerlSyn.MakeMethodTables;
var I: Char;
begin
  for I := #0 to #255 do
     case I of
        #0: fProcTable[I] := NullProc;
       #10: fProcTable[I] := LFProc;
       #13: fProcTable[I] := CRProc;
       #1..#9, #11..#12, #14..#32 : fProcTable[I] := SpaceProc;
      '#' : fProcTable[I] := CommentProc;
      '_' : fProcTable[I] := ProcUnderscore;
      '=' : fProcTable[I] := EqualProc;
      '<' : fProcTable[I] := LessProc;
      #34 : fProcTable[i] := QuoteProc;
      '`' : fProcTable[i] := BackQuoteProc;
      '/' : fProcTable[i] := SlashProc;
      '$' : fProcTable[i] := VarProc;
      '%' : fProctable[i] := HashProc;
      '@' : fProcTable[i] := ArrayProc;
      '-' : fProctable[i] := MinusProc;
      'q' : fProctable[i] := QProc;
      #39 : fProcTable[i] := SingleQuoteProc;
      'a'..'p','r'..'z','A'..'Z'  : fProcTable[i] := FromLetterProc;
      '0'..'9','.'        : fProcTable[i] := NumberProc;
      '{','}','[',']','(',')',':',';',',',
      '+','\'             : fProcTable[i] := SymbolProc;
      '^','*','~','?'     : fProcTable[i] := SymbolProc;
      '>','&','|','!'     : fProcTable[i] := SymbolProc;
     else
        fProcTable[I] := UnknownProc;
   end;
end;

procedure TSynMPerlSyn.UnknownProc;
begin
  inc(FCurrentLineOffset);
  fTokenID := tkUnknown;
end;

procedure TSynMPerlSyn.SymbolProc;
begin
  inc(FCurrentLineOffset);
  fTokenID := tkSymbol;
end;

procedure TSynMPerlSyn.MinusProc;
begin
   inc(FCurrentLineOffset);
   fTokenID := tkSymbol;
   // FileTest operations (like "-f")
   if (FCurrentLine[FCurrentLineOffset] in ['r','w','x','o','R','W','X','O','e','z','s','f','d','l','p','S','b','c','t','u','g','k','T','B','M','A','C']) and
      (FCurrentLine[FCurrentLineOffset+1] in [#0..#32,'$',#34,#39,'`']) then begin
      inc(FCurrentLineOffset);
      fTokenID := tkInternalFunction;
   end;
end;

function  TSynMPerlSyn.CheckSharp(Offset : integer) : boolean;
var i : integer;
begin
    result := false;
    i:=FCurrentLineOffset+Offset;
    while true do begin
       case FCurrentLine[i] of
          '#' : if i <> FCurrentLineOffset+Offset then break;
          ' ' : begin Inc(i); continue; end;
       end;
       result := not ( FCurrentLine[FCurrentLineOffset+Offset] in IdentArray[2]);
       exit;
    end;
end;

procedure TSynMPerlSyn.QProc;
begin
   if gltIdent = GenIdentArray[FCurrentLine[FCurrentLineOffset+1]] then begin
      if FCurrentLine[FCurrentLineOffset+1] in ['q','x','w'] then begin
         if CheckSharp(2) then begin
              FCurrentRange  := BuildSimpleRange(rsQuoted);
              ftokenID  := tkOperator;
              Inc(FCurrentLineOffset,2);
              exit;
         end;
      end;
   end else
         if CheckSharp(1) then begin
               FCurrentRange  := BuildSimpleRange(rsQuoted);
               ftokenID  := tkOperator;
               Inc(FCurrentLineOffset);
               exit;
         end;
   FromLetterProc;
end;

procedure TSynMPerlSyn.SlashProc;
var
  i,j: integer;
begin
   ftokenID  := tkOperator;
   i := FCurrentLineOffset-1;
   while true do
   begin
     if i < 0 then
     begin
       Inc(FCurrentLineOffset);
       FCurrentRange := BuildDelimiterRange('/',Byte(rsMSlash),2);
       exit;
     end;
     if not (GenIdentArray[FCurrentLine[i]] in [gltSpaces,gltCRLFNULL]) then
       break;
     Dec(i);
   end;
   // first non-space character was found
   if (FCurrentLine[i] in TSynValidStringChars) then
   begin     // Maybe we found tail of function or variable or number
     j:=i;
     while true do
     begin
       if not (FCurrentLine[j] in TSynValidStringChars) then
       begin
         Inc(j);
         break;
       end;
       Dec(J);
     end;
     if (j<>0) and (FCurrentLine[j] = '$') then Dec(j);
     case CalcCRC32(FCurrentLine+j,i-j+1) of
       $77D76D0C{split},
       $10EE1483{if},
       $8F1F1E9D{unless},
       $1034D0EA{for},
       $667B720C{elsif},
       $06F6A2E0{while},
       $DA8200D7{foreach} : begin
                              Inc(FCurrentLineOffset);
                              FCurrentRange := BuildDelimiterRange
                              ('/',Byte(rsMSlash),2);
                            end;
     else
       SymbolProc;
     end;
   end
   else if (FCurrentLine[i] in [')',']','}','>']) then  // Slash is probably division operator
     SymbolProc
   else
   begin
     Inc(FCurrentLineOffset);
     FCurrentRange := BuildDelimiterRange('/',Byte(rsMSlash),2);
   end;
end;

procedure TSynMPerlSyn.QWProc(Step : integer);
begin
  case Step of
   0 : begin
      FTokenID := tkSpace;
      case fCurrentLine[FCurrentLineOffset] of
          #0:  NullProc;
          #10: LFProc;
          #13: CRProc;
          else repeat
                 if not (fCurrentLine[FCurrentLineOffset] in [#0..#32]) then begin
                     if (FCurrentRange.Range = Byte(rsQuoted)) and
                          IsOpenBracket(fCurrentLine[FCurrentLineOffset]) then
                          FCurrentRange := BuildBracketRange(
                                           fCurrentLine[FCurrentLineOffset],
                                           Byte(rsQuotedBracket),1)
                     else
                          FCurrentRange := BuildDelimiterRange(
                                             fCurrentLine[FCurrentLineOffset],
                                             FCurrentRange.Range,1);
                     break
                 end;
                 Inc(FCurrentLineOffset);
               until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
      end;
   end;
   1 : begin
       Inc(FCurrentLineOffset);
       fTokenID := tkOperator;
       IncRangeStep(FCurrentRange);
   end;
   2 : begin
       case fCurrentLine[FCurrentLineOffset] of
             #0: NullProc;
             #10: LFProc;
             #13: CRProc;
          else begin
              fTokenID := tkString;
              repeat
                if FCurrentRange.Range = Byte(rsQuotedBracket) then begin
                    case CheckForWantedBracket(fCurrentLine[FCurrentLineOffset],
                                               FCurrentRange) of
                        bOpen    : if not IsPreviousBackSlash(FCurrentLineOffset) then
                                      IncBracketCounter(FCurrentRange);
                        bClose   : if not IsPreviousBackSlash(FCurrentLineOffset) then begin
                                      if IsBracketLevelsZero(FCurrentRange) then begin
                                          IncRangeStep(FCurrentRange);
                                          exit;
                                       end;
                                       DecBracketCounter(FCurrentRange);
                                   end;
                    end;
                end else
                    if (fCurrentLine[FCurrentLineOffset] = FCurrentRange.C) and
                       not IsPreviousBackSlash(FCurrentLineOffset) then begin
                           IncRangeStep(FCurrentRange);
                           exit;
                    end;
                Inc(FCurrentLineOffset);
              until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
          end;
       end;
   end;
   3 : begin
       FCurrentRange := BuildSimpleRange(rsUnknown);
       FTokenID := tkOperator;
       Inc(FCurrentLineOffset);
   end;
  end;
end;

function  TSynMPerlSyn.IsPreviousBackSlash(Offset : integer) : boolean;
var i : integer;
begin
   i:=Offset-1;
   while i >= 0 do if fCurrentLine[i] <> '\' then break else Dec(i);
   IsPreviousBackSlash := (Offset - i) mod 2 <> 1
end;

procedure TSynMPerlSyn.S_BetweenBrackets;
begin
   fTokenId := tkUnknown;

   case fCurrentLine[FCurrentLineOffset] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else repeat
           if bOpen = CheckForWantedBracket(fCurrentLine[FCurrentLineOffset],
                                      FCurrentRange) then begin
               FCurrentRange.Range := Byte(rsS_SecondBracket);
               FCurrentRange.Step  := 1;
               break;
           end;
           Inc(FCurrentLineOffset);
         until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
   end;
end;

procedure TSynMPerlSyn.Tr_BetweenBrackets;
begin
   fTokenId := tkUnknown;

   case fCurrentLine[FCurrentLineOffset] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else repeat
           if bOpen = CheckForWantedBracket(fCurrentLine[FCurrentLineOffset],
                                      FCurrentRange) then begin
               FCurrentRange.Range := Byte(rsTr_SecondBracket);
               FCurrentRange.Step  := 1;
               break;
           end;
           Inc(FCurrentLineOffset);
         until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
   end;
end;

procedure TSynMPerlSyn.MProc(Step : integer);
begin
  case Step of
   0 : begin
      FTokenID := tkSpace;
      case fCurrentLine[FCurrentLineOffset] of
          #0:  NullProc;
          #10: LFProc;
          #13: CRProc;
          else repeat
                 if not (fCurrentLine[FCurrentLineOffset] in [#0..#32]) then begin
                    if (TPredefinedRangeStates(FCurrentRange.Range) in
                        [rsM,rsQR,rsS_First,rsTr_First]) and
                        IsOpenBracket(fCurrentLine[FCurrentLineOffset]) then
                        FCurrentRange := BuildBracketRange(
                                           fCurrentLine[FCurrentLineOffset],
                                           Byte(FCurrentRange.Range)+1,1)
                    else
                         FCurrentRange := BuildDelimiterRange (
                                          fCurrentLine[FCurrentLineOffset],
                                          FCurrentRange.Range,1);
                    break;
                 end;
                 Inc(FCurrentLineOffset);
               until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
      end;
   end;
   1 : begin
       Inc(FCurrentLineOffset);
       fTokenID := tkOperator;
       IncRangeStep(FCurrentRange);
   end;
   2 : begin
       case fCurrentLine[FCurrentLineOffset] of
             #0: NullProc;
             #10: LFProc;
             #13: CRProc;
          else begin
              fTokenID := tkRe;
              repeat
                if TPredefinedRangeStates(FCurrentRange.Range) in
                    [rsMBracket,rsQRBracket,rsS_SecondBracket,rsS_FirstBracket,
                     rsTr_SecondBracket,rsTr_FirstBracket] then begin
                     
                    case CheckForWantedBracket(fCurrentLine[FCurrentLineOffset],
                                               FCurrentRange) of
                        bOpen    : if not IsPreviousBackSlash(FCurrentLineOffset) then
                                      IncBracketCounter(FCurrentRange);
                        bClose   : if not IsPreviousBackSlash(FCurrentLineOffset) then begin
                                      if IsBracketLevelsZero(FCurrentRange) then begin
                                         IncRangeStep(FCurrentRange);
                                         exit;
                                      end;
                                      DecBracketCounter(FCurrentRange);
                                   end;
                    end;
                end else
                   if (fCurrentLine[FCurrentLineOffset] = FCurrentRange.C)  and
                      not IsPreviousBackSlash(FCurrentLineOffset) then begin
                          IncRangeStep(FCurrentRange);
                          exit;
                   end;
                Inc(FCurrentLineOffset);
                
             until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
          end;
       end;
   end;
   3 : begin
       FTokenID := tkOperator;
       Inc(FCurrentLineOffset);

       case TPredefinedRangeStates(FCurrentRange.Range) of
          rsS_FirstBracket : begin
                                FCurrentRange.Range := Byte(rsS_BetweenBrackets);
                                exit;
                             end ;
                 rsS_First : begin
                               FCurrentRange.Range := Byte(rsS_Second);
                               FCurrentRange.Step := 2;
                               exit;
                             end;
         rsTr_FirstBracket : begin
                                FCurrentRange.Range := Byte(rsTr_BetweenBrackets);
                                exit;
                             end ;
                rsTr_First : begin
                               FCurrentRange.Range := Byte(rsTr_Second);
                               FCurrentRange.Step := 2;
                               exit;
                             end;
          rsQRBracket,rsQR : while fCurrentLine[FCurrentLineOffset] in
                               ['i','m','o','s','x' ] do Inc(FCurrentLineOffset);
              rsM,rsMSlash,
                rsMBracket : while fCurrentLine[FCurrentLineOffset] in
                               ['c','g','i','m','o','s','x' ] do Inc(FCurrentLineOffset);
        rsS_SecondBracket,
                rsS_Second : while fCurrentLine[FCurrentLineOffset] in
                               ['e','g','i','m','o','s','x' ] do Inc(FCurrentLineOffset);
       rsTr_SecondBracket,
               rsTr_Second : while fCurrentLine[FCurrentLineOffset] in
                               ['c','d','s' ] do Inc(FCurrentLineOffset);
       end;
       FCurrentRange := BuildSimpleRange(rsUnknown);
     end;
  end;
end;

function TSynMPerlSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := FCurrentLineOffset - fTokenPos;
  SetString(Result, (FCurrentLine + FTokenPos), Len);
end;

constructor TSynMPerlSyn.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

   FSpecialSUBAttributes := TSynHighlighterAttributes.Create(SYNS_AttrFunction);
   AddAttribute(FSpecialSUBAttributes);

   FUnknownAttributes := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar);
   AddAttribute(FUnknownAttributes);

   FCommentAttributes := TSynHighlighterAttributes.Create(SYNS_AttrComment);
   AddAttribute(FCommentAttributes);

   FPodAttributes := TSynHighlighterAttributes.Create(SYNS_AttrEmbedText);
   AddAttribute(FPodAttributes);

   FVariableAttributes := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
   AddAttribute(FVariableAttributes);

   FQuoteAttributes := TSynHighlighterAttributes.Create(SYNS_AttrString);
   AddAttribute(FQuoteAttributes);

   FOctNumberAttributes := TSynHighlighterAttributes.Create('OctalNumber');
   AddAttribute(FOctNumberAttributes);

   FPragmaAttributes := TSynHighlighterAttributes.Create(SYNS_AttrPragma);
   AddAttribute(FPragmaAttributes);

   FFormatAttributes := TSynHighlighterAttributes.Create('OutputFormats');
   AddAttribute(FFormatAttributes);

   FDecNumberAttributes := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
   AddAttribute(FDecNumberAttributes);

   FOperatorAttributes := TSynHighlighterAttributes.Create(SYNS_AttrOperator);
   AddAttribute(FOperatorAttributes);

   FSymbolAttributes := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
   AddAttribute(FSymbolAttributes);

   FInternalFunctionAttributes := TSynHighlighterAttributes.Create(SYNS_AttrInternalFunction);
   AddAttribute(FInternalFunctionAttributes);

   FKeywordAttributes := TSynHighlighterAttributes.Create(SYNS_AttrKey);
   AddAttribute(FKeywordAttributes);

   FREAttributes := TSynHighlighterAttributes.Create('RegularExpressions');
   AddAttribute(FREAttributes);

   FHexNumberAttributes := TSynHighlighterAttributes.Create('HexNumber');
   AddAttribute(FHexNumberAttributes);

   FEndOfDataAttributes := TSynHighlighterAttributes.Create(SYNS_AttrText);
   AddAttribute(FEndOfDataAttributes);

   FSpaceAttributes := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
   AddAttribute(FSpaceAttributes);

   FHEREDOCAttributes := TSynHighlighterAttributes.Create('HEREDOC');
   AddAttribute(FHEREDOCAttributes);

   FPredefinedVarAttributes := TSynHighlighterAttributes.Create(SYNS_AttrSpecialVariable);
   AddAttribute(FPredefinedVarAttributes);

   MakeMethodTables;
   FCurrentRange  := BuildSimpleRange(rsUnknown);
   SetAttributesOnChange(DefHighlightChange);
   fDefaultFilter := SYNS_FilterPerl;
end;

function TSynMPerlSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT:    Result := fCommentAttributes;
    SYN_ATTR_IDENTIFIER: Result := fVariableAttributes;
    SYN_ATTR_KEYWORD:    Result := fKeywordAttributes;
    SYN_ATTR_STRING:     Result := fQuoteAttributes;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttributes;
    SYN_ATTR_SYMBOL:     Result := fSymbolAttributes;
  else
    result := nil;
  end;
end;

function TSynMPerlSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynMPerlSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkSpecSub          : Result := FSpecialSUBAttributes;
    tkUnknown          : Result := FUnknownAttributes;
    tkComment          : Result := FCommentAttributes;
    tkPod              : Result := FPodAttributes;
    tkVariable         : Result := FVariableAttributes;
    tkString           : Result := FQuoteAttributes;
    tkSingleString     : Result := FQuoteAttributes;
    tkBackQuote        : Result := FQuoteAttributes;
    tkOctNumber        : Result := FOctNumberAttributes;
    tkPragma           : Result := FPragmaAttributes;
    tkFormatBody       : Result := FFormatAttributes;
    tkDecNumber        : Result := FDecNumberAttributes;
    tkOperator         : Result := FOperatorAttributes;
    tkSymbol           : Result := FSymbolAttributes;
    tkInternalFunction : Result := FInternalFunctionAttributes;
    tkKeyword          : Result := FKeywordAttributes;
    tkRE               : Result := FREAttributes;
    tkHexNumber        : Result := FHexNumberAttributes;
    tkEnd              : Result := FEndOfDataAttributes;
    tkSpace            : Result := FSpaceAttributes;
    tkHeredoc          : Result := FHEREDOCAttributes;
    tkPredefinedVar    : Result := FPredefinedVarAttributes;
    else Result := nil;
  end;
end;

function TSynMPerlSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynMPerlSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynMPerlSyn.Next;
begin
    FTokenPos := FCurrentLineOffset;

    case RecognizeRange(FCurrentRange) of
       rskSimple : case TPredefinedRangeStates(fCurrentRange.Range) of
                      rsUnknown      : fProcTable[fCurrentLine[FCurrentLineOffset]];
                      rsString       : GenQuoteProc;
                      rsSingleString : GenQuoteProc;
                      rsM,rsQR,
                      rsS_First,
                      rsS_Second,
                      rsMSlash       : MProc(0);
                      rsPod          : PodProc;
                      rsBackQuote    : GenQuoteProc;
                      rsEmptyHeredoc : HeredocProc(true);
                      rsQuoted       : QWProc(0);
                      rsTr_First,
                      rsTr_Second    : MProc(0);
                      rsFormatBody   : FormatProc;
                      rsEndOfData    : EndOfDataProc;
                      rsBeforeScript : BeforeScriptProc;
                      rsS_BetweenBrackets  : S_BetweenBrackets;
                      rsTr_BetweenBrackets : Tr_BetweenBrackets;
                    else
                       fProcTable[fCurrentLine[FCurrentLineOffset]];
                    end;
     rskDelimiter : case TPredefinedRangeStates(fCurrentRange.Range) of
                       rsS_BetweenBrackets  : S_BetweenBrackets;
                       rsTr_BetweenBrackets : Tr_BetweenBrackets;
                       rsQuoted,
                       rsQuotedBracket : QWProc(fCurrentRange.Step);
                       rsS_First,
                       rsS_FirstBracket,
                       rsS_SecondBracket,
                       rsS_Second,
                       rsMSlash,
                       rsTr_First,
                       rsTr_Second,
                       rsTr_FirstBracket,
                       rsTr_SecondBracket,
                       rsQR,rsM,
                       rsQRBracket,
                       rsMBracket      : MProc(fCurrentRange.Step);
                       else
                           fProcTable[fCurrentLine[FCurrentLineOffset]];
                    end;
       rskHeredoc : HeredocProc(false);
    end;
end;

procedure TSynMPerlSyn.EndOfDataProc;
begin

  case fCurrentLine[FCurrentLineOffset] of
     #0  : NullProc;
     #10 : LFProc;
     #13 : CRProc;
     else begin
       fTokenID := tkEnd;
       repeat inc(FCurrentLineOffset);
       until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
     end;
  end;
end;

procedure TSynMPerlSyn.BeforeScriptProc;
begin
  if (FCurrentLineOffset=0) and (FCurrentLine[0]='#') and (FCurrentLine[1]='!') then begin
     if 0<>pos('perl',FCurrentLine) then begin
        fTokenID := tkEnd;
        repeat inc(FCurrentLineOffset);
        until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
        FCurrentRange := BuildSimpleRange(rsUnknown);
        exit;
     end;
  end;
  case fCurrentLine[FCurrentLineOffset] of
     #0  : NullProc;
     #10 : LFProc;
     #13 : CRProc;
     else begin
       fTokenID := tkEnd;
       repeat inc(FCurrentLineOffset);
       until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
     end;
  end;
end;

procedure TSynMPerlSyn.SetLine(NewValue: String; LineNumber:Integer);
begin
  FCurrentLine := PChar(NewValue);
  FCurrentLineOffset := 0;

   if (LineNumber = 0) and (poParseWithXSwitch in FOptions) then
      FCurrentRange := BuildSimpleRange(rsBeforeScript);
   Next;
end;

procedure TSynMPerlSyn.NullProc;
begin
   fTokenID := tkNull;
   FSkipHeredocBeforeEOL := false;
end;
procedure TSynMPerlSyn.LFProc;

begin
  fTokenID := tkSpace;
  FSkipHeredocBeforeEOL := false;
  inc(FCurrentLineOffset)
end;

procedure TSynMPerlSyn.CRProc;
begin
  FSkipHeredocBeforeEOL := false;
  fTokenID := tkSpace;
  inc(FCurrentLineOffset);
  if fCurrentLine[FCurrentLineOffset] = #10 then inc(FCurrentLineOffset);
end;

procedure TSynMPerlSyn.SpaceProc;
begin
  inc(FCurrentLineOffset);
  fTokenID := tkSpace;
  while GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltSpaces do
      inc(FCurrentLineOffset);
end;

procedure TSynMPerlSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    if GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL then break;
    inc(FCurrentLineOffset);
  until fCurrentLine[FCurrentLineOffset] = #0;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}  //mh 2000-07-14
function TSynMPerlSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPerl;
end;

function TSynMPerlSyn.GetRange: Pointer;
begin
    Result := FCurrentRange.P;
end;

procedure TSynMPerlSyn.SetRange(Value: Pointer);
begin
   fCurrentRange.P := Value;
end;

procedure TSynMPerlSyn.ReSetRange;
begin
  fCurrentRange := BuildSimpleRange(rsUnknown);
end;

procedure TSynMPerlSyn.ProcUnderscore;
begin
   fTokenID := tkSymbol;
   ToIdent(2);

   if FCurrentLine[FCurrentLineOffset-FStringLen+1]='_' then
      case CalcCRC32(FToIdent,FStringLen) of
        $5B01334B{__DATA__},
        $72F9F3A9{__END__} : begin
                                   fTokenID := tkEND;
                                   if not (poParseAfter__END__ in FOptions) then
                                       fCurrentRange := BuildSimpleRange(rsEndOfData);
                               end;
        $B41D0E2D{__FILE__},
        $F7B6B1C1{__PACKAGE__},
        $BF8FE5C0{__LINE__} : fTokenID := tkPredefinedVar;
      end;
end;


procedure TSynMPerlSyn.PodProc;
begin
  fTokenID := tkPod;
  if (FCurrentLineOffset = 0) and
     (fCurrentLine[0] = '=')  and //
     (fCurrentLine[1] = 'c')  and // Very smart, isn't it ? :-)
     (fCurrentLine[2] = 'u')  and //
     (fCurrentLine[3] = 't')  then begin

     FCurrentRange := BuildSimpleRange(rsUnknown);
     repeat
        if GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL then break;
        inc(FCurrentLineOffset);
     until fCurrentLine[FCurrentLineOffset] = #0;
     exit;
  end;

  case fCurrentLine[FCurrentLineOffset] of
    #0  : NullProc;
    #10 : LFProc;
    #13 : CRProc;
    else begin
      repeat
          inc(FCurrentLineOffset);
      until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
    end;
  end;
end;

procedure TSynMPerlSyn.ToIdent(Index : integer);
begin
    FToIdent := FCurrentLine+ FCurrentLineOffset;
    while FCurrentLine[FCurrentLineOffset] in IdentArray[Index] do
          Inc(FCurrentLineOffset);
    FStringLen := (FCurrentLine + FCurrentLineOffset)-FToIdent;
end;

procedure TSynMPerlSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  if FCurrentLineOffset = 0 then begin
      if not ( (fCurrentLine[1] = 'c') and
         (fCurrentLine[2] = 'u') and
         (fCurrentLine[3] = 't') and not
         (FCurrentLine[4] in ['a'..'z','A'..'Z'])) then begin
         if FCurrentLine[1] in ['a'..'z','A'..'Z'] then begin
             Inc(FCurrentLineOffset,2);
             fTokenID := tkPod;
             fCurrentRange := BuildSimpleRange(rsPod);
             exit
         end;
     end;
  end;
  Inc(FCurrentLineOffset);
end;

procedure TSynMPerlSyn.LessProc;
var i,j,k,len : integer;
    Quote : Char;
    s : string;
    QuoteInside : boolean;
begin
  fTokenID := tkSymbol;
  Inc(FCurrentLineOffset);
  if FCurrentLine[FCurrentLineOffset] <> '<' then exit;
  Inc(FCurrentLineOffset);
  case FCurrentLine[FCurrentLineOffset] of
     ' ',#0,
     #13,#10 : ; // no HEREDOC, it is SHIFT LEFT operation
     ';'     : begin
                  //Dec(FCurrentLineOffset);
                  FCurrentRange:= BuildSimpleRange(rsEmptyHeredoc) ;
                  FSkipHeredocBeforeEOL := true;
                  fTokenID := tkHeredoc;
               end;
     #39,
     '"'     : begin
                  Quote :=  FCurrentLine[FCurrentLineOffset];
                  Inc(FCurrentLineOffset);
                  i:= FCurrentLineOffset;
                  QuoteInside := true;
                  while true do begin
                     if fCurrentLine[i] in [#0,#10,#13] then break; //Unterminated
                                                                    //string, no Heredoc
                     if (fCurrentLine[i] = Quote) then begin
                        if not IsPreviousBackSlash(i) then begin
                            len := i-FCurrentLineOffset;
                            fTokenID := tkHeredoc;
                            if len = 0 then
                                FCurrentRange:= BuildSimpleRange(rsEmptyHeredoc)
                            else begin
                                if QuoteInside then begin
                                   s := Copy(FCurrentLine+FCurrentLineOffset,1,len);
                                   j := 1;
                                   while s <> '' do begin
                                       k := j;
                                       j:=pos('\'+Quote,Copy(s,k,length(s)));
                                       if j = 0 then break;
                                       Delete(s,j+k-1,1);
                                   end;
                                   if s = '' then
                                       FCurrentRange:= BuildSimpleRange(rsEmptyHeredoc)
                                   else
                                       FCurrentRange.P := BuildRangePointer(
                                       CalcCRC32(PChar(s),Length(s)),Length(s));
                                end else
                                   FCurrentRange.P := BuildRangePointer(
                                            CalcCRC32(FCurrentLine+FCurrentLineOffset,
                                            len),len);
                            end;
                            FCurrentLineOffset := i+1;
                            FSkipHeredocBeforeEOL := true;
                            break;
                        end;
                     end else QuoteInside := true;
                     Inc(i);
                  end;
               end;
     else      begin
                  fTokenID := tkHeredoc;
                  i:= FCurrentLineOffset;
                  while fCurrentLine[i] in IdentArray[2] do Inc(i);
                  len := i-FCurrentLineOffset;
                  FCurrentRange.P := BuildRangePointer(
                          CalcCRC32(FCurrentLine+FCurrentLineOffset,len),len);
                  Inc(FCurrentLineOffset,len);
                  FSkipHeredocBeforeEOL := true;
               end;
  end;
end;

procedure TSynMPerlSyn.HeredocProc(IsEmptyHeredoc : boolean);
var i,l,CRC : integer; //T : TRangeState;
label l_main;
begin
  if FSkipHeredocBeforeEOL then begin
//     T := FCurrentRange;
     fProcTable[fCurrentLine[FCurrentLineOffset]];
//     FCurrentRange := T;
     exit;
  end;
  fTokenID := tkHeredoc;
  if (FCurrentLineOffset = 0) then begin
     if IsEmptyHeredoc then begin
        if FCurrentLine[0] in [#0,#13,#10] then
           FCurrentRange := BuildSimpleRange(rsUnknown);
        goto l_main;
     end;
     l:= ExtractStringLenFromHash(FCurrentRange.P);
     i:=0;
     while not (FCurrentLine[i] in [#0,#13,#10]) do begin
        if i > l then goto l_main;
        Inc(i);
     end;
     if i<>l then goto l_main;
     CRC := CalcCRC32(FCurrentLine,i) shr 8;
     
     if (Integer(FCurrentRange.P) shr 8) = CRC  then begin
        FCurrentRange := BuildSimpleRange(rsUnknown);
        FCurrentLineOffset := i;
        exit
     end;
  end;

l_main:
  case fCurrentLine[FCurrentLineOffset] of
    #0  : NullProc;
    #10 : LFProc;
    #13 : CRProc;
    else begin
      repeat
          inc(FCurrentLineOffset);
      until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
    end;
  end;
end;

procedure TSynMPerlSyn.FromLetterProc;
begin
   case FCurrentLine[FCurrentLineOffset] of
      'm' : if CheckSharp(1) then begin
              FCurrentRange  := BuildSimpleRange(rsM);
              ftokenID  := tkOperator;
              Inc(FCurrentLineOffset,1);
              exit;                                        
            end;
      's' : if CheckSharp(1) then begin
              FCurrentRange  := BuildSimpleRange(rsS_First);
              ftokenID  := tkOperator;
              Inc(FCurrentLineOffset,1);
              exit;                                        
            end;
      'y' : if CheckSharp(1) then begin
              FCurrentRange  := BuildSimpleRange(rsTr_First);
              ftokenID  := tkOperator;
              Inc(FCurrentLineOffset,1);
              exit;                                        
            end;
      't' : if (FCurrentLine[FCurrentLineOffset+1] = 'r') and CheckSharp(2) then begin
              FCurrentRange  := BuildSimpleRange(rsTr_First);
              ftokenID  := tkOperator;
              Inc(FCurrentLineOffset,2);
              exit;                                        
            end;                                           
      'q' : if (FCurrentLine[FCurrentLineOffset+1] = 'r') and CheckSharp(2) then begin
              FCurrentRange  := BuildSimpleRange(rsQR);
              ftokenID  := tkOperator;
              Inc(FCurrentLineOffset,2);
              exit;                                        
            end;                                           
   end;

   ToIdent(2);
   if FStringLen=0 then begin
     ftokenid := tkSymbol;
     Inc(FCurrentLineOffset);
     exit;
   end;
   
   FTokenID := CheckIdentCRC(CalcCRC32(FToIdent,FStringLen));
   if FTokenID = tkFormatHeader then begin
      FCurrentRange := BuildSimpleRange(rsFormatBody);
      FTokenID := tkInternalFunction
   end;
end;


procedure TSynMPerlSyn.QuoteProc;
begin
   fTokenID        := tkString;
   fCurrentRange   := BuildSimpleRange(rsString);
   Inc(FCurrentLineOffset);
end;

procedure TSynMPerlSyn.BackQuoteProc;
begin
   fTokenID        := tkBackQuote;
   fCurrentRange   := BuildSimpleRange(rsBackQuote);
   Inc(FCurrentLineOffset);
end;

procedure TSynMPerlSyn.SingleQuoteProc;
begin
   fTokenID        := tkSingleString;
   fCurrentRange   := BuildSimpleRange(rsSingleString);
   Inc(FCurrentLineOffset);
end;

procedure TSynMPerlSyn.GenQuoteProc;
var C : Char;
begin
   C := #0;
   case TPredefinedRangeStates(FCurrentRange.Range) of
      rsString       : begin C := '"'; fTokenID := tkString; end;
      rsSingleString : begin C := #39; fTokenID := tkSingleString; end;
      rsBackQuote    : begin C := '`'; fTokenID := tkBackQuote; end;
   end;

   case fCurrentLine[FCurrentLineOffset] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
       repeat
          if (fCurrentLine[FCurrentLineOffset] = C) and
              not IsPreviousBackSlash(FCurrentLineOffset) then begin

              Inc(FCurrentLineOffset);
              fCurrentRange := BuildSimpleRange(rsUnknown);
              break;
          end;
          Inc(FCurrentLineOffset);
       until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
    end;
  end;
end;

procedure TSynMPerlSyn.FormatProc;
begin
   fTokenID := tkFormatBody;

   if (FCurrentLineOffset = 0) then begin
      if (fCurrentLine[FCurrentLineOffset] ='.') and
         (GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL) then begin
         Inc(FCurrentLineOffset);
         FCurrentRange := BuildSimpleRange(rsUnknown);
         exit;
      end;
      if (fCurrentLine[FCurrentLineOffset] ='#') then begin
         fTokenID := tkComment;
         while not (GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL) do
             Inc(FCurrentLineOffset);
         exit;
      end;
   end;

   case fCurrentLine[FCurrentLineOffset] of
      #0: NullProc;
     #10: LFProc;
     #13: CRProc;
     else begin
            repeat
              Inc(FCurrentLineOffset);
            until GenIdentArray[fCurrentLine[FCurrentLineOffset]] = gltCRLFNULL;
     end;
   end;
end;

procedure TSynMPerlSyn.VarProc;
begin
   Inc(FCurrentLineOffset);
   ftokenid := tkSymbol;

   case VarIdentArray[FCurrentLine[FCurrentLineOffset]] of
      vltSpace : ;
      vltCaret : case FCurrentLine[FCurrentLineOffset+1] of
                     #0..#32      : begin
                                        fTokenID := tkPredefinedVar; // STDOUT_TOP var
                                        Inc(FCurrentLineOffset);
                                    end;
                     'L','A','E','F','C','D','V','H','M','O','R','S','X','I','P','T','W' : if FCurrentLine[FCurrentLineOffset+2] in [#0..#32] then begin
                                        fTokenID := tkPredefinedVar;
                                        Inc(FCurrentLineOffset,2);
                                    end;
                 end;
 vltPredefined : if not ( FCurrentLine[FCurrentLineOffset+1] in IdentArray[2]) then begin
                     fTokenID := tkPredefinedVar;
                     Inc(FCurrentLineOffset);
                 end;
 vltUnderscore : if FCurrentLine[FCurrentLineOffset+1] in IdentArray[3] then begin
                     fTokenID := tkPredefinedVar;
                     Inc(FCurrentLineOffset);
                 end else begin
                    ToIdent(2);
                    fTokenID := tkVariable;
                 end ;
      vltSharp : if FCurrentLine[FCurrentLineOffset+1] in IdentArray[3] then begin
                    fTokenID := tkPredefinedVar;
                    Inc(FCurrentLineOffset);
                 end else begin
                    Inc(FCurrentLineOffset);
                    ToIdent(2);
                    case CalcCRC32(FToIdent,FStringLen) of
                       $FE64C4EE{INC},$71A0E885{ARGV},$FBD44C65{_} : fTokenID := tkPredefinedVar;
                       else    fTokenID := tkVariable;
                    end;
                 end;
      vltOther : begin
                    ToIdent(2);
                    FTokenID := CheckVariableCRC(CalcCRC32(FToIdent,FStringLen));
                 end;
   end;
end;

procedure TSynMPerlSyn.ArrayProc;
begin
   Inc(FCurrentLineOffset);
   ftokenid := tkSymbol;
   ToIdent(2);
   case CalcCRC32(FToIdent,FStringLen) of
      $FE64C4EE{INC},
      $71A0E885{ARGV},
      $FBD44C65{_} : fTokenID := tkPredefinedVar;
      else if FStringLen <> 0 then ftokenid := tkVariable;
   end;
end;

procedure TSynMPerlSyn.HashProc;
begin
   Inc(FCurrentLineOffset);
   ftokenid := tkSymbol;
   ToIdent(2);
   case CalcCRC32(FToIdent,FStringLen) of
      $FE64C4EE{INC},
      $A7F9B096{SIG},
      $9AA3D961{ENV} : fTokenID := tkPredefinedVar;
      else if FStringLen <> 0 then ftokenid := tkVariable;
   end;
end;

procedure TSynMPerlSyn.NumberProc;
label l_dot;
begin
   inc (FCurrentLineOffset);
   FTokenID := tkSymbol;
   case FCurrentLine[FCurrentLineOffset-1] of
      '0' : case FCurrentLine[FCurrentLineOffset] of
             '0'..'9','_' : begin ToIdent(4); fTokenID := tkOctNumber; exit; end;
             '.'          : begin fTokenID := tkDecNumber; Inc(FCurrentLineOffset); goto l_dot; end;
             'x','X'      : begin Inc(FCurrentLineOffset); ToIdent(5);
                                  if FStringLen <>0 then fTokenID := tkHexNumber
                                  else begin
                                      Dec(FCurrentLineOffset);
                                      fTokenID := tkDecNumber;
                                  end;
                                  exit;
                            end;
             else fTokenID := tkDecNumber;
            end;
      '.' : if FCurrentLine[FCurrentLineOffset] in ['0'..'9','e','E'] then goto l_dot;
     else begin
        fTokenID := tkDecNumber;
        ToIdent(4);
        if FCurrentLine[FCurrentLineOffset] in ['.','e','E'] then begin
           Inc(FCurrentLineOffset);
           goto l_dot; 
        end;
     end;
   end;
   exit;
l_dot:
   if not (FCurrentLine[FCurrentLineOffset] in ['0'..'9','e','E']) then begin
      Dec(FCurrentLineOffset);
      exit;
   end;
   if FCurrentLine[FCurrentLineOffset] in ['0'..'9'] then begin
      ToIdent(4);
      fTokenID := tkDecNumber;
   end;
   if (FCurrentLine[FCurrentLineOffset] in ['e','E']) and
      (FCurrentLine[FCurrentLineOffset+1] in ['0'..'9','+','-']) then begin
      Inc(FCurrentLineOffset,2);
      ToIdent(6);
      fTokenID := tkDecNumber;
   end;
end;

function TSynMPerlSyn.GetSampleSource: string;
begin
  Result :=
    '#!/bin/perl'#13#10 +
    'require "cgi-lib.pl";'#13#10 +
    'use sigtrap;'#13#10 +
    'do ''envars.pl'';'#13#10 +
    '$_ = $password1;'#13#10 +
    'sub WriteBack {'#13#10 +
    '        while ($_ ne "fred")    {'#13#10 +
    '                sleep 5;'#13#10 +
    '        }'#13#10 +
    '}';
end;

initialization
   {$IFNDEF SYN_CPPB_1}
    RegisterPlaceableHighlighter(TSynMPerlSyn);
   {$ENDIF}
   BuildIdentArray;
end.

