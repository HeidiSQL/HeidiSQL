unit SynRegExpr;

{
  TRegExpr class library
  Delphi Regular Expressions

  Copyright (c) 1999-2004 Andrey V. Sorokin, St.Petersburg, Russia

  You can choose to use this Pascal unit in one of the two following licenses:

  Option 1>

  You may use this software in any kind of development,
  including comercial, redistribute, and modify it freely,
  under the following restrictions :
  1. This software is provided as it is, without any kind of
  warranty given. Use it at Your own risk.The author is not
  responsible for any consequences of use of this software.
  2. The origin of this software may not be mispresented, You
  must not claim that You wrote the original software. If
  You use this software in any kind of product, it would be
  appreciated that there in a information box, or in the
  documentation would be an acknowledgement like

  Partial Copyright (c) 2004 Andrey V. Sorokin
  https://sorokin.engineer/
  andrey@sorokin.engineer

  3. You may not have any income from distributing this source
  (or altered version of it) to other developers. When You
  use this product in a comercial package, the source may
  not be charged seperatly.
  4. Altered versions must be plainly marked as such, and must
  not be misrepresented as being the original software.
  5. RegExp Studio application and all the visual components as
  well as documentation is not part of the TRegExpr library
  and is not free for usage.

  https://sorokin.engineer/
  andrey@sorokin.engineer

  Option 2>

  The same modified LGPL with static linking exception as the Free Pascal RTL
}

interface

{ off $DEFINE DebugSynRegExpr }
// ======== Determine compiler
{$INCLUDE SynEdit.inc}
// ======== Define base compiler options
{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$IFDEF SYN_COMPILER_6_UP}
  {$WARN SYMBOL_PLATFORM OFF} // Suppress .Net warnings
{$ENDIF}
{$IFDEF SYN_COMPILER_7_UP}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
{$IFDEF FPC}
  {$MODE DELPHI} // Delphi-compatible mode in FreePascal
  {$INLINE ON}
{$ENDIF}
// ======== Define options for TRegExpr engine
{$DEFINE UniCode} // Use WideChar for characters and UnicodeString/WideString for strings
{ off $DEFINE UseWordChars} // Use WordChars property, otherwise fixed list 'a'..'z','A'..'Z','0'..'9','_'
{ off $DEFINE UseSpaceChars} // Use SpaceChars property, otherwise fixed list
{ off $DEFINE UnicodeWordDetection} // Additionally to ASCII word chars, detect word chars >=128 by Unicode table
{$DEFINE UseFirstCharSet} // Enable optimization, which finds possible first chars of input string
{$DEFINE RegExpPCodeDump} // Enable method Dump() to show opcode as string
{$IFNDEF FPC} // Not supported in FreePascal
  {$DEFINE reRealExceptionAddr} // Exceptions will point to appropriate source line, not to Error procedure
{$ENDIF}
{$DEFINE ComplexBraces} // Support braces in complex cases
{$IFNDEF UniCode}
  {$UNDEF UnicodeWordDetection}
{$ENDIF}
// ======== Define Pascal-language options
// Define 'UseAsserts' option (do not edit this definitions).
// Asserts used to catch 'strange bugs' in TRegExpr implementation (when something goes
// completely wrong). You can swith asserts on/off with help of {$C+}/{$C-} compiler options.
{$IFDEF SYN_COMPILER_3_UP} {$DEFINE UseAsserts} {$ENDIF}
{$IFDEF FPC} {$DEFINE UseAsserts} {$ENDIF}
// Define 'use subroutine parameters default values' option (do not edit this definition).
{$IFDEF SYN_COMPILER_4_UP} {$DEFINE DefParam} {$ENDIF}
{$IFDEF FPC} {$DEFINE DefParam} {$ENDIF}
// Define 'OverMeth' options, to use method overloading (do not edit this definitions).
{$IFDEF SYN_COMPILER_5_UP} {$DEFINE OverMeth} {$ENDIF}
{$IFDEF FPC} {$DEFINE OverMeth} {$ENDIF}
// Define 'InlineFuncs' options, to use inline keyword (do not edit this definitions).
{$IFDEF SYN_COMPILER_8_UP} {$DEFINE InlineFuncs} {$ENDIF}
{$IFDEF FPC} {$DEFINE InlineFuncs} {$ENDIF}

uses
  Classes, // TStrings in Split method
  SysUtils, // Exception
  Math;

type
  {$IFNDEF FPC}
  PtrInt = integer;
  PtrUInt = cardinal;
  {$ENDIF}
  {$IFDEF UniCode}
  PRegExprChar = PWideChar;
  {$IFDEF FPC}
  RegExprString = UnicodeString;
  {$ELSE}
    {$IFDEF SYN_DELPHI_2009_UP}
    RegExprString = UnicodeString;
    {$ELSE}
    RegExprString = WideString;
    {$ENDIF}
  {$ENDIF}
  REChar = WideChar;
  {$ELSE}
  PRegExprChar = PChar;
  RegExprString = AnsiString; // ###0.952 was string
  REChar = Char;
  {$ENDIF}
  TREOp = REChar; // internal p-code type //###0.933
  PREOp = ^TREOp;

type
  TRegExprInvertCaseFunction = function(const Ch: REChar): REChar of object;
  TRegExprCharset = set of byte;

const
  // Escape char ('\' in common r.e.) used for escaping metachars (\w, \d etc)
  EscChar = '\';

  RegExprModifierI: boolean = False; // default value for ModifierI
  RegExprModifierR: boolean = True; // default value for ModifierR
  RegExprModifierS: boolean = True; // default value for ModifierS
  RegExprModifierG: boolean = True; // default value for ModifierG
  RegExprModifierM: boolean = False; // default value for ModifierM
  RegExprModifierX: boolean = False; // default value for ModifierX

  {$IFDEF UseSpaceChars}
  // default value for SpaceChars
  RegExprSpaceChars: RegExprString = ' '#$9#$A#$D#$C;
  {$ENDIF}

  {$IFDEF UseWordChars}
  // default value for WordChars
  RegExprWordChars: RegExprString = '0123456789'
    + 'abcdefghijklmnopqrstuvwxyz'
    + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  {$ENDIF}

  // default value for LineSeparators
  RegExprLineSeparators: RegExprString = #$d#$a#$b#$c
    {$IFDEF UniCode}
    + #$2028#$2029#$85
    {$ENDIF};

  // default value for LinePairedSeparator
  RegExprLinePairedSeparator: RegExprString = #$d#$a;
  { if You need Unix-styled line separators (only \n), then use:
    RegExprLineSeparators = #$a;
    RegExprLinePairedSeparator = '';
  }

  // Tab and Unicode category "Space Separator":
  // https://www.compart.com/en/unicode/category/Zs
  RegExprHorzSeparators: RegExprString = #9#$20#$A0
    {$IFDEF UniCode}
    + #$1680#$2000#$2001#$2002#$2003#$2004#$2005#$2006#$2007#$2008#$2009#$200A#$202F#$205F#$3000
    {$ENDIF};

const
  NSUBEXP = 90; // max number of subexpression //###0.929
  // Cannot be more than NSUBEXPMAX
  // Be carefull - don't use values which overflow CLOSE opcode
  // (in this case you'll get compiler error).
  // Big NSUBEXP will cause more slow work and more stack required
  NSUBEXPMAX = 255; // Max possible value for NSUBEXP. //###0.945
  // Don't change it! It's defined by internal TRegExpr design.

{$IFDEF ComplexBraces}
const
  LoopStackMax = 10; // max depth of loops stack //###0.925

type
  TRegExprLoopStack = array [1 .. LoopStackMax] of integer;
{$ENDIF}

type
  TRegExprModifiers = record
    I: boolean;
       // Case-insensitive.
    R: boolean;
       // Extended syntax for Russian ranges in [].
       // If True, then а-я additionally includes letter 'ё',
       // А-Я additionally includes 'Ё', and а-Я includes all Russian letters.
       // Turn it off if it interferes with your national alphabet.
    S: boolean;
       // Dot '.' matches any char, otherwise only [^\n].
    G: boolean;
       // Greedy. Switching it off switches all operators to non-greedy style,
       // so if G=False, then '*' works like '*?', '+' works like '+?' and so on.
    M: boolean;
       // Treat string as multiple lines. It changes `^' and `$' from
       // matching at only the very start/end of the string to the start/end
       // of any line anywhere within the string.
    X: boolean;
       // Allow comments in regex using # char.
  end;

function IsModifiersEqual(const A, B: TRegExprModifiers): boolean;

type
  TRegExpr = class;
  TRegExprReplaceFunction = function(ARegExpr: TRegExpr): RegExprString of object;
  TRegExprCharChecker = function(ch: REChar): boolean of object;
  TRegExprCharCheckerArray = array[0 .. 30] of TRegExprCharChecker;
  TRegExprCharCheckerInfo = record
    CharBegin, CharEnd: REChar;
    CheckerIndex: integer;
  end;
  TRegExprCharCheckerInfos = array of TRegExprCharCheckerInfo;

  { TRegExpr }

  TRegExpr = class
  private
    startp: array [0 .. NSUBEXP - 1] of PRegExprChar; // found expr start points
    endp: array [0 .. NSUBEXP - 1] of PRegExprChar; // found expr end points

    GrpIndexes: array [0 .. NSUBEXP - 1] of integer;
    GrpCount: integer;

    {$IFDEF ComplexBraces}
    LoopStack: TRegExprLoopStack; // state before entering loop
    LoopStackIdx: integer; // 0 - out of all loops
    {$ENDIF}

    // The "internal use only" fields to pass info from compile
    // to execute that permits the execute phase to run lots faster on
    // simple cases.
    reganchored: REChar; // is the match anchored (at beginning-of-line only)?
    regmust: PRegExprChar; // string (pointer into program) that match must include, or nil
    regmustlen: integer; // length of regmust string
    regmustString: RegExprString;
    // reganchored permits very fast decisions on suitable starting points
    // for a match, cutting down the work a lot. Regmust permits fast rejection
    // of lines that cannot possibly match. The regmust tests are costly enough
    // that regcomp() supplies a regmust only if the r.e. contains something
    // potentially expensive (at present, the only such thing detected is * or +
    // at the start of the r.e., which can involve a lot of backup). regmustlen is
    // supplied because the test in regexec() needs it and regcomp() is computing
    // it anyway.

    {$IFDEF UseFirstCharSet}
    FirstCharSet: TRegExprCharset;
    FirstCharArray: array[byte] of boolean;
    {$ENDIF}

    // work variables for Exec routines - save stack in recursion
    reginput: PRegExprChar; // String-input pointer.
    fInputStart: PRegExprChar; // Pointer to first char of input string.
    fInputEnd: PRegExprChar; // Pointer to char AFTER last char of input string
    fRegexStart: PRegExprChar;
    fRegexEnd: PRegExprChar;

    // work variables for compiler's routines
    regparse: PRegExprChar; // Input-scan pointer.
    regnpar: integer; // Count of () brackets.
    regdummy: REChar;
    regcode: PRegExprChar; // Code-emit pointer; @regdummy = don't.
    regsize: integer; // Total programm size in REChars.
    regExactlyLen: PLongInt;
    regexpBegin: PRegExprChar; // only for error handling. Contains pointer to beginning of r.e. while compiling
    regexpIsCompiled: boolean; // true if r.e. successfully compiled
    fSecondPass: boolean;

    // programm is essentially a linear encoding
    // of a nondeterministic finite-state machine (aka syntax charts or
    // "railroad normal form" in parsing technology). Each node is an opcode
    // plus a "next" pointer, possibly plus an operand. "Next" pointers of
    // all nodes except BRANCH implement concatenation; a "next" pointer with
    // a BRANCH on both ends of it connects two alternatives. (Here we
    // have one of the subtle syntax dependencies: an individual BRANCH (as
    // opposed to a collection of them) is never concatenated with anything
    // because of operator precedence.) The operand of some types of node is
    // a literal string; for others, it is a node leading into a sub-FSM. In
    // particular, the operand of a BRANCH node is the first node of the branch.
    // (NB this is *not* a tree structure: the tail of the branch connects
    // to the thing following the set of BRANCHes.) The opcodes are:
    programm: PRegExprChar; // Unwarranted chumminess with compiler.

    fExpression: RegExprString; // source of compiled r.e.
    fInputString: RegExprString; // input string
    fLastError: integer; // see Error, LastError
    fLastErrorOpcode: TREOp;

    fModifiers: TRegExprModifiers; // modifiers
    fCompModifiers: TRegExprModifiers; // compiler's copy of modifiers
    fProgModifiers: TRegExprModifiers; // modifiers values from last programm compilation

    {$IFDEF UseSpaceChars}
    fSpaceChars: RegExprString;
    {$ENDIF}
    {$IFDEF UseWordChars}
    fWordChars: RegExprString;
    {$ENDIF}
    fInvertCase: TRegExprInvertCaseFunction;

    fLineSeparators: RegExprString;
    fLinePairedSeparatorAssigned: boolean;
    fLinePairedSeparatorHead, fLinePairedSeparatorTail: REChar;

    FReplaceLineEnd: RegExprString; // string to use for "\n" in Substitute method
    FUseOsLineEndOnReplace: boolean; // use OS LineBreak chars (LF or CRLF) for FReplaceLineEnd

    fSlowChecksSizeMax: integer;
    // use ASlowChecks=True in Exec() only when Length(InputString)<SlowChecksSizeMax
    // ASlowChecks enables to use regmustString optimization

    {$IFNDEF UniCode}
    fLineSepArray: array[byte] of boolean;
    {$ENDIF}
    {$IFDEF UnicodeWordDetection}
    FUseUnicodeWordDetection: boolean;
    {$ENDIF}

    CharCheckers: TRegExprCharCheckerArray;
    CharCheckerInfos: TRegExprCharCheckerInfos;
    CheckerIndex_Word: byte;
    CheckerIndex_NotWord: byte;
    CheckerIndex_Digit: byte;
    CheckerIndex_NotDigit: byte;
    CheckerIndex_Space: byte;
    CheckerIndex_NotSpace: byte;
    CheckerIndex_HorzSep: byte;
    CheckerIndex_NotHorzSep: byte;
    CheckerIndex_VertSep: byte;
    CheckerIndex_NotVertSep: byte;
    CheckerIndex_LowerAZ: byte;
    CheckerIndex_UpperAZ: byte;

    procedure InitCharCheckers;
    function CharChecker_Word(ch: REChar): boolean;
    function CharChecker_NotWord(ch: REChar): boolean;
    function CharChecker_Space(ch: REChar): boolean;
    function CharChecker_NotSpace(ch: REChar): boolean;
    function CharChecker_Digit(ch: REChar): boolean;
    function CharChecker_NotDigit(ch: REChar): boolean;
    function CharChecker_HorzSep(ch: REChar): boolean;
    function CharChecker_NotHorzSep(ch: REChar): boolean;
    function CharChecker_VertSep(ch: REChar): boolean;
    function CharChecker_NotVertSep(ch: REChar): boolean;
    function CharChecker_LowerAZ(ch: REChar): boolean;
    function CharChecker_UpperAZ(ch: REChar): boolean;

    procedure ClearMatches; {$IFDEF InlineFuncs}inline;{$ENDIF}
    procedure ClearInternalIndexes; {$IFDEF InlineFuncs}inline;{$ENDIF}
    function FindInCharClass(ABuffer: PRegExprChar; AChar: REChar; AIgnoreCase: boolean): boolean;
    procedure GetCharSetFromCharClass(ABuffer: PRegExprChar; AIgnoreCase: boolean; var ARes: TRegExprCharset);
    procedure GetCharSetFromSpaceChars(var ARes: TRegExprCharset); {$IFDEF InlineFuncs}inline;{$ENDIF}
    procedure GetCharSetFromWordChars(var ARes: TRegExprCharSet); {$IFDEF InlineFuncs}inline;{$ENDIF}
    function IsWordChar(AChar: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    function IsSpaceChar(AChar: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    function IsCustomLineSeparator(AChar: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    procedure InitLineSepArray;

    // Mark programm as having to be [re]compiled
    procedure InvalidateProgramm;

    // Check if we can use precompiled r.e. or
    // [re]compile it if something changed
    function IsProgrammOk: boolean; // ###0.941

    procedure SetExpression(const AStr: RegExprString);

    function GetModifierStr: RegExprString;
    procedure SetModifierStr(const AStr: RegExprString);
    function GetModifierG: boolean;
    function GetModifierI: boolean;
    function GetModifierM: boolean;
    function GetModifierR: boolean;
    function GetModifierS: boolean;
    function GetModifierX: boolean;
    procedure SetModifierG(AValue: boolean);
    procedure SetModifierI(AValue: boolean);
    procedure SetModifierM(AValue: boolean);
    procedure SetModifierR(AValue: boolean);
    procedure SetModifierS(AValue: boolean);
    procedure SetModifierX(AValue: boolean);

    // Default handler raises exception ERegExpr with
    // Message = ErrorMsg (AErrorID), ErrorCode = AErrorID
    // and CompilerErrorPos = value of property CompilerErrorPos.
    procedure Error(AErrorID: integer); virtual; // error handler.

    { ==================== Compiler section =================== }
    // compile a regular expression into internal code
    function CompileRegExpr(ARegExp: PRegExprChar): boolean;
    procedure SetUseOsLineEndOnReplace(AValue: boolean);

    // set the next-pointer at the end of a node chain
    procedure Tail(p: PRegExprChar; val: PRegExprChar);

    // regoptail - regtail on operand of first argument; nop if operandless
    procedure OpTail(p: PRegExprChar; val: PRegExprChar);

    // regnode - emit a node, return location
    function EmitNode(op: TREOp): PRegExprChar;

    // emit (if appropriate) a byte of code
    procedure EmitC(ch: REChar); {$IFDEF InlineFuncs}inline;{$ENDIF}

    // emit LongInt value
    procedure EmitInt(AValue: LongInt); {$IFDEF InlineFuncs}inline;{$ENDIF}

    // insert an operator in front of already-emitted operand
    // Means relocating the operand.
    procedure InsertOperator(op: TREOp; opnd: PRegExprChar; sz: integer);
    // ###0.90

    // regular expression, i.e. main body or parenthesized thing
    function ParseReg(paren: integer; var flagp: integer): PRegExprChar;

    // one alternative of an | operator
    function ParseBranch(var flagp: integer): PRegExprChar;

    // something followed by possible [*+?]
    function ParsePiece(var flagp: integer): PRegExprChar;

    function HexDig(Ch: REChar): integer;

    function UnQuoteChar(var APtr: PRegExprChar): REChar;

    // the lowest level
    function ParseAtom(var flagp: integer): PRegExprChar;

    // current pos in r.e. - for error hanling
    function GetCompilerErrorPos: PtrInt;

    {$IFDEF UseFirstCharSet} // ###0.929
    procedure FillFirstCharSet(prog: PRegExprChar);
    {$ENDIF}
    { ===================== Matching section =================== }
    // repeatedly match something simple, report how many
    function regrepeat(p: PRegExprChar; AMax: integer): integer;

    // dig the "next" pointer out of a node
    function regnext(p: PRegExprChar): PRegExprChar;

    // recursively matching routine
    function MatchPrim(prog: PRegExprChar): boolean;

    // match at specific position only, called from ExecPrim
    function MatchAtOnePos(APos: PRegExprChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}

    // Exec for stored InputString
    function ExecPrim(AOffset: integer; ATryOnce, ASlowChecks: boolean): boolean;

    {$IFDEF RegExpPCodeDump}
    function DumpOp(op: TREOp): RegExprString;
    {$ENDIF}
    function GetSubExprCount: integer;
    function GetMatchPos(Idx: integer): PtrInt;
    function GetMatchLen(Idx: integer): PtrInt;
    function GetMatch(Idx: integer): RegExprString;

    procedure SetInputString(const AInputString: RegExprString);
    procedure SetLineSeparators(const AStr: RegExprString);
    procedure SetLinePairedSeparator(const AStr: RegExprString);
    function GetLinePairedSeparator: RegExprString;

  public
    constructor Create; {$IFDEF OverMeth} overload;
    constructor Create(const AExpression: RegExprString); overload;
    {$ENDIF}
    destructor Destroy; override;

    class function VersionMajor: integer;
    class function VersionMinor: integer;

    // match a programm against a string AInputString
    // !!! Exec store AInputString into InputString property
    // For Delphi 5 and higher available overloaded versions - first without
    // parameter (uses already assigned to InputString property value)
    // and second that has int parameter and is same as ExecPos
    function Exec(const AInputString: RegExprString): boolean;
    {$IFDEF OverMeth} overload;
    function Exec: boolean; overload;
    function Exec(AOffset: integer): boolean; overload;
    {$ENDIF}

    // find next match:
    // ExecNext;
    // works the same as
    // if MatchLen [0] = 0 then ExecPos (MatchPos [0] + 1)
    // else ExecPos (MatchPos [0] + MatchLen [0]);
    // but it's more simpler !
    // Raises exception if used without preceeding SUCCESSFUL call to
    // Exec* (Exec, ExecPos, ExecNext). So You always must use something like
    // if Exec (InputString) then repeat { proceed results} until not ExecNext;
    function ExecNext: boolean;

    // find match for InputString starting from AOffset position
    // (AOffset=1 - first char of InputString)
    function ExecPos(AOffset: integer {$IFDEF DefParam} = 1{$ENDIF}): boolean;
    {$IFDEF OverMeth} overload;
    function ExecPos(AOffset: integer; ATryOnce: boolean): boolean; overload;
    {$ENDIF}

    // Returns ATemplate with '$&' or '$0' replaced by whole r.e.
    // occurence and '$1'...'$nn' replaced by subexpression with given index.
    // Symbol '$' is used instead of '\' (for future extensions
    // and for more Perl-compatibility) and accepts more than one digit.
    // If you want to place into template raw '$' or '\', use prefix '\'.
    // Example: '1\$ is $2\\rub\\' -> '1$ is <Match[2]>\rub\'
    // If you want to place any number after '$' you must enclose it
    // with curly braces: '${12}'.
    // Example: 'a$12bc' -> 'a<Match[12]>bc'
    // 'a${1}2bc' -> 'a<Match[1]>2bc'.
    function Substitute(const ATemplate: RegExprString): RegExprString;

    // Splits AInputStr to list by positions of all r.e. occurencies.
    // Internally calls Exec, ExecNext.
    procedure Split(const AInputStr: RegExprString; APieces: TStrings);

    function Replace(const AInputStr: RegExprString;
      const AReplaceStr: RegExprString;
      AUseSubstitution: boolean{$IFDEF DefParam} = False{$ENDIF}) // ###0.946
      : RegExprString; {$IFDEF OverMeth} overload;
    function Replace(const AInputStr: RegExprString;
      AReplaceFunc: TRegExprReplaceFunction): RegExprString; overload;
    {$ENDIF}
    // Returns AInputStr with r.e. occurencies replaced by AReplaceStr.
    // If AUseSubstitution is true, then AReplaceStr will be used
    // as template for Substitution methods.
    // For example:
    // Expression := '({-i}block|var)\s*\(\s*([^ ]*)\s*\)\s*';
    // Replace ('BLOCK( test1)', 'def "$1" value "$2"', True);
    // will return:  def 'BLOCK' value 'test1'
    // Replace ('BLOCK( test1)', 'def "$1" value "$2"')
    // will return:  def "$1" value "$2"
    // Internally calls Exec, ExecNext.
    // Overloaded version and ReplaceEx operate with callback function,
    // so you can implement really complex functionality.
    function ReplaceEx(const AInputStr: RegExprString;
      AReplaceFunc: TRegExprReplaceFunction): RegExprString;

    // Returns ID of last error, 0 if no errors (unusable if
    // Error method raises exception) and clear internal status
    // into 0 (no errors).
    function LastError: integer;

    // Returns Error message for error with ID = AErrorID.
    function ErrorMsg(AErrorID: integer): RegExprString; virtual;

    // Converts Ch into upper case if it in lower case or in lower
    // if it in upper (uses current system local setings)
    class function InvertCaseFunction(const Ch: REChar): REChar;

    // [Re]compile r.e. Useful for example for GUI r.e. editors (to check
    // all properties validity).
    procedure Compile; // ###0.941

    {$IFDEF RegExpPCodeDump}
    // dump a compiled regexp in vaguely comprehensible form
    function Dump: RegExprString;
    {$ENDIF}

    // Regular expression.
    // For optimization, TRegExpr will automatically compiles it into 'P-code'
    // (You can see it with help of Dump method) and stores in internal
    // structures. Real [re]compilation occures only when it really needed -
    // while calling Exec, ExecNext, Substitute, Dump, etc
    // and only if Expression or other P-code affected properties was changed
    // after last [re]compilation.
    // If any errors while [re]compilation occures, Error method is called
    // (by default Error raises exception - see below)
    property Expression: RegExprString read fExpression write SetExpression;

    // Set/get default values of r.e.syntax modifiers. Modifiers in
    // r.e. (?ismx-ismx) will replace this default values.
    // If you try to set unsupported modifier, Error will be called
    // (by defaul Error raises exception ERegExpr).
    property ModifierStr: RegExprString read GetModifierStr write SetModifierStr;

    property ModifierI: boolean read GetModifierI write SetModifierI;
    property ModifierR: boolean read GetModifierR write SetModifierR;
    property ModifierS: boolean read GetModifierS write SetModifierS;
    property ModifierG: boolean read GetModifierG write SetModifierG;
    property ModifierM: boolean read GetModifierM write SetModifierM;
    property ModifierX: boolean read GetModifierX write SetModifierX;

    // returns current input string (from last Exec call or last assign
    // to this property).
    // Any assignment to this property clear Match* properties !
    property InputString: RegExprString read fInputString write SetInputString;

    // Number of subexpressions has been found in last Exec* call.
    // If there are no subexpr. but whole expr was found (Exec* returned True),
    // then SubExprMatchCount=0, if no subexpressions nor whole
    // r.e. found (Exec* returned false) then SubExprMatchCount=-1.
    // Note, that some subexpr. may be not found and for such
    // subexpr. MathPos=MatchLen=-1 and Match=''.
    // For example: Expression := '(1)?2(3)?';
    // Exec ('123'): SubExprMatchCount=2, Match[0]='123', [1]='1', [2]='3'
    // Exec ('12'): SubExprMatchCount=1, Match[0]='12', [1]='1'
    // Exec ('23'): SubExprMatchCount=2, Match[0]='23', [1]='', [2]='3'
    // Exec ('2'): SubExprMatchCount=0, Match[0]='2'
    // Exec ('7') - return False: SubExprMatchCount=-1
    property SubExprMatchCount: integer read GetSubExprCount;

    // pos of entrance subexpr. #Idx into tested in last Exec*
    // string. First subexpr. has Idx=1, last - MatchCount,
    // whole r.e. has Idx=0.
    // Returns -1 if in r.e. no such subexpr. or this subexpr.
    // not found in input string.
    property MatchPos[Idx: integer]: PtrInt read GetMatchPos;

    // len of entrance subexpr. #Idx r.e. into tested in last Exec*
    // string. First subexpr. has Idx=1, last - MatchCount,
    // whole r.e. has Idx=0.
    // Returns -1 if in r.e. no such subexpr. or this subexpr.
    // not found in input string.
    // Remember - MatchLen may be 0 (if r.e. match empty string) !
    property MatchLen[Idx: integer]: PtrInt read GetMatchLen;

    // == copy (InputString, MatchPos [Idx], MatchLen [Idx])
    // Returns '' if in r.e. no such subexpr. or this subexpr.
    // not found in input string.
    property Match[Idx: integer]: RegExprString read GetMatch;

    // Returns position in r.e. where compiler stopped.
    // Useful for error diagnostics
    property CompilerErrorPos: PtrInt read GetCompilerErrorPos;

    {$IFDEF UseSpaceChars}
    // Contains chars, treated as /s (initially filled with RegExprSpaceChars
    // global constant)
    property SpaceChars: RegExprString read fSpaceChars write fSpaceChars;
    // ###0.927
    {$ENDIF}

    {$IFDEF UseWordChars}
    // Contains chars, treated as /w (initially filled with RegExprWordChars
    // global constant)
    property WordChars: RegExprString read fWordChars write fWordChars;
    // ###0.929
    {$ENDIF}

    {$IFDEF UnicodeWordDetection}
    // If set to true, in addition to using WordChars, a heuristic to detect unicode word letters is used for \w
    property UseUnicodeWordDetection: boolean read FUseUnicodeWordDetection write FUseUnicodeWordDetection;
    {$ENDIF}
    // line separators (like \n in Unix)
    property LineSeparators: RegExprString read fLineSeparators write SetLineSeparators; // ###0.941

    // paired line separator (like \r\n in DOS and Windows).
    // must contain exactly two chars or no chars at all
    property LinePairedSeparator: RegExprString read GetLinePairedSeparator write SetLinePairedSeparator; // ###0.941

    // Set this property if you want to override case-insensitive functionality.
    // Create set it to RegExprInvertCaseFunction (InvertCaseFunction by default)
    property InvertCase: TRegExprInvertCaseFunction read fInvertCase write fInvertCase; // ##0.935

    // Use OS line end on replace or not. Default is True for backwards compatibility.
    // Set to false to use #10.
    property UseOsLineEndOnReplace: boolean read FUseOsLineEndOnReplace write SetUseOsLineEndOnReplace;

    property SlowChecksSizeMax: integer read fSlowChecksSizeMax write fSlowChecksSizeMax;
  end;

type
  ERegExpr = class(Exception)
  public
    ErrorCode: integer;
    CompilerErrorPos: PtrInt;
  end;

const
  RegExprInvertCaseFunction: TRegExprInvertCaseFunction = nil;

  // true if string AInputString match regular expression ARegExpr
  // ! will raise exeption if syntax errors in ARegExpr
function ExecRegExpr(const ARegExpr, AInputStr: RegExprString): boolean;

// Split AInputStr into APieces by r.e. ARegExpr occurencies
procedure SplitRegExpr(const ARegExpr, AInputStr: RegExprString;
  APieces: TStrings);

// Returns AInputStr with r.e. occurencies replaced by AReplaceStr
// If AUseSubstitution is true, then AReplaceStr will be used
// as template for Substitution methods.
// For example:
// ReplaceRegExpr ('({-i}block|var)\s*\(\s*([^ ]*)\s*\)\s*',
// 'BLOCK( test1)', 'def "$1" value "$2"', True)
// will return:  def 'BLOCK' value 'test1'
// ReplaceRegExpr ('({-i}block|var)\s*\(\s*([^ ]*)\s*\)\s*',
// 'BLOCK( test1)', 'def "$1" value "$2"')
// will return:  def "$1" value "$2"
function ReplaceRegExpr(const ARegExpr, AInputStr, AReplaceStr: RegExprString;
  AUseSubstitution: boolean{$IFDEF DefParam} = False{$ENDIF}): RegExprString;
{$IFDEF OverMeth}overload; // ###0.947

// Alternate form allowing to set more parameters.

type
  TRegexReplaceOption = (
    rroModifierI,
    rroModifierR,
    rroModifierS,
    rroModifierG,
    rroModifierM,
    rroModifierX,
    rroUseSubstitution,
    rroUseOsLineEnd
    );
  TRegexReplaceOptions = set of TRegexReplaceOption;

function ReplaceRegExpr(const ARegExpr, AInputStr, AReplaceStr: RegExprString;
  Options: TRegexReplaceOptions): RegExprString; overload;
{$ENDIF}
// Replace all metachars with its safe representation,
// for example 'abc$cd.(' converts into 'abc\$cd\.\('
// This function useful for r.e. autogeneration from
// user input
function QuoteRegExprMetaChars(const AStr: RegExprString): RegExprString;
// Makes list of subexpressions found in ARegExpr r.e.
// In ASubExps every item represent subexpression,
// from first to last, in format:
// String - subexpression text (without '()')
// low word of Object - starting position in ARegExpr, including '('
// if exists! (first position is 1)
// high word of Object - length, including starting '(' and ending ')'
// if exist!
// AExtendedSyntax - must be True if modifier /m will be On while
// using the r.e.
// Useful for GUI editors of r.e. etc (You can find example of using
// in TestRExp.dpr project)
// Returns
// 0      Success. No unbalanced brackets was found;
// -1     There are not enough closing brackets ')';
// -(n+1) At position n was found opening '[' without  //###0.942
// corresponding closing ']';
// n      At position n was found closing bracket ')' without
// corresponding opening '('.
// If Result <> 0, then ASubExpr can contain empty items or illegal ones
function RegExprSubExpressions(const ARegExpr: string; ASubExprs: TStrings;
  AExtendedSyntax: boolean{$IFDEF DefParam} = False{$ENDIF}): integer;

implementation

{$IFDEF FPC}
{$IFDEF UnicodeWordDetection}
uses
  UnicodeData;
{$ENDIF}
{$ELSE}
{$IFDEF SYN_DELPHI_2009_UP}
uses
  // unit exists since Delphi 2009
  {$IFDEF SYN_DELPHI_XE2_UP}
  System.Character;
  {$ELSE}
  Character;
  {$ENDIF}
{$ENDIF}
{$ENDIF}

const
  // TRegExpr.VersionMajor/Minor return values of these constants:
  REVersionMajor = 0;
  REVersionMinor = 989;

  OpKind_End = REChar(1);
  OpKind_MetaClass = REChar(2);
  OpKind_Range = REChar(3);
  OpKind_Char = REChar(4);

  RegExprAllSet = [0 .. 255];
  RegExprWordSet = [Ord('a') .. Ord('z'), Ord('A') .. Ord('Z'), Ord('0') .. Ord('9'), Ord('_')];
  RegExprDigitSet = [Ord('0') .. Ord('9')];
  RegExprLowerAzSet = [Ord('a') .. Ord('z')];
  RegExprUpperAzSet = [Ord('A') .. Ord('Z')];
  RegExprAllAzSet = RegExprLowerAzSet + RegExprUpperAzSet;
  RegExprSpaceSet = [Ord(' '), $9, $A, $D, $C];
  RegExprLineSeparatorsSet = [$d, $a, $b, $c] {$IFDEF UniCode} + [$85] {$ENDIF};
  RegExprHorzSeparatorsSet = [9, $20, $A0];

  MaxBracesArg = $7FFFFFFF - 1; // max value for {n,m} arguments //###0.933

type
  TRENextOff = PtrInt;
  // internal Next "pointer" (offset to current p-code) //###0.933
  PRENextOff = ^TRENextOff;
  // used for extracting Next "pointers" from compiled r.e. //###0.933
  TREBracesArg = integer; // type of {m,n} arguments
  PREBracesArg = ^TREBracesArg;

const
  REOpSz = SizeOf(TREOp) div SizeOf(REChar);
  // size of OP_ command in REChars
  {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  // add space for aligning pointer
  // -1 is the correct max size but also needed for InsertOperator that needs a multiple of pointer size
  RENextOffSz = (2 * SizeOf(TRENextOff) div SizeOf(REChar)) - 1;
  REBracesArgSz = (2 * SizeOf(TREBracesArg) div SizeOf(REChar));
  // add space for aligning pointer
  {$ELSE}
  RENextOffSz = (SizeOf(TRENextOff) div SizeOf(REChar));
  // size of Next pointer in REChars
  REBracesArgSz = SizeOf(TREBracesArg) div SizeOf(REChar);
  // size of BRACES arguments in REChars
  {$ENDIF}
  RENumberSz = SizeOf(LongInt) div SizeOf(REChar);

function _FindCharInBuffer(SBegin, SEnd: PRegExprChar; Ch: REChar): PRegExprChar; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  while SBegin < SEnd do
  begin
    if SBegin^ = Ch then
    begin
      Result := SBegin;
      Exit;
    end;
    Inc(SBegin);
  end;
  Result := nil;
end;

function IsIgnoredChar(AChar: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    ' ', #9, #$d, #$a:
      Result := True
    else
      Result := False;
  end;
end;

function _IsMetaChar(AChar: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    'd', 'D',
    's', 'S',
    'w', 'W',
    'v', 'V',
    'h', 'H':
      Result := True
    else
      Result := False;
  end;
end;

function AlignToPtr(const p: Pointer): Pointer; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(Pointer));
  {$ELSE}
  Result := p;
  {$ENDIF}
end;

function AlignToInt(const p: Pointer): Pointer; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(integer));
  {$ELSE}
  Result := p;
  {$ENDIF}
end;

function _UpperCase(Ch: REChar): REChar;
begin
  Result := Ch;
  if (Ch >= 'a') and (Ch <= 'z') then
  begin
    Dec(Result, 32);
    Exit;
  end;
  if Ord(Ch) < 128 then
    Exit;

  {$IFDEF FPC}
    {$IFDEF UniCode}
    Result := UnicodeUpperCase(Ch)[1];
    {$ELSE}
    Result := AnsiUpperCase(Ch)[1];
    {$ENDIF}
  {$ELSE}
    {$IFDEF UniCode}
    {$IFDEF SYN_DELPHI_2009_UP}
    Result := TCharacter.ToUpper(Ch);
    {$ENDIF}
    {$ELSE}
    Result := AnsiUpperCase(Ch)[1];
    {$ENDIF}
  {$ENDIF}
end;

function _LowerCase(Ch: REChar): REChar;
begin
  Result := Ch;
  if (Ch >= 'A') and (Ch <= 'Z') then
  begin
    Inc(Result, 32);
    Exit;
  end;
  if Ord(Ch) < 128 then
    Exit;

  {$IFDEF FPC}
    {$IFDEF UniCode}
    Result := UnicodeLowerCase(Ch)[1];
    {$ELSE}
    Result := AnsiLowerCase(Ch)[1];
    {$ENDIF}
  {$ELSE}
    {$IFDEF UniCode}
    {$IFDEF SYN_DELPHI_2009_UP}
    Result := TCharacter.ToLower(Ch);
    {$ENDIF}
    {$ELSE}
    Result := AnsiLowerCase(Ch)[1];
    {$ENDIF}
  {$ENDIF}
end;

{ ============================================================= }
{ ===================== Global functions ====================== }
{ ============================================================= }

function IsModifiersEqual(const A, B: TRegExprModifiers): boolean;
begin
  Result :=
    (A.I = B.I) and
    (A.G = B.G) and
    (A.M = B.M) and
    (A.S = B.S) and
    (A.R = B.R) and
    (A.X = B.X);
end;

function ParseModifiers(const APtr: PRegExprChar;
  ALen: integer;
  var AValue: TRegExprModifiers): boolean;
// Parse string and set AValue if it's in format 'ismxrg-ismxrg'
var
  IsOn: boolean;
  i: integer;
begin
  Result := True;
  IsOn := True;
  for i := 0 to ALen-1 do
    case APtr[i] of
      '-':
        IsOn := False;
      'I', 'i':
        AValue.I := IsOn;
      'R', 'r':
        AValue.R := IsOn;
      'S', 's':
        AValue.S := IsOn;
      'G', 'g':
        AValue.G := IsOn;
      'M', 'm':
        AValue.M := IsOn;
      'X', 'x':
        AValue.X := IsOn;
    else
      begin
        Result := False;
        Exit;
      end;
    end;
end;

function ExecRegExpr(const ARegExpr, AInputStr: RegExprString): boolean;
var
  r: TRegExpr;
begin
  r := TRegExpr.Create;
  try
    r.Expression := ARegExpr;
    Result := r.Exec(AInputStr);
  finally
    r.Free;
  end;
end; { of function ExecRegExpr
  -------------------------------------------------------------- }

procedure SplitRegExpr(const ARegExpr, AInputStr: RegExprString;
  APieces: TStrings);
var
  r: TRegExpr;
begin
  APieces.Clear;
  r := TRegExpr.Create;
  try
    r.Expression := ARegExpr;
    r.Split(AInputStr, APieces);
  finally
    r.Free;
  end;
end; { of procedure SplitRegExpr
  -------------------------------------------------------------- }

function ReplaceRegExpr(const ARegExpr, AInputStr, AReplaceStr: RegExprString;
  AUseSubstitution: boolean{$IFDEF DefParam} = False{$ENDIF}): RegExprString;
begin
  with TRegExpr.Create do
    try
      Expression := ARegExpr;
      Result := Replace(AInputStr, AReplaceStr, AUseSubstitution);
    finally
      Free;
    end;
end; { of function ReplaceRegExpr
  -------------------------------------------------------------- }
{$IFDEF OverMeth}

function ReplaceRegExpr(const ARegExpr, AInputStr, AReplaceStr: RegExprString;
  Options: TRegexReplaceOptions): RegExprString; overload;

begin
  with TRegExpr.Create do
    try
      ModifierI := (rroModifierI in Options);
      ModifierR := (rroModifierR in Options);
      ModifierS := (rroModifierS in Options);
      ModifierG := (rroModifierG in Options);
      ModifierM := (rroModifierM in Options);
      ModifierX := (rroModifierX in Options);
      // Set this after the above, if the regex contains modifiers, they will be applied.
      Expression := ARegExpr;
      UseOsLineEndOnReplace := (rroUseOsLineEnd in Options);
      Result := Replace(AInputStr, AReplaceStr, rroUseSubstitution in Options);
    finally
      Free;
    end;
end;
{$ENDIF}

(*
const
  MetaChars_Init = '^$.[()|?+*' + EscChar + '{';
  MetaChars = MetaChars_Init; // not needed to be a variable, const is faster
  MetaAll = MetaChars_Init + ']}'; // Very similar to MetaChars, but slighly changed.
*)

function _IsMetaSymbol1(ch: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case ch of
    '^', '$', '.', '[', '(', ')', '|', '?', '+', '*', EscChar, '{':
      Result := True
    else
      Result := False
  end;
end;

function _IsMetaSymbol2(ch: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case ch of
    '^', '$', '.', '[', '(', ')', '|', '?', '+', '*', EscChar, '{',
    ']', '}':
      Result := True
    else
      Result := False
  end;
end;

function QuoteRegExprMetaChars(const AStr: RegExprString): RegExprString;
var
  i, i0, Len: integer;
  ch: REChar;
begin
  Result := '';
  Len := Length(AStr);
  i := 1;
  i0 := i;
  while i <= Len do
  begin
    ch := AStr[i];
    if _IsMetaSymbol2(ch) then
    begin
      Result := Result + System.Copy(AStr, i0, i - i0) + EscChar + ch;
      i0 := i + 1;
    end;
    Inc(i);
  end;
  Result := Result + System.Copy(AStr, i0, MaxInt); // Tail
end; { of function QuoteRegExprMetaChars
  -------------------------------------------------------------- }

function RegExprSubExpressions(const ARegExpr: string; ASubExprs: TStrings;
  AExtendedSyntax: boolean{$IFDEF DefParam} = False{$ENDIF}): integer;
type
  TStackItemRec = record // ###0.945
    SubExprIdx: integer;
    StartPos: PtrInt;
  end;

  TStackArray = packed array [0 .. NSUBEXPMAX - 1] of TStackItemRec;
var
  Len, SubExprLen: integer;
  i, i0: integer;
  Modif: TRegExprModifiers;
  Stack: ^TStackArray; // ###0.945
  StackIdx, StackSz: integer;
begin
  Result := 0; // no unbalanced brackets found at this very moment
  FillChar(Modif, SizeOf(Modif), 0);
  ASubExprs.Clear; // I don't think that adding to non empty list
  // can be useful, so I simplified algorithm to work only with empty list

  Len := Length(ARegExpr); // some optimization tricks

  // first we have to calculate number of subexpression to reserve
  // space in Stack array (may be we'll reserve more than needed, but
  // it's faster then memory reallocation during parsing)
  StackSz := 1; // add 1 for entire r.e.
  for i := 1 to Len do
    if ARegExpr[i] = '(' then
      Inc(StackSz);
  // SetLength (Stack, StackSz); //###0.945
  GetMem(Stack, SizeOf(TStackItemRec) * StackSz);

  try
    StackIdx := 0;
    i := 1;
    while (i <= Len) do
    begin
      case ARegExpr[i] of
        '(':
          begin
            if (i < Len) and (ARegExpr[i + 1] = '?') then
            begin
              // this is not subexpression, but comment or other
              // Perl extension. We must check is it (?ismxrg-ismxrg)
              // and change AExtendedSyntax if /x is changed.
              Inc(i, 2); // skip '(?'
              i0 := i;
              while (i <= Len) and (ARegExpr[i] <> ')') do
                Inc(i);
              if i > Len then
                Result := -1 // unbalansed '('
              else
              if ParseModifiers(@ARegExpr[i0], i - i0, Modif) then
                // Alexey-T: original code had copy from i, not from i0
                AExtendedSyntax := Modif.X;
            end
            else
            begin // subexpression starts
              ASubExprs.Add(''); // just reserve space
              with Stack[StackIdx] do
              begin
                SubExprIdx := ASubExprs.Count - 1;
                StartPos := i;
              end;
              Inc(StackIdx);
            end;
          end;
        ')':
          begin
            if StackIdx = 0 then
              Result := i // unbalanced ')'
            else
            begin
              Dec(StackIdx);
              with Stack[StackIdx] do
              begin
                SubExprLen := i - StartPos + 1;
                ASubExprs.Objects[SubExprIdx] :=
                  TObject(StartPos or (SubExprLen ShL 16));
                ASubExprs[SubExprIdx] := System.Copy(ARegExpr, StartPos + 1,
                  SubExprLen - 2); // add without brackets
              end;
            end;
          end;
        EscChar:
          Inc(i); // skip quoted symbol
        '[':
          begin
            // we have to skip character ranges at once, because they can
            // contain '#', and '#' in it must NOT be recognized as eXtended
            // comment beginning!
            i0 := i;
            Inc(i);
            if ARegExpr[i] = ']' // first ']' inside [] treated as simple char, no need to check '['
            then
              Inc(i);
            while (i <= Len) and (ARegExpr[i] <> ']') do
              if ARegExpr[i] = EscChar // ###0.942
              then
                Inc(i, 2) // skip 'escaped' char to prevent stopping at '\]'
              else
                Inc(i);
            if (i > Len) or (ARegExpr[i] <> ']') // ###0.942
            then
              Result := -(i0 + 1); // unbalansed '[' //###0.942
          end;
        '#':
          if AExtendedSyntax then
          begin
            // skip eXtended comments
            while (i <= Len) and (ARegExpr[i] <> #$d) and (ARegExpr[i] <> #$a)
            // do not use [#$d, #$a] due to UniCode compatibility
              do
              Inc(i);
            while (i + 1 <= Len) and
              ((ARegExpr[i + 1] = #$d) or (ARegExpr[i + 1] = #$a)) do
              Inc(i); // attempt to work with different kinds of line separators
            // now we are at the line separator that must be skipped.
          end;
        // here is no 'else' clause - we simply skip ordinary chars
      end; // of case
      Inc(i); // skip scanned char
      // ! can move after Len due to skipping quoted symbol
    end;

    // check brackets balance
    if StackIdx <> 0 then
      Result := -1; // unbalansed '('

    // check if entire r.e. added
    if (ASubExprs.Count = 0) or ((PtrInt(ASubExprs.Objects[0]) and $FFFF) <> 1)
      or (((PtrInt(ASubExprs.Objects[0]) ShR 16) and $FFFF) <> Len)
    // whole r.e. wasn't added because it isn't bracketed
    // well, we add it now:
    then
      ASubExprs.InsertObject(0, ARegExpr, TObject((Len ShL 16) or 1));

  finally
    FreeMem(Stack);
  end;
end; { of function RegExprSubExpressions
  -------------------------------------------------------------- }

const
  OP_MAGIC = TREOp(216); // programm signature

  // name            opcode    opnd? meaning
  OP_EEND = TREOp(0); // -    End of program
  OP_BOL = TREOp(1); // -    Match "" at beginning of line
  OP_EOL = TREOp(2); // -    Match "" at end of line
  OP_ANY = TREOp(3); // -    Match any one character
  OP_ANYOF = TREOp(4); // Str  Match any character in string Str
  OP_ANYBUT = TREOp(5); // Str  Match any char. not in string Str
  OP_BRANCH = TREOp(6); // Node Match this alternative, or the next
  OP_BACK = TREOp(7); // -    Jump backward (Next < 0)
  OP_EXACTLY = TREOp(8); // Str  Match string Str
  OP_NOTHING = TREOp(9); // -    Match empty string
  OP_STAR = TREOp(10); // Node Match this (simple) thing 0 or more times
  OP_PLUS = TREOp(11); // Node Match this (simple) thing 1 or more times
  OP_ANYDIGIT = TREOp(12); // -    Match any digit (equiv [0-9])
  OP_NOTDIGIT = TREOp(13); // -    Match not digit (equiv [0-9])
  OP_ANYLETTER = TREOp(14); // -    Match any letter from property WordChars
  OP_NOTLETTER = TREOp(15); // -    Match not letter from property WordChars
  OP_ANYSPACE = TREOp(16); // -    Match any space char (see property SpaceChars)
  OP_NOTSPACE = TREOp(17); // -    Match not space char (see property SpaceChars)
  OP_BRACES = TREOp(18);
  // Node,Min,Max Match this (simple) thing from Min to Max times.
  // Min and Max are TREBracesArg
  OP_COMMENT = TREOp(19); // -    Comment ;)
  OP_EXACTLYCI = TREOp(20); // Str  Match string Str case insensitive
  OP_ANYOFCI = TREOp(21);
  // Str  Match any character in string Str, case insensitive
  OP_ANYBUTCI = TREOp(22);
  // Str  Match any char. not in string Str, case insensitive
  OP_LOOPENTRY = TREOp(23); // Node Start of loop (Node - LOOP for this loop)
  OP_LOOP = TREOp(24); // Node,Min,Max,LoopEntryJmp - back jump for LOOPENTRY.
  // Min and Max are TREBracesArg
  // Node - next node in sequence,
  // LoopEntryJmp - associated LOOPENTRY node addr
  OP_BSUBEXP = TREOp(28);
  // Idx  Match previously matched subexpression #Idx (stored as REChar) //###0.936
  OP_BSUBEXPCI = TREOp(29); // Idx  -"- in case-insensitive mode

  // Non-Greedy Style Ops //###0.940
  OP_STARNG = TREOp(30); // Same as OP_START but in non-greedy mode
  OP_PLUSNG = TREOp(31); // Same as OP_PLUS but in non-greedy mode
  OP_BRACESNG = TREOp(32); // Same as OP_BRACES but in non-greedy mode
  OP_LOOPNG = TREOp(33); // Same as OP_LOOP but in non-greedy mode

  // Multiline mode \m
  OP_BOLML = TREOp(34); // -    Match "" at beginning of line
  OP_EOLML = TREOp(35); // -    Match "" at end of line
  OP_ANYML = TREOp(36); // -    Match any one character

  // Word boundary
  OP_BOUND = TREOp(37); // Match "" between words //###0.943
  OP_NOTBOUND = TREOp(38); // Match "" not between words //###0.943

  OP_ANYHORZSEP = TREOp(39); // Any horizontal whitespace \h
  OP_NOTHORZSEP = TREOp(40); // Not horizontal whitespace \H
  OP_ANYVERTSEP = TREOp(41); // Any vertical whitespace \v
  OP_NOTVERTSEP = TREOp(42); // Not vertical whitespace \V

  // !!! Change OP_OPEN value if you add new opcodes !!!

  OP_OPEN = TREOp(43); // -    Mark this point in input as start of \n
  // OP_OPEN + 1 is \1, etc.
  OP_CLOSE = TREOp(Ord(OP_OPEN) + NSUBEXP);
  // -    Analogous to OP_OPEN.

  // !!! Don't add new OpCodes after CLOSE !!!

  // We work with p-code through pointers, compatible with PRegExprChar.
  // Note: all code components (TRENextOff, TREOp, TREBracesArg, etc)
  // must have lengths that can be divided by SizeOf (REChar) !
  // A node is TREOp of opcode followed Next "pointer" of TRENextOff type.
  // The Next is a offset from the opcode of the node containing it.
  // An operand, if any, simply follows the node. (Note that much of
  // the code generation knows about this implicit relationship!)
  // Using TRENextOff=PtrInt speed up p-code processing.

  // Opcodes description:
  //
  // BRANCH The set of branches constituting a single choice are hooked
  // together with their "next" pointers, since precedence prevents
  // anything being concatenated to any individual branch. The
  // "next" pointer of the last BRANCH in a choice points to the
  // thing following the whole choice. This is also where the
  // final "next" pointer of each individual branch points; each
  // branch starts with the operand node of a BRANCH node.
  // BACK Normal "next" pointers all implicitly point forward; BACK
  // exists to make loop structures possible.
  // STAR,PLUS,BRACES '?', and complex '*' and '+', are implemented as
  // circular BRANCH structures using BACK. Complex '{min,max}'
  // - as pair LOOPENTRY-LOOP (see below). Simple cases (one
  // character per match) are implemented with STAR, PLUS and
  // BRACES for speed and to minimize recursive plunges.
  // LOOPENTRY,LOOP {min,max} are implemented as special pair
  // LOOPENTRY-LOOP. Each LOOPENTRY initialize loopstack for
  // current level.
  // OPEN,CLOSE are numbered at compile time.

  { ============================================================= }
  { ================== Error handling section =================== }
  { ============================================================= }

const
  reeOk = 0;
  reeCompNullArgument = 100;
  reeCompParseRegTooManyBrackets = 102;
  reeCompParseRegUnmatchedBrackets = 103;
  reeCompParseRegUnmatchedBrackets2 = 104;
  reeCompParseRegJunkOnEnd = 105;
  reePlusStarOperandCouldBeEmpty = 106;
  reeNestedSQP = 107;
  reeBadHexDigit = 108;
  reeInvalidRange = 109;
  reeParseAtomTrailingBackSlash = 110;
  reeNoHexCodeAfterBSlashX = 111;
  reeHexCodeAfterBSlashXTooBig = 112;
  reeUnmatchedSqBrackets = 113;
  reeInternalUrp = 114;
  reeQPSBFollowsNothing = 115;
  reeTrailingBackSlash = 116;
  reeNoLetterAfterBSlashC = 117;
  reeMetaCharAfterMinusInRange = 118;
  reeRarseAtomInternalDisaster = 119;
  reeIncorrectBraces = 121;
  reeBRACESArgTooBig = 122;
  reeUnknownOpcodeInFillFirst = 123;
  reeBracesMinParamGreaterMax = 124;
  reeUnclosedComment = 125;
  reeComplexBracesNotImplemented = 126;
  reeUnrecognizedModifier = 127;
  reeBadLinePairedSeparator = 128;
  // Runtime errors must be >= 1000
  reeRegRepeatCalledInappropriately = 1000;
  reeMatchPrimMemoryCorruption = 1001;
  reeMatchPrimCorruptedPointers = 1002;
  reeNoExpression = 1003;
  reeCorruptedProgram = 1004;
  //reeNoInputStringSpecified = 1005;
  reeOffsetMustBePositive = 1006;
  reeExecNextWithoutExec = 1007;
  reeBadOpcodeInCharClass = 1008;
  reeDumpCorruptedOpcode = 1011;
  reeModifierUnsupported = 1013;
  reeLoopStackExceeded = 1014;
  reeLoopWithoutEntry = 1015;

function TRegExpr.ErrorMsg(AErrorID: integer): RegExprString;
begin
  case AErrorID of
    reeOk:
      Result := 'No errors';
    reeCompNullArgument:
      Result := 'TRegExpr compile: null argument';
    reeCompParseRegTooManyBrackets:
      Result := 'TRegExpr compile: ParseReg: too many ()';
    reeCompParseRegUnmatchedBrackets:
      Result := 'TRegExpr compile: ParseReg: unmatched ()';
    reeCompParseRegUnmatchedBrackets2:
      Result := 'TRegExpr compile: ParseReg: unmatched ()';
    reeCompParseRegJunkOnEnd:
      Result := 'TRegExpr compile: ParseReg: junk at end';
    reePlusStarOperandCouldBeEmpty:
      Result := 'TRegExpr compile: *+ operand could be empty';
    reeNestedSQP:
      Result := 'TRegExpr compile: nested *?+';
    reeBadHexDigit:
      Result := 'TRegExpr compile: bad hex digit';
    reeInvalidRange:
      Result := 'TRegExpr compile: invalid [] range';
    reeParseAtomTrailingBackSlash:
      Result := 'TRegExpr compile: parse atom trailing \';
    reeNoHexCodeAfterBSlashX:
      Result := 'TRegExpr compile: no hex code after \x';
    reeNoLetterAfterBSlashC:
      Result := 'TRegExpr compile: no letter "A".."Z" after \c';
    reeMetaCharAfterMinusInRange:
      Result := 'TRegExpr compile: metachar after "-" in [] range';
    reeHexCodeAfterBSlashXTooBig:
      Result := 'TRegExpr compile: hex code after \x is too big';
    reeUnmatchedSqBrackets:
      Result := 'TRegExpr compile: unmatched []';
    reeInternalUrp:
      Result := 'TRegExpr compile: internal fail on char "|", ")"';
    reeQPSBFollowsNothing:
      Result := 'TRegExpr compile: ?+*{ follows nothing';
    reeTrailingBackSlash:
      Result := 'TRegExpr compile: trailing \';
    reeRarseAtomInternalDisaster:
      Result := 'TRegExpr compile: RarseAtom internal disaster';
    reeIncorrectBraces:
      Result := 'TRegExpr compile: incorrect {} braces';
    reeBRACESArgTooBig:
      Result := 'TRegExpr compile: braces {} argument too big';
    reeUnknownOpcodeInFillFirst:
      Result := 'TRegExpr compile: unknown opcode in FillFirstCharSet ('+DumpOp(fLastErrorOpcode)+')';
    reeBracesMinParamGreaterMax:
      Result := 'TRegExpr compile: braces {} min param greater then max';
    reeUnclosedComment:
      Result := 'TRegExpr compile: unclosed (?#comment)';
    reeComplexBracesNotImplemented:
      Result := 'TRegExpr compile: if you use braces {} and non-greedy ops *?, +?, ?? for complex cases, enable {$DEFINE ComplexBraces}';
    reeUnrecognizedModifier:
      Result := 'TRegExpr compile: unrecognized modifier';
    reeBadLinePairedSeparator:
      Result := 'TRegExpr compile: LinePairedSeparator must countain two different chars or be empty';

    reeRegRepeatCalledInappropriately:
      Result := 'TRegExpr exec: RegRepeat called inappropriately';
    reeMatchPrimMemoryCorruption:
      Result := 'TRegExpr exec: MatchPrim memory corruption';
    reeMatchPrimCorruptedPointers:
      Result := 'TRegExpr exec: MatchPrim corrupted pointers';
    reeNoExpression:
      Result := 'TRegExpr exec: empty expression';
    reeCorruptedProgram:
      Result := 'TRegExpr exec: corrupted opcode (no magic byte)';
    //reeNoInputStringSpecified:
    //  Result := 'TRegExpr exec: empty input string';
    reeOffsetMustBePositive:
      Result := 'TRegExpr exec: offset must be >0';
    reeExecNextWithoutExec:
      Result := 'TRegExpr exec: ExecNext without Exec(Pos)';
    reeBadOpcodeInCharClass:
      Result := 'TRegExpr exec: invalid opcode in char class';
    reeDumpCorruptedOpcode:
      Result := 'TRegExpr dump: corrupted opcode';
    reeLoopStackExceeded:
      Result := 'TRegExpr exec: loop stack exceeded';
    reeLoopWithoutEntry:
      Result := 'TRegExpr exec: loop without loop entry';
  else
    Result := 'Unknown error';
  end;
end; { of procedure TRegExpr.Error
  -------------------------------------------------------------- }

function TRegExpr.LastError: integer;
begin
  Result := fLastError;
  fLastError := reeOk;
end; { of function TRegExpr.LastError
  -------------------------------------------------------------- }

{ ============================================================= }
{ ===================== Common section ======================== }
{ ============================================================= }

class function TRegExpr.VersionMajor: integer;
begin
  Result := REVersionMajor;
end;

class function TRegExpr.VersionMinor: integer;
begin
  Result := REVersionMinor;
end;

constructor TRegExpr.Create;
begin
  inherited;
  programm := nil;
  fExpression := '';
  fInputString := '';

  regexpBegin := nil;
  regexpIsCompiled := False;

  FillChar(fModifiers, SIzeOf(fModifiers), 0);
  ModifierI := RegExprModifierI;
  ModifierR := RegExprModifierR;
  ModifierS := RegExprModifierS;
  ModifierG := RegExprModifierG;
  ModifierM := RegExprModifierM;
  ModifierX := RegExprModifierX;

  {$IFDEF UseSpaceChars}
  SpaceChars := RegExprSpaceChars; // ###0.927
  {$ENDIF}
  {$IFDEF UseWordChars}
  WordChars := RegExprWordChars; // ###0.929
  {$ENDIF}
  fInvertCase := RegExprInvertCaseFunction; // ###0.927

  fLineSeparators := RegExprLineSeparators; // ###0.941
  LinePairedSeparator := RegExprLinePairedSeparator; // ###0.941

  FUseOsLineEndOnReplace := True;
  FReplaceLineEnd := sLineBreak;

  {$IFDEF UnicodeWordDetection}
  FUseUnicodeWordDetection := True;
  {$ENDIF}

  fSlowChecksSizeMax := 2000;

  InitLineSepArray;
  InitCharCheckers;
end; { of constructor TRegExpr.Create
  -------------------------------------------------------------- }

{$IFDEF OverMeth}
constructor TRegExpr.Create(const AExpression: RegExprString);
begin
  Create;
  Expression := AExpression;
end;
{$ENDIF}

destructor TRegExpr.Destroy;
begin
  if programm <> nil then
  begin
    FreeMem(programm);
    programm := nil;
  end;
end; { of destructor TRegExpr.Destroy
  -------------------------------------------------------------- }

class function TRegExpr.InvertCaseFunction(const Ch: REChar): REChar;
begin
  Result := Ch;
  if (Ch >= 'a') and (Ch <= 'z') then
  begin
    Dec(Result, 32);
    Exit;
  end;
  if (Ch >= 'A') and (Ch <= 'Z') then
  begin
    Inc(Result, 32);
    Exit;
  end;
  if Ord(Ch) < 128 then
    Exit;

  {$IFDEF FPC}
  Result := _UpperCase(Ch);
  if Result = Ch then
    Result := _LowerCase(Ch);
  {$ELSE}
  {$IFDEF UniCode}
  {$IFDEF SYN_DELPHI_2009_UP}
  if TCharacter.IsUpper(Ch) then
    Result := TCharacter.ToLower(Ch)
  else
    Result := TCharacter.ToUpper(Ch);
  {$ENDIF}
  {$ELSE}
  Result := _UpperCase(Ch);
  if Result = Ch then
    Result := _LowerCase(Ch);
  {$ENDIF}
  {$ENDIF}
end; { of function TRegExpr.InvertCaseFunction
  -------------------------------------------------------------- }

procedure TRegExpr.SetExpression(const AStr: RegExprString);
begin
  if (AStr <> fExpression) or not regexpIsCompiled then
  begin
    regexpIsCompiled := False;
    fExpression := AStr;
    UniqueString(fExpression);
    fRegexStart := PRegExprChar(fExpression);
    fRegexEnd := fRegexStart + Length(fExpression);
    InvalidateProgramm; // ###0.941
  end;
end; { of procedure TRegExpr.SetExpression
  -------------------------------------------------------------- }

function TRegExpr.GetSubExprCount: integer;
begin
  // if nothing found, we must return -1 per TRegExpr docs
  if startp[0] = nil then
    Result := -1
  else
    Result := GrpCount;
end;

function TRegExpr.GetMatchPos(Idx: integer): PtrInt;
begin
  Idx := GrpIndexes[Idx];
  if (Idx >= 0) and (startp[Idx] <> nil) then
    Result := startp[Idx] - fInputStart + 1
  else
    Result := -1;
end; { of function TRegExpr.GetMatchPos
  -------------------------------------------------------------- }

function TRegExpr.GetMatchLen(Idx: integer): PtrInt;
begin
  Idx := GrpIndexes[Idx];
  if (Idx >= 0) and (startp[Idx] <> nil) then
    Result := endp[Idx] - startp[Idx]
  else
    Result := -1;
end; { of function TRegExpr.GetMatchLen
  -------------------------------------------------------------- }

function TRegExpr.GetMatch(Idx: integer): RegExprString;
begin
  Result := '';
  Idx := GrpIndexes[Idx];
  if (Idx >= 0) and (endp[Idx] > startp[Idx]) then
    SetString(Result, startp[Idx], endp[Idx] - startp[Idx]);
  {
  // then Result := copy (fInputString, MatchPos [Idx], MatchLen [Idx]) //###0.929
  then
  begin
    SetLength(Result, endp[Idx] - startp[Idx]);
    System.Move(startp[Idx]^, Result[1], Length(Result) * SizeOf(REChar));
  end;
  }
end; { of function TRegExpr.GetMatch
  -------------------------------------------------------------- }

function TRegExpr.GetModifierStr: RegExprString;
begin
  Result := '-';

  if ModifierI then
    Result := 'i' + Result
  else
    Result := Result + 'i';
  if ModifierR then
    Result := 'r' + Result
  else
    Result := Result + 'r';
  if ModifierS then
    Result := 's' + Result
  else
    Result := Result + 's';
  if ModifierG then
    Result := 'g' + Result
  else
    Result := Result + 'g';
  if ModifierM then
    Result := 'm' + Result
  else
    Result := Result + 'm';
  if ModifierX then
    Result := 'x' + Result
  else
    Result := Result + 'x';

  if Result[Length(Result)] = '-' // remove '-' if all modifiers are 'On'
  then
    System.Delete(Result, Length(Result), 1);
end; { of function TRegExpr.GetModifierStr
  -------------------------------------------------------------- }

procedure TRegExpr.SetModifierG(AValue: boolean);
begin
  fModifiers.G := AValue;
end;

procedure TRegExpr.SetModifierI(AValue: boolean);
begin
  fModifiers.I := AValue;
end;

procedure TRegExpr.SetModifierM(AValue: boolean);
begin
  fModifiers.M := AValue;
end;

procedure TRegExpr.SetModifierR(AValue: boolean);
begin
  fModifiers.R := AValue;
end;

procedure TRegExpr.SetModifierS(AValue: boolean);
begin
  fModifiers.S := AValue;
end;

procedure TRegExpr.SetModifierX(AValue: boolean);
begin
  fModifiers.X := AValue;
end;

procedure TRegExpr.SetModifierStr(const AStr: RegExprString);
begin
  if not ParseModifiers(PRegExprChar(AStr), Length(AStr), fModifiers) then
    Error(reeModifierUnsupported);
end; { of procedure TRegExpr.SetModifierStr
  -------------------------------------------------------------- }

{ ============================================================= }
{ ==================== Compiler section ======================= }
{ ============================================================= }

{$IFDEF UnicodeWordDetection}
  {$IFDEF FPC}
  function IsUnicodeWordChar(AChar: WideChar): boolean; inline;
  var
    NType: byte;
  begin
    if Ord(AChar) >= LOW_SURROGATE_BEGIN then
      Exit(False);
    NType := GetProps(Ord(AChar))^.Category;
    Result := (NType <= UGC_OtherNumber);
  end;
  {$ELSE}
  function IsUnicodeWordChar(AChar: WideChar): boolean; inline;
  begin
    Result := System.Character.IsLetterOrDigit(AChar);
  end;
  {$ENDIF}
{$ENDIF}

function TRegExpr.IsWordChar(AChar: REChar): boolean;
begin
  {$IFDEF UseWordChars}
  Result := Pos(AChar, fWordChars) > 0;
  {$ELSE}
  case AChar of
    'a' .. 'z',
    'A' .. 'Z',
    '0' .. '9', '_':
      Result := True
    else
      Result := False;
  end;
  {$ENDIF}
  {$IFDEF UnicodeWordDetection}
  if not Result and (Ord(AChar) >= 128) and UseUnicodeWordDetection then
    Result := IsUnicodeWordChar(AChar);
  {$ENDIF}
end;

function TRegExpr.IsSpaceChar(AChar: REChar): boolean;
begin
  {$IFDEF UseSpaceChars}
  Result := Pos(AChar, fSpaceChars) > 0;
  {$ELSE}
  case AChar of
    ' ', #$9, #$A, #$D, #$C:
      Result := True
    else
      Result := False;
  end;
  {$ENDIF}
end;

function TRegExpr.IsCustomLineSeparator(AChar: REChar): boolean;
begin
  {$IFDEF UniCode}
  Result := Pos(AChar, fLineSeparators) > 0;
  {$ELSE}
  Result := fLineSepArray[byte(AChar)];
  {$ENDIF}
end;

function IsDigitChar(AChar: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    '0' .. '9':
      Result := True;
  else
    Result := False;
  end;
end;

function IsHorzSeparator(AChar: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  // Tab and Unicode categoty "Space Separator": https://www.compart.com/en/unicode/category/Zs
  case AChar of
    #9, #$20, #$A0:
      Result := True;
    {$IFDEF UniCode}
    #$1680, #$2000 .. #$200A, #$202F, #$205F, #$3000:
      Result := True;
    {$ENDIF}
  else
    Result := False;
  end;
end;

function IsLineSeparator(AChar: REChar): boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    #$d, #$a, #$b, #$c:
      Result := True;
    {$IFDEF UniCode}
    #$2028, #$2029, #$85:
      Result := True;
    {$ENDIF}
  else
    Result := False;
  end;
end;

procedure TRegExpr.InvalidateProgramm;
begin
  if programm <> nil then
  begin
    FreeMem(programm);
    programm := nil;
  end;
end; { of procedure TRegExpr.InvalidateProgramm
  -------------------------------------------------------------- }

procedure TRegExpr.Compile;
begin
  if fExpression = '' then
  begin
    Error(reeNoExpression);
    Exit;
  end;

  CompileRegExpr(PRegExprChar(fExpression));
end; { of procedure TRegExpr.Compile
  -------------------------------------------------------------- }

procedure TRegExpr.InitLineSepArray;
{$IFNDEF UniCode}
var
  i: integer;
{$ENDIF}
begin
  {$IFNDEF UniCode}
  FillChar(fLineSepArray, SizeOf(fLineSepArray), 0);
  for i := 1 to Length(fLineSeparators) do
    fLineSepArray[byte(fLineSeparators[i])] := True;
  {$ENDIF}
end;

function TRegExpr.IsProgrammOk: boolean;
begin
  Result := False;

  // check modifiers
  if not IsModifiersEqual(fModifiers, fProgModifiers) // ###0.941
  then
    InvalidateProgramm;

  // [Re]compile if needed
  if programm = nil then
  begin
    Compile; // ###0.941
    // Check [re]compiled programm
    if programm = nil then
      Exit; // error was set/raised by Compile (was reeExecAfterCompErr)
  end;

  if programm[0] <> OP_MAGIC // Program corrupted.
  then
    Error(reeCorruptedProgram)
  else
    Result := True;
end; { of function TRegExpr.IsProgrammOk
  -------------------------------------------------------------- }

procedure TRegExpr.Tail(p: PRegExprChar; val: PRegExprChar);
// set the next-pointer at the end of a node chain
var
  scan: PRegExprChar;
  temp: PRegExprChar;
begin
  if p = @regdummy then
    Exit;
  // Find last node.
  scan := p;
  repeat
    temp := regnext(scan);
    if temp = nil then
      Break;
    scan := temp;
  until False;
  // Set Next 'pointer'
  if val < scan then
    PRENextOff(AlignToPtr(scan + REOpSz))^ := -(scan - val) // ###0.948
    // work around PWideChar subtraction bug (Delphi uses
    // shr after subtraction to calculate widechar distance %-( )
    // so, if difference is negative we have .. the "feature" :(
    // I could wrap it in $IFDEF UniCode, but I didn't because
    // "P – Q computes the difference between the address given
    // by P (the higher address) and the address given by Q (the
    // lower address)" - Delphi help quotation.
  else
    PRENextOff(AlignToPtr(scan + REOpSz))^ := val - scan; // ###0.933
end; { of procedure TRegExpr.Tail
  -------------------------------------------------------------- }

procedure TRegExpr.OpTail(p: PRegExprChar; val: PRegExprChar);
// regtail on operand of first argument; nop if operandless
begin
  // "Operandless" and "op != OP_BRANCH" are synonymous in practice.
  if (p = nil) or (p = @regdummy) or (PREOp(p)^ <> OP_BRANCH) then
    Exit;
  Tail(p + REOpSz + RENextOffSz, val); // ###0.933
end; { of procedure TRegExpr.OpTail
  -------------------------------------------------------------- }

function TRegExpr.EmitNode(op: TREOp): PRegExprChar; // ###0.933
// emit a node, return location
begin
  Result := regcode;
  if Result <> @regdummy then
  begin
    PREOp(regcode)^ := op;
    Inc(regcode, REOpSz);
    PRENextOff(AlignToPtr(regcode))^ := 0; // Next "pointer" := nil
    Inc(regcode, RENextOffSz);

    if (op = OP_EXACTLY) or (op = OP_EXACTLYCI) then
      regExactlyLen := PLongInt(regcode)
    else
      regExactlyLen := nil;

    {$IFDEF DebugSynRegExpr}
    if regcode - programm > regsize then
      raise Exception.Create('TRegExpr.EmitNode buffer overrun');
    {$ENDIF}
  end
  else
    Inc(regsize, REOpSz + RENextOffSz);
    // compute code size without code generation
end; { of function TRegExpr.EmitNode
  -------------------------------------------------------------- }

procedure TRegExpr.EmitC(ch: REChar);
begin
  if regcode <> @regdummy then
  begin
    regcode^ := ch;
    Inc(regcode);
    {$IFDEF DebugSynRegExpr}
    if regcode - programm > regsize then
      raise Exception.Create('TRegExpr.EmitC buffer overrun');
    {$ENDIF}
  end
  else
    Inc(regsize, REOpSz); // Type of p-code pointer always is ^REChar
end; { of procedure TRegExpr.EmitC
  -------------------------------------------------------------- }

procedure TRegExpr.EmitInt(AValue: LongInt);
begin
  if regcode <> @regdummy then
  begin
    PLongInt(regcode)^ := AValue;
    Inc(regcode, RENumberSz);
    {$IFDEF DebugSynRegExpr}
    if regcode - programm > regsize then
      raise Exception.Create('TRegExpr.EmitInt buffer overrun');
    {$ENDIF}
  end
  else
    Inc(regsize, RENumberSz);
end;

procedure TRegExpr.InsertOperator(op: TREOp; opnd: PRegExprChar; sz: integer);
// insert an operator in front of already-emitted operand
// Means relocating the operand.
var
  src, dst, place: PRegExprChar;
  i: integer;
begin
  if regcode = @regdummy then
  begin
    Inc(regsize, sz);
    Exit;
  end;
  // move code behind insert position
  src := regcode;
  Inc(regcode, sz);
  {$IFDEF DebugSynRegExpr}
  if regcode - programm > regsize then
    raise Exception.Create('TRegExpr.InsertOperator buffer overrun');
  // if (opnd<regcode) or (opnd-regcode>regsize) then
  // raise Exception.Create('TRegExpr.InsertOperator invalid opnd');
  {$ENDIF}
  dst := regcode;
  while src > opnd do
  begin
    Dec(dst);
    Dec(src);
    dst^ := src^;
  end;
  place := opnd; // Op node, where operand used to be.
  PREOp(place)^ := op;
  Inc(place, REOpSz);
  for i := 1 + REOpSz to sz do
  begin
    place^ := #0;
    Inc(place);
  end;
end; { of procedure TRegExpr.InsertOperator
  -------------------------------------------------------------- }

function FindSkippedMetaLen(PStart, PEnd: PRegExprChar): integer; {$IFDEF InlineFuncs}inline;{$ENDIF}
// find length of initial segment of PStart string consisting
// entirely of characters not from IsMetaSymbol1.
begin
  Result := 0;
  while PStart < PEnd do
  begin
    if _IsMetaSymbol1(PStart^) then
      Exit;
    Inc(Result);
    Inc(PStart)
  end;
end;

const
  // Flags to be passed up and down.
  flag_HasWidth = 01; // Known never to match nil string.
  flag_Simple = 02; // Simple enough to be OP_STAR/OP_PLUS/OP_BRACES operand.
  flag_SpecStart = 04; // Starts with * or +.
  flag_Worst = 0; // Worst case.

  {$IFDEF UniCode}
  RusRangeLoLow = #$430; // 'а'
  RusRangeLoHigh = #$44F; // 'я'
  RusRangeHiLow = #$410; // 'А'
  RusRangeHiHigh = #$42F; // 'Я'
  {$ELSE}
  RusRangeLoLow = #$E0; // 'а' in cp1251
  RusRangeLoHigh = #$FF; // 'я' in cp1251
  RusRangeHiLow = #$C0; // 'А' in cp1251
  RusRangeHiHigh = #$DF; // 'Я' in cp1251
  {$ENDIF}

function TRegExpr.FindInCharClass(ABuffer: PRegExprChar; AChar: REChar; AIgnoreCase: boolean): boolean;
// Buffer contains char pairs: (Kind, Data), where Kind is one of OpKind_ values,
// and Data depends on Kind
var
  ch, ch2: REChar;
  N, i: integer;
begin
  if AIgnoreCase then
    AChar := _UpperCase(AChar);
  repeat
    case ABuffer^ of
      OpKind_End:
        begin
          Result := False;
          Exit;
        end;

      OpKind_Range:
        begin
          Inc(ABuffer);
          ch := ABuffer^;
          Inc(ABuffer);
          ch2 := ABuffer^;
          Inc(ABuffer);
          {
          // if AIgnoreCase, ch, ch2 are upcased in opcode
          if AIgnoreCase then
          begin
            ch := _UpperCase(ch);
            ch2 := _UpperCase(ch2);
          end;
          }
          if (AChar >= ch) and (AChar <= ch2) then
          begin
            Result := True;
            Exit;
          end;
        end;

      OpKind_MetaClass:
        begin
          Inc(ABuffer);
          N := Ord(ABuffer^);
          Inc(ABuffer);
          if CharCheckers[N](AChar) then
          begin
            Result := True;
            Exit
          end;
        end;

      OpKind_Char:
        begin
          Inc(ABuffer);
          N := PLongInt(ABuffer)^;
          Inc(ABuffer, RENumberSz);
          for i := 1 to N do
          begin
            ch := ABuffer^;
            Inc(ABuffer);
            {
            // already upcased in opcode
            if AIgnoreCase then
              ch := _UpperCase(ch);
            }
            if ch = AChar then
            begin
              Result := True;
              Exit;
            end;
          end;
        end;

      else
        Error(reeBadOpcodeInCharClass);
    end;
  until False; // assume that Buffer is ended correctly
end;


procedure TRegExpr.GetCharSetFromWordChars(var ARes: TRegExprCharset);
{$IFDEF UseWordChars}
var
  i: integer;
  ch: REChar;
{$ENDIF}
begin
  {$IFDEF UseWordChars}
  ARes := [];
  for i := 1 to Length(fWordChars) do
  begin
    ch := fWordChars[i];
    {$IFDEF UniCode}
    if Ord(ch) <= $FF then
    {$ENDIF}
      Include(ARes, byte(ch));
  end;
  {$ELSE}
  ARes := RegExprWordSet;
  {$ENDIF}
end;

procedure TRegExpr.GetCharSetFromSpaceChars(var ARes: TRegExprCharset);
{$IFDEF UseSpaceChars}
var
  i: integer;
  ch: REChar;
{$ENDIF}
begin
  {$IFDEF UseSpaceChars}
  ARes := [];
  for i := 1 to Length(fSpaceChars) do
  begin
    ch := fSpaceChars[i];
    {$IFDEF UniCode}
    if Ord(ch) <= $FF then
    {$ENDIF}
      Include(ARes, byte(ch));
  end;
  {$ELSE}
  ARes := RegExprSpaceSet;
  {$ENDIF}
end;

procedure TRegExpr.GetCharSetFromCharClass(ABuffer: PRegExprChar; AIgnoreCase: boolean; var ARes: TRegExprCharset);
var
  ch, ch2: REChar;
  TempSet: TRegExprCharSet;
  N, i: integer;
begin
  ARes := [];
  TempSet := [];
  repeat
    case ABuffer^ of
      OpKind_End:
        Exit;

      OpKind_Range:
        begin
          Inc(ABuffer);
          ch := ABuffer^;
          Inc(ABuffer);
          ch2 := ABuffer^;
          Inc(ABuffer);
          for i := Ord(ch) to
            {$IFDEF UniCode} Min(Ord(ch2), $FF) {$ELSE} Ord(ch2) {$ENDIF} do
          begin
            Include(ARes, byte(i));
            if AIgnoreCase then
              Include(ARes, byte(InvertCase(REChar(i))));
          end;
        end;

      OpKind_MetaClass:
        begin
          Inc(ABuffer);
          N := Ord(ABuffer^);
          Inc(ABuffer);

          if N = CheckerIndex_Word then
          begin
            GetCharSetFromWordChars(TempSet);
            ARes := ARes + TempSet;
          end
          else
          if N = CheckerIndex_NotWord then
          begin
            GetCharSetFromWordChars(TempSet);
            ARes := ARes + (RegExprAllSet - TempSet);
          end
          else
          if N = CheckerIndex_Space then
          begin
            GetCharSetFromSpaceChars(TempSet);
            ARes := ARes + TempSet;
          end
          else
          if N = CheckerIndex_NotSpace then
          begin
            GetCharSetFromSpaceChars(TempSet);
            ARes := ARes + (RegExprAllSet - TempSet);
          end
          else
          if N = CheckerIndex_Digit then
            ARes := ARes + RegExprDigitSet
          else
          if N = CheckerIndex_NotDigit then
            ARes := ARes + (RegExprAllSet - RegExprDigitSet)
          else
          if N = CheckerIndex_VertSep then
            ARes := ARes + RegExprLineSeparatorsSet
          else
          if N = CheckerIndex_NotVertSep then
            ARes := ARes + (RegExprAllSet - RegExprLineSeparatorsSet)
          else
          if N = CheckerIndex_HorzSep then
            ARes := ARes + RegExprHorzSeparatorsSet
          else
          if N = CheckerIndex_NotHorzSep then
            ARes := ARes + (RegExprAllSet - RegExprHorzSeparatorsSet)
          else
          if N = CheckerIndex_LowerAZ then
          begin
            if AIgnoreCase then
              ARes := ARes + RegExprAllAzSet
            else
              ARes := ARes + RegExprLowerAzSet;
          end
          else
          if N = CheckerIndex_UpperAZ then
          begin
            if AIgnoreCase then
              ARes := ARes + RegExprAllAzSet
            else
              ARes := ARes + RegExprUpperAzSet;
          end
          else
            Error(reeBadOpcodeInCharClass);
        end;

      OpKind_Char:
        begin
          Inc(ABuffer);
          N := PLongInt(ABuffer)^;
          Inc(ABuffer, RENumberSz);
          for i := 1 to N do
          begin
            ch := ABuffer^;
            Inc(ABuffer);
            {$IFDEF UniCode}
            if Ord(ch) <= $FF then
            {$ENDIF}
            begin
              Include(ARes, byte(ch));
              if AIgnoreCase then
                Include(ARes, byte(InvertCase(ch)));
            end;
          end;
        end;

      else
        Error(reeBadOpcodeInCharClass);
    end;
  until False; // assume that Buffer is ended correctly
end;


function TRegExpr.GetModifierG: boolean;
begin
  Result := fModifiers.G;
end;

function TRegExpr.GetModifierI: boolean;
begin
  Result := fModifiers.I;
end;

function TRegExpr.GetModifierM: boolean;
begin
  Result := fModifiers.M;
end;

function TRegExpr.GetModifierR: boolean;
begin
  Result := fModifiers.R;
end;

function TRegExpr.GetModifierS: boolean;
begin
  Result := fModifiers.S;
end;

function TRegExpr.GetModifierX: boolean;
begin
  Result := fModifiers.X;
end;

function TRegExpr.CompileRegExpr(ARegExp: PRegExprChar): boolean;
// Compile a regular expression into internal code
// We can't allocate space until we know how big the compiled form will be,
// but we can't compile it (and thus know how big it is) until we've got a
// place to put the code. So we cheat: we compile it twice, once with code
// generation turned off and size counting turned on, and once "for real".
// This also means that we don't allocate space until we are sure that the
// thing really will compile successfully, and we never have to move the
// code and thus invalidate pointers into it. (Note that it has to be in
// one piece because free() must be able to free it all.)
// Beware that the optimization-preparation code in here knows about some
// of the structure of the compiled regexp.
var
  scan, longest, longestTemp: PRegExprChar;
  Len, LenTemp: integer;
  flags: integer;
begin
  Result := False; // life too dark
  flags := 0;
  regparse := nil; // for correct error handling
  regexpBegin := ARegExp;
  regExactlyLen := nil;

  ClearInternalIndexes;
  fLastError := reeOk;
  fLastErrorOpcode := TREOp(0);

  try
    if programm <> nil then
    begin
      FreeMem(programm);
      programm := nil;
    end;

    if ARegExp = nil then
    begin
      Error(reeCompNullArgument);
      Exit;
    end;

    fProgModifiers := fModifiers;
    // well, may it's paranoia. I'll check it later... !!!!!!!!

    // First pass: determine size, legality.
    fSecondPass := False;
    fCompModifiers := fModifiers;
    regparse := ARegExp;
    regnpar := 1;
    regsize := 0;
    regcode := @regdummy;
    EmitC(OP_MAGIC);
    if ParseReg(0, flags) = nil then
      Exit;

    // Allocate space.
    GetMem(programm, regsize * SizeOf(REChar));

    // Second pass: emit code.
    fSecondPass := True;
    fCompModifiers := fModifiers;
    regparse := ARegExp;
    regnpar := 1;
    regcode := programm;
    EmitC(OP_MAGIC);
    if ParseReg(0, flags) = nil then
      Exit;

    // Dig out information for optimizations.
    {$IFDEF UseFirstCharSet} // ###0.929
    FirstCharSet := [];
    FillFirstCharSet(programm + REOpSz);
    for Len := 0 to 255 do
      FirstCharArray[Len] := byte(Len) in FirstCharSet;
    {$ENDIF}

    reganchored := #0;
    regmust := nil;
    regmustlen := 0;
    regmustString := '';

    scan := programm + REOpSz; // First OP_BRANCH.
    if PREOp(regnext(scan))^ = OP_EEND then
    begin // Only one top-level choice.
      scan := scan + REOpSz + RENextOffSz;

      // Starting-point info.
      if PREOp(scan)^ = OP_BOL then
        Inc(reganchored);

      // If there's something expensive in the r.e., find the longest
      // literal string that must appear and make it the regmust. Resolve
      // ties in favor of later strings, since the regstart check works
      // with the beginning of the r.e. and avoiding duplication
      // strengthens checking. Not a strong reason, but sufficient in the
      // absence of others.
      if (flags and flag_SpecStart) <> 0 then
      begin
        longest := nil;
        Len := 0;
        while scan <> nil do
        begin
          if PREOp(scan)^ = OP_EXACTLY then
          begin
            longestTemp := scan + REOpSz + RENextOffSz + RENumberSz;
            LenTemp := PLongInt(scan + REOpSz + RENextOffSz)^;
            if LenTemp >= Len then
            begin
              longest := longestTemp;
              Len := LenTemp;
            end;
          end;
          scan := regnext(scan);
        end;
        regmust := longest;
        regmustlen := Len;
        if regmustlen > 1 then // don't use regmust if too short
          SetString(regmustString, regmust, regmustlen);
      end;
    end;

    Result := True;

  finally
    begin
      if not Result then
        InvalidateProgramm;
      regexpBegin := nil;
      regexpIsCompiled := Result; // ###0.944
    end;
  end;

end; { of function TRegExpr.CompileRegExpr
  -------------------------------------------------------------- }

procedure TRegExpr.SetUseOsLineEndOnReplace(AValue: boolean);
begin
  if FUseOsLineEndOnReplace = AValue then
    Exit;
  FUseOsLineEndOnReplace := AValue;
  if FUseOsLineEndOnReplace then
    FReplaceLineEnd := sLineBreak
  else
    FReplaceLineEnd := #10;
end;

function TRegExpr.ParseReg(paren: integer; var flagp: integer): PRegExprChar;
// regular expression, i.e. main body or parenthesized thing
// Caller must absorb opening parenthesis.
// Combining parenthesis handling with the base level of regular expression
// is a trifle forced, but the need to tie the tails of the branches to what
// follows makes it hard to avoid.
var
  ret, br, ender: PRegExprChar;
  parno: integer;
  flags: integer;
  SavedModifiers: TRegExprModifiers;
begin
  flags := 0;
  Result := nil;
  flagp := flag_HasWidth; // Tentatively.
  parno := 0; // eliminate compiler stupid warning
  SavedModifiers := fCompModifiers;

  // Make an OP_OPEN node, if parenthesized.
  if paren <> 0 then
  begin
    if regnpar >= NSUBEXP then
    begin
      Error(reeCompParseRegTooManyBrackets);
      Exit;
    end;
    parno := regnpar;
    Inc(regnpar);
    ret := EmitNode(TREOp(Ord(OP_OPEN) + parno));
  end
  else
    ret := nil;

  // Pick up the branches, linking them together.
  br := ParseBranch(flags);
  if br = nil then
  begin
    Result := nil;
    Exit;
  end;
  if ret <> nil then
    Tail(ret, br) // OP_OPEN -> first.
  else
    ret := br;
  if (flags and flag_HasWidth) = 0 then
    flagp := flagp and not flag_HasWidth;
  flagp := flagp or flags and flag_SpecStart;
  while (regparse^ = '|') do
  begin
    Inc(regparse);
    br := ParseBranch(flags);
    if br = nil then
    begin
      Result := nil;
      Exit;
    end;
    Tail(ret, br); // OP_BRANCH -> OP_BRANCH.
    if (flags and flag_HasWidth) = 0 then
      flagp := flagp and not flag_HasWidth;
    flagp := flagp or flags and flag_SpecStart;
  end;

  // Make a closing node, and hook it on the end.
  if paren <> 0 then
    ender := EmitNode(TREOp(Ord(OP_CLOSE) + parno))
  else
    ender := EmitNode(OP_EEND);
  Tail(ret, ender);

  // Hook the tails of the branches to the closing node.
  br := ret;
  while br <> nil do
  begin
    OpTail(br, ender);
    br := regnext(br);
  end;

  // Check for proper termination.
  if paren <> 0 then
    if regparse^ <> ')' then
    begin
      Error(reeCompParseRegUnmatchedBrackets);
      Exit;
    end
    else
      Inc(regparse); // skip trailing ')'
  if (paren = 0) and (regparse < fRegexEnd) then
  begin
    if regparse^ = ')' then
      Error(reeCompParseRegUnmatchedBrackets2)
    else
      Error(reeCompParseRegJunkOnEnd);
    Exit;
  end;
  fCompModifiers := SavedModifiers; // restore modifiers of parent
  Result := ret;
end; { of function TRegExpr.ParseReg
  -------------------------------------------------------------- }

function TRegExpr.ParseBranch(var flagp: integer): PRegExprChar;
// one alternative of an | operator
// Implements the concatenation operator.
var
  ret, chain, latest: PRegExprChar;
  flags: integer;
begin
  flags := 0;
  flagp := flag_Worst; // Tentatively.

  ret := EmitNode(OP_BRANCH);
  chain := nil;
  while (regparse < fRegexEnd) and (regparse^ <> '|') and (regparse^ <> ')') do
  begin
    latest := ParsePiece(flags);
    if latest = nil then
    begin
      Result := nil;
      Exit;
    end;
    flagp := flagp or flags and flag_HasWidth;
    if chain = nil // First piece.
    then
      flagp := flagp or flags and flag_SpecStart
    else
      Tail(chain, latest);
    chain := latest;
  end;
  if chain = nil // Loop ran zero times.
  then
    EmitNode(OP_NOTHING);
  Result := ret;
end; { of function TRegExpr.ParseBranch
  -------------------------------------------------------------- }

function TRegExpr.ParsePiece(var flagp: integer): PRegExprChar;
// something followed by possible [*+?{]
// Note that the branching code sequences used for ? and the general cases
// of * and + and { are somewhat optimized:  they use the same OP_NOTHING node as
// both the endmarker for their branch list and the body of the last branch.
// It might seem that this node could be dispensed with entirely, but the
// endmarker role is not redundant.

  function ParseNumber(AStart, AEnd: PRegExprChar): TREBracesArg;
  begin
    Result := 0;
    if AEnd - AStart + 1 > 8 then
    begin // prevent stupid scanning
      Error(reeBRACESArgTooBig);
      Exit;
    end;
    while AStart <= AEnd do
    begin
      Result := Result * 10 + (Ord(AStart^) - Ord('0'));
      Inc(AStart);
    end;
    if (Result > MaxBracesArg) or (Result < 0) then
    begin
      Error(reeBRACESArgTooBig);
      Exit;
    end;
  end;

var
  TheOp: TREOp;
  NextNode: PRegExprChar;

  procedure EmitComplexBraces(ABracesMin, ABracesMax: TREBracesArg; ANonGreedyOp: boolean); // ###0.940
  {$IFDEF ComplexBraces}
  var
    off: TRENextOff;
  {$ENDIF}
  begin
    {$IFNDEF ComplexBraces}
    Error(reeComplexBracesNotImplemented);
    {$ELSE}
    if ANonGreedyOp then
      TheOp := OP_LOOPNG
    else
      TheOp := OP_LOOP;
    InsertOperator(OP_LOOPENTRY, Result, REOpSz + RENextOffSz);
    NextNode := EmitNode(TheOp);
    if regcode <> @regdummy then
    begin
      off := (Result + REOpSz + RENextOffSz) - (regcode - REOpSz - RENextOffSz);
      // back to Atom after OP_LOOPENTRY
      PREBracesArg(AlignToInt(regcode))^ := ABracesMin;
      Inc(regcode, REBracesArgSz);
      PREBracesArg(AlignToInt(regcode))^ := ABracesMax;
      Inc(regcode, REBracesArgSz);
      PRENextOff(AlignToPtr(regcode))^ := off;
      Inc(regcode, RENextOffSz);
      {$IFDEF DebugSynRegExpr}
      if regcode - programm > regsize then
        raise Exception.Create
          ('TRegExpr.ParsePiece.EmitComplexBraces buffer overrun');
      {$ENDIF}
    end
    else
      Inc(regsize, REBracesArgSz * 2 + RENextOffSz);
    Tail(Result, NextNode); // OP_LOOPENTRY -> OP_LOOP
    if regcode <> @regdummy then
      Tail(Result + REOpSz + RENextOffSz, NextNode); // Atom -> OP_LOOP
    {$ENDIF}
  end;

  procedure EmitSimpleBraces(ABracesMin, ABracesMax: TREBracesArg; ANonGreedyOp: boolean); // ###0.940
  begin
    if ANonGreedyOp // ###0.940
    then
      TheOp := OP_BRACESNG
    else
      TheOp := OP_BRACES;
    InsertOperator(TheOp, Result, REOpSz + RENextOffSz + REBracesArgSz * 2);
    if regcode <> @regdummy then
    begin
      PREBracesArg(AlignToInt(Result + REOpSz + RENextOffSz))^ := ABracesMin;
      PREBracesArg(AlignToInt(Result + REOpSz + RENextOffSz + REBracesArgSz))^ := ABracesMax;
    end;
  end;

var
  op: REChar;
  NonGreedyOp, NonGreedyCh: boolean; // ###0.940
  flags: integer;
  BracesMin, Bracesmax: TREBracesArg;
  p: PRegExprChar;
begin
  flags := 0;
  Result := ParseAtom(flags);
  if Result = nil then
    Exit;

  op := regparse^;
  if not ((op = '*') or (op = '+') or (op = '?') or (op = '{')) then
  begin
    flagp := flags;
    Exit;
  end;
  if ((flags and flag_HasWidth) = 0) and (op <> '?') then
  begin
    Error(reePlusStarOperandCouldBeEmpty);
    Exit;
  end;

  case op of
    '*':
      begin
        flagp := flag_Worst or flag_SpecStart;
        NonGreedyCh := (regparse + 1)^ = '?'; // ###0.940
        NonGreedyOp := NonGreedyCh or not fCompModifiers.G;
        // ###0.940
        if (flags and flag_Simple) = 0 then
        begin
          if NonGreedyOp // ###0.940
          then
            EmitComplexBraces(0, MaxBracesArg, NonGreedyOp)
          else
          begin // Emit x* as (x&|), where & means "self".
            InsertOperator(OP_BRANCH, Result, REOpSz + RENextOffSz); // Either x
            OpTail(Result, EmitNode(OP_BACK)); // and loop
            OpTail(Result, Result); // back
            Tail(Result, EmitNode(OP_BRANCH)); // or
            Tail(Result, EmitNode(OP_NOTHING)); // nil.
          end
        end
        else
        begin // Simple
          if NonGreedyOp // ###0.940
          then
            TheOp := OP_STARNG
          else
            TheOp := OP_STAR;
          InsertOperator(TheOp, Result, REOpSz + RENextOffSz);
        end;
        if NonGreedyCh // ###0.940
        then
          Inc(regparse); // Skip extra char ('?')
      end; { of case '*' }
    '+':
      begin
        flagp := flag_Worst or flag_SpecStart or flag_HasWidth;
        NonGreedyCh := (regparse + 1)^ = '?'; // ###0.940
        NonGreedyOp := NonGreedyCh or not fCompModifiers.G;
        // ###0.940
        if (flags and flag_Simple) = 0 then
        begin
          if NonGreedyOp // ###0.940
          then
            EmitComplexBraces(1, MaxBracesArg, NonGreedyOp)
          else
          begin // Emit x+ as x(&|), where & means "self".
            NextNode := EmitNode(OP_BRANCH); // Either
            Tail(Result, NextNode);
            Tail(EmitNode(OP_BACK), Result); // loop back
            Tail(NextNode, EmitNode(OP_BRANCH)); // or
            Tail(Result, EmitNode(OP_NOTHING)); // nil.
          end
        end
        else
        begin // Simple
          if NonGreedyOp // ###0.940
          then
            TheOp := OP_PLUSNG
          else
            TheOp := OP_PLUS;
          InsertOperator(TheOp, Result, REOpSz + RENextOffSz);
        end;
        if NonGreedyCh // ###0.940
        then
          Inc(regparse); // Skip extra char ('?')
      end; { of case '+' }
    '?':
      begin
        flagp := flag_Worst;
        NonGreedyCh := (regparse + 1)^ = '?'; // ###0.940
        NonGreedyOp := NonGreedyCh or not fCompModifiers.G;
        // ###0.940
        if NonGreedyOp then
        begin // ###0.940  // We emit x?? as x{0,1}?
          if (flags and flag_Simple) = 0 then
            EmitComplexBraces(0, 1, NonGreedyOp)
          else
            EmitSimpleBraces(0, 1, NonGreedyOp);
        end
        else
        begin // greedy '?'
          InsertOperator(OP_BRANCH, Result, REOpSz + RENextOffSz); // Either x
          Tail(Result, EmitNode(OP_BRANCH)); // or
          NextNode := EmitNode(OP_NOTHING); // nil.
          Tail(Result, NextNode);
          OpTail(Result, NextNode);
        end;
        if NonGreedyCh // ###0.940
        then
          Inc(regparse); // Skip extra char ('?')
      end; { of case '?' }
    '{':
      begin
        Inc(regparse);
        p := regparse;
        while IsDigitChar(regparse^) do // <min> MUST appear
          Inc(regparse);
        if (regparse^ <> '}') and (regparse^ <> ',') or (p = regparse) then
        begin
          Error(reeIncorrectBraces);
          Exit;
        end;
        BracesMin := ParseNumber(p, regparse - 1);
        if regparse^ = ',' then
        begin
          Inc(regparse);
          p := regparse;
          while IsDigitChar(regparse^) do
            Inc(regparse);
          if regparse^ <> '}' then
          begin
            Error(reeIncorrectBraces);
            Exit;
          end;
          if p = regparse then
            Bracesmax := MaxBracesArg
          else
            Bracesmax := ParseNumber(p, regparse - 1);
        end
        else
          Bracesmax := BracesMin; // {n} == {n,n}
        if BracesMin > Bracesmax then
        begin
          Error(reeBracesMinParamGreaterMax);
          Exit;
        end;
        if BracesMin > 0 then
          flagp := flag_Worst;
        if Bracesmax > 0 then
          flagp := flagp or flag_HasWidth or flag_SpecStart;

        NonGreedyCh := (regparse + 1)^ = '?'; // ###0.940
        NonGreedyOp := NonGreedyCh or not fCompModifiers.G;
        // ###0.940
        if (flags and flag_Simple) <> 0 then
          EmitSimpleBraces(BracesMin, Bracesmax, NonGreedyOp)
        else
          EmitComplexBraces(BracesMin, Bracesmax, NonGreedyOp);
        if NonGreedyCh // ###0.940
        then
          Inc(regparse); // Skip extra char '?'
      end; // of case '{'
    // else // here we can't be
  end; { of case op }

  Inc(regparse);
  op := regparse^;
  if (op = '*') or (op = '+') or (op = '?') or (op = '{') then
    Error(reeNestedSQP);
end; { of function TRegExpr.ParsePiece
  -------------------------------------------------------------- }

function TRegExpr.HexDig(Ch: REChar): integer;
begin
  case Ch of
    '0' .. '9':
      Result := Ord(Ch) - Ord('0');
    'a' .. 'f':
      Result := Ord(Ch) - Ord('a') + 10;
    'A' .. 'F':
      Result := Ord(Ch) - Ord('A') + 10;
    else
      begin
        Result := 0;
        Error(reeBadHexDigit);
      end;
  end;
end;

function TRegExpr.UnQuoteChar(var APtr: PRegExprChar): REChar;
var
  Ch: REChar;
begin
  case APtr^ of
    't':
      Result := #$9; // \t => tab (HT/TAB)
    'n':
      Result := #$a; // \n => newline (NL)
    'r':
      Result := #$d; // \r => carriage return (CR)
    'f':
      Result := #$c; // \f => form feed (FF)
    'a':
      Result := #$7; // \a => alarm (bell) (BEL)
    'e':
      Result := #$1b; // \e => escape (ESC)
    'c':
      begin // \cK => code for Ctrl+K
        Inc(APtr);
        if APtr >= fRegexEnd then
          Error(reeNoLetterAfterBSlashC);
        Ch := APtr^;
        case Ch of
          'a' .. 'z':
            Result := REChar(Ord(Ch) - Ord('a') + 1);
          'A' .. 'Z':
            Result := REChar(Ord(Ch) - Ord('A') + 1);
          else
            Error(reeNoLetterAfterBSlashC);
        end;
      end;
    'x':
      begin // \x: hex char
        Result := #0;
        Inc(APtr);
        if APtr >= fRegexEnd then
        begin
          Error(reeNoHexCodeAfterBSlashX);
          Exit;
        end;
        if APtr^ = '{' then
        begin // \x{nnnn} //###0.936
          repeat
            Inc(APtr);
            if APtr >= fRegexEnd then
            begin
              Error(reeNoHexCodeAfterBSlashX);
              Exit;
            end;
            if APtr^ <> '}' then
            begin
              if (Ord(Result) ShR (SizeOf(REChar) * 8 - 4)) and $F <> 0 then
              begin
                Error(reeHexCodeAfterBSlashXTooBig);
                Exit;
              end;
              Result := REChar((Ord(Result) ShL 4) or HexDig(APtr^));
              // HexDig will cause Error if bad hex digit found
            end
            else
              Break;
          until False;
        end
        else
        begin
          Result := REChar(HexDig(APtr^));
          // HexDig will cause Error if bad hex digit found
          Inc(APtr);
          if APtr >= fRegexEnd then
          begin
            Error(reeNoHexCodeAfterBSlashX);
            Exit;
          end;
          Result := REChar((Ord(Result) ShL 4) or HexDig(APtr^));
          // HexDig will cause Error if bad hex digit found
        end;
      end;
  else
    Result := APtr^;
  end;
end;

function TRegExpr.ParseAtom(var flagp: integer): PRegExprChar;
// the lowest level
// Optimization:  gobbles an entire sequence of ordinary characters so that
// it can turn them into a single node, which is smaller to store and
// faster to run. Backslashed characters are exceptions, each becoming a
// separate node; the code is simpler that way and it's not worth fixing.
var
  ret: PRegExprChar;
  RangeBeg, RangeEnd: REChar;
  CanBeRange: boolean;
  AddrOfLen: PLongInt;

  procedure EmitExactly(Ch: REChar);
  begin
    if fCompModifiers.I then
      ret := EmitNode(OP_EXACTLYCI)
    else
      ret := EmitNode(OP_EXACTLY);
    EmitInt(1);
    EmitC(Ch);
    flagp := flagp or flag_HasWidth or flag_Simple;
  end;

  procedure EmitRangeChar(Ch: REChar; AStartOfRange: boolean);
  begin
    CanBeRange := AStartOfRange;
    if fCompModifiers.I then
      Ch := _UpperCase(Ch);
    if AStartOfRange then
    begin
      AddrOfLen := nil;
      RangeBeg := Ch;
    end
    else
    begin
      if AddrOfLen = nil then
      begin
        EmitC(OpKind_Char);
        Pointer(AddrOfLen) := regcode;
        EmitInt(0);
      end;
      Inc(AddrOfLen^);
      EmitC(Ch);
    end;
  end;

  procedure EmitRangePacked(ch1, ch2: REChar);
  var
    ChkIndex: integer;
  begin
    AddrOfLen := nil;
    CanBeRange := False;

    if fCompModifiers.I then
    begin
      ch1 := _UpperCase(ch1);
      ch2 := _UpperCase(ch2);
    end;

    for ChkIndex := Low(CharCheckerInfos) to High(CharCheckerInfos) do
      if (CharCheckerInfos[ChkIndex].CharBegin = ch1) and
        (CharCheckerInfos[ChkIndex].CharEnd = ch2) then
      begin
        EmitC(OpKind_MetaClass);
        EmitC(REChar(CharCheckerInfos[ChkIndex].CheckerIndex));
        Exit;
      end;

    EmitC(OpKind_Range);
    EmitC(ch1);
    EmitC(ch2);
  end;

var
  flags: integer;
  Len: integer;
  SavedPtr: PRegExprChar;
  EnderChar, TempChar: REChar;
begin
  Result := nil;
  flags := 0;
  flagp := flag_Worst;
  AddrOfLen := nil;

  Inc(regparse);
  case (regparse - 1)^ of
    '^':
      if not fCompModifiers.M or
        ((fLineSeparators = '') and not fLinePairedSeparatorAssigned) then
        ret := EmitNode(OP_BOL)
      else
        ret := EmitNode(OP_BOLML);
    '$':
      if not fCompModifiers.M or
        ((fLineSeparators = '') and not fLinePairedSeparatorAssigned) then
        ret := EmitNode(OP_EOL)
      else
        ret := EmitNode(OP_EOLML);
    '.':
      if fCompModifiers.S then
      begin
        ret := EmitNode(OP_ANY);
        flagp := flagp or flag_HasWidth or flag_Simple;
      end
      else
      begin // not /s, so emit [^:LineSeparators:]
        ret := EmitNode(OP_ANYML);
        flagp := flagp or flag_HasWidth; // not so simple ;)
      end;
    '[':
      begin
        if regparse^ = '^' then
        begin // Complement of range.
          if fCompModifiers.I then
            ret := EmitNode(OP_ANYBUTCI)
          else
            ret := EmitNode(OP_ANYBUT);
          Inc(regparse);
        end
        else if fCompModifiers.I then
          ret := EmitNode(OP_ANYOFCI)
        else
          ret := EmitNode(OP_ANYOF);

        CanBeRange := False;

        if regparse^ = ']' then
        begin
          // first ']' inside [] treated as simple char, no need to check '['
          EmitRangeChar(regparse^, (regparse + 1)^ = '-');
          Inc(regparse);
        end;

        while (regparse < fRegexEnd) and (regparse^ <> ']') do
        begin
          if (regparse^ = '-') and ((regparse + 1) < fRegexEnd) and
            ((regparse + 1)^ <> ']') and CanBeRange then
          begin
            Inc(regparse);
            RangeEnd := regparse^;
            if RangeEnd = EscChar then
            begin
              if _IsMetaChar((regparse + 1)^) then
              begin
                Error(reeMetaCharAfterMinusInRange);
                Exit;
              end;
              Inc(regparse);
              RangeEnd := UnQuoteChar(regparse);
            end;

            // special handling for Russian range a-YA, add 2 ranges: a-ya and A-YA
            if fCompModifiers.R and
              (RangeBeg = RusRangeLoLow) and (RangeEnd = RusRangeHiHigh) then
            begin
              EmitRangePacked(RusRangeLoLow, RusRangeLoHigh);
              EmitRangePacked(RusRangeHiLow, RusRangeHiHigh);
            end
            else
            begin // standard r.e. handling
              if RangeBeg > RangeEnd then
              begin
                Error(reeInvalidRange);
                Exit;
              end;
              EmitRangePacked(RangeBeg, RangeEnd);
            end;
            Inc(regparse);
          end
          else
          begin
            if regparse^ = EscChar then
            begin
              Inc(regparse);
              if regparse >= fRegexEnd then
              begin
                Error(reeParseAtomTrailingBackSlash);
                Exit;
              end;
              if _IsMetaChar(regparse^) then
              begin
                AddrOfLen := nil;
                CanBeRange := False;
                EmitC(OpKind_MetaClass);
                case regparse^ of
                  'w':
                    EmitC(REChar(CheckerIndex_Word));
                  'W':
                    EmitC(REChar(CheckerIndex_NotWord));
                  's':
                    EmitC(REChar(CheckerIndex_Space));
                  'S':
                    EmitC(REChar(CheckerIndex_NotSpace));
                  'd':
                    EmitC(REChar(CheckerIndex_Digit));
                  'D':
                    EmitC(REChar(CheckerIndex_NotDigit));
                  'v':
                    EmitC(REChar(CheckerIndex_VertSep));
                  'V':
                    EmitC(REChar(CheckerIndex_NotVertSep));
                  'h':
                    EmitC(REChar(CheckerIndex_HorzSep));
                  'H':
                    EmitC(REChar(CheckerIndex_NotHorzSep));
                  else
                    Error(reeBadOpcodeInCharClass);
                end;
              end
              else
              begin
                TempChar := UnQuoteChar(regparse);
                EmitRangeChar(TempChar, (regparse + 1)^ = '-');
              end;
            end
            else
            begin
              EmitRangeChar(regparse^, (regparse + 1)^ = '-');
            end;
            Inc(regparse);
          end;
        end; { of while }
        AddrOfLen := nil;
        CanBeRange := False;
        EmitC(OpKind_End);
        if regparse^ <> ']' then
        begin
          Error(reeUnmatchedSqBrackets);
          Exit;
        end;
        Inc(regparse);
        flagp := flagp or flag_HasWidth or flag_Simple;
      end;
    '(':
      begin
        if regparse^ = '?' then
        begin
          // check for non-capturing group: (?:text)
          if (regparse + 1)^ = ':' then
          begin
            Inc(regparse, 2);
            ret := ParseReg(1, flags);
            if ret = nil then
            begin
              Result := nil;
              Exit;
            end;
            flagp := flagp or flags and (flag_HasWidth or flag_SpecStart);
          end
          else
            // check for extended Perl syntax : (?..)
            if (regparse + 1)^ = '#' then
            begin // (?#comment)
              Inc(regparse, 2); // find closing ')'
              while (regparse < fRegexEnd) and (regparse^ <> ')') do
                Inc(regparse);
              if regparse^ <> ')' then
              begin
                Error(reeUnclosedComment);
                Exit;
              end;
              Inc(regparse); // skip ')'
              ret := EmitNode(OP_COMMENT); // comment
            end
            else
            begin // modifiers ?
              Inc(regparse); // skip '?'
              SavedPtr := regparse;
              while (regparse < fRegexEnd) and (regparse^ <> ')') do
                Inc(regparse);
              if (regparse^ <> ')') or
                not ParseModifiers(SavedPtr, regparse - SavedPtr, fCompModifiers) then
              begin
                Error(reeUnrecognizedModifier);
                Exit;
              end;
              Inc(regparse); // skip ')'
              ret := EmitNode(OP_COMMENT); // comment
              // Error (reeQPSBFollowsNothing);
              // Exit;
            end;
        end
        else
        begin
          // normal (capturing) group
          if fSecondPass then
          // must skip this block for one of passes, to not double groups count
            if GrpCount < NSUBEXP - 1 then
            begin
              Inc(GrpCount);
              GrpIndexes[GrpCount] := regnpar;
            end;
          ret := ParseReg(1, flags);
          if ret = nil then
          begin
            Result := nil;
            Exit;
          end;
          flagp := flagp or flags and (flag_HasWidth or flag_SpecStart);
        end;
      end;
    '|', ')':
      begin // Supposed to be caught earlier.
        Error(reeInternalUrp);
        Exit;
      end;
    '?', '+', '*':
      begin
        Error(reeQPSBFollowsNothing);
        Exit;
      end;
    EscChar:
      begin
        if regparse >= fRegexEnd then
        begin
          Error(reeTrailingBackSlash);
          Exit;
        end;
        case regparse^ of // r.e.extensions
          'b':
            ret := EmitNode(OP_BOUND); // ###0.943
          'B':
            ret := EmitNode(OP_NOTBOUND); // ###0.943
          'A':
            ret := EmitNode(OP_BOL); // ###0.941
          'Z':
            ret := EmitNode(OP_EOL); // ###0.941
          'd':
            begin // r.e.extension - any digit ('0' .. '9')
              ret := EmitNode(OP_ANYDIGIT);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          'D':
            begin // r.e.extension - not digit ('0' .. '9')
              ret := EmitNode(OP_NOTDIGIT);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          's':
            begin // r.e.extension - any space char
              ret := EmitNode(OP_ANYSPACE);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          'S':
            begin // r.e.extension - not space char
              ret := EmitNode(OP_NOTSPACE);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          'w':
            begin // r.e.extension - any english char / digit / '_'
              ret := EmitNode(OP_ANYLETTER);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          'W':
            begin // r.e.extension - not english char / digit / '_'
              ret := EmitNode(OP_NOTLETTER);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          'v':
            begin
              ret := EmitNode(OP_ANYVERTSEP);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          'V':
            begin
              ret := EmitNode(OP_NOTVERTSEP);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          'h':
            begin
              ret := EmitNode(OP_ANYHORZSEP);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          'H':
            begin
              ret := EmitNode(OP_NOTHORZSEP);
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
          '1' .. '9':
            begin // ###0.936
              if fCompModifiers.I then
                ret := EmitNode(OP_BSUBEXPCI)
              else
                ret := EmitNode(OP_BSUBEXP);
              EmitC(REChar(Ord(regparse^) - Ord('0')));
              flagp := flagp or flag_HasWidth or flag_Simple;
            end;
        else
          EmitExactly(UnQuoteChar(regparse));
        end; { of case }
        Inc(regparse);
      end;
  else
    begin
      Dec(regparse);
      if fCompModifiers.X and // check for eXtended syntax
        ((regparse^ = '#') or IsIgnoredChar(regparse^)) then
      begin // ###0.941 \x
        if regparse^ = '#' then
        begin // Skip eXtended comment
          // find comment terminator (group of \n and/or \r)
          while (regparse < fRegexEnd) and (regparse^ <> #$d) and
            (regparse^ <> #$a) do
            Inc(regparse);
          while (regparse^ = #$d) or (regparse^ = #$a)
          // skip comment terminator
            do
            Inc(regparse);
          // attempt to support different type of line separators
        end
        else
        begin // Skip the blanks!
          while IsIgnoredChar(regparse^) do
            Inc(regparse);
        end;
        ret := EmitNode(OP_COMMENT); // comment
      end
      else
      begin
        Len := FindSkippedMetaLen(regparse, fRegexEnd);
        if Len <= 0 then
          if regparse^ <> '{' then
          begin
            Error(reeRarseAtomInternalDisaster);
            Exit;
          end
          else
            Len := FindSkippedMetaLen(regparse + 1, fRegexEnd) + 1;
            // bad {n,m} - compile as EXACTLY
        EnderChar := (regparse + Len)^;
        if (Len > 1) and ((EnderChar = '*') or (EnderChar = '+') or (EnderChar = '?') or (EnderChar = '{')) then
          Dec(Len); // back off clear of ?+*{ operand.
        flagp := flagp or flag_HasWidth;
        if Len = 1 then
          flagp := flagp or flag_Simple;
        if fCompModifiers.I then
          ret := EmitNode(OP_EXACTLYCI)
        else
          ret := EmitNode(OP_EXACTLY);
        EmitInt(0);
        while (Len > 0) and ((not fCompModifiers.X) or (regparse^ <> '#')) do
        begin
          if not fCompModifiers.X or not IsIgnoredChar(regparse^) then
          begin
            EmitC(regparse^);
            if regcode <> @regdummy then
              Inc(regExactlyLen^);
          end;
          Inc(regparse);
          Dec(Len);
        end;
      end; { of if not comment }
    end; { of case else }
  end; { of case }

  Result := ret;
end; { of function TRegExpr.ParseAtom
  -------------------------------------------------------------- }

function TRegExpr.GetCompilerErrorPos: PtrInt;
begin
  Result := 0;
  if (regexpBegin = nil) or (regparse = nil) then
    Exit; // not in compiling mode ?
  Result := regparse - regexpBegin;
end; { of function TRegExpr.GetCompilerErrorPos
  -------------------------------------------------------------- }

{ ============================================================= }
{ ===================== Matching section ====================== }
{ ============================================================= }

function TRegExpr.regrepeat(p: PRegExprChar; AMax: integer): integer;
// repeatedly match something simple, report how many
var
  scan: PRegExprChar;
  opnd: PRegExprChar;
  TheMax, NLen: integer;
  InvChar: REChar; // ###0.931
  GrpStart, GrpEnd: PRegExprChar; // ###0.936
  ArrayIndex: integer;
begin
  Result := 0;
  scan := reginput;
  opnd := p + REOpSz + RENextOffSz; // OPERAND
  TheMax := fInputEnd - scan;
  if TheMax > AMax then
    TheMax := AMax;
  case PREOp(p)^ of
    OP_ANY:
      begin
        // note - OP_ANYML cannot be proceeded in regrepeat because can skip
        // more than one char at once
        Result := TheMax;
        Inc(scan, Result);
      end;
    OP_EXACTLY:
      begin // in opnd can be only ONE char !!!
        NLen := PLongInt(opnd)^;
        if TheMax > NLen then
          TheMax := NLen;
        Inc(opnd, RENumberSz);
        while (Result < TheMax) and (opnd^ = scan^) do
        begin
          Inc(Result);
          Inc(scan);
        end;
      end;
    OP_EXACTLYCI:
      begin // in opnd can be only ONE char !!!
        NLen := PLongInt(opnd)^;
        if TheMax > NLen then
          TheMax := NLen;
        Inc(opnd, RENumberSz);
        while (Result < TheMax) and (opnd^ = scan^) do
        begin // prevent unneeded InvertCase //###0.931
          Inc(Result);
          Inc(scan);
        end;
        if Result < TheMax then
        begin // ###0.931
          InvChar := InvertCase(opnd^); // store in register
          while (Result < TheMax) and ((opnd^ = scan^) or (InvChar = scan^)) do
          begin
            Inc(Result);
            Inc(scan);
          end;
        end;
      end;
    OP_BSUBEXP:
      begin // ###0.936
        ArrayIndex := GrpIndexes[Ord(opnd^)];
        if ArrayIndex < 0 then
          Exit;
        GrpStart := startp[ArrayIndex];
        if GrpStart = nil then
          Exit;
        GrpEnd := endp[ArrayIndex];
        if GrpEnd = nil then
          Exit;
        repeat
          opnd := GrpStart;
          while opnd < GrpEnd do
          begin
            if (scan >= fInputEnd) or (scan^ <> opnd^) then
              Exit;
            Inc(scan);
            Inc(opnd);
          end;
          Inc(Result);
          reginput := scan;
        until Result >= AMax;
      end;
    OP_BSUBEXPCI:
      begin // ###0.936
        ArrayIndex := GrpIndexes[Ord(opnd^)];
        if ArrayIndex < 0 then
          Exit;
        GrpStart := startp[ArrayIndex];
        if GrpStart = nil then
          Exit;
        GrpEnd := endp[ArrayIndex];
        if GrpEnd = nil then
          Exit;
        repeat
          opnd := GrpStart;
          while opnd < GrpEnd do
          begin
            if (scan >= fInputEnd) or
              ((scan^ <> opnd^) and (scan^ <> InvertCase(opnd^))) then
              Exit;
            Inc(scan);
            Inc(opnd);
          end;
          Inc(Result);
          reginput := scan;
        until Result >= AMax;
      end;
    OP_ANYDIGIT:
      while (Result < TheMax) and IsDigitChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_NOTDIGIT:
      while (Result < TheMax) and not IsDigitChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_ANYLETTER:
      while (Result < TheMax) and IsWordChar(scan^) do // ###0.940
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_NOTLETTER:
      while (Result < TheMax) and not IsWordChar(scan^) do // ###0.940
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_ANYSPACE:
      while (Result < TheMax) and IsSpaceChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_NOTSPACE:
      while (Result < TheMax) and not IsSpaceChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_ANYVERTSEP:
      while (Result < TheMax) and IsLineSeparator(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_NOTVERTSEP:
      while (Result < TheMax) and not IsLineSeparator(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_ANYHORZSEP:
      while (Result < TheMax) and IsHorzSeparator(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_NOTHORZSEP:
      while (Result < TheMax) and not IsHorzSeparator(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_ANYOF:
      while (Result < TheMax) and FindInCharClass(opnd, scan^, False) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_ANYBUT:
      while (Result < TheMax) and not FindInCharClass(opnd, scan^, False) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_ANYOFCI:
      while (Result < TheMax) and FindInCharClass(opnd, scan^, True) do
      begin
        Inc(Result);
        Inc(scan);
      end;
    OP_ANYBUTCI:
      while (Result < TheMax) and not FindInCharClass(opnd, scan^, True) do
      begin
        Inc(Result);
        Inc(scan);
      end;
  else
    begin // Oh dear. Called inappropriately.
      Result := 0; // Best compromise.
      Error(reeRegRepeatCalledInappropriately);
      Exit;
    end;
  end; { of case }
  reginput := scan;
end; { of function TRegExpr.regrepeat
  -------------------------------------------------------------- }

function TRegExpr.regnext(p: PRegExprChar): PRegExprChar;
// dig the "next" pointer out of a node
var
  offset: TRENextOff;
begin
  if p = @regdummy then
  begin
    Result := nil;
    Exit;
  end;
  offset := PRENextOff(AlignToPtr(p + REOpSz))^; // ###0.933 inlined NEXT
  if offset = 0 then
    Result := nil
  else
    Result := p + offset;
end; { of function TRegExpr.regnext
  -------------------------------------------------------------- }

function TRegExpr.MatchPrim(prog: PRegExprChar): boolean;
// recursively matching routine
// Conceptually the strategy is simple:  check to see whether the current
// node matches, call self recursively to see whether the rest matches,
// and then act accordingly. In practice we make some effort to avoid
// recursion, in particular by going through "ordinary" nodes (that don't
// need to know whether the rest of the match failed) by a loop instead of
// by recursion.
var
  scan: PRegExprChar; // Current node.
  next: PRegExprChar; // Next node.
  Len: PtrInt;
  opnd: PRegExprChar;
  no: integer;
  save: PRegExprChar;
  nextch: REChar;
  BracesMin, Bracesmax: integer;
  // we use integer instead of TREBracesArg for better support */+
  {$IFDEF ComplexBraces}
  SavedLoopStack: TRegExprLoopStack; // :(( very bad for recursion
  SavedLoopStackIdx: integer; // ###0.925
  {$ENDIF}
  bound1, bound2: boolean;
begin
  Result := False;
  scan := prog;
  while scan <> nil do
  begin
    Len := PRENextOff(AlignToPtr(scan + 1))^; // ###0.932 inlined regnext
    if Len = 0 then
      next := nil
    else
      next := scan + Len;

    case scan^ of
      OP_NOTBOUND,
      OP_BOUND:
        begin
          bound1 := (reginput = fInputStart) or not IsWordChar((reginput - 1)^);
          bound2 := (reginput = fInputEnd) or not IsWordChar(reginput^);
          if (scan^ = OP_BOUND) xor (bound1 <> bound2) then
            Exit;
        end;
      OP_BOL:
        begin
          if reginput <> fInputStart then
            Exit;
        end;
      OP_EOL:
        begin
          if reginput < fInputEnd then
            Exit;
        end;
      OP_BOLML:
        if reginput > fInputStart then
        begin
          nextch := (reginput - 1)^;
          if (nextch <> fLinePairedSeparatorTail) or
            ((reginput - 1) <= fInputStart) or
            ((reginput - 2)^ <> fLinePairedSeparatorHead) then
          begin
            if (nextch = fLinePairedSeparatorHead) and
              (reginput^ = fLinePairedSeparatorTail) then
              Exit; // don't stop between paired separator
            if not IsCustomLineSeparator(nextch) then
              Exit;
          end;
        end;
      OP_EOLML:
        if reginput < fInputEnd then
        begin
          nextch := reginput^;
          if (nextch <> fLinePairedSeparatorHead) or
            ((reginput + 1)^ <> fLinePairedSeparatorTail) then
          begin
            if (nextch = fLinePairedSeparatorTail) and (reginput > fInputStart)
              and ((reginput - 1)^ = fLinePairedSeparatorHead) then
              Exit; // don't stop between paired separator
            if not IsCustomLineSeparator(nextch) then
              Exit;
          end;
        end;
      OP_ANY:
        begin
          if reginput = fInputEnd then
            Exit;
          Inc(reginput);
        end;
      OP_ANYML:
        begin // ###0.941
          if (reginput = fInputEnd) or
            ((reginput^ = fLinePairedSeparatorHead) and
            ((reginput + 1)^ = fLinePairedSeparatorTail)) or
            IsCustomLineSeparator(reginput^)
          then
            Exit;
          Inc(reginput);
        end;
      OP_ANYDIGIT:
        begin
          if (reginput = fInputEnd) or not IsDigitChar(reginput^) then
            Exit;
          Inc(reginput);
        end;
      OP_NOTDIGIT:
        begin
          if (reginput = fInputEnd) or IsDigitChar(reginput^) then
            Exit;
          Inc(reginput);
        end;
      OP_ANYLETTER:
        begin
          if (reginput = fInputEnd) or not IsWordChar(reginput^) // ###0.943
          then
            Exit;
          Inc(reginput);
        end;
      OP_NOTLETTER:
        begin
          if (reginput = fInputEnd) or IsWordChar(reginput^) // ###0.943
          then
            Exit;
          Inc(reginput);
        end;
      OP_ANYSPACE:
        begin
          if (reginput = fInputEnd) or not IsSpaceChar(reginput^) // ###0.943
          then
            Exit;
          Inc(reginput);
        end;
      OP_NOTSPACE:
        begin
          if (reginput = fInputEnd) or IsSpaceChar(reginput^) // ###0.943
          then
            Exit;
          Inc(reginput);
        end;
      OP_ANYVERTSEP:
        begin
          if (reginput = fInputEnd) or not IsLineSeparator(reginput^) then
            Exit;
          Inc(reginput);
        end;
      OP_NOTVERTSEP:
        begin
          if (reginput = fInputEnd) or IsLineSeparator(reginput^) then
            Exit;
          Inc(reginput);
        end;
      OP_ANYHORZSEP:
        begin
          if (reginput = fInputEnd) or not IsHorzSeparator(reginput^) then
            Exit;
          Inc(reginput);
        end;
      OP_NOTHORZSEP:
        begin
          if (reginput = fInputEnd) or IsHorzSeparator(reginput^) then
            Exit;
          Inc(reginput);
        end;
      OP_EXACTLYCI:
        begin
          opnd := scan + REOpSz + RENextOffSz; // OPERAND
          Len := PLongInt(opnd)^;
          Inc(opnd, RENumberSz);
          // Inline the first character, for speed.
          if (opnd^ <> reginput^) and (InvertCase(opnd^) <> reginput^) then
            Exit;
          // ###0.929 begin
          no := Len;
          save := reginput;
          while no > 1 do
          begin
            Inc(save);
            Inc(opnd);
            if (opnd^ <> save^) and (InvertCase(opnd^) <> save^) then
              Exit;
            Dec(no);
          end;
          // ###0.929 end
          Inc(reginput, Len);
        end;
      OP_EXACTLY:
        begin
          opnd := scan + REOpSz + RENextOffSz; // OPERAND
          Len := PLongInt(opnd)^;
          Inc(opnd, RENumberSz);
          // Inline the first character, for speed.
          if opnd^ <> reginput^ then
            Exit;
          // ###0.929 begin
          no := Len;
          save := reginput;
          while no > 1 do
          begin
            Inc(save);
            Inc(opnd);
            if opnd^ <> save^ then
              Exit;
            Dec(no);
          end;
          // ###0.929 end
          Inc(reginput, Len);
        end;
      OP_BSUBEXP:
        begin // ###0.936
          no := Ord((scan + REOpSz + RENextOffSz)^);
          no := GrpIndexes[no];
          if no < 0 then
            Exit;
          if startp[no] = nil then
            Exit;
          if endp[no] = nil then
            Exit;
          save := reginput;
          opnd := startp[no];
          while opnd < endp[no] do
          begin
            if (save >= fInputEnd) or (save^ <> opnd^) then
              Exit;
            Inc(save);
            Inc(opnd);
          end;
          reginput := save;
        end;
      OP_BSUBEXPCI:
        begin // ###0.936
          no := Ord((scan + REOpSz + RENextOffSz)^);
          no := GrpIndexes[no];
          if no < 0 then
            Exit;
          if startp[no] = nil then
            Exit;
          if endp[no] = nil then
            Exit;
          save := reginput;
          opnd := startp[no];
          while opnd < endp[no] do
          begin
            if (save >= fInputEnd) or
              ((save^ <> opnd^) and (save^ <> InvertCase(opnd^))) then
              Exit;
            Inc(save);
            Inc(opnd);
          end;
          reginput := save;
        end;
      OP_ANYOF:
        begin
          if (reginput = fInputEnd) or
            not FindInCharClass(scan + REOpSz + RENextOffSz, reginput^, False) then
            Exit;
          Inc(reginput);
        end;
      OP_ANYBUT:
        begin
          if (reginput = fInputEnd) or
            FindInCharClass(scan + REOpSz + RENextOffSz, reginput^, False) then
            Exit;
          Inc(reginput);
        end;
      OP_ANYOFCI:
        begin
          if (reginput = fInputEnd) or
            not FindInCharClass(scan + REOpSz + RENextOffSz, reginput^, True) then
            Exit;
          Inc(reginput);
        end;
      OP_ANYBUTCI:
        begin
          if (reginput = fInputEnd) or
            FindInCharClass(scan + REOpSz + RENextOffSz, reginput^, True) then
            Exit;
          Inc(reginput);
        end;
      OP_NOTHING:
        ;
      OP_COMMENT:
        ;
      OP_BACK:
        ;
      Succ(OP_OPEN) .. TREOp(Ord(OP_OPEN) + NSUBEXP - 1):
        begin // ###0.929
          no := Ord(scan^) - Ord(OP_OPEN);
          // save := reginput;
          save := startp[no]; // ###0.936
          startp[no] := reginput; // ###0.936
          Result := MatchPrim(next);
          if not Result // ###0.936
          then
            startp[no] := save;
          // if Result and (startp [no] = nil)
          // then startp [no] := save;
          // Don't set startp if some later invocation of the same
          // parentheses already has.
          Exit;
        end;
      Succ(OP_CLOSE) .. TREOp(Ord(OP_CLOSE) + NSUBEXP - 1):
        begin // ###0.929
          no := Ord(scan^) - Ord(OP_CLOSE);
          // save := reginput;
          save := endp[no]; // ###0.936
          endp[no] := reginput; // ###0.936
          Result := MatchPrim(next);
          if not Result // ###0.936
          then
            endp[no] := save;
          // if Result and (endp [no] = nil)
          // then endp [no] := save;
          // Don't set endp if some later invocation of the same
          // parentheses already has.
          Exit;
        end;
      OP_BRANCH:
        begin
          if (next^ <> OP_BRANCH) // No choice.
          then
            next := scan + REOpSz + RENextOffSz // Avoid recursion
          else
          begin
            repeat
              save := reginput;
              Result := MatchPrim(scan + REOpSz + RENextOffSz);
              if Result then
                Exit;
              reginput := save;
              scan := regnext(scan);
            until (scan = nil) or (scan^ <> OP_BRANCH);
            Exit;
          end;
        end;
      {$IFDEF ComplexBraces}
      OP_LOOPENTRY:
        begin // ###0.925
          no := LoopStackIdx;
          Inc(LoopStackIdx);
          if LoopStackIdx > LoopStackMax then
          begin
            Error(reeLoopStackExceeded);
            Exit;
          end;
          save := reginput;
          LoopStack[LoopStackIdx] := 0; // init loop counter
          Result := MatchPrim(next); // execute loop
          LoopStackIdx := no; // cleanup
          if Result then
            Exit;
          reginput := save;
          Exit;
        end;
      OP_LOOP, OP_LOOPNG:
        begin // ###0.940
          if LoopStackIdx <= 0 then
          begin
            Error(reeLoopWithoutEntry);
            Exit;
          end;
          opnd := scan + PRENextOff(AlignToPtr(scan + REOpSz + RENextOffSz + 2 * REBracesArgSz))^;
          BracesMin := PREBracesArg(AlignToInt(scan + REOpSz + RENextOffSz))^;
          Bracesmax := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz + REBracesArgSz))^;
          save := reginput;
          if LoopStack[LoopStackIdx] >= BracesMin then
          begin // Min alredy matched - we can work
            if scan^ = OP_LOOP then
            begin
              // greedy way - first try to max deep of greed ;)
              if LoopStack[LoopStackIdx] < Bracesmax then
              begin
                Inc(LoopStack[LoopStackIdx]);
                no := LoopStackIdx;
                Result := MatchPrim(opnd);
                LoopStackIdx := no;
                if Result then
                  Exit;
                reginput := save;
              end;
              Dec(LoopStackIdx); // Fail. May be we are too greedy? ;)
              Result := MatchPrim(next);
              if not Result then
                reginput := save;
              Exit;
            end
            else
            begin
              // non-greedy - try just now
              Result := MatchPrim(next);
              if Result then
                Exit
              else
                reginput := save; // failed - move next and try again
              if LoopStack[LoopStackIdx] < Bracesmax then
              begin
                Inc(LoopStack[LoopStackIdx]);
                no := LoopStackIdx;
                Result := MatchPrim(opnd);
                LoopStackIdx := no;
                if Result then
                  Exit;
                reginput := save;
              end;
              Dec(LoopStackIdx); // Failed - back up
              Exit;
            end
          end
          else
          begin // first match a min_cnt times
            Inc(LoopStack[LoopStackIdx]);
            no := LoopStackIdx;
            Result := MatchPrim(opnd);
            LoopStackIdx := no;
            if Result then
              Exit;
            Dec(LoopStack[LoopStackIdx]);
            reginput := save;
            Exit;
          end;
        end;
      {$ENDIF}
      OP_STAR, OP_PLUS, OP_BRACES, OP_STARNG, OP_PLUSNG, OP_BRACESNG:
        begin
          // Lookahead to avoid useless match attempts when we know
          // what character comes next.
          nextch := #0;
          if next^ = OP_EXACTLY then
            nextch := (next + REOpSz + RENextOffSz + RENumberSz)^;
          Bracesmax := MaxInt; // infinite loop for * and + //###0.92
          if (scan^ = OP_STAR) or (scan^ = OP_STARNG) then
            BracesMin := 0 // star
          else if (scan^ = OP_PLUS) or (scan^ = OP_PLUSNG) then
            BracesMin := 1 // plus
          else
          begin // braces
            BracesMin := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz))^;
            Bracesmax := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz + REBracesArgSz))^;
          end;
          save := reginput;
          opnd := scan + REOpSz + RENextOffSz;
          if (scan^ = OP_BRACES) or (scan^ = OP_BRACESNG) then
            Inc(opnd, 2 * REBracesArgSz);

          if (scan^ = OP_PLUSNG) or (scan^ = OP_STARNG) or (scan^ = OP_BRACESNG) then
          begin
            // non-greedy mode
            Bracesmax := regrepeat(opnd, Bracesmax);
            // don't repeat more than BracesMax
            // Now we know real Max limit to move forward (for recursion 'back up')
            // In some cases it can be faster to check only Min positions first,
            // but after that we have to check every position separtely instead
            // of fast scannig in loop.
            no := BracesMin;
            while no <= Bracesmax do
            begin
              reginput := save + no;
              // If it could work, try it.
              if (nextch = #0) or (reginput^ = nextch) then
              begin
                {$IFDEF ComplexBraces}
                System.Move(LoopStack, SavedLoopStack, SizeOf(LoopStack));
                // ###0.925
                SavedLoopStackIdx := LoopStackIdx;
                {$ENDIF}
                if MatchPrim(next) then
                begin
                  Result := True;
                  Exit;
                end;
                {$IFDEF ComplexBraces}
                System.Move(SavedLoopStack, LoopStack, SizeOf(LoopStack));
                LoopStackIdx := SavedLoopStackIdx;
                {$ENDIF}
              end;
              Inc(no); // Couldn't or didn't - move forward.
            end; { of while }
            Exit;
          end
          else
          begin // greedy mode
            no := regrepeat(opnd, Bracesmax); // don't repeat more than max_cnt
            while no >= BracesMin do
            begin
              // If it could work, try it.
              if (nextch = #0) or (reginput^ = nextch) then
              begin
                {$IFDEF ComplexBraces}
                System.Move(LoopStack, SavedLoopStack, SizeOf(LoopStack));
                // ###0.925
                SavedLoopStackIdx := LoopStackIdx;
                {$ENDIF}
                if MatchPrim(next) then
                begin
                  Result := True;
                  Exit;
                end;
                {$IFDEF ComplexBraces}
                System.Move(SavedLoopStack, LoopStack, SizeOf(LoopStack));
                LoopStackIdx := SavedLoopStackIdx;
                {$ENDIF}
              end;
              Dec(no); // Couldn't or didn't - back up.
              reginput := save + no;
            end; { of while }
            Exit;
          end;
        end;
      OP_EEND:
        begin
          Result := True; // Success!
          Exit;
        end;
    else
      begin
        Error(reeMatchPrimMemoryCorruption);
        Exit;
      end;
    end; { of case scan^ }
    scan := next;
  end; { of while scan <> nil }

  // We get here only if there's trouble -- normally "case EEND" is the
  // terminating point.
  Error(reeMatchPrimCorruptedPointers);
end; { of function TRegExpr.MatchPrim
  -------------------------------------------------------------- }

function TRegExpr.Exec(const AInputString: RegExprString): boolean;
begin
  InputString := AInputString;
  Result := ExecPrim(1, False, False);
end; { of function TRegExpr.Exec
  -------------------------------------------------------------- }

{$IFDEF OverMeth}
function TRegExpr.Exec: boolean;
var
  SlowChecks: boolean;
begin
  SlowChecks := Length(fInputString) < fSlowChecksSizeMax;
  Result := ExecPrim(1, False, SlowChecks);
end; { of function TRegExpr.Exec
  -------------------------------------------------------------- }

function TRegExpr.Exec(AOffset: integer): boolean;
begin
  Result := ExecPrim(AOffset, False, False);
end; { of function TRegExpr.Exec
  -------------------------------------------------------------- }
{$ENDIF}

function TRegExpr.ExecPos(AOffset: integer {$IFDEF DefParam} = 1{$ENDIF}): boolean;
begin
  Result := ExecPrim(AOffset, False, False);
end; { of function TRegExpr.ExecPos
  -------------------------------------------------------------- }

{$IFDEF OverMeth}
function TRegExpr.ExecPos(AOffset: integer; ATryOnce: boolean): boolean;
begin
  Result := ExecPrim(AOffset, ATryOnce, False);
end;
{$ENDIF}

function TRegExpr.MatchAtOnePos(APos: PRegExprChar): boolean;
begin
  reginput := APos;
  Result := MatchPrim(programm + REOpSz);
  if Result then
  begin
    startp[0] := APos;
    endp[0] := reginput;
  end;
end;

procedure TRegExpr.ClearMatches;
begin
  FillChar(startp, SizeOf(startp), 0);
  FillChar(endp, SizeOf(endp), 0);
end;

procedure TRegExpr.ClearInternalIndexes;
var
  i: integer;
begin
  FillChar(startp, SizeOf(startp), 0);
  FillChar(endp, SizeOf(endp), 0);
  for i := 0 to NSUBEXP - 1 do
    GrpIndexes[i] := -1;
  GrpIndexes[0] := 0;
  GrpCount := 0;
end;

function TRegExpr.ExecPrim(AOffset: integer; ATryOnce, ASlowChecks: boolean): boolean;
var
  Ptr: PRegExprChar;
begin
  Result := False;

  // Ensure that Match cleared either if optimization tricks or some error
  // will lead to leaving ExecPrim without actual search. That is
  // important for ExecNext logic and so on.
  ClearMatches;

  // Don't check IsProgrammOk here! it causes big slowdown in test_benchmark!
  if programm = nil then
  begin
    Compile;
    if programm = nil then
      Exit;
  end;

  // Check InputString presence
  if fInputString = '' then
  begin
    //Error(reeNoInputStringSpecified); // better don't raise error, breaks some apps
    Exit;
  end;

  // Check that the start position is not negative
  if AOffset < 1 then
  begin
    Error(reeOffsetMustBePositive);
    Exit;
  end;

  // Check that the start position is not longer than the line
  // If so then exit with nothing found
  if AOffset > (Length(fInputString) + 1) // for matching empty string after last char.
  then
    Exit;

  Ptr := fInputStart + AOffset - 1;

  // If there is a "must appear" string, look for it.
  if ASlowChecks then
    if regmustString <> '' then
      if Pos(regmustString, fInputString) = 0 then Exit;

  {$IFDEF ComplexBraces}
  // no loops started
  LoopStackIdx := 0; // ###0.925
  {$ENDIF}

  // ATryOnce or anchored match (it needs to be tried only once).
  if ATryOnce or (reganchored <> #0) then
  begin
    {$IFDEF UseFirstCharSet}
    {$IFDEF UniCode}
    if Ord(Ptr^) <= $FF then
    {$ENDIF}
      if not FirstCharArray[byte(Ptr^)] then
        Exit;
    {$ENDIF}

    Result := MatchAtOnePos(Ptr);
    Exit;
  end;

  // Messy cases: unanchored match.
  Dec(Ptr);
  repeat
    Inc(Ptr);
    if Ptr > fInputEnd then
      Exit;

    {$IFDEF UseFirstCharSet}
    {$IFDEF UniCode}
    if Ord(Ptr^) <= $FF then
    {$ENDIF}
      if not FirstCharArray[byte(Ptr^)] then
        Continue;
    {$ENDIF}

    Result := MatchAtOnePos(Ptr);
    // Exit on a match or after testing the end-of-string
    if Result then
      Exit;
  until False;
end; { of function TRegExpr.ExecPrim
  -------------------------------------------------------------- }

function TRegExpr.ExecNext: boolean;
var
  PtrBegin, PtrEnd: PRegExprChar;
  Offset: PtrInt;
begin
  PtrBegin := startp[0];
  PtrEnd := endp[0];
  if (PtrBegin = nil) or (PtrEnd = nil) then
  begin
    Error(reeExecNextWithoutExec);
    Result := False;
    Exit;
  end;

  Offset := PtrEnd - fInputStart + 1;
  // prevent infinite looping if empty string matches r.e.
  if PtrBegin = PtrEnd then
    Inc(Offset);

  Result := ExecPrim(Offset, False, False);
end; { of function TRegExpr.ExecNext
  -------------------------------------------------------------- }

procedure TRegExpr.SetInputString(const AInputString: RegExprString);
begin
  ClearMatches;

  fInputString := AInputString;
  UniqueString(fInputString);

  fInputStart := PRegExprChar(fInputString);
  fInputEnd := fInputStart + Length(fInputString);
end; { of procedure TRegExpr.SetInputString
  -------------------------------------------------------------- }

procedure TRegExpr.SetLineSeparators(const AStr: RegExprString);
begin
  if AStr <> fLineSeparators then
  begin
    fLineSeparators := AStr;
    InitLineSepArray;
    InvalidateProgramm;
  end;
end; { of procedure TRegExpr.SetLineSeparators
  -------------------------------------------------------------- }

procedure TRegExpr.SetLinePairedSeparator(const AStr: RegExprString);
begin
  if Length(AStr) = 2 then
  begin
    if AStr[1] = AStr[2] then
    begin
      // it's impossible for our 'one-point' checking to support
      // two chars separator for identical chars
      Error(reeBadLinePairedSeparator);
      Exit;
    end;
    if not fLinePairedSeparatorAssigned or (AStr[1] <> fLinePairedSeparatorHead)
      or (AStr[2] <> fLinePairedSeparatorTail) then
    begin
      fLinePairedSeparatorAssigned := True;
      fLinePairedSeparatorHead := AStr[1];
      fLinePairedSeparatorTail := AStr[2];
      InvalidateProgramm;
    end;
  end
  else if Length(AStr) = 0 then
  begin
    if fLinePairedSeparatorAssigned then
    begin
      fLinePairedSeparatorAssigned := False;
      InvalidateProgramm;
    end;
  end
  else
    Error(reeBadLinePairedSeparator);
end; { of procedure TRegExpr.SetLinePairedSeparator
  -------------------------------------------------------------- }

function TRegExpr.GetLinePairedSeparator: RegExprString;
begin
  if fLinePairedSeparatorAssigned then
  begin
    {$IFDEF UniCode}
    // Here is some UniCode 'magic'
    // If You do know better decision to concatenate
    // two WideChars, please, let me know!
    Result := fLinePairedSeparatorHead; // ###0.947
    Result := Result + fLinePairedSeparatorTail;
    {$ELSE}
    Result := fLinePairedSeparatorHead + fLinePairedSeparatorTail;
    {$ENDIF}
  end
  else
    Result := '';
end; { of function TRegExpr.GetLinePairedSeparator
  -------------------------------------------------------------- }

function TRegExpr.Substitute(const ATemplate: RegExprString): RegExprString;
// perform substitutions after a regexp match
var
  TemplateBeg, TemplateEnd: PRegExprChar;

  function ParseVarName(var APtr: PRegExprChar): integer;
  // extract name of variable (digits, may be enclosed with
  // curly braces) from APtr^, uses TemplateEnd !!!
  var
    p: PRegExprChar;
    Delimited: boolean;
  begin
    Result := 0;
    p := APtr;
    Delimited := (p < TemplateEnd) and (p^ = '{');
    if Delimited then
      Inc(p); // skip left curly brace
    if (p < TemplateEnd) and (p^ = '&') then
      Inc(p) // this is '$&' or '${&}'
    else
      while (p < TemplateEnd) and IsDigitChar(p^) do
      begin
        Result := Result * 10 + (Ord(p^) - Ord('0')); // ###0.939
        Inc(p);
      end;
    if Delimited then
      if (p < TemplateEnd) and (p^ = '}') then
        Inc(p) // skip right curly brace
      else
        p := APtr; // isn't properly terminated
    if p = APtr then
      Result := -1; // no valid digits found or no right curly brace
    APtr := p;
  end;

type
  TSubstMode = (smodeNormal, smodeOneUpper, smodeOneLower, smodeAllUpper, smodeAllLower);
var
  Mode: TSubstMode;
  p, p0, p1, ResultPtr: PRegExprChar;
  ResultLen, n: integer;
  Ch, QuotedChar: REChar;
begin
  // Check programm and input string
  if not IsProgrammOk then
    Exit;
  {
  // don't check for empty, user needs to replace regex "\b", zero length
  if fInputString = '' then
  begin
    Error(reeNoInputStringSpecified);
    Exit;
  end;
  }
  // Prepare for working
  if ATemplate = '' then
  begin // prevent nil pointers
    Result := '';
    Exit;
  end;
  TemplateBeg := PRegExprChar(ATemplate);
  TemplateEnd := TemplateBeg + Length(ATemplate);
  // Count result length for speed optimization.
  ResultLen := 0;
  p := TemplateBeg;
  while p < TemplateEnd do
  begin
    Ch := p^;
    Inc(p);
    if Ch = '$' then
      n := GrpIndexes[ParseVarName(p)]
    else
      n := -1;
    if n >= 0 then
    begin
      Inc(ResultLen, endp[n] - startp[n]);
    end
    else
    begin
      if (Ch = EscChar) and (p < TemplateEnd) then
      begin // quoted or special char followed
        Ch := p^;
        Inc(p);
        case Ch of
          'n':
            Inc(ResultLen, Length(FReplaceLineEnd));
          'u', 'l', 'U', 'L': { nothing }
            ;
          'x':
            begin
              Inc(ResultLen);
              if (p^ = '{') then
              begin // skip \x{....}
                while ((p^ <> '}') and (p < TemplateEnd)) do
                  p := p + 1;
                p := p + 1;
              end
              else
                p := p + 2 // skip \x..
            end;
        else
          Inc(ResultLen);
        end;
      end
      else
        Inc(ResultLen);
    end;
  end;
  // Get memory. We do it once and it significant speed up work !
  if ResultLen = 0 then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, ResultLen);
  // Fill Result
  ResultPtr := Pointer(Result);
  p := TemplateBeg;
  Mode := smodeNormal;
  while p < TemplateEnd do
  begin
    Ch := p^;
    p0 := p;
    Inc(p);
    p1 := p;
    if Ch = '$' then
      n := GrpIndexes[ParseVarName(p)]
    else
      n := -1;
    if (n >= 0) then
    begin
      p0 := startp[n];
      p1 := endp[n];
    end
    else
    begin
      if (Ch = EscChar) and (p < TemplateEnd) then
      begin // quoted or special char followed
        Ch := p^;
        Inc(p);
        case Ch of
          'n':
            begin
              p0 := PRegExprChar(FReplaceLineEnd);
              p1 := p0 + Length(FReplaceLineEnd);
            end;
          'x', 't', 'r', 'f', 'a', 'e':
            begin
              p := p - 1;
              // UnquoteChar expects the escaped char under the pointer
              QuotedChar := UnQuoteChar(p);
              p := p + 1;
              // Skip after last part of the escaped sequence - UnquoteChar stops on the last symbol of it
              p0 := @QuotedChar;
              p1 := p0 + 1;
            end;
          'l':
            begin
              Mode := smodeOneLower;
              p1 := p0;
            end;
          'L':
            begin
              Mode := smodeAllLower;
              p1 := p0;
            end;
          'u':
            begin
              Mode := smodeOneUpper;
              p1 := p0;
            end;
          'U':
            begin
              Mode := smodeAllUpper;
              p1 := p0;
            end;
        else
          begin
            Inc(p0);
            Inc(p1);
          end;
        end;
      end
    end;
    if p0 < p1 then
    begin
      while p0 < p1 do
      begin
        case Mode of
          smodeOneLower:
            begin
              ResultPtr^ := _LowerCase(p0^);
              Mode := smodeNormal;
            end;
          smodeAllLower:
            begin
              ResultPtr^ := _LowerCase(p0^);
            end;
          smodeOneUpper:
            begin
              ResultPtr^ := _UpperCase(p0^);
              Mode := smodeNormal;
            end;
          smodeAllUpper:
            begin
              ResultPtr^ := _UpperCase(p0^);
            end;
        else
          ResultPtr^ := p0^;
        end;
        Inc(ResultPtr);
        Inc(p0);
      end;
      Mode := smodeNormal;
    end;
  end;
end; { of function TRegExpr.Substitute
  -------------------------------------------------------------- }

procedure TRegExpr.Split(const AInputStr: RegExprString; APieces: TStrings);
var
  PrevPos: PtrInt;
begin
  PrevPos := 1;
  if Exec(AInputStr) then
    repeat
      APieces.Add(System.Copy(AInputStr, PrevPos, MatchPos[0] - PrevPos));
      PrevPos := MatchPos[0] + MatchLen[0];
    until not ExecNext;
  APieces.Add(System.Copy(AInputStr, PrevPos, MaxInt)); // Tail
end; { of procedure TRegExpr.Split
  -------------------------------------------------------------- }

function TRegExpr.Replace(const AInputStr: RegExprString;
  const AReplaceStr: RegExprString;
  AUseSubstitution: boolean{$IFDEF DefParam} = False{$ENDIF}): RegExprString;
var
  PrevPos: PtrInt;
begin
  Result := '';
  PrevPos := 1;
  if Exec(AInputStr) then
    repeat
      Result := Result + System.Copy(AInputStr, PrevPos, MatchPos[0] - PrevPos);
      if AUseSubstitution // ###0.946
      then
        Result := Result + Substitute(AReplaceStr)
      else
        Result := Result + AReplaceStr;
      PrevPos := MatchPos[0] + MatchLen[0];
    until not ExecNext;
  Result := Result + System.Copy(AInputStr, PrevPos, MaxInt); // Tail
end; { of function TRegExpr.Replace
  -------------------------------------------------------------- }

function TRegExpr.ReplaceEx(const AInputStr: RegExprString;
  AReplaceFunc: TRegExprReplaceFunction): RegExprString;
var
  PrevPos: PtrInt;
begin
  Result := '';
  PrevPos := 1;
  if Exec(AInputStr) then
    repeat
      Result := Result + System.Copy(AInputStr, PrevPos, MatchPos[0] - PrevPos)
        + AReplaceFunc(Self);
      PrevPos := MatchPos[0] + MatchLen[0];
    until not ExecNext;
  Result := Result + System.Copy(AInputStr, PrevPos, MaxInt); // Tail
end; { of function TRegExpr.ReplaceEx
  -------------------------------------------------------------- }

{$IFDEF OverMeth}
function TRegExpr.Replace(const AInputStr: RegExprString;
  AReplaceFunc: TRegExprReplaceFunction): RegExprString;
begin
  Result := ReplaceEx(AInputStr, AReplaceFunc);
end; { of function TRegExpr.Replace
  -------------------------------------------------------------- }
{$ENDIF}
{ ============================================================= }
{ ====================== Debug section ======================== }
{ ============================================================= }

{$IFDEF UseFirstCharSet}
procedure TRegExpr.FillFirstCharSet(prog: PRegExprChar);
var
  scan: PRegExprChar; // Current node.
  Next: PRegExprChar; // Next node.
  opnd: PRegExprChar;
  Oper: TREOp;
  ch: REChar;
  min_cnt, i: integer;
  TempSet: TRegExprCharset;
begin
  TempSet := [];
  scan := prog;
  while scan <> nil do
  begin
    Next := regnext(scan);
    Oper := PREOp(scan)^;
    case Oper of
      OP_BSUBEXP,
      OP_BSUBEXPCI:
        begin
          // we cannot optimize r.e. if it starts with back reference
          FirstCharSet := RegExprAllSet; //###0.930
          Exit;
        end;
      OP_BOL,
      OP_BOLML:
        ; // Exit; //###0.937
      OP_EOL,
      OP_EOLML:
        begin //###0.948 was empty in 0.947, was EXIT in 0.937
          Include(FirstCharSet, 0);
          if ModifierM then
            for i := 1 to Length(LineSeparators) do
              Include(FirstCharSet, byte(LineSeparators[i]));
          Exit;
        end;
      OP_BOUND,
      OP_NOTBOUND:
        ; //###0.943 ?!!
      OP_ANY,
      OP_ANYML:
        begin // we can better define ANYML !!!
          FirstCharSet := RegExprAllSet; //###0.930
          Exit;
        end;
      OP_ANYDIGIT:
        begin
          FirstCharSet := FirstCharSet + RegExprDigitSet;
          Exit;
        end;
      OP_NOTDIGIT:
        begin
          FirstCharSet := FirstCharSet + (RegExprAllSet - RegExprDigitSet);
          Exit;
        end;
      OP_ANYLETTER:
        begin
          GetCharSetFromWordChars(TempSet);
          FirstCharSet := FirstCharSet + TempSet;
          Exit;
        end;
      OP_NOTLETTER:
        begin
          GetCharSetFromWordChars(TempSet);
          FirstCharSet := FirstCharSet + (RegExprAllSet - TempSet);
          Exit;
        end;
      OP_ANYSPACE:
        begin
          GetCharSetFromSpaceChars(TempSet);
          FirstCharSet := FirstCharSet + TempSet;
          Exit;
        end;
      OP_NOTSPACE:
        begin
          GetCharSetFromSpaceChars(TempSet);
          FirstCharSet := FirstCharSet + (RegExprAllSet - TempSet);
          Exit;
        end;
      OP_ANYVERTSEP:
        begin
          FirstCharSet := FirstCharSet + RegExprLineSeparatorsSet;
          Exit;
        end;
      OP_NOTVERTSEP:
        begin
          FirstCharSet := FirstCharSet + (RegExprAllSet - RegExprLineSeparatorsSet);
          Exit;
        end;
      OP_ANYHORZSEP:
        begin
          FirstCharSet := FirstCharSet + RegExprHorzSeparatorsSet;
          Exit;
        end;
      OP_NOTHORZSEP:
        begin
          FirstCharSet := FirstCharSet + (RegExprAllSet - RegExprHorzSeparatorsSet);
          Exit;
        end;
      OP_EXACTLYCI:
        begin
          ch := (scan + REOpSz + RENextOffSz + RENumberSz)^;
          {$IFDEF UniCode}
          if Ord(ch) <= $FF then
          {$ENDIF}
          begin
            Include(FirstCharSet, byte(ch));
            Include(FirstCharSet, byte(InvertCase(ch)));
          end;
          Exit;
        end;
      OP_EXACTLY:
        begin
          ch := (scan + REOpSz + RENextOffSz + RENumberSz)^;
          {$IFDEF UniCode}
          if Ord(ch) <= $FF then
          {$ENDIF}
            Include(FirstCharSet, byte(ch));
          Exit;
        end;
      OP_ANYOF:
        begin
          GetCharSetFromCharClass(scan + REOpSz + RENextOffSz, False, TempSet);
          FirstCharSet := FirstCharSet + TempSet;
          Exit;
        end;
      OP_ANYBUT:
        begin
          GetCharSetFromCharClass(scan + REOpSz + RENextOffSz, False, TempSet);
          FirstCharSet := FirstCharSet + (RegExprAllSet - TempSet);
          Exit;
        end;
      OP_ANYOFCI:
        begin
          GetCharSetFromCharClass(scan + REOpSz + RENextOffSz, True, TempSet);
          FirstCharSet := FirstCharSet + TempSet;
          Exit;
        end;
      OP_ANYBUTCI:
        begin
          GetCharSetFromCharClass(scan + REOpSz + RENextOffSz, True, TempSet);
          FirstCharSet := FirstCharSet + (RegExprAllSet - TempSet);
          Exit;
        end;
      OP_NOTHING:
        ;
      OP_COMMENT:
        ;
      OP_BACK:
        ;
      Succ(OP_OPEN) .. TREOp(Ord(OP_OPEN) + NSUBEXP - 1):
        begin //###0.929
          FillFirstCharSet(Next);
          Exit;
        end;
      Succ(OP_CLOSE) .. TREOp(Ord(OP_CLOSE) + NSUBEXP - 1):
        begin //###0.929
          FillFirstCharSet(Next);
          Exit;
        end;
      OP_BRANCH:
        begin
          if (PREOp(Next)^ <> OP_BRANCH) // No choice.
          then
            Next := scan + REOpSz + RENextOffSz // Avoid recursion.
          else
          begin
            repeat
              FillFirstCharSet(scan + REOpSz + RENextOffSz);
              scan := regnext(scan);
            until (scan = nil) or (PREOp(scan)^ <> OP_BRANCH);
            Exit;
          end;
        end;
      {$IFDEF ComplexBraces}
      OP_LOOPENTRY:
        begin //###0.925
          //LoopStack [LoopStackIdx] := 0; //###0.940 line removed
          FillFirstCharSet(Next); // execute LOOP
          Exit;
        end;
      OP_LOOP,
      OP_LOOPNG:
        begin //###0.940
          opnd := scan + PRENextOff(AlignToPtr(scan + REOpSz + RENextOffSz + REBracesArgSz * 2))^;
          min_cnt := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz))^;
          FillFirstCharSet(opnd);
          if min_cnt = 0 then
            FillFirstCharSet(Next);
          Exit;
        end;
      {$ENDIF}
      OP_STAR,
      OP_STARNG: //###0.940
        FillFirstCharSet(scan + REOpSz + RENextOffSz);
      OP_PLUS,
      OP_PLUSNG:
        begin //###0.940
          FillFirstCharSet(scan + REOpSz + RENextOffSz);
          Exit;
        end;
      OP_BRACES,
      OP_BRACESNG:
        begin //###0.940
          opnd := scan + REOpSz + RENextOffSz + REBracesArgSz * 2;
          min_cnt := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz))^; // BRACES
          FillFirstCharSet(opnd);
          if min_cnt > 0 then
            Exit;
        end;
      OP_EEND:
        begin
          FirstCharSet := RegExprAllSet; //###0.948
          Exit;
        end;
      else
        begin
          fLastErrorOpcode := Oper;
          Error(reeUnknownOpcodeInFillFirst);
          Exit;
        end;
    end; { of case scan^}
    scan := Next;
  end; { of while scan <> nil}
end; { of procedure FillFirstCharSet
--------------------------------------------------------------}
{$ENDIF}

procedure TRegExpr.InitCharCheckers;
var
  Cnt: integer;
  //
  function Add(AChecker: TRegExprCharChecker): byte;
  begin
    Inc(Cnt);
    if Cnt > High(CharCheckers) then
      raise Exception.Create('Too small CharCheckers array');
    CharCheckers[Cnt - 1] := AChecker;
    Result := Cnt - 1;
  end;
  //
begin
  Cnt := 0;
  FillChar(CharCheckers, SizeOf(CharCheckers), 0);

  CheckerIndex_Word := Add(CharChecker_Word);
  CheckerIndex_NotWord := Add(CharChecker_NotWord);
  CheckerIndex_Space := Add(CharChecker_Space);
  CheckerIndex_NotSpace := Add(CharChecker_NotSpace);
  CheckerIndex_Digit := Add(CharChecker_Digit);
  CheckerIndex_NotDigit := Add(CharChecker_NotDigit);
  CheckerIndex_VertSep := Add(CharChecker_VertSep);
  CheckerIndex_NotVertSep := Add(CharChecker_NotVertSep);
  CheckerIndex_HorzSep := Add(CharChecker_HorzSep);
  CheckerIndex_NotHorzSep := Add(CharChecker_NotHorzSep);
  //CheckerIndex_AllAZ := Add(CharChecker_AllAZ);
  CheckerIndex_LowerAZ := Add(CharChecker_LowerAZ);
  CheckerIndex_UpperAZ := Add(CharChecker_UpperAZ);

  SetLength(CharCheckerInfos, 3);
  with CharCheckerInfos[0] do
  begin
    CharBegin := 'a';
    CharEnd:= 'z';
    CheckerIndex := CheckerIndex_LowerAZ;
  end;
  with CharCheckerInfos[1] do
  begin
    CharBegin := 'A';
    CharEnd := 'Z';
    CheckerIndex := CheckerIndex_UpperAZ;
  end;
  with CharCheckerInfos[2] do
  begin
    CharBegin := '0';
    CharEnd := '9';
    CheckerIndex := CheckerIndex_Digit;
  end;
end;

function TRegExpr.CharChecker_Word(ch: REChar): boolean;
begin
  Result := IsWordChar(ch);
end;

function TRegExpr.CharChecker_NotWord(ch: REChar): boolean;
begin
  Result := not IsWordChar(ch);
end;

function TRegExpr.CharChecker_Space(ch: REChar): boolean;
begin
  Result := IsSpaceChar(ch);
end;

function TRegExpr.CharChecker_NotSpace(ch: REChar): boolean;
begin
  Result := not IsSpaceChar(ch);
end;

function TRegExpr.CharChecker_Digit(ch: REChar): boolean;
begin
  Result := IsDigitChar(ch);
end;

function TRegExpr.CharChecker_NotDigit(ch: REChar): boolean;
begin
  Result := not IsDigitChar(ch);
end;

function TRegExpr.CharChecker_VertSep(ch: REChar): boolean;
begin
  Result := IsLineSeparator(ch);
end;

function TRegExpr.CharChecker_NotVertSep(ch: REChar): boolean;
begin
  Result := not IsLineSeparator(ch);
end;

function TRegExpr.CharChecker_HorzSep(ch: REChar): boolean;
begin
  Result := IsHorzSeparator(ch);
end;

function TRegExpr.CharChecker_NotHorzSep(ch: REChar): boolean;
begin
  Result := not IsHorzSeparator(ch);
end;

function TRegExpr.CharChecker_LowerAZ(ch: REChar): boolean;
begin
  case ch of
    'a' .. 'z':
      Result := True;
    else
      Result := False;
  end;
end;

function TRegExpr.CharChecker_UpperAZ(ch: REChar): boolean;
begin
  case ch of
    'A' .. 'Z':
      Result := True;
    else
      Result := False;
  end;
end;


{$IFDEF RegExpPCodeDump}

function TRegExpr.DumpOp(op: TREOp): RegExprString;
// printable representation of opcode
begin
  case op of
    OP_BOL:
      Result := 'BOL';
    OP_EOL:
      Result := 'EOL';
    OP_BOLML:
      Result := 'BOLML';
    OP_EOLML:
      Result := 'EOLML';
    OP_BOUND:
      Result := 'BOUND'; // ###0.943
    OP_NOTBOUND:
      Result := 'NOTBOUND'; // ###0.943
    OP_ANY:
      Result := 'ANY';
    OP_ANYML:
      Result := 'ANYML'; // ###0.941
    OP_ANYLETTER:
      Result := 'ANYLETTER';
    OP_NOTLETTER:
      Result := 'NOTLETTER';
    OP_ANYDIGIT:
      Result := 'ANYDIGIT';
    OP_NOTDIGIT:
      Result := 'NOTDIGIT';
    OP_ANYSPACE:
      Result := 'ANYSPACE';
    OP_NOTSPACE:
      Result := 'NOTSPACE';
    OP_ANYHORZSEP:
      Result := 'ANYHORZSEP';
    OP_NOTHORZSEP:
      Result := 'NOTHORZSEP';
    OP_ANYVERTSEP:
      Result := 'ANYVERTSEP';
    OP_NOTVERTSEP:
      Result := 'NOTVERTSEP';
    OP_ANYOF:
      Result := 'ANYOF';
    OP_ANYBUT:
      Result := 'ANYBUT';
    OP_ANYOFCI:
      Result := 'ANYOF/CI';
    OP_ANYBUTCI:
      Result := 'ANYBUT/CI';
    OP_BRANCH:
      Result := 'BRANCH';
    OP_EXACTLY:
      Result := 'EXACTLY';
    OP_EXACTLYCI:
      Result := 'EXACTLY/CI';
    OP_NOTHING:
      Result := 'NOTHING';
    OP_COMMENT:
      Result := 'COMMENT';
    OP_BACK:
      Result := 'BACK';
    OP_EEND:
      Result := 'END';
    OP_BSUBEXP:
      Result := 'BSUBEXP';
    OP_BSUBEXPCI:
      Result := 'BSUBEXP/CI';
    Succ(OP_OPEN) .. TREOp(Ord(OP_OPEN) + NSUBEXP - 1): // ###0.929
      Result := Format('OPEN[%d]', [Ord(op) - Ord(OP_OPEN)]);
    Succ(OP_CLOSE) .. TREOp(Ord(OP_CLOSE) + NSUBEXP - 1): // ###0.929
      Result := Format('CLOSE[%d]', [Ord(op) - Ord(OP_CLOSE)]);
    OP_STAR:
      Result := 'STAR';
    OP_PLUS:
      Result := 'PLUS';
    OP_BRACES:
      Result := 'BRACES';
    {$IFDEF ComplexBraces}
    OP_LOOPENTRY:
      Result := 'LOOPENTRY'; // ###0.925
    OP_LOOP:
      Result := 'LOOP'; // ###0.925
    OP_LOOPNG:
      Result := 'LOOPNG'; // ###0.940
    {$ENDIF}
    OP_STARNG:
      Result := 'STARNG'; // ###0.940
    OP_PLUSNG:
      Result := 'PLUSNG'; // ###0.940
    OP_BRACESNG:
      Result := 'BRACESNG'; // ###0.940
  else
    Error(reeDumpCorruptedOpcode);
  end; { of case op }
  Result := ':' + Result;
end; { of function TRegExpr.DumpOp
  -------------------------------------------------------------- }

function PrintableChar(AChar: REChar): RegExprString; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  if AChar < ' ' then
    Result := '#' + IntToStr(Ord(AChar))
  else
    Result := AChar;
end;

function TRegExpr.Dump: RegExprString;
// dump a regexp in vaguely comprehensible form
var
  s: PRegExprChar;
  op: TREOp; // Arbitrary non-END op.
  next: PRegExprChar;
  i, NLen: integer;
  Diff: PtrInt;
  iByte: byte;
begin
  if not IsProgrammOk then
    Exit;

  op := OP_EXACTLY;
  Result := '';
  s := programm + REOpSz;
  while op <> OP_EEND do
  begin // While that wasn't END last time...
    op := s^;
    Result := Result + Format('%2d%s', [s - programm, DumpOp(s^)]);
    // Where, what.
    next := regnext(s);
    if next = nil // Next ptr.
    then
      Result := Result + ' (0)'
    else
    begin
      if next > s
      // ###0.948 PWideChar subtraction workaround (see comments in Tail method for details)
      then
        Diff := next - s
      else
        Diff := -(s - next);
      Result := Result + Format(' (%d) ', [(s - programm) + Diff]);
    end;
    Inc(s, REOpSz + RENextOffSz);
    if (op = OP_ANYOF) or (op = OP_ANYOFCI) or (op = OP_ANYBUT) or (op = OP_ANYBUTCI) then
    begin
      repeat
        case s^ of
          OpKind_End:
            begin
              Inc(s);
              Break;
            end;
          OpKind_Range:
            begin
              Result := Result + 'Rng(';
              Inc(s);
              Result := Result + PrintableChar(s^) + '-';
              Inc(s);
              Result := Result + PrintableChar(s^);
              Result := Result + ') ';
              Inc(s);
            end;
          OpKind_MetaClass:
            begin
              Inc(s);
              Result := Result + '\' + PrintableChar(s^) + ' ';
              Inc(s);
            end;
          OpKind_Char:
            begin
              Inc(s);
              NLen := PLongInt(s)^;
              Inc(s, RENumberSz);
              Result := Result + 'Ch(';
              for i := 1 to NLen do
              begin
                Result := Result + PrintableChar(s^);
                Inc(s);
              end;
              Result := Result + ') ';
            end;
          else
            Error(reeDumpCorruptedOpcode);
        end;
      until false;
    end;
    if (op = OP_EXACTLY) or (op = OP_EXACTLYCI) then
    begin
      // Literal string, where present.
      NLen := PLongInt(s)^;
      Inc(s, RENumberSz);
      for i := 1 to NLen do
      begin
        Result := Result + PrintableChar(s^);
        Inc(s);
      end;
    end;
    if (op = OP_BSUBEXP) or (op = OP_BSUBEXPCI) then
    begin
      Result := Result + ' \' + IntToStr(Ord(s^));
      Inc(s);
    end;
    if (op = OP_BRACES) or (op = OP_BRACESNG) then
    begin // ###0.941
      // show min/max argument of braces operator
      Result := Result + Format('{%d,%d}', [PREBracesArg(AlignToInt(s))^,
        PREBracesArg(AlignToInt(s + REBracesArgSz))^]);
      Inc(s, REBracesArgSz * 2);
    end;
    {$IFDEF ComplexBraces}
    if (op = OP_LOOP) or (op = OP_LOOPNG) then
    begin // ###0.940
      Result := Result + Format(' -> (%d) {%d,%d}',
        [(s - programm - (REOpSz + RENextOffSz)) +
        PRENextOff(AlignToPtr(s + 2 * REBracesArgSz))^,
        PREBracesArg(AlignToInt(s))^,
        PREBracesArg(AlignToInt(s + REBracesArgSz))^]);
      Inc(s, 2 * REBracesArgSz + RENextOffSz);
    end;
    {$ENDIF}
    Result := Result + #$d#$a;
  end; { of while }

  // Header fields of interest.
  if reganchored <> #0 then
    Result := Result + 'Anchored; ';
  if regmustString <> '' then
    Result := Result + 'Must have: "' + regmustString + '"; ';

  {$IFDEF UseFirstCharSet} // ###0.929
  Result := Result + #$d#$a'First charset: ';
  if FirstCharSet = [] then
    Result := Result + '<empty set>'
  else
  if FirstCharSet = RegExprAllSet then
    Result := Result + '<all chars>'
  else
  for iByte := 0 to 255 do
    if iByte in FirstCharSet then
      Result := Result + PrintableChar(REChar(iByte));
  {$ENDIF}
  Result := Result + #$d#$a;
end; { of function TRegExpr.Dump
  -------------------------------------------------------------- }
{$ENDIF}

{$IFDEF reRealExceptionAddr}
{$OPTIMIZATION ON}
// ReturnAddr works correctly only if compiler optimization is ON
// I placed this method at very end of unit because there are no
// way to restore compiler optimization flag ...
{$ENDIF}

procedure TRegExpr.Error(AErrorID: integer);
  {$IFDEF reRealExceptionAddr}
  function ReturnAddr: Pointer; // ###0.938
  asm
    mov  eax,[ebp+4]
  end;
  {$ENDIF}
var
  e: ERegExpr;
begin
  fLastError := AErrorID; // dummy stub - useless because will raise exception
  if AErrorID < 1000 // compilation error ?
  then
    e := ERegExpr.Create(ErrorMsg(AErrorID) // yes - show error pos
      + ' (pos ' + IntToStr(CompilerErrorPos) + ')')
  else
    e := ERegExpr.Create(ErrorMsg(AErrorID));
  e.ErrorCode := AErrorID;
  e.CompilerErrorPos := CompilerErrorPos;
  raise e
    {$IFDEF reRealExceptionAddr}
    at ReturnAddr; // ###0.938
    {$ENDIF}
end; { of procedure TRegExpr.Error
  -------------------------------------------------------------- }

(*
  PCode persistence:
  FirstCharSet
  programm, regsize
  reganchored // -> programm
  regmust, regmustlen // -> programm
  fExprIsCompiled
*)

// be carefull - placed here code will be always compiled with
// compiler optimization flag

initialization

  RegExprInvertCaseFunction := TRegExpr.InvertCaseFunction;

end.
