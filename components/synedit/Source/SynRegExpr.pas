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

{
program is essentially a linear encoding
of a nondeterministic finite-state machine (aka syntax charts or
"railroad normal form" in parsing technology). Each node is an opcode
plus a "next" pointer, possibly plus an operand. "Next" pointers of
all nodes except BRANCH implement concatenation; a "next" pointer with
a BRANCH on both ends of it connects two alternatives. (Here we
have one of the subtle syntax dependencies: an individual BRANCH (as
opposed to a collection of them) is never concatenated with anything
because of operator precedence.) The operand of some types of node is
a literal string; for others, it is a node leading into a sub-FSM. In
particular, the operand of a BRANCH node is the first node of the branch.
(NB this is *not* a tree structure: the tail of the branch connects
to the thing following the set of BRANCHes.)
}

interface

{ off $DEFINE DebugSynRegExpr }
// ======== Determine compiler
{$I SynEdit.inc}
// ======== Define base compiler options
{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$IFDEF FPC}
  {$MODE DELPHI} // Delphi-compatible mode in FreePascal
  {$INLINE ON}
{$ENDIF}
// ======== Define options for TRegExpr engine
{$DEFINE UnicodeRE} // Use WideChar for characters and UnicodeString/WideString for strings
{ off $DEFINE UnicodeEx} // Support Unicode >0xFFFF, e.g. emoji, e.g. "." must find 2 WideChars of 1 emoji
{ off $DEFINE UseWordChars} // Use WordChars property, otherwise fixed list 'a'..'z','A'..'Z','0'..'9','_' 
{ off $DEFINE UseSpaceChars} // Use SpaceChars property, otherwise fixed list
{ off $DEFINE UseLineSep} // Use LineSeparators property, otherwise fixed line-break chars
{$IFDEF UNICODE}
  {$IFNDEF UnicodeRE}
  {$MESSAGE ERROR 'You cannot undefine UnicodeRE for Unicode Delphi versions'}
  {$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE FastUnicodeData} // Use arrays for UpperCase/LowerCase/IsWordChar, they take 320K more memory
{$ENDIF}
{ off $DEFINE RegExpWithStackOverflowCheck} // Check the recursion depth and abort matching before stack overflows (available only for some OS/CPU)
{$DEFINE UseFirstCharSet} // Enable optimization, which finds possible first chars of input string
{$DEFINE RegExpPCodeDump} // Enable method Dump() to show opcode as string
{$IFNDEF FPC} // Not supported in FreePascal
  {$DEFINE reRealExceptionAddr} // Exceptions will point to appropriate source line, not to Error procedure
{$ENDIF}
{$DEFINE ComplexBraces} // Support braces in complex cases
{$IFNDEF UnicodeRE}
  {$UNDEF UnicodeEx}
  {$UNDEF FastUnicodeData}
{$ENDIF}
{.$DEFINE Compat} // Enable compatability methods/properties for forked version in Free Pascal 3.0
// ======== Define Pascal-language options
// Asserts used to catch 'strange bugs' in TRegExpr implementation (when something goes
// completely wrong). You can swith asserts on/off with help of {$C+}/{$C-} compiler options.
{$IFDEF SYN_COMPILER_3_UP} { $DEFINE WITH_REGEX_ASSERT} {$ENDIF}
{$IFDEF FPC}{$IFOPT C+} {$DEFINE WITH_REGEX_ASSERT} {$ENDIF}{$ENDIF} // Only if compile with -Sa
// Define 'use subroutine parameters default values' option (do not edit this definition).
{$IFDEF SYN_COMPILER_4_UP} {$DEFINE DefParam} {$ENDIF}
{$IFDEF FPC} {$DEFINE DefParam} {$ENDIF}
// Define 'OverMeth' options, to use method overloading (do not edit this definitions).
{$IFDEF SYN_COMPILER_5_UP} {$DEFINE OverMeth} {$ENDIF}
{$IFDEF FPC} {$DEFINE OverMeth} {$ENDIF}
// Define 'InlineFuncs' options, to use inline keyword (do not edit this definitions).
// Disabled for HeidiSQL due to compiler errors
{$IFDEF SYN_COMPILER_8_UP} {.$DEFINE InlineFuncs} {$ENDIF}
{$IFDEF FPC} {$DEFINE InlineFuncs} {$ENDIF}

{$IFDEF RegExpWithStackOverflowCheck} // Define the stack checking algorithm for the current platform/CPU
  {$IF defined(Linux) or defined(Windows)}{$IF defined(CPU386) or defined(CPUX86_64)}
    {$DEFINE RegExpWithStackOverflowCheck_DecStack_Frame} // Stack-pointer decrements // use getframe over Sptr()
  {$ENDIF}{$ENDIF}
{$ENDIF}
uses
  SysUtils, // Exception
  {$IFDEF SYN_DELPHI_2009_UP}
    {$IFDEF SYN_COMPILER_16_UP}
    System.Character,
    {$ELSE}
    Character,
    {$ENDIF}
  {$ENDIF}
  Classes; // TStrings in Split method

type
  {$IFNDEF FPC}
  // Delphi doesn't have PtrInt but has NativeInt
  // but unfortunately NativeInt is declared wrongly in several versions
    {$IF SizeOf(Pointer)=4}
    PtrInt = Integer;
    PtrUInt = Cardinal;
    {$ELSE}
    PtrInt = Int64;
    PtrUInt = UInt64;
    {$IFEND}
  {$ENDIF}

  {$IFDEF UnicodeRE}
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
  PRegExprChar = PAnsiChar;
  RegExprString = AnsiString;
  REChar = AnsiChar;
  {$ENDIF}
  TREOp = REChar; // internal opcode type
  PREOp = ^TREOp;

type
  TRegExprCharset = set of Byte;

const
  // Escape char ('\' in common r.e.) used for escaping metachars (\w, \d etc)
  EscChar = '\';

  // Substitute method: prefix of group reference: $1 .. $9 and $<name>
  SubstituteGroupChar = '$';

  RegExprModifierI: Boolean = False; // default value for ModifierI
  RegExprModifierR: Boolean = True; // default value for ModifierR
  RegExprModifierS: Boolean = True; // default value for ModifierS
  RegExprModifierG: Boolean = True; // default value for ModifierG
  RegExprModifierM: Boolean = False; // default value for ModifierM
  RegExprModifierX: Boolean = False; // default value for ModifierX

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

  {$IFDEF UseLineSep}
  // default value for LineSeparators
  RegExprLineSeparators: RegExprString = #$d#$a#$b#$c
    {$IFDEF UnicodeRE}
    + #$2028#$2029#$85
    {$ENDIF};
  {$ENDIF}

  // Tab and Unicode category "Space Separator":
  // https://www.compart.com/en/unicode/category/Zs
  RegExprHorzSeparators: RegExprString = #9#$20#$A0
    {$IFDEF UnicodeRE}
    + #$1680#$2000#$2001#$2002#$2003#$2004#$2005#$2006#$2007#$2008#$2009#$200A#$202F#$205F#$3000
    {$ENDIF};

  RegExprUsePairedBreak: Boolean = True;
  RegExprReplaceLineBreak: RegExprString = sLineBreak;

const
  // Increment/keep-capacity for the size of arrays holding 'Group' related data
  // e.g., GrpBounds, GrpIndexes, GrpOpCodes and GrpNames
  RegexGroupCountIncrement = 50;

  // Max possible amount of groups.
  // Don't change it! It's defined by internal TRegExpr design.
  RegexMaxMaxGroups = MaxInt div 16;

  // Max depth of recursion for (?R) and (?1)..(?9)
  RegexMaxRecursion = 20;

type
  TRegExprModifiers = record
    I: Boolean;
       // Case-insensitive.
    R: Boolean;
       // Extended syntax for Russian ranges in [].
       // If True, then а-я additionally includes letter 'ё',
       // А-Я additionally includes 'Ё', and а-Я includes all Russian letters.
       // Turn it off if it interferes with your national alphabet.
    S: Boolean;
       // Dot '.' matches any char, otherwise only [^\n].
    G: Boolean;
       // Greedy. Switching it off switches all operators to non-greedy style,
       // so if G=False, then '*' works like '*?', '+' works like '+?' and so on.
    M: Boolean;
       // Treat string as multiple lines. It changes `^' and `$' from
       // matching at only the very start/end of the string to the start/end
       // of any line anywhere within the string.
    X: Boolean;
       // Allow comments in regex using # char.
  end;

function IsModifiersEqual(const A, B: TRegExprModifiers): Boolean;

type
  TRegExpr = class;
  TRegExprReplaceFunction = function(ARegExpr: TRegExpr): RegExprString of object;
  TRegExprCharChecker = function(ch: REChar): Boolean of object;
  TRegExprCharCheckerArray = array[0 .. 30] of TRegExprCharChecker;
  TRegExprCharCheckerInfo = record
    CharBegin, CharEnd: REChar;
    CheckerIndex: Integer;
  end;
  TRegExprCharCheckerInfos = array of TRegExprCharCheckerInfo;

  TRegExprAnchor = (
    raNone,     // Not anchored
    raBOL,      // Must start at BOL
    raEOL,      // Must start at EOL (maybe look behind)
    raContinue, // Must start at continue pos \G
    raOnlyOnce  // Starts with .* must match from the start pos only. Must not be tried from a later pos
  );

  TRegExprFindFixedLengthFlag = (
    flfForceToStopAt,
    flfReturnAtNextNil,
    flfSkipLookAround
  );
  TRegExprFindFixedLengthFlags = set of TRegExprFindFixedLengthFlag;

  {$IFDEF Compat}
  TRegExprInvertCaseFunction = function(const Ch: REChar): REChar of object;
  {$ENDIF}

  {$IFDEF ComplexBraces}
  POpLoopInfo = ^TOpLoopInfo;
  TOpLoopInfo = record
    Count: Integer;
    CurrentRegInput: PRegExprChar;
    BackTrackingAsAtom: Boolean;
    OuterLoop: POpLoopInfo; // for nested loops
  end;
  {$ENDIF}


  TRegExprBounds = record
    GrpStart: array of PRegExprChar; // pointer to group start in InputString
    GrpEnd: array of PRegExprChar; // pointer to group end in InputString
  end;
  TRegExprBoundsArray = array[0 .. RegexMaxRecursion] of TRegExprBounds;

  PRegExprLookAroundInfo = ^TRegExprLookAroundInfo;
  TRegExprLookAroundInfo = record
    InputPos: PRegExprChar; // pointer to start of look-around in the input string
    savedInputCurrentEnd: PRegExprChar; // pointer to start of look-around in the input string
    IsNegative, HasMatchedToEnd: Boolean;
    IsBackTracking: Boolean;
    OuterInfo: PRegExprLookAroundInfo; // for nested lookaround
  end;

  TRegExprGroupName = record
    Name: RegExprString;
    Index: Integer;
  end;

  { TRegExprGroupNameList }

  TRegExprGroupNameList = object
    Names: array of TRegExprGroupName;
    NameCount: Integer;
    // get index of group (subexpression) by name, to support named groups
    // like in Python: (?P<name>regex)
    function MatchIndexFromName(const AName: RegExprString): Integer;
    procedure Clear;
    procedure Add(const AName: RegExprString; AnIndex: Integer);
  end;

  { TRegExpr }

  TRegExpr = class
  private
    FAllowBraceWithoutMin: Boolean;
    FAllowUnsafeLookBehind: Boolean;
    FAllowLiteralBraceWithoutRange: Boolean;
    FMatchesCleared: Boolean;
    fRaiseForRuntimeError: Boolean;
    GrpBounds: TRegExprBoundsArray;
    GrpIndexes: array of Integer; // map global group index to _capturing_ group index
    GrpNames: TRegExprGroupNameList; // names of groups, if non-empty
    GrpBacktrackingAsAtom: array of Boolean; // close of group[i] has set IsBacktrackingGroupAsAtom
    IsBacktrackingGroupAsAtom: Boolean;  // Backtracking an entire atomic group that had matched.
    // Once the group matched it should not try any alternative matches within the group
    // If the pattern after the group fails, then the group fails (regardless of any alternative match in the group)

    GrpOpCodes: array of PRegExprChar; // pointer to opcode of group[i] (used by OP_SUBCALL*)
    GrpCount, ParsedGrpCount: Integer;

    {$IFDEF ComplexBraces}
    CurrentLoopInfoListPtr: POpLoopInfo;
    {$ENDIF}

    // The "internal use only" fields to pass info from compile
    // to execute that permits the execute phase to run lots faster on
    // simple cases.

    regAnchored: TRegExprAnchor; // is the match anchored (at beginning-of-line only)?
    // regAnchored permits very fast decisions on suitable starting points
    // for a match, cutting down the work a lot. regMust permits fast rejection
    // of lines that cannot possibly match. The regMust tests are costly enough
    // that regcomp() supplies a regMust only if the r.e. contains something
    // potentially expensive (at present, the only such thing detected is * or +
    // at the start of the r.e., which can involve a lot of backup). regMustLen is
    // supplied because the test in regexec() needs it and regcomp() is computing
    // it anyway.

    regMust: PRegExprChar; // string (pointer into program) that match must include, or nil
    regMustLen: Integer; // length of regMust string
    regMustString: RegExprString; // string which must occur in match (got from regMust/regMustLen)
    LookAroundInfoList: PRegExprLookAroundInfo;
    //regNestedCalls: integer; // some attempt to prevent 'catastrophic backtracking' but not used
    CurrentSubCalled: Integer;

    FMinMatchLen: integer;
    {$IFDEF UseFirstCharSet}
    FirstCharSet: TRegExprCharset;
    FirstCharArray: array[Byte] of Boolean;
    {$ENDIF}

    // work variables for Exec routines - save stack in recursion
    regInput: PRegExprChar; // pointer to currently handling char of input string
    fInputStart: PRegExprChar; // pointer to first char of input string
    fInputContinue: PRegExprChar; // pointer to char specified with Exec(AOffset), or start pos of ExecNext
    fInputEnd: PRegExprChar; // pointer after last char of input string
    fInputCurrentEnd: PRegExprChar; // pointer after last char of the current visible part of input string (can be limited by look-behind)
    fRegexStart: PRegExprChar; // pointer to first char of regex
    fRegexEnd: PRegExprChar; // pointer after last char of regex
    regRecursion: Integer; // current level of recursion (?R) (?1); always 0 if no recursion is used

    // work variables for compiler's routines
    regParse: PRegExprChar; // pointer to currently handling char of regex
    regNumBrackets: Integer; // count of () brackets
    regDummy: array [0..8 div SizeOf(REChar)] of REChar; // dummy pointer, used to detect 1st/2nd pass of Compile
                      // if p=@regDummy, it is pass-1: opcode memory is not yet allocated
    programm: PRegExprChar; // pointer to opcode, =nil in pass-1
    regCode: PRegExprChar; // pointer to last emitted opcode; changing in pass-2, but =@regDummy in pass-1
    regCodeSize: Integer; // total opcode size in REChars
    regCodeWork: PRegExprChar; // pointer to opcode, to first code after MAGIC
    regExactlyLen: PLongInt; // pointer to length of substring of OP_EXACTLY* inside opcode
    fSecondPass: Boolean; // true inside pass-2 of Compile

    fExpression: RegExprString; // regex string
    fInputString: RegExprString; // input string
    fLastError: Integer; // Error call sets code of LastError
    fLastErrorOpcode: TREOp;
    fLastErrorSymbol: REChar;

    fModifiers: TRegExprModifiers; // regex modifiers
    fCompModifiers: TRegExprModifiers; // compiler's copy of modifiers
    fProgModifiers: TRegExprModifiers; // modifiers values from last programm compilation

    {$IFDEF UseSpaceChars}
    fSpaceChars: RegExprString;
    {$ENDIF}
    {$IFDEF UseWordChars}
    fWordChars: RegExprString;
    {$ENDIF}

    {$IFDEF UseLineSep}
    fLineSeparators: RegExprString;
    {$ENDIF}

    fUsePairedBreak: Boolean;
    fReplaceLineEnd: RegExprString; // string to use for "\n" in Substitute method

    fSlowChecksSizeMax: Integer;
    // Exec() param ASlowChecks is set to True, when Length(InputString)<SlowChecksSizeMax
    // This ASlowChecks enables to use regMustString optimization

    {$IFDEF UseLineSep}
      {$IFNDEF UnicodeRE}
      fLineSepArray: array[Byte] of Boolean;
      {$ENDIF}
    {$ENDIF}

    CharCheckers: TRegExprCharCheckerArray;
    CharCheckerInfos: TRegExprCharCheckerInfos;
    CheckerIndex_Word: Byte;
    CheckerIndex_NotWord: Byte;
    CheckerIndex_Digit: Byte;
    CheckerIndex_NotDigit: Byte;
    CheckerIndex_Space: Byte;
    CheckerIndex_NotSpace: Byte;
    CheckerIndex_HorzSep: Byte;
    CheckerIndex_NotHorzSep: Byte;
    CheckerIndex_VertSep: Byte;
    CheckerIndex_NotVertSep: Byte;
    CheckerIndex_LowerAZ: Byte;
    CheckerIndex_UpperAZ: Byte;
    CheckerIndex_AnyLineBreak: Byte;
    {$IFDEF RegExpWithStackOverflowCheck_DecStack_Frame}
    StackLimit: Pointer;
    {$ENDIF}

    {$IFDEF Compat}
    fUseUnicodeWordDetection: Boolean;
    fInvertCase: TRegExprInvertCaseFunction;
    fEmptyInputRaisesError: Boolean;
    fUseOsLineEndOnReplace: Boolean;
    function OldInvertCase(const Ch: REChar): REChar;
    function GetLinePairedSeparator: RegExprString;
    procedure SetLinePairedSeparator(const AValue: RegExprString);
    procedure SetUseOsLineEndOnReplace(AValue: Boolean);
    {$ENDIF}

    procedure InitCharCheckers;
    function CharChecker_Word(ch: REChar): Boolean;
    function CharChecker_NotWord(ch: REChar): Boolean;
    function CharChecker_Space(ch: REChar): Boolean;
    function CharChecker_NotSpace(ch: REChar): Boolean;
    function CharChecker_Digit(ch: REChar): Boolean;
    function CharChecker_NotDigit(ch: REChar): Boolean;
    function CharChecker_HorzSep(ch: REChar): Boolean;
    function CharChecker_NotHorzSep(ch: REChar): Boolean;
    function CharChecker_VertSep(ch: REChar): Boolean;
    function CharChecker_NotVertSep(ch: REChar): Boolean;
    function CharChecker_AnyLineBreak(ch: REChar): Boolean;
    function CharChecker_LowerAZ(ch: REChar): Boolean;
    function CharChecker_UpperAZ(ch: REChar): Boolean;
    function DumpCheckerIndex(N: Byte): RegExprString;
    function DumpCategoryChars(ch, ch2: REChar; Positive: Boolean): RegExprString;

    procedure ClearMatches;
    procedure ClearInternalExecData;
    procedure InitInternalGroupData;
    function FindInCharClass(ABuffer: PRegExprChar; AChar: REChar; AIgnoreCase: Boolean): Boolean;
    procedure GetCharSetFromCharClass(ABuffer: PRegExprChar; AIgnoreCase: Boolean; var ARes: TRegExprCharset);
    procedure GetCharSetFromSpaceChars(var ARes: TRegExprCharset); {$IFDEF InlineFuncs}inline;{$ENDIF}
    procedure GetCharSetFromWordChars(var ARes: TRegExprCharSet); {$IFDEF InlineFuncs}inline;{$ENDIF}
    function IsWordChar(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    function IsSpaceChar(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    function IsCustomLineSeparator(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    {$IFDEF UseLineSep}
    procedure InitLineSepArray;
    {$ENDIF}
    procedure FindGroupName(APtr, AEndPtr: PRegExprChar; AEndChar: REChar; var AName: RegExprString);

    // Mark programm as having to be [re]compiled
    procedure InvalidateProgramm;

    // Check if we can use compiled regex, compile it if something changed
    function IsProgrammOk: Boolean;

    procedure SetExpression(const AStr: RegExprString);

    function GetModifierStr: RegExprString;
    procedure SetModifierStr(const AStr: RegExprString);
    function GetModifierG: Boolean;
    function GetModifierI: Boolean;
    function GetModifierM: Boolean;
    function GetModifierR: Boolean;
    function GetModifierS: Boolean;
    function GetModifierX: Boolean;
    procedure SetModifierG(AValue: Boolean);
    procedure SetModifierI(AValue: Boolean);
    procedure SetModifierM(AValue: Boolean);
    procedure SetModifierR(AValue: Boolean);
    procedure SetModifierS(AValue: Boolean);
    procedure SetModifierX(AValue: Boolean);

    { ==================== Compiler section =================== }
    // compile a regular expression into internal code
    function CompileRegExpr(ARegExp: PRegExprChar): Boolean;

    // set the next-pointer at the end of a node chain
    procedure Tail(p: PRegExprChar; val: PRegExprChar);

    // regoptail - regtail on operand of first argument; nop if operandless
    procedure OpTail(p: PRegExprChar; val: PRegExprChar);

    // regnode - emit a node, return location
    function EmitNode(op: TREOp): PRegExprChar;

    // emit OP_BRANCH (and fillchars)
    function EmitBranch: PRegExprChar; {$IFDEF InlineFuncs}inline;{$ENDIF}

    // emit (if appropriate) a byte of code
    procedure EmitC(ch: REChar); {$IFDEF InlineFuncs}inline;{$ENDIF}

    // emit LongInt value
    procedure EmitInt(AValue: LongInt); {$IFDEF InlineFuncs}inline;{$ENDIF}

    // for groups
    function EmitNodeWithGroupIndex(op: TREOp; AIndex: Integer): PRegExprChar;

    // emit back-reference to group
    function EmitGroupRef(AIndex: Integer; AIgnoreCase: Boolean): PRegExprChar;

    {$IFDEF FastUnicodeData}
    procedure FindCategoryName(var scan: PRegExprChar; var ch1, ch2: REChar);
    function EmitCategoryMain(APositive: Boolean): PRegExprChar;
    {$ENDIF}

    // insert an operator in front of already-emitted operand
    // Means relocating the operand.
    procedure InsertOperator(op: TREOp; opnd: PRegExprChar; sz: Integer);
    procedure RemoveOperator(opnd: PRegExprChar; sz: Integer);

    // regular expression, i.e. main body or parenthesized thing
    function ParseReg(InBrackets: Boolean; var FlagParse: Integer): PRegExprChar;
    function DoParseReg(InBrackets, IndexBrackets: Boolean; var FlagParse: Integer; BeginGroupOp, EndGroupOP: TReOp): PRegExprChar;

    // one alternative of an | operator
    function ParseBranch(var FlagParse: Integer): PRegExprChar;

    // something followed by possible [*+?]
    function ParsePiece(var FlagParse: Integer): PRegExprChar;

    function HexDig(Ch: REChar): Integer;

    function UnQuoteChar(var APtr, AEnd: PRegExprChar): REChar;

    // the lowest level
    function ParseAtom(var FlagParse: Integer): PRegExprChar;

    // current pos in r.e. - for error hanling
    function GetCompilerErrorPos: PtrInt;

    {$IFDEF UseFirstCharSet}
    procedure FillFirstCharSet(prog: PRegExprChar);
    {$ENDIF}

    function IsPartFixedLength(var prog: PRegExprChar; var op: TREOp; var AMinLen, AMaxLen: integer; StopAt: TREOp; StopMaxProg: PRegExprChar; Flags: TRegExprFindFixedLengthFlags): boolean;

    { ===================== Matching section =================== }
    // repeatedly match something simple, report how many
    function FindRepeated(p: PRegExprChar; AMax: Integer): Integer;

    // dig the "next" pointer out of a node
    function regNext(p: PRegExprChar): PRegExprChar;
    function regNextQuick(p: PRegExprChar): PRegExprChar; {$IFDEF FPC}inline;{$ENDIF}

    // dig the "last" pointer out of a chain of node
    function regLast(p: PRegExprChar): PRegExprChar;

    // recursively matching routine
    function MatchPrim(prog: PRegExprChar): Boolean;

    // match at specific position only, called from ExecPrim
    function MatchAtOnePos(APos: PRegExprChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}

    // Exec for stored InputString
    function ExecPrim(AOffset: Integer; ASlowChecks, ABackward: Boolean; ATryMatchOnlyStartingBefore: Integer): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    function ExecPrimProtected(AOffset: Integer; ASlowChecks, ABackward: Boolean; ATryMatchOnlyStartingBefore: Integer): Boolean;

    function GetSubExprCount: Integer;
    function GetMatchPos(Idx: Integer): PtrInt;
    function GetMatchLen(Idx: Integer): PtrInt;
    function GetMatch(Idx: Integer): RegExprString;

    procedure SetInputString(const AInputString: RegExprString);
    procedure SetInputRange(AStart, AEnd, AContinueAnchor: PRegExprChar);

    {$IFDEF UseLineSep}
    procedure SetLineSeparators(const AStr: RegExprString);
    {$ENDIF}
    procedure SetUsePairedBreak(AValue: Boolean);

  protected
    // Default handler raises exception ERegExpr with
    // Message = ErrorMsg (AErrorID), ErrorCode = AErrorID
    // and CompilerErrorPos = value of property CompilerErrorPos.
    procedure Error(AErrorID: Integer); virtual; // error handler.

  public
    constructor Create; {$IFDEF OverMeth} overload;
    constructor Create(const AExpression: RegExprString); overload;
    {$ENDIF}
    destructor Destroy; override;

    class function VersionMajor: Integer;
    class function VersionMinor: Integer;

    // match a programm against a string AInputString
    // !!! Exec store AInputString into InputString property
    // For Delphi 5 and higher available overloaded versions - first without
    // parameter (uses already assigned to InputString property value)
    // and second that has int parameter and is same as ExecPos
    function Exec(const AInputString: RegExprString): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    {$IFDEF OverMeth} overload;
    function Exec: Boolean; overload; {$IFDEF InlineFuncs}inline;{$ENDIF}
    function Exec(AOffset: Integer): Boolean; overload; {$IFDEF InlineFuncs}inline;{$ENDIF}
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
    function ExecNext(ABackward: Boolean {$IFDEF DefParam} = False{$ENDIF}): Boolean;

    // find match for InputString starting from AOffset position
    // (AOffset=1 - first char of InputString)
    function ExecPos(AOffset: Integer {$IFDEF DefParam} = 1{$ENDIF}): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
    {$IFDEF OverMeth} overload;
    // find match for InputString at AOffset.
    // if ATryOnce=True then only match exactly at AOffset (like anchor \G)
    // if ATryMatchOnlyStartingBefore then only when the match can start before
    // that position: Result := MatchPos[0] < ATryMatchOnlyStartingBefore;
    function ExecPos(AOffset: Integer; ATryOnce, ABackward: Boolean): Boolean; overload; {$IFDEF InlineFuncs}inline;{$ENDIF}
    function ExecPos(AOffset, ATryMatchOnlyStartingBefore: Integer): Boolean; overload; {$IFDEF InlineFuncs}inline;{$ENDIF}
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
      AUseSubstitution: Boolean{$IFDEF DefParam} = False{$ENDIF})
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

    {$IFDEF Compat}
    function ExecPos(AOffset: Integer; ATryOnce: Boolean): Boolean; overload; deprecated 'Use modern form of ExecPos()';
    class function InvertCaseFunction(const Ch: REChar): REChar; deprecated 'This has no effect now';
    property InvertCase: TRegExprInvertCaseFunction read fInvertCase write fInvertCase; deprecated 'This has no effect now';
    property UseUnicodeWordDetection: Boolean read fUseUnicodeWordDetection write fUseUnicodeWordDetection; deprecated 'This has no effect, use {$DEFINE UnicodeRE} instead';
    property LinePairedSeparator: RegExprString read GetLinePairedSeparator write SetLinePairedSeparator; deprecated 'This has no effect now';
    property EmptyInputRaisesError: Boolean read fEmptyInputRaisesError write fEmptyInputRaisesError; deprecated 'This has no effect now';
    property UseOsLineEndOnReplace: Boolean read fUseOsLineEndOnReplace write SetUseOsLineEndOnReplace; deprecated 'Use property ReplaceLineEnd instead';
    {$ENDIF}

    // Returns ID of last error, 0 if no errors (unusable if
    // Error method raises exception) and clear internal status
    // into 0 (no errors).
    function LastError: Integer;

    // Returns Error message for error with ID = AErrorID.
    function ErrorMsg(AErrorID: Integer): RegExprString; virtual;

    // Re-compile regex
    procedure Compile;

    {$IFDEF RegExpPCodeDump}
    // Show compiled regex in textual form
    function Dump(Indent: Integer = 0): RegExprString;
    // Show single opcode in textual form
    function DumpOp(op: TREOp): RegExprString;
    {$ENDIF}

    function IsCompiled: Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}

    // Opcode contains only operations for fixed match length: EXACTLY*, ANY*, etc
    function IsFixedLength(var op: TREOp; var ALen: Integer): Boolean;
    function IsFixedLengthEx(var op: TREOp; var AMinLen, AMaxLen: integer): boolean;

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

    property ModifierI: Boolean read GetModifierI write SetModifierI;
    property ModifierR: Boolean read GetModifierR write SetModifierR;
    property ModifierS: Boolean read GetModifierS write SetModifierS;
    property ModifierG: Boolean read GetModifierG write SetModifierG;
    property ModifierM: Boolean read GetModifierM write SetModifierM;
    property ModifierX: Boolean read GetModifierX write SetModifierX;

    // returns current input string (from last Exec call or last assign
    // to this property).
    // Any assignment to this property clear Match* properties !
    property InputString: RegExprString read fInputString write SetInputString;
    // SetInputSubString
    // Only looks at copy(AInputString, AInputStartPos, AInputLen)
    procedure SetInputSubString(const AInputString: RegExprString; AInputStartPos, AInputLen: Integer);

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
    property SubExprMatchCount: Integer read GetSubExprCount;

    // pos of entrance subexpr. #Idx into tested in last Exec*
    // string. First subexpr. has Idx=1, last - MatchCount,
    // whole r.e. has Idx=0.
    // Returns -1 if in r.e. no such subexpr. or this subexpr.
    // not found in input string.
    property MatchPos[Idx: Integer]: PtrInt read GetMatchPos;

    // len of entrance subexpr. #Idx r.e. into tested in last Exec*
    // string. First subexpr. has Idx=1, last - MatchCount,
    // whole r.e. has Idx=0.
    // Returns -1 if in r.e. no such subexpr. or this subexpr.
    // not found in input string.
    // Remember - MatchLen may be 0 (if r.e. match empty string) !
    property MatchLen[Idx: Integer]: PtrInt read GetMatchLen;

    // == copy (InputString, MatchPos [Idx], MatchLen [Idx])
    // Returns '' if in r.e. no such subexpr. or this subexpr.
    // not found in input string.
    property Match[Idx: Integer]: RegExprString read GetMatch;

    // get index of group (subexpression) by name, to support named groups
    // like in Python: (?P<name>regex)
    function MatchIndexFromName(const AName: RegExprString): Integer;

    function MatchFromName(const AName: RegExprString): RegExprString;

    // Returns position in r.e. where compiler stopped.
    // Useful for error diagnostics
    property CompilerErrorPos: PtrInt read GetCompilerErrorPos;

    {$IFDEF UseSpaceChars}
    // Contains chars, treated as /s (initially filled with RegExprSpaceChars
    // global constant)
    property SpaceChars: RegExprString read fSpaceChars write fSpaceChars;
    {$ENDIF}

    {$IFDEF UseWordChars}
    // Contains chars, treated as /w (initially filled with RegExprWordChars
    // global constant)
    property WordChars: RegExprString read fWordChars write fWordChars;
    {$ENDIF}

    {$IFDEF UseLineSep}
    // line separators (like \n in Unix)
    property LineSeparators: RegExprString read fLineSeparators write SetLineSeparators;
    {$ENDIF}

    // support paired line-break CR LF
    property UseLinePairedBreak: Boolean read fUsePairedBreak write SetUsePairedBreak;

    property ReplaceLineEnd: RegExprString read fReplaceLineEnd write fReplaceLineEnd;

    property SlowChecksSizeMax: Integer read fSlowChecksSizeMax write fSlowChecksSizeMax;

    // Errors during Exec() return false and set LastError. This option allows
    // them to raise an Exception
    property RaiseForRuntimeError: Boolean read fRaiseForRuntimeError write fRaiseForRuntimeError;

    property AllowUnsafeLookBehind: Boolean read FAllowUnsafeLookBehind write FAllowUnsafeLookBehind;

    // Make sure a { always is a range / don't allow unescaped literal usage
    property AllowLiteralBraceWithoutRange: Boolean read FAllowLiteralBraceWithoutRange write FAllowLiteralBraceWithoutRange;
    // support {,123} defaulting the min-matches to 0
    property AllowBraceWithoutMin: Boolean read FAllowBraceWithoutMin write FAllowBraceWithoutMin;
  end;

type
  ERegExpr = class(Exception)
  public
    ErrorCode: Integer;
    CompilerErrorPos: PtrInt;
  end;

  // true if string AInputString match regular expression ARegExpr
  // ! will raise exeption if syntax errors in ARegExpr
function ExecRegExpr(const ARegExpr, AInputStr: RegExprString): Boolean;

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
  AUseSubstitution: Boolean{$IFDEF DefParam} = False{$ENDIF}): RegExprString;
{$IFDEF OverMeth}overload;

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
// -(n+1) At position n was found opening '[' without
// corresponding closing ']';
// n      At position n was found closing bracket ')' without
// corresponding opening '('.
// If Result <> 0, then ASubExpr can contain empty items or illegal ones
function RegExprSubExpressions(const ARegExpr: RegExprString; ASubExprs: TStrings;
  AExtendedSyntax: Boolean{$IFDEF DefParam} = False{$ENDIF}): Integer;

implementation

{$IFDEF FastUnicodeData}
uses
  regexpr_unicodedata;
{$ENDIF}

const
  // TRegExpr.VersionMajor/Minor return values of these constants:
  REVersionMajor = 1;
  REVersionMinor = 181;

  OpKind_End = REChar(1);
  OpKind_MetaClass = REChar(2);
  OpKind_Range = REChar(3);
  OpKind_Char = REChar(4);
  OpKind_CategoryYes = REChar(5);
  OpKind_CategoryNo = REChar(6);

  RegExprAllSet = [0 .. 255];
  RegExprWordSet = [Ord('a') .. Ord('z'), Ord('A') .. Ord('Z'), Ord('0') .. Ord('9'), Ord('_')];
  RegExprDigitSet = [Ord('0') .. Ord('9')];
  RegExprLowerAzSet = [Ord('a') .. Ord('z')];
  RegExprUpperAzSet = [Ord('A') .. Ord('Z')];
  RegExprAllAzSet = RegExprLowerAzSet + RegExprUpperAzSet;
  RegExprSpaceSet = [Ord(' '), $9, $A, $D, $C];
  RegExprLineSeparatorsSet = [$d, $a, $b, $c] {$IFDEF UnicodeRE} + [$85] {$ENDIF};
  RegExprHorzSeparatorsSet = [9, $20, $A0];

  MaxBracesArg = $7FFFFFFF - 1; // max value for {n,m} arguments

type
  TRENextOff = PtrInt;
  // internal Next "pointer" (offset to current p-code)
  PRENextOff = ^TRENextOff;
  // used for extracting Next "pointers" from compiled r.e.
  TREBracesArg = Integer; // type of {m,n} arguments
  PREBracesArg = ^TREBracesArg;

  TREGroupKind = (
    gkNormalGroup,
    gkNonCapturingGroup,
    gkAtomicGroup,
    gkNamedGroupReference,
    gkComment,
    gkModifierString,
    gkLookahead,
    gkLookaheadNeg,
    gkLookbehind,
    gkLookbehindNeg,
    gkRecursion,
    gkSubCall
    );

  TReOpLookBehindOptions = packed record
    MatchLenMin, MatchLenMax: TREBracesArg;
    IsGreedy: REChar;
  end;
  PReOpLookBehindOptions = ^TReOpLookBehindOptions;

const
  ReOpLookBehindOptionsSz = SizeOf(TReOpLookBehindOptions) div SizeOf(REChar);
  OPT_LOOKBEHIND_NON_GREEDY = REChar(0);
  OPT_LOOKBEHIND_GREEDY = REChar(1);
  OPT_LOOKBEHIND_FIXED = REChar(2);

// Alexey T.: handling of that define FPC_REQUIRES_PROPER_ALIGNMENT was present even 15 years ago,
// but with it, we have failing of some RegEx tests, on ARM64 CPU.
// If I undefine FPC_REQUIRES_PROPER_ALIGNMENT, all tests run OK on ARM64 again.
{$undef FPC_REQUIRES_PROPER_ALIGNMENT}

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

  REBranchArgSz = 2; // 2 * (REChar div REChar)

type
  TReGroupIndex = LongInt;
  PReGroupIndex = ^TReGroupIndex;
const
  ReGroupIndexSz = SizeOf(TReGroupIndex) div SizeOf(REChar);

type
  PtrPair = {$IFDEF UnicodeRE} ^LongInt; {$ELSE} ^Word; {$ENDIF}

function GroupDataArraySize(ARequired, ACurrent: Integer): Integer;
begin
  Result := ARequired;
  if Result > ACurrent then
    Exit;

  // Keep some extra
  if Result > ACurrent - RegexGroupCountIncrement then
    Result := ACurrent;
end;

function IsPairedBreak(p: PRegExprChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
const
  cBreak = {$IFDEF UnicodeRE} $000D000A; {$ELSE} $0D0A; {$ENDIF}
begin
  Result := PtrPair(p)^ = cBreak;
end;

function IsAnyLineBreak(C: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case C of
    #10,
    #13,
    #$0B,
    #$0C
    {$ifdef UnicodeRE}
    , #$85
    , #$2028
    , #$2029
    {$endif}:
      Result := True;
  else
    Result := False;
  end;
end;

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

function IsIgnoredChar(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    ' ', #9, #$d, #$a:
      Result := True
  else
    Result := False;
  end;
end;

function _IsMetaChar(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    'd', 'D',
    's', 'S',
    'w', 'W',
    'v', 'V',
    'h', 'H',
    'R':
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
  Result := Align(p, SizeOf(Integer));
  {$ELSE}
  Result := p;
  {$ENDIF}
end;

function StrLScan(P: PRegExprChar; C: REChar; len: PtrInt): PRegExprChar;
Var
   count: PtrInt;
Begin
  count := 0;
  { Find first matching character of Ch in Str }
  while (count < len) do
  begin
    if C = P[count] then
     begin
       StrLScan := @(P[count]);
       exit;
     end;
    Inc(count);
  end;
  { nothing found. }
  StrLScan := nil;
end;

function StrLComp(str1,str2 : PRegExprChar; len : PtrInt) : PtrInt;
var
  counter: PtrInt;
  c1, c2: REChar;
begin
  if len = 0 then
  begin
    StrLComp := 0;
    exit;
  end;
  counter:=0;
  repeat
    c1:=str1[counter];
    c2:=str2[counter];
    inc(counter);
  until (c1<>c2) or (counter>=len) or (c1=#0) or (c2=#0);
  StrLComp:=ord(c1)-ord(c2);
end;

function StrLPos(str1,str2 : PRegExprChar; len1, len2: PtrInt) : PRegExprChar;
var
  p : PRegExprChar;
begin
  StrLPos := nil;
  if (str1 = nil) or (str2 = nil) then
    exit;
  len1 := len1 - len2 + 1;
  p := StrLScan(str1,str2^, len1);
  while p <> nil do
  begin
    if StrLComp(p, str2, len2)=0 then
    begin
       StrLPos := p;
       exit;
    end;
    inc(p);
    p := StrLScan(p, str2^, len1 - (p-str1));
  end;
end;

{$IFDEF FastUnicodeData}
function _UpperCase(Ch: REChar): REChar; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  Result := CharUpperArray[Ord(Ch)];
end;

function _LowerCase(Ch: REChar): REChar; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  Result := CharLowerArray[Ord(Ch)];
end;

{$ELSE}
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
    {$IFDEF UnicodeRE}
    Result := UnicodeUpperCase(Ch)[1];
    {$ELSE}
    Result := AnsiUpperCase(Ch)[1];
    {$ENDIF}
  {$ELSE}
    {$IFDEF UnicodeRE}
      {$IFDEF SYN_COMPILER_18_UP}
    Result := Ch.ToUpper;
      {$ELSE}
      {$IFDEF SYN_DELPHI_2009_UP}
    Result := TCharacter.ToUpper(Ch);
      {$ENDIF}
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
    {$IFDEF UnicodeRE}
    Result := UnicodeLowerCase(Ch)[1];
    {$ELSE}
    Result := AnsiLowerCase(Ch)[1];
    {$ENDIF}
  {$ELSE}
    {$IFDEF UnicodeRE}
      {$IFDEF SYN_COMPILER_18_UP}
    Result := Ch.ToLower;
      {$ELSE}
      {$IFDEF SYN_DELPHI_2009_UP}
    Result := TCharacter.ToLower(Ch);
      {$ENDIF}
    {$ENDIF}
    {$ELSE}
    Result := AnsiLowerCase(Ch)[1];
    {$ENDIF}
  {$ENDIF}
end;
{$ENDIF}

function InvertCase(const Ch: REChar): REChar; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  Result := _UpperCase(Ch);
  if Result = Ch then
    Result := _LowerCase(Ch);
end;

function _FindClosingBracket(P, PEnd: PRegExprChar): PRegExprChar;
var
  Level: Integer;
begin
  Result := nil;
  Level := 1;
  repeat
    if P >= PEnd then Exit;
    case P^ of
      EscChar:
        Inc(P);
      '(':
        begin
          Inc(Level);
        end;
      ')':
        begin
          Dec(Level);
          if Level = 0 then
          begin
            Result := P;
            Exit;
          end;
        end;
    end;
    Inc(P);
  until False;
end;

{$IFDEF UNICODEEX}
procedure IncUnicode(var p: PRegExprChar); {$IFDEF InlineFuncs}inline;{$ENDIF}
// make additional increment if we are on low-surrogate char
// no need to check p<fInputEnd, at the end of string we have chr(0)
var
  ch: REChar;
begin
  Inc(p);
  ch := p^;
  if (Ord(ch) >= $DC00) and (Ord(ch) <= $DFFF) then
    Inc(p);
end;

procedure IncUnicode2(var p: PRegExprChar; var N: Integer); {$IFDEF InlineFuncs}inline;{$ENDIF}
var
  ch: REChar;
begin
  Inc(p);
  Inc(N);
  ch := p^;
  if (Ord(ch) >= $DC00) and (Ord(ch) <= $DFFF) then
  begin
    Inc(p);
    Inc(N);
  end;
end;
{$ENDIF}

{ ============================================================= }
{ ===================== Global functions ====================== }
{ ============================================================= }

function IsModifiersEqual(const A, B: TRegExprModifiers): Boolean;
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
  ALen: Integer;
  var AValue: TRegExprModifiers): Boolean;
// Parse string and set AValue if it's in format 'ismxrg-ismxrg'
var
  IsOn: Boolean;
  i: Integer;
begin
  Result := True;
  IsOn := True;
  for i := 0 to ALen-1 do
    case APtr[i] of
      '-':
        if IsOn then
        begin
          IsOn := False;
        end
        else
        begin
          Result := False;
          Exit;
        end;
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
      Result := False;
      Exit;
    end;
end;

function ExecRegExpr(const ARegExpr, AInputStr: RegExprString): Boolean;
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
  AUseSubstitution: Boolean{$IFDEF DefParam} = False{$ENDIF}): RegExprString;
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
      if rroUseOsLineEnd in Options then
        ReplaceLineEnd := sLineBreak
      else
        ReplaceLineEnd := #10;
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

function _IsMetaSymbol1(ch: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case ch of
    '^', '$', '.', '[', '(', ')', '|', '?', '+', '*', EscChar, '{':
      Result := True
  else
    Result := False
  end;
end;

function _IsMetaSymbol2(ch: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
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
  i, i0, Len: Integer;
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

function RegExprSubExpressions(const ARegExpr: RegExprString; ASubExprs: TStrings;
  AExtendedSyntax: Boolean{$IFDEF DefParam} = False{$ENDIF}): Integer;
type
  TStackItemRec = record
    SubExprIdx: Integer;
    StartPos: PtrInt;
  end;

  TStackArray = packed array [0 .. RegexMaxMaxGroups - 1] of TStackItemRec;
var
  Len, SubExprLen: Integer;
  i, i0: Integer;
  Modif: TRegExprModifiers;
  Stack: ^TStackArray;
  StackIdx, StackSz: Integer;
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
  // SetLength (Stack, StackSz);
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
              if ARegExpr[i] = EscChar
              then
                Inc(i, 2) // skip 'escaped' char to prevent stopping at '\]'
              else
                Inc(i);
            if (i > Len) or (ARegExpr[i] <> ']')
            then
              Result := -(i0 + 1); // unbalanced '['
          end;
        '#':
          if AExtendedSyntax then
          begin
            // skip eXtended comments
            while (i <= Len) and (ARegExpr[i] <> #$d) and (ARegExpr[i] <> #$a)
            // do not use [#$d, #$a] due to Unicode compatibility
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

  OP_EEND = TREOp(0); // End of program
  OP_BOL = TREOp(1); // Empty match at beginning of line
  OP_EOL = TREOp(2); // Empty match at end of line
  OP_ANY = TREOp(3); // Match any one character
  OP_ANYOF = TREOp(4); // Match any character in string
  OP_ANYBUT = TREOp(5); // Match any character not in string
  OP_BRANCH = TREOp(6); // Match this alternative, or the next
  OP_BACK = TREOp(7); // Jump backward (Next < 0)
  OP_EXACTLY = TREOp(8); // Match string exactly
  OP_NOTHING = TREOp(9); // Match empty string
  OP_STAR = TREOp(10); // Match this (simple) thing 0 or more times
  OP_PLUS = TREOp(11); // Match this (simple) thing 1 or more times
  OP_ANYDIGIT = TREOp(12); // Match any digit (equiv [0-9])
  OP_NOTDIGIT = TREOp(13); // Match not digit (equiv [0-9])
  OP_ANYLETTER = TREOp(14); // Match any 'word' char
  OP_NOTLETTER = TREOp(15); // Match any 'non-word' char
  OP_ANYSPACE = TREOp(16); // Match any 'space' char
  OP_NOTSPACE = TREOp(17); // Match 'not space' char
  OP_BRACES = TREOp(18);
  // Node,Min,Max Match this (simple) thing from Min to Max times.
  // Min and Max are TREBracesArg
  OP_COMMENT = TREOp(19); // Comment
  OP_EXACTLY_CI = TREOp(20); // Match string, case insensitive
  OP_ANYOF_CI = TREOp(21); // Match any character in string, case insensitive
  OP_ANYBUT_CI = TREOp(22); // Match any char not in string, case insensitive
  OP_LOOPENTRY = TREOp(23); // Start of loop (Node - LOOP for this loop)
  OP_LOOP = TREOp(24); // Back jump for LOOPENTRY
  // Min and Max are TREBracesArg
  // Node - next node in sequence,
  // LoopEntryJmp - associated LOOPENTRY node addr
  OP_EOL2 = TReOp(25); // like OP_EOL, but also matches before final line-break
  OP_CONTINUE_POS = TReOp(26); // \G, where offset is from last match end or from Exec(AOffset)
  OP_ANYLINEBREAK = TReOp(27); // \R
  OP_BSUBEXP = TREOp(28); // Match previously matched subexpression #Idx (stored as REChar)
  OP_BSUBEXP_CI = TREOp(29); // -"- in case-insensitive mode

  // Non-greedy ops
  OP_STAR_NG = TREOp(30); // Same as OP_START but in non-greedy mode
  OP_PLUS_NG = TREOp(31); // Same as OP_PLUS but in non-greedy mode
  OP_BRACES_NG = TREOp(32); // Same as OP_BRACES but in non-greedy mode
  OP_LOOP_NG = TREOp(33); // Same as OP_LOOP but in non-greedy mode

  // Multiline mode \m
  OP_BOL_ML = TREOp(34); // Match "" at beginning of line
  OP_EOL_ML = TREOp(35); // Match "" at end of line
  OP_ANY_ML = TREOp(36); // Match any one character

  // Word boundary
  OP_BOUND = TREOp(37); // Match "" between word char and non-word char
  OP_NOTBOUND = TREOp(38); // Opposite to OP_BOUND

  OP_ANYHORZSEP = TREOp(39); // Any horizontal whitespace \h
  OP_NOTHORZSEP = TREOp(40); // Not horizontal whitespace \H
  OP_ANYVERTSEP = TREOp(41); // Any vertical whitespace \v
  OP_NOTVERTSEP = TREOp(42); // Not vertical whitespace \V

  OP_ANYCATEGORY = TREOp(43); // \p{L}
  OP_NOTCATEGORY = TREOp(44); // \P{L}

  // Possessive quantifiers
  OP_STAR_POSS = TReOp(45);
  OP_PLUS_POSS = TReOp(46);
  OP_BRACES_POSS = TReOp(47);

  OP_RECUR = TReOp(48);

  OP_OPEN = TREOp(50); // Opening of group
  OP_CLOSE = TREOp(51); // Closing of group
  OP_OPEN_ATOMIC = TREOp(52); // Opening of group
  OP_CLOSE_ATOMIC = TREOp(53); // Closing of group

  OP_LOOKAHEAD = TREOp(55);
  OP_LOOKAHEAD_NEG = TREOp(56);
  OP_LOOKAHEAD_END = TREOp(57);
  OP_LOOKBEHIND = TREOp(58);
  OP_LOOKBEHIND_NEG = TREOp(59);
  OP_LOOKBEHIND_END = TREOp(60);
  OP_LOOKAROUND_OPTIONAL = TREOp(61);

  OP_SUBCALL = TREOp(65); // Call of subroutine; OP_SUBCALL+i is for group i
  OP_LOOP_POSS = TREOp(66); // Same as OP_LOOP but in non-greedy mode

  OP_GBRANCH = TREOp(67); // Guarded branch
  OP_GBRANCH_EX = TREOp(68);
  OP_GBRANCH_EX_CI = TREOp(69);

  OP_RESET_MATCHPOS = TReOp(70);

  OP_NONE = High(TREOp);

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
  reeUnknownMetaSymbol = 101;
  reeCompParseRegTooManyBrackets = 102;
  reeCompParseRegUnmatchedBrackets = 103;
  reeCompParseRegUnmatchedBrackets2 = 104;
  reeCompParseRegJunkOnEnd = 105;
  reeNotQuantifiable = 106;
  reeNestedQuantif = 107;
  reeBadHexDigit = 108;
  reeInvalidRange = 109;
  reeParseAtomTrailingBackSlash = 110;
  reeNoHexCodeAfterBSlashX = 111;
  reeHexCodeAfterBSlashXTooBig = 112;
  reeUnmatchedSqBrackets = 113;
  reeInternalUrp = 114;
  reeQuantifFollowsNothing = 115;
  reeTrailingBackSlash = 116;
  reeNoLetterAfterBSlashC = 117;
  reeMetaCharAfterMinusInRange = 118;
  reeRarseAtomInternalDisaster = 119;
  reeIncorrectSpecialBrackets = 120;
  reeIncorrectBraces = 121;
  reeBRACESArgTooBig = 122;
  reeUnknownOpcodeInFillFirst = 123;
  reeBracesMinParamGreaterMax = 124;
  reeUnclosedComment = 125;
  reeComplexBracesNotImplemented = 126;
  reeUnrecognizedModifier = 127;
  reeBadLinePairedSeparator = 128;
  reeBadUnicodeCategory = 129;
  reeTooSmallCheckersArray = 130;
  reeBadRecursion = 132;
  reeBadSubCall = 133;
  reeNamedGroupBad = 140;
  reeNamedGroupBadName = 141;
  reeNamedGroupBadRef = 142;
  reeNamedGroupDupName = 143;
  reeLookaheadBad = 150;
  reeLookbehindBad = 152;
  reeLookaroundNotSafe = 153;
  reeBadReference = 154;
  // Runtime errors must be >= reeFirstRuntimeCode
  reeFirstRuntimeCode = 1000;
  reeRegRepeatCalledInappropriately = 1000;
  reeMatchPrimMemoryCorruption = 1001;
  reeNoExpression = 1003;
  reeCorruptedProgram = 1004;
  reeOffsetMustBePositive = 1006;
  reeExecNextWithoutExec = 1007;
  reeBadOpcodeInCharClass = 1008;
  reeDumpCorruptedOpcode = 1011;
  reeLoopStackExceeded = 1014;
  reeLoopWithoutEntry = 1015;
  reeUnknown = 1016;

function TRegExpr.ErrorMsg(AErrorID: Integer): RegExprString;
begin
  case AErrorID of
    reeOk:
      Result := 'No errors';
    reeCompNullArgument:
      Result := 'TRegExpr compile: null argument';
    reeUnknownMetaSymbol:
      Result := 'TRegExpr compile: unknown meta-character: \' + fLastErrorSymbol;
    reeCompParseRegTooManyBrackets:
      Result := 'TRegExpr compile: ParseReg: too many ()';
    reeCompParseRegUnmatchedBrackets:
      Result := 'TRegExpr compile: ParseReg: unmatched ()';
    reeCompParseRegUnmatchedBrackets2:
      Result := 'TRegExpr compile: ParseReg: unmatched ()';
    reeCompParseRegJunkOnEnd:
      Result := 'TRegExpr compile: ParseReg: junk at end';
    reeNotQuantifiable:
      Result := 'TRegExpr compile: Token before *+ operand is not quantifiable';
    reeNestedQuantif:
      Result := 'TRegExpr compile: nested quantifier *?+';
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
    reeQuantifFollowsNothing:
      Result := 'TRegExpr compile: quantifier ?+*{ follows nothing';
    reeTrailingBackSlash:
      Result := 'TRegExpr compile: trailing \';
    reeRarseAtomInternalDisaster:
      Result := 'TRegExpr compile: RarseAtom internal disaster';
    reeIncorrectSpecialBrackets:
      Result := 'TRegExpr compile: incorrect expression in (?...) brackets';
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
      Result := 'TRegExpr compile: incorrect modifier';
    reeBadLinePairedSeparator:
      Result := 'TRegExpr compile: LinePairedSeparator must countain two different chars or be empty';
    reeBadUnicodeCategory:
      Result := 'TRegExpr compile: invalid category after \p or \P';
    reeTooSmallCheckersArray:
      Result := 'TRegExpr compile: too small CharCheckers array';
    reeBadRecursion:
      Result := 'TRegExpr compile: bad recursion (?R)';
    reeBadSubCall:
      Result := 'TRegExpr compile: bad subroutine call';
    reeNamedGroupBad:
      Result := 'TRegExpr compile: bad named group';
    reeNamedGroupBadName:
      Result := 'TRegExpr compile: bad identifier in named group';
    reeNamedGroupBadRef:
      Result := 'TRegExpr compile: bad back-reference to named group';
    reeNamedGroupDupName:
      Result := 'TRegExpr compile: named group defined more than once';
    reeLookaheadBad:
      Result := 'TRegExpr compile: bad lookahead';
    reeLookbehindBad:
      Result := 'TRegExpr compile: bad lookbehind';
    reeLookaroundNotSafe:
      Result := 'TRegExpr compile: lookbehind brackets with variable length do not support captures';
    reeBadReference:
      Result := 'TRegExpr compile: invalid syntax for reference to capture group';

    reeRegRepeatCalledInappropriately:
      Result := 'TRegExpr exec: RegRepeat called inappropriately';
    reeMatchPrimMemoryCorruption:
      Result := 'TRegExpr exec: MatchPrim memory corruption';
    reeNoExpression:
      Result := 'TRegExpr exec: empty expression';
    reeCorruptedProgram:
      Result := 'TRegExpr exec: corrupted opcode (no magic byte)';
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
    reeUnknown:
      Result := 'TRegExpr exec: unknow error';
  else
    Result := 'Unknown error';
  end;
end; { of procedure TRegExpr.Error
  -------------------------------------------------------------- }

function TRegExpr.LastError: Integer;
begin
  Result := fLastError;
  fLastError := reeOk;
end; { of function TRegExpr.LastError
  -------------------------------------------------------------- }

{ ============================================================= }
{ ===================== Common section ======================== }
{ ============================================================= }

class function TRegExpr.VersionMajor: Integer;
begin
  Result := REVersionMajor;
end;

class function TRegExpr.VersionMinor: Integer;
begin
  Result := REVersionMinor;
end;

constructor TRegExpr.Create;
begin
  inherited;
  programm := nil;
  fExpression := '';
  fInputString := '';

  FillChar(fModifiers, SizeOf(fModifiers), 0);
  fModifiers.I := RegExprModifierI;
  fModifiers.R := RegExprModifierR;
  fModifiers.S := RegExprModifierS;
  fModifiers.G := RegExprModifierG;
  fModifiers.M := RegExprModifierM;
  fModifiers.X := RegExprModifierX;

  {$IFDEF UseSpaceChars}
  SpaceChars := RegExprSpaceChars;
  {$ENDIF}
  {$IFDEF UseWordChars}
  WordChars := RegExprWordChars;
  {$ENDIF}

  {$IFDEF UseLineSep}
  fLineSeparators := RegExprLineSeparators;
  {$ENDIF}

  fUsePairedBreak := RegExprUsePairedBreak;
  fReplaceLineEnd := RegExprReplaceLineBreak;

  fSlowChecksSizeMax := 2000;
  FAllowUnsafeLookBehind := False;
  fRaiseForRuntimeError := True;

  {$IFDEF UseLineSep}
  InitLineSepArray;
  {$ENDIF}

  InitCharCheckers;

  {$IFDEF Compat}
  fInvertCase := OldInvertCase;
  {$ENDIF}
end; { of constructor TRegExpr.Create
  -------------------------------------------------------------- }

{ TRegExprGroupNameList }

function TRegExprGroupNameList.MatchIndexFromName(const AName: RegExprString
  ): Integer;
var
  i: Integer;
begin
  for i := 0 to NameCount - 1 do
    if Names[i].Name = AName then
    begin
      Result := Names[i].Index;
      Exit;
    end;
  Result := -1;
end;

procedure TRegExprGroupNameList.Clear;
begin
  NameCount := 0;
  if Length(Names) > RegexGroupCountIncrement then
    SetLength(Names, RegexGroupCountIncrement);
end;

procedure TRegExprGroupNameList.Add(const AName: RegExprString; AnIndex: Integer
  );
begin
  if NameCount >= Length(Names) then
    SetLength(Names, Length(Names) + 1 + RegexGroupCountIncrement);
  Names[NameCount].Name := AName;
  Names[NameCount].Index := AnIndex;
  inc(NameCount);
end;

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
end;

procedure TRegExpr.SetExpression(const AStr: RegExprString);
begin
  if (AStr <> fExpression) or not IsCompiled then
  begin
    fExpression := AStr;
    //UniqueString(fExpression);
    fRegexStart := PRegExprChar(fExpression);
    fRegexEnd := fRegexStart + Length(fExpression);
    InvalidateProgramm;
  end;
end;

function TRegExpr.GetSubExprCount: Integer;
begin
  Result := -1;
  if Length(GrpIndexes) = 0 then
    Exit;
  // if nothing found, we must return -1 per TRegExpr docs
  if (GrpBounds[0].GrpStart[0] <> nil) then
    Result := GrpCount;
end;

function TRegExpr.GetMatchPos(Idx: Integer): PtrInt;
begin
  Result := -1;
  if Length(GrpIndexes) = 0 then
    Exit;
  if (Idx < 0) or (Idx >= Length(GrpIndexes)) then
    Exit;
  Idx := GrpIndexes[Idx];
  if (Idx >= 0) and (GrpBounds[0].GrpStart[Idx] <> nil) then
    Result := GrpBounds[0].GrpStart[Idx] - fInputStart + 1;
end;

function TRegExpr.GetMatchLen(Idx: Integer): PtrInt;
begin
  Result := -1;
  if Length(GrpIndexes) = 0 then
    Exit;
  if (Idx < 0) or (Idx >= Length(GrpIndexes)) then
    Exit;
  Idx := GrpIndexes[Idx];
  if (Idx >= 0) and (GrpBounds[0].GrpStart[Idx] <> nil) then
    Result := GrpBounds[0].GrpEnd[Idx] - GrpBounds[0].GrpStart[Idx];
end;

function TRegExpr.GetMatch(Idx: Integer): RegExprString;
begin
  Result := '';
  if Length(GrpIndexes) = 0 then
    Exit;
  if (Idx < 0) or (Idx >= Length(GrpIndexes)) then
    Exit;
  Idx := GrpIndexes[Idx];
  if (Idx >= 0) and (GrpBounds[0].GrpStart[Idx] <> nil) and
     (GrpBounds[0].GrpEnd[Idx] > GrpBounds[0].GrpStart[Idx])
  then
    SetString(Result, GrpBounds[0].GrpStart[Idx], GrpBounds[0].GrpEnd[Idx] - GrpBounds[0].GrpStart[Idx]);
end;

function TRegExpr.MatchIndexFromName(const AName: RegExprString): Integer;
begin
  Result := GrpNames.MatchIndexFromName(AName);
end;

function TRegExpr.MatchFromName(const AName: RegExprString): RegExprString;
var
  Idx: Integer;
begin
  Result := '';
  if Length(GrpIndexes) = 0 then
    Exit;
  Idx := GrpNames.MatchIndexFromName(AName);
  if Idx >= 0 then
    Result := GetMatch(Idx)
  else
    Result := '';
end;

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

procedure TRegExpr.SetModifierG(AValue: Boolean);
begin
  if fModifiers.G <> AValue then
  begin
    fModifiers.G := AValue;
    InvalidateProgramm;
  end;
end;

procedure TRegExpr.SetModifierI(AValue: Boolean);
begin
  if fModifiers.I <> AValue then
  begin
    fModifiers.I := AValue;
    InvalidateProgramm;
  end;
end;

procedure TRegExpr.SetModifierM(AValue: Boolean);
begin
  if fModifiers.M <> AValue then
  begin
    fModifiers.M := AValue;
    InvalidateProgramm;
  end;
end;

procedure TRegExpr.SetModifierR(AValue: Boolean);
begin
  if fModifiers.R <> AValue then
  begin
    fModifiers.R := AValue;
    InvalidateProgramm;
  end;
end;

procedure TRegExpr.SetModifierS(AValue: Boolean);
begin
  if fModifiers.S <> AValue then
  begin
    fModifiers.S := AValue;
    InvalidateProgramm;
  end;
end;

procedure TRegExpr.SetModifierX(AValue: Boolean);
begin
  if fModifiers.X <> AValue then
  begin
    fModifiers.X := AValue;
    InvalidateProgramm;
  end;
end;

procedure TRegExpr.SetModifierStr(const AStr: RegExprString);
begin
  if ParseModifiers(PRegExprChar(AStr), Length(AStr), fModifiers) then
    InvalidateProgramm
  else
    Error(reeUnrecognizedModifier);
end;

{ ============================================================= }
{ ==================== Compiler section ======================= }
{ ============================================================= }

{$IFDEF FastUnicodeData}
function TRegExpr.IsWordChar(AChar: REChar): Boolean;
begin
  // bit 7 in value: is word char
  Result := CharCategoryArray[Ord(AChar)] and 128 <> 0;
end;

(*
  // Unicode General Category
  UGC_UppercaseLetter         = 0; Lu
  UGC_LowercaseLetter         = 1; Ll
  UGC_TitlecaseLetter         = 2; Lt
  UGC_ModifierLetter          = 3; Lm
  UGC_OtherLetter             = 4; Lo

  UGC_NonSpacingMark          = 5; Mn
  UGC_CombiningMark           = 6; Mc
  UGC_EnclosingMark           = 7; Me

  UGC_DecimalNumber           = 8; Nd
  UGC_LetterNumber            = 9; Nl
  UGC_OtherNumber             = 10; No

  UGC_ConnectPunctuation      = 11; Pc
  UGC_DashPunctuation         = 12; Pd
  UGC_OpenPunctuation         = 13; Ps
  UGC_ClosePunctuation        = 14; Pe
  UGC_InitialPunctuation      = 15; Pi
  UGC_FinalPunctuation        = 16; Pf
  UGC_OtherPunctuation        = 17; Po

  UGC_MathSymbol              = 18; Sm
  UGC_CurrencySymbol          = 19; Sc
  UGC_ModifierSymbol          = 20; Sk
  UGC_OtherSymbol             = 21; So

  UGC_SpaceSeparator          = 22; Zs
  UGC_LineSeparator           = 23; Zl
  UGC_ParagraphSeparator      = 24; Zp

  UGC_Control                 = 25; Cc
  UGC_Format                  = 26; Cf
  UGC_Surrogate               = 27; Cs
  UGC_PrivateUse              = 28; Co
  UGC_Unassigned              = 29; Cn
*)

const
  CategoryNames: array[0..29] of array[0..1] of REChar = (
    ('L', 'u'),
    ('L', 'l'),
    ('L', 't'),
    ('L', 'm'),
    ('L', 'o'),
    ('M', 'n'),
    ('M', 'c'),
    ('M', 'e'),
    ('N', 'd'),
    ('N', 'l'),
    ('N', 'o'),
    ('P', 'c'),
    ('P', 'd'),
    ('P', 's'),
    ('P', 'e'),
    ('P', 'i'),
    ('P', 'f'),
    ('P', 'o'),
    ('S', 'm'),
    ('S', 'c'),
    ('S', 'k'),
    ('S', 'o'),
    ('Z', 's'),
    ('Z', 'l'),
    ('Z', 'p'),
    ('C', 'c'),
    ('C', 'f'),
    ('C', 's'),
    ('C', 'o'),
    ('C', 'n')
    );

function IsCategoryFirstChar(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    'L', 'M', 'N', 'P', 'S', 'C', 'Z':
      Result := True;
  else
    Result := False;
  end;
end;

function IsCategoryChars(AChar, AChar2: REChar): Boolean;
var
  i: Integer;
begin
  for i := Low(CategoryNames) to High(CategoryNames) do
    if (AChar = CategoryNames[i][0]) then
      if (AChar2 = CategoryNames[i][1]) then
      begin
        Result := True;
        Exit
      end;
  Result := False;
end;

function CheckCharCategory(AChar: REChar; Ch0, Ch1: REChar): Boolean;
// AChar: check this char against opcode
// Ch0, Ch1: opcode operands after OP_*CATEGORY
var
  N: Byte;
  Name0, Name1: REChar;
begin
  Result := False;
  // bits 0..6 are category
  N := CharCategoryArray[Ord(AChar)] and 127;
  if N <= High(CategoryNames) then
  begin
    Name0 := CategoryNames[N][0];
    Name1 := CategoryNames[N][1];
    if Ch0 <> Name0 then Exit;
    if Ch1 <> #0 then
      if Ch1 <> Name1 then Exit;
    Result := True;
  end;
end;

function MatchOneCharCategory(opnd, scan: PRegExprChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
// opnd: points to opcode operands after OP_*CATEGORY
// scan: points into InputString
begin
  Result := CheckCharCategory(scan^, opnd^, (opnd + 1)^);
end;

{$ELSE}
function TRegExpr.IsWordChar(AChar: REChar): Boolean;
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
end;
{$ENDIF}

function TRegExpr.IsSpaceChar(AChar: REChar): Boolean;
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

function TRegExpr.IsCustomLineSeparator(AChar: REChar): Boolean;
begin
  {$IFDEF UseLineSep}
    {$IFDEF UnicodeRE}
    Result := Pos(AChar, fLineSeparators) > 0;
    {$ELSE}
    Result := fLineSepArray[Byte(AChar)];
    {$ENDIF}
  {$ELSE}
  case AChar of
    #$d, #$a,
    {$IFDEF UnicodeRE}
    #$85, #$2028, #$2029,
    {$ENDIF}
    #$b, #$c:
      Result := True;
  else
    Result := False;
  end;
  {$ENDIF}
end;

function IsDigitChar(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    '0' .. '9':
      Result := True;
  else
    Result := False;
  end;
end;

function IsHorzSeparator(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  // Tab and Unicode categoty "Space Separator": https://www.compart.com/en/unicode/category/Zs
  case AChar of
    #9, #$20, #$A0:
      Result := True;
    {$IFDEF UnicodeRE}
    #$1680, #$2000 .. #$200A, #$202F, #$205F, #$3000:
      Result := True;
    {$ENDIF}
  else
    Result := False;
  end;
end;

function IsVertLineSeparator(AChar: REChar): Boolean; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  case AChar of
    #$d, #$a, #$b, #$c:
      Result := True;
    {$IFDEF UnicodeRE}
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

  CompileRegExpr(fRegexStart);
end; { of procedure TRegExpr.Compile
  -------------------------------------------------------------- }

{$IFDEF UseLineSep}
procedure TRegExpr.InitLineSepArray;
{$IFNDEF UnicodeRE}
var
  i: Integer;
{$ENDIF}
begin
  {$IFNDEF UnicodeRE}
  FillChar(fLineSepArray, SizeOf(fLineSepArray), 0);
  for i := 1 to Length(fLineSeparators) do
    fLineSepArray[Byte(fLineSeparators[i])] := True;
  {$ENDIF}
end;
{$ENDIF}

function TRegExpr.IsProgrammOk: Boolean;
begin
  Result := False;

  // check modifiers
  if not IsModifiersEqual(fModifiers, fProgModifiers) then
    InvalidateProgramm;

  // compile if needed
  if programm = nil then
  begin
    Compile;
    // Check compiled programm
    if programm = nil then
      Exit;
  end;

  if programm[0] <> OP_MAGIC then
    Error(reeCorruptedProgram)
  else
    Result := True;
end; { of function TRegExpr.IsProgrammOk
  -------------------------------------------------------------- }

procedure TRegExpr.Tail(p: PRegExprChar; val: PRegExprChar);
// set the next-pointer at the end of a node chain
var
  scan: PRegExprChar;
begin
  if p = @regDummy then
    Exit;
  // Find last node.
  scan := regLast(p);
  // Set Next 'pointer'
  if val < scan then
    PRENextOff(AlignToPtr(scan + REOpSz))^ := -(scan - val)
    // work around PWideChar subtraction bug (Delphi uses
    // shr after subtraction to calculate widechar distance %-( )
    // so, if difference is negative we have .. the "feature" :(
    // I could wrap it in $IFDEF UnicodeRE, but I didn't because
    // "P – Q computes the difference between the address given
    // by P (the higher address) and the address given by Q (the
    // lower address)" - Delphi help quotation.
  else
    PRENextOff(AlignToPtr(scan + REOpSz))^ := val - scan;
end; { of procedure TRegExpr.Tail
  -------------------------------------------------------------- }

procedure TRegExpr.OpTail(p: PRegExprChar; val: PRegExprChar);
// regtail on operand of first argument; nop if operandless
begin
  // "Operandless" and "op != OP_BRANCH" are synonymous in practice.
  if (p = nil) or (p = @regDummy) or
     (PREOp(p)^ <> OP_BRANCH) and (PREOp(p)^ <> OP_GBRANCH) and
     (PREOp(p)^ <> OP_GBRANCH_EX) and (PREOp(p)^ <> OP_GBRANCH_EX_CI)
  then
    Exit;
  Tail(p + REOpSz + RENextOffSz + REBranchArgSz, val);
end; { of procedure TRegExpr.OpTail
  -------------------------------------------------------------- }

function TRegExpr.EmitNode(op: TREOp): PRegExprChar;
// emit a node, return location
begin
  Result := regCode;
  if Result <> @regDummy then
  begin
    PREOp(regCode)^ := op;
    Inc(regCode, REOpSz);
    PRENextOff(AlignToPtr(regCode))^ := 0; // Next "pointer" := nil
    Inc(regCode, RENextOffSz);

    if (op = OP_EXACTLY) or (op = OP_EXACTLY_CI) then
      regExactlyLen := PLongInt(regCode)
    else
      regExactlyLen := nil;

    {$IFDEF DebugSynRegExpr}
    if regcode - programm > regCodeSize then
      raise Exception.Create('TRegExpr.EmitNode buffer overrun');
    {$ENDIF}
  end
  else
    Inc(regCodeSize, REOpSz + RENextOffSz);
    // compute code size without code generation
end; { of function TRegExpr.EmitNode
  -------------------------------------------------------------- }

function TRegExpr.EmitBranch: PRegExprChar;
begin
  Result := EmitNode(OP_BRANCH);
  EmitC(#0);
  EmitC(#0);
end;

procedure TRegExpr.EmitC(ch: REChar);
begin
  if regCode <> @regDummy then
  begin
    regCode^ := ch;
    Inc(regCode);
    {$IFDEF DebugSynRegExpr}
    if regcode - programm > regCodeSize then
      raise Exception.Create('TRegExpr.EmitC buffer overrun');
    {$ENDIF}
  end
  else
    Inc(regCodeSize, REOpSz); // Type of p-code pointer always is ^REChar
end; { of procedure TRegExpr.EmitC
  -------------------------------------------------------------- }

procedure TRegExpr.EmitInt(AValue: LongInt);
begin
  if regCode <> @regDummy then
  begin
    PLongInt(regCode)^ := AValue;
    Inc(regCode, RENumberSz);
    {$IFDEF DebugSynRegExpr}
    if regcode - programm > regCodeSize then
      raise Exception.Create('TRegExpr.EmitInt buffer overrun');
    {$ENDIF}
  end
  else
    Inc(regCodeSize, RENumberSz);
end;

function TRegExpr.EmitNodeWithGroupIndex(op: TREOp; AIndex: Integer): PRegExprChar;
begin
  Result := EmitNode(op);
  EmitInt(AIndex);  // TReGroupIndex = LongInt;
end;

function TRegExpr.EmitGroupRef(AIndex: Integer; AIgnoreCase: Boolean): PRegExprChar;
begin
  if AIgnoreCase then
    Result := EmitNode(OP_BSUBEXP_CI)
  else
    Result := EmitNode(OP_BSUBEXP);
  EmitInt(AIndex);  // TReGroupIndex = LongInt;
end;

{$IFDEF FastUnicodeData}
procedure TRegExpr.FindCategoryName(var scan: PRegExprChar; var ch1, ch2: REChar);
// scan: points into regex string after '\p', to find category name
// ch1, ch2: 2-char name of category; ch2 can be #0
var
  ch: REChar;
  pos1, pos2, namePtr: PRegExprChar;
  nameLen: Integer;
begin
  ch1 := #0;
  ch2 := #0;
  ch := scan^;
  if IsCategoryFirstChar(ch) then
  begin
    ch1 := ch;
    Exit;
  end;
  if ch = '{' then
  begin
    pos1 := scan;
    pos2 := pos1;
    while (pos2 < fRegexEnd) and (pos2^ <> '}') do
      Inc(pos2);
    if pos2 >= fRegexEnd then
      Error(reeIncorrectBraces);

    namePtr := pos1+1;
    nameLen := pos2-pos1-1;
    Inc(scan, nameLen+1);

    if nameLen<1 then
      Error(reeBadUnicodeCategory);
    if nameLen>2 then
      Error(reeBadUnicodeCategory);

    if nameLen = 1 then
    begin
      ch1 := namePtr^;
      ch2 := #0;
      if not IsCategoryFirstChar(ch1) then
        Error(reeBadUnicodeCategory);
      Exit;
    end;

    if nameLen = 2 then
    begin
      ch1 := namePtr^;
      ch2 := (namePtr+1)^;
      if not IsCategoryChars(ch1, ch2) then
        Error(reeBadUnicodeCategory);
      Exit;
    end;
  end
  else
    Error(reeBadUnicodeCategory);
end;

function TRegExpr.EmitCategoryMain(APositive: Boolean): PRegExprChar;
var
  ch, ch2: REChar;
begin
  Inc(regParse);
  if regParse >= fRegexEnd then
    Error(reeBadUnicodeCategory);
  FindCategoryName(regParse, ch, ch2);
  if APositive then
    Result := EmitNode(OP_ANYCATEGORY)
  else
    Result := EmitNode(OP_NOTCATEGORY);
  EmitC(ch);
  EmitC(ch2);
end;
{$ENDIF}

procedure TRegExpr.InsertOperator(op: TREOp; opnd: PRegExprChar; sz: Integer);
// insert an operator in front of already-emitted operand
// Means relocating the operand.
var
  src, dst, place: PRegExprChar;
  i: Integer;
begin
  if regCode = @regDummy then
  begin
    Inc(regCodeSize, sz);
    Exit;
  end;
  // move code behind insert position
  src := regCode;
  Inc(regCode, sz);
  {$IFDEF DebugSynRegExpr}
  if regCode - programm > regCodeSize then
    raise Exception.Create('TRegExpr.InsertOperator buffer overrun');
   if fSecondPass and ( (opnd<regCodeWork) or (opnd-regCodeWork>regCodeSize) ) then
   raise Exception.Create('TRegExpr.InsertOperator invalid opnd');
  {$ENDIF}
  dst := regCode;
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
  for i := 0 to regNumBrackets - 1 do
    if (GrpOpCodes[i] <> nil) and (GrpOpCodes[i] >= opnd) then
      GrpOpCodes[i] := GrpOpCodes[i] + sz;
end; { of procedure TRegExpr.InsertOperator
  -------------------------------------------------------------- }

procedure TRegExpr.RemoveOperator(opnd: PRegExprChar; sz: Integer);
// remove an operator in front of already-emitted operand
// Means relocating the operand.
var
  src, dst: PRegExprChar;
  i: Integer;
begin
  if regCode = @regDummy then
  begin
    // Do not decrement regCodeSize => the fSecondPass may temporary fill the extra memory;
    Exit;
  end;
  // move code behind insert position
  {$IFDEF DebugSynRegExpr}
   if fSecondPass and ( (opnd<regCodeWork) or (opnd>=regCodeWork+regCodeSize) ) then
   raise Exception.Create('TRegExpr.RemoveOperator() invalid opnd');
  if (sz > regCodeSize-(opnd-regCodeWork)) then
    raise Exception.Create('TRegExpr.RemoveOperator buffer underrun');
  {$ENDIF}
  src := opnd + sz;
  dst := opnd;
  while src < regCode do
  begin
    dst^ := src^;
    Inc(dst);
    Inc(src);
  end;
  Dec(regCode, sz);
  for i := 0 to regNumBrackets - 1 do
    if (GrpOpCodes[i] <> nil) and (GrpOpCodes[i] > opnd) then
      GrpOpCodes[i] := GrpOpCodes[i] - sz;
end;

function FindSkippedMetaLen(PStart, PEnd: PRegExprChar): Integer; {$IFDEF InlineFuncs}inline;{$ENDIF}
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
  FLAG_WORST = 0; // Worst case
  FLAG_HASWIDTH = 1; // Cannot match empty string
  FLAG_SIMPLE = 2; // Simple enough to be OP_STAR/OP_PLUS/OP_BRACES operand
  FLAG_SPECSTART = 4; // Starts with * or +
  FLAG_LOOP = 8; // Has eithe *, + or {,n} with n>=2
  FLAG_GREEDY = 16; // Has any greedy code
  FLAG_LOOKAROUND = 32; // "Piece" (ParsePiece) is look-around
  FLAG_NOT_QUANTIFIABLE = 64; // "Piece" (ParsePiece) is look-around

  {$IFDEF UnicodeRE}
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

function TRegExpr.FindInCharClass(ABuffer: PRegExprChar; AChar: REChar; AIgnoreCase: Boolean): Boolean;
// Buffer contains char pairs: (Kind, Data), where Kind is one of OpKind_ values,
// and Data depends on Kind
var
  OpKind: REChar;
  ch, ch2: REChar;
  N, i: Integer;
begin
  if AIgnoreCase then
    AChar := _UpperCase(AChar);
  repeat
    OpKind := ABuffer^;
    case OpKind of
      OpKind_End:
        begin
          Result := False;
          Exit;
        end;

      OpKind_Range:
        begin
          Inc(ABuffer);
          ch := ABuffer^;
          if (AChar >= ch) then
          begin
            Inc(ABuffer);
            ch2 := ABuffer^;
            {
            // if AIgnoreCase, ch, ch2 are upcased in opcode
            if AIgnoreCase then
            begin
              ch := _UpperCase(ch);
              ch2 := _UpperCase(ch2);
            end;
            }
            if (AChar <= ch2) then
            begin
              Result := True;
              Exit;
            end;
            Inc(ABuffer);
          end
          else
            Inc(ABuffer, 2);
        end;

      OpKind_MetaClass:
        begin
          Inc(ABuffer);
          N := Ord(ABuffer^);
          if CharCheckers[N](AChar) then
          begin
            Result := True;
            Exit
          end;
          Inc(ABuffer);
        end;

      OpKind_Char:
        begin
          Inc(ABuffer);
          N := PLongInt(ABuffer)^;
          Inc(ABuffer, RENumberSz);
          repeat
            ch := ABuffer^;
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
            Inc(ABuffer);
            dec(n);
          until n = 0;
        end;

      {$IFDEF FastUnicodeData}
      OpKind_CategoryYes,
      OpKind_CategoryNo:
        begin
          Inc(ABuffer);
          ch := ABuffer^;
          Inc(ABuffer);
          ch2 := ABuffer^;
          Inc(ABuffer);
          Result := CheckCharCategory(AChar, ch, ch2);
          if OpKind = OpKind_CategoryNo then
            Result := not Result;
          if Result then
            Exit;
        end;
      {$ENDIF}

    {$IFDEF WITH_REGEX_ASSERT}
    else
      Error(reeBadOpcodeInCharClass);
    {$ENDIF}
    end;
  until False; // assume that Buffer is ended correctly
end;


procedure TRegExpr.GetCharSetFromWordChars(var ARes: TRegExprCharSet);
{$IFDEF UseWordChars}
var
  i: Integer;
  ch: REChar;
{$ENDIF}
begin
  {$IFDEF UseWordChars}
  ARes := [];
  for i := 1 to Length(fWordChars) do
  begin
    ch := fWordChars[i];
    {$IFDEF UnicodeRE}
    if Ord(ch) <= $FF then
    {$ENDIF}
      Include(ARes, Byte(ch));
  end;
  {$ELSE}
  ARes := RegExprWordSet;
  {$ENDIF}
end;

procedure TRegExpr.GetCharSetFromSpaceChars(var ARes: TRegExprCharset);
{$IFDEF UseSpaceChars}
var
  i: Integer;
  ch: REChar;
{$ENDIF}
begin
  {$IFDEF UseSpaceChars}
  ARes := [];
  for i := 1 to Length(fSpaceChars) do
  begin
    ch := fSpaceChars[i];
    {$IFDEF UnicodeRE}
    if Ord(ch) <= $FF then
    {$ENDIF}
      Include(ARes, Byte(ch));
  end;
  {$ELSE}
  ARes := RegExprSpaceSet;
  {$ENDIF}
end;

procedure TRegExpr.GetCharSetFromCharClass(ABuffer: PRegExprChar; AIgnoreCase: Boolean; var ARes: TRegExprCharset);
var
  ch, ch2: REChar;
  TempSet: TRegExprCharSet;
  N, i: Integer;
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
          {$IFDEF UnicodeRE}
          if Ord(ch2) > $FF then
            ch2 := REChar($FF);
          {$ENDIF}
          Inc(ABuffer);
          for i := Ord(ch) to Ord(ch2) do
          begin
            Include(ARes, Byte(i));
            if AIgnoreCase then
              Include(ARes, Byte(InvertCase(REChar(i))));
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
          if N = CheckerIndex_AnyLineBreak then
          begin
            ARes := ARes + RegExprLineSeparatorsSet;
            //we miss U+2028 and U+2029 here
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
            {$IFDEF UnicodeRE}
            if Ord(ch) <= $FF then
            {$ENDIF}
            begin
              Include(ARes, Byte(ch));
              if AIgnoreCase then
                Include(ARes, Byte(InvertCase(ch)));
            end;
          end;
        end;

      {$IFDEF FastUnicodeData}
      OpKind_CategoryYes,
      OpKind_CategoryNo:
        begin
          // usage of FirstCharSet makes no sense for regex with \p \P
          ARes := RegExprAllSet;
          Exit;
        end;
      {$ENDIF}

    {$IFDEF WITH_REGEX_ASSERT}
    else
      Error(reeBadOpcodeInCharClass);
    {$ENDIF}
    end;
  until False; // assume that Buffer is ended correctly
end;


function TRegExpr.GetModifierG: Boolean;
begin
  Result := fModifiers.G;
end;

function TRegExpr.GetModifierI: Boolean;
begin
  Result := fModifiers.I;
end;

function TRegExpr.GetModifierM: Boolean;
begin
  Result := fModifiers.M;
end;

function TRegExpr.GetModifierR: Boolean;
begin
  Result := fModifiers.R;
end;

function TRegExpr.GetModifierS: Boolean;
begin
  Result := fModifiers.S;
end;

function TRegExpr.GetModifierX: Boolean;
begin
  Result := fModifiers.X;
end;

function TRegExpr.CompileRegExpr(ARegExp: PRegExprChar): Boolean;
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
  scan, scanTemp, longest, longestTemp: PRegExprChar;
  Len, LenTemp: Integer;
  FlagTemp, MaxMatchLen: integer;
  op: TREOp;
begin
  Result := False;
  FlagTemp := 0;
  regParse := nil; // for correct error handling
  regExactlyLen := nil;

  GrpCount := 0;
  ParsedGrpCount := 0;
  GrpNames.Clear;
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
    // well, may it's paranoia. I'll check it later.

    // First pass: calculate opcode size, validate regex
    fSecondPass := False;
    fCompModifiers := fModifiers;
    regParse := ARegExp;
    regNumBrackets := 1;
    regCodeSize := 0;
    regCode := @regDummy;
    regCodeWork := nil;

    EmitC(OP_MAGIC);
    if ParseReg(False, FlagTemp) = nil then begin
      regNumBrackets := 0; // Not calling InitInternalGroupData => array sizes not adjusted for FillChar
      Exit;
    end;

    // Allocate memory
    GetMem(programm, regCodeSize * SizeOf(REChar));
    InitInternalGroupData;

    // Second pass: emit opcode
    fSecondPass := True;
    fCompModifiers := fModifiers;
    regParse := ARegExp;
    regNumBrackets := 1;
    GrpCount := ParsedGrpCount;
    ParsedGrpCount := 0;
    regCode := programm;
    regCodeWork := programm + REOpSz;

    EmitC(OP_MAGIC);
    if ParseReg(False, FlagTemp) = nil then
      Exit;

    // Dig out information for optimizations.
    IsFixedLengthEx(op, FMinMatchLen, MaxMatchLen);
    {$IFDEF UseFirstCharSet}
    FirstCharSet := [];
    FillFirstCharSet(regCodeWork);
    for Len := 0 to 255 do
      FirstCharArray[Len] := Byte(Len) in FirstCharSet;
    {$ENDIF}

    regAnchored := raNone;
    regMust := nil;
    regMustLen := 0;
    regMustString := '';

    scan := regCodeWork; // First OP_BRANCH.
    // Starting-point info.
    if PREOp(scan)^ = OP_BOL then
      regAnchored := raBOL
    else
    if PREOp(scan)^ = OP_EOL then
      regAnchored := raEOL
    else
    if PREOp(scan)^ = OP_CONTINUE_POS then
      regAnchored := raContinue
    else
    // ".*", ".*?", ".*+" at the very start of the pattern, only need to be
    // tested from the start-pos of the InputString.
    // If a pattern matches, then the ".*" will always go forward to where the
    // rest of the pattern starts matching
    // OP_ANY is "ModifierS=True"
    if (PREOp(scan)^ = OP_STAR) or (PREOp(scan)^ = OP_STAR_NG) or (PREOp(scan)^ = OP_STAR_POSS) then begin
      scanTemp := AlignToInt(scan + REOpSz + RENextOffSz);
      if PREOp(scanTemp)^ = OP_ANY then
        regAnchored := raOnlyOnce;
    end
    else
    // "{0,} is the same as ".*". So the same optimization applies
    if (PREOp(scan)^ = OP_BRACES) or (PREOp(scan)^ = OP_BRACES_NG) or (PREOp(scan)^ = OP_BRACES_POSS) then begin
      scanTemp := AlignToInt(scan + REOpSz + RENextOffSz);
      if (PREBracesArg(scanTemp)^ = 0)  // BracesMinCount
      and (PREBracesArg(scanTemp + REBracesArgSz)^ = MaxBracesArg)  // BracesMaxCount
      then begin
        scanTemp := AlignToPtr(scanTemp + REBracesArgSz + REBracesArgSz);
        if PREOp(scanTemp)^ = OP_ANY then
          regAnchored := raOnlyOnce;
      end;
    end;

    // If there's something expensive in the r.e., find the longest
    // literal string that must appear and make it the regMust. Resolve
    // ties in favor of later strings, since the regstart check works
    // with the beginning of the r.e. and avoiding duplication
    // strengthens checking. Not a strong reason, but sufficient in the
    // absence of others.
    if (FlagTemp and FLAG_SPECSTART) <> 0 then
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
        scan := regNext(scan);
      end;
      regMust := longest;
      regMustLen := Len;
      if regMustLen > 1 then // don't use regMust if too short
        SetString(regMustString, regMust, regMustLen);
    end;

    Result := True;

  finally
    begin
      if not Result then
        InvalidateProgramm;
    end;
  end;

end; { of function TRegExpr.CompileRegExpr
  -------------------------------------------------------------- }

function TRegExpr.ParseReg(InBrackets: Boolean; var FlagParse: Integer): PRegExprChar;
begin
  Result := DoParseReg(InBrackets, True, FlagParse, OP_OPEN, OP_CLOSE);
end;

function TRegExpr.DoParseReg(InBrackets, IndexBrackets: Boolean;
  var FlagParse: Integer; BeginGroupOp, EndGroupOP: TReOp): PRegExprChar;
// regular expression, i.e. main body or parenthesized thing
// Caller must absorb opening parenthesis.
// Combining parenthesis handling with the base level of regular expression
// is a trifle forced, but the need to tie the tails of the branches to what
// follows makes it hard to avoid.
var
  ret, br, ender, brStart: PRegExprChar;
  NBrackets: Integer;
  FlagTemp: Integer;
  SavedModifiers: TRegExprModifiers;
  HasGBranch, HasChoice: Boolean;
begin
  Result := nil;
  FlagTemp := 0;
  FlagParse := FLAG_HASWIDTH; // Tentatively.
  NBrackets := 0;
  SavedModifiers := fCompModifiers;

  // Make an OP_OPEN node, if parenthesized.
  ret := nil;
  if InBrackets then
  begin
    if IndexBrackets then begin
      if regNumBrackets >= RegexMaxMaxGroups then
      begin
        Error(reeCompParseRegTooManyBrackets);
        Exit;
      end;
      NBrackets := regNumBrackets;
      Inc(regNumBrackets);
      if BeginGroupOp <> OP_NONE then
        ret := EmitNodeWithGroupIndex(BeginGroupOp, NBrackets);
      if fSecondPass then
        GrpOpCodes[NBrackets] := ret;
    end
    else
    if BeginGroupOp <> OP_NONE then
      ret := EmitNode(BeginGroupOp);
  end;

  // Pick up the branches, linking them together.
  br := ParseBranch(FlagTemp);
  brStart := br;
  if br = nil then
  begin
    Result := nil;
    Exit;
  end;
  if ret <> nil then
    Tail(ret, br) // OP_OPEN -> first.
  else
    ret := br;
  if (FlagTemp and FLAG_HASWIDTH) = 0 then
    FlagParse := FlagParse and not FLAG_HASWIDTH;
  FlagParse := FlagParse or FlagTemp and (FLAG_SPECSTART or FLAG_LOOP or FLAG_GREEDY);
  HasGBranch := False;
  HasChoice := regParse^ = '|';
  while (regParse^ = '|') do
  begin
    Inc(regParse);
    br := ParseBranch(FlagTemp);
    if br = nil then
    begin
      Result := nil;
      Exit;
    end;
    if br^ <> OP_BRANCH then
      HasGBranch := True;
    Tail(ret, br); // OP_BRANCH -> OP_BRANCH.
    if (FlagTemp and FLAG_HASWIDTH) = 0 then
      FlagParse := FlagParse and not FLAG_HASWIDTH;
    FlagParse := FlagParse or FlagTemp and (FLAG_SPECSTART or FLAG_LOOP or FLAG_GREEDY);
  end;
  if fSecondPass then begin
    if HasGBranch then begin
      if brStart^ = OP_BRANCH then
        brStart^ := OP_GBRANCH;
    end
    else
    if not HasChoice then
      RemoveOperator(brStart, REOpSz + RENextOffSz + REBranchArgSz);
  end;

  // Make a closing node, and hook it on the end.
  if InBrackets and (EndGroupOP <> OP_NONE) then begin
    if IndexBrackets then
      ender := EmitNodeWithGroupIndex(EndGroupOP, NBrackets)
    else
      ender := EmitNode(EndGroupOP);
  end
  else
  if (EndGroupOP = OP_NONE) then begin
    if HasChoice then
      ender := EmitNode(OP_COMMENT) // need something to hook the branches' tails too
    else
      ender := nil;
  end
  else
    ender := EmitNode(OP_EEND);

  if ender <> nil then begin
    Tail(ret, ender);

    // Hook the tails of the branches to the closing node.
    br := ret;
    while br <> nil do
    begin
      OpTail(br, ender);
      br := regNext(br);
    end;
  end;

  // Check for proper termination.
  if InBrackets then
    if regParse^ <> ')' then
    begin
      Error(reeCompParseRegUnmatchedBrackets);
      Exit;
    end
    else
      Inc(regParse); // skip trailing ')'
  if (not InBrackets) and (regParse < fRegexEnd) then
  begin
    if regParse^ = ')' then
      Error(reeCompParseRegUnmatchedBrackets2)
    else
      Error(reeCompParseRegJunkOnEnd);
    Exit;
  end;
  fCompModifiers := SavedModifiers; // restore modifiers of parent
  Result := ret;
end; { of function TRegExpr.ParseReg
  -------------------------------------------------------------- }

function TRegExpr.ParseBranch(var FlagParse: Integer): PRegExprChar;
// one alternative of an | operator
// Implements the concatenation operator.
var
  ret, chain, latest: PRegExprChar;
  ch: REChar;
  FlagTemp: Integer;
begin
  FlagTemp := 0;
  FlagParse := FLAG_WORST; // Tentatively.

  ret := EmitBranch;
  chain := nil;
  while (regParse < fRegexEnd) and (regParse^ <> '|') and (regParse^ <> ')') do
  begin
    latest := ParsePiece(FlagTemp);
    if latest = nil then
    begin
      Result := nil;
      Exit;
    end;
    if fSecondPass and
       (latest <> nil) and (latest^ = OP_COMMENT) and
       ( ((regParse < fRegexEnd) and (regParse^ <> '|') and (regParse^ <> ')')) or
         (chain <> nil)
       )
    then begin
      regCode := latest;
      continue;
    end;

    FlagParse := FlagParse or FlagTemp and (FLAG_HASWIDTH or FLAG_LOOP or FLAG_GREEDY);
    if chain = nil // First piece.
    then begin
      FlagParse := FlagParse or FlagTemp and FLAG_SPECSTART;
      if fSecondPass then begin
        case latest^ of
          OP_EXACTLY: begin
              ret^ := OP_GBRANCH_EX;
              ch := (latest + REOpSz + RENextOffSz + RENumberSz)^;
              (ret + REOpSz + RENextOffSz)^ := ch;
            end;
          OP_EXACTLY_CI: begin
              ret^ := OP_GBRANCH_EX_CI;
              ch := (latest + REOpSz + RENextOffSz + RENumberSz)^;
              (ret + REOpSz + RENextOffSz)^ := _UpperCase(ch);
              (ret + REOpSz + RENextOffSz + 1)^ := _LowerCase(ch);
            end;
        end;
      end
      else begin
      end;
    end
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

function TRegExpr.ParsePiece(var FlagParse: Integer): PRegExprChar;
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

  procedure EmitComplexBraces(ABracesMin, ABracesMax: TREBracesArg; ANonGreedyOp, APossesive: boolean);
  {$IFDEF ComplexBraces}
  var
    off: TRENextOff;
  {$ENDIF}
  begin
    {$IFNDEF ComplexBraces}
    Error(reeComplexBracesNotImplemented);
    {$ELSE}
    if APossesive then
      TheOp := OP_LOOP_POSS
    else
    if ANonGreedyOp then
      TheOp := OP_LOOP_NG
    else
      TheOp := OP_LOOP;
    InsertOperator(OP_LOOPENTRY, Result, REOpSz + RENextOffSz);
    NextNode := EmitNode(TheOp);
    if regCode <> @regDummy then
    begin
      off := (Result + REOpSz + RENextOffSz) - (regCode - REOpSz - RENextOffSz);
      // back to Atom after OP_LOOPENTRY
      PREBracesArg(AlignToInt(regCode))^ := ABracesMin;
      Inc(regCode, REBracesArgSz);
      PREBracesArg(AlignToInt(regCode))^ := ABracesMax;
      Inc(regCode, REBracesArgSz);
      PRENextOff(AlignToPtr(regCode))^ := off;
      Inc(regCode, RENextOffSz);
      {$IFDEF DebugSynRegExpr}
      if regcode - programm > regCodeSize then
        raise Exception.Create
          ('TRegExpr.ParsePiece.EmitComplexBraces buffer overrun');
      {$ENDIF}
    end
    else
      Inc(regCodeSize, REBracesArgSz * 2 + RENextOffSz);
    Tail(Result, NextNode); // OP_LOOPENTRY -> OP_LOOP
    if regCode <> @regDummy then
      Tail(Result + REOpSz + RENextOffSz, NextNode); // Atom -> OP_LOOP
    {$ENDIF}
  end;

  procedure EmitSimpleBraces(ABracesMin, ABracesMax: TREBracesArg; ANonGreedyOp, APossessive: Boolean);
  begin
    if APossessive then
      TheOp := OP_BRACES_POSS
    else
    if ANonGreedyOp then
      TheOp := OP_BRACES_NG
    else
      TheOp := OP_BRACES;
    InsertOperator(TheOp, Result, REOpSz + RENextOffSz + REBracesArgSz * 2);
    if regCode <> @regDummy then
    begin
      PREBracesArg(AlignToInt(Result + REOpSz + RENextOffSz))^ := ABracesMin;
      PREBracesArg(AlignToInt(Result + REOpSz + RENextOffSz + REBracesArgSz))^ := ABracesMax;
    end;
  end;

  function DoParseBraceMinMax(var BMin, BMax: TREBracesArg): Boolean;
  var
    p: PRegExprChar;
  begin
    Result := False;
    p := regParse;
    while IsDigitChar(regParse^) do // <min> MUST appear
      Inc(regParse);
    if FAllowBraceWithoutMin and (regParse^ = ',') and (p = regParse) then
    begin
      if not (((regParse+1)^ >= '0') and ((regParse+1)^ <= '9')) then
        Exit;
      BMin := 0
    end
    else
    if (regParse^ <> '}') and (regParse^ <> ',') or (p = regParse) then
    begin
      if not FAllowLiteralBraceWithoutRange then
        Error(reeIncorrectBraces);
      Exit;
    end
    else
      BMin := ParseNumber(p, regParse - 1);
    if regParse^ = ',' then
    begin
      Inc(regParse);
      p := regParse;
      while IsDigitChar(regParse^) do
        Inc(regParse);
      if regParse^ <> '}' then
      begin
        if not FAllowLiteralBraceWithoutRange then
          Error(reeIncorrectBraces);
        Exit;
      end;
      if p = regParse then
        BMax := MaxBracesArg
      else
        BMax := ParseNumber(p, regParse - 1);
    end
    else
      BMax := BMin; // {n} == {n,n}
    Result := True;
  end;

  function ParseBraceMinMax(var BMin, BMax: TREBracesArg): Boolean;
  begin
    Result := DoParseBraceMinMax(BMin, BMax);
    if Result and (BMin > BMax) then
    begin
      Error(reeBracesMinParamGreaterMax);
      Exit;
    end;
  end;

  function CheckBraceIsLiteral: Boolean;
  var
    dummyBracesMin, dummyBracesMax: TREBracesArg;
    savedRegParse: PRegExprChar;
  begin
    Result := False;
    if not FAllowLiteralBraceWithoutRange then
      exit;
    savedRegParse := regParse;
    Inc(regParse);
    Result := not DoParseBraceMinMax(dummyBracesMin, dummyBracesMax);
    regParse := savedRegParse;
  end;

var
  op, nextch: REChar;
  NonGreedyOp, NonGreedyCh, PossessiveCh: Boolean;
  FlagTemp: Integer;
  BracesMin, BracesMax: TREBracesArg;
  savedRegParse: PRegExprChar;
begin
  FlagTemp := 0;
  Result := ParseAtom(FlagTemp);
  if Result = nil then
    Exit;

  op := regParse^;
  if not ((op = '*') or (op = '+') or (op = '?') or (op = '{')) then
  begin
    FlagParse := FlagTemp and not FLAG_LOOKAROUND;
    Exit;
  end;

  if (FlagTemp and FLAG_LOOKAROUND) <> 0 then begin
    FlagTemp:= FlagTemp and not FLAG_LOOKAROUND;
    FlagParse := FlagParse or FlagTemp and (FLAG_LOOP or FLAG_GREEDY);
    BracesMin := 0;
    if op = '{' then begin
      savedRegParse := regParse;
      Inc(regParse);
      if not ParseBraceMinMax(BracesMin, BracesMax) then
      begin
        regParse := savedRegParse;
        Exit;
      end;
    end;
    if op = '+' then
      BracesMin := 1;
    if BracesMin = 0 then
      EmitNode(OP_LOOKAROUND_OPTIONAL);

    nextch := (regParse + 1)^;
    if (nextch = '+') or  (nextch = '?') then
      Inc(regParse);
    Inc(regParse);
    op := regParse^;
    if (op = '*') or (op = '+') or (op = '?') or
       ( (op = '{') and not CheckBraceIsLiteral)
    then
      Error(reeNestedQuantif);
    Exit;
  end;

  case op of
    '*':
      begin
        if (FlagTemp and FLAG_NOT_QUANTIFIABLE) <> 0 then begin
          Error(reeNotQuantifiable);
          exit;
        end;
        FlagParse := FLAG_WORST or FLAG_SPECSTART or FLAG_LOOP;
        nextch := (regParse + 1)^;
        PossessiveCh := nextch = '+';
        if PossessiveCh then
        begin
          NonGreedyCh := False;
          NonGreedyOp := False;
        end
        else
        begin
          NonGreedyCh := nextch = '?';
          NonGreedyOp := NonGreedyCh or not fCompModifiers.G;
        end;
        if not NonGreedyCh then
          FlagParse := FlagParse or FLAG_GREEDY;
        if (FlagTemp and (FLAG_SIMPLE or FLAG_HASWIDTH)) <> (FLAG_SIMPLE or FLAG_HASWIDTH) then
        begin
          if NonGreedyOp or PossessiveCh or ((FlagTemp and FLAG_HASWIDTH) = 0) then
            EmitComplexBraces(0, MaxBracesArg, NonGreedyOp, PossessiveCh)
          else
          begin // Emit x* as (x&|), where & means "self".
            InsertOperator(OP_BRANCH, Result, REOpSz + RENextOffSz + REBranchArgSz); // Either x
            OpTail(Result, EmitNode(OP_BACK)); // and loop
            OpTail(Result, Result); // back
            Tail(Result, EmitBranch); // or
            Tail(Result, EmitNode(OP_NOTHING)); // nil.
          end
        end
        else
        begin // Simple AND has Width
          if PossessiveCh then
            TheOp := OP_STAR_POSS
          else
          if NonGreedyOp then
            TheOp := OP_STAR_NG
          else
            TheOp := OP_STAR;
          InsertOperator(TheOp, Result, REOpSz + RENextOffSz);
        end;
        if NonGreedyCh or PossessiveCh then
          Inc(regParse); // Skip extra char ('?')
      end; { of case '*' }
    '+':
      begin
        if (FlagTemp and FLAG_NOT_QUANTIFIABLE) <> 0 then begin
          Error(reeNotQuantifiable);
          exit;
        end;
        FlagParse := FLAG_WORST or FLAG_SPECSTART or (FlagTemp and FLAG_HASWIDTH) or FLAG_LOOP;
        nextch := (regParse + 1)^;
        PossessiveCh := nextch = '+';
        if PossessiveCh then
        begin
          NonGreedyCh := False;
          NonGreedyOp := False;
        end
        else
        begin
          NonGreedyCh := nextch = '?';
          NonGreedyOp := NonGreedyCh or not fCompModifiers.G;
        end;
        if not NonGreedyCh then
          FlagParse := FlagParse or FLAG_GREEDY;
        if (FlagTemp and (FLAG_SIMPLE or FLAG_HASWIDTH)) <> (FLAG_SIMPLE or FLAG_HASWIDTH) then
        begin
          if NonGreedyOp or PossessiveCh or ((FlagTemp and FLAG_HASWIDTH) = 0) then
            EmitComplexBraces(1, MaxBracesArg, NonGreedyOp, PossessiveCh)
          else
          begin // Emit x+ as x(&|), where & means "self".
            NextNode := EmitBranch; // Either
            Tail(Result, NextNode);
            Tail(EmitNode(OP_BACK), Result); // loop back
            Tail(NextNode, EmitBranch); // or
            Tail(Result, EmitNode(OP_NOTHING)); // nil.
          end
        end
        else
        begin // Simple
          if PossessiveCh then
            TheOp := OP_PLUS_POSS
          else
          if NonGreedyOp then
            TheOp := OP_PLUS_NG
          else
            TheOp := OP_PLUS;
          InsertOperator(TheOp, Result, REOpSz + RENextOffSz);
        end;
        if NonGreedyCh or PossessiveCh then
          Inc(regParse); // Skip extra char ('?')
      end; { of case '+' }
    '?':
      begin
        FlagParse := FLAG_WORST;
        nextch := (regParse + 1)^;
        PossessiveCh := nextch = '+';
        if PossessiveCh then
        begin
          NonGreedyCh := False;
          NonGreedyOp := False;
        end
        else
        begin
          NonGreedyCh := nextch = '?';
          NonGreedyOp := NonGreedyCh or not fCompModifiers.G;
        end;
        if not NonGreedyCh then
          FlagParse := FlagParse or FLAG_GREEDY;
        if NonGreedyOp or PossessiveCh then
        begin // We emit x?? as x{0,1}?
          if (FlagTemp and FLAG_SIMPLE) = 0 then
          begin
            EmitComplexBraces(0, 1, NonGreedyOp, PossessiveCh);
          end
          else
            EmitSimpleBraces(0, 1, NonGreedyOp, PossessiveCh);
        end
        else
        begin // greedy '?'
          InsertOperator(OP_BRANCH, Result, REOpSz + RENextOffSz + REBranchArgSz); // Either x
          Tail(Result, EmitBranch); // or
          NextNode := EmitNode(OP_NOTHING); // nil.
          Tail(Result, NextNode);
          OpTail(Result, NextNode);
        end;
        if NonGreedyCh or PossessiveCh then
          Inc(regParse); // Skip extra char ('?')
      end; { of case '?' }
    '{':
      begin
        savedRegParse := regParse;
        Inc(regParse);
        if not ParseBraceMinMax(BracesMin, BracesMax) then
        begin
          regParse := savedRegParse;
          Exit;
        end;
        if (FlagTemp and FLAG_NOT_QUANTIFIABLE) <> 0 then begin
          Error(reeNotQuantifiable);
          exit;
        end;
        if BracesMin > 0 then
          FlagParse := FLAG_WORST or (FlagTemp and FLAG_HASWIDTH);
        if BracesMax > 0 then
          FlagParse := FlagParse or FLAG_SPECSTART;

        nextch := (regParse + 1)^;
        PossessiveCh := nextch = '+';
        if PossessiveCh then
        begin
          NonGreedyCh := False;
          NonGreedyOp := False;
        end
        else
        begin
          NonGreedyCh := nextch = '?';
          NonGreedyOp := NonGreedyCh or not fCompModifiers.G;
        end;
        if not NonGreedyCh then
          FlagParse := FlagParse or FLAG_GREEDY;
        if BracesMax >= 2 then
          FlagParse := FlagParse or FLAG_LOOP;
        if (FlagTemp and (FLAG_SIMPLE or FLAG_HASWIDTH)) = (FLAG_SIMPLE or FLAG_HASWIDTH) then
          EmitSimpleBraces(BracesMin, BracesMax, NonGreedyOp, PossessiveCh)
        else
        begin
          EmitComplexBraces(BracesMin, BracesMax, NonGreedyOp, PossessiveCh);
        end;
        if NonGreedyCh or PossessiveCh then
          Inc(regParse); // Skip extra char '?'
      end; // of case '{'
    // else // here we can't be
  end; { of case op }

  FlagParse := FlagParse or FlagTemp and (FLAG_LOOP or FLAG_GREEDY);
  Inc(regParse);
  op := regParse^;
  if (op = '*') or (op = '+') or (op = '?') or
     ( (op = '{') and not CheckBraceIsLiteral)
  then
    Error(reeNestedQuantif);
end; { of function TRegExpr.ParsePiece
  -------------------------------------------------------------- }

function TRegExpr.HexDig(Ch: REChar): Integer;
begin
  case Ch of
    '0' .. '9':
      Result := Ord(Ch) - Ord('0');
    'a' .. 'f':
      Result := Ord(Ch) - Ord('a') + 10;
    'A' .. 'F':
      Result := Ord(Ch) - Ord('A') + 10;
  else
    Result := 0;
    Error(reeBadHexDigit);
  end;
end;

function TRegExpr.UnQuoteChar(var APtr, AEnd: PRegExprChar): REChar;
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
        Result := #0;
        Inc(APtr);
        if APtr >= AEnd then
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
        if APtr >= AEnd then
        begin
          Error(reeNoHexCodeAfterBSlashX);
          Exit;
        end;
        if APtr^ = '{' then
        begin // \x{nnnn}
          repeat
            Inc(APtr);
            if APtr >= AEnd then
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
          if APtr >= AEnd then
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
    if (Result <> '_') and IsWordChar(Result) then
    begin
      fLastErrorSymbol := Result;
      Error(reeUnknownMetaSymbol);
    end;
  end;
end;

function TRegExpr.ParseAtom(var FlagParse: Integer): PRegExprChar;
// the lowest level
// Optimization:  gobbles an entire sequence of ordinary characters so that
// it can turn them into a single node, which is smaller to store and
// faster to run. Backslashed characters are exceptions, each becoming a
// separate node; the code is simpler that way and it's not worth fixing.
var
  ret, ret2, regLookBehindOption: PRegExprChar;
  RangeBeg, RangeEnd: REChar;
  CanBeRange: Boolean;
  AddrOfLen: PLongInt;
  HasCaseSenseChars: boolean;

  function ParseNumber(var AParsePos: PRegExprChar; out ANumber: Integer): Boolean;
  begin
    Result := False;
    ANumber := 0;
    while (AParsePos^ >= '0') and (AParsePos^ <= '9') do
    begin
      if ANumber > (High(ANumber)-10) div 10 then
        exit;
      ANumber := ANumber * 10 + (Ord(AParsePos^) - Ord('0'));
      inc(AParsePos);
    end;
    Result := True;
  end;

  procedure EmitExactly(Ch: REChar);
  var
    cs: Boolean;
  begin
    if fCompModifiers.I then
      ret := EmitNode(OP_EXACTLY_CI)
    else
      ret := EmitNode(OP_EXACTLY);
    EmitInt(1);
    cs := False;
    if fCompModifiers.I then begin
      Ch := _UpperCase(Ch);
      EmitC(Ch);
      if Ch <> _LowerCase(Ch) then
        cs := True;
    end
    else
      EmitC(Ch);
    if not cs then
      PREOp(ret)^ := OP_EXACTLY;
    FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
  end;

  procedure EmitRangeChar(Ch: REChar; AStartOfRange: Boolean);
  begin
    CanBeRange := AStartOfRange;
    if fCompModifiers.I then begin
      Ch := _UpperCase(Ch);
      if Ch <> _LowerCase(Ch) then
        HasCaseSenseChars := True;
    end;
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
        Pointer(AddrOfLen) := regCode;
        EmitInt(0);
      end;
      Inc(AddrOfLen^);
      EmitC(Ch);
    end;
  end;

  procedure EmitRangePacked(ch1, ch2: REChar);
  var
    ChkIndex: Integer;
  begin
    AddrOfLen := nil;
    CanBeRange := False;

    if fCompModifiers.I then
    begin
      ch1 := _UpperCase(ch1);
      ch2 := _UpperCase(ch2);
      if (Ch1 <> _LowerCase(Ch1)) or (Ch2 <> _LowerCase(Ch2)) then
        HasCaseSenseChars := True;
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

  {$IFDEF FastUnicodeData}
  procedure EmitCategoryInCharClass(APositive: Boolean);
  var
    ch, ch2: REChar;
  begin
    AddrOfLen := nil;
    CanBeRange := False;
    Inc(regParse);
    FindCategoryName(regParse, ch, ch2);
    if APositive then
      EmitC(OpKind_CategoryYes)
    else
      EmitC(OpKind_CategoryNo);
    EmitC(ch);
    EmitC(ch2);
  end;
  {$ENDIF}

var
  FlagTemp: Integer;
  Len: Integer;
  SavedPtr: PRegExprChar;
  EnderChar, TempChar: REChar;
  DashForRange: Boolean;
  GrpKind: TREGroupKind;
  GrpName: RegExprString;
  GrpIndex, ALen, RegGrpCountBefore, AMaxLen: integer;
  NextCh: REChar;
  op: TREOp;
  SavedModifiers: TRegExprModifiers;
begin
  Result := nil;
  FlagTemp := 0;
  FlagParse := FLAG_WORST;
  AddrOfLen := nil;
  GrpIndex := -1;

  Inc(regParse);
  case (regParse - 1)^ of
    '^':
     begin
      FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
      if not fCompModifiers.M
        {$IFDEF UseLineSep} or (fLineSeparators = '') {$ENDIF} then
        ret := EmitNode(OP_BOL)
      else
        ret := EmitNode(OP_BOL_ML);
     end;

    '$':
     begin
      FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
      if not fCompModifiers.M
        {$IFDEF UseLineSep} or (fLineSeparators = '') {$ENDIF} then
        ret := EmitNode(OP_EOL)
      else
        ret := EmitNode(OP_EOL_ML);
     end;

    '.':
     begin
      if fCompModifiers.S then
      begin
        ret := EmitNode(OP_ANY);
        FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
      end
      else
      begin // not /s, so emit [^:LineSeparators:]
        ret := EmitNode(OP_ANY_ML);
        FlagParse := FlagParse or FLAG_HASWIDTH; // not so simple ;)
      end;
     end;

    '[':
      begin
        HasCaseSenseChars := False;
        if regParse^ = '^' then
        begin // Complement of range.
          if fCompModifiers.I then
            ret := EmitNode(OP_ANYBUT_CI)
          else
            ret := EmitNode(OP_ANYBUT);
          Inc(regParse);
        end
        else if fCompModifiers.I then
          ret := EmitNode(OP_ANYOF_CI)
        else
          ret := EmitNode(OP_ANYOF);

        CanBeRange := False;

        if regParse^ = ']' then
        begin
          // first ']' inside [] treated as simple char, no need to check '['
          EmitRangeChar(regParse^, (regParse + 1)^ = '-');
          Inc(regParse);
        end;

        while (regParse < fRegexEnd) and (regParse^ <> ']') do
        begin
          // last '-' inside [] treated as simple dash
          if (regParse^ = '-') and
            ((regParse + 1) < fRegexEnd) and
            ((regParse + 1)^ = ']') then
          begin
            EmitRangeChar('-', False);
            Inc(regParse);
            Break;
          end;

          // char '-' which (maybe) makes a range
          if (regParse^ = '-') and ((regParse + 1) < fRegexEnd) and CanBeRange then
          begin
            Inc(regParse);
            RangeEnd := regParse^;
            if RangeEnd = EscChar then
            begin
              if _IsMetaChar((regParse + 1)^) then
              begin
                Error(reeMetaCharAfterMinusInRange);
                Exit;
              end;
              Inc(regParse);
              RangeEnd := UnQuoteChar(regParse, fRegexEnd);
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
            Inc(regParse);
          end
          else
          begin
            if regParse^ = EscChar then
            begin
              Inc(regParse);
              if regParse >= fRegexEnd then
              begin
                Error(reeParseAtomTrailingBackSlash);
                Exit;
              end;
              if _IsMetaChar(regParse^) then
              begin
                AddrOfLen := nil;
                CanBeRange := False;
                EmitC(OpKind_MetaClass);
                case regParse^ of
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
                  'R':
                    EmitC(REChar(CheckerIndex_AnyLineBreak));
                else
                  Error(reeBadOpcodeInCharClass);
                end;
              end
              else
              {$IFDEF FastUnicodeData}
              if regParse^ = 'p' then
                EmitCategoryInCharClass(True)
              else
              if regParse^ = 'P' then
                EmitCategoryInCharClass(False)
              else
              {$ENDIF}
              begin
                TempChar := UnQuoteChar(regParse, fRegexEnd);
                // False if '-' is last char in []
                DashForRange :=
                  (regParse + 2 < fRegexEnd) and
                  ((regParse + 1)^ = '-') and
                  ((regParse + 2)^ <> ']');
                EmitRangeChar(TempChar, DashForRange);
              end;
            end
            else
            begin
              // False if '-' is last char in []
              DashForRange :=
                (regParse + 2 < fRegexEnd) and
                ((regParse + 1)^ = '-') and
                ((regParse + 2)^ <> ']');
              EmitRangeChar(regParse^, DashForRange);
            end;
            Inc(regParse);
          end;
        end; { of while }
        AddrOfLen := nil;
        CanBeRange := False;
        EmitC(OpKind_End);
        if fCompModifiers.I and not HasCaseSenseChars then begin
          if PREOp(ret)^ = OP_ANYBUT_CI then
            PREOp(ret)^ := OP_ANYBUT;
          if PREOp(ret)^ = OP_ANYOF_CI then
            PREOp(ret)^ := OP_ANYOF;
        end;
        if regParse^ <> ']' then
        begin
          Error(reeUnmatchedSqBrackets);
          Exit;
        end;
        Inc(regParse);
        FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
      end;

    '(':
      begin
        GrpKind := gkNormalGroup;
        GrpName := '';

        // A: detect kind of expression in brackets
        if regParse^ = '?' then
        begin
          NextCh := (regParse + 1)^;
          case NextCh of
            ':':
              begin
                // non-capturing group: (?:regex)
                GrpKind := gkNonCapturingGroup;
                Inc(regParse, 2);
              end;
            '>':
              begin
                // atomic group: (?>regex)
                GrpKind := gkAtomicGroup;
                Inc(regParse, 2);
              end;
            'P':
              begin
                if (regParse + 4 >= fRegexEnd) then
                  Error(reeNamedGroupBad);
                case (regParse + 2)^ of
                  '<':
                    begin
                      // named group: (?P<name>regex)
                      GrpKind := gkNormalGroup;
                      FindGroupName(regParse + 3, fRegexEnd, '>', GrpName);
                      Inc(regParse, Length(GrpName) + 4);
                    end;
                  '=':
                    begin
                      // back-reference to named group: (?P=name)
                      GrpKind := gkNamedGroupReference;
                      FindGroupName(regParse + 3, fRegexEnd, ')', GrpName);
                      Inc(regParse, Length(GrpName) + 4);
                    end;
                  '>':
                    begin
                      // subroutine call to named group: (?P>name)
                      GrpKind := gkSubCall;
                      FindGroupName(regParse + 3, fRegexEnd, ')', GrpName);
                      Inc(regParse, Length(GrpName) + 4);
                      if fSecondPass then begin
                        GrpIndex := GrpNames.MatchIndexFromName(GrpName);
                        if GrpIndex < 1 then
                          Error(reeNamedGroupBadRef);
                      end;
                    end;
                  else
                    Error(reeNamedGroupBad);
                end;
              end;
            '<':
              begin
                // lookbehind: (?<=foo)bar
                case (regParse + 2)^ of
                  '=':
                    begin
                      if (regParse + 4 >= fRegexEnd) then
                        Error(reeLookbehindBad);
                      GrpKind := gkLookbehind;
                      Inc(regParse, 3);
                    end;
                  '!':
                    begin
                      if (regParse + 4 >= fRegexEnd) then
                        Error(reeLookbehindBad);
                      GrpKind := gkLookbehindNeg;
                      Inc(regParse, 3);
                    end;
                  'A'..'Z', 'a'..'z':
                    begin
                      // named group: (?<name>regex)
                      if (regParse + 4 >= fRegexEnd) then
                        Error(reeNamedGroupBad);
                      GrpKind := gkNormalGroup;
                      FindGroupName(regParse + 2, fRegexEnd, '>', GrpName);
                      Inc(regParse, Length(GrpName) + 3);
                    end;
                  else
                    Error(reeIncorrectSpecialBrackets);
                end;
              end;
            '=', '!':
              begin
                // lookaheads: foo(?=bar) and foo(?!bar)
                if (regParse + 3 >= fRegexEnd) then
                  Error(reeLookaheadBad);
                if NextCh = '=' then
                begin
                  GrpKind := gkLookahead;
                end
                else
                begin
                  GrpKind := gkLookaheadNeg;
                end;
                Inc(regParse, 2);
              end;
            '#':
              begin
                // (?#comment)
                FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
                GrpKind := gkComment;
                Inc(regParse, 2);
              end;
            'a'..'z', '-':
              begin
                // modifiers string like (?mxr)
                FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
                GrpKind := gkModifierString;
                Inc(regParse);
              end;
            'R', '0':
              begin
                // recursion (?R), (?0)
                GrpKind := gkRecursion;
                Inc(regParse, 2);
                if regParse^ <> ')' then
                  Error(reeBadRecursion);
                Inc(regParse);
              end;
            '1'..'9':
              begin
                // subroutine call (?1)..(?99)
                GrpKind := gkSubCall;
                Inc(regParse, 1);
                if not ParseNumber(regParse, GrpIndex) or (regParse^ <> ')') then
                begin
                  Error(reeBadRecursion);
                  Exit;
                end;
                Inc(regParse, 1);
                if fSecondPass and (GrpIndex > GrpCount) then
                  Error(reeBadSubCall);
              end;
            '''':
              begin
                // named group: (?'name'regex)
                if (regParse + 4 >= fRegexEnd) then
                  Error(reeNamedGroupBad);
                GrpKind := gkNormalGroup;
                FindGroupName(regParse + 2, fRegexEnd, '''', GrpName);
                Inc(regParse, Length(GrpName) + 3);
              end;
            '&':
              begin
                // subroutine call to named group: (?&name)
                if (regParse + 2 >= fRegexEnd) then
                  Error(reeBadSubCall);
                GrpKind := gkSubCall;
                FindGroupName(regParse + 2, fRegexEnd, ')', GrpName);
                Inc(regParse, Length(GrpName) + 3);
                if fSecondPass then begin
                  GrpIndex := GrpNames.MatchIndexFromName(GrpName);
                  if GrpIndex < 1 then
                    Error(reeNamedGroupBadRef);
                end;
              end;
            else
              Error(reeIncorrectSpecialBrackets);
          end;
        end;

        // B: process found kind of brackets
        case GrpKind of
          gkNonCapturingGroup:
            begin
              ret := DoParseReg(True, False, FlagTemp, OP_NONE, OP_NONE);
              if ret = nil then
              begin
                Result := nil;
                Exit;
              end;
              FlagParse := FlagParse or FlagTemp and (FLAG_HASWIDTH or FLAG_SPECSTART or FLAG_LOOP or FLAG_GREEDY);
            end;

          gkNormalGroup,
          gkAtomicGroup:
            begin
              // skip this block for one of passes, to not double groups count;
              // must take first pass (we need GrpNames filled)
              if (GrpKind = gkNormalGroup) then begin
                Inc(ParsedGrpCount);
                if fSecondPass then begin
                  GrpIndexes[ParsedGrpCount] := regNumBrackets;
                end
                else
                if (GrpName <> '') then
                begin
                  // first pass
                  if GrpNames.MatchIndexFromName(GrpName) >= 0 then
                    Error(reeNamedGroupDupName);
                  GrpNames.Add(GrpName, ParsedGrpCount);
                end;
              end;

              if GrpKind = gkAtomicGroup then
                ret := DoParseReg(True, True, FlagTemp, OP_OPEN_ATOMIC, OP_CLOSE_ATOMIC)
              else
                ret := ParseReg(True, FlagTemp);
              if ret = nil then
              begin
                Result := nil;
                Exit;
              end;
              FlagParse := FlagParse or FlagTemp and (FLAG_HASWIDTH or FLAG_SPECSTART or FLAG_LOOP or FLAG_GREEDY);
            end;

          gkLookahead,
          gkLookaheadNeg:
            begin
              case GrpKind of
                gkLookahead: ret := EmitNode(OP_LOOKAHEAD);
                gkLookaheadNeg: ret := EmitNode(OP_LOOKAHEAD_NEG);
              end;

              Result := DoParseReg(True, False, FlagTemp, OP_NONE, OP_LOOKAHEAD_END);
              if Result = nil then
                Exit;

              Tail(ret, regLast(Result));
              FlagParse := FlagParse and not FLAG_HASWIDTH or FLAG_LOOKAROUND;
            end;

          gkLookbehind,
          gkLookbehindNeg:
            begin
              case GrpKind of
                gkLookbehind: ret := EmitNode(OP_LOOKBEHIND);
                gkLookbehindNeg: ret := EmitNode(OP_LOOKBEHIND_NEG);
              end;
              regLookBehindOption := regCode;
              if (regCode <> @regDummy) then
                Inc(regCode, ReOpLookBehindOptionsSz)
              else
                Inc(regCodeSize, ReOpLookBehindOptionsSz);

              RegGrpCountBefore := ParsedGrpCount;
              Result := DoParseReg(True, False, FlagTemp, OP_NONE, OP_LOOKBEHIND_END);
              if Result = nil then
                Exit;

              Tail(ret, regLast(Result));

              ret2 := Result;
              if (regCode <> @regDummy) then begin
                ALen := 0;
                if IsPartFixedLength(ret2, op, ALen, AMaxLen, OP_LOOKBEHIND_END, nil, [flfSkipLookAround]) then
                  PReOpLookBehindOptions(regLookBehindOption)^.IsGreedy := OPT_LOOKBEHIND_FIXED
                else
                if (ParsedGrpCount > RegGrpCountBefore) and (not FAllowUnsafeLookBehind) then
                  Error(reeLookaroundNotSafe)
                else
                if (FlagTemp and (FLAG_GREEDY)) = (FLAG_GREEDY) then
                  PReOpLookBehindOptions(regLookBehindOption)^.IsGreedy := OPT_LOOKBEHIND_GREEDY
                else
                  PReOpLookBehindOptions(regLookBehindOption)^.IsGreedy := OPT_LOOKBEHIND_NON_GREEDY;
                PReOpLookBehindOptions(regLookBehindOption)^.MatchLenMin := ALen;
                PReOpLookBehindOptions(regLookBehindOption)^.MatchLenMax := AMaxLen;
              end;

              FlagParse := FlagParse and not FLAG_HASWIDTH or FLAG_LOOKAROUND;
            end;

          gkNamedGroupReference:
            begin
              Len := GrpNames.MatchIndexFromName(GrpName);
              if fSecondPass and (Len < 0) then
                Error(reeNamedGroupBadRef);
              ret := EmitGroupRef(Len, fCompModifiers.I);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;

          gkModifierString:
            begin
              SavedPtr := regParse;
              while (regParse < fRegexEnd) and (regParse^ <> ')') and (regParse^ <> ':') do
                Inc(regParse);
              SavedModifiers := fCompModifiers;
              if (regParse^ = ':') and ParseModifiers(SavedPtr, regParse - SavedPtr, fCompModifiers) then
              begin
                Inc(regParse); // skip ')'
                ret := ParseReg(True, FlagTemp);
                fCompModifiers := SavedModifiers;
                if ret = nil then
                begin
                  Result := nil;
                  Exit;
                end;
                FlagParse := FlagParse or FlagTemp and (FLAG_HASWIDTH or FLAG_SPECSTART or FLAG_LOOP or FLAG_GREEDY);
              end
              else
              if (regParse^ = ')') and ParseModifiers(SavedPtr, regParse - SavedPtr, fCompModifiers) then
              begin
                Inc(regParse); // skip ')'
                ret := EmitNode(OP_COMMENT); // comment
              end
              else
              begin
                Error(reeUnrecognizedModifier);
                Exit;
              end;
            end;

          gkComment:
            begin
              while (regParse < fRegexEnd) and (regParse^ <> ')') do
                Inc(regParse);
              if regParse^ <> ')' then
              begin
                Error(reeUnclosedComment);
                Exit;
              end;
              Inc(regParse); // skip ')'
              ret := EmitNode(OP_COMMENT); // comment
            end;

          gkRecursion:
            begin
              // set FLAG_HASWIDTH to allow compiling of such regex: b(?:m|(?R))*e
              FlagParse := FlagParse or FLAG_HASWIDTH;
              ret := EmitNode(OP_RECUR);
            end;

          gkSubCall:
            begin
              // set FLAG_HASWIDTH like for (?R)
              FlagParse := FlagParse or FLAG_HASWIDTH;
              ret := EmitNodeWithGroupIndex(OP_SUBCALL, GrpIndex);
            end;
        end; // case GrpKind of
      end;

    '|', ')':
      begin // Supposed to be caught earlier.
        Error(reeInternalUrp);
        Exit;
      end;

    '?', '+', '*':
      begin
        Error(reeQuantifFollowsNothing);
        Exit;
      end;

    EscChar:
      begin
        if regParse >= fRegexEnd then
        begin
          Error(reeTrailingBackSlash);
          Exit;
        end;
        case regParse^ of
          'b':
            begin
              FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
              ret := EmitNode(OP_BOUND);
            end;
          'B':
            begin
              FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
              ret := EmitNode(OP_NOTBOUND);
            end;
          'A':
            begin
              FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
              ret := EmitNode(OP_BOL);
            end;
          'z':
            begin
              FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
              ret := EmitNode(OP_EOL);
            end;
          'Z':
            begin
              FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
              ret := EmitNode(OP_EOL2);
            end;
          'G':
            begin
              FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
              ret := EmitNode(OP_CONTINUE_POS);
            end;
          'd':
            begin // r.e.extension - any digit ('0' .. '9')
              ret := EmitNode(OP_ANYDIGIT);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'D':
            begin // r.e.extension - not digit ('0' .. '9')
              ret := EmitNode(OP_NOTDIGIT);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          's':
            begin // r.e.extension - any space char
              ret := EmitNode(OP_ANYSPACE);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'S':
            begin // r.e.extension - not space char
              ret := EmitNode(OP_NOTSPACE);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'w':
            begin // r.e.extension - any english char / digit / '_'
              ret := EmitNode(OP_ANYLETTER);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'W':
            begin // r.e.extension - not english char / digit / '_'
              ret := EmitNode(OP_NOTLETTER);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'v':
            begin
              ret := EmitNode(OP_ANYVERTSEP);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'V':
            begin
              ret := EmitNode(OP_NOTVERTSEP);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'h':
            begin
              ret := EmitNode(OP_ANYHORZSEP);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'H':
            begin
              ret := EmitNode(OP_NOTHORZSEP);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          '1' .. '9':
            begin
              if fSecondPass and (Ord(regParse^) - Ord('0') > GrpCount) then
                Error(reeBadReference);
              ret := EmitGroupRef(Ord(regParse^) - Ord('0'), fCompModifiers.I);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'g':
            begin
              case (regParse + 1)^ of
                '<', '''':
                  begin
                    // subroutine call to named group
                    case (regParse + 1)^ of
                      '<':  FindGroupName(regParse + 2, fRegexEnd, '>', GrpName);
                      '''': FindGroupName(regParse + 2, fRegexEnd, '''', GrpName);
                    end;
                    Inc(regParse, Length(GrpName) + 2);
                    GrpIndex := GrpNames.MatchIndexFromName(GrpName);
                    if fSecondPass and (GrpIndex < 1) then
                      Error(reeNamedGroupBadRef);
                    ret := EmitNodeWithGroupIndex(OP_SUBCALL, GrpIndex);
                    FlagParse := FlagParse or FLAG_HASWIDTH;
                  end;
                '{':
                  begin
                    // back-reference to named group
                    FindGroupName(regParse + 2, fRegexEnd, '}', GrpName);
                    Inc(regParse, Length(GrpName) + 2);
                    GrpIndex := GrpNames.MatchIndexFromName(GrpName);
                    if fSecondPass and  (GrpIndex < 1) then
                      Error(reeNamedGroupBadRef);
                    ret := EmitGroupRef(GrpIndex, fCompModifiers.I);
                    FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
                  end;
                '0'..'9':
                  begin
                    inc(regParse);
                    if not ParseNumber(regParse, GrpIndex) then begin
                      Error(reeBadReference);
                      Exit;
                    end;
                    dec(regParse);
                    if GrpIndex = 0 then
                      Error(reeBadReference);
                    if fSecondPass and (GrpIndex > GrpCount) then
                      Error(reeBadReference);
                    ret := EmitGroupRef(GrpIndex, fCompModifiers.I);
                    FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
                  end;
              else
                Error(reeBadReference);
              end;
            end;
          'k':
            begin
              // back-reference to named group
              case (regParse + 1)^ of
                '<':
                  FindGroupName(regParse + 2, fRegexEnd, '>', GrpName);
                '''':
                  FindGroupName(regParse + 2, fRegexEnd, '''', GrpName);
                '{':
                  FindGroupName(regParse + 2, fRegexEnd, '}', GrpName);
              else
                Error(reeBadReference);
              end;
              Inc(regParse, Length(GrpName) + 2);
              GrpIndex := GrpNames.MatchIndexFromName(GrpName);
              if fSecondPass and (GrpIndex < 1) then
                Error(reeNamedGroupBadRef);
              ret := EmitGroupRef(GrpIndex, fCompModifiers.I);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'K':
            begin
              ret := EmitNode(OP_RESET_MATCHPOS);
              FlagParse := FlagParse or FLAG_NOT_QUANTIFIABLE;
            end;
          {$IFDEF FastUnicodeData}
          'p':
            begin
              ret := EmitCategoryMain(True);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          'P':
            begin
              ret := EmitCategoryMain(False);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
          {$ENDIF}
          'R':
            begin
              ret := EmitNode(OP_ANYLINEBREAK);
              FlagParse := FlagParse or FLAG_HASWIDTH or FLAG_SIMPLE;
            end;
        else
          EmitExactly(UnQuoteChar(regParse, fRegexEnd));
        end; { of case }
        Inc(regParse);
      end;

  else
    begin
      Dec(regParse);
      if fCompModifiers.X and // check for eXtended syntax
        ((regParse^ = '#') or IsIgnoredChar(regParse^)) then
      begin // \x
        if regParse^ = '#' then
        begin // Skip eXtended comment
          // find comment terminator (group of \n and/or \r)
          while (regParse < fRegexEnd) and (regParse^ <> #$d) and
            (regParse^ <> #$a) do
            Inc(regParse);
          while (regParse^ = #$d) or (regParse^ = #$a)
          // skip comment terminator
            do
            Inc(regParse);
          // attempt to support different type of line separators
        end
        else
        begin // Skip the blanks!
          while IsIgnoredChar(regParse^) do
            Inc(regParse);
        end;
        ret := EmitNode(OP_COMMENT); // comment
      end
      else
      begin
        Len := FindSkippedMetaLen(regParse, fRegexEnd);
        if Len <= 0 then
          if regParse^ <> '{' then
          begin
            Error(reeRarseAtomInternalDisaster);
            Exit;
          end
          else
            Len := FindSkippedMetaLen(regParse + 1, fRegexEnd) + 1;
            // bad {n,m} - compile as EXACTLY
        EnderChar := (regParse + Len)^;
        if (Len > 1) and ((EnderChar = '*') or (EnderChar = '+') or (EnderChar = '?') or (EnderChar = '{')) then
          Dec(Len); // back off clear of ?+*{ operand.
        FlagParse := FlagParse or FLAG_HASWIDTH;
        if Len = 1 then
          FlagParse := FlagParse or FLAG_SIMPLE;
        if fCompModifiers.I then
          ret := EmitNode(OP_EXACTLY_CI)
        else
          ret := EmitNode(OP_EXACTLY);
        EmitInt(0);
        while (Len > 0) and ((not fCompModifiers.X) or (regParse^ <> '#')) do
        begin
          if not fCompModifiers.X or not IsIgnoredChar(regParse^) then
          begin
            if fCompModifiers.I then
              EmitC(_UpperCase(regParse^))
            else
              EmitC(regParse^);
            if regCode <> @regDummy then
              Inc(regExactlyLen^);
          end;
          Inc(regParse);
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
  if (fRegexStart = nil) or (regParse = nil) then
    Exit; // not in compiling mode ?
  Result := regParse - fRegexStart;
end; { of function TRegExpr.GetCompilerErrorPos
  -------------------------------------------------------------- }

{ ============================================================= }
{ ===================== Matching section ====================== }
{ ============================================================= }

procedure TRegExpr.FindGroupName(APtr, AEndPtr: PRegExprChar; AEndChar: REChar; var AName: RegExprString);
// check that group name is valid identifier, started from non-digit
// this is to be like in Python regex
var
  P: PRegExprChar;
begin
  P := APtr;
  if IsDigitChar(P^) or not IsWordChar(P^) then
    Error(reeNamedGroupBadName);

  repeat
    if P >= AEndPtr then
      Error(reeNamedGroupBad);
    if P^ = AEndChar then
      Break;
    if not (IsWordChar(P^) or (P^ = '_')) then
      Error(reeNamedGroupBadName);
    Inc(P);
  until False;

  SetString(AName, APtr, P-APtr);
end;

function TRegExpr.FindRepeated(p: PRegExprChar; AMax: Integer): Integer;
// repeatedly match something simple, report how many
// p: points to current opcode
var
  scan: PRegExprChar;
  opnd: PRegExprChar;
  TheMax: PtrInt; // PtrInt, gets diff of 2 pointers
  InvChar: REChar;
  CurStart, CurEnd: PRegExprChar;
  ArrayIndex: Integer;
  {$IFDEF UnicodeEx}
  i: Integer;
  {$ENDIF}
begin
  Result := 0;
  scan := regInput; // points into InputString
  opnd := p + REOpSz + RENextOffSz; // points to operand of opcode (after OP_nnn code)
  TheMax := fInputEnd - scan;
  if TheMax > AMax then
    TheMax := AMax;
  case PREOp(p)^ of
    OP_ANY:
      begin
        // note - OP_ANY_ML cannot be proceeded in FindRepeated because can skip
        // more than one char at once
        {$IFDEF UnicodeEx}
        for i := 1 to TheMax do
          IncUnicode2(scan, Result);
        {$ELSE}
        Result := TheMax;
        Inc(scan, Result);
        {$ENDIF}
      end;

    OP_EXACTLY:
      begin // in opnd can be only ONE char !!!
        {
        // Alexey: commented because of https://github.com/andgineer/TRegExpr/issues/145
        NLen := PLongInt(opnd)^;
        if TheMax > NLen then
          TheMax := NLen;
        }
        Inc(opnd, RENumberSz);
        while (Result < TheMax) and (opnd^ = scan^) do
        begin
          Inc(Result);
          Inc(scan);
        end;
      end;

    OP_EXACTLY_CI:
      begin // in opnd can be only ONE char !!!
        {
        // Alexey: commented because of https://github.com/andgineer/TRegExpr/issues/145
        NLen := PLongInt(opnd)^;
        if TheMax > NLen then
          TheMax := NLen;
        }
        Inc(opnd, RENumberSz);
        while (Result < TheMax) and (opnd^ = scan^) do
        begin // prevent unneeded InvertCase
          Inc(Result);
          Inc(scan);
        end;
        if Result < TheMax then
        begin
          InvChar := _LowerCase(opnd^); // store in register
          while (Result < TheMax) and ((opnd^ = scan^) or (InvChar = scan^)) do
          begin
            Inc(Result);
            Inc(scan);
          end;
        end;
      end;

    OP_BSUBEXP:
      begin
        ArrayIndex := GrpIndexes[PReGroupIndex(opnd)^];
        if ArrayIndex < 0 then
          Exit;
        CurStart := GrpBounds[regRecursion].GrpStart[ArrayIndex];
        if CurStart = nil then
          Exit;
        CurEnd := GrpBounds[regRecursion].GrpEnd[ArrayIndex];
        if CurEnd = nil then
          Exit;
        repeat
          opnd := CurStart;
          while opnd < CurEnd do
          begin
            if (scan >= fInputEnd) or (scan^ <> opnd^) then
              Exit;
            Inc(scan);
            Inc(opnd);
          end;
          Inc(Result);
          regInput := scan;
        until Result >= AMax;
      end;

    OP_BSUBEXP_CI:
      begin
        ArrayIndex := GrpIndexes[PReGroupIndex(opnd)^];
        if ArrayIndex < 0 then
          Exit;
        CurStart := GrpBounds[regRecursion].GrpStart[ArrayIndex];
        if CurStart = nil then
          Exit;
        CurEnd := GrpBounds[regRecursion].GrpEnd[ArrayIndex];
        if CurEnd = nil then
          Exit;
        repeat
          opnd := CurStart;
          while opnd < CurEnd do
          begin
            if (scan >= fInputEnd) or
              ((scan^ <> opnd^) and (scan^ <> InvertCase(opnd^))) then
              Exit;
            Inc(scan);
            Inc(opnd);
          end;
          Inc(Result);
          regInput := scan;
        until Result >= AMax;
      end;

    OP_ANYDIGIT:
      while (Result < TheMax) and IsDigitChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;

    OP_NOTDIGIT:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and not IsDigitChar(scan^) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and not IsDigitChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_ANYLETTER:
      while (Result < TheMax) and IsWordChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;

    OP_NOTLETTER:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and not IsWordChar(scan^) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and not IsWordChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_ANYSPACE:
      while (Result < TheMax) and IsSpaceChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;

    OP_NOTSPACE:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and not IsSpaceChar(scan^) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and not IsSpaceChar(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_ANYVERTSEP:
      while (Result < TheMax) and IsVertLineSeparator(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;

    OP_NOTVERTSEP:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and not IsVertLineSeparator(scan^) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and not IsVertLineSeparator(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_ANYHORZSEP:
      while (Result < TheMax) and IsHorzSeparator(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;

    OP_NOTHORZSEP:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and not IsHorzSeparator(scan^) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and not IsHorzSeparator(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_ANYOF:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and FindInCharClass(opnd, scan^, False) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and FindInCharClass(opnd, scan^, False) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_ANYBUT:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and not FindInCharClass(opnd, scan^, False) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and not FindInCharClass(opnd, scan^, False) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_ANYOF_CI:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and FindInCharClass(opnd, scan^, True) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and FindInCharClass(opnd, scan^, True) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_ANYBUT_CI:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and not FindInCharClass(opnd, scan^, True) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and not FindInCharClass(opnd, scan^, True) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    {$IFDEF FastUnicodeData}
    OP_ANYCATEGORY:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and MatchOneCharCategory(opnd, scan) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and MatchOneCharCategory(opnd, scan) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}

    OP_NOTCATEGORY:
      {$IFDEF UNICODEEX}
      begin
        i := 0;
        while (i < TheMax) and not MatchOneCharCategory(opnd, scan) do
        begin
          Inc(i);
          IncUnicode2(scan, Result);
        end;
      end;
      {$ELSE}
      while (Result < TheMax) and not MatchOneCharCategory(opnd, scan) do
      begin
        Inc(Result);
        Inc(scan);
      end;
      {$ENDIF}
    {$ENDIF}

    OP_ANYLINEBREAK:
      while (Result < TheMax) and IsAnyLineBreak(scan^) do
      begin
        Inc(Result);
        Inc(scan);
      end;

  else
    Result := 0;
    Error(reeRegRepeatCalledInappropriately);
    Exit;
  end; { of case }
  regInput := scan;
end; { of function TRegExpr.FindRepeated
  -------------------------------------------------------------- }

function TRegExpr.regNext(p: PRegExprChar): PRegExprChar;
// dig the "next" pointer out of a node
var
  offset: TRENextOff;
begin
  if p = @regDummy then
  begin
    Result := nil;
    Exit;
  end;
  offset := PRENextOff(AlignToPtr(p + REOpSz))^;
  if offset = 0 then
    Result := nil
  else
    Result := p + offset;
end;

function TRegExpr.regNextQuick(p: PRegExprChar): PRegExprChar; {$IFDEF FPC}inline;{$ENDIF}
var
  offset: TRENextOff;
begin
  // The inlined version is never called in the first pass.
  Assert(fSecondPass); // fSecondPass will also be true in MatchPrim.
  offset := PRENextOff(AlignToPtr(p + REOpSz))^;
  {$IFDEF WITH_REGEX_ASSERT}
  if offset = 0 then
    Result := nil
  else
  begin
  {$ENDIF}
    Result := p + offset;
  {$IFDEF WITH_REGEX_ASSERT}
    assert((Result >= programm) and (Result < programm + regCodeSize * SizeOf(REChar)));
  end;
  {$ENDIF}
end;

function TRegExpr.regLast(p: PRegExprChar): PRegExprChar;
var
  temp: PRegExprChar;
begin
  Result := p;
  if p = @regDummy then
    Exit;
  // Find last node.
  repeat
    temp := regNext(Result);
    if temp = nil then
      Break;
    Result := temp;
  until False;
end;

type
  TRegExprMatchPrimLocals =   record
    case TREOp of
      {$IFDEF ComplexBraces}
      OP_LOOPENTRY: (
        LoopInfo: TOpLoopInfo;
      );
      OP_LOOP: ( // and OP_LOOP_NG
        LoopInfoListPtr: POpLoopInfo;
      );
      {$ENDIF}
      OP_LOOKAHEAD, OP_LOOKBEHIND: (
        IsNegativeLook: Boolean;
        IsGreedy: REChar;
        LookAroundInfo: TRegExprLookAroundInfo;
        InpStart: PRegExprChar; // only OP_LOOKBEHIND
      );
      OP_LOOKAHEAD_END, OP_LOOKBEHIND_END: (
        LookAroundInfoPtr: PRegExprLookAroundInfo;
      );
      OP_SUBCALL: (
        savedCurrentSubCalled: Integer;
      );
  end;

function TRegExpr.MatchPrim(prog: PRegExprChar): Boolean;
// recursively matching routine
// Conceptually the strategy is simple:  check to see whether the current
// node matches, call self recursively to see whether the rest matches,
// and then act accordingly. In practice we make some effort to avoid
// recursion, in particular by going through "ordinary" nodes (that don't
// need to know whether the rest of the match failed) by a loop instead of
// by recursion.

var
  scan: PRegExprChar; // current node
  next: PRegExprChar; // next node
  Len: PtrInt;
  opnd, opGrpEnd: PRegExprChar;
  no: Integer;
  save: PRegExprChar;
  nextch: REChar;
  BracesMin, BracesMax: Integer;
  // we use integer instead of TREBracesArg to better support */+
  bound1, bound2: Boolean;
  Local: TRegExprMatchPrimLocals;
begin
  Result := False;
  {$IFDEF RegExpWithStackOverflowCheck_DecStack_Frame}
  if get_frame < StackLimit then begin
    error(reeLoopStackExceeded);
    exit;
  end;
  {$ENDIF}


  {
  // Alexey: not sure it's ok for long searches in big texts, so disabled
  if regNestedCalls > MaxRegexBackTracking then
    Exit;
  Inc(regNestedCalls);
  }

  scan := prog;
  while True do
  begin
    Assert(scan <> nil);
    next := regNextQuick(scan);

    case scan^ of
      OP_BOUND:
        begin
          bound1 := (regInput = fInputStart) or not IsWordChar((regInput - 1)^);
          bound2 := (regInput >= fInputEnd) or not IsWordChar(regInput^);
          if bound1 = bound2 then
            Exit;
        end;

      OP_NOTBOUND:
        begin
          bound1 := (regInput = fInputStart) or not IsWordChar((regInput - 1)^);
          bound2 := (regInput >= fInputEnd) or not IsWordChar(regInput^);
          if bound1 <> bound2 then
            Exit;
        end;

      OP_BOL:
        begin
          if regInput <> fInputStart then
            Exit;
        end;

      OP_CONTINUE_POS:
        begin
          if regInput <> fInputContinue then
            Exit;
        end;

      OP_RESET_MATCHPOS:
        begin
          save := GrpBounds[0].GrpStart[0];
          GrpBounds[0].GrpStart[0] := regInput;
          Result := MatchPrim(next);
          if not Result then
            GrpBounds[0].GrpStart[0] := save;
          exit;
        end;

      OP_EOL:
        begin
          // \z matches at the very end
          if regInput < fInputEnd then
            Exit;
        end;

      OP_EOL2:
        begin
          // \Z matches at the very and + before the final line-break (LF and CR LF)
          if regInput < fInputEnd then
          begin
            if (regInput = fInputEnd - 1) and (regInput^ = #10) then
              begin end
            else
            if (regInput = fInputEnd - 2) and (regInput^ = #13) and ((regInput + 1) ^ = #10) then
              begin end
            else
              Exit;
          end;
        end;

      OP_BOL_ML:
        if regInput > fInputStart then
        begin
          if ((regInput - 1) <= fInputStart) or
            not IsPairedBreak(regInput - 2) then
          begin
            // don't stop between paired separator
            if IsPairedBreak(regInput - 1) then
              Exit;
            if not IsCustomLineSeparator((regInput - 1)^) then
              Exit;
          end;
        end;

      OP_EOL_ML:
        if regInput < fInputEnd then
        begin
          if not IsPairedBreak(regInput) then
          begin
            // don't stop between paired separator
            if (regInput > fInputStart) and IsPairedBreak(regInput - 1) then
              Exit;
            if not IsCustomLineSeparator(regInput^) then
              Exit;
          end;
        end;

      OP_ANY:
        begin
          if regInput >= fInputCurrentEnd then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANY_ML:
        begin
          if (regInput >= fInputCurrentEnd) or
            IsPairedBreak(regInput) or
            IsCustomLineSeparator(regInput^)
          then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANYDIGIT:
        begin
          if (regInput >= fInputCurrentEnd) or not IsDigitChar(regInput^) then
            Exit;
          Inc(regInput);
        end;

      OP_NOTDIGIT:
        begin
          if (regInput >= fInputCurrentEnd) or IsDigitChar(regInput^) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANYLETTER:
        begin
          if (regInput >= fInputCurrentEnd) or not IsWordChar(regInput^) then
            Exit;
          Inc(regInput);
        end;

      OP_NOTLETTER:
        begin
          if (regInput >= fInputCurrentEnd) or IsWordChar(regInput^) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANYSPACE:
        begin
          if (regInput >= fInputCurrentEnd) or not IsSpaceChar(regInput^) then
            Exit;
          Inc(regInput);
        end;

      OP_NOTSPACE:
        begin
          if (regInput >= fInputCurrentEnd) or IsSpaceChar(regInput^) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANYVERTSEP:
        begin
          if (regInput >= fInputCurrentEnd) or not IsVertLineSeparator(regInput^) then
            Exit;
          Inc(regInput);
        end;

      OP_NOTVERTSEP:
        begin
          if (regInput >= fInputCurrentEnd) or IsVertLineSeparator(regInput^) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANYHORZSEP:
        begin
          if (regInput >= fInputCurrentEnd) or not IsHorzSeparator(regInput^) then
            Exit;
          Inc(regInput);
        end;

      OP_NOTHORZSEP:
        begin
          if (regInput >= fInputCurrentEnd) or IsHorzSeparator(regInput^) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_EXACTLY_CI:
        begin
          opnd := scan + REOpSz + RENextOffSz; // OPERAND
          Len := PLongInt(opnd)^;
          if (regInput + Len > fInputCurrentEnd) then
            Exit;
          Inc(opnd, RENumberSz);
          // Inline the first character, for speed.
          if (opnd^ <> regInput^) and (_LowerCase(opnd^) <> regInput^) then
            Exit;
          no := Len;
          save := regInput;
          while no > 1 do
          begin
            Inc(save);
            Inc(opnd);
            if (opnd^ <> save^) and (_LowerCase(opnd^) <> save^) then
              Exit;
            Dec(no);
          end;
          Inc(regInput, Len);
        end;

      OP_EXACTLY:
        begin
          opnd := scan + REOpSz + RENextOffSz; // OPERAND
          Len := PLongInt(opnd)^;
          if (regInput + Len > fInputCurrentEnd) then
            Exit;
          Inc(opnd, RENumberSz);
          // Inline the first character, for speed.
          if opnd^ <> regInput^ then
            Exit;
          no := Len;
          save := regInput;
          while no > 1 do
          begin
            Inc(save);
            Inc(opnd);
            if opnd^ <> save^ then
              Exit;
            Dec(no);
          end;
          Inc(regInput, Len);
        end;

      OP_BSUBEXP:
        begin
          no := PReGroupIndex((scan + REOpSz + RENextOffSz))^;
          no := GrpIndexes[no];
          if no < 0 then
            Exit;
          opnd := GrpBounds[regRecursion].GrpStart[no];
          if opnd = nil then
            Exit;
          opGrpEnd := GrpBounds[regRecursion].GrpEnd[no];
          if opGrpEnd = nil then
            Exit;
          save := regInput;
          while opnd < opGrpEnd do
          begin
            if (save >= fInputCurrentEnd) or (save^ <> opnd^) then
              Exit;
            Inc(save);
            Inc(opnd);
          end;
          regInput := save;
        end;

      OP_BSUBEXP_CI:
        begin
          no := PReGroupIndex((scan + REOpSz + RENextOffSz))^;
          no := GrpIndexes[no];
          if no < 0 then
            Exit;
          opnd := GrpBounds[regRecursion].GrpStart[no];
          if opnd = nil then
            Exit;
          opGrpEnd := GrpBounds[regRecursion].GrpEnd[no];
          if opGrpEnd = nil then
            Exit;
          save := regInput;
          while opnd < opGrpEnd do
          begin
            if (save >= fInputCurrentEnd) or
              ((save^ <> opnd^) and (save^ <> InvertCase(opnd^))) then
              Exit;
            Inc(save);
            Inc(opnd);
          end;
          regInput := save;
        end;

      OP_ANYOF:
        begin
          if (regInput >= fInputCurrentEnd) or
            not FindInCharClass(scan + REOpSz + RENextOffSz, regInput^, False) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANYBUT:
        begin
          if (regInput >= fInputCurrentEnd) or
            FindInCharClass(scan + REOpSz + RENextOffSz, regInput^, False) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANYOF_CI:
        begin
          if (regInput >= fInputCurrentEnd) or
            not FindInCharClass(scan + REOpSz + RENextOffSz, regInput^, True) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_ANYBUT_CI:
        begin
          if (regInput >= fInputCurrentEnd) or
            FindInCharClass(scan + REOpSz + RENextOffSz, regInput^, True) then
            Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_NOTHING:
        ;
      OP_COMMENT:
        ;
      OP_BACK:
        ;

      OP_OPEN, OP_OPEN_ATOMIC:
        begin
          no := PReGroupIndex((scan + REOpSz + RENextOffSz))^;
          save := GrpBounds[regRecursion].GrpStart[no];
          opnd := GrpBounds[regRecursion].GrpEnd[no]; // save2
          GrpBounds[regRecursion].GrpStart[no] := regInput;
          Result := MatchPrim(next);
          if GrpBacktrackingAsAtom[no] then
            IsBacktrackingGroupAsAtom := False;
          GrpBacktrackingAsAtom[no] := False;
          if not Result then begin
            GrpBounds[regRecursion].GrpStart[no] := save;
            GrpBounds[regRecursion].GrpEnd[no] := opnd;
          end;
          Exit;
        end;

      OP_CLOSE:
        begin
          no := PReGroupIndex((scan + REOpSz + RENextOffSz))^;
          // handle atomic group, mark it as "done"
          // (we are here because some OP_BRANCH is matched)
          GrpBounds[regRecursion].GrpEnd[no] := regInput;

          // if we are in OP_SUBCALL* call, it called OP_OPEN*, so we must return
          // in OP_CLOSE, without going to next opcode
          if CurrentSubCalled = no then
          begin
            Result := True;
            Exit;
          end;
        end;

      OP_CLOSE_ATOMIC:
        begin
          no := PReGroupIndex((scan + REOpSz + RENextOffSz))^;
          // handle atomic group, mark it as "done"
          // (we are here because some OP_BRANCH is matched)
          GrpBounds[regRecursion].GrpEnd[no] := regInput;

          Result := MatchPrim(next);
          if not Result then begin
            if not IsBacktrackingGroupAsAtom then begin
              GrpBacktrackingAsAtom[no] := True;
              IsBacktrackingGroupAsAtom := True;
            end;
          end;
          Exit;
        end;

      OP_LOOKAHEAD, OP_LOOKAHEAD_NEG:
        begin
          Local.IsNegativeLook := (scan^ = OP_LOOKAHEAD_NEG);

          Local.LookAroundInfo.InputPos := regInput;
          Local.LookAroundInfo.IsNegative := Local.IsNegativeLook;
          Local.LookAroundInfo.HasMatchedToEnd := False;
          Local.LookAroundInfo.IsBackTracking := False;
          Local.LookAroundInfo.OuterInfo := LookAroundInfoList;
          Local.LookAroundInfo.savedInputCurrentEnd := fInputCurrentEnd;
          LookAroundInfoList := @Local.LookAroundInfo;
          fInputCurrentEnd := fInputEnd;

          scan := PRegExprChar(AlignToPtr(scan + 1)) + RENextOffSz;
          Result := MatchPrim(scan);

          if Local.LookAroundInfo.IsBackTracking then
            IsBacktrackingGroupAsAtom := False;
          LookAroundInfoList := Local.LookAroundInfo.OuterInfo;
          fInputCurrentEnd := Local.LookAroundInfo.savedInputCurrentEnd;

          opnd := PRegExprChar(AlignToPtr(next + 1)) + RENextOffSz; // Successor of OP_LOOKAHEAD_END;
          if Local.IsNegativeLook then begin
            Result := (opnd^ = OP_LOOKAROUND_OPTIONAL);
            if not Result then
              Result := (not Local.LookAroundInfo.HasMatchedToEnd);
            if Result then begin
              next := regNextQuick(next);                             // Next-Pointer of OP_LOOKAHEAD_END
              if (next^ = OP_LOOKAROUND_OPTIONAL) then
                next := PRegExprChar(AlignToPtr(next + 1)) + RENextOffSz;
              regInput := Local.LookAroundInfo.InputPos;
              Result := False;
              scan := next;
              continue;
            end;
          end
          else
          if (opnd^ = OP_LOOKAROUND_OPTIONAL) then begin
            if not Local.LookAroundInfo.HasMatchedToEnd then begin
              next := regNextQuick(next);                             // Next-Pointer of OP_LOOKAHEAD_END
              if (next^ = OP_LOOKAROUND_OPTIONAL) then
                next := PRegExprChar(AlignToPtr(next + 1)) + RENextOffSz;
              regInput := Local.LookAroundInfo.InputPos;
              Result := False;
              scan := next;
              continue;
            end;
          end;

          if not Result then
            regInput := Local.LookAroundInfo.InputPos;

          Exit;
        end;

      OP_LOOKBEHIND, OP_LOOKBEHIND_NEG:
        begin
          Local.IsNegativeLook := (scan^ = OP_LOOKBEHIND_NEG);
          scan := PRegExprChar(AlignToPtr(scan + 1)) + RENextOffSz;
          Local.IsGreedy := PReOpLookBehindOptions(scan)^.IsGreedy;

          Local.LookAroundInfo.InputPos := regInput;
          Local.LookAroundInfo.IsNegative := Local.IsNegativeLook;
          Local.LookAroundInfo.HasMatchedToEnd := False;
          Local.LookAroundInfo.IsBackTracking := False;
          Local.LookAroundInfo.OuterInfo := LookAroundInfoList;
          Local.LookAroundInfo.savedInputCurrentEnd := fInputCurrentEnd;
          LookAroundInfoList := @Local.LookAroundInfo;
          fInputCurrentEnd := regInput;

          Result := regInput - fInputStart >= PReOpLookBehindOptions(scan)^.MatchLenMin;
          if Result then begin
            if Local.IsGreedy = OPT_LOOKBEHIND_FIXED then begin
              regInput := regInput - PReOpLookBehindOptions(scan)^.MatchLenMin;
              inc(scan, ReOpLookBehindOptionsSz);
              Result := MatchPrim(scan)
            end
            else
            if Local.IsGreedy = OPT_LOOKBEHIND_NON_GREEDY then begin
              Local.InpStart := regInput - PReOpLookBehindOptions(scan)^.MatchLenMin;
              if regInput - fInputStart >= PReOpLookBehindOptions(scan)^.MatchLenMax then
                save := regInput - PReOpLookBehindOptions(scan)^.MatchLenMax
              else
                save := fInputStart;
              inc(scan, ReOpLookBehindOptionsSz);
              repeat
                regInput := Local.InpStart;
                dec(Local.InpStart);
                Result := MatchPrim(scan);
              until Local.LookAroundInfo.HasMatchedToEnd or (Local.InpStart < save);
            end
            else begin
              if regInput - fInputStart >= PReOpLookBehindOptions(scan)^.MatchLenMax then
                Local.InpStart := regInput - PReOpLookBehindOptions(scan)^.MatchLenMax
              else
                Local.InpStart := fInputStart;
              save := Local.LookAroundInfo.InputPos - PReOpLookBehindOptions(scan)^.MatchLenMin;
              inc(scan, ReOpLookBehindOptionsSz);
              repeat
                regInput := Local.InpStart;
                inc(Local.InpStart);
                Result := MatchPrim(scan);
              until Local.LookAroundInfo.HasMatchedToEnd or (Local.InpStart > save);
            end;
          end;

          if Local.LookAroundInfo.IsBackTracking then
            IsBacktrackingGroupAsAtom := False;
          LookAroundInfoList := Local.LookAroundInfo.OuterInfo;
          fInputCurrentEnd := Local.LookAroundInfo.savedInputCurrentEnd;

          opnd := PRegExprChar(AlignToPtr(next + 1)) + RENextOffSz; // Successor of OP_LOOKAHEAD_END;
          if Local.IsNegativeLook then begin
            Result := (opnd^ = OP_LOOKAROUND_OPTIONAL);
            if not Result then
              Result := not Local.LookAroundInfo.HasMatchedToEnd;
            if Result then begin
              next := regNextQuick(next);                             // Next-Pointer of OP_LOOKAHEAD_END
              if (next^ = OP_LOOKAROUND_OPTIONAL) then
                next := PRegExprChar(AlignToPtr(next + 1)) + RENextOffSz;
              regInput := Local.LookAroundInfo.InputPos;
              Result := False;
              scan := next;
              continue;
            end;
          end
          else
          if (opnd^ = OP_LOOKAROUND_OPTIONAL) then begin
            if not Local.LookAroundInfo.HasMatchedToEnd then begin
              next := regNextQuick(next);                             // Next-Pointer of OP_LOOKAHEAD_END
              if (next^ = OP_LOOKAROUND_OPTIONAL) then
                next := PRegExprChar(AlignToPtr(next + 1)) + RENextOffSz;
              regInput := Local.LookAroundInfo.InputPos;
              Result := False;
              scan := next;
              continue;
            end;
          end;

          if not Result then
            regInput := Local.LookAroundInfo.InputPos;
          Exit;
        end;

      OP_LOOKAHEAD_END:
        begin
          if LookAroundInfoList = nil then
            Exit;
          Local.LookAroundInfoPtr := LookAroundInfoList;
          Local.LookAroundInfoPtr.HasMatchedToEnd := True;

          if not Local.LookAroundInfoPtr^.IsNegative then begin
            fInputCurrentEnd := Local.LookAroundInfoPtr^.savedInputCurrentEnd;
            regInput := Local.LookAroundInfoPtr^.InputPos;
            LookAroundInfoList := Local.LookAroundInfoPtr^.OuterInfo;

            if (next^ = OP_LOOKAROUND_OPTIONAL) then
              next := PRegExprChar(AlignToPtr(next + 1)) + RENextOffSz;
            Result := MatchPrim(next);
            LookAroundInfoList := Local.LookAroundInfoPtr;
          end;

          if (not Result) and not IsBacktrackingGroupAsAtom then begin
            IsBacktrackingGroupAsAtom := True;
            Local.LookAroundInfoPtr.IsBackTracking := True;
          end;
          Exit;
        end;

      OP_LOOKBEHIND_END:
        begin
          if LookAroundInfoList = nil then
            Exit;

          Local.LookAroundInfoPtr := LookAroundInfoList;
          if not (Local.LookAroundInfoPtr^.InputPos = regInput) then
            Exit;

          Local.LookAroundInfoPtr.HasMatchedToEnd := True;

          if not Local.LookAroundInfoPtr^.IsNegative then begin
            regInput := Local.LookAroundInfoPtr^.InputPos;
            fInputCurrentEnd := Local.LookAroundInfoPtr^.savedInputCurrentEnd;
            LookAroundInfoList := Local.LookAroundInfoPtr^.OuterInfo;

            if (next^ = OP_LOOKAROUND_OPTIONAL) then
              next := PRegExprChar(AlignToPtr(next + 1)) + RENextOffSz;
            Result := MatchPrim(next);
            LookAroundInfoList := Local.LookAroundInfoPtr;
          end;

          if (not Result) and not IsBacktrackingGroupAsAtom then begin
            IsBacktrackingGroupAsAtom := True;
            Local.LookAroundInfoPtr.IsBackTracking := True;
          end;
          Exit;
        end;

      OP_BRANCH:
        begin
          repeat
            save := regInput;
            Result := MatchPrim(scan + REOpSz + RENextOffSz + REBranchArgSz);
            if Result then
              Exit;
            // if branch worked until OP_CLOSE, and marked atomic group as "done", then exit
            regInput := save;
            if IsBacktrackingGroupAsAtom then
              Exit;
            scan := next;
            Assert(scan <> nil);
            next := regNextQuick(scan);
            if  (next^ <> OP_BRANCH) then
              break;
          until  False;
          next := scan + REOpSz + RENextOffSz + REBranchArgSz; // Avoid recursion
        end;

      OP_GBRANCH, OP_GBRANCH_EX, OP_GBRANCH_EX_CI:
        begin
          Assert((next^ = OP_BRANCH) or (next^ = OP_GBRANCH) or (next^ = OP_GBRANCH_EX) or (next^ = OP_GBRANCH_EX_CI));
          repeat
            save := regInput;
            case scan^ of
              OP_GBRANCH, OP_BRANCH:
                Result := MatchPrim(scan + REOpSz + RENextOffSz + REBranchArgSz);
              OP_GBRANCH_EX:
                if (regInput^ = (scan + REOpSz + RENextOffSz)^) then
                  Result := MatchPrim(scan + REOpSz + RENextOffSz + REBranchArgSz);
              OP_GBRANCH_EX_CI:
                if (regInput^ = (scan + REOpSz + RENextOffSz)^) or
                   (regInput^ = (scan + REOpSz + RENextOffSz + 1)^)
                then
                  Result := MatchPrim(scan + REOpSz + RENextOffSz + REBranchArgSz);
            end;
            if Result then
              Exit;
            // if branch worked until OP_CLOSE, and marked atomic group as "done", then exit
            regInput := save;
            if IsBacktrackingGroupAsAtom then
              Exit;
            scan := next;
            Assert(scan <> nil);
            next := regNextQuick(scan);
            if  (next^ <> OP_BRANCH) and (next^ <> OP_GBRANCH) and (next^ <> OP_GBRANCH_EX) and (next^ <> OP_GBRANCH_EX_CI) then
              break;
          until  False;
          case scan^ of
            OP_GBRANCH_EX:
              if (regInput^ <> (scan + REOpSz + RENextOffSz)^) then
                exit;
            OP_GBRANCH_EX_CI:
              if (regInput^ <> (scan + REOpSz + RENextOffSz)^) and
                 (regInput^ <> (scan + REOpSz + RENextOffSz + 1)^)
              then
                exit;
          end;
          next := scan + REOpSz + RENextOffSz + REBranchArgSz; // Avoid recursion
        end;

      {$IFDEF ComplexBraces}
      OP_LOOPENTRY:
        begin
          Local.LoopInfo.Count := 0;
          Local.LoopInfo.BackTrackingAsAtom := False;
          Local.LoopInfo.CurrentRegInput := nil;
          Local.LoopInfo.OuterLoop := CurrentLoopInfoListPtr;
          CurrentLoopInfoListPtr := @Local.LoopInfo;
          save := regInput;
          Result := MatchPrim(next); // execute loop
          CurrentLoopInfoListPtr := Local.LoopInfo.OuterLoop;
          if Local.LoopInfo.BackTrackingAsAtom then
            IsBacktrackingGroupAsAtom := False;
          if not Result then
            regInput := save;
          Exit;
        end;

      OP_LOOP, OP_LOOP_NG, OP_LOOP_POSS:
        begin
          if CurrentLoopInfoListPtr = nil then begin
            Error(reeLoopWithoutEntry);
            Exit;
          end;
          opnd := scan + PRENextOff(AlignToPtr(scan + REOpSz + RENextOffSz + 2 * REBracesArgSz))^;
          BracesMin := PREBracesArg(AlignToInt(scan + REOpSz + RENextOffSz))^;
          BracesMax := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz + REBracesArgSz))^;
          save := regInput;
          Local.LoopInfoListPtr := CurrentLoopInfoListPtr;
          if Local.LoopInfoListPtr^.Count >= BracesMin then
          begin // Min alredy matched - we can work
            Result := (BracesMax = MaxBracesArg) and // * or +
                      (Local.LoopInfoListPtr^.CurrentRegInput = regInput);
            if Result then begin
              CurrentLoopInfoListPtr := Local.LoopInfoListPtr^.OuterLoop;
              Result := MatchPrim(next);
              CurrentLoopInfoListPtr := Local.LoopInfoListPtr;
              if not Result then
                regInput := save;
              exit;
            end;

            Local.LoopInfoListPtr^.CurrentRegInput := regInput;
            if not (scan^ = OP_LOOP_NG) then
            begin
              // greedy way - first try to max deep of greed ;)
              if Local.LoopInfoListPtr^.Count < BracesMax then
              begin
                Inc(Local.LoopInfoListPtr^.Count);
                Result := MatchPrim(opnd);
                if Result then
                  Exit;
                if IsBacktrackingGroupAsAtom then
                  Exit;
                Dec(Local.LoopInfoListPtr^.Count);
                regInput := save;
              end;
              CurrentLoopInfoListPtr := Local.LoopInfoListPtr^.OuterLoop;
              Result := MatchPrim(next);
              CurrentLoopInfoListPtr := Local.LoopInfoListPtr;

              if IsBacktrackingGroupAsAtom then
                Exit;
              if (scan^ = OP_LOOP_POSS) and (not Result) then begin
                Local.LoopInfoListPtr^.BackTrackingAsAtom := True;
                IsBacktrackingGroupAsAtom := True;
                exit;
              end;
              if not Result then
                regInput := save;
              Exit;
            end
            else
            begin
              // non-greedy - try just now
              CurrentLoopInfoListPtr := Local.LoopInfoListPtr^.OuterLoop;
              Result := MatchPrim(next);
              CurrentLoopInfoListPtr := Local.LoopInfoListPtr;
              if Result then
                Exit;
              if IsBacktrackingGroupAsAtom then
                Exit;
              regInput := save; // failed - move next and try again
              if Local.LoopInfoListPtr^.Count < BracesMax then
              begin
                Inc(Local.LoopInfoListPtr^.Count);
                Result := MatchPrim(opnd);
                if Result then
                  Exit;
                if IsBacktrackingGroupAsAtom then
                  Exit;
                Dec(Local.LoopInfoListPtr^.Count);
                regInput := save;
              end;
              Exit;
            end
          end
          else
          begin // first match a min_cnt times
            Inc(Local.LoopInfoListPtr^.Count);
            Local.LoopInfoListPtr^.CurrentRegInput := regInput;
            Result := MatchPrim(opnd);
            if Result then
              Exit;
            if IsBacktrackingGroupAsAtom then
              Exit;
            Dec(Local.LoopInfoListPtr^.Count);
            regInput := save;
            Exit;
          end;
        end;
      {$ENDIF}

      OP_STAR, OP_PLUS, OP_BRACES, OP_STAR_NG, OP_PLUS_NG, OP_BRACES_NG:
        begin
          // Lookahead to avoid useless match attempts when we know
          // what character comes next.
          nextch := #0;
          if next^ = OP_EXACTLY then
            nextch := (next + REOpSz + RENextOffSz + RENumberSz)^;
          BracesMax := MaxInt; // infinite loop for * and +
          if (scan^ = OP_STAR) or (scan^ = OP_STAR_NG) then
            BracesMin := 0 // star
          else if (scan^ = OP_PLUS) or (scan^ = OP_PLUS_NG) then
            BracesMin := 1 // plus
          else
          begin // braces
            BracesMin := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz))^;
            BracesMax := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz + REBracesArgSz))^;
          end;
          save := regInput;
          opnd := scan + REOpSz + RENextOffSz;
          if (scan^ = OP_BRACES) or (scan^ = OP_BRACES_NG) then
            Inc(opnd, 2 * REBracesArgSz);

          if (scan^ = OP_PLUS_NG) or (scan^ = OP_STAR_NG) or (scan^ = OP_BRACES_NG) then
          begin
            // non-greedy mode
            BracesMax := FindRepeated(opnd, BracesMax);
            // don't repeat more than BracesMax
            // Now we know real Max limit to move forward (for recursion 'back up')
            // In some cases it can be faster to check only Min positions first,
            // but after that we have to check every position separtely instead
            // of fast scannig in loop.
            no := BracesMin;
            while no <= BracesMax do
            begin
              regInput := save + no;
              // If it could work, try it.
              if (nextch = #0) or (regInput^ = nextch) then
              begin
                if MatchPrim(next) then
                begin
                  Result := True;
                  Exit;
                end;
                if IsBacktrackingGroupAsAtom then
                  Exit;
              end;
              Inc(no); // Couldn't or didn't - move forward.
            end; { of while }
            Exit;
          end
          else
          begin // greedy mode
            no := FindRepeated(opnd, BracesMax); // don't repeat more than max_cnt
            while no >= BracesMin do
            begin
              // If it could work, try it.
              if (nextch = #0) or (regInput^ = nextch) then
              begin
                if MatchPrim(next) then
                begin
                  Result := True;
                  Exit;
                end;
                if IsBacktrackingGroupAsAtom then
                  Exit;
              end;
              Dec(no); // Couldn't or didn't - back up.
              regInput := save + no;
            end; { of while }
            Exit;
          end;
        end;

      OP_STAR_POSS, OP_PLUS_POSS, OP_BRACES_POSS:
        begin
          // Lookahead to avoid useless match attempts when we know
          // what character comes next.
          nextch := #0;
          if next^ = OP_EXACTLY then
            nextch := (next + REOpSz + RENextOffSz + RENumberSz)^;
          opnd := scan + REOpSz + RENextOffSz;
          case scan^ of
            OP_STAR_POSS:
              begin
                BracesMin := 0;
                BracesMax := MaxInt;
              end;
            OP_PLUS_POSS:
              begin
                BracesMin := 1;
                BracesMax := MaxInt;
              end;
            else
              begin // braces
                BracesMin := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz))^;
                BracesMax := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz + REBracesArgSz))^;
                Inc(opnd, 2 * REBracesArgSz);
              end;
          end;
          no := FindRepeated(opnd, BracesMax);
          if no >= BracesMin then
            if (nextch = #0) or (regInput^ = nextch) then begin
              scan := next;
              continue;
            end;
          Exit;
        end;

      OP_EEND:
        begin
          Result := True; // Success!
          Exit;
        end;

      {$IFDEF FastUnicodeData}
      OP_ANYCATEGORY:
        begin
          if (regInput >= fInputCurrentEnd) then Exit;
          if not MatchOneCharCategory(scan + REOpSz + RENextOffSz, regInput) then Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;

      OP_NOTCATEGORY:
        begin
          if (regInput >= fInputCurrentEnd) then Exit;
          if MatchOneCharCategory(scan + REOpSz + RENextOffSz, regInput) then Exit;
          {$IFDEF UNICODEEX}
          IncUnicode(regInput);
          {$ELSE}
          Inc(regInput);
          {$ENDIF}
        end;
      {$ENDIF}

      OP_RECUR:
        begin
          // call opcode start
          if regRecursion < RegexMaxRecursion then
          begin
            Inc(regRecursion);
            FillChar(GrpBounds[regRecursion].GrpStart[0], SizeOf(GrpBounds[regRecursion].GrpStart[0])*regNumBrackets, 0);
            bound1 := MatchPrim(regCodeWork);
            Dec(regRecursion);
          end
          else
            bound1 := False;
          if not bound1 then Exit;
        end;

      OP_SUBCALL:
        begin
          // call subroutine
          no := PReGroupIndex((scan + REOpSz + RENextOffSz))^;
          no := GrpIndexes[no];
          if no < 0 then Exit;
          save := GrpOpCodes[no];
          if save = nil then Exit;
          if regRecursion < RegexMaxRecursion then
          begin
            Local.savedCurrentSubCalled := CurrentSubCalled;
            CurrentSubCalled := no;
            Inc(regRecursion);
            FillChar(GrpBounds[regRecursion].GrpStart[0], SizeOf(GrpBounds[regRecursion].GrpStart[0])*regNumBrackets, 0);
            bound1 := MatchPrim(save);
            Dec(regRecursion);
            CurrentSubCalled := Local.savedCurrentSubCalled;
          end
          else
            bound1 := False;
          if not bound1 then Exit;
        end;

      OP_ANYLINEBREAK:
        begin
          if (regInput >= fInputCurrentEnd) or not IsAnyLineBreak(regInput^) then
            Exit;
          nextch := regInput^;
          Inc(regInput);
          if (nextch = #13) and (regInput < fInputCurrentEnd) and (regInput^ = #10) then
            Inc(regInput);
        end;

    {$IFDEF WITH_REGEX_ASSERT}
    else
      Error(reeMatchPrimMemoryCorruption);
      Exit;
    {$ENDIF}
    end; { of case scan^ }
    scan := next;
  end; { of while scan <> nil }
end; { of function TRegExpr.MatchPrim
  -------------------------------------------------------------- }

function TRegExpr.Exec(const AInputString: RegExprString): Boolean;
begin
  InputString := AInputString;
  Result := ExecPrim(1, False, False, 0);
end; { of function TRegExpr.Exec
  -------------------------------------------------------------- }

{$IFDEF OverMeth}
function TRegExpr.Exec: Boolean;
var
  SlowChecks: Boolean;
begin
  SlowChecks := fInputEnd - fInputStart < fSlowChecksSizeMax;
  Result := ExecPrim(1, SlowChecks, False, 0);
end; { of function TRegExpr.Exec
  -------------------------------------------------------------- }

function TRegExpr.Exec(AOffset: Integer): Boolean;
begin
  Result := ExecPrim(AOffset, False, False, 0);
end; { of function TRegExpr.Exec
  -------------------------------------------------------------- }
{$ENDIF}

function TRegExpr.ExecPos(AOffset: Integer {$IFDEF DefParam} = 1{$ENDIF}): Boolean;
begin
  Result := ExecPrim(AOffset, False, False, 0);
end; { of function TRegExpr.ExecPos
  -------------------------------------------------------------- }

{$IFDEF OverMeth}
function TRegExpr.ExecPos(AOffset: Integer; ATryOnce, ABackward: Boolean): Boolean;
begin
  if ATryOnce then
    Result := ExecPrim(AOffset, False, ABackward, AOffset + 1)
  else
    Result := ExecPrim(AOffset, False, ABackward, 0);
end;

function TRegExpr.ExecPos(AOffset, ATryMatchOnlyStartingBefore: Integer): Boolean;
begin
  Result := ExecPrim(AOffset, False, False, ATryMatchOnlyStartingBefore);
end;
{$ENDIF}

function TRegExpr.MatchAtOnePos(APos: PRegExprChar): Boolean;
begin
  regInput := APos;
  //regNestedCalls := 0;
  fInputCurrentEnd := fInputEnd;
  GrpBounds[0].GrpStart[0] := APos;
  Result := MatchPrim(regCodeWork);
  if Result then
    Result := regInput >= GrpBounds[0].GrpStart[0];
  if Result then
    GrpBounds[0].GrpEnd[0] := regInput
  else
    GrpBounds[0].GrpStart[0] := nil;
end;

procedure TRegExpr.ClearMatches;
begin
  if FMatchesCleared then
    exit;
  FMatchesCleared := True;
  if Length(GrpBounds[0].GrpStart) > 0 then
    FillChar(GrpBounds[0].GrpStart[0], SizeOf(GrpBounds[0].GrpStart[0])*regNumBrackets, 0);
end;

procedure TRegExpr.ClearInternalExecData;
begin
  fLastError := reeOk;
  FillChar(GrpBacktrackingAsAtom[0], SizeOf(GrpBacktrackingAsAtom[0])*regNumBrackets, 0);
  IsBacktrackingGroupAsAtom := False;
  {$IFDEF ComplexBraces}
  // no loops started
  CurrentLoopInfoListPtr := nil;
  {$ENDIF}
  LookAroundInfoList := nil;
  CurrentSubCalled := -1;
  regRecursion := 0;
end;

procedure TRegExpr.InitInternalGroupData;
var
  BndLen, i: Integer;
begin
  BndLen := GroupDataArraySize(regNumBrackets, Length(GrpBounds[0].GrpStart));
  for i := low(GrpBounds) to high(GrpBounds) do begin
    SetLength(GrpBounds[i].GrpStart, BndLen);
    SetLength(GrpBounds[i].GrpEnd, BndLen);
  end;

  SetLength(GrpIndexes, GroupDataArraySize(regNumBrackets, Length(GrpIndexes)));
  for i := 1 to regNumBrackets - 1 do
    GrpIndexes[i] := -1;
  GrpIndexes[0] := 0;

  SetLength(GrpOpCodes, GroupDataArraySize(regNumBrackets, Length(GrpOpCodes)));
  SetLength(GrpBacktrackingAsAtom, GroupDataArraySize(regNumBrackets, Length(GrpBacktrackingAsAtom)));

  GrpOpCodes[0] := nil;
end;

function TRegExpr.ExecPrim(AOffset: Integer; ASlowChecks, ABackward: Boolean;
  ATryMatchOnlyStartingBefore: Integer): Boolean;
begin
  if fRaiseForRuntimeError then begin
    Result := ExecPrimProtected(AOffset, ASlowChecks, ABackward, ATryMatchOnlyStartingBefore);
  end
  else begin
    try
      Result := ExecPrimProtected(AOffset, ASlowChecks, ABackward, ATryMatchOnlyStartingBefore);
    except
      on E: EStackOverflow do begin
        Result := False;
        fLastError := reeLoopStackExceeded;
        Error(reeLoopStackExceeded);
      end;
      on E: ERegExpr do begin
        Result := False;
        raise;
      end;
      else begin
        fLastError := reeUnknown;
        Error(reeUnknown);
      end;
    end;
  end;
end;

function TRegExpr.ExecPrimProtected(AOffset: Integer; ASlowChecks,
  ABackward: Boolean; ATryMatchOnlyStartingBefore: Integer): Boolean;
var
  Ptr, SearchEnd: PRegExprChar;
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

  if fInputEnd = fInputStart then
  begin
    // Empty string can match e.g. '^$'
    if regMustLen > 0 then
      Exit;
  end;

  // Check that the start position is not negative
  if AOffset < 1 then
  begin
    Error(reeOffsetMustBePositive);
    Exit;
  end;
  if (ATryMatchOnlyStartingBefore > 0) and (AOffset >= ATryMatchOnlyStartingBefore) then
    Exit;

  // Check that the start position is not longer than the line
  if (AOffset - 1) > (fInputEnd - fInputStart) then
    Exit;

  ClearInternalExecData;

  Ptr := fInputStart + AOffset - 1;
  fInputContinue := Ptr;

  // If there is a "must appear" string, look for it.
  if ASlowChecks then
    if regMustString <> '' then
      if StrLPos(fInputStart, PRegExprChar(regMustString), fInputEnd - fInputStart, length(regMustString)) = nil then
        exit;

  {$IFDEF RegExpWithStackOverflowCheck_DecStack_Frame}
  StackLimit := StackBottom;
  if StackLimit <> nil then
    StackLimit := StackLimit + 36000; // Add for any calls within the current MatchPrim // FPC has "STACK_MARGIN = 16384;", but we need to call Error, ..., raise
  {$ENDIF}

  FMatchesCleared := False;
  // ATryOnce or anchored match (it needs to be tried only once).
  if (ATryMatchOnlyStartingBefore = AOffset + 1) or (regAnchored in [raBOL, raOnlyOnce, raContinue]) then
  begin
    case regAnchored of
      raBOL: if AOffset > 1 then Exit; // can't match the BOL
      raEOL: Ptr := fInputEnd;
    end;
    {$IFDEF UseFirstCharSet}
    if (Ptr < fInputEnd)
    {$IFDEF UnicodeRE} and (Ord(Ptr^) <= $FF)  {$ENDIF}
    then
      if not FirstCharArray[Byte(Ptr^)] then
        Exit;
    {$ENDIF}

    Result := MatchAtOnePos(Ptr);
    Exit;
  end;

  // Messy cases: unanchored match.
  if ABackward then begin
    Inc(Ptr, 2);
    repeat
      Dec(Ptr);
      if Ptr < fInputStart then
        Exit;

      {$IFDEF UseFirstCharSet}
      {$IFDEF UnicodeRE}
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
  end
  else begin
    Dec(Ptr);
    SearchEnd := fInputEnd - FMinMatchLen;
    if (ATryMatchOnlyStartingBefore > 0) and (fInputStart + ATryMatchOnlyStartingBefore < SearchEnd) then
      SearchEnd := fInputStart + ATryMatchOnlyStartingBefore - 2;
    repeat
      Inc(Ptr);
      if Ptr > SearchEnd then
        Exit;

      {$IFDEF UseFirstCharSet}
      {$IFDEF UnicodeRE}
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
  end;
end; { of function TRegExpr.ExecPrim
  -------------------------------------------------------------- }

function TRegExpr.ExecNext(ABackward: Boolean {$IFDEF DefParam} = False{$ENDIF}): Boolean;
var
  PtrBegin, PtrEnd: PRegExprChar;
  Offset: PtrInt;
begin
  PtrBegin := GrpBounds[0].GrpStart[0];
  PtrEnd := GrpBounds[0].GrpEnd[0];
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

  Result := ExecPrim(Offset, False, ABackward, 0);
end; { of function TRegExpr.ExecNext
  -------------------------------------------------------------- }

procedure TRegExpr.SetInputString(const AInputString: RegExprString);
begin
  ClearMatches;

  fInputString := AInputString;
  //UniqueString(fInputString);

  fInputStart := PRegExprChar(fInputString);
  fInputEnd := fInputStart + Length(fInputString);
  fInputContinue := fInputStart;
end;

procedure TRegExpr.SetInputRange(AStart, AEnd, AContinueAnchor: PRegExprChar);
begin
  ClearMatches;
  fInputString := '';
  fInputStart := AStart;
  fInputEnd := AEnd;
  fInputContinue := AContinueAnchor;
end;

{$IFDEF UseLineSep}
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
{$ENDIF}

procedure TRegExpr.SetUsePairedBreak(AValue: Boolean);
begin
  if AValue <> fUsePairedBreak then
  begin
    fUsePairedBreak := AValue;
    InvalidateProgramm;
  end;
end;

function TRegExpr.Substitute(const ATemplate: RegExprString): RegExprString;
// perform substitutions after a regexp match
var
  TemplateBeg, TemplateEnd: PRegExprChar;

  function ParseVarName(var APtr: PRegExprChar): Integer;
  // extract name of variable: $1 or ${1} or ${name}
  // from APtr^, uses TemplateEnd
  var
    p: PRegExprChar;
    Delimited: Boolean;
    GrpName: RegExprString;
  begin
    Result := 0;
    GrpName := '';
    p := APtr;
    Delimited := (p < TemplateEnd) and (p^ = '{');
    if Delimited then
      Inc(p); // skip left curly brace
    if (p < TemplateEnd) and (p^ = '&') then
      Inc(p) // this is '$&' or '${&}'
    else
    begin
      if IsDigitChar(p^) then
      begin
        while (p < TemplateEnd) and IsDigitChar(p^) do
        begin
          Result := Result * 10 + (Ord(p^) - Ord('0'));
          Inc(p);
        end
      end
      else
      if Delimited then
      begin
        FindGroupName(p, TemplateEnd, '}', GrpName);
        Result := GrpNames.MatchIndexFromName(GrpName);
        Inc(p, Length(GrpName));
      end;
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

  procedure FindSubstGroupIndex(var p: PRegExprChar; var Idx: Integer; var NumberFound: Boolean);
  begin
    Idx := ParseVarName(p);
    NumberFound := Idx >= 0;
    if NumberFound and (Idx <= High(GrpIndexes)) then
      Idx := GrpIndexes[Idx]
    else
      Idx := -1;
  end;

type
  TSubstMode = (smodeNormal, smodeOneUpper, smodeOneLower, smodeAllUpper, smodeAllLower);
var
  Mode: TSubstMode;
  p, p0, p1, ResultPtr: PRegExprChar;
  ResultLen, n: Integer;
  Ch, QuotedChar: REChar;
  GroupFound: Boolean;
begin
  // Check programm and input string
  if not IsProgrammOk then
    Exit;
  // Note: don't check for empty fInputString, it's valid case,
  // e.g. user needs to replace regex "\b" to "_", it's zero match length
  if ATemplate = '' then
  begin
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
    n := -1;
    GroupFound := False;
    if Ch = SubstituteGroupChar then
      FindSubstGroupIndex(p, n, GroupFound);
    if GroupFound then
    begin
      if (n >= 0) and (GrpBounds[0].GrpStart[n] <> nil) then
        Inc(ResultLen, GrpBounds[0].GrpEnd[n] - GrpBounds[0].GrpStart[n]);
    end
    else
    begin
      if (Ch = EscChar) and (p < TemplateEnd) then
      begin // quoted or special char followed
        Ch := p^;
        Inc(p);
        case Ch of
          'n':
            Inc(ResultLen, Length(fReplaceLineEnd));
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
  ResultPtr := PRegExprChar(Result);
  p := TemplateBeg;
  Mode := smodeNormal;
  while p < TemplateEnd do
  begin
    Ch := p^;
    p0 := p;
    Inc(p);
    p1 := p;
    n := -1;
    GroupFound := False;
    if Ch = SubstituteGroupChar then
      FindSubstGroupIndex(p, n, GroupFound);
    if GroupFound then
    begin
      if n >= 0 then
      begin
        p0 := GrpBounds[0].GrpStart[n];
        if p0 = nil then
          p1 := nil
        else
          p1 := GrpBounds[0].GrpEnd[n];
      end
      else
        p1 := p0;
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
              p0 := PRegExprChar(fReplaceLineEnd);
              p1 := p0 + Length(fReplaceLineEnd);
            end;
          'x', 't', 'r', 'f', 'a', 'e':
            begin
              p := p - 1;
              // UnquoteChar expects the escaped char under the pointer
              QuotedChar := UnQuoteChar(p, TemplateEnd);
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
          Inc(p0);
          Inc(p1);
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
  AUseSubstitution: Boolean{$IFDEF DefParam} = False{$ENDIF}): RegExprString;
var
  PrevPos: PtrInt;
begin
  Result := '';
  PrevPos := 1;
  if Exec(AInputStr) then
    repeat
      Result := Result + System.Copy(AInputStr, PrevPos, MatchPos[0] - PrevPos);
      if AUseSubstitution
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
  min_cnt: Integer;
  {$IFDEF UseLineSep}
  i: Integer;
  {$ENDIF}
  TempSet, TmpFirstCharSet: TRegExprCharset;
begin
  TempSet := [];
  scan := prog;
  while scan <> nil do
  begin
    Next := regNextQuick(scan);
    Oper := PREOp(scan)^;
    case Oper of
      OP_BSUBEXP,
      OP_BSUBEXP_CI:
        begin
          // we cannot optimize r.e. if it starts with back reference
          FirstCharSet := RegExprAllSet;
          Exit;
        end;

      OP_BOL,
      OP_BOL_ML,
      OP_CONTINUE_POS,
      OP_RESET_MATCHPOS:
        ; // Exit;

      OP_EOL,
      OP_EOL2,
      OP_EOL_ML:
        begin
          Include(FirstCharSet, 0);
          if ModifierM then
          begin
            {$IFDEF UseLineSep}
            for i := 1 to Length(LineSeparators) do
              Include(FirstCharSet, Byte(LineSeparators[i]));
            {$ELSE}
            FirstCharSet := FirstCharSet + RegExprLineSeparatorsSet;
            {$ENDIF}
          end;
          Exit;
        end;

      OP_BOUND,
      OP_NOTBOUND:
        ;

      OP_ANY,
      OP_ANY_ML:
        begin // we can better define ANYML
          FirstCharSet := RegExprAllSet;
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

      OP_EXACTLY_CI:
        begin
          ch := (scan + REOpSz + RENextOffSz + RENumberSz)^;
          {$IFDEF UnicodeRE}
          if Ord(ch) <= $FF then
          {$ENDIF}
          begin
            Include(FirstCharSet, Byte(ch));
            Include(FirstCharSet, Byte(InvertCase(ch)));
          end;
          Exit;
        end;

      OP_EXACTLY:
        begin
          ch := (scan + REOpSz + RENextOffSz + RENumberSz)^;
          {$IFDEF UnicodeRE}
          if Ord(ch) <= $FF then
          {$ENDIF}
            Include(FirstCharSet, Byte(ch));
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

      OP_ANYOF_CI:
        begin
          GetCharSetFromCharClass(scan + REOpSz + RENextOffSz, True, TempSet);
          FirstCharSet := FirstCharSet + TempSet;
          Exit;
        end;

      OP_ANYBUT_CI:
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
        begin
          // No point to rescan the code again
          Next := PRegExprChar(AlignToPtr(scan + 1)) + RENextOffSz;;
        end;

      OP_OPEN, OP_OPEN_ATOMIC:
        begin
          FillFirstCharSet(Next);
          Exit;
        end;

      OP_CLOSE, OP_CLOSE_ATOMIC:
        begin
          FillFirstCharSet(Next);
          Exit;
        end;

      OP_LOOKAHEAD:
        begin
          opnd := PRegExprChar(AlignToPtr(Next + 1)) + RENextOffSz;
          Next := regNextQuick(Next);
          FillFirstCharSet(Next);
          if opnd^ = OP_LOOKAROUND_OPTIONAL then
            Exit;

          Next := PRegExprChar(AlignToPtr(scan + 1)) + RENextOffSz;
          TmpFirstCharSet := FirstCharSet;
          FirstCharSet := [];
          FillFirstCharSet(Next);

          if TmpFirstCharSet = [] then
            exit;
          if FirstCharSet = [] then
            FirstCharSet := TmpFirstCharSet
          else
            FirstCharSet := FirstCharSet * TmpFirstCharSet;
          exit;
        end;

      OP_LOOKAHEAD_NEG,
      OP_LOOKBEHIND, OP_LOOKBEHIND_NEG:
        begin
          Next := PRegExprChar(AlignToPtr(Next + 1)) + RENextOffSz;
        end;

      OP_LOOKAHEAD_END, OP_LOOKBEHIND_END:
        begin
          Exit;
        end;

      OP_LOOKAROUND_OPTIONAL:
        begin
          Next := PRegExprChar(AlignToPtr(scan + 1)) + RENextOffSz;
        end;

      OP_BRANCH, OP_GBRANCH, OP_GBRANCH_EX, OP_GBRANCH_EX_CI:
        begin
          repeat
            TmpFirstCharSet := FirstCharSet;
            FirstCharSet := [];
            FillFirstCharSet(scan + REOpSz + RENextOffSz + REBranchArgSz);
            FirstCharSet := FirstCharSet + TmpFirstCharSet;
            scan := regNextQuick(scan);
          until (scan = nil) or
            ( (PREOp(scan)^ <> OP_BRANCH) and (PREOp(Next)^ <> OP_GBRANCH) and
              (PREOp(scan)^ <> OP_GBRANCH_EX) and (PREOp(scan)^ <> OP_GBRANCH_EX_CI) );
          Exit;
        end;

      {$IFDEF ComplexBraces}
      OP_LOOPENTRY:
        begin
          min_cnt := PREBracesArg(AlignToPtr(Next + REOpSz + RENextOffSz))^;
          if min_cnt = 0 then begin
            opnd := AlignToPtr(Next + REOpSz + 2 * RENextOffSz + 2 * REBracesArgSz);
            FillFirstCharSet(opnd); // FirstChar may be after loop
          end;
          Next := PRegExprChar(AlignToPtr(scan + 1)) + RENextOffSz;
        end;

      OP_LOOP,
      OP_LOOP_NG,
      OP_LOOP_POSS:
        begin
          min_cnt := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz))^;
          if min_cnt = 0 then
            Exit;
          // zero width loop
          Next := AlignToPtr(scan + REOpSz + 2 * RENextOffSz + 2 * REBracesArgSz);
        end;
      {$ENDIF}

      OP_STAR,
      OP_STAR_NG,
      OP_STAR_POSS:
        FillFirstCharSet(scan + REOpSz + RENextOffSz);

      OP_PLUS,
      OP_PLUS_NG,
      OP_PLUS_POSS:
        begin
          FillFirstCharSet(scan + REOpSz + RENextOffSz);
          Exit;
        end;

      OP_BRACES,
      OP_BRACES_NG,
      OP_BRACES_POSS:
        begin
          opnd := scan + REOpSz + RENextOffSz + REBracesArgSz * 2;
          min_cnt := PREBracesArg(AlignToPtr(scan + REOpSz + RENextOffSz))^; // BRACES
          FillFirstCharSet(opnd);
          if min_cnt > 0 then
            Exit;
        end;

      OP_EEND:
        begin
          FirstCharSet := RegExprAllSet;
          Exit;
        end;

      OP_ANYCATEGORY,
      OP_NOTCATEGORY:
        begin
          FirstCharSet := RegExprAllSet;
          Exit;
        end;

      OP_RECUR,
      OP_SUBCALL:
        begin
          // we cannot optimize // TODO: lookup the called group
          FirstCharSet := RegExprAllSet;
          Exit;
        end;

      OP_ANYLINEBREAK:
        begin
          Include(FirstCharSet, Byte(10));
          Include(FirstCharSet, Byte(13));
          Include(FirstCharSet, Byte($0B));
          Include(FirstCharSet, Byte($0C));
          Include(FirstCharSet, Byte($85));
        end;

    else
      fLastErrorOpcode := Oper;
      Error(reeUnknownOpcodeInFillFirst);
      Exit;
    end; { of case scan^}
    scan := Next;
  end; { of while scan <> nil}
end; { of procedure FillFirstCharSet
--------------------------------------------------------------}
{$ENDIF}

procedure TRegExpr.InitCharCheckers;
var
  Cnt: Integer;
  //
  function Add(AChecker: TRegExprCharChecker): Byte;
  begin
    Inc(Cnt);
    if Cnt > High(CharCheckers) then
      Error(reeTooSmallCheckersArray);
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
  CheckerIndex_AnyLineBreak := Add(CharChecker_AnyLineBreak);

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

function TRegExpr.CharChecker_Word(ch: REChar): Boolean;
begin
  Result := IsWordChar(ch);
end;

function TRegExpr.CharChecker_NotWord(ch: REChar): Boolean;
begin
  Result := not IsWordChar(ch);
end;

function TRegExpr.CharChecker_Space(ch: REChar): Boolean;
begin
  Result := IsSpaceChar(ch);
end;

function TRegExpr.CharChecker_NotSpace(ch: REChar): Boolean;
begin
  Result := not IsSpaceChar(ch);
end;

function TRegExpr.CharChecker_Digit(ch: REChar): Boolean;
begin
  Result := IsDigitChar(ch);
end;

function TRegExpr.CharChecker_NotDigit(ch: REChar): Boolean;
begin
  Result := not IsDigitChar(ch);
end;

function TRegExpr.CharChecker_VertSep(ch: REChar): Boolean;
begin
  Result := IsVertLineSeparator(ch);
end;

function TRegExpr.CharChecker_NotVertSep(ch: REChar): Boolean;
begin
  Result := not IsVertLineSeparator(ch);
end;

function TRegExpr.CharChecker_AnyLineBreak(ch: REChar): Boolean;
begin
  Result := IsAnyLineBreak(ch);
end;

function TRegExpr.CharChecker_HorzSep(ch: REChar): Boolean;
begin
  Result := IsHorzSeparator(ch);
end;

function TRegExpr.CharChecker_NotHorzSep(ch: REChar): Boolean;
begin
  Result := not IsHorzSeparator(ch);
end;

function TRegExpr.CharChecker_LowerAZ(ch: REChar): Boolean;
begin
  case ch of
    'a' .. 'z':
      Result := True;
  else
    Result := False;
  end;
end;

function TRegExpr.CharChecker_UpperAZ(ch: REChar): Boolean;
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
    OP_EOL2:
      Result := 'EOL2';
    OP_BOL_ML:
      Result := 'BOL_ML';
    OP_CONTINUE_POS:
      Result := 'CONTINUE_POS';
    OP_EOL_ML:
      Result := 'EOL_ML';
    OP_BOUND:
      Result := 'BOUND';
    OP_NOTBOUND:
      Result := 'NOTBOUND';
    OP_ANY:
      Result := 'ANY';
    OP_ANY_ML:
      Result := 'ANY_ML';
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
    OP_ANYOF_CI:
      Result := 'ANYOF_CI';
    OP_ANYBUT_CI:
      Result := 'ANYBUT_CI';
    OP_BRANCH:
      Result := 'BRANCH';
    OP_GBRANCH:
      Result := 'G_BRANCH';
    OP_GBRANCH_EX:
      Result := 'G_BRANCH_EX';
    OP_GBRANCH_EX_CI:
      Result := 'G_BRANCH_EX_CI';
    OP_EXACTLY:
      Result := 'EXACTLY';
    OP_EXACTLY_CI:
      Result := 'EXACTLY_CI';
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
    OP_BSUBEXP_CI:
      Result := 'BSUBEXP_CI';
    OP_OPEN:
      Result := 'OPEN';
    OP_CLOSE:
      Result := 'CLOSE';
    OP_OPEN_ATOMIC:
      Result := 'OPEN_ATOMIC';
    OP_CLOSE_ATOMIC:
      Result := 'CLOSE_ATOMIC';
    OP_LOOKAHEAD:
      Result := 'LOOKAHEAD';
    OP_LOOKAHEAD_NEG:
      Result := 'LOOKAHEAD_NEG';
    OP_LOOKBEHIND:
      Result := 'LOOKBEHIND';
    OP_LOOKBEHIND_NEG:
      Result := 'LOOKBEHIND_NEG';
    OP_LOOKAHEAD_END:
      Result := 'LOOKAHEAD_END';
    OP_LOOKBEHIND_END:
      Result := 'LOOKBEHIND_END';
    OP_LOOKAROUND_OPTIONAL:
      Result := 'OP_LOOKAROUND_OPTIONAL';
    OP_STAR:
      Result := 'STAR';
    OP_PLUS:
      Result := 'PLUS';
    OP_BRACES:
      Result := 'BRACES';
    {$IFDEF ComplexBraces}
    OP_LOOPENTRY:
      Result := 'LOOPENTRY';
    OP_LOOP:
      Result := 'LOOP';
    OP_LOOP_NG:
      Result := 'LOOP_NG';
    OP_LOOP_POSS:
      Result := 'LOOP_POSS';
    {$ENDIF}
    OP_STAR_NG:
      Result := 'STAR_NG';
    OP_PLUS_NG:
      Result := 'PLUS_NG';
    OP_BRACES_NG:
      Result := 'BRACES_NG';
    OP_STAR_POSS:
      Result := 'STAR_POSS';
    OP_PLUS_POSS:
      Result := 'PLUS_POSS';
    OP_BRACES_POSS:
      Result := 'BRACES_POSS';
    OP_ANYCATEGORY:
      Result := 'ANYCATEGORY';
    OP_NOTCATEGORY:
      Result := 'NOTCATEGORY';
    OP_RECUR:
      Result := 'RECURSION';
    OP_SUBCALL:
      Result := 'SUBCALL';
    OP_ANYLINEBREAK:
      Result := 'ANYLINEBREAK';
    OP_RESET_MATCHPOS:
      Result := 'RESET_MATCHPOS';
  else
    Error(reeDumpCorruptedOpcode);
  end;
end; { of function TRegExpr.DumpOp
  -------------------------------------------------------------- }

function TRegExpr.IsCompiled: Boolean;
begin
  Result := programm <> nil;
end;

function PrintableChar(AChar: REChar): RegExprString; {$IFDEF InlineFuncs}inline;{$ENDIF}
begin
  if AChar < ' ' then
    Result := '#' + IntToStr(Ord(AChar))
  else
    Result := AChar;
end;

function TRegExpr.DumpCheckerIndex(N: Byte): RegExprString;
begin
  Result := '?';
  if N = CheckerIndex_Word       then Result := '\w' else
  if N = CheckerIndex_NotWord    then Result := '\W' else
  if N = CheckerIndex_Digit      then Result := '\d' else
  if N = CheckerIndex_NotDigit   then Result := '\D' else
  if N = CheckerIndex_Space      then Result := '\s' else
  if N = CheckerIndex_NotSpace   then Result := '\S' else
  if N = CheckerIndex_HorzSep    then Result := '\h' else
  if N = CheckerIndex_NotHorzSep then Result := '\H' else
  if N = CheckerIndex_VertSep    then Result := '\v' else
  if N = CheckerIndex_NotVertSep then Result := '\V' else
  if N = CheckerIndex_LowerAZ    then Result := 'az' else
  if N = CheckerIndex_UpperAZ    then Result := 'AZ' else
  if N = CheckerIndex_AnyLineBreak then Result := '\R'
  ;
end;

function TRegExpr.DumpCategoryChars(ch, ch2: REChar; Positive: Boolean): RegExprString;
const
  S: array[Boolean] of RegExprString = ('P', 'p');
begin
  Result := '\' + S[Positive] + '{' + ch;
  if ch2 <> #0 then
    Result := Result + ch2;
  Result := Result + '} ';
end;

function TRegExpr.Dump(Indent: Integer): RegExprString;
// dump a regexp in vaguely comprehensible form
var
  s: PRegExprChar;
  op: TREOp; // Arbitrary non-END op.
  next: PRegExprChar;
  i, NLen, CurIndent: Integer;
  Diff: PtrInt;
  iByte: Byte;
  ch, ch2: REChar;
begin
  Result := '';
  if not IsProgrammOk then
    Exit;

  CurIndent := 0;
  op := OP_EXACTLY;
  s := regCodeWork;
  while op <> OP_EEND do
  begin // While that wasn't END last time...
    op := s^;
    if ((op =OP_CLOSE) or (op = OP_CLOSE_ATOMIC) or (op = OP_LOOP) or (op = OP_LOOP_NG) or (op = OP_LOOP_POSS)) and (CurIndent > 0) then
      dec(CurIndent, Indent);
    Result := Result + Format('%2d:%s %s', [s - programm, StringOfChar(' ', CurIndent), DumpOp(s^)]);
    if ((op = OP_OPEN) or (op = OP_OPEN_ATOMIC) or (op = OP_LOOPENTRY)) then
      inc(CurIndent, Indent);
    // Where, what.
    next := regNext(s);
    if next = nil // Next ptr.
    then
      Result := Result + ' (0)'
    else
    begin
      if next > s
      // PWideChar subtraction workaround (see comments in Tail method for details)
      then
        Diff := next - s
      else
        Diff := -(s - next);
      Result := Result + Format(' (%d) ', [(s - programm) + Diff]);
    end;
    Inc(s, REOpSz + RENextOffSz);
    if (op = OP_ANYOF) or (op = OP_ANYOF_CI) or (op = OP_ANYBUT) or (op = OP_ANYBUT_CI) then
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
              Result := Result + DumpCheckerIndex(Byte(s^)) + ' ';
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
          OpKind_CategoryYes:
            begin
              Inc(s);
              ch := s^;
              Inc(s);
              ch2 := s^;
              Result := Result + DumpCategoryChars(ch, ch2, True);
              Inc(s);
            end;
          OpKind_CategoryNo:
            begin
              Inc(s);
              ch := s^;
              Inc(s);
              ch2 := s^;
              Result := Result + DumpCategoryChars(ch, ch2, False);
              Inc(s);
            end;
        else
          Error(reeDumpCorruptedOpcode);
        end;
      until false;
    end;
    if (op = OP_EXACTLY) or (op = OP_EXACTLY_CI) then
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
    if (op = OP_BSUBEXP) or (op = OP_BSUBEXP_CI) then
    begin
      Result := Result + ' \' + IntToStr(PReGroupIndex(s)^);
      Inc(s, ReGroupIndexSz);
    end;
    if (op = OP_SUBCALL) then
    begin
      Result := Result + ' (?' + IntToStr(PReGroupIndex(s)^) + ') @'  + IntToStr(GrpOpCodes[PReGroupIndex(s)^]-programm);
      Inc(s, ReGroupIndexSz);
    end;
    if (op = OP_OPEN) or (op = OP_OPEN_ATOMIC) or (op = OP_CLOSE) or (op = OP_CLOSE_ATOMIC) then
    begin
      Result := Result + ' [' + IntToStr(PReGroupIndex(s)^) + ']';
      Inc(s, ReGroupIndexSz);
    end;
    if (op = OP_BRACES) or (op = OP_BRACES_NG) or (op = OP_BRACES_POSS) then
    begin
      // show min/max argument of braces operator
      Result := Result + Format('{%d,%d}', [PREBracesArg(AlignToInt(s))^,
        PREBracesArg(AlignToInt(s + REBracesArgSz))^]);
      Inc(s, REBracesArgSz * 2);
    end;
    {$IFDEF ComplexBraces}
    if (op = OP_LOOP) or (op = OP_LOOP_NG) or (op = OP_LOOP_POSS) then
    begin
      Result := Result + Format(' -> (%d) {%d,%d}',
        [(s - programm - (REOpSz + RENextOffSz)) +
        PRENextOff(AlignToPtr(s + 2 * REBracesArgSz))^,
        PREBracesArg(AlignToInt(s))^,
        PREBracesArg(AlignToInt(s + REBracesArgSz))^]);
      Inc(s, 2 * REBracesArgSz + RENextOffSz);
    end;
    {$ENDIF}
    if (op = OP_ANYCATEGORY) or (op = OP_NOTCATEGORY) then
    begin
      ch := s^;
      Inc(s);
      ch2 := s^;
      Inc(s);
      if ch2<>#0 then
        Result := Result + '{' + ch + ch2 + '}'
      else
        Result := Result + '{' + ch + '}';
    end;
    if (op = OP_LOOKBEHIND) or (op = OP_LOOKBEHIND_NEG) then
    begin
      if PReOpLookBehindOptions(s)^.IsGreedy = OPT_LOOKBEHIND_FIXED then
        Result := Result + ' (fixed)'
      else
      if PReOpLookBehindOptions(s)^.IsGreedy = OPT_LOOKBEHIND_NON_GREEDY then
        Result := Result + ' (not greedy)'
      else
        Result := Result + ' (greedy)';
        Result := Result
               + ' Len: ' + IntToStr(PReOpLookBehindOptions(s)^.MatchLenMin)
               + '..' + IntToStr(PReOpLookBehindOptions(s)^.MatchLenMax);
      Inc(s, ReOpLookBehindOptionsSz);
    end
    else
    if (op = OP_BRANCH) or (op = OP_GBRANCH) or
       (op = OP_GBRANCH_EX) or (op = OP_GBRANCH_EX_CI)
    then
    begin
      Inc(s, REBranchArgSz);
    end;
    Result := Result + #$d#$a;
  end; { of while }

  // Header fields of interest.
  case regAnchored of
    raBOL:      Result := Result + 'Anchored(BOL); ';
    raEOL:      Result := Result + 'Anchored(EOL); ';
    raContinue: Result := Result + 'Anchored(\G); ';
    raOnlyOnce: Result := Result + 'Anchored(start); ';
  end;

  if regMustString <> '' then
    Result := Result + 'Must have: "' + regMustString + '"; ';

  {$IFDEF UseFirstCharSet}
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


function TRegExpr.IsFixedLength(var op: TREOp; var ALen: Integer): Boolean;
var
  s: PRegExprChar;
  ADummyMaxLen: integer;
begin
  Result := False;
  if not IsCompiled then Exit;
  s := regCodeWork;
  Result := IsPartFixedLength(s, op, ALen, ADummyMaxLen, OP_EEND, nil, []);
end;

function TRegExpr.IsFixedLengthEx(var op: TREOp; var AMinLen, AMaxLen: integer
  ): boolean;
var
  s: PRegExprChar;
begin
  Result := False;
  if not IsCompiled then Exit;
  s := regCodeWork;
  Result := IsPartFixedLength(s, op, AMinLen, AMaxLen, OP_EEND, nil, []);
end;

function TRegExpr.IsPartFixedLength(var prog: PRegExprChar; var op: TREOp;
  var AMinLen, AMaxLen: integer; StopAt: TREOp; StopMaxProg: PRegExprChar;
  Flags: TRegExprFindFixedLengthFlags): boolean;

  function MultiplyLen(AVal, AFactor: Integer): Integer;
  begin
    if AFactor > High(AVal) div AVal then
      Result := high(AVal)
    else
      Result := AVal * AFactor;
  end;

  procedure IncMaxLen(var AVal: Integer; AInc: Integer);
  begin
    if AInc > High(AVal) - AVal then
      AVal := high(AVal)
    else
      AVal := AVal + AInc;
  end;


var
  s, next: PRegExprChar;
  N, N2, FndMaxLen, ASubLen, ABranchLen, ABranchMaxLen, ASubMaxLen: integer;
  NotFixedLen, r, NextIsNil: Boolean;
  FirstVarLenOp: TREOp;
begin
  Result := False;
  NotFixedLen := False;
  AMinLen := 0;
  AMaxLen := High(AMaxLen);
  FndMaxLen := 0;
  next := prog;
  s := prog;

  repeat
    NextIsNil := next = nil;
    next := regNext(s);
    prog := s;
    op := s^;
    if not NotFixedLen then
      FirstVarLenOp := op;

    if (op = StopAt) or
       ((StopMaxProg <> nil) and (s >= StopMaxProg)) or
       (NextIsNil and (flfReturnAtNextNil in Flags))
    then begin
      AMaxLen := FndMaxLen;
      op := FirstVarLenOp;
      if not NotFixedLen then
        Result := True;
      Exit;
    end;

    Inc(s, REOpSz + RENextOffSz);

    case op of
      OP_EEND:
        begin
          AMaxLen := FndMaxLen;
          op := FirstVarLenOp;
          if not NotFixedLen then
            Result := True;
          Exit;
        end;

      OP_BRANCH, OP_GBRANCH, OP_GBRANCH_EX, OP_GBRANCH_EX_CI:
        begin
          s := s + REBranchArgSz;
          if not IsPartFixedLength(s, op, ABranchLen, ABranchMaxLen, OP_EEND, next, []) then
          begin
            if not NotFixedLen then
              FirstVarLenOp := op;
            NotFixedLen := True;
            if (ABranchMaxLen = high(ABranchMaxLen)) and not(flfForceToStopAt in Flags) then
              exit;
          end;
          s := next;
          repeat
            next := regNext(s);
            s := s + REBranchArgSz;
            Inc(s, REOpSz + RENextOffSz);
            if not IsPartFixedLength(s, op, ASubLen, ASubMaxLen, OP_EEND, next, []) then
            begin
              if not NotFixedLen then
                FirstVarLenOp := op;
              NotFixedLen := True;
              if (ABranchMaxLen = high(ABranchMaxLen)) and not(flfForceToStopAt in Flags) then
                exit;
            end;
            s := next;
            if (ASubLen <> ABranchLen) then
              NotFixedLen := True;
            if ASubLen < ABranchLen then
              ABranchLen := ASubLen;
            if ASubMaxLen > ABranchMaxLen then
              ABranchMaxLen := ASubMaxLen;
          until (next^ <> OP_BRANCH) and (next^ <> OP_GBRANCH) and
                (next^ <> OP_GBRANCH_EX) and (next^ <> OP_GBRANCH_EX_CI);
          AMinLen := AMinLen + ABranchLen;
          IncMaxLen(FndMaxLen, ABranchMaxLen);
        end;

      OP_OPEN:
        begin
          Inc(s, ReGroupIndexSz);
          if not IsPartFixedLength(s, op, ASubLen, ASubMaxLen, OP_CLOSE, nil, [flfForceToStopAt]) then
          begin
            if not NotFixedLen then
              FirstVarLenOp := op;
            NotFixedLen := True;
            if (ABranchMaxLen = high(ABranchMaxLen)) and not(flfForceToStopAt in Flags) then
              exit;
          end;
          assert(s^=OP_CLOSE);
          AMinLen := AMinLen + ASubLen;
          IncMaxLen(FndMaxLen, ASubMaxLen);
          Inc(s, REOpSz + RENextOffSz + ReGroupIndexSz); // consume the OP_CLOSE
          continue;
        end;

      OP_OPEN_ATOMIC:
        begin
          Inc(s, ReGroupIndexSz);
          if not IsPartFixedLength(s, op, ASubLen, ASubMaxLen, OP_CLOSE_ATOMIC, nil, [flfForceToStopAt]) then
          begin
            if not NotFixedLen then
              FirstVarLenOp := op;
            NotFixedLen := True;
            if (ABranchMaxLen = high(ABranchMaxLen)) and not(flfForceToStopAt in Flags) then
              exit;
          end;
          assert(s^=OP_CLOSE_ATOMIC);
          AMinLen := AMinLen + ASubLen;
          IncMaxLen(FndMaxLen, ASubMaxLen);
          Inc(s, REOpSz + RENextOffSz + ReGroupIndexSz); // consume the OP_CLOSE_ATOMIC;
          continue;
        end;

      OP_CLOSE, OP_CLOSE_ATOMIC:
        begin
          Inc(s, ReGroupIndexSz);
          continue;
        end;

      OP_LOOKAHEAD, OP_LOOKAHEAD_NEG:
        begin
          r := IsPartFixedLength(s, op, ASubLen, ASubMaxLen, OP_LOOKAHEAD_END, next, [flfSkipLookAround, flfForceToStopAt]);
          s := next;
          Inc(s, REOpSz + RENextOffSz); // skip the OP_LOOKAHEAD_END
          if not (flfSkipLookAround in Flags) then
          begin
            //if not r then
              NotFixedLen := True;
          end;
        end;

      OP_LOOKBEHIND, OP_LOOKBEHIND_NEG:
        begin
          Inc(s, ReOpLookBehindOptionsSz);
          r := IsPartFixedLength(s, op, ASubLen, ASubMaxLen, OP_LOOKBEHIND_END, next, [flfSkipLookAround, flfForceToStopAt]);
          s := next;
          Inc(s, REOpSz + RENextOffSz); // skip the OP_LOOKBEHIND_END
          if not (flfSkipLookAround in Flags) then
            //if flfForceToStopAt in Flags then
              NotFixedLen := True
            //else
            //  Exit;
        end;

      OP_LOOKAHEAD_END, OP_LOOKBEHIND_END:
        if flfSkipLookAround in Flags then
        begin
          continue;
        end;

      OP_LOOKAROUND_OPTIONAL:
        continue;

      OP_NOTHING,
      OP_COMMENT,
      OP_BOUND,
      OP_NOTBOUND,
      OP_BOL,
      OP_BOL_ML,
      OP_EOL,
      OP_EOL2,
      OP_EOL_ML,
      OP_CONTINUE_POS:
        Continue;

      OP_ANY,
      OP_ANY_ML,
      OP_ANYDIGIT,
      OP_NOTDIGIT,
      OP_ANYLETTER,
      OP_NOTLETTER,
      OP_ANYSPACE,
      OP_NOTSPACE,
      OP_ANYHORZSEP,
      OP_NOTHORZSEP,
      OP_ANYVERTSEP,
      OP_NOTVERTSEP:
        begin
          Inc(AMinLen);
          IncMaxLen(FndMaxLen, 1);
          Continue;
        end;

      OP_ANYOF,
      OP_ANYOF_CI,
      OP_ANYBUT,
      OP_ANYBUT_CI:
        begin
          Inc(AMinLen);
          IncMaxLen(FndMaxLen, 1);
          repeat
            case s^ of
              OpKind_End:
                begin
                  Inc(s);
                  Break;
                end;
              OpKind_Range:
                begin
                  Inc(s);
                  Inc(s);
                  Inc(s);
                end;
              OpKind_MetaClass:
                begin
                  Inc(s);
                  Inc(s);
                end;
              OpKind_Char:
                begin
                  Inc(s);
                  Inc(s, RENumberSz + PLongInt(s)^);
                end;
              OpKind_CategoryYes,
              OpKind_CategoryNo:
                begin
                  Inc(s);
                  Inc(s);
                  Inc(s);
                end;
            end;
          until False;
        end;

      OP_EXACTLY,
      OP_EXACTLY_CI:
        begin
          N := PLongInt(s)^;
          Inc(AMinLen, N);
          IncMaxLen(FndMaxLen, N);
          Inc(s, RENumberSz + N);
          Continue;
        end;

      OP_ANYCATEGORY,
      OP_NOTCATEGORY:
        begin
          Inc(AMinLen);
          IncMaxLen(FndMaxLen, 1);
          Inc(s, 2);
          Continue;
        end;

      OP_BRACES,
      OP_BRACES_NG,
      OP_BRACES_POSS:
        begin
          // allow only d{n,n}
          N := PREBracesArg(AlignToInt(s))^;
          N2 := PREBracesArg(AlignToInt(s + REBracesArgSz))^;
          Inc(s, REBracesArgSz * 2);
          r := IsPartFixedLength(s, op, ASubLen, ASubMaxLen, OP_EEND, next, [flfSkipLookAround, flfReturnAtNextNil, flfForceToStopAt]);
          if not r then
          begin
            if not NotFixedLen then
              FirstVarLenOp := op;
            if (ABranchMaxLen = high(ABranchMaxLen)) and not(flfForceToStopAt in Flags) then
              exit;
          end;

          Inc(AMinLen, MultiplyLen(ASubLen, N));
          IncMaxLen(FndMaxLen, MultiplyLen(ASubMaxLen, N2));
          if (not r) or (N <> N2) then
            NotFixedLen := True;
          s := next;
        end;

      OP_BSUBEXP, OP_BSUBEXP_CI, OP_SUBCALL:
        begin
          s := next;
          NotFixedLen := True; // group may be in look-around. Could be anything
          FndMaxLen := high(FndMaxLen);
        end;

    else
      begin
        s := next;
        FndMaxLen := high(FndMaxLen);
        if flfForceToStopAt in Flags then
          NotFixedLen := True
        else
          Exit;
      end;
    end;
  until False;
end;

procedure TRegExpr.SetInputSubString(const AInputString: RegExprString;
  AInputStartPos, AInputLen: Integer);
begin
  ClearMatches;

  if AInputStartPos < 1 then
    AInputStartPos := 1
  else
  if AInputStartPos > Length(AInputString) then
    AInputStartPos := Length(AInputString) + 1;
  if AInputLen < 0 then
    AInputLen := 0
  else
  if AInputLen > Length(AInputString) + 1 - AInputStartPos then
    AInputLen := Length(AInputString) + 1 - AInputStartPos;

  fInputString := AInputString;
  //UniqueString(fInputString);

  fInputStart := PRegExprChar(fInputString) + AInputStartPos - 1;
  fInputEnd := fInputStart + AInputLen;
  fInputContinue := fInputStart;
end;

{$IFDEF reRealExceptionAddr}
{$OPTIMIZATION ON}
// ReturnAddr works correctly only if compiler optimization is ON
// I placed this method at very end of unit because there are no
// way to restore compiler optimization flag ...
{$ENDIF}

procedure TRegExpr.Error(AErrorID: Integer);
  {$IFDEF windows}
  {$IFDEF reRealExceptionAddr}
  function ReturnAddr: Pointer;
  asm
    mov  eax,[ebp+4]
  end;
  {$ENDIF}
  {$ENDIF}
var
  e: ERegExpr;
  Msg: string;
begin
  fLastError := AErrorID; // dummy stub - useless because will raise exception
  Msg := ErrorMsg(AErrorID);
  // compilation error ?
  if AErrorID < reeFirstRuntimeCode then
    Msg := Msg + ' (pos ' + IntToStr(CompilerErrorPos) + ')';
  e := ERegExpr.Create(Msg);
  e.ErrorCode := AErrorID;
  e.CompilerErrorPos := CompilerErrorPos;
  raise e
    {$IFDEF windows}
    {$IFDEF reRealExceptionAddr}
    at ReturnAddr
    {$ENDIF}
    {$ENDIF};
end; { of procedure TRegExpr.Error
  -------------------------------------------------------------- }

{$IFDEF Compat} // APIs needed only for users of old FPC 3.0
function TRegExpr.ExecPos(AOffset: Integer; ATryOnce: Boolean): Boolean; overload;
begin
  if ATryOnce then
    Result := ExecPrim(AOffset, False, False, AOffset + 1)
  else
    Result := ExecPrim(AOffset, False, False, 0);
end;

function TRegExpr.OldInvertCase(const Ch: REChar): REChar;
begin
  Result := _UpperCase(Ch);
  if Result = Ch then
    Result := _LowerCase(Ch);
end;

class function TRegExpr.InvertCaseFunction(const Ch: REChar): REChar;
begin
  Result := _UpperCase(Ch);
  if Result = Ch then
    Result := _LowerCase(Ch);
end;

function TRegExpr.GetLinePairedSeparator: RegExprString;
begin
  // not supported anymore
  Result := '';
end;

procedure TRegExpr.SetLinePairedSeparator(const AValue: RegExprString);
begin
  // not supported anymore
end;

procedure TRegExpr.SetUseOsLineEndOnReplace(AValue: Boolean);
begin
  if fUseOsLineEndOnReplace = AValue then
    Exit;
  fUseOsLineEndOnReplace := AValue;
  if fUseOsLineEndOnReplace then
    fReplaceLineEnd := sLineBreak
  else
    fReplaceLineEnd := #10;
end;
{$ENDIF}

end.
