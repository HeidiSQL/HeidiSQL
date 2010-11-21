{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPerl.pas, released 2000-04-10.
The Original Code is based on the DcjSynPerl.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
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

$Id: SynHighlighterPerl.pas,v 1.14.2.8 2008/09/14 16:25:01 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - Using q, qq, qw, qx, m, s, tr will not properly parse the contained
    information.
  - Not very optimized.
-------------------------------------------------------------------------------}
{
@abstract(Provides a Perl syntax highlighter for SynEdit)
@author(Michael Trier)
@created(1999, converted to SynEdit 2000-04-10 by Michael Hieke)
@lastmod(2000-06-23)
The SynHighlighterPerl unit provides SynEdit with a Perl syntax highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERPERL}
unit SynHighlighterPerl;
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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkOperator,
    tkPragma, tkSpace, tkString, tkSymbol, tkUnknown, tkVariable);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynPerlSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..2422] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fOperatorAttri: TSynHighlighterAttributes;
    fPragmaAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function Func36accumulator(Index: Integer): TtkTokenKind;
    function Func36arg(Index: Integer): TtkTokenKind;
    function Func36argv(Index: Integer): TtkTokenKind;
    function Func36basetime(Index: Integer): TtkTokenKind;
    function Func36child95error(Index: Integer): TtkTokenKind;
    function Func36debugging(Index: Integer): TtkTokenKind;
    function Func36effective95group95id(Index: Integer): TtkTokenKind;
    function Func36effective95user95id(Index: Integer): TtkTokenKind;
    function Func36egid(Index: Integer): TtkTokenKind;
    function Func36env(Index: Integer): TtkTokenKind;
    function Func36errno(Index: Integer): TtkTokenKind;
    function Func36euid(Index: Integer): TtkTokenKind;
    function Func36eval95error(Index: Integer): TtkTokenKind;
    function Func36executable95name(Index: Integer): TtkTokenKind;
    function Func36format95formfeed(Index: Integer): TtkTokenKind;
    function Func36format95line95break95characters(Index: Integer): TtkTokenKind;
    function Func36format95lines95left(Index: Integer): TtkTokenKind;
    function Func36format95lines95per95page(Index: Integer): TtkTokenKind;
    function Func36format95name(Index: Integer): TtkTokenKind;
    function Func36format95page95number(Index: Integer): TtkTokenKind;
    function Func36format95top95name(Index: Integer): TtkTokenKind;
    function Func36gid(Index: Integer): TtkTokenKind;
    function Func36inplace95edit(Index: Integer): TtkTokenKind;
    function Func36input95line95number(Index: Integer): TtkTokenKind;
    function Func36input95record95separator(Index: Integer): TtkTokenKind;
    function Func36last95paren95match(Index: Integer): TtkTokenKind;
    function Func36list95separator(Index: Integer): TtkTokenKind;
    function Func36match(Index: Integer): TtkTokenKind;
    function Func36multiline95matching(Index: Integer): TtkTokenKind;
    function Func36nr(Index: Integer): TtkTokenKind;
    function Func36ofmt(Index: Integer): TtkTokenKind;
    function Func36ors(Index: Integer): TtkTokenKind;
    function Func36os95error(Index: Integer): TtkTokenKind;
    function Func36output95autoflush(Index: Integer): TtkTokenKind;
    function Func36output95field95separator(Index: Integer): TtkTokenKind;
    function Func36perl95version(Index: Integer): TtkTokenKind;
    function Func36perldb(Index: Integer): TtkTokenKind;
    function Func36pid(Index: Integer): TtkTokenKind;
    function Func36postmatch(Index: Integer): TtkTokenKind;
    function Func36prematch(Index: Integer): TtkTokenKind;
    function Func36process95id(Index: Integer): TtkTokenKind;
    function Func36program95name(Index: Integer): TtkTokenKind;
    function Func36real95group95id(Index: Integer): TtkTokenKind;
    function Func36real95user95id(Index: Integer): TtkTokenKind;
    function Func36rs(Index: Integer): TtkTokenKind;
    function Func36sig(Index: Integer): TtkTokenKind;
    function Func36subscript95separator(Index: Integer): TtkTokenKind;
    function Func36subsep(Index: Integer): TtkTokenKind;
    function Func36system95fd95max(Index: Integer): TtkTokenKind;
    function Func36uid(Index: Integer): TtkTokenKind;
    function Func36warning(Index: Integer): TtkTokenKind;
    function Func37inc(Index: Integer): TtkTokenKind;
    function Func64argv(Index: Integer): TtkTokenKind;
    function Func64inc(Index: Integer): TtkTokenKind;
    function FuncAbs(Index: Integer): TtkTokenKind;
    function FuncAccept(Index: Integer): TtkTokenKind;
    function FuncAlarm(Index: Integer): TtkTokenKind;
    function FuncAnd(Index: Integer): TtkTokenKind;
    function FuncAtan2(Index: Integer): TtkTokenKind;
    function FuncBind(Index: Integer): TtkTokenKind;
    function FuncBinmode(Index: Integer): TtkTokenKind;
    function FuncBless(Index: Integer): TtkTokenKind;
    function FuncCaller(Index: Integer): TtkTokenKind;
    function FuncChdir(Index: Integer): TtkTokenKind;
    function FuncChmod(Index: Integer): TtkTokenKind;
    function FuncChomp(Index: Integer): TtkTokenKind;
    function FuncChop(Index: Integer): TtkTokenKind;
    function FuncChown(Index: Integer): TtkTokenKind;
    function FuncChr(Index: Integer): TtkTokenKind;
    function FuncChroot(Index: Integer): TtkTokenKind;
    function FuncClose(Index: Integer): TtkTokenKind;
    function FuncClosedir(Index: Integer): TtkTokenKind;
    function FuncCmp(Index: Integer): TtkTokenKind;
    function FuncConnect(Index: Integer): TtkTokenKind;
    function FuncConstant(Index: Integer): TtkTokenKind;
    function FuncCos(Index: Integer): TtkTokenKind;
    function FuncCrypt(Index: Integer): TtkTokenKind;
    function FuncDbmclose(Index: Integer): TtkTokenKind;
    function FuncDbmopen(Index: Integer): TtkTokenKind;
    function FuncDefined(Index: Integer): TtkTokenKind;
    function FuncDelete(Index: Integer): TtkTokenKind;
    function FuncDiagnostics(Index: Integer): TtkTokenKind;
    function FuncDie(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDump(Index: Integer): TtkTokenKind;
    function FuncEach(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncElsif(Index: Integer): TtkTokenKind;
    function FuncEndgrent(Index: Integer): TtkTokenKind;
    function FuncEndhostent(Index: Integer): TtkTokenKind;
    function FuncEndnetent(Index: Integer): TtkTokenKind;
    function FuncEndprotoent(Index: Integer): TtkTokenKind;
    function FuncEndpwent(Index: Integer): TtkTokenKind;
    function FuncEndservent(Index: Integer): TtkTokenKind;
    function FuncEof(Index: Integer): TtkTokenKind;
    function FuncEq(Index: Integer): TtkTokenKind;
    function FuncEval(Index: Integer): TtkTokenKind;
    function FuncExec(Index: Integer): TtkTokenKind;
    function FuncExists(Index: Integer): TtkTokenKind;
    function FuncExit(Index: Integer): TtkTokenKind;
    function FuncExp(Index: Integer): TtkTokenKind;
    function FuncFcntl(Index: Integer): TtkTokenKind;
    function FuncFileno(Index: Integer): TtkTokenKind;
    function FuncFlock(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncForeach(Index: Integer): TtkTokenKind;
    function FuncFork(Index: Integer): TtkTokenKind;
    function FuncFormat(Index: Integer): TtkTokenKind;
    function FuncFormline(Index: Integer): TtkTokenKind;
    function FuncGe(Index: Integer): TtkTokenKind;
    function FuncGetc(Index: Integer): TtkTokenKind;
    function FuncGetgrent(Index: Integer): TtkTokenKind;
    function FuncGetgrgid(Index: Integer): TtkTokenKind;
    function FuncGetgrnam(Index: Integer): TtkTokenKind;
    function FuncGethostbyaddr(Index: Integer): TtkTokenKind;
    function FuncGethostbyname(Index: Integer): TtkTokenKind;
    function FuncGethostent(Index: Integer): TtkTokenKind;
    function FuncGetlogin(Index: Integer): TtkTokenKind;
    function FuncGetnetbyaddr(Index: Integer): TtkTokenKind;
    function FuncGetnetbyname(Index: Integer): TtkTokenKind;
    function FuncGetnetent(Index: Integer): TtkTokenKind;
    function FuncGetpeername(Index: Integer): TtkTokenKind;
    function FuncGetpgrp(Index: Integer): TtkTokenKind;
    function FuncGetppid(Index: Integer): TtkTokenKind;
    function FuncGetpriority(Index: Integer): TtkTokenKind;
    function FuncGetprotobyname(Index: Integer): TtkTokenKind;
    function FuncGetprotobynumber(Index: Integer): TtkTokenKind;
    function FuncGetprotoent(Index: Integer): TtkTokenKind;
    function FuncGetpwent(Index: Integer): TtkTokenKind;
    function FuncGetpwnam(Index: Integer): TtkTokenKind;
    function FuncGetpwuid(Index: Integer): TtkTokenKind;
    function FuncGetservbyname(Index: Integer): TtkTokenKind;
    function FuncGetservbyport(Index: Integer): TtkTokenKind;
    function FuncGetservent(Index: Integer): TtkTokenKind;
    function FuncGetsockname(Index: Integer): TtkTokenKind;
    function FuncGetsockopt(Index: Integer): TtkTokenKind;
    function FuncGlob(Index: Integer): TtkTokenKind;
    function FuncGmtime(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncGrep(Index: Integer): TtkTokenKind;
    function FuncGt(Index: Integer): TtkTokenKind;
    function FuncHex(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncImport(Index: Integer): TtkTokenKind;
    function FuncIndex(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncInteger(Index: Integer): TtkTokenKind;
    function FuncIoctl(Index: Integer): TtkTokenKind;
    function FuncJoin(Index: Integer): TtkTokenKind;
    function FuncKeys(Index: Integer): TtkTokenKind;
    function FuncKill(Index: Integer): TtkTokenKind;
    function FuncLast(Index: Integer): TtkTokenKind;
    function FuncLc(Index: Integer): TtkTokenKind;
    function FuncLcfirst(Index: Integer): TtkTokenKind;
    function FuncLe(Index: Integer): TtkTokenKind;
    function FuncLength(Index: Integer): TtkTokenKind;
    function FuncLess(Index: Integer): TtkTokenKind;
    function FuncLink(Index: Integer): TtkTokenKind;
    function FuncListen(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLocale(Index: Integer): TtkTokenKind;
    function FuncLocaltime(Index: Integer): TtkTokenKind;
    function FuncLog(Index: Integer): TtkTokenKind;
    function FuncLstat(Index: Integer): TtkTokenKind;
    function FuncLt(Index: Integer): TtkTokenKind;
    function FuncM(Index: Integer): TtkTokenKind;
    function FuncMap(Index: Integer): TtkTokenKind;
    function FuncMkdir(Index: Integer): TtkTokenKind;
    function FuncMsgctl(Index: Integer): TtkTokenKind;
    function FuncMsgget(Index: Integer): TtkTokenKind;
    function FuncMsgrcv(Index: Integer): TtkTokenKind;
    function FuncMsgsnd(Index: Integer): TtkTokenKind;
    function FuncMy(Index: Integer): TtkTokenKind;
    function FuncNe(Index: Integer): TtkTokenKind;
    function FuncNext(Index: Integer): TtkTokenKind;
    function FuncNo(Index: Integer): TtkTokenKind;
    function FuncNot(Index: Integer): TtkTokenKind;
    function FuncOct(Index: Integer): TtkTokenKind;
    function FuncOpen(Index: Integer): TtkTokenKind;
    function FuncOpendir(Index: Integer): TtkTokenKind;
    function FuncOr(Index: Integer): TtkTokenKind;
    function FuncOrd(Index: Integer): TtkTokenKind;
    function FuncPack(Index: Integer): TtkTokenKind;
    function FuncPackage(Index: Integer): TtkTokenKind;
    function FuncPipe(Index: Integer): TtkTokenKind;
    function FuncPop(Index: Integer): TtkTokenKind;
    function FuncPos(Index: Integer): TtkTokenKind;
    function FuncPrint(Index: Integer): TtkTokenKind;
    function FuncPush(Index: Integer): TtkTokenKind;
    function FuncQ(Index: Integer): TtkTokenKind;
    function FuncQq(Index: Integer): TtkTokenKind;
    function FuncQuotemeta(Index: Integer): TtkTokenKind;
    function FuncQw(Index: Integer): TtkTokenKind;
    function FuncQx(Index: Integer): TtkTokenKind;
    function FuncRand(Index: Integer): TtkTokenKind;
    function FuncRead(Index: Integer): TtkTokenKind;
    function FuncReaddir(Index: Integer): TtkTokenKind;
    function FuncReadlink(Index: Integer): TtkTokenKind;
    function FuncRecv(Index: Integer): TtkTokenKind;
    function FuncRedo(Index: Integer): TtkTokenKind;
    function FuncRef(Index: Integer): TtkTokenKind;
    function FuncRename(Index: Integer): TtkTokenKind;
    function FuncRequire(Index: Integer): TtkTokenKind;
    function FuncReset(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncReverse(Index: Integer): TtkTokenKind;
    function FuncRewinddir(Index: Integer): TtkTokenKind;
    function FuncRindex(Index: Integer): TtkTokenKind;
    function FuncRmdir(Index: Integer): TtkTokenKind;
    function FuncScalar(Index: Integer): TtkTokenKind;
    function FuncSeek(Index: Integer): TtkTokenKind;
    function FuncSeekdir(Index: Integer): TtkTokenKind;
    function FuncSelect(Index: Integer): TtkTokenKind;
    function FuncSemctl(Index: Integer): TtkTokenKind;
    function FuncSemget(Index: Integer): TtkTokenKind;
    function FuncSemop(Index: Integer): TtkTokenKind;
    function FuncSend(Index: Integer): TtkTokenKind;
    function FuncSetgrent(Index: Integer): TtkTokenKind;
    function FuncSethostent(Index: Integer): TtkTokenKind;
    function FuncSetnetent(Index: Integer): TtkTokenKind;
    function FuncSetpgrp(Index: Integer): TtkTokenKind;
    function FuncSetpriority(Index: Integer): TtkTokenKind;
    function FuncSetprotoent(Index: Integer): TtkTokenKind;
    function FuncSetpwent(Index: Integer): TtkTokenKind;
    function FuncSetservent(Index: Integer): TtkTokenKind;
    function FuncSetsockopt(Index: Integer): TtkTokenKind;
    function FuncShift(Index: Integer): TtkTokenKind;
    function FuncShmctl(Index: Integer): TtkTokenKind;
    function FuncShmget(Index: Integer): TtkTokenKind;
    function FuncShmread(Index: Integer): TtkTokenKind;
    function FuncShmwrite(Index: Integer): TtkTokenKind;
    function FuncShutdown(Index: Integer): TtkTokenKind;
    function FuncSigtrap(Index: Integer): TtkTokenKind;
    function FuncSin(Index: Integer): TtkTokenKind;
    function FuncSleep(Index: Integer): TtkTokenKind;
    function FuncSocket(Index: Integer): TtkTokenKind;
    function FuncSocketpair(Index: Integer): TtkTokenKind;
    function FuncSort(Index: Integer): TtkTokenKind;
    function FuncSplice(Index: Integer): TtkTokenKind;
    function FuncSplit(Index: Integer): TtkTokenKind;
    function FuncSprintf(Index: Integer): TtkTokenKind;
    function FuncSqrt(Index: Integer): TtkTokenKind;
    function FuncSrand(Index: Integer): TtkTokenKind;
    function FuncStat(Index: Integer): TtkTokenKind;
    function FuncStrict(Index: Integer): TtkTokenKind;
    function FuncStudy(Index: Integer): TtkTokenKind;
    function FuncSub(Index: Integer): TtkTokenKind;
    function FuncSubs(Index: Integer): TtkTokenKind;
    function FuncSubstr(Index: Integer): TtkTokenKind;
    function FuncSymlink(Index: Integer): TtkTokenKind;
    function FuncSyscall(Index: Integer): TtkTokenKind;
    function FuncSysread(Index: Integer): TtkTokenKind;
    function FuncSystem(Index: Integer): TtkTokenKind;
    function FuncSyswrite(Index: Integer): TtkTokenKind;
    function FuncTell(Index: Integer): TtkTokenKind;
    function FuncTelldir(Index: Integer): TtkTokenKind;
    function FuncTie(Index: Integer): TtkTokenKind;
    function FuncTime(Index: Integer): TtkTokenKind;
    function FuncTimes(Index: Integer): TtkTokenKind;
    function FuncTr(Index: Integer): TtkTokenKind;
    function FuncTruncate(Index: Integer): TtkTokenKind;
    function FuncUc(Index: Integer): TtkTokenKind;
    function FuncUcfirst(Index: Integer): TtkTokenKind;
    function FuncUmask(Index: Integer): TtkTokenKind;
    function FuncUndef(Index: Integer): TtkTokenKind;
    function FuncUnless(Index: Integer): TtkTokenKind;
    function FuncUnlink(Index: Integer): TtkTokenKind;
    function FuncUnpack(Index: Integer): TtkTokenKind;
    function FuncUnshift(Index: Integer): TtkTokenKind;
    function FuncUntie(Index: Integer): TtkTokenKind;
    function FuncUse(Index: Integer): TtkTokenKind;
    function FuncUtime(Index: Integer): TtkTokenKind;
    function FuncValues(Index: Integer): TtkTokenKind;
    function FuncVars(Index: Integer): TtkTokenKind;
    function FuncVec(Index: Integer): TtkTokenKind;
    function FuncWait(Index: Integer): TtkTokenKind;
    function FuncWaitpid(Index: Integer): TtkTokenKind;
    function FuncWantarray(Index: Integer): TtkTokenKind;
    function FuncWarn(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncWrite(Index: Integer): TtkTokenKind;
    function FuncXor(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommentProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringInterpProc;
    procedure StringLiteralProc;
    procedure SymbolProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri
      write fOperatorAttri;
    property PragmaAttri: TSynHighlighterAttributes read fPragmaAttri
      write fPragmaAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..281] of UnicodeString = (
    '$ACCUMULATOR', '$ARG', '$ARGV', '$BASETIME', '$CHILD_ERROR', '$DEBUGGING', 
    '$EFFECTIVE_GROUP_ID', '$EFFECTIVE_USER_ID', '$EGID', '$ENV', '$ERRNO', 
    '$EUID', '$EVAL_ERROR', '$EXECUTABLE_NAME', '$FORMAT_FORMFEED', 
    '$FORMAT_LINE_BREAK_CHARACTERS', '$FORMAT_LINES_LEFT', 
    '$FORMAT_LINES_PER_PAGE', '$FORMAT_NAME', '$FORMAT_PAGE_NUMBER', 
    '$FORMAT_TOP_NAME', '$GID', '$INPLACE_EDIT', '$INPUT_LINE_NUMBER', 
    '$INPUT_RECORD_SEPARATOR', '$LAST_PAREN_MATCH', '$LIST_SEPARATOR', '$MATCH', 
    '$MULTILINE_MATCHING', '$NR', '$OFMT', '$ORS', '$OS_ERROR', 
    '$OUTPUT_AUTOFLUSH', '$OUTPUT_FIELD_SEPARATOR', '$PERL_VERSION', '$PERLDB', 
    '$PID', '$POSTMATCH', '$PREMATCH', '$PROCESS_ID', '$PROGRAM_NAME', 
    '$REAL_GROUP_ID', '$REAL_USER_ID', '$RS', '$SIG', '$SUBSCRIPT_SEPARATOR', 
    '$SUBSEP', '$SYSTEM_FD_MAX', '$UID', '$WARNING', '%INC', '@ARGV', '@INC', 
    'abs', 'accept', 'alarm', 'and', 'atan2', 'bind', 'binmode', 'bless', 
    'caller', 'chdir', 'chmod', 'chomp', 'chop', 'chown', 'chr', 'chroot', 
    'close', 'closedir', 'cmp', 'connect', 'constant', 'cos', 'crypt', 
    'dbmclose', 'dbmopen', 'defined', 'delete', 'diagnostics', 'die', 'do', 
    'dump', 'each', 'else', 'elsif', 'endgrent', 'endhostent', 'endnetent', 
    'endprotoent', 'endpwent', 'endservent', 'eof', 'eq', 'eval', 'exec', 
    'exists', 'exit', 'exp', 'fcntl', 'fileno', 'flock', 'for', 'foreach', 
    'fork', 'format', 'formline', 'ge', 'getc', 'getgrent', 'getgrgid', 
    'getgrnam', 'gethostbyaddr', 'gethostbyname', 'gethostent', 'getlogin', 
    'getnetbyaddr', 'getnetbyname', 'getnetent', 'getpeername', 'getpgrp', 
    'getppid', 'getpriority', 'getprotobyname', 'getprotobynumber', 
    'getprotoent', 'getpwent', 'getpwnam', 'getpwuid', 'getservbyname', 
    'getservbyport', 'getservent', 'getsockname', 'getsockopt', 'glob', 
    'gmtime', 'goto', 'grep', 'gt', 'hex', 'if', 'import', 'index', 'int', 
    'integer', 'ioctl', 'join', 'keys', 'kill', 'last', 'lc', 'lcfirst', 'le', 
    'length', 'less', 'link', 'listen', 'local', 'locale', 'localtime', 'log', 
    'lstat', 'lt', 'm', 'map', 'mkdir', 'msgctl', 'msgget', 'msgrcv', 'msgsnd', 
    'my', 'ne', 'next', 'no', 'not', 'oct', 'open', 'opendir', 'or', 'ord', 
    'pack', 'package', 'pipe', 'pop', 'pos', 'print', 'push', 'q', 'qq', 
    'quotemeta', 'qw', 'qx', 'rand', 'read', 'readdir', 'readlink', 'recv', 
    'redo', 'ref', 'rename', 'require', 'reset', 'return', 'reverse', 
    'rewinddir', 'rindex', 'rmdir', 'scalar', 'seek', 'seekdir', 'select', 
    'semctl', 'semget', 'semop', 'send', 'setgrent', 'sethostent', 'setnetent', 
    'setpgrp', 'setpriority', 'setprotoent', 'setpwent', 'setservent', 
    'setsockopt', 'shift', 'shmctl', 'shmget', 'shmread', 'shmwrite', 
    'shutdown', 'sigtrap', 'sin', 'sleep', 'socket', 'socketpair', 'sort', 
    'splice', 'split', 'sprintf', 'sqrt', 'srand', 'stat', 'strict', 'study', 
    'sub', 'subs', 'substr', 'symlink', 'syscall', 'sysread', 'system', 
    'syswrite', 'tell', 'telldir', 'tie', 'time', 'times', 'tr', 'truncate', 
    'uc', 'ucfirst', 'umask', 'undef', 'unless', 'unlink', 'unpack', 'unshift', 
    'untie', 'use', 'utime', 'values', 'vars', 'vec', 'wait', 'waitpid', 
    'wantarray', 'warn', 'while', 'write', 'xor' 
  );

  KeyIndices: array[0..2422] of Integer = (
    -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    62, -1, -1, -1, -1, -1, -1, 133, -1, -1, -1, -1, -1, -1, -1, -1, 10, -1, -1, 
    -1, -1, -1, -1, 212, 189, -1, -1, -1, -1, -1, -1, -1, 111, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 55, -1, 242, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 34, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 77, 247, 
    -1, -1, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, -1, 
    155, -1, -1, -1, -1, -1, -1, -1, -1, 9, -1, -1, -1, -1, -1, -1, -1, 254, -1, 
    -1, -1, -1, -1, -1, -1, -1, 253, -1, 273, -1, -1, -1, 180, -1, -1, -1, -1, 
    41, -1, -1, 18, -1, 173, -1, -1, -1, -1, -1, -1, -1, -1, -1, 243, -1, 132, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 172, -1, 45, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 44, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 208, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 281, -1, 142, -1, -1, -1, -1, 233, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 23, -1, 7, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 87, 179, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    161, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 256, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 165, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 198, -1, -1, -1, -1, -1, 116, 124, -1, -1, 203, 47, -1, -1, -1, -1, 
    150, -1, -1, -1, 205, -1, -1, 152, -1, -1, 271, -1, -1, -1, -1, 76, 92, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 186, -1, -1, -1, 207, -1, -1, -1, 
    -1, -1, 72, -1, -1, -1, -1, -1, -1, -1, 175, -1, -1, -1, -1, -1, -1, 153, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, -1, 170, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 258, -1, -1, -1, -1, 99, -1, -1, -1, -1, 22, -1, -1, 33, -1, 
    -1, -1, -1, -1, -1, -1, -1, 135, -1, -1, -1, -1, -1, -1, -1, -1, 227, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    259, 228, -1, -1, -1, -1, 115, -1, -1, 215, -1, -1, -1, -1, -1, -1, -1, 167, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 158, 40, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 174, -1, 169, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 4, -1, -1, -1, 59, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 197, -1, -1, 32, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 261, -1, -1, 
    276, -1, -1, -1, -1, -1, -1, -1, -1, 266, -1, -1, -1, -1, 101, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 144, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 75, -1, -1, 38, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 134, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 190, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 262, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 239, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 202, -1, -1, 70, -1, -1, -1, -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, -1, 
    -1, -1, -1, 112, -1, -1, 20, -1, -1, -1, -1, -1, 238, -1, -1, 8, -1, 249, 
    -1, -1, -1, -1, -1, -1, 246, -1, 232, 216, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 146, 54, -1, -1, -1, -1, -1, -1, -1, -1, 39, -1, -1, -1, -1, -1, -1, 
    218, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    214, -1, -1, -1, -1, 277, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 89, 183, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6, -1, -1, -1, 79, -1, -1, -1, 
    -1, -1, 86, 63, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 53, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 267, 48, 131, 91, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 69, -1, -1, -1, -1, -1, 94, -1, -1, -1, -1, -1, -1, -1, 270, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, 166, -1, 73, -1, -1, -1, -1, -1, 
    -1, -1, 43, -1, -1, -1, -1, -1, -1, 279, -1, 26, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 68, -1, 280, -1, -1, -1, -1, 61, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 278, -1, 184, -1, -1, -1, -1, -1, -1, -1, -1, 206, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 264, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    163, -1, -1, -1, -1, 52, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 81, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 176, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 21, -1, -1, -1, -1, -1, 117, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 250, -1, -1, -1, -1, -1, -1, -1, 244, -1, -1, -1, 
    -1, -1, 129, -1, -1, -1, -1, -1, 95, -1, 234, -1, -1, -1, -1, -1, -1, -1, 
    -1, 231, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 230, -1, 138, -1, -1, 
    -1, -1, -1, 191, -1, 200, -1, -1, -1, 125, -1, -1, 268, 108, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 178, -1, -1, -1, -1, -1, -1, -1, 185, -1, -1, 66, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 194, -1, 222, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 143, -1, 226, 182, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 16, 
    -1, -1, -1, -1, -1, -1, 251, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, -1, -1, -1, -1, -1, -1, -1, -1, 
    113, -1, -1, -1, -1, -1, -1, -1, 37, -1, 71, -1, 15, -1, -1, -1, 154, 257, 
    -1, -1, -1, -1, 209, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 160, -1, -1, -1, 126, -1, -1, -1, -1, -1, 58, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 140, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    240, -1, -1, -1, -1, -1, -1, 241, -1, -1, -1, -1, -1, -1, 275, -1, -1, -1, 
    -1, -1, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, -1, 139, -1, -1, -1, -1, -1, 
    -1, -1, -1, 100, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, 177, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 223, -1, -1, -1, -1, -1, -1, 130, -1, -1, 97, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 224, -1, -1, -1, -1, -1, 196, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 120, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 114, -1, 148, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 168, -1, -1, -1, 274, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 137, 35, 159, -1, -1, -1, -1, -1, -1, -1, 260, -1, 
    -1, -1, -1, -1, 24, -1, 118, 245, -1, -1, 88, -1, -1, -1, -1, -1, -1, -1, 
    -1, 211, 119, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 187, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 217, -1, -1, -1, -1, -1, 237, -1, -1, -1, -1, 188, 147, 
    -1, 50, -1, -1, -1, -1, -1, -1, 103, -1, -1, -1, -1, -1, 96, 181, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, -1, -1, -1, 
    -1, 210, 27, -1, 136, -1, -1, 106, -1, -1, -1, -1, -1, -1, -1, 107, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 236, -1, -1, -1, 
    -1, 141, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 85, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 25, -1, 164, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 265, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 193, -1, -1, -1, -1, 67, -1, -1, -1, -1, -1, 
    121, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, 
    -1, -1, -1, 51, -1, -1, -1, -1, -1, -1, -1, -1, 151, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 98, 213, -1, -1, -1, -1, 5, 
    -1, 219, -1, -1, -1, -1, 162, -1, -1, -1, -1, -1, 74, -1, -1, -1, -1, -1, 
    -1, -1, 221, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 12, -1, 255, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 272, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 56, -1, -1, -1, -1, 83, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    82, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 195, 225, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, -1, -1, -1, -1, -1, -1, 171, 
    -1, -1, -1, 157, 149, -1, -1, -1, -1, -1, -1, 127, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 42, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 252, -1, -1, -1, 65, 28, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    229, -1, -1, -1, -1, -1, -1, -1, 199, -1, -1, -1, 105, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 64, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 248, -1, -1, -1, -1, 104, -1, -1, 
    -1, -1, -1, -1, 3, -1, -1, -1, -1, -1, -1, -1, 269, -1, -1, -1, -1, -1, -1, 
    -1, 220, 110, -1, -1, -1, 128, -1, -1, -1, -1, 235, 263, -1, -1, -1, -1, -1, 
    -1, -1, 201, -1, -1, -1, -1, -1, 29, -1, 156, -1, -1, -1, 19, -1, 123, -1, 
    204, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 122, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 57, -1, -1, 145, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 84, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 90, -1, -1, -1, -1, -1, 
    80, -1, -1, -1, -1 
  );

{$Q-}
function TSynPerlSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) or CharInSet(Str^, ['$', '%', '@']) do
  begin
    Result := Result * 975 + Ord(Str^) * 515;
    inc(Str);
  end;
  Result := Result mod 2423;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynPerlSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynPerlSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[368] := Func36accumulator;
  fIdentFuncTable[2] := Func36arg;
  fIdentFuncTable[804] := Func36argv;
  fIdentFuncTable[2272] := Func36basetime;
  fIdentFuncTable[626] := Func36child95error;
  fIdentFuncTable[2026] := Func36debugging;
  fIdentFuncTable[981] := Func36effective95group95id;
  fIdentFuncTable[317] := Func36effective95user95id;
  fIdentFuncTable[876] := Func36egid;
  fIdentFuncTable[141] := Func36env;
  fIdentFuncTable[35] := Func36errno;
  fIdentFuncTable[495] := Func36euid;
  fIdentFuncTable[2067] := Func36eval95error;
  fIdentFuncTable[1589] := Func36executable95name;
  fIdentFuncTable[1835] := Func36format95formfeed;
  fIdentFuncTable[1465] := Func36format95line95break95characters;
  fIdentFuncTable[1415] := Func36format95lines95left;
  fIdentFuncTable[201] := Func36format95lines95per95page;
  fIdentFuncTable[172] := Func36format95name;
  fIdentFuncTable[2319] := Func36format95page95number;
  fIdentFuncTable[867] := Func36format95top95name;
  fIdentFuncTable[1237] := Func36gid;
  fIdentFuncTable[519] := Func36inplace95edit;
  fIdentFuncTable[315] := Func36input95line95number;
  fIdentFuncTable[1733] := Func36input95record95separator;
  fIdentFuncTable[1923] := Func36last95paren95match;
  fIdentFuncTable[1093] := Func36list95separator;
  fIdentFuncTable[1841] := Func36match;
  fIdentFuncTable[2201] := Func36multiline95matching;
  fIdentFuncTable[2313] := Func36nr;
  fIdentFuncTable[2149] := Func36ofmt;
  fIdentFuncTable[955] := Func36ors;
  fIdentFuncTable[648] := Func36os95error;
  fIdentFuncTable[522] := Func36output95autoflush;
  fIdentFuncTable[97] := Func36output95field95separator;
  fIdentFuncTable[1718] := Func36perl95version;
  fIdentFuncTable[1568] := Func36perldb;
  fIdentFuncTable[1461] := Func36pid;
  fIdentFuncTable[723] := Func36postmatch;
  fIdentFuncTable[908] := Func36prematch;
  fIdentFuncTable[594] := Func36process95id;
  fIdentFuncTable[169] := Func36program95name;
  fIdentFuncTable[2182] := Func36real95group95id;
  fIdentFuncTable[1084] := Func36real95user95id;
  fIdentFuncTable[238] := Func36rs;
  fIdentFuncTable[220] := Func36sig;
  fIdentFuncTable[261] := Func36subscript95separator;
  fIdentFuncTable[427] := Func36subsep;
  fIdentFuncTable[1016] := Func36system95fd95max;
  fIdentFuncTable[856] := Func36uid;
  fIdentFuncTable[1803] := Func36warning;
  fIdentFuncTable[1992] := Func37inc;
  fIdentFuncTable[1181] := Func64argv;
  fIdentFuncTable[1004] := Func64inc;
  fIdentFuncTable[899] := FuncAbs;
  fIdentFuncTable[79] := FuncAccept;
  fIdentFuncTable[2102] := FuncAlarm;
  fIdentFuncTable[2365] := FuncAnd;
  fIdentFuncTable[1501] := FuncAtan2;
  fIdentFuncTable[630] := FuncBind;
  fIdentFuncTable[125] := FuncBinmode;
  fIdentFuncTable[1110] := FuncBless;
  fIdentFuncTable[19] := FuncCaller;
  fIdentFuncTable[992] := FuncChdir;
  fIdentFuncTable[2236] := FuncChmod;
  fIdentFuncTable[2200] := FuncChomp;
  fIdentFuncTable[1341] := FuncChop;
  fIdentFuncTable[1964] := FuncChown;
  fIdentFuncTable[1103] := FuncChr;
  fIdentFuncTable[1046] := FuncChroot;
  fIdentFuncTable[846] := FuncClose;
  fIdentFuncTable[1463] := FuncClosedir;
  fIdentFuncTable[470] := FuncCmp;
  fIdentFuncTable[1076] := FuncConnect;
  fIdentFuncTable[2039] := FuncConstant;
  fIdentFuncTable[720] := FuncCos;
  fIdentFuncTable[447] := FuncCrypt;
  fIdentFuncTable[111] := FuncDbmclose;
  fIdentFuncTable[1988] := FuncDbmopen;
  fIdentFuncTable[985] := FuncDefined;
  fIdentFuncTable[2418] := FuncDelete;
  fIdentFuncTable[1194] := FuncDiagnostics;
  fIdentFuncTable[2120] := FuncDie;
  fIdentFuncTable[2107] := FuncDo;
  fIdentFuncTable[2381] := FuncDump;
  fIdentFuncTable[1909] := FuncEach;
  fIdentFuncTable[991] := FuncElse;
  fIdentFuncTable[341] := FuncElsif;
  fIdentFuncTable[1739] := FuncEndgrent;
  fIdentFuncTable[967] := FuncEndhostent;
  fIdentFuncTable[2412] := FuncEndnetent;
  fIdentFuncTable[1018] := FuncEndprotoent;
  fIdentFuncTable[448] := FuncEndpwent;
  fIdentFuncTable[1681] := FuncEndservent;
  fIdentFuncTable[1052] := FuncEof;
  fIdentFuncTable[1278] := FuncEq;
  fIdentFuncTable[1816] := FuncEval;
  fIdentFuncTable[1618] := FuncExec;
  fIdentFuncTable[2020] := FuncExists;
  fIdentFuncTable[514] := FuncExit;
  fIdentFuncTable[1586] := FuncExp;
  fIdentFuncTable[686] := FuncFcntl;
  fIdentFuncTable[115] := FuncFileno;
  fIdentFuncTable[1810] := FuncFlock;
  fIdentFuncTable[2265] := FuncFor;
  fIdentFuncTable[2225] := FuncForeach;
  fIdentFuncTable[1846] := FuncFork;
  fIdentFuncTable[1854] := FuncFormat;
  fIdentFuncTable[1319] := FuncFormline;
  fIdentFuncTable[1072] := FuncGe;
  fIdentFuncTable[2289] := FuncGetc;
  fIdentFuncTable[51] := FuncGetgrent;
  fIdentFuncTable[864] := FuncGetgrgid;
  fIdentFuncTable[1453] := FuncGetgrnam;
  fIdentFuncTable[1663] := FuncGethostbyaddr;
  fIdentFuncTable[567] := FuncGethostbyname;
  fIdentFuncTable[422] := FuncGethostent;
  fIdentFuncTable[1243] := FuncGetlogin;
  fIdentFuncTable[1735] := FuncGetnetbyaddr;
  fIdentFuncTable[1749] := FuncGetnetbyname;
  fIdentFuncTable[1647] := FuncGetnetent;
  fIdentFuncTable[1970] := FuncGetpeername;
  fIdentFuncTable[2348] := FuncGetpgrp;
  fIdentFuncTable[2321] := FuncGetppid;
  fIdentFuncTable[423] := FuncGetpriority;
  fIdentFuncTable[1315] := FuncGetprotobyname;
  fIdentFuncTable[1495] := FuncGetprotobynumber;
  fIdentFuncTable[2168] := FuncGetprotoent;
  fIdentFuncTable[2293] := FuncGetpwent;
  fIdentFuncTable[1272] := FuncGetpwnam;
  fIdentFuncTable[1615] := FuncGetpwuid;
  fIdentFuncTable[1017] := FuncGetservbyname;
  fIdentFuncTable[186] := FuncGetservbyport;
  fIdentFuncTable[26] := FuncGetservent;
  fIdentFuncTable[737] := FuncGetsockname;
  fIdentFuncTable[531] := FuncGetsockopt;
  fIdentFuncTable[1843] := FuncGlob;
  fIdentFuncTable[1717] := FuncGmtime;
  fIdentFuncTable[1303] := FuncGoto;
  fIdentFuncTable[1577] := FuncGrep;
  fIdentFuncTable[1528] := FuncGt;
  fIdentFuncTable[1896] := FuncHex;
  fIdentFuncTable[292] := FuncIf;
  fIdentFuncTable[1381] := FuncImport;
  fIdentFuncTable[708] := FuncIndex;
  fIdentFuncTable[2368] := FuncInt;
  fIdentFuncTable[898] := FuncInteger;
  fIdentFuncTable[1801] := FuncIoctl;
  fIdentFuncTable[1665] := FuncJoin;
  fIdentFuncTable[2161] := FuncKeys;
  fIdentFuncTable[432] := FuncKill;
  fIdentFuncTable[2001] := FuncLast;
  fIdentFuncTable[439] := FuncLc;
  fIdentFuncTable[485] := FuncLcfirst;
  fIdentFuncTable[1469] := FuncLe;
  fIdentFuncTable[132] := FuncLength;
  fIdentFuncTable[2315] := FuncLess;
  fIdentFuncTable[2160] := FuncLink;
  fIdentFuncTable[593] := FuncListen;
  fIdentFuncTable[1719] := FuncLocal;
  fIdentFuncTable[1491] := FuncLocale;
  fIdentFuncTable[357] := FuncLocaltime;
  fIdentFuncTable[2033] := FuncLog;
  fIdentFuncTable[1176] := FuncLstat;
  fIdentFuncTable[1925] := FuncLt;
  fIdentFuncTable[406] := FuncM;
  fIdentFuncTable[1074] := FuncMap;
  fIdentFuncTable[578] := FuncMkdir;
  fIdentFuncTable[1701] := FuncMsgctl;
  fIdentFuncTable[613] := FuncMsgget;
  fIdentFuncTable[497] := FuncMsgrcv;
  fIdentFuncTable[2156] := FuncMsgsnd;
  fIdentFuncTable[218] := FuncMy;
  fIdentFuncTable[174] := FuncNe;
  fIdentFuncTable[611] := FuncNext;
  fIdentFuncTable[478] := FuncNo;
  fIdentFuncTable[1217] := FuncNot;
  fIdentFuncTable[1597] := FuncOct;
  fIdentFuncTable[1330] := FuncOpen;
  fIdentFuncTable[342] := FuncOpendir;
  fIdentFuncTable[164] := FuncOr;
  fIdentFuncTable[1817] := FuncOrd;
  fIdentFuncTable[1384] := FuncPack;
  fIdentFuncTable[968] := FuncPackage;
  fIdentFuncTable[1125] := FuncPipe;
  fIdentFuncTable[1338] := FuncPop;
  fIdentFuncTable[460] := FuncPos;
  fIdentFuncTable[1768] := FuncPrint;
  fIdentFuncTable[1800] := FuncPush;
  fIdentFuncTable[43] := FuncQ;
  fIdentFuncTable[777] := FuncQq;
  fIdentFuncTable[1309] := FuncQuotemeta;
  fIdentFuncTable[1444] := FuncQw;
  fIdentFuncTable[1959] := FuncQx;
  fIdentFuncTable[1367] := FuncRand;
  fIdentFuncTable[2133] := FuncRead;
  fIdentFuncTable[1635] := FuncReaddir;
  fIdentFuncTable[645] := FuncReadlink;
  fIdentFuncTable[416] := FuncRecv;
  fIdentFuncTable[2221] := FuncRedo;
  fIdentFuncTable[1311] := FuncRef;
  fIdentFuncTable[2307] := FuncRename;
  fIdentFuncTable[843] := FuncRequire;
  fIdentFuncTable[426] := FuncReset;
  fIdentFuncTable[2323] := FuncReturn;
  fIdentFuncTable[436] := FuncReverse;
  fIdentFuncTable[1134] := FuncRewinddir;
  fIdentFuncTable[464] := FuncRindex;
  fIdentFuncTable[272] := FuncRmdir;
  fIdentFuncTable[1475] := FuncScalar;
  fIdentFuncTable[1840] := FuncSeek;
  fIdentFuncTable[1748] := FuncSeekdir;
  fIdentFuncTable[42] := FuncSelect;
  fIdentFuncTable[2021] := FuncSemctl;
  fIdentFuncTable[933] := FuncSemget;
  fIdentFuncTable[570] := FuncSemop;
  fIdentFuncTable[888] := FuncSend;
  fIdentFuncTable[1789] := FuncSetgrent;
  fIdentFuncTable[915] := FuncSethostent;
  fIdentFuncTable[2028] := FuncSetnetent;
  fIdentFuncTable[2288] := FuncSetpgrp;
  fIdentFuncTable[2047] := FuncSetpriority;
  fIdentFuncTable[1369] := FuncSetprotoent;
  fIdentFuncTable[1608] := FuncSetpwent;
  fIdentFuncTable[1629] := FuncSetservent;
  fIdentFuncTable[2134] := FuncSetsockopt;
  fIdentFuncTable[1383] := FuncShift;
  fIdentFuncTable[540] := FuncShmctl;
  fIdentFuncTable[562] := FuncShmget;
  fIdentFuncTable[2213] := FuncShmread;
  fIdentFuncTable[1301] := FuncShmwrite;
  fIdentFuncTable[1289] := FuncShutdown;
  fIdentFuncTable[887] := FuncSigtrap;
  fIdentFuncTable[297] := FuncSin;
  fIdentFuncTable[1280] := FuncSleep;
  fIdentFuncTable[2298] := FuncSocket;
  fIdentFuncTable[1891] := FuncSocketpair;
  fIdentFuncTable[1795] := FuncSort;
  fIdentFuncTable[873] := FuncSplice;
  fIdentFuncTable[830] := FuncSplit;
  fIdentFuncTable[1546] := FuncSprintf;
  fIdentFuncTable[1553] := FuncSqrt;
  fIdentFuncTable[81] := FuncSrand;
  fIdentFuncTable[184] := FuncStat;
  fIdentFuncTable[1266] := FuncStrict;
  fIdentFuncTable[1736] := FuncStudy;
  fIdentFuncTable[885] := FuncSub;
  fIdentFuncTable[112] := FuncSubs;
  fIdentFuncTable[2260] := FuncSubstr;
  fIdentFuncTable[878] := FuncSymlink;
  fIdentFuncTable[1258] := FuncSyscall;
  fIdentFuncTable[1422] := FuncSysread;
  fIdentFuncTable[2196] := FuncSystem;
  fIdentFuncTable[158] := FuncSyswrite;
  fIdentFuncTable[149] := FuncTell;
  fIdentFuncTable[2069] := FuncTelldir;
  fIdentFuncTable[387] := FuncTie;
  fIdentFuncTable[1470] := FuncTime;
  fIdentFuncTable[509] := FuncTimes;
  fIdentFuncTable[561] := FuncTr;
  fIdentFuncTable[1727] := FuncTruncate;
  fIdentFuncTable[669] := FuncUc;
  fIdentFuncTable[819] := FuncUcfirst;
  fIdentFuncTable[2299] := FuncUmask;
  fIdentFuncTable[1162] := FuncUndef;
  fIdentFuncTable[1946] := FuncUnless;
  fIdentFuncTable[681] := FuncUnlink;
  fIdentFuncTable[1015] := FuncUnpack;
  fIdentFuncTable[1318] := FuncUnshift;
  fIdentFuncTable[2280] := FuncUntie;
  fIdentFuncTable[1060] := FuncUse;
  fIdentFuncTable[442] := FuncUtime;
  fIdentFuncTable[2080] := FuncValues;
  fIdentFuncTable[160] := FuncVars;
  fIdentFuncTable[1705] := FuncVec;
  fIdentFuncTable[1560] := FuncWait;
  fIdentFuncTable[672] := FuncWaitpid;
  fIdentFuncTable[938] := FuncWantarray;
  fIdentFuncTable[1123] := FuncWarn;
  fIdentFuncTable[1091] := FuncWhile;
  fIdentFuncTable[1105] := FuncWrite;
  fIdentFuncTable[290] := FuncXor;
end;

function TSynPerlSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPerlSyn.Func36accumulator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36arg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36argv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36basetime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36child95error(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36debugging(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36effective95group95id(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36effective95user95id(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36egid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36env(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36errno(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36euid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36eval95error(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36executable95name(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36format95formfeed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36format95line95break95characters(Index: Integer):
  TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36format95lines95left(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36format95lines95per95page(Index: Integer):
  TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36format95name(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36format95page95number(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36format95top95name(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36gid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36inplace95edit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36input95line95number(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36input95record95separator(Index: Integer):
  TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36last95paren95match(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36list95separator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36match(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36multiline95matching(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36nr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36ofmt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36ors(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36os95error(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36output95autoflush(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36output95field95separator(Index: Integer):
  TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36perl95version(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36perldb(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36pid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36postmatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36prematch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36process95id(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36program95name(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36real95group95id(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36real95user95id(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36rs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36sig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36subscript95separator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36subsep(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36system95fd95max(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36uid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func36warning(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func37inc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func64argv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.Func64inc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncAbs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncAccept(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncAlarm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncAnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncAtan2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncBind(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncBinmode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncBless(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncCaller(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncChdir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncChmod(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncChomp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncChop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncChown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncChr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncChroot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncClose(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncClosedir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncCmp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncConnect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncConstant(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncCos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncCrypt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncDbmclose(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncDbmopen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncDefined(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncDelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncDiagnostics(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncDie(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncDump(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEach(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncElsif(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEndgrent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEndhostent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEndnetent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEndprotoent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEndpwent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEndservent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEq(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncEval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncExec(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncExists(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncExit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncExp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncFcntl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncFileno(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncFlock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncForeach(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncFork(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncFormat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncFormline(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetgrent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetgrgid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetgrnam(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGethostbyaddr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGethostbyname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGethostent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetlogin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetnetbyaddr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetnetbyname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetnetent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetpeername(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetpgrp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetppid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetpriority(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetprotobyname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetprotobynumber(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetprotoent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetpwent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetpwnam(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetpwuid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetservbyname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetservbyport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetservent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetsockname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGetsockopt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGlob(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGmtime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGrep(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncGt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncHex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncImport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncIndex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncInteger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncIoctl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncJoin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncKeys(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncKill(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLast(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLcfirst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLength(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLess(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncListen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLocale(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLocaltime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLog(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLstat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncLt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncM(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncMap(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncMkdir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncMsgctl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncMsgget(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncMsgrcv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncMsgsnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncMy(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncNe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncNext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncNo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncNot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncOct(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncOpen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncOpendir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncOr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncOrd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncPack(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncPackage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncPipe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncPop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncPos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncPrint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncPush(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncQ(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncQq(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncQuotemeta(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncQw(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncQx(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRand(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRead(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncReaddir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncReadlink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRecv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRedo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRef(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRename(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRequire(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncReset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncReverse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRewinddir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRindex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncRmdir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncScalar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSeek(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSeekdir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSelect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSemctl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSemget(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSemop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSend(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSetgrent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSethostent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSetnetent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSetpgrp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSetpriority(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSetprotoent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSetpwent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSetservent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSetsockopt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncShift(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncShmctl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncShmget(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncShmread(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncShmwrite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncShutdown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSigtrap(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSleep(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSocket(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSocketpair(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSplice(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSplit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSprintf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSqrt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSrand(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncStat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncStrict(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncStudy(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSub(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSubs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSubstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSymlink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSyscall(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSysread(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSystem(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncSyswrite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncTell(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncTelldir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncTie(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncTime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncTimes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncTr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncTruncate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUcfirst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUmask(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUndef(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUnless(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUnlink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUnpack(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUnshift(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUntie(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncUtime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncValues(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncVars(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncVec(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncWait(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncWaitpid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncWantarray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncWarn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncWrite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncXor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

constructor TSynPerlSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fOperatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrOperator, SYNS_FriendlyAttrOperator);
  AddAttribute(fOperatorAttri);
  fPragmaAttri := TSynHighlighterAttributes.Create(SYNS_AttrPragma, SYNS_FriendlyAttrPragma);
  fPragmaAttri.Style := [fsBold];
  AddAttribute(fPragmaAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  fVariableAttri.Style := [fsBold];
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterPerl;
end; { Create }

procedure TSynPerlSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {bit and assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '&':
      begin
        if FLine[Run + 2] = '=' then   {logical and assign}
          inc(Run, 3)
        else                           {logical and}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {bit and}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynPerlSyn.ColonProc;
begin
  Case FLine[Run + 1] of
    ':':                               {double colon}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {colon}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #0;
end;

procedure TSynPerlSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {digraph}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '~':                               {bind scalar to pattern}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.GreaterProc;
begin
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
          inc(Run, 3)
        else                           {shift right}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.IdentProc;
begin
  case FLine[Run] of
    '$':
      begin
        Case FLine[Run + 1] of
          '!'..'+', '-'..'@', '['..']', '_', '`', '|', '~':
            begin                      {predefined variables}
              inc(Run, 2);
              fTokenID := tkVariable;
              exit;
            end;
          '^':
            begin
              Case FLine[Run + 2] of
                'A', 'D', 'F', 'I', 'L', 'P', 'T', 'W', 'X':
                  begin                {predefined variables}
                    inc(Run, 3);
                    fTokenID := tkVariable;
                    exit;
                  end;
                #0, #10, #13:          {predefined variables}
                  begin
                    inc(Run, 2);
                    fTokenID := tkVariable;
                    exit;
                  end;
              end;
            end;
        end;
      end;
    '%':
      begin
        Case FLine[Run + 1] of
          '=':                         {mod assign}
            begin
              inc(Run, 2);
              fTokenID := tkSymbol;
              exit;
            end;
          #0, #10, #13:                {mod}
            begin
              inc(Run);
              fTokenID := tkSymbol;
              exit;
            end;
        end;
      end;
    'x':
      begin
        Case FLine[Run + 1] of
          '=':                         {repetition assign}
            begin
              inc(Run, 2);
              fTokenID := tkSymbol;
              exit;
            end;
          #0, #10, #13:                {repetition}
            begin
              inc(Run);
              fTokenID := tkSymbol;
              exit;
            end;
        end;
      end;
  end;
  {regular identifier}
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynPerlSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPerlSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':
      begin
        if FLine[Run + 2] = '>' then   {compare - less than, equal, greater}
          inc(Run, 3)
        else                           {less than or equal to}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
          inc(Run, 3)
        else                           {shift left}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {arrow}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '~':                               {logical negated bind like =~}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '=':                               {not equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {not}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynPerlSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '-', '_', '.', 'A'..'F', 'a'..'f', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if FLine[Run] = '.' then
  begin
    case FLine[Run + 1] of
      '.':
        begin
          inc(Run, 2);
          if FLine[Run] = '.' then     {sed range}
            inc(Run);

          fTokenID := tkSymbol;        {range}
          exit;
        end;
      '=':
        begin
          inc(Run, 2);
          fTokenID := tkSymbol;        {concatenation assign}
          exit;
        end;
      'a'..'z', 'A'..'Z', '_':
        begin
          fTokenID := tkSymbol;        {concatenation}
          inc(Run);
          exit;
        end;
    end;
  end;
  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
      '-':                             {check for e notation}
        if not ((FLine[Run + 1] = 'e') or (FLine[Run + 1] = 'E')) then break;
    end;
    inc(Run);
  end;
end;

procedure TSynPerlSyn.OrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {bit or assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '|':
      begin
        if FLine[Run + 2] = '=' then   {logical or assign}
          inc(Run, 3)
        else                           {logical or}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {bit or}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {add}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '=':                               {division assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynPerlSyn.StarProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '*':
      begin
        if FLine[Run + 2] = '=' then   {exponentiation assign}
          inc(Run, 3)
        else                           {exponentiation}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {multiply}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.StringInterpProc;
var
  fBackslashCount : Integer;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        { If we're looking at a backslash, and the following character is an
          end quote, and it's preceeded by an odd number of backslashes, then
          it shouldn't mark the end of the string.  If it's preceeded by an
          even number, then it should. }
        if (FLine[Run + 1] = #34) then
          begin
            fBackslashCount := 1;

            while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = #92)) do
              fBackslashCount := fBackslashCount + 1;

            if (fBackslashCount mod 2 = 1) then inc(Run)
          end;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPerlSyn.StringLiteralProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPerlSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPerlSyn.XOrSymbolProc;
begin
  Case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPerlSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    '&': AndSymbolProc;
    #13: CRProc;
    ':': ColonProc;
    '#': CommentProc;
    '=': EqualProc;
    '>': GreaterProc;
    '%', '@', '$', 'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9', '.': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '*': StarProc;
    #34: StringInterpProc;
    #39: StringLiteralProc;
    '^': XOrSymbolProc;
    '(', ')', '[', ']', '\', '{', '}', ',', ';', '?', '~': SymbolProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynPerlSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynPerlSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynPerlSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPerlSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkOperator: Result := fOperatorAttri;
    tkPragma: Result := fPragmaAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    tkVariable: Result := fVariableAttri;
    else Result := nil;
  end;
end;

function TSynPerlSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynPerlSyn.GetSampleSource: UnicodeString;
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

function TSynPerlSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPerl;
end;

function TSynPerlSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '%', '@', '$', '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynPerlSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPerl;
end;

class function TSynPerlSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPerl;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynPerlSyn);
{$ENDIF}
end.
