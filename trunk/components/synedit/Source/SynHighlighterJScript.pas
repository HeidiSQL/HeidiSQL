{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJScript.pas, released 2000-04-14.
The Original Code is based on the mwJScript.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Tony de Buys.
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

$Id: SynHighlighterJScript.pas,v 1.21.2.7 2005/12/16 16:10:37 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a JavaScript/JScript highlighter for SynEdit)
@author(Tony De Buys [tony@lad.co.za], converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(December 1999, converted to SynEdit April 14, 2000)
@lastmod(2000-06-23)
The SynHighlighterJScript unit provides SynEdit with a JScript/JavaScript (.js) highlighter.
The highlighter formats JavaScript source code highlighting keywords, strings, numbers and characters.
}

{$IFNDEF QSYNHIGHLIGHTERJSCRIPT}
unit SynHighlighterJScript;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils, Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown, tkNonReservedKey, tkEvent);

  TRangeState = (rsUnknown, rsANSI);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynJScriptSyn = class(TSynCustomHighLighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..5152] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNonReservedKeyAttri: TSynHighlighterAttributes;
    fEventAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAbs(Index: Integer): TtkTokenKind;
    function FuncAbstract(Index: Integer): TtkTokenKind;
    function FuncAcos(Index: Integer): TtkTokenKind;
    function FuncAction(Index: Integer): TtkTokenKind;
    function FuncAlert(Index: Integer): TtkTokenKind;
    function FuncAlign(Index: Integer): TtkTokenKind;
    function FuncAlinkcolor(Index: Integer): TtkTokenKind;
    function FuncAll(Index: Integer): TtkTokenKind;
    function FuncAnchor(Index: Integer): TtkTokenKind;
    function FuncAnchors(Index: Integer): TtkTokenKind;
    function FuncAppcodename(Index: Integer): TtkTokenKind;
    function FuncApplet(Index: Integer): TtkTokenKind;
    function FuncApplets(Index: Integer): TtkTokenKind;
    function FuncAppname(Index: Integer): TtkTokenKind;
    function FuncAppversion(Index: Integer): TtkTokenKind;
    function FuncArea(Index: Integer): TtkTokenKind;
    function FuncArguments(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncAsin(Index: Integer): TtkTokenKind;
    function FuncAtan(Index: Integer): TtkTokenKind;
    function FuncAtan2(Index: Integer): TtkTokenKind;
    function FuncBack(Index: Integer): TtkTokenKind;
    function FuncBackground(Index: Integer): TtkTokenKind;
    function FuncBgcolor(Index: Integer): TtkTokenKind;
    function FuncBig(Index: Integer): TtkTokenKind;
    function FuncBlink(Index: Integer): TtkTokenKind;
    function FuncBlur(Index: Integer): TtkTokenKind;
    function FuncBody(Index: Integer): TtkTokenKind;
    function FuncBold(Index: Integer): TtkTokenKind;
    function FuncBoolean(Index: Integer): TtkTokenKind;
    function FuncBoolean2(Index: Integer): TtkTokenKind;
    function FuncBorder(Index: Integer): TtkTokenKind;
    function FuncBottom(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncButton(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncCall(Index: Integer): TtkTokenKind;
    function FuncCallee(Index: Integer): TtkTokenKind;
    function FuncCaller(Index: Integer): TtkTokenKind;
    function FuncCaptureevents(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCatch(Index: Integer): TtkTokenKind;
    function FuncCeil(Index: Integer): TtkTokenKind;
    function FuncChar(Index: Integer): TtkTokenKind;
    function FuncCharat(Index: Integer): TtkTokenKind;
    function FuncCharcodeat(Index: Integer): TtkTokenKind;
    function FuncCheckbox(Index: Integer): TtkTokenKind;
    function FuncChecked(Index: Integer): TtkTokenKind;
    function FuncClass(Index: Integer): TtkTokenKind;
    function FuncClear(Index: Integer): TtkTokenKind;
    function FuncClearinterval(Index: Integer): TtkTokenKind;
    function FuncCleartimeout(Index: Integer): TtkTokenKind;
    function FuncClick(Index: Integer): TtkTokenKind;
    function FuncClose(Index: Integer): TtkTokenKind;
    function FuncClosed(Index: Integer): TtkTokenKind;
    function FuncColor(Index: Integer): TtkTokenKind;
    function FuncComplete(Index: Integer): TtkTokenKind;
    function FuncConcat(Index: Integer): TtkTokenKind;
    function FuncConfirm(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncConstructor(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncCookie(Index: Integer): TtkTokenKind;
    function FuncCos(Index: Integer): TtkTokenKind;
    function FuncCurrent(Index: Integer): TtkTokenKind;
    function FuncDate(Index: Integer): TtkTokenKind;
    function FuncDebugger(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDefaultchecked(Index: Integer): TtkTokenKind;
    function FuncDefaultselected(Index: Integer): TtkTokenKind;
    function FuncDefaultstatus(Index: Integer): TtkTokenKind;
    function FuncDefaultvalue(Index: Integer): TtkTokenKind;
    function FuncDelete(Index: Integer): TtkTokenKind;
    function FuncDescription(Index: Integer): TtkTokenKind;
    function FuncDisplay(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDocument(Index: Integer): TtkTokenKind;
    function FuncDomain(Index: Integer): TtkTokenKind;
    function FuncDouble(Index: Integer): TtkTokenKind;
    function FuncE(Index: Integer): TtkTokenKind;
    function FuncElements(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEmbed(Index: Integer): TtkTokenKind;
    function FuncEmbeds(Index: Integer): TtkTokenKind;
    function FuncEnabledplugin(Index: Integer): TtkTokenKind;
    function FuncEncoding(Index: Integer): TtkTokenKind;
    function FuncEnum(Index: Integer): TtkTokenKind;
    function FuncEscape(Index: Integer): TtkTokenKind;
    function FuncEval(Index: Integer): TtkTokenKind;
    function FuncEvent(Index: Integer): TtkTokenKind;
    function FuncExp(Index: Integer): TtkTokenKind;
    function FuncExport(Index: Integer): TtkTokenKind;
    function FuncExtends(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFgcolor(Index: Integer): TtkTokenKind;
    function FuncFilename(Index: Integer): TtkTokenKind;
    function FuncFileupload(Index: Integer): TtkTokenKind;
    function FuncFinal(Index: Integer): TtkTokenKind;
    function FuncFinally(Index: Integer): TtkTokenKind;
    function FuncFind(Index: Integer): TtkTokenKind;
    function FuncFixed(Index: Integer): TtkTokenKind;
    function FuncFloat(Index: Integer): TtkTokenKind;
    function FuncFloat2(Index: Integer): TtkTokenKind;
    function FuncFloor(Index: Integer): TtkTokenKind;
    function FuncFocus(Index: Integer): TtkTokenKind;
    function FuncFontcolor(Index: Integer): TtkTokenKind;
    function FuncFontsize(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncForm(Index: Integer): TtkTokenKind;
    function FuncForms(Index: Integer): TtkTokenKind;
    function FuncForward(Index: Integer): TtkTokenKind;
    function FuncFrame(Index: Integer): TtkTokenKind;
    function FuncFrames(Index: Integer): TtkTokenKind;
    function FuncFromcharcode(Index: Integer): TtkTokenKind;
    function FuncFunction(Index: Integer): TtkTokenKind;
    function FuncFunction2(Index: Integer): TtkTokenKind;    
    function FuncGetdate(Index: Integer): TtkTokenKind;
    function FuncGetday(Index: Integer): TtkTokenKind;
    function FuncGetelementbyid(Index: Integer): TtkTokenKind;
    function FuncGetfullyear(Index: Integer): TtkTokenKind;
    function FuncGethours(Index: Integer): TtkTokenKind;
    function FuncGetmilliseconds(Index: Integer): TtkTokenKind;
    function FuncGetminutes(Index: Integer): TtkTokenKind;
    function FuncGetmonth(Index: Integer): TtkTokenKind;
    function FuncGetseconds(Index: Integer): TtkTokenKind;
    function FuncGettime(Index: Integer): TtkTokenKind;
    function FuncGettimezoneoffset(Index: Integer): TtkTokenKind;
    function FuncGetutcdate(Index: Integer): TtkTokenKind;
    function FuncGetutcday(Index: Integer): TtkTokenKind;
    function FuncGetutcfullyear(Index: Integer): TtkTokenKind;
    function FuncGetutchours(Index: Integer): TtkTokenKind;
    function FuncGetutcmilliseconds(Index: Integer): TtkTokenKind;
    function FuncGetutcminutes(Index: Integer): TtkTokenKind;
    function FuncGetutcmonth(Index: Integer): TtkTokenKind;
    function FuncGetutcseconds(Index: Integer): TtkTokenKind;
    function FuncGetyear(Index: Integer): TtkTokenKind;
    function FuncGlobal(Index: Integer): TtkTokenKind;
    function FuncGo(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncHandleevent(Index: Integer): TtkTokenKind;
    function FuncHash(Index: Integer): TtkTokenKind;
    function FuncHeight(Index: Integer): TtkTokenKind;
    function FuncHidden(Index: Integer): TtkTokenKind;
    function FuncHistory(Index: Integer): TtkTokenKind;
    function FuncHome(Index: Integer): TtkTokenKind;
    function FuncHost(Index: Integer): TtkTokenKind;
    function FuncHostname(Index: Integer): TtkTokenKind;
    function FuncHref(Index: Integer): TtkTokenKind;
    function FuncHspace(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncImage(Index: Integer): TtkTokenKind;
    function FuncImages(Index: Integer): TtkTokenKind;
    function FuncImplements(Index: Integer): TtkTokenKind;
    function FuncImport(Index: Integer): TtkTokenKind;
    function FuncIn(Index: Integer): TtkTokenKind;
    function FuncIndex(Index: Integer): TtkTokenKind;
    function FuncIndexof(Index: Integer): TtkTokenKind;
    function FuncInfinity(Index: Integer): TtkTokenKind;
    function FuncInnerheight(Index: Integer): TtkTokenKind;
    function FuncInnerwidth(Index: Integer): TtkTokenKind;
    function FuncInput(Index: Integer): TtkTokenKind;
    function FuncInstanceof(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncInterface(Index: Integer): TtkTokenKind;
    function FuncIsfinite(Index: Integer): TtkTokenKind;
    function FuncIsnan(Index: Integer): TtkTokenKind;
    function FuncItalics(Index: Integer): TtkTokenKind;
    function FuncJava(Index: Integer): TtkTokenKind;
    function FuncJavaenabled(Index: Integer): TtkTokenKind;
    function FuncJoin(Index: Integer): TtkTokenKind;
    function FuncLastindexof(Index: Integer): TtkTokenKind;
    function FuncLastmodified(Index: Integer): TtkTokenKind;
    function FuncLayer(Index: Integer): TtkTokenKind;
    function FuncLayers(Index: Integer): TtkTokenKind;
    function FuncLeft(Index: Integer): TtkTokenKind;
    function FuncLength(Index: Integer): TtkTokenKind;
    function FuncLink(Index: Integer): TtkTokenKind;
    function FuncLinkcolor(Index: Integer): TtkTokenKind;
    function FuncLinks(Index: Integer): TtkTokenKind;
    function FuncLn10(Index: Integer): TtkTokenKind;
    function FuncLn2(Index: Integer): TtkTokenKind;
    function FuncLocation(Index: Integer): TtkTokenKind;
    function FuncLocationbar(Index: Integer): TtkTokenKind;
    function FuncLog(Index: Integer): TtkTokenKind;
    function FuncLog10e(Index: Integer): TtkTokenKind;
    function FuncLog2e(Index: Integer): TtkTokenKind;
    function FuncLogon(Index: Integer): TtkTokenKind;
    function FuncLong(Index: Integer): TtkTokenKind;
    function FuncLowsrc(Index: Integer): TtkTokenKind;
    function FuncMatch(Index: Integer): TtkTokenKind;
    function FuncMath(Index: Integer): TtkTokenKind;
    function FuncMax(Index: Integer): TtkTokenKind;
    function FuncMax_value(Index: Integer): TtkTokenKind;
    function FuncMenubar(Index: Integer): TtkTokenKind;
    function FuncMethod(Index: Integer): TtkTokenKind;
    function FuncMimetype(Index: Integer): TtkTokenKind;
    function FuncMimetypes(Index: Integer): TtkTokenKind;
    function FuncMin(Index: Integer): TtkTokenKind;
    function FuncMin_value(Index: Integer): TtkTokenKind;
    function FuncMoveby(Index: Integer): TtkTokenKind;
    function FuncMoveto(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNan(Index: Integer): TtkTokenKind;
    function FuncNative(Index: Integer): TtkTokenKind;
    function FuncNavigator(Index: Integer): TtkTokenKind;
    function FuncNegative_infinity(Index: Integer): TtkTokenKind;
    function FuncNetscape(Index: Integer): TtkTokenKind;
    function FuncNew(Index: Integer): TtkTokenKind;
    function FuncNext(Index: Integer): TtkTokenKind;
    function FuncNull(Index: Integer): TtkTokenKind;
    function FuncNull2(Index: Integer): TtkTokenKind;
    function FuncNumber(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncOnabort(Index: Integer): TtkTokenKind;
    function FuncOnblur(Index: Integer): TtkTokenKind;
    function FuncOnchange(Index: Integer): TtkTokenKind;
    function FuncOnclick(Index: Integer): TtkTokenKind;
    function FuncOndblclick(Index: Integer): TtkTokenKind;
    function FuncOnerror(Index: Integer): TtkTokenKind;
    function FuncOnfocus(Index: Integer): TtkTokenKind;
    function FuncOnkeydown(Index: Integer): TtkTokenKind;
    function FuncOnkeypress(Index: Integer): TtkTokenKind;
    function FuncOnkeyup(Index: Integer): TtkTokenKind;
    function FuncOnload(Index: Integer): TtkTokenKind;
    function FuncOnmousedown(Index: Integer): TtkTokenKind;
    function FuncOnmousemove(Index: Integer): TtkTokenKind;
    function FuncOnmouseout(Index: Integer): TtkTokenKind;
    function FuncOnmouseover(Index: Integer): TtkTokenKind;
    function FuncOnmouseup(Index: Integer): TtkTokenKind;
    function FuncOnreset(Index: Integer): TtkTokenKind;
    function FuncOnselect(Index: Integer): TtkTokenKind;
    function FuncOnsubmit(Index: Integer): TtkTokenKind;
    function FuncOnunload(Index: Integer): TtkTokenKind;
    function FuncOpen(Index: Integer): TtkTokenKind;
    function FuncOpener(Index: Integer): TtkTokenKind;
    function FuncOption(Index: Integer): TtkTokenKind;
    function FuncOptions(Index: Integer): TtkTokenKind;
    function FuncOuterheight(Index: Integer): TtkTokenKind;
    function FuncOuterwidth(Index: Integer): TtkTokenKind;
    function FuncPackage(Index: Integer): TtkTokenKind;
    function FuncPackages(Index: Integer): TtkTokenKind;
    function FuncPagex(Index: Integer): TtkTokenKind;
    function FuncPagexoffset(Index: Integer): TtkTokenKind;
    function FuncPagey(Index: Integer): TtkTokenKind;
    function FuncPageyoffset(Index: Integer): TtkTokenKind;
    function FuncParent(Index: Integer): TtkTokenKind;
    function FuncParse(Index: Integer): TtkTokenKind;
    function FuncParsefloat(Index: Integer): TtkTokenKind;
    function FuncParseint(Index: Integer): TtkTokenKind;
    function FuncPassword(Index: Integer): TtkTokenKind;
    function FuncPathname(Index: Integer): TtkTokenKind;
    function FuncPersonalbar(Index: Integer): TtkTokenKind;
    function FuncPi(Index: Integer): TtkTokenKind;
    function FuncPlatform(Index: Integer): TtkTokenKind;
    function FuncPlugin(Index: Integer): TtkTokenKind;
    function FuncPlugins(Index: Integer): TtkTokenKind;
    function FuncPort(Index: Integer): TtkTokenKind;
    function FuncPositive_infinity(Index: Integer): TtkTokenKind;
    function FuncPow(Index: Integer): TtkTokenKind;
    function FuncPrevious(Index: Integer): TtkTokenKind;
    function FuncPrint(Index: Integer): TtkTokenKind;
    function FuncPrivate(Index: Integer): TtkTokenKind;
    function FuncPrompt(Index: Integer): TtkTokenKind;
    function FuncProtected(Index: Integer): TtkTokenKind;
    function FuncProtocol(Index: Integer): TtkTokenKind;
    function FuncPrototype(Index: Integer): TtkTokenKind;
    function FuncPublic(Index: Integer): TtkTokenKind;
    function FuncRadio(Index: Integer): TtkTokenKind;
    function FuncRandom(Index: Integer): TtkTokenKind;
    function FuncReferrer(Index: Integer): TtkTokenKind;
    function FuncRefresh(Index: Integer): TtkTokenKind;
    function FuncRegexp(Index: Integer): TtkTokenKind;
    function FuncReleaseevents(Index: Integer): TtkTokenKind;
    function FuncReload(Index: Integer): TtkTokenKind;
    function FuncReplace(Index: Integer): TtkTokenKind;
    function FuncReset(Index: Integer): TtkTokenKind;
    function FuncResizeby(Index: Integer): TtkTokenKind;
    function FuncResizeto(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncReverse(Index: Integer): TtkTokenKind;
    function FuncRight(Index: Integer): TtkTokenKind;
    function FuncRound(Index: Integer): TtkTokenKind;
    function FuncRouteevent(Index: Integer): TtkTokenKind;
    function FuncScreen(Index: Integer): TtkTokenKind;
    function FuncScroll(Index: Integer): TtkTokenKind;
    function FuncScrollbars(Index: Integer): TtkTokenKind;
    function FuncScrollby(Index: Integer): TtkTokenKind;
    function FuncScrollto(Index: Integer): TtkTokenKind;
    function FuncSearch(Index: Integer): TtkTokenKind;
    function FuncSelect(Index: Integer): TtkTokenKind;
    function FuncSelected(Index: Integer): TtkTokenKind;
    function FuncSelectedindex(Index: Integer): TtkTokenKind;
    function FuncSelf(Index: Integer): TtkTokenKind;
    function FuncSetdate(Index: Integer): TtkTokenKind;
    function FuncSetfullyear(Index: Integer): TtkTokenKind;
    function FuncSethours(Index: Integer): TtkTokenKind;
    function FuncSetinterval(Index: Integer): TtkTokenKind;
    function FuncSetmilliseconds(Index: Integer): TtkTokenKind;
    function FuncSetminutes(Index: Integer): TtkTokenKind;
    function FuncSetmonth(Index: Integer): TtkTokenKind;
    function FuncSetseconds(Index: Integer): TtkTokenKind;
    function FuncSettime(Index: Integer): TtkTokenKind;
    function FuncSettimeout(Index: Integer): TtkTokenKind;
    function FuncSetutcdate(Index: Integer): TtkTokenKind;
    function FuncSetutcfullyear(Index: Integer): TtkTokenKind;
    function FuncSetutchours(Index: Integer): TtkTokenKind;
    function FuncSetutcmilliseconds(Index: Integer): TtkTokenKind;
    function FuncSetutcminutes(Index: Integer): TtkTokenKind;
    function FuncSetutcmonth(Index: Integer): TtkTokenKind;
    function FuncSetutcseconds(Index: Integer): TtkTokenKind;
    function FuncSetyear(Index: Integer): TtkTokenKind;
    function FuncShort(Index: Integer): TtkTokenKind;
    function FuncSin(Index: Integer): TtkTokenKind;
    function FuncSlice(Index: Integer): TtkTokenKind;
    function FuncSmall(Index: Integer): TtkTokenKind;
    function FuncSort(Index: Integer): TtkTokenKind;
    function FuncSplit(Index: Integer): TtkTokenKind;
    function FuncSqrt(Index: Integer): TtkTokenKind;
    function FuncSqrt1_2(Index: Integer): TtkTokenKind;
    function FuncSqrt2(Index: Integer): TtkTokenKind;
    function FuncSrc(Index: Integer): TtkTokenKind;
    function FuncStart(Index: Integer): TtkTokenKind;
    function FuncStatic(Index: Integer): TtkTokenKind;
    function FuncStatus(Index: Integer): TtkTokenKind;
    function FuncStatusbar(Index: Integer): TtkTokenKind;
    function FuncStop(Index: Integer): TtkTokenKind;
    function FuncStrike(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStyle(Index: Integer): TtkTokenKind;
    function FuncSub(Index: Integer): TtkTokenKind;
    function FuncSubmit(Index: Integer): TtkTokenKind;
    function FuncSubstr(Index: Integer): TtkTokenKind;
    function FuncSubstring(Index: Integer): TtkTokenKind;
    function FuncSuffixes(Index: Integer): TtkTokenKind;
    function FuncSup(Index: Integer): TtkTokenKind;
    function FuncSuper(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncSynchronized(Index: Integer): TtkTokenKind;
    function FuncTags(Index: Integer): TtkTokenKind;
    function FuncTaint(Index: Integer): TtkTokenKind;
    function FuncTaintenabled(Index: Integer): TtkTokenKind;
    function FuncTan(Index: Integer): TtkTokenKind;
    function FuncTarget(Index: Integer): TtkTokenKind;
    function FuncText(Index: Integer): TtkTokenKind;
    function FuncTextarea(Index: Integer): TtkTokenKind;
    function FuncThis(Index: Integer): TtkTokenKind;
    function FuncThrow(Index: Integer): TtkTokenKind;
    function FuncThrows(Index: Integer): TtkTokenKind;
    function FuncTitle(Index: Integer): TtkTokenKind;
    function FuncTogmtstring(Index: Integer): TtkTokenKind;
    function FuncTolocalestring(Index: Integer): TtkTokenKind;
    function FuncTolowercase(Index: Integer): TtkTokenKind;
    function FuncToolbar(Index: Integer): TtkTokenKind;
    function FuncTop(Index: Integer): TtkTokenKind;
    function FuncTosource(Index: Integer): TtkTokenKind;
    function FuncTostring(Index: Integer): TtkTokenKind;
    function FuncTouppercase(Index: Integer): TtkTokenKind;
    function FuncToutcstring(Index: Integer): TtkTokenKind;
    function FuncTransient(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncTry(Index: Integer): TtkTokenKind;
    function FuncType(Index: Integer): TtkTokenKind;
    function FuncTypeof(Index: Integer): TtkTokenKind;
    function FuncUndefined(Index: Integer): TtkTokenKind;
    function FuncUnescape(Index: Integer): TtkTokenKind;
    function FuncUntaint(Index: Integer): TtkTokenKind;
    function FuncUnwatch(Index: Integer): TtkTokenKind;
    function FuncUrl(Index: Integer): TtkTokenKind;
    function FuncUseragent(Index: Integer): TtkTokenKind;
    function FuncUtc(Index: Integer): TtkTokenKind;
    function FuncValue(Index: Integer): TtkTokenKind;
    function FuncValueof(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function FuncVisibility(Index: Integer): TtkTokenKind;
    function FuncVlinkcolor(Index: Integer): TtkTokenKind;
    function FuncVoid(Index: Integer): TtkTokenKind;
    function FuncVspace(Index: Integer): TtkTokenKind;
    function FuncWatch(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncWidth(Index: Integer): TtkTokenKind;
    function FuncWindow(Index: Integer): TtkTokenKind;
    function FuncWith(Index: Integer): TtkTokenKind;
    function FuncWrite(Index: Integer): TtkTokenKind;
    function FuncWriteln(Index: Integer): TtkTokenKind;
    function FuncZindex(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure CommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
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
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NonReservedKeyAttri: TSynHighlighterAttributes read fNonReservedKeyAttri write fNonReservedKeyAttri;
    property EventAttri: TSynHighlighterAttributes read fEventAttri write fEventAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
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
  QSynEditStrConst, Variants;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..398] of WideString = (
    'abs', 'abstract', 'acos', 'action', 'alert', 'align', 'alinkColor', 'all',
    'All', 'anchor', 'Anchor', 'anchors', 'appCodeName', 'Applet', 'applets',
    'appName', 'appVersion', 'Area', 'arguments', 'Arguments', 'Array', 'asin',
    'atan', 'atan2', 'back', 'background', 'bgColor', 'big', 'blink', 'blur',
    'body', 'bold', 'boolean', 'Boolean', 'border', 'bottom', 'break', 'Button',
    'byte', 'call', 'callee', 'caller', 'captureEvents', 'case', 'catch',
    'ceil', 'char', 'charAt', 'charCodeAt', 'Checkbox', 'checked', 'class',
    'clear', 'clearInterval', 'clearTimeout', 'click', 'close', 'closed',
    'color', 'complete', 'concat', 'confirm', 'const', 'constructor',
    'continue', 'cookie', 'cos', 'current', 'Date', 'debugger', 'default',
    'defaultChecked', 'defaultSelected', 'defaultStatus', 'defaultValue',
    'delete', 'description', 'display', 'do', 'document', 'domain', 'double',
    'E', 'elements', 'else', 'Embed', 'embeds', 'enabledPlugin', 'encoding',
    'enum', 'escape', 'eval', 'event', 'exp', 'export', 'extends', 'false',
    'fgColor', 'filename', 'FileUpload', 'final', 'finally', 'find', 'fixed',
    'float', 'Float', 'floor', 'focus', 'fontcolor', 'fontsize', 'for', 'form',
    'Form', 'forms', 'forward', 'Frame', 'frames', 'fromCharCode', 'function',
    'Function', 'getDate', 'getDay', 'getElementById', 'getFullYear',
    'getHours', 'getMilliseconds', 'getMinutes', 'getMonth', 'getSeconds',
    'getTime', 'getTimezoneOffset', 'getUTCDate', 'getUTCDay', 'getUTCFullYear',
    'getUTCHours', 'getUTCMilliseconds', 'getUTCMinutes', 'getUTCMonth',
    'getUTCSeconds', 'getYear', 'Global', 'go', 'goto', 'handleEvent', 'hash',
    'height', 'Hidden', 'history', 'History', 'home', 'host', 'hostname',
    'href', 'hspace', 'if', 'Image', 'images', 'implements', 'import', 'in',
    'index', 'indexOf', 'Infinity', 'innerHeight', 'innerWidth', 'input',
    'instanceof', 'int', 'interface', 'isFinite', 'isNaN', 'italics', 'java',
    'javaEnabled', 'join', 'lastIndexOf', 'lastModified', 'Layer', 'layers',
    'left', 'length', 'link', 'Link', 'linkColor', 'links', 'LN10', 'LN2',
    'location', 'Location', 'locationbar', 'log', 'LOG10E', 'LOG2E', 'logon',
    'long', 'lowsrc', 'match', 'Math', 'max', 'MAX_VALUE', 'menubar', 'method',
    'MimeType', 'mimeTypes', 'min', 'MIN_VALUE', 'moveBy', 'moveTo', 'name',
    'NaN', 'native', 'navigator', 'Navigator', 'NEGATIVE_INFINITY', 'netscape',
    'new', 'next', 'null', 'Null', 'Number', 'Object', 'onAbort', 'onBlur',
    'onChange', 'onClick', 'onDblClick', 'onError', 'onFocus', 'onKeyDown',
    'onKeyPress', 'onKeyUp', 'onLoad', 'onMouseDown', 'onMouseMove',
    'onMouseOut', 'onMouseOver', 'onMouseUp', 'onReset', 'onSelect', 'onSubmit',
    'onUnload', 'open', 'opener', 'Option', 'options', 'outerHeight',
    'outerWidth', 'package', 'Packages', 'pageX', 'pageXOffset', 'pageY',
    'pageYOffset', 'parent', 'parse', 'parseFloat', 'parseInt', 'Password',
    'pathname', 'personalbar', 'PI', 'platform', 'Plugin', 'plugins', 'port',
    'POSITIVE_INFINITY', 'pow', 'previous', 'print', 'private', 'prompt',
    'protected', 'protocol', 'prototype', 'public', 'Radio', 'random',
    'referrer', 'refresh', 'RegExp', 'releaseEvents', 'reload', 'replace',
    'reset', 'Reset', 'resizeBy', 'resizeTo', 'return', 'reverse', 'right',
    'round', 'routeEvent', 'screen', 'scroll', 'scrollbars', 'scrollBy',
    'scrollTo', 'search', 'select', 'Select', 'selected', 'selectedIndex',
    'self', 'setDate', 'setFullYear', 'setHours', 'setInterval',
    'setMilliseconds', 'setMinutes', 'setMonth', 'setSeconds', 'setTime',
    'setTimeout', 'setUTCDate', 'setUTCFullYear', 'setUTCHours',
    'setUTCMilliseconds', 'setUTCMinutes', 'setUTCMonth', 'setUTCSeconds',
    'setYear', 'short', 'sin', 'slice', 'small', 'sort', 'split', 'sqrt',
    'SQRT1_2', 'SQRT2', 'src', 'start', 'static', 'status', 'statusbar', 'stop',
    'strike', 'String', 'style', 'sub', 'submit', 'Submit', 'substr',
    'substring', 'suffixes', 'sup', 'super', 'switch', 'synchronized', 'tags',
    'taint', 'taintEnabled', 'tan', 'target', 'text', 'Text', 'Textarea',
    'this', 'throw', 'throws', 'title', 'toGMTString', 'toLocaleString',
    'toLowerCase', 'toolbar', 'top', 'toSource', 'toString', 'toUpperCase',
    'toUTCString', 'transient', 'true', 'try', 'type', 'typeof', 'undefined',
    'Undefined', 'unescape', 'untaint', 'unwatch', 'URL', 'userAgent', 'UTC',
    'value', 'valueOf', 'var', 'visibility', 'vlinkColor', 'void', 'vspace',
    'watch', 'while', 'width', 'window', 'Window', 'with', 'write', 'writeln',
    'zIndex'
  );

  KeyIndices: array[0..5152] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 231, -1, -1, -1, -1, -1, 296, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 55,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 292, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 168, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 208, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 200, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 295, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 75, 351, -1, -1, -1, -1, -1, -1, 315, 37, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 239, -1, -1, -1, -1, -1, 326, -1, -1, -1, 31,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 143, -1, 99, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 339, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 241, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 235, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 145, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 204, -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1,
    -1, -1, 16, 52, 389, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    259, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 331, 30, -1, -1, -1, -1, -1, -1,
    -1, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 304, -1, 396, 2, -1, -1, 323, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 167,
    -1, -1, -1, -1, -1, -1, -1, 122, -1, -1, -1, -1, -1, -1, -1, -1, -1, 34, -1,
    -1, -1, -1, 203, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, 83, -1, -1,
    -1, -1, -1, 101, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    268, -1, -1, -1, -1, -1, -1, -1, -1, 182, -1, -1, -1, -1, -1, 246, 18, -1,
    -1, -1, -1, -1, 209, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 220, 161,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 134, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 332, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 229, -1, -1, -1, -1, -1, -1, -1, 157, 319, -1, 210, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 234, -1, -1, -1, -1, -1, -1, -1, -1, -1, 105,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 381, 78, -1,
    -1, -1, -1, -1, -1, -1, 257, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 219, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 196, -1, -1, -1, -1, -1, 379, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 363, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 309, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 386, 146, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 103, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 269, -1, -1, -1, 199, 172, -1, 15, 123, -1, -1, -1, -1, -1, -1, -1, 136,
    -1, -1, -1, 128, -1, -1, -1, -1, 366, -1, -1, 185, -1, -1, -1, -1, 153, -1,
    -1, -1, -1, 388, -1, -1, 165, -1, -1, -1, -1, -1, -1, 338, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 360, -1, -1,
    194, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 77, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 307, -1, -1, -1, -1, -1, -1, -1, 258, -1,
    -1, -1, 96, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 180, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 69, -1, -1, -1, -1, -1, -1, 129, -1, -1,
    -1, -1, -1, -1, -1, -1, 120, -1, -1, 95, -1, 233, -1, -1, -1, -1, -1, -1,
    -1, -1, 40, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 160, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 90, 282, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 341, 232, 121, 155, -1,
    -1, -1, -1, -1, 247, -1, -1, -1, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 327, -1, -1, -1, -1, -1, -1, -1, -1, -1, 74, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 170, -1, -1, -1, -1, 298, -1,
    -1, -1, -1, -1, -1, -1, 114, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 94, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 271, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 324, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 70, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 197, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 91, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 106, -1, -1, 237, -1, -1, -1, -1, -1, 6,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 240, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 250, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 205, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 238, -1, -1, -1,
    -1, -1, -1, -1, -1, 275, -1, -1, -1, -1, -1, -1, -1, -1, -1, 287, -1, -1,
    -1, -1, -1, -1, -1, 227, -1, -1, 383, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    58, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 29, 148, 171, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 392, -1, -1, -1, -1, -1, 125, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 201, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 135, -1, -1, 212,
    -1, -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, 272, -1, -1,
    -1, -1, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 334,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 289, -1, -1, -1, -1, 312, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 385, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 51, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, -1, -1, -1, -1, -1, 371, 76,
    -1, -1, 330, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 68, -1, -1, -1, -1, -1, -1, 225, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 13, -1, -1, -1, 156, -1, 23, -1, -1, -1, -1, -1, -1,
    -1, -1, 280, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 178, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 277, -1, -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, -1, -1, -1, 93, -1,
    -1, -1, -1, -1, -1, -1, 202, -1, 5, 343, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 255, -1, -1, -1, -1, -1, -1, -1,
    -1, 43, -1, -1, -1, 44, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 333, -1, -1, -1, -1, -1, 12, -1, -1, -1, -1, 139,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 320, -1, -1, -1, -1, -1, -1,
    214, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 152, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 278, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 302, 316, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 137, -1, -1, -1,
    254, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 86, -1, -1, -1, -1, -1, -1, -1, -1, 345, -1, -1, 144, -1, -1, -1, 7,
    -1, -1, 306, -1, -1, -1, -1, 113, -1, -1, -1, -1, -1, -1, 308, -1, -1, -1,
    -1, 357, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 361, -1, -1, -1, -1, -1, -1, -1, 195, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 387, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 169, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 376, -1, -1, -1, -1, 188, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 359, 98, -1, -1, -1,
    -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, 116, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 299, -1, -1, -1, 369, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 54, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 147, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 118, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    356, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 8, -1, 300, -1, -1, 228, 59, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 213, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 179, -1, -1, -1, -1, -1,
    -1, -1, 176, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 350, -1, -1, -1, -1,
    -1, -1, 284, -1, -1, -1, 256, -1, -1, 276, -1, -1, -1, -1, -1, -1, -1, -1,
    190, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 102, -1, 230, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 35, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 72, -1, 71, 26, -1, -1, -1, -1,
    -1, -1, 60, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 322, -1, -1, 175, -1, -1, 393, -1, 124, 85, -1, -1, -1, -1,
    -1, -1, -1, -1, 150, -1, 236, -1, -1, -1, -1, -1, -1, -1, -1, -1, 140, -1,
    -1, -1, -1, -1, -1, 183, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 111, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 20, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    42, 244, -1, -1, -1, -1, -1, -1, -1, 47, 313, -1, 41, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 64,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    294, -1, -1, -1, -1, -1, -1, -1, -1, 374, -1, -1, -1, -1, -1, -1, -1, 245,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 177, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 347, -1, -1, -1, -1, -1, -1, -1, 391, -1, -1, -1, -1, -1, -1, -1,
    217, -1, -1, -1, 87, -1, -1, -1, 329, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    39, -1, -1, -1, -1, -1, -1, -1, -1, 189, -1, -1, 222, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 174, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    274, -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    382, -1, -1, -1, 138, 226, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 24, -1, -1, -1, -1, -1, -1, -1, -1, 100, -1, -1, -1, -1, -1, -1,
    -1, -1, 318, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 335,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 260, -1, -1, -1, -1, -1, -1, 191, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 288, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 342, -1, -1, -1, -1, -1, -1,
    61, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 377, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 132, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 158, -1, -1, 166, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, -1, -1, -1, 57,
    -1, -1, -1, 211, -1, -1, -1, -1, 243, -1, -1, -1, -1, -1, 264, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 321, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 207, -1, -1,
    216, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    149, -1, -1, -1, -1, -1, 89, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, 293,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 117, -1, -1, -1, -1, 242, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 56,
    -1, 154, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 92, 193, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    325, 126, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 206, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 372, -1, -1, -1, 380, -1, -1,
    352, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 263, -1, -1, -1, -1, -1, -1, -1, 373, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 286, -1, 46, -1, -1, -1, -1, 184, -1, -1, -1, -1, -1, -1, 19,
    -1, -1, -1, 25, -1, -1, -1, -1, -1, -1, -1, 367, -1, -1, -1, -1, -1, 270,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 283,
    -1, -1, -1, -1, -1, -1, -1, -1, 151, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 65, -1, -1, -1,
    -1, -1, -1, -1, 398, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 252,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 355, -1, -1, 365, -1, -1, -1,
    -1, -1, -1, -1, -1, 28, -1, -1, 378, -1, -1, -1, -1, 354, -1, -1, -1, -1,
    -1, -1, -1, -1, 349, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 97, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 107, -1, -1, -1, -1, 285,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 215, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 198, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 81, 394, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 358, -1, -1, -1, -1, -1, -1, -1, 173, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 224, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 181, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 375,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 9, -1, -1, -1, -1, -1, 305, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 141,
    281, 115, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 84, -1, -1, -1, -1, -1,
    -1, -1, 261, -1, -1, -1, -1, 265, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    273, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 362, -1, 290, -1, 66, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 112, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    131, -1, 279, -1, -1, -1, 249, -1, -1, -1, -1, -1, -1, 223, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 297, -1, -1, -1, -1,
    127, -1, -1, 142, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 364, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 88, -1, -1, -1, -1, -1, -1, 248, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 395, 251, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 45, -1, -1, -1, -1, -1, -1, -1, -1, 80, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 310, -1, 218, -1, -1, -1, -1, -1, -1, 187, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 130, 390, -1, -1, -1, -1, -1, -1, -1,
    328, -1, 221, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 336, -1, -1, -1, -1, -1, -1, 311, -1, -1, -1, -1,
    -1, -1, -1, -1, 303, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 344, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 337, -1, -1, -1, -1, -1, 262, -1, -1, -1, -1,
    -1, -1, -1, 267, -1, -1, -1, -1, -1, -1, -1, 253, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 397, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 162, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 346, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, 348, 159, -1, -1, -1, -1, -1, -1, -1,
    368, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 370, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 164, -1, 314, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 291, -1, -1, -1, -1, -1, -1, 384, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, -1, 340, -1, -1,
    -1, -1, -1, -1, 317, -1, 79, -1, -1, -1, -1, 133, -1, -1, -1, -1, -1, -1,
    353, -1, 301, -1, -1, -1, -1, -1, -1, 163, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 266, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 186, -1, -1, -1
  );

{$Q-}
function TSynJScriptSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 751 + Ord(Str^) * 148;
    inc(Str);
  end;
  Result := Result mod 5153;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynJScriptSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynJScriptSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[4966] := FuncAbs;
  fIdentFuncTable[2170] := FuncAbstract;
  fIdentFuncTable[520] := FuncAcos;
  fIdentFuncTable[319] := FuncAction;
  fIdentFuncTable[4368] := FuncAlert;
  fIdentFuncTable[2070] := FuncAlign;
  fIdentFuncTable[1500] := FuncAlinkcolor;
  fIdentFuncTable[2362] := FuncAll;
  fIdentFuncTable[2706] := FuncAll;
  fIdentFuncTable[4383] := FuncAnchor;
  fIdentFuncTable[491] := FuncAnchor;
  fIdentFuncTable[2516] := FuncAnchors;
  fIdentFuncTable[2207] := FuncAppcodename;
  fIdentFuncTable[1993] := FuncApplet;
  fIdentFuncTable[1805] := FuncApplets;
  fIdentFuncTable[965] := FuncAppname;
  fIdentFuncTable[416] := FuncAppversion;
  fIdentFuncTable[2052] := FuncArea;
  fIdentFuncTable[618] := FuncArguments;
  fIdentFuncTable[3950] := FuncArguments;
  fIdentFuncTable[2987] := FuncArray;
  fIdentFuncTable[4131] := FuncAsin;
  fIdentFuncTable[5117] := FuncAtan;
  fIdentFuncTable[1999] := FuncAtan2;
  fIdentFuncTable[3356] := FuncBack;
  fIdentFuncTable[3954] := FuncBackground;
  fIdentFuncTable[2882] := FuncBgcolor;
  fIdentFuncTable[1824] := FuncBig;
  fIdentFuncTable[4067] := FuncBlink;
  fIdentFuncTable[1709] := FuncBlur;
  fIdentFuncTable[483] := FuncBody;
  fIdentFuncTable[243] := FuncBold;
  fIdentFuncTable[4200] := FuncBoolean;
  fIdentFuncTable[3265] := FuncBoolean2;
  fIdentFuncTable[563] := FuncBorder;
  fIdentFuncTable[2857] := FuncBottom;
  fIdentFuncTable[2410] := FuncBreak;
  fIdentFuncTable[223] := FuncButton;
  fIdentFuncTable[575] := FuncByte;
  fIdentFuncTable[3204] := FuncCall;
  fIdentFuncTable[1125] := FuncCallee;
  fIdentFuncTable[3049] := FuncCaller;
  fIdentFuncTable[3037] := FuncCaptureevents;
  fIdentFuncTable[2101] := FuncCase;
  fIdentFuncTable[2105] := FuncCatch;
  fIdentFuncTable[4662] := FuncCeil;
  fIdentFuncTable[3938] := FuncChar;
  fIdentFuncTable[3046] := FuncCharat;
  fIdentFuncTable[3724] := FuncCharcodeat;
  fIdentFuncTable[4522] := FuncCheckbox;
  fIdentFuncTable[2127] := FuncChecked;
  fIdentFuncTable[1908] := FuncClass;
  fIdentFuncTable[417] := FuncClear;
  fIdentFuncTable[4574] := FuncClearinterval;
  fIdentFuncTable[2626] := FuncCleartimeout;
  fIdentFuncTable[55] := FuncClick;
  fIdentFuncTable[3783] := FuncClose;
  fIdentFuncTable[3615] := FuncClosed;
  fIdentFuncTable[1688] := FuncColor;
  fIdentFuncTable[2712] := FuncComplete;
  fIdentFuncTable[2889] := FuncConcat;
  fIdentFuncTable[3503] := FuncConfirm;
  fIdentFuncTable[820] := FuncConst;
  fIdentFuncTable[3079] := FuncConstructor;
  fIdentFuncTable[3092] := FuncContinue;
  fIdentFuncTable[4022] := FuncCookie;
  fIdentFuncTable[4452] := FuncCos;
  fIdentFuncTable[1188] := FuncCurrent;
  fIdentFuncTable[1955] := FuncDate;
  fIdentFuncTable[1095] := FuncDebugger;
  fIdentFuncTable[1389] := FuncDefault;
  fIdentFuncTable[2881] := FuncDefaultchecked;
  fIdentFuncTable[2879] := FuncDefaultselected;
  fIdentFuncTable[3607] := FuncDefaultstatus;
  fIdentFuncTable[1234] := FuncDefaultvalue;
  fIdentFuncTable[214] := FuncDelete;
  fIdentFuncTable[1929] := FuncDescription;
  fIdentFuncTable[1046] := FuncDisplay;
  fIdentFuncTable[748] := FuncDo;
  fIdentFuncTable[5075] := FuncDocument;
  fIdentFuncTable[4671] := FuncDomain;
  fIdentFuncTable[4176] := FuncDouble;
  fIdentFuncTable[5059] := FuncE;
  fIdentFuncTable[581] := FuncElements;
  fIdentFuncTable[4413] := FuncElse;
  fIdentFuncTable[2919] := FuncEmbed;
  fIdentFuncTable[2346] := FuncEmbeds;
  fIdentFuncTable[3190] := FuncEnabledplugin;
  fIdentFuncTable[4627] := FuncEncoding;
  fIdentFuncTable[3716] := FuncEnum;
  fIdentFuncTable[1147] := FuncEscape;
  fIdentFuncTable[1465] := FuncEval;
  fIdentFuncTable[3807] := FuncEvent;
  fIdentFuncTable[2060] := FuncExp;
  fIdentFuncTable[1298] := FuncExport;
  fIdentFuncTable[1114] := FuncExtends;
  fIdentFuncTable[1069] := FuncFalse;
  fIdentFuncTable[4097] := FuncFgcolor;
  fIdentFuncTable[2508] := FuncFilename;
  fIdentFuncTable[271] := FuncFileupload;
  fIdentFuncTable[3365] := FuncFinal;
  fIdentFuncTable[587] := FuncFinally;
  fIdentFuncTable[2843] := FuncFind;
  fIdentFuncTable[931] := FuncFixed;
  fIdentFuncTable[1921] := FuncFloat;
  fIdentFuncTable[730] := FuncFloat2;
  fIdentFuncTable[1491] := FuncFloor;
  fIdentFuncTable[4111] := FuncFocus;
  fIdentFuncTable[4774] := FuncFontcolor;
  fIdentFuncTable[4932] := FuncFontsize;
  fIdentFuncTable[407] := FuncFor;
  fIdentFuncTable[2968] := FuncForm;
  fIdentFuncTable[4469] := FuncForm;
  fIdentFuncTable[2370] := FuncForms;
  fIdentFuncTable[1279] := FuncForward;
  fIdentFuncTable[4402] := FuncFrame;
  fIdentFuncTable[2522] := FuncFrames;
  fIdentFuncTable[3737] := FuncFromcharcode;
  fIdentFuncTable[2666] := FuncFunction;
  fIdentFuncTable[1982] := FuncFunction2;
  fIdentFuncTable[1111] := FuncGetdate;
  fIdentFuncTable[1176] := FuncGetday;
  fIdentFuncTable[553] := FuncGetelementbyid;
  fIdentFuncTable[966] := FuncGetfullyear;
  fIdentFuncTable[2918] := FuncGethours;
  fIdentFuncTable[1735] := FuncGetmilliseconds;
  fIdentFuncTable[3823] := FuncGetminutes;
  fIdentFuncTable[4549] := FuncGetmonth;
  fIdentFuncTable[978] := FuncGetseconds;
  fIdentFuncTable[1102] := FuncGettime;
  fIdentFuncTable[4707] := FuncGettimezoneoffset;
  fIdentFuncTable[4493] := FuncGetutcdate;
  fIdentFuncTable[3536] := FuncGetutcday;
  fIdentFuncTable[5080] := FuncGetutcfullyear;
  fIdentFuncTable[671] := FuncGetutchours;
  fIdentFuncTable[1795] := FuncGetutcmilliseconds;
  fIdentFuncTable[974] := FuncGetutcminutes;
  fIdentFuncTable[2302] := FuncGetutcmonth;
  fIdentFuncTable[3282] := FuncGetutcseconds;
  fIdentFuncTable[2212] := FuncGetyear;
  fIdentFuncTable[2940] := FuncGlobal;
  fIdentFuncTable[4400] := FuncGo;
  fIdentFuncTable[4552] := FuncGoto;
  fIdentFuncTable[269] := FuncHandleevent;
  fIdentFuncTable[2358] := FuncHash;
  fIdentFuncTable[380] := FuncHeight;
  fIdentFuncTable[911] := FuncHidden;
  fIdentFuncTable[2645] := FuncHistory;
  fIdentFuncTable[1710] := FuncHistory;
  fIdentFuncTable[3710] := FuncHome;
  fIdentFuncTable[2928] := FuncHost;
  fIdentFuncTable[3996] := FuncHostname;
  fIdentFuncTable[2246] := FuncHref;
  fIdentFuncTable[991] := FuncHspace;
  fIdentFuncTable[3785] := FuncIf;
  fIdentFuncTable[1177] := FuncImage;
  fIdentFuncTable[1997] := FuncImages;
  fIdentFuncTable[706] := FuncImplements;
  fIdentFuncTable[3582] := FuncImport;
  fIdentFuncTable[4969] := FuncIn;
  fIdentFuncTable[1137] := FuncIndex;
  fIdentFuncTable[656] := FuncIndexof;
  fIdentFuncTable[4918] := FuncInfinity;
  fIdentFuncTable[5096] := FuncInnerheight;
  fIdentFuncTable[5008] := FuncInnerwidth;
  fIdentFuncTable[999] := FuncInput;
  fIdentFuncTable[3585] := FuncInstanceof;
  fIdentFuncTable[545] := FuncInt;
  fIdentFuncTable[124] := FuncInterface;
  fIdentFuncTable[2465] := FuncIsfinite;
  fIdentFuncTable[1266] := FuncIsnan;
  fIdentFuncTable[1711] := FuncItalics;
  fIdentFuncTable[963] := FuncJava;
  fIdentFuncTable[4225] := FuncJavaenabled;
  fIdentFuncTable[3229] := FuncJoin;
  fIdentFuncTable[2913] := FuncLastindexof;
  fIdentFuncTable[2778] := FuncLastmodified;
  fIdentFuncTable[3139] := FuncLayer;
  fIdentFuncTable[2021] := FuncLayers;
  fIdentFuncTable[2770] := FuncLeft;
  fIdentFuncTable[1083] := FuncLength;
  fIdentFuncTable[4263] := FuncLink;
  fIdentFuncTable[611] := FuncLink;
  fIdentFuncTable[2947] := FuncLinkcolor;
  fIdentFuncTable[3943] := FuncLinks;
  fIdentFuncTable[986] := FuncLn10;
  fIdentFuncTable[5149] := FuncLn2;
  fIdentFuncTable[4694] := FuncLocation;
  fIdentFuncTable[2489] := FuncLocation;
  fIdentFuncTable[3213] := FuncLocationbar;
  fIdentFuncTable[2812] := FuncLog;
  fIdentFuncTable[3420] := FuncLog10e;
  fIdentFuncTable[3346] := FuncLog2e;
  fIdentFuncTable[3808] := FuncLogon;
  fIdentFuncTable[1030] := FuncLong;
  fIdentFuncTable[2430] := FuncLowsrc;
  fIdentFuncTable[830] := FuncMatch;
  fIdentFuncTable[1454] := FuncMath;
  fIdentFuncTable[4163] := FuncMax;
  fIdentFuncTable[962] := FuncMax_value;
  fIdentFuncTable[165] := FuncMenubar;
  fIdentFuncTable[1767] := FuncMethod;
  fIdentFuncTable[2068] := FuncMimetype;
  fIdentFuncTable[568] := FuncMimetypes;
  fIdentFuncTable[398] := FuncMin;
  fIdentFuncTable[1580] := FuncMin_value;
  fIdentFuncTable[3868] := FuncMoveby;
  fIdentFuncTable[3688] := FuncMoveto;
  fIdentFuncTable[147] := FuncName;
  fIdentFuncTable[624] := FuncNan;
  fIdentFuncTable[709] := FuncNative;
  fIdentFuncTable[3619] := FuncNavigator;
  fIdentFuncTable[1798] := FuncNavigator;
  fIdentFuncTable[2749] := FuncNegative_infinity;
  fIdentFuncTable[2232] := FuncNetscape;
  fIdentFuncTable[4150] := FuncNew;
  fIdentFuncTable[3691] := FuncNext;
  fIdentFuncTable[3186] := FuncNull;
  fIdentFuncTable[4687] := FuncNull2;
  fIdentFuncTable[811] := FuncNumber;
  fIdentFuncTable[655] := FuncObject;
  fIdentFuncTable[4718] := FuncOnabort;
  fIdentFuncTable[3216] := FuncOnblur;
  fIdentFuncTable[4506] := FuncOnchange;
  fIdentFuncTable[4236] := FuncOnclick;
  fIdentFuncTable[1962] := FuncOndblclick;
  fIdentFuncTable[3283] := FuncOnerror;
  fIdentFuncTable[1618] := FuncOnfocus;
  fIdentFuncTable[2711] := FuncOnkeydown;
  fIdentFuncTable[698] := FuncOnkeypress;
  fIdentFuncTable[2845] := FuncOnkeyup;
  fIdentFuncTable[9] := FuncOnload;
  fIdentFuncTable[1175] := FuncOnmousedown;
  fIdentFuncTable[1116] := FuncOnmousemove;
  fIdentFuncTable[720] := FuncOnmouseout;
  fIdentFuncTable[356] := FuncOnmouseover;
  fIdentFuncTable[2930] := FuncOnmouseup;
  fIdentFuncTable[1494] := FuncOnreset;
  fIdentFuncTable[1591] := FuncOnselect;
  fIdentFuncTable[233] := FuncOnsubmit;
  fIdentFuncTable[1527] := FuncOnunload;
  fIdentFuncTable[309] := FuncOpen;
  fIdentFuncTable[3742] := FuncOpener;
  fIdentFuncTable[3624] := FuncOption;
  fIdentFuncTable[3038] := FuncOptions;
  fIdentFuncTable[3129] := FuncOuterheight;
  fIdentFuncTable[617] := FuncOuterwidth;
  fIdentFuncTable[1183] := FuncPackage;
  fIdentFuncTable[4634] := FuncPackages;
  fIdentFuncTable[4499] := FuncPagex;
  fIdentFuncTable[1543] := FuncPagexoffset;
  fIdentFuncTable[4647] := FuncPagey;
  fIdentFuncTable[4043] := FuncPageyoffset;
  fIdentFuncTable[4818] := FuncParent;
  fIdentFuncTable[2306] := FuncParse;
  fIdentFuncTable[2092] := FuncParsefloat;
  fIdentFuncTable[2800] := FuncParseint;
  fIdentFuncTable[756] := FuncPassword;
  fIdentFuncTable[1065] := FuncPathname;
  fIdentFuncTable[433] := FuncPersonalbar;
  fIdentFuncTable[3413] := FuncPi;
  fIdentFuncTable[4421] := FuncPlatform;
  fIdentFuncTable[4802] := FuncPlugin;
  fIdentFuncTable[3917] := FuncPlugins;
  fIdentFuncTable[3630] := FuncPort;
  fIdentFuncTable[4426] := FuncPositive_infinity;
  fIdentFuncTable[5137] := FuncPow;
  fIdentFuncTable[4810] := FuncPrevious;
  fIdentFuncTable[602] := FuncPrint;
  fIdentFuncTable[958] := FuncPrivate;
  fIdentFuncTable[3968] := FuncPrompt;
  fIdentFuncTable[1326] := FuncProtected;
  fIdentFuncTable[1815] := FuncProtocol;
  fIdentFuncTable[4437] := FuncPrototype;
  fIdentFuncTable[3260] := FuncPublic;
  fIdentFuncTable[1600] := FuncRadio;
  fIdentFuncTable[2803] := FuncRandom;
  fIdentFuncTable[2045] := FuncReferrer;
  fIdentFuncTable[2270] := FuncRefresh;
  fIdentFuncTable[4495] := FuncRegexp;
  fIdentFuncTable[2008] := FuncReleaseevents;
  fIdentFuncTable[4401] := FuncReload;
  fIdentFuncTable[1148] := FuncReplace;
  fIdentFuncTable[3987] := FuncReset;
  fIdentFuncTable[2796] := FuncReset;
  fIdentFuncTable[4116] := FuncResizeby;
  fIdentFuncTable[3936] := FuncResizeto;
  fIdentFuncTable[1610] := FuncReturn;
  fIdentFuncTable[3457] := FuncReverse;
  fIdentFuncTable[1857] := FuncRight;
  fIdentFuncTable[4450] := FuncRound;
  fIdentFuncTable[5041] := FuncRouteevent;
  fIdentFuncTable[100] := FuncScreen;
  fIdentFuncTable[3727] := FuncScroll;
  fIdentFuncTable[3112] := FuncScrollbars;
  fIdentFuncTable[195] := FuncScrollby;
  fIdentFuncTable[15] := FuncScrollto;
  fIdentFuncTable[4544] := FuncSearch;
  fIdentFuncTable[1271] := FuncSelect;
  fIdentFuncTable[2532] := FuncSelect;
  fIdentFuncTable[2708] := FuncSelected;
  fIdentFuncTable[5089] := FuncSelectedindex;
  fIdentFuncTable[2283] := FuncSelf;
  fIdentFuncTable[4756] := FuncSetdate;
  fIdentFuncTable[517] := FuncSetfullyear;
  fIdentFuncTable[4389] := FuncSethours;
  fIdentFuncTable[2365] := FuncSetinterval;
  fIdentFuncTable[1057] := FuncSetmilliseconds;
  fIdentFuncTable[2377] := FuncSetminutes;
  fIdentFuncTable[867] := FuncSetmonth;
  fIdentFuncTable[4685] := FuncSetseconds;
  fIdentFuncTable[4747] := FuncSettime;
  fIdentFuncTable[1862] := FuncSettimeout;
  fIdentFuncTable[3047] := FuncSetutcdate;
  fIdentFuncTable[5010] := FuncSetutcfullyear;
  fIdentFuncTable[222] := FuncSetutchours;
  fIdentFuncTable[2284] := FuncSetutcmilliseconds;
  fIdentFuncTable[5073] := FuncSetutcminutes;
  fIdentFuncTable[3374] := FuncSetutcmonth;
  fIdentFuncTable[707] := FuncSetutcseconds;
  fIdentFuncTable[2225] := FuncSetyear;
  fIdentFuncTable[3661] := FuncShort;
  fIdentFuncTable[2910] := FuncSin;
  fIdentFuncTable[523] := FuncSlice;
  fIdentFuncTable[1345] := FuncSmall;
  fIdentFuncTable[3822] := FuncSort;
  fIdentFuncTable[239] := FuncSplit;
  fIdentFuncTable[1224] := FuncSqrt;
  fIdentFuncTable[4716] := FuncSqrt1_2;
  fIdentFuncTable[3194] := FuncSqrt2;
  fIdentFuncTable[1932] := FuncSrc;
  fIdentFuncTable[482] := FuncStart;
  fIdentFuncTable[684] := FuncStatic;
  fIdentFuncTable[2201] := FuncStatus;
  fIdentFuncTable[1836] := FuncStatusbar;
  fIdentFuncTable[3389] := FuncStop;
  fIdentFuncTable[4740] := FuncStrike;
  fIdentFuncTable[4796] := FuncString;
  fIdentFuncTable[1006] := FuncStyle;
  fIdentFuncTable[283] := FuncSub;
  fIdentFuncTable[5066] := FuncSubmit;
  fIdentFuncTable[1174] := FuncSubmit;
  fIdentFuncTable[3496] := FuncSubstr;
  fIdentFuncTable[2071] := FuncSubstring;
  fIdentFuncTable[4785] := FuncSuffixes;
  fIdentFuncTable[2355] := FuncSup;
  fIdentFuncTable[4953] := FuncSuper;
  fIdentFuncTable[3170] := FuncSwitch;
  fIdentFuncTable[4968] := FuncSynchronized;
  fIdentFuncTable[4084] := FuncTags;
  fIdentFuncTable[2789] := FuncTaint;
  fIdentFuncTable[215] := FuncTaintenabled;
  fIdentFuncTable[3896] := FuncTan;
  fIdentFuncTable[5087] := FuncTarget;
  fIdentFuncTable[4075] := FuncText;
  fIdentFuncTable[4055] := FuncText;
  fIdentFuncTable[2681] := FuncTextarea;
  fIdentFuncTable[2382] := FuncThis;
  fIdentFuncTable[4217] := FuncThrow;
  fIdentFuncTable[2507] := FuncThrows;
  fIdentFuncTable[1027] := FuncTitle;
  fIdentFuncTable[2422] := FuncTogmtstring;
  fIdentFuncTable[4448] := FuncTolocalestring;
  fIdentFuncTable[857] := FuncTolowercase;
  fIdentFuncTable[4611] := FuncToolbar;
  fIdentFuncTable[4058] := FuncTop;
  fIdentFuncTable[983] := FuncTosource;
  fIdentFuncTable[3962] := FuncTostring;
  fIdentFuncTable[4977] := FuncTouppercase;
  fIdentFuncTable[2536] := FuncToutcstring;
  fIdentFuncTable[4990] := FuncTransient;
  fIdentFuncTable[1928] := FuncTrue;
  fIdentFuncTable[3889] := FuncTry;
  fIdentFuncTable[3925] := FuncType;
  fIdentFuncTable[3121] := FuncTypeof;
  fIdentFuncTable[4305] := FuncUndefined;
  fIdentFuncTable[2484] := FuncUndefined;
  fIdentFuncTable[3518] := FuncUnescape;
  fIdentFuncTable[4070] := FuncUntaint;
  fIdentFuncTable[836] := FuncUnwatch;
  fIdentFuncTable[3893] := FuncUrl;
  fIdentFuncTable[747] := FuncUseragent;
  fIdentFuncTable[3278] := FuncUtc;
  fIdentFuncTable[1621] := FuncValue;
  fIdentFuncTable[5048] := FuncValueof;
  fIdentFuncTable[1890] := FuncVar;
  fIdentFuncTable[910] := FuncVisibility;
  fIdentFuncTable[2454] := FuncVlinkcolor;
  fIdentFuncTable[996] := FuncVoid;
  fIdentFuncTable[418] := FuncVspace;
  fIdentFuncTable[4708] := FuncWatch;
  fIdentFuncTable[3178] := FuncWhile;
  fIdentFuncTable[1729] := FuncWidth;
  fIdentFuncTable[2916] := FuncWindow;
  fIdentFuncTable[4177] := FuncWindow;
  fIdentFuncTable[4646] := FuncWith;
  fIdentFuncTable[519] := FuncWrite;
  fIdentFuncTable[4841] := FuncWriteln;
  fIdentFuncTable[4030] := FuncZindex;
end;

function TSynJScriptSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAbs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAbstract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAcos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAlert(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAlign(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAlinkcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAll(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAnchor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAnchors(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAppcodename(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncApplet(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncApplets(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAppname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAppversion(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncArea(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncArguments(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAsin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAtan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncAtan2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBack(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBackground(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBgcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBlink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBlur(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBody(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBold(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBoolean(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBoolean2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;


function TSynJScriptSyn.FuncBorder(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBottom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncButton(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCall(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCallee(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCaller(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCaptureevents(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCeil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncChar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCharat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCharcodeat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCheckbox(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncChecked(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncClass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncClear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncClearinterval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCleartimeout(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncClick(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncClose(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncClosed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncColor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncComplete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncConcat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncConfirm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncConstructor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCookie(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncCurrent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDebugger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDefaultchecked(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDefaultselected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDefaultstatus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDefaultvalue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDescription(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDisplay(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDocument(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDomain(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncDouble(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncE(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncElements(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncEmbed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncEmbeds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncEnabledplugin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncEncoding(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncEnum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncEscape(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncEval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncEvent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncExp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncExport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncExtends(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFgcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFilename(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFileupload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFinal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFinally(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFind(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFixed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFloat2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFloor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFocus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFontcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFontsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncForm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncForms(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncForward(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFrame(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFrames(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFromcharcode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFunction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncFunction2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetdate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetday(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetelementbyid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetfullyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGethours(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetmilliseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetminutes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetmonth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGettime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGettimezoneoffset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetutcdate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetutcday(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetutcfullyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetutchours(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetutcmilliseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetutcminutes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetutcmonth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetutcseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGetyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGlobal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHandleevent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHash(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHeight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHidden(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHistory(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHome(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHost(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHostname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHref(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncHspace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncImage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncImages(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncImplements(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncImport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncIn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncIndex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncIndexof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncInfinity(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncInnerheight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncInnerwidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncInput(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncInstanceof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncInterface(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncIsfinite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncIsnan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncItalics(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncJava(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncJavaenabled(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncJoin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLastindexof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLastmodified(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLayer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLayers(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLeft(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLength(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLinkcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLinks(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLn10(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLn2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLocation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLocationbar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLog(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLog10e(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLog2e(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLogon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLong(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncLowsrc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMath(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMax(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMax_value(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMenubar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMethod(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMimetype(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMimetypes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMin_value(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMoveby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncMoveto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncName(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNative(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNavigator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNegative_infinity(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNetscape(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNull(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNull2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncNumber(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnabort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnblur(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnchange(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnclick(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOndblclick(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnerror(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnfocus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnkeydown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnkeypress(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnkeyup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnmousedown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnmousemove(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnmouseout(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnmouseover(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnmouseup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnreset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnselect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnsubmit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOnunload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkEvent
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOpen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOpener(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOption(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOuterheight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncOuterwidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPackage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPackages(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPagex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPagexoffset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPagey(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPageyoffset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncParent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncParse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncParsefloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncParseint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPassword(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPathname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPersonalbar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPi(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPlatform(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPlugin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPlugins(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPositive_infinity(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPrevious(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPrint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPrivate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPrompt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncProtected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncProtocol(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPrototype(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncPublic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncRadio(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncRandom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncReferrer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncRefresh(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncRegexp(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncReleaseevents(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncReload(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncReplace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncReset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncResizeby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncResizeto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncReverse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncRight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncRound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncRouteevent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncScreen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncScroll(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncScrollbars(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncScrollby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncScrollto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSearch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSelect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSelected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSelectedindex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSelf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetdate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetfullyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSethours(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetinterval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetmilliseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetminutes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetmonth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSettime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSettimeout(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetutcdate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetutcfullyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetutchours(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetutcmilliseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetutcminutes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetutcmonth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetutcseconds(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSetyear(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncShort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSlice(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSmall(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSplit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSqrt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSqrt1_2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSqrt2(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSrc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncStart(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncStatic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncStatus(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncStatusbar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncStop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncStrike(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncStyle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSub(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSubmit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSubstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSubstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSuffixes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSuper(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncSynchronized(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTags(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTaint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTaintenabled(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTarget(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncText(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTextarea(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncThis(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncThrow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncThrows(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTitle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTogmtstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTolocalestring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTolowercase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncToolbar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTosource(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTostring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTouppercase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncToutcstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTransient(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncType(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncTypeof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncUndefined(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncUnescape(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncUntaint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncUnwatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncUrl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncUseragent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncUtc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncValue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncValueof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncVisibility(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncVlinkcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncVoid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncVspace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncWatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncWidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncWindow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncWith(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncWrite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncWriteln(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynJScriptSyn.FuncZindex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

constructor TSynJScriptSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNonReservedKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  AddAttribute(fNonReservedKeyAttri);
  fEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrEvent, SYNS_FriendlyAttrEvent);
  AddAttribute(fEventAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterJScript;
  fRange := rsUnknown;
end;

procedure TSynJScriptSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('&')] then inc(Run);
end;

procedure TSynJScriptSyn.CommentProc;
begin
  if fLine[Run] = #0 then
    NullProc
  else
  begin
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
      begin
        fRange := rsUnKnown;
        inc(Run, 2);
        break;
      end;
      inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynJScriptSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynJScriptSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynJScriptSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynJScriptSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('-'), WideChar('>')] then inc(Run);
end;

procedure TSynJScriptSyn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynJScriptSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynJScriptSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: Integer; // token[1]
  isHex: Boolean;
begin
  fTokenID := tkNumber;
  isHex := False;
  idx1 := Run;
  Inc(Run);
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break;
      'a'..'f', 'A'..'F':
        if not isHex then
          Break;
      'x', 'X':
        begin
          if (FLine[idx1] <> '0') or (Run > Succ(idx1)) then
            Break;
          if not IsHexChar(Succ(Run)) then
            Break;
          isHex := True;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynJScriptSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('|')] then inc(Run);
end;

procedure TSynJScriptSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('+')] then inc(Run);
end;

procedure TSynJScriptSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '.') and (fLine[Run + 1] = '.') then inc(Run, 2);
end;

procedure TSynJScriptSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '/': begin
           fTokenID := tkComment;
           repeat
             Inc(Run);
           until IsLineEnd(Run);
         end;
    '*': begin
           fTokenID := tkComment;
           fRange := rsAnsi;
           repeat
             Inc(Run);
             if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
               fRange := rsUnKnown;
               Inc(Run, 2);
               break;
             end;
           until IsLineEnd(Run);
         end;
    '=': begin
           Inc(Run);
           fTokenID := tkSymbol;
         end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynJScriptSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynJScriptSyn.StarProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynJScriptSyn.StringProc;
var
  l_strChar: WideString;
begin
  fTokenID := tkString;
  l_strChar := FLine[Run];   // We could have '"' or #39
  if (FLine[Run + 1] = l_strChar) and (FLine[Run + 2] = l_strChar) then inc(Run, 2);
  repeat
    if IsLineEnd(Run) then break;
    inc(Run);
  until (FLine[Run] = l_strChar) and (FLine[Pred(Run)] <> '\');
  if not IsLineEnd(Run) then
    Inc(Run);
end;

procedure TSynJScriptSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynJScriptSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynJScriptSyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsANSI then
    CommentProc
  else
    case fLine[Run] of
      '&': AndSymbolProc;
      #13: CRProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      #10: LFProc;
      '-': MinusProc;
      '%': ModSymbolProc;
      #0: NullProc;
      '0'..'9': NumberProc;
      '|': OrSymbolProc;
      '+': PlusProc;
      '.': PointProc;
      '/': SlashProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '*': StarProc;
      '"', #39: StringProc;
      '~', '{', '}', ',', '(', ')', '[', ']', '<', '>', ':', '?', ';', '!', '=':
        SymbolProc;
      else UnknownProc;
    end;
  inherited;
end;

function TSynJScriptSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynJScriptSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynJScriptSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynJScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynJScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNonReservedKey: Result := fNonReservedKeyAttri;
    tkEvent: Result := fEventAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynJScriptSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynJScriptSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynJScriptSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynJScriptSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterJScript;
end;

class function TSynJScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangJScript;
end;

function TSynJScriptSyn.GetSampleSource: WideString;
begin
  Result := '// Syntax highlighting'#13#10+
            'function printNumber()'#13#10+
            '{'#13#10+
            '  var number = 1234;'#13#10+
            '  var x;'#13#10+
            '  document.write("The number is " + number);'#13#10+
            '  for (var i = 0; i <= number; i++)'#13#10+
            '  {'#13#10+
            '    x++;'#13#10+
            '    x--;'#13#10+
            '    x += 1.0;'#13#10+
            '  }'#13#10+
            '  i += @; // illegal character'#13#10+
            '}'#13#10+
            'body.onLoad = printNumber;';
end;

class function TSynJScriptSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangJScript;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynJScriptSyn);
{$ENDIF}
end.
