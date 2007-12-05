{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterASM.pas, released 2000-04-18.
The Original Code is based on the nhAsmSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Nick Hoddinott.
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

$Id: SynHighlighterAsm.pas,v 1.14.2.5 2005/11/27 22:22:44 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a x86 Assembler highlighter for SynEdit)
@author(Nick Hoddinott <nickh@conceptdelta.com>, converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(7 November 1999, converted to SynEdit April 18, 2000)
@lastmod(April 18, 2000)
The SynHighlighterASM unit provides SynEdit with a x86 Assembler (.asm) highlighter.
The highlighter supports all x86 op codes, Intel MMX and AMD 3D NOW! op codes.
Thanks to Martin Waldenburg, Hideo Koiso.
}

{$IFNDEF QSYNHIGHLIGHTERASM}
unit SynHighlighterAsm;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynHighlighterHashEntries,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

type
  TSynAsmSyn = class(TSynCustomHighlighter)
  private
    fTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    function HashKey(Str: PWideChar): Cardinal;
    procedure CommentProc;
    procedure CRProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SingleQuoteStringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure DoAddKeyword(AKeyword: WideString; AKind: integer);
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
  protected
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
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
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  Mnemonics: WideString =
    'aaa,aad,aam,adc,add,and,arpl,bound,bsf,bsr,bswap,bt,btc,' +
    'btr,bts,call,cbw,cdq,clc,cld,cli,clts,cmc,cmp,cmps,cmpsb,cmpsd,cmpsw,' +
    'cmpxchg,cwd,cwde,daa,das,dec,div,emms,enter,f2xm1,fabs,fadd,faddp,fbld,' +
    'fbstp,fchs,fclex,fcmovb,fcmovbe,fcmove,fcmovnb,fcmovnbe,fcmovne,fcmovnu,' +
    'fcmovu,fcom,fcomi,fcomip,fcomp,fcompp,fcos,fdecstp,fdiv,fdivp,fdivr,' +
    'fdivrp,femms,ffree,fiadd,ficom,ficomp,fidiv,fidivr,fild,fimul,fincstp,' +
    'finit,fist,fistp,fisub,fisubr,fld,fld1,fldcw,fldenv,fldl2e,fldl2t,fldlg2,' +
    'fldln2,fldpi,fldz,fmul,fmulp,fnclex,fninit,fnop,fnsave,fnstcw,fnstenv,' +
    'fnstsw,fpatan,fprem1,fptan,frndint,frstor,fsave,fscale,fsin,fsincos,' +
    'fsqrt,fst,fstcw,fstenv,fstp,fstsw,fsub,fsubp,fsubr,fsubrp,ftst,' +
    'fucom,fucomi,fucomip,fucomp,fucompp,fwait,fxch,fxtract,fyl2xp1,hlt,idiv,' +
    'imul,in,inc,ins,insb,insd,insw,int,into,invd,invlpg,iret,iretd,iretw,' +
    'ja,jae,jb,jbe,jc,jcxz,je,jecxz,jg,jge,jl,jle,jmp,jna,jnae,jnb,jnbe,jnc,' +
    'jne,jng,jnge,jnl,jnle,jno,jnp,jns,jnz,jo,jp,jpe,jpo,js,jz,lahf,lar,lds,' +
    'lea,leave,les,lfs,lgdt,lgs,lidt,lldt,lmsw,lock,lods,lodsb,lodsd,lodsw,' +
    'loop,loope,loopne,loopnz,loopz,lsl,lss,ltr,mov,movd,movq, movs,movsb,' +
    'movsd,movsw,movsx,movzx,mul,neg,nop,not,or,out,outs,outsb,outsd,outsw,' +
    'packssdw,packsswb,packuswb,paddb,paddd,paddsb,paddsw,paddusb,paddusw,' +
    'paddw,pand,pandn,pavgusb,pcmpeqb,pcmpeqd,pcmpeqw,pcmpgtb,pcmpgtd,pcmpgtw,' +
    'pf2id,pfacc,pfadd,pfcmpeq,pfcmpge,pfcmpgt,pfmax,pfmin,pfmul,pfrcp,' +
    'pfrcpit1,pfrcpit2,pfrsqit1,pfrsqrt,pfsub,pfsubr,pi2fd,pmaddwd,pmulhrw,' +
    'pmulhw,pmullw,pop,popa,popad,popaw,popf,popfd,popfw,por,prefetch,prefetchw,' +
    'pslld,psllq,psllw,psrad,psraw,psrld,psrlq,psrlw,psubb,psubd,psubsb,' +
    'psubsw,psubusb,psubusw,psubw,punpckhbw,punpckhdq,punpckhwd,punpcklbw,' +
    'punpckldq,punpcklwd,push,pusha,pushad,pushaw,pushf,pushfd,pushfw,pxor,' +
    'rcl,rcr,rep,repe,repne,repnz,repz,ret,rol,ror,sahf,sal,sar,sbb,scas,' +
    'scasb,scasd,scasw,seta,setae,setb,setbe,setc,sete,setg,setge,setl,setle,' +
    'setna,setnae,setnb,setnbe,setnc,setne,setng,setnge,setnl,setnle,setno,' +
    'setnp,setns,setnz,seto,setp,setpo,sets,setz,sgdt,shl,shld,shr,shrd,sidt,' +
    'sldt,smsw,stc,std,sti,stos,stosb,stosd,stosw,str,sub,test,verr,verw,' +
    'wait,wbinvd,xadd,xchg,xlat,xlatb,xor';

procedure TSynAsmSyn.DoAddKeyword(AKeyword: WideString; AKind: integer);
var
  HashValue: Cardinal;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

{$Q-}
function TSynAsmSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 197 + Ord(Str^) * 14;
    inc(Str);
  end;
  Result := Result mod 4561;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynAsmSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

constructor TSynAsmSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fKeywords := TSynHashEntryList.Create;

  fCommentAttri       := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri    := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri           := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style     := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri        := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri         := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri        := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri        := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);

  EnumerateKeywords(Ord(tkKey), Mnemonics, IsIdentChar, DoAddKeyword);
  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter      := SYNS_FilterX86Assembly;
end;

destructor TSynAsmSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynAsmSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynAsmSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynAsmSyn.GreaterProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynAsmSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynAsmSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynAsmSyn.LowerProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in [WideChar('='), WideChar('>')] then Inc(Run);
end;

procedure TSynAsmSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynAsmSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'a'..'f', 'h', 'A'..'F', 'H':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
    Inc(Run);
end;

procedure TSynAsmSyn.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end else
    fTokenID := tkSymbol;
end;

procedure TSynAsmSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynAsmSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
    inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynAsmSyn.SingleQuoteStringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
    inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynAsmSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynAsmSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkIdentifier;
end;

procedure TSynAsmSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #34: StringProc;
    #39: SingleQuoteStringProc;
    '>': GreaterProc;
    '<': LowerProc;
    '/': SlashProc;

    'A'..'Z', 'a'..'z', '_':
      IdentProc;
    '0'..'9':
      NumberProc;
    #1..#9, #11, #12, #14..#32:
      SpaceProc;
    '#', ';':
      CommentProc;
    '.', ':', '&', '{', '}', '=', '^', '-', '+', '(', ')', '*':
      SymbolProc;
    else
      UnknownProc;
  end;
  inherited;
end;

function TSynAsmSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynAsmSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynAsmSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynAsmSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynAsmSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

class function TSynAsmSyn.GetLanguageName: string;
begin
  Result := SYNS_LangX86Asm;
end;

function TSynAsmSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterX86Assembly;
end;

function TSynAsmSyn.GetSampleSource: WideString;
begin
  Result := '; x86 assembly sample source'#13#10 +
            '  CODE	SEGMENT	BYTE PUBLIC'#13#10 +
            '    ASSUME	CS:CODE'#13#10 +
            #13#10 +
            '    PUSH SS'#13#10 +
            '    POP DS'#13#10 +
            '    MOV AX, AABBh'#13#10 +
            '    MOV	BYTE PTR ES:[DI], 255'#13#10 +
            '    JMP SHORT AsmEnd'#13#10 +
            #13#10 +
            '  welcomeMsg DB ''Hello World'', 0'#13#10 +
            #13#10 +
            '  AsmEnd:'#13#10 +
            '    MOV AX, 0'#13#10 +
            #13#10 +
            '  CODE	ENDS'#13#10 +
            'END';
end;

class function TSynAsmSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangX86Asm;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynAsmSyn);
{$ENDIF}
end.

