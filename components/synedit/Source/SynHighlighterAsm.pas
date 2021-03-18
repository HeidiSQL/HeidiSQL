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

$Id: SynHighlighterAsm.pas,v 1.14.2.6 2008/09/14 16:24:59 maelh Exp $

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

unit SynHighlighterAsm;

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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

type
  TSynAsmSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FKeywords: TSynHashEntryList;
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
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
  protected
    function GetSampleSource: UnicodeString; override;
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
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
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
  Mnemonics: UnicodeString =
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

procedure TSynAsmSyn.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Cardinal;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

{$Q-}
function TSynAsmSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 197 + Ord(Str^) * 14;
    Inc(Str);
  end;
  Result := Result mod 4561;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynAsmSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

constructor TSynAsmSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FKeywords := TSynHashEntryList.Create;

  FCommentAttri       := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri    := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri           := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style     := [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri        := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri         := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri        := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri        := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  EnumerateKeywords(Ord(tkKey), Mnemonics, IsIdentChar, DoAddKeyword);
  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter      := SYNS_FilterX86Assembly;
end;

destructor TSynAsmSyn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynAsmSyn.CommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynAsmSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynAsmSyn.GreaterProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynAsmSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynAsmSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynAsmSyn.LowerProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  if CharInSet(FLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSynAsmSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynAsmSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'a'..'f', 'h', 'A'..'F', 'H':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkNumber;
  while IsNumberChar do
    Inc(Run);
end;

procedure TSynAsmSyn.SlashProc;
begin
  Inc(Run);
  if FLine[Run] = '/' then begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end else
    FTokenID := tkSymbol;
end;

procedure TSynAsmSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynAsmSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
    end;
    Inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynAsmSyn.SingleQuoteStringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynAsmSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynAsmSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkIdentifier;
end;

procedure TSynAsmSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
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

function TSynAsmSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynAsmSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynAsmSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynAsmSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynAsmSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

class function TSynAsmSyn.GetLanguageName: string;
begin
  Result := SYNS_LangX86Asm;
end;

function TSynAsmSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterX86Assembly;
end;

function TSynAsmSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '; x86 assembly sample source'#13#10 +
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

class function TSynAsmSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangX86Asm;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynAsmSyn);
{$ENDIF}
end.

