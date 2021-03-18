{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterTclTk.pas, released 2000-05-05.
The Original Code is based on the siTclTkSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Igor Shitikov.
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

$Id: SynHighlighterTclTk.pas,v 1.18.2.12 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a TCL/Tk highlighter for SynEdit)
@author(Igor Shitikov, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(5 December 1999, converted to SynEdit April 18, 2000)
@lastmod(2000-06-23)
The SynHighlighterTclTk unit provides SynEdit with a TCL/Tk highlighter.
}

unit SynHighlighterTclTk;

{$I SynEdit.inc}

interface

uses
  Windows,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkIdentifier, tkNull, tkNumber, tkSecondKey,
    tkTixKey, tkSpace, tkString, tkOptions, tkVariable, tkWidgetKey, tkPath, tkUnknown);

  TRangeState = (rsUnknown, rsAnsi, rsPasStyle, rsCStyle);

type
  TSynTclTkSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FSecondKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FOptionsAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FPathAttri: TSynHighlighterAttributes;
    FKeyWords: TUnicodeStrings;
    FSecondKeys: TUnicodeStrings;
    FTixWords: TUnicodeStrings;
    FTixKeyAttri: TSynHighlighterAttributes;
    FWidgetWords: TUnicodeStrings;
    FWidgetKeyAttri: TSynHighlighterAttributes;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure VariableProc;
    procedure PathProc;
    procedure MinusProc;
    procedure SymbolProc;
    procedure SetKeyWords(const Value: TUnicodeStrings);
    procedure SetSecondKeys(const Value: TUnicodeStrings);
    function IsKeywordListStored: Boolean;
    function IsSecondKeywordListStored: Boolean;
    function InternalIsKeyword(const AKeyword: UnicodeString;
        KeyWordList: TUnicodeStrings; ACaseSensitive: Boolean = False): Boolean;
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
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function IsKeyword(const AKeyword: UnicodeString): Boolean; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property KeyWords: TUnicodeStrings read FKeyWords write SetKeyWords
      stored IsKeywordListStored;
    property SecondKeyAttri: TSynHighlighterAttributes read FSecondKeyAttri
      write FSecondKeyAttri;
    property SecondKeyWords: TUnicodeStrings read FSecondKeys write SetSecondKeys
      stored IsSecondKeywordListStored;
    property TixKeyAttri: TSynHighlighterAttributes read FTixKeyAttri
      write FTixKeyAttri;
    property TixWords: TUnicodeStrings read FTixWords;
    property WidgetKeyAttri: TSynHighlighterAttributes read FWidgetKeyAttri
      write FWidgetKeyAttri;
    property WidgetWords: TUnicodeStrings read FWidgetWords;

    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property OptionsAttri: TSynHighlighterAttributes read FOptionsAttri
      write FOptionsAttri;
    property PathAttri: TSynHighlighterAttributes read FPathAttri
      write FPathAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri
      write FVariableAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  TclTkKeys: array[0..128] of UnicodeString = (
    'after', 'append', 'array', 'auto_execok', 'auto_import', 'auto_load', 
    'auto_mkindex', 'auto_mkindex_old', 'auto_qualify', 'auto_reset', 'base', 
    'bgerror', 'binary', 'body', 'break', 'catch', 'cd', 'class', 'clock', 
    'close', 'code', 'concat', 'configbody', 'constructor', 'continue', 'dde', 
    'delete', 'destructor', 'else', 'elseif', 'encoding', 'ensemble', 'eof', 
    'error', 'eval', 'exec', 'exit', 'expr', 'fblocked', 'fconfigure', 'fcopy', 
    'file', 'fileevent', 'filename', 'find', 'flush', 'for', 'foreach', 
    'format', 'gets', 'glob', 'global', 'history', 'http', 'if', 'incr', 'info', 
    'inherit', 'interp', 'is', 'join', 'lappend', 'lindex', 'linsert', 'list', 
    'llength', 'load', 'local', 'lrange', 'lreplace', 'lsearch', 'lset', 
    'lsort', 'memory', 'method', 'msgcat', 'namespace', 'open', 'package', 
    'parray', 'pid', 'pkg_mkindex', 'private', 'proc', 'protected', 'public', 
    'puts', 'pwd', 're_syntax', 'read', 'regexp', 'registry', 'regsub', 
    'rename', 'resource', 'return', 'safe', 'safebase', 'scan', 'scope', 'seek', 
    'set', 'socket', 'source', 'split', 'string', 'subst', 'switch', 'tcl', 
    'tcl_endofword', 'tcl_findlibrary', 'tcl_startofnextword', 
    'tcl_startofpreviousword', 'tcl_wordbreakafter', 'tcl_wordbreakbefore', 
    'tcltest', 'tclvars', 'tell', 'then', 'time', 'trace', 'unknown', 'unset', 
    'update', 'uplevel', 'upvar', 'variable', 'vwait', 'while' 
  );
   
  SecondTclTkKeys: array[0..91] of UnicodeString = (
    'bell', 'bind', 'bindidproc', 'bindproc', 'bindtags', 'bitmap', 'button', 
    'canvas', 'checkbutton', 'clipboard', 'colors', 'combobox', 'console', 
    'cursors', 'debug', 'destroy', 'entry', 'event', 'exp_after', 'exp_before', 
    'exp_continue', 'exp_internal', 'exp_send', 'expect', 'focus', 'font', 
    'frame', 'grab', 'grid', 'image', 'interact', 'interpreter', 'keysyms', 
    'label', 'labelframe', 'listbox', 'loadtk', 'log_file', 'log_user', 'lower', 
    'menu', 'menubutton', 'message', 'namespupd', 'option', 'options', 'pack', 
    'panedwindow', 'photo', 'place', 'radiobutton', 'raise', 'rgb', 'scale', 
    'scrollbar', 'selection', 'send', 'send_error', 'send_log', 'send_tty', 
    'send_user', 'sendout', 'sleep', 'spawn', 'spinbox', 'stty', 'text', 'tk', 
    'tk_bisque', 'tk_choosecolor', 'tk_choosedirectory', 'tk_dialog', 
    'tk_focusfollowsmouse', 'tk_focusnext', 'tk_focusprev', 'tk_getopenfile', 
    'tk_getsavefile', 'tk_menusetfocus', 'tk_messagebox', 'tk_optionmenu', 
    'tk_popup', 'tk_setpalette', 'tk_textcopy', 'tk_textcut', 'tk_textpaste', 
    'tkerror', 'tkvars', 'tkwait', 'toplevel', 'wait', 'winfo', 'wm' 
  );

  TixKeys: array[0..43] of UnicodeString = (
    'compound', 'pixmap', 'tix', 'tixballoon', 'tixbuttonbox', 'tixchecklist', 
    'tixcombobox', 'tixcontrol', 'tixdestroy', 'tixdirlist', 
    'tixdirselectdialog', 'tixdirtree', 'tixdisplaystyle', 'tixexfileselectbox', 
    'tixexfileselectdialog', 'tixfileentry', 'tixfileselectbox', 
    'tixfileselectdialog', 'tixform', 'tixgetboolean', 'tixgetint', 'tixgrid', 
    'tixhlist', 'tixinputonly', 'tixlabelentry', 'tixlabelframe', 
    'tixlistnotebook', 'tixmeter', 'tixmwm', 'tixnbframe', 'tixnotebook', 
    'tixoptionmenu', 'tixpanedwindow', 'tixpopupmenu', 'tixscrolledhlist', 
    'tixscrolledlistbox', 'tixscrolledtext', 'tixscrolledwindow', 'tixselect', 
    'tixstdbuttonbox', 'tixtlist', 'tixtree', 'tixutils', 'tixwish' 
  );
  
  WidgetKeys: array[0..32] of UnicodeString = (
    'ArrowButton', 'Button', 'ButtonBox', 'BWidget', 'ComboBox', 'Dialog', 
    'DragSite', 'DropSite', 'DynamicHelp', 'Entry', 'Label', 'LabelEntry', 
    'LabelFrame', 'ListBox', 'MainFrame', 'MessageDlg', 'NoteBook', 
    'PagesManager', 'PanedWindow', 'PasswdDlg', 'ProgressBar', 'ProgressDlg', 
    'ScrollableFrame', 'ScrollableWindow', 'ScrolledWindow', 'ScrollView', 
    'SelectColor', 'SelectFont', 'Separator', 'SpinBox', 'TitleFrame', 'Tree', 
    'Widget' 
  );

function TSynTclTkSyn.InternalIsKeyword(const AKeyword: UnicodeString;
  KeyWordList: TUnicodeStrings; ACaseSensitive: Boolean = False): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: UnicodeString;
begin
  First := 0;
  Last := KeyWordList.Count - 1;
  Result := False;
  if ACaseSensitive then
    Token := AKeyword
  else
    Token := SynWideLowerCase(AKeyword);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareStr(KeyWordList[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      Break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end;

function TSynTclTkSyn.IsKeyword(const AKeyword: UnicodeString): Boolean;
begin
  Result := InternalIsKeyword(AKeyword, FWidgetWords, True) or
    InternalIsKeyword(AKeyword, FTixWords) or
    InternalIsKeyword(AKeyword, FKeyWords) or
    InternalIsKeyword(AKeyword, FSecondKeys);
end;

constructor TSynTclTkSyn.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FKeyWords := TUnicodeStringList.Create;
  TUnicodeStringList(FKeyWords).Sorted := True;
  TUnicodeStringList(FKeyWords).Duplicates := dupIgnore;
  FSecondKeys := TUnicodeStringList.Create;
  TUnicodeStringList(FSecondKeys).Sorted := True;
  TUnicodeStringList(FSecondKeys).Duplicates := dupIgnore;
  FTixWords := TUnicodeStringList.Create;
  TUnicodeStringList(FTixWords).Sorted := True;
  TUnicodeStringList(FTixWords).Duplicates := dupIgnore;
  FWidgetWords := TUnicodeStringList.Create;
  TUnicodeStringList(FWidgetWords).Sorted := True;
  TUnicodeStringList(FWidgetWords).Duplicates := dupIgnore;
  FKeyWords.BeginUpdate;
  for i := Low(TclTkKeys) to High(TclTkKeys) do
    FKeyWords.Add(TclTkKeys[i]);
  FKeyWords.EndUpdate;
  FSecondKeys.BeginUpdate;
  for i := Low(SecondTclTkKeys) to High(SecondTclTkKeys) do
    FSecondKeys.Add(SecondTclTkKeys[i]);
  FSecondKeys.EndUpdate;
  FTixWords.BeginUpdate;
  for i := Low(TixKeys) to High(TixKeys) do
    FTixWords.Add(TixKeys[i]);
  FTixWords.EndUpdate;
  FWidgetWords.BeginUpdate;
  for i := Low(WidgetKeys) to High(WidgetKeys) do
    FWidgetWords.Add(WidgetKeys[i]);
  FWidgetWords.EndUpdate;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  FSecondKeyAttri.Style := [fsBold];
  AddAttribute(FSecondKeyAttri);

  FTixKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrTixKeyWords, SYNS_FriendlyAttrTixKeyWords);
  FTixKeyAttri.Style := [fsBold, fsItalic];
  AddAttribute(FTixKeyAttri);

  FWidgetKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrWidgetWords, SYNS_FriendlyAttrWidgetWords);
  FWidgetKeyAttri.Style := [fsBold, fsItalic];
  AddAttribute(FWidgetKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  FOptionsAttri := TSynHighlighterAttributes.Create(SYNS_AttrOptions, SYNS_FriendlyAttrOptions);
  AddAttribute(FOptionsAttri);
  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(FVariableAttri);
  FPathAttri := TSynHighlighterAttributes.Create(SYNS_AttrPath, SYNS_FriendlyAttrPath);
  AddAttribute(FPathAttri);

  FRange := rsUnknown;
  FDefaultFilter := SYNS_FilterTclTk;
end;

destructor TSynTclTkSyn.Destroy;
begin
  FWidgetWords.Free;
  FTixWords.Free;
  FSecondKeys.Free;
  FKeyWords.Free;
  inherited Destroy;
end;

procedure TSynTclTkSyn.AnsiProc;
begin
  FTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if FLine[Run] = '*' then
    begin
      if FLine[Run + 1] = ')' then
      begin
        FRange := rsUnknown;
        Inc(Run, 2);
        Break;
      end
      else
        Inc(Run)
    end
    else
      Inc(Run);
end;

procedure TSynTclTkSyn.PasStyleProc;
begin
  FTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if FLine[Run] = '}' then
    begin
      FRange := rsUnknown;
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
end;

procedure TSynTclTkSyn.CStyleProc;
begin
  FTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if FLine[Run] = '*' then
    begin
      if FLine[Run + 1] = '/' then
      begin
        FRange := rsUnknown;
        Inc(Run, 2);
        Break;
      end
      else Inc(Run)
    end
    else
      Inc(Run);
end;

procedure TSynTclTkSyn.BraceOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynTclTkSyn.PointCommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynTclTkSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
    else Inc(Run);
  end;
end;

procedure TSynTclTkSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do Inc(Run);
  if InternalIsKeyword(GetToken, FWidgetWords, True) then
    FTokenID := tkWidgetKey
  else if InternalIsKeyword(GetToken, FTixWords) then
    FTokenID := tkTixKey
  else if InternalIsKeyword(GetToken, FKeyWords) then
    FTokenID := tkKey
  else if InternalIsKeyword(GetToken, FSecondKeys) then
    FTokenID := tkSecondKey
  else
    FTokenID := tkIdentifier;
end;

procedure TSynTclTkSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynTclTkSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynTclTkSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynTclTkSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynTclTkSyn.SlashProc;
begin
  if FLine[Run] = '#' then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end;
end;

procedure TSynTclTkSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynTclTkSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
    Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then Break;
    Inc(Run);
  until (FLine[Run] = #34) and (FLine[Pred(Run)] <> '\');
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynTclTkSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnKnown;
end;

procedure TSynTclTkSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsAnsi: AnsiProc;
    rsPasStyle: PasStyleProc;
    rsCStyle: CStyleProc;
    else
      case FLine[Run] of
        '-': MinusProc;
        '#': SlashProc;
        '{': BraceOpenProc;
        ';': PointCommaProc;
        #13: CRProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        #10: LFProc;
        #0: NullProc;
        '0'..'9':  NumberProc;
        '(': RoundOpenProc;
        '/': SlashProc;
        '[', ']', ')', '}': SymbolProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        #34: StringProc;
        '$': VariableProc;
        '.': PathProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynTclTkSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynTclTkSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynTclTkSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynTclTkSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynTclTkSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkSecondKey: Result := FSecondKeyAttri;
    tkTixKey: Result := FTixKeyAttri;
    tkWidgetKey: Result := FWidgetKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkOptions: Result := FOptionsAttri;
    tkVariable: Result := FVariableAttri;
    tkPath: Result := FPathAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynTclTkSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynTclTkSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynTclTkSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynTclTkSyn.SetKeyWords(const Value: TUnicodeStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := SynWideUpperCase(Value[i]);
      Value.EndUpdate;
    end;
  FKeyWords.Assign(Value);
  DefHighLightChange(nil);
end;

procedure TSynTclTkSyn.SetSecondKeys(const Value: TUnicodeStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := SynWideUpperCase(Value[i]);
      Value.EndUpdate;
    end;
  FSecondKeys.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynTclTkSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterTclTk;
end;

class function TSynTclTkSyn.GetLanguageName: string;
begin
  Result := SYNS_LangTclTk;
end;

function TSynTclTkSyn.LoadFromRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then
    begin
      if r.ValueExists('KeyWords') then KeyWords.Text := r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynTclTkSyn.SaveToRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
begin
  r:= TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then begin
      {$IFNDEF SYN_COMPILER_25_UP}
      Result := true;
      {$ENDIF}
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else Result := False;
  finally
    r.Free;
  end;
end;

function TSynTclTkSyn.IsKeywordListStored: Boolean;
var
  Keys: TUnicodeStringList;
  DefKey: Integer;
  Index: Integer;
begin
  Keys := TUnicodeStringList.Create;
  try
    Keys.Assign(KeyWords);
    Index := 0;
    for DefKey := Low(TclTkKeys) to High(TclTkKeys) do
    begin
      if not Keys.Find(TclTkKeys[DefKey], Index) then
      begin
        Result := True;
        Exit;
      end;
      Keys.Delete(Index);
    end;
    Result := Keys.Count <> 0;
  finally
    Keys.Free;
  end;
end;

function TSynTclTkSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '#!/usr/local/tclsh8.0'#13#10 +
    'if {$argc < 2} {'#13#10 +
    '	puts stderr "Usage: $argv0 parameter"'#13#10 +
    '	exit 1'#13#10 +
    '}';
end;

class function TSynTclTkSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangTclTk;
end;

procedure TSynTclTkSyn.MinusProc;
const
  EmptyChars = [' ', #9, #0, #10, #13];
var
  OK: Boolean;
begin
  OK := False;
  Inc(Run);
  { minus like symbol }
  if CharInSet(FLine[Run], ['0'..'9']) then
    FTokenID := tkSymbol
  else
  { special option -- }
  if (FLine[Run] = '-') and CharInSet(FLine[Run + 1], EmptyChars) then
  begin
    OK := True;
    Inc(Run);
  end
  { normal options -options }
  else begin
    if CharInSet(FLine[Run], ['a'..'z', 'A'..'Z']) then
    begin
      Inc(Run);
      while CharInSet(FLine[Run], ['a'..'z', 'A'..'Z']) do
        Inc(Run);
      OK := CharInSet(FLine[Run], EmptyChars);
    end
    { bad option syntax }
    else
      while not CharInSet(FLine[Run], EmptyChars) do
        Inc(Run);
  end;
  if OK then
    FTokenID := tkOptions
  else
    FTokenID := tkUnknown;
end;

procedure TSynTclTkSyn.PathProc;
begin
  if CharInSet(FLine[Run + 1], ['a'..'z', 'A'..'Z']) then
  begin
    FTokenID := tkPath;
    Inc(Run);
    while CharInSet(FLine[Run], ['a'..'z', 'A'..'Z', '0'..'9']) do Inc(Run);
  end
  else
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end;
end;

procedure TSynTclTkSyn.VariableProc;
begin
  FTokenID := tkVariable;
  Inc(Run);
  while CharInSet(FLine[Run], ['_', '0'..'9', 'A'..'Z', 'a'..'z']) do Inc(Run);
end;

function TSynTclTkSyn.IsSecondKeywordListStored: Boolean;
var
  Keys: TUnicodeStringList;
  DefKey: Integer;
  Index: Integer;
begin
  Keys := TUnicodeStringList.Create;
  try
    Keys.Assign(SecondKeyWords);
    Index := 0;
    for DefKey := Low(SecondTclTkKeys) to High(SecondTclTkKeys) do
    begin
      if not Keys.Find(SecondTclTkKeys[DefKey], Index) then
      begin
        Result := True;
        Exit;
      end;
      Keys.Delete(Index);
    end;
    Result := Keys.Count <> 0;
  finally
    Keys.Free;
  end;
end;

procedure TSynTclTkSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynTclTkSyn);
{$ENDIF}
end.
