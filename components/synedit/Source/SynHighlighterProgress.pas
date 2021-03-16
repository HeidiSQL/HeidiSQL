{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterProgress.pas, released 2000-04-20.
The Initial Author of the Original Code is Bruno Mikkelsen.
Portions written by Bruno Mikkelsen are copyright 2000 Bruno Mikkelsen.
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

$Id: SynHighlighterProgress.pas,v 1.16.2.8 2009/09/28 19:16:08 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Progress Syntax highlighter for SynEdit)
@author(Bruno Mikkelsen <btm@scientist.com>)
@created(2000-04-16)
@lastmod(2000-06-20)
The SynHighlighterProgress provides SynEdit with a syntax highlighter for the
Progress programming language.
Thanks to Michael Hieke for providing a sample highlighter on which this
highlighter is based.
}

unit SynHighlighterProgress;

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
  {Enumerates the different tokens in Progress.}
  TtkTokenKind = (tkComment, tkEvent, tkIdentifier, tkInclude, tkKey,
    tkNonReserved, tkNull, tkNumber, tkPreprocessor, tkSpace, tkDataType,
    tkString, tkSymbol, tkUnknown);

  {Enumerates the ranges in Progress syntax.}
  TRangeState = (rsNone, rsInclude, rsPreprocessorDef, rsPreprocessor,
    rsComment);

  {Used to hold extra rangeinfo in the Lines.Objects pointer.}
  TRangeInfo = packed record
    case Boolean of
      False: (Ptr: Pointer);
      True: (Range: Word; Level: Word);
  end;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynProgressSyn = class(TSynCustomHighLighter)
  private
    FRange: TRangeState;
    FCommentLevel: Integer;
    FIncludeLevel: Integer;
    FPreProcessorLevel: Integer;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FEventAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FIncludeAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNonReservedKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FPreprocessorAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FDataTypeAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FHashList: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure AsciiCharProc;
    procedure CommentRangeProc;
    procedure IncludeRangeProc;
    procedure PreprocessorRangeProc;
    procedure PreprocessorDefinitionProc;
    procedure PreprocessorDefinitionRangeProc;
    procedure BraceOpenProc;
    procedure IdentProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure SymbolProc;
  protected
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
{$IFDEF DEBUG}
  public
    property Keywords: TSynHashEntryList read FHashList;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property EventAttri: TSynHighlighterAttributes read FEventAttri
      write FEventAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property IncludeAttri: TSynHighlighterAttributes read FIncludeAttri
      write FIncludeAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NonReservedKeyAttri: TSynHighlighterAttributes
      read FNonReservedKeyAttri write FNonReservedKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property PreprocessorAttri: TSynHighlighterAttributes
      read FPreprocessorAttri write FPreprocessorAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property DataTypeAttri: TSynHighlighterAttributes read FDataTypeAttri
      write FDataTypeAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

const
  DefaultKeywords: UnicodeString =
                    'accum accumulate active-window add alias ' +
                    'all alter ambig ambiguous analyze ' +
                    'analyze-resume analyze-suspend and any apply ' +
                    'as asc ascending assign at ' +
                    'attr-space authorization auto-return avail available ' +
                    'background before-hide begins bell between ' +
                    'bin blank break btos by ' +
                    'byte call can-do can-find case ' +
                    'case-sensitive center centered check chr ' +
                    'clear clipboard col colon color ' +
                    'column column-label columns compiler control ' +
                    'count-of cpstream create ctos current ' +
                    'current-changed current-lang current-language current-window cursor ' +
                    'database dataservers dbcodepage dbcollation dbname ' +
                    'dbparam dbrestrictions dbtaskid dbtype dbversion ' +
                    'dde deblank debug-list debugger decimals ' +
                    'declare def default default-noxlate default-window ' +
                    'define delete delimiter desc descending ' +
                    'dict dictionary disable disconnect disp ' +
                    'display distinct do dos down ' +
                    'drop each editing else ' +
                    'elseif enable encode end endif ' +
                    'entry error-status escape etime except ' +
                    'exclusive exclusive-lock exists export false ' +
                    'fetch field fields file-info file-information ' +
                    'fill find find-case-sensitive find-global find-next-occurrence ' +
                    'find-prev-occurrence find-select find-wrap-around first first-of ' +
                    'focus font font-based-grid for form ' +
                    'format frame frame-col frame-db frame-down ' +
                    'frame-field frame-file frame-index frame-line frame-name ' +
                    'frame-row frame-val frame-value from from-chars ' +
                    'from-pixels gateways get-byte get-codepages get-collations ' +
                    'get-key-value getbyte glob global ' +
                    'global-define go-on go-pending grant graphic-edge ' +
                    'group having header help hide ' +
                    'if import in index ' +
                    'indicator input input-output insert into ' +
                    'is is-attr-space join kblabel key-code ' +
                    'key-function key-label keycode keyfunction keylabel ' +
                    'keys keyword label last last-event ' +
                    'last-key last-of lastkey ldbname leave ' +
                    'library like line-count line-counter line-number ' +
                    'listing locked long lookup machine-class ' +
                    'map max-button member memptr message ' +
                    'message-lines mouse mpe new next ' +
                    'next-prompt no no-attr-space no-error no-fill ' +
                    'no-help no-hide no-label no-labels no-lobs no-lock ' +
                    'no-map no-message no-pause no-prefetch no-undo ' +
                    'no-validate no-wait not null num-aliases ' +
                    'num-dbs num-entries of off old ' +
                    'on open opsys option ' +
                    'or os-append os-command os-copy os-create-dir ' +
                    'os-delete os-dir os-rename os2 os400 ' +
                    'otherwise output overlay page page-bottom ' +
                    'page-num page-number page-top param parameter ' +
                    'pause pdbname persistent pixels preprocess ' +
                    'privileges proc-handle proc-status process program-name ' +
                    'progress prompt prompt-for promsgs propath ' +
                    'proversion put put-byte put-key-value putbyte ' +
                    'query query-tuning quit r-index rcode-information ' +
                    'readkey recid record-length rectangle ' +
                    'release repeat reposition retain retry ' +
                    'return revert revoke run save ' +
                    'schema scop scoped scoped-define screen ' +
                    'screen-io screen-lines scroll sdbname search ' +
                    'seek select self sequence session ' +
                    'set setuserid share share-lock shared ' +
                    'short show-stats skip some space ' +
                    'status stream stream-io string-xref system-dialog ' +
                    'table tab-stop term terminal text text-cursor ' +
                    'text-height text-seg-growth then this-procedure ' +
                    'time title to top-only trans ' +
                    'transaction trigger triggers trim true ' +
                    'undefine underline undo unformatted union ' +
                    'unique unix unless-hidden unsigned-short up ' +
                    'update use-index use-revvideo use-underline user ' +
                    'userid using v6frame value values ' +
                    'view view-as vms wait-for web-context ' +
                    'when where while widget-id window window-maximized ' +
                    'window-minimized window-normal with work-table workfile ' +
                    'write xcode xref yes _actailog ' +
                    '_actbilog _actbuffer _actindex _actiofile _actiotype ' +
                    '_actlock _actother _actpws _actrecord _actserver ' +
                    '_actspace _actsummary _block _buffstatus _cbit ' +
                    '_checkpoint _connect _control _db _dbstatus ' +
                    '_dcm _field _field-trig _file _file-trig ' +
                    '_filelist _index _index-field _license _list ' +
                    '_lock _lockreq _logging _memory _msg ' +
                    '_mstrblk _pcontrol _segments _sequence _serial-num ' +
                    '_servers _startup _trace _trans _user ' +
                    '_userio _userlock _view _view-col _view-ref';

  DefaultNonReservedKeywords: UnicodeString =
                               'abs absolute accelerator across add-events-procedure ' +
                               'add-first add-interval add-last advise alert-box allow-replication ' +
                               'ansi-only anywhere append appl-alert appl-alert-boxes ' +
                               'application as-cursor ask-overwrite attachment auto-endkey ' +
                               'auto-end-key auto-go auto-indent auto-resize auto-zap ' +
                               'available-formats average avg backwards base-key ' +
                               'batch batch-mode bgc bgcolor ' +
                               'binary bind-where block-iteration-display border-bottom border-bottom-chars ' +
                               'border-bottom-pixels border-left border-left-chars border-left-pixels border-right ' +
                               'border-right-chars border-right-pixels border-top border-top-chars border-top-pixels ' +
                               'both bottom box box-select box-selectable ' +
                               'browse browse-header btn-down-arrow btn-left-arrow btn-right-arrow ' +
                               'btn-up-arrow buffer buffer-chars buffer-compare buffer-copy ' +
                               'buffer-lines button buttons cache cache-size ' +
                               'cancel-break cancel-button can-query can-set caps ' +
                               'cdecl character_length charset checked clear-select ' +
                               'clear-selection code codepage codepage-convert col-of ' +
                               'colon-align colon-aligned color-table column-bgcolor column-dcolor ' +
                               'column-fgcolor column-font column-label-bgcolor column-label-dcolor column-label-fgcolor ' +
                               'column-label-font column-of column-scrolling com1 com2 ' +
                               'com3 com4 com5 com6 com7 ' +
                               'com8 com9 combo-box command complete ' +
                               'com-self con connect connected ' +
                               'constrained contains contents context context-popup ' +
                               'control-container convert convert-3d-colors convert-to-offset count copy-lob ' +
                               'cpcase cpcoll cpinternal cplog cpprint ' +
                               'cprcodein cprcodeout cpterm crc-value create-control ' +
                               'create-result-list-entry create-test-file current_date current-column ' +
                               'current-iteration current-result-row current-row-modified current-value cursor-char ' +
                               'cursor-line cursor-offset data-entry-return data-type date-format ' +
                               'day db-references dcolor dde-error dde-id ' +
                               'dde-item dde-name dde-topic debug default-button ' +
                               'default-extension defined delete-current-row delete-selected-row delete-selected-rows ' +
                               'deselect-focused-row deselect-rows deselect-selected-row design-mode dialog-box ' +
                               'dialog-help dir disabled display-message display-type ' +
                               'drag-enabled drop-down drop-down-list dump dynamic dynamic-function ' +
                               'echo edge edge-chars edge-pixels edit-can-undo ' +
                               'editor edit-undo empty end-key entered ' +
                               'eq error error-col error-column error-row ' +
                               'events event-type exp expand extended ' +
                               'extent external extract fetch-selected-row fgc ' +
                               'fgcolor file filename file-create-date file-create-time file-mod-date file-mod-time file-name ' +
                               'file-offset file-size file-type filled fill-in filters ' +
                               'first-child first-column first-proc first-procedure first-server ' +
                               'first-tab-item fixed-only focused-row font-table force-file ' +
                               'foreground forwards frame-spacing frame-x frame-y ' +
                               'frequency from-current full-height full-height-chars full-height-pixels ' +
                               'full-pathname full-width full-width-chars full-width-pixels function ' +
                               'ge get-blue get-blue-value get-char-property get-double ' +
                               'get-dynamic get-file get-float get-green get-green-value ' +
                               'get-iteration get-license get-long get-message get-number ' +
                               'get-pointer-value get-red get-red-value get-repositioned-row get-selected ' +
                               'get-selected-widget get-short get-signature get-size get-string ' +
                               'get-tab-item get-text-height get-text-height-chars get-text-height-pixels get-text-width ' +
                               'get-text-width-chars get-text-width-pixels get-unsigned-short grayed grid-factor-h ' +
                               'grid-factor-horizontal grid-factor-v grid-factor-vertical grid-set grid-snap ' +
                               'grid-unit-height grid-unit-height-chars grid-unit-height-pixels grid-unit-width grid-unit-width-chars ' +
                               'grid-unit-width-pixels grid-visible gt height height-chars ' +
                               'height-pixels help-context hidden horizontal hwnd ' +
                               'image image-down image-insensitive image-size image-size-chars ' +
                               'image-size-pixels image-up immediate-display indexed-reposition index-hint ' +
                               'info information init initial initial-dir ' +
                               'initial-filter initiate inner inner-chars inner-lines input-value ' +
                               'insert-backtab insert-file insert-row insert-string insert-tab instantiating-procedure ' +
                               'internal-entries is-lead-byte is-row-selected is-selected item ' +
                               'items-per-row join-by-sqldb keep-frame-z-order keep-messages keep-tab-order ' +
                               'key keyword-all label-bgc label-bgcolor label-dc ' +
                               'label-dcolor label-fgc label-fgcolor label-font label-pfc ' +
                               'label-pfcolor labels languages large large-to-small ' +
                               'last-child last-proc last-procedure last-server last-tab-item ' +
                               'lc le leading left-aligned left-trim ' +
                               'length line list-events list-items list-item-pairs list-query-attrs ' +
                               'list-set-attrs list-widgets load load-control loadcontrols ' +
                               'load-icon load-image load-image-down load-image-insensitive load-image-up ' +
                               'load-mouse-pointer load-small-icon log-id lookahead lower ' +
                               'lpt0 lpt1 lpt2 lpt3 lpt4 ' +
                               'lpt5 lpt6 lpt7 lpt8 lpt9 ' +
                               'lt manual-highlight margin-extra margin-height margin-height-chars ' +
                               'margin-height-pixels margin-width margin-width-chars margin-width-pixels matches ' +
                               'max max-chars max-data-guess max-height ' +
                               'max-height-chars max-height-pixels maximize maximum max-rows ' +
                               'max-size max-value max-width max-width-chars max-width-pixels ' +
                               'memory menu menubar menu-bar menu-item ' +
                               'menu-key menu-mouse message-area message-area-font message-line ' +
                               'min min-height min-height-chars min-height-pixels minimum ' +
                               'min-size min-value min-width min-width-chars min-width-pixels ' +
                               'mod modified modulo month mouse-pointer ' +
                               'movable move-after move-after-tab-item move-before move-before-tab-item ' +
                               'move-column move-to-bottom move-to-eof move-to-top multiple ' +
                               'multiple-key multitasking-interval must-exist name native ' +
                               'ne new-row next-column next-sibling next-tab-item ' +
                               'next-value no-apply no-assign no-bind-where no-box ' +
                               'no-column-scrolling no-convert no-current-value no-debug no-drag ' +
                               'no-echo no-focus no-index-hint no-join-by-sqldb no-lookahead ' +
                               'no-return-value no-row-markers no-scrolling no-separate-connection no-separators ' +
                               'no-underline no-word-wrap num-buttons num-columns num-copies ' +
                               'numeric numeric-format num-formats num-items num-iterations ' +
                               'num-lines num-locked-columns num-messages num-results num-selected ' +
                               'num-selected-rows num-selected-widgets num-tabs num-to-retain octet_length ' +
                               'ok ok-cancel on-frame on-frame-border ordinal ' +
                               'orientation os-drives os-error ' +
                               'os-getenv outer outer-join override owner ' +
                               'paged page-size page-width parent partial-key ' +
                               'pascal password-field pathname pfc pfcolor pinnable ' +
                               'pixels-per-col pixels-per-column pixels-per-row popup-menu popup-only ' +
                               'position precision preselect prev prev-column ' +
                               'prev-sibling prev-tab-item primary printer-control-handle printer-name ' +
                               'printer-port printer-setup private private-data prn procedure ' +
                               'progress-source proxy put-double put-float put-long ' +
                               'put-short put-string put-unsigned-short query-off-end question ' +
                               'radio-buttons radio-set random raw-transfer read-file ' +
                               'read-only real recursive refresh refreshable ' +
                               'remote remove-events-list replace replace-selection-text replication-create ' +
                               'replication-delete replication-write request resizable resize ' +
                               'retry-cancel return-inserted returns return-to-start-dir return-value ' +
                               'right-aligned right-trim round row ' +
                               'row-markers row-of rule rule-row rule-y ' +
                               'save-file screen-value scrollable scrollbar-h scrollbar-horizontal ' +
                               'scroll-bars scrollbar-v scrollbar-vertical scroll-delta scrolled-row-pos ' +
                               'scrolled-row-position scroll-horiz-value scrolling scroll-offset scroll-to-current-row ' +
                               'scroll-to-item scroll-to-selected-row scroll-vert-value se-check-pools section ' +
                               'se-enable-off se-enable-on selectable selected selected-items ' +
                               'select-focused-row selection-end selection-list selection-start selection-text ' +
                               'select-next-row select-prev-row select-repositioned-row select-row send ' +
                               'sensitive se-num-pools separate-connection separators server ' +
                               'set-blue set-blue-value set-break set-cell-focus set-contents ' +
                               'set-dynamic set-green set-green-value set-leakpoint set-pointer-value ' +
                               'set-property set-red set-red-value set-repositioned-row set-selection ' +
                               'set-size set-wait-state se-use-message side-label-handle side-labels ' +
                               'silent simple single size size-chars ' +
                               'size-pixels slider smallint sort source ' +
                               'sql sqrt start status-area status-area-font ' +
                               'status-bar stdcall stenciled stopped stored-procedure ' +
                               'string sub-average sub-count sub-max sub-maximum ' +
                               'sub-menu sub-menu-help sub-min sub-minimum substitute ' +
                               'substr substring sub-total subtype sum ' +
                               'suppress-warnings system-alert-boxes system-help tab-position target ' +
                               'temp-dir temp-directory temp-table terminate text-selected ' +
                               'three-d through thru tic-marks time-source ' +
                               'title-bgc title-bgcolor title-dc title-dcolor title-fgc ' +
                               'title-fgcolor title-font today toggle-box ' +
                               'tool-bar tooltip tooltips top topic ' +
                               'to-rowid total trailing trunc truncate ' +
                               'type unbuffered unique-id unload upper ' +
                               'use use-dict-exps use-filename use-text v6display ' +
                               'validate validate-condition validate-message valid-event valid-handle ' +
                               'var variable vertical virtual-height virtual-height-chars ' +
                               'virtual-height-pixels virtual-width virtual-width-chars virtual-width-pixels visible ' +
                               'wait warning weekday widget-enter widget-leave ' +
                               'widget-pool width width-chars width-pixels window-name ' +
                               'window-state window-system word-wrap x ' +
                               'x-of y year year-offset yes-no ' +
                               'yes-no-cancel y-of';

  DefaultEvents: UnicodeString =
                    'abort any-key any-printable append-line backspace ' +
                    'back-tab block blue bottom-column break-line ' +
                    'bs cancel cancel-move cancel-pick cancel-resize ' +
                    'choices choose close compile container-event ' +
                    'copy cr ctrl-alt-del ctrl-break ctrl-g ' +
                    'ctrl-j ctrl-l cursor-down cursor-left cursor-right ' +
                    'cursor-up cut data-refresh-line data-refresh-page dde-notify ' +
                    'default-action default-pop-up del del-char delete-char ' +
                    'delete-character delete-column delete-end-line delete-field delete-line ' +
                    'delete-word del-line deselect deselect-extend deselection ' +
                    'deselection-extend dismiss-menu dos-end down-arrow editor-backtab ' +
                    'editor-tab empty-selection end-box-selection end-error endkey ' +
                    'end-move end-resize end-search enter enter-menubar ' +
                    'erase esc execute exit ' +
                    'ff find-next find-previous focus-in formfeed ' +
                    'forward get go goto help-key ' +
                    'home horiz-end horiz-home horiz-scroll-drag ins ' +
                    'ins-char insert-column insert-field insert-field-data insert-field-label ' +
                    'insert-here insert-mode ins-line iteration-changed left ' +
                    'left-arrow left-end left-mouse-click left-mouse-dblclick left-mouse-down ' +
                    'left-mouse-up lf line-del line-down line-erase ' +
                    'linefeed line-ins line-left line-right line-up ' +
                    'main-menu menu-drop middle-mouse-click middle-mouse-dblclick middle-mouse-down ' +
                    'middle-mouse-up mouse-extend-click mouse-extend-dblclick mouse-extend-down mouse-extend-drag ' +
                    'mouse-extend-up mouse-menu-click mouse-menu-dblclick mouse-menu-down mouse-menu-drag ' +
                    'mouse-menu-up mouse-move mouse-move-click mouse-move-dblclick mouse-move-down ' +
                    'mouse-move-drag mouse-move-up mouse-select-click mouse-select-dblclick mouse-select-down ' +
                    'mouse-select-drag mouse-select-up move new-line next-error ' +
                    'next-frame next-page next-scrn next-word object ' +
                    'off-end off-home open-line-above options out-of-data ' +
                    'page-down page-erase page-left page-right page-right-text ' +
                    'page-up parent-window-close paste pgdn pgup ' +
                    'pick pick-area pick-both popup-menu-key prev-frame ' +
                    'prev-page prev-scrn prev-word recall red ' +
                    'remove reports reset resume-display ' +
                    'right right-arrow right-end right-mouse-click ' +
                    'right-mouse-dblclick right-mouse-down right-mouse-up row-display row-entry ' +
                    'row-leave save-as scrollbar-drag scroll-left ' +
                    'scroll-mode scroll-notify scroll-right select-extend selection ' +
                    'selection-extend settings shift-tab start-box-selection start-extend-box-selection ' +
                    'start-move start-resize start-search stop stop-display ' +
                    'tab top-column u1 u10 u2 ' +
                    'u3 u4 u5 u6 u7 ' +
                    'u8 u9 unix-end up-arrow value-changed ' +
                    'white window-close window-resized window-restored';

  DefaultDataTypes: UnicodeString =
    'char character com-handle component-handle date datetime datetime-tz dec ' +
    'decimal double float handle int ' +
    'integer int64 log logical longchar raw rowid ' +
    'widget widget-handle';

implementation

uses
  SynEditStrConst;

function TSynProgressSyn.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
    case Str^ of
      'a'..'z': Result := 1 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 1 + Ord(Str^) - Ord('A');
      '0'..'9': Result := 27 + Ord(Str^) - Ord('0');
      '_': Result := 37;
      '-': Result := 38;
      else Result := 0;
    end;
  end;

begin                       
  Result := 0;
  while IsIdentChar(Str^) do
  begin
{$IFOPT Q-}
    Result := 3 * Result + GetOrd;
{$ELSE}
    Result := (3 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    Inc(Str);
  end;
  Result := Result and $3FF;
  FStringLen := Str - FToIdent;
end;

function TSynProgressSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  FToIdent := MayBe;
  Entry := FHashList[HashKey(MayBe)];
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

procedure TSynProgressSyn.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FHashList[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

constructor TSynProgressSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FHashList := TSynHashEntryList.Create;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clRed;
  AddAttribute(FCommentAttri);

  FEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrEvent, SYNS_FriendlyAttrEvent);
  FEventAttri.Foreground := clOlive;
  AddAttribute(FEventAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  FIdentifierAttri.Foreground := clNavy;
  AddAttribute(FIdentifierAttri);

  FIncludeAttri := TSynHighlighterAttributes.Create(SYNS_AttrInclude, SYNS_FriendlyAttrInclude);
  FIncludeAttri.Foreground := clPurple;
  AddAttribute(FIncludeAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clMaroon;
  AddAttribute(FKeyAttri);

  FNonReservedKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  FNonReservedKeyAttri.Foreground := clTeal;
  AddAttribute(FNonReservedKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  AddAttribute(FNumberAttri);

  FPreprocessorAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FPreprocessorAttri.Foreground := clPurple;
  AddAttribute(FPreprocessorAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FDataTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  FDataTypeAttri.Foreground := clSilver;
  AddAttribute(FDataTypeAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FDefaultFilter := SYNS_FilterProgress;

  EnumerateKeywords(Ord(tkKey), DefaultKeywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkNonReserved), DefaultNonReservedKeywords,
    IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkEvent), DefaultEvents, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkDataType), DefaultDataTypes, IsIdentChar,
    DoAddKeyword);
  SetAttributesOnChange(DefHighlightChange);
end;

destructor TSynProgressSyn.Destroy;
begin
  FHashList.Free;
  inherited Destroy;
end;

procedure TSynProgressSyn.IdentProc;
begin
  FTokenID := IdentKind(FLine + Run);
  Inc(Run, FStringLen);
end;

procedure TSynProgressSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynProgressSyn.NumberProc;
var
  p: PWideChar;
begin
  FTokenID := tkNumber;
  p := PWideChar(@FLine[Run]);
  repeat
    Inc(p);
  until not CharInSet(p^, ['0'..'9']);
  Run := p - FLine;
end;

procedure TSynProgressSyn.PreprocessorDefinitionProc;
var
  p: PWideChar;
begin
  FTokenID := tkPreprocessor;
  p := PWideChar(@FLine[Run]);
  while p^ <> #0 do
  begin
    case p^ of
      '~': if (p + 1)^ = #0 then
             FRange := rsPreprocessorDef;
    end;
    Inc(p);
  end;
  Run := p - FLine;
end;

procedure TSynProgressSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynProgressSyn.StringProc;
var
  p: PWideChar;
begin
  FTokenID := tkString;
  p := PWideChar(@FLine[Run]);
  repeat
    Inc(p);
  until (p^ = #0) or (p^ = '"');
  if (p^ = '"') then Inc(p);
  Run := p - FLine;
end;

procedure TSynProgressSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynProgressSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynProgressSyn.AsciiCharProc;
var
  p: PWideChar;
begin
  FTokenID := tkString;
  p := PWideChar(@FLine[Run]);
  repeat
    Inc(p);
  until (p^ = #0) or (p^ = '''');
  if (p^ = '''') then Inc(p);
  Run := p - FLine;
end;

procedure TSynProgressSyn.SlashProc;
var
  p: PWideChar;
begin
  p := PWideChar(@FLine[Run]);
  Inc(p);
  case p^ of
    '*': begin  {c style comments}
           FTokenID := tkComment;
           FRange := rsComment;
           FCommentLevel := 1;
           Inc(p);
           while (p^ <> #0) and (FRange = rsComment) do
           begin
             case p^ of
               '*': begin
                      Inc(p);
                      if p^ = '/' then
                      begin
                        Inc(p);
                        Dec(FCommentLevel);
                        if FCommentLevel = 0 then
                          FRange := rsNone;
                      end;
                    end;
               '/': begin
                      Inc(p);
                      if p^ = '*' then
                      begin
                        Inc(p);
                        Inc(FCommentLevel); // Max 65535 commentlevels.
                      end;
                    end;
             else
               Inc(p);
             end;
           end;
         end;
  else  {division}
    FTokenID := tkSymbol;
  end;
  Run := p - FLine;
end;

procedure TSynProgressSyn.CommentRangeProc;
var
  p: PWideChar;
begin
  FTokenID := tkComment;
  p := PWideChar(@FLine[Run]);

  if p^ = #0 then
  begin
    NullProc;
    Exit;
  end;

  while (p^ <> #0) and (FRange = rsComment) do
  begin
    case p^ of
      '*': begin
             Inc(p);
             if p^ = '/' then
             begin
               Inc(p);
               Dec(FCommentLevel);
               if FCommentLevel = 0 then
                 FRange := rsNone;
             end;
           end;
      '/': begin
             Inc(p);
             if p^ = '*' then
             begin
               Inc(p);
               Inc(FCommentLevel);
             end;
           end;
    else
      Inc(p);
    end;
  end;
  Run := p - FLine;
end;

procedure TSynProgressSyn.IncludeRangeProc;
var
  p: PWideChar;
begin
  FTokenID := tkInclude;
  p := PWideChar(@FLine[Run]);

  if p^ = #0 then
  begin
    NullProc;
    Exit;
  end;

  while p^ <> #0 do
  begin
    case p^ of
      '}': begin
             Dec(FIncludeLevel);
             if FIncludeLevel = 0 then
             begin
               FRange := rsNone;
               Break;
             end
             else
               Inc(p);
           end;
    else
      Inc(p);
    end;
  end;
  Run := p - FLine;
end;

procedure TSynProgressSyn.PreprocessorRangeProc;
var
  p: PWideChar;
begin
  FTokenID := tkPreprocessor;
  p := PWideChar(@FLine[Run]);

  if p^ = #0 then
  begin
    NullProc;
    Exit;
  end;

  while (p^ <> #0) and (FRange = rsPreprocessor) do
  begin
    case p^ of
      '{': Inc(FPreProcessorLevel);
      '}': begin
             Dec(FPreProcessorLevel);
             if FPreProcessorLevel = 0 then
               FRange := rsNone;
           end;
    end;
    Inc(p);
  end;
  Run := p - FLine;
end;

procedure TSynProgressSyn.PreprocessorDefinitionRangeProc;
var
  p: PWideChar;
begin
  FTokenID := tkPreprocessor;
  p := PWideChar(@FLine[Run]);

  if Run = 0 then
    FRange := rsNone;

  if p^ = #0 then
  begin
    NullProc;
    Exit;
  end;

  while p^ <> #0 do
  begin
    case p^ of
      '~': if (p+1)^ = #0 then
             FRange := rsPreprocessorDef;
    end;
    Inc(p);
  end;
  Run := p - FLine;
end;

procedure TSynProgressSyn.BraceOpenProc;
var
  p: PWideChar;

  function LevelCount: Integer;
  begin
    if FTokenID = tkInclude then
      Result := FIncludeLevel
    else
      Result := FPreProcessorLevel;
  end;

begin
  p := PWideChar(@FLine[Run]);

  Inc(p);
  case p^ of
    'A'..'Z', 'a'..'z', '_': FTokenID := tkInclude;
    '&'                    : FTokenID := tkPreprocessor;
  else
    FTokenID := tkUnknown;
  end;

  case FTokenID of
    tkInclude     : FIncludeLevel      := 1;
    tkPreprocessor: FPreProcessorLevel := 1;
  end;

  while LevelCount > 0 do
  begin
    case p^ of
      #0 : begin
             if FTokenID = tkInclude then
               FRange := rsInclude
             else
               FRange := rsPreprocessor;
             Break;
           end;
      '}': case FTokenID of
             tkInclude     : Dec(FIncludeLevel);
             tkPreprocessor: Dec(FPreProcessorLevel);
           end;
      '{': case FTokenID of
             tkInclude     : Inc(FIncludeLevel);
             tkPreprocessor: Inc(FPreProcessorLevel);
           end;
    end;
    Inc(p);
  end;
  Run := p - FLine;
end;

procedure TSynProgressSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsInclude: IncludeRangeProc;
    rsPreprocessor: PreprocessorRangeProc;
    rsPreprocessorDef: PreprocessorDefinitionRangeProc;
    rsComment: CommentRangeProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z','a'..'z','_': IdentProc;
      '0'..'9': NumberProc;
      '''': AsciiCharProc;
      '"': StringProc;
      '{': BraceOpenProc;
      '+','-','*','@',':','=','<','>','.','^','(',')','[',']': SymbolProc;
      '&': PreprocessorDefinitionProc;
      '/': SlashProc;
      else UnknownProc;
    end;
  end;
  inherited;
end;

function TSynProgressSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := nil;
end;

function TSynProgressSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynProgressSyn.GetRange: Pointer;
var
  rng: TRangeInfo;
begin
  rng.Range := Ord(FRange);
  rng.Level := 0;
  case FRange of
    rsComment: rng.Level := FCommentLevel;
    rsInclude: rng.Level := FIncludeLevel;
    rsPreProcessor: rng.Level := FPreProcessorLevel;
  end;
  Result := rng.Ptr;
end;

function TSynProgressSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynProgressSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkEvent: Result := FEventAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkInclude: Result := FIncludeAttri;
    tkKey: Result := FKeyAttri;
    tkNonReserved: Result := FNonReservedKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkPreprocessor: Result := FPreprocessorAttri;
    tkSpace: Result := FSpaceAttri;
    tkDataType: Result := FDataTypeAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynProgressSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynProgressSyn.ResetRange;
begin
  FRange := rsNone;
  FCommentLevel := 0;
  FIncludeLevel := 0;
  FPreProcessorLevel := 0;
end;

procedure TSynProgressSyn.SetRange(Value: Pointer);
var
  rng: TRangeInfo;
begin
  rng := TRangeInfo(Value);
  FRange := TRangeState(rng.Range);
  FCommentLevel := 0;
  FIncludeLevel := 0;
  FPreProcessorLevel := 0;
  case FRange of
    rsComment: FCommentLevel := rng.Level;
    rsInclude: FIncludeLevel := rng.Level;
    rsPreProcessor: FPreProcessorLevel := rng.Level;
  end;
end;

function TSynProgressSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterProgress;
end;

function TSynProgressSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '-', '_', '0'..'9', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynProgressSyn.GetLanguageName: string;
begin
  Result := SYNS_LangProgress;
end;

function TSynProgressSyn.GetSampleSource: UnicodeString;
begin
  Result := '&scoped-define FirstChar 65'#13#10+
            '&scoped-define LastChar  90'#13#10+
            #13#10+
            'def var i as int no-undo.'#13#10+
            'def var s as char no-undo.'#13#10+
            #13#10+
            'function GetRandomChar returns char (input SomeValue as int):'#13#10+
            '  return chr(random({&FirstChar}, {&LastChar})).'#13#10+
            'end.'#13#10+
            #13#10+
            'procedure ClearString:'#13#10+
            '  def input-output param str as char no-undo.'#13#10+
            '  str = "".'#13#10+
            'end.'#13#10+
            #13#10+
            'run ClearString(input-output s).'#13#10+
            'do i = 1 to 100:'#13#10+
            '  s = s + GetRandomChar(17).'#13#10+
            'end.'#13#10+
            'display s.';
end;

class function TSynProgressSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangProgress;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynProgressSyn);
{$ENDIF}
end.
