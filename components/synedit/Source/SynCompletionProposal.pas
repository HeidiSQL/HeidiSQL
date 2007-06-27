{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynCompletionProposal.pas, released 2000-04-11.
The Original Code is based on mwCompletionProposal.pas by Cyrille de Brebisson,
part of the mwEdit component suite.
Portions created by Cyrille de Brebisson are Copyright (C) 1999
Cyrille de Brebisson. All Rights Reserved.

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

$Id: SynCompletionProposal.pas,v 1.36 2002/05/30 14:31:35 harmeister Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynCompletionProposal;

{$I SynEdit.inc}

interface

uses
  SysUtils,
  Classes,
{$IFDEF SYN_CLX}
  Qt,
  Types,
  QControls,
  QGraphics,
  QForms,
  QStdCtrls,
  QExtCtrls,
  QMenus,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Menus,
{$ENDIF}
  SynEditTypes,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditKbdHandler,
  SynEdit;

type
  SynCompletionType = (ctCode, ctHint, ctParams);

  TSynForm = {$IFDEF SYN_COMPILER_3_UP}TCustomForm{$ELSE}TForm{$ENDIF};

  TSynBaseCompletionProposalPaintItem = procedure(Index: Integer; ACanvas: TCanvas;
    Rect: TRect; var aCustomDraw: boolean) of object;

  TCodeCompletionEvent = procedure(var Value: string; Shift: TShiftState; Index: Integer; EndToken: Char)
    of object;

  //GBN 14/11/2001
  TAfterCodeCompletionEvent = procedure(const Value: string; Shift: TShiftState;
    Index: Integer; EndToken: Char) of object;

  TValidateEvent = procedure(Sender: TObject; Shift: TShiftState; EndToken: Char) of object; //GBN 15/11/2001, Added EndToken

  TCompletionParameter = procedure(Sender : TObject; CurrentIndex : Integer;
    VAR Level, IndexToDisplay : Integer; VAR Key : Char;
    VAR DisplayString : String) of object;

  TCompletionExecute = procedure(Kind : SynCompletionType; Sender : TObject;
    VAR AString : String; var x, y : Integer; var CanExecute: Boolean) of object;

  TCompletionChange = procedure(Sender: TObject; AIndex: Integer) of object;

  TSynCompletionOption = (scoAnsiStrings,         //Use Ansi comparison during string operations
                          scoCaseSensitive,       //use case sensitivity to do matches
                          scoLimitToMatchedText,  //Limit the matched text to only what they have typed in
                          scoTitleIsCentered,     //Center the title in the box if you choose to use titles
                          scoUseInsertList,       //Use the InsertList to insert text instead of the ItemList (which will be displayed)
                          scoUsePrettyText,       //Use the PrettyText function to output the words
                          scoUseBuiltInTimer,     //Use the built in timer and the trigger keys to execute the proposal as well as the shortcut
                          scoEndCharCompletion);  //When an end char is pressed, it triggers completion to occur (like the Delphi IDE)

  TSynCompletionOptions = set of TSynCompletionOption;

const
  DefaultProposalOptions = [scoLimitToMatchedText, scoEndCharCompletion];

type

  TSynBaseCompletionProposalForm = class(TSynForm)
  protected
    FCurrentString  : string;
    FOnKeyPress     : TKeyPressEvent;
    FOnKeyDelete    : TNotifyEvent;
    FOnPaintItem    : TSynBaseCompletionProposalPaintItem;
    FChangePosition : TCompletionChange;
    FItemList       : TStrings;
    FInsertList     : TStrings;
    FAssignedList   : TStrings;
    FPosition       : Integer;
    FNbLinesInWindow: Integer;
    FTitleFontHeight: Integer;
    FFontHeight     : integer;
    Scroll          : TScrollBar;
    FOnValidate     : TValidateEvent;
    FOnCancel       : TNotifyEvent;
    FClSelect       : TColor;
    fClSelectText   : TColor;
    fClBackGround   : TColor;

//These are the relections of the Options property of the CompletionProposal
    FAnsi           : boolean;
    fCase           : boolean;
    FUsePrettyText  : Boolean;
    FMatchText      : Boolean;

    FMouseWheelAccumulator: integer;
    FDisplayKind    : SynCompletionType;
    FParameterToken : TCompletionParameter;
    FCurrentIndex   : Integer;
    FCurrentLevel   : Integer;
    FDefaultKind    : SynCompletionType;
    FBiggestWord    : string;
    FEndOfTokenChr  : String;
    fTriggerChars   : String;
    OldShowCaret    : Boolean;
    fHeightBuffer   : Integer;
    procedure SetCurrentString(const Value: string);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure Paint; override;
    procedure ScrollGetFocus(Sender: TObject);
    procedure Activate; override;
    procedure Deactivate; override;
    procedure MoveLine (cnt: Integer);
    procedure ScrollChange(Sender: TObject);
    procedure ScrollOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure SetItemList(const Value: TStrings);
    procedure SetInsertList(const Value: TStrings);
    procedure SetPosition(const Value: Integer);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
{$IFDEF SYN_CLX}
{$ELSE}
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
{$ENDIF}
    procedure StringListChange(Sender: TObject);
    procedure DoDoubleClick(Sender : TObject);
    function intLowerCase (s: string): string;
    procedure DoFormShow(Sender: TObject);
    procedure DoFormHide(Sender: TObject);
    procedure AdjustScrollBarPosition;
  private
    Bitmap: TBitmap; // used for drawing
    fCurrentEditor: TComponent;
    FUseInsertList: boolean;
    fTitle: string;
    fTitleFont: TFont;
    fFont: TFont;
    fClTitleBackground: TColor;
    FCenterTitle: boolean;
{$IFDEF SYN_CLX}
{$ELSE}
    procedure WMActivate (var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBackgrnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE; //GBN 24/02/2002
{$ENDIF}
    procedure SetTitle(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    procedure TitleFontChange(Sender: TObject);
    procedure FontChange(Sender: TObject);
  public
    constructor Create(AOwner: Tcomponent); override;
{$IFDEF SYN_CLX}
{$ELSE}
    procedure CreateParams (var Params: TCreateParams); override;
{$ENDIF}
    destructor destroy; override;
    property DisplayType : SynCompletionType read FDisplayKind write FDisplayKind;
    property DefaultType : SynCompletionType read FDefaultKind write FDefaultKind;
//  published
    property CurrentString  : string read FCurrentString write SetCurrentString;
    Property CurrentIndex   : Integer read FCurrentIndex write FCurrentIndex;
    Property CurrentLevel   : Integer read FCurrentLevel write FCurrentLevel;
    Property OnParameterToken : TCompletionParameter read FParameterToken write FParameterToken;
    property OnKeyPress     : TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyDelete    : TNotifyEvent read FOnKeyDelete write FOnKeyDelete;
    property OnPaintItem    : TSynBaseCompletionProposalPaintItem read FOnPaintItem
      write FOnPaintItem;
    property OnValidate     : TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel       : TNotifyEvent read FOnCancel write FOnCancel;
    property ItemList       : TStrings   read FItemList write SetItemList;
    Property InsertList     : TStrings   read FInsertList write SetInsertList;
    property AssignedList   : TStrings   read FAssignedList write FAssignedList;
    property Position       : Integer    read FPosition write SetPosition;
    property NbLinesInWindow: Integer    read FNbLinesInWindow
      write SetNbLinesInWindow;
    property BiggestWord    : string     read FBiggestWord write FBiggestWord;
    property Title          : string     read fTitle write SetTitle;
    property ClSelect       : TColor     read FClSelect write FClSelect;
    property ClSelectedText : TColor     read fClSelectText write fClSelectText;
    property ClBackground   : TColor     read fClBackGround write fClBackGround;
    property ClTitleBackground: TColor   read fClTitleBackground write fClTitleBackground;

    property UsePrettyText  : boolean    read FUsePrettyText write FUsePrettyText default False;
    property UseInsertList  : boolean    read FUseInsertList write FUseInsertList default False;
    property CenterTitle    : boolean    read FCenterTitle   write FCenterTitle   default True;
    property AnsiStrings    : boolean    read fansi write fansi;
    property CaseSensitive  : Boolean    read fCase write fCase;
    property CurrentEditor  : TComponent read fCurrentEditor write fCurrentEditor;
    property MatchText      : Boolean    read fMatchText write fMatchText;
    property EndOfTokenChr  : String     read FEndOfTokenChr write FEndOfTokenChr;
    property TriggerChars   : String     read FTriggerChars write FTriggerChars;

    property TitleFont      : TFont      read fTitleFont write SetTitleFont;
    property Font           : TFont      read fFont      write SetFont;
  end;

  TSynBaseCompletionProposal = class(TComponent)
  private
    FForm: TSynBaseCompletionProposalForm;
    FOnExecute: TCompletionExecute;
    FWidth: Integer;
    FBiggestWord  : string;
    FDotOffset    : Integer;
    FOldPos       : Integer;
    FOldLeft      : Integer;
    FOldStr       : String;
    fOptions      : TSynCompletionOptions;
    function GetClSelect: TColor;
    procedure SetClSelect(const Value: TColor);
    function GetCurrentString: string;
    function GetItemList: TStrings;
    function GetInsertList: TStrings;
    function GetNbLinesInWindow: Integer;
    function GetOnCancel: TNotifyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnPaintItem: TSynBaseCompletionProposalPaintItem;
    function GetOnValidate: TValidateEvent;
    function GetPosition: Integer;
    procedure SetCurrentString(const Value: string);
    procedure SetItemList(const Value: TStrings);
    procedure SetInsertList(const Value: TStrings);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnCancel(const Value: TNotifyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnPaintItem(const Value: TSynBaseCompletionProposalPaintItem);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    function GetOnKeyDelete: TNotifyEvent;
    procedure SetOnKeyDelete(const Value: TNotifyEvent);
    procedure SetWidth(Value: Integer);
    function GetDisplayKind: SynCompletionType;
    procedure SetDisplayKind(const Value: SynCompletionType);
    function GetParameterToken: TCompletionParameter;
    procedure SetParameterToken(const Value: TCompletionParameter);
    function GetDefaultKind: SynCompletionType;
    procedure SetDefaultKind(const Value: SynCompletionType);
    procedure SetUseBiggestWord(const Value: String);
    function IsEndToken(AChar : Char) : Boolean;
    function GetClBack: TColor;
    procedure SetClBack(const Value: TColor);
    function GetClSelectedText: TColor;
    procedure SetClSelectedText(const Value: TColor);
    function GetEndOfTokenChar: string;
    procedure SetEndOfTokenChar(const Value: string);
    function GetClTitleBackground: TColor;
    procedure SetClTitleBackground(const Value: TColor);
    procedure SetTitle(const Value: string);
    function GetTitle: string;
    function GetFont: TFont;
    function GetTitleFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    function GetOptions: TSynCompletionOptions;
    function GetTriggerChars: String;
    procedure SetTriggerChars(const Value: String);
    function GetOnChange: TCompletionChange;
    procedure SetOnChange(const Value: TCompletionChange);
  protected
    procedure loaded; override;
    procedure SetOptions(const Value: TSynCompletionOptions); virtual;
    procedure EditorCancelMode(Sender: TObject); virtual;                      //GBN 13/11/2001
    procedure HookedEditorCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: char;
      Data: pointer; HandlerData: pointer); virtual;                           //GBN 13/11/2001
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(s: string; x, y: integer);
    procedure ExecuteEx(s: string; x, y: integer; Kind : SynCompletionType
      {$IFDEF SYN_COMPILER_4_UP} = ctCode {$ENDIF}); virtual;
    procedure Activate;
    procedure Deactivate;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyDelete: TNotifyEvent read GetOnKeyDelete write SetOnKeyDelete;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property OnCancel: TNotifyEvent read GetOnCancel write SetOnCancel;
    property CurrentString: string read GetCurrentString write SetCurrentString;
    property DotOffset : Integer read FDotOffset write FDotOffset;
    property DisplayType : SynCompletionType read GetDisplayKind write SetDisplayKind;
    property Form: TSynBaseCompletionProposalForm read FForm write FForm;
    property PreviousWord: String read FOldStr;
  published
    property DefaultType : SynCompletionType read GetDefaultKind write SetDefaultKind;
    property Options: TSynCompletionOptions read GetOptions write SetOptions;
    property OnExecute: TCompletionExecute read FOnExecute write FOnExecute;
    property OnParameterToken: TCompletionParameter read GetParameterToken
      write SetParameterToken;
    property OnPaintItem: TSynBaseCompletionProposalPaintItem
      read GetOnPaintItem write SetOnPaintItem;
    property OnChange: TCompletionChange read GetOnChange write SetOnChange;
    procedure ClearList;
    function DisplayItem(AIndex : Integer) : String;
    function InsertItem(AIndex : Integer) : String;
    Procedure AddItemAt(Where : Integer; ADisplayText, AInsertText : String);
    Procedure AddItem(ADisplayText, AInsertText : String);
    property ItemList: TStrings read GetItemList write SetItemList;
    procedure ResetAssignedList;
    property InsertList: TStrings read GetInsertList write SetInsertList;
    property Position: Integer read GetPosition write SetPosition;
    property NbLinesInWindow: Integer read GetNbLinesInWindow
      write SetNbLinesInWindow;
    property ClSelect: TColor read GetClSelect write SetClSelect;
    property ClSelectedText: TColor read GetClSelectedText write SetClSelectedText;
    property ClBackground: TColor read GetClBack write SetClBack;
    property Width: Integer read FWidth write SetWidth;
    property BiggestWord: string read FBiggestWord write SetUseBiggestWord;
    property EndOfTokenChr: string read GetEndOfTokenChar write SetEndOfTokenChar;
    property TriggerChars: String read GetTriggerChars write SetTriggerChars;
    property Title: string read GetTitle write SetTitle;
    property ClTitleBackground: TColor read GetClTitleBackground write SetClTitleBackground;
    property Font: TFont read GetFont write SetFont;
    property TitleFont: TFont read GetTitleFont write SetTitleFont;
  end;

  TSynCompletionProposal = class(TSynBaseCompletionProposal)
  private
    fEditors: TList;
    FShortCut: TShortCut;
    fKeyDownProc: TKeyDownProc;
    fKeyPressProc: TKeyPressProc;
    fNoNextKey: Boolean;
    FOnCodeCompletion: TCodeCompletionEvent;
    fTimer: TTimer;
    fTimerInterval: Integer;
    fCurEditor: Integer;
    FOnAfterCodeCompletion: TAfterCodeCompletionEvent; //GBN 18/11/2001
    FOnCancelled: TNotifyEvent; //GBN 13/11/2001
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure HandleOnKeyDelete(Sender: TObject);
    procedure HandleOnCancel(Sender: TObject);
    procedure HandleOnValidate(Sender: TObject; Shift: TShiftState; EndToken: Char);
    procedure HandleOnKeyPress(Sender: TObject; var Key: Char);
    procedure HandleDblClick(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    procedure TimerExecute(Sender: TObject);
    function GetPreviousToken(FEditor: TCustomSynEdit): string;
    function GetTimerInterval: Integer;
    procedure SetTimerInterval(const Value: Integer);
    function GetFEditor: TCustomSynEdit;
    function GetEditor(i: integer): TCustomSynEdit;
    procedure RemoveCurrentEditor;
    procedure InternalCancelCompletion; //GBN 25/02/2002
  protected
    procedure DoExecute(AEditor: TCustomSynEdit); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetShortCut(Value: TShortCut);
    procedure SetOptions(const Value: TSynCompletionOptions); override;
    procedure EditorCancelMode(Sender: TObject); override; //GBN 13/11/2001
    procedure HookedEditorCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: char;
      Data: pointer; HandlerData: pointer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Editors[i: integer]: TCustomSynEdit read GetEditor;
    procedure AddEditor(AEditor: TCustomSynEdit);
    function RemoveEditor(AEditor: TCustomSynEdit): boolean;
    function EditorsCount: integer;
    procedure ExecuteEx(s: string; x, y: integer; Kind : SynCompletionType
      {$IFDEF SYN_COMPILER_4_UP} = ctCode {$ENDIF}); override;
    procedure ActivateCompletion; //GBN 13/11/2001
    procedure CancelCompletion; //GBN 11/11/2001
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property Editor: TCustomSynEdit read GetFEditor write SetEditor;
    property TimerInterval: Integer read GetTimerInterval write SetTimerInterval;
    property EndOfTokenChr;
    property OnCodeCompletion: TCodeCompletionEvent
      read FOnCodeCompletion write FOnCodeCompletion;
    property OnAfterCodeCompletion: TAfterCodeCompletionEvent read FOnAfterCodeCompletion write FOnAfterCodeCompletion;
    property OnCancelled: TNotifyEvent read FOnCancelled write FOnCancelled;
  end;

  TSynAutoComplete = class(TComponent)
  private
    FShortCut: TShortCut;
    fEditor: TCustomSynEdit;
    fAutoCompleteList: TStrings;
    fKeyDownProc : TKeyDownProc;
    fKeyPressProc : TKeyPressProc;
    fNoNextKey : Boolean;
    FEndOfTokenChr: string;
    FOnBeforeExecute: TNotifyEvent;  //GBN 2002-14-04
    FOnAfterExecute: TNotifyEvent;   //GBN 2002-14-04
    procedure SetAutoCompleteList(List: TStrings);
    procedure SetEditor(const Value: TCustomSynEdit);
  protected
    procedure SetShortCut(Value: TShortCut);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual;
    procedure EditorKeyPress(Sender: TObject; var Key: char); virtual;
    function GetPreviousToken(Editor: TCustomSynEdit): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure Execute(token: string; Editor: TCustomSynEdit);
    function RemoveEditor(aEditor: TCustomSynEdit): boolean;
    function GetTokenList: string;
    function GetTokenValue(Token: string): string;
  published
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property Editor: TCustomSynEdit read fEditor write SetEditor;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
  end;

  Procedure PrettyTextOut(c: TCanvas; x, y: integer; s: String;
                          DoAlign: Boolean; BiggestWord: string;
                          Highlighted: Boolean); //D.J

implementation

uses
  SynEditKeyConst, SynEditStrConst, SynEditTextBuffer, SynEditMiscProcs;

Type TProtectedAccessEditor=class(TCustomSynEdit); //GBN 13/11/2001

//GBN 10/11/2001
//Moved from completion component
function FormatParamList(const S: String; CurrentIndex: Integer): string;
Var i: Integer;
List: TStrings;
begin
  result := '';
  List:=TStringList.Create;
  try
    List.CommaText:=S;
    for i := 0 to List.Count - 1 do
    begin
      if i = CurrentIndex then
        Result := Result + #9 + List[i] + #9
      else Result := Result + List[i];
      if i < List.Count - 1 then
        Result := Result + ', ';
    end;
  finally
  List.Free;
  end;
end;
// End GBN 10/11/2001

{ TSynBaseCompletionProposalForm }

constructor TSynBaseCompletionProposalForm.Create(AOwner: TComponent);
begin
{$IFDEF SYN_CPPB_1}
  CreateNew(AOwner, 0);
{$ELSE}
  CreateNew(AOwner);
{$ENDIF}
  FItemList := TStringList.Create;
  FInsertList := TStringList.Create;
  fAssignedList := TStringList.Create;
  FMatchText := False;
{$IFDEF SYN_CLX}
  BorderStyle := fbsNone;
{$ELSE}
  BorderStyle := bsNone;
{$ENDIF}
  Scroll := TScrollBar.Create(self);
  Scroll.Kind := sbVertical;
{$IFNDEF SYN_CLX}
  Scroll.ParentCtl3D := False;
{$ENDIF}
  Scroll.OnChange := ScrollChange;
  Scroll.OnScroll := ScrollOnScroll;
  Scroll.Parent := self;
  Scroll.OnEnter := ScrollGetFocus;
  Visible := false;

  FTitleFont := TFont.Create;
  FTitleFont.Name := 'MS Sans Serif';
  FTitleFont.Size := 8;
  FTitleFont.Style := [fsBold];
  FTitleFont.OnChange := TitleFontChange;
  Canvas.Font.Assign(FTitleFont);
  FTitleFontHeight := Canvas.TextHeight('CompletionProposal');

  FFont := TFont.Create;
  FFont.Name := 'MS Sans Serif';
  FFont.Size := 8;
  FFont.OnChange := FontChange;
  Canvas.Font.Assign(FFont);
  FFontHeight := Canvas.TextHeight('CompletionProposal');

  ClSelect := clHighlight;
  ClSelectedText := clHighlightText;
  ClBackground := clWindow;
  TitleFont.Color := clBtnText;
  ClTitleBackground := clBtnFace;
  fHeightBuffer := 0;

  TStringList(FItemList).OnChange := StringListChange;
  bitmap := TBitmap.Create;
  fTitle := '';
  NbLinesInWindow := 8;
  OnDblClick := DoDoubleClick;

  OnShow := DoFormShow;
  OnHide := DoFormHide;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynBaseCompletionProposalForm.CreateParams (var Params: TCreateParams);
const
  ThickFrames: array[Boolean] of DWORD = (0, WS_THICKFRAME);
begin
  inherited;
  with Params do begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW;
  end;
end;
{$ENDIF}

procedure TSynBaseCompletionProposalForm.Activate;
begin
  Visible := True;
  if DisplayType = ctCode then
    TCustomSynEdit (CurrentEditor).AddFocusControl(self);
end;

procedure TSynBaseCompletionProposalForm.Deactivate;
begin
  if FormStyle = fsNormal then
  begin
    try
      if (DisplayType = ctCode) then
        TCustomSynEdit (CurrentEditor).RemoveFocusControl(self);
    except
      on exception do;
    end;
    Visible := False;
  end;
end;

destructor TSynBaseCompletionProposalForm.destroy;
begin
  bitmap.free;
//  Scroll.Free;                                                                //DDH The form will free this
  FItemList.Free;
  FInsertList.Free;
  fAssignedList.Free;
  FTitleFont.Free;
  FFont.Free;
  inherited destroy;
end;

procedure TSynBaseCompletionProposalForm.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if DisplayType = ctCode then
  begin
    case Key of
      //GBN 24/02/2002 Added TAB and Space key as doing same as enter as per Visual Studio
      SYNEDIT_RETURN,
      SYNEDIT_TAB,
      SYNEDIT_SPACE  : if Assigned(OnValidate) then OnValidate(Self, Shift, #0); //GBN 15/11/2001
      SYNEDIT_ESCAPE : if Assigned(OnCancel) then OnCancel(Self);
      SYNEDIT_LEFT   : begin
                         if Assigned(OnCancel) then OnCancel(Self);
                         //Since we have control, we need to re-send the key to
                         //the editor so that the cursor behaves properly
                         if Assigned(CurrentEditor) then
                           TCustomSynEdit(CurrentEditor).CommandProcessor(ecLeft, 'a', nil);
                       end;
      SYNEDIT_RIGHT  : begin
                         if Assigned(OnCancel) then OnCancel(Self);
                         //Since we have control, we need to re-send the key to
                         //the editor so that the cursor behaves properly
                         if Assigned(CurrentEditor) then
                           TCustomSynEdit(CurrentEditor).CommandProcessor(ecRight, 'a', nil);
                       end;
      SYNEDIT_PRIOR  : MoveLine (NbLinesInWindow * -1);
      SYNEDIT_NEXT   : MoveLine (NbLinesInWindow);
      SYNEDIT_END    : Position := ItemList.count - 1;
      SYNEDIT_HOME   : Position := 0;
      SYNEDIT_UP     : if ssCtrl in Shift then
                         Position := 0
                       else MoveLine (-1);
      SYNEDIT_DOWN   : if ssCtrl in Shift then
                         Position := ItemList.count - 1
                       else MoveLine (1);
      SYNEDIT_BACK   : if (Shift = []) then
                       begin
                         if length(CurrentString) > 0 then
                         begin
                           CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);
                           if Assigned (OnKeyDelete) then OnKeyDelete(Self);
                         end else begin
                           if Assigned(OnCancel) then OnCancel(Self);
                           //Since we have control, we need to re-send the key to
                           //the editor so that the cursor behaves properly
                           if Assigned(CurrentEditor) then
                             TCustomSynEdit(CurrentEditor).CommandProcessor(ecDeleteLastChar, 'a', nil);
                         end;
                       end;
    end;
  end else if DisplayType = ctParams then
  begin
    case Key of
      SYNEDIT_RETURN : begin
                         Top := Top + TCustomSynEdit(CurrentEditor).LineHeight;
                       end;
      SYNEDIT_ESCAPE, SYNEDIT_PRIOR, SYNEDIT_NEXT, SYNEDIT_HOME, SYNEDIT_UP, SYNEDIT_DOWN :
                       begin
                          if Assigned(OnCancel) then OnCancel(Self);
                          Visible := False;
                       end;
      SYNEDIT_BACK   : if Assigned(OnKeyDelete) then OnKeyDelete(Self);

    end;
  end;
  invalidate;
end;

procedure TSynBaseCompletionProposalForm.KeyPress(var Key: char);
VAR TmpIndex, TmpLevel : Integer;
    TmpStr : String;
begin
  if DisplayType = ctCode then
  begin
    case key of    //
      #32..'z': Begin
                  if Pos(Key, EndOfTokenChr) <> 0 then
                    OnValidate(Self, [], Key);         //GBN 15/11/2001

                  CurrentString:= CurrentString+key;

                  if Assigned(OnKeyPress) then
                    OnKeyPress(self, Key);
                end;
      #8: if Assigned(OnKeyPress) then OnKeyPress(self, Key);
      else if Assigned(OnCancel) then OnCancel(Self);
    end;    // case
  end else if DisplayType = ctHint then
  begin
    if Assigned(OnKeyPress) then OnKeyPress(self, Key);
    if Assigned(OnCancel) then OnCancel(Self);
    Hide;
  end else begin
    case key of
      #27     : begin
                  Hide;
                  if Assigned(OnCancel) then OnCancel(Self);
                end;
      #32..'z': begin
                  if pos(Key, fTriggerChars) > 0 then
                  begin
                    if Assigned(FParameterToken) then
                    begin
                      TmpIndex := CurrentIndex;
                      TmpLevel := CurrentLevel;
                      TmpStr := CurrentString;
                      OnParameterToken(self, CurrentIndex, TmpLevel, TmpIndex, key, TmpStr);
                      CurrentIndex := TmpIndex;
                      CurrentLevel := TmpLevel;
                      CurrentString := TmpStr;
                    end;
                  end;
                end;
      #8: if Assigned(OnKeyPress) then OnKeyPress(self, Key);
    end;
  end;
  Invalidate;
end;

procedure TSynBaseCompletionProposalForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  y := (y - fHeightBuffer - 1) div FFontHeight;
  Position := Scroll.Position + y;
  TCustomSynEdit(CurrentEditor).UpdateCaret;
end;

procedure TSynBaseCompletionProposalForm.Paint;
var
  i: integer;
  function Min(a, b: integer): integer;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;
var
  TmpRect: TRect;
  TmpX, TmpWidth: Integer;
  iCustomDraw: boolean;
  TmpString: String;
begin
//There are now multiple kinds of painting.
//this is for code completion

// draw a rectangle around the window
  Canvas.Brush.Color:= ClBlack;
//  Canvas.FrameRect(Rect(0,0, Width, Height));
  Canvas.Moveto(0, 0);
  Canvas.LineTo(Width-1, 0);
  Canvas.LineTo(Width-1, Height-1);
  Canvas.LineTo(0, Height-1);
  Canvas.LineTo(0, 0);

  if FDisplayKind = ctCode then
  begin

    // update scroll bar
    if ItemList.Count - NbLinesInWindow < 0 then
      Scroll.Max := 0
    else
      Scroll.Max := ItemList.Count - NbLinesInWindow;
//    Scroll.Position := Position;
    Scroll.LargeChange := NbLinesInWindow;

    with bitmap do
    begin
      canvas.pen.color := fClBackGround;
      canvas.brush.color := fClBackGround;
      canvas.Rectangle(0, 0, Width, Height);
      for i := 0 to min(NbLinesInWindow - 1, ItemList.Count - 1) do
      begin
        if i + Scroll.Position = Position then
        begin
          Canvas.Brush.Color := fClSelect;
          Canvas.Pen.Color   := fClSelect;
          Canvas.Rectangle(0, FFontHeight * i, width, FFontHeight * (i + 1));
          Canvas.Pen.Color   := fClSelectText;
          Canvas.Font.Assign(fFont);
          Canvas.Font.Color  := fClSelectText;
        end else begin
          Canvas.Brush.Color := fclBackground;
          Canvas.Pen.Color   := fClBackGround;
          Canvas.Font.Assign(fFont);
        end;

        iCustomDraw := False;
        if Assigned(OnPaintItem) then
          OnPaintItem( Scroll.Position + i, Canvas,
            Rect(0, FFontHeight * i, width, FFontHeight * (i + 1)), iCustomDraw );
        if not iCustomDraw then
        begin
          if FUsePrettyText then
            PrettyTextOut( Canvas, 1, FFontHeight*i, ItemList[Scroll.Position+i], True, FBiggestWord, (i + Scroll.Position = Position)) //D.J
          else Canvas.TextOut(2, FFontHeight * i, ItemList[Scroll.Position + i]);
        end;
      end;
    end;
    canvas.Draw(1, 1 + fHeightBuffer, bitmap);
    if fTitle <> '' then
    begin
      canvas.Pen.Color   := ClBlack;
      canvas.Brush.Color := fClTitleBackground;
      canvas.FillRect(Rect(1, 1, width - 1, fHeightBuffer + 1));
      TmpRect:=Rect(1, 1, width - 1, fHeightBuffer + 1);                        //GBN
      Frame3D(Canvas,TmpRect,clBtnHighlight,clBtnShadow,1);                     //GBN

      Canvas.Font.Assign(fTitleFont);

      InflateRect(TmpRect,-1,-1);                                               //GBN

      TmpRect := Rect(1, 1, width - 1, fHeightBuffer - 1);
      if CenterTitle then
      begin
        TmpWidth := Canvas.TextWidth(Title);
        TmpX := (Width - TmpWidth) div 2;
        if TmpX < 2 then TmpX := 2;  //We still want to be able to read it, even if it does go over the edge
      end else begin
        TmpX := 2;
      end;
      Canvas.TextRect(TmpRect, TmpX, 1, fTitle);
    end;
  end else if (FDisplayKind = ctHint) or (FDisplayKind = ctParams) then
  begin
    with bitmap do
    begin
      canvas.pen.color:= fClBackGround;
      canvas.brush.color:= fClBackGround;
      canvas.Rectangle(0,0,Width,Height);

      Canvas.Brush.Color := fClBackGround;
      Canvas.Font.Assign(fFont);
      //GBN 10/11/2001
      for i := 0 to ItemList.Count - 1 do
      begin
        iCustomDraw := False;
        if Assigned(OnPaintItem) then
           OnPaintItem(i, Canvas, Rect(0, FFontHeight * i, width, FFontHeight * (i + 1)), iCustomDraw );
        if not iCustomDraw then
        begin
          if FDisplayKind = ctParams then
            TmpString := FormatParamList(ItemList[i],CurrentIndex)
          else TmpString := ItemList[i];
          
          PrettyTextOut(Canvas, 3, (FFontHeight * i)+1, TmpString, False, '', False);
        end;
      end;
      //End GBN 10/11/2001
    end;
    canvas.Draw(1, 1, bitmap);
  end;
end;

procedure TSynBaseCompletionProposalForm.ScrollChange(Sender: TObject);
begin
  if Position < Scroll.Position then
    Position := Scroll.Position
  else if Position > Scroll.Position + NbLinesInWindow - 1 then
    Position := Scroll.Position + NbLinesInWindow - 1;
  Invalidate;
end;

procedure TSynBaseCompletionProposalForm.ScrollOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  with TCustomSynEdit(CurrentEditor) do
  begin
    SetFocus;
    //This tricks the caret into showing itself again.
    with TCustomSynEdit(CurrentEditor) do
    begin
      AlwaysShowCaret := False;
      AlwaysShowCaret := True;
      UpdateCaret;
    end;
  end;
end;

procedure TSynBaseCompletionProposalForm.ScrollGetFocus(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSynBaseCompletionProposalForm.MoveLine (cnt: Integer);
begin
  if (cnt > 0) then begin
    if (Position < (ItemList.Count - cnt)) then
      Position := Position + cnt
    else
      Position := ItemList.Count - 1;
  end else begin
    if (Position + cnt) > 0 then
      Position := Position + cnt
    else
      Position := 0;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetCurrentString(const Value: string);
var i: integer;
    cs: string;

  function MatchItem (item: string): Boolean;
  var ci: string;
      TmpStr: String;
  begin
    if UsePrettyText then
    begin
      if pos(#9, item) <> 0 then
        TmpStr := Copy(Item, pos(#9, item) + 1, length(item))
      else TmpStr := Item;
    end else TmpStr := Item;

    ci := intLowerCase (Copy (TmpStr,1,Length (Value)));

    if fAnsi then
      Result := (AnsiCompareText (ci,cs) = 0)
    else
      Result := (CompareText (ci,cs) = 0);
  end;

  procedure RecalcList;
  var idx: Integer;
  begin
    if FMatchText then
    begin
      with fAssignedList do
        if (FItemList.Count > Count) then
          Assign (FItemList);

      ItemList.Clear;

      for idx := 0 to fAssignedList.Count - 1 do begin
        if MatchItem (fAssignedList[idx]) then
          ItemList.AddObject(fAssignedList[idx], TObject(idx));
      end;
    end;
  end;
begin
  FCurrentString := Value;
  if DisplayType = ctHint then exit;
  i:= 0;
  cs := intLowerCase (Value);
  if FMatchText then
    RecalcList;
  while (i <= ItemList.count-1) and not MatchItem (ItemList[i]) do
    Inc (i);
  if (i <= ItemList.Count-1) then Position:= i
    else Position := 0;
end;

procedure TSynBaseCompletionProposalForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  fAssignedList.Assign(Value);
  CurrentString := CurrentString;
end;

procedure TSynBaseCompletionProposalForm.SetInsertList(const Value: TStrings);
begin
  FInsertList.Assign(Value);
end;

procedure TSynBaseCompletionProposalForm.SetNbLinesInWindow(
  const Value: Integer);
begin
  FNbLinesInWindow := Value;
  AdjustScrollBarPosition;
end;

procedure TSynBaseCompletionProposalForm.DoDoubleClick(Sender: TObject);
begin
//we need to do the same as the enter key;
  if DisplayType = ctCode then
    if Assigned(OnValidate) then OnValidate(Self, [],#0);                       //GBN 15/11/2001
end;

procedure TSynBaseCompletionProposalForm.SetPosition(const Value: Integer);
begin
  if ((Value <= 0) and (FPosition = 0)) or
     (FPosition = Value) then exit;

  if Value <= ItemList.Count - 1 then
  begin
    if FPosition <> Value then
    begin
      FPosition := Value;
      if Position < Scroll.Position then
        Scroll.Position := Position
      else if Scroll.Position < Position - NbLinesInWindow + 1 then
        Scroll.Position := Position - NbLinesInWindow + 1;

      if Visible and Assigned(FChangePosition) then
        FChangePosition(TSynBaseCompletionProposal(Owner), Position);

      invalidate;
    end;
  end;
end;

procedure TSynBaseCompletionProposalForm.StringListChange(Sender: TObject);
begin
  if ItemList.Count - NbLinesInWindow < 0 then
    Scroll.Max := 0
  else
    Scroll.Max := ItemList.Count - NbLinesInWindow;
  Scroll.Position := Position;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynBaseCompletionProposalForm.WMMouseWheel(var Msg: TMessage);
var
  nDelta: integer;
  nWheelClicks: integer;
{$IFNDEF SYN_COMPILER_4_UP}
const
  LinesToScroll = 3;
  WHEEL_DELTA = 120;
  WHEEL_PAGESCROLL = MAXDWORD;
{$ENDIF}
begin
  if csDesigning in ComponentState then exit;

{$IFDEF SYN_COMPILER_4_UP}
  if GetKeyState(VK_CONTROL) >= 0 then nDelta := Mouse.WheelScrollLines
{$ELSE}
  if GetKeyState(VK_CONTROL) >= 0 then nDelta := LinesToScroll
{$ENDIF}
    else nDelta := FNbLinesInWindow;

  Inc(fMouseWheelAccumulator, SmallInt(Msg.wParamHi));
  nWheelClicks := fMouseWheelAccumulator div WHEEL_DELTA;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DELTA;
  if (nDelta = integer(WHEEL_PAGESCROLL)) or (nDelta > FNbLinesInWindow) then
    nDelta := FNbLinesInWindow;

  Position := Position - (nDelta * nWheelClicks);
  TCustomSynEdit(CurrentEditor).UpdateCaret;
end;
{$ENDIF}

function TSynBaseCompletionProposalForm.intLowerCase (s: string): string;
begin
  if fCase then Result := s
    else Result := LowerCase (s);
end;

function GetMDIParent (const Form: TSynForm): TSynForm;
{ Returns the parent of the specified MDI child form. But, if Form isn't a
  MDI child, it simply returns Form. }
var
  I, J: Integer;
begin
  Result := Form;
  if Form = nil then Exit;
  if {$IFDEF SYN_COMPILER_3_UP} (Form is TForm) and {$ENDIF}
     (TForm(Form).FormStyle = fsMDIChild) then
    for I := 0 to Screen.FormCount-1 do
      with Screen.Forms[I] do begin
        if FormStyle <> fsMDIForm then Continue;
        for J := 0 to MDIChildCount-1 do
          if MDIChildren[J] = Form then begin
            Result := Screen.Forms[I];
            Exit;
          end;
      end;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynBaseCompletionProposalForm.WMActivate(var Message: TWMActivate);
var
  ParentForm: TSynForm;
begin
  if csDesigning in ComponentState then begin
    inherited;
    Exit;
  end;
     {Owner of the component that created me}
  if Owner.Owner is TSynForm then
    ParentForm := GetMDIParent(TSynForm(Owner.Owner))
  else ParentForm := nil;

  if Assigned(ParentForm) and ParentForm.HandleAllocated then
    SendMessage (ParentForm.Handle, WM_NCACTIVATE, Ord(Message.Active <> WA_INACTIVE), 0);
end;
{$ENDIF}

procedure TSynBaseCompletionProposalForm.DoFormHide(Sender: TObject);
begin
  if CurrentEditor <> nil then
  begin
    TCustomSynEdit(CurrentEditor).AlwaysShowCaret := OldShowCaret;
    TCustomSynEdit(CurrentEditor).UpdateCaret;
  end;
end;

procedure TSynBaseCompletionProposalForm.DoFormShow(Sender: TObject);
begin
  if CurrentEditor <> nil then
  begin
    OldShowCaret := TCustomSynEdit(CurrentEditor).AlwaysShowCaret;

    TCustomSynEdit(CurrentEditor).AlwaysShowCaret := True;
    TCustomSynEdit(CurrentEditor).UpdateCaret;
  end;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynBaseCompletionProposalForm.WMEraseBackgrnd(
  var Message: TMessage);
begin
  Message.Result:=1;
end;

//GBN 24/02/2002
procedure TSynBaseCompletionProposalForm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

{$ENDIF}

procedure TSynBaseCompletionProposalForm.AdjustScrollBarPosition;
begin
  if fTitle <> '' then
    fHeightBuffer := fTitleFontHeight + 2
  else fHeightBuffer := 0;

  Height := fFontHeight * NbLinesInWindow + 2 + fHeightBuffer;
  if assigned(scroll) then
  begin
    Scroll.Top    := 1 + fHeightBuffer;
    Scroll.Height := Height - 2 - (fHeightBuffer);
    Scroll.Left   := ClientWidth - Scroll.Width - 1;
  end;

  if assigned(bitmap) then
  begin
    Bitmap.Width  := Scroll.Left - 1;
    Bitmap.Height := Height - 2 - (fHeightBuffer);
  end;

end;

procedure TSynBaseCompletionProposalForm.SetTitle(const Value: string);
begin
  fTitle := Value;
  AdjustScrollBarPosition;
end;

procedure TSynBaseCompletionProposalForm.SetFont(const Value: TFont);
begin
  fFont.Assign(Value);
  Canvas.Font.Assign(fFont);
  FFontHeight := Canvas.TextHeight('CompletionProposal');
  AdjustScrollBarPosition;
end;

procedure TSynBaseCompletionProposalForm.SetTitleFont(const Value: TFont);
begin
  fTitleFont.Assign(Value);
  Canvas.Font.Assign(fTitleFont);
  FTitleFontHeight := Canvas.TextHeight('CompletionProposal');
  Canvas.Font.Assign(fFont);
  AdjustScrollBarPosition;
end;

procedure TSynBaseCompletionProposalForm.TitleFontChange(Sender: TObject);
begin
  Canvas.Font.Assign(fTitleFont);
  FTitleFontHeight := Canvas.TextHeight('CompletionProposal');
  Canvas.Font.Assign(fFont);
  AdjustScrollBarPosition;
end;

procedure TSynBaseCompletionProposalForm.FontChange(Sender: TObject);
begin
  Canvas.Font.Assign(fFont);
  FFontHeight := Canvas.TextHeight('CompletionProposal');
  AdjustScrollBarPosition;
end;

{ TSynBaseCompletionProposal }

constructor TSynBaseCompletionProposal.Create(Aowner: TComponent);
begin
  FWidth := 262;
  FBiggestWord := 'CONSTRUCTOR';
  inherited Create(AOwner);
  Form := TSynBaseCompletionProposalForm.Create(Self);
  Form.Width := FWidth;
  EndOfTokenChr := '()[]. ';
  FDotOffset := 0;
  FOldPos := 0;
  FOldLeft := 0;
  FOldStr := '';
end;

destructor TSynBaseCompletionProposal.Destroy;
begin
  FreeAndNil(Fform);
  inherited Destroy;
end;

procedure TSynBaseCompletionProposal.Execute(s: string; x, y: integer);
begin
  ExecuteEx(s, x, y, ctCode);
end;

procedure TSynBaseCompletionProposal.ExecuteEx(s: string; x, y: integer; Kind : SynCompletionType);

  //GBN 08/01/2002
  procedure CheckFormPosition(Form: TSynBaseCompletionProposalForm);
  var TaskbarRect: TRect;
  begin
    {$IFNDEF SYN_CLX}  //js 07-04-2002 no Systemsparametersinfo in CLX
    SystemParametersInfo(SPI_GETWORKAREA,0,@TaskbarRect,0);
    {$ENDIF}
    if (Form.Left+Form.Width>(TaskBarRect.Right-TaskbarRect.Left)) then
      begin
      Form.Left:=Form.Left-Form.Width;
      if (Form.Left<0) then Form.Left:=0;
      end;
    if (Form.Top+Form.Height>(TaskBarRect.Bottom-TaskbarRect.Top)) then Form.Top:=Form.Top-Form.Height-TCustomSynEdit(Form.CurrentEditor).LineHeight;
  end;

VAR CanExecute : Boolean;
    TmpOffset  : Integer;
    i: Integer;
begin
  DisplayType := Kind;

  CanExecute := True;
  if assigned(OnExecute) then
    OnExecute(Kind, Self, s, x, y, CanExecute);

  if not(CanExecute) then
  begin
    if Form.Visible and (Kind = ctParams) then Form.Visible := False;
    FOldStr := '';
    exit;
  end;

  if Kind = ctParams then
  begin
    Form.FormStyle := fsStayOnTop;
  end else begin
    Form.FormStyle := fsNormal;
    FOldPos := -1;
    FOldLeft := -1;
    FOldStr := '';
  end;

  if form.CurrentEditor <> nil then
  begin
    TmpOffset := TCustomSynEdit(form.CurrentEditor).Canvas.TextWidth(copy(s, 1, DotOffset));

    if DotOffset > 1 then TmpOffset := TmpOffset + (3 * (DotOffset - 1))
  end else TmpOffset := 0;

  if Kind = ctCode then
  begin
    CurrentString:= s;
    with form do
    begin
      top   := y;
      left  := x - TmpOffset;
      width := FWidth;                                                        
      Color := CLBackground;
      AdjustScrollBarPosition;
      Scroll.Visible := True;
      CheckFormPosition(Form); //GBN 08/01/2002
      Show;
    end;
  //GBN 10/11/2001
  end else if Kind=ctParams then begin
    if (ItemList.Count=0) then exit;
    form.width := 0;
    form.height := 0;
    form.Canvas.Font.Style := [fsBold];
    for i:=0 to ItemList.Count-1 do
    begin
      if (form.Canvas.TextWidth(ItemList[i])>form.width) then
        form.width:=form.Canvas.TextWidth(ItemList[i]);
    end;
    form.height:=(form.Canvas.TextHeight('Wg')+4)*ItemList.Count;
    form.top   := y;
    form.left  := x;
    form.bitmap.Width := form.width - 2;
    form.bitmap.height:= form.Height - 2;

    form.Color := ClBackground;
    form.Scroll.Visible := False;
    try
      //form.Show;
      CheckFormPosition(Form); //GBN 08/01/2002
      {$IFNDEF SYN_CLX}  //js 07-04-2002 no showwindow in CLX - killed without substitute
      ShowWindow(form.handle,SW_SHOWNOACTIVATE);
      {$ENDIF}
      form.visible:=true;
    except
      on exception do ;
    end;
    //TCustomSynEdit(form.CurrentEditor).SetFocus;  //Removed GBN 13/11/2001
  //end GBN 10/11/2001
  end else begin

    with form do
    begin
      form.Invalidate;
      Canvas.Font.Style := [fsBold];
      CurrentString := ItemList.Text;     //GBN 10/11/2001, not sure if this is correct behavior for ctHint

      if (FOldStr <> CurrentString) or (FOldStr = '') then
      begin
        form.height:= 0;
        form.width := 0;
        form.top   := y;
        form.left  := x - TmpOffset;
        FOldStr := CurrentString;
        FOldPos := -1;
      end;

      width := Canvas.TextWidth(CurrentString);
      Canvas.Font.Style := [];
      TmpOffset := Canvas.TextWidth(copy(CurrentString,1, pos(#9,CurrentString) - 1));

      if Top <> y then Top := y;

      if TmpOffset <> FOldPos then
      begin
        left := Left - TmpOffset;
        FOldPos := TmpOffset;
        FOldLeft := Left;
      end else Left := FOldLeft;
      Height := FFontHeight + 4;

      if (Width - 2 < 1) or
         (Height - 2 < 1) then
      begin
        //There is nothing to display
        exit;
      end;
      bitmap.Width := width - 2;

      bitmap.height:= Height - 2;

      Color := ClBackground;
      Scroll.Visible := False;

      CheckFormPosition(Form);
      try
        form.Show;
      except
        on exception do ;
      end;

      if Kind = ctParams then
        TCustomSynEdit(form.CurrentEditor).SetFocus;

    end;
  end;
end;

function TSynBaseCompletionProposal.GetCurrentString: string;
begin
  result := Form.CurrentString;
end;

function TSynBaseCompletionProposal.GetItemList: TStrings;
begin
  result := Form.ItemList;
end;

function TSynBaseCompletionProposal.GetInsertList: TStrings;
begin
  result := Form.InsertList;
end;

function TSynBaseCompletionProposal.GetNbLinesInWindow: Integer;
begin
  Result := Form.NbLinesInWindow;
end;

function TSynBaseCompletionProposal.GetOnCancel: TNotifyEvent;
begin
  Result := Form.OnCancel;
end;

function TSynBaseCompletionProposal.GetOnKeyPress: TKeyPressEvent;
begin
  Result := Form.OnKeyPress;
end;

function TSynBaseCompletionProposal.GetOnPaintItem: TSynBaseCompletionProposalPaintItem;
begin
  Result := Form.OnPaintItem;
end;

function TSynBaseCompletionProposal.GetOnValidate: TValidateEvent;
begin
  Result := Form.OnValidate;
end;

function TSynBaseCompletionProposal.GetPosition: Integer;
begin
  Result := Form.Position;
end;

procedure TSynBaseCompletionProposal.SetCurrentString(const Value: string);
begin
  form.CurrentString := Value;
end;

procedure TSynBaseCompletionProposal.SetItemList(const Value: TStrings);
begin
  form.ItemList := Value;
end;

procedure TSynBaseCompletionProposal.SetInsertList(const Value: TStrings);
begin
  form.InsertList := Value;
end;

procedure TSynBaseCompletionProposal.SetNbLinesInWindow(const Value: Integer);
begin
  form.NbLinesInWindow := Value;
end;

procedure TSynBaseCompletionProposal.SetOnCancel(const Value: TNotifyEvent);
begin
  form.OnCancel := Value;
end;

procedure TSynBaseCompletionProposal.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  form.OnKeyPress := Value;
end;

procedure TSynBaseCompletionProposal.SetOnPaintItem(const Value:
  TSynBaseCompletionProposalPaintItem);
begin
  form.OnPaintItem := Value;
end;

procedure TSynBaseCompletionProposal.SetPosition(const Value: Integer);
begin
  form.Position := Value;
end;

procedure TSynBaseCompletionProposal.SetOnValidate(const Value: TValidateEvent);
begin
  form.OnValidate := Value;
end;

function TSynBaseCompletionProposal.GetClSelect: TColor;
begin
  Result := Form.ClSelect;
end;

procedure TSynBaseCompletionProposal.SetClSelect(const Value: TColor);
begin
  Form.ClSelect := Value;
end;

function TSynBaseCompletionProposal.GetOnKeyDelete: TNotifyEvent;
begin
  result := Form.OnKeyDelete;
end;

procedure TSynBaseCompletionProposal.SetOnKeyDelete(const Value: TNotifyEvent);
begin
  form.OnKeyDelete := Value;
end;

procedure TSynBaseCompletionProposal.SetWidth(Value: Integer);
begin
  FWidth := Value;
  Form.Width := FWidth;
  Form.SetNbLinesInWindow(Form.FNbLinesInWindow);
end;

procedure TSynBaseCompletionProposal.Activate;
begin
  if Assigned(Form) then Form.Activate;
end;

procedure TSynBaseCompletionProposal.Deactivate;
begin
  if Assigned(Form) then Form.Deactivate;
end;

function TSynBaseCompletionProposal.GetClBack: TColor;
begin
  Result := form.ClBackground;
end;

procedure TSynBaseCompletionProposal.SetClBack(const Value: TColor);
begin
  form.ClBackground := Value
end;

function TSynBaseCompletionProposal.GetClSelectedText: TColor;
begin
  Result := form.ClSelectedText;
end;

procedure TSynBaseCompletionProposal.SetClSelectedText(const Value: TColor);
begin
  form.ClSelectedText := Value;
end;

Procedure PrettyTextOut(c: TCanvas; x, y: integer; s: String; DoAlign: Boolean;
                        BiggestWord: String; Highlighted: Boolean);             //D.J
var
  i: integer;
  b: TBrush;
  f: TFont;
  InBold : Boolean;
  DidAlign : Boolean;
Begin
  b:= TBrush.Create;
  b.Assign(c.Brush);
  f:= TFont.Create;
  f.Assign(c.Font);
  InBold := False;
  DidAlign := False;
  try
    i:= 1;
    while i<=Length(s) do
      case s[i] of
        #1: begin //D.J (till #3)
              if not Highlighted then
                C.Font.Color:= (Ord(s[i+3]) shl 8 + Ord(s[i+2])) shl 8 + Ord(s[i+1]);
              inc(i, 4);
            end;
        #2: Begin
              if not Highlighted then
                C.Font.Color:= (Ord(s[i+3]) shl 8 + Ord(s[i+2])) shl 8 + Ord(s[i+1]);
              inc(i, 4);
            end;
        #3: Begin
              case s[i+1] of
                'B': c.Font.Style:= c.Font.Style+[fsBold];
                'b': c.Font.Style:= c.Font.Style-[fsBold];
                'U': c.Font.Style:= c.Font.Style+[fsUnderline];
                'u': c.Font.Style:= c.Font.Style-[fsUnderline];
                'I': c.Font.Style:= c.Font.Style+[fsItalic];
                'i': c.Font.Style:= c.Font.Style-[fsItalic];
              end;
              inc(i, 2);
            end;
        #9: Begin
              InBold := not(InBold);

              if InBold then
                c.Font.Style:= c.Font.Style+[fsBold]
              else begin
                c.Font.Style:= c.Font.Style-[fsBold];
                x := x + 1;  //spacing issue
              end;

              if not(DidAlign) and DoAlign then
              begin
                x := 1 + c.TextWidth(BiggestWord);
                DidAlign := True;
              end;
              inc(i);
            end;
        else
          if (not(DidAlign) and (x < c.TextWidth(BiggestWord)) and (pos(#9, s) <> 0) and DoAlign) or
             ((pos(#9, s) <> 0) and not(DoAlign)) or
             (pos(#9, s) = 0) or
             (DidAlign) then
          begin
            C.TextOut(x, y, s[i]);
            x:= x+c.TextWidth(s[i])
          end;

        {$IFNDEF SYN_CLX}
          if Win32Platform <> VER_PLATFORM_WIN32_NT then
            if InBold then x := x - 1;
        {$ENDIF}

          inc(i);
      end;
  except
  end;
  c.Font.Assign(f);
  f.Free;
  c.Brush.Assign(b);
  b.Free;
end;

procedure TSynBaseCompletionProposal.AddItem(ADisplayText, AInsertText: String);
begin
  GetInsertList.Add(AInsertText);
  GetItemList.Add(ADisplayText);
end;

procedure TSynBaseCompletionProposal.AddItemAt(Where : Integer; ADisplayText, AInsertText: String);
begin
  try
    GetInsertList.Insert(Where, AInsertText);
    GetItemList.Insert(Where, ADisplayText);
  except
    on exception do Exception.Create('Cannot insert item at position ' + IntToStr(Where) + '.');
  end;
end;


procedure TSynBaseCompletionProposal.ClearList;
begin
  GetInsertList.Clear;
  GetItemList.Clear;
end;

function TSynBaseCompletionProposal.DisplayItem(AIndex : Integer): String;
begin
  Result := GetItemList[AIndex];
end;

function TSynBaseCompletionProposal.InsertItem(AIndex : Integer): String;
begin
  Result := GetInsertList[AIndex];
end;

function TSynBaseCompletionProposal.GetDisplayKind: SynCompletionType;
begin
  result := form.DisplayType;
end;

procedure TSynBaseCompletionProposal.SetDisplayKind(const Value: SynCompletionType);
begin
  form.DisplayType := Value;
end;

function TSynBaseCompletionProposal.GetParameterToken: TCompletionParameter;
begin
  Result := Form.OnParameterToken;
end;

procedure TSynBaseCompletionProposal.SetParameterToken(
  const Value: TCompletionParameter);
begin
  Form.OnParameterToken := Value;
end;

function TSynBaseCompletionProposal.GetDefaultKind: SynCompletionType;
begin
  result := Form.DefaultType;
end;

procedure TSynBaseCompletionProposal.SetDefaultKind(const Value: SynCompletionType);
begin
  Form.DefaultType := Value;
  Form.DisplayType := Value;
end;

procedure TSynBaseCompletionProposal.SetUseBiggestWord(const Value: String);
begin
  FBiggestWord := Value;
  Form.BiggestWord := Value;
end;

function TSynBaseCompletionProposal.IsEndToken(AChar: Char): Boolean;
var i : Integer;
begin
  Result := False;
  i := 1;
  while i < length(EndOfTokenChr) do
    if AChar = EndOfTokenChr[i] then
    begin
      Result := True;
      break;
    end else inc(i);
end;

procedure TSynBaseCompletionProposal.SetEndOfTokenChar(
  const Value: string);
begin
  if EndOfTokenChr <> Value then
  begin
    Form.EndOfTokenChr := Value;
  end;
end;

function TSynBaseCompletionProposal.GetClTitleBackground: TColor;
begin
  Result := Form.ClTitleBackground;
end;

procedure TSynBaseCompletionProposal.SetClTitleBackground(
  const Value: TColor);
begin
    Form.ClTitleBackground := Value;
end;

function TSynBaseCompletionProposal.GetTitle: string;
begin
  Result := Form.Title;
end;

procedure TSynBaseCompletionProposal.SetTitle(const Value: string);
begin
  Form.Title := Value;
end;

function TSynBaseCompletionProposal.GetFont: TFont;
begin
  Result := Form.Font;
end;

function TSynBaseCompletionProposal.GetTitleFont: TFont;
begin
  Result := Form.TitleFont;
end;

procedure TSynBaseCompletionProposal.SetFont(const Value: TFont);
begin
  Form.Font := Value;
end;

procedure TSynBaseCompletionProposal.SetTitleFont(const Value: TFont);
begin
  Form.TitleFont := Value;
end;

function TSynBaseCompletionProposal.GetEndOfTokenChar: string;
begin
  Result := Form.EndOfTokenChr;
end;

function TSynBaseCompletionProposal.GetOptions: TSynCompletionOptions;
begin
  Result := fOptions;
end;

procedure TSynBaseCompletionProposal.SetOptions(
  const Value: TSynCompletionOptions);
begin
  if fOptions <> Value then
  begin
    fOptions := Value;
    Form.CenterTitle   := scoTitleIsCentered in Value;
    Form.AnsiStrings   := scoAnsiStrings in Value;
    Form.CaseSensitive := scoCaseSensitive in Value;
    Form.UsePrettyText := scoUsePrettyText in Value;
    Form.UseInsertList := scoUseInsertList in Value;
    Form.MatchText     := scoLimitToMatchedText in Value;
{  form.ShrinkList := Value;
  fShrink := Value;
  with FItemList do
    if (Count < fAssignedList.Count) then
      Assign(fAssignedList);
}
  end;
end;

function TSynBaseCompletionProposal.GetTriggerChars: String;
begin
  Result := Form.TriggerChars;
end;

procedure TSynBaseCompletionProposal.SetTriggerChars(const Value: String);
begin
  Form.TriggerChars := Value;
end;

procedure TSynBaseCompletionProposal.EditorCancelMode(Sender: TObject);
begin
  //Do nothing here, used in TSynCompletionProposal
end;

procedure TSynBaseCompletionProposal.HookedEditorCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var AChar: char; Data,
  HandlerData: pointer);
begin

end;

function TSynBaseCompletionProposal.GetOnChange: TCompletionChange;
begin
  Result := Form.FChangePosition;
end;

procedure TSynBaseCompletionProposal.SetOnChange(
  const Value: TCompletionChange);
begin
  Form.FChangePosition := Value;
end;

procedure TSynBaseCompletionProposal.loaded;
begin
  inherited;
  Form.AssignedList.Assign(ItemList);
end;

procedure TSynBaseCompletionProposal.ResetAssignedList;
begin
  Form.AssignedList.Assign(ItemList);
end;

{ TSynCompletionProposal }

procedure TSynCompletionProposal.HandleOnKeyDelete(Sender: TObject);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then begin
    (F.CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0,
      nil);
  end;
end;

procedure TSynCompletionProposal.HandleOnCancel(Sender: TObject);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then
  begin
    if ((F.CurrentEditor as TCustomSynEdit).Owner is TWinControl) and
       (TWinControl((F.CurrentEditor as TCustomSynEdit).Owner).Visible) then
    begin
      TWinControl((F.CurrentEditor as TCustomSynEdit).Owner).SetFocus;
    end;

    if Assigned(FTimer) then
      FTimer.Enabled := False;

    (F.CurrentEditor as TCustomSynEdit).SetFocus;

    F.Hide;

    if Assigned(OnCancelled) then OnCancelled(Self); //GBN 13/11/2001
  end;
end;

procedure TSynCompletionProposal.HandleOnValidate(Sender: TObject; Shift: TShiftState; EndToken: Char);
var
  F: TSynBaseCompletionProposalForm;
  Value: string;
  //TmpChr : Char; GBN 15/01/2002
  //Pos: TPoint; GBN 25/02/2002
  Index: Integer; //GBN 15/11/2001
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then
    with F.CurrentEditor as TCustomSynEdit do begin

      //GBN 2002-03-07 START
      //Treat entire completion as a single undo operation
      Editor.BeginUpdate;
      Editor.BeginUndoBlock;
      try
      //GBN 2002-03-07 END

        if not SelAvail then
        begin
          BlockBegin := Point(CaretX - length(CurrentString), CaretY);
          BlockEnd := Point(CaretX, CaretY);
        end;

        //when there is a dot at the end, then GetSelstart = the first letter of
        //the thing we will replace
        //when there is *NO* dot at the end, getselstart refers to the dot
        if length(CurrentString) <> 0 then
        begin

          if IsEndToken(Text[SelStart]) then
          begin
            BlockBegin:= Point(CaretX - length(CurrentString) + 1, CaretY);
            BlockEnd:= Point(CaretX + 1, CaretY);
          end;
        end;

        if scoUseInsertList in fOptions then
        begin
          if scoLimitToMatchedText in fOptions then
          begin
            if (ItemList.Count > Position) then
              //GBN 15/01/2002 - Added check to make sure item is only used when no EndChar
              if (InsertList.Count > Integer(ItemList.Objects[position])) and
                 ((scoEndCharCompletion in fOptions) or (EndToken=#0)) then
              begin
                Value := InsertList[Integer(ItemList.Objects[position])]
              end else Value := SelText
            else Value := SelText;
          end else begin
            //GBN 15/01/2002 - Added check to make sure item is only used when no EndChar
            if (InsertList.Count > Position) and
               ((scoEndCharCompletion in fOptions) or (EndToken=#0)) then
              Value := InsertList[position]
            else Value := SelText;
          end;
        end else begin
          //GBN 15/01/2002 - Added check to make sure item is only used when no EndChar
          if (ItemList.Count > position) and
             ((scoEndCharCompletion in fOptions) or (EndToken=#0)) then
            Value := ItemList[position]
          else Value := SelText;
        end;
        Index:=Position; //GBN 15/11/2001, need to assign position to temp var since it changes later

        //GBN 15/01/2002 - Cleaned this code up a bit
        if Assigned(FOnCodeCompletion) then FOnCodeCompletion(Value, Shift,Index,EndToken); //GBN 15/11/2001
        if SelText <> Value then
          SelText := Value;

        //GBN 15/01/2002 - Remarked this out, causes problems and not needed
        //                 since EndToken is added automatically,
        //                 this just seemed to insert an extra space
        //CurrentString := SelText;
        //if length(Text) <> SelEnd + length(CurrentString) + 1 then
        //  if IsEndToken(Text[SelEnd + length(CurrentString)]) then
        //  begin
        //    TmpChr := Text[SelEnd + length(CurrentString)];
        //    SelEnd := (length(CurrentString) + SelEnd);
        //    SelStart := (length(CurrentString) + SelEnd + 1);
        //    SelText := TmpChr;
        //  end;
        //End commented out block

        with Editor do begin
          //GBN 25/02/2002
          //This replaces the previous way of cancelling the completion by
          //sending a WM_MOUSEDOWN message. The problem with the mouse down is
          //that the editor would bounce back to the left margin, very irritating
          InternalCancelCompletion;
          SetFocus;
          EnsureCursorPosVisible; //GBN 25/02/2002
          BlockBegin := CaretXY;
          BlockEnd := CaretXY;
        end;
        //GBN 15/11/2001
        if Assigned(FOnAfterCodeCompletion) then
          FOnAfterCodeCompletion(Value, Shift, Index,EndToken);
      //GBN 2002-03-07 START
      finally
      Editor.EndUndoBlock;
      Editor.EndUpdate;
      end;
      //GBN 2002-03-07 END
    end;
end;

procedure TSynCompletionProposal.HandleOnKeyPress(Sender: TObject; var Key: Char);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then begin
    with F.CurrentEditor as TCustomSynEdit do
      CommandProcessor(ecChar, Key, nil);
    //GBN 22/11/2001
    //Daisy chain completions
    Application.ProcessMessages;
    if (System.Pos(Key, TriggerChars)>0) and (not F.Visible) then
      begin
      //GBN 18/02/2002
      if (Sender is TCustomSynEdit) then DoExecute(Sender as TCustomSynEdit)
      else if (Form.CurrentEditor<>nil) then DoExecute(Form.CurrentEditor as TCustomSynEdit);
      end;
  end;
end;

procedure TSynCompletionProposal.SetEditor(const Value: TCustomSynEdit);
begin
  RemoveCurrentEditor;
  if Assigned(Value) then begin
    AddEditor(Value);
  end;
end;

procedure TSynCompletionProposal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent is TCustomSynEdit) then
    RemoveEditor(AComponent as TCustomSynEdit);
  inherited Notification(AComponent, Operation);
end;

constructor TSynCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form.OnKeyPress := HandleOnKeyPress;
  Form.OnKeyDelete := HandleOnKeyDelete;
  Form.OnValidate := HandleOnValidate;
  Form.OnCancel := HandleOnCancel;
  Form.OnDblClick := HandleDblClick;
  EndOfTokenChr := '()[]. ';
  TriggerChars := '.';
  fTimerInterval:= 1000;
  fKeyDownProc := TKeyDownProc.Create (EditorKeyDown);
  fKeyPressProc := TKeyPressProc.Create (EditorKeyPress);
  fNoNextKey := false;
{$IFDEF SYN_CLX}
  fShortCut := QMenus.ShortCut(Ord(' '), [ssCtrl]);
{$ELSE}
  fShortCut := Menus.ShortCut(Ord(' '), [ssCtrl]);
{$ENDIF}
  Options := DefaultProposalOptions;
  fEditors := TList.Create;
  fCurEditor := -1;
end;

procedure TSynCompletionProposal.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

procedure TSynCompletionProposal.EditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  ShortCutKey   : Word;
  ShortCutShift : TShiftState;
begin
  ShortCutToKey (fShortCut,ShortCutKey,ShortCutShift);
  with sender as TCustomSynEdit do begin
    if not ReadOnly and (Shift = ShortCutShift) and (Key = ShortCutKey) then begin
      Form.CurrentEditor:= Sender as TCustomSynEdit;
      DoExecute(Sender as TCustomSynEdit);
      Key := 0;
    end;
  end;

end;

function TSynCompletionProposal.GetPreviousToken(FEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  Result := '';
  if FEditor <> nil then begin
    s := FEditor.LineText;
    i := FEditor.CaretX - 1;
    if i <= length(s) then begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], EndOfTokenChr) = 0) do
        dec(i);
      result := copy(s, i + 1, FEditor.CaretX - i - 1);
    end;
  end;
end;

procedure TSynCompletionProposal.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if Form.Visible and fNoNextKey then begin
    fNoNextKey := false;
    Key := #0;
  end else if Assigned(FTimer) then
  begin
    if (pos(key, TriggerChars) <> 0) then
      begin
      Form.CurrentEditor:=Sender as TCustomSynEdit;  //GBN 18/02/2002
      FTimer.Enabled := True;
      end
    else FTimer.Enabled := False or Form.Visible;
  end;
end;

procedure TSynCompletionProposal.HandleDblClick(Sender: TObject);
begin
  HandleOnValidate(Sender, [], #0);
end;

destructor TSynCompletionProposal.Destroy;
begin
  // necessary to get Notification called before fEditors is freed
  Form.Free;
  Form := nil;

  while fEditors.Count <> 0 do
    RemoveEditor(TCustomSynEdit(fEditors.last));
  fEditors.Free;
  fKeyDownProc.Free;
  fKeyPressProc.Free;

  inherited;
end;

procedure TSynCompletionProposal.TimerExecute(Sender: TObject);
begin
  if not Assigned(fTimer) then Exit;
  fTimer.Enabled := false; //GBN 13/11/2001  DefaultType <> ctCode) and Application.Active;
  if Application.Active then
    DoExecute(TCustomSynEdit(Form.CurrentEditor))
  else if Form.Visible then Form.Hide;
end;

function TSynCompletionProposal.GetTimerInterval: Integer;
begin
  Result := FTimerInterval;
end;

procedure TSynCompletionProposal.SetTimerInterval(const Value: Integer);
begin
  fTimerInterval := Value;
  if Assigned(FTimer) then
    FTimer.Interval := Value;
end;

procedure TSynCompletionProposal.SetOptions(const Value: TSynCompletionOptions);
begin
  inherited;

  if scoUseBuiltInTimer in Value then
  begin
    if not(Assigned(FTimer)) then
    begin
      FTimer := TTimer.Create(Self);
      FTimer.Enabled := False;
      FTimer.Interval := FTimerInterval;
      FTimer.OnTimer := TimerExecute;
    end;
  end else begin
    if Assigned(FTimer) then
    begin
      FreeAndNil(FTimer);
    end;
  end;

end;

procedure TSynCompletionProposal.ExecuteEx(s: string; x, y: integer;
  Kind: SynCompletionType);
begin
  inherited;
  if FTimer <> nil then
    FTimer.Enabled := (FTimer.Enabled and (Kind = ctCode));// or ((Kind = ctParams) and (Form.Visible));
end;

function TSynCompletionProposal.GetFEditor: TCustomSynEdit;
begin
  if (EditorsCount > 0) and (fCurEditor <> -1) then
    result := Editors[fCurEditor]
  else
    result := nil;
end;

procedure TSynCompletionProposal.AddEditor(AEditor: TCustomSynEdit);
var
  i : integer;
begin
  if not Assigned(AEditor) then Exit;
  i := fEditors.IndexOf(AEditor);
  if i = -1 then begin
    i := fEditors.Add(AEditor);
    with AEditor do
    begin
      AddKeyDownHandler(fKeyDownProc);
      AddKeyPressHandler(fKeyPressProc);
    end;
    AEditor.FreeNotification(self);
    TProtectedAccessEditor(AEditor).OnCancelMode:=EditorCancelMode;
//  GBN 02/20/2002 Removed csLoading from line below, hook never established if component setup is done
//  at design time
    if ComponentState * [csDesigning] = [] then
      AEditor.RegisterCommandHandler(HookedEditorCommand,Self);
  end;
  fCurEditor := i;
end;

function TSynCompletionProposal.EditorsCount: integer;
begin
  result := fEditors.count;
end;

function TSynCompletionProposal.GetEditor(i: integer): TCustomSynEdit;
begin
  if (i < 0) or (i >= EditorsCount) then
    result := nil
  else
    result := TCustomSynEdit(fEditors[i]);
end;

function TSynCompletionProposal.RemoveEditor(AEditor: TCustomSynEdit): boolean;
var
  i: integer;
begin
  Result := False;
  if not Assigned(AEditor) then Exit;
  i := fEditors.Remove(AEditor);
  result := i <> -1;
  if result then begin
    AEditor.RemoveKeyDownHandler(fKeyDownProc);
    AEditor.RemoveKeyPressHandler(fKeyPressProc);
    TProtectedAccessEditor(AEditor).OnCancelMode:=nil;
    if ComponentState * [csDesigning, csLoading] = [] then
      AEditor.UnregisterCommandHandler(HookedEditorCommand);
    if fCurEditor = i then
      fCurEditor := -1
    else if fCurEditor > i then
      fCurEditor := EditorsCount - 1;
    {$IFDEF SYN_COMPILER_5_UP}
    RemoveFreeNotification( AEditor );
    {$ENDIF}
  end;
end;

procedure TSynCompletionProposal.RemoveCurrentEditor;
begin
  if (EditorsCount > 0) and (fCurEditor <> -1) then
  begin
    RemoveEditor(Editors[fCurEditor]);
    fCurEditor := -1;
  end;
end;

procedure TSynCompletionProposal.DoExecute(AEditor: TCustomSynEdit);
var
  p : TPoint;
  i : integer;
begin
  i := fEditors.indexOf(AEditor);
  if i <> -1 then
    with AEditor do begin
      if not ReadOnly then
      begin
        p := ClientToScreen(Point(CaretXPix, CaretYPix + LineHeight));
        Form.CurrentEditor := AEditor;
        ExecuteEx(GetPreviousToken(AEditor), p.x, p.y, DefaultType);
        fNoNextKey := DefaultType = ctCode;
      end;
    end;
end;

//25/02/2002 GBN
procedure TSynCompletionProposal.InternalCancelCompletion;
begin
  if Assigned(FTimer) then FTimer.Enabled := False;
  if (Form.Visible) and (DisplayType <> ctHint) then
  begin
    Deactivate;
    Form.Hide;
  end;
end;

procedure TSynCompletionProposal.CancelCompletion;
begin
  InternalCancelCompletion; //25/02/2002 GBN
  if Assigned(OnCancelled) then OnCancelled(Self); //GBN 13/11/2001
end;

//GBN 13/11/2001
procedure TSynCompletionProposal.EditorCancelMode(Sender: TObject);
begin
  if (DisplayType=ctParams) then CancelCompletion;
end;

//GBN 13/11/2001
procedure TSynCompletionProposal.HookedEditorCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var AChar: char; Data,
  HandlerData: pointer);
begin
  inherited;
  if (DisplayType=ctParams) then
    begin
    if (Command=ecLostFocus) or ((Command=ecChar) and (AChar=#27)) then CancelCompletion
    else if (Form.Visible) then DoExecute(Sender as TCustomSynEdit);
    end;
end;

procedure TSynCompletionProposal.ActivateCompletion;
begin
  DoExecute(Editor);
end;

{ TSynAutoComplete }

constructor TSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited;

  FEndOfTokenChr := '()[]. ';
  fAutoCompleteList := TStringList.Create;
  fKeyDownProc := TKeyDownProc.Create (EditorKeyDown);
  fKeyPressProc := TKeyPressProc.Create (EditorKeyPress);
  fNoNextKey := false;
{$IFDEF SYN_CLX}
  fShortCut := QMenus.ShortCut(Ord(' '), [ssShift]);
{$ELSE}
  fShortCut := Menus.ShortCut(Ord(' '), [ssShift]);
{$ENDIF}
end;

procedure TSynAutoComplete.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

destructor TSynAutoComplete.destroy;
begin
  RemoveEditor (fEditor);
  fKeyDownProc.Free;
  fKeyPressProc.Free;
  fAutoCompleteList.free;
  inherited;
end;

procedure TSynAutoComplete.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShortCutKey   : Word;
  ShortCutShift : TShiftState;
begin
  ShortCutToKey (fShortCut,ShortCutKey,ShortCutShift);
  if not (Sender as TCustomSynEdit).ReadOnly and
    (Shift = ShortCutShift) and (Key = ShortCutKey) then
  begin
    Execute(GetPreviousToken (Sender as TCustomSynEdit),Sender as TCustomSynEdit);
    fNoNextKey := true;
    Key := 0;
  end;
end;

procedure TSynAutoComplete.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if fNoNextKey then begin
    fNoNextKey := false;
    Key := #0;
  end;
end;

procedure TSynAutoComplete.Execute(token: string; Editor: TCustomSynEdit);
var
  Temp: string;
  i, j: integer;
  StartOfBlock: tpoint;
  ChangedIndent   : Boolean;
  ChangedTrailing : Boolean;
  TmpOptions : TSynEditorOptions;
  OrigOptions: TSynEditorOptions;
  BeginningSpaceCount : Integer;
  Spacing: String;
  PEditor: TProtectedAccessEditor;
begin
  if Assigned(OnBeforeExecute) then OnBeforeExecute(Self);
  try
    PEditor:=TProtectedAccessEditor(Editor);
    i := AutoCompleteList.IndexOf(token);
    if (i <> -1) then
    begin
      TmpOptions := Editor.Options;
      OrigOptions:= Editor.Options;
      ChangedIndent   := eoAutoIndent in TmpOptions;
      ChangedTrailing := eoTrimTrailingSpaces in TmpOptions;

      if ChangedIndent then Exclude(TmpOptions, eoAutoIndent);
      if ChangedTrailing then Exclude(TmpOptions, eoTrimTrailingSpaces);

      if ChangedIndent or ChangedTrailing then
        Editor.Options := TmpOptions;

      Editor.UndoList.AddChange(crAutoCompleteBegin, StartOfBlock, StartOfBlock, '',
        smNormal);

      fNoNextKey := true;
      for j := 1 to length(token) do
        Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
      BeginningSpaceCount := Editor.DisplayX - 1;  //GBN 2002/04/24
      if (not (eoTabsToSpaces in Editor.Options)) and (BeginningSpaceCount>=PEditor.TabWidth) then
          Spacing:=StringOfChar(#9,BeginningSpaceCount div PEditor.TabWidth)+StringOfChar(' ',BeginningSpaceCount mod PEditor.TabWidth)
      else Spacing:=StringOfChar(' ',BeginningSpaceCount);

      inc(i);
      StartOfBlock := Point(-1, -1);
      while (i < AutoCompleteList.Count) and
            (length(AutoCompleteList[i]) > 0) and
            (AutoCompleteList[i][1] = '=') do
      begin
  {      for j := 0 to PrevSpace - 1 do
          Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);}
        Temp := AutoCompleteList[i];
        for j := 2 to length(Temp) do begin
          if (Temp[j]=#9) then Editor.CommandProcessor(ecTab, Temp[j], nil)
          else Editor.CommandProcessor(ecChar, Temp[j], nil);
          if (Temp[j] = '|') then
            StartOfBlock := Editor.CaretXY
        end;
        inc(i);
        if (i < AutoCompleteList.Count) and
           (length(AutoCompleteList[i]) > 0) and
           (AutoCompleteList[i][1] = '=') then
        begin
           Editor.CommandProcessor (ecLineBreak,' ',nil);
           for j := 1 to length(Spacing) do
             if (Spacing[j]=#9) then Editor.CommandProcessor(ecTab,#9,nil)
             else Editor.CommandProcessor (ecChar, ' ', nil);
        end;
      end;
      if (StartOfBlock.x <> -1) and (StartOfBlock.y <> -1) then begin
        Editor.CaretXY := StartOfBlock;
        Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
      end;

      if ChangedIndent or ChangedTrailing then Editor.Options := OrigOptions;

      Editor.UndoList.AddChange(crAutoCompleteEnd, StartOfBlock, StartOfBlock, '',
        smNormal);
      fNoNextKey:=false;   //GBN 2002-03-07
    end;
  finally
  if Assigned(OnAfterExecute) then OnAfterExecute(Self);
  end;
end;

function TSynAutoComplete.GetPreviousToken(Editor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  Result := '';
  if Editor <> nil then begin
    s := Editor.LineText;
    i := Editor.CaretX - 1;
    if i <= Length (s) then begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        Dec(i);
      Result := copy(s, i + 1, Editor.CaretX - i - 1);
    end;
  end
end;

procedure TSynAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent is TCustomSynEdit) then
    RemoveEditor(AComponent as TCustomSynEdit);
  inherited Notification(AComponent, Operation);
end;

function TSynAutoComplete.RemoveEditor(aEditor: TCustomSynEdit): boolean;
begin
  Result := Assigned (aEditor);
  if Result then begin
    aEditor.RemoveKeyDownHandler (fKeyDownProc);
    aEditor.RemoveKeyPressHandler (fKeyPressProc);
    if aEditor = fEditor then
      fEditor := nil;
    {$IFDEF SYN_COMPILER_5_UP}
    RemoveFreeNotification( aEditor );
    {$ENDIF}
  end;
end;

procedure TSynAutoComplete.SetAutoCompleteList(List: TStrings);
begin
  fAutoCompleteList.Assign(List);
end;

procedure TSynAutoComplete.SetEditor(const Value: TCustomSynEdit);
begin
  if (fEditor <> nil) then
    RemoveEditor (fEditor);
  fEditor := Value;
  if (fEditor <> nil) then
    with fEditor do begin
      AddKeyDownHandler (fKeyDownProc);
      AddKeyPressHandler (fKeyPressProc);
      FreeNotification( fEditor );
    end;
end;

function TSynAutoComplete.GetTokenList: string;
var
  List: TStringList;
  i: integer;
begin
  Result := '';
  if AutoCompleteList.Count < 1 then Exit;
  List := TStringList.Create;
  i := 0;
  while (i < AutoCompleteList.Count) do begin
    if (length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') then
      List.Add(Trim(AutoCompleteList[i]));
    inc(i);
  end;
  Result := List.Text;
  List.Free;
end;

function TSynAutoComplete.GetTokenValue(Token: string): string;
var
  i: integer;
  List: TStringList;
begin
  Result := '';
  i := AutoCompleteList.IndexOf(Token);
  if i <> -1 then begin
    List := TStringList.Create;
    Inc(i);
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      if Length(AutoCompleteList[i]) = 1 then
        List.Add('')
      else
        List.Add(Copy(AutoCompleteList[i], 2, Length(AutoCompleteList[i])));
      inc(i);
    end;
    Result := List.Text;
    List.Free;
  end;
end;

end.
