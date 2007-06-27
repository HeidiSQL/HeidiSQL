{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEdit.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynEditOptionsDialog.pas,v 1.8 2002/05/20 18:18:04 harmeister Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:

-------------------------------------------------------------------------------}

unit SynEditOptionsDialog;

interface

uses
  Classes,
  SysUtils,
{$IFDEF SYN_CLX}
  Qt,
  Types,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QComCtrls,
  QExtCtrls,
  QButtons,
  QImgList,
  QMenus,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  Registry,
  ExtCtrls,
  Buttons,
  ImgList,
  Menus,
{$ENDIF}
  SynEdit,
  SynEditHighlighter,
  SynEditMiscClasses,
  SynEditKeyCmds;

const
  cpGutter     = 1;
  cpRightEdge  = 2;

type

  TSynEditorOptionsUserCommand = procedure(AUserCommand: Integer;
                                           var ADescription: String) of object;

  //NOTE: in order for the user commands to be recorded correctly, you must
  //      put the command itself in the object property.
  //      you can do this like so:
  //
  //      StringList.AddObject('ecSomeCommand', TObject(ecSomeCommand))
  //
  //      where ecSomeCommand is the command that you want to add

  TSynEditorOptionsAllUserCommands = procedure(ACommands: TStrings) of object;

  TSynEditorOptionsContainer = class;

  TfmEditorOptionsDialog = class(TForm)
    PageControl1: TPageControl;
    btnOk: TButton;
    btnCancel: TButton;
    Display: TTabSheet;
    ColorDialog: TColorDialog;
    ColorPopup: TPopupMenu;
    None1: TMenuItem;
    Scrollbar1: TMenuItem;
    ActiveCaption1: TMenuItem;
    Background1: TMenuItem;
    InactiveCaption1: TMenuItem;
    Menu1: TMenuItem;
    Window1: TMenuItem;
    WindowFrame1: TMenuItem;
    MEnu2: TMenuItem;
    WindowText1: TMenuItem;
    CaptionText1: TMenuItem;
    ActiveBorder1: TMenuItem;
    InactiveBorder1: TMenuItem;
    ApplicationWorkspace1: TMenuItem;
    Highlight1: TMenuItem;
    HighlightText1: TMenuItem;
    ButtonFace1: TMenuItem;
    ButtonShadow1: TMenuItem;
    GrayText1: TMenuItem;
    ButtonText1: TMenuItem;
    InactiveCaptionText1: TMenuItem;
    Highlight2: TMenuItem;
    N3dDarkShadow1: TMenuItem;
    N3DLight1: TMenuItem;
    InfoTipText1: TMenuItem;
    InfoTipBackground1: TMenuItem;
    ImageList1: TImageList;
    Options: TTabSheet;
    Keystrokes: TTabSheet;
    gbBookmarks: TGroupBox;
    ckBookmarkKeys: TCheckBox;
    ckBookmarkVisible: TCheckBox;
    gbLineSpacing: TGroupBox;
    eLineSpacing: TEdit;
    gbGutter: TGroupBox;
    Label1: TLabel;
    ckGutterAutosize: TCheckBox;
    ckGutterShowLineNumbers: TCheckBox;
    ckGutterShowLeaderZeros: TCheckBox;
    ckGutterStartAtZero: TCheckBox;
    ckGutterVisible: TCheckBox;
    gbRightEdge: TGroupBox;
    Label3: TLabel;
    pRightEdgeBack: TPanel;
    eRightEdge: TEdit;
    gbEditorFont: TGroupBox;
    btnFont: TButton;
    gbOptions: TGroupBox;
    ckAutoIndent: TCheckBox;
    ckDragAndDropEditing: TCheckBox;
    ckDragAndDropFiles: TCheckBox;
    ckHalfPageScroll: TCheckBox;
    ckNoSelection: TCheckBox;
    ckNoCaret: TCheckBox;
    ckScrollByOneLess: TCheckBox;
    ckScrollPastEOF: TCheckBox;
    ckScrollPastEOL: TCheckBox;
    ckShowScrollHint: TCheckBox;
    ckSmartTabs: TCheckBox;
    ckTabsToSpaces: TCheckBox;
    ckTrimTrailingSpaces: TCheckBox;
    ckWantTabs: TCheckBox;
    gbCaret: TGroupBox;
    cInsertCaret: TComboBox;
    Label2: TLabel;
    Label4: TLabel;
    cOverwriteCaret: TComboBox;
    Panel3: TPanel;
    labFont: TLabel;
    FontDialog: TFontDialog;
    btnAddKey: TButton;
    btnRemKey: TButton;
    gbKeyStrokes: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cKeyCommand: TComboBox;
    btnUpdateKey: TButton;
    ckAltSetsColumnMode: TCheckBox;
    ckKeepCaretX: TCheckBox;
    eTabWidth: TEdit;
    pRightEdgeColor: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    cbGutterFont: TCheckBox;
    btnGutterFont: TButton;
    btnRightEdge: TPanel;
    Image1: TImage;
    pGutterBack: TPanel;
    pGutterColor: TPanel;
    btnGutterColor: TPanel;
    Image2: TImage;
    ckScrollHintFollows: TCheckBox;
    ckGroupUndo: TCheckBox;
    ckSmartTabDelete: TCheckBox;
    ckRightMouseMoves: TCheckBox;
    pnlGutterFontDisplay: TPanel;
    lblGutterFont: TLabel;
    ckEnhanceHomeKey: TCheckBox;
    pnlCommands: TPanel;
    KeyList: TListView;
    ckHideShowScrollbars: TCheckBox;
    ckDisableScrollArrows: TCheckBox;
    procedure PopupMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pGutterColorClick(Sender: TObject);
    procedure pRightEdgeColorClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure KeyListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnUpdateKeyClick(Sender: TObject);
    procedure btnAddKeyClick(Sender: TObject);
    procedure btnRemKeyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure KeyListEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure btnOkClick(Sender: TObject);
    procedure btnGutterFontClick(Sender: TObject);
    procedure cbGutterFontClick(Sender: TObject);
    procedure btnRightEdgeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnGutterColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cKeyCommandExit(Sender: TObject);
    procedure cKeyCommandKeyPress(Sender: TObject; var Key: Char);
    procedure cKeyCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure KeyListChanging(Sender: TObject; Item: TListItem;
      Change: TItemChange; var AllowChange: Boolean);
  private
    FSynEdit: TSynEditorOptionsContainer;
    FPoppedFrom : Integer;
    FUserCommand: TSynEditorOptionsUserCommand;
    FAllUserCommands: TSynEditorOptionsAllUserCommands;

    OldSelected: TListItem;
    InChanging: Boolean;
    FExtended: Boolean;

    function GetColor(Item : TMenuItem) : TColor;
    procedure GetData;
    procedure PutData;
    procedure EditStrCallback(const S: string);
    procedure FillInKeystrokeInfo(AKey: TSynEditKeystroke; AItem: TListItem);
  public
    eKeyShort2: TSynHotKey;
    eKeyShort1: TSynHotKey;

    function Execute(EditOptions : TSynEditorOptionsContainer) : Boolean;
    property GetUserCommandNames: TSynEditorOptionsUserCommand read FUserCommand
      write FUserCommand;
    property GetAllUserCommands: TSynEditorOptionsAllUserCommands
      read FAllUserCommands
      write FAllUserCommands;
    property UseExtendedStrings: Boolean read FExtended write FExtended;
  end;

  TSynEditOptionsDialog = class(TComponent)
  private
    FForm: TfmEditorOptionsDialog;
    function GetUserCommandNames: TSynEditorOptionsUserCommand;
    procedure SetUserCommandNames(
      const Value: TSynEditorOptionsUserCommand);
    function GetUserCommands: TSynEditorOptionsAllUserCommands;
    procedure SetUserCommands(
      const Value: TSynEditorOptionsAllUserCommands);
    function GetExtended: Boolean;
    procedure SetExtended(const Value: Boolean);
  public
    constructor create(AOwner : TComponent); override;
    destructor destroy; override;
    function Execute(EditOptions : TSynEditorOptionsContainer) : Boolean;
  published
    property GetUserCommand: TSynEditorOptionsUserCommand
      read GetUserCommandNames
      write SetUserCommandNames;
    property GetAllUserCommands: TSynEditorOptionsAllUserCommands
      read GetUserCommands
      write SetUserCommands;
    property UseExtendedStrings: Boolean read GetExtended write SetExtended;
  end;

  //This class is assignable to a SynEdit without modifying key properties that affect function
  TSynEditorOptionsContainer = class(TComponent)
  private
    FHideSelection: Boolean;
    FWantTabs: Boolean;
    FMaxUndo: Integer;
    FExtraLineSpacing: Integer;
    FTabWidth: Integer;
    FMaxLeftChar: Integer;
    FRightEdge: Integer;
    FSelectedColor: TSynSelectedColor;
    FRightEdgeColor: TColor;
    FFont: TFont;
    FBookmarks: TSynBookMarkOpt;
    FOverwriteCaret: TSynEditCaretType;
    FInsertCaret: TSynEditCaretType;
    FKeystrokes: TSynEditKeyStrokes;
    FOptions: TSynEditorOptions;
    FSynGutter: TSynGutter;
    FWordBreakChars: String;
    FColor: TColor;
    procedure SetBookMarks(const Value: TSynBookMarkOpt);
    procedure SetFont(const Value: TFont);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetOptions(const Value: TSynEditorOptions);
    procedure SetSynGutter(const Value: TSynGutter);
  public
    constructor create(AOwner : TComponent); override;
    destructor destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure AssignTo(Dest : TPersistent); override;
  published
    property Options : TSynEditorOptions read FOptions write SetOptions;
    property BookMarkOptions : TSynBookMarkOpt read FBookmarks write SetBookMarks;
    property Color : TColor read FColor write FColor;
    property Font : TFont read FFont write SetFont;
    property ExtraLineSpacing : Integer read FExtraLineSpacing write FExtraLineSpacing;
    property Gutter : TSynGutter read FSynGutter write SetSynGutter;
    property RightEdge : Integer read FRightEdge write FRightEdge;
    property RightEdgeColor : TColor read FRightEdgeColor write FRightEdgeColor;
    property WantTabs : Boolean read FWantTabs write FWantTabs;
    property InsertCaret : TSynEditCaretType read FInsertCaret write FInsertCaret;
    property OverwriteCaret : TSynEditCaretType read FOverwriteCaret write FOverwriteCaret;
    property HideSelection : Boolean read FHideSelection write FHideSelection;
    property MaxLeftChar : Integer read FMaxLeftChar write FMaxLeftChar;
    property MaxUndo : Integer read FMaxUndo write FMaxUndo;
    property SelectedColor : TSynSelectedColor read FSelectedColor write FSelectedColor;
    property TabWidth : Integer read FTabWidth write FTabWidth;
    property WordBreakChars : String read FWordBreakChars write FWordBreakChars;
    property Keystrokes : TSynEditKeyStrokes read FKeystrokes write SetKeystrokes;
  end;

  procedure Register;

implementation

{$R *.DFM}

  procedure Register;
  begin
    RegisterComponents('SynEdit', [TSynEditOptionsDialog]);
  end;

{ TfmOptions }

function TfmEditorOptionsDialog.Execute(EditOptions : TSynEditorOptionsContainer) : Boolean;
begin
  if (EditOptions = nil) then
  begin
    Result:= False;
    Exit;
  end;
  //Assign the Containers
  FSynEdit:= EditOptions;
  //Get Data
  GetData;
  //Show the form
  Result:= Showmodal = mrOk;
  //PutData
  if Result then PutData;
end;


procedure TfmEditorOptionsDialog.GetData;
var I : Integer;
    Item : TListItem;
begin
  //Gutter
  ckGutterVisible.Checked:= FSynEdit.Gutter.Visible;
  ckGutterAutosize.Checked:= FSynEdit.Gutter.Visible;
  ckGutterShowLineNumbers.Checked:= FSynEdit.Gutter.ShowLineNumbers;
  ckGutterShowLeaderZeros.Checked:= FSynEdit.Gutter.LeadingZeros;
  ckGutterStartAtZero.Checked:= FSynEdit.Gutter.ZeroStart;
  cbGutterFont.Checked := FSynEdit.Gutter.UseFontStyle;
  pGutterColor.Color:= FSynEdit.Gutter.Color;
  lblGutterFont.Font.Assign(FSynEdit.Gutter.Font);
  lblGutterFont.Caption:= lblGutterFont.Font.Name + ' ' + IntToStr(lblGutterFont.Font.Size) + 'pt';  
  //Right Edge
  eRightEdge.Text:= IntToStr(FSynEdit.RightEdge);
  pRightEdgeColor.Color:= FSynEdit.RightEdgeColor;
  //Line Spacing
  eLineSpacing.Text:= IntToStr(FSynEdit.ExtraLineSpacing);
  eTabWidth.Text:= IntToStr(FSynEdit.TabWidth);
  //Break Chars
//!!  eBreakchars.Text:= FSynEdit.WordBreakChars;
  //Bookmarks
  ckBookmarkKeys.Checked:= FSynEdit.BookMarkOptions.EnableKeys;
  ckBookmarkVisible.Checked:= FSynEdit.BookMarkOptions.GlyphsVisible;
  //Font
  labFont.Font.Assign(FSynEdit.Font);
  labFont.Caption:= labFont.Font.Name + ' ' + IntToStr(labFont.Font.Size) + 'pt';
  //Options
  ckAutoIndent.Checked:= eoAutoIndent in FSynEdit.Options;
  ckDragAndDropEditing.Checked:= eoDragDropEditing in FSynEdit.Options;
  ckDragAndDropFiles.Checked:= eoDropFiles in FSynEdit.Options;
  ckNoSelection.Checked:= eoNoSelection in FSynEdit.Options;
  ckNoCaret.Checked:= eoNoCaret in FSynEdit.Options;
  ckWantTabs.Checked:= FSynEdit.WantTabs;
  ckSmartTabs.Checked:= eoSmartTabs in FSynEdit.Options;
  ckAltSetsColumnMode.Checked:= eoAltSetsColumnMode in FSynEdit.Options;
  ckHalfPageScroll.Checked:= eoHalfPageScroll in FSynEdit.Options;
  ckScrollByOneLess.Checked:= eoScrollByOneLess in FSynEdit.Options;
  ckScrollPastEOF.Checked:= eoScrollPastEof in FSynEdit.Options;
  ckScrollPastEOL.Checked:= eoScrollPastEol in FSynEdit.Options;
  ckShowScrollHint.Checked:= eoShowScrollHint in FSynEdit.Options;
  ckTabsToSpaces.Checked:= eoTabsToSpaces in FSynEdit.Options;
  ckTrimTrailingSpaces.Checked:= eoTrimTrailingSpaces in FSynEdit.Options;
  ckKeepCaretX.Checked:= eoKeepCaretX in FSynEdit.Options;
  ckSmartTabDelete.Checked := eoSmartTabDelete in FSynEdit.Options;
  ckRightMouseMoves.Checked := eoRightMouseMovesCursor in FSynEdit.Options;
  ckEnhanceHomeKey.Checked := eoEnhanceHomeKey in FSynEdit.Options;
  ckGroupUndo.Checked := eoGroupUndo in FSynEdit.Options;
  ckDisableScrollArrows.Checked := eoDisableScrollArrows in FSynEdit.Options;
  ckHideShowScrollbars.Checked := eoHideShowScrollbars in FSynEdit.Options;

  //Caret
  cInsertCaret.ItemIndex:= ord(FSynEdit.InsertCaret);
  cOverwriteCaret.ItemIndex:= ord(FSynEdit.OverwriteCaret);


  KeyList.Items.BeginUpdate;
  try
    KeyList.Items.Clear;
    for I:= 0 to FSynEdit.Keystrokes.Count-1 do
    begin
      Item:= KeyList.Items.Add;
      FillInKeystrokeInfo(FSynEdit.Keystrokes.Items[I], Item);
      Item.Data:= FSynEdit.Keystrokes.Items[I];
    end;
    if (KeyList.Items.Count > 0) then KeyList.Items[0].Selected:= True;
  finally
    KeyList.Items.EndUpdate;
  end;
end;

procedure TfmEditorOptionsDialog.PutData;
var SynEditOptions : TSynEditorOptions;
begin
  //Gutter
  FSynEdit.Gutter.Visible:= ckGutterVisible.Checked;
  FSynEdit.Gutter.Visible:= ckGutterAutosize.Checked;
  FSynEdit.Gutter.ShowLineNumbers:= ckGutterShowLineNumbers.Checked;
  FSynEdit.Gutter.LeadingZeros:= ckGutterShowLeaderZeros.Checked;
  FSynEdit.Gutter.ZeroStart:= ckGutterStartAtZero.Checked;
  FSynEdit.Gutter.Color:= pGutterColor.Color;
  FSynEdit.Gutter.UseFontStyle := cbGutterFont.Checked;
  FSynEdit.Gutter.Font.Assign(lblGutterFont.Font);
  //Right Edge
  FSynEdit.RightEdge:= StrToIntDef(eRightEdge.Text, 80);
  FSynEdit.RightEdgeColor:= pRightEdgeColor.Color;
  //Line Spacing
  FSynEdit.ExtraLineSpacing:= StrToIntDef(eLineSpacing.Text, 0);
  FSynEdit.TabWidth:= StrToIntDef(eTabWidth.Text, 8);
  //Break Chars
//!!  FSynEdit.WordBreakChars:= eBreakchars.Text;
  //Bookmarks
  FSynEdit.BookMarkOptions.EnableKeys:= ckBookmarkKeys.Checked;
  FSynEdit.BookMarkOptions.GlyphsVisible:= ckBookmarkVisible.Checked;
  //Font
  FSynEdit.Font.Assign(labFont.Font);
  //Options
  SynEditOptions:= [];
  if ckAutoIndent.Checked then SynEditOptions:= SynEditOptions + [ eoAutoIndent ];
  if ckDragAndDropEditing.Checked then SynEditOptions:= SynEditOptions + [ eoDragDropEditing ];
  if ckDragAndDropFiles.Checked then SynEditOptions:= SynEditOptions + [ eoDropFiles ];
  if ckNoSelection.Checked then SynEditOptions:= SynEditOptions + [ eoNoSelection ];
  if ckNoCaret.Checked then SynEditOptions:= SynEditOptions + [ eoNoCaret ];
  FSynEdit.WantTabs:= ckWantTabs.Checked;
  if ckSmartTabs.Checked then SynEditOptions:= SynEditOptions + [ eoSmartTabs ];
  if ckAltSetsColumnMode.Checked then SynEditOptions:= SynEditOptions + [ eoAltSetsColumnMode ];
  if ckHalfPageScroll.Checked then SynEditOptions:= SynEditOptions + [ eoHalfPageScroll ];
  if ckScrollByOneLess.Checked then SynEditOptions:= SynEditOptions + [ eoScrollByOneLess ];
  if ckScrollPastEOF.Checked then SynEditOptions:= SynEditOptions + [ eoScrollPastEof ];
  if ckScrollPastEOL.Checked then SynEditOptions:= SynEditOptions + [ eoScrollPastEol ];
  if ckShowScrollHint.Checked then SynEditOptions:= SynEditOptions + [ eoShowScrollHint ];
  if ckTabsToSpaces.Checked then SynEditOptions:= SynEditOptions + [ eoTabsToSpaces ];
  if ckTrimTrailingSpaces.Checked then SynEditOptions:= SynEditOptions + [ eoTrimTrailingSpaces ];
  if ckKeepCaretX.Checked then SynEditOptions:= SynEditOptions + [ eoKeepCaretX ];
  if ckSmartTabDelete.Checked then SynEditOptions:= SynEditOptions + [eoSmartTabDelete];
  if ckRightMouseMoves.Checked then SynEditOptions:= SynEditOptions + [eoRightMouseMovesCursor];
  if ckEnhanceHomeKey.Checked then SynEditOptions:= SynEditOptions + [eoEnhanceHomeKey];
  if ckGroupUndo.Checked then SynEditOptions:= SynEditOptions + [eoGroupUndo];
  if ckDisableScrollArrows.Checked then SynEditOptions:= SynEditOptions + [eoDisableScrollArrows];
  if ckHideShowScrollbars.Checked then SynEditOptions:= SynEditOptions + [eoHideShowScrollbars];

  FSynEdit.Options:= SynEditOptions;
  //Caret
  FSynEdit.InsertCaret:= TSynEditCaretType(cInsertCaret.ItemIndex);
  FSynEdit.OverwriteCaret:= TSynEditCaretType(cOverwriteCaret.ItemIndex);
end;

function TfmEditorOptionsDialog.GetColor(Item: TMenuItem): TColor;
begin
 if (Item.Tag = -1) or (Item.Tag > 24) then
  Result:= clNone
 else
  Result:= TColor(Byte(Item.Tag) or $80000000);
end;

procedure TfmEditorOptionsDialog.PopupMenuClick(Sender: TObject);
var C : TColor;
begin
  C:= GetColor(TMenuItem(Sender));
  //Set the color based on where it was "popped from"
  if (FPoppedFrom = cpGutter) then
    pGutterColor.Color:= C
  else if (FPoppedFrom = cpRightEdge) then
    pRightEdgeColor.Color:= C;
end;

procedure TfmEditorOptionsDialog.FormCreate(Sender: TObject);
var I : Integer;
    C : TColor;
    B : TBitmap;
    R : TRect;
begin
  InChanging := False;
  B:= TBitmap.Create;
  try
    B.Width:= 16;
    B.Height:= 16;
    R:= B.Canvas.ClipRect;
    //Loop through and create colored images
    for I:= 0 to ColorPopup.Items.Count-1 do
    begin
      if ColorPopup.Items[I].Tag = -1 then Continue;
      C:= GetColor(ColorPopup.Items[I]);
      B.Canvas.Brush.Color:= C;
      B.Canvas.Brush.Style:= bsSolid;
      B.Canvas.Pen.Style:= psSolid;
      B.Canvas.Pen.Color:= clBlack;
      B.Canvas.Rectangle(R);
      ImageList1.Add(B, nil);
      ColorPopup.Items[I].ImageIndex:= ColorPopup.Items[I].Tag;
    end;
  finally
    B.Free;
  end;

  eKeyShort1:= TSynHotKey.Create(Self);
  with eKeyShort1 do
  begin
    Parent := gbKeystrokes;
    Left := 120;
    Top := 55;
    Width := 185;
    Height := 21;
    TabOrder := 1;
  end;

  eKeyShort2:= TSynHotKey.Create(Self);
  with eKeyShort2 do
  begin
    Parent := gbKeystrokes;
    Left := 120;
    Top := 87;
    Width := 185;
    Height := 21;
    TabOrder := 2;
  end;
end;

{ TSynEditOptionsDialog }

constructor TSynEditOptionsDialog.create(AOwner: TComponent);
begin
  inherited;
  FForm:= TfmEditorOptionsDialog.Create(Self);
end;

destructor TSynEditOptionsDialog.destroy;
begin
  FForm.Free;
  inherited;
end;

function TSynEditOptionsDialog.Execute(EditOptions : TSynEditorOptionsContainer) : Boolean;
begin
  Result:= FForm.Execute(EditOptions);
end;

function TSynEditOptionsDialog.GetUserCommands: TSynEditorOptionsAllUserCommands;
begin
  Result := FForm.GetAllUserCommands;
end;

function TSynEditOptionsDialog.GetUserCommandNames: TSynEditorOptionsUserCommand;
begin
  Result := FForm.GetUserCommandNames
end;

procedure TSynEditOptionsDialog.SetUserCommands(
  const Value: TSynEditorOptionsAllUserCommands);
begin
  FForm.GetAllUserCommands := Value;
end;

procedure TSynEditOptionsDialog.SetUserCommandNames(
  const Value: TSynEditorOptionsUserCommand);
begin
  FForm.GetUserCommandNames := Value;
end;

function TSynEditOptionsDialog.GetExtended: Boolean;
begin
  Result := FForm.UseExtendedStrings;
end;

procedure TSynEditOptionsDialog.SetExtended(const Value: Boolean);
begin
  FForm.UseExtendedStrings := Value;
end;

{ TSynEditorOptionsContainer }

procedure TSynEditorOptionsContainer.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TCustomSynEdit) then
  begin
    Self.Font.Assign(TCustomSynEdit(Source).Font);
    Self.BookmarkOptions.Assign(TCustomSynEdit(Source).BookmarkOptions);
    Self.Gutter.Assign(TCustomSynEdit(Source).Gutter);
    Self.Keystrokes.Assign(TCustomSynEdit(Source).Keystrokes);
    Self.SelectedColor.Assign(TCustomSynEdit(Source).SelectedColor);

    Self.Color := TCustomSynEdit(Source).Color;
    Self.Options := TCustomSynEdit(Source).Options;
    Self.ExtraLineSpacing := TCustomSynEdit(Source).ExtraLineSpacing;
    Self.HideSelection := TCustomSynEdit(Source).HideSelection;
    Self.InsertCaret := TCustomSynEdit(Source).InsertCaret;
    Self.OverwriteCaret := TCustomSynEdit(Source).OverwriteCaret;
    Self.MaxLeftChar := TCustomSynEdit(Source).MaxLeftChar;
    Self.MaxUndo := TCustomSynEdit(Source).MaxUndo;
    Self.RightEdge := TCustomSynEdit(Source).RightEdge;
    Self.RightEdgeColor := TCustomSynEdit(Source).RightEdgeColor;
    Self.TabWidth := TCustomSynEdit(Source).TabWidth;
    Self.WantTabs := TCustomSynEdit(Source).WantTabs;
//!!    Self.WordBreakChars := TSynEdit(Source).WordBreakChars;
  end else
    inherited;
end;

procedure TSynEditorOptionsContainer.AssignTo(Dest: TPersistent);
begin
  if Assigned(Dest) and (Dest is TCustomSynEdit) then
  begin
    TCustomSynEdit(Dest).Font.Assign(Self.Font);
    TCustomSynEdit(Dest).BookmarkOptions.Assign(Self.BookmarkOptions);
    TCustomSynEdit(Dest).Gutter.Assign(Self.Gutter);
    TCustomSynEdit(Dest).Keystrokes.Assign(Self.Keystrokes);
    TCustomSynEdit(Dest).SelectedColor.Assign(Self.SelectedColor);

    TCustomSynEdit(Dest).Color := Self.Color;
    TCustomSynEdit(Dest).Options := Self.Options;
    TCustomSynEdit(Dest).ExtraLineSpacing := Self.ExtraLineSpacing;
    TCustomSynEdit(Dest).HideSelection := Self.HideSelection;
    TCustomSynEdit(Dest).InsertCaret := Self.InsertCaret;
    TCustomSynEdit(Dest).OverwriteCaret := Self.OverwriteCaret;
    TCustomSynEdit(Dest).MaxLeftChar := Self.MaxLeftChar;
    TCustomSynEdit(Dest).MaxUndo := Self.MaxUndo;
    TCustomSynEdit(Dest).RightEdge := Self.RightEdge;
    TCustomSynEdit(Dest).RightEdgeColor := Self.RightEdgeColor;
    TCustomSynEdit(Dest).TabWidth := Self.TabWidth;
    TCustomSynEdit(Dest).WantTabs := Self.WantTabs;
  end else
    inherited;
end;

constructor TSynEditorOptionsContainer.create(AOwner: TComponent);
begin
  inherited;
  FBookmarks:= TSynBookMarkOpt.Create(Self);
  FKeystrokes:= TSynEditKeyStrokes.Create(Self);
  FSynGutter:= TSynGutter.Create;
  FSelectedColor:= TSynSelectedColor.Create;
  FSelectedColor.Foreground:= clHighlightText;
  FSelectedColor.Background:= clHighlight;
  FFont:= TFont.Create;
  FFont.Name:= 'Courier New';
  FFont.Size:= 8;
  Color:= clWindow;
  Keystrokes.ResetDefaults;
  Options := [eoAutoIndent,eoDragDropEditing,eoDropFiles,eoScrollPastEol,
    eoShowScrollHint,eoSmartTabs,eoAltSetsColumnMode, eoTabsToSpaces,eoTrimTrailingSpaces, eoKeepCaretX];
  ExtraLineSpacing := 0;
  HideSelection := False;
  InsertCaret := ctVerticalLine;
  OverwriteCaret := ctBlock;
  MaxLeftChar := 1024;
  MaxUndo := 1024;
  RightEdge := 80;
  RightEdgeColor := clSilver;
  TabWidth := 8;
  WantTabs := True;
//!!  WordBreakChars:= '.,;:''"&!?$%#@<>[](){}^-=+-*/\|';
end;

destructor TSynEditorOptionsContainer.destroy;
begin
  FBookMarks.Free;
  FKeyStrokes.Free;
  FSynGutter.Free;
  FFont.Free;
  inherited;
end;

procedure TSynEditorOptionsContainer.SetBookMarks(
  const Value: TSynBookMarkOpt);
begin
  FBookmarks.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetKeystrokes(
  const Value: TSynEditKeyStrokes);
begin
  FKeystrokes.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetOptions(
  const Value: TSynEditorOptions);
begin
  FOptions:= Value;
end;

procedure TSynEditorOptionsContainer.SetSynGutter(const Value: TSynGutter);
begin
  FSynGutter.Assign(Value);
end;

procedure TfmEditorOptionsDialog.pGutterColorClick(Sender: TObject);
begin
  ColorDialog.Color:= pGutterColor.Color;
  if (ColorDialog.Execute) then
  begin
    pGutterColor.Color:= ColorDialog.Color;
  end;
end;

procedure TfmEditorOptionsDialog.pRightEdgeColorClick(Sender: TObject);
begin
  ColorDialog.Color:= pRightEdgeColor.Color;
  if (ColorDialog.Execute) then
  begin
    pRightEdgeColor.Color:= ColorDialog.Color;
  end;
end;

procedure TfmEditorOptionsDialog.btnFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(labFont.Font);
  if FontDialog.Execute then
  begin
    labFont.Font.Assign(FontDialog.Font);
    labFont.Caption:= labFont.Font.Name;
    labFont.Caption:= labFont.Font.Name + ' ' + IntToStr(labFont.Font.Size) + 'pt';    
  end;
end;

procedure TfmEditorOptionsDialog.KeyListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if KeyList.Selected = nil then Exit;
  cKeyCommand.Text      := KeyList.Selected.Caption;
  cKeyCommand.ItemIndex := cKeyCommand.Items.IndexOf(KeyList.Selected.Caption);
  eKeyShort1.HotKey     := TSynEditKeyStroke(KeyList.Selected.Data).ShortCut;
  eKeyShort2.HotKey     := TSynEditKeyStroke(KeyList.Selected.Data).ShortCut2;
  OldSelected := Item;
end;

procedure TfmEditorOptionsDialog.btnUpdateKeyClick(Sender: TObject);
var Cmd          : Integer;
{    KeyLoc       : Integer;
    TmpCommand   : String;
    OldShortcut  : TShortcut;
    OldShortcut2 : TShortcut;
}
begin
  if (KeyList.Selected = nil) and (Sender <> btnAddKey) then
  begin
    btnAddKey.Click;
    Exit;
  end;

  if KeyList.Selected = nil then Exit;
  if cKeyCommand.ItemIndex < 0 then Exit;

  Cmd := Integer(cKeyCommand.Items.Objects[cKeyCommand.ItemIndex]);

  TSynEditKeyStroke(OldSelected.Data).Command:= Cmd;

  if (eKeyShort1.Text <> '') then
    TSynEditKeyStroke(OldSelected.Data).ShortCut:= eKeyShort1.HotKey;

  if (eKeyShort2.Text <> '') then
    TSynEditKeyStroke(OldSelected.Data).ShortCut2:= eKeyShort2.HotKey;

  FillInKeystrokeInfo(TSynEditKeyStroke(OldSelected.Data), KeyList.Selected);
end;

procedure TfmEditorOptionsDialog.btnAddKeyClick(Sender: TObject);
var Item : TListItem;
begin
  Item:= KeyList.Items.Add;
  Item.Data:= FSynEdit.Keystrokes.Add;
  Item.Selected:= True;
  btnUpdateKeyClick(btnAddKey);
end;

procedure TfmEditorOptionsDialog.btnRemKeyClick(Sender: TObject);
begin
  if KeyList.Selected = nil then Exit;
  TSynEditKeyStroke(KeyList.Selected.Data).Free;
  KeyList.Selected.Delete;
end;

procedure TfmEditorOptionsDialog.EditStrCallback(const S: string);
begin
  //Add the Item
  if FExtended then
    cKeyCommand.Items.AddObject(S, TObject(ConvertExtendedToCommand(S)))
  else cKeyCommand.Items.AddObject(S, TObject(ConvertCodeStringToCommand(S)));
end;

procedure TfmEditorOptionsDialog.FormShow(Sender: TObject);
var Commands: TStringList;
    i : Integer;
begin
//We need to do this now because it will not have been assigned when
//create occurs
  cKeyCommand.Items.Clear;
  //Start the callback to add the strings
  if FExtended then
    GetEditorCommandExtended(EditStrCallback)
  else GetEditorCommandValues(EditStrCallBack);
  //Now add in the user defined ones if they have any
  if Assigned(FAllUserCommands) then
  begin
    Commands := TStringList.Create;
    try
      FAllUserCommands(Commands);
      for i := 0 to Commands.Count - 1 do
        if Commands.Objects[i] <> nil then
          cKeyCommand.Items.AddObject(Commands[i], Commands.Objects[i]);
    finally
      Commands.Free;
    end;
  end;

  PageControl1.ActivePageIndex:= 0;
end;

procedure TfmEditorOptionsDialog.KeyListEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  AllowEdit:= False;
end;


procedure TfmEditorOptionsDialog.btnOkClick(Sender: TObject);
begin
  btnUpdateKey.Click;
  ModalResult:= mrOk;
end;

procedure TfmEditorOptionsDialog.btnGutterFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(lblGutterFont.Font);
  if FontDialog.Execute then
  begin
    lblGutterFont.Font.Assign(FontDialog.Font);
    lblGutterFont.Caption:= lblGutterFont.Font.Name + ' ' + IntToStr(lblGutterFont.Font.Size) + 'pt';
  end;
end;

procedure TfmEditorOptionsDialog.cbGutterFontClick(Sender: TObject);
begin
  lblGutterFont.Enabled := cbGutterFont.Checked;
  btnGutterFont.Enabled := cbGutterFont.Checked;
end;

procedure TfmEditorOptionsDialog.btnRightEdgeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  FPoppedFrom:= cpRightEdge;
  P:= pRightEdgeColor.ClientToScreen(Point(-1, pRightEdgeColor.Height-1));
  btnRightEdge.BevelOuter := bvLowered;
  ColorPopup.Popup(P.X, P.Y);
  btnRightEdge.BevelOuter := bvNone;
end;

procedure TfmEditorOptionsDialog.btnGutterColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  FPoppedFrom:= cpGutter;
  P:= pGutterColor.ClientToScreen(Point(-1, pGutterColor.Height-1));
  btnGutterColor.BevelOuter := bvLowered;
  ColorPopup.Popup(P.X, P.Y);
  btnGutterColor.BevelOuter := bvNone;
end;

procedure TfmEditorOptionsDialog.FillInKeystrokeInfo(
  AKey: TSynEditKeystroke; AItem: TListItem);
var TmpString: String;
begin
  with AKey do
  begin
    if Command >= ecUserFirst then
    begin
      TmpString := 'User Command';
      if Assigned(GetUserCommandNames) then
        GetUserCommandNames(Command, TmpString);
    end else begin
      if FExtended then
        TmpString := ConvertCodeStringToExtended(EditorCommandToCodeString(Command))
      else TmpString := EditorCommandToCodeString(Command);
    end;

    AItem.Caption:= TmpString;
    AItem.SubItems.Clear;

    TmpString := '';
    if Shortcut <> 0 then
      TmpString := ShortCutToText(ShortCut);

    if (TmpString <> '') and (Shortcut2 <> 0) then
      TmpString := TmpString + ' ' + ShortCutToText(ShortCut2);

    AItem.SubItems.Add(TmpString);

  end;

end;

procedure TfmEditorOptionsDialog.cKeyCommandExit(Sender: TObject);
VAR TmpIndex : Integer;
begin
  TmpIndex := cKeyCommand.Items.IndexOf(cKeyCommand.Text);
  if TmpIndex = -1 then
  begin
    if FExtended then
      cKeyCommand.ItemIndex := cKeyCommand.Items.IndexOf(ConvertCodeStringToExtended('ecNone'))
    else cKeyCommand.ItemIndex := cKeyCommand.Items.IndexOf('ecNone');
  end else cKeyCommand.ItemIndex := TmpIndex;  //need to force it incase they just typed something in

end;

procedure TfmEditorOptionsDialog.cKeyCommandKeyPress(Sender: TObject;
  var Key: Char);
var WorkStr : String;
    i       : Integer;
begin
//This would be better if componentized, but oh well...
  WorkStr := AnsiUppercase(Copy(cKeyCommand.Text, 1, cKeyCommand.SelStart) + Key);
  i := 0;
  While i < cKeyCommand.Items.Count do
  begin
    if pos(WorkStr, AnsiUppercase(cKeyCommand.Items[i])) = 1 then
    begin
      cKeyCommand.Text := cKeyCommand.Items[i];
      cKeyCommand.SelStart := length(WorkStr);
      cKeyCommand.SelLength := Length(cKeyCommand.Text) - cKeyCommand.SelStart;
      Key := #0;
      break;
    end else inc(i);
  end;

end;

procedure TfmEditorOptionsDialog.cKeyCommandKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then btnUpdateKey.Click;
end;

procedure TfmEditorOptionsDialog.KeyListChanging(Sender: TObject;
  Item: TListItem; Change: TItemChange; var AllowChange: Boolean);
begin
//make sure that it's saved.
  if InChanging then exit;
  InChanging := True;
  if Visible then
  begin
    if (Item = OldSelected) and
       ((Item.Caption <> cKeyCommand.Text) or
       (TSynEditKeystroke(Item.Data).ShortCut <> eKeyShort1.HotKey) or
       (TSynEditKeystroke(Item.Data).ShortCut2 <> eKeyShort2.HotKey)) then
    begin
      btnUpdateKeyClick(btnUpdateKey);
    end;
  end;
  InChanging := False;
end;

initialization

  RegisterClasses([TSynEditorOptionsContainer]);

end.
