unit texteditor;

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, VirtualTrees,
  ComCtrls, ToolWin, Dialogs, SysUtils, Menus, ExtDlgs,
  helpers, gnugettext, ActnList, StdActns;

{$I const.inc}

type
  TfrmTextEditor = class(TForm)
    memoText: TMemo;
    tlbStandard: TToolBar;
    btnWrap: TToolButton;
    btnLoadText: TToolButton;
    btnApply: TToolButton;
    btnCancel: TToolButton;
    lblTextLength: TLabel;
    btnLinebreaks: TToolButton;
    popupLinebreaks: TPopupMenu;
    menuWindowsLB: TMenuItem;
    menuUnixLB: TMenuItem;
    menuMacLB: TMenuItem;
    menuMixedLB: TMenuItem;
    menuWideLB: TMenuItem;
    ActionList1: TActionList;
    actSearchFind: TSearchFind;
    btnSearchFind: TToolButton;
    actSearchFindNext: TSearchFindNext;
    actSearchReplace: TSearchReplace;
    btnSearchReplace: TToolButton;
    btnSearchFindNext: TToolButton;
    btnSeparator1: TToolButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadTextClick(Sender: TObject);
    procedure btnWrapClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memoTextChange(Sender: TObject);
    procedure memoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SelectLinebreaks(Sender: TObject);
  private
    { Private declarations }
    FModified: Boolean;
    FStopping: Boolean;
    DetectedLineBreaks,
    SelectedLineBreaks: TLineBreaks;
    procedure SetModified(NewVal: Boolean);
  public
    function GetText: String;
    procedure SetText(text: String);
    procedure SetMaxLength(len: integer);
    procedure SetFont(font: TFont);
    property Modified: Boolean read FModified write SetModified;
  end;


implementation

uses main;

{$R *.dfm}


function TfrmTextEditor.GetText: String;
var
  LB: String;
begin
  Result := memoText.Text;
  // Convert linebreaks back to selected
  LB := '';
  case SelectedLineBreaks of
    lbsUnix: LB := LB_UNIX;
    lbsMac: LB := LB_MAC;
    lbsWide: LB := LB_WIDE;
  end;
  if LB <> '' then
    Result := StringReplace(Result, CRLF, LB, [rfReplaceAll]);
end;


procedure TfrmTextEditor.SetText(text: String);
var
  LB: String;
  Detected: TMenuItem;
begin
  DetectedLineBreaks := ScanLineBreaks(text);
  case DetectedLineBreaks of
    lbsWindows, lbsNone: Detected := menuWindowsLB;
    lbsUnix: Detected := menuUnixLB;
    lbsMac: Detected := menuMacLB;
    lbsWide: Detected := menuWideLB;
    lbsMixed: Detected := menuMixedLB;
    else Detected := nil;
  end;
  if Assigned(Detected) then
    SelectLineBreaks(Detected);

  // Replace consistent linebreaks with CRLF so they're displayed properly
  LB := '';
  case DetectedLineBreaks of
    lbsUnix: LB := LB_UNIX;
    lbsMac: LB := LB_MAC;
    lbsWide: LB := LB_WIDE;
  end;
  if LB <> '' then
    text := StringReplace(text, LB, CRLF, [rfReplaceAll]);

  if ScanNulChar(text) then begin
    MessageDialog(_(SContainsNulCharGrid), mtInformation, [mbOK]);
    text := RemoveNulChars(text);
  end;

  // TODO: Find out why the Delphi IDE insists hinting that this
  //       property is ANSI when it is in fact a WideString.
  memoText.Text := text;

  memoText.SelectAll;
  Modified := False;
end;


procedure TfrmTextEditor.SelectLinebreaks(Sender: TObject);
var
  Selected, Detected: TMenuItem;
begin
  Selected := Sender as TMenuItem;
  case DetectedLineBreaks of
    lbsWindows, lbsNone: Detected := menuWindowsLB;
    lbsUnix: Detected := menuUnixLB;
    lbsMac: Detected := menuMacLB;
    lbsWide: Detected := menuWideLB;
    lbsMixed: Detected := menuMixedLB;
    else Detected := nil;
  end;
  menuWindowsLB.Caption := _('Windows linebreaks');
  menuUnixLB.Caption := _('UNIX linebreaks');
  menuMacLB.Caption := _('Mac OS linebreaks');
  menuWideLB.Caption := _('Unicode linebreaks');
  menuMixedLB.Caption := _('Mixed linebreaks');
  Detected.Caption := Detected.Caption + ' (detected)';
  Selected.Default := True;
  btnLineBreaks.Hint := Selected.Caption;
  btnLineBreaks.ImageIndex := Selected.ImageIndex;
  if Selected = menuWindowsLB then SelectedLineBreaks := lbsWindows
  else if Selected = menuUnixLB then SelectedLineBreaks := lbsUnix
  else if Selected = menuMacLB then SelectedLineBreaks := lbsMac
  else if Selected = menuWideLB then SelectedLineBreaks := lbsWide
  else if Selected = menuMixedLB then SelectedLineBreaks := lbsMixed;
  Modified := True;
end;


procedure TfrmTextEditor.SetMaxLength(len: integer);
begin
  // Input: Length in number of bytes.
  memoText.MaxLength := len;
end;

procedure TfrmTextEditor.SetFont(font: TFont);
begin
  memoText.Font.Name := font.Name;
  memoText.Font.Size := font.Size;
end;

procedure TfrmTextEditor.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
  // Use same text properties as in query/find/replace actions
  actSearchFind.Caption := MainForm.actQueryFind.Caption;
  actSearchFind.Hint := MainForm.actQueryFind.Hint;
  actSearchFindNext.Caption := MainForm.actQueryFindAgain.Caption;
  actSearchFindNext.Hint := MainForm.actQueryFindAgain.Hint;
  actSearchReplace.Caption := MainForm.actQueryReplace.Caption;
  actSearchReplace.Hint := MainForm.actQueryReplace.Hint;
  TranslateComponent(Self);
  // Work around broken dropdown toolbutton after translation:
  // https://sourceforge.net/tracker/index.php?func=detail&aid=902470&group_id=74086&atid=539908
  btnLinebreaks.Style := tbsButton;
  btnLinebreaks.Style := tbsDropDown;
end;


procedure TfrmTextEditor.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asMemoEditorWidth, Width);
  AppSettings.WriteInt(asMemoEditorHeight, Height);
  AppSettings.WriteBool(asMemoEditorWrap, btnWrap.Down);
end;


procedure TfrmTextEditor.FormShow(Sender: TObject);
begin
  // Restore form dimensions
  Width := AppSettings.ReadInt(asMemoEditorWidth);
  Height := AppSettings.ReadInt(asMemoEditorHeight);
  if AppSettings.ReadBool(asMemoEditorWrap) then
    btnWrap.Click;
  // Fix label position:
  lblTextLength.Top := tlbStandard.Top + (tlbStandard.Height-lblTextLength.Height) div 2;
  SetWindowSizeGrip(Handle, True);
  memoText.SetFocus;
end;


procedure TfrmTextEditor.memoTextKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  case Key of
    // Cancel by Escape
    VK_ESCAPE: btnCancelClick(Sender);
    // Apply changes and end editing by Ctrl + Enter
    VK_RETURN: if ssCtrl in Shift then btnApplyClick(Sender);
    Ord('a'), Ord('A'): if (ssCtrl in Shift) and (not (ssAlt in Shift)) then Mainform.actSelectAllExecute(Sender);
  end;
end;

procedure TfrmTextEditor.btnWrapClick(Sender: TObject);
var
  WasModified: Boolean;
begin
  Screen.Cursor := crHourglass;
  // Changing the scrollbars invoke the OnChange event. We avoid thinking the text was really modified.
  WasModified := Modified;
  if memoText.ScrollBars = ssBoth then
    memoText.ScrollBars := ssVertical
  else
    memoText.ScrollBars := ssBoth;
  TToolbutton(Sender).Down := memoText.ScrollBars = ssVertical;
  Modified := WasModified;
  Screen.Cursor := crDefault;
end;


procedure TfrmTextEditor.btnLoadTextClick(Sender: TObject);
var
  d: TOpenTextFileDialog;
begin
  d := TOpenTextFileDialog.Create(Self);
  d.Filter := _('Text files')+' (*.txt)|*.txt|'+_('All files')+' (*.*)|*.*';
  d.FilterIndex := 0;
  d.Encodings.Assign(MainForm.FileEncodings);
  d.EncodingIndex := 0;
  if d.Execute then try
    Screen.Cursor := crHourglass;
    memoText.Text := ReadTextFile(d.FileName, MainForm.GetEncodingByName(d.Encodings[d.EncodingIndex]));
    if (memoText.MaxLength > 0) and (Length(memoText.Text) > memoText.MaxLength) then
      memoText.Text := copy(memoText.Text, 0, memoText.MaxLength);
  finally
    Screen.Cursor := crDefault;
  end;
  d.Free;
end;


procedure TfrmTextEditor.btnCancelClick(Sender: TObject);
begin
  if FStopping then
    Exit;
  FStopping := True;
  TCustomVirtualStringTree(Owner).CancelEditNode;
end;


procedure TfrmTextEditor.FormClose(Sender: TObject; var Action: TCloseAction);
var
  DoPost: Boolean;
begin
  if FStopping then
    Exit;
  FStopping := True;
  if Modified then
    DoPost := MessageDialog(_('Apply modifications?'), mtConfirmation, [mbYes, mbNo]) = mrYes
  else
    DoPost := False;
  if DoPost then
    TCustomVirtualStringTree(Owner).EndEditNode
  else
    TCustomVirtualStringTree(Owner).CancelEditNode;
end;


procedure TfrmTextEditor.btnApplyClick(Sender: TObject);
begin
  FStopping := True;
  TCustomVirtualStringTree(Owner).EndEditNode;
end;


procedure TfrmTextEditor.memoTextChange(Sender: TObject);
begin
  lblTextLength.Caption := FormatNumber(Length(memoText.Text)) + ' characters.';
  if memoText.MaxLength > 0 then
    lblTextLength.Caption := lblTextLength.Caption + ' (Max: '+FormatNumber(memoText.MaxLength)+')';
  Modified := True;
end;


procedure TfrmTextEditor.SetModified(NewVal: Boolean);
begin
  if FModified <> NewVal then begin
    FModified := NewVal;
    btnApply.Enabled := FModified;
  end;
end;


end.
