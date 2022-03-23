unit texteditor;

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, VirtualTrees,
  ComCtrls, ToolWin, Dialogs, SysUtils, Menus, ExtDlgs,
  apphelpers, gnugettext, ActnList, StdActns, extra_controls, System.Actions,
  Vcl.ExtCtrls, dbconnection, SynEdit, SynMemo, SynEditHighlighter,

  SynHighlighterADSP21xx, SynHighlighterAWK, SynHighlighterAsm,
  SynHighlighterBaan, SynHighlighterBat, SynHighlighterCAC, SynHighlighterCPM, SynHighlighterCS,
  SynHighlighterCache, SynHighlighterCobol, SynHighlighterCpp, SynHighlighterCss, SynHighlighterDOT,
  SynHighlighterDWS, SynHighlighterDfm, SynHighlighterDml, SynHighlighterEiffel,
  SynHighlighterFortran, SynHighlighterFoxpro, SynHighlighterGLSL, SynHighlighterGWS,
  SynHighlighterGalaxy, SynHighlighterGeneral, SynHighlighterGo, SynHighlighterHC11,
  SynHighlighterHP48, SynHighlighterHashEntries, SynHighlighterHaskell, SynHighlighterHtml,
  SynHighlighterIDL, SynHighlighterIni, SynHighlighterInno, SynHighlighterJSON, SynHighlighterJScript,
  SynHighlighterJava, SynHighlighterKix, SynHighlighterLDraw, SynHighlighterLLVM, SynHighlighterM3,
  SynHighlighterModelica, SynHighlighterMsg, SynHighlighterPHP, SynHighlighterPas, SynHighlighterPerl,
  SynHighlighterProgress, SynHighlighterPython, SynHighlighterRC, SynHighlighterRexx,
  SynHighlighterRuby, SynHighlighterSDD, SynHighlighterSQL, SynHighlighterST, SynHighlighterSml,
  SynHighlighterTclTk, SynHighlighterTeX, SynHighlighterUNIXShellScript, SynHighlighterURI,
  SynHighlighterUnreal, SynHighlighterVB, SynHighlighterVBScript, SynHighlighterVrml97,
  SynHighlighterWebIDL, SynHighlighterXML, SynHighlighterZPL
  ;

{$I const.inc}

type
  TfrmTextEditor = class(TExtForm)
    Panel1: TPanel;
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
    btnSearchFind: TToolButton;
    btnSearchReplace: TToolButton;
    btnSearchFindNext: TToolButton;
    btnSeparator1: TToolButton;
    TimerMemoChange: TTimer;
    comboHighlighter: TComboBox;
    MemoText: TSynMemo;
    popupEditor: TPopupMenu;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Selectall1: TMenuItem;
    Undo1: TMenuItem;
    Findtext1: TMenuItem;
    Findorreplaceagain1: TMenuItem;
    Replacetext1: TMenuItem;
    N1: TMenuItem;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadTextClick(Sender: TObject);
    procedure btnWrapClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoTextChange(Sender: TObject);
    procedure MemoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SelectLinebreaks(Sender: TObject);
    procedure TimerMemoChangeTimer(Sender: TObject);
    procedure comboHighlighterSelect(Sender: TObject);
  private
    { Private declarations }
    FModified: Boolean;
    FStopping: Boolean;
    FDetectedLineBreaks,
    FSelectedLineBreaks: TLineBreaks;
    FMaxLength: Integer;
    FTableColumn: TTableColumn;
    FHighlighter: TSynCustomHighlighter;
    procedure SetModified(NewVal: Boolean);
  public
    function GetText: String;
    procedure SetText(text: String);
    procedure SetTitleText(Title: String);
    procedure SetMaxLength(len: integer);
    procedure SetFont(font: TFont);
    property Modified: Boolean read FModified write SetModified;
    property TableColumn: TTableColumn read FTableColumn write FTableColumn;
  end;


implementation

uses main;

{$R *.dfm}


function TfrmTextEditor.GetText: String;
var
  LB: String;
begin
  Result := MemoText.Text;
  // Convert linebreaks back to selected
  LB := '';
  case FSelectedLineBreaks of
    lbsUnix: LB := LB_UNIX;
    lbsMac: LB := LB_MAC;
    lbsWide: LB := LB_WIDE;
  end;
  if LB <> '' then
    Result := StringReplace(Result, CRLF, LB, [rfReplaceAll]);
end;


procedure TfrmTextEditor.SetText(text: String);
var
  Detected, Item: TMenuItem;
begin
  // Apply text string, and detect type of line breaks in it
  FDetectedLineBreaks := ScanLineBreaks(text);
  Detected := nil;
  if FDetectedLineBreaks = lbsNone then
    FDetectedLineBreaks := TLineBreaks(AppSettings.ReadInt(asLineBreakStyle));
  for Item in popupLinebreaks.Items do begin
    if Item.Tag = Integer(FDetectedLineBreaks) then begin
      Detected := Item;
    end;
  end;
  if Assigned(Detected) then
    SelectLineBreaks(Detected);
  MemoText.Text := text;
  MemoText.SelectAll;
  Modified := False;
end;


procedure TfrmTextEditor.SetTitleText(Title: String);
begin
  // Add column name to window title bar
  if Title <> '' then
    Caption := Title + ' - ' + Caption;
end;


procedure TfrmTextEditor.TimerMemoChangeTimer(Sender: TObject);
var
  MaxLen, CursorPos: String;
begin
  // Timer based onchange handler, so we don't scan the whole text on every typed character
  TimerMemoChange.Enabled := False;
  if FMaxLength = 0 then
    MaxLen := '?'
  else
    MaxLen := FormatNumber(FMaxLength);
  CursorPos := FormatNumber(MemoText.CaretY) + ':' + FormatNumber(MemoText.CaretX);
  lblTextLength.Caption := f_('%s characters (max: %s), %s lines, cursor at %s', [FormatNumber(MemoText.GetTextLen), MaxLen, FormatNumber(MemoText.Lines.Count), CursorPos]);
  if MemoText.ReadOnly then
    lblTextLength.Caption := lblTextLength.Caption + ', read-only';
end;


procedure TfrmTextEditor.SelectLinebreaks(Sender: TObject);
var
  Selected, Item: TMenuItem;
begin
  Selected := Sender as TMenuItem;
  menuWindowsLB.Caption := _('Windows linebreaks');
  menuUnixLB.Caption := _('UNIX linebreaks');
  menuMacLB.Caption := _('Mac OS linebreaks');
  menuWideLB.Caption := _('Unicode linebreaks');
  menuMixedLB.Caption := _('Mixed linebreaks');
  for Item in popupLinebreaks.Items do begin
    if Item.Tag = Integer(FDetectedLineBreaks) then begin
      Item.Caption := Item.Caption + ' (' + _('detected') + ')';
    end;
  end;

  Selected.Default := True;
  btnLineBreaks.Hint := Selected.Caption;
  btnLineBreaks.ImageIndex := Selected.ImageIndex;
  FSelectedLineBreaks := TLineBreaks(Selected.Tag);
  Modified := True;
end;


procedure TfrmTextEditor.SetMaxLength(len: integer);
begin
  // Input: Length in number of bytes.
  FMaxLength := len;
end;

procedure TfrmTextEditor.SetFont(font: TFont);
begin
  MemoText.Font.Name := font.Name;
  MemoText.Font.Size := font.Size;
end;

procedure TfrmTextEditor.FormCreate(Sender: TObject);
var
  Highlighters: TSynHighlighterList;
  i: Integer;
begin
  HasSizeGrip := True;
  // Assign linebreak values to their menu item tags, to write less code later
  menuWindowsLB.Tag := Integer(lbsWindows);
  menuUnixLB.Tag := Integer(lbsUnix);
  menuMacLB.Tag := Integer(lbsMac);
  menuWideLB.Tag := Integer(lbsWide);
  menuMixedLB.Tag := Integer(lbsMixed);

  Highlighters := SynEditHighlighter.GetPlaceableHighlighters;
  for i:=0 to Highlighters.Count-1 do begin
    comboHighlighter.Items.Add(Highlighters[i].GetFriendlyLanguageName);
  end;

  FTableColumn := nil;

  // Fix label position:
  lblTextLength.Top := tlbStandard.Top + (tlbStandard.Height-lblTextLength.Height) div 2;
end;


procedure TfrmTextEditor.FormDestroy(Sender: TObject);
begin
  if WindowState <> wsMaximized then begin
    AppSettings.WriteIntDpiAware(asMemoEditorWidth, Self, Width);
    AppSettings.WriteIntDpiAware(asMemoEditorHeight, Self, Height);
  end;
  AppSettings.WriteBool(asMemoEditorMaximized, WindowState=wsMaximized);
  AppSettings.WriteBool(asMemoEditorWrap, btnWrap.Down);
  if Assigned(FTableColumn) and (comboHighlighter.Text <> AppSettings.GetDefaultString(asMemoEditorHighlighter)) then begin
    AppSettings.SessionPath := MainForm.GetRegKeyTable;
    AppSettings.WriteString(asMemoEditorHighlighter, comboHighlighter.Text, FTableColumn.Name);
  end;
end;


procedure TfrmTextEditor.FormShow(Sender: TObject);
var
  HighlighterName: String;
begin
  // Restore form dimensions
  Width := AppSettings.ReadIntDpiAware(asMemoEditorWidth, Self);
  Height := AppSettings.ReadIntDpiAware(asMemoEditorHeight, Self);
  if AppSettings.ReadBool(asMemoEditorMaximized) then
    WindowState := wsMaximized;

  if AppSettings.ReadBool(asMemoEditorWrap) then
    btnWrap.Click;

  // Select previously used highlighter
  HighlighterName := AppSettings.GetDefaultString(asMemoEditorHighlighter);
  if Assigned(FTableColumn) then begin
    AppSettings.SessionPath := MainForm.GetRegKeyTable;
    HighlighterName := AppSettings.ReadString(asMemoEditorHighlighter, FTableColumn.Name, HighlighterName);
  end;

  if MemoText.ReadOnly then begin
    MemoText.Color := clBtnFace;
  end;

  comboHighlighter.ItemIndex := comboHighlighter.Items.IndexOf(HighlighterName);
  comboHighlighter.OnSelect(comboHighlighter);
  // Trigger change event, which is not fired when text is empty. See #132.
  TimerMemoChangeTimer(Self);
  MemoText.SetFocus;
end;


procedure TfrmTextEditor.MemoTextKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  TimerMemoChange.Enabled := False;
  TimerMemoChange.Enabled := True;
  case Key of
    // Cancel active dialog by Escape
    VK_ESCAPE: begin
      btnCancelClick(Sender);
    end;
    // Apply changes and end editing by Ctrl + Enter
    VK_RETURN: if ssCtrl in Shift then btnApplyClick(Sender);
    Ord('a'), Ord('A'): if (ssCtrl in Shift) and (not (ssAlt in Shift)) then Mainform.actSelectAllExecute(Sender);
  end;
end;

procedure TfrmTextEditor.MemoTextClick(Sender: TObject);
begin
  TimerMemoChange.Enabled := False;
  TimerMemoChange.Enabled := True;
end;

procedure TfrmTextEditor.btnWrapClick(Sender: TObject);
var
  WasModified: Boolean;
begin
  Screen.Cursor := crHourglass;
  // Changing the scrollbars invoke the OnChange event. We avoid thinking the text was really modified.
  WasModified := Modified;
  if MemoText.ScrollBars = ssBoth then begin
    MemoText.ScrollBars := ssVertical;
    MemoText.WordWrap := True;
  end else begin
    MemoText.ScrollBars := ssBoth;
    MemoText.WordWrap := False;
  end;
  TToolbutton(Sender).Down := MemoText.ScrollBars = ssVertical;
  Modified := WasModified;
  Screen.Cursor := crDefault;
end;


procedure TfrmTextEditor.comboHighlighterSelect(Sender: TObject);
var
  Highlighters: TSynHighlighterList;
  i: Integer;
  SelStart, SelLength: Integer;
begin
  // Code highlighter selected
  SelStart := MemoText.SelStart;
  SelLength := MemoText.SelLength;
  MemoText.Highlighter := nil;
  FHighlighter.Free;
  Highlighters := SynEditHighlighter.GetPlaceableHighlighters;
  for i:=0 to Highlighters.Count-1 do begin
    if comboHighlighter.Text = Highlighters[i].GetFriendlyLanguageName then begin
      FHighlighter := Highlighters[i].Create(Self);
      MemoText.Highlighter := FHighlighter;
      Break;
    end;
  end;
  // In case the combobox is empty:
  if MemoText.Highlighter = nil then begin
    FHighlighter := TSynGeneralSyn.Create(Self);
    MemoText.Highlighter := FHighlighter;
  end;

  MemoText.SelStart := SelStart;
  MemoText.SelLength := SelLength;
end;

procedure TfrmTextEditor.btnLoadTextClick(Sender: TObject);
var
  d: TExtFileOpenDialog;
begin
  AppSettings.ResetPath;
  d := TExtFileOpenDialog.Create(Self);
  d.AddFileType('*.txt', _('Text files'));
  d.AddFileType('*.*', _('All files'));
  d.Encodings.Assign(MainForm.FileEncodings);
  d.EncodingIndex := AppSettings.ReadInt(asFileDialogEncoding, Self.Name);
  if d.Execute then try
    Screen.Cursor := crHourglass;
    MemoText.Text := ReadTextFile(d.FileName, MainForm.GetEncodingByName(d.Encodings[d.EncodingIndex]));
    if (FMaxLength > 0) and (Length(MemoText.Text) > FMaxLength) then
      MemoText.Text := copy(MemoText.Text, 0, FMaxLength);
    AppSettings.WriteInt(asFileDialogEncoding, d.EncodingIndex, Self.Name);
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


procedure TfrmTextEditor.MemoTextChange(Sender: TObject);
begin
  Modified := True;
  TimerMemoChange.Enabled := False;
  TimerMemoChange.Enabled := True;
end;


procedure TfrmTextEditor.SetModified(NewVal: Boolean);
begin
  // Enables or disables "apply" button, and resets SynEdit's modification marker in its gutter
  if FModified <> NewVal then begin
    FModified := NewVal;
    if not FModified then
      MemoText.ResetModificationIndicator;
    btnApply.Enabled := FModified;
  end;
end;


end.
