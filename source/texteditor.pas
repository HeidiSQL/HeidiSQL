unit texteditor;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, VirtualTrees,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.Dialogs, System.SysUtils, Vcl.Menus, Vcl.ExtDlgs,
  apphelpers, gnugettext, Vcl.ActnList, Vcl.StdActns, extra_controls, System.Actions,
  Vcl.ExtCtrls, dbconnection, SynEdit, SynMemo, SynEditHighlighter, customize_highlighter,
  System.JSON, Rest.Json, Xml.VerySimple, reformatter,

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
    ToolButton1: TToolButton;
    btnCustomizeHighlighter: TToolButton;
    popupHighlighter: TPopupMenu;
    menuCustomizeHighlighter: TMenuItem;
    menuFormatCodeOnce: TMenuItem;
    menuAlwaysFormatCode: TMenuItem;
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
    procedure btnCustomizeHighlighterClick(Sender: TObject);
    procedure menuFormatCodeOnceClick(Sender: TObject);
    procedure menuAlwaysFormatCodeClick(Sender: TObject);
  private
    { Private declarations }
    FModified: Boolean;
    FClosingByApplyButton: Boolean;
    FClosingByCancelButton: Boolean;
    FDetectedLineBreaks,
    FSelectedLineBreaks: TLineBreaks;
    FMaxLength: Integer;
    FTableColumn: TTableColumn;
    FHighlighter: TSynCustomHighlighter;
    FHighlighterFormatters: TStringList;
    procedure SetModified(NewVal: Boolean);
    procedure CustomizeHighlighterChanged(Sender: TObject);
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
  LB := GetLineBreak(FSelectedLineBreaks);
  if LB <> CRLF then
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
  if (Length(text) > SIZE_MB) then begin
    MainForm.LogSQL(_('Auto-disabling wordwrap for large text'));
    btnWrap.Enabled := False;
  end else begin
    btnWrap.Enabled := True;
  end;

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


procedure TfrmTextEditor.btnCustomizeHighlighterClick(Sender: TObject);
var
  Dialog: TfrmCustomizeHighlighter;
begin
  // let user customize highlighter colors
  Dialog := TfrmCustomizeHighlighter.Create(Self);
  Dialog.FriendlyLanguageName := MemoText.Highlighter.FriendlyLanguageName;
  Dialog.OnChange := CustomizeHighlighterChanged;
  Dialog.ShowModal;
  Dialog.Free;
end;

procedure TfrmTextEditor.CustomizeHighlighterChanged(Sender: TObject);
var
  Dialog: TfrmCustomizeHighlighter;
begin
  Dialog := Sender as TfrmCustomizeHighlighter;
  comboHighlighter.ItemIndex := comboHighlighter.Items.IndexOf(Dialog.FriendlyLanguageName);
  comboHighlighter.OnSelect(comboHighlighter);
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
  FClosingByApplyButton := False;
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

  // Define highlighters for which we have a reformatter
  FHighlighterFormatters := TStringList.Create;
  FHighlighterFormatters.Add(TSynJSONSyn.ClassName);
  FHighlighterFormatters.Add(TSynSQLSyn.ClassName);
  FHighlighterFormatters.Add(TSynXMLSyn.ClassName);

  MemoText.OnMouseWheel := MainForm.AnySynMemoMouseWheel;
  MemoText.OnPaintTransient := MainForm.SynMemoQuery.OnPaintTransient;
  if AppSettings.ReadBool(asMemoEditorMaximized) then
    WindowState := wsMaximized;
end;


procedure TfrmTextEditor.FormDestroy(Sender: TObject);
begin
  if WindowState <> wsMaximized then begin
    AppSettings.WriteIntDpiAware(asMemoEditorWidth, Self, Width);
    AppSettings.WriteIntDpiAware(asMemoEditorHeight, Self, Height);
  end;
  AppSettings.WriteBool(asMemoEditorMaximized, WindowState=wsMaximized);
  if btnWrap.Enabled then begin
    AppSettings.WriteBool(asMemoEditorWrap, btnWrap.Down);
  end;
  if Assigned(FTableColumn) and (comboHighlighter.Text <> AppSettings.GetDefaultString(asMemoEditorHighlighter)) then begin
    AppSettings.SessionPath := MainForm.GetRegKeyTable;
    AppSettings.WriteString(asMemoEditorHighlighter, comboHighlighter.Text, FTableColumn.Name);
  end;
  // Fixes EAccessViolation under 64-bit when using non-default themes
  if Assigned(Panel1) then
    Panel1.Parent := nil;
end;


procedure TfrmTextEditor.FormShow(Sender: TObject);
var
  HighlighterName: String;
begin
  // Restore form dimensions
  if WindowState <> wsMaximized then begin
    Width := AppSettings.ReadIntDpiAware(asMemoEditorWidth, Self);
    Height := AppSettings.ReadIntDpiAware(asMemoEditorHeight, Self);
  end;

  if AppSettings.ReadBool(asMemoEditorWrap) and btnWrap.Enabled then begin
    btnWrap.Click;
  end;
  menuAlwaysFormatCode.Checked := AppSettings.ReadBool(asMemoEditorAlwaysFormatCode);

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
  btnWrap.Down := MemoText.ScrollBars = ssVertical;
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

  menuFormatCodeOnce.Enabled := FHighlighterFormatters.IndexOf(FHighlighter.ClassName) > -1;
  if menuAlwaysFormatCode.Checked and menuFormatCodeOnce.Enabled then begin
    menuFormatCodeOnce.OnClick(Sender);
    SelStart := 0;
    SelLength := 0;
  end;

  // Load custom highlighter settings from ini file, if exists:
  MemoText.Highlighter.LoadFromFile(AppSettings.DirnameHighlighters + MemoText.Highlighter.LanguageName + '.ini');

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
  FClosingByCancelButton := True;
  Close;
end;


procedure TfrmTextEditor.menuAlwaysFormatCodeClick(Sender: TObject);
begin
  // Change setting for "always reformat"
  AppSettings.WriteBool(asMemoEditorAlwaysFormatCode, menuAlwaysFormatCode.Checked);
  if menuAlwaysFormatCode.Checked and menuFormatCodeOnce.Enabled then begin
    menuFormatCodeOnce.OnClick(Sender);
  end;
end;


procedure TfrmTextEditor.menuFormatCodeOnceClick(Sender: TObject);
var
  JsonTmp: TJSONValue;
  Xml: TXmlVerySimple;
  //XmlTmp: IXMLDocument;
begin
  // Reformat code if possible
  try
    if FHighlighter is TSynJSONSyn then begin
      JsonTmp := TJSONObject.ParseJSONValue(MemoText.Text);
      MemoText.Text := JsonTmp.Format;
      JsonTmp.Free;
      MemoText.SelStart := 0;
      MemoText.SelLength := 0;
    end
    else if FHighlighter is TSynSQLSyn then begin
      // Prefer old internal formatter here, so the user does not run into request limits
      frmReformatter := TfrmReformatter.Create(Self);
      MemoText.Text := frmReformatter.FormatSqlInternal(MemoText.Text);
      MemoText.SelStart := 0;
      MemoText.SelLength := 0;
      frmReformatter.Free;
    end
    else if FHighlighter is TSynXMLSyn then begin
      {XmlTmp := TXMLDocument.Create(nil);
      XmlTmp.LoadFromXML(MemoText.Text);
      MemoText.BeginUpdate;
      MemoText.Text := XMLDoc.FormatXMLData(MemoText.Text);
      MemoText.EndUpdate;}
      Xml := TXmlVerySimple.Create;
      //Xml.Options := [doNodeAutoIndent, doParseProcessingInstr, doCaseInsensitive, doWriteBOM, doSimplifyTextNodes];
      Xml.Clear;
      Xml.Text := MemoText.Lines.Text.Trim;
      MemoText.BeginUpdate;
      MemoText.Lines.Text := Xml.Text;
      MemoText.EndUpdate;
      Xml.Free;
      MemoText.SelStart := 0;
      MemoText.SelLength := 0;
    end
    else begin
      MessageBeep(MB_ICONEXCLAMATION);
    end;
  except
    on E:Exception do begin
      MessageBeep(MB_ICONERROR);
      MainForm.LogSQL(f_('Error in code formatting: %s', [E.Message]));
    end;
  end;
end;


procedure TfrmTextEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Modified then begin
    if FClosingByCancelButton then
      ModalResult := mrCancel
    else if FClosingByApplyButton then
      ModalResult := mrYes
    else
      ModalResult := MessageDialog(_('Apply modifications?'), mtConfirmation, [mbYes, mbNo]);
  end
  else
    ModalResult := mrCancel;
end;


procedure TfrmTextEditor.btnApplyClick(Sender: TObject);
begin
  FClosingByApplyButton := True;
  Close;
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
