unit texteditor;

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, VirtualTrees,
  ComCtrls, ToolWin, Dialogs, SysUtils, Menus, ExtDlgs,
  apphelpers, gnugettext, ActnList, StdActns, extra_controls, System.Actions,
  Vcl.ExtCtrls;

{$I const.inc}

type
  TfrmTextEditor = class(TFormWithSizeGrip)
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
    FDetectedLineBreaks,
    FSelectedLineBreaks: TLineBreaks;
    FmemoText: TLineNormalizingMemo;
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
  Result := FmemoText.Text;
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
  Detected:= NIL; // Fix possible unitialized and "if Assigned()" going fail
  FDetectedLineBreaks := ScanLineBreaks(text);
  if FDetectedLineBreaks = lbsNone then
    FDetectedLineBreaks := TLineBreaks(AppSettings.ReadInt(asLineBreakStyle));
  for Item in popupLinebreaks.Items do begin
    if Item.Tag = Integer(FDetectedLineBreaks) then begin
      Detected := Item;
    end;
  end;
  if Assigned(Detected) then
    SelectLineBreaks(Detected);
  FmemoText.Text := text;
  FmemoText.SelectAll;
  // Trigger change event, which is not fired when text is empty. See #132.
  FmemoText.OnChange(FmemoText);
  Modified := False;
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
  FmemoText.MaxLength := len;
end;

procedure TfrmTextEditor.SetFont(font: TFont);
begin
  FmemoText.Font.Name := font.Name;
  FmemoText.Font.Size := font.Size;
end;

procedure TfrmTextEditor.FormCreate(Sender: TObject);
begin
  FmemoText := TLineNormalizingMemo.Create(Self);
  FmemoText.Parent := Self;
  FmemoText.Align := alClient;
  FmemoText.ScrollBars := ssBoth;
  FmemoText.WantTabs := True;
  FmemoText.OnChange := memoTextChange;
  FmemoText.OnKeyDown := memoTextKeyDown;
  InheritFont(Font);
  // Use same text properties as in query/find/replace actions
  actSearchFind.Caption := MainForm.actQueryFind.Caption;
  actSearchFind.Hint := MainForm.actQueryFind.Hint;
  actSearchFindNext.Caption := MainForm.actQueryFindAgain.Caption;
  actSearchFindNext.Hint := MainForm.actQueryFindAgain.Hint;
  actSearchReplace.Caption := MainForm.actQueryReplace.Caption;
  actSearchReplace.Hint := MainForm.actQueryReplace.Hint;
  TranslateComponent(Self);
  FixDropDownButtons(Self);
  // Assign linebreak values to their menu item tags, to write less code later
  menuWindowsLB.Tag := Integer(lbsWindows);
  menuUnixLB.Tag := Integer(lbsUnix);
  menuMacLB.Tag := Integer(lbsMac);
  menuWideLB.Tag := Integer(lbsWide);
  menuMixedLB.Tag := Integer(lbsMixed);
  // Restore form dimensions
  Width := AppSettings.ReadInt(asMemoEditorWidth);
  Height := AppSettings.ReadInt(asMemoEditorHeight);
  if AppSettings.ReadBool(asMemoEditorMaximized) then
    WindowState := wsMaximized;
  if AppSettings.ReadBool(asMemoEditorWrap) then
    btnWrap.Click;
  // Fix label position:
  lblTextLength.Top := tlbStandard.Top + (tlbStandard.Height-lblTextLength.Height) div 2;
end;


procedure TfrmTextEditor.FormDestroy(Sender: TObject);
begin
  if WindowState <> wsMaximized then begin
    AppSettings.WriteInt(asMemoEditorWidth, Width);
    AppSettings.WriteInt(asMemoEditorHeight, Height);
  end;
  AppSettings.WriteBool(asMemoEditorMaximized, WindowState=wsMaximized);
  AppSettings.WriteBool(asMemoEditorWrap, btnWrap.Down);
end;


procedure TfrmTextEditor.FormShow(Sender: TObject);
begin
  FmemoText.SetFocus;
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
  if FmemoText.ScrollBars = ssBoth then
    FmemoText.ScrollBars := ssVertical
  else
    FmemoText.ScrollBars := ssBoth;
  TToolbutton(Sender).Down := FmemoText.ScrollBars = ssVertical;
  Modified := WasModified;
  Screen.Cursor := crDefault;
end;


procedure TfrmTextEditor.btnLoadTextClick(Sender: TObject);
var
  d: TOpenTextFileDialog;
begin
  AppSettings.ResetPath;
  d := TOpenTextFileDialog.Create(Self);
  d.Filter := _('Text files')+' (*.txt)|*.txt|'+_('All files')+' (*.*)|*.*';
  d.FilterIndex := 0;
  d.Encodings.Assign(MainForm.FileEncodings);
  d.EncodingIndex := AppSettings.ReadInt(asFileDialogEncoding, Self.Name);
  if d.Execute then try
    Screen.Cursor := crHourglass;
    FmemoText.Text := ReadTextFile(d.FileName, MainForm.GetEncodingByName(d.Encodings[d.EncodingIndex]));
    if (FmemoText.MaxLength > 0) and (Length(FmemoText.Text) > FmemoText.MaxLength) then
      FmemoText.Text := copy(FmemoText.Text, 0, FmemoText.MaxLength);
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


procedure TfrmTextEditor.memoTextChange(Sender: TObject);
begin
  lblTextLength.Caption := FormatNumber(Length(FmemoText.Text)) + ' characters.';
  if FmemoText.MaxLength > 0 then
    lblTextLength.Caption := lblTextLength.Caption + ' (Max: '+FormatNumber(FmemoText.MaxLength)+')';
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
