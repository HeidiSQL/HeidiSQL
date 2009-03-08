unit texteditor;

interface

uses
  Windows, Classes, Graphics, Forms, Controls, helpers, StdCtrls, TntStdCtrls, VirtualTrees,
  ComCtrls, ToolWin, Dialogs, SysUtils, Menus, WideStrUtils;

{$I const.inc}

type
  TfrmTextEditor = class(TMemoEditor)
    memoText: TTntMemo;
    tlbStandard: TToolBar;
    btnWrap: TToolButton;
    btnLoadText: TToolButton;
    btnApply: TToolButton;
    btnCancel: TToolButton;
    lblTextLength: TLabel;
    btnLinefeedStyle: TToolButton;
    popupLinefeedStyle: TPopupMenu;
    menuWindowsLF: TMenuItem;
    menuUnixLF: TMenuItem;
    menuMacLF: TMenuItem;
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
    procedure SelectLinefeedStyle(Sender: TObject);
  private
    { Private declarations }
    FModified: Boolean;
    FStopping: Boolean;
    DetectedLineFeedStyle: Integer;
    SelectedLineFeedStyle: Integer;
    procedure SetModified(NewVal: Boolean);
    property Modified: Boolean read FModified write SetModified;
  public
    function GetText: WideString; override;
    procedure SetText(text: WideString); override;
    procedure SetMaxLength(len: integer); override;
    procedure SetFont(font: TFont); override;
  end;


implementation

uses main;

{$R *.dfm}

const
  NL_WIN = 0;
  NL_UNX = 1;
  NL_MAC = 2;


function TfrmTextEditor.GetText: WideString;
begin
  Result := memoText.Text;
  // Convert linefeed style back to selected
  if SelectedLineFeedStyle = NL_UNX then
    Result := WideStringReplace(Result, CRLF, #10, [rfReplaceAll])
  else if SelectedLineFeedStyle = NL_MAC then
    Result := WideStringReplace(Result, CRLF, #10#13, [rfReplaceAll])
end;


procedure TfrmTextEditor.SetText(text: WideString);
begin
  // Automatic detection of linefeed style
  if Pos(CRLF, text) > 0 then DetectedLineFeedStyle := NL_WIN
  else if Pos(#10#13, text) > 0 then DetectedLineFeedStyle := NL_MAC
  else if Pos(#10, text) > 0 then DetectedLineFeedStyle := NL_UNX
  else DetectedLineFeedStyle := NL_WIN;
  SelectLinefeedStyle(popupLineFeedStyle.Items[DetectedLineFeedStyle]);
  // Unify linefeeds so they're displayed properly 
  text := WideStringReplace(text, #13, '', [rfReplaceAll]);
  text := WideStringReplace(text, #10, CRLF, [rfReplaceAll]);
  // TODO: Find out why the Delphi IDE insists hinting that this
  //       property is ANSI when it is in fact a WideString.
  memoText.Text := text;
end;


procedure TfrmTextEditor.SelectLinefeedStyle(Sender: TObject);
var
  Selected, Detected: TMenuItem;
  Menu: TMenu;
begin
  Selected := Sender as TMenuItem;
  Menu := Selected.GetParentMenu;
  Detected := Menu.Items[DetectedLineFeedStyle];
  Menu.Items[NL_WIN].Caption := 'Windows® linefeeds';
  Menu.Items[NL_UNX].Caption := 'UNIX linefeeds';
  Menu.Items[NL_MAC].Caption := 'Mac OS linefeeds';
  Detected.Caption := Detected.Caption + ' (detected)';
  Selected.Default := True;
  btnLineFeedStyle.Hint := Selected.Caption;
  btnLineFeedStyle.ImageIndex := Selected.ImageIndex;
  SelectedLineFeedStyle := Selected.MenuIndex;
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
end;


procedure TfrmTextEditor.FormDestroy(Sender: TObject);
begin
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_EDITOR_WIDTH, Width );
  MainReg.WriteInteger( REGNAME_EDITOR_HEIGHT, Height );
end;


procedure TfrmTextEditor.FormShow(Sender: TObject);
begin
  // Restore form dimensions
  Width := GetRegValue(REGNAME_EDITOR_WIDTH, DEFAULT_EDITOR_WIDTH);
  Height := GetRegValue(REGNAME_EDITOR_HEIGHT, DEFAULT_EDITOR_HEIGHT);
  // Fix label position:
  lblTextLength.Top := tlbStandard.Top + (tlbStandard.Height-lblTextLength.Height) div 2;
  SetWindowSizeGrip(Handle, True);
  memoText.SelectAll;
  memoText.SetFocus;
  memoTextChange(Sender);
  Modified := False;
end;


procedure TfrmTextEditor.memoTextKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  case Key of
    // Cancel by Escape
    VK_ESCAPE: btnCancelClick(Sender);
    // Apply changes and end editing by Ctrl + Enter
    VK_RETURN: if ssCtrl in Shift then btnApplyClick(Sender);
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
  d: TOpenDialog;
begin
  d := TOpenDialog.Create(Self);
  d.Filter := 'Textfiles (*.txt)|*.txt|All files (*.*)|*.*';
  d.FilterIndex := 0;
  if d.Execute then try
    Screen.Cursor := crHourglass;
    memoText.Text := ReadTextFile(d.FileName);
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
    DoPost := MessageDlg('Apply modifications?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
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
