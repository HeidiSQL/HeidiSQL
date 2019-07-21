unit bineditor;

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, VirtualTrees,
  ComCtrls, ToolWin, Dialogs, SysUtils, gnugettext, extra_controls;

{$I const.inc}

type
  TfrmBinEditor = class(TExtForm)
    memoText: TMemo;
    tlbStandard: TToolBar;
    btnWrap: TToolButton;
    btnLoadBinary: TToolButton;
    btnApply: TToolButton;
    btnCancel: TToolButton;
    lblTextLength: TLabel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadBinaryClick(Sender: TObject);
    procedure btnWrapClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memoTextChange(Sender: TObject);
    procedure memoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FModified: Boolean;
    FStopping: Boolean;
    procedure SetModified(NewVal: Boolean);
  public
    function GetText: String;
    procedure SetText(text: String);
    procedure SetTitleText(Title: String);
    procedure SetMaxLength(len: integer);
    procedure SetFont(font: TFont);
    property Modified: Boolean read FModified write SetModified;
  end;


implementation

uses apphelpers, main;


{$R *.dfm}


function TfrmBinEditor.GetText: String;
var
  Ansi: AnsiString;
begin
  // Convert hex to binary string before returning
  SetLength(Ansi, memoText.GetTextLen div 2);
  HexToBin(PWideChar(memoText.Text), PAnsiChar(Ansi), Length(Ansi));
  Result := String(Ansi);
end;

procedure TfrmBinEditor.SetText(text: String);
begin
  // Skip '0x'.
  memoText.Text := Copy(text, 3);
end;


procedure TfrmBinEditor.SetTitleText(Title: String);
begin
  // Add column name to window title bar
  if Title <> '' then
    Caption := Title + ' - ' + Caption;
end;


procedure TfrmBinEditor.SetMaxLength(len: integer);
begin
  // Input: Length in bytes.
  memoText.MaxLength := len * 2;
end;

procedure TfrmBinEditor.SetFont(font: TFont);
begin
  memoText.Font.Name := font.Name;
  memoText.Font.Size := font.Size;
end;

procedure TfrmBinEditor.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
end;


procedure TfrmBinEditor.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asMemoEditorWidth, Width);
  AppSettings.WriteInt(asMemoEditorHeight, Height);
  AppSettings.WriteBool(asMemoEditorWrap, btnWrap.Down);
end;


procedure TfrmBinEditor.FormShow(Sender: TObject);
begin
  // Restore form dimensions
  Width := AppSettings.ReadInt(asMemoEditorWidth);
  Height := AppSettings.ReadInt(asMemoEditorHeight);
  if AppSettings.ReadBool(asMemoEditorWrap) then
    btnWrap.Click;
  // Fix label position:
  lblTextLength.Top := tlbStandard.Top + (tlbStandard.Height-lblTextLength.Height) div 2;
  memoText.SelectAll;
  memoText.SetFocus;
  memoTextChange(Sender);
  Modified := False;
end;


procedure TfrmBinEditor.memoTextKeyDown(Sender: TObject; var Key: Word; Shift:
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

procedure TfrmBinEditor.btnWrapClick(Sender: TObject);
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


procedure TfrmBinEditor.btnLoadBinaryClick(Sender: TObject);
var
  d: TOpenDialog;
begin
  d := TOpenDialog.Create(Self);
  d.Filter := _('All files')+' (*.*)|*.*';
  d.FilterIndex := 0;
  if d.Execute then try
    Screen.Cursor := crHourglass;
    memoText.Text := BinToWideHex(ReadBinaryFile(d.FileName, memoText.MaxLength));
  finally
    Screen.Cursor := crDefault;
  end;
  d.Free;
end;


procedure TfrmBinEditor.btnCancelClick(Sender: TObject);
begin
  if FStopping then
    Exit;
  FStopping := True;
  TCustomVirtualStringTree(Owner).CancelEditNode;
end;


procedure TfrmBinEditor.FormClose(Sender: TObject; var Action: TCloseAction);
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


procedure TfrmBinEditor.btnApplyClick(Sender: TObject);
begin
  FStopping := True;
  TCustomVirtualStringTree(Owner).EndEditNode;
end;


procedure TfrmBinEditor.memoTextChange(Sender: TObject);
begin
  lblTextLength.Caption := FormatNumber(Length(memoText.Text) / 2) + ' bytes.';
  if memoText.MaxLength > 0 then
    lblTextLength.Caption := lblTextLength.Caption + ' (Max: '+FormatNumber(memoText.MaxLength / 2)+')';
  Modified := True;
end;


procedure TfrmBinEditor.SetModified(NewVal: Boolean);
begin
  if FModified <> NewVal then begin
    FModified := NewVal;
    btnApply.Enabled := FModified;
  end;
end;


end.
