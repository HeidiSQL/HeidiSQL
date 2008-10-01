unit bineditor;

interface

uses
  Windows, Classes, Graphics, Forms, Controls, helpers, StdCtrls, TntStdCtrls, Registry, VirtualTrees,
  ComCtrls, ToolWin, Dialogs, SysUtils;

{$I const.inc}

type
  TfrmBinEditor = class(TMemoEditor)
    memoText: TTntMemo;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    FModified: Boolean;
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


function TfrmBinEditor.GetText: WideString;
begin
  Result := '0x' + memoText.Text;
end;

procedure TfrmBinEditor.SetText(text: WideString);
begin
  // Skip '0x'.
  memoText.Text := Copy(text, 3);
end;

procedure TfrmBinEditor.SetMaxLength(len: integer);
begin
  // Input: Length in bytes.
  memoText.MaxLength := len * 2;
end;

procedure TfrmBinEditor.SetFont(font: TFont);
begin
  memoText.Font := font;
end;

procedure TfrmBinEditor.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
end;


procedure TfrmBinEditor.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, False) then begin
    reg.WriteInteger( REGNAME_EDITOR_WIDTH, Width );
    reg.WriteInteger( REGNAME_EDITOR_HEIGHT, Height );
    reg.CloseKey;
  end;
  reg.Free;
end;


procedure TfrmBinEditor.FormShow(Sender: TObject);
begin
  // Restore form dimensions
  Width := Mainform.GetRegValue(REGNAME_EDITOR_WIDTH, DEFAULT_EDITOR_WIDTH);
  Height := Mainform.GetRegValue(REGNAME_EDITOR_HEIGHT, DEFAULT_EDITOR_HEIGHT);
  // Fix label position:
  lblTextLength.Top := tlbStandard.Top + (tlbStandard.Height-lblTextLength.Height) div 2;
  SetWindowSizeGrip(Handle, True);
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
  d.Filter := 'All binary files (*.*)|*.*';
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
var
  DoPost: Boolean;
begin
  if Modified then
    DoPost := MessageDlg('Apply modifications?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
  else
    DoPost := False;
  if DoPost then
    TCustomVirtualStringTree(Owner).EndEditNode
  else
    TCustomVirtualStringTree(Owner).CancelEditNode;
end;


procedure TfrmBinEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  btnCancelClick(Sender);
  CanClose := False; // Done by editor link
end;


procedure TfrmBinEditor.FormDeactivate(Sender: TObject);
begin
  // Fixes an AV when another control steels focus, reported in bug #774
  btnCancelClick(Sender);
end;


procedure TfrmBinEditor.btnApplyClick(Sender: TObject);
begin
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
