unit memoeditor;

interface

uses
  Windows, Classes, Graphics, Controls, Forms, StdCtrls, TntStdCtrls, Registry, VirtualTrees,
  ComCtrls, ToolWin, Dialogs;

{$I const.inc}

type
  TfrmMemoEditor = class(TForm)
    memoText: TTntMemo;
    tlbStandard: TToolBar;
    btnWrap: TToolButton;
    btnLoadText: TToolButton;
    btnApply: TToolButton;
    btnCancel: TToolButton;
    lblTextLength: TLabel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadTextClick(Sender: TObject);
    procedure btnWrapClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memoTextChange(Sender: TObject);
    procedure memoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FModified: Boolean;
  public
    { Public declarations }
  end;


implementation

uses main, helpers;

{$R *.dfm}


procedure TfrmMemoEditor.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, False) then begin
    reg.WriteInteger( REGNAME_MEMOEDITOR_WIDTH, Width );
    reg.WriteInteger( REGNAME_MEMOEDITOR_HEIGHT, Height );
    reg.CloseKey;
  end;
  reg.Free;
end;


procedure TfrmMemoEditor.FormShow(Sender: TObject);
begin
  // Restore form dimensions
  Width := Mainform.GetRegValue(REGNAME_MEMOEDITOR_WIDTH, DEFAULT_MEMOEDITOR_WIDTH);
  Height := Mainform.GetRegValue(REGNAME_MEMOEDITOR_HEIGHT, DEFAULT_MEMOEDITOR_HEIGHT);
  // Fix label position:
  lblTextLength.Top := tlbStandard.Top + (tlbStandard.Height-lblTextLength.Height) div 2;
  SetWindowSizeGrip(Handle, True);
  memoText.SelectAll;
  memoText.SetFocus;
  memoTextChange(Sender);
  FModified := False;
end;


procedure TfrmMemoEditor.memoTextKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  case Key of
    // Cancel by Escape
    VK_ESCAPE: btnCancelClick(Sender);
    // Apply changes and end editing by Ctrl + Enter
    VK_RETURN: if ssCtrl in Shift then btnApplyClick(Sender);
  end;
end;

procedure TfrmMemoEditor.btnWrapClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  if memoText.ScrollBars = ssBoth then
    memoText.ScrollBars := ssVertical
  else
    memoText.ScrollBars := ssBoth;
  TToolbutton(Sender).Down := memoText.ScrollBars = ssVertical;
  Screen.Cursor := crDefault;
end;


procedure TfrmMemoEditor.btnLoadTextClick(Sender: TObject);
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


procedure TfrmMemoEditor.btnCancelClick(Sender: TObject);
var
  DoPost: Boolean;
begin
  if FModified then
    DoPost := MessageDlg('Apply modifications?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
  else
    DoPost := False;
  if DoPost then
    TCustomVirtualStringTree(Owner).EndEditNode
  else
    TCustomVirtualStringTree(Owner).CancelEditNode;
end;


procedure TfrmMemoEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  btnCancelClick(Sender);
  CanClose := False; // Done by editor link
end;


procedure TfrmMemoEditor.btnApplyClick(Sender: TObject);
begin
  TCustomVirtualStringTree(Owner).EndEditNode;
end;


procedure TfrmMemoEditor.memoTextChange(Sender: TObject);
begin
  lblTextLength.Caption := FormatNumber(Length(memoText.Text)) + ' characters.';
  if memoText.MaxLength > 0 then
    lblTextLength.Caption := lblTextLength.Caption + ' (Max: '+FormatNumber(memoText.MaxLength)+')';
  FModified := True;
end;

end.
