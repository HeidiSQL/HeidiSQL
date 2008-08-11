unit memoeditor;

interface

uses
  Windows, Classes, Graphics, Controls, Forms, StdCtrls, TntStdCtrls, Registry, VirtualTrees;

{$I const.inc}

type
  TfrmMemoEditor = class(TForm)
    memoText: TTntMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    TopLeft: TPoint; // Used to position this form on the top left corner of the edited cell
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
  // Set top left corner to match the edited cell
  Left := TopLeft.X;
  Top := TopLeft.Y;
  // Hide window caption
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong( Handle, GWL_STYLE ) and not WS_CAPTION );
  SetWindowSizeGrip(Handle, True);
  memoText.SelectAll;
  memoText.SetFocus;
end;


procedure TfrmMemoEditor.memoTextKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
var
  Tree: TCustomVirtualStringTree;
begin
  Tree := TCustomVirtualStringTree(Parent);
  case Key of
    // Cancel by Escape
    VK_ESCAPE: Tree.CancelEditNode;
    // Apply changes and end editing by Ctrl + Enter
    VK_RETURN: if ssCtrl in Shift then Tree.EndEditNode;
  end;
end;

end.
