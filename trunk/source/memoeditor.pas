unit memoeditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, Registry, VirtualTrees;

{$I const.inc}

type
  TfrmMemoEditor = class(TForm)
    memoText: TTntMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TopLeft: TPoint; // Used to position this form on the top left corner of the edited cell
  end;


  // The editor link, instanciated by VirtualTree.CreateEditor
  TMemoEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FForm: TfrmMemoEditor;
    FTree: TCustomVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;             // The node to be edited.
    FColumn: TColumnIndex;           // The column of the node.
    FTextBounds: TRect;              // Smallest rectangle around the text.
    FStopping: Boolean;              // Set to True when the edit link requests stopping the edit action.
  public
    FieldType: Integer;
    MaxInputLength: Integer;
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
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
  ClientHeight := Height;
  SetWindowSizeGrip(Handle, True);
  memoText.SelectAll;
  memoText.SetFocus;
end;


constructor TMemoEditorLink.Create;
begin
  inherited;
end;

destructor TMemoEditorLink.Destroy;
begin
  inherited;
  FForm.Free;
end;


function TMemoEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
// Retrieves the true text bounds from the owner tree.
var
  Text: WideString;
  F: TFont;
begin
  Result := Tree is TCustomVirtualStringTree;
  if not Result then
    exit;

  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // Initial size, font and text of the node.
  F := TFont.Create;
  FTree.GetTextInfo(Node, Column, F, FTextBounds, Text);

  // Create the editor form
  FForm := TfrmMemoEditor.Create(Ftree);
  FForm.Parent := Tree;
  FForm.memoText.Font := F;
  FForm.memoText.Text := Text;
end;


function TMemoEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
    FForm.Show;
end;


function TMemoEditorLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    FForm.Hide;
    FTree.CancelEditNode;
  end;
end;


function TMemoEditorLink.EndEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then try
    FStopping := True;
    if FForm.memoText.Text <> FTree.Text[FNode, FColumn] then
      FTree.Text[FNode, FColumn] := FForm.memoText.Text;
    FForm.Hide;
  except
    FStopping := False;
    raise;
  end;
end;


function TMemoEditorLink.GetBounds: TRect; stdcall;
begin
  Result := FForm.BoundsRect;
end;


procedure TMemoEditorLink.ProcessMessage(var Message: TMessage); stdcall;
begin
end;


procedure TMemoEditorLink.SetBounds(R: TRect); stdcall;
begin
  // Sets the top left corner of the edit control
  if not FStopping then
    FForm.TopLeft := R.TopLeft;
end;


end.
