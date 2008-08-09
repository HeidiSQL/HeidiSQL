unit grideditlinks;

// The editor links, instanciated by VirtualTree.CreateEditor

interface

uses Windows, Graphics, messages, VirtualTrees, memoeditor;

type
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
    FTree.SetFocus;
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
    FTree.SetFocus;
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
