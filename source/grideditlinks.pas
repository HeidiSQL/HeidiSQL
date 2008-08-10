unit grideditlinks;

// The editor links, instanciated by VirtualTree.CreateEditor

interface

uses Windows, Graphics, messages, VirtualTrees, memoeditor, ComCtrls, SysUtils, Classes,
  mysql_structures;

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
    MaxLength: Integer;
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

  TDateTimeEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FPicker: TDateTimePicker;
    FTree: TCustomVirtualStringTree;
    FNode: PVirtualNode;
    FColumn: TColumnIndex;
    FStopping: Boolean;
    FModified: Boolean;
    procedure PickerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PickerChange(Sender: TObject);
  public
    DataType: Byte; // @see mysql_structures
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
  FForm.memoText.MaxLength := MaxLength;
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



{ DateTime editor }

constructor TDateTimeEditorLink.Create;
begin
  inherited;
end;


destructor TDateTimeEditorLink.Destroy;
begin
  inherited;
  FPicker.Free;
end;


function TDateTimeEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
    FPicker.Show;
end;


function TDateTimeEditorLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    FPicker.Hide;
    FTree.CancelEditNode;
    FTree.SetFocus;
  end;
end;


function TDateTimeEditorLink.EndEdit: Boolean; stdcall;
var
  newtext: WideString;
begin
  Result := not FStopping;
  if Not Result then
    Exit;
  if FModified then begin
    case DataType of
      tpDATE:
        newtext := FormatDateTime(ShortDateFormat, FPicker.Date);
      tpDATETIME, tpTIMESTAMP:
        newtext := FormatDateTime(ShortDateFormat+' '+LongTimeFormat, FPicker.DateTime);
      tpTIME:
        newtext := FormatDateTime(LongTimeFormat, FPicker.Time);
    end;
    if newtext <> FTree.Text[FNode, FColumn] then
      FTree.Text[FNode, FColumn] := newtext;
  end;
  FPicker.Hide;
  FTree.SetFocus;
end;


function TDateTimeEditorLink.GetBounds: TRect; stdcall;
begin
  Result := FPicker.BoundsRect;
end;


function TDateTimeEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  Text: WideString;
begin
  Result := Tree is TCustomVirtualStringTree;
  if not Result then
    Exit;
  Ftree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  FPicker := TDateTimePicker.Create(Tree);
  FPicker.Parent := FTree;
  FPicker.OnKeyDown := PickerKeyDown;
  DateSeparator := '-';
  TimeSeparator := ':';
  ShortDateFormat := 'yyyy-MM-dd';
  LongTimeFormat := 'HH:mm:ss';
  case DataType of
    tpDATE: begin
      FPicker.Kind := dtkDate;
      FPicker.Format := ShortDateFormat;
    end;
    tpDATETIME, tpTIMESTAMP: begin
      FPicker.Kind := dtkDate;
      FPicker.Format := ShortDateFormat + ' ' + LongTimeFormat;
    end;
    tpTIME: begin
      FPicker.Kind := dtkTime;
      FPicker.Format := LongTimeFormat;
    end;
  end;
  Text := FTree.Text[Node, Column];
  try
    FPicker.DateTime := StrToDateTime(FTree.Text[Node, Column]);
  except
    FPicker.DateTime := Now;
  end;
  FModified := False;
  FPicker.SetFocus;
  FPicker.OnChange := PickerChange;
end;


procedure TDateTimeEditorLink.ProcessMessage(var Message: TMessage); stdcall;
begin
end;


procedure TDateTimeEditorLink.SetBounds(R: TRect); stdcall;
begin
  FPicker.BoundsRect := R;
end;


procedure TDateTimeEditorLink.PickerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    // Cancel by Escape
    VK_ESCAPE: FTree.CancelEditNode;
    // Apply changes and end editing by [Ctrl +] Enter
    VK_RETURN: FTree.EndEditNode;
  end;
end;


procedure TDateTimeEditorLink.PickerChange(Sender: TObject);
begin
  FModified := True;
end;

end.
