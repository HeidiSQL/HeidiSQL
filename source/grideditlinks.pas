unit grideditlinks;

// The editor links, instanciated by VirtualTree.CreateEditor

interface

uses Windows, Forms, Graphics, messages, VirtualTrees, texteditor, bineditor, ComCtrls, SysUtils, Classes,
  mysql_structures, Main, ChildWin, helpers, TntStdCtrls, WideStrings, StdCtrls, ExtCtrls, TntCheckLst;

type
  TMemoEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FForm: TMemoEditor;
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

type
  TEnumEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FCombo: TTnTComboBox;
    FTree: TCustomVirtualStringTree;
    FNode: PVirtualNode;
    FColumn: TColumnIndex;
    FStopping: Boolean;
    procedure ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    ValueList: TWideStringList;
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

type
  TSetEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FPanel: TPanel;
    FCheckList: TTNTCheckListBox;
    FBtnOK, FBtnCancel: TButton;
    FTree: TCustomVirtualStringTree;
    FNode: PVirtualNode;
    FColumn: TColumnIndex;
    FTextBounds: TRect;
    FStopping: Boolean;
    procedure CheckListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  public
    ValueList: TWideStringList;
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
  FForm := nil;
end;

destructor TMemoEditorLink.Destroy;
begin
  inherited;
  FreeAndNil(FForm);
end;


function TMemoEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
// Retrieves the true text bounds from the owner tree.
var
  IsBinary: Boolean;
  Text: WideString;
  F: TFont;
begin
  Result := Tree is TCustomVirtualStringTree;
  if not Result then
    exit;

  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // Initial size, font and text (ANSI) of the node.
  F := TFont.Create;
  FTree.GetTextInfo(Node, Column, F, FTextBounds, Text);

  IsBinary := MainForm.ChildWin.FDataGridResult.Columns[Column].IsBinary;

  // Get wide text of the node.
  Text := FTree.Text[FNode, FColumn];

  // Create the text editor form
  if IsBinary then FForm := TfrmBinEditor.Create(Ftree)
  else FForm := TfrmTextEditor.Create(Ftree);

  FForm.SetFont(F);
  FForm.SetText(Text);
  FForm.SetMaxLength(MaxLength);
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
    if FForm.GetText <> FTree.Text[FNode, FColumn] then
      FTree.Text[FNode, FColumn] := FForm.GetText;
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
  // Not in use, form's position is centered on mainform
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




{ Enum editor }

constructor TEnumEditorLink.Create;
begin
  inherited;
end;


destructor TEnumEditorLink.Destroy;
begin
  inherited;
  FCombo.Free;
end;


function TEnumEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
    FCombo.Show;
end;


function TEnumEditorLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    FCombo.Hide;
    FTree.CancelEditNode;
    FTree.SetFocus;
  end;
end;


function TEnumEditorLink.EndEdit: Boolean; stdcall;
var
  newtext: WideString;
begin
  Result := not FStopping;
  if Not Result then
    Exit;
  newText := FCombo.Text;
  if newtext <> FTree.Text[FNode, FColumn] then
    FTree.Text[FNode, FColumn] := newtext;
  FCombo.Hide;
  FTree.SetFocus;
end;


function TEnumEditorLink.GetBounds: TRect; stdcall;
begin
  Result := FCombo.BoundsRect;
end;


function TEnumEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  i: Integer;
begin
  Result := Tree is TCustomVirtualStringTree;
  if not Result then
    Exit;
  Ftree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  FCombo := TTnTComboBox.Create(Tree);
  FCombo.Parent := FTree;
  // Set style to OwnerDraw, otherwise we wouldn't be able to adjust the combo's height
  FCombo.Style := csOwnerDrawFixed;
  for i := 0 to ValueList.Count - 1 do
    FCombo.Items.Add(ValueList[i]);
  FCombo.ItemIndex := FCombo.Items.IndexOf(FTree.Text[FNode, FColumn]);
  FCombo.SetFocus;
  FCombo.OnKeyDown := ComboKeyDown;
end;


procedure TEnumEditorLink.ProcessMessage(var Message: TMessage); stdcall;
begin
end;


procedure TEnumEditorLink.SetBounds(R: TRect); stdcall;
begin
  FCombo.BoundsRect := R;
  FCombo.ItemHeight := R.Bottom - R.Top - 4;
end;


procedure TEnumEditorLink.ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    // Cancel by Escape
    VK_ESCAPE: FTree.CancelEditNode;
    // Apply changes and end editing by [Ctrl +] Enter
    VK_RETURN: FTree.EndEditNode;
  end;
end;




{ SET editor }

constructor TSetEditorLink.Create;
begin
  inherited;
end;


destructor TSetEditorLink.Destroy;
begin
  inherited;
  FCheckList.Free;
  FPanel.Free;
end;


function TSetEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
    FPanel.Show;
end;


function TSetEditorLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    FPanel.Hide;
    FTree.CancelEditNode;
    FTree.SetFocus;
  end;
end;


function TSetEditorLink.EndEdit: Boolean; stdcall;
var
  newtext: WideString;
  i: Integer;
begin
  Result := not FStopping;
  if Not Result then
    Exit;
  newText := '';
  for i := 0 to FCheckList.Items.Count - 1 do
    if FCheckList.Checked[i] then newText := newText + FCheckList.Items[i] + ',';
  Delete(newText, Length(newText), 1);
    
  if newtext <> FTree.Text[FNode, FColumn] then
    FTree.Text[FNode, FColumn] := newtext;
  FPanel.Hide;
  FTree.SetFocus;
end;


function TSetEditorLink.GetBounds: TRect; stdcall;
begin
  Result := FPanel.BoundsRect;
end;


function TSetEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  F: TFont;
  SelValues: TWideStringList;
  Text: WideString;
begin
  Result := Tree is TCustomVirtualStringTree;
  if not Result then
    Exit;
  Ftree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // Initial size, font and text of the node.
  F := TFont.Create;
  FTree.GetTextInfo(Node, Column, F, FTextBounds, Text);
  SelValues := TWideStringList.Create;
  SelValues.Delimiter := ',';
  SelValues.StrictDelimiter := True;
  SelValues.DelimitedText := Text;

  FPanel := TPanel.Create(Tree);
  FPanel.Parent := FTree;
  FPanel.Left := FTextBounds.Left;
  FPanel.Top := FTextBounds.Top;

  FCheckList := TTNTCheckListBox.Create(FPanel);
  FCheckList.Parent := FPanel;
  FCheckList.Font := F;
  FCheckList.Items.Assign(ValueList);
  ToggleCheckListBox(FCheckList, True, SelValues);
  FCheckList.SetFocus;
  FCheckList.OnKeyDown := CheckListKeyDown;

  FBtnOk := TButton.Create(FPanel);
  FBtnOk.Parent := FPanel;
  FBtnOk.Caption := 'OK';
  FBtnOk.OnClick := BtnOkClick;

  FBtnCancel := TButton.Create(FPanel);
  FBtnCancel.Parent := FPanel;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.OnClick := BtnCancelClick;
end;


procedure TSetEditorLink.ProcessMessage(var Message: TMessage); stdcall;
begin
end;


procedure TSetEditorLink.SetBounds(R: TRect); stdcall;
const
  margin = 3;
begin
  FPanel.Top := R.Top;
  FPanel.Left := R.Left;
  FPanel.Width := R.Right - R.Left;
  FPanel.Height := 200;

  FBtnOk.Width := (FPanel.Width - 3*margin) div 2;
  FBtnOk.Left := margin;
  FBtnOk.Height := 24;
  FBtnOk.Top := FPanel.Height - margin - FBtnOk.Height;

  FBtnCancel.Width := FBtnOk.Width;
  FBtnCancel.Left := 2*margin + FBtnOk.Width;
  FBtnCancel.Height := FBtnOk.Height;
  FBtnCancel.Top := FBtnOk.Top;

  FCheckList.Top := margin;
  FCheckList.Left := margin;
  FCheckList.Width := FPanel.Width - 2*margin;
  FCheckList.Height := FPanel.Height - 3*margin - FBtnOk.Height;
end;


procedure TSetEditorLink.CheckListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    // Cancel by Escape
    VK_ESCAPE: FTree.CancelEditNode;
    // Apply changes and end editing by [Ctrl +] Enter
    VK_RETURN: FTree.EndEditNode;
  end;
end;

procedure TSetEditorLink.BtnOkClick(Sender: TObject);
begin
  FTree.EndEditNode;
end;

procedure TSetEditorLink.BtnCancelClick(Sender: TObject);
begin
  FTree.CancelEditNode;
end;

end.
