unit grideditlinks;

// The editor links, instanciated by VirtualTree.CreateEditor

interface

uses Windows, Forms, Graphics, messages, VirtualTrees, texteditor, bineditor, ComCtrls, SysUtils, Classes,
  mysql_structures, Main, helpers, TntStdCtrls, WideStrings, StdCtrls, ExtCtrls, TntCheckLst,
  Buttons, Controls, Types, PngSpeedButton;

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
    FDatePicker, FTimePicker: TDateTimePicker;
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

  // Handler for custom button click processing
  TButtonClickEvent = procedure (Sender: TObject; Tree: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex);

  // Inplace editor with button
  TInplaceEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FPanel: TPanel;
    FEdit: TTntEdit;
    FButton: TPNGSpeedButton;
    FTextEditor: TfrmTextEditor;
    FTree: TVirtualStringTree;
    FNode: PVirtualNode;
    FColumn: TColumnIndex;
    FAlignment: TAlignment;
    FTextBounds: TRect;
    FStopping: Boolean;
    FButtonVisible: boolean;
    FOnButtonClick: TButtonClickEvent;
    FFinalAction: integer;
    FMaxLength: integer;
    FOldWndProc: TWndMethod;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonClick(Sender: TObject);
    procedure SetButtonVisible(const Value: boolean);
    procedure EditWndProc(var Message: TMessage);
  protected
    procedure DoButtonClick;
    procedure CalcEditorPosition;
    procedure CalcButtonPosition;
  public
    constructor Create(Tree: TVirtualStringTree); overload;
    destructor Destroy; override;
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
    property Panel: TPanel read FPanel;
    property Edit: TTntEdit read FEdit;
    property Button: TPNGSpeedButton read FButton;
    property ButtonVisible: boolean read FButtonVisible write SetButtonVisible;
    property MaxLength: integer read FMaxLength write FMaxLength; // Used for frmTextEditor initialization
    property OnButtonClick: TButtonClickEvent read FOnButtonClick write FOnButtonClick;
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

  IsBinary := Mainform.FDataGridResult.Columns[Column].IsBinary;

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
    FForm.ShowModal;
end;


function TMemoEditorLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    FForm.Close;
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
    FForm.Close;
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
  FreeAndNil(FDatePicker);
  FreeAndNil(FTimePicker);
end;


function TDateTimeEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    if Assigned(FDatePicker) then
      FDatePicker.Show;
    if Assigned(FTimePicker) then
      FTimePicker.Show;
  end;
end;


function TDateTimeEditorLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    if Assigned(FDatePicker) then
      FDatePicker.Hide;
    if Assigned(FTimePicker) then
      FTimePicker.Hide;
    FTree.CancelEditNode;
    FTree.SetFocus;
  end;
end;


function TDateTimeEditorLink.EndEdit: Boolean; stdcall;
var
  dt: TDateTime;
  newtext: WideString;
begin
  Result := not FStopping;
  if Not Result then
    Exit;
  if FModified then begin
    case DataType of
      tpDATE:
        newtext := FormatDateTime(ShortDateFormat, FDatePicker.Date);
      tpDATETIME, tpTIMESTAMP:
      begin
	      // Take date and add time
        dt := FDatePicker.Date;
        ReplaceTime(dt, FTimePicker.Time);
        newtext := FormatDateTime(ShortDateFormat+' '+LongTimeFormat, dt);
      end;
      tpTIME:
        newtext := FormatDateTime(LongTimeFormat, FTimePicker.Time);
    end;
    if newtext <> FTree.Text[FNode, FColumn] then
      FTree.Text[FNode, FColumn] := newtext;
  end;
  if Assigned(FDatePicker) then
    FDatePicker.Hide;
  if Assigned(FTimePicker) then
    FTimePicker.Hide;
  FTree.SetFocus;
end;


function TDateTimeEditorLink.GetBounds: TRect; stdcall;
begin
  // Strange: Seems never called
  if Assigned(FDatePicker) and (not Assigned(FTimePicker)) then
    Result := FDatePicker.BoundsRect
  else if (not Assigned(FDatePicker)) and Assigned(FTimePicker) then
    Result := FTimePicker.BoundsRect
  else begin
    Result.TopLeft := FDatePicker.BoundsRect.TopLeft;
    Result.BottomRight := FDatePicker.BoundsRect.BottomRight;
  end;
end;


function TDateTimeEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  dt: TDateTime;
begin
  Result := Tree is TCustomVirtualStringTree;
  if not Result then
    Exit;
  Ftree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  DateSeparator := '-';
  TimeSeparator := ':';
  ShortDateFormat := 'yyyy-MM-dd';
  LongTimeFormat := 'HH:mm:ss';
  try
    dt := StrToDateTime(FTree.Text[Node, Column]);
  except
    dt := Now;
  end;
  if DataType in [tpDATE, tpDATETIME, tpTIMESTAMP] then begin
    FDatePicker := TDateTimePicker.Create(Tree);
    FDatePicker.Parent := FTree;
    FDatePicker.OnKeyDown := PickerKeyDown;
    FDatePicker.Kind := dtkDate;
    FDatePicker.Format := ShortDateFormat;
    FDatePicker.DateTime := dt;
    FDatePicker.OnChange := PickerChange;
  end;
  if DataType in [tpDATETIME, tpTIMESTAMP, tpTIME] then begin
    FTimePicker := TDateTimePicker.Create(Tree);
    FTimePicker.Parent := FTree;
    FTimePicker.OnKeyDown := PickerKeyDown;
    FTimePicker.Kind := dtkTime;
    FTimePicker.Format := LongTimeFormat;
    FTimePicker.DateTime := dt;
    FTimePicker.OnChange := PickerChange;
  end;
  if Assigned(FDatePicker) then
    FDatePicker.SetFocus
  else if Assigned(FTimePicker) then 
    FTimePicker.SetFocus;
  FModified := False;
end;


procedure TDateTimeEditorLink.ProcessMessage(var Message: TMessage); stdcall;
begin
end;


procedure TDateTimeEditorLink.SetBounds(R: TRect); stdcall;
var
  w: Integer;
begin
  if Assigned(FDatePicker) and (not Assigned(FTimePicker)) then
    FDatePicker.BoundsRect := R
  else if (not Assigned(FDatePicker)) and Assigned(FTimePicker) then
    FTimePicker.BoundsRect := R
  else begin
    w := (R.Right - R.Left) div 2;

    FDatePicker.Left := R.Left;
    FDatePicker.Top := R.Top;
    FDatePicker.Height := R.Bottom - R.Top;
    FDatePicker.Width := w;

    FTimePicker.Left := R.Left + w + 1;
    FTimePicker.Top := R.Top;
    FTimePicker.Height := R.Bottom - R.Top;
    FTimePicker.Width := w - 1;
  end;
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
  FPanel.ParentBackground := False;

  FCheckList := TTNTCheckListBox.Create(FPanel);
  FCheckList.Parent := FPanel;
  FCheckList.Font.Name := F.Name;
  FCheckList.Font.Size := F.Size;
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
  FPanel.Height := 130;

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

{ TInplaceEditorLink }

constructor TInplaceEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create;
  FTree := Tree;
  SendMessage(FTree.Handle, WM_SETREDRAW, 0, 0);  // Avoid flikering
  FButtonVisible := false;
  FOnButtonClick := nil;
  FTextEditor := nil;

  FPanel := TPanel.Create(nil);
  FPanel.Hide;
  FPanel.BevelOuter := bvNone;
  FPanel.ParentBackground := false; // Prevents transparency under XP theme
  FPanel.ParentColor := false;
  FPanel.Color := clWindow;

  FEdit := TTntEdit.Create(FPanel);
  FEdit.Hide;
  FEdit.Parent := FPanel;
  FEdit.BorderStyle := bsNone;
  FEdit.Color := clWindow;
  FEdit.OnKeyDown := EditKeyDown;
  FOldWndProc := FEdit.WindowProc;
  FEdit.WindowProc := EditWndProc;

  FButton := TPNGSpeedButton.Create(FPanel);
  FButton.PNGImage := Mainform.PngImageListMain.PngImages[33].PngImage;
  FButton.Hide;
  FButton.Parent := FPanel;
  FButton.Width := 20;
  FButton.Hint := 'Edit text in popup editor ...';
  FButton.ShowHint := True;
  FButton.OnClick := ButtonClick;
end;

destructor TInplaceEditorLink.Destroy;
begin
  if Assigned(FTextEditor) then
    FTextEditor.Release;
  FPanel.Free;
  case FFinalAction of
    VK_TAB: begin
      SendMessage(FTree.Handle, WM_KEYDOWN, VK_TAB, 0);
      SendMessage(FTree.Handle, WM_KEYDOWN, VK_RETURN, 0);
    end;
  end;
  inherited;
end;

function TInplaceEditorLink.BeginEdit: Boolean;
begin
  Result := not FStopping;
  if Result then begin
    if FButtonVisible then
      FButton.Show;
    FEdit.SelectAll;
    FEdit.Show;
    FPanel.Show;
    FEdit.SetFocus;
    SendMessage(FTree.Handle, WM_SETREDRAW, 1, 0);
    FTree.Repaint;
    FPanel.Repaint;
    FEdit.Repaint;
  end;
end;

function TInplaceEditorLink.CancelEdit: Boolean;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    if Assigned(FTextEditor) then
      FTextEditor.Close;
    FPanel.Hide;
    FTree.SetFocus;
  end;
end;

function TInplaceEditorLink.EndEdit: Boolean;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    if Assigned(FTextEditor) then begin
      if FTextEditor.GetText <> FTree.Text[FNode, FColumn] then
        FTree.Text[FNode, FColumn] := FTextEditor.GetText;
      FTextEditor.Close;
    end else begin
      if FEdit.Text <> FTree.Text[FNode, FColumn] then
        FTree.Text[FNode, FColumn] := FEdit.Text;
    end;
    FPanel.Hide;
    FTree.SetFocus;
  end;
end;

procedure TInplaceEditorLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      FTree.CancelEditNode;
    VK_RETURN:
      begin
        if (ssCtrl in Shift) then begin
          Key := 0;
          ButtonClick(FButton);
        end else
          FTree.EndEditNode;
      end;
    VK_TAB:
      begin
        FFinalAction := VK_TAB;
        FTree.EndEditNode;
      end;
  end;
end;

procedure TInplaceEditorLink.ButtonClick(Sender: TObject);
begin
  if not FButtonVisible then Exit; // Button was invisible, but hotkey was pressed
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, FTree, FNode, FColumn)
  else
    DoButtonClick;
end;

procedure TInplaceEditorLink.DoButtonClick;
begin
  FTextEditor := TfrmTextEditor.Create(FTree);
  FTextEditor.SetFont(FEdit.Font);
  FTextEditor.SetText(FTree.Text[FNode, FColumn]);
  FTextEditor.SetMaxLength(Self.FMaxLength);
  FTextEditor.ShowModal;
end;

procedure TInplaceEditorLink.EditWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_CHAR:
      if not (TWMChar(Message).CharCode in [VK_ESCAPE, VK_TAB]) then
        FOldWndProc(Message);
    WM_GETDLGCODE:
      Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS or DLGC_WANTTAB;
  else
    FOldWndProc(Message);
  end;
end;

function TInplaceEditorLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  NodeText: widestring;
begin
  Result := not FStopping;
  if not Result then Exit; 
  FNode := Node;
  FColumn := Column;

  FTree.GetTextInfo(Node, Column, FEdit.Font, FTextBounds, NodeText);
  FPanel.Parent := FTree;
  FEdit.Font.Color := clWindowText;
  FEdit.Text := NodeText;

  if Column <= NoColumn then begin
    FEdit.BidiMode := FTree.BidiMode;
    FAlignment := FTree.Alignment;
  end else begin
    FEdit.BidiMode := FTree.Header.Columns[Column].BidiMode;
    FAlignment := FTree.Header.Columns[Column].Alignment;
  end;
  if FEdit.BidiMode <> bdLeftToRight then
    ChangeBidiModeAlignment(FAlignment);
  Result := true;
end;

procedure TInplaceEditorLink.ProcessMessage(var Message: TMessage);
begin
  FEdit.WindowProc(Message);
end;

function TInplaceEditorLink.GetBounds: TRect;
begin
  Result := FPanel.BoundsRect;
end;

procedure TInplaceEditorLink.SetBounds(R: TRect);
begin
  if not FStopping then begin
    // Fix for wrong rect calculation, when left alignment is used
    if FAlignment = taLeftJustify then begin
      Dec(R.Left, 4);
      Dec(R.Right, 1);
    end;
    // Set the edit's bounds but make sure there's a minimum width and the right border does not
    // extend beyond the parent's left/right border.
    if R.Left < 0 then
      R.Left := 0;
    if R.Right - R.Left < 30 then begin
      if FAlignment = taRightJustify then
        R.Left := R.Right - 30
      else
        R.Right := R.Left + 30;
    end;
    if R.Right > FTree.ClientWidth then
      R.Right := FTree.ClientWidth;
    FPanel.BoundsRect := R;
    // Position edit control according to FTextBounds
    CalcEditorPosition;
    CalcButtonPosition;
  end;
end;

procedure TInplaceEditorLink.CalcEditorPosition;
var
  R: TRect;
begin
  if not Assigned(FTree) then
    Exit;
  R.Top := FTextBounds.Top - FPanel.Top;
  R.Bottom := FTextBounds.Bottom - FPanel.Top;
  R.Left := FTree.TextMargin;
  R.Right := FPanel.Width - R.Left;
  if FButtonVisible then
    Dec(R.Right, FButton.Width);
  FEdit.BoundsRect := R;
end;

procedure TInplaceEditorLink.CalcButtonPosition;
var
  R: TRect;
begin
  R.Top := 0;
  R.Bottom := FPanel.Height;
  R.Left := FPanel.Width - 16;
  R.Right := FPanel.Width;
  FButton.BoundsRect := R;
end;

procedure TInplaceEditorLink.SetButtonVisible(const Value: boolean);
begin
  FButtonVisible := Value;
  CalcEditorPosition;
end;

end.
