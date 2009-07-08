unit grideditlinks;

// The editor links, instanciated by VirtualTree.CreateEditor

interface

uses Windows, Forms, Graphics, messages, VirtualTrees, texteditor, bineditor, ComCtrls, SysUtils, Classes,
  mysql_structures, helpers, TntStdCtrls, WideStrings, StdCtrls, ExtCtrls, TntCheckLst,
  Buttons, Controls, Types, PngSpeedButton, Dialogs, Mask, MaskUtils, DateUtils ;

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
    FMaskEdit: TMaskEdit;
    FTimer: TTimer;
    FModifyOffset: Integer;
    FTimerCalls: Integer;
    FUpDown: TUpDown;
    FTree: TVirtualStringTree;
    FNode: PVirtualNode;
    FColumn: TColumnIndex;
    FTextBounds: TRect;
    FStopping: Boolean;
    FFinalKeyDown: Integer;
    FOldWndProc: TWndMethod;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
    procedure UpDownMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnTimer(Sender: TObject);
    procedure ModifyDate(Offset: Integer);
    procedure EditWndProc(var Message: TMessage);
  public
    Datatype: TDatatypeIndex; // @see mysql_structures
    constructor Create(Tree: TVirtualStringTree); overload;
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
    AllowCustomText: Boolean;
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
    FFinalKeyDown: integer;
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

  TColumnDefaultType = (cdtText, cdtTextUpdateTS, cdtNull, cdtNullUpdateTS, cdtCurTS, cdtCurTSUpdateTS, cdtAutoInc);
  TColumnDefaultEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FTree: TCustomVirtualStringTree;
    FNode: PVirtualNode;
    FColumn: TColumnIndex;
    FStopping: Boolean;
    FPanel: TPanel;
    FRadioText, FRadioNULL, FRadioCurTS, FRadioAutoInc: TRadioButton;
    FCheckCurTS: TCheckbox;
    FMemoText: TTNTMemo;
    FBtnOK, FBtnCancel: TButton;
    procedure RadioClick(Sender: TObject);
    procedure TextChange(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  public
    DefaultType: TColumnDefaultType;
    DefaultText: WideString;
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

  TDataTypeEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FTree: TVirtualStringTree;
    FTreeSelect: TVirtualStringTree;
    FMemoHelp: TMemo;
    FNode: PVirtualNode;
    FColumn: TColumnIndex;
    FTextBounds: TRect;
    FStopping: Boolean;
    FFinalKeyDown: Integer;
    FOldWndProc: TWndMethod;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditWndProc(var Message: TMessage);
    procedure DoTreeSelectGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure DoTreeSelectInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DoTreeSelectInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure DoTreeSelectHotChange(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode);
    procedure DoTreeSelectPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure DoTreeSelectFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode:
        PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure DoTreeSelectFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  public
    Datatype: TDatatypeIndex;
    constructor Create(Tree: TVirtualStringTree); overload;
    destructor Destroy; override;
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
  end;

function GetColumnDefaultType(var Text: WideString): TColumnDefaultType;
function GetColumnDefaultClause(DefaultType: TColumnDefaultType; Text: WideString): WideString;


implementation

uses main;


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

  IsBinary := Mainform.FDataGridResult.Columns[Column].DatatypeCat = dtcBinary;

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
    if FTree.CanFocus then
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
    if FTree.CanFocus then
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

constructor TDateTimeEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create;
  FTree := Tree;
  // Avoid flicker
  SendMessage(FTree.Handle, WM_SETREDRAW, 0, 0);

  FMaskEdit := TMaskEdit.Create(FTree);
  FMaskEdit.Parent := FTree;
  FMaskEdit.Hide;
  FMaskEdit.BorderStyle := bsNone;
  FMaskEdit.OnKeyDown := DoKeyDown;
  FMaskEdit.OnKeyUp := DoKeyUp;
  FOldWndProc := FMaskEdit.WindowProc;
  FMaskEdit.WindowProc := EditWndProc;

  FUpDown := TUpDown.Create(FTree);
  FUpDown.Hide;
  FUpDown.Parent := FTree;
  FUpDown.OnChangingEx := UpDownChangingEx;
  FUpDown.OnMouseUp := UpDownMouseUp;

  FTimer := TTimer.Create(FMaskEdit);
  FTimer.Interval := 50;
  FTimer.OnTimer := DoOnTimer;
  FTimer.Enabled := False;
end;


destructor TDateTimeEditorLink.Destroy;
begin
  inherited;
  OpenRegistry;
  Mainreg.WriteInteger(REGPREFIX_DATEEDITOR_CURSOR+IntToStr(Integer(Datatype)), FMaskEdit.SelStart);
  FreeAndNil(FTimer);
  FreeAndNil(FUpDown);
  FreeAndNil(FMaskEdit);
  case FFinalKeyDown of
    VK_TAB: begin
      SendMessage(FTree.Handle, WM_KEYDOWN, FFinalKeyDown, 0);
      SendMessage(FTree.Handle, WM_KEYDOWN, VK_RETURN, 0);
    end;
  end;
end;


function TDateTimeEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    SendMessage(FTree.Handle, WM_SETREDRAW, 1, 0);
    FMaskEdit.Show;
    FUpDown.Show;
    FMaskEdit.SetFocus;
    // Focus very last segment of date
    FMaskEdit.SelStart := GetRegValue(REGPREFIX_DATEEDITOR_CURSOR+IntToStr(Integer(Datatype)), Length(FMaskEdit.Text)-1);
    FMaskEdit.SelLength := 1;
  end;
end;


function TDateTimeEditorLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    FTree.CancelEditNode;
    if FTree.CanFocus then
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
  newtext := FMaskEdit.Text;
  if newtext <> FTree.Text[FNode, FColumn] then
    FTree.Text[FNode, FColumn] := newtext;
  if FTree.CanFocus then
    FTree.SetFocus;
end;


function TDateTimeEditorLink.GetBounds: TRect; stdcall;
begin
  Result := FMaskEdit.BoundsRect;
end;


function TDateTimeEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  NodeText: WideString;
  MinColWidth: Integer;
begin
  Result := not FStopping;
  if not Result then
    Exit;
  case Datatype of
    dtDate: FMaskEdit.EditMask := '0000-00-00;1; ';
    dtDatetime, dtTimestamp: FMaskEdit.EditMask := '0000-00-00 00\:00\:00;1; ';
    dtTime: FMaskEdit.EditMask := '00\:00\:00;1; ';
    //dtYear??
  end;
  FNode := Node;
  FColumn := Column;
  FTree.GetTextInfo(FNode, FColumn, FMaskEdit.Font, FTextBounds, NodeText);
  FMaskEdit.Font.Color := clWindowText;
  if NodeText = '' then case Datatype of
    dtDate: NodeText := DateToStr(Now);
    dtDatetime, dtTimestamp: NodeText := DateTimeToStr(Now);
    dtTime: NodeText := TimeToStr(Now);
  end;
  FMaskEdit.Text := NodeText;
  // Auto-enlarge current tree column so the text in the edit is not cut
  MinColWidth := FTextBounds.Right - FTextBounds.Left + FUpDown.Width + 5;
  if FTree.Header.Columns[FColumn].Width < MinColWidth then
    FTree.Header.Columns[FColumn].Width := MinColWidth;
end;


procedure TDateTimeEditorLink.ProcessMessage(var Message: TMessage); stdcall;
begin
  FMaskEdit.WindowProc(Message);
end;


procedure TDateTimeEditorLink.EditWndProc(var Message: TMessage);
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


procedure TDateTimeEditorLink.SetBounds(R: TRect); stdcall;
var
  r2: TRect;
  OldSelStart, OldSelLen: Integer;
begin
  r2 := R;
  Inc(r2.Left, R.Right-R.Left-FUpDown.Width);
  Dec(r2.Top, 2);
  FUpDown.BoundsRect := r2;
  r2.Top := FTextBounds.Top;
  r2.Bottom := FTextBounds.Bottom;
  r2.Left := FTextBounds.Left + FTree.TextMargin;
  r2.Right := R.Right - FUpDown.Width;
  FMaskEdit.BoundsRect := r2;
  OldSelStart := FMaskEdit.SelStart;
  OldSelLen := FMaskEdit.SelLength;
  FMaskEdit.SelStart := 0;
  FMaskEdit.SelStart := OldSelStart;
  FMaskEdit.SelLength := OldSelLen;
end;


procedure TDateTimeEditorLink.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FFinalKeyDown := Key;
  case Key of
    // Cancel by Escape
    VK_ESCAPE: FTree.CancelEditNode;
    // Apply changes and end editing by [Ctrl +] Enter
    VK_RETURN, VK_TAB: FTree.EndEditNode;
    // Increase date on arrow-up, decrease it on arrow-down
    VK_UP, VK_DOWN: if not FTimer.Enabled then begin
      if Key = VK_UP then FModifyOffset := 1
      else FModifyOffset := -1;
      FTimerCalls := 0;
      DoOnTimer(Sender);
      FTimer.Enabled := True;
    end;
  end;
end;


procedure TDateTimeEditorLink.DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FTimer.Enabled := False;
end;


procedure TDateTimeEditorLink.UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
  NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FTimer.Enabled then
    Exit;
  if Direction = updUp then FModifyOffset := 1
  else FModifyOffset := -1;
  FTimerCalls := 0;
  DoOnTimer(Sender);
  FTimer.Enabled := True;
end;


procedure TDateTimeEditorLink.UpDownMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FTimer.Enabled := False;
end;


procedure TDateTimeEditorLink.DoOnTimer(Sender: TObject);
var
  DelayCalls: Integer;
begin
  Inc(FTimerCalls);
  // short delay before counting up/down
  DelayCalls := 350 Div FTimer.Interval;
  if (FTimerCalls > DelayCalls) or (not (Sender is TTimer)) then
    ModifyDate(FModifyOffset);
  // Speed up counting in steps
  if FTimerCalls in [DelayCalls*5, DelayCalls*10] then begin
    if FModifyOffset > 0 then
      Inc(FModifyOffset, 3)
    else
      Dec(FModifyOffset, 3);
  end;
end;


procedure TDateTimeEditorLink.ModifyDate(Offset: Integer);
var
  dt: TDateTime;
  d: TDate;
  t: TTime;
  text: String;
  OldSelStart, OldSelLength: Integer;
begin
  try
    case Datatype of
      dtDate: begin
        d := StrToDate(FMaskEdit.Text);
        // De- or increase focused date segment
        case FMaskEdit.SelStart of
          0..3: d := IncYear(d, Offset);
          5,6: d := IncMonth(d, Offset);
          8,9: d := IncDay(d, Offset);
        end;
        text := DateToStr(d);
      end;

      dtDateTime, dtTimestamp: begin
        dt := StrToDateTime(FMaskEdit.Text);
        case FMaskEdit.SelStart of
          0..3: dt := IncYear(dt, Offset);
          5,6: dt := IncMonth(dt, Offset);
          8,9: dt := IncDay(dt, Offset);
          11,12: dt := IncHour(dt, Offset);
          14,15: dt := IncMinute(dt, Offset);
          17,18: dt := IncSecond(dt, Offset);
        end;
        text := DateTimeToStr(dt);
        if Length(text) = 10 then
          text := text + ' 00:00:00';
      end;

      dtTime: begin
        t := StrToTime(FMaskEdit.Text);
        case FMaskEdit.SelStart of
          0,1: t := IncHour(t, Offset);
          3,4: t := IncMinute(t, Offset);
          6,7: t := IncSecond(t, Offset);
        end;
        text := TimeToStr(t);
      end;

      else text := '';
    end;

    if text <> '' then begin
      OldSelStart := FMaskEdit.SelStart;
      OldSelLength := FMaskEdit.SelLength;
      FMaskEdit.Text := text;
      FMaskEdit.SelStart := OldSelStart;
      FMaskEdit.SelLength := OldSelLength;
    end;
  except
    // Ignore any DateToStr exception. Should only appear in cases where the users
    // enters invalid dates
  end;
end;



{ Enum editor }

constructor TEnumEditorLink.Create;
begin
  inherited;
  AllowCustomText := False;
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
    if FTree.CanFocus then
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
  if FTree.CanFocus then
    FTree.SetFocus;
end;


function TEnumEditorLink.GetBounds: TRect; stdcall;
begin
  Result := FCombo.BoundsRect;
end;


function TEnumEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  i: Integer;
  CellRect: TRect;
begin
  Result := Tree is TCustomVirtualStringTree;
  if not Result then
    Exit;
  Ftree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  FCombo := TTnTComboBox.Create(FTree);
  FCombo.Parent := FTree;

  CellRect := Ftree.GetDisplayRect(FNode, FColumn, False);
  FCombo.BoundsRect := CellRect;
  for i := 0 to ValueList.Count - 1 do
    FCombo.Items.Add(ValueList[i]);
  if AllowCustomText then begin
    FCombo.Style := csDropDown;
    FCombo.Text := FTree.Text[FNode, FColumn];
  end else begin
    // Set style to OwnerDraw, otherwise we wouldn't be able to adjust the combo's height
    FCombo.Style := csOwnerDrawFixed;
    FCombo.ItemIndex := FCombo.Items.IndexOf(FTree.Text[FNode, FColumn]);
  end;
  FCombo.SetFocus;
  FCombo.OnKeyDown := ComboKeyDown;
end;


procedure TEnumEditorLink.ProcessMessage(var Message: TMessage); stdcall;
begin
end;


procedure TEnumEditorLink.SetBounds(R: TRect); stdcall;
begin
  FCombo.BoundsRect := Ftree.GetDisplayRect(FNode, FColumn, False);
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
    if FTree.CanFocus then
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
  if FTree.CanFocus then
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
  case FFinalKeyDown of
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
    if FTree.CanFocus then
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
    if FTree.CanFocus then
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
    VK_F2:
      ButtonClick(FButton);
    VK_TAB:
      begin
        FFinalKeyDown := VK_TAB;
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
  FTextEditor.SetText(FEdit.Text);
  FTextEditor.Modified := FEdit.Text <> FTree.Text[FNode, FColumn];
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
  if ScanNulChar(NodeText) then begin
    MessageDlg(SContainsNulCharGrid, mtInformation, [mbOK], 0);
    NodeText := RemoveNulChars(NodeText);
  end;
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



{ Column default editor }

constructor TColumnDefaultEditorLink.Create;
begin
  inherited;
end;


destructor TColumnDefaultEditorLink.Destroy;
begin
  inherited;
  FPanel.Free;
end;


function TColumnDefaultEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  F: TFont;
  TextBounds: TRect;
  NodeText: WideString;
  Default: TColumnDefaultType;
const
  m = 3;
begin
  Ftree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // Initial size, font and text of the node.
  F := TFont.Create;
  FTree.GetTextInfo(Node, Column, F, TextBounds, NodeText);

  FPanel := TPanel.Create(Tree);
  FPanel.Parent := FTree;
  SetBounds(TextBounds);
  FPanel.Width := 200;
  FPanel.ParentBackground := False;
  // usefull but looks ugly:
  //SetWindowSizeGrip(FPanel.Handle, True);

  FRadioText := TRadioButton.Create(FPanel);
  FRadioText.Parent := FPanel;
  FRadioText.Top := m;
  FRadioText.Left := m;
  FRadioText.Width := FRadioText.Parent.Width - 2 * FRadioText.Left;
  FRadioText.OnClick := RadioClick;
  FRadioText.Caption := 'Custom:';

  FMemoText := TTNTMemo.Create(FPanel);
  FMemoText.Parent := FPanel;
  FMemoText.Top := FRadioText.Top + FRadioText.Height + m;
  FMemoText.Left := 2*m;
  FMemoText.Width := FMemoText.Parent.Width - FMemoText.Left - m;
  FMemoText.Height := 40;
  FMemoText.ScrollBars := ssVertical;
  FMemoText.OnChange := TextChange;

  FRadioNull := TRadioButton.Create(FPanel);
  FRadioNull.Parent := FPanel;
  FRadioNull.Top := FMemoText.Top + FMemoText.Height + m;
  FRadioNull.Left := m;
  FRadioNull.Width := FRadioNull.Parent.Width - 2 * FRadioNull.Left;
  FRadioNull.OnClick := RadioClick;
  FRadioNull.Caption := 'NULL';

  FRadioCurTS := TRadioButton.Create(FPanel);
  FRadioCurTS.Parent := FPanel;
  FRadioCurTS.Top := FRadioNull.Top + FRadioNull.Height + m;
  FRadioCurTS.Left := m;
  FRadioCurTS.Width := FRadioCurTS.Parent.Width - 2 * FRadioCurTS.Left;
  FRadioCurTS.OnClick := RadioClick;
  FRadioCurTS.Caption := 'CURRENT_TIMESTAMP';

  FCheckCurTS := TCheckbox.Create(FPanel);
  FCheckCurTS.Parent := FPanel;
  FCheckCurTS.Top := FRadioCurTS.Top + FRadioCurTS.Height + m;
  FCheckCurTS.Left := m;
  FCheckCurTS.Width := FCheckCurTS.Parent.Width - 2 * FCheckCurTS.Left;
  FCheckCurTS.OnClick := RadioClick;
  FCheckCurTS.Caption := 'ON UPDATE CURRENT_TIMESTAMP';

  FRadioAutoInc := TRadioButton.Create(FPanel);
  FRadioAutoInc.Parent := FPanel;
  FRadioAutoInc.Top := FCheckCurTS.Top + FCheckCurTS.Height + m;
  FRadioAutoInc.Left := m;
  FRadioAutoInc.Width := FRadioAutoInc.Parent.Width - 2 * FRadioAutoInc.Left;
  FRadioAutoInc.OnClick := RadioClick;
  FRadioAutoInc.Caption := 'AUTO_INCREMENT';

  FBtnOk := TButton.Create(FPanel);
  FBtnOk.Parent := FPanel;
  FBtnOk.Width := 60;
  FBtnOk.Top := FRadioAutoInc.Top + FRadioAutoInc.Height + m;
  FBtnOk.Left := FPanel.Width - 2*m - 2*FBtnOk.Width;
  FBtnOk.OnClick := BtnOkClick;
  FBtnOk.Default := True;
  FBtnOk.Caption := 'OK';

  FBtnCancel := TButton.Create(FPanel);
  FBtnCancel.Parent := FPanel;
  FBtnCancel.Top := FBtnOk.Top;
  FBtnCancel.Width := FBtnOk.Width;
  FBtnCancel.Left := FBtnOk.Left + FBtnOk.Width + m;
  FBtnCancel.OnClick := BtnCancelClick;
  FBtnCancel.Cancel := True;
  FBtnCancel.Caption := 'Cancel';

  FPanel.Height := FBtnOk.Top + FBtnOk.Height + m;
  FRadioText.Anchors := [akLeft, akTop, akRight];
  FMemoText.Anchors := [akLeft, akTop, akRight, akBottom];
  FRadioNull.Anchors := [akLeft, akBottom, akRight];
  FRadioCurTS.Anchors := [akLeft, akBottom, akRight];
  FCheckCurTS.Anchors := [akLeft, akBottom, akRight];
  FRadioAutoInc.Anchors := [akLeft, akBottom, akRight];
  FBtnOk.Anchors := [akBottom, akRight];
  FBtnCancel.Anchors := FBtnOk.Anchors;
  FPanel.Width := GetParentForm(FPanel).Canvas.TextWidth(FCheckCurTS.Caption) + 2*FCheckCurTS.Left + 16;

  case DefaultType of
    cdtText, cdtTextUpdateTS: begin
      FRadioText.Checked := True;
      FMemoText.Text := DefaultText;
    end;
    cdtNull, cdtNullUpdateTS: FRadioNull.Checked := True;
    cdtCurTS, cdtCurTSUpdateTS: FRadioCurTS.Checked := True;
    cdtAutoInc: FRadioAutoInc.Checked := True;
  end;
  FCheckCurTS.Checked := DefaultType in [cdtTextUpdateTS, cdtNullUpdateTS, cdtCurTSUpdateTS];
  Result := True;
end;


procedure TColumnDefaultEditorLink.ProcessMessage(var Message: TMessage); stdcall;
begin
end;


function TColumnDefaultEditorLink.GetBounds: TRect; stdcall;
begin
  Result := FPanel.BoundsRect;
end;


procedure TColumnDefaultEditorLink.SetBounds(R: TRect); stdcall;
var
  TreeBottom, TreeRight,
  PanelTop, PanelLeft: Integer;
  TreeRect: TRect;
begin
  TreeRect := FTree.BoundsRect;

  PanelTop := R.Top;
  TreeBottom := TreeRect.Bottom - TreeRect.Top -
    (FTree as TVirtualStringtree).Header.Height - 20; // Column header and scrollbar
  if R.Top + FPanel.Height > TreeBottom then
    PanelTop := Max(0, TreeBottom - FPanel.Height);

  PanelLeft := R.Left;
  TreeRight := TreeRect.Right - TreeRect.Left - 20;
  if R.Left + FPanel.Width > TreeRight then
    PanelLeft := Max(0, TreeRight - FPanel.Width);

  FPanel.Top := PanelTop;
  FPanel.Left := PanelLeft;
end;


function TColumnDefaultEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
    FPanel.Show;
end;


function TColumnDefaultEditorLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    if FTree.CanFocus then
      FTree.SetFocus;
  end;
end;


function TColumnDefaultEditorLink.EndEdit: Boolean; stdcall;
var
  newText: WideString;
  newDefaultType: TColumnDefaultType;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    if FRadioText.Checked and FCheckCurTS.Checked then
      newDefaultType := cdtTextUpdateTS
    else if FRadioText.Checked then
      newDefaultType := cdtText
    else if FRadioNull.Checked and FCheckCurTS.Checked then
      newDefaultType := cdtNullUpdateTS
    else if FRadioNull.Checked then
      newDefaultType := cdtNull
    else if FRadioCurTS.Checked and FCheckCurTS.Checked then
      newDefaultType := cdtCurTSUpdateTS
    else if FRadioCurTS.Checked then
      newDefaultType := cdtCurTS
    else if FRadioAutoInc.Checked then
      newDefaultType := cdtAutoInc
    else
      newDefaultType := cdtText;

    case newDefaultType of
      cdtText, cdtTextUpdateTS: newText := FMemoText.Text;
      cdtNull, cdtNullUpdateTS: newText := 'NULL';
      cdtCurTS, cdtCurTSUpdateTS: newText := 'CURRENT_TIMESTAMP';
      cdtAutoInc: newText := 'AUTO_INCREMENT';
    end;
    newText := IntToStr(Integer(newDefaultType)) + newText;
    if newtext <> IntToStr(Integer(DefaultType)) + DefaultText then
      FTree.Text[FNode, FColumn] := newtext;
    if FTree.CanFocus then
      FTree.SetFocus;
  end;
end;


procedure TColumnDefaultEditorLink.RadioClick(Sender: TObject);
begin
  if not FRadioText.Checked then
    FMemoText.Color := clBtnFace
  else
    FMemoText.Color := clWindow;
end;


procedure TColumnDefaultEditorLink.TextChange(Sender: TObject);
begin
  FRadioText.Checked := True;
end;


procedure TColumnDefaultEditorLink.BtnOkClick(Sender: TObject);
begin
  FTree.EndEditNode;
end;


procedure TColumnDefaultEditorLink.BtnCancelClick(Sender: TObject);
begin
  FTree.CancelEditNode;
end;


function GetColumnDefaultType(var Text: WideString): TColumnDefaultType;
begin
  Result := TColumnDefaultType(MakeInt(Copy(Text, 1, 1)));
  Text := Copy(Text, 2, Length(Text)-1);
end;


function GetColumnDefaultClause(DefaultType: TColumnDefaultType; Text: WideString): WideString;
begin
  case DefaultType of
    cdtText:           Result := ' DEFAULT '+esc(Text);
    cdtTextUpdateTS:   Result := ' DEFAULT '+esc(Text)+' ON UPDATE CURRENT_TIMESTAMP';
    cdtNull:           Result := ' DEFAULT NULL';
    cdtNullUpdateTS:   Result := ' DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP';
    cdtCurTS:          Result := ' DEFAULT CURRENT_TIMESTAMP';
    cdtCurTSUpdateTS:  Result := ' DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP';
    cdtAutoInc:        Result := ' AUTO_INCREMENT';
  end;
end;



{ Datatype selector }
constructor TDataTypeEditorLink.Create(Tree: TVirtualStringTree);
var
  ParentControl: TWinControl;
begin
  inherited Create;
  FTree := Tree;
  // Enable mouse scrolling on FtreeSelect, plus ensure the
  // tree is not partly hidden when it pops up in a bottom cell
  ParentControl := GetParentForm(FTree);
  // Avoid flicker
  SendMessage(ParentControl.Handle, WM_SETREDRAW, 0, 0);

  FTreeSelect := TVirtualStringTree.Create(FTree);
  FTreeSelect.TreeOptions.PaintOptions := FTreeSelect.TreeOptions.PaintOptions
    - [toShowTreeLines, toShowButtons, toShowRoot]
    + [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
  FTreeSelect.TreeOptions.SelectionOptions := FTreeSelect.TreeOptions.SelectionOptions
    + [toFullRowSelect];
  FTreeSelect.Header.Columns.Add;
  FTreeSelect.Parent := ParentControl;
  FTreeSelect.TextMargin := 0;
  FTreeSelect.RootNodeCount := Length(DatatypeCategories);
  FTreeSelect.OnGetText := DoTreeSelectGetText;
  FTreeSelect.OnInitNode := DoTreeSelectInitNode;
  FTreeSelect.OnInitChildren := DoTreeSelectInitChildren;
  FTreeSelect.OnKeyDown := DoKeyDown;
  FTreeSelect.OnHotChange := DoTreeSelectHotChange;
  FTreeSelect.OnPaintText := DoTreeSelectPaintText;
  FTreeSelect.Hide;
  FOldWndProc := FTreeSelect.WindowProc;
  FTreeSelect.WindowProc := EditWndProc;

  FMemoHelp := TMemo.Create(FTree);
  FMemoHelp.Parent := ParentControl;
  FMemoHelp.Color := clInfoBk;
  FMemoHelp.Font.Color := clInfoText;
  FMemoHelp.BorderStyle := bsNone;
  FMemoHelp.BevelKind := bkFlat;
  FMemoHelp.BevelInner := bvNone;
  FMemoHelp.Hide;
end;


destructor TDataTypeEditorLink.Destroy;
begin
  inherited;
  FreeAndNil(FTreeSelect);
  FreeAndNil(FMemoHelp);
  if FFinalKeyDown = VK_TAB then begin
    SendMessage(FTree.Handle, WM_KEYDOWN, FFinalKeyDown, 0);
    SendMessage(FTree.Handle, WM_KEYDOWN, VK_F2, 0);
  end;
end;


function TDataTypeEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  NodeText: WideString;
  dt: TDatatype;
  CatNode, TypeNode: PVirtualNode;
begin
  Result := not FStopping;
  if not Result then
    Exit;
  FNode := Node;
  FColumn := Column;
  FTree.GetTextInfo(FNode, FColumn, FTreeSelect.Font, FTextBounds, NodeText);
  // Highlighted cell has white font, fix that
  FTreeSelect.Font.Color := FTree.Font.Color;

  // Find and select current datatype in tree
  dt := GetDataTypeByName(NodeText);
  CatNode := FTreeSelect.GetFirst;
  while Assigned(CatNode) do begin
    if CatNode.Index = Cardinal(dt.Category) then begin
      TypeNode := FTreeSelect.GetFirstChild(CatNode);
      while Assigned(TypeNode) do begin
        if FTreeSelect.Text[TypeNode, 0] = NodeText then begin
          FTreeSelect.FocusedNode := TypeNode;
          FTreeSelect.Selected[TypeNode] := True;
          break;
        end;
        TypeNode := FTreeSelect.GetNextSibling(TypeNode);
      end;
    end;
    CatNode := FTreeSelect.GetNextSibling(CatNode);
  end;
  FTreeSelect.Header.AutoFitColumns(False, smaUseColumnOption, 0, 0);
  if Assigned(FTreeSelect.FocusedNode) then
    FTreeSelect.ScrollIntoView(FTreeSelect.FocusedNode, True);
  FTreeSelect.OnFocusChanging := DoTreeSelectFocusChanging;
  FTreeSelect.OnFocusChanged := DoTreeSelectFocusChanged;
end;


function TDataTypeEditorLink.BeginEdit: Boolean;
begin
  Result := not FStopping;
  if Result then begin
    SendMessage(FTreeSelect.Parent.Handle, WM_SETREDRAW, 1, 0);
    FTreeSelect.Show;
    FTreeSelect.SetFocus;
  end;
end;


function TDataTypeEditorLink.CancelEdit: Boolean;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    FTree.CancelEditNode;
    if FTree.CanFocus then
      FTree.SetFocus;
  end;
end;


function TDataTypeEditorLink.EndEdit: Boolean;
var
  newtext: WideString;
begin
  Result := not FStopping;
  if Not Result then
    Exit;
  newtext := FTreeSelect.Text[FTreeSelect.FocusedNode, 0];
  if newtext <> FTree.Text[FNode, FColumn] then
    FTree.Text[FNode, FColumn] := newtext;
  if FTree.CanFocus then
    FTree.SetFocus;
end;


function TDataTypeEditorLink.GetBounds: TRect;
begin
  Result := FTreeSelect.BoundsRect;
end;


procedure TDataTypeEditorLink.SetBounds(R: TRect);
begin
  // Set position of tree. As the tree's parent is mainform, not listcolumns, add listcolumn's x + y positions
  FTreeSelect.SetBounds(R.Left + FTree.ClientOrigin.X,
    R.Top + FTree.ClientOrigin.Y - FTreeSelect.Header.Height - GetSystemMetrics(SM_CYMENU),
    FTreeSelect.Header.Columns[0].Width + GetSystemMetrics(SM_CXVSCROLL) + 5,
    250);
end;


procedure TDataTypeEditorLink.ProcessMessage(var Message: TMessage);
begin
  FTreeSelect.WindowProc(Message);
end;


procedure TDataTypeEditorLink.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FFinalKeyDown := Key;
  case Key of
    // Cancel by Escape
    VK_ESCAPE: FTree.CancelEditNode;
    // Apply changes and end editing by [Ctrl +] Enter or Tab
    VK_RETURN, VK_TAB: FTree.EndEditNode;
  end;
end;


procedure TDataTypeEditorLink.EditWndProc(var Message: TMessage);
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


procedure TDataTypeEditorLink.DoTreeSelectInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  // First level nodes always expanded
  if Sender.GetNodeLevel(Node) = 0 then
    InitialStates := InitialStates + [ivsExpanded, ivsHasChildren];
end;


procedure TDataTypeEditorLink.DoTreeSelectInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  i: Integer;
begin
  // Tell number of datatypes per category
  ChildCount := 0;
  if Sender.GetNodeLevel(Node) = 0 then for i:=Low(Datatypes) to High(Datatypes) do begin
    if Datatypes[i].Category = DatatypeCategories[Node.Index].Index then
      Inc(ChildCount);
  end;
end;


procedure TDataTypeEditorLink.DoTreeSelectGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  i: Integer;
  Counter: Cardinal;
begin
  // Get cell text
  case Sender.GetNodeLevel(Node) of
    0: CellText := DatatypeCategories[Node.Index].Name;
    1: begin
         Counter := 0;
         for i:=Low(Datatypes) to High(Datatypes) do begin
           if Datatypes[i].Category = DatatypeCategories[Node.Parent.Index].Index then begin
             Inc(Counter);
             if Counter = Node.Index+1 then begin
               CellText := Datatypes[i].Name;
               break;
             end;
           end;
         end;
       end;
  end;
end;


procedure TDataTypeEditorLink.DoTreeSelectHotChange(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode);
var
  R: TRect;
  NodeText: WideString;
  bmp: TBitMap;
begin
  // Display help box for hovered datatype
  FMemoHelp.Clear;
  if Assigned(NewNode) and (Sender.GetNodeLevel(NewNode) = 1) then begin
    R := FTreeSelect.GetDisplayRect(NewNode, 0, False);
    NodeText := FTreeSelect.Text[NewNode, 0];
    FMemoHelp.Width := Min(250, FTreeSelect.Left);
    FMemoHelp.Left := FTreeSelect.Left - FMemoHelp.Width + (Integer(FTreeSelect.Indent) Div 2);
    FMemoHelp.Top := FTreeSelect.Top + R.Top + 3;
    FMemoHelp.Text := GetDatatypeByName(NodeText).Description;
    // Calc height of memo
    bmp := TBitMap.Create;
    bmp.Canvas.Font.Assign(FMemoHelp.Font);
    R := Rect(0, 0, FMemoHelp.Width, 0);
    DrawText(bmp.Canvas.Handle, PChar(FMemoHelp.Text), Length(FMemoHelp.Text), R, DT_WORDBREAK or DT_CALCRECT);
    FreeAndNil(bmp);
    FMemoHelp.Height := R.Bottom + 2;
    FMemoHelp.Show;
  end;
  if FMemoHelp.GetTextLen = 0 then
    FMemoHelp.Hide;
end;


procedure TDataTypeEditorLink.DoTreeSelectPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  // Give datatype column specific color, as set in preferences
  case Sender.GetNodeLevel(Node) of
    0: TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    1: if not (vsSelected in Node.States) then
      TargetCanvas.Font.Color := DatatypeCategories[Node.Parent.Index].Color;
  end;
end;


procedure TDataTypeEditorLink.DoTreeSelectFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode:
    PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  // Allow only 2nd level datatypes to be focused, not their category
  Allowed := Sender.GetNodeLevel(NewNode) = 1;
end;

procedure TDataTypeEditorLink.DoTreeSelectFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  // Datatype selected - end editing
  FTree.EndEditNode;
end;


end.
