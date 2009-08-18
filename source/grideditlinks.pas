unit grideditlinks;

// The editor links, instanciated by VirtualTree.CreateEditor

interface

uses Windows, Forms, Graphics, messages, VirtualTrees, texteditor, bineditor, ComCtrls, SysUtils, Classes,
  mysql_structures, helpers, TntStdCtrls, WideStrings, StdCtrls, ExtCtrls, TntCheckLst,
  Buttons, Controls, Types, Dialogs, Mask, MaskUtils, DateUtils ;

type
  TBaseGridEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FParentForm: TWinControl;      // A back reference to the main form
    FTree: TVirtualStringTree;        // A back reference to the tree calling.
    FNode: PVirtualNode;              // The node to be edited.
    FColumn: TColumnIndex;            // The column of the node.
    FCellText: WideString;            // Original cell text value
    FCellFont: TFont;                 // Cosmetic
    FCellBackground: TColor;
    FMainControl: TWinControl;        // The editor's most important component
    FStopping: Boolean;               // Set to True when the edit link requests stopping the edit action.
    FLastKeyDown: Integer;            // Set in OnKeyDown on the editor's main control
    FOldWindowProc: TWndMethod;       // Temporary switched to TempWindowProc to be able to catch Tab key
    procedure TempWindowProc(var Message: TMessage);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEndEdit(Sender: TObject);
    procedure DoCancelEdit(Sender: TObject);
    function GetCellRect(InnerTextBounds: Boolean): TRect;
  public
    Datatype: TDatatypeIndex;                        // The data type of the cell being edited. Mostly used in data grids.
    constructor Create; overload;                    // The original constructor, not used any more, throws an exception if you do
    constructor Create(Tree: TVirtualStringTree); overload; virtual; // The right constructor, we need the Tree reference
    destructor Destroy; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall; abstract;
    function EndEditHelper(NewText: WideString): Boolean;
    function GetBounds: TRect; virtual; stdcall;     // Normally useless and unused
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall; abstract;
  end;

  THexEditorLink = class(TBaseGridEditorLink)
  private
    FForm: TfrmBinEditor;
  public
    MaxLength: Integer;
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
  end;

  TDateTimeEditorLink = class(TBaseGridEditorLink)
  private
    FPanel: TPanel;
    FMaskEdit: TMaskEdit;
    FTimer: TTimer;
    FModifyOffset: Integer;
    FTimerCalls: Integer;
    FUpDown: TUpDown;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
    procedure UpDownMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnTimer(Sender: TObject);
    procedure ModifyDate(Offset: Integer);
  public
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
  end;

  TEnumEditorLink = class(TBaseGridEditorLink)
  private
    FCombo: TTnTComboBox;
  public
    ValueList: TWideStringList;
    AllowCustomText: Boolean;
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
  end;

  TSetEditorLink = class(TBaseGridEditorLink)
  private
    FPanel: TPanel;
    FCheckList: TTNTCheckListBox;
    FBtnOK, FBtnCancel: TButton;
  public
    ValueList: TWideStringList;
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
  end;

  // Inplace editor with button
  TInplaceEditorLink = class(TBaseGridEditorLink)
  private
    FPanel: TPanel;
    FEdit: TTntEdit;
    FButton: TButton;
    FTextEditor: TfrmTextEditor;
    FMaxLength: Integer;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonClick(Sender: TObject);
  public
    ButtonVisible: Boolean;
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function CancelEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
    property MaxLength: Integer read FMaxLength write FMaxLength;
  end;

  TColumnDefaultType = (cdtText, cdtTextUpdateTS, cdtNull, cdtNullUpdateTS, cdtCurTS, cdtCurTSUpdateTS, cdtAutoInc);
  TColumnDefaultEditorLink = class(TBaseGridEditorLink)
  private
    FPanel: TPanel;
    FRadioText, FRadioNULL, FRadioCurTS, FRadioAutoInc: TRadioButton;
    FCheckCurTS: TCheckbox;
    FMemoText: TTNTMemo;
    FBtnOK, FBtnCancel: TButton;
    procedure RadioClick(Sender: TObject);
    procedure TextChange(Sender: TObject);
  public
    DefaultType: TColumnDefaultType;
    DefaultText: WideString;
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
  end;

  TDataTypeEditorLink = class(TBaseGridEditorLink)
  private
    FTreeSelect: TVirtualStringTree;
    FMemoHelp: TMemo;
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
  public
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
  end;

function GetColumnDefaultType(var Text: WideString): TColumnDefaultType;
function GetColumnDefaultClause(DefaultType: TColumnDefaultType; Text: WideString): WideString;


implementation



constructor TBaseGridEditorLink.Create;
begin
  raise Exception.Create('Wrong constructor called: ' + Self.ClassName + '.Create.' + CRLF +
    'Instead, please call the overloaded version ' + Self.ClassName + '.Create(VirtualStringTree).');
end;

constructor TBaseGridEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create;
  FTree := Tree;
  // Enable mouse scrolling, plus ensure the editor component
  // is not partly hidden when it pops up in a bottom cell
  FParentForm := GetParentForm(FTree);
  // Avoid flicker
  FParentForm.Repaint;
  SendMessage(FParentForm.Handle, WM_SETREDRAW, 0, 0);
end;

destructor TBaseGridEditorLink.Destroy;
begin
  inherited;
  if FLastKeyDown = VK_TAB then begin
    SendMessage(FTree.Handle, WM_KEYDOWN, FLastKeyDown, 0);
    SendMessage(FTree.Handle, WM_KEYDOWN, VK_F2, 0);
  end;
end;

function TBaseGridEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  FCellTextBounds: TRect;
begin
  Result := not FStopping;
  if not Result then
    Exit;
  FNode := Node;
  FColumn := Column;
  FCellFont := TFont.Create;
  FTree.GetTextInfo(FNode, FColumn, FCellFont, FCellTextBounds, FCellText);
  FCellFont.Color := DatatypeCategories[Integer(Datatypes[Integer(Datatype)].Category)].Color;
  FCellBackground := FTree.Header.Columns[FColumn].Color;
  if Assigned(FMainControl) then begin
    FOldWindowProc := FMainControl.WindowProc;
    FMainControl.WindowProc := TempWindowProc;
  end;
  // Adjust editor position and allow repainting mainform  
  SetBounds(FCellTextBounds);
  SendMessage(FParentForm.Handle, WM_SETREDRAW, 1, 0);
end;

function TBaseGridEditorLink.BeginEdit: Boolean;
begin
  Result := not FStopping;
end;

function TBaseGridEditorLink.CancelEdit: Boolean;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    FTree.CancelEditNode;
    if FTree.CanFocus then
      FTree.SetFocus;
  end;
end;

function TBaseGridEditorLink.EndEditHelper(NewText: WideString): Boolean;
begin
  Result := not FStopping;
  if FStopping then Exit;
  FStopping := True;
  if NewText <> FCellText then
    FTree.Text[FNode, FColumn] := NewText;
  if FTree.CanFocus and (FLastKeyDown <> VK_TAB) then
    FTree.SetFocus;
end;

procedure TBaseGridEditorLink.TempWindowProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_CHAR: // Catch hotkeys
      if not (TWMChar(Message).CharCode = VK_TAB) then
        FOldWindowProc(Message);
    WM_GETDLGCODE: // "WantTabs" mode for main control
      Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS or DLGC_WANTTAB;
    else
      FOldWindowProc(Message);
  end;
end;

procedure TBaseGridEditorLink.ProcessMessage(var Message: TMessage);
begin
  if Assigned(FMainControl) then
    FMainControl.WindowProc(Message);
end;

function TBaseGridEditorLink.GetBounds: TRect; stdcall;
begin
  // Only important if the editor resizes itself.
  Result := Rect(0, 0, 0, 0);
end;

function TBaseGridEditorLink.GetCellRect(InnerTextBounds: Boolean): TRect;
var
  Text: WideString;
  CellBounds, TextBounds: TRect;
  Ghosted: Boolean;
  ImageIndex: Integer;
  f: TFont;
begin
  // Return the cell's rectangle, relative to the parent form.
  f := TFont.Create;
  FTree.GetTextInfo(FNode, FColumn, f, TextBounds, Text);
  CellBounds := FTree.GetDisplayRect(FNode, FColumn, False);

  Inc(CellBounds.Left, Integer(FTree.GetNodeLevel(FNode)) * (Integer(FTree.Indent)+FTree.TextMargin));
  if (toShowRoot in FTree.TreeOptions.PaintOptions)
    and (FColumn = FTree.Header.MainColumn) then begin
    // Reserve space for plus or minus button
    Inc(CellBounds.Left, FTree.Indent);
  end;
  if Assigned(FTree.Images) and Assigned(FTree.OnGetImageIndex) then begin
    // Reserve space for image
    ImageIndex := -1;
    FTree.OnGetImageIndex(FTree, FNode, ikNormal, FColumn, Ghosted, ImageIndex);
    if ImageIndex > -1 then
      Inc(CellBounds.Left, FTree.Images.Width+2);
  end;
  TextBounds.Left := CellBounds.Left + 2*FTree.TextMargin;

  if InnerTextBounds then begin
    // Inner bounds are considered to be relative to the outer cell bounds
    Result := Rect(TextBounds.Left-CellBounds.Left,
      TextBounds.Top-CellBounds.Top,
      CellBounds.Right-CellBounds.Left, // Far right edge of cell, not of text
      CellBounds.Bottom-CellBounds.Top
      );
  end else begin
    // Recalculate top left corner of rectangle, so it is relative to the parent form (which is FParentForm)
    Result := CellBounds;
    OffsetRect(Result,
      FTree.ClientOrigin.X - FParentForm.ClientOrigin.X,
      FTree.ClientOrigin.Y - FParentForm.ClientOrigin.Y
      );
    Dec(Result.Bottom, 1);
  end;
end;

procedure TBaseGridEditorLink.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FLastKeyDown := Key;
  case Key of
    // Cancel by Escape
    VK_ESCAPE: FTree.CancelEditNode;
    // Apply changes and end editing by [Ctrl +] Enter or Tab
    VK_RETURN, VK_TAB: FTree.EndEditNode;
  end;
end;

procedure TBaseGridEditorLink.DoEndEdit(Sender: TObject);
begin
  FTree.EndEditNode;
end;

procedure TBaseGridEditorLink.DoCancelEdit(Sender: TObject);
begin
  FTree.CancelEditNode;
end;




constructor THexEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create(Tree);
end;

destructor THexEditorLink.Destroy;
begin
  inherited;
  FForm.Close;
  FreeAndNil(FForm);
end;


function THexEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if not Result then
    Exit;

  // Create the text editor form
  FForm := TfrmBinEditor.Create(Ftree);
  FForm.SetFont(FCellFont);
  FForm.SetText(FCellText);
  FForm.SetMaxLength(MaxLength);
end;


function THexEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := inherited BeginEdit;
  if Result then
    FForm.ShowModal;
end;


function THexEditorLink.EndEdit: Boolean; stdcall;
begin
  Result := EndEditHelper(FForm.GetText);
end;


procedure THexEditorLink.SetBounds(R: TRect); stdcall;
begin
  // Not in use, form's position is centered on mainform
end;



{ DateTime editor }

constructor TDateTimeEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create(Tree);

  FPanel := TPanel.Create(FParentForm);
  FPanel.Parent := FParentForm;
  FPanel.Hide;
  FPanel.ParentBackground := False;
  FPanel.BevelOuter := bvNone;
  FPanel.OnExit := DoEndEdit;

  FMaskEdit := TMaskEdit.Create(FPanel);
  FMaskEdit.Parent := FPanel;
  FMaskEdit.ParentColor := True;
  FMaskEdit.BorderStyle := bsNone;
  FMaskEdit.OnKeyDown := DoKeyDown;
  FMaskEdit.OnKeyUp := DoKeyUp;
  FMainControl := FMaskEdit;

  FUpDown := TUpDown.Create(FPanel);
  FUpDown.Parent := FPanel;
  FUpDown.OnChangingEx := UpDownChangingEx;
  FUpDown.OnMouseUp := UpDownMouseUp;

  FTimer := TTimer.Create(FMaskEdit);
  FTimer.Interval := 50;
  FTimer.OnTimer := DoOnTimer;
  FTimer.Enabled := False;
end;


destructor TDateTimeEditorLink.Destroy;
begin
  OpenRegistry;
  Mainreg.WriteInteger(REGPREFIX_DATEEDITOR_CURSOR+IntToStr(Integer(Datatype)), FMaskEdit.SelStart);
  FreeAndNil(FTimer);
  FreeAndNil(FUpDown);
  FreeAndNil(FMaskEdit);
  FreeAndNil(FPanel);
  inherited;
end;


function TDateTimeEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := inherited BeginEdit;
  if Result then begin
    FPanel.Show;
    FMaskEdit.SetFocus;
    // Focus very last segment of date
    FMaskEdit.SelStart := GetRegValue(REGPREFIX_DATEEDITOR_CURSOR+IntToStr(Integer(Datatype)), Length(FMaskEdit.Text)-1);
    FMaskEdit.SelLength := 1;
  end;
end;


function TDateTimeEditorLink.EndEdit: Boolean;
begin
  Result := EndEditHelper(FMaskEdit.Text);
end;


function TDateTimeEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  MinColWidth: Integer;
  CellText: WideString;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if not Result then
    Exit;
  case Datatype of
    dtDate: FMaskEdit.EditMask := '0000-00-00;1; ';
    dtDatetime, dtTimestamp: FMaskEdit.EditMask := '0000-00-00 00\:00\:00;1; ';
    dtTime: FMaskEdit.EditMask := '00\:00\:00;1; ';
    //dtYear??
  end;
  CellText := FCellText;
  if CellText = '' then case Datatype of
    dtDate: CellText := DateToStr(Now);
    dtDatetime, dtTimestamp: CellText := DateTimeToStr(Now);
    dtTime: CellText := TimeToStr(Now);
  end;
  FMaskEdit.Text := CellText;
  FMaskEdit.Font.Assign(FCellFont);
  FPanel.Color := FCellBackground;
  // Auto-enlarge current tree column so the text in the edit is not cut
  MinColWidth := FTree.Canvas.TextWidth(CellText) + FTree.TextMargin + FUpDown.Width + 5;
  if FTree.Header.Columns[FColumn].Width < MinColWidth then
    FTree.Header.Columns[FColumn].Width := MinColWidth;
end;


procedure TDateTimeEditorLink.SetBounds(R: TRect); stdcall;
var
  EditRect: TRect;
  OldSelStart, OldSelLen: Integer;
begin
  FPanel.BoundsRect := GetCellRect(False);

  FUpDown.Left := FPanel.Width - FUpDown.Width;
  FUpDown.Height := FPanel.Height;

  EditRect := GetCellRect(True);
  EditRect.Right := FUpDown.Left;
  FMaskEdit.BoundsRect := EditRect;

  OldSelStart := FMaskEdit.SelStart;
  OldSelLen := FMaskEdit.SelLength;
  FMaskEdit.SelStart := 0;
  FMaskEdit.SelStart := OldSelStart;
  FMaskEdit.SelLength := OldSelLen;
end;


procedure TDateTimeEditorLink.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited DoKeyDown(Sender, Key, Shift);
  if (Key in [VK_UP, VK_DOWN]) and (not FTimer.Enabled) then begin
    if Key = VK_UP then FModifyOffset := 1
    else FModifyOffset := -1;
    FTimerCalls := 0;
    DoOnTimer(Sender);
    FTimer.Enabled := True;
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
          8..10: d := IncDay(d, Offset);
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
          17..19: dt := IncSecond(dt, Offset);
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
          6..8: t := IncSecond(t, Offset);
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

constructor TEnumEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create(Tree);
  AllowCustomText := False;
  FCombo := TTnTComboBox.Create(FParentForm);
  FCombo.Hide;
  FCombo.Parent := FParentForm;
  FCombo.OnKeyDown := DoKeyDown;
  FCombo.OnExit := DoEndEdit;
  ValueList := TWideStringList.Create;
  FMainControl := FCombo;
end;


destructor TEnumEditorLink.Destroy;
begin
  FCombo.Free;
  inherited;
end;


function TEnumEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := inherited BeginEdit;
  if Result then begin
    FCombo.Show;
    FCombo.SetFocus;
  end;
end;


function TEnumEditorLink.EndEdit: Boolean; stdcall;
begin
  Result := EndEditHelper(FCombo.Text);
end;


function TEnumEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  i: Integer;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then begin
    for i := 0 to ValueList.Count - 1 do
      FCombo.Items.Add(ValueList[i]);
    if AllowCustomText then begin
      FCombo.Style := csDropDown;
      FCombo.Text := FCellText;
    end else begin
      // Set style to OwnerDraw, otherwise we wouldn't be able to adjust the combo's height
      FCombo.Style := csOwnerDrawFixed;
      FCombo.ItemIndex := FCombo.Items.IndexOf(FCellText);
    end;
  end;
end;


procedure TEnumEditorLink.SetBounds(R: TRect); stdcall;
begin
  FCombo.BoundsRect := GetCellRect(False);
end;




{ SET editor }

constructor TSetEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create(Tree);
  ValueList := TWideStringList.Create;

  FPanel := TPanel.Create(FParentForm);
  FPanel.Hide;
  FPanel.Parent := FParentForm;
  FPanel.ParentBackground := False;
  FPanel.OnExit := DoEndEdit;

  FCheckList := TTNTCheckListBox.Create(FPanel);
  FCheckList.Parent := FPanel;
  FCheckList.OnKeyDown := DoKeyDown;
  FMainControl := FCheckList;

  FBtnOk := TButton.Create(FPanel);
  FBtnOk.Parent := FPanel;
  FBtnOk.Caption := 'OK';
  FBtnOk.OnClick := DoEndEdit;

  FBtnCancel := TButton.Create(FPanel);
  FBtnCancel.Parent := FPanel;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.OnClick := DoCancelEdit;
end;


destructor TSetEditorLink.Destroy;
begin
  FreeAndNil(FPanel);
  inherited;
end;


function TSetEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := inherited BeginEdit;
  if Result then begin
    FPanel.Show;
    FCheckList.SetFocus;
  end;
end;


function TSetEditorLink.EndEdit: Boolean; stdcall;
var
  newtext: WideString;
  i: Integer;
begin
  Result := not FStopping;
  if FStopping then Exit;
  newText := '';
  for i := 0 to FCheckList.Items.Count - 1 do
    if FCheckList.Checked[i] then newText := newText + FCheckList.Items[i] + ',';
  Delete(newText, Length(newText), 1);
  Result := EndEditHelper(newText);
end;


function TSetEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  SelValues: TWideStringlist;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if not Result then
    Exit;

  FCheckList.Font.Assign(FCellFont);
  FCheckList.Items.Assign(ValueList);
  SelValues := TWideStringList.Create;
  SelValues.Delimiter := ',';
  SelValues.StrictDelimiter := True;
  SelValues.DelimitedText := FCellText;
  ToggleCheckListBox(FCheckList, True, SelValues);
end;


procedure TSetEditorLink.SetBounds(R: TRect); stdcall;
const
  margin = 3;
begin
  R := GetCellRect(False);
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


{ TInplaceEditorLink }

constructor TInplaceEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create(Tree);
  ButtonVisible := false;
  FTextEditor := nil;

  FPanel := TPanel.Create(FParentForm);
  FPanel.Parent := FParentForm;
  FPanel.Hide;
  FPanel.ParentBackground := False;
  FPanel.BevelOuter := bvNone;
  FPanel.OnExit := DoEndEdit;

  FEdit := TTntEdit.Create(FPanel);
  FEdit.Parent := FPanel;
  FEdit.ParentColor := True;
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := DoKeyDown;
  FMainControl := FEdit;

  FButton := TButton.Create(FPanel);
  FButton.Parent := FPanel;
  FButton.TabStop := False;
  FButton.Caption := '…';
  FButton.Hint := 'Edit text in popup editor ...';
  FButton.ShowHint := True;
  FButton.OnClick := ButtonClick;
end;

destructor TInplaceEditorLink.Destroy;
begin
  if Assigned(FTextEditor) then
    FTextEditor.Release;
  FEdit.Free;
  FButton.Free;
  FPanel.Free;
  inherited;
end;

function TInplaceEditorLink.BeginEdit: Boolean;
begin
  Result := inherited BeginEdit;
  if Result then begin
    FButton.Visible := ButtonVisible;
    SetBounds(Rect(0, 0, 0, 0));
    FPanel.Show;
    FEdit.SetFocus;
  end;
end;

function TInplaceEditorLink.CancelEdit: Boolean;
begin
  Result := inherited CancelEdit;
  if Result then begin
    if Assigned(FTextEditor) then
      FTextEditor.Close;
  end;
end;

function TInplaceEditorLink.EndEdit: Boolean;
var
  NewText: WideString;
begin
  Result := not FStopping;
  if FStopping then Exit;
  if Assigned(FTextEditor) then begin
    NewText := FTextEditor.GetText;
    FTextEditor.Close;
  end else begin
    NewText := FEdit.Text;
  end;
  Result := EndEditHelper(NewText);
end;

procedure TInplaceEditorLink.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited DoKeyDown(Sender, Key, Shift);
  if Key = VK_F2 then
    ButtonClick(FButton);
end;

procedure TInplaceEditorLink.ButtonClick(Sender: TObject);
begin
  if not FButton.Visible then Exit; // Button was invisible, but hotkey was pressed
  FTextEditor := TfrmTextEditor.Create(FTree);
  FTextEditor.SetFont(FEdit.Font);
  FTextEditor.SetText(FEdit.Text);
  FTextEditor.Modified := FEdit.Text <> FCellText;
  FTextEditor.SetMaxLength(Self.FMaxLength);
  FTextEditor.ShowModal;
end;

function TInplaceEditorLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if not Result then
    Exit;

  FEdit.Font.Assign(FCellFont);
  FEdit.Font.Color := clWindowText;
  FPanel.Color := FCellBackground;
  if ScanNulChar(FCellText) then begin
    MessageDlg(SContainsNulCharGrid, mtInformation, [mbOK], 0);
    FEdit.Text := RemoveNulChars(FCellText);
  end else
    FEdit.Text := FCellText;
end;

procedure TInplaceEditorLink.SetBounds(R: TRect);
begin
  if not FStopping then begin
    // Position edit control according to cell text bounds
    FPanel.BoundsRect := GetCellRect(False);
    R := GetCellRect(True);
    if FButton.Visible then
      Dec(R.Right, 20);
    FEdit.BoundsRect := R;

    FButton.BoundsRect := Rect(FEdit.BoundsRect.Right, 0, FPanel.Width, FPanel.Height);
  end;
end;



{ Column default editor }

constructor TColumnDefaultEditorLink.Create(Tree: TVirtualStringTree);
const
  m = 3;
begin
  inherited Create(Tree);

  FPanel := TPanel.Create(FParentForm);
  FPanel.Hide;
  FPanel.Parent := FParentForm;
  FPanel.OnExit := DoEndEdit;
  FPanel.Width := 200;
  FPanel.ParentBackground := False;
  FPanel.Color := clWindow;
  FPanel.BevelKind := bkFlat;
  FPanel.BevelOuter := bvNone;
  FMainControl := FPanel;

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
  FBtnOk.OnClick := DoEndEdit;
  FBtnOk.Default := True;
  FBtnOk.Caption := 'OK';

  FBtnCancel := TButton.Create(FPanel);
  FBtnCancel.Parent := FPanel;
  FBtnCancel.Top := FBtnOk.Top;
  FBtnCancel.Width := FBtnOk.Width;
  FBtnCancel.Left := FBtnOk.Left + FBtnOk.Width + m;
  FBtnCancel.OnClick := DoCancelEdit;
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
end;


destructor TColumnDefaultEditorLink.Destroy;
begin
  FPanel.Free;
  inherited;
end;


function TColumnDefaultEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
begin
  inherited PrepareEdit(Tree, Node, Column);

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


procedure TColumnDefaultEditorLink.SetBounds(R: TRect); stdcall;
var
  CellRect: TRect;
begin
  CellRect := GetCellRect(False);
  FPanel.Left := CellRect.Left;
  FPanel.Top := CellRect.Top;
end;


function TColumnDefaultEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FPanel.Show;
    if FRadioText.Checked then FRadioText.SetFocus
    else if FRadioNull.Checked then FRadioNull.SetFocus
    else if FRadioCurTS.Checked then FRadioCurTS.SetFocus
    else if FRadioAutoInc.Checked then FRadioAutoInc.SetFocus;
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


function GetColumnDefaultType(var Text: WideString): TColumnDefaultType;
begin
  Result := TColumnDefaultType(MakeInt(Copy(Text, 1, 1)));
  Text := Copy(Text, 2, Length(Text)-1);
end;


function GetColumnDefaultClause(DefaultType: TColumnDefaultType; Text: WideString): WideString;
begin
  case DefaultType of
    cdtText:           Result := 'DEFAULT '+esc(Text);
    cdtTextUpdateTS:   Result := 'DEFAULT '+esc(Text)+' ON UPDATE CURRENT_TIMESTAMP';
    cdtNull:           Result := 'DEFAULT NULL';
    cdtNullUpdateTS:   Result := 'DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP';
    cdtCurTS:          Result := 'DEFAULT CURRENT_TIMESTAMP';
    cdtCurTSUpdateTS:  Result := 'DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP';
    cdtAutoInc:        Result := 'AUTO_INCREMENT';
  end;
end;



{ Datatype selector }
constructor TDataTypeEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create(Tree);

  FTreeSelect := TVirtualStringTree.Create(FParentForm);
  FTreeSelect.Hide;
  FTreeSelect.TreeOptions.PaintOptions := FTreeSelect.TreeOptions.PaintOptions
    - [toShowTreeLines, toShowButtons, toShowRoot]
    + [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
  FTreeSelect.TreeOptions.SelectionOptions := FTreeSelect.TreeOptions.SelectionOptions
    + [toFullRowSelect];
  FTreeSelect.Header.Columns.Add;
  FTreeSelect.Parent := FParentForm;
  FTreeSelect.TextMargin := 0;
  FTreeSelect.BorderStyle := bsNone;
  FTreeSelect.BevelKind := bkFlat;
  FTreeSelect.BevelInner := bvNone;
  FTreeSelect.IncrementalSearch := isAll;
  FTreeSelect.RootNodeCount := Length(DatatypeCategories);
  FTreeSelect.OnGetText := DoTreeSelectGetText;
  FTreeSelect.OnInitNode := DoTreeSelectInitNode;
  FTreeSelect.OnInitChildren := DoTreeSelectInitChildren;
  FTreeSelect.OnKeyDown := DoKeyDown;
  FTreeSelect.OnHotChange := DoTreeSelectHotChange;
  FTreeSelect.OnPaintText := DoTreeSelectPaintText;
  FTreeSelect.OnExit := DoEndEdit;
  FMainControl := FTreeSelect;

  FMemoHelp := TMemo.Create(FParentForm);
  FMemoHelp.Hide;
  FMemoHelp.Parent := FParentForm;
  FMemoHelp.Color := clInfoBk;
  FMemoHelp.Font.Color := clInfoText;
  FMemoHelp.BorderStyle := bsNone;
  FMemoHelp.BevelKind := bkFlat;
  FMemoHelp.BevelInner := bvNone;
end;


destructor TDataTypeEditorLink.Destroy;
begin
  FreeAndNil(FTreeSelect);
  FreeAndNil(FMemoHelp);
  inherited;
end;


function TDataTypeEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  dt: TDatatype;
  CatNode, TypeNode: PVirtualNode;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if not Result then
    Exit;
  // Just use font name and size, avoid bold style and white color
  FTreeSelect.Font.Name := FCellFont.Name;
  FTreeSelect.Font.Size := FCellFont.Size;

  // Find and select current datatype in tree
  dt := GetDataTypeByName(FCellText);
  CatNode := FTreeSelect.GetFirst;
  while Assigned(CatNode) do begin
    if CatNode.Index = Cardinal(dt.Category) then begin
      TypeNode := FTreeSelect.GetFirstChild(CatNode);
      while Assigned(TypeNode) do begin
        if FTreeSelect.Text[TypeNode, 0] = FCellText then begin
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
  FTreeSelect.OnClick := DoEndEdit;
end;


function TDataTypeEditorLink.BeginEdit: Boolean;
begin
  Result := inherited BeginEdit;
  if Result then begin
    FTreeSelect.Show;
    FTreeSelect.SetFocus;
  end;
end;


function TDataTypeEditorLink.EndEdit: Boolean;
begin
  if Assigned(FTreeSelect.FocusedNode) then
    Result := EndEditHelper(FTreeSelect.Text[FTreeSelect.FocusedNode, 0])
  else
    Result := FTree.CancelEditNode;
end;


procedure TDataTypeEditorLink.SetBounds(R: TRect);
var
  CellRect: TRect;
begin
  // Set position of tree. As the tree's parent is mainform, not listcolumns, add listcolumn's x + y positions
  CellRect := GetCellRect(False);
  FTreeSelect.SetBounds(CellRect.Left,
    CellRect.Top,
    FTreeSelect.Header.Columns[0].Width + GetSystemMetrics(SM_CXVSCROLL) + 5,
    250);
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
    R := Rect(0, 0, FMemoHelp.Width-10, 0);
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
var
  JumpToNode: PVirtualNode;
begin
  // Allow only 2nd level datatypes to be focused, not their category
  Allowed := Sender.GetNodeLevel(NewNode) = 1;
  if not Allowed then begin
    JumpToNode := nil;
    if FLastKeyDown = VK_UP then
      JumpToNode := Sender.GetPrevious(NewNode)
    else if FLastKeyDown = VK_DOWN then
      JumpToNode := Sender.GetNext(NewNode);
    if Assigned(JumpToNode) then
      Sender.FocusedNode := JumpToNode;
  end;
end;


end.
