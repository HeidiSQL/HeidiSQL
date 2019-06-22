unit grideditlinks;

// The editor links, instanciated by VirtualTree.CreateEditor

interface

uses
  Windows, Forms, Graphics, Messages, VirtualTrees, ComCtrls, SysUtils, Classes,
  StdCtrls, ExtCtrls, CheckLst, Controls, Types, Dialogs, Menus, Mask, DateUtils, Math,
  dbconnection, dbstructures, apphelpers, texteditor, bineditor, gnugettext,
  StrUtils, System.UITypes, SynRegExpr, Vcl.Themes;

type
  // Radio buttons and checkboxes which do not pass <Enter> key to their parent control
  // so a OnKeyDown event using <Enter> has the chance to end editing.
  TAllKeysRadioButton = class(TRadioButton)
    procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
  end;
  TAllKeysCheckBox = class(TCheckBox)
    procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
  end;

  TBaseGridEditorLink = class(TInterfacedObject, IVTEditLink)
  private
    FParentForm: TWinControl;      // A back reference to the main form
    FTree: TVirtualStringTree;        // A back reference to the tree calling.
    FNode: PVirtualNode;              // The node to be edited.
    FColumn: TColumnIndex;            // The column of the node.
    FCellText: String;            // Original cell text value
    FCellFont: TFont;                 // Cosmetic
    FCellBackground: TColor;
    FMainControl: TWinControl;        // The editor's most important component
    FStopping: Boolean;               // Set to True when the edit link requests stopping the edit action.
    FLastKeyDown: Integer;            // Set in OnKeyDown on the editor's main control
    FLastShiftState: TShiftState;
    FOldWindowProc: TWndMethod;       // Temporary switched to TempWindowProc to be able to catch Tab key
    FTableColumn: TTableColumn;
    FModified: Boolean;
    FBeginEditTime: Cardinal;
    procedure TempWindowProc(var Message: TMessage);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEndEdit(Sender: TObject);
    procedure DoCancelEdit(Sender: TObject);
    function GetCellRect(InnerTextBounds: Boolean): TRect;
  public
    property TableColumn: TTableColumn read FTableColumn write FTableColumn; // The table column of the cell being edited. Mostly used in data grids.
    constructor Create; overload;                    // The original constructor, not used any more, throws an exception if you do
    constructor Create(Tree: TVirtualStringTree); overload; virtual; // The right constructor, we need the Tree reference
    destructor Destroy; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall; abstract;
    function EndEditHelper(NewText: String): Boolean;
    function GetBounds: TRect; virtual; stdcall;     // Normally useless and unused
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall; abstract;
  end;

  THexEditorLink = class(TBaseGridEditorLink)
  private
    FForm: TfrmBinEditor;
  public
    MaxLength: Integer;
    TitleText: String;
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function CancelEdit: Boolean; override;
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
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    procedure UpDownMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnTimer(Sender: TObject);
    procedure ModifyDate(Offset: Integer);
    procedure TextChange(Sender: TObject);
    function MicroSecondsPrecision: Integer;
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
    FCombo: TComboBox;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    ValueList, DisplayList: TStringList;
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
    FCheckList: TCheckListBox;
    FBtnOK, FBtnCancel: TButton;
    FEndTimer: TTimer;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  public
    ValueList: TStringList;
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
    FEdit: TEdit;
    FButton: TButton;
    FTextEditor: TfrmTextEditor;
    FMaxLength: Integer;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonClick(Sender: TObject);
  public
    ButtonVisible: Boolean;
    TitleText: String;
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function CancelEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
    property MaxLength: Integer read FMaxLength write FMaxLength;
  end;

  TColumnDefaultEditorLink = class(TBaseGridEditorLink)
  private
    FPanel: TPanel;
    FRadioNothing, FRadioText, FRadioNULL, FRadioExpression, FRadioAutoInc: TAllKeysRadioButton;
    FlblOnUpdate: TLabel;
    FTextEdit: TButtonedEdit;
    FTextDropDown: TPopupMenu;
    FExpressionEdit: TComboBox;
    FOnUpdateEdit: TComboBox;
    FBtnOK, FBtnCancel: TButton;
    FEndTimer: TTimer;
    procedure RadioClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditDropDownClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  public
    DefaultType, OnUpdateType: TColumnDefaultType;
    DefaultText, OnUpdateText: String;
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
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
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
    constructor Create(Tree: TVirtualStringTree); override;
    destructor Destroy; override;
    function BeginEdit: Boolean; override;
    function EndEdit: Boolean; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure SetBounds(R: TRect); override;
  end;



implementation


procedure TAllKeysRadioButton.WMGetDlgCode(var Msg: TMessage);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;


procedure TAllKeysCheckBox.WMGetDlgCode(var Msg: TMessage);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;


constructor TBaseGridEditorLink.Create;
begin
  raise Exception.CreateFmt(_('Wrong constructor called: %s.%s. Instead, please call the overloaded version %s.%s.'),
    [Self.ClassName, 'Create', Self.ClassName, 'Create(VirtualStringTree)']);
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
  FModified := False;
end;

destructor TBaseGridEditorLink.Destroy;
var
  NewColumn, FirstCol, LastCol: TColumnIndex;
  NewNode: PVirtualNode;
  DoPrev: Boolean;
begin
  inherited;
  if FLastKeyDown = VK_TAB then begin
    DoPrev := ssShift in FLastShiftState;
    // Advance to next/previous visible column/node.
    NewNode := FNode;
    NewColumn := FColumn;
    FirstCol := FTree.Header.Columns.GetFirstVisibleColumn;
    LastCol := FTree.Header.Columns.GetLastVisibleColumn;
    while true do begin
      // Find a column for the current node which can be focused.
      if DoPrev then begin
        if NewColumn = FirstCol then begin
          NewColumn := LastCol;
          NewNode := FTree.GetPreviousVisible(NewNode);
        end else
          NewColumn := FTree.Header.Columns.GetPreviousColumn(NewColumn);
      end else begin
        if NewColumn = LastCol then begin
          NewColumn := FirstCol;
          NewNode := FTree.GetNextVisible(NewNode);
        end else
          NewColumn := FTree.Header.Columns.GetNextColumn(NewColumn);
      end;
      if not Assigned(NewNode) then
        Break;
      FTree.FocusedNode := NewNode;
      FTree.FocusedColumn := NewColumn;
      if not FTree.EditNode(FTree.FocusedNode, FTree.FocusedColumn) then
        FTree.SetFocus;
      Break;
    end;
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
  // Not all editors have a connection assigned, e.g. session manager tree
  if Assigned(FTableColumn) then begin
    FCellFont.Color := DatatypeCategories[FTableColumn.DataType.Category].Color;
  end;
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
  FBeginEditTime := GetTickCount;
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

function TBaseGridEditorLink.EndEditHelper(NewText: String): Boolean;
begin
  Result := not FStopping;
  if FStopping then Exit;
  FStopping := True;
  if FModified then
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
    else begin
      try
        FOldWindowProc(Message);
      except
        // EAccessViolation occurring in some cases
      end;
    end;
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
  Text: String;
  CellBounds, TextBounds: TRect;
  Ghosted: Boolean;
  ImageIndex: TImageIndex;
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
  FLastShiftState := Shift;
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
  FForm.SetTitleText(TitleText);
  FForm.SetMaxLength(MaxLength);
end;


function THexEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := inherited BeginEdit;
  if Result then
    FForm.ShowModal;
end;


function THexEditorLink.CancelEdit: Boolean;
begin
  Result := inherited CancelEdit;
  if Result then
    FForm.Close;
end;


function THexEditorLink.EndEdit: Boolean; stdcall;
begin
  FForm.Close;
  FModified := FForm.Modified;
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
  FMaskEdit.OnChange := TextChange;
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
  AppSettings.WriteInt(asDateTimeEditorCursorPos, FMaskEdit.SelStart, IntToStr(Integer(FTableColumn.DataType.Category)));
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
    FMaskEdit.SelStart := AppSettings.ReadInt(asDateTimeEditorCursorPos, IntToStr(Integer(FTableColumn.DataType.Category)));
    FMaskEdit.SelLength := 1;
  end;
end;


function TDateTimeEditorLink.EndEdit: Boolean;
begin
  Result := EndEditHelper(Trim(FMaskEdit.Text));
end;


function TDateTimeEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  MinColWidth,
  ForceTextLen: Integer;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if not Result then
    Exit;
  case FTableColumn.DataType.Index of
    dtDate:
      FMaskEdit.EditMask := '0000-00-00;1; ';
    dtDatetime, dtDatetime2, dtTimestamp, dtInt, dtBigint: begin
        if MicroSecondsPrecision > 0 then
          FMaskEdit.EditMask := '0000-00-00 00\:00\:00.'+StringOfChar('0', MicroSecondsPrecision)+';1; '
        else
          FMaskEdit.EditMask := '0000-00-00 00\:00\:00;1; ';
      end;
    dtTime: begin
        ForceTextLen := 10;
        if MicroSecondsPrecision > 0 then begin
          FMaskEdit.EditMask := '#900\:00\:00.'+StringOfChar('0', MicroSecondsPrecision)+';1; ';
          Inc(ForceTextLen, MicroSecondsPrecision + 1);
        end else
          FMaskEdit.EditMask := '#900\:00\:00;1; ';
        while Length(FCellText) < ForceTextLen do
          FCellText := ' ' + FCellText;
      end;
    dtYear:
      FMaskEdit.EditMask := '0000;1; ';
  end;
  FMaskEdit.Text := FCellText;
  FModified := False;
  FMaskEdit.Font.Assign(FCellFont);
  FPanel.Color := FCellBackground;
  // Auto-enlarge current tree column so the text in the edit is not cut
  MinColWidth := FTree.Canvas.TextWidth(FCellText) + FTree.TextMargin + FUpDown.Width + 5;
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
  NewValue: Integer; Direction: TUpDownDirection);
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
  i, MaxSeconds, MinSeconds: Int64;
  text: String;
  OldSelStart, OldSelLength,
  ms, DotPos: Integer;

  function TimeToSeconds(Str: String): Int64;
  var
    Hours: String;
    Seconds: Int64;
  begin
    Hours := Trim(Copy(Str, 1, 4));
    Result := MakeInt(Hours) * 60 * 60;
    Seconds := MakeInt(Copy(Str, 6, 2)) * 60 + MakeInt(Copy(Str, 9, 2));
    if (Result < 0) or ((Result = 0) and (Hours[1]='-')) then
      Dec(Result, Seconds)
    else
      Inc(Result, Seconds);
  end;

  function SecondsToTime(Seconds: Int64): String;
  var
    HoursNum, Minutes: Int64;
    Hours: String;
  begin
    HoursNum := Abs(Seconds div (60*60));
    Hours := IntToStr(HoursNum);
    if Length(Hours) = 1 then
      Hours := '0' + Hours;
    if Seconds < 0 then
      Hours := '-' + Hours;
    Seconds := Abs(Seconds) mod (60*60);
    Minutes := Seconds div 60;
    Seconds := Seconds mod 60;
    Result := Format('%4s:%.2u:%.2u', [Hours, Minutes, Seconds]);
  end;
begin
  try
    // Detect microseconds part of value if any
    if MicroSecondsPrecision > 0 then begin
      DotPos := Length(FMaskEdit.Text) - Pos('.', ReverseString(FMaskEdit.Text)) + 2;
      ms := MakeInt(Copy(FMaskEdit.Text, DotPos, Length(FMaskEdit.Text)));
    end else
      ms := 0;

    case FTableColumn.DataType.Index of
      dtYear: begin
        i := MakeInt(FMaskEdit.Text);
        i := i + Offset;
        text := IntToStr(i);
      end;

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

      dtDateTime, dtDateTime2, dtTimestamp, dtInt, dtBigint: begin
        dt := StrToDateTime(FMaskEdit.Text);
        case FMaskEdit.SelStart of
          0..3: dt := IncYear(dt, Offset);
          5,6: dt := IncMonth(dt, Offset);
          8,9: dt := IncDay(dt, Offset);
          11,12: dt := IncHour(dt, Offset);
          14,15: dt := IncMinute(dt, Offset);
          17..19: dt := IncSecond(dt, Offset);
          20..26: Inc(ms, Offset);
        end;
        text := DateTimeToStr(dt);
        if Length(text) = 10 then
          text := text + ' 00:00:00';
        if MicroSecondsPrecision > 0 then
          text := text + '.' + Format('%.'+IntToStr(MicroSecondsPrecision)+'d', [ms]);
      end;

      dtTime: begin
        i := TimeToSeconds(FMaskEdit.Text);
        case FMaskEdit.SelStart of
          0..3: Inc(i, Offset*60*60);
          5,6: Inc(i, Offset*60);
          8,9: Inc(i, Offset);
          10..16: Inc(ms, Offset);
        end;
        // Stop at max and min values. See http://dev.mysql.com/doc/refman/5.0/en/time.html
        MaxSeconds := 839*60*60-1;
        MinSeconds := -(MaxSeconds);
        if i > MaxSeconds then
          i := MaxSeconds;
        if i < MinSeconds then
          i := MinSeconds;
        text := SecondsToTime(i);
        if MicroSecondsPrecision > 0 then
          text := text + '.' + Format('%.'+IntToStr(MicroSecondsPrecision)+'d', [ms]);
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
    on E:EConvertError do begin
      // Ignore any DateToStr exception. Should only appear in cases where the users
      // enters invalid dates
    end else
      raise;
  end;
end;


procedure TDateTimeEditorLink.TextChange;
begin
  FModified := True;
end;


function TDateTimeEditorLink.MicroSecondsPrecision: Integer;
var
  rx: TRegExpr;
begin
  if not FTableColumn.LengthSet.IsEmpty then
    Result := MakeInt(FTableColumn.LengthSet)
  else begin
    // Find default length of supported microseconds in datatype definition
    // See dbstructures
    rx := TRegExpr.Create;
    rx.Expression := '\.([^\.]+)$';
    if rx.Exec(FTableColumn.DataType.Format) then
      Result := rx.MatchLen[1]
    else
      Result := 0;
    rx.Free;
  end;
  // No microseconds for UNIX timestamp columns
  if FTableColumn.DataType.Index in [dtInt, dtBigint] then
    Result := 0;
end;



{ Enum editor }

constructor TEnumEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create(Tree);
  AllowCustomText := False;
  FCombo := TComboBox.Create(FParentForm);
  FCombo.Hide;
  FCombo.Parent := FParentForm;
  FCombo.OnKeyDown := DoKeyDown;
  FCombo.OnExit := DoEndEdit;
  // Show some more than the default 8 items
  FCombo.DropDownCount := 16;
  ValueList := TStringList.Create;
  DisplayList := TStringList.Create;
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
var
  NewText: String;
begin
  if AllowCustomText then
    NewText := FCombo.Text
  else if (ValueList.Count > 0) and (FCombo.ItemIndex > -1) then
    NewText := ValueList[FCombo.ItemIndex]
  else
    NewText := '';
  FModified := NewText <> FCellText;
  Result := EndEditHelper(NewText);
end;


function TEnumEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  i: Integer;
  Items: TStringList;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if Result then begin
    if DisplayList.Count = ValueList.Count then
      Items := DisplayList
    else
      Items := ValueList;
    for i:=0 to Items.Count - 1 do
      FCombo.Items.Add(Items[i]);
    if AllowCustomText then begin
      FCombo.Style := csDropDown;
      FCombo.Text := FCellText;
    end else begin
      // Set style to OwnerDraw, otherwise we wouldn't be able to adjust the combo's height
      FCombo.Style := csOwnerDrawFixed;
      FCombo.ItemIndex := ValueList.IndexOf(FCellText);
    end;
  end;
end;


procedure TEnumEditorLink.SetBounds(R: TRect); stdcall;
begin
  FCombo.BoundsRect := GetCellRect(False);
end;


procedure TEnumEditorLink.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Work around a magic automatic TAB key arriving the editor if the user got
  // into this cell via TAB. Only seen for a TComboBox with style=csDropDown.
  // See issue #2809
  if (not AllowCustomText) and (GetTickCount-FBeginEditTime > 200) then
    inherited;
end;



{ SET editor }

constructor TSetEditorLink.Create(Tree: TVirtualStringTree);
begin
  inherited Create(Tree);
  ValueList := TStringList.Create;

  FPanel := TPanel.Create(FParentForm);
  FPanel.Hide;
  FPanel.Parent := FParentForm;
  FPanel.ParentBackground := False;
  FPanel.Height := 150;
  FPanel.OnExit := DoEndEdit;

  FCheckList := TCheckListBox.Create(FPanel);
  FCheckList.Parent := FPanel;
  FCheckList.OnKeyDown := DoKeyDown;
  FMainControl := FCheckList;

  FBtnOk := TButton.Create(FPanel);
  FBtnOk.Parent := FPanel;
  FBtnOk.Caption := _('OK');
  FBtnOk.OnClick := BtnOkClick;

  FBtnCancel := TButton.Create(FPanel);
  FBtnCancel.Parent := FPanel;
  FBtnCancel.Caption := _('Cancel');
  FBtnCancel.OnClick := BtnCancelClick;

  FEndTimer := TTimer.Create(FPanel);
  FEndTimer.Interval := 50;
  FEndTimer.Enabled := False;
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
  newtext: String;
  i: Integer;
begin
  Result := not FStopping;
  if FStopping then Exit;
  newText := '';
  for i := 0 to FCheckList.Items.Count - 1 do
    if FCheckList.Checked[i] then newText := newText + FCheckList.Items[i] + ',';
  Delete(newText, Length(newText), 1);
  FModified := newText <> FCellText;
  Result := EndEditHelper(newText);
end;


function TSetEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  SelValues: TStringList;
  i: Integer;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if not Result then
    Exit;

  FCheckList.Font.Assign(FCellFont);
  FCheckList.Items.Assign(ValueList);
  SelValues := TStringList.Create;
  SelValues.Delimiter := ',';
  SelValues.StrictDelimiter := True;
  SelValues.DelimitedText := FCellText;
  for i:=0 to FCheckList.Items.Count-1 do begin
    FCheckList.Checked[i] := SelValues.IndexOf(FCheckList.Items[i]) > -1;
  end;
  SelValues.Free;
end;


procedure TSetEditorLink.SetBounds(R: TRect); stdcall;
const
  margin = 5;
begin
  R := GetCellRect(False);
  FPanel.Top := R.Top;
  FPanel.Left := R.Left;
  FPanel.Width := R.Width;

  FBtnOk.Width := (FPanel.Width - 3*margin) div 2;
  FBtnOk.Left := margin;
  FBtnOk.Height := 24;
  FBtnOk.Top := FPanel.Height - 2*margin - FBtnOk.Height;

  FBtnCancel.Width := FBtnOk.Width;
  FBtnCancel.Left := 2*margin + FBtnOk.Width;
  FBtnCancel.Height := FBtnOk.Height;
  FBtnCancel.Top := FBtnOk.Top;

  FCheckList.Top := margin;
  FCheckList.Left := margin;
  FCheckList.Width := FPanel.Width - 2*margin;
  FCheckList.Height := FBtnOk.Top - margin - FCheckList.Top;
end;


procedure TSetEditorLink.BtnOkClick(Sender: TObject);
begin
  // Timer based click on OK button, to prevent crash when theming is active
  FEndTimer.OnTimer := DoEndEdit;
  FEndTimer.Enabled := True;
end;


procedure TSetEditorLink.BtnCancelClick(Sender: TObject);
begin
  // Timer based click on Cancel button, to prevent crash when theming is active
  FEndTimer.OnTimer := DoCancelEdit;
  FEndTimer.Enabled := True;
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

  FEdit := TEdit.Create(FPanel);
  FEdit.Parent := FPanel;
  FEdit.ParentColor := True;
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := DoKeyDown;
  FMainControl := FEdit;

  FButton := TButton.Create(FPanel);
  FButton.Parent := FPanel;
  FButton.TabStop := False;
  FButton.Caption := '…';
  FButton.Hint := _('Edit text in popup editor ...');
  FButton.ShowHint := True;
  FButton.OnClick := ButtonClick;
end;

destructor TInplaceEditorLink.Destroy;
begin
  if Assigned(FTextEditor) then
    FTextEditor.Release;
  if not ((csDestroying in FPanel.ComponentState) or (csCreating in FPanel.ControlState)) then
    FPanel.Free;
  inherited;
end;

function TInplaceEditorLink.BeginEdit: Boolean;
begin
  Result := inherited BeginEdit;
  if Result then begin
    FButton.Visible := ButtonVisible;
    SetBounds(Rect(0, 0, 0, 0));
    if (Length(FEdit.Text) > SIZE_KB*100) or (ScanLineBreaks(FEdit.Text) <> lbsNone) then
      ButtonClick(FTree)
    else begin
      FPanel.Show;
      FEdit.SetFocus;
    end;
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
  NewText: String;
begin
  Result := not FStopping;
  if FStopping then Exit;
  if Assigned(FTextEditor) then begin
    NewText := FTextEditor.GetText;
    FModified := FTextEditor.Modified;
    FTextEditor.Close;
  end else begin
    NewText := FEdit.Text;
    FModified := FEdit.Modified;
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
  FTextEditor.SetTitleText(TitleText);
  FTextEditor.Modified := FEdit.Modified;
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
  FEdit.Font.Color := GetThemeColor(clWindowText);
  FPanel.Color := FCellBackground;
  FEdit.Text := FCellText;
  FEdit.Modified := False;
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
  m = 5;
var
  i: Integer;
begin
  inherited Create(Tree);

  FPanel := TPanel.Create(FParentForm);
  FPanel.Hide;
  FPanel.Parent := FParentForm;
  FPanel.OnExit := DoEndEdit;
  FPanel.ParentBackground := False;
  FPanel.Color := GetThemeColor(clWindow);
  FPanel.BevelKind := bkFlat;
  FPanel.BevelOuter := bvNone;
  FPanel.DoubleBuffered := True; // Avoid flicker?
  FMainControl := FPanel;

  FRadioNothing := TAllKeysRadioButton.Create(FPanel);
  FRadioNothing.Parent := FPanel;
  FRadioNothing.Top := m;
  FRadioNothing.Left := m;
  FRadioNothing.Width := FRadioNothing.Parent.Width - 2 * FRadioNothing.Left;
  FRadioNothing.OnClick := RadioClick;
  FRadioNothing.OnKeyDown := DoKeyDown;
  FRadioNothing.Caption := _('No default value');

  FRadioText := TAllKeysRadioButton.Create(FPanel);
  FRadioText.Parent := FPanel;
  FRadioText.Top := FRadioNothing.Top + FRadioNothing.Height + m;;
  FRadioText.Left := m;
  FRadioText.Width := FRadioText.Parent.Width - 2 * FRadioText.Left;
  FRadioText.OnClick := RadioClick;
  FRadioText.OnKeyDown := DoKeyDown;
  FRadioText.Caption := _('Custom text')+':';

  FTextDropDown := TPopupMenu.Create(FPanel);

  FTextEdit := TButtonedEdit.Create(FPanel);
  FTextEdit.Parent := FPanel;
  FTextEdit.Top := FRadioText.Top + FRadioText.Height + m;
  FTextEdit.Left := 2*m;
  FTextEdit.Width := FTextEdit.Parent.Width - 2*FTextEdit.Left;
  FTextEdit.OnChange := EditChange;
  FTextEdit.Images := Tree.Images;
  FTextEdit.RightButton.ImageIndex := 75; // Drop down arrow
  FTextEdit.RightButton.DropDownMenu := FTextDropDown;

  FRadioNull := TAllKeysRadioButton.Create(FPanel);
  FRadioNull.Parent := FPanel;
  FRadioNull.Top := FTextEdit.Top + FTextEdit.Height + 2*m;
  FRadioNull.Left := m;
  FRadioNull.Width := FRadioNull.Parent.Width - 2 * FRadioNull.Left;
  FRadioNull.OnClick := RadioClick;
  FRadioNull.OnKeyDown := DoKeyDown;
  FRadioNull.Caption := 'NULL';

  FRadioExpression := TAllKeysRadioButton.Create(FPanel);
  FRadioExpression.Parent := FPanel;
  FRadioExpression.Top := FRadioNull.Top + FRadioNull.Height + m;
  FRadioExpression.Left := m;
  FRadioExpression.Width := FRadioExpression.Parent.Width - 2 * FRadioExpression.Left;
  FRadioExpression.OnClick := RadioClick;
  FRadioExpression.OnKeyDown := DoKeyDown;
  FRadioExpression.Caption := _('Expression')+':';

  FExpressionEdit := TComboBox.Create(FPanel);
  FExpressionEdit.Parent := FPanel;
  FExpressionEdit.Top := FRadioExpression.Top + FRadioExpression.Height + m;
  FExpressionEdit.Left := 2*m;
  FExpressionEdit.Width := FExpressionEdit.Parent.Width - 2*FExpressionEdit.Left;
  FExpressionEdit.OnChange := EditChange;
  FExpressionEdit.DropDownCount := 20;
  for i:=Low(MySQLFunctions) to High(MySQLFunctions) do begin
    FExpressionEdit.Items.Add(MySQLFunctions[i].Name + MySQLFunctions[i].Declaration);
  end;

  FlblOnUpdate := TLabel.Create(FPanel);
  FlblOnUpdate.Parent := FPanel;
  FlblOnUpdate.Top := FExpressionEdit.Top + FExpressionEdit.Height + m;
  FlblOnUpdate.Left := 2*m;
  FlblOnUpdate.Width := FlblOnUpdate.Parent.Width - 2*FlblOnUpdate.Left;
  FlblOnUpdate.Caption := _('On update') + ':';

  FOnUpdateEdit := TComboBox.Create(FPanel);
  FOnUpdateEdit.Parent := FPanel;
  FOnUpdateEdit.Top := FlblOnUpdate.Top + FlblOnUpdate.Height + m;
  FOnUpdateEdit.Left := 2*m;
  FOnUpdateEdit.Width := FOnUpdateEdit.Parent.Width - 2*FOnUpdateEdit.Left;
  FOnUpdateEdit.OnChange := EditChange;
  FOnUpdateEdit.DropDownCount := 20;
  for i:=Low(MySQLFunctions) to High(MySQLFunctions) do begin
    FOnUpdateEdit.Items.Add(MySQLFunctions[i].Name + MySQLFunctions[i].Declaration);
  end;

  FRadioAutoInc := TAllKeysRadioButton.Create(FPanel);
  FRadioAutoInc.Parent := FPanel;
  FRadioAutoInc.Top := FOnUpdateEdit.Top + FOnUpdateEdit.Height + m;
  FRadioAutoInc.Left := m;
  FRadioAutoInc.Width := FRadioAutoInc.Parent.Width - 2 * FRadioAutoInc.Left;
  FRadioAutoInc.OnClick := RadioClick;
  FRadioAutoInc.OnKeyDown := DoKeyDown;
  FRadioAutoInc.Caption := 'AUTO_INCREMENT';

  FBtnOk := TButton.Create(FPanel);
  FBtnOk.Parent := FPanel;
  FBtnOk.Width := 60;
  FBtnOk.Top := FRadioAutoInc.Top + FRadioAutoInc.Height + m;
  FBtnOk.Left := FPanel.Width - 3*m - 2*FBtnOk.Width - 2*FPanel.BorderWidth;
  FBtnOk.OnClick := BtnOkClick;
  FBtnOk.Default := True;
  FBtnOk.Caption := _('OK');

  FBtnCancel := TButton.Create(FPanel);
  FBtnCancel.Parent := FPanel;
  FBtnCancel.Top := FBtnOk.Top;
  FBtnCancel.Width := FBtnOk.Width;
  FBtnCancel.Left := FBtnOk.Left + FBtnOk.Width + m;
  FBtnCancel.OnClick := BtnCancelClick;
  FBtnCancel.Cancel := True;
  FBtnCancel.Caption := _('Cancel');

  FEndTimer := TTimer.Create(FPanel);
  FEndTimer.Interval := 50;
  FEndTimer.Enabled := False;

  // Set outer panel (minimum) dimensions. Width is set in .SetBounds()
  FPanel.Height := 2*FPanel.BorderWidth + FBtnOk.Top + FBtnOk.Height + 2*m;
  FPanel.Constraints.MinWidth := 2*m + FBtnOK.Width + m + FBtnCancel.Width + 2*m;

  // Set anchors for all controls, so they are sticky when resizing the underlying column width
  FRadioNothing.Anchors := [akLeft, akTop, akRight];
  FRadioText.Anchors := [akLeft, akTop, akRight];
  FTextEdit.Anchors := [akLeft, akTop, akRight, akBottom];
  FRadioNull.Anchors := [akLeft, akBottom, akRight];
  FRadioExpression.Anchors := [akLeft, akBottom, akRight];
  FExpressionEdit.Anchors := [akLeft, akBottom, akRight];
  FOnUpdateEdit.Anchors := [akLeft, akBottom, akRight];
  FRadioAutoInc.Anchors := [akLeft, akBottom, akRight];
  FBtnOk.Anchors := [akBottom, akRight];
  FBtnCancel.Anchors := FBtnOk.Anchors;
end;


destructor TColumnDefaultEditorLink.Destroy;
begin
  FPanel.Free;
  inherited;
end;


function TColumnDefaultEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  ValueList, SelectedValues: TStringList;
  i: Integer;
  Item: TMenuItem;
begin
  inherited PrepareEdit(Tree, Node, Column);

  // Check relevant radio button
  FRadioNothing.Checked := DefaultType = cdtNothing;
  FRadioText.Checked := DefaultType = cdtText;
  FRadioNull.Checked := DefaultType = cdtNull;
  FRadioExpression.Checked := DefaultType = cdtExpression;
  FRadioAutoInc.Checked := DefaultType = cdtAutoInc;

  if FRadioText.Checked then begin
    FTextEdit.Text := DefaultText;
  end;

  if FRadioExpression.Checked then begin
    FExpressionEdit.Text := DefaultText;
  end;

  FOnUpdateEdit.Text := OnUpdateText;

  // Disable non working default options per data type
  FRadioAutoInc.Enabled := FRadioAutoInc.Checked or (FTableColumn.DataType.Category = dtcInteger);

  // Provide items with a check mark for ENUM and SET columns
  if FTableColumn.DataType.Index in [dtEnum, dtSet] then begin
    FTextEdit.RightButton.Visible := True;
    ValueList := FTableColumn.ValueList;
    SelectedValues := Explode(',', FTextEdit.Text);

    for i:=0 to ValueList.Count-1 do begin
      Item := TMenuItem.Create(FTextDropDown);
      Item.Caption := ValueList[i];
      Item.RadioItem := FTableColumn.DataType.Index = dtEnum;
      Item.Checked := SelectedValues.IndexOf(Item.Caption) > -1;
      Item.OnClick := EditDropDownClick;
      FTextDropDown.Items.Add(Item);
    end;

    ValueList.Free;
    SelectedValues.Free;
  end;

  Result := True;
end;


procedure TColumnDefaultEditorLink.SetBounds(R: TRect); stdcall;
var
  CellRect: TRect;
  P: TPoint;
  Room: Integer;
begin
  CellRect := GetCellRect(False);
  FPanel.Left := CellRect.Left;
  FPanel.Top := CellRect.Top;
  FPanel.Width := CellRect.Width;

  // Reposition editor so it's not outside the main form
  P := FParentForm.ClientToScreen(FPanel.BoundsRect.TopLeft);
  Room := FParentForm.BoundsRect.Bottom - 8 {Borderwidth} - (P.Y + FPanel.Height);
  if Room < 0 then
    FPanel.Top := CellRect.Top + Room;
  P := FParentForm.ClientToScreen(FPanel.BoundsRect.BottomRight);
  Room := FParentForm.BoundsRect.Right - 8 {Borderwidth} - P.X;
  if Room < 0 then
    FPanel.Left := CellRect.Left + Room;
end;


function TColumnDefaultEditorLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then begin
    FPanel.Show;
    if FRadioNothing.Checked then FRadioNothing.SetFocus
    else if FRadioText.Checked then FTextEdit.SetFocus
    else if FRadioNull.Checked then FRadioNull.SetFocus
    else if FRadioExpression.Checked then FExpressionEdit.SetFocus
    else if FRadioAutoInc.Checked then FRadioAutoInc.SetFocus;
  end;
end;


function TColumnDefaultEditorLink.EndEdit: Boolean; stdcall;
var
  Col: PTableColumn;
begin
  Result := not FStopping;
  if Result then begin
    FStopping := True;
    Col := FTree.GetNodeData(FNode);

    if FRadioNothing.Checked then
      Col.DefaultType := cdtNothing
    else if FRadioText.Checked then
      Col.DefaultType := cdtText
    else if FRadioNull.Checked then
      Col.DefaultType := cdtNull
    else if FRadioExpression.Checked then
      Col.DefaultType := cdtExpression
    else if FRadioAutoInc.Checked then
      Col.DefaultType := cdtAutoInc
    else
      Col.DefaultType := cdtText;

    case Col.DefaultType of
      cdtNothing: Col.DefaultText := '';
      cdtText: Col.DefaultText := FTextEdit.Text;
      cdtNull: Col.DefaultText := 'NULL';
      cdtExpression: Col.DefaultText := FExpressionEdit.Text;
      cdtAutoInc: Col.DefaultText := 'AUTO_INCREMENT';
    end;

    if FOnUpdateEdit.Text <> '' then
      Col.OnUpdateType := cdtExpression
    else
      Col.OnUpdateType := cdtNothing;
    Col.OnUpdateText := FOnUpdateEdit.Text;

    FTree.Text[FNode, FColumn] := Col.DefaultText;
    if FTree.CanFocus then
      FTree.SetFocus;
  end;
end;


procedure TColumnDefaultEditorLink.RadioClick(Sender: TObject);
begin
  if not FRadioText.Checked then
    FTextEdit.Color := clBtnFace
  else begin
    FTextEdit.Color := clWindow;
    if FTextEdit.CanFocus then
      FTextEdit.SetFocus;
  end;
  if not FRadioExpression.Checked then
    FExpressionEdit.Color := clBtnFace
  else begin
    FExpressionEdit.Color := clWindow;
    if FExpressionEdit.CanFocus then
      FExpressionEdit.SetFocus;
  end;
  FModified := True;
end;


procedure TColumnDefaultEditorLink.EditChange(Sender: TObject);
begin
  if Sender = FTextEdit then
    FRadioText.SetChecked(True)
  else if Sender = FExpressionEdit then
    FRadioExpression.SetChecked(True);
  FModified := True;
end;


procedure TColumnDefaultEditorLink.EditDropDownClick(Sender: TObject);
var
  Item: TMenuItem;
  NewValue: String;
begin
  // ENUM or SET value clicked in drop down menu
  Item := Sender as TMenuItem;
  Item.Checked := not Item.Checked;
  NewValue := '';
  for Item in Item.GetParentMenu.Items do begin
    if Item.Checked then
      NewValue := NewValue + StripHotkey(Item.Caption) + ',';
  end;
  if not IsEmpty(NewValue) then
    Delete(NewValue, Length(NewValue), 1);
  FTextEdit.Text := NewValue;
  FModified := True;
end;

procedure TColumnDefaultEditorLink.BtnOkClick(Sender: TObject);
begin
  // Timer based click on OK button, to prevent crash when theming is active
  FEndTimer.OnTimer := DoEndEdit;
  FEndTimer.Enabled := True;
end;

procedure TColumnDefaultEditorLink.BtnCancelClick(Sender: TObject);
begin
  // Timer based click on Cancel button, to prevent crash when theming is active
  FEndTimer.OnTimer := DoCancelEdit;
  FEndTimer.Enabled := True;
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
  // See further events in PrepareEdit
  FixVT(FTreeSelect);
  FMainControl := FTreeSelect;

  FMemoHelp := TMemo.Create(FParentForm);
  FMemoHelp.Hide;
  FMemoHelp.Parent := FParentForm;
  FMemoHelp.Color := clInfoBk;
  FMemoHelp.Font.Color := clInfoText;
  FMemoHelp.BevelKind := bkFlat;
  if TStyleManager.IsCustomStyleActive then begin
    FMemoHelp.BorderStyle := bsSingle;
    FMemoHelp.BevelInner := bvNone;
  end else begin
    FMemoHelp.BorderStyle := bsNone;
    FMemoHelp.BevelInner := bvSpace;
  end;
end;


destructor TDataTypeEditorLink.Destroy;
begin
  FreeAndNil(FTreeSelect);
  FreeAndNil(FMemoHelp);
  inherited;
end;


function TDataTypeEditorLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  dt: TDBDatatype;
  CatNode, TypeNode: PVirtualNode;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);
  if not Result then
    Exit;
  // Just use font name and size, avoid bold style and white color
  FTreeSelect.Font.Name := FCellFont.Name;
  FTreeSelect.Font.Size := FCellFont.Size;

  // Find and select current datatype in tree
  dt := FTableColumn.Connection.GetDataTypeByName(FCellText, False, FTableColumn.Name);
  CatNode := FTreeSelect.GetFirst;
  while Assigned(CatNode) do begin
    // Since recent update to VT 5.2.1 we need to initialize root nodes by hand for some reason:
    FTreeSelect.ReinitNode(CatNode, True);
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
  FTreeSelect.OnFocusChanged := DoTreeSelectFocusChanged;
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
  TreeHeight: Integer;
begin
  // Set position of tree. As the tree's parent is mainform, not listcolumns, add listcolumn's x + y positions
  CellRect := GetCellRect(False);
  // Do not exceed lower edge of mainform, as that portion would be hidden
  TreeHeight := Min(250, FParentForm.ClientHeight-CellRect.Top-10);
  FTreeSelect.SetBounds(CellRect.Left,
    CellRect.Top,
    FTreeSelect.Header.Columns[0].Width + GetSystemMetrics(SM_CXVSCROLL) + 5,
    TreeHeight);
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
  if Sender.GetNodeLevel(Node) = 0 then for i:=0 to High(FTableColumn.Connection.Datatypes) do begin
    if FTableColumn.Connection.Datatypes[i].Category = TDBDatatypeCategoryIndex(Node.Index) then
      Inc(ChildCount);
  end;
end;


procedure TDataTypeEditorLink.DoTreeSelectGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  i: Integer;
  Counter: Cardinal;
begin
  // Get cell text
  case Sender.GetNodeLevel(Node) of
    0: CellText := DatatypeCategories[TDBDatatypeCategoryIndex(Node.Index)].Name;
    1: begin
         Counter := 0;
         for i:=0 to High(FTableColumn.Connection.Datatypes) do begin
           if FTableColumn.Connection.Datatypes[i].Category = TDBDatatypeCategoryIndex(Node.Parent.Index) then begin
             Inc(Counter);
             if Counter = Node.Index+1 then begin
               CellText := FTableColumn.Connection.Datatypes[i].Name;
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
  NodeText: String;
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
    FMemoHelp.Text := FTableColumn.Connection.GetDatatypeByName(NodeText, False, FTableColumn.Name).Description;
    // Calc height of memo
    bmp := TBitMap.Create;
    bmp.Canvas.Font.Assign(FMemoHelp.Font);
    R := Rect(0, 0, FMemoHelp.Width-10, 0);
    DrawText(bmp.Canvas.Handle, PChar(FMemoHelp.Text), Length(FMemoHelp.Text), R, DT_WORDBREAK or DT_CALCRECT);
    FreeAndNil(bmp);
    FMemoHelp.Height := R.Bottom + 8;
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
      TargetCanvas.Font.Color := DatatypeCategories[TDBDatatypeCategoryIndex(Node.Parent.Index)].Color;
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


procedure TDataTypeEditorLink.DoTreeSelectFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  FModified := True;
end;


end.
