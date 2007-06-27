{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 Extended String Grid                   }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZDataGrid;

interface

{$R *.dcr}

{$INCLUDE ..\ZeosDef.inc}

uses
  {$IFNDEF LINUX}
  Windows, Grids, Controls, Messages, StdCtrls, Forms, Graphics,
  {$ELSE}
  QGrids, QControls, {QMessages,} QStdCtrls, QForms, QGraphics,
  {$ENDIF}
  {$IFDEF VERCLX}Variants, {$ENDIF}
  SysUtils, Classes, Math, ZExtra;

type
  TZDataGrid = class;

  TZColumnButtonStyle = (bsNone, bsPickup, bsEllipsis);

  TZColumn = class(TCollectionItem)
  private
    FTitle: string;
    FWidth: Integer;
    FPickList: TStrings;
    FDropDownRows: Cardinal;
    FButtonStyle: TZColumnButtonStyle;
    FReadOnly: Boolean;
    FListSelect: Boolean;

    procedure SetTitle(Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetPickList(Value: TStrings);
    procedure SetButtonStyle(Value: TZColumnButtonStyle);
    procedure SetReadOnly(Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property  Title: string read FTitle write SetTitle;
    property  Width: Integer read FWidth write SetWidth;
    property  PickList: TStrings read FPickList write SetPickList;
    property  DropDownRows: Cardinal read FDropDownRows write FDropDownRows default 7;
    property  ButtonStyle: TZColumnButtonStyle read FButtonStyle write SetButtonStyle
      default bsPickup;
    property  ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property ListSelect: Boolean read FListSelect write FListSelect;
  end;

  TZColumnClass = class of TZColumn;

  TZColumnsState = (csDefault, csCustomized);

  TZDataColumns = class(TCollection)
  private
    FGrid: TZDataGrid;
    function GetColumn(Index: Integer): TZColumn;
    procedure SetColumn(Index: Integer; Value: TZColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TZDataGrid; ColumnClass: TZColumnClass);

    function  Add: TZColumn;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(S: TStream);
    property Grid: TZDataGrid read FGrid;
    function FindByTitle(Value: string): TZColumn;

    property Items[Index: Integer]: TZColumn read GetColumn write SetColumn; default;
  end;

  TZGridOption = (zoEditing, zoAlwaysShowEditor, zoTitles, zoIndicator,
    zoColumnResize, zoColLines, zoRowLines, zoTabs, zoRowSelect,
    zoAlwaysShowSelection, zoConfirmDelete, zoCancelOnExit, zoMultiSelect,
    zoAutoResize, zoAppendRecord);
  TZGridOptions = set of TZGridOption;

  TZDataGrid = class (TStringGrid)
  private
    FReadOnly: Boolean;
    FColor: TColor;
    FLayoutLock: Byte;
    FUpdateLock: Byte;
    FTitleOffset: Byte;
    FIndicatorOffset: Byte;
    FColumns: TZDataColumns;
    FOptions: TZGridOptions;
    FRecordCount: Integer;
    FSyncWidths: Boolean;
    FOnEditButtonClick: TNotifyEvent;
    FOnAppend: TNotifyEvent;
    FOnInsert: TNotifyEvent;
    FOnDelete: TNotifyEvent;
    FOnUpdate: TNotifyEvent;

    procedure SetOptions(Value: TZGridOptions);
    procedure EditButtonClick; dynamic;
    procedure InternalLayout;
    function GetFieldCount: Integer;
    function GetRecNo: Integer;
    procedure SetRecNo(const Value: Integer);
    function GetField(RecNo, FieldNo: Integer): string;
    procedure SetField(RecNo, FieldNo: Integer; const Value: string);
    function GetCurField(Index: Integer): string;
    procedure SetCurField(Index: Integer; Value: string);
    procedure SetColor(const Value: TColor);
    procedure PutSign(X, Y: Integer; Sign: array of Byte; Width, Height: Integer);
    function GetColCount: Integer;
    function GetFixedCols: Integer;
    function GetFixedRows: Integer;
    function GetRowCount: Integer;
    function GetBackColor: TColor;
    procedure SetBackColor(Value: TColor);
    procedure ReadColumns(Reader: TReader);
    procedure WriteColumns(Writer: TWriter);
    procedure SetColumns(const Value: TZDataColumns);
  protected
    function CreateEditor: TInplaceEdit; override;
    function CreateColumns: TZDataColumns; dynamic;
    function RawToColumn(ACol: Integer): TZColumn;

    procedure DoAppend;
    procedure DoInsert;
    procedure DoDelete;
    procedure DoUpdate;

    function  AcquireLayoutLock: Boolean;
    procedure BeginLayout;
    procedure EndLayout;
    procedure LayoutChanged; virtual;
    procedure CancelLayout;
    procedure BeginUpdate;
    procedure EndUpdate;

    function  CanEditModify: Boolean; override;
    function  CanEditShow: Boolean; override;
    procedure ColWidthsChanged; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure UpdateRows;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure AutoResize;
    procedure Resize; {$IFNDEF VER100} override; {$ENDIF}

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

    property LayoutLock: Byte read FLayoutLock;
    property UpdateLock: Byte read FUpdateLock;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure InsertRecord(RecNo: Integer; Values: array of Variant);
    procedure AppendRecord(Values: array of Variant);
    procedure DeleteRecord(RecNo: Integer);
    procedure UpdateRecord(RecNo: Integer; Values: array of Variant);

    procedure SetFieldByName(RecNo: Integer; FieldName, Value: string);
    function GetFieldByName(RecNo: Integer; FieldName: string): string;

    property RecordCount: Integer read FRecordCount;
    property FieldCount: Integer read GetFieldCount;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property Fields[RecNo, FieldNo: Integer]: string read GetField write SetField;
    property CurrentFields[Index: Integer]: string read GetCurField write SetCurField;
  published
    property Color: TColor read FColor write SetColor;
    property BackColor: TColor read GetBackColor write SetBackColor;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Columns: TZDataColumns read FColumns write SetColumns;
    property Options: TZGridOptions read FOptions write SetOptions;

    property FixedCols: Integer read GetFixedCols;
    property FixedRows: Integer read GetFixedRows;
    property ColCount: Integer read GetColCount;
    property RowCount: Integer read GetRowCount;

    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write FOnEditButtonClick;
    property OnAppend: TNotifyEvent read FOnAppend write FOnAppend;
    property OnInsert: TNotifyEvent read FOnInsert write FOnInsert;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

implementation

{ TZInplaceEdit }

type
  TEditStyle = (esSimple, esEllipsis, esPickList);
  TPopupListbox = class;

  TZInplaceEdit = class(TInplaceEdit)
  private
    FButtonWidth: Integer;
    FPickList: TPopupListbox;
    FActiveList: TWinControl;
    FEditStyle: TEditStyle;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetEditStyle(Value: TEditStyle);
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    function OverButton(const P: TPoint): Boolean;
    function ButtonRect: TRect;
  protected
    procedure BoundsChanged; override;
    procedure CloseUp(Accept: Boolean);
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
    property  EditStyle: TEditStyle read FEditStyle write SetEditStyle;
    property  ActiveList: TWinControl read FActiveList write FActiveList;
    property  PickList: TPopupListbox read FPickList;
  public
    constructor Create(Owner: TComponent); override;
  end;

{ TPopupListbox }

  TPopupListbox = class(TCustomListbox)
  private
    FSearchText: string;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

procedure TPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    {$IFNDEF VER100}
    AddBiDiModeExStyle(ExStyle);
    {$ENDIF}
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TPopupListbox.Keypress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 2000 then FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SelectString, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TZInPlaceEdit(Owner).CloseUp((X >= 0) and (Y >= 0) and
      (X < Width) and (Y < Height));
end;


constructor TZInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FEditStyle := esSimple;
end;

procedure TZInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if FEditStyle <> esSimple then
    {$IFNDEF VER100}
    if not TCustomGrid(Owner).UseRightToLeftAlignment then
      Dec(R.Right, FButtonWidth)
    else
    {$ENDIF}
      Inc(R.Left, FButtonWidth - 2);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.FarEast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
end;

procedure TZInplaceEdit.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if FPickList.ItemIndex <> -1 then
      ListValue := FPickList.Items[FPicklist.ItemIndex];
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
    if Accept then
      if (not VarIsNull(ListValue)) and EditCanModify then
        with TZDataGrid(Grid) do
          Cells[Col, Row] := ListValue;
  end;
end;

procedure TZInplaceEdit.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if FListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if FListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TZInplaceEdit.DropDown;
var
  P: TPoint;
  I,J,Y: Integer;
  Column: TZColumn;
begin
  if not FListVisible and Assigned(FActiveList) then
  begin
    FActiveList.Width := Width;
    with TZDataGrid(Grid) do
      Column := RawToColumn(Col);
    FPickList.Color := Color;
    FPickList.Font := Font;
    if Assigned(Column) then
      FPickList.Items := Column.Picklist
    else
      FPickList.Items.Clear;
    if Assigned(Column) and (FPickList.Items.Count >= Integer(Column.DropDownRows)) then
      FPickList.Height := Integer(Column.DropDownRows) * FPickList.ItemHeight + 4
    else
      FPickList.Height := FPickList.Items.Count * FPickList.ItemHeight + 4;
      with TZDataGrid(Grid) do
        FPickList.ItemIndex := FPickList.Items.IndexOf(Cells[Col, Row]);
    J := FPickList.ClientWidth;
    for I := 0 to FPickList.Items.Count - 1 do
    begin
      Y := FPickList.Canvas.TextWidth(FPickList.Items[I]);
      if Y > J then J := Y;
    end;
    FPickList.ClientWidth := J;

    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FActiveList.Height > Screen.Height then Y := P.Y - FActiveList.Height;
    SetWindowPos(FActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    Windows.SetFocus(Handle);
  end;
end;

type
  TWinControlCracker = class(TWinControl) end;

procedure TZInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    TZDataGrid(Grid).EditButtonClick;
//    KillMessage(Handle, WM_CHAR);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TZInplaceEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TZInplaceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FEditStyle <> esSimple) and
    OverButton(Point(X,Y)) then
  begin
    if FListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(FActiveList) then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TZInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TZInplaceEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;
  if (Button = mbLeft) and (FEditStyle = esEllipsis) and WasPressed then
    TZDataGrid(Grid).EditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TZInplaceEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W, X, Y: Integer;
begin
  if FEditStyle <> esSimple then
  begin
    R := ButtonRect;
    Flags := 0;
    if FEditStyle in [esPickList] then
    begin
      if FActiveList = nil then
        Flags := DFCS_INACTIVE
      else if FPressed then
        Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end
    else   { esEllipsis }
    begin
      if FPressed then Flags := BF_FLAT;
      DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      X := R.Left + ((R.Right - R.Left) shr 1) - 1 + Ord(FPressed);
      Y := R.Top + ((R.Bottom - R.Top) shr 1) - 1 + Ord(FPressed);
      W := FButtonWidth shr 3;
      if W = 0 then W := 1;
      PatBlt(DC, X, Y, W, W, BLACKNESS);
      PatBlt(DC, X - (W * 2), Y, W, W, BLACKNESS);
      PatBlt(DC, X + (W * 2), Y, W, W, BLACKNESS);
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TZInplaceEdit.SetEditStyle(Value: TEditStyle);
var
  Column: TZColumn;
begin
  if Value = FEditStyle then Exit;
  FEditStyle := Value;
  case Value of
    esPickList:
      begin
        if FPickList = nil then
        begin
          FPickList := TPopupListbox.Create(Self);
          FPickList.Visible := False;
          FPickList.Parent := Self;
          FPickList.OnMouseUp := ListMouseUp;
          FPickList.IntegralHeight := True;
          FPickList.ItemHeight := 11;
        end;
        FActiveList := FPickList;
      end;
  else  { cbsNone, cbsEllipsis, or read only field }
    FActiveList := nil;
  end;
  with TZDataGrid(Grid) do
    Column := RawToColumn(Col);
  if Assigned(Column) then
    Self.ReadOnly := Column.ReadOnly or Column.ListSelect
  else
    Self.ReadOnly := False;
  Repaint;
end;

procedure TZInplaceEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TZInplaceEdit.TrackButton(X,Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := ButtonRect;
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TZInplaceEdit.UpdateContents;
var
  Column: TZColumn;
  NewStyle: TEditStyle;
begin
  with TZDataGrid(Grid) do
    Column := RawToColumn(Col);
  NewStyle := esSimple;
  if Assigned(Column) then
    case Column.ButtonStyle of
      bsEllipsis: NewStyle := esEllipsis;
      bsPickup: NewStyle := esPickList;
    end;
  EditStyle := NewStyle;
  inherited UpdateContents;
end;

procedure TZInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    CloseUp(False);
end;

procedure TZInplaceEdit.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TZInplaceEdit.WMKillFocus(var Message: TMessage);
begin
  if not SysLocale.FarEast then inherited
  else
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if HWND(Message.WParam) <> TCustomGrid(Grid).Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
  CloseUp(False);
end;

function TZInplaceEdit.ButtonRect: TRect;
begin
  {$IFNDEF VER100}
  if not TCustomGrid(Owner).UseRightToLeftAlignment then
    Result := Rect(Width - FButtonWidth, 0, Width, Height)
  else
  {$ENDIF}
    Result := Rect(0, 0, FButtonWidth, Height);
end;

function TZInplaceEdit.OverButton(const P: TPoint): Boolean;
begin
  Result := PtInRect(ButtonRect, P);
end;

procedure TZInplaceEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
  if (FEditStyle <> esSimple) and OverButton(Point(XPos, YPos)) then
    Exit;
  inherited;
end;

procedure TZInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TZInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (FEditStyle <> esSimple) and OverButton(P) then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

procedure TZInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle in [esPickList] then
      with TWMKey(Message) do
      begin
        DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        if (CharCode <> 0) and FListVisible then
        begin
          with TMessage(Message) do
            SendMessage(FActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end
  end;
  inherited;
end;

{ TZDataGrid }

constructor TZDataGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited RowCount := 2;
  inherited ColCount := 2;
  DefaultRowHeight := 17;
  FColumns := CreateColumns;
  FOptions := [zoEditing, zoTitles, zoIndicator, zoColumnResize,
    zoColLines, zoRowLines, zoTabs, zoConfirmDelete, zoCancelOnExit,
    zoAppendRecord];
  inherited Options := [goFixedHorzLine, goFixedVertLine, goHorzLine,
    goVertLine, goColSizing, {goColMoving, }goTabs, goEditing];
  Color := clWindow;
  inherited Color := clAppWorkSpace;
  FSyncWidths := True;
  InternalLayout;
end;

destructor TZDataGrid.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

function TZDataGrid.CreateColumns: TZDataColumns;
begin
  Result := TZDataColumns.Create(Self, TZColumn);
end;

function TZDataGrid.CreateEditor: TInplaceEdit;
begin
  Result := TZInplaceEdit.Create(Self);
end;

procedure TZDataGrid.EditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Self)
end;

function TZDataGrid.GetBackColor: TColor;
begin
  Result := inherited Color;
end;

procedure TZDataGrid.SetBackColor(Value: TColor);
begin
  inherited Color := Value;
end;

procedure TZDataGrid.SetOptions(Value: TZGridOptions);
const
  LayoutOptions = [zoEditing, zoAlwaysShowEditor, zoTitles, zoIndicator,
    zoColLines, zoRowLines, zoRowSelect, zoAlwaysShowSelection, zoAutoResize,
    zoAppendRecord];
var
  NewGridOptions: TGridOptions;
  ChangedOptions: TZGridOptions;
begin
  if FOptions <> Value then
  begin
    NewGridOptions := [];
    if zoColLines in Value then
      NewGridOptions := NewGridOptions + [goFixedVertLine, goVertLine];
    if zoRowLines in Value then
      NewGridOptions := NewGridOptions + [goFixedHorzLine, goHorzLine];
    if zoColumnResize in Value then
      NewGridOptions := NewGridOptions + [goColSizing{, goColMoving}];
    if zoTabs in Value then
      Include(NewGridOptions, goTabs);
    if zoRowSelect in Value then
    begin
      Include(NewGridOptions, goRowSelect);
      Exclude(Value, zoAlwaysShowEditor);
      Exclude(Value, zoEditing);
    end;
    if zoEditing in Value then
      Include(NewGridOptions, goEditing);
    if zoAlwaysShowEditor in Value then
      Include(NewGridOptions, goAlwaysShowEditor);
    inherited Options := NewGridOptions;
    ChangedOptions := (FOptions + Value) - (FOptions * Value);
    FOptions := Value;
    if ChangedOptions * LayoutOptions <> [] then
      LayoutChanged;
  end;
end;

function TZDataGrid.AcquireLayoutLock: Boolean;
begin
  Result := (FUpdateLock = 0) and (FLayoutLock = 0);
  if Result then BeginLayout;
end;

procedure TZDataGrid.BeginLayout;
begin
  BeginUpdate;
  if FLayoutLock = 0 then Columns.BeginUpdate;
  Inc(FLayoutLock);
end;

procedure TZDataGrid.CancelLayout;
begin
  if FLayoutLock > 0 then
  begin
    if FLayoutLock = 1 then
      Columns.EndUpdate;
    Dec(FLayoutLock);
    EndUpdate;
  end;
end;

procedure TZDataGrid.EndLayout;
begin
  if FLayoutLock > 0 then
  begin
    try
      try
        if FLayoutLock = 1 then
          InternalLayout;
      finally
        if FLayoutLock = 1 then
          FColumns.EndUpdate;
      end;
    finally
      Dec(FLayoutLock);
      EndUpdate;
    end;
  end;
end;

procedure TZDataGrid.InternalLayout;

  procedure MeasureCells;
  var
    I, K: Integer;
    RestoreCanvas: Boolean;
  begin
    RestoreCanvas := not HandleAllocated;
    if RestoreCanvas then
      Canvas.Handle := GetDC(0);
    try
      Canvas.Font.Assign(Font);
      K := Canvas.TextHeight('Wg') + 4;
      for I := 0 to RowCount-1 do
        RowHeights[I] := K;
      for I := 0 to ColCount-1 do
      begin
        K := Canvas.TextWidth(Cells[I, 0]) + 4;
        if ColWidths[I] < K then
          ColWidths[I] := K;
      end;
    finally
      if RestoreCanvas then
      begin
        ReleaseDC(0,Canvas.Handle);
        Canvas.Handle := 0;
      end;
    end;
  end;

var
  I: Integer;
begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then Exit;

  FIndicatorOffset := 0;
  if zoIndicator in Options then
    Inc(FIndicatorOffset);
  inherited ColCount := MaxIntValue([FColumns.Count,1]) + FIndicatorOffset;
  inherited FixedCols := FIndicatorOffset;

  FTitleOffset := 0;
  if zoTitles in Options then
    Inc(FTitleOffset);

  FSyncWidths := False;
  for I := 0 to ColCount-1 do
  begin
    if I < FIndicatorOffset then
    begin
      ColWidths[I] := 11;
      Cells[I, 0] := '';
    end
    else if (I-FIndicatorOffset) < FColumns.Count then
    begin
      ColWidths[I] := FColumns[I-FIndicatorOffset].Width;
      if FTitleOffset > 0 then
        Cells[I, 0] := FColumns[I-FIndicatorOffset].Title;
    end
    else
    begin
      ColWidths[I] := DefaultColWidth;
      Cells[I, 0] := '';
    end;
  end;

  if FTitleOffset > FixedRows then
  begin
    inherited RowCount := RowCount + FTitleOffset;
    inherited FixedRows := FTitleOffset;
  end
  else if FTitleOffset < FixedRows then
  begin
    I := FixedRows;
    inherited FixedRows := FTitleOffset;
    inherited RowCount := RowCount - I;
  end;

  MeasureCells;
  FSyncWidths := True;

  UpdateRows;
  Invalidate;
end;

procedure TZDataGrid.LayoutChanged;
begin
  if AcquireLayoutLock then
    EndLayout;
end;

procedure TZDataGrid.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TZDataGrid.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
end;

function TZDataGrid.RawToColumn(ACol: Integer): TZColumn;
var
  Index: Integer;
begin
  Result := nil;
  Index := ACol - FIndicatorOffset;
  if Assigned(Columns) and (Index >= 0) and (Index < Columns.Count) then
    Result := Columns[Index];
end;

function TZDataGrid.CanEditModify: Boolean;
var
  Column: TZColumn;
begin
  Result := False;
  Column := RawToColumn(Col);
  if not ReadOnly and Assigned(Column) then
    Result := not Column.ReadOnly;
end;

function TZDataGrid.CanEditShow: Boolean;
begin
  Result := (LayoutLock = 0) and inherited CanEditShow;
end;

procedure TZDataGrid.ColWidthsChanged;
var
  I: Integer;
begin
  inherited ColWidthsChanged;
  if FSyncWidths and AcquireLayoutLock then
  try
    for I := FIndicatorOffset to ColCount - 1 do
      if ((I - FIndicatorOffset) < Columns.Count) and
        (Columns[I - FIndicatorOffset].Width <> ColWidths[I]) then
        Columns[I - FIndicatorOffset].Width := ColWidths[I];
  finally
    EndLayout;
  end;
  AutoResize;
end;

procedure TZDataGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  FromIndex := FromIndex - FIndicatorOffset;
  ToIndex := ToIndex - FIndicatorOffset;
  if (FromIndex < Columns.Count) and (ToIndex < Columns.Count) then
    Columns[FromIndex].Index := ToIndex;
//  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;

procedure TZDataGrid.UpdateRows;
var
  I, N: Integer;
begin
  N := RecordCount;
  if not ReadOnly and (zoAppendRecord in Options) then Inc(N);
  inherited RowCount := FTitleOffset + MaxIntValue([1, N]);
  if (not ReadOnly and (zoAppendRecord in Options)) or (RecordCount = 0) then
    for I := 0 to ColCount-1 do
      Cells[I, RowCount-1] := '';
end;

procedure TZDataGrid.AppendRecord(Values: array of Variant);
var
  I, N: Integer;
begin
  if Columns.Count = 0 then Exit;
  Inc(FRecordCount);
  if not ReadOnly and (zoAppendRecord in Options) then N := 2
  else N := 1;
  UpdateRows;
  for I := 0 to MinIntValue([Columns.Count-1, High(Values)]) do
    Cells[I + FIndicatorOffset, RowCount - N] := VarAsType(Values[I], varString);
  DoAppend;
end;

procedure TZDataGrid.Clear;
begin
  FRecordCount := 0;
  UpdateRows;
  DoDelete;
end;

procedure TZDataGrid.DeleteRecord(RecNo: Integer);
var
  I, J: Integer;
begin
  if (RecNo >= RecordCount) or (RecordCount <= 0) then Exit;
  for I := RecNo + FTitleOffset to RowCount-1 do
    for J := FIndicatorOffset to ColCount-1 do
      Cells[J, I] := Cells[J, I+1];
  Dec(FRecordCount);
  UpdateRows;
  DoDelete;
end;

function TZDataGrid.GetFieldCount: Integer;
begin
  Result := Columns.Count;
end;

procedure TZDataGrid.InsertRecord(RecNo: Integer; Values: array of Variant);
var
 I, J: Integer;
begin
  if (Columns.Count = 0) or (RecNo >= FRecordCount) then Exit;
  Inc(FRecordCount);
  for I := FRecordCount-1 downto RecNo+1 do
    for J := 0 to Columns.Count-1 do
      Cells[J+FIndicatorOffset, I+FTitleOffset] :=
        Cells[J+FIndicatorOffset, I+FTitleOffset-1];
  UpdateRows;
  for I := 0 to MinIntValue([Columns.Count-1, High(Values)]) do
    Cells[I + FIndicatorOffset, RecNo+FTitleOffset] := VarAsType(Values[I], varString);
  DoInsert;
end;

procedure TZDataGrid.UpdateRecord(RecNo: Integer; Values: array of Variant);
var
  I: Integer;
begin
  if RecNo >= RecordCount then Exit;
  for I := 0 to MinIntValue([Columns.Count-1, High(Values)]) do
    Cells[I+FIndicatorOffset, RecNo+FTitleOffset] := VarAsType(Values[I], varString);
  DoUpdate;
end;

function TZDataGrid.GetRecNo: Integer;
begin
  Result := Row - FTitleOffset;
end;

procedure TZDataGrid.SetRecNo(const Value: Integer);
begin
  if Value < RecordCount then
    Row := Value + FTitleOffset
  else
    Row := RowCount-1;
end;

function TZDataGrid.GetField(RecNo, FieldNo: Integer): string;
begin
  Result := '';
  if (RecNo < RecordCount) and (FieldNo < Columns.Count) then
    Result := Cells[FieldNo+FIndicatorOffset, RecNo+FTitleOffset];
end;

procedure TZDataGrid.SetField(RecNo, FieldNo: Integer;
  const Value: string);
begin
  if (RecNo < RecordCount) and (FieldNo < Columns.Count) then
    Cells[FieldNo+FIndicatorOffset, RecNo+FTitleOffset] := Value;
end;

function TZDataGrid.GetCurField(Index: Integer): string;
begin
  Result := Cells[Index+FIndicatorOffset, Row];
end;

procedure TZDataGrid.SetCurField(Index: Integer; Value: string);
begin
  Cells[Index+FIndicatorOffset, Row] := Value;
end;

procedure TZDataGrid.SetEditText(ACol, ARow: Integer;
  const Value: string);
begin
  inherited SetEditText(ACol, ARow, Value);
  if not ReadOnly  and (zoAppendRecord in Options)
    and (ARow = (RowCount-1)) and (Value <> '') then
  begin
    Inc(FRecordCount);
    UpdateRows;
    DoAppend;
  end else
    DoUpdate;
end;

const
  ArrowRight: array[0..8] of Byte = (
    $10, $18, $1c, $1e, $1f, $1e, $1c, $18, $10
  );

  NewSign: array[0..6] of Byte = (
    $10, $54, $38, $fe, $38, $54, $10
  );

procedure TZDataGrid.PutSign(X, Y: Integer; Sign: array of Byte; Width,
  Height: Integer);
var
  I, J, K: Integer;
  P: Byte;
begin
  for I := 0 to (Width div 8)-1 do
    for J := 0 to Height-1 do
    begin
      P := Sign[I + J * (Width div 8)];
      for K := 0 to 7 do
        if ((P shl K) and $80) <> 0 then
          Canvas.Pixels[X+I*8+K, Y+J] := Font.Color;
    end;
end;

procedure TZDataGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
  with Canvas do
  begin
    if Brush.Color = inherited Color then
      Brush.Color := Color;
    if (ARow > 0) and (ACol < FIndicatorOffset) then
    begin
      if (ARow = (RowCount-1)) and not ReadOnly
        and (zoAppendRecord in Options) then
        PutSign(ARect.Left+2, ARect.Top+5, NewSign, 8, 7)
      else if ARow = Row then
        PutSign(ARect.Left, ARect.Top+4, ArrowRight, 8, 9);
    end else
      inherited DrawCell(ACol, ARow, ARect, AState);
    if (ACol >= FIndicatorOffset) and (FIndicatorOffset > 0) then
      Cells[0, ARow] := '';
  end;
end;

procedure TZDataGrid.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Repaint;
  end;
end;

procedure TZDataGrid.DoAppend;
begin
  if Assigned(FOnAppend) then
    FOnAppend(Self);
end;

procedure TZDataGrid.DoDelete;
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self);
end;

procedure TZDataGrid.DoInsert;
begin
  if Assigned(FOnInsert) then
    FOnInsert(Self);
end;

procedure TZDataGrid.DoUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

function TZDataGrid.GetColCount: Integer;
begin
  Result := inherited ColCount;
end;

function TZDataGrid.GetFixedCols: Integer;
begin
  Result := inherited FixedCols;
end;

function TZDataGrid.GetFixedRows: Integer;
begin
  Result := inherited FixedRows;
end;

function TZDataGrid.GetRowCount: Integer;
begin
  Result := inherited RowCount;
end;

procedure TZDataGrid.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;

procedure TZDataGrid.WriteColumns(Writer: TWriter);
begin
  Writer.WriteCollection(Columns)
end;

procedure TZDataGrid.DefineProperties(Filer: TFiler);
var
  StoreIt: Boolean;
begin
  if Assigned(Filer.Ancestor) then
    StoreIt := not CollectionsEqual(Columns, TZDataGrid(Filer.Ancestor).Columns
      {$IFDEF VERCLX}, nil, nil{$ENDIF})
  else
    StoreIt := True;
  Filer.DefineProperty('Columns', ReadColumns, WriteColumns, StoreIt);
end;

procedure TZDataGrid.SetColumns(const Value: TZDataColumns);
begin
  FColumns.Assign(Value);
end;

procedure TZDataGrid.Loaded;
begin
  inherited Loaded;
  InternalLayout;
end;

procedure TZDataGrid.CMFontChanged(var Message: TMessage);
begin
  inherited;
  InternalLayout;
end;

procedure TZDataGrid.Resize;
begin
  AutoResize;
  {$IFNDEF VER100}
  inherited Resize;
  {$ENDIF}
end;

procedure TZDataGrid.AutoResize;
var
  I, W: Integer;
begin
  if zoAutoResize in Options then
  begin
    W := ClientWidth - ColCount;
    for I := 0 to ColCount-2 do
      Dec(W, ColWidths[I]);
    FSyncWidths := False;
    if (W > 10) and (ColWidths[ColCount-1] <> W) then
      ColWidths[ColCount-1] := W;
    FSyncWidths := True;
  end;
end;

function TZDataGrid.GetFieldByName(RecNo: Integer;
  FieldName: string): string;
var
  Column: TZColumn;
begin
  Column := Columns.FindByTitle(FieldName);
  if Column <> nil then
    Result := Fields[RecNo, Column.Index]
  else
    Result := '';
end;

procedure TZDataGrid.SetFieldByName(RecNo: Integer; FieldName,
  Value: string);
var
  Column: TZColumn;
begin
  Column := Columns.FindByTitle(FieldName);
  if Column <> nil then
    Fields[RecNo, Column.Index] := Value;
end;

{ TZColumn }

constructor TZColumn.Create(Collection: TCollection);
var
  Grid: TZDataGrid;
begin
  Grid := nil;
  if Assigned(Collection) and (Collection is TZDataColumns) then
    Grid := TZDataColumns(Collection).Grid;
  if Assigned(Grid) then Grid.BeginLayout;
  try
    inherited Create(Collection);
    FDropDownRows := 7;
    FButtonStyle := bsPickup;
    FPickList := TStringList.Create;
  finally
    if Assigned(Grid) then Grid.EndLayout;
  end;
end;

destructor TZColumn.Destroy;
begin
  FPickList.Free;
  inherited Destroy;
end;

procedure TZColumn.Assign(Source: TPersistent);
begin
  if Source is TZColumn then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      Title := TZColumn(Source).Title;
      Width := TZColumn(Source).Width;
      ReadOnly := TZColumn(Source).ReadOnly;
      DropDownRows := TZColumn(Source).DropDownRows;
      ButtonStyle := TZColumn(Source).ButtonStyle;
      PickList.Assign(TZColumn(Source).PickList);
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TZColumn.SetButtonStyle(Value: TZColumnButtonStyle);
begin
  if FButtonStyle <> Value then
  begin
    FButtonStyle := Value;
    Changed(False);
  end;
end;

procedure TZColumn.SetPickList(Value: TStrings);
begin
  if not FPickList.Equals(Value) then
  begin
    FPickList.Assign(Value);
  end;
end;

procedure TZColumn.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed(False);
  end;
end;

procedure TZColumn.SetTitle(Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    Changed(False);
  end;
end;

procedure TZColumn.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

{ TZDataColumns }

constructor TZDataColumns.Create(Grid: TZDataGrid;
  ColumnClass: TZColumnClass);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TZDataColumns.Add: TZColumn;
begin
  Result := TZColumn(inherited Add);
end;

function TZDataColumns.GetColumn(Index: Integer): TZColumn;
begin
  Result := TZColumn(inherited Items[Index]);
end;

procedure TZDataColumns.SetColumn(Index: Integer; Value: TZColumn);
begin
  Items[Index].Assign(Value);
end;

function TZDataColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TZDataColumns.Update(Item: TCollectionItem);
begin
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;
  FGrid.LayoutChanged;
end;

procedure TZDataColumns.LoadFromFile(const Filename: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

type
  TColumnsWrapper = class(TComponent)
  private
    FColumns: TZDataColumns;
  published
    property Columns: TZDataColumns read FColumns write FColumns;
  end;

procedure TZDataColumns.LoadFromStream(S: TStream);
var
  Wrapper: TColumnsWrapper;
begin
  Wrapper := TColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := FGrid.CreateColumns;
    S.ReadComponent(Wrapper);
    Assign(Wrapper.Columns);
  finally
    Wrapper.Columns.Free;
    Wrapper.Free;
  end;
end;

procedure TZDataColumns.SaveToFile(const Filename: string);
var
  S: TStream;
begin
  S := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TZDataColumns.SaveToStream(S: TStream);
var
  Wrapper: TColumnsWrapper;
begin
  Wrapper := TColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := Self;
    S.WriteComponent(Wrapper);
  finally
    Wrapper.Free;
  end;
end;

function TZDataColumns.FindByTitle(Value: string): TZColumn;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do
    if StrCaseCmp(Items[I].Title, Value) then
    begin
      Result := Items[I];
      Exit;
    end;
end;

end.
