{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                  Hex Editor control                    }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZHexEdit;

interface

{$R *.DCR}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, StdCtrls, DbCtrls, Db;

type
  TZCustomHexEdit = class(TCustomPanel)
  private
    FGrid: TStringGrid;
    FScrollBar: TScrollBar;
    FStartRow, FRowCount: LongInt;
    FData: string;
    FOnChange: TNotifyEvent;
    FOffsetWidth: Integer;
    FLineWidth: Integer;
    FShowLines: Boolean;

    function CountRows(Length: LongInt): Integer;
    procedure SetData(Value: string);
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetOffsetWidth(Value: Integer);
    procedure SetLineWidth(Value: Integer);
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    procedure SetShowLines(Value: Boolean);

    procedure GridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridKeyPress(Sender: TObject; var Key: Char);
    procedure GridSetEdit(Sender: TObject; ACol, ARow: Longint; const Value: string);
    procedure ScrollOnChange(Sender: TObject);
  protected
    procedure Resize; override;
    procedure Redraw; dynamic;
    procedure Change; dynamic;
    procedure EditMode; dynamic;
    procedure Paint; override;

    property Data: string read FData write SetData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Align;
    property Color: TColor read GetColor write SetColor;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font read GetFont write SetFont;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
    property OffsetWidth: Integer read FOffsetWidth write SetOffsetWidth;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowLines: Boolean read FShowLines write SetShowLines;
    property TabOrder;
    property TabStop default True;
    property Visible;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TZHexEdit = class (TZCustomHexEdit)
  published
    property Data;
  end;

  TZDbHexEdit = class (TZCustomHexEdit)
  private
    FDataLink: TFieldDataLink;

    function GetDataField: string;
    procedure SetDataField(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure ActiveData(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure EditMode; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

implementation

uses ZExtra;

{************* TZCustomHexEdit class implementation *********}

{ Class constructor }
constructor TZCustomHexEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BorderStyle := bsSingle;
  Font.Name := 'Courier New';
  FStartRow := 0;
  FOffsetWidth := 4;
  FLineWidth := 8;
  FData := '';
  FRowCount := CountRows(Length(FData));

  FGrid := TStringGrid.Create(Self);
  with FGrid do
  begin
    Parent := Self;
    FixedRows := 0;
    FixedCols := 0;
    Col := 1;
    Options := [goDrawFocusSelected, goEditing, goTabs, goThumbTracking];
    BorderStyle := bsNone;
    ScrollBars := ssNone;
  end;
  FGrid.OnSelectCell := GridSelectCell;
  FGrid.OnKeyPress := GridKeyPress;
  FGrid.OnKeyDown := GridKeyDown;
  FGrid.OnSetEditText := GridSetEdit;

  FScrollBar := TScrollBar.Create(Self);
  with FScrollBar do
  begin
    Parent := Self;
    Kind := sbVertical;
    Max := FRowCount;
    TabStop := False;
  end;
  FScrollBar.OnChange := ScrollOnChange;
end;

{ Class destructor }
destructor TZCustomHexEdit.Destroy;
begin
  inherited Destroy;
end;

{ Set new data }
procedure TZCustomHexEdit.SetData(Value: string);
begin
  FData := Value;
  FStartRow := 0;
  FScrollBar.Position := 0;
  Resize;
end;

{ Get control color }
function TZCustomHexEdit.GetColor: TColor;
begin
  Result := FGrid.Color;
end;

{ Set new control color }
procedure TZCustomHexEdit.SetColor(Value: TColor);
begin
  FGrid.Color := Value;
end;

{ Get current font }
function TZCustomHexEdit.GetFont: TFont;
begin
  Result := inherited Font;
end;

{ Set new font }
procedure TZCustomHexEdit.SetFont(Value: TFont);
begin
  inherited Font.Assign(Value);
  Resize;
end;

{ Set new offset digits }
procedure TZCustomHexEdit.SetOffsetWidth(Value: Integer);
begin
  Value := Max(2, Min(8, Value));
  if Value <> FOffsetWidth then
  begin
    FOffsetWidth := Value;
    Resize;
  end;
end;

{ Set new offset digits }
procedure TZCustomHexEdit.SetLineWidth(Value: Integer);
begin
  Value := Max(2, Min(32, Value));
  if Value <> FLineWidth then
  begin
    FLineWidth := Value;
    Resize;
  end;
end;

{ Set show grid lines }
procedure TZCustomHexEdit.SetShowLines(Value: Boolean);
begin
  if Value <> FShowLines then
  begin
    FShowLines := Value;
    if FShowLines then
      FGrid.Options := FGrid.Options + [goVertLine, goHorzLine]
    else
      FGrid.Options := FGrid.Options - [goVertLine, goHorzLine];
  end;
end;

{ Count row count }
function TZCustomHexEdit.CountRows(Length: Integer): Integer;
begin
  Result := Length div FLineWidth;
  if (Length mod FLineWidth) <> 0 then
    Inc(Result);
end;

{ Make a string with repeat symbols }
function RepSymb(Chr: Char; Length: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length do
    Result := Result + Chr;
end;

{ Resize inside controls }
procedure TZCustomHexEdit.Resize;
begin
  with FScrollBar do
  begin
    Left := Self.ClientWidth - Width - Self.BorderWidth;
    Top := Self.BorderWidth;
    Height := Self.ClientHeight - Self.BorderWidth * 2;
  end;

  with FGrid do
  begin
    Canvas.Font.Assign(Font);
    DefaultRowHeight := Canvas.TextHeight('X')+2;
    Left := Self.BorderWidth;
    Top := Self.BorderWidth;
    Width := FScrollBar.Left - Left;
    Height := Self.ClientHeight - Self.BorderWidth * 2;
    ColCount := FLineWidth + 2;
    RowCount := Min(CountRows(Length(FData)), Height div DefaultRowHeight);
    ColWidths[0] := Canvas.TextWidth(RepSymb('0',FOffsetWidth)+'  ');
    ColWidths[FLineWidth+1] := Canvas.TextWidth('    '+RepSymb('M',FLineWidth));
    DefaultColWidth := Max((Width - ColWidths[0] - ColWidths[FLineWidth+1] - 5)
      div FLineWidth, Canvas.TextWidth('XX'));
    ColWidths[0] := Canvas.TextWidth(RepSymb('0',FOffsetWidth)+'  ');
    ColWidths[FLineWidth+1] := Canvas.TextWidth('   '+RepSymb('M',FLineWidth));
  end;

  FScrollBar.LargeChange := FGrid.RowCount;
  FRowCount := CountRows(Length(FData));
  FScrollBar.Max := FRowCount;
  Redraw;
end;

{ Byte to hex string }
function ByteToHex(Value: Char): ShortString;
const
  CArray: ShortString = '0123456789ABCDEF';
begin
  Result := CArray[(Byte(Value) shr 4) + 1] + CArray[(Byte(Value) and $0F) + 1];
end;

{ Double word to hex string }
function DWordToHex(Value: Cardinal): ShortString;
var
  Ptr: PChar;
begin
  Ptr := @Value;
  Result := ByteToHex(Ptr[3]) + ByteToHex(Ptr[2]) +
    ByteToHex(Ptr[1]) + ByteToHex(Ptr[0]);
end;

{ Fix unvisible symbols in string }
function FixString(Value: string): string;
var
  I: Integer;
begin
  Result := Value;
  for I := 1 to Length(Result) do
    if Result[I] < ' ' then
      Result[I] := '.';
end;

{ Fill grid with values }
procedure TZCustomHexEdit.Redraw;
var
  I, J, Offs: Integer;
  Temp: string;
begin
  Offs := FStartRow * FLineWidth;

  for I := 0 to FGrid.RowCount-1 do
  begin
    FGrid.Cells[0, I] := Copy(DWordToHex(Cardinal(Offs + I * FLineWidth)),
      8-FOffsetWidth+1, FOffsetWidth);
    Temp := Copy(FData, Offs + I * FLineWidth + 1, FLineWidth);
    FGrid.Cells[FLineWidth+1, I] := '  ' + FixString(Temp);
    for J := 1 to FLineWidth do
      if Length(Temp) >= J then
        FGrid.Cells[J, I] := ByteToHex(Temp[J])
      else
        FGrid.Cells[J, I] := '';
  end;
end;

{ Paint a control }
procedure TZCustomHexEdit.Paint;
begin
  if FScrollBar.Left = 0 then
    Resize;
  inherited Paint;
end;

{ Process on key down event }
procedure TZCustomHexEdit.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Offs: LongInt;
begin
  if (Key in [VK_INSERT, VK_DELETE]) and (Shift = []) then
  begin
    EditMode;
    Offs := (FStartRow + FGrid.Row) * FLineWidth + FGrid.Col;

    if Offs <= Length(FData) then
    begin
      case Key of
        VK_INSERT:
          Insert(#0, FData, Offs);
        VK_DELETE:
          Delete(FData, Offs, 1);
      end;
      Change;
      FRowCount := CountRows(Length(FData));
      FScrollBar.Max := FRowCount;
      Redraw;
    end;

    Key := 0;
    Exit;
  end;

  if (Key in [VK_UP, 33]) and (FGrid.Row = 0) then
  begin
    if Key = VK_UP then
      FScrollBar.Position := FScrollBar.Position - 1;
    if Key = 33 then
      FScrollBar.Position := FScrollBar.Position - FGrid.RowCount;
  end;

  if (Key in [VK_DOWN, 34]) and (FGrid.Row = FGrid.RowCount-1) then
  begin
    if Key = VK_DOWN then
      FScrollBar.Position := FScrollBar.Position + 1;
    if Key = 34 then
      FScrollBar.Position := FScrollBar.Position + FGrid.RowCount;
  end;
end;

{ Process key press }
procedure TZCustomHexEdit.GridKeyPress(Sender: TObject; var Key: Char);
var
  Offs: LongInt;
begin
  if Key in ['a'..'f'] then
    Key := Char(Ord(Key) - Ord('a') + Ord('A'));
  if not (Key in [#13, #27, #8, '0'..'9', 'A'..'F']) then
    Key := #0;
  if Key = #27 then
  begin
    Offs := (FStartRow + FGrid.Row) * FLineWidth + FGrid.Col;
    FGrid.Cells[FGrid.Col, FGrid.Row] := ByteToHex(FData[Offs]);
  end;
  if Key = #13 then
    Redraw;
end;

{ Do not allow to select special cells }
procedure TZCustomHexEdit.GridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if ACol in [0, FLineWidth+1] then
    CanSelect := False
  else
    Redraw;
  FScrollBar.Position := FStartRow + ARow;
end;

{ Hex string convert to byte }
function HexToByte(Value: string): char;
const
  CArray: ShortString = '0123456789ABCDEF';
var
  I, J: Integer;
  Temp, Temp1: Byte;
begin
  Temp := 0;
  for I := 1 to 2 do
  begin
    Temp1 := 0;
    if Length(Value) >= I then
    begin
      for J := 1 to 16 do
        if CArray[J] = Value[I] then
        begin
          Temp1 := J - 1;
          Break;
        end;
    end;
    Temp := (Temp shl 4) or Temp1;
  end;
  Result := Char(Temp);
end;

{ Set edit text }
procedure TZCustomHexEdit.GridSetEdit(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
var
  Temp: Char;
  Offs: LongInt;
begin
  EditMode;
  Temp := HexToByte(Value);
  Offs := (FStartRow + ARow) * FLineWidth + ACol;

  if Length(FData) < Offs then
  begin
    while Length(FData) < Offs do
      FData := FData + #0;
    Change;
  end;
  FRowCount := CountRows(Length(FData));
  FScrollBar.Max := FRowCount;

  if Value = '' then
    FGrid.Cells[ACol, ARow] := Copy(Value, 1, 2);

  if FData[Offs] <> Temp then
  begin
    FData[Offs] := Temp;
    Change;
  end;
end;

{ On scroll event }
procedure TZCustomHexEdit.ScrollOnChange(Sender: TObject);
begin
  if FScrollBar.Position < FStartRow then
  begin
    FStartRow := FScrollBar.Position;
    FGrid.Row := 0;
  end
  else if FScrollBar.Position >= (FStartRow + FGrid.RowCount) then
  begin
    FGrid.Row := FGrid.RowCount - 1;
    FStartRow := FScrollBar.Position - FGrid.Row;
  end
  else
    FGrid.Row := FScrollBar.Position - FStartRow;
  Redraw;
end;

{ On change }
procedure TZCustomHexEdit.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ Set edit mode }
procedure TZCustomHexEdit.EditMode;
begin
//
end;

{************** TZDbHexEdit class implementation **************}

{ Class constructor }
constructor TZDbHexEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnActiveChange := ActiveData;
end;

{ Class destructor }
destructor TZDbHexEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

{ Get data field }
function TZDbHexEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

{ Get data source }
function TZDbHexEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

{ Get read only }
function TZDbHexEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

{ Set data field }
procedure TZDbHexEdit.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

{ Set data source }
procedure TZDbHexEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

{ Set read only }
procedure TZDbHexEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{ On activate/deactivate datasource }
procedure TZDbHexEdit.ActiveData(Sender: TObject);
begin

end;

{ On change control }
procedure TZDbHexEdit.Change;
begin
  if FDataLink <> nil then
    FDataLink.Modified;

  inherited Change;
end;

{ Data change }
procedure TZDbHexEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Data := FDataLink.Field.AsString
  else
    Data := '';
end;

{ On change editing mode }
procedure TZDbHexEdit.EditingChange(Sender: TObject);
begin

end;

{ Process notification event }
procedure TZDbHexEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil)
    and (AComponent = DataSource) then
    DataSource := nil;
end;

{ On update data }
procedure TZDbHexEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Edit;
  FDataLink.Field.AsString := Data;
end;

{ Set edit mode }
procedure TZDbHexEdit.EditMode;
begin
  if FDataLink <> nil then
    FDataLink.Edit;
end;

end.
