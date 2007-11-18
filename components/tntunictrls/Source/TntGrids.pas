
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntGrids;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, TntClasses, Grids, Windows, Controls, Messages;

type
{TNT-WARN TInplaceEdit}
  TTntInplaceEdit = class(TInplaceEdit{TNT-ALLOW TInplaceEdit})
  private
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure UpdateContents; override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
  public
    property Text: WideString read GetText write SetText;
  end;

  TTntGetEditEvent = procedure (Sender: TObject; ACol, ARow: Longint; var Value: WideString) of object;
  TTntSetEditEvent = procedure (Sender: TObject; ACol, ARow: Longint; const Value: WideString) of object;

{TNT-WARN TCustomDrawGrid}
  _TTntInternalCustomDrawGrid = class(TCustomDrawGrid{TNT-ALLOW TCustomDrawGrid})
  private
    FSettingEditText: Boolean;
    procedure InternalSetEditText(ACol, ARow: Longint; const Value: string{TNT-ALLOW string}); dynamic; abstract;
  protected
    procedure SetEditText(ACol, ARow: Longint; const Value: string{TNT-ALLOW string}); override;
  end;

  TTntCustomDrawGrid = class(_TTntInternalCustomDrawGrid)
  private
    FOnGetEditText: TTntGetEditEvent;
    FOnSetEditText: TTntSetEditEvent;
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  protected
    function CreateEditor: TInplaceEdit{TNT-ALLOW TInplaceEdit}; override;
    procedure InternalSetEditText(ACol, ARow: Longint; const Value: string{TNT-ALLOW string}); override;
    function GetEditText(ACol, ARow: Longint): WideString; reintroduce; virtual;
    procedure SetEditText(ACol, ARow: Longint; const Value: WideString); reintroduce; virtual;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure ShowEditorChar(Ch: WideChar); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    property OnGetEditText: TTntGetEditEvent read FOnGetEditText write FOnGetEditText;
    property OnSetEditText: TTntSetEditEvent read FOnSetEditText write FOnSetEditText;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TDrawGrid}
  TTntDrawGrid = class(TTntCustomDrawGrid)
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property ColCount;
    property Constraints;
    property Ctl3D;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property RowCount;
    property FixedRows;
    property Font;
    property GridLineWidth;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
    property OnClick;
    property OnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
  end;

  TTntStringGrid = class;

{TNT-WARN TStringGridStrings}
  TTntStringGridStrings = class(TTntStrings)
  private
    FIsCol: Boolean;
    FColRowIndex: Integer;
    FGrid: TTntStringGrid;
    function GridAnsiStrings: TStrings{TNT-ALLOW TStrings};
  protected
    function Get(Index: Integer): WideString; override;
    procedure Put(Index: Integer; const S: WideString); override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AGrid: TTntStringGrid; AIndex: Longint);
    function Add(const S: WideString): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: WideString); override;
  end;

{TNT-WARN TStringGrid}
  _TTntInternalStringGrid = class(TStringGrid{TNT-ALLOW TStringGrid})
  private
    FSettingEditText: Boolean;
    procedure InternalSetEditText(ACol, ARow: Longint; const Value: string{TNT-ALLOW string}); dynamic; abstract;
  protected
    procedure SetEditText(ACol, ARow: Longint; const Value: string{TNT-ALLOW string}); override;
  end;

  TTntStringGrid = class(_TTntInternalStringGrid)
  private
    FCreatedRowStrings: TStringList{TNT-ALLOW TStringList};
    FCreatedColStrings: TStringList{TNT-ALLOW TStringList};
    FOnGetEditText: TTntGetEditEvent;
    FOnSetEditText: TTntSetEditEvent;
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    function GetCells(ACol, ARow: Integer): WideString;
    procedure SetCells(ACol, ARow: Integer; const Value: WideString);
    function FindGridStrings(const IsCol: Boolean; const ListIndex: Integer): TTntStrings;
    function GetCols(Index: Integer): TTntStrings;
    function GetRows(Index: Integer): TTntStrings;
    procedure SetCols(Index: Integer; const Value: TTntStrings);
    procedure SetRows(Index: Integer; const Value: TTntStrings);
  protected
    function CreateEditor: TInplaceEdit{TNT-ALLOW TInplaceEdit}; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure InternalSetEditText(ACol, ARow: Longint; const Value: string{TNT-ALLOW string}); override;
    function GetEditText(ACol, ARow: Longint): WideString; reintroduce; virtual;
    procedure SetEditText(ACol, ARow: Longint; const Value: WideString); reintroduce; virtual;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure ShowEditorChar(Ch: WideChar); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Cells[ACol, ARow: Integer]: WideString read GetCells write SetCells;
    property Cols[Index: Integer]: TTntStrings read GetCols write SetCols;
    property Rows[Index: Integer]: TTntStrings read GetRows write SetRows;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
    property OnGetEditText: TTntGetEditEvent read FOnGetEditText write FOnGetEditText;
    property OnSetEditText: TTntSetEditEvent read FOnSetEditText write FOnSetEditText;
  end;

implementation

uses
  SysUtils, TntSystem, TntGraphics, TntControls, TntStdCtrls, TntActnList, TntSysUtils;

{ TBinaryCompareAnsiStringList }
type
  TBinaryCompareAnsiStringList = class(TStringList{TNT-ALLOW TStringList})
  protected
    function CompareStrings(const S1, S2: string{TNT-ALLOW string}): Integer; override;
  end;

function TBinaryCompareAnsiStringList.CompareStrings(const S1, S2: string{TNT-ALLOW string}): Integer;
begin
  // must compare strings via binary for speed
  if S1 = S2 then
    result := 0
  else if S1 < S2 then
    result := -1
  else
    result := 1;
end;

{ TTntInplaceEdit }

procedure TTntInplaceEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  TntCustomEdit_CreateWindowHandle(Self, Params);
end;

function TTntInplaceEdit.GetText: WideString;
begin
  if IsMasked then
    Result := inherited Text
  else
    Result := TntControl_GetText(Self);
end;

procedure TTntInplaceEdit.SetText(const Value: WideString);
begin
  if IsMasked then
    inherited Text := Value
  else
    TntControl_SetText(Self, Value);
end;

type TAccessCustomGrid = class(TCustomGrid);

procedure TTntInplaceEdit.UpdateContents;
begin
  Text := '';
  with TAccessCustomGrid(Grid) do
    Self.EditMask := GetEditMask(Col, Row);
  if (Grid is TTntStringGrid) then
    with (Grid as TTntStringGrid) do
      Self.Text := GetEditText(Col, Row)
  else if (Grid is TTntCustomDrawGrid) then
    with (Grid as TTntCustomDrawGrid) do
      Self.Text := GetEditText(Col, Row)
  else
    with TAccessCustomGrid(Grid) do
      Self.Text := GetEditText(Col, Row);
  with TAccessCustomGrid(Grid) do
    Self.MaxLength := GetEditLimit;
end;

{ _TTntInternalCustomDrawGrid }

procedure _TTntInternalCustomDrawGrid.SetEditText(ACol, ARow: Integer; const Value: string{TNT-ALLOW string});
begin
  if FSettingEditText then
    inherited
  else
    InternalSetEditText(ACol, ARow, Value);
end;


{ TTntCustomDrawGrid }

function TTntCustomDrawGrid.CreateEditor: TInplaceEdit{TNT-ALLOW TInplaceEdit};
begin
  Result := TTntInplaceEdit.Create(Self);
end;

procedure TTntCustomDrawGrid.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TTntCustomDrawGrid.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomDrawGrid.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntCustomDrawGrid.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TTntCustomDrawGrid.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

function TTntCustomDrawGrid.GetEditText(ACol, ARow: Integer): WideString;
begin
  Result := '';
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TTntCustomDrawGrid.InternalSetEditText(ACol, ARow: Integer; const Value: string{TNT-ALLOW string});
begin
  if not FSettingEditText then
    SetEditText(ACol, ARow, TntControl_GetText(InplaceEditor));
end;

procedure TTntCustomDrawGrid.SetEditText(ACol, ARow: Integer; const Value: WideString);
begin
  if Assigned(FOnSetEditText) then FOnSetEditText(Self, ACol, ARow, Value);
end;

procedure TTntCustomDrawGrid.WMChar(var Msg: TWMChar);
begin
  if (goEditing in Options)
  and (AnsiChar(Msg.CharCode) in [^H, #32..#255]) then begin
    RestoreWMCharMsg(TMessage(Msg));
    ShowEditorChar(WideChar(Msg.CharCode));
  end else
    inherited;
end;

procedure TTntCustomDrawGrid.ShowEditorChar(Ch: WideChar);
begin
  ShowEditor;
  if InplaceEditor <> nil then begin
    if Win32PlatformIsUnicode then
      PostMessageW(InplaceEditor.Handle, WM_CHAR, Word(Ch), 0)
    else
      PostMessageA(InplaceEditor.Handle, WM_CHAR, Word(Ch), 0);
  end;
end;

procedure TTntCustomDrawGrid.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomDrawGrid.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntStringGridStrings }

procedure TTntStringGridStrings.Assign(Source: TPersistent);
var
  UTF8Strings: TStringList{TNT-ALLOW TStringList};
  i: integer;
begin
  UTF8Strings := TStringList{TNT-ALLOW TStringList}.Create;
  try
    if Source is TStrings{TNT-ALLOW TStrings} then begin
      for i := 0 to TStrings{TNT-ALLOW TStrings}(Source).Count - 1 do
        UTF8Strings.AddObject(WideStringToUTF8(WideString(TStrings{TNT-ALLOW TStrings}(Source).Strings[i])),
          TStrings{TNT-ALLOW TStrings}(Source).Objects[i]);
      GridAnsiStrings.Assign(UTF8Strings);
    end else if Source is TTntStrings then begin
      for i := 0 to TTntStrings(Source).Count - 1 do
        UTF8Strings.AddObject(WideStringToUTF8(TTntStrings(Source).Strings[i]),
          TTntStrings(Source).Objects[i]);
      GridAnsiStrings.Assign(UTF8Strings);
    end else
      GridAnsiStrings.Assign(Source);
  finally
    UTF8Strings.Free;
  end;
end;

function TTntStringGridStrings.GridAnsiStrings: TStrings{TNT-ALLOW TStrings};
begin
  Assert(Assigned(FGrid));
  if FIsCol then
    Result := TStringGrid{TNT-ALLOW TStringGrid}(FGrid).Cols[FColRowIndex]
  else
    Result := TStringGrid{TNT-ALLOW TStringGrid}(FGrid).Rows[FColRowIndex];
end;

procedure TTntStringGridStrings.Clear;
begin
  GridAnsiStrings.Clear;
end;

procedure TTntStringGridStrings.Delete(Index: Integer);
begin
  GridAnsiStrings.Delete(Index);
end;

function TTntStringGridStrings.GetCount: Integer;
begin
  Result := GridAnsiStrings.Count;
end;

function TTntStringGridStrings.Get(Index: Integer): WideString;
begin
  Result := UTF8ToWideString(GridAnsiStrings[Index]);
end;

procedure TTntStringGridStrings.Put(Index: Integer; const S: WideString);
begin
  GridAnsiStrings[Index] := WideStringToUTF8(S);
end;

procedure TTntStringGridStrings.Insert(Index: Integer; const S: WideString);
begin
  GridAnsiStrings.Insert(Index, WideStringToUTF8(S));
end;

function TTntStringGridStrings.Add(const S: WideString): Integer;
begin
  Result := GridAnsiStrings.Add(WideStringToUTF8(S));
end;

function TTntStringGridStrings.GetObject(Index: Integer): TObject;
begin
  Result := GridAnsiStrings.Objects[Index];
end;

procedure TTntStringGridStrings.PutObject(Index: Integer; AObject: TObject);
begin
  GridAnsiStrings.Objects[Index] := AObject;
end;

type TAccessStrings = class(TStrings{TNT-ALLOW TStrings});

procedure TTntStringGridStrings.SetUpdateState(Updating: Boolean);
begin
  TAccessStrings(GridAnsiStrings).SetUpdateState(Updating);
end;

constructor TTntStringGridStrings.Create(AGrid: TTntStringGrid; AIndex: Integer);
begin
  inherited Create;
  FGrid := AGrid;
  if AIndex > 0 then begin
    FIsCol := False;
    FColRowIndex := AIndex - 1;
  end else begin
    FIsCol := True;
    FColRowIndex := -AIndex - 1;
  end;
end;

{ _TTntInternalStringGrid }

procedure _TTntInternalStringGrid.SetEditText(ACol, ARow: Integer; const Value: string{TNT-ALLOW string});
begin
  if FSettingEditText then
    inherited
  else
    InternalSetEditText(ACol, ARow, Value);
end;

{ TTntStringGrid }

constructor TTntStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FCreatedRowStrings := TBinaryCompareAnsiStringList.Create;
  FCreatedRowStrings.Sorted := True;
  FCreatedRowStrings.Duplicates := dupError;
  FCreatedColStrings := TBinaryCompareAnsiStringList.Create;
  FCreatedColStrings.Sorted := True;
  FCreatedColStrings.Duplicates := dupError;
end;

destructor TTntStringGrid.Destroy;
var
  i: integer;
begin
  for i := FCreatedColStrings.Count - 1 downto 0 do
    FCreatedColStrings.Objects[i].Free;
  for i := FCreatedRowStrings.Count - 1 downto 0 do
    FCreatedRowStrings.Objects[i].Free;
  FreeAndNil(FCreatedColStrings);
  FreeAndNil(FCreatedRowStrings);
  inherited;
end;

function TTntStringGrid.CreateEditor: TInplaceEdit{TNT-ALLOW TInplaceEdit};
begin
  Result := TTntInplaceEdit.Create(Self);
end;

procedure TTntStringGrid.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TTntStringGrid.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntStringGrid.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntStringGrid.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntStringGrid.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

function TTntStringGrid.GetCells(ACol, ARow: Integer): WideString;
begin
  Result := UTF8ToWideString(inherited Cells[ACol, ARow])
end;

procedure TTntStringGrid.SetCells(ACol, ARow: Integer; const Value: WideString);
var
  UTF8Str: AnsiString;
begin
  UTF8Str := WideStringToUTF8(Value);
  if inherited Cells[ACol, ARow] <> UTF8Str then
    inherited Cells[ACol, ARow] := UTF8Str;
end;

function TTntStringGrid.FindGridStrings(const IsCol: Boolean; const ListIndex: Integer): TTntStrings;
var
  idx: integer;
  SrcStrings: TStrings{TNT-ALLOW TStrings};
  RCIndex: Integer;
begin
  if IsCol then
    SrcStrings := FCreatedColStrings
  else
    SrcStrings := FCreatedRowStrings;
  Assert(Assigned(SrcStrings));
  idx := SrcStrings.IndexOf(IntToStr(ListIndex));
  if idx <> -1 then
    Result := SrcStrings.Objects[idx] as TTntStrings
  else begin
    if IsCol then RCIndex := -ListIndex - 1 else RCIndex := ListIndex + 1;
    Result := TTntStringGridStrings.Create(Self, RCIndex);
    SrcStrings.AddObject(IntToStr(ListIndex), Result);
  end;
end;

function TTntStringGrid.GetCols(Index: Integer): TTntStrings;
begin
  Result := FindGridStrings(True, Index);
end;

function TTntStringGrid.GetRows(Index: Integer): TTntStrings;
begin
  Result := FindGridStrings(False, Index);
end;

procedure TTntStringGrid.SetCols(Index: Integer; const Value: TTntStrings);
begin
  FindGridStrings(True, Index).Assign(Value);
end;

procedure TTntStringGrid.SetRows(Index: Integer; const Value: TTntStrings);
begin
  FindGridStrings(False, Index).Assign(Value);
end;

procedure TTntStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  SaveDefaultDrawing: Boolean;
begin
  if DefaultDrawing then
    WideCanvasTextRect(Canvas, ARect, ARect.Left+2, ARect.Top+2, Cells[ACol, ARow]);
  SaveDefaultDrawing := DefaultDrawing;
  try
    DefaultDrawing := False;
    inherited DrawCell(ACol, ARow, ARect, AState);
  finally
    DefaultDrawing := SaveDefaultDrawing;
  end;
end;

function TTntStringGrid.GetEditText(ACol, ARow: Integer): WideString;
begin
  Result := Cells[ACol, ARow];
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TTntStringGrid.InternalSetEditText(ACol, ARow: Integer; const Value: string{TNT-ALLOW string});
begin
  if not FSettingEditText then
    SetEditText(ACol, ARow, TntControl_GetText(InplaceEditor));
end;

procedure TTntStringGrid.SetEditText(ACol, ARow: Integer; const Value: WideString);
begin
  FSettingEditText := True;
  try
    inherited SetEditText(ACol, ARow, WideStringToUTF8(Value));
  finally
    FSettingEditText := False;
  end;
  if Assigned(FOnSetEditText) then FOnSetEditText(Self, ACol, ARow, Value);
end;

procedure TTntStringGrid.WMChar(var Msg: TWMChar);
begin
  if (goEditing in Options)
  and (AnsiChar(Msg.CharCode) in [^H, #32..#255]) then begin
    RestoreWMCharMsg(TMessage(Msg));
    ShowEditorChar(WideChar(Msg.CharCode));
  end else
    inherited;
end;

procedure TTntStringGrid.ShowEditorChar(Ch: WideChar);
begin
  ShowEditor;
  if InplaceEditor <> nil then begin
    if Win32PlatformIsUnicode then
      PostMessageW(InplaceEditor.Handle, WM_CHAR, Word(Ch), 0)
    else
      PostMessageA(InplaceEditor.Handle, WM_CHAR, Word(Ch), 0);
  end;
end;

procedure TTntStringGrid.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntStringGrid.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

end.
