unit lazaruscompat;

{$mode delphi}{$H+}

{$IFDEF LINUX}
  {$if defined(LCLQt) or defined(LCLQt5) or defined(LCLQt6)}
    {$DEFINE HEIDI_LINUX_QT}
  {$endif}
{$ENDIF}

interface

uses
  Classes, SysUtils, SynEdit, SynEditKeyCmds, SynEditHighlighter, laz.VirtualTrees,
  Graphics, SynCompletion
  {$IFDEF HEIDI_LINUX_QT}
  , Forms, LMessages, LResources
  {$ENDIF};

type

  // VirtualTreeView fixes for Linux Qt.
  THeidiVirtualStringTree = class(TLazVirtualStringTree)
  {$IFDEF HEIDI_LINUX_QT}
  private
    FQtShowHorzGridLines: Boolean;
    FQtShowVertGridLines: Boolean;
    FQtWindowsHotTrack: Boolean;
    function BlendQtColor(BaseColor, AccentColor: TColor; AccentPercent: Byte): TColor;
    procedure DrawQtSolidLine(Canvas: TCanvas; Left, Top, Right, Bottom: Integer);
  protected
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMHintShowPause(var Message: TCMHintShowPause); message CM_HINTSHOWPAUSE;
    function QtHintMaxWidth: Integer;
    function WrapQtHintLongTokens(const S: String; MaxWidth: Integer): String;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex;
      TextType: TVSTTextType); override;
    procedure DrawDottedHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: Integer); override;
    procedure DrawDottedVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: Integer;
      UseSelectedBkColor: Boolean = False); override;
  public
    procedure ConfigureQtGridLines(ShowHorz, ShowVert: Boolean);
    procedure ConfigureQtHotTrack(Enabled: Boolean);
  {$ENDIF}
  end;

  {$IFDEF HEIDI_LINUX_QT}
  THeidiLRSObjectReader = class(TLRSObjectReader)
  public
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); override;
  end;
  {$ENDIF}

  TProgressBarState = (pbsNormal, pbsError, pbsPaused);

  // Add methods which exist in Delphi but not in Lazarus
  TSynEditHelper = class helper for TSynEdit
    public
      function GetTextLen: Integer;
      function HasText: Boolean;
  end;
  TSynHighlighterAttributesHelper = class helper for TSynHighlighterAttributes
    public
      procedure AssignColorAndStyle(Source: TSynHighlighterAttributes);
  end;
  TSynCompletionHelper = class helper for TSynCompletion
    public
      function IndexFromVisibleIndex(AIndex: Integer): Integer;
  end;

  TVTHeaderHelper = class helper for TVTHeader
    public
      // AutoFitColumns version which takes the header caption into account
      procedure AutoFitColumnsWithHeaderMin;
  end;

  TStringsHelper = class helper for TStrings
    public
      function Contains(const S: String): Boolean;
      function IsEmpty: Boolean;
  end;

function CreateVirtualStringTree(AOwner: TComponent): TLazVirtualStringTree;


implementation

{$IFDEF HEIDI_LINUX_QT}
uses
  Math, LazUTF8;

procedure THeidiLRSObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
begin
  inherited BeginComponent(Flags, AChildPos, CompClassName, CompName);
  if CompClassName = 'TLazVirtualStringTree' then
    CompClassName := THeidiVirtualStringTree.ClassName;
end;

const
  QtHintInitialPauseMs = 250;
  QtHintKeepAliveMs = 24 * 60 * 60 * 1000;

function THeidiVirtualStringTree.QtHintMaxWidth: Integer;
var
  Monitor: TMonitor;
  WorkWidth, PreferredWidth, MinimumWidth, ScreenCap: Integer;
begin
  PreferredWidth := Canvas.TextWidth(StringOfChar('0', 96)) + 24;
  MinimumWidth := Canvas.TextWidth(StringOfChar('0', 48)) + 24;
  Monitor := Screen.MonitorFromWindow(Handle);
  if Assigned(Monitor) then
    WorkWidth := Monitor.WorkareaRect.Right - Monitor.WorkareaRect.Left
  else
    WorkWidth := Screen.Width;

  Result := PreferredWidth;
  if WorkWidth > 0 then begin
    ScreenCap := (WorkWidth * 2) div 3;
    if ScreenCap < MinimumWidth then
      Result := ScreenCap
    else
      Result := Min(Result, ScreenCap);
  end;
  Result := Max(240, Result);
end;

function THeidiVirtualStringTree.WrapQtHintLongTokens(const S: String; MaxWidth: Integer): String;
const
  BreakChars = '.,;:/\_-+=)]}>';

  function WrapToken(Token: String): String;
  var
    CharCount, LowPos, HighPos, MidPos, FitPos, BreakPos, I: Integer;
    Prefix, C: String;
  begin
    Result := '';
    while (Token <> '') and (Canvas.TextWidth(Token) > MaxWidth) do begin
      CharCount := UTF8Length(Token);
      if CharCount <= 1 then
        Break;

      LowPos := 1;
      HighPos := CharCount;
      FitPos := 1;
      while LowPos <= HighPos do begin
        MidPos := (LowPos + HighPos) div 2;
        Prefix := UTF8Copy(Token, 1, MidPos);
        if Canvas.TextWidth(Prefix) <= MaxWidth then begin
          FitPos := MidPos;
          LowPos := MidPos + 1;
        end
        else
          HighPos := MidPos - 1;
      end;

      BreakPos := FitPos;
      for I := FitPos downto Max(1, (FitPos * 3) div 4) do begin
        C := UTF8Copy(Token, I, 1);
        if Pos(C, BreakChars) > 0 then begin
          BreakPos := I;
          Break;
        end;
      end;

      Result := Result + UTF8Copy(Token, 1, BreakPos) + LineEnding;
      Token := UTF8Copy(Token, BreakPos + 1, CharCount - BreakPos);
    end;
    Result := Result + Token;
  end;

var
  I, TokenStart: Integer;
  Token: String;
begin
  if (S = '') or (MaxWidth <= 0) then begin
    Result := S;
    Exit;
  end;

  Result := '';
  TokenStart := 1;
  I := 1;
  while I <= Length(S) do begin
    if S[I] in [#9, #10, #13, ' '] then begin
      if I > TokenStart then begin
        Token := Copy(S, TokenStart, I - TokenStart);
        Result := Result + WrapToken(Token);
      end;
      Result := Result + S[I];
      Inc(I);
      TokenStart := I;
    end
    else
      Inc(I);
  end;
  if TokenStart <= Length(S) then begin
    Token := Copy(S, TokenStart, Length(S) - TokenStart + 1);
    Result := Result + WrapToken(Token);
  end;
end;

procedure THeidiVirtualStringTree.CMHintShowPause(var Message: TCMHintShowPause);
begin
  if Assigned(Message.Pause) and (Message.WasActive = 0) and
    (Message.Pause^ > QtHintInitialPauseMs) then
    Message.Pause^ := QtHintInitialPauseMs;
  Message.Result := 0;
end;

procedure THeidiVirtualStringTree.CMHintShow(var Message: TCMHintShow);
var
  MaxWidth: Integer;
begin
  LastHintRect := Rect(0, 0, 0, 0);
  inherited;

  if (Message.Result <> 0) or not Assigned(Message.HintInfo) then
    Exit;

  MaxWidth := QtHintMaxWidth;
  Message.HintInfo^.HintMaxWidth := MaxWidth;
  Message.HintInfo^.HideTimeout := QtHintKeepAliveMs;
  Message.HintInfo^.ReshowTimeout := 0;
  Message.HintInfo^.HintStr := WrapQtHintLongTokens(Message.HintInfo^.HintStr, Max(1, MaxWidth - 16));
end;

function THeidiVirtualStringTree.BlendQtColor(BaseColor, AccentColor: TColor;
  AccentPercent: Byte): TColor;
var
  BaseRGB, AccentRGB: LongInt;
  BaseWeight: Integer;
begin
  BaseRGB := ColorToRGB(BaseColor);
  AccentRGB := ColorToRGB(AccentColor);
  BaseWeight := 100 - AccentPercent;
  Result := RGBToColor(
    (((BaseRGB and $FF) * BaseWeight) + ((AccentRGB and $FF) * AccentPercent)) div 100,
    ((((BaseRGB shr 8) and $FF) * BaseWeight) + (((AccentRGB shr 8) and $FF) * AccentPercent)) div 100,
    ((((BaseRGB shr 16) and $FF) * BaseWeight) + (((AccentRGB shr 16) and $FF) * AccentPercent)) div 100);
end;


procedure THeidiVirtualStringTree.DrawQtSolidLine(Canvas: TCanvas; Left, Top, Right, Bottom: Integer);
var
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
begin
  OldBrushColor := Canvas.Brush.Color;
  OldBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Colors.TreeLineColor;
    Canvas.FillRect(Rect(Left, Top, Right, Bottom));
  finally
    Canvas.Brush.Style := OldBrushStyle;
    Canvas.Brush.Color := OldBrushColor;
  end;
end;

procedure THeidiVirtualStringTree.ConfigureQtGridLines(ShowHorz, ShowVert: Boolean);
begin
  FQtShowHorzGridLines := ShowHorz;
  FQtShowVertGridLines := ShowVert;
end;

procedure THeidiVirtualStringTree.ConfigureQtHotTrack(Enabled: Boolean);
begin
  FQtWindowsHotTrack := Enabled;
end;

procedure THeidiVirtualStringTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
begin
  inherited DoBeforeCellPaint(Canvas, Node, Column, CellPaintMode, CellRect, ContentRect);
  if (CellPaintMode <> cpmPaint) or (not FQtWindowsHotTrack) or
    (Node <> HotNode) or (vsSelected in Node.States) then
    Exit;

  OldBrushColor := Canvas.Brush.Color;
  OldBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := BlendQtColor(OldBrushColor, clHighlight, 12);
    Canvas.FillRect(CellRect);
  finally
    Canvas.Brush.Style := OldBrushStyle;
    Canvas.Brush.Color := OldBrushColor;
  end;
end;

procedure THeidiVirtualStringTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited DoPaintText(Node, Canvas, Column, TextType);
  if FQtWindowsHotTrack and (Node = HotNode) then
    Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
end;

procedure THeidiVirtualStringTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const CellRect: TRect);
var
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
begin
  inherited DoAfterCellPaint(Canvas, Node, Column, CellRect);
  if not (FQtShowHorzGridLines or FQtShowVertGridLines) then
    Exit;

  OldBrushColor := Canvas.Brush.Color;
  OldBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Colors.GridLineColor;
    if FQtShowVertGridLines and (CellRect.Right > CellRect.Left) then
      Canvas.FillRect(Rect(CellRect.Right - 1, CellRect.Top, CellRect.Right, CellRect.Bottom));
    if FQtShowHorzGridLines and (CellRect.Bottom > CellRect.Top) then
      Canvas.FillRect(Rect(CellRect.Left, CellRect.Bottom - 1, CellRect.Right, CellRect.Bottom));
  finally
    Canvas.Brush.Style := OldBrushStyle;
    Canvas.Brush.Color := OldBrushColor;
  end;
end;

procedure THeidiVirtualStringTree.DrawDottedHLine(const PaintInfo: TVTPaintInfo;
  Left, Right, Top: Integer);
begin
  if LineStyle = lsSolid then
    DrawQtSolidLine(PaintInfo.Canvas, Min(Left, Right), Top, Max(Left, Right) + 1, Top + 1)
  else
    inherited DrawDottedHLine(PaintInfo, Left, Right, Top);
end;

procedure THeidiVirtualStringTree.DrawDottedVLine(const PaintInfo: TVTPaintInfo;
  Top, Bottom, Left: Integer; UseSelectedBkColor: Boolean);
begin
  if LineStyle = lsSolid then
    DrawQtSolidLine(PaintInfo.Canvas, Left, Min(Top, Bottom), Left + 1, Max(Top, Bottom) + 1)
  else
    inherited DrawDottedVLine(PaintInfo, Top, Bottom, Left, UseSelectedBkColor);
end;
{$ENDIF}

function CreateVirtualStringTree(AOwner: TComponent): TLazVirtualStringTree;
begin
  {$IFDEF HEIDI_LINUX_QT}
  Result := THeidiVirtualStringTree.Create(AOwner);
  {$ELSE}
  Result := TLazVirtualStringTree.Create(AOwner);
  {$ENDIF}
end;


function TSynEditHelper.GetTextLen: Integer;
begin
  Result := Length(Text);
end;

function TSynEditHelper.HasText: Boolean;
begin
  // Introduced because GetTextLen seems to be unreliable, probably due to the lack of Trim()
  Result := not Trim(Text).IsEmpty;
end;

procedure TSynHighlighterAttributesHelper.AssignColorAndStyle(Source: TSynHighlighterAttributes);
var
  bChanged: Boolean;
begin
  bChanged := False;
  if Background <> Source.Background then
  begin
    Background := Source.Background;
    bChanged := True;
  end;
  if Foreground <> Source.Foreground then
  begin
    Foreground := Source.Foreground;
    bChanged := True;
  end;
  if Style <> Source.Style then
  begin
    Style := Source.Style;
    bChanged := True;
  end;
  if bChanged then
    Changed;
end;

function TSynCompletionHelper.IndexFromVisibleIndex(AIndex: Integer): Integer;
begin
  Result := PtrInt(ItemList.Objects[AIndex]);
end;


procedure TVTHeaderHelper.AutoFitColumnsWithHeaderMin;
var
  cnv: TCanvas;
  c: Integer;
  CapWidth: Integer;
  ws: String;
  ExtraPad: Integer;
begin
  cnv := Treeview.Canvas;
  ExtraPad := 5;
  for c:=0 to Columns.Count - 1 do begin
    ws := Columns[c].Text;
    if ws <> '' then begin
      CapWidth := cnv.TextWidth(ws);
      Columns[c].MinWidth := CapWidth + (Columns[c].Margin * 2) + ExtraPad;
    end;
  end;
  // now let VST auto-fit using node content, but not below header-based MinWidth
  AutoFitColumns(False);
end;

function TStringsHelper.Contains(const S: String): Boolean;
begin
  Result := IndexOf(S) >= 0;
end;

function TStringsHelper.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

initialization
  {$IFDEF HEIDI_LINUX_QT}
  RegisterClass(THeidiVirtualStringTree);
  LRSObjectReaderClass := THeidiLRSObjectReader;
  {$ENDIF}

end.

