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
  Graphics, SynCompletion, Types
  {$IFDEF HEIDI_LINUX_QT}
  , Forms, LMessages, LResources
  {$ENDIF};

type

  // Delphi type aliases
  TSynMemo = TSynEdit;
  TVirtualStringTree = TLazVirtualStringTree;

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
      function ConvertCodeStringToCommand(AString: string): TSynEditorCommand;
      function IndexToEditorCommand(const AIndex: Integer): Integer;
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

function CreateVirtualStringTree(AOwner: TComponent): TVirtualStringTree;

const
{$IFDEF SYN_CodeFolding}
  EditorCommandStrs: array[0..109] of TIdentMapEntry = (
{$ELSE}
  EditorCommandStrs: array[0..97] of TIdentMapEntry = (
{$ENDIF}
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGotoXY'),
    (Value: ecSelLeft; Name: 'ecSelLeft'),
    (Value: ecSelRight; Name: 'ecSelRight'),
    (Value: ecSelUp; Name: 'ecSelUp'),
    (Value: ecSelDown; Name: 'ecSelDown'),
    (Value: ecSelWordLeft; Name: 'ecSelWordLeft'),
    (Value: ecSelWordRight; Name: 'ecSelWordRight'),
    (Value: ecSelLineStart; Name: 'ecSelLineStart'),
    (Value: ecSelLineEnd; Name: 'ecSelLineEnd'),
    (Value: ecSelPageUp; Name: 'ecSelPageUp'),
    (Value: ecSelPageDown; Name: 'ecSelPageDown'),
    (Value: ecSelPageLeft; Name: 'ecSelPageLeft'),
    (Value: ecSelPageRight; Name: 'ecSelPageRight'),
    (Value: ecSelPageTop; Name: 'ecSelPageTop'),
    (Value: ecSelPageBottom; Name: 'ecSelPageBottom'),
    (Value: ecSelEditorTop; Name: 'ecSelEditorTop'),
    (Value: ecSelEditorBottom; Name: 'ecSelEditorBottom'),
    (Value: ecSelGotoXY; Name: 'ecSelGotoXY'),
    //(Value: ecSelWord; Name: 'ecSelWord'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecDeleteLastChar; Name: 'ecDeleteLastChar'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBOL; Name: 'ecDeleteBOL'),
    (Value: ecDeleteEOL; Name: 'ecDeleteEOL'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClearAll; Name: 'ecClearAll'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecImeStr; Name: 'ecImeStr'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecMatchBracket; Name: 'ecMatchBracket'),
    ///(Value: ecCommentBlock; Name: 'ecCommentBlock'),
    (Value: ecNormalSelect; Name: 'ecNormalSelect'),
    (Value: ecColumnSelect; Name: 'ecColumnSelect'),
    (Value: ecLineSelect; Name: 'ecLineSelect'),
    (Value: ecAutoCompletion; Name: 'ecAutoCompletion'),
    (Value: ecUserFirst; Name: 'ecUserFirst'),
    //(Value: ecContextHelp; Name: 'ecContextHelp'),
    (Value: ecGotoMarker0; Name: 'ecGotoMarker0'),
    (Value: ecGotoMarker1; Name: 'ecGotoMarker1'),
    (Value: ecGotoMarker2; Name: 'ecGotoMarker2'),
    (Value: ecGotoMarker3; Name: 'ecGotoMarker3'),
    (Value: ecGotoMarker4; Name: 'ecGotoMarker4'),
    (Value: ecGotoMarker5; Name: 'ecGotoMarker5'),
    (Value: ecGotoMarker6; Name: 'ecGotoMarker6'),
    (Value: ecGotoMarker7; Name: 'ecGotoMarker7'),
    (Value: ecGotoMarker8; Name: 'ecGotoMarker8'),
    (Value: ecGotoMarker9; Name: 'ecGotoMarker9'),
    (Value: ecSetMarker0; Name: 'ecSetMarker0'),
    (Value: ecSetMarker1; Name: 'ecSetMarker1'),
    (Value: ecSetMarker2; Name: 'ecSetMarker2'),
    (Value: ecSetMarker3; Name: 'ecSetMarker3'),
    (Value: ecSetMarker4; Name: 'ecSetMarker4'),
    (Value: ecSetMarker5; Name: 'ecSetMarker5'),
    (Value: ecSetMarker6; Name: 'ecSetMarker6'),
    (Value: ecSetMarker7; Name: 'ecSetMarker7'),
    (Value: ecSetMarker8; Name: 'ecSetMarker8'),
    (Value: ecSetMarker9; Name: 'ecSetMarker9'),
    (Value: {%H-}ecUpperCase; Name: 'ecUpperCase'),
    (Value: {%H-}ecLowerCase; Name: 'ecLowerCase'),
    (Value: {%H-}ecToggleCase; Name: 'ecToggleCase'),
    (Value: {%H-}ecTitleCase; Name: 'ecTitleCase'),
    (Value: {%H-}ecUpperCaseBlock; Name: 'ecUpperCaseBlock'),
    (Value: {%H-}ecLowerCaseBlock; Name: 'ecLowerCaseBlock'),
    (Value: {%H-}ecToggleCaseBlock; Name: 'ecToggleCaseBlock'),
    //(Value: ecTitleCaseBlock; Name: 'ecTitleCaseBlock'),
{$IFDEF SYN_CodeFolding}
    (Value: ecString; Name:'ecString'),
    (Value: ecFoldAll; Name:'ecFoldAll'),
    (Value: ecUnfoldAll; Name:'ecUnfoldAll'),
    (Value: ecFoldNearest; Name:'ecFoldNearest'),
    (Value: ecUnfoldNearest; Name:'ecUnfoldNearest'),
    (Value: ecFoldLevel1; Name:'ecFoldLevel1'),
    (Value: ecFoldLevel2; Name:'ecFoldLevel2'),
    (Value: ecFoldLevel3; Name:'ecFoldLevel3'),
    (Value: ecUnfoldLevel1; Name:'ecUnfoldLevel1'),
    (Value: ecUnfoldLevel2; Name:'ecUnfoldLevel2'),
    (Value: ecUnfoldLevel3; Name:'ecUnfoldLevel3'),
    (Value: ecFoldRegions; Name:'ecFoldRanges'),
    (Value: ecUnfoldRegions; Name:'ecUnfoldRanges'));
{$ELSE}
    (Value: ecString; Name:'ecString'));
{$ENDIF}


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

function CreateVirtualStringTree(AOwner: TComponent): TVirtualStringTree;
begin
  {$IFDEF HEIDI_LINUX_QT}
  Result := THeidiVirtualStringTree.Create(AOwner);
  {$ELSE}
  Result := TVirtualStringTree.Create(AOwner);
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

function TSynEditHelper.ConvertCodeStringToCommand(AString: string): TSynEditorCommand;
var
  I: Integer;
begin
  Result := ecNone;

  AString := Uppercase(AString);
  for i := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    if Uppercase(EditorCommandStrs[i].Name) = AString then
    begin
      Result := EditorCommandStrs[i].Value;
      Break;
    end;
end;


function TSynEditHelper.IndexToEditorCommand(const AIndex: Integer): Integer;
begin
  Result := EditorCommandStrs[AIndex].Value;
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

