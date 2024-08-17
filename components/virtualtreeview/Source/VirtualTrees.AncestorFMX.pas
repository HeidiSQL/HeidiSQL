unit VirtualTrees.AncestorFMX;

{$SCOPEDENUMS ON}

{****************************************************************************************************************}
{ Project          : VirtualTrees                                                                                }
{                                                                                                                }
{ author           : Karol Bieniaszewski                                                                         }
{ year             : 2022                                                                                        }
{ contibutors      :                                                                                             }
{****************************************************************************************************************}

interface

uses
    System.Classes, System.UITypes,
    FMX.Graphics,
    VirtualTrees.FMX, VirtualTrees.BaseTree;

const
  EVENT_OBJECT_STATECHANGE         = $800A;

type
  TVTAncestorFMX = class abstract(TBaseVirtualTree)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

    function PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush; override;

    function GetClientHeight: Single; override;
    function GetClientWidth: Single; override;
    function GetClientRect: TRect; override;

    procedure NotifyAccessibleEvent(pEvent: Uint32 = EVENT_OBJECT_STATECHANGE); virtual;
    procedure HScrollChangeProc(Sender: TObject); override;
    procedure VScrollChangeProc(Sender: TObject); override;

    procedure Resize; override;
    //TODO: CopyCutPaste - need to be implemented
    {
    function PasteFromClipboard(): Boolean; override;
    procedure CopyToClipboard(); override;
    procedure CutToClipboard(); override;
    }
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation
uses
  System.SysUtils,
  FMX.Forms,
  VirtualTrees.Header,
  VirtualTrees.Types;

type
  TVTHeaderCracker = class(TVTHeader);

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); //wymaga BaseTree
Var MM: TWMMouse;
  hInfo: THitInfo;
  P: TPoint;
  isNC: Boolean;
begin
  P.X:= X;
  P.Y:= Y;
  if ClientRect.Contains(P) then
    begin
      isNc:= false;
    end else
    begin
      isNC:= true;
      P:= ClientToScreen(P);
    end;
  FillTWMMouse(MM, Button, Shift, P.X, P.Y, isNC, false);
  if TVTHeaderCracker(Header).HandleMessage(TMessage(MM)) then
    exit;//!!!

  FillTWMMouse(MM, Button, Shift, X, Y, isNC, false);
  // get information about the hit
  GetHitTestInfoAt(X, Y, True, hInfo);

  HandleMouseDown(MM, hInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); //wymaga BaseTree
Var MM: TWMMouse;
  hInfo: THitInfo;
  P: TPoint;
  isNC: Boolean;
begin
  P.X:= X;
  P.Y:= Y;
  if ClientRect.Contains(P) then
    begin
      isNc:= false;
    end else
    begin
      isNC:= true;
      P:= ClientToScreen(P);
    end;
  FillTWMMouse(MM, Button, Shift, P.X, P.Y, isNC, true);
  if TVTHeaderCracker(Header).HandleMessage(TMessage(MM)) then
    exit;//!!!

  FillTWMMouse(MM, Button, Shift, X, Y, isNC, true);
  // get information about the hit
  GetHitTestInfoAt(X, Y, True, hInfo);
  HandleMouseUp(MM, hInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); //wymaga BaseTree
Var M: TCMMouseWheel;
  P: TPoint;
begin
  P:= Screen.MousePos;
  if not ClientRect.Contains(P) then
	P:= ClientToScreen(P);

  M.Msg:= CM_MOUSEWHEEL;
  M.ShiftState:= Shift;
  M.WheelDelta:= WheelDelta;
  M.XPos:= P.X;
  M.YPos:= P.Y;
  M.Result:= 0;
  CMMouseWheel(M);
  Handled:= M.Result<>0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.NotifyAccessibleEvent(pEvent: Uint32);
begin
  // Currently empty by intention as highly platfrom depedant
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorFMX.PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush;
Var PatternBitmap: TBitmap;
  i_bmp, line, bit: Integer;
begin
  //FMX pattern brush is different then VCL. Where color is derived from current one...
  //We should have 2 brushes 1 for Tree lines 1 for grid lines
  //and recreate it every time when color is changing

  CurrentDottedBrush.Free;
  FDottedBrushGridLines.Free;

  Result := nil;
  for i_bmp:= 1 to 2 do
    begin
      PatternBitmap := TBitmap.Create(8, BitsLinesCount);
      PatternBitmap.Clear(TAlphaColorRec.Null); //fully transparent
      PatternBitmap.Canvas.BeginScene;

      PatternBitmap.Map(TMapAccess.Write, BitmapData);
      try
        {
        DestPitch := PixelFormatBytes[PatternBitmap.PixelFormat];
        System.Move(PAlphaColorArray(BitmapData.Data)[0], PAlphaColorArray(Bits)[0], 8 * 4);
        }
        for line:= 0 to BitsLinesCount-1 do
          begin
            for bit:= 0 to 7 do
              begin
                if PWordArray(Bits)^[line] and (1 shl bit)=0 then
                  BitmapData.SetPixel(bit, line, clWhite) else
                  begin
                    if i_bmp=1 then
                      BitmapData.SetPixel(bit, line, TreeColors.TreeLineColor) else
                      BitmapData.SetPixel(bit, line, TreeColors.GridLineColor);
                  end;
              end;
          end;
      finally
        PatternBitmap.UnMap(BitmapData);
      end;

      PatternBitmap.Canvas.EndScene;

      if i_bmp=1 then
        begin
          Result := TStrokeBrush.Create(TBrushKind.Bitmap, clWhite);
          Result.Bitmap.Bitmap.Assign(PatternBitmap);
        end else
        begin
          FDottedBrushGridLines := TStrokeBrush.Create(TBrushKind.Bitmap, clWhite);
          FDottedBrushGridLines.Bitmap.Bitmap.Assign(PatternBitmap);
        end;
      FreeAndNil(PatternBitmap);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.Resize;
Var M: TWMSize;
begin
  inherited;

  if FInCreate then
    exit; //!!

  M.Msg:= WM_SIZE;
  M.SizeType:= SIZE_RESTORED;
  M.Width:= Width;
  M.Height:= Height;
  M.Result:= 0;
  WMSize(M);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.VScrollChangeProc(Sender: TObject);
Var M: TWMHScroll;
begin
  M.Msg:= WM_VSCROLL;
  M.ScrollCode:= SB_THUMBPOSITION;
  M.Pos:= GetScrollPos(SB_VERT);
  M.ScrollBar:= SB_VERT;
  M.Result:= 0;

  WMVScroll(M);
  Repaint;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.HScrollChangeProc(Sender: TObject);
Var M: TWMHScroll;
begin
  M.Msg:= WM_HSCROLL;
  M.ScrollCode:= SB_THUMBPOSITION;
  M.Pos:= GetScrollPos(SB_HORZ);
  M.ScrollBar:= SB_HORZ;
  M.Result:= 0;

  WMHScroll(M);
  Repaint;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TVTAncestorFMX.Create(AOwner: TComponent);
begin
  FInCreate:= true;

  inherited;

  BackgroundOffsetX:= 0;
  BackgroundOffsetY:= 0;
  Margin:= 4;
  TextMargin:= 4;
  DefaultNodeHeight:= 18; //???
  Indent:= 18; //???

  FInCreate:= false;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorFMX.GetClientHeight: Single;
begin
  Result:= ClientRect.Height;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorFMX.GetClientWidth: Single;
begin
  Result:= ClientRect.Width;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorFMX.GetClientRect: TRect;
begin
  Result:= ClipRect;
  if Assigned(Header) then
    begin
      if TVTHeaderOption.hoVisible in Header.Options then
        Inc(Result.Top, Header.Height);
    end;
  if FVScrollBar.Visible then
    Dec(Result.Right, VScrollBar.Width);
  if HScrollBar.Visible then
    Dec(Result.Bottom, HScrollBar.Height);
    
  if Result.Left>Result.Right then
    Result.Left:= Result.Right;
    
  if Result.Top>Result.Bottom then
    Result.Top:= Result.Bottom;

  //OffsetRect(Result, OffsetX, OffsetY);
  //Dec(Result.Left, -OffsetX); //increase width
  //Dec(Result.Top, -OffsetY);  //increase height
end;

end.
