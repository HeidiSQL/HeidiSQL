unit RXUtils; //Some functions from RX's VCLUtils.Pas;

{$IFNDEF VER80}     {-- Delphi 1.0     }
 {$IFNDEF VER90}    {-- Delphi 2.0     }
  {$IFNDEF VER93}   {-- C++Builder 1.0 }
    {$DEFINE RX_D3} { Delphi 3.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$P+,W-,R-,V-}

interface

{$IFDEF WIN32}
uses Windows, Classes, Graphics;
{$ELSE}
uses WinTypes, WinProcs, Classes, Graphics;
{$ENDIF}

{ Windows resources (bitmaps and icons) VCL-oriented routines }
procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);

{ Service routines }
function Max(X, Y: Integer): Integer;
function Min(X, Y: Integer): Integer;

type
  TSMGradientDirection = (fdNone, fdTopToBottom, fdBottomToTop, fdLeftToRight, fdRightToLeft);

procedure SMDrawGradient(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TSMGradientDirection; NumColors: Byte);

{ Standard Windows colors that are not defined by Delphi }
const
{$IFNDEF WIN32}
  clInfoBk = TColor($02E1FFFF);
  clNone = TColor($02FFFFFF);
{$ENDIF}
  clCream = TColor($A6CAF0);
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($FFFBF0);

implementation

{ Service routines }
function Max(X, Y: Integer): Integer;
begin
  if X > Y then
    Result := X
  else
    Result := Y;
end;

function Min(X, Y: Integer): Integer;
begin
  if X < Y then
    Result := X
  else
    Result := Y;
end;

procedure SMDrawGradient(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TSMGradientDirection; NumColors: Byte);
var
  StartRGB: array[0..2] of Byte;    { Start RGB values }
  RGBDelta: array[0..2] of Integer; { Difference between start and end RGB values }
  ColorBand: TRect;                 { Color band rectangular coordinates }
  I, Delta: Integer;
  Brush: HBrush;
begin
  if (Direction=fdNone) then exit;

  if IsRectEmpty(ARect) then Exit;
  if NumColors < 2 then
  begin
    Brush := CreateSolidBrush(ColorToRGB(StartColor));
    FillRect(Canvas.Handle, ARect, Brush);
    DeleteObject(Brush);
    Exit;
  end;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  case Direction of
    fdTopToBottom, fdLeftToRight: begin
      { Set the Red, Green and Blue colors }
      StartRGB[0] := GetRValue(StartColor);
      StartRGB[1] := GetGValue(StartColor);
      StartRGB[2] := GetBValue(StartColor);
      { Calculate the difference between begin and end RGB values }
      RGBDelta[0] := GetRValue(EndColor) - StartRGB[0];
      RGBDelta[1] := GetGValue(EndColor) - StartRGB[1];
      RGBDelta[2] := GetBValue(EndColor) - StartRGB[2];
    end;
    fdBottomToTop, fdRightToLeft: begin
      { Set the Red, Green and Blue colors }
      { Reverse of TopToBottom and LeftToRight directions }
      StartRGB[0] := GetRValue(EndColor);
      StartRGB[1] := GetGValue(EndColor);
      StartRGB[2] := GetBValue(EndColor);
      { Calculate the difference between begin and end RGB values }
      { Reverse of TopToBottom and LeftToRight directions }
      RGBDelta[0] := GetRValue(StartColor) - StartRGB[0];
      RGBDelta[1] := GetGValue(StartColor) - StartRGB[1];
      RGBDelta[2] := GetBValue(StartColor) - StartRGB[2];
    end;
  end; {case}
  { Calculate the color band's coordinates }
  ColorBand := ARect;
  if Direction in [fdTopToBottom, fdBottomToTop] then
  begin
    NumColors := Max(2, Min(NumColors, ARect.Bottom - ARect.Top));
    Delta := (ARect.Bottom - ARect.Top) div NumColors;
  end
  else
  begin
    NumColors := Max(2, Min(NumColors, ARect.Right - ARect.Left));
    Delta := (ARect.Right - ARect.Left) div NumColors;
  end;
  with Canvas.Pen do
  begin { Set the pen style and mode }
    Style := psSolid;
    Mode := pmCopy;
  end;
  { Perform the fill }
  if Delta > 0 then
  begin
    for I := 0 to NumColors do
    begin
      case Direction of
        { Calculate the color band's top and bottom coordinates }
        fdTopToBottom, fdBottomToTop: begin
          ColorBand.Top := ARect.Top + I * Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end;
        { Calculate the color band's left and right coordinates }
        fdLeftToRight, fdRightToLeft: begin
          ColorBand.Left := ARect.Left + I * Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
      end;
      { Calculate the color band's color }
      Brush := CreateSolidBrush(RGB(
        StartRGB[0] + MulDiv(I, RGBDelta[0], NumColors - 1),
        StartRGB[1] + MulDiv(I, RGBDelta[1], NumColors - 1),
        StartRGB[2] + MulDiv(I, RGBDelta[2], NumColors - 1)));
      FillRect(Canvas.Handle, ColorBand, Brush);
      DeleteObject(Brush);
    end;
  end;
  if Direction in [fdTopToBottom, fdBottomToTop] then
    Delta := (ARect.Bottom - ARect.Top) mod NumColors
  else
    Delta := (ARect.Right - ARect.Left) mod NumColors;
  if Delta > 0 then
  begin
    case Direction of
      { Calculate the color band's top and bottom coordinates }
      fdTopToBottom, fdBottomToTop: begin
                                      ColorBand.Top := ARect.Bottom - Delta;
                                      ColorBand.Bottom := ColorBand.Top + Delta;
                                    end;
      { Calculate the color band's left and right coordinates }
      fdLeftToRight, fdRightToLeft: begin
                                      ColorBand.Left := ARect.Right - Delta;
                                      ColorBand.Right := ColorBand.Left + Delta;
                                    end;
    end; {case}
    case Direction of
      fdTopToBottom, fdLeftToRight: Brush := CreateSolidBrush(EndColor);
      else {fdBottomToTop, fdRightToLeft }
        Brush := CreateSolidBrush(StartColor);
    end;
    FillRect(Canvas.Handle, ColorBand, Brush);
    DeleteObject(Brush);
  end;
end;

function PaletteColor(Color: TColor): Longint;
const
{ TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  PaletteMask = $02000000;
begin
  Result := ColorToRGB(Color) or PaletteMask;
end;


{ Transparent bitmap }
procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPalette;
  TransparentColor: TColorRef);
var
  Color: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave: HBitmap;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBitmap;
  MemDC, BackDC, ObjectDC, SaveDC: HDC;
  palDst, palMem, palSave, palObj: HPalette;
begin
  { Create some DCs to hold temporary data }
  BackDC := CreateCompatibleDC(DstDC);
  ObjectDC := CreateCompatibleDC(DstDC);
  MemDC := CreateCompatibleDC(DstDC);
  SaveDC := CreateCompatibleDC(DstDC);
  { Create a bitmap for each DC }
  bmAndObject := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndBack := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndMem := CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave := CreateCompatibleBitmap(DstDC, SrcW, SrcH);
  { Each DC must select a bitmap object to store pixel data }
  bmBackOld := SelectObject(BackDC, bmAndBack);
  bmObjectOld := SelectObject(ObjectDC, bmAndObject);
  bmMemOld := SelectObject(MemDC, bmAndMem);
  bmSaveOld := SelectObject(SaveDC, bmSave);
  { Select palette }
  palDst := 0; palMem := 0; palSave := 0; palObj := 0;
  if Palette <> 0 then begin
    palDst := SelectPalette(DstDC, Palette, True);
    RealizePalette(DstDC);
    palSave := SelectPalette(SaveDC, Palette, False);
    RealizePalette(SaveDC);
    palObj := SelectPalette(ObjectDC, Palette, False);
    RealizePalette(ObjectDC);
    palMem := SelectPalette(MemDC, Palette, True);
    RealizePalette(MemDC);
  end;
  { Set proper mapping mode }
  SetMapMode(SrcDC, GetMapMode(DstDC));
  SetMapMode(SaveDC, GetMapMode(DstDC));
  { Save the bitmap sent here }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);
  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  Color := SetBkColor(SaveDC, PaletteColor(TransparentColor));
  { Create the object mask for the bitmap by performing a BitBlt()  }
  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);
  { Set the background color of the source DC back to the original  }
  SetBkColor(SaveDC, Color);
  { Create the inverse of the object mask }
  BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
  { Copy the background of the main DC to the destination }
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
  { Mask out the places where the bitmap will be placed }
  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
  { XOR the bitmap with the background on the destination DC }
  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
  { Copy the destination to the screen }
  BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0,
    SRCCOPY);
  { Restore palette }
  if Palette <> 0 then begin
    SelectPalette(MemDC, palMem, False);
    SelectPalette(ObjectDC, palObj, False);
    SelectPalette(SaveDC, palSave, False);
    SelectPalette(DstDC, palDst, True);
  end;
  { Delete the memory bitmaps }
  DeleteObject(SelectObject(BackDC, bmBackOld));
  DeleteObject(SelectObject(ObjectDC, bmObjectOld));
  DeleteObject(SelectObject(MemDC, bmMemOld));
  DeleteObject(SelectObject(SaveDC, bmSaveOld));
  { Delete the memory DCs }
  DeleteDC(MemDC);
  DeleteDC(BackDC);
  DeleteDC(ObjectDC);
  DeleteDC(SaveDC);
end;

procedure StretchBitmapTransparent(Dest: TCanvas; Bitmap: TBitmap;
  TransparentColor: TColor; DstX, DstY, DstW, DstH, SrcX, SrcY,
  SrcW, SrcH: Integer);
var
  CanvasChanging: TNotifyEvent;
  Temp: TBitmap;
begin
  if DstW <= 0 then DstW := Bitmap.Width;
  if DstH <= 0 then DstH := Bitmap.Height;
  if (SrcW <= 0) or (SrcH <= 0) then begin
    SrcX := 0; SrcY := 0;
    SrcW := Bitmap.Width;
    SrcH := Bitmap.Height;
  end;
  if not Bitmap.Monochrome then
    SetStretchBltMode(Dest.Handle, STRETCH_DELETESCANS);
  CanvasChanging := Bitmap.Canvas.OnChanging;
  try
    Bitmap.Canvas.OnChanging := nil;
    Temp := Bitmap;
    try
      if TransparentColor = clNone then begin
        StretchBlt(Dest.Handle, DstX, DstY, DstW, DstH, Temp.Canvas.Handle,
          SrcX, SrcY, SrcW, SrcH, Dest.CopyMode);
      end
      else
      begin
{$IFDEF RX_D3}
        if TransparentColor = clDefault then
          TransparentColor := Temp.Canvas.Pixels[0, Temp.Height - 1];
{$ENDIF}
        if Temp.Monochrome then TransparentColor := clWhite
        else TransparentColor := ColorToRGB(TransparentColor);
        StretchBltTransparent(Dest.Handle, DstX, DstY, DstW, DstH,
          Temp.Canvas.Handle, SrcX, SrcY, SrcW, SrcH, Temp.Palette,
          TransparentColor);
      end;
    finally
    end;
  finally
    Bitmap.Canvas.OnChanging := CanvasChanging;
  end;
end;

procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
begin
  StretchBitmapTransparent(Dest, Bitmap, TransparentColor, DstX, DstY,
    Bitmap.Width, Bitmap.Height, 0, 0, Bitmap.Width, Bitmap.Height);
end;

end.
