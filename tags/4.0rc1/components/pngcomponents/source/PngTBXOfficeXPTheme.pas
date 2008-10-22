{***********************************************************}
{                    PngTBXOfficeXPTheme                    }
{       A PNG-aware version of the TBX OfficeXP theme       }
{                                                           }
{              Copyright (c) 2004 Martijn Saly              }
{***********************************************************}

unit PngTBXOfficeXPTheme;

interface

uses
  Windows, Graphics, ImgList, TB2Item, TBXUtils, TBXThemes, TBXOfficeXPTheme,
  PngImageList, PngFunctions, pngimage;

type
  TPngTBXOfficeXPTheme = class(TTBXOfficeXPTheme)
  public
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
  end;

implementation

const
  ThemeName = 'OfficeXP';

{ Globals }

procedure HighlightTBXIcon(Canvas: TCanvas; const R: TRect; ImageList: TCustomImageList; ImageIndex: Integer; HighlightColor: TColor; Amount: Byte);
var
   ImageWidth, ImageHeight, X, Y: Integer;
   DestPng, Png: TPNGObject;
   W1, W2: Byte;
   CBRB, CBG, S, C: Cardinal;
   Assigner: TBitmap;
   TransparencyColor: TColor;
   Line: PByteArray;
begin
//If the imagelist is not the PngImageList, then invoke the default call.
if not (ImageList is TPngImageList)
then begin
     TBXUtils.HighlightTBXIcon(Canvas, R, ImageList, ImageIndex, HighlightColor, Amount);
     Exit;
     end;

//Get the size of the image
ImageWidth := R.Right - R.Left;
ImageHeight := R.Bottom - R.Top;
with ImageList
do begin
   if Width < ImageWidth
   then ImageWidth := Width;
   if Height < ImageHeight
   then ImageHeight :=  Height;
   end;

//Recieve a copy of the image in the imagelist
Png := TPngImageList(ImageList).PngImages[ImageIndex].PngImage;
DestPng := TPNGObject.Create;
try
  //Create a new PNG by assigning a TBitmap
  Assigner := TBitmap.Create;
  try
    Assigner.Width := Png.Width;
    Assigner.Height := Png.Height;
    DestPng.Assign(Assigner);
  finally
    Assigner.Free;
   end;

  //Copy alpha channel, if available
  if Png.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA]
  then begin
       DestPng.CreateAlpha;
       for Y := 0 to Png.Height - 1
       do CopyMemory(DestPng.AlphaScanline[Y], Png.AlphaScanline[Y], Png.Width);
       end
  else if Png.TransparencyMode = ptmBit
       then begin
            TransparencyColor := Png.TransparentColor;
            DestPng.CreateAlpha;
            for Y := 0 to Png.Height - 1
            do begin
               Line := DestPng.AlphaScanline[Y];
               for X := 0 to Png.Width - 1
               do if Png.Pixels[X, Y] = TransparencyColor
                  then Line^[X] := 0
                  else Line^[X] := 255;
               end;
            end;

  //Initialize variables that help generate the lighted icon
  W2 := Amount;
  W1 := 255 - W2;
  HighlightColor := GetBGR(ColorToRGB(HighlightColor));
  CBRB := (Cardinal(HighlightColor) and $00FF00FF) * W1;
  CBG := (Cardinal(HighlightColor) and $0000FF00) * W1;

  //Loop through every pixel
  for Y := 0 to ImageHeight - 1
  do for X := 0 to ImageWidth - 1
     do begin
        //Lighten a pixel, basically the same way as in the original OfficeXP theme,
        //only this function preserves the alpha channel
        S := Png.Pixels[X, Y];
        C := ((S and $FF00FF) * W2 + CBRB) and $FF00FF00 + ((S and $00FF00) * W2 + CBG) and $00FF0000;
        DestPng.Pixels[X, Y] := C shr 8;
        end;
  DestPng.Draw(Canvas, R);
finally
  DestPng.Free;
 end;
end;

procedure DrawTBXIconFullShadow(Canvas: TCanvas; const R: TRect; ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);

  function Perform_DSPDxax(const Source, Dest, Brush: Byte): Byte;
  begin
  //It was neccesary to translate the ternary raster operation ROP_DSPDxax into
  //normal code, because it's not using a Blt function.
  //See also: http://msdn.microsoft.com/library/en-us/gdi/pantdraw_6n77.asp
  Result := Dest xor Brush and Source xor Dest;
  end;

var
   Assigner: TBitmap;
   ImageWidth, ImageHeight, X, Y: Integer;
   DestPng, Png: TPNGObject;
   DestColor, TransparencyColor: TColor;
   BrushR, BrushG, BrushB, DestR, DestG, DestB: Byte;
   Line: PByteArray;
begin
//If the imagelist is not the PngImageList, then invoke the default call.
if not (ImageList is TPngImageList)
then begin
     TBXUtils.DrawTBXIconFullShadow(Canvas, R, ImageList, ImageIndex, ShadowColor);
     Exit;
     end;

//Get the size of the image
ImageWidth := R.Right - R.Left;
ImageHeight := R.Bottom - R.Top;
with ImageList
do begin
   if Width < ImageWidth
   then ImageWidth := Width;
   if Height < ImageHeight
   then ImageHeight :=  Height;
   end;

//Recieve a copy of the image in the imagelist
Png := TPngImageList(ImageList).PngImages[ImageIndex].PngImage;
DestPng := TPNGObject.Create;
try
  //Create a new PNG by assigning a TBitmap
  Assigner := TBitmap.Create;
  try
    Assigner.Width := Png.Width;
    Assigner.Height := Png.Height;
    DestPng.Assign(Assigner);
  finally
    Assigner.Free;
   end;

  //Copy alpha channel, if available
  if Png.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA]
  then begin
       DestPng.CreateAlpha;
       for Y := 0 to Png.Height - 1
       do CopyMemory(DestPng.AlphaScanline[Y], Png.AlphaScanline[Y], Png.Width);
       end
  else if Png.TransparencyMode = ptmBit
       then begin
            TransparencyColor := Png.TransparentColor;
            DestPng.CreateAlpha;
            for Y := 0 to Png.Height - 1
            do begin
               Line := DestPng.AlphaScanline[Y];
               for X := 0 to Png.Width - 1
               do if Png.Pixels[X, Y] = TransparencyColor
                  then Line^[X] := 0
                  else Line^[X] := 255;
               end;
            end;

  //BrushX values for the raster opration
  BrushR := ShadowColor and $FF;
  BrushG := ShadowColor shr 8 and $FF;
  BrushB := ShadowColor shr 16 and $FF;

  //Loop through every pixel to generate a shadow
  for Y := 0 to ImageHeight - 1
  do for X := 0 to ImageWidth - 1
     do begin
        //These call the raster operation that generate a shadow image. The raster
        //operation "DSPDxax" is basically the same as BitBlt with ROP_DSPDxax as the
        //last parameter, but since we're modifying the PNG image itself, the operation
        //needed to be translated to normal code.
        DestColor := GetPixel(Canvas.Handle, R.Left + X, R.Top + Y);
        DestR := Perform_DSPDxax(255, DestColor and $FF, BrushR);
        DestG := Perform_DSPDxax(255, DestColor shr 8 and $FF, BrushG);
        DestB := Perform_DSPDxax(255, DestColor shr 16 and $FF, BrushB);
        DestPng.Pixels[X, Y] := RGB(DestR, DestG, DestB);
        end;
  DestPng.Draw(Canvas, R);
finally
  DestPng.Free;
 end;
end;

procedure DrawTBXIconFlatShadow(Canvas: TCanvas; const R: TRect; ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);
var
   Png: TPNGObject;
begin
//If the imagelist is not the PngImageList, then invoke the default call.
if not (ImageList is TPngImageList)
then begin
     TBXUtils.DrawTBXIconFullShadow(Canvas, R, ImageList, ImageIndex, ShadowColor);
     Exit;
     end;

//Recieve a copy of the image in the imagelist, if available
Png := TPngImageList(ImageList).PngImages[ImageIndex].Duplicate;
try
  //Now draw the PNG with the appropriate options, to make it appear
  //disabled
  DrawPNG(Png, Canvas, R, TPngImageList(ImageList).PngOptions);
finally
  Png.Free;
 end;
end;

{ TPngTBXOfficeXPTheme }

procedure TPngTBXOfficeXPTheme.PaintImage(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
   HiContrast: Boolean;
begin
//This method is the same as in the original OfficeXP theme, for the most part.
with ItemInfo
do begin
   if ImageList is TTBCustomImageList
   then begin
        TTBCustomImageList(ImageList).DrawState(Canvas, ARect.Left, ARect.Top, ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
        Exit;
        end;

   HiContrast := IsDarkColor(GetItemImageBackground(ItemInfo), 64);
   if not Enabled
   then begin
        DrawTBXIconFlatShadow(Canvas, ARect, ImageList, ImageIndex, BtnItemColors[bisDisabled, ipText]);
        end
   else if Selected or Pushed or (HoverKind <> hkNone)
   then begin
        if not (Selected or Pushed and not IsPopupParent)
        then begin
             OffsetRect(ARect, 1, 1);
             DrawTBXIconFullShadow(Canvas, ARect, ImageList, ImageIndex, IconShadowColor);
             OffsetRect(ARect, -2, -2);
             end;
        DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast);
        end
   else if HiContrast or TBXHiContrast or TBXLoColor
   then DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
   else HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, clWindow, 178);
   end;
end;

initialization
  //Unregister the original OfficeXP theme and replace it with the PNG-aware
  //version. It is neccesary either to not include TBXOfficeXPTheme in your uses
  //clause, or to make sure this unit gets initialized *after* TBXOfficeXPTheme.
  if IsTBXThemeAvailable(ThemeName)
  then UnregisterTBXTheme(ThemeName);
  RegisterTBXTheme(ThemeName, TPngTBXOfficeXPTheme);

end.
