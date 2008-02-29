unit PngFunctions;

{$I ..\Include\Thany.inc}

interface

uses
  Windows, Graphics, Classes, ImgList, Contnrs, pngimage;

type
  TPngOption = (pngBlendOnDisabled, pngGrayscaleOnDisabled);
  TPngOptions = set of TPngOption;
  TRGBLine = array[Word] of TRGBTriple;
  PRGBLine = ^TRGBLine;
  TRGBALine = array[Word] of TRGBQuad;
  PRGBALine = ^TRGBALine;

procedure MakeImageBlended(Image: TPNGObject; Amount: Byte = 127);
procedure MakeImageGrayscale(Image: TPNGObject; Amount: Byte = 255);
procedure DrawPNG(Png: TPNGObject; Canvas: TCanvas; const Rect: TRect; const Options: TPngOptions);
procedure ConvertToPNG(Source: TGraphic; out Dest: TPNGObject);
procedure CreatePNG(Color, Mask: TBitmap; out Dest: TPNGObject; InverseMask: Boolean = False);
procedure CreatePNGMasked(Bitmap: TBitmap; Mask: TColor; out Dest: TPNGObject);
procedure CopyImageFromImageList(Dest: TPNGObject; ImageList: TCustomImageList; Index: Integer);
procedure SlicePNG(JoinedPNG: TPNGObject; Columns, Rows: Integer; out SlicedPNGs: TObjectList);

implementation

uses SysUtils, PngImageList;

function ColorToTriple(Color: TColor): TRGBTriple;
begin
Color := ColorToRGB(Color);
Result.rgbtBlue := Color shr 16 and $FF;
Result.rgbtGreen := Color shr 8 and $FF;
Result.rgbtRed := Color and $FF;
end;

procedure MakeImageBlended(Image: TPNGObject; Amount: Byte = 127);

  procedure ForceAlphachannel(BitTransparency: Boolean; TransparentColor: TColor);
  var
     Assigner: TBitmap;
     Temp: TPNGObject;
     X, Y: Integer;
     Line: pngimage.PByteArray;
     Current: TColor;
  begin
  //Not all formats of PNG support an alpha-channel (paletted images for example),
  //so with this function, I simply recreate the PNG as being 32-bits, effectivly
  //forcing an alpha-channel on it.
  Temp := TPNGObject.Create;
  try
    Assigner := TBitmap.Create;
    try
      Assigner.Width := Image.Width;
      Assigner.Height := Image.Height;
      Temp.Assign(Assigner);
    finally
      Assigner.Free;
     end;
    Temp.CreateAlpha;
    for Y := 0 to Image.Height - 1
    do begin
       Line := Temp.AlphaScanline[Y];
       for X := 0 to Image.Width - 1
       do begin
          Current := Image.Pixels[X, Y];
          Temp.Pixels[X, Y] := Current;
          if BitTransparency and (Current = TransparentColor)
          then Line^[X] := 0
          else Line^[X] := Amount;
          end;
       end;
    Image.Assign(Temp);
  finally
    Temp.Free;
   end;
  end;

var
   X, Y: Integer;
   Line: pngimage.PByteArray;
   Forced: Boolean;
   TransparentColor: TColor;
   BitTransparency: Boolean;
begin
//If the PNG is doesn't have an alpha channel, then add one
BitTransparency := Image.TransparencyMode = ptmBit;
TransparentColor := Image.TransparentColor;
if not (Image.Header.ColorType in [COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA])
then begin
     Forced := Image.Header.ColorType in [COLOR_GRAYSCALE, COLOR_PALETTE];
     if Forced
     then ForceAlphachannel(BitTransparency, TransparentColor)
     else Image.CreateAlpha;
     end
else Forced := False;

//Divide the alpha values by 2
if not Forced and (Image.Header.ColorType in [COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA])
then for Y := 0 to Image.Height - 1
     do begin
        Line := Image.AlphaScanline[Y];
        for X := 0 to Image.Width - 1
        do if BitTransparency and (Image.Pixels[X, Y] = TransparentColor)
           then Line^[X] := 0
           else Line^[X] := Round(Line^[X] / 256 * (Amount + 1));
        end;
end;

procedure MakeImageGrayscale(Image: TPNGObject; Amount: Byte = 255);

  procedure GrayscaleRGB(var R, G, B: Byte);
  var
     X: Byte;
  begin
  X := Round(R * 0.30 + G * 0.59 + B * 0.11);
  R := Round(R / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
  G := Round(G / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
  B := Round(B / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
  end;

var
   X, Y, PalCount: Integer;
   Line: Pointer;
   PaletteHandle: HPalette;
   Palette: array[Byte] of TPaletteEntry;
begin
//Don't do anything if the image is already a grayscaled one
if not (Image.Header.ColorType in [COLOR_GRAYSCALE, COLOR_GRAYSCALEALPHA])
then begin
     if Image.Header.ColorType = COLOR_PALETTE
     then begin
          //Grayscale every palette entry
          PaletteHandle := Image.Palette;
          PalCount := GetPaletteEntries(PaletteHandle, 0, 256, Palette);
          for X := 0 to PalCount - 1
          do GrayscaleRGB(Palette[X].peRed, Palette[X].peGreen, Palette[X].peBlue);
          SetPaletteEntries(PaletteHandle, 0, PalCount, Palette);
          Image.Palette := PaletteHandle;
          end
     else begin
          //Grayscale every pixel
          for Y := 0 to Image.Height - 1
          do begin
             Line := Image.Scanline[Y];
             for X := 0 to Image.Width - 1
             do GrayscaleRGB(PRGBLine(Line)^[X].rgbtRed, PRGBLine(Line)^[X].rgbtGreen, PRGBLine(Line)^[X].rgbtBlue);
             end;
          end;
     end;
end;

procedure DrawPNG(Png: TPNGObject; Canvas: TCanvas; const Rect: TRect; const Options: TPngOptions);
var
   PngCopy: TPNGObject;
begin
if Options <> []
then begin
     PngCopy := TPNGObject.Create;
     try
       PngCopy.Assign(Png);
       if pngBlendOnDisabled in Options
       then MakeImageBlended(PngCopy);
       if pngGrayscaleOnDisabled in Options
       then MakeImageGrayscale(PngCopy);
       PngCopy.Draw(Canvas, Rect);
     finally
       PngCopy.Free;
      end;
     end
else Png.Draw(Canvas, Rect);
end;

procedure ConvertToPNG(Source: TGraphic; out Dest: TPNGObject);
var
   MaskLines: array of pngimage.PByteArray;

  function CompareColors(const Color1: TRGBTriple; const Color2: TColor): Boolean;
  begin
  Result := (Color1.rgbtBlue = Color2 shr 16 and $FF) and
            (Color1.rgbtGreen = Color2 shr 8 and $FF) and
            (Color1.rgbtRed = Color2 and $FF);
  end;

  function ColorToTriple(const Color: TColor): TRGBTriple;
  begin
  Result.rgbtBlue := Color shr 16 and $FF;
  Result.rgbtGreen := Color shr 8 and $FF;
  Result.rgbtRed := Color and $FF;
  end;

  procedure GetAlphaMask(SourceColor: TBitmap);
  type
     TBitmapInfo = packed record
       bmiHeader: TBitmapV4Header; //Otherwise I may not get per-pixel alpha values.
       bmiColors: array[0..0] of TRGBQuad;
     end;
  var
     Bits: PRGBALine;
     BitmapInfo: TBitmapInfo;
     I, X, Y: Integer;
     HasAlpha: Boolean;
  begin
  Bits := AllocMem(4 * SourceColor.Width * SourceColor.Height);
  try
    ZeroMemory(Bits, 4 * SourceColor.Width * SourceColor.Height);
    ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
    BitmapInfo.bmiHeader.bV4Size := SizeOf(BitmapInfo.bmiHeader);
    BitmapInfo.bmiHeader.bV4Width := SourceColor.Width;
    BitmapInfo.bmiHeader.bV4Height := -SourceColor.Height; //Otherwise the image is upside down.
    BitmapInfo.bmiHeader.bV4Planes := 1;
    BitmapInfo.bmiHeader.bV4BitCount := 32;
    BitmapInfo.bmiHeader.bV4V4Compression := BI_BITFIELDS;
    BitmapInfo.bmiHeader.bV4SizeImage := 4 * SourceColor.Width * SourceColor.Height;

    if GetDIBits(SourceColor.Canvas.Handle, SourceColor.Handle, 0, SourceColor.Height, Bits, Windows.PBitmapInfo(@BitmapInfo)^, DIB_RGB_COLORS) > 0
    then begin
         //Because Win32 API is a piece of crap when it comes to icons, I have to check
         //whether an has an alpha-channel the hard way.
         I := 0;
         HasAlpha := False;
         for Y := 0 to SourceColor.Height - 1
         do for X := 0 to SourceColor.Width - 1
            do begin
               if Bits^[I].rgbReserved <> 0
               then HasAlpha := True;
               Inc(I);
               end;
         if HasAlpha
         then begin
              //OK, so not all alpha-values are 0, which indicates the existence of an
              //alpha-channel.
              I := 0;
              for Y := 0 to SourceColor.Height - 1
              do for X := 0 to SourceColor.Width - 1
                 do begin
                    MaskLines[Y]^[X] := Bits^[I].rgbReserved;
                    Inc(I);
                    end;
              end;
         end;
  finally
    FreeMem(Bits, 4 * SourceColor.Width * SourceColor.Height);
   end;
  end;

  function WinXPOrHigher: Boolean;
  var
     Info: TOSVersionInfo;
  begin
  Info.dwOSVersionInfoSize := SizeOf(Info);
  GetVersionEx(Info);
  Result := (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and ((Info.dwMajorVersion > 5) or ((Info.dwMajorVersion = 5) and (Info.dwMinorVersion >= 1)));
  end;

var
   Temp, SourceColor, SourceMask: TBitmap;
   X, Y: Integer;
   Line: PRGBLine;
   MaskLine, AlphaLine: pngimage.PByteArray;
   TransparentColor, CurrentColor: TColor;
   IconInfo: TIconInfo;
   AlphaNeeded: Boolean;
begin
//A PNG does not have to be converted
if Source is TPNGObject
then begin
     Dest := TPNGObject.Create;
     Dest.Assign(Source);
     Exit;
     end;

AlphaNeeded := False;
Temp := TBitmap.Create;
SetLength(MaskLines, Source.Height);
for Y := 0 to Source.Height - 1
do begin
   MaskLines[Y] := AllocMem(Source.Width);
   FillMemory(MaskLines[Y], Source.Width, 255);
   end;
try
  //Initialize intermediate color bitmap
  Temp.Width := Source.Width;
  Temp.Height := Source.Height;
  Temp.PixelFormat := pf24bit;

  //Now figure out the transparency
  if Source is TBitmap
  then if Source.Transparent
       then begin
            //TBitmap is just about comparing the drawn colors against the TransparentColor
            if TBitmap(Source).TransparentMode = tmFixed
            then TransparentColor := TBitmap(Source).TransparentColor
            else TransparentColor := TBitmap(Source).Canvas.Pixels[0, Source.Height - 1];
            for Y := 0 to Temp.Height - 1
            do begin
               Line := Temp.ScanLine[Y];
               MaskLine := MaskLines[Y];
               for X := 0 to Temp.Width - 1
               do begin
                  CurrentColor := GetPixel(TBitmap(Source).Canvas.Handle, X, Y);
                  if CurrentColor = TransparentColor
                  then begin
                       MaskLine^[X] := 0;
                       AlphaNeeded := True;
                       end;
                  Line^[X] := ColorToTriple(CurrentColor);
                  end;
               end;
            end
       else Temp.Canvas.Draw(0, 0, Source)
  else if Source is TIcon
  then begin
       //TIcon is more complicated, because there are bitmasked (classic) icons and
       //alphablended (modern) icons. Not to forget about the "inverse" color.
       GetIconInfo(TIcon(Source).Handle, IconInfo);
       SourceColor := TBitmap.Create;
       SourceMask := TBitmap.Create;
       try
         SourceColor.Handle := IconInfo.hbmColor;
         SourceMask.Handle := IconInfo.hbmMask;
         Temp.Canvas.Draw(0, 0, SourceColor);
         for Y := 0 to Temp.Height - 1
         do begin
            MaskLine := MaskLines[Y];
            for X := 0 to Temp.Width - 1
            do if GetPixel(SourceMask.Canvas.Handle, X, Y) <> 0
               then begin
                    MaskLine^[X] := 0;
                    AlphaNeeded := True;
                    end;
            end;
         if (GetDeviceCaps(SourceColor.Canvas.Handle, BITSPIXEL) = 32) and WinXPOrHigher
         then begin
              //This doesn't neccesarily mean we actually have 32bpp in the icon, because the
              //bpp of an icon is always the same as the display settings, regardless of the
              //actual color depth of the icon :(
              AlphaNeeded := True;
              GetAlphaMask(SourceColor);
              end;
         //This still doesn't work for alphablended icons...
       finally
         SourceColor.Free;
         SourceMask.Free
        end;
       end;

  //And finally, create the destination PNG image
  Dest := TPNGObject.Create;
  Dest.Assign(Temp);
  if AlphaNeeded
  then begin
       Dest.CreateAlpha;
       for Y := 0 to Dest.Height - 1
       do begin
          AlphaLine := Dest.AlphaScanline[Y];
          CopyMemory(AlphaLine, MaskLines[Y], Temp.Width);
          end;
       end;

finally
  for Y := 0 to Source.Height - 1
  do FreeMem(MaskLines[Y], Source.Width);
  Temp.Free;
 end;
end;

procedure CreatePNG(Color, Mask: TBitmap; out Dest: TPNGObject; InverseMask: Boolean = False);
var
   Temp: TBitmap;
   Line: pngimage.PByteArray;
   X, Y: Integer;
begin
//Create a PNG from two separate color and mask bitmaps. InverseMask should be
//True if white means transparent, and black means opaque.
Dest := TPNGObject.Create;
if not (Color.PixelFormat in [pf24bit, pf32bit])
then begin
     Temp := TBitmap.Create;
     try
       Temp.Assign(Color);
       Temp.PixelFormat := pf24bit;
       Dest.Assign(Temp);
     finally
       Temp.Free;
      end;
     end
else Dest.Assign(Color);

//Copy the alpha channel.
Dest.CreateAlpha;
for Y := 0 to Dest.Height - 1
do begin
   Line := Dest.AlphaScanline[Y];
   for X := 0 to Dest.Width - 1
   do if InverseMask
      then Line^[X] := 255 - (GetPixel(Mask.Canvas.Handle, X, Y) and $FF)
      else Line^[X] := GetPixel(Mask.Canvas.Handle, X, Y) and $FF;
   end;
end;

procedure CreatePNGMasked(Bitmap: TBitmap; Mask: TColor; out Dest: TPNGObject);
var
   Temp: TBitmap;
   Line: pngimage.PByteArray;
   X, Y: Integer;
begin
//Create a PNG from two separate color and mask bitmaps. InverseMask should be
//True if white means transparent, and black means opaque.
Dest := TPNGObject.Create;
if not (Bitmap.PixelFormat in [pf24bit, pf32bit])
then begin
     Temp := TBitmap.Create;
     try
       Temp.Assign(Bitmap);
       Temp.PixelFormat := pf24bit;
       Dest.Assign(Temp);
     finally
       Temp.Free;
      end;
     end
else Dest.Assign(Bitmap);

//Copy the alpha channel.
Dest.CreateAlpha;
for Y := 0 to Dest.Height - 1
do begin
   Line := Dest.AlphaScanline[Y];
   for X := 0 to Dest.Width - 1
   do Line^[X] := Integer(TColor(GetPixel(Bitmap.Canvas.Handle, X, Y)) <> Mask) * $FF;
   end;
end;

procedure CopyImageFromImageList(Dest: TPNGObject; ImageList: TCustomImageList; Index: Integer);
var
   Icon: TIcon;
   IconInfo: TIconInfo;
   ColorBitmap, MaskBitmap: TBitmap;
   X, Y: Integer;
   AlphaLine: pngimage.PByteArray;
   Png: TPngImageCollectionItem;
begin
if ImageList is TPngImageList
then begin
     //This is easy, just copy the PNG object from the imagelist to the PNG object
     //from the button
     Png := TPNGImageList(ImageList).PngImages[Index];
     if Png <> nil
     then Dest.Assign(Png.PngImage);
     end
else begin
     Icon := TIcon.Create;
     ColorBitmap := TBitmap.Create;
     MaskBitmap := TBitmap.Create;
     try
       //Try to copy an icon to a PNG object, including transparency
       ImageList.GetIcon(Index, Icon);
       if GetIconInfo(Icon.Handle, IconInfo)
       then begin
            //First, pump the colors into the PNG object
            ColorBitmap.Handle := IconInfo.hbmColor;
            ColorBitmap.PixelFormat := pf24bit;
            Dest.Assign(ColorBitmap);

            //Finally, copy the transparency
            Dest.CreateAlpha;
            MaskBitmap.Handle := IconInfo.hbmMask;
            for Y := 0 to Dest.Height - 1
            do begin
               AlphaLine := Dest.AlphaScanline[Y];
               for X := 0 to Dest.Width - 1
               do AlphaLine^[X] := Integer(GetPixel(MaskBitmap.Canvas.Handle, X, Y) = COLORREF(clBlack)) * 255;
               end;
            end;
     finally
       MaskBitmap.Free;
       ColorBitmap.Free;
       Icon.Free;
      end;
     end;
end;

procedure SlicePNG(JoinedPNG: TPNGObject; Columns, Rows: Integer; out SlicedPNGs: TObjectList);
var
   X, Y, ImageX, ImageY, OffsetX, OffsetY: Integer;
   Width, Height: Integer;
   Bitmap: TBitmap;
   BitmapLine: PRGBLine;
   AlphaLineA, AlphaLineB: pngimage.PByteArray;
   PNG: TPNGObject;
begin
//This function slices a large PNG file (e.g. an image with all images for a
//toolbar) into smaller, equally-sized pictures.
SlicedPNGs := TObjectList.Create(False);
Width := JoinedPNG.Width div Columns;
Height := JoinedPNG.Height div Rows;

//Loop through the columns and rows to create each individual image
for ImageY := 0 to Rows - 1
do begin
   for ImageX := 0 to Columns - 1
   do begin
      OffsetX := ImageX * Width;
      OffsetY := ImageY * Height;
      Bitmap := TBitmap.Create;
      try
        Bitmap.Width := Width;
        Bitmap.Height := Height;
        Bitmap.PixelFormat := pf24bit;

        //Copy the color information into a temporary bitmap. We can't use TPNGObject.Draw
        //here, because that would combine the color and alpha values.
        for Y := 0 to Bitmap.Height - 1
        do begin
           BitmapLine := Bitmap.Scanline[Y];
           for X := 0 to Bitmap.Width - 1
           do BitmapLine^[X] := ColorToTriple(JoinedPNG.Pixels[X + OffsetX, Y + OffsetY]);
           end;

        PNG := TPNGObject.Create;
        PNG.Assign(Bitmap);

        if JoinedPNG.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA]
        then begin
             //Copy the alpha channel
             PNG.CreateAlpha;
             for Y := 0 to PNG.Height - 1
             do begin
                AlphaLineA := JoinedPNG.AlphaScanline[Y + OffsetY];
                AlphaLineB := JoinedPNG.AlphaScanline[Y];
                for X := 0 to PNG.Width - 1
                do AlphaLineB^[X] := AlphaLineA^[X + OffsetX];
                end;
             end;

        SlicedPNGs.Add(PNG);
      finally
        Bitmap.Free;
       end;
      end;
   end;
end;

end.
