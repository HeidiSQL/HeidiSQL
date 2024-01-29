unit VirtualTrees.Utils;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is VirtualTrees.pas, released September 30, 2000.
//
// The initial developer of the original code is digital publishing AG (Munich, Germany, www.digitalpublishing.de),
// written by Mike Lischke (public@soft-gems.net, www.soft-gems.net).
//
// Portions created by digital publishing AG are Copyright
// (C) 1999-2001 digital publishing AG. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Types,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.Controls,
  VirtualTrees.Types;


type
  /// <summary>
  /// Describes the mode how to blend pixels.
  /// </summary>
  TBlendMode = (
    bmConstantAlpha,         // apply given constant alpha
    bmPerPixelAlpha,         // use alpha value of the source pixel
    bmMasterAlpha,           // use alpha value of source pixel and multiply it with the constant alpha value
    bmConstantAlphaAndColor  // blend the destination color with the given constant color und the constant alpha value
  );

procedure AlphaBlend(Source, Destination: HDC; R: TRect; Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);
function GetRGBColor(Value: TColor): DWORD;
procedure PrtStretchDrawDIB(Canvas: TCanvas; DestRect: TRect; ABitmap: TBitmap);

procedure SetBrushOrigin(Canvas: TCanvas; X, Y: Integer); inline;


procedure SetCanvasOrigin(Canvas: TCanvas; X, Y: Integer); inline;

/// <summary>
/// Clip a given canvas to ClipRect while transforming the given rect to device coordinates.
/// </summary>
procedure ClipCanvas(Canvas: TCanvas; ClipRect: TRect; VisibleRegion: HRGN = 0);

procedure DrawImage(ImageList: TCustomImageList; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);

/// <summary>
/// Adjusts the given string S so that it fits into the given width. EllipsisWidth gives the width of
/// the three points to be added to the shorted string. If this value is 0 then it will be determined implicitely.
/// For higher speed (and multiple entries to be shorted) specify this value explicitely.
/// </summary>
function ShortenString(DC: HDC; const S: string; Width: TDimension; EllipsisWidth: TDimension = 0): string; overload;

//--------------------------
// ShortenString similar to VTV's version, except:
// -- Does not assume using three dots or any particular character for ellipsis
// -- Does not add ellipsis to string, so could be added anywhere
// -- Requires EllipsisWidth, and zero does nothing special
// Returns:
//   ShortenedString as var param
//   True if shortened (ie: add ellipsis somewhere), otherwise false
function ShortenString(TargetCanvasDC: HDC; const StrIn: string; const AllowedWidth_px: Integer; const EllipsisWidth_px: Integer; var ShortenedString: string): boolean; overload;

/// <summary>
/// Wrap the given string S so that it fits into a space of given width.
/// RTL determines if right-to-left reading is active.
/// </summary>
function WrapString(DC: HDC; const S: string; const Bounds: TRect; RTL: Boolean; DrawFormat: Cardinal): string;

/// <summary>
/// Calculates bounds of a drawing rectangle for the given string
/// </summary>
procedure GetStringDrawRect(DC: HDC; const S: string; var Bounds: TRect; DrawFormat: Cardinal);

/// <summary>
/// Converts the incoming rectangle so that left and top are always less than or equal to right and bottom.
/// </summary>
function OrderRect(const R: TRect): TRect;

/// <summary>
/// Fills the given rectangles with values which can be used while dragging around an image
/// </summary>
/// <remarks>
/// (used in DragMove of the drag manager and DragTo of the header columns).
/// </remarks>
procedure FillDragRectangles(DragWidth, DragHeight, DeltaX, DeltaY: Integer; var RClip, RScroll, RSamp1, RSamp2, RDraw1, RDraw2: TRect);

/// <summary>
/// Attaches a bitmap as drag image to an IDataObject, see issue #405
/// <code>
/// Usage: Set property DragImageKind to diNoImage, in your event handler OnCreateDataObject
/// <para>       call VirtualTrees.Utils.ApplyDragImage() with your `IDataObject` and your bitmap.</para>
/// </code>
/// </summary>
procedure ApplyDragImage(const pDataObject: IDataObject; pBitmap: TBitmap);

/// <summary>
/// Returns True if the mouse cursor is currently visible and False in case it is suppressed.
/// Useful when doing hot-tracking on touchscreens, see issue #766
/// </summary>
function IsMouseCursorVisible(): Boolean;

procedure ScaleImageList(const ImgList: TImageList; M, D: Integer);

/// <summary>
/// Returns True if the high contrast theme is anabled in the system settings, False otherwise.
/// </summary>
function IsHighContrastEnabled(): Boolean;

/// <summary>
/// Divide depend of parameter type uses different division operator:
/// <code>Integer uses div</code>
/// <code>Single uses /</code>
/// </summary>
function Divide(const Dimension: Integer; const DivideBy: Integer): Integer; overload; inline;

/// <summary>
/// Divide depend of parameter type uses different division operator:
/// <code>Integer uses div</code>
/// <code>Single uses /</code>
/// </summary>
function Divide(const Dimension: Single; const DivideBy: Integer): Single; overload; inline;

implementation

uses
  Winapi.CommCtrl,
  Winapi.ShlObj,
  System.SysUtils,
  System.StrUtils,
  System.Math;

const
  WideLF = Char(#10);

procedure ApplyDragImage(const pDataObject: IDataObject; pBitmap: TBitmap);
var
  DragSourceHelper: IDragSourceHelper;
  DragInfo: SHDRAGIMAGE;
  lDragSourceHelper2: IDragSourceHelper2;// Needed to get Windows Vista+ style drag hints.
  lNullPoint: TPoint;
begin

  if Assigned(pDataObject) and Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER,
    IID_IDragSourceHelper, DragSourceHelper)) then
  begin
    if Supports(DragSourceHelper, IDragSourceHelper2, lDragSourceHelper2) then
      lDragSourceHelper2.SetFlags(DSH_ALLOWDROPDESCRIPTIONTEXT);// Show description texts
    if not Succeeded(DragSourceHelper.InitializeFromWindow(0, lNullPoint, pDataObject)) then begin   // First let the system try to initialze the DragSourceHelper, this works fine e.g. for file system objects
      // Create drag image

      if not Assigned(pBitmap) then
        Exit();
      DragInfo.crColorKey := clBlack;
      DragInfo.sizeDragImage.cx := pBitmap.Width;
      DragInfo.sizeDragImage.cy := pBitmap.Height;
      DragInfo.ptOffset.X := pBitmap.Width div 8;
      DragInfo.ptOffset.Y := pBitmap.Height div 10;
      DragInfo.hbmpDragImage := CopyImage(pBitmap.Handle, IMAGE_BITMAP, pBitmap.Width, pBitmap.Height, LR_COPYRETURNORG);
      if not Succeeded(DragSourceHelper.InitializeFromBitmap(@DragInfo, pDataObject)) then
        DeleteObject(DragInfo.hbmpDragImage);
    end;//if not InitializeFromWindow
  end;
end;


function OrderRect(const R: TRect): TRect;

begin
  if R.Left < R.Right then
  begin
    Result.Left := R.Left;
    Result.Right := R.Right;
  end
  else
  begin
    Result.Left := R.Right;
    Result.Right := R.Left;
  end;
  if R.Top < R.Bottom then
  begin
    Result.Top := R.Top;
    Result.Bottom := R.Bottom;
  end
  else
  begin
    Result.Top := R.Bottom;
    Result.Bottom := R.Top;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------


procedure SetBrushOrigin(Canvas: TCanvas; X, Y: Integer);

// Set the brush origin of a given canvas.

//var
//  P: TPoint;

begin
  //P := Point(X, Y);
  //LPtoDP(Canvas.Handle, P, 1);// No longer used, see issue #608
  //SetBrushOrgEx(Canvas.Handle, P.X, P.Y, nil);
  SetBrushOrgEx(Canvas.Handle, X, Y, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetCanvasOrigin(Canvas: TCanvas; X, Y: Integer);

// Set the coordinate space origin of a given canvas.

var
  P: TPoint;

begin
  // Reset origin as otherwise we would accumulate the origin shifts when calling LPtoDP.
  SetWindowOrgEx(Canvas.Handle, 0, 0, nil);

  // The shifting is expected in physical points, so we have to transform them accordingly.
  P := Point(X, Y);
  LPtoDP(Canvas.Handle, P, 1);

  // Do the shift.
  SetWindowOrgEx(Canvas.Handle, P.X, P.Y, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ClipCanvas(Canvas: TCanvas; ClipRect: TRect; VisibleRegion: HRGN = 0);

var
  ClipRegion: HRGN;

begin
  // Regions expect their coordinates in device coordinates, hence we have to transform the region rectangle.
  LPtoDP(Canvas.Handle, ClipRect, 2);
  ClipRegion := CreateRectRgnIndirect(ClipRect);
  if VisibleRegion <> 0 then
    CombineRgn(ClipRegion, ClipRegion, VisibleRegion, RGN_AND);
  SelectClipRgn(Canvas.Handle, ClipRegion);
  DeleteObject(ClipRegion);
end;

//----------------------------------------------------------------------------------------------------------------------


procedure GetStringDrawRect(DC: HDC; const S: string; var Bounds: TRect; DrawFormat: Cardinal);

begin
  Bounds.Right := Bounds.Left + 1;
  Bounds.Bottom := Bounds.Top + 1;

  Winapi.Windows.DrawTextW(DC, PWideChar(S), Length(S), Bounds, DrawFormat or DT_CALCRECT);
end;

//----------------------------------------------------------------------------------------------------------------------


function ShortenString(DC: HDC; const S: string; Width: TDimension; EllipsisWidth: TDimension = 0): string;

var
  Size: TSize;
  Len: Integer;
  L, H, N: Integer;
  W: TDimension;
  
begin
  Len := Length(S);
  if (Len = 0) or (Width <= 0) then
    Result := ''
  else
  begin
    // Determine width of triple point using the current DC settings (if not already done).
    if EllipsisWidth = 0 then
    begin
      GetTextExtentPoint32W(DC, '...', 3, Size);
      EllipsisWidth := Size.cx;
    end;

    begin
      // Do a binary search for the optimal string length which fits into the given width.
      L := 0;
      N := 0;
      W := Width;
      H := Len;
      while L < H do
      begin
        N := (L + H + 1) shr 1;
        GetTextExtentPoint32W(DC, PWideChar(S), N, Size);
        W := Size.cx + EllipsisWidth;
        if W <= Width then
          L := N
        else
          H := N - 1;
      end;
      if W <= Width then
        L := N;
      if L >= Len then
        Result := S
      else if Width <= EllipsisWidth then
        Result := ''
      else
        Result := Copy(S, 1, L) + '...';
    end;
  end;
end;


//--------------------------
function ShortenString(TargetCanvasDC: HDC; const StrIn: string; const AllowedWidth_px: Integer; const EllipsisWidth_px: Integer; var ShortenedString: string): boolean;
//--------------------------
var
  Size_px_x_px: TSize;  // cx, cy
  StrInLen: Integer;
  LoLen, HiLen, TestLen, TestWidth_px: Integer;

begin
  StrInLen := Length(StrIn);
  if (StrInLen = 0) then
  Begin
    ShortenedString := '';
    Result := False;  // No ellipsis needed since original was empty
  End else
  if (AllowedWidth_px <= 0) then
  Begin
    ShortenedString := '';
    Result := True;  // Ellipsis needed, since non-empty string replaced.
                     // But likely will get clipped if AllowedWidth is really zero
  End else
  begin
      // Do a binary search for the optimal string length which fits into the given width.
      LoLen := 0;
      TestLen := 0;
      TestWidth_px := AllowedWidth_px;
      HiLen := StrInLen;

      while LoLen < HiLen do
      begin
        TestLen := (LoLen + HiLen + 1) shr 1;  // Test average of Lo and Hi

        GetTextExtentPoint32W(TargetCanvasDC, PWideChar(StrIn), TestLen, Size_px_x_px);
        TestWidth_px := Size_px_x_px.cx + EllipsisWidth_px;

        if TestWidth_px <= AllowedWidth_px then
        Begin
          LoLen := TestLen      // Low bound must be at least as much as TestLen
        End else
        Begin
          HiLen := TestLen - 1; // Continue until Hi bound string produces width below AllowedWidth_px
        End;
      end;

      if TestWidth_px <= AllowedWidth_px then
      Begin
        LoLen := TestLen;
      End;
      if LoLen >= StrInLen then
      Begin
        ShortenedString := StrIn;
        Result := False;
      End else if AllowedWidth_px <= EllipsisWidth_px then
      Begin
        ShortenedString := '';
        Result      := True; // Even though Ellipsis won't fit in AllowedWidth,
                             // let clipping decide how much of ellipsis to show
      End else
      Begin
        ShortenedString := Copy(StrIn, 1, LoLen);
        Result := True;
      End;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------


function WrapString(DC: HDC; const S: string; const Bounds: TRect; RTL: Boolean; DrawFormat: Cardinal): string;

var
  Width,
  Len,
  WordCounter,
  WordsInLine,
  I, W: Integer;
  Buffer,
  Line: string;
  Words: array of string;
  R: TRect;

begin
  Result := '';
  // Leading and trailing are ignored.
  Buffer := Trim(S);
  Len := Length(Buffer);
  if Len < 1 then
    Exit;

  Width := Bounds.Right - Bounds.Left;
  R := Rect(0, 0, 0, 0);

  // Count the words in the string.
  WordCounter := 1;
  for I := 1 to Len do
    if Buffer[I] = ' ' then
      Inc(WordCounter);
  SetLength(Words, WordCounter);

  if RTL then
  begin
    // At first we split the string into words with the last word being the
    // first element in Words.
    W := 0;
    for I := 1 to Len do
      if Buffer[I] = ' ' then
        Inc(W)
      else
        Words[W] := Words[W] + Buffer[I];

    // Compose Result.
    while WordCounter > 0 do
    begin
      WordsInLine := 0;
      Line := '';

      while WordCounter > 0 do
      begin
        GetStringDrawRect(DC, Line + IfThen(WordsInLine > 0, ' ', '') + Words[WordCounter - 1], R, DrawFormat);
        if R.Right > Width then
        begin
          // If at least one word fits into this line then continue with the next line.
          if WordsInLine > 0 then
            Break;

          Buffer := Words[WordCounter - 1];
          if Len > 1 then
          begin
            for Len := Length(Buffer) - 1 downto 2 do
            begin
              GetStringDrawRect(DC, RightStr(Buffer, Len), R, DrawFormat);
              if R.Right <= Width then
                Break;
            end;
          end
          else
            Len := Length(Buffer);

          Line := Line + RightStr(Buffer, Max(Len, 1));
          Words[WordCounter - 1] := LeftStr(Buffer, Length(Buffer) - Max(Len, 1));
          if Words[WordCounter - 1] = '' then
            Dec(WordCounter);
          Break;
        end
        else
        begin
          Dec(WordCounter);
          Line := Words[WordCounter] + IfThen(WordsInLine > 0, ' ', '') + Line;
          Inc(WordsInLine);
        end;
      end;

      Result := Result + Line + WideLF;
    end;
  end
  else
  begin
    // At first we split the string into words with the last word being the
    // first element in Words.
    W := WordCounter - 1;
    for I := 1 to Len do
      if Buffer[I] = ' ' then
        Dec(W)
      else
        Words[W] := Words[W] + Buffer[I];

    // Compose Result.
    while WordCounter > 0 do
    begin
      WordsInLine := 0;
      Line := '';

      while WordCounter > 0 do
      begin
        GetStringDrawRect(DC, Line + IfThen(WordsInLine > 0, ' ', '') + Words[WordCounter - 1], R, DrawFormat);
        if R.Right > Width then
        begin
          // If at least one word fits into this line then continue with the next line.
          if WordsInLine > 0 then
            Break;

          Buffer := Words[WordCounter - 1];
          if Len > 1 then
          begin
            for Len := Length(Buffer) - 1 downto 2 do
            begin
              GetStringDrawRect(DC, LeftStr(Buffer, Len), R, DrawFormat);
              if R.Right <= Width then
                Break;
            end;
          end
          else
            Len := Length(Buffer);

          Line := Line + LeftStr(Buffer, Max(Len, 1));
          Words[WordCounter - 1] := RightStr(Buffer, Length(Buffer) - Max(Len, 1));
          if Words[WordCounter - 1] = '' then
            Dec(WordCounter);
          Break;
        end
        else
        begin
          Dec(WordCounter);
          Line := Line + IfThen(WordsInLine > 0, ' ', '') + Words[WordCounter];
          Inc(WordsInLine);
        end;
      end;

      Result := Result + Line + WideLF;
    end;
  end;

  Len := Length(Result);
  if Result[Len] = WideLF then
    SetLength(Result, Len - 1);
end;

//----------------------------------------------------------------------------------------------------------------------


function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;

// Helper function to calculate the start address for the given row.

begin
  if Height > 0 then  // bottom-up DIB
    Row := Height - Row - 1;
  // Return DWORD aligned address of the requested scanline.
  Result := PAnsiChar(Bits) + Row * ((Width * 32 + 31) and not 31) div 8;
end;



//----------------------------------------------------------------------------------------------------------------------

function GetBitmapBitsFromDeviceContext(DC: HDC; var Width, Height: Integer): Pointer;

// Helper function used to retrieve the bitmap selected into the given device context. If there is a bitmap then
// the function will return a pointer to its bits otherwise nil is returned.
// Additionally the dimensions of the bitmap are returned.

var
  Bitmap: HBITMAP;
  DIB: TDIBSection;

begin
  Result := nil;
  Width := 0;
  Height := 0;

  Bitmap := GetCurrentObject(DC, OBJ_BITMAP);
  if Bitmap <> 0 then
  begin
    if GetObject(Bitmap, SizeOf(DIB), @DIB) = SizeOf(DIB) then
    begin
      Assert(DIB.dsBm.bmPlanes * DIB.dsBm.bmBitsPixel = 32, 'Alpha blending error: bitmap must use 32 bpp.');
      Result := DIB.dsBm.bmBits;
      Width := DIB.dsBmih.biWidth;
      Height := DIB.dsBmih.biHeight;
    end;
  end;
  Assert(Result <> nil, 'Alpha blending DC error: no bitmap available.');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLineConstant(Source, Destination: Pointer; Count: Integer; ConstantAlpha, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using a constant alpha value.
// The layout of a pixel must be BGRA where A is ignored (but is calculated as the other components).
// ConstantAlpha must be in the range 0..255 where 0 means totally transparent (destination pixel only)
// and 255 totally opaque (source pixel only).
// Bias is an additional value which gets added to every component and must be in the range -128..127
//
{$ifdef CPUX64}
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains ConstantAlpha
// Bias is on the stack

asm
        //.NOFRAME

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOVD        XMM3, R9D  // ConstantAlpha
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Load XMM5 with the bias value.
        MOVD        XMM5, [Bias]
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}    // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RDX]}    // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM3    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW     XMM0, XMM4
        PADDSW    XMM0, XMM5
        PADDW     XMM0, XMM4
        PACKUSWB  XMM0, XMM0      // convert words to bytes with saturation
        MOVD      DWORD PTR [RDX], XMM0     // store the result
@3:
        ADD       RCX, 4
        ADD       RDX, 4
        DEC       R8D
        JNZ       @1
end;
{$else}
// EAX contains Source
// EDX contains Destination
// ECX contains Count
// ConstantAlpha and Bias are on the stack

asm
        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM6 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOV     EAX, [ConstantAlpha]
        DB      $0F, $6E, $F0          /// MOVD      MM6, EAX
        DB      $0F, $61, $F6          /// PUNPCKLWD MM6, MM6
        DB      $0F, $62, $F6          /// PUNPCKLDQ MM6, MM6

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C6          /// PMULLW    MM0, MM6,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
end;
{$endif CPUX64}

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLinePerPixel(Source, Destination: Pointer; Count, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using the alpha value of the source pixels.
// The layout of a pixel must be BGRA.
// Bias is an additional value which gets added to every component and must be in the range -128..127
//
{$ifdef CPUX64}
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains Bias

asm
        //.NOFRAME

        // Load XMM5 with the bias value.
        MOVD        XMM5, R9D   // Bias
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}    // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RDX]}    // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // Load XMM3 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        MOVQ        XMM3, XMM0
        PUNPCKHWD   XMM3, XMM3
        PUNPCKHDQ   XMM3, XMM3

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM3    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW       XMM0, XMM4
        PADDSW      XMM0, XMM5
        PADDW       XMM0, XMM4
        PACKUSWB    XMM0, XMM0    // convert words to bytes with saturation
        MOVD        DWORD PTR [RDX], XMM0   // store the result
@3:
        ADD         RCX, 4
        ADD         RDX, 4
        DEC         R8D
        JNZ         @1
end;
{$else}
// EAX contains Source
// EDX contains Destination
// ECX contains Count
// Bias is on the stack

asm
        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // Load MM6 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        DB      $0F, $6F, $F0          /// MOVQ MM6, MM0
        DB      $0F, $69, $F6          /// PUNPCKHWD MM6, MM6
        DB      $0F, $6A, $F6          /// PUNPCKHDQ MM6, MM6

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C6          /// PMULLW    MM0, MM6,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
end;
{$endif CPUX64}

//----------------------------------------------------------------------------------------------------------------------

procedure EMMS;

// Reset MMX state to use the FPU for other tasks again.

{$ifdef CPUX64}
  inline;
begin
end;
{$else}
asm
        DB      $0F, $77               /// EMMS
end;
{$endif CPUX64}

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLineMaster(Source, Destination: Pointer; Count: Integer; ConstantAlpha, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using the source pixel and a constant alpha value.
// The layout of a pixel must be BGRA.
// ConstantAlpha must be in the range 0..255.
// Bias is an additional value which gets added to every component and must be in the range -128..127
//
{$ifdef CPUX64}
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains ConstantAlpha
// Bias is on the stack

asm
        .SAVENV XMM6

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOVD        XMM3, R9D    // ConstantAlpha
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Load XMM5 with the bias value.
        MOV         R10D, [Bias]
        MOVD        XMM5, R10D
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}     // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RCX]}     // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // Load XMM6 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        MOVQ        XMM6, XMM0
        PUNPCKHWD   XMM6, XMM6
        PUNPCKHDQ   XMM6, XMM6
        PMULLW      XMM6, XMM3    // source alpha * master alpha
        PSRLW       XMM6, 8       // divide by 256

        // calculation is: target = (alpha * master alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM6    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW       XMM0, XMM4
        PADDSW      XMM0, XMM5
        PADDW       XMM0, XMM4
        PACKUSWB    XMM0, XMM0    // convert words to bytes with saturation
        MOVD        DWORD PTR [RDX], XMM0   // store the result
@3:
        ADD         RCX, 4
        ADD         RDX, 4
        DEC         R8D
        JNZ         @1
end;
{$else}
// EAX contains Source
// EDX contains Destination
// ECX contains Count
// ConstantAlpha and Bias are on the stack

asm
        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM6 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOV     EAX, [ConstantAlpha]
        DB      $0F, $6E, $F0          /// MOVD      MM6, EAX
        DB      $0F, $61, $F6          /// PUNPCKLWD MM6, MM6
        DB      $0F, $62, $F6          /// PUNPCKLDQ MM6, MM6

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // Load MM7 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        DB      $0F, $6F, $F8          /// MOVQ      MM7, MM0
        DB      $0F, $69, $FF          /// PUNPCKHWD MM7, MM7
        DB      $0F, $6A, $FF          /// PUNPCKHDQ MM7, MM7
        DB      $0F, $D5, $FE          /// PMULLW    MM7, MM6,   source alpha * master alpha
        DB      $0F, $71, $D7, $08     /// PSRLW     MM7, 8,     divide by 256

        // calculation is: target = (alpha * master alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C7          /// PMULLW    MM0, MM7,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
end;
{$endif CPUX64}

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLineMasterAndColor(Destination: Pointer; Count: Integer; ConstantAlpha, Color: Integer);

// Blends a line of Count pixels in Destination against the given color using a constant alpha value.
// The layout of a pixel must be BGRA and Color must be rrggbb00 (as stored by a COLORREF).
// ConstantAlpha must be in the range 0..255.
//
{$ifdef CPUX64}
// RCX contains Destination
// EDX contains Count
// R8D contains ConstantAlpha
// R9D contains Color

asm
        //.NOFRAME

        // The used formula is: target = (alpha * color + (256 - alpha) * target) / 256.
        // alpha * color (factor 1) and 256 - alpha (factor 2) are constant values which can be calculated in advance.
        // The remaining calculation is therefore: target = (F1 + F2 * target) / 256

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size. (Every calculation here works on word sized operands.)
        MOVD        XMM3, R8D   // ConstantAlpha
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Calculate factor 2.
        MOV         R10D, $100
        MOVD        XMM2, R10D
        PUNPCKLWD   XMM2, XMM2
        PUNPCKLDQ   XMM2, XMM2
        PSUBW       XMM2, XMM3             // XMM2 contains now: 255 - alpha = F2

        // Now calculate factor 1. Alpha is still in XMM3, but the r and b components of Color must be swapped.
        BSWAP       R9D  // Color
        ROR         R9D, 8
        MOVD        XMM1, R9D              // Load the color and convert to word sized values.
        PXOR        XMM4, XMM4
        PUNPCKLBW   XMM1, XMM4
        PMULLW      XMM1, XMM3             // XMM1 contains now: color * alpha = F1

@1:     // The pixel loop calculates an entire pixel in one run.
        MOVD        XMM0, DWORD PTR [RCX]
        PUNPCKLBW   XMM0, XMM4

        PMULLW      XMM0, XMM2             // calculate F1 + F2 * target
        PADDW       XMM0, XMM1
        PSRLW       XMM0, 8                // divide by 256

        PACKUSWB    XMM0, XMM0             // convert words to bytes with saturation
        MOVD        DWORD PTR [RCX], XMM0            // store the result

        ADD         RCX, 4
        DEC         EDX
        JNZ         @1
end;
{$else}
// EAX contains Destination
// EDX contains Count
// ECX contains ConstantAlpha
// Color is passed on the stack

asm
        // The used formula is: target = (alpha * color + (256 - alpha) * target) / 256.
        // alpha * color (factor 1) and 256 - alpha (factor 2) are constant values which can be calculated in advance.
        // The remaining calculation is therefore: target = (F1 + F2 * target) / 256

        // Load MM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size. (Every calculation here works on word sized operands.)
        DB      $0F, $6E, $D9          /// MOVD      MM3, ECX
        DB      $0F, $61, $DB          /// PUNPCKLWD MM3, MM3
        DB      $0F, $62, $DB          /// PUNPCKLDQ MM3, MM3

        // Calculate factor 2.
        MOV     ECX, $100
        DB      $0F, $6E, $D1          /// MOVD      MM2, ECX
        DB      $0F, $61, $D2          /// PUNPCKLWD MM2, MM2
        DB      $0F, $62, $D2          /// PUNPCKLDQ MM2, MM2
        DB      $0F, $F9, $D3          /// PSUBW     MM2, MM3             // MM2 contains now: 255 - alpha = F2

        // Now calculate factor 1. Alpha is still in MM3, but the r and b components of Color must be swapped.
        MOV     ECX, [Color]
        BSWAP   ECX
        ROR     ECX, 8
        DB      $0F, $6E, $C9          /// MOVD      MM1, ECX             // Load the color and convert to word sized values.
        DB      $0F, $EF, $E4          /// PXOR      MM4, MM4
        DB      $0F, $60, $CC          /// PUNPCKLBW MM1, MM4
        DB      $0F, $D5, $CB          /// PMULLW    MM1, MM3             // MM1 contains now: color * alpha = F1

@1:     // The pixel loop calculates an entire pixel in one run.
        DB      $0F, $6E, $00          /// MOVD      MM0, [EAX]
        DB      $0F, $60, $C4          /// PUNPCKLBW MM0, MM4

        DB      $0F, $D5, $C2          /// PMULLW    MM0, MM2             // calculate F1 + F2 * target
        DB      $0F, $FD, $C1          /// PADDW     MM0, MM1
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8               // divide by 256

        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0             // convert words to bytes with saturation
        DB      $0F, $7E, $00          /// MOVD      [EAX], MM0           // store the result

        ADD     EAX, 4
        DEC     EDX
        JNZ     @1
end;
{$endif CPUX64}

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlend(Source, Destination: HDC; R: TRect; Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);

// Optimized alpha blend procedure using MMX instructions to perform as quick as possible.
// For this procedure to work properly it is important that both source and target bitmap use the 32 bit color format.
// R describes the source rectangle to work on.
// Target is the place (upper left corner) in the target bitmap where to blend to. Note that source width + X offset
// must be less or equal to the target width. Similar for the height.
// If Mode is bmConstantAlpha then the blend operation uses the given ConstantAlpha value for all pixels.
// If Mode is bmPerPixelAlpha then each pixel is blended using its individual alpha value (the alpha value of the source).
// If Mode is bmMasterAlpha then each pixel is blended using its individual alpha value multiplied by ConstantAlpha.
// If Mode is bmConstantAlphaAndColor then each destination pixel is blended using ConstantAlpha but also a constant
// color which will be obtained from Bias. In this case no offset value is added, otherwise Bias is used as offset.
// Blending of a color into target only (bmConstantAlphaAndColor) ignores Source (the DC) and Target (the position).
// CAUTION: This procedure does not check whether MMX instructions are actually available! Call it only if MMX is really
//          usable.

var
  Y: Integer;
  SourceRun,
  TargetRun: PByte;

  SourceBits,
  DestBits: Pointer;
  SourceWidth,
  SourceHeight,
  DestWidth,
  DestHeight: Integer;

begin
  if not IsRectEmpty(R) then
  begin
    // Note: it is tempting to optimize the special cases for constant alpha 0 and 255 by just ignoring soure
    //       (alpha = 0) or simply do a blit (alpha = 255). But this does not take the bias into account.
    case Mode of
      bmConstantAlpha:
        begin
          // Get a pointer to the bitmap bits for the source and target device contexts.
          // Note: this supposes that both contexts do actually have bitmaps assigned!
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * R.Left);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              Inc(TargetRun, 4 * Target.X);
              AlphaBlendLineConstant(SourceRun, TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
      bmPerPixelAlpha:
        begin
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * R.Left);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              Inc(TargetRun, 4 * Target.X);
              AlphaBlendLinePerPixel(SourceRun, TargetRun, R.Right - R.Left, Bias);
            end;
          end;
          EMMS;
        end;
      bmMasterAlpha:
        begin
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * Target.X);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              AlphaBlendLineMaster(SourceRun, TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
      bmConstantAlphaAndColor:
        begin
          // Source is ignored since there is a constant color value.
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + R.Top);
              Inc(TargetRun, 4 * R.Left);
              AlphaBlendLineMasterAndColor(TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
    end;
  end;
end;

function GetRGBColor(Value: TColor): DWORD;

// Little helper to convert a Delphi color to an image list color.

begin
  Result := ColorToRGB(Value);
  case Result of
    clNone:
      Result := CLR_NONE;
    clDefault:
      Result := CLR_DEFAULT;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure PrtStretchDrawDIB(Canvas: TCanvas; DestRect: TRect; ABitmap: TBitmap);

// Stretch draw on to the new canvas.

var
  Header,
  Bits: Pointer;
  HeaderSize,
  BitsSize: Cardinal;

begin
  GetDIBSizes(ABitmap.Handle, HeaderSize, BitsSize);

  GetMem(Header, HeaderSize);
  GetMem(Bits, BitsSize);
  try
    GetDIB(ABitmap.Handle, ABitmap.Palette, Header^, Bits^);
    StretchDIBits(Canvas.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom -
      DestRect.Top, 0, 0, ABitmap.Width, ABitmap.Height, Bits, TBitmapInfo(Header^), DIB_RGB_COLORS, SRCCOPY);
  finally
    FreeMem(Header);
    FreeMem(Bits);
  end;
end;


//----------------------------------------------------------------------------------------------------------------------


procedure FillDragRectangles(DragWidth, DragHeight, DeltaX, DeltaY: Integer; var RClip, RScroll, RSamp1, RSamp2, RDraw1, RDraw2: TRect);

begin
  // ScrollDC limits
  RClip := Rect(0, 0, DragWidth, DragHeight);
  if DeltaX > 0 then
  begin
    // move to the left
    if DeltaY = 0 then
    begin
      // move only to the left
      // background movement
      RScroll := Rect(0, 0, DragWidth - DeltaX, DragHeight);
      RSamp1 := Rect(0, 0, DeltaX, DragHeight);
      RDraw1 := Rect(DragWidth - DeltaX, 0, DeltaX, DragHeight);
    end
    else
      if DeltaY < 0 then
      begin
        // move to bottom left
        RScroll := Rect(0, -DeltaY, DragWidth - DeltaX, DragHeight);
        RSamp1 := Rect(0, 0, DeltaX, DragHeight);
        RSamp2 := Rect(DeltaX, DragHeight + DeltaY, DragWidth - DeltaX, -DeltaY);
        RDraw1 := Rect(0, 0, DragWidth - DeltaX, -DeltaY);
        RDraw2 := Rect(DragWidth - DeltaX, 0, DeltaX, DragHeight);
      end
      else
      begin
        // move to upper left
        RScroll := Rect(0, 0, DragWidth - DeltaX, DragHeight - DeltaY);
        RSamp1 := Rect(0, 0, DeltaX, DragHeight);
        RSamp2 := Rect(DeltaX, 0, DragWidth - DeltaX, DeltaY);
        RDraw1 := Rect(0, DragHeight - DeltaY, DragWidth - DeltaX, DeltaY);
        RDraw2 := Rect(DragWidth - DeltaX, 0, DeltaX, DragHeight);
      end;
  end
  else
    if DeltaX = 0 then
    begin
      // vertical movement only
      if DeltaY < 0 then
      begin
        // move downwards
        RScroll := Rect(0, -DeltaY, DragWidth, DragHeight);
        RSamp2 := Rect(0, DragHeight + DeltaY, DragWidth, -DeltaY);
        RDraw2 := Rect(0, 0, DragWidth, -DeltaY);
      end
      else
      begin
        // move upwards
        RScroll := Rect(0, 0, DragWidth, DragHeight - DeltaY);
        RSamp2 := Rect(0, 0, DragWidth, DeltaY);
        RDraw2 := Rect(0, DragHeight - DeltaY, DragWidth, DeltaY);
      end;
    end
    else
    begin
      // move to the right
      if DeltaY > 0 then
      begin
        // move up right
        RScroll := Rect(-DeltaX, 0, DragWidth, DragHeight);
        RSamp1 := Rect(0, 0, DragWidth + DeltaX, DeltaY);
        RSamp2 := Rect(DragWidth + DeltaX, 0, -DeltaX, DragHeight);
        RDraw1 := Rect(0, 0, -DeltaX, DragHeight);
        RDraw2 := Rect(-DeltaX, DragHeight - DeltaY, DragWidth + DeltaX, DeltaY);
      end
      else
        if DeltaY = 0 then
        begin
          // to the right only
          RScroll := Rect(-DeltaX, 0, DragWidth, DragHeight);
          RSamp1 := Rect(DragWidth + DeltaX, 0, -DeltaX, DragHeight);
          RDraw1 := Rect(0, 0, -DeltaX, DragHeight);
        end
        else
        begin
          // move down right
          RScroll := Rect(-DeltaX, -DeltaY, DragWidth, DragHeight);
          RSamp1 := Rect(0, DragHeight + DeltaY, DragWidth + DeltaX, -DeltaY);
          RSamp2 := Rect(DragWidth + DeltaX, 0, -DeltaX, DragHeight);
          RDraw1 := Rect(0, 0, -DeltaX, DragHeight);
          RDraw2 := Rect(-DeltaX, 0, DragWidth + DeltaX, -DeltaY);
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TCustomImageListCast = class(TCustomImageList);

procedure DrawImage(ImageList: TCustomImageList; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
begin
  TCustomImageListCast(ImageList).DoDraw(Index, Canvas, X, Y, Style, Enabled)
end;

//----------------------------------------------------------------------------------------------------------------------

function IsMouseCursorVisible(): Boolean;
var
  CI: TCursorInfo;
begin
  CI.cbSize := SizeOf(CI);
  Result := GetCursorInfo(CI) and (CI.flags = CURSOR_SHOWING);
  // 0                     Hidden
  // CURSOR_SHOWING (1)    Visible
  // CURSOR_SUPPRESSED (2) Touch/Pen Input (Windows 8+)
  // https://msdn.microsoft.com/en-us/library/windows/desktop/ms648381(v=vs.85).aspx
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ScaleImageList(const ImgList: TImageList; M, D: Integer);
var
  ii : integer;
  mb, ib, sib, smb : TBitmap;
  TmpImgList : TImageList;
begin
  if M <= D then Exit;

  //clear images
  TmpImgList := TImageList.Create(nil);
  try
    TmpImgList.Assign(ImgList);

    ImgList.Clear;
    ImgList.SetSize(MulDiv(ImgList.Width, M, D), MulDiv(ImgList.Height, M, D));

    //add images back to original ImageList stretched (if DPI scaling > 150%) or centered (if DPI scaling <= 150%)
    for ii := 0 to -1 + TmpImgList.Count do
    begin
      ib := TBitmap.Create;
      mb := TBitmap.Create;
      try
        ib.SetSize(TmpImgList.Width, TmpImgList.Height);
        ib.Canvas.FillRect(ib.Canvas.ClipRect);

        mb.SetSize(TmpImgList.Width, TmpImgList.Height);
        mb.Canvas.FillRect(mb.Canvas.ClipRect);

        ImageList_DrawEx(TmpImgList.Handle, ii, ib.Canvas.Handle, 0, 0, ib.Width, ib.Height, CLR_NONE, CLR_NONE, ILD_NORMAL);
        ImageList_DrawEx(TmpImgList.Handle, ii, mb.Canvas.Handle, 0, 0, mb.Width, mb.Height, CLR_NONE, CLR_NONE, ILD_MASK);

        sib := TBitmap.Create; //stretched (or centered) image
        smb := TBitmap.Create; //stretched (or centered) mask
        try
          sib.SetSize(ImgList.Width, ImgList.Height);
          sib.Canvas.FillRect(sib.Canvas.ClipRect);
          smb.SetSize(ImgList.Width, ImgList.Height);
          smb.Canvas.FillRect(smb.Canvas.ClipRect);

          if M * 100 / D >= 150 then //stretch if >= 150%
          begin
            sib.Canvas.StretchDraw(Rect(0, 0, sib.Width, sib.Width), ib);
            smb.Canvas.StretchDraw(Rect(0, 0, smb.Width, smb.Width), mb);
          end
          else //center if < 150%
          begin
            sib.Canvas.Draw((sib.Width - ib.Width) DIV 2, (sib.Height - ib.Height) DIV 2, ib);
            smb.Canvas.Draw((smb.Width - mb.Width) DIV 2, (smb.Height - mb.Height) DIV 2, mb);
          end;
          ImgList.Add(sib, smb);
        finally
          sib.Free;
          smb.Free;
        end;
    finally
        ib.Free;
        mb.Free;
      end;
    end;
  finally
    TmpImgList.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function IsHighContrastEnabled(): Boolean;
var
  l: HIGHCONTRAST;
begin
  l.cbSize := SizeOf(l);
  Result := SystemParametersInfo(SPI_GETHIGHCONTRAST, 0, @l, 0) and ((l.dwFlags and HCF_HIGHCONTRASTON) <> 0);
end;

//----------------------------------------------------------------------------------------------------------------------
function Divide(const Dimension: Single; const DivideBy: Integer): Single;
begin
  Result:= Dimension / DivideBy;
end;

//----------------------------------------------------------------------------------------------------------------------

function Divide(const Dimension: Integer; const DivideBy: Integer): Integer;
begin
  Result:= Dimension div DivideBy;
end;

end.
