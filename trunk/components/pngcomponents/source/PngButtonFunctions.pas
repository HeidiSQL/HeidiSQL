unit PngButtonFunctions;

{$I ..\Include\Thany.inc}

interface

uses
  Windows, Buttons, Graphics, Classes, pngimage;

procedure CalcButtonLayout(Canvas: TCanvas; PngImage: TPNGObject; const Client: TRect; Pressed, Down: Boolean; const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer; var GlyphPos, TextPos: TPoint; BiDiFlags: LongInt);

implementation

procedure CalcButtonLayout(Canvas: TCanvas; PngImage: TPNGObject; const Client: TRect; Pressed, Down: Boolean; const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer; var GlyphPos, TextPos: TPoint; BiDiFlags: LongInt);
var
   ClientSize, GlyphSize, TextSize, TotalSize: TPoint;
   TextBounds: TRect;
begin
if (BiDiFlags and DT_RIGHT) = DT_RIGHT
then if Layout = blGlyphLeft
     then Layout := blGlyphRight
     else if Layout = blGlyphRight
          then Layout := blGlyphLeft;

//Calculate the item sizes
ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

if PngImage <> nil
then GlyphSize := Point(PngImage.Width, PngImage.Height)
else GlyphSize := Point(0, 0);

if Length(Caption) > 0
then begin
     TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
     DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT or BiDiFlags);
     TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
     end
else begin
     TextBounds := Rect(0, 0, 0, 0);
     TextSize := Point(0,0);
     end;

//If the layout has the glyph on the right or the left, then both the
//text and the glyph are centered vertically.  If the glyph is on the top
//or the bottom, then both the text and the glyph are centered horizontally.
if Layout in [blGlyphLeft, blGlyphRight]
then GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2
else GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;

//If there is no text or no bitmap, then Spacing is irrelevant
if (TextSize.X = 0) or (GlyphSize.X = 0)
then Spacing := 0;

//Adjust Margin and Spacing
if Margin = -1
then if Spacing = -1
     then begin
          TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
          if Layout in [blGlyphLeft, blGlyphRight]
          then Margin := (ClientSize.X - TotalSize.X) div 3
          else Margin := (ClientSize.Y - TotalSize.Y) div 3;
          end
     else begin
          TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
          if Layout in [blGlyphLeft, blGlyphRight]
          then Margin := (ClientSize.X - TotalSize.X) div 2
          else Margin := (ClientSize.Y - TotalSize.Y) div 2;
          end
else if Spacing = -1
     then TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y - (Margin + GlyphSize.Y));

case Layout of
     blGlyphLeft:   GlyphPos.X := Margin;
     blGlyphRight:  GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
     blGlyphTop:    GlyphPos.Y := Margin;
     blGlyphBottom: GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
     end;

if Layout in [blGlyphLeft, blGlyphRight]
then TextPos.Y := (ClientSize.Y - TextSize.Y) div 2
else TextPos.X := (ClientSize.X - TextSize.X) div 2;
case Layout of
     blGlyphLeft:   TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
     blGlyphRight:  TextPos.X := GlyphPos.X - Spacing - TextSize.X;
     blGlyphTop:    TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
     blGlyphBottom: TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
     end;

//Fixup the result variables
with GlyphPos
do begin
   Inc(X, Client.Left + Integer(Pressed or Down));
   Inc(Y, Client.Top + Integer(Pressed or Down));
   end;
with TextPos
do begin
   Inc(X, Client.Left + Integer(Pressed or Down));
   Inc(Y, Client.Top + Integer(Pressed or Down));
   end;
end;

end.
