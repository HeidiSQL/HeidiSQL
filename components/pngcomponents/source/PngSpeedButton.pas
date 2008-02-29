unit PngSpeedButton;

{$I ..\Include\Thany.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Buttons, Graphics, ActnList,
  PngFunctions, PngButtonFunctions, pngimage;

type
  TPngSpeedButton = class(TSpeedButton)
  private
    FPngImage: TPngObject;
    FPngOptions: TPngOptions;
    FImageFromAction: Boolean;
    function PngImageStored: Boolean;
    procedure SetPngImage(const Value: TPngObject);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure CreatePngGlyph;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PngImage: TPngObject read FPngImage write SetPngImage stored PngImageStored;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];
    property Glyph stored False;
    property NumGlyphs stored False;
  end;

implementation

{ TPngSpeedButton }

constructor TPngSpeedButton.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FPngImage := TPNGObject.Create;
FPngOptions := [pngBlendOnDisabled];
FImageFromAction := False;
end;

destructor TPngSpeedButton.Destroy;
begin
FPngImage.Free;
inherited Destroy;
end;

procedure TPngSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
inherited ActionChange(Sender, CheckDefaults);
if Sender is TCustomAction
then with TCustomAction(Sender)
     do begin
        //Copy image from action's imagelist
        if (PngImage.Empty or FImageFromAction) and (ActionList <> nil) and (ActionList.Images <> nil) and (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count)
        then begin
             CopyImageFromImageList(FPngImage, ActionList.Images, ImageIndex);
             CreatePngGlyph;
             FImageFromAction := True;
             end;
        end;
end;

procedure TPngSpeedButton.Paint;
var
   PaintRect: TRect;
   GlyphPos, TextPos: TPoint;
begin
inherited Paint;

if FPngImage <> nil
then begin
     //Calculate the position of the PNG glyph
     CalcButtonLayout(Canvas, FPngImage, ClientRect, FState = bsDown, Down, Caption, Layout, Margin, Spacing, GlyphPos, TextPos, DrawTextBiDiModeFlags(0));
     PaintRect := Rect(GlyphPos.X, GlyphPos.Y, GlyphPos.X + FPngImage.Width, GlyphPos.Y + FPngImage.Height);

     if Enabled
     then DrawPNG(FPngImage, Canvas, PaintRect, [])
     else DrawPNG(FPngImage, Canvas, PaintRect, FPngOptions);
     end;
end;

procedure TPngSpeedButton.Loaded;
begin
inherited Loaded;
CreatePngGlyph;
end;

function TPngSpeedButton.PngImageStored: Boolean;
begin
Result := not FImageFromAction;
end;

procedure TPngSpeedButton.SetPngImage(const Value: TPngObject);
begin
//This is all neccesary, because you can't assign a nil to a TPNGObject
if Value = nil
then begin
     FPngImage.Free;
     FPngImage := TPNGObject.Create;
     end
else FPngImage.Assign(Value);

//To work around the gamma-problem
with FPngImage
do if Header.ColorType in [COLOR_RGB, COLOR_RGBALPHA, COLOR_PALETTE]
   then Chunks.RemoveChunk(Chunks.ItemFromClass(TChunkgAMA));

FImageFromAction := False;
CreatePngGlyph;
Repaint;
end;

procedure TPngSpeedButton.SetPngOptions(const Value: TPngOptions);
begin
if FPngOptions <> Value
then begin
     FPngOptions := Value;
     CreatePngGlyph;
     Repaint;
     end;
end;

procedure TPngSpeedButton.CreatePngGlyph;
var
   Bmp: TBitmap;
begin
//Create an empty glyph, just to align the text correctly
Bmp := TBitmap.Create;
try
  Bmp.Width := FPngImage.Width;
  Bmp.Height := FPngImage.Height;
  Bmp.Canvas.Brush.Color := clBtnFace;
  Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
  Glyph.Assign(Bmp);
  NumGlyphs := 1;
finally
  Bmp.Free;
 end;
end;


end.
