unit PngBitBtn;

{$I ..\Include\Thany.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Buttons, Graphics,
  ImgList, ActnList, PngImageList, PngFunctions, PngButtonFunctions, pngimage
  {$IFDEF ThemeSupport}, Themes{$ENDIF};

type
  TPngBitBtn = class(TBitBtn)
  private
    FPngImage: TPngObject;
    FPngOptions: TPngOptions;
    FCanvas: TCanvas;
    FLastKind: TBitBtnKind;
    FImageFromAction: Boolean;
    {$IFDEF ThemeSupport}
    FMouseInControl: Boolean;
    {$ENDIF}
    IsFocused: Boolean;
    function PngImageStored: Boolean;
    procedure SetPngImage(const Value: TPngObject);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
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

{ TPngBitBtn }

constructor TPngBitBtn.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FPngImage := TPNGObject.Create;
FPngOptions := [pngBlendOnDisabled];
FCanvas := TCanvas.Create;
FLastKind := bkCustom;
FImageFromAction := False;
end;

destructor TPngBitBtn.Destroy;
begin
FPngImage.Free;
FCanvas.Free;
inherited Destroy;
end;

procedure TPngBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
inherited ActionChange(Sender, CheckDefaults);
if Sender is TCustomAction
then with TCustomAction(Sender)
     do begin
        //Copy image from action's imagelist
        if (PngImage.Empty or FImageFromAction) and (ActionList <> nil) and (ActionList.Images <> nil) and (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count)
        then begin
             CopyImageFromImageList(FPngImage, ActionList.Images, ImageIndex);
             FImageFromAction := True;
             end;
        end;
end;

procedure TPngBitBtn.SetButtonStyle(ADefault: Boolean);
begin
inherited SetButtonStyle(ADefault);
if ADefault <> IsFocused
then begin
     IsFocused := ADefault;
     Refresh;
     end;
end;

function TPngBitBtn.PngImageStored: Boolean;
begin
Result := not FImageFromAction;
end;

procedure TPngBitBtn.SetPngImage(const Value: TPngObject);
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
do if not Empty and (Header.ColorType in [COLOR_RGB, COLOR_RGBALPHA, COLOR_PALETTE])
   then Chunks.RemoveChunk(Chunks.ItemFromClass(TChunkgAMA));

FImageFromAction := False;
Repaint;
end;

procedure TPngBitBtn.SetPngOptions(const Value: TPngOptions);
begin
if FPngOptions <> Value
then begin
     FPngOptions := Value;
     Repaint;
     end;
end;

procedure TPngBitBtn.CNDrawItem(var Message: TWMDrawItem);
var
   R, PaintRect: TRect;
   GlyphPos, TextPos: TPoint;
   IsDown, IsDefault: Boolean;
   Flags: Cardinal;
   {$IFDEF ThemeSupport}
   Button: TThemedButton;
   Details: TThemedElementDetails;
   {$ENDIF}
begin
R := ClientRect;
FCanvas.Handle := Message.DrawItemStruct^.hDC;
FCanvas.Font := Self.Font;
IsDown := Message.DrawItemStruct^.itemState and ODS_SELECTED <> 0;
IsDefault := Message.DrawItemStruct^.itemState and ODS_FOCUS <> 0;

//Draw the border
if {$IFDEF ThemeSupport}ThemeServices.ThemesEnabled{$ELSE}False{$ENDIF}
then begin
     {$IFDEF ThemeSupport}
     //Themed border
     if not Enabled
     then Button := tbPushButtonDisabled
     else if IsDown
          then Button := tbPushButtonPressed
          else if FMouseInControl
               then Button := tbPushButtonHot
               else if IsFocused or IsDefault
                    then Button := tbPushButtonDefaulted
                    else Button := tbPushButtonNormal;

     //Paint the background, border, and finally get the inner rect
     Details := ThemeServices.GetElementDetails(Button);
     ThemeServices.DrawParentBackground(Handle, Message.DrawItemStruct.hDC, @Details, True);
     ThemeServices.DrawElement(Message.DrawItemStruct.hDC, Details, Message.DrawItemStruct.rcItem);
     R := ThemeServices.ContentRect(FCanvas.Handle, Details, Message.DrawItemStruct.rcItem);
     {$ENDIF}
     end
else begin
     //Draw the outer border, when focused
     if IsFocused or IsDefault
     then begin
          FCanvas.Pen.Color := clWindowFrame;
          FCanvas.Pen.Width := 1;
          FCanvas.Brush.Style := bsClear;
          FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          InflateRect(R, -1, -1);
          end;
     //Draw the inner border
     if IsDown
     then begin
          FCanvas.Pen.Color := clBtnShadow;
          FCanvas.Pen.Width := 1;
          FCanvas.Brush.Color := clBtnFace;
          FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          InflateRect(R, -1, -1);
          end
     else begin
          Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
          if Message.DrawItemStruct.itemState and ODS_DISABLED <> 0
          then Flags := Flags or DFCS_INACTIVE;
          DrawFrameControl(Message.DrawItemStruct.hDC, R, DFC_BUTTON, Flags);
          end;
     //Adjust the rect when focused and/or down
     if IsFocused
     then begin
          R := ClientRect;
          InflateRect(R, -1, -1);
          end;
     if IsDown
     then OffsetRect(R, 1, 1);
     end;

//Calculate the position of the PNG glyph
CalcButtonLayout(FCanvas, FPngImage, ClientRect, IsDown, False, Caption, Layout, Margin, Spacing, GlyphPos, TextPos, DrawTextBiDiModeFlags(0));

//Draw the image
if (FPngImage <> nil) and (Kind = bkCustom) and not FPngImage.Empty
then begin
     PaintRect := Rect(GlyphPos.X, GlyphPos.Y, GlyphPos.X + FPngImage.Width, GlyphPos.Y + FPngImage.Height);
     if Enabled
     then DrawPNG(FPngImage, FCanvas, PaintRect, [])
     else DrawPNG(FPngImage, FCanvas, PaintRect, FPngOptions);
     end;

//Draw the text
if Length(Caption) > 0
then begin
     PaintRect := Rect(TextPos.X, TextPos.Y, Width, Height);
     FCanvas.Brush.Style := bsClear;
     DrawText(FCanvas.Handle, PChar(Caption), -1, PaintRect, DrawTextBiDiModeFlags(0) or DT_TOP or DT_LEFT or DT_SINGLELINE);
     end;

//Draw the focus rectangle
if IsFocused and IsDefault
then begin
     {$IFDEF ThemeSupport}
     if not ThemeServices.ThemesEnabled
     then begin
          R := ClientRect;
          InflateRect(R, -3, -3);
          end;
     {$ELSE}
     R := ClientRect;
     InflateRect(R, -3, -3);
     {$ENDIF}
     FCanvas.Pen.Color := clWindowFrame;
     FCanvas.Brush.Color := clBtnFace;
     DrawFocusRect(FCanvas.Handle, R);
     end;

FLastKind := Kind;
FCanvas.Handle := 0;
end;

procedure TPngBitBtn.CMMouseEnter(var Message: TMessage);
begin
{$IFDEF ThemeSupport}
inherited;
if ThemeServices.ThemesEnabled and not FMouseInControl and not (csDesigning in ComponentState)
then begin
     FMouseInControl := True;
     Repaint;
     end;
{$ENDIF}
end;

procedure TPngBitBtn.CMMouseLeave(var Message: TMessage);
begin
{$IFDEF ThemeSupport}
inherited;
if ThemeServices.ThemesEnabled and FMouseInControl
then begin
     FMouseInControl := False;
     Repaint;
     end;
{$ENDIF}
end;

end.
