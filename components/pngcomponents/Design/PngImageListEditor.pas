unit PngImageListEditor;

{$I ..\Include\Thany.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF THANY_COMPILER_6_UP}Variants,{$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, PngBitBtn,
  ExtCtrls, PngImageList, ExtDlgs, pngimage;

type
  TPngImageListEditorDlg = class(TForm)
    cmbBackgroundColor: TComboBox;
    cmbPreviewBackground: TComboBox;
    dlgColor: TColorDialog;
    dlgOpenPicture: TOpenPictureDialog;
    edtName: TEdit;
    gbxImageInfo: TGroupBox;
    gbxProperties: TGroupBox;
    lblBackgroundColor: TLabel;
    lblColorDepth: TLabel;
    lblColorDepthValue: TLabel;
    lblCompression: TLabel;
    lblCompressionValue: TLabel;
    lblDimensions: TLabel;
    lblDimensionsValue: TLabel;
    lblFiltering: TLabel;
    lblFilteringValue: TLabel;
    lblName: TLabel;
    lblTransparency: TLabel;
    lblTransparencyValue: TLabel;
    lbxImages: TListBox;
    pnlActionButtons: TPanel;
    pnlBackgroundColor: TPanel;
    pnlMain: TPanel;
    btnAdd: TPngBitBtn;
    btnDelete: TPngBitBtn;
    btnReplace: TPngBitBtn;
    btnClear: TPngBitBtn;
    btnUp: TPngBitBtn;
    btnDown: TPngBitBtn;
    Images: TPngImageCollection;
    pnlButtons: TPanel;
    pnlModalButtons: TPanel;
    btnOK: TPngBitBtn;
    btnCancel: TPngBitBtn;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure cmbBackgroundColorChange(Sender: TObject);
    procedure cmbBackgroundColorDblClick(Sender: TObject);
    procedure cmbBackgroundColorExit(Sender: TObject);
    procedure cmbPreviewBackgroundChange(Sender: TObject);
    procedure cmbPreviewBackgroundDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure edtNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxImagesClick(Sender: TObject);
    procedure lbxImagesDblClick(Sender: TObject);
    procedure lbxImagesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbxImagesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbxImagesEnter(Sender: TObject);
    procedure lbxImagesExit(Sender: TObject);
    procedure lbxImagesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbxImagesMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure lbxImagesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbxImagesStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    DraggingIndex, MaxWidth: Integer;
    SelectionBodyColor, SelectionBorderColor: TColor;
    function ConformDimensions(Png: TPNGObject): Boolean;
    function FirstSelected: Integer;
    function LastSelected: Integer;
    procedure DrawBackground(Canvas: TCanvas; const Rect: TRect; ScrollPos, Index: Integer; BlendColor: TColor = clNone; IgnoreScrollPos: Boolean = False);
    procedure GetColorProc(const S: string);
    procedure ParseBackgroundColor(Sender: TObject; CanDisplayError, CanChangeText: Boolean);
    procedure SelectBackgroundColor(Sender: TObject; Color: TColor);
  public
    ImageWidth, ImageHeight: Integer;
  end;

var
  PngImageListEditorDlg: TPngImageListEditorDlg;

implementation

uses Math;

{$R *.dfm}

//For calculating OfficeXP colors
const
  WeightR: single = 0.764706;
  WeightG: single = 1.52941;
  WeightB: single = 0.254902;

const
  SIncorrectSize = 'The selected PNG "%s" must be %dx%d in size, while its actual size is %dx%d';

var
  ResX, ResY: Integer;

{ Globals }

function Blend(C1, C2: TColor; W1: Integer): TColor;
var
   W2, A1, A2, D, F, G: Integer;
begin
if C1 < 0
then C1 := GetSysColor(C1 and $FF);
if C2 < 0
then C2 := GetSysColor(C2 and $FF);

if W1 >= 100
then D := 1000
else D := 100;

W2 := D - W1;
F := D div 2;

A2 := C2 shr 16 * W2;
A1 := C1 shr 16 * W1;
G := (A1 + A2 + F) div D and $FF;
Result := G shl 16;

A2 := (C2 shr 8 and $FF) * W2;
A1 := (C1 shr 8 and $FF) * W1;
G := (A1 + A2 + F) div D and $FF;
Result := Result or G shl 8;

A2 := (C2 and $FF) * W2;
A1 := (C1 and $FF) * W1;
G := (A1 + A2 + F) div D and $FF;
Result := Result or G;
end;

function ColorDistance(C1, C2: Integer): Single;
var
   DR, DG, DB: Integer;
begin
DR := (C1 and $FF) - (C2 and $FF);
Result := Sqr(DR * WeightR);
DG := (C1 shr 8 and $FF) - (C2 shr 8 and $FF);
Result := Result + Sqr(DG * WeightG);
DB := (C1 shr 16) - (C2 shr 16);
Result := Result + Sqr(DB * WeightB);
Result := Sqrt(Result);
end;

function GetAdjustedThreshold(BkgndIntensity, Threshold: Single): Single;
begin
if BkgndIntensity < 220
then Result := (2 - BkgndIntensity / 220) * Threshold
else Result := Threshold;
end;

function IsContrastEnough(AColor, ABkgndColor: Integer; DoAdjustThreshold: Boolean; Threshold: Single): Boolean;
begin
if DoAdjustThreshold
then Threshold := GetAdjustedThreshold(ColorDistance(ABkgndColor, $000000), Threshold);
Result := ColorDistance(ABkgndColor, AColor) > Threshold;
end;

procedure AdjustContrast(var AColor: Integer; ABkgndColor: Integer; Threshold: Single);
var
   X, Y, Z: Single;
   R, G, B: Single;
   RR, GG, BB: Integer;
   I1, I2, S, Q, W: Single;
   DoInvert: Boolean;
begin
I1 := ColorDistance(AColor, $000000);
I2 := ColorDistance(ABkgndColor, $000000);
Threshold := GetAdjustedThreshold(I2, Threshold);

if I1 > I2
then DoInvert := I2 < 442 - Threshold
else DoInvert := I2 < Threshold;

X := (ABkgndColor and $FF) * WeightR;
Y := (ABkgndColor shr 8 and $FF) * WeightG;
Z := (ABkgndColor shr 16) * WeightB;

R := (AColor and $FF) * WeightR;
G := (AColor shr 8 and $FF) * WeightG;
B := (AColor shr  16) * WeightB;

if DoInvert
then begin
     R := 195 - R;
     G := 390 - G;
     B := 65 - B;
     X := 195 - X;
     Y := 390 - Y;
     Z := 65 - Z;
     end;

S := Sqrt(Sqr(B) + Sqr(G) + Sqr(R));
if S < 0.01
then S := 0.01;

Q := (R * X + G * Y + B * Z) / S;

X := Q / S * R - X;
Y := Q / S * G - Y;
Z := Q / S * B - Z;

W :=  Sqrt(Sqr(Threshold) - Sqr(X) - Sqr(Y) - Sqr(Z));

R := (Q - W) * R / S;
G := (Q - W) * G / S;
B := (Q - W) * B / S;

if DoInvert
then begin
     R := 195 - R;
     G := 390 - G;
     B := 65 - B;
end;

if R < 0 then R := 0 else if R > 195 then R := 195;
if G < 0 then G := 0 else if G > 390 then G := 390;
if B < 0 then B := 0 else if B >  65 then B :=  65;

RR := Trunc(R * (1 / WeightR) + 0.5);
GG := Trunc(G * (1 / WeightG) + 0.5);
BB := Trunc(B * (1 / WeightB) + 0.5);

if RR > $FF then RR := $FF else if RR < 0 then RR := 0;
if GG > $FF then GG := $FF else if GG < 0 then GG := 0;
if BB > $FF then BB := $FF else if BB < 0 then BB := 0;

AColor := (BB and $FF) shl 16 or (GG and $FF) shl 8 or (RR and $FF);
end;

procedure SetContrast(var Color: TColor; BkgndColor: TColor; Threshold: Integer);
var
   T: Single;
begin
if Color < 0
then Color := GetSysColor(Color and $FF);
if BkgndColor < 0
then BkgndColor := GetSysColor(BkgndColor and $FF);
T := Threshold;
if not IsContrastEnough(Color, BkgndColor, True, T)
then AdjustContrast(Integer(Color), BkgndColor, T);
end;

function ResizeProportionalX(InitialValue: Integer): Integer;
begin
Result := InitialValue * ResX div 96;
end;

function ResizeProportionalY(InitialValue: Integer): Integer;
begin
Result := InitialValue * ResY div 96;
end;

procedure InitResolution;
var
   DC: HDC;
begin
DC := GetDC(0);
ResX := GetDeviceCaps(DC, LOGPIXELSX);
ResY := GetDeviceCaps(DC, LOGPIXELSY);
ReleaseDC(0, DC);
end;

{ TPngImageListEditorDlg }

function TPngImageListEditorDlg.ConformDimensions(Png: TPNGObject): Boolean;
begin
//Returns whether an image conforms the specified dimensions, if available
Result := ((ImageHeight = 0) and (ImageWidth = 0)) or ((ImageHeight = Png.Height) and (ImageWidth = Png.Width));
end;

function TPngImageListEditorDlg.FirstSelected: Integer;
begin
//Return the first selected image
Result := 0;
while not lbxImages.Selected[Result] and (Result < lbxImages.Items.Count)
do Inc(Result);
end;

function TPngImageListEditorDlg.LastSelected: Integer;
begin
//Return the last selected image
Result := lbxImages.Items.Count - 1;
while not lbxImages.Selected[Result] and (Result >= 0)
do Dec(Result);
end;

procedure TPngImageListEditorDlg.DrawBackground(Canvas: TCanvas; const Rect: TRect; ScrollPos, Index: Integer; BlendColor: TColor = clNone; IgnoreScrollPos: Boolean = False);
var
   I, X, Y: Integer;
   PatBitmap, BkBitmap: TBitmap;
   Even: Boolean;
begin
//Draw the background of the listbox, if any
if Index = 0
then begin
     //No background, then skip the hard part
     if BlendColor = clNone
     then Canvas.Brush.Color := clWindow
     else Canvas.Brush.Color := BlendColor;
     Canvas.FillRect(Rect);
     Exit;
     end;

//Draw the background
BkBitmap := TBitmap.Create;
PatBitmap := TBitmap.Create;
try
  PatBitmap.Height := 16;
  PatBitmap.Width := 16;
  with PatBitmap.Canvas
  do begin
     //First, draw the background for the pattern bitmap
     if BlendColor = clNone
     then begin
          Brush.Color := clWindow;
          FillRect(Classes.Rect(0, 0, PatBitmap.Height, PatBitmap.Width));
          Brush.Color := Blend(clWindow, clBtnFace, 50);
          end
     else begin
          Brush.Color := Blend(clWindow, BlendColor, 50);
          FillRect(Classes.Rect(0, 0, PatBitmap.Height, PatBitmap.Width));
          Brush.Color := BlendColor;
          end;

     //Then, draw the foreground on the pattern bitmap
     Pen.Color := Brush.Color;
     case Index of
          1: begin
             //Checkerboard background
             FillRect(Classes.Rect(PatBitmap.Width div 2, 0, PatBitmap.Width, PatBitmap.Height div 2));
             FillRect(Classes.Rect(0, PatBitmap.Height div 2, PatBitmap.Width div 2, PatBitmap.Height));
             end;
          2: begin
             //Diamonds background
             PatBitmap.Width := 10;
             PatBitmap.Height := 10;
             Polygon([Point(PatBitmap.Width div 2, 0), Point(PatBitmap.Width, PatBitmap.Height div 2), Point(PatBitmap.Width div 2, PatBitmap.Height), Point(0, PatBitmap.Height div 2)]);
             end;
          3: begin
             //Slashed background
             Even := True;
             I := 2;
             while I < PatBitmap.Width + PatBitmap.Height
             do begin
                if I < PatBitmap.Width
                then begin
                     MoveTo(I, 0);
                     LineTo(-1, I + 1);
                     end
                else begin
                     MoveTo(PatBitmap.Width, I - PatBitmap.Width);
                     LineTo(I - PatBitmap.Width, PatBitmap.Height);
                     end;
                if Even
                then Inc(I, 1)
                else Inc(I, 3);
                Even := not Even;
                end;
             end;
          4: begin
             //Backslashed background
             Even := True;
             I := 2;
             while I < PatBitmap.Width + PatBitmap.Height
             do begin
                if I < PatBitmap.Width
                then begin
                     MoveTo(I, 0);
                     LineTo(PatBitmap.Width, PatBitmap.Height - I);
                     end
                else begin
                     MoveTo(0, I - PatBitmap.Width - 1);
                     LineTo(PatBitmap.Width - (I - PatBitmap.Width) + 1, PatBitmap.Height);
                     end;
                if Even
                then Inc(I, 1)
                else Inc(I, 3);
                Even := not Even;
                end;
             end;
          end;
     end;

  //The actual background bitmap, its width and height are increased to compensate
  //for scrolling distance
  BkBitmap.Width := Rect.Left mod PatBitmap.Width + Rect.Right - Rect.Left;
  if IgnoreScrollPos
  then ScrollPos := 0
  else ScrollPos := (Rect.Top + ScrollPos) mod PatBitmap.Height;
  BkBitmap.Height := ScrollPos + Rect.Bottom - Rect.Top;

  //Now repeat the pattern bitmap onto the background bitmap
  with BkBitmap.Canvas
  do begin
     Y := 0;
     while Y < BkBitmap.Height
     do begin
        X := 0;
        while X < BkBitmap.Width
        do begin
           Draw(X, Y, PatBitmap);
           Inc(X, PatBitmap.Width);
           end;
        Inc(Y, PatBitmap.Height);
        end;
     end;

  //And finally, draw the background bitmap to the canvas
  BitBlt(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, BkBitmap.Canvas.Handle, Rect.Left mod PatBitmap.Width, ScrollPos, SRCCOPY);
finally
  BkBitmap.Free;
  PatBitmap.Free;
 end;
end;

//Method for getting color values
procedure TPngImageListEditorDlg.GetColorProc(const S: string);
begin
cmbBackgroundColor.Items.Add(S);
end;

//Parse a background color name or code
procedure TPngImageListEditorDlg.ParseBackgroundColor(Sender: TObject; CanDisplayError, CanChangeText: Boolean);
var
   S: string;
   I, ParsedColor: Integer;
begin
with cmbBackgroundColor
do begin
   //First, see if its a known color name
   if IdentToColor(Text, ParsedColor)
   then begin
        ItemIndex := Items.IndexOf(Text);
        pnlBackgroundColor.Color := ParsedColor;
        end
   else begin
        S := Text;
        //Replace # with $ so StringToColor recognizes it
        if (Length(S) > 0) and (S[1] = '#')
        then S[1] := '$';
        try
          //Try to convert to a real color value
          ParsedColor := StringToColor(S);
          if CanChangeText
          then begin
               //And try to convert back to an identifier (i.e. if you type in $000000, it'll become clBlack)
               if ColorToIdent(ParsedColor, S)
               then ItemIndex := Items.IndexOf(S)
               else Text := S;
               end;
          pnlBackgroundColor.Color := ParsedColor;
        except
          //If it fails, display a message if neccesary
          on EConvertError do
          if CanDisplayError
          then begin
               MessageBox(Self.Handle, PChar(Format('"%s" is not a valid color value', [Text])), PChar(Self.Caption), MB_ICONERROR or MB_OK);
               SetFocus;
               end;
         end;
        end;
   end;
//And finally, set the background color to every selected image
if (Sender <> lbxImages)
then for I := 0 to lbxImages.Items.Count - 1
     do if lbxImages.Selected[I]
        then Images.Items[I].Background := pnlBackgroundColor.Color;
end;


procedure TPngImageListEditorDlg.SelectBackgroundColor(Sender: TObject; Color: TColor);
var
   S: string;
begin
//This happens after a background color has been slected from the color dialog
//Try to convert a color into an identifier, or else into a hexadecimal representation
if ColorToIdent(Color, S)
then cmbBackgroundColor.ItemIndex := cmbBackgroundColor.Items.IndexOf(S)
else cmbBackgroundColor.Text := '$' + IntToHex(dlgColor.Color, 6);
ParseBackgroundColor(Sender, False, True);
end;

procedure TPngImageListEditorDlg.btnAddClick(Sender: TObject);
var
   Png: TPngImageCollectionItem;
   I, Selected, FirstSelected: Integer;
begin
//The Add button is pressed, let the programmer look for an image
dlgOpenPicture.Options := dlgOpenPicture.Options + [ofAllowMultiSelect];
if dlgOpenPicture.Execute
then begin
     for I := 0 to lbxImages.Items.Count - 1
     do lbxImages.Selected[I] := False;
     FirstSelected := -1;
     for I := 0 to dlgOpenPicture.Files.Count - 1
     do begin
        Png := Images.Items.Add;
        with Png.PngImage
        do begin
           //Load the image, but remove any gamma, so that the gamma won't be reapplied
           //when loading the image from the DFM
           LoadFromFile(dlgOpenPicture.Files[I]);
           if Png.PngImage.Header.ColorType in [COLOR_RGB, COLOR_RGBALPHA, COLOR_PALETTE]
           then Chunks.RemoveChunk(Chunks.ItemFromClass(TChunkgAMA));
           end;
        //Does the image conform the specified dimensions, if any?
        if ConformDimensions(Png.PngImage)
        then begin
             //Update maximum image width
             if MaxWidth < Png.PngImage.Width
             then MaxWidth := Png.PngImage.Width;

             //Invent a name for the image, and initialize its background color
             Png.Name := 'PngImage' + IntToStr(Images.Items.Count - 1);
             Png.Background := clWindow;

             //Finally, add it and select it
             Selected := lbxImages.Items.AddObject(Png.Name, Png);
             lbxImages.Selected[Selected] := True;
             if FirstSelected = -1
             then FirstSelected := Selected;
             end
        else begin
             //The image does not conform the specified dimensions
             MessageBox(Handle, PChar(Format(SIncorrectSize, [ExtractFilename(dlgOpenPicture.Files[I]), ImageWidth, ImageHeight, Png.PngImage.Width, Png.PngImage.Height])), PChar(Caption), MB_ICONERROR or MB_OK);
             Images.Items.Delete(Png.Index);
             end;
        end;

     //Focus the first selected (added) image
     lbxImages.ItemIndex := FirstSelected;
     lbxImages.SetFocus;
     lbxImagesClick(nil);
     end;
end;

procedure TPngImageListEditorDlg.btnClearClick(Sender: TObject);
begin
//Clear the listbox and the collection
if (lbxImages.Items.Count > 0) and (MessageBox(Handle, 'This will clear the entire image list. Are you sure you want to do this?', PChar(Self.Caption), MB_ICONEXCLAMATION or MB_YESNO or MB_DEFBUTTON2) = IDYES)
then begin
     lbxImages.Items.Clear;
     Images.Items.Clear;
     lbxImagesClick(nil);
     end;
end;

procedure TPngImageListEditorDlg.btnDeleteClick(Sender: TObject);

  function GetCommaList: string;
  var
     I: Integer;
     S: TStringList;
  begin
  //Get a comma list of the names of the selected images in the form "name1,
  //name2 and name3"
  Result := '';
  S := TStringList.Create;
  try
    for I := 0 to lbxImages.Items.Count - 1
    do if lbxImages.Selected[I]
       then S.Add(Images.Items[I].Name);
    for I := 0 to S.Count - 1
    do begin
       Result := Result + S[I];
       if I < S.Count - 2
       then Result := Result + ', '
       else if I < S.Count - 1
            then Result := Result + ' and ';
       end;
  finally
    S.Free;
   end;
  end;

var
   I, NewIndex: Integer;
begin
with lbxImages
do if (SelCount > 0) and (MessageBox(Handle, PChar(Format('Are you sure you want to delete %s?', [GetCommaList])), PChar(Self.Caption), MB_ICONEXCLAMATION or MB_YESNO) = IDYES)
   then begin
        //Delete every selected image from the listbox and from the collection
        NewIndex := -1;
        I := 0;
        while I < Items.Count
        do if Selected[I]
           then begin
                if NewIndex = -1
                then NewIndex := I;
                lbxImages.Items.Delete(I);
                Images.Items.Delete(I);
                end
           else Inc(I);

        //Figure out the new selection index
        if NewIndex > Items.Count - 1
        then NewIndex := Items.Count - 1
        else if (NewIndex = -1) and (Items.Count > 0)
             then NewIndex := 0;
        Selected[NewIndex] := True;
        ItemIndex := NewIndex;
        lbxImagesClick(nil);
        end;
end;

procedure TPngImageListEditorDlg.btnDownClick(Sender: TObject);
var
   I: Integer;
begin
//Move the selected items one position down
with lbxImages
do if (SelCount > 0) and (LastSelected < Items.Count - 1)
   then for I := Items.Count - 1 downto 0
        do if Selected[I]
           then begin
                Images.Items[I].Index := I + 1;
                Items.Exchange(I, I + 1);
                Selected[I + 1] := True;
                end;
lbxImagesClick(nil);
end;

procedure TPngImageListEditorDlg.btnReplaceClick(Sender: TObject);
var
   Item: TPngImageCollectionItem;
   Index: Integer;
   Png: TPNGObject;
begin
//The Replace button is pressed, let the programmer look for an image
dlgOpenPicture.Options := dlgOpenPicture.Options - [ofAllowMultiSelect];
with lbxImages
do if (SelCount = 1) and dlgOpenPicture.Execute
   then begin
        Index := FirstSelected;
        Png := TPNGObject.Create;
        try
          //First see if the image conforms the specified dimensions
          Png.LoadFromFile(dlgOpenPicture.Filename);
          if ConformDimensions(Png)
          then begin
               //Then remove any gamma, so that the gamma won't be reapplied when loading the
               //image from the DFM
               if Png.Header.ColorType in [COLOR_RGB, COLOR_RGBALPHA]
               then Png.Chunks.RemoveChunk(Png.Chunks.ItemFromClass(TChunkgAMA));
               Item := Images.Items[Index];
               Item.PngImage := Png;

               //Update the maximum image width
               if MaxWidth < Item.PngImage.Width
               then MaxWidth := Item.PngImage.Width;

               //Repaint and update everything, to be sure
               lbxImages.Repaint;
               lbxImagesClick(nil);
               end
          else MessageBox(Handle, PChar(Format(SIncorrectSize, [ExtractFilename(dlgOpenPicture.Filename), ImageWidth, ImageHeight, Png.Width, Png.Height])), PChar(Caption), MB_ICONERROR or MB_OK);
        finally
          Png.Free;
         end;
        end;
end;

procedure TPngImageListEditorDlg.btnUpClick(Sender: TObject);
var
   I: Integer;
begin
//Move the selected items one position up
with lbxImages
do if (SelCount > 0) and (FirstSelected > 0)
   then for I := 0 to Items.Count - 1
        do if Selected[I]
           then begin
                Images.Items[I].Index := I - 1;
                Items.Exchange(I, I - 1);
                Selected[I - 1] := True;
                end;
lbxImagesClick(nil);
end;

procedure TPngImageListEditorDlg.cmbBackgroundColorChange(Sender: TObject);
begin
//While typing, try parsing the background color, but without any error messages
ParseBackgroundColor(Sender, False, False);
end;

procedure TPngImageListEditorDlg.cmbBackgroundColorDblClick(Sender: TObject);
begin
//Just like in Delphi, when doubleclicking a color, the color dialog pops up
dlgColor.Color := pnlBackgroundColor.Color;
if dlgColor.Execute
then SelectBackgroundColor(Sender, dlgColor.Color);
end;

procedure TPngImageListEditorDlg.cmbBackgroundColorExit(Sender: TObject);
begin
//When leaving the background combobox, parse the color, but this with error
//message, if neccesary
ParseBackgroundColor(Sender, True, True);
end;

procedure TPngImageListEditorDlg.cmbPreviewBackgroundChange(Sender: TObject);
begin
//Pewview background is changes, repaint all items
lbxImages.Repaint;
end;

procedure TPngImageListEditorDlg.cmbPreviewBackgroundDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
   IconWidth: Integer;
begin
with cmbPreviewBackground
do begin
   //Draw the background "icon" of the preview background combobox
   IconWidth := (Rect.Bottom - Rect.Top) * 4 div 3;
   DrawBackground(Canvas, Classes.Rect(Rect.Left, Rect.Top, Rect.Left + IconWidth, Rect.Bottom), 0, Index, clNone, True);
   Inc(Rect.Left, IconWidth);

   //Background color of the rest of the item
   if odSelected in State
   then Canvas.Brush.Color := clHighlight
   else Canvas.Brush.Color := clWindow;
   Canvas.FillRect(Rect);
   Inc(Rect.Left, 4);

   //And the text
   DrawText(Canvas.Handle, PChar(Items[Index]), -1, Rect, DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER);

   Canvas.Brush.Color := clWindow;
   end;
end;

procedure TPngImageListEditorDlg.edtNameChange(Sender: TObject);
begin
//Update the selected image with the entered name, in realtime
with lbxImages
do if ItemIndex >= 0
   then begin
        Images.Items[ItemIndex].Name := edtName.Text;
        Items[ItemIndex] := edtName.Text;
        end;
end;

procedure TPngImageListEditorDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TPngImageListEditorDlg.FormCreate(Sender: TObject);
var
   Space8H: Integer;
begin
//Initialize OfficeXP colors for selection
SelectionBodyColor := Blend(clHighlight, clWindow, 30);
SetContrast(SelectionBodyColor, Blend(clWindow, clBtnFace, 165), 50);
SelectionBorderColor := clHighlight;

//Initialize a value that keeps track of dragging
DraggingIndex := -1;

//Get all available color names
GetColorValues(GetColorProc);

//Initialize the background to clWindow
cmbBackgroundColor.ItemIndex := cmbBackgroundColor.Items.IndexOf('clWindow');
cmbBackgroundColorChange(nil);

//Do not specify image width and height by default (the imagelist will fill
//these up, so that you cannot select an image other than these dimensions)
ImageWidth := 0;
ImageHeight := 0;

//Resize everything to make it fit on "large fonts" setting. Note that these
//operations are also needed on normal setting.
Space8H := lbxImages.Top;
Width := ResizeProportionalX(Width);
Height := ResizeProportionalY(Height);
Constraints.MinHeight := gbxProperties.Top + cmbBackgroundColor.Top + cmbBackgroundColor.Height + Space8H + Space8H + gbxImageInfo.Height + Space8H + (Height - pnlMain.Height);
Constraints.MinWidth := Width;
pnlButtons.Align := alBottom;
pnlMain.Align := alClient;
cmbPreviewBackground.ItemHeight := ResizeProportionalY(cmbPreviewBackground.ItemHeight);
pnlBackgroundColor.Height := cmbBackgroundColor.Height;

//Make sure the background color isn't reset when themes are enabled
{$IFDEF ThemeSupport}
pnlBackgroundColor.ParentBackground := True;
pnlBackgroundColor.ParentBackground := False;
{$ENDIF}
end;

procedure TPngImageListEditorDlg.FormResize(Sender: TObject);
begin
//There appears to be a bug that prevents a listbox from being redrawn correctly
//when the form is resized
lbxImages.Repaint;
end;

procedure TPngImageListEditorDlg.FormShow(Sender: TObject);
var
   I: Integer;
begin
//Initialize the maximum width of the images, to align text in the listbox
MaxWidth := 0;
for I := 0 to Images.Items.Count - 1
do if Images.Items[I].PngImage.Width > MaxWidth
   then MaxWidth := Images.Items[I].PngImage.Width;

//Fill the listbox with the images
for I := 0 to Images.Items.Count - 1
do lbxImages.Items.AddObject(Images.Items[I].Name, Images.Items[I]);
if lbxImages.Items.Count > 0
then begin
     lbxImages.Selected[0] := True;
     lbxImages.ItemIndex := 0;
     end;
lbxImages.SetFocus;
lbxImagesClick(nil);

cmbPreviewBackground.ItemIndex := 0;
FormResize(nil);
end;

procedure TPngImageListEditorDlg.lbxImagesClick(Sender: TObject);

  function GetDimensions(Png: TPNGObject): string;
  begin
  //Return the formatted dimensions of an image
  Result := Format('%dx%d', [Png.Width, Png.Height]);
  if Png.InterlaceMethod <> imNone
  then Result := Result + ' interlace';
  end;

  function GetColorDepth(Png: TPNGObject): string;
  begin
  //Return the color depth, including whether the image is grayscale or paletted
  case Png.Header.ColorType of
       COLOR_GRAYSCALE, COLOR_GRAYSCALEALPHA:
          Result := IntToStr(Png.Header.BitDepth) + '-bits grayscale';
       COLOR_RGB, COLOR_RGBALPHA:
          Result := IntToStr(Png.Header.BitDepth * 3) + '-bits';
       COLOR_PALETTE:
          Result := IntToStr(Png.Header.BitDepth) + '-bits paletted';
       end;
  end;

  function GetTransparency(Png: TPNGObject): string;
  begin
  //Return the formatted transparency depth, or transparency mode
  if Png.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA]
  then Result := IntToStr(Png.Header.BitDepth) + '-bits alpha'
  else case Png.TransparencyMode of
            ptmBit:     Result := 'bitmask';
            ptmPartial: Result := 'indexed';
            else        Result := 'none';
            end;
  end;

  function GetCompression(Png: TPNGObject): string;
  begin
  //Return the formatted compression level
  Result := Format('level %d', [Png.CompressionLevel]);
  end;

  function GetFiltering(Png: TPNGObject): string;
  begin
  //Return the formatted filtering method
  case Png.Header.FilterMethod of
       FILTER_SUB:     Result := 'sub';
       FILTER_UP:      Result := 'up';
       FILTER_AVERAGE: Result := 'average';
       FILTER_PAETH:   Result := 'paeth';
       else            Result := 'none';
       end;
  end;

  function SameBackgroundColor: Boolean;
  var
     FirstBgColor: TColor;
     I: Integer;
     First: Boolean;
  begin
  //Determine whether the background color of all selected images is the same
  FirstBgColor := clNone;
  First := True;
  Result := True;
  for I := 0 to lbxImages.Items.Count - 1
  do if lbxImages.Selected[I]
     then if First
          then begin
               //Found the first selected and its background color
               FirstBgColor := Images.Items[I].Background;
               First := False;
               end
          else begin
               //If not equal to first background color, then break, continue otherwise
               Result := FirstBgColor = Images.Items[I].Background;
               if not Result
               then Break;
               end;
  end;

const
   NoneSelected = '[ none ]';
   MultipleSelected = '[ multiple ]';
begin
with lbxImages
do begin
   //Refresh the enabled state of the buttons and controls
   btnReplace.Enabled := SelCount = 1;
   btnDelete.Enabled := SelCount > 0;
   btnClear.Enabled := Items.Count > 0;
   btnUp.Enabled := (SelCount > 0) and (FirstSelected > 0);
   btnDown.Enabled := (SelCount > 0) and (LastSelected < Items.Count - 1);
   lblName.Enabled := SelCount = 1;
   edtName.Enabled := SelCount = 1;
   lblBackgroundColor.Enabled := SelCount > 0;
   cmbBackgroundColor.Enabled := SelCount > 0;
   case SelCount of
        0:   begin
             //None is selected, so no information to display
             lblDimensionsValue.Caption := NoneSelected;
             lblColorDepthValue.Caption := NoneSelected;
             lblTransparencyValue.Caption := NoneSelected;
             lblCompressionValue.Caption := NoneSelected;
             lblFilteringValue.Caption := NoneSelected;
             end;
        1:   with Images.Items[FirstSelected]
             do begin
                edtName.OnChange := nil;
                try
                  //One item is selected, display its properties and information
                  edtName.Text := Name;
                  SelectBackgroundColor(Sender, Background);
                  lblDimensionsValue.Caption := GetDimensions(PngImage);
                  lblColorDepthValue.Caption := GetColorDepth(PngImage);
                  lblTransparencyValue.Caption := GetTransparency(PngImage);
                  lblCompressionValue.Caption := GetCompression(PngImage);
                  lblFilteringValue.Caption := GetFiltering(PngImage);
                finally
                  edtName.OnChange := edtNameChange;
                 end;
                end;
        else begin
             //More than 1 is selected, so no image information can be displayed
             if SameBackgroundColor
             then SelectBackgroundColor(Sender, Images.Items[FirstSelected].Background)
             else SelectBackgroundColor(Sender, clNone);
             lblDimensionsValue.Caption := MultipleSelected;
             lblColorDepthValue.Caption := MultipleSelected;
             lblTransparencyValue.Caption := MultipleSelected;
             lblCompressionValue.Caption := MultipleSelected;
             lblFilteringValue.Caption := MultipleSelected;
             end;
        end;
   end;
end;

procedure TPngImageListEditorDlg.lbxImagesDblClick(Sender: TObject);
begin
//Doubleclicking is the same as the Replace button
if lbxImages.SelCount = 1
then btnReplaceClick(nil);
end;

procedure TPngImageListEditorDlg.lbxImagesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

  procedure MoveItem(Index, Delta: Integer);
  begin
  //Move a single item up or down, depending on Delta
  if lbxImages.Selected[Index]
  then begin
       Images.Items[Index].Index := Index + Delta;
       lbxImages.Items.Exchange(Index, Index + Delta);
       lbxImages.Selected[Index + Delta] := True;
       end;
  end;

  function InRange(Index: Integer): Boolean;
  begin
  //Return whether Index exists in the listbox
  Result := (Index >= 0) and (Index < lbxImages.Items.Count);
  end;

var
   NewIndex, NewItemIndex, Delta, I: Integer;
begin
Accept := DraggingIndex >= 0;
if Accept
then begin
     //Figure out to which index is dragged
     NewIndex := lbxImages.ItemAtPos(Point(X, Y), False);
     if NewIndex > lbxImages.Items.Count - 1
     then NewIndex := lbxImages.Items.Count - 1;

     //Figure out the distance (delta) of the drag
     Delta := NewIndex - DraggingIndex;

     //The destination index has to exist and has to be differend from where we
     //started the drag. On to pof that, the drag destination of the first and
     //last selected items have to be in range.
     if (NewIndex >= 0) and (NewIndex <> DraggingIndex) and InRange(FirstSelected + Delta) and InRange(LastSelected + Delta)
     then begin
          //Calc the new focus index
          NewItemIndex := lbxImages.ItemIndex + Delta;

          //To prevent things to get messed up, moving downwards needs walking through the
          //images in opposite direction
          if Delta < 0
          then for I := 0 to lbxImages.Items.Count - 1
               do MoveItem(I, Delta)
          else for I := lbxImages.Items.Count - 1 downto 0
               do MoveItem(I, Delta);

          //Set the new focus index and tracking value of the drag
          lbxImages.ItemIndex := NewItemIndex;
          DraggingIndex := NewIndex;

          lbxImagesClick(nil);
          end;
     end;
end;

procedure TPngImageListEditorDlg.lbxImagesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
   DrawRect: TRect;
   ScrollInfo: TScrollInfo;
   I, ScrollPos: Integer;
begin
//Get the scrolling distance
ScrollPos := 0;
ScrollInfo.cbSize := SizeOf(ScrollInfo);
ScrollInfo.fMask := SIF_POS;
if GetScrollInfo(lbxImages.Handle, SB_VERT, ScrollInfo)
then for I := 0 to ScrollInfo.nPos - 1
     do with lbxImages.ItemRect(I)
        do Inc(ScrollPos, Bottom - Top);

//First, draw the background
if odSelected in State
then if lbxImages.Focused
     then DrawBackground(lbxImages.Canvas, Rect, ScrollPos, cmbPreviewBackground.ItemIndex, SelectionBodyColor)
     else DrawBackground(lbxImages.Canvas, Rect, ScrollPos, cmbPreviewBackground.ItemIndex, Blend(SelectionBodyColor, clWindow, 50))
else DrawBackground(lbxImages.Canvas, Rect, ScrollPos, cmbPreviewBackground.ItemIndex);
with lbxImages.Canvas
do begin
   //Then, draw a focus border, if focused
   Brush.Style := bsClear;
   if odFocused in State
   then begin
        if lbxImages.Focused
        then Pen.Color := SelectionBorderColor
        else Pen.Color := Blend(SelectionBorderColor, clWindow, 50);
        Pen.Style := psSolid;
        Rectangle(Rect);
        end;

   //Draw the image at the center of (Rect.Left, Rect.Top, Rect.Left + MaxWidth,
   //Rect.Bottom)
   with Images.Items[Index]
   do begin
      DrawRect.Left := Rect.Left + (MaxWidth - PngImage.Width) div 2 + 2;
      DrawRect.Top := Rect.Top + (Rect.Bottom - Rect.Top - PngImage.Height) div 2;
      DrawRect.Right := DrawRect.Left + PngImage.Width;
      DrawRect.Bottom := DrawRect.Top + PngImage.Height;
      PngImage.Draw(lbxImages.Canvas, DrawRect);
      end;

   //Draw the image index number and the name
   Font.Color := clWindowText;
   DrawRect := Classes.Rect(Rect.Left + MaxWidth + 8, Rect.Top, Rect.Left + MaxWidth + Canvas.TextWidth(IntToStr(lbxImages.Items.Count - 1)) + 8, Rect.Bottom);
   DrawText(Handle, PChar(IntToStr(Index)), -1, DrawRect, DT_RIGHT or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER);
   DrawRect.Left := DrawRect.Right;
   DrawRect.Right := Rect.Right;
   DrawText(Handle, PChar(' - ' + Images.Items[Index].Name), -1, DrawRect, DT_END_ELLIPSIS or DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER);

   //Draw the normal focusrect, so that it'll become invisible
   if (odFocused in State) and lbxImages.Focused
   then DrawFocusRect(Rect);
   end;
end;

procedure TPngImageListEditorDlg.lbxImagesEnter(Sender: TObject);
begin
//Just to be sure
lbxImages.Repaint;
end;

procedure TPngImageListEditorDlg.lbxImagesExit(Sender: TObject);
begin
//Just to be sure
lbxImages.Repaint;
end;

procedure TPngImageListEditorDlg.lbxImagesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//I would expect this "ctrl"-navigation would work standardly, but appearantly
//it doesn't, so we'll have to code it ourselves
with lbxImages
do if ssCtrl in Shift
   then begin
        case Key of
             VK_DOWN:  begin
                       if ItemIndex < Items.Count - 1
                       then ItemIndex := ItemIndex + 1;
                       Key := 0;
                       end;
             VK_UP:    begin
                       if ItemIndex > 0
                       then ItemIndex := ItemIndex - 1;
                       Key := 0;
                       end;
             VK_SPACE: begin
                       Selected[ItemIndex] := not Selected[ItemIndex];
                       lbxImagesClick(nil);
                       Key := 0;
                       end;
             end;
        end;
end;

procedure TPngImageListEditorDlg.lbxImagesMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
var
   Temp: Integer;
begin
//Figure out the height of an item, when editing an image collection, the height
//of an image may differ
Height := Images.Items[Index].PngImage.Height + 4;
Temp := lbxImages.Canvas.TextHeight('0') + 4;
if Temp > Height
then Height := Temp;
end;

procedure TPngImageListEditorDlg.lbxImagesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//If the mouse button is released, the tracking value of the drag needs to be
//reset as well
DraggingIndex := -1;
end;

procedure TPngImageListEditorDlg.lbxImagesStartDrag(Sender: TObject; var DragObject: TDragObject);
var
   Pos: TPoint;
begin
//Figure out where this drag start is
GetCursorPos(Pos);
DraggingIndex := lbxImages.ItemAtPos(lbxImages.ScreenToClient(Pos), True);
if DraggingIndex >= 0
then lbxImages.ItemIndex := DraggingIndex;
end;

initialization
  InitResolution;

end.
