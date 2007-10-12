unit QREDBImage;
{
 TQREDBImage 1.0 (Enhaced TQRDBImage):
  by Sebastián Mayorá - Argentina - DelphiHelper@yahoo.com.ar
 Please read QREDBImage.txt or readme.txt for more information
}
interface

uses
  Windows,  Classes,{Tstream } Graphics {TGraphic}, Db{field}, Controls{csopaque,csframe},forms {screen},
  QuickRpt;

type

  TLoadCustomImageEvent = procedure (var B: TGraphic; Stream: TStream)of object;

  TQREDBImage = class(TQRPrintable)
  private
    FField : TField;
    FDataSet : TDataSet;
    FDataField : string;
    FPicture: TPicture;
    FStretch: boolean;
    FCenter: boolean;
    FPictureLoaded: boolean;
    fOnLoadCustomImage: TLoadCustomImageEvent;
    procedure PictureChanged(Sender: TObject);
    procedure SetCenter(Value: Boolean);
    procedure SetDataField(const Value: string);
    procedure SetDataSet(Value: TDataSet);
    procedure SetPicture(Value: TPicture);
    procedure SetStretch(Value: Boolean);
  protected
    Memoria: TMemoryStream;
    function GetPalette: HPALETTE; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Prepare; override;
    procedure Print(OfsX, OfsY : integer); override;
    procedure UnPrepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadPicture;//Si hubiera sido virtual en la clase ancestro...
    property Field: TField read FField;
    property Picture: TPicture read FPicture write SetPicture;
  published
    property Center: boolean read FCenter write SetCenter default True;
    property DataField: string read FDataField write SetDataField;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Stretch: boolean read FStretch write SetStretch default False;
    property OnLoadCustomImage :TLoadCustomImageEvent read fOnLoadCustomImage write fOnLoadCustomImage;
  end;

implementation
uses  JPeg;

constructor TQREDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  Width := 105;
  Height := 105;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FCenter := True;
  Memoria := TMemoryStream.Create;
end;

destructor TQREDBImage.Destroy;
begin
  FPicture.Free;
  Memoria.Free;
  inherited Destroy;
end;

procedure TQREDBImage.Prepare;
begin
  inherited Prepare;
  if assigned(FDataSet) then
    begin
    FField := DataSet.FindField(FDataField);
    if Field is TBlobField then
      Caption := '';
    LoadPicture;
    end
  else
    FField := nil;
end;

procedure TQREDBImage.Print(OfsX, OfsY : integer);
var
  H: integer;
  Dest: TRect;
  DrawPict: TPicture;
begin
  with QRPrinter.Canvas do
    begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    DrawPict := TPicture.Create;
    H := 0;
    try
      if assigned(FField) and (FField is TBlobField) then
      begin
        if not fPictureLoaded then
          LoadPicture;
        DrawPict.Assign(Picture);
        if (DrawPict.Graphic <> nil) and (DrawPict.Graphic.Palette <> 0) then
          begin
          H := SelectPalette(Handle, DrawPict.Graphic.Palette, false);
          RealizePalette(Handle);
          end;
        Dest.Left   := QRPrinter.XPos(OfsX + Size.Left);
        Dest.Top    := QRPrinter.YPos(OfsY + Size.Top);
        Dest.Right  := QRPrinter.XPos(OfsX + Size.Width + Size.Left);
        Dest.Bottom := QRPrinter.YPos(OfsY + Size.Height + Size.Top);
        if Stretch then
          begin
          if (DrawPict.Graphic = nil) or DrawPict.Graphic.Empty then
            FillRect(Dest)
          else
            with QRPrinter.Canvas do
              StretchDraw(Dest, DrawPict.Graphic);
          end
        else
          begin
          IntersectClipRect(Handle, Dest.Left, Dest.Top, Dest.Right, Dest.Bottom);
          Dest.Right := Dest.Left +
            round(DrawPict.Width / Screen.PixelsPerInch * 254 * ParentReport.QRPrinter.XFactor);
          Dest.Bottom := Dest.Top +
            round(DrawPict.Height / Screen.PixelsPerInch * 254 * ParentReport.QRPrinter.YFactor);
          if Center then OffsetRect(Dest,
            (QRPrinter.XSize(Size.Width) -
              round(DrawPict.Width / Screen.PixelsPerInch * 254 * ParentReport.QRPrinter.XFactor)) div 2,
            (QRPrinter.YSize(Size.Height) -
              round(DrawPict.Height / Screen.PixelsPerInch * 254 * ParentReport.QRPrinter.YFactor)) div 2);
          QRPrinter.Canvas.StretchDraw(Dest, DrawPict.Graphic);
          SelectClipRgn(Handle, 0);
        end;
      end;
    finally
      if H <> 0 then SelectPalette(Handle, H, True);
      DrawPict.Free;
      fPictureLoaded := False;
    end;
  end;
  inherited Print(OfsX,OfsY);
end;

procedure TQREDBImage.Unprepare;
begin
  FField := nil;
  inherited Unprepare;
end;

procedure TQREDBImage.SetDataSet(Value: TDataSet);
begin
  FDataSet := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TQREDBImage.SetDataField(const Value: string);
begin
  FDataField := Value;
end;

function TQREDBImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette;
end;

procedure TQREDBImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TQREDBImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TQREDBImage.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Invalidate;
  end;
end;

procedure TQREDBImage.Paint;
var
  W, H: Integer;
  R: TRect;
  S: string;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Font := Self.Font;
    if Field <> nil then
      S := Field.DisplayLabel
    else
      S := Name;
    S := '(' + S + ')';
    W := TextWidth(S);
    H := TextHeight(S);
    R := ClientRect;
    TextRect(R, (R.Right - W) div 2, (R.Bottom - H) div 2, S);
  end;
  Inherited Paint;
end;

procedure TQREDBImage.PictureChanged(Sender: TObject);
begin
  FPictureLoaded := True;
  Invalidate;
end;

procedure TQREDBImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DataSet) then
    DataSet := nil;
end;

procedure TQREDBImage.LoadPicture;
{ begin
  if not FPictureLoaded and (Field is TBlobField) then
    Picture.Assign(FField); }
var B: TGraphic;
  Buf: word;
begin
   if (Field = nil) or not Field.IsBlob or Field.IsNull then
     begin
     Picture.Assign(Nil);
     Exit;
     end;

  if not FPictureLoaded and (not Assigned(Field)
     or Field.IsBlob) then
    try
      Memoria.Clear;
      TBlobField(Field).SaveToStream(Memoria);
      Memoria.Position := 0;
      B:= nil;
      if Memoria.Size > SizeOf(buf) then
        begin
        Memoria.read(buf, SizeOf(buf));
        case buf of
          $0000 : B := TIcon.Create;
          $0001 : begin
                  Picture.Assign(Field);
                  Exit;
                  end;
          $4D42 : B := TBitmap.Create;
          $CDD7 : B := TMetafile.Create;
          $D8FF : B := TJPEGImage.Create;
        end;
        end;
      Memoria.Position := 0;
      if B <> nil then
        try
          B.LoadFromStream(Memoria);
        except
          B:= nil;
        end;
  //if the stored image is not ico, bmp, wmf, emf, jpg, jpeg
  //or I cant load it then fire the event
      if (b = nil) and Assigned(fOnLoadCustomImage) then
        try
          memoria.Position := 0;
          fOnLoadCustomImage(B, Memoria);
        except
          B:= nil;
        end;

        Picture.Assign(B);//B can be NIL, no problem with that
    finally
      B.Free;
      Memoria.Clear;
    end;
end;


end.

