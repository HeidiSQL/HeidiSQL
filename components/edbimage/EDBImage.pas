unit EDBImage;
{$I Definitions.inc}
{
 TEDBImage 1.4 (Enhaced TDBImage):
  by Sebastián Mayorá - Argentina - DelphiHelper@yahoo.com.ar
 Please read EDBImage.txt or readme.txt for more information
}

interface

uses
  DBCtrls {TDataLink}, Windows {HPalette}, db {TField}, graphics, classes {TStream},
  Controls {TCustomControl}, Forms {TBorderStyle},
  Messages {TMessage};

type
  //Evento para cargar imágenes personalizadas desde el stream guardado
  TLoadCustomImageEvent = procedure (var B: TGraphic; Stream: TStream)of object;

  TEDBImage = class(TCustomControl)
  private
    FDataLink: TFieldDataLink;
    FPicture: TPicture;
    FBorderStyle: TBorderStyle;
    FAutoDisplay: Boolean;
    FStretch: Boolean;
    FCenter: Boolean;
    FPictureLoaded: Boolean;
    FQuickDraw: Boolean;
    fOnLoadCustomImage: TLoadCustomImageEvent;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCenter(Value: Boolean);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetPicture(Value: TPicture);
    procedure SetReadOnly(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  protected
    Memoria: TmemoryStream;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LoadPicture;
    procedure PasteFromClipboard;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
    property Picture: TPicture read FPicture write SetPicture;
    procedure LoadFromFile(const FileName: string);{v1.3}
    procedure SaveToFile(const FileName: string); {v1.3}
  published
    property Align;
    property Anchors;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Center: Boolean read FCenter write SetCenter default True;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property QuickDraw: Boolean read FQuickDraw write FQuickDraw default True;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    {$IFDEF DELPHI5}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnLoadCustomImage :TLoadCustomImageEvent read fOnLoadCustomImage write fOnLoadCustomImage;
  end;


implementation
uses clipbrd, JPeg;

constructor TEDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  Width := 105;
  Height := 105;
  TabStop := True;
  ParentColor := False;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FBorderStyle := bsSingle;
  FAutoDisplay := True;
  FCenter := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FQuickDraw := True;
  Memoria := TmemoryStream.Create;
end;

destructor TEDBImage.Destroy;
begin
  FPicture.Free;
  FDataLink.Free;
  FDataLink := nil;
  Memoria.Free;
  inherited Destroy;
end;

function TEDBImage.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TEDBImage.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TEDBImage.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TEDBImage.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TEDBImage.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TEDBImage.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TEDBImage.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TEDBImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
     Result := FPicture.Graphic.Palette;
{TGraphic has a property Pallete read GetPalette. and GetPalette is Virtual
Desendants re-implements GetPalette so I don't need ask what kind of Graphic is stored}
end;

procedure TEDBImage.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
    begin
    FAutoDisplay := Value;
    if Value then LoadPicture;
    end;
end;

procedure TEDBImage.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TEDBImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TEDBImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TEDBImage.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Invalidate;
  end;
end;

procedure TEDBImage.Paint;
var
  Size: TSize;
  R: TRect;
  S: string;
  DrawPict: TPicture;
  Form: TCustomForm;
  Pal: HPalette;
begin
  with Canvas do
    begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(ClientRect);
    if FPictureLoaded or (csPaintCopy in ControlState) then
      begin
      DrawPict := TPicture.Create;
      Pal := 0;
      try
        if (csPaintCopy in ControlState) and Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
          begin
          DrawPict.Assign(FDataLink.Field);
          if DrawPict.Graphic is TBitmap then
            DrawPict.Bitmap.IgnorePalette := QuickDraw;
          end
        else
          begin
          DrawPict.Assign(Picture);
          if Focused and (DrawPict.Graphic <> nil) and (DrawPict.Graphic.Palette <> 0) then
            begin { Control has focus, so realize the bitmap palette in foreground }
            Pal := SelectPalette(Handle, DrawPict.Graphic.Palette, False);
            RealizePalette(Handle);
            end;
        end;
        if Stretch then
          if (DrawPict.Graphic = nil) or DrawPict.Graphic.Empty then
            FillRect(ClientRect)
          else
            StretchDraw(ClientRect, DrawPict.Graphic)
        else
          begin
          SetRect(R, 0, 0, DrawPict.Width, DrawPict.Height);
          if Center then
            OffsetRect(R, (ClientWidth - DrawPict.Width) div 2, (ClientHeight - DrawPict.Height) div 2);
          StretchDraw(R, DrawPict.Graphic);
          ExcludeClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
          FillRect(ClientRect);
          SelectClipRgn(Handle, 0);
        end;
      finally
        if Pal <> 0 then SelectPalette(Handle, Pal, True);
        DrawPict.Free;
      end;
      end
    else
      begin
      Font := Self.Font;
      if FDataLink.Field <> nil then
        S := FDataLink.Field.DisplayLabel
      else
        S := Name;
      S := '(' + S + ')';
      Size := TextExtent(S);
      R := ClientRect;
      TextRect(R, (R.Right - Size.cx) div 2, (R.Bottom - Size.cy) div 2, S);
      end;
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.ActiveControl = Self) and
       not (csDesigning in ComponentState) and not (csPaintCopy in ControlState) then
      begin
      Brush.Color := clWindowFrame;
      FrameRect(ClientRect);
      end;
  end;
end;

procedure TEDBImage.PictureChanged(Sender: TObject);
begin
  if FPictureLoaded then FDataLink.Modified;
  FPictureLoaded := True;
  Invalidate;
end;

procedure TEDBImage.Notification(AComponent: TComponent;  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TEDBImage.LoadPicture;
var B: TGraphic;
  Buf: word;
begin
  with fDatalink do
   if (Field = nil) or not Field.IsBlob or Field.IsNull then
     begin
     Picture.Assign(Nil);
     Exit;
     end;

  if not FPictureLoaded and (not Assigned(FDataLink.Field)
     or FDataLink.Field.IsBlob) then
    try
      Memoria.Clear;
      TBlobField(Field).SaveToStream(Memoria);
      Memoria.Position :=0;
      B:= nil;
      if Memoria.Size > SizeOf(buf) then
        begin
        Memoria.read(buf, SizeOf(buf));
        case buf of
          $0000 : B := TIcon.Create;
          $0001 : begin
                  Picture.Assign(FDataLink.Field);
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
      if (b = nil) and Assigned(fonloadCustomImage) then
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

procedure TEDBImage.DataChange(Sender: TObject);
begin
  Picture.Graphic := nil;
  FPictureLoaded := False;
  if FAutoDisplay then LoadPicture;
end;

procedure TEDBImage.UpdateData(Sender: TObject);
var MS: TMemoryStream;
begin
  if (Picture.Graphic is TBitmap) then
     FDataLink.Field.Assign(Picture.Graphic)
  else
    begin
    MS := TMemoryStream.Create;
    try
      try
        Ms.Clear;
        Picture.Graphic.SaveToStream(MS);
        Ms.Position := 0;
        TBlobField(Fdatalink.field).LoadFromStream(MS);
      except
        FDataLink.Field.Clear;
      end;
    finally
    Ms.Free;
    end;
    end;
end;

procedure TEDBImage.CopyToClipboard;
var TempBMP: tbitmap;
begin
  if Picture.Graphic <> nil then
   if Picture.Graphic is TICon then
       begin
       Tempbmp := Tbitmap.Create;
       tempbmp.Width := Picture.graphic.width;
       tempbmp.Height:= Picture.graphic.Height;
       tempbmp.Canvas.Draw(0,0, Picture.Graphic);
       clipboard.Assign(tempBmp);
       tempbmp.Free;
       end
   else
    Clipboard.Assign(Picture);
end;

procedure TEDBImage.CutToClipboard;
begin
  if Picture.Graphic <> nil then
    if FDataLink.Edit then
    begin
      CopyToClipboard;
      Picture.Graphic := nil;
      PictureChanged(Self);
    end;
end;

procedure TEDBImage.PasteFromClipboard;
begin
  if fDatalink.edit then
    if ClipBoard.HasFormat(CF_BITMAP) then
     Picture.Bitmap.Assign(Clipboard)
    else
      if ClipBoard.HasFormat(CF_METAFILEPICT)or ClipBoard.HasFormat(CF_ENHMETAFILE) then
        picture.Metafile.Assign(ClipBoard)
      else
        if ClipBoard.hasformat(Cf_picture) then
          picture.Assign(ClipBoard);
  PictureChanged(Self);
end;

procedure TEDBImage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TEDBImage.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_INSERT:  if ssShift in Shift then
                   PasteFromClipBoard
                else
                  if ssCtrl in Shift then
                    CopyToClipBoard;
    VK_DELETE:  if ssShift in Shift then
                  CutToClipBoard;
  end;
end;

procedure TEDBImage.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ^X: CutToClipBoard;
    ^C: CopyToClipBoard;
    ^V: PasteFromClipBoard;
    #13: LoadPicture;
    #27: FDataLink.Reset;
  end;
end;

procedure TEDBImage.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TEDBImage.CMEnter(var Message: TCMEnter);
begin
  Invalidate; { Draw the focus marker }
  inherited;
end;

procedure TEDBImage.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  Invalidate; { Erase the focus marker }
  inherited;
end;

procedure TEDBImage.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if not FPictureLoaded then Invalidate;
end;

procedure TEDBImage.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if TabStop and CanFocus then SetFocus;
  inherited;
end;

procedure TEDBImage.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  LoadPicture;
  inherited;
end;

procedure TEDBImage.WMCut(var Message: TMessage);
begin
  CutToClipboard;
end;

procedure TEDBImage.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
end;

procedure TEDBImage.WMPaste(var Message: TMessage);
begin
  PasteFromClipboard;
end;

procedure TEDBImage.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

function TEDBImage.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TEDBImage.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TEDBImage.LoadFromFile(const FileName: string);
begin
  if Assigned(fDatalink.Field) and fDatalink.Field.IsBlob and fDatalink.Edit then
    begin
    TBlobField(fDatalink.Field).LoadFromFile(FileName);
    PictureChanged(Self);
    end;

end;

procedure TEDBImage.SaveToFile(const FileName: string);
begin
   if FPictureLoaded and Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
     TBlobField(fDatalink.Field).SaveToFile(FileName);
end;

end.
