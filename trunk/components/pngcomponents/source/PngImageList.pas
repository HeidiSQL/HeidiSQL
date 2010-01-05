unit PngImageList;

{$I compilers.inc}

{$IF RTLVersion < 15.0 }
This unit only compiles with Delphi 7 and higher!
{$IFEND}

interface

uses
  Windows, Classes, SysUtils, Controls, Graphics, ImgList, PngImage,
  PngFunctions;

type
  TPngImageCollection = class;
  TPngImageCollectionItem = class;
  TPngImageCollectionItems = class;

  TPngImageList = class(TImageList)
  private
    FEnabledImages: Boolean;
    FLocked: Integer;
    FPngImages: TPngImageCollectionItems;
    FPngOptions: TPngOptions;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure SetWidth(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CopyPngs; virtual;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure InternalInsertPng(Index: Integer; const Png: TPngImage; Background:
        TColor = clNone);
    procedure InternalAddPng(const Png: TPngImage; Background: TColor = clNone);
    function PngToIcon(const Png: TPngImage; Background: TColor = clNone): HICON;
    procedure ReadData(Stream: TStream); override;
    procedure SetEnabledImages(const Value: Boolean); virtual;
    procedure SetPngImages(const Value: TPngImageCollectionItems); virtual;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Patched methods
    function Add(Image, Mask: TBitmap): Integer; virtual;
    function AddIcon(Image: TIcon): Integer; virtual;
    function AddPng(Image: TPngImage; Background: TColor = clNone): Integer;
    function AddImage(Value: TCustomImageList; Index: Integer): Integer; virtual;
    procedure AddImages(Value: TCustomImageList); virtual;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer; virtual;
    procedure BeginUpdate;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure EndUpdate(Update: Boolean = True);
    procedure Insert(Index: Integer; Image, Mask: TBitmap); virtual;
    procedure InsertIcon(Index: Integer; Image: TIcon); virtual;
    procedure InsertPng(Index: Integer; Image: TPngImage; Background: TColor =
        clNone);
    procedure InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure Replace(Index: Integer; Image, Mask: TBitmap); virtual;
    procedure ReplaceIcon(Index: Integer; Image: TIcon); virtual;
    procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor); virtual;
  published
    property EnabledImages: Boolean read FEnabledImages write SetEnabledImages default True;
    property Height read GetHeight write SetHeight default 16;
    property PngImages: TPngImageCollectionItems read FPngImages write SetPngImages;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];
    property Width read GetWidth write SetWidth default 16;
  end;

  TPngImageCollection = class(TComponent)
  private
    FItems: TPngImageCollectionItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TPngImageCollectionItems read FItems write FItems;
  end;

  TPngImageCollectionItems = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TPngImageCollectionItem;
    procedure SetItem(Index: Integer; const Value: TPngImageCollectionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add(DontCreatePNG: Boolean = False): TPngImageCollectionItem; reintroduce;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer; DontCreatePNG: Boolean = False): TPngImageCollectionItem; reintroduce;
    property Items[index: Integer]: TPngImageCollectionItem read GetItem write SetItem; default;
  end;

  TPngImageCollectionItem = class(TCollectionItem)
  private
    FBackground: TColor;
    FName: string;
    FPngImage: TPngImage;
    procedure SetBackground(const Value: TColor);
    procedure SetPngImage(const Value: TPngImage);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(Collection: TCollection; DontCreatePNG: Boolean = False); reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Duplicate: TPngImage;
  published
    property Background: TColor read FBackground write SetBackground default clBtnFace;
    property Name: string read FName write FName;
    property PngImage: TPngImage read FPngImage write SetPngImage;
  end;

implementation

uses
  Math, Contnrs, CommCtrl, ComCtrls;

var
  ImageListCount: Integer = 0;
  MethodPatches: TObjectList = nil;

type
  TMethodPatch = class
  private
    Name: string;
    OldBody: array[0..5] of Byte;
    OldPointer, NewPointer: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginInvokeOldMethod;
    procedure FinishInvokeOldMethod;
    function PatchBack: Boolean;
  end;

{ Global }

function FindMethodPatch(const Name: string): TMethodPatch;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to MethodPatches.Count - 1 do begin
    if TMethodPatch(MethodPatches[I]).Name = Name then begin
      Result := TMethodPatch(MethodPatches[I]);
      Break;
    end;
  end;
end;

function PatchPtr(OldPtr, NewPtr: Pointer; const Name: string; Patch: TMethodPatch): Boolean;
var
  Access: Cardinal;
begin
  Result := False;
  Patch.Name := Name;
  if OldPtr <> NewPtr then begin
    Patch.OldPointer := OldPtr;
    Patch.NewPointer := NewPtr;
    Move(PByte(OldPtr)^, Patch.OldBody[0], SizeOf(Patch.OldBody));
    if VirtualProtect(OldPtr, 16, PAGE_EXECUTE_READWRITE, @Access) then begin
      PByte(OldPtr)^ := $E9; // Near jump
      PCardinal(Cardinal(OldPtr) + 1)^ := Cardinal(NewPtr) - Cardinal(OldPtr) - 5;
      VirtualProtect(OldPtr, 16, Access, @Access);
      Result := True;
    end;
  end;
  if not Result then
    Patch.OldPointer := nil;
end;

procedure ApplyMethodPatches;
type
  TPointerCombo = record
    OldPtr, NewPtr: Pointer;
    Name: string;
  end;

  function Combo(OldPtr, NewPtr: Pointer; const Name: string): TPointerCombo;
  begin
    Result.OldPtr := OldPtr;
    Result.NewPtr := NewPtr;
    Result.Name := Name;
  end;

const
  EmptyCombo: TPointerCombo = (OldPtr: nil; NewPtr: nil; Name: '');
var
  Pointers: array of TPointerCombo;
  Patch: TMethodPatch;
  I: Integer;
begin
  if ImageListCount = 0 then begin
    SetLength(Pointers, 14);
    Pointers[0] := Combo(@TCustomImageList.Add, @TPngImageList.Add, 'Add');
    Pointers[1] := Combo(@TCustomImageList.AddIcon, @TPngImageList.AddIcon, 'AddIcon');
    Pointers[2] := Combo(@TCustomImageList.AddImage, @TPngImageList.AddImage, 'AddImage');
    Pointers[3] := Combo(@TCustomImageList.AddImages, @TPngImageList.AddImages, 'AddImages');
    Pointers[4] := Combo(@TCustomImageList.AddMasked, @TPngImageList.AddMasked, 'AddMasked');
    Pointers[5] := Combo(@TCustomImageList.Clear, @TPngImageList.Clear, 'Clear');
    Pointers[6] := Combo(@TCustomImageList.Delete, @TPngImageList.Delete, 'Delete');
    Pointers[7] := Combo(@TCustomImageList.Insert, @TPngImageList.Insert, 'Insert');
    Pointers[8] := Combo(@TCustomImageList.InsertIcon, @TPngImageList.InsertIcon, 'InsertIcon');
    Pointers[9] := Combo(@TCustomImageList.InsertMasked, @TPngImageList.InsertMasked, 'InsertMasked');
    Pointers[10] := Combo(@TCustomImageList.Move, @TPngImageList.Move, 'Move');
    Pointers[11] := Combo(@TCustomImageList.Replace, @TPngImageList.Replace, 'Replace');
    Pointers[12] := Combo(@TCustomImageList.ReplaceIcon, @TPngImageList.ReplaceIcon, 'ReplaceIcon');
    Pointers[13] := Combo(@TCustomImageList.ReplaceMasked, @TPngImageList.ReplaceMasked, 'ReplaceMasked');

    MethodPatches := TObjectList.Create;
    for I := Low(Pointers) to High(Pointers) do begin
      if Pointers[I].OldPtr <> nil then begin
        Patch := TMethodPatch.Create;
        if PatchPtr(Pointers[I].OldPtr, Pointers[I].NewPtr, Pointers[I].Name, Patch) then
          MethodPatches.Add(Patch)
        else
          Patch.Free;
      end;
    end;
  end;
end;

procedure RevertPatchedMethods;
begin
  if ImageListCount = 0 then
    FreeAndNil(MethodPatches);
end;

{ TMethodPatch }

constructor TMethodPatch.Create;
begin
  inherited Create;
  OldPointer := nil;
end;

destructor TMethodPatch.Destroy;
begin
  if OldPointer <> nil then
    PatchBack;
  inherited Destroy;
end;

procedure TMethodPatch.BeginInvokeOldMethod;
begin
  PatchBack;
end;

procedure TMethodPatch.FinishInvokeOldMethod;
begin
  PatchPtr(OldPointer, NewPointer, Name, Self);
end;

function TMethodPatch.PatchBack: Boolean;
var
  Access: Cardinal;
begin
  Result := False;
  if VirtualProtect(OldPointer, 16, PAGE_EXECUTE_READWRITE, @Access) then begin
    Move(OldBody[0], OldPointer^, SizeOf(OldBody));
    VirtualProtect(OldPointer, 16, Access, @Access);
    Result := True;
  end;
end;

constructor TPngImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if ImageListCount = 0 then
    ApplyMethodPatches;
  Inc(ImageListCount);
  FEnabledImages := True;
  FPngOptions := [pngBlendOnDisabled];
  FPngImages := TPngImageCollectionItems.Create(Self);
  FLocked := 0;
end;

destructor TPngImageList.Destroy;
begin
  FPngImages.Free;
  Dec(ImageListCount);
  if ImageListCount = 0 then
    RevertPatchedMethods;
  inherited Destroy;
end;

//--- Patched methods ---

function TPngImageList.Add(Image, Mask: TBitmap): Integer;
var
  Patch: TMethodPatch;
  Png: TPngImage;
begin
  if TObject(Self) is TPngImageList then begin
    Png := TPngImage.Create;
    try
      CreatePNG(Image, Mask, Png);
      result := AddPng(Png);
    finally
      Png.Free;
    end;
  end
  else begin
    Patch := FindMethodPatch('Add');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        Result := TCustomImageList(Self).Add(Image, Mask);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end
    else
      Result := -1;
  end;
end;

function TPngImageList.AddIcon(Image: TIcon): Integer;
var
  Patch: TMethodPatch;
  Png: TPngImage;
begin
  if TObject(Self) is TPngImageList then begin
    Png := TPngImage.Create;
    try
      ConvertToPNG(Image, Png);
      result := AddPng(Png);
    finally
      Png.Free;
    end;
  end
  else begin
    Patch := FindMethodPatch('AddIcon');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        Result := TCustomImageList(Self).AddIcon(Image);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end
    else
      Result := -1;
  end;
end;

function TPngImageList.AddPng(Image: TPngImage; Background: TColor = clNone):
    Integer;
var
  Item: TPngImageCollectionItem;
begin
  Result := -1;
  if Image = nil then Exit;

  BeginUpdate;
  try
    Item := FPngImages.Add(True);
    Item.PngImage := Image;
    Item.Background := Background;
    Result := Item.Index;
    InternalAddPng(Item.PngImage, Item.Background);
    Change;
  finally
    EndUpdate(false);
  end;
end;

function TPngImageList.AddImage(Value: TCustomImageList; Index: Integer): Integer;
var
  Patch: TMethodPatch;
  Png: TPngImage;
begin
  if TObject(Self) is TPngImageList then begin
    Png := TPngImage.Create;
    try
      CopyImageFromImageList(Png, Value, Index);
      result := AddPng(Png);
    finally
      Png.Free;
    end;
  end
  else begin
    Patch := FindMethodPatch('AddImage');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        Result := TCustomImageList(Self).AddImage(Value, Index);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end
    else
      Result := -1;
  end;
end;

procedure TPngImageList.AddImages(Value: TCustomImageList);
var
  Patch: TMethodPatch;
  I: Integer;
  Png: TPngImage;
begin
  if TObject(Self) is TPngImageList then begin
    BeginUpdate;
    try
      //Copy every image from Value into this imagelist.
      Png := TPngImage.Create;
      try
        for I := 0 to Value.Count - 1 do begin
          CopyImageFromImageList(Png, Value, I);
          AddPng(Png);
        end;
      finally
        Png.Free;
      end;
    finally
      EndUpdate;
    end;
  end
  else begin
    Patch := FindMethodPatch('AddImages');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).AddImages(Value);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

function TPngImageList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
var
  Patch: TMethodPatch;
  Png: TPngImage;
begin
  if TObject(Self) is TPngImageList then begin
    Png := TPngImage.Create;
    try
      CreatePNGMasked(Image, MaskColor, Png);
      result := AddPng(Png);
    finally
      Png.Free;
    end;
  end
  else begin
    Patch := FindMethodPatch('AddMasked');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        Result := TCustomImageList(Self).AddMasked(Image, MaskColor);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end
    else
      Result := -1;
  end;
end;

procedure TPngImageList.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TPngImageList then begin
    TPngImageList(Dest).PngImages := FPngImages;
    TPngImageList(Dest).EnabledImages := FEnabledImages;
  end;
end;

procedure TPngImageList.BeginUpdate;
begin
  Inc(FLocked);
end;

procedure TPngImageList.Clear;
var
  Patch: TMethodPatch;
begin
  if TObject(Self) is TPngImageList then begin
    //Clear the PngImages collection and the internal imagelist.
    BeginUpdate;
    try
      FPngImages.Clear;
      ImageList_Remove(Handle, -1);
      Change;
    finally
      EndUpdate(False);
    end;
  end
  else begin
    Patch := FindMethodPatch('Clear');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).Clear;
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

procedure TPngImageList.CopyPngs;
var
  I: Integer;
  Png: TPngImage;
  Icon: HIcon;
  item: TPngImageCollectionItem;
begin
  //Are we adding a bunch of images?
  if FLocked > 0 then
    Exit;

  //Copy PNG images to the imagelist. These images will not be stored in the DFM.
  ImageList_Remove(Handle, -1);
  Handle := ImageList_Create(Width, Height, ILC_COLOR32 or (Integer(Masked) *
    ILC_MASK), 0, AllocBy);

  Png := TPngImage.Create;
  try
    for I := 0 to FPngImages.Count - 1 do begin
      item := FPngImages.Items[I];
      if (item.PngImage = nil) or item.PngImage.Empty then
        Continue;
      if FEnabledImages or (FPngOptions = []) then begin
        Icon := PngToIcon(item.PngImage, item.Background);
      end
      else begin
        //Basically the same as in the DrawPNG function
        Png.Assign(item.PngImage);
        if pngBlendOnDisabled in FPngOptions then
          MakeImageBlended(Png);
        if pngGrayscaleOnDisabled in FPngOptions then
          MakeImageGrayscale(Png);
        Icon := PngToIcon(Png);
      end;
      ImageList_AddIcon(Handle, Icon);
      DestroyIcon(Icon);
    end;
  finally
    Png.Free;
  end;
end;

procedure TPngImageList.Delete(Index: Integer);
var
  Patch: TMethodPatch;
begin
  if TObject(Self) is TPngImageList then begin
    //Delete an image from the PngImages collection and from the internal imagelist.
    if (Index >= 0) and (Index < Count) then begin
      BeginUpdate;
      try
        FPngImages.Delete(Index);
        ImageList_Remove(Handle, Index);
        Change;
      finally
        EndUpdate(False);
      end;
    end;
  end
  else begin
    Patch := FindMethodPatch('Delete');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).Delete(Index);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

//--- End of patched methods ---

procedure TPngImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  PaintRect: TRect;
  Options: TPngOptions;
  Png: TPngImageCollectionItem;
begin
  //Draw a PNG directly to the Canvas. This is the preferred method to call,
  //because this omits the API calls that use a fixed background.
  PaintRect := Bounds(X, Y, Width, Height);
  if Enabled then
    Options := []
  else
    Options := FPngOptions;
  Png := FPngImages.Items[Index];
  if Png <> nil then
    DrawPNG(Png.PngImage, Canvas, PaintRect, Options);
end;

procedure TPngImageList.EndUpdate(Update: Boolean);
begin
  Dec(FLocked);
  if Update and (FLocked = 0) then
    CopyPngs;
end;

function TPngImageList.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TPngImageList.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TPngImageList.Insert(Index: Integer; Image, Mask: TBitmap);
var
  Patch: TMethodPatch;
  Png: TPngImage;
begin
  if TObject(Self) is TPngImageList then begin
    //Insert a new PNG based on the image and its mask.
    if Image <> nil then begin
      Png := TPngImage.Create;
      try
        CreatePNG(Image, Mask, Png);
        InsertPng(Index, Png);
      finally
        Png.Free;
      end;
    end;
  end
  else begin
    Patch := FindMethodPatch('Insert');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).Insert(Index, Image, Mask);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

procedure TPngImageList.InsertIcon(Index: Integer; Image: TIcon);
var
  Patch: TMethodPatch;
  Png: TPngImage;
begin
  if TObject(Self) is TPngImageList then begin
    //Insert a new PNG based on the image.
    if Image <> nil then begin
      Png := TPngImage.Create;
      try
        ConvertToPNG(Image, Png);
        InsertPng(Index, Png);
      finally
        Png.Free;
      end;
    end;
  end
  else begin
    Patch := FindMethodPatch('InsertIcon');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).InsertIcon(Index, Image);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

procedure TPngImageList.InsertPng(Index: Integer; Image: TPngImage; Background:
    TColor = clNone);
var
  Item: TPngImageCollectionItem;
begin
  if Image <> nil then begin
    BeginUpdate;
    try
      Item := PngImages.Insert(Index, True);
      Item.PngImage := Image;
      Item.Background := Background;
      InternalInsertPng(Index, Item.PngImage, Item.Background);
      Change;
    finally
      EndUpdate(False);
    end;
  end;
end;

procedure TPngImageList.InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor);
var
  Patch: TMethodPatch;
  Png: TPngImage;
begin
  if TObject(Self) is TPngImageList then begin
    //Insert a new PNG based on the image and a colored mask.
    if Image <> nil then begin
      Png := TPngImage.Create;
      try
        CreatePNGMasked(Image, MaskColor, Png);
        InsertPng(Index, Png);
      finally
        Png.Free;
      end;
    end;
  end
  else begin
    Patch := FindMethodPatch('InsertMasked');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).InsertMasked(Index, Image, MaskColor);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

procedure TPngImageList.InternalInsertPng(Index: Integer; const Png: TPngImage;
    Background: TColor);
var
  I: Integer;
  Icon: HICON;
  TempList: TPngImageList;
begin
  TempList := TPngImageList(TComponentClass(ClassType).Create(nil));
  try
    TempList.Assign(Self);
    ImageList_RemoveAll(Handle);
    for I := 0 to Index - 1 do begin
      Icon := ImageList_GetIcon(TempList.Handle, I, ILD_NORMAL);
      ImageList_AddIcon(Handle, Icon);
      DestroyIcon(Icon);
    end;
    Icon := PngToIcon(Png, Background);
    ImageList_AddIcon(Handle, Icon);
    DestroyIcon(Icon);
    for I := Index to TempList.Count - 1 do begin
      Icon := ImageList_GetIcon(TempList.Handle, I, ILD_NORMAL);
      ImageList_AddIcon(Handle, Icon);
      DestroyIcon(Icon);
    end;
  finally
    TempList.Free;
  end;
end;

procedure TPngImageList.InternalAddPng(const Png: TPngImage; Background: TColor
    = clNone);
var
  Icon: HICON;
begin
  Icon := PngToIcon(Png, Background);
  try
    ImageList_AddIcon(Handle, Icon);
  finally
    DestroyIcon(Icon);
  end;
end;

procedure TPngImageList.Move(CurIndex, NewIndex: Integer);
var
  Patch: TMethodPatch;
begin
  if TObject(Self) is TPngImageList then begin
    //Move an image from one position to another. Don't try doing so in the internal
    //imagelist, just recreate it, since this method won't be called very often.
    BeginUpdate;
    try
      ImageList_Remove(Handle, CurIndex);
      InternalInsertPng(NewIndex, FPngImages[CurIndex].PngImage,
        FPngImages[CurIndex].Background);
      FPngImages[CurIndex].Index := NewIndex;
      Change;
    finally
      EndUpdate(False);
    end;
  end
  else begin
    Patch := FindMethodPatch('Move');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).Move(CurIndex, NewIndex);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

function TPngImageList.PngToIcon(const Png: TPngImage; Background: TColor): HICON;
const
  MaxRGBQuads = MaxInt div SizeOf(TRGBQuad) - 1;
type
  TRGBQuadArray = array[0..MaxRGBQuads] of TRGBQuad;
  PRGBQuadArray = ^TRGBQuadArray;
  TBitmapInfo4 = packed record
    bmiHeader: TBitmapV4Header;
    bmiColors: array[0..0] of TRGBQuad;
  end;

  function PngToIcon32(Png: TPngImage): HIcon;
  var
    ImageBits: PRGBQuadArray;
    BitmapInfo: TBitmapInfo4;
    IconInfo: TIconInfo;
    AlphaBitmap: HBitmap;
    MaskBitmap: TBitmap;
    X, Y: Integer;
    AlphaLine: PByteArray;
    HasAlpha, HasBitmask: Boolean;
    Color, TransparencyColor: TColor;
  begin
    //Convert a PNG object to an alpha-blended icon resource
    ImageBits := nil;

    //Allocate a DIB for the color data and alpha channel
    with BitmapInfo.bmiHeader do begin
      bV4Size := SizeOf(BitmapInfo.bmiHeader);
      bV4Width := Png.Width;
      bV4Height := Png.Height;
      bV4Planes := 1;
      bV4BitCount := 32;
      bV4V4Compression := BI_BITFIELDS;
      bV4SizeImage := 0;
      bV4XPelsPerMeter := 0;
      bV4YPelsPerMeter := 0;
      bV4ClrUsed := 0;
      bV4ClrImportant := 0;
      bV4RedMask := $00FF0000;
      bV4GreenMask := $0000FF00;
      bV4BlueMask := $000000FF;
      bV4AlphaMask := $FF000000;
    end;
    AlphaBitmap := CreateDIBSection(0, PBitmapInfo(@BitmapInfo)^,
      DIB_RGB_COLORS, Pointer(ImageBits), 0, 0);
    try
      //Spin through and fill it with a wash of color and alpha.
      AlphaLine := nil;
      HasAlpha := Png.Header.ColorType in [COLOR_GRAYSCALEALPHA,
        COLOR_RGBALPHA];
      HasBitmask := Png.TransparencyMode = ptmBit;
      TransparencyColor := Png.TransparentColor;
      for Y := 0 to Png.Height - 1 do begin
        if HasAlpha then
          AlphaLine := Png.AlphaScanline[Png.Height - Y - 1];
        for X := 0 to Png.Width - 1 do begin
          Color := Png.Pixels[X, Png.Height - Y - 1];
          ImageBits^[Y * Png.Width + X].rgbRed := Color and $FF;
          ImageBits^[Y * Png.Width + X].rgbGreen := Color shr 8 and $FF;
          ImageBits^[Y * Png.Width + X].rgbBlue := Color shr 16 and $FF;
          if HasAlpha then
            ImageBits^[Y * Png.Width + X].rgbReserved := AlphaLine^[X]
          else if HasBitmask then
            ImageBits^[Y * Png.Width + X].rgbReserved := Integer(Color <>
              TransparencyColor) * 255;
        end;
      end;

      //Create an empty mask
      MaskBitmap := TBitmap.Create;
      try
        MaskBitmap.Width := Png.Width;
        MaskBitmap.Height := Png.Height;
        MaskBitmap.PixelFormat := pf1bit;
        MaskBitmap.Canvas.Brush.Color := clBlack;
        MaskBitmap.Canvas.FillRect(Rect(0, 0, MaskBitmap.Width,
          MaskBitmap.Height));

        //Create the alpha blended icon
        IconInfo.fIcon := True;
        IconInfo.hbmColor := AlphaBitmap;
        IconInfo.hbmMask := MaskBitmap.Handle;
        Result := CreateIconIndirect(IconInfo);
      finally
        MaskBitmap.Free;
      end;
    finally
      DeleteObject(AlphaBitmap);
    end;
  end;

  function PngToIcon24(Png: TPngImage; Background: TColor): HIcon;
  var
    ColorBitmap, MaskBitmap: TBitmap;
    X, Y: Integer;
    AlphaLine: PByteArray;
    IconInfo: TIconInfo;
    TransparencyColor: TColor;
  begin
    ColorBitmap := TBitmap.Create;
    MaskBitmap := TBitmap.Create;
    try
      ColorBitmap.Width := Png.Width;
      ColorBitmap.Height := Png.Height;
      ColorBitmap.PixelFormat := pf32bit;
      MaskBitmap.Width := Png.Width;
      MaskBitmap.Height := Png.Height;
      MaskBitmap.PixelFormat := pf32bit;

      //Draw the color bitmap
      ColorBitmap.Canvas.Brush.Color := Background;
      ColorBitmap.Canvas.FillRect(Rect(0, 0, Png.Width, Png.Height));
      Png.Draw(ColorBitmap.Canvas, Rect(0, 0, Png.Width, Png.Height));

      //Create the mask bitmap
      if Png.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA] then
        for Y := 0 to Png.Height - 1 do begin
          AlphaLine := Png.AlphaScanline[Y];
          for X := 0 to Png.Width - 1 do
            if AlphaLine^[X] = 0 then
              SetPixelV(MaskBitmap.Canvas.Handle, X, Y, clWhite)
            else
              SetPixelV(MaskBitmap.Canvas.Handle, X, Y, clBlack);
        end
      else if Png.TransparencyMode = ptmBit then begin
        TransparencyColor := Png.TransparentColor;
        for Y := 0 to Png.Height - 1 do
          for X := 0 to Png.Width - 1 do
            if Png.Pixels[X, Y] = TransparencyColor then
              SetPixelV(MaskBitmap.Canvas.Handle, X, Y, clWhite)
            else
              SetPixelV(MaskBitmap.Canvas.Handle, X, Y, clBlack);
      end;

      //Create the icon
      IconInfo.fIcon := True;
      IconInfo.hbmColor := ColorBitmap.Handle;
      IconInfo.hbmMask := MaskBitmap.Handle;
      Result := CreateIconIndirect(IconInfo);
    finally
      ColorBitmap.Free;
      MaskBitmap.Free;
    end;
  end;

begin
  if GetComCtlVersion >= ComCtlVersionIE6 then begin
    //Windows XP or later, using the modern method: convert every PNG to
    //an icon resource with alpha channel
    Result := PngToIcon32(Png);
  end
  else begin
    //No Windows XP, using the legacy method: copy every PNG to a normal
    //bitmap using a fixed background color
    Result := PngToIcon24(Png, Background);
  end;
end;

procedure TPngImageList.ReadData(Stream: TStream);
begin
  //Make sure nothing gets read from the DFM
end;

procedure TPngImageList.Replace(Index: Integer; Image, Mask: TBitmap);
var
  Item: TPngImageCollectionItem;
  Patch: TMethodPatch;
  Icon: HICON;
begin
  if TObject(Self) is TPngImageList then begin
    //Replace an existing PNG based with a new image and its mask.
    if Image <> nil then begin
      BeginUpdate;
      try
        Item := FPngImages[Index];
        Item.FPngImage.Free;
        CreatePNG(Image, Mask, Item.FPngImage);
        Icon := PngToIcon(Item.PngImage, Item.Background);
        ImageList_ReplaceIcon(Handle, Index, Icon);
        DestroyIcon(Icon);
        Change;
      finally
        EndUpdate(False);
      end;
    end;
  end
  else begin
    Patch := FindMethodPatch('Replace');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).Replace(Index, Image, Mask);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

procedure TPngImageList.ReplaceIcon(Index: Integer; Image: TIcon);
var
  Item: TPngImageCollectionItem;
  Patch: TMethodPatch;
  Icon: HICON;
begin
  if TObject(Self) is TPngImageList then begin
    //Replace an existing PNG based with a new image.
    if Image <> nil then begin
      BeginUpdate;
      try
        Item := FPngImages[Index];
        Item.FPngImage.Free;
        ConvertToPNG(Image, Item.FPngImage);
        Icon := PngToIcon(Item.PngImage, Item.Background);
        ImageList_ReplaceIcon(Handle, Index, Icon);
        DestroyIcon(Icon);
        Change;
      finally
        EndUpdate(False);
      end;
    end
  end
  else begin
    Patch := FindMethodPatch('ReplaceIcon');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).ReplaceIcon(Index, Image);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

procedure TPngImageList.ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
var
  Item: TPngImageCollectionItem;
  Patch: TMethodPatch;
  Icon: HICON;
begin
  if TObject(Self) is TPngImageList then begin
    //Replace an existing PNG based with a new image and a colored mask.
    if NewImage <> nil then begin
      BeginUpdate;
      try
        Item := FPngImages[Index];
        Item.FPngImage.Free;
        CreatePNGMasked(NewImage, MaskColor, Item.FPngImage);
        Icon := PngToIcon(Item.PngImage, Item.Background);
        ImageList_ReplaceIcon(Handle, Index, Icon);
        DestroyIcon(Icon);
        Change;
      finally
        EndUpdate(False);
      end;
    end
  end
  else begin
    Patch := FindMethodPatch('ReplaceMasked');
    if Patch <> nil then begin
      Patch.BeginInvokeOldMethod;
      try
        TCustomImageList(Self).ReplaceMasked(Index, NewImage, MaskColor);
      finally
        Patch.FinishInvokeOldMethod;
      end;
    end;
  end;
end;

procedure TPngImageList.SetEnabledImages(const Value: Boolean);
begin
  if FEnabledImages xor Value then begin
    FEnabledImages := Value;
    CopyPngs;
  end;
end;

procedure TPngImageList.SetHeight(const Value: Integer);
begin
  if inherited Height <> Value then begin
    inherited Height := Value;
    Clear;
  end;
end;

procedure TPngImageList.SetPngImages(const Value: TPngImageCollectionItems);
begin
  if FPngImages <> Value then begin
    FPngImages.Assign(Value);
    Change;
  end;
end;

procedure TPngImageList.SetPngOptions(const Value: TPngOptions);
begin
  if FPngOptions <> Value then begin
    FPngOptions := Value;
    CopyPngs;
  end;
end;

procedure TPngImageList.SetWidth(const Value: Integer);
begin
  if inherited Width <> Value then begin
    inherited Width := Value;
    Clear;
  end;
end;

procedure TPngImageList.WriteData(Stream: TStream);
begin
  //Make sure nothing gets written to the DFM
end;

{ TPngImageCollection }

constructor TPngImageCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TPngImageCollectionItems.Create(Self);
end;

destructor TPngImageCollection.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{ TPngImageCollectionItems }

constructor TPngImageCollectionItems.Create(AOwner: TPersistent);
begin
  inherited Create(TPngImageCollectionItem);
  FOwner := AOwner;
end;

function TPngImageCollectionItems.Add(DontCreatePNG: Boolean = False): TPngImageCollectionItem;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  Result := TPngImageCollectionItem.Create(Self, DontCreatePNG);
  Added(TCollectionItem(Result));
end;

procedure TPngImageCollectionItems.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Update(nil);
end;

function TPngImageCollectionItems.GetItem(Index: Integer): TPngImageCollectionItem;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TPngImageCollectionItem(inherited Items[Index])
  else
    Result := nil;
end;

function TPngImageCollectionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TPngImageCollectionItems.Insert(Index: Integer; DontCreatePNG: Boolean = False): TPngImageCollectionItem;
begin
  Result := Add(DontCreatePNG);
  Result.Index := Index;
end;

procedure TPngImageCollectionItems.SetItem(Index: Integer; const Value: TPngImageCollectionItem);
begin
  if (Index >= 0) and (Index < Count) then
    inherited Items[Index] := Value;
end;

procedure TPngImageCollectionItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if FOwner is TPngImageList then
    TPngImageList(FOwner).CopyPngs;
end;

constructor TPngImageCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPngImage := TPngImage.Create;
  FName := Format('PngImage%d', [Index]);
  FBackground := clBtnFace;
end;

constructor TPngImageCollectionItem.Create(Collection: TCollection; DontCreatePNG: Boolean = False);
begin
  inherited Create(Collection);
  if DontCreatePng then
    FPngImage := nil
  else
    FPngImage := TPngImage.Create;
  FName := Format('PngImage%d', [Index]);
  FBackground := clBtnFace;
end;

destructor TPngImageCollectionItem.Destroy;
begin
  FPngImage.Free;
  inherited Destroy;
end;

procedure TPngImageCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TPngImageCollectionItem then begin
    PngImage.Assign(TPngImageCollectionItem(Source).PngImage);
    Background := TPngImageCollectionItem(Source).Background;
    Name := TPngImageCollectionItem(Source).Name;
  end
  else
    inherited Assign(Source);
end;

{ TPngImageCollectionItem }

procedure TPngImageCollectionItem.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if (Dest is TPngImageCollectionItem) then
    TPngImageCollectionItem(Dest).PngImage := PngImage;
end;

function TPngImageCollectionItem.Duplicate: TPngImage;
begin
  Result := TPngImage.Create;
  Result.Assign(FPngImage);
end;

function TPngImageCollectionItem.GetDisplayName: string;
begin
  if Length(FName) = 0 then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

procedure TPngImageCollectionItem.SetBackground(const Value: TColor);
begin
  if FBackground <> Value then begin
    FBackground := Value;
    Changed(False);
  end;
end;

procedure TPngImageCollectionItem.SetPngImage(const Value: TPngImage);
begin
  if FPngImage = nil then
    FPngImage := TPngImage.Create;
  FPngImage.Assign(Value);
  Changed(False);
end;

initialization

finalization
  MethodPatches.Free;

end.

