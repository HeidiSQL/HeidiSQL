{-----------------------------------------------------------------------------
 Unit Name: VCL.Styles.DPIAware
 Author:    PyScripter  (https://github.com/pyscripter)
 Date:      13-Nov-2017
 Purpose:   Use VCL Styles in DPI Aware applications by scaling styles
 History:
-----------------------------------------------------------------------------}
{
  To use the unit just add it to the implementation uses statement of the main form and add 
  the following code to the FormCreate handler.
  
  procedure TFrmMain.FormCreate(Sender: TObject);
  Var
    StyleDPIAwareness : TStyleDPIAwareness;
  begin
    StyleDPIAwareness := TStyleDPIAwareness.Create(Self);
    StyleDPIAwareness.Parent := Self;

  By default the component scales the styles at multiples of 100%. You can change that, 
  by adding the line:
  
  StyleDPIAwareness.RoundScalingFactor := False;
  
  With this statement styles are scaled to whatever scaling factor results for Screen.PixelsPerInch. 
  Most of the styles would work fine, but a few may show some visual defects.

  Limitations:
    Does not support perMonitor DPI Awareness.
    You need to set DPI Awareness to System.
}

unit VCL.Styles.DPIAware;

interface
uses
  Winapi.Windows, WinAPI.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Themes, Vcl.Styles;

Type
  TStyleDPIAwareness = class(TControl)
  private
    FScaledStyles : TStringList;
    FRoundScalingFactor : Boolean;
    FUseCustomScalingFactor : Boolean;
    FCustomPPI : integer;
   protected
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure RecreateForms;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScaleStyle(Style : TCustomStyleServices);
  published
    {Rounds the scaling factorto the nearest 100%}
    property  RoundScalingFactor : Boolean read FRoundScalingFactor
      write FRoundScalingFactor default True;
    property  UseCustomScalingFactor : Boolean read FUseCustomScalingFactor
      write FUseCustomScalingFactor default False;
    property CustomPPI : integer read FCustomPPI write FCustomPPI default 96;
  end;

implementation

Uses
  System.Rtti,
  {$IFDEF VER330} // RAD Studio 10.3
  DDetours;
  {$ENDIF VER330}

{ TStyleDPIAwareness }

procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);
var
  buffer: TBitmap;
begin
  buffer := TBitmap.Create;
  try
    buffer.SetSize(NewWidth, NewHeight);
    buffer.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);
    Bitmap.SetSize(NewWidth, NewHeight);
    Bitmap.Canvas.Draw(0, 0, buffer);
  finally
    buffer.Free;
  end;
end;

procedure TStyleDPIAwareness.CMStyleChanged(var Message: TMessage);
begin
  ScaleStyle(TStyleManager.ActiveStyle);
end;

constructor TStyleDPIAwareness.Create(AOwner: TComponent);
begin
  inherited;
  FRoundScalingFactor := True;
  FUseCustomScalingFactor := False;
  FCustomPPI :=96;

  FScaledStyles := TStringList.Create;
  FScaledStyles.Sorted := False;

  ScaleStyle(TStyleManager.ActiveStyle);
end;

destructor TStyleDPIAwareness.Destroy;
begin
  FScaledStyles.Free;
  inherited;
end;

procedure TStyleDPIAwareness.RecreateForms;
Var
  i : Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    Screen.Forms[i].Perform(CM_RECREATEWND, 0, 0);
end;

procedure TStyleDPIAwareness.ScaleStyle(Style: TCustomStyleServices);
Var
  DPI : integer;
  SeStyle : TObject;
  SeStyleSource : TObject;
  BitmapList : TList;
  BitMap : TBitmap;
  StyleObjectList : Tlist;
  i : integer;
  StyleObject : TComponent;

  procedure ProcessBitmapLink(BL : TObject);
  Var
    BLType : TRTTIType;
  begin
    BLType := TRttiContext.Create.GetType(BL.ClassType);
    BLType.GetProperty('Bottom').SetValue(BL,  mulDiv(BLType.GetProperty('Bottom').GetValue(BL).AsInteger, DPI, 96));
    BLType.GetProperty('Right').SetValue(BL,  mulDiv(BLType.GetProperty('Right').GetValue(BL).AsInteger, DPI, 96));
    BLType.GetProperty('Left').SetValue(BL,  mulDiv(BLType.GetProperty('Left').GetValue(BL).AsInteger, DPI, 96));
    BLType.GetProperty('Top').SetValue(BL,  mulDiv(BLType.GetProperty('Top').GetValue(BL).AsInteger, DPI, 96));
  end;

  procedure ProcessSO(aSO : TComponent;  aSOType : TRTTIType);
  begin
    aSOType.GetProperty('Top').SetValue(aSO,  mulDiv(aSOType.GetProperty('Top').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('Left').SetValue(aSO,  mulDiv(aSOType.GetProperty('Left').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('Width').SetValue(aSO,  mulDiv(aSOType.GetProperty('Width').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('Height').SetValue(aSO,  mulDiv(aSOType.GetProperty('Height').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('MarginTop').SetValue(aSO,  mulDiv(aSOType.GetProperty('MarginTop').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('MarginLeft').SetValue(aSO,  mulDiv(aSOType.GetProperty('MarginLeft').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('MarginBottom').SetValue(aSO,  mulDiv(aSOType.GetProperty('MarginBottom').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('MarginRight').SetValue(aSO,  mulDiv(aSOType.GetProperty('MarginRight').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('TextMarginTop').SetValue(aSO,  mulDiv(aSOType.GetProperty('TextMarginTop').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('TextMarginLeft').SetValue(aSO,  mulDiv(aSOType.GetProperty('TextMarginLeft').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('TextMarginRight').SetValue(aSO,  mulDiv(aSOType.GetProperty('TextMarginRight').GetValue(aSO).AsInteger, DPI, 96));
  end;

  procedure ProcessStyleObject(SO : TComponent);
  var
    i : integer;
    ChildSo : TComponent;
    SOType : TRTTIType;
    BitmapLink : TObject;
  begin
      SOType := TRttiContext.Create.GetType(SO.ClassType);
      ProcessSO(SO, SOType);

    if So.ClassName = 'TSeBitmapObject' then begin
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
    end;

    if So.ClassName = 'TSeActiveBitmap' then begin
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FActiveBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
    end;

    if So.ClassName = 'TSeSystemButton' then begin
      // Shift the form title to the right
      if SO.Name = 'btnSysMenu' then
        SOType.GetProperty('Width').SetValue(SO,  MulDiv(28, DPI, 96));
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FActiveBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapPressed').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapHot').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
    end;

   if So.ClassName = 'TSeButtonObject' then begin
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapFocused').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapHot').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapPressed').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapDisabled').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
    end;

     for i := 0 to SO.ComponentCount - 1 do begin
       ChildSo := SO.Components[i];
       ProcessStyleObject(ChildSo);
     end;
  end;

begin
  // Check if Style already scaled then
  if FScaledStyles.IndexOf(Style.Name) >= 0 then
    Exit;

  if UseCustomScalingFactor then
    DPI := CustomPPI
  else begin
    DPI := Screen.PixelsPerInch;
    if FRoundScalingFactor then
      DPI := Round(DPI / 96) * 96;
  end;

  if (Style = TStyleManager.SystemStyle) or  (DPI <= 120) then
    Exit;

  SeStyle := TRttiContext.Create.GetType(Style.ClassType).GetField('FSource').GetValue(Style).AsObject;
  SeStyleSource := TRttiContext.Create.GetType(SeStyle.ClassType).GetField('FCleanCopy').GetValue(SeStyle).AsObject;
  BitMapList := TRttiContext.Create.GetType(SeStyleSource.ClassType).GetField('FBitmaps').GetValue(SeStyleSource).AsObject as TList;

  if BitMapList.Count = 1 then begin
    Bitmap := TObject(BitmapList[0]) as TBitmap;
    ResizeBitmap(Bitmap, MulDiv(Bitmap.Width, DPI, 96), Muldiv(Bitmap.Height, DPI, 96));

    StyleObjectList := TRttiContext.Create.GetType(SeStyleSource.ClassType).GetField('FObjects').GetValue(SeStyleSource).AsObject as TList;
    for i := 0 to StyleObjectList.Count -1 do begin
      StyleObject := TObject(StyleObjectList[i]) as TComponent;
      ProcessStyleObject(StyleObject);
    end;
   TRttiContext.Create.GetType(SeStyle.ClassType).GetMethod('ResetStyle').Invoke(SeStyle, []);

  end;
  FScaledStyles.Add(Style.Name);
  if Style = TStyleManager.ActiveStyle then
    RecreateForms;
end;
{$IFDEF VER330} // RAD Studio 10.3
  type
   TGetBorderSize = function: TRect of object;

   TFormStyleHookFix = class helper for TFormStyleHook
     procedure SetStretchedCaptionInc(Value : Integer);
     function GetBorderSizeAddr: Pointer;
     function Detour_GetBorderSize: TRect;
   end;

var
  Trampoline_TFormStyleHook_GetBorderSize : TGetBorderSize;
  Detour_TFormStyleHook_GetBorderSize : TGetBorderSize;


{ TFormStyleHookFix }

function TFormStyleHookFix.GetBorderSizeAddr: Pointer;
var
  MethodPtr: TGetBorderSize;
begin
  with Self do MethodPtr := GetBorderSize;
  Result := TMethod(MethodPtr).Code;
end;

procedure TFormStyleHookFix.SetStretchedCaptionInc(Value: Integer);
begin
  with Self do FStretchedCaptionInc := Value;
end;

function TFormStyleHookFix.Detour_GetBorderSize: TRect;
var
  MethodPtr: TGetBorderSize;
begin
  TMethod(MethodPtr).Code := TMethod(Trampoline_TFormStyleHook_GetBorderSize).Code;
  TMethod(MethodPtr).Data := Pointer(Self);
  Result := MethodPtr;
  Self.SetStretchedCaptionInc(0);
  if (Screen.PixelsPerInch > 96) then
    Result.Top := MulDiv(Result.Top, 96, Screen.PixelsPerInch);
end;

initialization
 Detour_TFormStyleHook_GetBorderSize := TFormStyleHook(nil).Detour_GetBorderSize;
 TMethod(Trampoline_TFormStyleHook_GetBorderSize).Code :=
   InterceptCreate(TFormStyleHook(nil).GetBorderSizeAddr,
   TMethod(Detour_TFormStyleHook_GetBorderSize).Code)
finalization
 InterceptRemove(TMethod(Trampoline_TFormStyleHook_GetBorderSize).Code);
{$ENDIF VER330}
end.
