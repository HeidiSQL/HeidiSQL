{
 Modified 20-Mar-2019 by Rickard Johansson (www.rj-texted.se).
 Purpose: Add per-monitor DPI awareness
 Usage: Add the unit to the interface uses statement of the main form and add code below:

 TMyForm = class(TForm)
 private
   FStyleDPIAwareness : TStyleDPIAwareness;

 procedure TFrmMain.FormCreate(Sender: TObject);
 begin
   FStyleDPIAwareness := TStyleDPIAwareness.Create(Self);
   FStyleDPIAwareness.Parent := Self;

 procedure TFrmMain.FormDestroy(Sender: TObject);
 begin
   FStyleDPIAwareness.Free;

 procedure TFrmMain.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
 begin
   FStyleDPIAwareness.AfterDPIChange(OldDPI, NewDPI);
 end;

 procedure TFrmMain.FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
 begin
   FStyleDPIAwareness.BeforeDPIChange(OldDPI, NewDPI);
 end;
}

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
  TStyleDPI = class(TObject)
  private
    FCurrentDPI: Integer;
  public
    property CurrentDPI: Integer read FCurrentDPI write FCurrentDPI;
  end;

  TStyleDPIAwareness = class(TControl)
  private
    FScaledStyles : TStringList;
    FRoundScalingFactor : Boolean;
    FUseCustomScalingFactor : Boolean;
    FCustomPPI : integer;
    FOldDPI: Integer;
   protected
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure RecreateForms;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterDPIChange(OldDPI, NewDPI: Integer);
    procedure BeforeDPIChange(OldDPI, NewDPI: Integer);
    procedure ScaleStyle(Style : TCustomStyleServices);
    property OldDPI: Integer read FOldDPI write FOldDPI;
  published
    property  RoundScalingFactor : Boolean read FRoundScalingFactor
      write FRoundScalingFactor default True;
    property  UseCustomScalingFactor : Boolean read FUseCustomScalingFactor
      write FUseCustomScalingFactor default False;
    property CustomPPI : integer read FCustomPPI write FCustomPPI default 96;
  end;

implementation

Uses
  System.Rtti, DDetours, System.Math;

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
  FCustomPPI := 96;
  FOldDPI := 96;

  FScaledStyles := TStringList.Create;
  FScaledStyles.Sorted := False;

  ScaleStyle(TStyleManager.ActiveStyle);
end;

destructor TStyleDPIAwareness.Destroy;
var
  i : Integer;
begin
  for i := 0 to FScaledStyles.Count - 1 do
    TStyleDPI(FScaledStyles.Objects[i]).Free;
  FScaledStyles.Free;
  inherited;
end;

procedure TStyleDPIAwareness.AfterDPIChange(OldDPI, NewDPI: Integer);
begin
  ScaleStyle(TStyleManager.ActiveStyle);
end;

procedure TStyleDPIAwareness.BeforeDPIChange(OldDPI, NewDPI: Integer);
begin
  FOldDPI := OldDPI;
end;

procedure TStyleDPIAwareness.RecreateForms;
Var
  i : Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] <> TForm(Owner) then
      Screen.Forms[i].Perform(CM_RECREATEWND, 0, 0);
  end;
end;

procedure TStyleDPIAwareness.ScaleStyle(Style: TCustomStyleServices);
Var
  NewDPI : integer;
  SeStyle : TObject;
  SeStyleSource : TObject;
  BitmapList : TList;
  BitMap : TBitmap;
  StyleObjectList : Tlist;
  i,n: integer;
  StyleObject : TComponent;
  obj: TStyleDPI;

  procedure ProcessBitmapLink(BL : TObject);
  Var
    BLType : TRTTIType;
  begin
    BLType := TRttiContext.Create.GetType(BL.ClassType);
    BLType.GetProperty('Bottom').SetValue(BL,  Round((BLType.GetProperty('Bottom').GetValue(BL).AsInteger * NewDPI - 1) / OldDPI));
    BLType.GetProperty('Right').SetValue(BL,  Round(BLType.GetProperty('Right').GetValue(BL).AsInteger * NewDPI / OldDPI));
    BLType.GetProperty('Left').SetValue(BL,  Round(BLType.GetProperty('Left').GetValue(BL).AsInteger * NewDPI / OldDPI));
    BLType.GetProperty('Top').SetValue(BL,  Round(BLType.GetProperty('Top').GetValue(BL).AsInteger * NewDPI / OldDPI));
  end;

  procedure ProcessSO(aSO : TComponent;  aSOType : TRTTIType);
  begin
    aSOType.GetProperty('Top').SetValue(aSO,  Round(aSOType.GetProperty('Top').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('Left').SetValue(aSO,  Round(aSOType.GetProperty('Left').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('Width').SetValue(aSO,  Round(aSOType.GetProperty('Width').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('Height').SetValue(aSO,  Round(aSOType.GetProperty('Height').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('MarginTop').SetValue(aSO,  Round(aSOType.GetProperty('MarginTop').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('MarginLeft').SetValue(aSO,  Round(aSOType.GetProperty('MarginLeft').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('MarginBottom').SetValue(aSO,  Round(aSOType.GetProperty('MarginBottom').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('MarginRight').SetValue(aSO,  Round(aSOType.GetProperty('MarginRight').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('TextMarginTop').SetValue(aSO,  Round(aSOType.GetProperty('TextMarginTop').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('TextMarginLeft').SetValue(aSO,  Round(aSOType.GetProperty('TextMarginLeft').GetValue(aSO).AsInteger * NewDPI / OldDPI));
    aSOType.GetProperty('TextMarginRight').SetValue(aSO,  Round(aSOType.GetProperty('TextMarginRight').GetValue(aSO).AsInteger * NewDPI / OldDPI));
  end;

  procedure ProcessStyleObject(SO : TComponent);
  var
    i: integer;
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
        SOType.GetProperty('Width').SetValue(SO,  MulDiv(28, NewDPI, OldDPI));
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
  n := FScaledStyles.IndexOf(TStyleManager.ActiveStyle.Name);
  if n >= 0 then
  begin
    obj := TStyleDPI(FScaledStyles.Objects[n]);
    OldDPI := obj.FCurrentDPI;
  end;

  if UseCustomScalingFactor then
    NewDPI := CustomPPI
  else
    NewDPI := TForm(Owner).Monitor.PixelsPerInch;

  if (Style = TStyleManager.SystemStyle) then
    Exit;

  SeStyle := TRttiContext.Create.GetType(Style.ClassType).GetField('FSource').GetValue(Style).AsObject;
  SeStyleSource := TRttiContext.Create.GetType(SeStyle.ClassType).GetField('FCleanCopy').GetValue(SeStyle).AsObject;
  BitMapList := TRttiContext.Create.GetType(SeStyleSource.ClassType).GetField('FBitmaps').GetValue(SeStyleSource).AsObject as TList;

  if BitMapList.Count = 1 then
  begin
    Bitmap := TObject(BitmapList[0]) as TBitmap;
    ResizeBitmap(Bitmap, Round(Bitmap.Width * NewDPI / OldDPI), Round(Bitmap.Height * NewDPI / OldDPI));

    StyleObjectList := TRttiContext.Create.GetType(SeStyleSource.ClassType).GetField('FObjects').GetValue(SeStyleSource).AsObject as TList;
    for i := 0 to StyleObjectList.Count -1 do begin
      StyleObject := TObject(StyleObjectList[i]) as TComponent;
      ProcessStyleObject(StyleObject);
    end;
   TRttiContext.Create.GetType(SeStyle.ClassType).GetMethod('ResetStyle').Invoke(SeStyle, []);

  end;

  n := FScaledStyles.IndexOf(Style.Name);
  if n >= 0 then
  begin
    obj := TStyleDPI(FScaledStyles.Objects[n]);
    obj.FCurrentDPI := NewDPI;
  end
  else
  begin
    obj := TStyleDPI.Create;
    obj.FCurrentDPI := NewDPI;
    FScaledStyles.AddObject(Style.Name, obj);
  end;

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
  Self.SetStretchedCaptionInc(1);
  if (Form.Monitor.PixelsPerInch > 96) then
    Result.Top := MulDiv(Result.Top, 96, Form.Monitor.PixelsPerInch);
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
