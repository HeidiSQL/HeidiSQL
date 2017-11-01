unit Vcl.FormsFix;

interface

implementation

uses
  System.SysUtils, Winapi.Windows, Vcl.Forms, Vcl.Graphics, System.UITypes
  //
  , DDetours
  //
  ;

var
  trampoline_GetMetricSettings: Procedure = nil;

type
  TFontHelper = class helper for TFont
  public
    function Equals(const AOther: TFont): Boolean;
  end;

function TFontHelper.Equals(const AOther: TFont): Boolean;
begin
  Result := (AOther.PixelsPerInch = self.PixelsPerInch)
    and (AOther.Charset = self.Charset)
    and (AOther.Color = self.Color)
    and (AOther.Height = self.Height)
    and (AOther.Name = self.Name)
    and (AOther.Orientation = self.Orientation)
    and (AOther.Pitch = self.Pitch)
    and (AOther.Size = self.Size)
    and (AOther.Style = self.Style)
    and (AOther.Quality = self.Quality);
end;

type
  TScreenHelper =  class Helper for TScreen
  public
    function getPtr_GetMetricSettings:Pointer;
  end;

function TScreenHelper.getPtr_GetMetricSettings:Pointer;
begin
  result:=@TScreen.GetMetricSettings;
end;

procedure HookedGetMetricSettings(const Self);

  procedure CheckedFontChange(const ACurrFont: TFont; const ANewFont: tagLOGFONTW);
  var
    TmpFont: TFont;
  begin
    TmpFont := TFont.Create;
    try
      TmpFont.Assign(ACurrFont);
      TmpFont.Handle := CreateFontIndirect(ANewFont);
      if not TmpFont.Equals(ACurrFont) then
      begin
        ACurrFont.Handle := CreateFontIndirect(ANewFont);
      end;
    finally
      FreeAndNil(TmpFont);
    end;
  end;

var
  LSize: Cardinal;
  LogFont: TLogFont;
  NonClientMetrics: TNonClientMetrics;
  SaveShowHint: Boolean;

begin
  SaveShowHint := False;
  if Assigned(Application) then SaveShowHint := Application.ShowHint;
  try
    if Assigned(Application) then Application.ShowHint := False;
{$IF DEFINED(CLR)}
    LSize := Marshal.SizeOf(TypeOf(TLogFont));
{$ELSE}
    LSize := SizeOf(TLogFont);
{$IFEND}
    if SystemParametersInfo(SPI_GETICONTITLELOGFONT, LSize, {$IFNDEF CLR}@{$ENDIF}LogFont, 0) then
    begin
       CheckedFontChange(Screen.IconFont, LogFont);
    end
    else
      Screen.IconFont.Handle := GetStockObject(SYSTEM_FONT);
{$IF DEFINED(CLR)}
    LSize := Marshal.SizeOf(TypeOf(TNonClientMetrics));
{$ELSE}
    LSize := TNonClientMetrics.SizeOf;
{$IFEND}
    NonClientMetrics.cbSize := LSize;
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, {$IFNDEF CLR}@{$ENDIF}NonClientMetrics, 0) then
    begin
      CheckedFontChange(Screen.HintFont, NonClientMetrics.lfStatusFont);
      CheckedFontChange(Screen.MenuFont, NonClientMetrics.lfMenuFont);
      CheckedFontChange(Screen.MessageFont, NonClientMetrics.lfMessageFont);
      CheckedFontChange(Screen.CaptionFont, NonClientMetrics.lfCaptionFont);
    end else
    begin
      Screen.HintFont.Size := 8;
      Screen.MenuFont.Handle := GetStockObject(SYSTEM_FONT);
      Screen.MessageFont.Handle := GetStockObject(SYSTEM_FONT);
      Screen.CaptionFont.Handle := GetStockObject(SYSTEM_FONT);
    end;
    Screen.HintFont.Color := clInfoText;
    Screen.MenuFont.Color := clMenuText;
    Screen.MessageFont.Color := clWindowText;
  finally
    if Assigned(Application) then Application.ShowHint := SaveShowHint;
  end;

end;

initialization
  @trampoline_GetMetricSettings := InterceptCreate(Screen.getPtr_GetMetricSettings, @HookedGetMetricSettings);

finalization
  InterceptRemove(@trampoline_GetMetricSettings);

end.
