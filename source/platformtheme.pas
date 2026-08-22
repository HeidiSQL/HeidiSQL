unit platformtheme;

{$mode delphi}{$H+}

{$IFDEF LINUX}
  {$if defined(LCLQt5) or defined(LCLQt6)}
    {$DEFINE HEIDI_LINUX_QT_THEME}
  {$endif}
{$ENDIF}

interface

procedure ApplyPlatformTheme(ThemeMode: Integer);
function PlatformThemeIsDark(ThemeMode: Integer): Boolean;

implementation

{$IFDEF HEIDI_LINUX_QT_THEME}
uses
  {$IFDEF LCLQt5}
  qt5;
  {$ELSE}
  qt6;
  {$ENDIF}

procedure SetPaletteRole(Palette: QPaletteH; var Color: TQColor;
  Role: QPaletteColorRole; R, G, B: Integer);
begin
  QColor_fromRgb(@Color, R, G, B, 255);
  QPalette_setColor(Palette, Role, @Color);
end;

procedure SetPaletteGroupRole(Palette: QPaletteH; var Color: TQColor;
  Group: QPaletteColorGroup; Role: QPaletteColorRole; R, G, B: Integer);
begin
  QColor_fromRgb(@Color, R, G, B, 255);
  QPalette_setColor(Palette, Group, Role, @Color);
end;

procedure ApplyLightPalette(Palette: QPaletteH; var Color: TQColor);
begin
  SetPaletteRole(Palette, Color, QPaletteWindow,         239, 240, 241);
  SetPaletteRole(Palette, Color, QPaletteWindowText,      35,  38,  41);
  SetPaletteRole(Palette, Color, QPaletteBase,           255, 255, 255);
  SetPaletteRole(Palette, Color, QPaletteAlternateBase,  247, 247, 247);
  SetPaletteRole(Palette, Color, QPaletteText,             35,  38,  41);
  SetPaletteRole(Palette, Color, QPaletteButton,         239, 240, 241);
  SetPaletteRole(Palette, Color, QPaletteButtonText,       35,  38,  41);
  SetPaletteRole(Palette, Color, QPaletteToolTipBase,    255, 255, 220);
  SetPaletteRole(Palette, Color, QPaletteToolTipText,      35,  38,  41);
  SetPaletteRole(Palette, Color, QPaletteBrightText,      255, 255, 255);
  SetPaletteRole(Palette, Color, QPaletteLight,          255, 255, 255);
  SetPaletteRole(Palette, Color, QPaletteMidlight,       247, 247, 247);
  SetPaletteRole(Palette, Color, QPaletteMid,            200, 200, 200);
  SetPaletteRole(Palette, Color, QPaletteDark,           160, 160, 160);
  SetPaletteRole(Palette, Color, QPaletteShadow,         118, 118, 118);

  SetPaletteGroupRole(Palette, Color, QPaletteDisabled, QPaletteWindowText, 120, 120, 120);
  SetPaletteGroupRole(Palette, Color, QPaletteDisabled, QPaletteText,       120, 120, 120);
  SetPaletteGroupRole(Palette, Color, QPaletteDisabled, QPaletteButtonText, 120, 120, 120);
end;

procedure ApplyDarkPalette(Palette: QPaletteH; var Color: TQColor);
begin
  SetPaletteRole(Palette, Color, QPaletteWindow,          49,  54,  59);
  SetPaletteRole(Palette, Color, QPaletteWindowText,     239, 240, 241);
  SetPaletteRole(Palette, Color, QPaletteBase,            35,  38,  41);
  SetPaletteRole(Palette, Color, QPaletteAlternateBase,   43,  48,  53);
  SetPaletteRole(Palette, Color, QPaletteText,           239, 240, 241);
  SetPaletteRole(Palette, Color, QPaletteButton,          49,  54,  59);
  SetPaletteRole(Palette, Color, QPaletteButtonText,     239, 240, 241);
  SetPaletteRole(Palette, Color, QPaletteToolTipBase,     49,  54,  59);
  SetPaletteRole(Palette, Color, QPaletteToolTipText,    239, 240, 241);
  SetPaletteRole(Palette, Color, QPaletteBrightText,      255, 255, 255);
  SetPaletteRole(Palette, Color, QPaletteLight,           91,  98, 104);
  SetPaletteRole(Palette, Color, QPaletteMidlight,        69,  75,  80);
  SetPaletteRole(Palette, Color, QPaletteMid,             37,  41,  44);
  SetPaletteRole(Palette, Color, QPaletteDark,            27,  30,  32);
  SetPaletteRole(Palette, Color, QPaletteShadow,          16,  18,  20);

  SetPaletteGroupRole(Palette, Color, QPaletteDisabled, QPaletteWindowText, 130, 133, 136);
  SetPaletteGroupRole(Palette, Color, QPaletteDisabled, QPaletteText,       130, 133, 136);
  SetPaletteGroupRole(Palette, Color, QPaletteDisabled, QPaletteButtonText, 130, 133, 136);
end;

procedure ApplyPlatformTheme(ThemeMode: Integer);
var
  Palette: QPaletteH;
  Color: TQColor;
begin
  if not (ThemeMode in [1, 2]) then
    Exit;

  Palette := QPalette_Create;
  Color := Default(TQColor);
  try
    // Keep the current Qt palette as the base so accent and selection colors stay intact.
    QApplication_palette(Palette, QWidgetH(nil));
    if ThemeMode = 1 then
      ApplyLightPalette(Palette, Color)
    else
      ApplyDarkPalette(Palette, Color);
    QApplication_setPalette(Palette, nil);
  finally
    QPalette_Destroy(Palette);
  end;
end;

function PlatformThemeIsDark(ThemeMode: Integer): Boolean;
var
  Palette: QPaletteH;
  Color: PQColor;
  R, G, B: Integer;
begin
  case ThemeMode of
    1: Exit(False);
    2: Exit(True);
  end;

  Palette := QPalette_Create;
  try
    QApplication_palette(Palette, QWidgetH(nil));
    Color := QPalette_color(Palette, QPaletteActive, QPaletteWindow);
    QColor_getRgb(QColorH(Color), @R, @G, @B);
    Result := (R * 299 + G * 587 + B * 114) < 128000;
  finally
    QPalette_Destroy(Palette);
  end;
end;

{$ELSE}

procedure ApplyPlatformTheme(ThemeMode: Integer);
begin
end;

function PlatformThemeIsDark(ThemeMode: Integer): Boolean;
begin
  Result := False;
end;

{$ENDIF}

end.
