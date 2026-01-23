program heidisql;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX} cthreads, {$ENDIF}
  {$IFDEF DARWIN} iosxlocale, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  uMetaDarkStyle, uDarkStyleParams, uDarkStyleSchemes,
  SysUtils, Dialogs,
  Forms, printer4lazarus, datetimectrls, LCLTranslator, Translations,
  { you can add units after this }
  main, apphelpers, dbconnection;

{$R *.res}

var
  AppLanguage: String;
  WasDarkMode: Boolean;
begin
  PostponedLogItems := TDBLogItems.Create(True);
  Application.{%H-}MainFormOnTaskBar := True; // hide warning: Symbol "MainFormOnTaskBar" is not portable

  // Use MySQL standard format for date/time variables: YYYY-MM-DD HH:MM:SS
  // Be aware that Delphi internally converts the slashes in ShortDateFormat to the DateSeparator
  Application.{%H-}UpdateFormatSettings := False;
  DefaultFormatSettings.DateSeparator := '-';
  DefaultFormatSettings.TimeSeparator := ':';
  DefaultFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  DefaultFormatSettings.LongTimeFormat := 'hh:nn:ss';

  // Issue #2189 and #2325:
  // Auto-replace French and Russian non-breaking white space, broken through the Char type
  // DefaultFormatSettings.ThousandSeparator:= #160;
  if DefaultFormatSettings.ThousandSeparator in [#226, #160] then
    DefaultFormatSettings.ThousandSeparator := ' ';

  // Issue #2366:
  // https://wiki.freepascal.org/Locale_settings_for_macOS
  // On macOS, initializing locale settings through iosxlocale mostly does not work, so we do it hardcoded here:
  if DefaultFormatSettings.DecimalSeparator = #0 then
    DefaultFormatSettings.DecimalSeparator  := '.';
  if DefaultFormatSettings.ThousandSeparator = #0 then
    DefaultFormatSettings.ThousandSeparator := ' ';

  AppSettings := TAppSettings.Create;

  AppLanguage := AppSettings.ReadString(asAppLanguage);
  // SysLanguage may be zh_CN, while we don't offer such a language, but anyway, this is just the current system language:
  SysLanguage := GetLanguageID.LanguageCode;
  LCLTranslator.SetDefaultLang(AppLanguage, '', GetApplicationName);
  InitMoFile(AppLanguage);

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;

  {$IFDEF WINDOWS}
  PreferredAppMode := pamAllowDark;
  case AppSettings.ReadInt(asThemeMode) of
    // 0: dark mode based on Windows setting
    0: if AppSettings.IsWindowsDarkApps then PreferredAppMode := pamForceDark;
    // 1: light, no change required, leave pamAllowDark
    2: PreferredAppMode := pamForceDark;
  end;
  if PreferredAppMode = pamForceDark then
    uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);

  WasDarkMode := AppSettings.ReadBool(asCurrentThemeIsDark);
  if (not WasDarkMode) and IsDarkModeEnabled then begin
    // todo: switch synedit colors and grid colors after current dark mode has changed
    //       probably store a CurrentThemeIsDark=True/False in settings, then check here if that now changes and flip colors if so?
  end
  else if WasDarkMode and (not IsDarkModeEnabled) then begin
    // dito
  end;
  AppSettings.WriteBool(asCurrentThemeIsDark, IsDarkModeEnabled);
  {$ENDIF}

  Application.Initialize;

  Application.CreateForm(TMainForm, MainForm);
  Application.OnException := MainForm.ApplicationException;
  MainForm.AfterFormCreate;
  Application.OnDeactivate := MainForm.ApplicationDeActivate;
  Application.OnIdle := MainForm.ApplicationIdle;
  Application.OnShortcut := MainForm.ApplicationShortCut;
  Application.OnShowHint := MainForm.ApplicationShowHint;
  Application.Run;
end.

