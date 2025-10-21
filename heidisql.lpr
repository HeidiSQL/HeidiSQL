program heidisql;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Dialogs,
  Forms, printer4lazarus, datetimectrls, LCLTranslator, Translations,
  { you can add units after this }
  main, apphelpers, dbconnection;

{$R *.res}

var
  AppLanguage: String;
begin
  PostponedLogItems := TDBLogItems.Create(True);
  Application.{%H-}MainFormOnTaskBar := True; // hide warning: Symbol "MainFormOnTaskBar" is not portable

  // Use MySQL standard format for date/time variables: YYYY-MM-DD HH:MM:SS
  // Be aware that Delphi internally converts the slashes in ShortDateFormat to the DateSeparator
  DefaultFormatSettings.DateSeparator := '-';
  DefaultFormatSettings.TimeSeparator := ':';
  DefaultFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  DefaultFormatSettings.LongTimeFormat := 'hh:nn:ss';
  // Testing issue #2189:
  // DefaultFormatSettings.ThousandSeparator:= chr(160);

  AppSettings := TAppSettings.Create;

  AppLanguage := AppSettings.ReadString(asAppLanguage);
  // SysLanguage may be zh_CN, while we don't offer such a language, but anyway, this is just the current system language:
  SysLanguage := GetLanguageID.LanguageCode;
  LCLTranslator.SetDefaultLang(AppLanguage);
  InitMoFile(AppLanguage);

  // Enable padding in customized tooltips
  //HintWindowClass := TExtHintWindow;

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  //Application.UpdateFormatSettings := False;

  Application.CreateForm(TMainForm, MainForm);
  MainForm.AfterFormCreate;
  Application.OnDeactivate := MainForm.ApplicationDeActivate;
  Application.OnShowHint := MainForm.ApplicationShowHint;
  //Application.MainFormOnTaskBar := True;
  Application.Run;
end.

