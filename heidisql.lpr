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
  SysUtils,
  Forms, printer4lazarus, datetimectrls, LCLTranslator, Translations,
  { you can add units after this }
  main, apphelpers, dbconnection, { gnugettext,}
  dbstructures, dbstructures.mysql, About, generic_types,
  dbstructures.interbase, dbstructures.mssql, dbstructures.postgresql,
  dbstructures.sqlite, change_password, loginform, data_sorting, extra_controls,
  column_selection, loaddata, csv_detector, createdatabase, editvar, copytable,
  exportgrid, usermanager, selectdbobject, reformatter, searchreplace,
  connections, jsonregistry, sqlhelp, updatecheck, insertfiles, texteditor,
  customize_highlighter, preferences, table_editor, view, routine_editor,
  trigger_editor, event_editor, tabletools, bineditor, grideditlinks,
  lazaruscompat, crashdialog;

{$R *.res}
{.$R resources.rc}

var
  AppLanguage: String;
begin
  PostponedLogItems := TDBLogItems.Create(True);
  Application.MainFormOnTaskBar := True;

  // Use MySQL standard format for date/time variables: YYYY-MM-DD HH:MM:SS
  // Be aware that Delphi internally converts the slashes in ShortDateFormat to the DateSeparator
  DefaultFormatSettings.DateSeparator := '-';
  DefaultFormatSettings.TimeSeparator := ':';
  DefaultFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  DefaultFormatSettings.LongTimeFormat := 'hh:nn:ss';
  // Testing issue #2189: DefaultFormatSettings.ThousandSeparator:= chr(160);

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

