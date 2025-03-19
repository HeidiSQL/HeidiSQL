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
  Forms, printer4lazarus,
  { you can add units after this }
  main, apphelpers, dbconnection, { gnugettext,}
  dbstructures, dbstructures.mysql, About, generic_types,
  dbstructures.interbase, dbstructures.mssql, dbstructures.postgresql,
  dbstructures.sqlite, change_password, loginform, data_sorting, extra_controls,
  column_selection, loaddata, csv_detector, createdatabase, editvar, copytable,
  exportgrid, usermanager, selectdbobject, reformatter, searchreplace,
  connections, jsonregistry {, printlist (EnablePrint not defined) }
  ;

{$R *.res}
{.$R resources.rc}

begin
  PostponedLogItems := TDBLogItems.Create(True);
  Application.MainFormOnTaskBar := True;

  // Use MySQL standard format for date/time variables: YYYY-MM-DD HH:MM:SS
  // Be aware that Delphi internally converts the slashes in ShortDateFormat to the DateSeparator
  DefaultFormatSettings.DateSeparator := '-';
  DefaultFormatSettings.TimeSeparator := ':';
  DefaultFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  DefaultFormatSettings.LongTimeFormat := 'hh:nn:ss';

  AppSettings := TAppSettings.Create;
  //SecondInstMsgId := RegisterWindowMessage(APPNAME);
  if false then begin // (not AppSettings.ReadBool(asAllowMultipleInstances)) and CheckForSecondInstance then begin
    AppSettings.Free;
    Application.Terminate;
  end else begin

    {AppLanguage := AppSettings.ReadString(asAppLanguage);
    // SysLanguage may be zh_CN, while we don't offer such a language, but anyway, this is just the current system language:
    SysLanguage := gnugettext.DefaultInstance.GetCurrentLocaleName;
    gnugettext.UseLanguage(AppLanguage);
    // First time translation via dxgettext.
    // Issue #3064: Ignore TFont, so "Default" on mainform for WinXP users does not get broken.
    gnugettext.TP_GlobalIgnoreClass(TFont);}

    // Enable padding in customized tooltips
    //HintWindowClass := TExtHintWindow;

    RequireDerivedFormResource:=True;
  Application.Scaled:=True;
    Application.Initialize;
    //Application.UpdateFormatSettings := False;

    // Try to set style name. If that fails, the user gets an error message box - reset it to default when that happened
    {WantedStyle := AppSettings.ReadString(asTheme);
    TStyleManager.TrySetStyle(WantedStyle);
    if TStyleManager.ActiveStyle.Name <> WantedStyle then begin
      AppSettings.WriteString(asTheme, TStyleManager.ActiveStyle.Name);
    end;}
    Application.CreateForm(TMainForm, MainForm);
    MainForm.AfterFormCreate;
    Application.OnDeactivate := MainForm.ApplicationDeActivate;
    Application.OnShowHint := MainForm.ApplicationShowHint;
    //Application.MainFormOnTaskBar := True;
    Application.Run;
  end;
end.

