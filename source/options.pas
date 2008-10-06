unit options;


// -------------------------------------
// Preferences
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Registry, ExtCtrls;

type
  Toptionsform = class(TForm)
    pagecontrolMain: TPageControl;
    tabMisc: TTabSheet;
    btnCancel: TButton;
    btnOK: TButton;
    btnApply: TButton;
    tabSQL: TTabSheet;
    chkAutoReconnect: TCheckBox;
    pagecontrolSQL: TPageControl;
    tabSQLfont: TTabSheet;
    tabSQLcolors: TTabSheet;
    pnlSQLFontPattern: TPanel;
    lblSQLFontSizeHint: TLabel;
    lblSQLFontSize: TLabel;
    lblSQLFontName: TLabel;
    comboSQLFontName: TComboBox;
    lblLogLinesHint: TLabel;
    lblSQLColKeywords: TLabel;
    pnlSQLColKeywords: TPanel;
    coldlgSQLColors: TColorDialog;
    lblSQLColFunctions: TLabel;
    pnlSQLColFunctions: TPanel;
    lblSQLColNumeric: TLabel;
    pnlSQLColDatatypes: TPanel;
    lblSQLColDatatypes: TLabel;
    pnlSQLColNumeric: TPanel;
    lblSQLColString: TLabel;
    pnlSQLColString: TPanel;
    lblSQLColComments: TLabel;
    pnlSQLColComments: TPanel;
    tabCSV: TTabSheet;
    grpCSV: TGroupBox;
    lblCSVSeparator: TLabel;
    editCSVSeparator: TEdit;
    editCSVEncloser: TEdit;
    lblCSVTerminator: TLabel;
    lblCSVEncloser: TLabel;
    editCSVTerminator: TEdit;
    lblCSVHintCR: TLabel;
    lblCSVHintLF: TLabel;
    lblCSVHintTAB: TLabel;
    lblCSVHintEscaped: TLabel;
    tabData: TTabSheet;
    lblDataFont: TLabel;
    comboDataFontName: TComboBox;
    editDataFontSize: TEdit;
    updownDataFontSize: TUpDown;
    lblSQLFontPattern: TLabel;
    lblDataFontHint: TLabel;
    editSQLFontSize: TEdit;
    updownSQLFontSize: TUpDown;
    lblMaxColWidth: TLabel;
    lblSQLColTablenames: TLabel;
    pnlSQLColTablenames: TPanel;
    updownLogLines: TUpDown;
    editLogLines: TEdit;
    editMaxColWidth: TEdit;
    updownMaxColWidth: TUpDown;
    chkRestoreLastDB: TCheckBox;
    chkRememberFilters: TCheckBox;
    chkLogToFile: TCheckBox;
    btnOpenLogFolder: TButton;
    lblSQLColActiveLine: TLabel;
    pnlSQLColActiveLine: TPanel;
    lblLogSnip: TLabel;
    editLogSnip: TEdit;
    updownLogSnip: TUpDown;
    lblLogSnipHint: TLabel;
    chkUpdatecheck: TCheckBox;
    editUpdatecheckInterval: TEdit;
    updownUpdatecheckInterval: TUpDown;
    chkPreferShowTables: TCheckBox;
    chkUpdateCheckBuilds: TCheckBox;
    grpFieldLayout: TGroupBox;
    lblFieldDatetime: TLabel;
    cboxText: TColorBox;
    chkEditorText: TCheckBox;
    lblFieldText: TLabel;
    lblFieldBinary: TLabel;
    lblFieldNumeric: TLabel;
    lblFieldEnum: TLabel;
    cboxBinary: TColorBox;
    cboxDatetime: TColorBox;
    cboxNumeric: TColorBox;
    cboxEnum: TColorBox;
    chkEditorBinary: TCheckBox;
    chkEditorDatetime: TCheckBox;
    chkEditorEnum: TCheckBox;
    lblFieldSet: TLabel;
    cboxSet: TColorBox;
    chkEditorSet: TCheckBox;
    chkNullBG: TCheckBox;
    lblFieldNull: TLabel;
    cboxNullBG: TColorBox;
    procedure FormShow(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure Apply(Sender: TObject);
    procedure FontsChange(Sender: TObject);
    procedure CallColorDialog(Sender: TObject);
    procedure DataFontsChange(Sender: TObject);
    procedure anyUpDownLimitChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure btnOpenLogFolderClick(Sender: TObject);
    procedure chkUpdatecheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkNullBGClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation
uses childwin, main, helpers;
{$R *.DFM}


procedure Toptionsform.Modified(Sender: TObject);
begin
  // Modified
  btnOK.Enabled := True;
  btnApply.Enabled := True;
end;


{**
  Apply settings to registry, childwin and mainform
}
procedure Toptionsform.Apply(Sender: TObject);
var
  cwin : TMDIChild;
  reg  : TRegistry;
  ServerKeys, ValueNames: TStringList;
  i, j: Integer;
begin
  Screen.Cursor := crHourGlass;

  // Open registry key
  reg := TRegistry.Create;
  reg.OpenKey(REGPATH, true);

  // Save values
  reg.WriteBool(REGNAME_AUTORECONNECT, chkAutoReconnect.Checked);
  reg.WriteBool(REGNAME_RESTORELASTUSEDDB, chkRestoreLastDB.Checked);
  reg.WriteString(REGNAME_FONTNAME, comboSQLFontName.Text);
  reg.WriteInteger(REGNAME_FONTSIZE, updownSQLFontSize.Position);
  reg.WriteInteger(REGNAME_LOGSQLNUM, updownLogLines.Position);
  reg.WriteInteger(REGNAME_LOGSQLWIDTH, updownLogSnip.Position);
  reg.WriteString(REGNAME_SQLCOLKEYATTRI, colortostring(pnlSQLColKeywords.Color));
  reg.WriteString(REGNAME_SQLCOLFUNCTIONATTRI, colortostring(pnlSQLColFunctions.Color));
  reg.WriteString(REGNAME_SQLCOLDATATYPEATTRI, colortostring(pnlSQLColDatatypes.Color));
  reg.WriteString(REGNAME_SQLCOLNUMBERATTRI, colortostring(pnlSQLColNumeric.Color));
  reg.WriteString(REGNAME_SQLCOLSTRINGATTRI, colortostring(pnlSQLColString.Color));
  reg.WriteString(REGNAME_SQLCOLCOMMENTATTRI, colortostring(pnlSQLColComments.Color));
  reg.WriteString(REGNAME_SQLCOLTABLENAMEATTRI, colortostring(pnlSQLColTablenames.Color));
  reg.WriteString(REGNAME_SQLCOLACTIVELINE, ColorToString(pnlSQLColActiveLine.Color));
  reg.WriteString(REGNAME_CSV_SEPARATOR, editCSVSeparator.Text);
  reg.WriteString(REGNAME_CSV_ENCLOSER, editCSVEncloser.Text);
  reg.WriteString(REGNAME_CSV_TERMINATOR, editCSVTerminator.Text);
  reg.WriteInteger(REGNAME_MAXCOLWIDTH, updownMaxColWidth.Position);
  reg.WriteString(REGNAME_DATAFONTNAME, comboDataFontName.Text);
  reg.WriteInteger(REGNAME_DATAFONTSIZE, updownDataFontSize.Position);
  reg.WriteBool(REGNAME_REMEMBERFILTERS, chkRememberFilters.Checked);
  reg.WriteBool(REGNAME_LOGTOFILE, chkLogToFile.Checked);
  reg.WriteBool(REGNAME_DO_UPDATECHECK, chkUpdatecheck.Checked);
  reg.WriteBool(REGNAME_DO_UPDATECHECK_BUILDS, chkUpdatecheckBuilds.Checked);
  reg.WriteInteger(REGNAME_UPDATECHECK_INTERVAL, updownUpdatecheckInterval.Position);
  reg.WriteBool(REGNAME_PREFER_SHOWTABLES, chkPreferShowTables.Checked);
  // Save color settings
  reg.WriteInteger(REGNAME_FIELDCOLOR_NUMERIC, cboxNumeric.Selected);
  reg.WriteInteger(REGNAME_FIELDCOLOR_TEXT, cboxText.Selected);
  reg.WriteInteger(REGNAME_FIELDCOLOR_BINARY, cboxBinary.Selected);
  reg.WriteInteger(REGNAME_FIELDCOLOR_DATETIME, cboxDatetime.Selected);
  reg.WriteInteger(REGNAME_FIELDCOLOR_ENUM, cboxEnum.Selected);
  reg.WriteInteger(REGNAME_FIELDCOLOR_SET, cboxSet.Selected);
  reg.WriteInteger(REGNAME_BG_NULL, cboxNullBg.Selected);
  // Editor enablings
  reg.WriteBool(REGNAME_FIELDEDITOR_TEXT, chkEditorText.Checked);
  reg.WriteBool(REGNAME_FIELDEDITOR_BINARY, chkEditorBinary.Checked);
  reg.WriteBool(REGNAME_FIELDEDITOR_DATETIME, chkEditorDatetime.Checked);
  reg.WriteBool(REGNAME_FIELDEDITOR_ENUM, chkEditorEnum.Checked);
  reg.WriteBool(REGNAME_FIELDEDITOR_SET, chkEditorSet.Checked);
  reg.WriteBool(REGNAME_BG_NULL_ENABLED, chkNullBg.Checked);

  // Clean registry from unwanted WHERE clauses if "Remember WHERE filters" was unchecked
  if not chkRememberFilters.Checked then begin
    reg.OpenKey(REGKEY_SESSIONS, True);
    ServerKeys := TStringList.Create;
    reg.GetKeyNames(ServerKeys);
    for i := 0 to ServerKeys.Count - 1 do begin
      reg.OpenKey(REGPATH + REGKEY_SESSIONS + ServerKeys[i], True);
      ValueNames := TStringList.Create;
      reg.GetValueNames(ValueNames);
      for j := 0 to ValueNames.Count - 1 do begin
        if Pos(REGPREFIX_WHERECLAUSE, ValueNames[j]) = 1 then
          reg.DeleteValue(ValueNames[j]);
      end;
      ValueNames.Free;
    end;
    ServerKeys.Free;
  end;

  // Close registry key
  reg.CloseKey;
  reg.Free;

  // Set relevant properties in childwin
  cwin := Mainform.Childwin;
  if cwin <> nil then
  begin
    cwin.SynMemoQuery.Font := pnlSQLFontPattern.Font;
    cwin.SynMemoSQLLog.Font := pnlSQLFontPattern.Font;
    cwin.SynMemoProcessView.Font := pnlSQLFontPattern.Font;
    cwin.SynMemoFilter.Font := pnlSQLFontPattern.Font;
    cwin.SynSQLSyn1.KeyAttri.Foreground := pnlSQLColKeywords.Color;
    cwin.SynSQLSyn1.FunctionAttri.Foreground := pnlSQLColFunctions.Color;
    cwin.SynSQLSyn1.DataTypeAttri.Foreground := pnlSQLColDatatypes.Color;
    cwin.SynSQLSyn1.NumberAttri.Foreground := pnlSQLColNumeric.Color;
    cwin.SynSQLSyn1.StringAttri.Foreground := pnlSQLColString.Color;
    cwin.SynSQLSyn1.CommentAttri.Foreground := pnlSQLColComments.Color;
    cwin.SynSQLSyn1.TablenameAttri.Foreground := pnlSQLColTablenames.Color;
    cwin.SynMemoQuery.ActiveLineColor := pnlSQLColActiveLine.Color;
    cwin.DataGrid.Font.Name := comboDataFontName.Text;
    cwin.QueryGrid.Font.Name := comboDataFontName.Text;
    cwin.DataGrid.Font.Size := updownDataFontSize.Position;
    cwin.QueryGrid.Font.Size := updownDataFontSize.Position;
    FixVT(cwin.QueryGrid);
    FixVT(cwin.DataGrid);
    cwin.prefRememberFilters := chkRememberFilters.Checked;
    cwin.prefLogsqlnum := updownLogLines.Position;
    cwin.prefLogSqlWidth := updownLogSnip.Position;
    cwin.TrimSQLLog;
    if chkLogToFile.Checked then
      cwin.ActivateFileLogging
    else if cwin.prefLogToFile then
      cwin.DeactivateFileLogging;
    btnOpenLogFolder.Enabled := DirectoryExists(DirnameSessionLogs);
    cwin.prefMaxColWidth := updownMaxColWidth.Position;
    cwin.prefCSVSeparator := editCSVSeparator.Text;
    cwin.prefCSVEncloser := editCSVEncloser.Text;
    cwin.prefCSVTerminator := editCSVTerminator.Text;
    cwin.prefPreferShowTables := chkPreferShowTables.Checked;
    cwin.prefFieldColorNumeric := cboxNumeric.Selected;
    cwin.prefFieldColorText := cboxText.Selected;
    cwin.prefFieldColorBinary := cboxBinary.Selected;
    cwin.prefFieldColorDatetime := cboxDatetime.Selected;
    cwin.prefFieldColorEnum := cboxEnum.Selected;
    cwin.prefFieldColorSet := cboxSet.Selected;
    cwin.prefNullBG := cboxNullBg.Selected;
    cwin.CalcNullColors;
    cwin.DataGrid.Repaint;
    cwin.QueryGrid.Repaint;
    cwin.prefEnableTextEditor := chkEditorText.Checked;
    cwin.prefEnableBinaryEditor := chkEditorBinary.Checked;
    cwin.prefEnableDatetimeEditor := chkEditorDatetime.Checked;
    cwin.prefEnableEnumEditor := chkEditorEnum.Checked;
    cwin.prefEnableSetEditor := chkEditorSet.Checked;
    cwin.prefEnableNullBG := chkNullBg.Checked;
  end;

  // Settings have been applied, send a signal to the user
  btnOK.Enabled := False;
  btnApply.Enabled := False;
  Screen.Cursor := crDefault;
end;



procedure Toptionsform.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
end;

procedure Toptionsform.FormShow(Sender: TObject);

// ----------- Callback.Funktion for Fixed_Pitch -----------------//
function EnumFixedProc(lpelf: PEnumLogFont;
                       lpntm: PNewTextMetric;
                       FontType: Integer;
                       Data: LPARAM)  // Strings-Objekt
                       : Integer;     // 0 = Cancel
                       stdcall;       // Important for all API-Callbacks
begin
  Result := 1;  // don't cancel
  if (lpelf^.elfLogFont.lfPitchAndFamily and FIXED_PITCH) <> 0 then
    (TStrings(Data)).Add(String(lpelf^.elfLogFont.lfFaceName));
end;

var
  sqlfontname : String;
  sqlfontsize : Integer;
  datafontname : String;
  datafontsize : Integer;
begin
  screen.Cursor := crHourGlass;

  // Read and display values
  sqlfontname := Mainform.GetRegValue(REGNAME_FONTNAME, DEFAULT_FONTNAME);
  sqlfontsize := Mainform.GetRegValue(REGNAME_FONTSIZE, DEFAULT_FONTSIZE);
  datafontname := Mainform.GetRegValue(REGNAME_DATAFONTNAME, DEFAULT_DATAFONTNAME);
  datafontsize := Mainform.GetRegValue(REGNAME_DATAFONTSIZE, DEFAULT_DATAFONTSIZE);
  chkAutoReconnect.Checked := Mainform.GetRegValue(REGNAME_AUTORECONNECT, DEFAULT_AUTORECONNECT);
  chkRestoreLastDB.Checked := Mainform.GetRegValue(REGNAME_RESTORELASTUSEDDB, DEFAULT_RESTORELASTUSEDDB);
  updownLogLines.Position := Mainform.GetRegValue(REGNAME_LOGSQLNUM, DEFAULT_LOGSQLNUM);
  updownLogSnip.Position := Mainform.GetRegValue(REGNAME_LOGSQLWIDTH, DEFAULT_LOGSQLWIDTH);
  chkUpdatecheck.Checked := Mainform.GetRegValue(REGNAME_DO_UPDATECHECK, DEFAULT_DO_UPDATECHECK);
  chkUpdatecheckBuilds.Checked := Mainform.GetRegValue(REGNAME_DO_UPDATECHECK_BUILDS, DEFAULT_DO_UPDATECHECK_BUILDS);
  updownUpdatecheckInterval.Position := Mainform.GetRegValue(REGNAME_UPDATECHECK_INTERVAL, DEFAULT_UPDATECHECK_INTERVAL);
  chkUpdatecheckClick(Sender);
  chkPreferShowTables.Checked := Mainform.GetRegValue(REGNAME_PREFER_SHOWTABLES, DEFAULT_PREFER_SHOWTABLES);

  // Default Column-Width in DBGrids:
  updownMaxColWidth.Position := Mainform.GetRegValue(REGNAME_MAXCOLWIDTH, DEFAULT_MAXCOLWIDTH);
  // Color-coding:
  pnlSQLColKeywords.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLKEYATTRI, ColorToString(DEFAULT_SQLCOLKEYATTRI)));
  pnlSQLColFunctions.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLFUNCTIONATTRI, ColorToString(DEFAULT_SQLCOLFUNCTIONATTRI)));
  pnlSQLColDatatypes.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLDATATYPEATTRI, ColorToString(DEFAULT_SQLCOLDATATYPEATTRI)));
  pnlSQLColNumeric.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLNUMBERATTRI, ColorToString(DEFAULT_SQLCOLNUMBERATTRI)));
  pnlSQLColString.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLSTRINGATTRI, ColorToString(DEFAULT_SQLCOLSTRINGATTRI)));
  pnlSQLColComments.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLCOMMENTATTRI, ColorToString(DEFAULT_SQLCOLCOMMENTATTRI)));
  pnlSQLColTablenames.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLTABLENAMEATTRI, ColorToString(DEFAULT_SQLCOLTABLENAMEATTRI)));
  pnlSQLColActiveLine.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLACTIVELINE, ColorToString(DEFAULT_SQLCOLACTIVELINE)));
  // CSV-Options:
  editCSVSeparator.Text := Mainform.GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  editCSVEncloser.Text := Mainform.GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  editCSVTerminator.Text := Mainform.GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  // Remember data pane filters across sessions
  chkRememberFilters.Checked := Mainform.GetRegValue(REGNAME_REMEMBERFILTERS, DEFAULT_REMEMBERFILTERS);
  // Log to file
  chkLogToFile.Checked := Mainform.GetRegValue(REGNAME_LOGTOFILE, DEFAULT_LOGTOFILE);
  btnOpenLogFolder.Enabled := DirectoryExists(DirnameSessionLogs);

  // SQL-Appearance:
  EnumFontFamilies(Canvas.Handle,  // HDC of Device-Context.
                   nil,            // Name of Font-Family (PChar)
                   @EnumFixedProc, // Address of Callback-Function
                   LPARAM(Pointer(comboSQLFontName.Items))); // customized data

  comboSQLFontName.ItemIndex := comboSQLFontName.Items.IndexOf(sqlfontname);
  updownSQLFontSize.Position := sqlfontsize;
  pnlSQLFontPattern.Font.Name := sqlfontname;
  pnlSQLFontPattern.Font.Size := sqlfontsize;

  // Data-Appearance:
  comboDataFontName.Items := Screen.Fonts;
  comboDataFontName.ItemIndex := comboDataFontName.Items.IndexOf(datafontname);
  updownDataFontSize.Position := datafontsize;
  // Load color settings
  cboxNumeric.Selected := Mainform.GetRegValue(REGNAME_FIELDCOLOR_NUMERIC, DEFAULT_FIELDCOLOR_NUMERIC);
  cboxText.Selected := Mainform.GetRegValue(REGNAME_FIELDCOLOR_TEXT, DEFAULT_FIELDCOLOR_TEXT);
  cboxBinary.Selected := Mainform.GetRegValue(REGNAME_FIELDCOLOR_BINARY, DEFAULT_FIELDCOLOR_BINARY);
  cboxDatetime.Selected := Mainform.GetRegValue(REGNAME_FIELDCOLOR_DATETIME, DEFAULT_FIELDCOLOR_DATETIME);
  cboxEnum.Selected := Mainform.GetRegValue(REGNAME_FIELDCOLOR_ENUM, DEFAULT_FIELDCOLOR_ENUM);
  cboxSet.Selected := Mainform.GetRegValue(REGNAME_FIELDCOLOR_SET, DEFAULT_FIELDCOLOR_SET);
  cboxNullBG.Selected := Mainform.GetRegValue(REGNAME_BG_NULL, DEFAULT_BG_NULL);
  // Editor enablings
  chkEditorText.Checked := Mainform.GetRegValue(REGNAME_FIELDEDITOR_TEXT, DEFAULT_FIELDEDITOR_TEXT);
  chkEditorBinary.Checked := Mainform.GetRegValue(REGNAME_FIELDEDITOR_BINARY, DEFAULT_FIELDEDITOR_BINARY);
  chkEditorDatetime.Checked := Mainform.GetRegValue(REGNAME_FIELDEDITOR_DATETIME, DEFAULT_FIELDEDITOR_DATETIME);
  chkEditorEnum.Checked := Mainform.GetRegValue(REGNAME_FIELDEDITOR_ENUM, DEFAULT_FIELDEDITOR_ENUM);
  chkEditorSet.Checked := Mainform.GetRegValue(REGNAME_FIELDEDITOR_SET, DEFAULT_FIELDEDITOR_SET);
  chkNullBG.Checked := Mainform.GetRegValue(REGNAME_BG_NULL_ENABLED, DEFAULT_BG_NULL_ENABLED);

  btnOK.Enabled := False;
  btnApply.Enabled := False;
  screen.Cursor := crdefault;
end;



procedure Toptionsform.FontsChange(Sender: TObject);
begin
  pnlSQLFontPattern.Font.Name := comboSQLFontName.Items[comboSQLFontName.ItemIndex];
  pnlSQLFontPattern.Font.Size := updownSQLFontSize.Position;
  Modified(Sender);
end;


procedure Toptionsform.CallColorDialog(Sender: TObject);
begin
  coldlgSQLColors.Color := (sender as TPanel).Color;
  if coldlgSQLColors.Execute then
  begin
    (sender as TPanel).Color := coldlgSQLColors.Color;
    Modified(Sender);
  end;
end;

procedure Toptionsform.DataFontsChange(Sender: TObject);
begin
  Modified(Sender);
end;

procedure Toptionsform.anyUpDownLimitChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  Modified(Sender);
end;


{**
  Open folder with session logs
}
procedure Toptionsform.btnOpenLogFolderClick(Sender: TObject);
begin
  ShellExec( '', DirnameSessionLogs );
end;

{**
  Updatecheck checkbox was clicked
}
procedure Toptionsform.chkUpdatecheckClick(Sender: TObject);
begin
  updownUpdatecheckInterval.Enabled := chkUpdatecheck.Checked;
  editUpdatecheckInterval.Enabled := chkUpdatecheck.Checked;
  chkUpdatecheckBuilds.Enabled := chkUpdatecheck.Checked;
  Modified(Sender);
end;

procedure Toptionsform.chkNullBGClick(Sender: TObject);
begin
  cboxNullBG.Enabled := (Sender as TCheckbox).Checked;
  Modified(Sender);
end;

end.
