unit options;


// -------------------------------------
// Preferences
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, SynEditHighlighter, SynHighlighterSQL,
  SynEdit, SynMemo;

type
  Toptionsform = class(TForm)
    pagecontrolMain: TPageControl;
    tabMisc: TTabSheet;
    btnCancel: TButton;
    btnOK: TButton;
    btnApply: TButton;
    tabSQL: TTabSheet;
    chkAutoReconnect: TCheckBox;
    lblLogLinesHint: TLabel;
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
    lblDataFontHint: TLabel;
    lblMaxColWidth: TLabel;
    updownLogLines: TUpDown;
    editLogLines: TEdit;
    editMaxColWidth: TEdit;
    updownMaxColWidth: TUpDown;
    chkRestoreLastDB: TCheckBox;
    chkLogToFile: TCheckBox;
    btnOpenLogFolder: TButton;
    lblLogSnip: TLabel;
    editLogSnip: TEdit;
    updownLogSnip: TUpDown;
    lblLogSnipHint: TLabel;
    chkUpdatecheck: TCheckBox;
    editUpdatecheckInterval: TEdit;
    updownUpdatecheckInterval: TUpDown;
    chkUpdateCheckBuilds: TCheckBox;
    grpFieldLayout: TGroupBox;
    lblFieldDatetime: TLabel;
    cboxText: TColorBox;
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
    grpSQLFont: TGroupBox;
    comboSQLFontName: TComboBox;
    lblSQLFontSize: TLabel;
    editSQLFontSize: TEdit;
    updownSQLFontSize: TUpDown;
    grpSQLColors: TGroupBox;
    comboSQLColElement: TComboBox;
    lblSQLColElement: TLabel;
    lblSQLColForeground: TLabel;
    cboxSQLColForeground: TColorBox;
    grpSQLSample: TGroupBox;
    SynMemoSQLSample: TSynMemo;
    SynSQLSynSQLSample: TSynSQLSyn;
    lblCopyDataMaxSize: TLabel;
    editCopyDataMaxSize: TEdit;
    updownCopyDataMaxSize: TUpDown;
    chkSQLBold: TCheckBox;
    chkSQLItalic: TCheckBox;
    lblSQLColBackground: TLabel;
    cboxSQLColBackground: TColorBox;
    btnRestoreDefaults: TButton;
    lblMaxTotalRows: TLabel;
    editMaxTotalRows: TEdit;
    chkDoStatistics: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure Apply(Sender: TObject);
    procedure SQLFontChange(Sender: TObject);
    procedure DataFontsChange(Sender: TObject);
    procedure anyUpDownLimitChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure btnOpenLogFolderClick(Sender: TObject);
    procedure chkUpdatecheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkNullBGClick(Sender: TObject);
    procedure comboSQLColElementChange(Sender: TObject);
    procedure pagecontrolMainChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure pagecontrolMainChange(Sender: TObject);
    procedure updownSQLFontSizeClick(Sender: TObject; Button: TUDBtnType);
    procedure SynMemoSQLSampleClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
  private
    { Private declarations }
    FWasModified: Boolean;
  public
    { Public declarations }
  end;


implementation
uses main, helpers, mysql_structures;
{$R *.DFM}


procedure Toptionsform.Modified(Sender: TObject);
begin
  // Modified
  btnApply.Enabled := True;
end;


procedure Toptionsform.pagecontrolMainChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Remember modification state. First tab switch leads TEdit's with TUpDown
  // to fire OnChange. Avoid enabling the buttons in that case.
  FWasModified := btnApply.Enabled;
end;


procedure Toptionsform.pagecontrolMainChange(Sender: TObject);
begin
  // See OnChanging procedure
  btnApply.Enabled := FWasModified;
end;


{**
  Apply settings to registry and mainform
}
procedure Toptionsform.Apply(Sender: TObject);
var
  i, maxrows: Integer;
  Attri: TSynHighlighterAttributes;
begin
  Screen.Cursor := crHourGlass;

  // Open registry key
  OpenRegistry;

  // Save values
  MainReg.WriteBool(REGNAME_AUTORECONNECT, chkAutoReconnect.Checked);
  MainReg.WriteBool(REGNAME_RESTORELASTUSEDDB, chkRestoreLastDB.Checked);
  MainReg.WriteString(REGNAME_FONTNAME, comboSQLFontName.Text);
  MainReg.WriteInteger(REGNAME_FONTSIZE, updownSQLFontSize.Position);
  MainReg.WriteInteger(REGNAME_LOGSQLNUM, updownLogLines.Position);
  MainReg.WriteInteger(REGNAME_LOGSQLWIDTH, updownLogSnip.Position);
  for i:=0 to SynSQLSynSQLSample.AttrCount - 1 do begin
    Attri := SynSQLSynSQLSample.Attribute[i];
    MainReg.WriteInteger(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_FG, Attri.Foreground);
    MainReg.WriteInteger(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_BG, Attri.Background);
    MainReg.WriteInteger(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_STYLE, Attri.IntegerStyle);
  end;
  MainReg.WriteString(REGNAME_SQLCOLACTIVELINE, ColorToString(SynMemoSQLSample.ActiveLineColor));

  MainReg.WriteString(REGNAME_CSV_SEPARATOR, editCSVSeparator.Text);
  MainReg.WriteString(REGNAME_CSV_ENCLOSER, editCSVEncloser.Text);
  MainReg.WriteString(REGNAME_CSV_TERMINATOR, editCSVTerminator.Text);
  MainReg.WriteInteger(REGNAME_COPYMAXSIZE, updownCopyDataMaxSize.Position);

  MainReg.WriteInteger(REGNAME_MAXCOLWIDTH, updownMaxColWidth.Position);
  maxrows := StrToIntDef(editMaxTotalRows.Text, DEFAULT_MAXTOTALROWS);
  MainReg.WriteInteger(REGNAME_MAXTOTALROWS, maxrows);
  MainReg.WriteString(REGNAME_DATAFONTNAME, comboDataFontName.Text);
  MainReg.WriteInteger(REGNAME_DATAFONTSIZE, updownDataFontSize.Position);
  MainReg.WriteBool(REGNAME_LOGTOFILE, chkLogToFile.Checked);
  MainReg.WriteBool(REGNAME_DO_UPDATECHECK, chkUpdatecheck.Checked);
  MainReg.WriteBool(REGNAME_DO_UPDATECHECK_BUILDS, chkUpdatecheckBuilds.Checked);
  MainReg.WriteInteger(REGNAME_UPDATECHECK_INTERVAL, updownUpdatecheckInterval.Position);
  MainReg.WriteBool(REGNAME_DO_STATISTICS, chkDoStatistics.Checked);
  // Save color settings
  MainReg.WriteInteger(REGNAME_FIELDCOLOR_NUMERIC, cboxNumeric.Selected);
  MainReg.WriteInteger(REGNAME_FIELDCOLOR_TEXT, cboxText.Selected);
  MainReg.WriteInteger(REGNAME_FIELDCOLOR_BINARY, cboxBinary.Selected);
  MainReg.WriteInteger(REGNAME_FIELDCOLOR_DATETIME, cboxDatetime.Selected);
  MainReg.WriteInteger(REGNAME_FIELDCOLOR_ENUM, cboxEnum.Selected);
  MainReg.WriteInteger(REGNAME_FIELDCOLOR_SET, cboxSet.Selected);
  MainReg.WriteInteger(REGNAME_BG_NULL, cboxNullBg.Selected);
  // Editor enablings
  MainReg.WriteBool(REGNAME_FIELDEDITOR_BINARY, chkEditorBinary.Checked);
  MainReg.WriteBool(REGNAME_FIELDEDITOR_DATETIME, chkEditorDatetime.Checked);
  MainReg.WriteBool(REGNAME_FIELDEDITOR_ENUM, chkEditorEnum.Checked);
  MainReg.WriteBool(REGNAME_FIELDEDITOR_SET, chkEditorSet.Checked);
  MainReg.WriteBool(REGNAME_BG_NULL_ENABLED, chkNullBg.Checked);

  // Set relevant properties in mainform
  Mainform.SynMemoQuery.Font := SynMemoSQLSample.Font;
  Mainform.SynMemoQuery.Gutter.Font := SynMemoSQLSample.Font;
  Mainform.SynMemoSQLLog.Font := SynMemoSQLSample.Font;
  Mainform.SynMemoSQLLog.Gutter.Font := SynMemoSQLSample.Font;
  Mainform.SynMemoProcessView.Font := SynMemoSQLSample.Font;
  Mainform.SynMemoFilter.Font := SynMemoSQLSample.Font;
  for i := 0 to SynSQLSynSQLSample.AttrCount - 1 do begin
    Mainform.SynSQLSyn1.Attribute[i].AssignColorAndStyle(SynSQLSynSQLSample.Attribute[i]);
  end;
  Mainform.SynMemoQuery.ActiveLineColor := SynMemoSQLSample.ActiveLineColor;
  Mainform.DataGrid.Font.Name := comboDataFontName.Text;
  Mainform.QueryGrid.Font.Name := comboDataFontName.Text;
  Mainform.DataGrid.Font.Size := updownDataFontSize.Position;
  Mainform.QueryGrid.Font.Size := updownDataFontSize.Position;
  FixVT(Mainform.QueryGrid);
  FixVT(Mainform.DataGrid);
  Mainform.prefLogsqlnum := updownLogLines.Position;
  Mainform.prefLogSqlWidth := updownLogSnip.Position;
  Mainform.TrimSQLLog;
  if chkLogToFile.Checked then
    Mainform.ActivateFileLogging
  else if Mainform.prefLogToFile then
    Mainform.DeactivateFileLogging;
  btnOpenLogFolder.Enabled := DirectoryExists(DirnameSessionLogs);
  Mainform.prefMaxColWidth := updownMaxColWidth.Position;
  Mainform.prefMaxTotalRows := maxrows;
  Mainform.prefCSVSeparator := editCSVSeparator.Text;
  Mainform.prefCSVEncloser := editCSVEncloser.Text;
  Mainform.prefCSVTerminator := editCSVTerminator.Text;
  DatatypeCategories[Integer(dtcInteger)].Color := cboxNumeric.Selected;
  DatatypeCategories[Integer(dtcReal)].Color := cboxNumeric.Selected;
  DatatypeCategories[Integer(dtcText)].Color := cboxText.Selected;
  DatatypeCategories[Integer(dtcBinary)].Color := cboxBinary.Selected;
  DatatypeCategories[Integer(dtcTemporal)].Color := cboxDatetime.Selected;
  DatatypeCategories[Integer(dtcIntegerNamed)].Color := cboxEnum.Selected;
  DatatypeCategories[Integer(dtcSet)].Color := cboxSet.Selected;
  DatatypeCategories[Integer(dtcSetNamed)].Color := cboxSet.Selected;
  Mainform.prefNullBG := cboxNullBg.Selected;
  Mainform.CalcNullColors;
  Mainform.DataGrid.Repaint;
  Mainform.QueryGrid.Repaint;
  Mainform.prefEnableBinaryEditor := chkEditorBinary.Checked;
  Mainform.prefEnableDatetimeEditor := chkEditorDatetime.Checked;
  Mainform.prefEnableEnumEditor := chkEditorEnum.Checked;
  Mainform.prefEnableSetEditor := chkEditorSet.Checked;
  Mainform.prefEnableNullBG := chkNullBg.Checked;

  // Settings have been applied, send a signal to the user
  btnApply.Enabled := False;
  Screen.Cursor := crDefault;
end;



procedure Toptionsform.FormCreate(Sender: TObject);
var
  i: Integer;
  // Callback function used by EnumFontFamilies()
  function EnumFixedProc(lpelf: PEnumLogFont; lpntm: PNewTextMetric; FontType: Integer; Data: LPARAM): Integer; stdcall;
  begin
    Result := 1;  // don't cancel
    if (lpelf^.elfLogFont.lfPitchAndFamily and FIXED_PITCH) <> 0 then
      (TStrings(Data)).Add(String(lpelf^.elfLogFont.lfFaceName));
  end;
begin
  InheritFont(Font);
  EnumFontFamilies(Canvas.Handle, nil, @EnumFixedProc, LPARAM(Pointer(comboSQLFontName.Items)));
  SynMemoSQLSample.Text := 'SELECT DATE_SUB(NOW(), INTERVAL 1 DAY),' + CRLF +
    '  ''String literal'' AS lit' + CRLF +
    'FROM tableA AS ta -- A comment' + CRLF +
    'WHERE `columnA` IS NULL; # More comment' + CRLF +
    CRLF +
    'CREATE TABLE /*!32312 IF NOT EXISTS*/ tableB' + CRLF +
    '  (id INT, name VARCHAR(30) DEFAULT "standard")';
  SynSQLSynSQLSample.TableNames.CommaText := 'tableA,tableB';
  for i:=0 to SynSQLSynSQLSample.AttrCount - 1 do
    comboSQLColElement.Items.Add(SynSQLSynSQLSample.Attribute[i].FriendlyName);
  comboSQLColElement.Items.Add('Active line background');
  comboSQLColElement.ItemIndex := 0;
end;

procedure Toptionsform.FormShow(Sender: TObject);
var
  sqlfontname : String;
  sqlfontsize : Integer;
  datafontname : String;
  datafontsize : Integer;
begin
  screen.Cursor := crHourGlass;

  // Read and display values
  sqlfontname := GetRegValue(REGNAME_FONTNAME, DEFAULT_FONTNAME);
  sqlfontsize := GetRegValue(REGNAME_FONTSIZE, DEFAULT_FONTSIZE);
  datafontname := GetRegValue(REGNAME_DATAFONTNAME, DEFAULT_DATAFONTNAME);
  datafontsize := GetRegValue(REGNAME_DATAFONTSIZE, DEFAULT_DATAFONTSIZE);
  chkAutoReconnect.Checked := GetRegValue(REGNAME_AUTORECONNECT, DEFAULT_AUTORECONNECT);
  chkRestoreLastDB.Checked := GetRegValue(REGNAME_RESTORELASTUSEDDB, DEFAULT_RESTORELASTUSEDDB);
  updownLogLines.Position := GetRegValue(REGNAME_LOGSQLNUM, DEFAULT_LOGSQLNUM);
  updownLogSnip.Position := GetRegValue(REGNAME_LOGSQLWIDTH, DEFAULT_LOGSQLWIDTH);
  chkUpdatecheck.Checked := GetRegValue(REGNAME_DO_UPDATECHECK, DEFAULT_DO_UPDATECHECK);
  chkUpdatecheckBuilds.Checked := GetRegValue(REGNAME_DO_UPDATECHECK_BUILDS, DEFAULT_DO_UPDATECHECK_BUILDS);
  updownUpdatecheckInterval.Position := GetRegValue(REGNAME_UPDATECHECK_INTERVAL, DEFAULT_UPDATECHECK_INTERVAL);
  chkUpdatecheckClick(Sender);
  chkDoStatistics.Checked := GetRegValue(REGNAME_DO_STATISTICS, DEFAULT_DO_STATISTICS);

  // Default Column-Width in DBGrids:
  updownMaxColWidth.Position := GetRegValue(REGNAME_MAXCOLWIDTH, DEFAULT_MAXCOLWIDTH);
  editMaxTotalRows.Text := IntToStr(GetRegValue(REGNAME_MAXTOTALROWS, DEFAULT_MAXTOTALROWS));

  // Export-Options:
  editCSVSeparator.Text := GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  editCSVEncloser.Text := GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  editCSVTerminator.Text := GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  updownCopyDataMaxSize.Position := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE);

  // Log to file
  chkLogToFile.Checked := GetRegValue(REGNAME_LOGTOFILE, DEFAULT_LOGTOFILE);
  btnOpenLogFolder.Enabled := DirectoryExists(DirnameSessionLogs);

  // SQL:
  RestoreSyneditStyles(SynSQLSynSQLSample);
  SynMemoSQLSample.ActiveLineColor := StringToColor(GetRegValue(REGNAME_SQLCOLACTIVELINE, ColorToString(DEFAULT_SQLCOLACTIVELINE)));
  comboSQLFontName.ItemIndex := comboSQLFontName.Items.IndexOf(sqlfontname);
  updownSQLFontSize.Position := sqlfontsize;
  SynMemoSQLSample.Font.Name := sqlfontname;
  SynMemoSQLSample.Font.Size := sqlfontsize;
  comboSQLColElementChange(Sender);

  // Data-Appearance:
  comboDataFontName.Items := Screen.Fonts;
  comboDataFontName.ItemIndex := comboDataFontName.Items.IndexOf(datafontname);
  updownDataFontSize.Position := datafontsize;
  // Load color settings
  cboxNumeric.Selected := GetRegValue(REGNAME_FIELDCOLOR_NUMERIC, DEFAULT_FIELDCOLOR_NUMERIC);
  cboxText.Selected := GetRegValue(REGNAME_FIELDCOLOR_TEXT, DEFAULT_FIELDCOLOR_TEXT);
  cboxBinary.Selected := GetRegValue(REGNAME_FIELDCOLOR_BINARY, DEFAULT_FIELDCOLOR_BINARY);
  cboxDatetime.Selected := GetRegValue(REGNAME_FIELDCOLOR_DATETIME, DEFAULT_FIELDCOLOR_DATETIME);
  cboxEnum.Selected := GetRegValue(REGNAME_FIELDCOLOR_ENUM, DEFAULT_FIELDCOLOR_ENUM);
  cboxSet.Selected := GetRegValue(REGNAME_FIELDCOLOR_SET, DEFAULT_FIELDCOLOR_SET);
  cboxNullBG.Selected := GetRegValue(REGNAME_BG_NULL, DEFAULT_BG_NULL);
  // Editor enablings
  chkEditorBinary.Checked := GetRegValue(REGNAME_FIELDEDITOR_BINARY, DEFAULT_FIELDEDITOR_BINARY);
  chkEditorDatetime.Checked := GetRegValue(REGNAME_FIELDEDITOR_DATETIME, DEFAULT_FIELDEDITOR_DATETIME);
  chkEditorEnum.Checked := GetRegValue(REGNAME_FIELDEDITOR_ENUM, DEFAULT_FIELDEDITOR_ENUM);
  chkEditorSet.Checked := GetRegValue(REGNAME_FIELDEDITOR_SET, DEFAULT_FIELDEDITOR_SET);
  chkNullBG.Checked := GetRegValue(REGNAME_BG_NULL_ENABLED, DEFAULT_BG_NULL_ENABLED);

  btnApply.Enabled := False;
  screen.Cursor := crdefault;
end;



procedure Toptionsform.SQLFontChange(Sender: TObject);
var
  AttriIdx: Integer;
  Attri: TSynHighlighterAttributes;
  Foreground, Background: TColor;
begin
  SynMemoSQLSample.Font.Name := comboSQLFontName.Items[comboSQLFontName.ItemIndex];
  SynMemoSQLSample.Font.Size := updownSQLFontSize.Position;
  AttriIdx := comboSQLColElement.ItemIndex;
  Foreground := cboxSQLColForeground.Selected;
  Background := cboxSQLColBackground.Selected;
  if AttriIdx = comboSQLColElement.Items.Count-1 then begin
    SynMemoSQLSample.ActiveLineColor := Foreground;
  end else begin
    Attri := SynSqlSynSQLSample.Attribute[AttriIdx];
    Attri.Foreground := Foreground;
    Attri.Background := Background;
    if chkSQLBold.Checked then Attri.Style := Attri.Style + [fsBold]
    else Attri.Style := Attri.Style - [fsBold];
    if chkSQLItalic.Checked then Attri.Style := Attri.Style + [fsItalic]
    else Attri.Style := Attri.Style - [fsItalic];
  end;
  Modified(Sender);
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


procedure Toptionsform.comboSQLColElementChange(Sender: TObject);
var
  AttriIdx: Integer;
  Attri: TSynHighlighterAttributes;
  Foreground, Background: TColor;
begin
  AttriIdx := comboSQLColElement.ItemIndex;
  if AttriIdx = comboSQLColElement.Items.Count-1 then begin
    Foreground := SynMemoSQLSample.ActiveLineColor;
    Background := clNone;
    chkSQLBold.Enabled := False;
    chkSQLItalic.Enabled := False;
  end else begin
    Attri := SynSqlSynSQLSample.Attribute[AttriIdx];
    Foreground := Attri.Foreground;
    Background := Attri.Background;
    chkSQLBold.Enabled := True;
    chkSQLItalic.Enabled := True;
    chkSQLBold.OnClick := nil;
    chkSQLItalic.OnClick := nil;
    chkSQLBold.Checked := fsBold in Attri.Style;
    chkSQLItalic.Checked := fsItalic in Attri.Style;
    chkSQLBold.OnClick := SQLFontChange;
    chkSQLItalic.OnClick := SQLFontChange;
  end;
  cboxSQLColForeground.Selected := Foreground;
  cboxSQLColBackground.Selected := Background;
end;


procedure Toptionsform.updownSQLFontSizeClick(Sender: TObject;
  Button: TUDBtnType);
begin
  SQLFontChange(Sender);
end;


{**
  Select attribute in pulldown by click into SynMemo
}
procedure Toptionsform.SynMemoSQLSampleClick(Sender: TObject);
var
  Token: WideString;
  Attri: TSynHighlighterAttributes;
  AttriIdx: Integer;
  sm: TSynMemo;
begin
  sm := Sender as TSynMemo;
  sm.GetHighlighterAttriAtRowCol(sm.CaretXY, Token, Attri);
  if Attri = nil then
    Exit;
  AttriIdx := ComboSQLColElement.Items.IndexOf(Attri.FriendlyName);
  if AttriIdx = -1 then
    Exit;
  ComboSQLColElement.ItemIndex := AttriIdx;
  ComboSQLColElement.OnChange(Sender);
end;


procedure Toptionsform.btnRestoreDefaultsClick(Sender: TObject);
var
  ValueList: TStringlist;
  i: Integer;
begin
  // Factory defaults
  if MessageDlg('Reset all preference options to default values?'+CRLF+CRLF+'This also applies to automatic settings, e.g. toolbar positions.',
    mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
    Exit;
  OpenRegistry;
  ValueList := TStringlist.Create;
  Mainreg.GetValueNames(ValueList);
  for i:=0 to ValueList.Count-1 do
    Mainreg.DeleteValue(ValueList[i]);
  FormShow(Sender);
end;


end.
