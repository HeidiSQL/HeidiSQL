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
    lblSQLColColor: TLabel;
    cboxSQLColColor: TColorBox;
    grpSQLSample: TGroupBox;
    SynMemoSQLSample: TSynMemo;
    SynSQLSynSQLSample: TSynSQLSyn;
    lblCopyDataMaxSize: TLabel;
    editCopyDataMaxSize: TEdit;
    updownCopyDataMaxSize: TUpDown;
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
  private
    { Private declarations }
    FWasModified: Boolean;
  public
    { Public declarations }
  end;


implementation
uses main, helpers;
{$R *.DFM}

const
  SQLEL_KEYWORD = 'Keywords';
  SQLEL_FUNCTION = 'Functions';
  SQLEL_DATATYPE = 'Data types';
  SQLEL_NUMBER = 'Numeric values';
  SQLEL_STRING = 'String values';
  SQLEL_COMMENT = 'Comments';
  SQLEL_CONDCOMM = 'Conditional comments';
  SQLEL_TABLE = 'Table names';
  SQLEL_SYMBOL = 'Symbols';
  SQLEL_IDENT = 'Identifiers';
  SQLEL_DELIMIDENT = 'Delimited identifiers';
  SQLEL_ACTLINE = 'Active line background';


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
  MainReg.WriteString(REGNAME_SQLCOLKEYATTRI, colortostring(SynSQLSynSQLSample.KeyAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLFUNCTIONATTRI, colortostring(SynSQLSynSQLSample.FunctionAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLDATATYPEATTRI, colortostring(SynSQLSynSQLSample.DataTypeAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLNUMBERATTRI, colortostring(SynSQLSynSQLSample.NumberAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLSTRINGATTRI, colortostring(SynSQLSynSQLSample.StringAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLCOMMENTATTRI, colortostring(SynSQLSynSQLSample.CommentAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLCONDCOMMATTRI, colortostring(SynSQLSynSQLSample.ConditionalCommentAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLTABLENAMEATTRI, colortostring(SynSQLSynSQLSample.TableNameAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLSYMBOLATTRI, colortostring(SynSQLSynSQLSample.SymbolAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLIDENTATTRI, colortostring(SynSQLSynSQLSample.IdentifierAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLDELIMIDENTATTRI, colortostring(SynSQLSynSQLSample.DelimitedIdentifierAttri.Foreground));
  MainReg.WriteString(REGNAME_SQLCOLACTIVELINE, ColorToString(SynMemoSQLSample.ActiveLineColor));
  MainReg.WriteString(REGNAME_CSV_SEPARATOR, editCSVSeparator.Text);
  MainReg.WriteString(REGNAME_CSV_ENCLOSER, editCSVEncloser.Text);
  MainReg.WriteString(REGNAME_CSV_TERMINATOR, editCSVTerminator.Text);
  MainReg.WriteInteger(REGNAME_COPYMAXSIZE, updownCopyDataMaxSize.Position);

  MainReg.WriteInteger(REGNAME_MAXCOLWIDTH, updownMaxColWidth.Position);
  MainReg.WriteString(REGNAME_DATAFONTNAME, comboDataFontName.Text);
  MainReg.WriteInteger(REGNAME_DATAFONTSIZE, updownDataFontSize.Position);
  MainReg.WriteBool(REGNAME_LOGTOFILE, chkLogToFile.Checked);
  MainReg.WriteBool(REGNAME_DO_UPDATECHECK, chkUpdatecheck.Checked);
  MainReg.WriteBool(REGNAME_DO_UPDATECHECK_BUILDS, chkUpdatecheckBuilds.Checked);
  MainReg.WriteInteger(REGNAME_UPDATECHECK_INTERVAL, updownUpdatecheckInterval.Position);
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
  Mainform.SynSQLSyn1.KeyAttri.Foreground := SynSQLSynSQLSample.KeyAttri.Foreground;
  Mainform.SynSQLSyn1.FunctionAttri.Foreground := SynSQLSynSQLSample.FunctionAttri.Foreground;
  Mainform.SynSQLSyn1.DataTypeAttri.Foreground := SynSQLSynSQLSample.DataTypeAttri.Foreground;
  Mainform.SynSQLSyn1.NumberAttri.Foreground := SynSQLSynSQLSample.NumberAttri.Foreground;
  Mainform.SynSQLSyn1.StringAttri.Foreground := SynSQLSynSQLSample.StringAttri.Foreground;
  Mainform.SynSQLSyn1.CommentAttri.Foreground := SynSQLSynSQLSample.CommentAttri.Foreground;
  Mainform.SynSQLSyn1.ConditionalCommentAttri.Foreground := SynSQLSynSQLSample.ConditionalCommentAttri.Foreground;
  Mainform.SynSQLSyn1.TablenameAttri.Foreground := SynSQLSynSQLSample.TablenameAttri.Foreground;
  Mainform.SynSQLSyn1.SymbolAttri.Foreground := SynSQLSynSQLSample.SymbolAttri.Foreground;
  Mainform.SynSQLSyn1.IdentifierAttri.Foreground := SynSQLSynSQLSample.IdentifierAttri.Foreground;
  Mainform.SynSQLSyn1.DelimitedIdentifierAttri.Foreground := SynSQLSynSQLSample.DelimitedIdentifierAttri.Foreground;
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
  Mainform.prefCSVSeparator := editCSVSeparator.Text;
  Mainform.prefCSVEncloser := editCSVEncloser.Text;
  Mainform.prefCSVTerminator := editCSVTerminator.Text;
  Mainform.prefFieldColorNumeric := cboxNumeric.Selected;
  Mainform.prefFieldColorText := cboxText.Selected;
  Mainform.prefFieldColorBinary := cboxBinary.Selected;
  Mainform.prefFieldColorDatetime := cboxDatetime.Selected;
  Mainform.prefFieldColorEnum := cboxEnum.Selected;
  Mainform.prefFieldColorSet := cboxSet.Selected;
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

  // Default Column-Width in DBGrids:
  updownMaxColWidth.Position := GetRegValue(REGNAME_MAXCOLWIDTH, DEFAULT_MAXCOLWIDTH);

  // Export-Options:
  editCSVSeparator.Text := GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  editCSVEncloser.Text := GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  editCSVTerminator.Text := GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  updownCopyDataMaxSize.Position := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE);

  // Log to file
  chkLogToFile.Checked := GetRegValue(REGNAME_LOGTOFILE, DEFAULT_LOGTOFILE);
  btnOpenLogFolder.Enabled := DirectoryExists(DirnameSessionLogs);

  // SQL-Appearance:
  EnumFontFamilies(Canvas.Handle,  // HDC of Device-Context.
                   nil,            // Name of Font-Family (PChar)
                   @EnumFixedProc, // Address of Callback-Function
                   LPARAM(Pointer(comboSQLFontName.Items))); // customized data

  // Color-coding:
  SynSQLSynSQLSample.KeyAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLKEYATTRI, ColorToString(DEFAULT_SQLCOLKEYATTRI)));
  SynSQLSynSQLSample.FunctionAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLFUNCTIONATTRI, ColorToString(DEFAULT_SQLCOLFUNCTIONATTRI)));
  SynSQLSynSQLSample.DataTypeAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLDATATYPEATTRI, ColorToString(DEFAULT_SQLCOLDATATYPEATTRI)));
  SynSQLSynSQLSample.NumberAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLNUMBERATTRI, ColorToString(DEFAULT_SQLCOLNUMBERATTRI)));
  SynSQLSynSQLSample.StringAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLSTRINGATTRI, ColorToString(DEFAULT_SQLCOLSTRINGATTRI)));
  SynSQLSynSQLSample.CommentAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLCOMMENTATTRI, ColorToString(DEFAULT_SQLCOLCOMMENTATTRI)));
  SynSQLSynSQLSample.ConditionalCommentAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLCONDCOMMATTRI, ColorToString(DEFAULT_SQLCOLCONDCOMMATTRI)));
  SynSQLSynSQLSample.TableNameAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLTABLENAMEATTRI, ColorToString(DEFAULT_SQLCOLTABLENAMEATTRI)));
  SynSQLSynSQLSample.SymbolAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLSYMBOLATTRI, ColorToString(DEFAULT_SQLCOLSYMBOLATTRI)));
  SynSQLSynSQLSample.IdentifierAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLIDENTATTRI, ColorToString(DEFAULT_SQLCOLIDENTATTRI)));
  SynSQLSynSQLSample.DelimitedIdentifierAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLDELIMIDENTATTRI, ColorToString(DEFAULT_SQLCOLDELIMIDENTATTRI)));
  SynMemoSQLSample.ActiveLineColor := StringToColor(GetRegValue(REGNAME_SQLCOLACTIVELINE, ColorToString(DEFAULT_SQLCOLACTIVELINE)));
  comboSQLFontName.ItemIndex := comboSQLFontName.Items.IndexOf(sqlfontname);
  updownSQLFontSize.Position := sqlfontsize;
  SynMemoSQLSample.Text := 'SELECT DATE_SUB(NOW(), INTERVAL 1 DAY),' + CRLF +
    '  ''String literal'' AS lit' + CRLF +
    'FROM tableA AS ta -- A comment' + CRLF +
    'WHERE `columnA` IS NULL; # More comment' + CRLF +
    CRLF +
    'CREATE TABLE /*!32312 IF NOT EXISTS*/ tableB' + CRLF +
    '  (id INT, name VARCHAR(30) DEFAULT "standard")';
  SynMemoSQLSample.Font.Name := sqlfontname;
  SynMemoSQLSample.Font.Size := sqlfontsize;
  SynSQLSynSQLSample.TableNames.CommaText := 'tableA,tableB';
  comboSQLColElement.Items.Delimiter := ',';
  comboSQLColElement.Items.StrictDelimiter := True;
  comboSQLColElement.Items.DelimitedText := SQLEL_KEYWORD+','+SQLEL_FUNCTION+','+SQLEL_DATATYPE+','+
    SQLEL_NUMBER+','+SQLEL_STRING+','+SQLEL_COMMENT+','+SQLEL_CONDCOMM+','+SQLEL_TABLE+','+
    SQLEL_SYMBOL+','+SQLEL_IDENT+','+SQLEL_DELIMIDENT+','+SQLEL_ACTLINE;
  comboSQLColElement.ItemIndex := 0;
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
  elem: String;
  attr: TSynHighlighterAttributes;
  col: TColor;
begin
  SynMemoSQLSample.Font.Name := comboSQLFontName.Items[comboSQLFontName.ItemIndex];
  SynMemoSQLSample.Font.Size := updownSQLFontSize.Position;
  elem := comboSQLColElement.Text;
  col := cboxSQLColColor.Selected;
  if elem = SQLEL_ACTLINE then begin
    SynMemoSQLSample.ActiveLineColor := col;
  end else begin
    if elem = SQLEL_KEYWORD then attr := SynSqlSynSQLSample.KeyAttri
    else if elem = SQLEL_FUNCTION then attr := SynSqlSynSQLSample.FunctionAttri
    else if elem = SQLEL_DATATYPE then attr := SynSqlSynSQLSample.DatatypeAttri
    else if elem = SQLEL_NUMBER then attr := SynSqlSynSQLSample.NumberAttri
    else if elem = SQLEL_STRING then attr := SynSqlSynSQLSample.StringAttri
    else if elem = SQLEL_COMMENT then attr := SynSqlSynSQLSample.CommentAttri
    else if elem = SQLEL_CONDCOMM then attr := SynSqlSynSQLSample.ConditionalCommentAttri
    else if elem = SQLEL_TABLE then attr := SynSqlSynSQLSample.TablenameAttri
    else if elem = SQLEL_SYMBOL then attr := SynSqlSynSQLSample.SymbolAttri
    else if elem = SQLEL_IDENT then attr := SynSqlSynSQLSample.IdentifierAttri
    else attr := SynSqlSynSQLSample.DelimitedIdentifierAttri;
    attr.Foreground := col;
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
  elem: String;
  attr: TSynHighlighterAttributes;
  col: TColor;
begin
  elem := comboSQLColElement.Text;
  if elem = SQLEL_ACTLINE then begin
    col := SynMemoSQLSample.ActiveLineColor;
  end else begin
    if elem = SQLEL_KEYWORD then attr := SynSqlSynSQLSample.KeyAttri
    else if elem = SQLEL_FUNCTION then attr := SynSqlSynSQLSample.FunctionAttri
    else if elem = SQLEL_DATATYPE then attr := SynSqlSynSQLSample.DatatypeAttri
    else if elem = SQLEL_NUMBER then attr := SynSqlSynSQLSample.NumberAttri
    else if elem = SQLEL_STRING then attr := SynSqlSynSQLSample.StringAttri
    else if elem = SQLEL_COMMENT then attr := SynSqlSynSQLSample.CommentAttri
    else if elem = SQLEL_CONDCOMM then attr := SynSqlSynSQLSample.ConditionalCommentAttri
    else if elem = SQLEL_TABLE then attr := SynSqlSynSQLSample.TablenameAttri
    else if elem = SQLEL_SYMBOL then attr := SynSqlSynSQLSample.SymbolAttri
    else if elem = SQLEL_IDENT then attr := SynSqlSynSQLSample.IdentifierAttri
    else attr := SynSqlSynSQLSample.DelimitedIdentifierAttri;
    col := attr.Foreground;
  end;
  cboxSQLColColor.Selected := col;
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
  Start, TokenTypeInt: Integer;
  Attri: TSynHighlighterAttributes;
  sm: TSynMemo;
  f: String;
begin
  sm := Sender as TSynMemo;
  sm.GetHighlighterAttriAtRowColEx(sm.CaretXY, Token, TokenTypeInt, Start, Attri);
  case TtkTokenKind(TokenTypeInt) of
    tkKey: f := SQLEL_KEYWORD;
    tkFunction: f := SQLEL_FUNCTION;
    tkDatatype: f := SQLEL_DATATYPE;
    tkNumber: f := SQLEL_NUMBER;
    tkString: f := SQLEL_STRING;
    tkComment: f := SQLEL_COMMENT;
    tkConditionalComment: f := SQLEL_CONDCOMM;
    tkTableName: f := SQLEL_TABLE;
    tkSymbol: f := SQLEL_SYMBOL;
    tkIdentifier: f := SQLEL_IDENT;
    tkDelimitedIdentifier: f := SQLEL_DELIMIDENT;
    else f := '';
  end;
  if f = '' then
    Exit;
  ComboSQLColElement.ItemIndex := ComboSQLColElement.Items.IndexOf(f);
  ComboSQLColElement.OnChange(Sender);
end;


end.
