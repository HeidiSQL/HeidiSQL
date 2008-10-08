unit options;


// -------------------------------------
// Preferences
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Registry, ExtCtrls, SynEditHighlighter, SynHighlighterSQL,
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
    chkRememberFilters: TCheckBox;
    chkLogToFile: TCheckBox;
    btnOpenLogFolder: TButton;
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation
uses childwin, main, helpers;
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
  reg.WriteString(REGNAME_SQLCOLKEYATTRI, colortostring(SynSQLSynSQLSample.KeyAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLFUNCTIONATTRI, colortostring(SynSQLSynSQLSample.FunctionAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLDATATYPEATTRI, colortostring(SynSQLSynSQLSample.DataTypeAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLNUMBERATTRI, colortostring(SynSQLSynSQLSample.NumberAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLSTRINGATTRI, colortostring(SynSQLSynSQLSample.StringAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLCOMMENTATTRI, colortostring(SynSQLSynSQLSample.CommentAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLCONDCOMMATTRI, colortostring(SynSQLSynSQLSample.ConditionalCommentAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLTABLENAMEATTRI, colortostring(SynSQLSynSQLSample.TableNameAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLSYMBOLATTRI, colortostring(SynSQLSynSQLSample.SymbolAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLIDENTATTRI, colortostring(SynSQLSynSQLSample.IdentifierAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLDELIMIDENTATTRI, colortostring(SynSQLSynSQLSample.DelimitedIdentifierAttri.Foreground));
  reg.WriteString(REGNAME_SQLCOLACTIVELINE, ColorToString(SynMemoSQLSample.ActiveLineColor));
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
    cwin.SynMemoQuery.Font := SynMemoSQLSample.Font;
    cwin.SynMemoSQLLog.Font := SynMemoSQLSample.Font;
    cwin.SynMemoProcessView.Font := SynMemoSQLSample.Font;
    cwin.SynMemoFilter.Font := SynMemoSQLSample.Font;
    cwin.SynSQLSyn1.KeyAttri.Foreground := SynSQLSynSQLSample.KeyAttri.Foreground;
    cwin.SynSQLSyn1.FunctionAttri.Foreground := SynSQLSynSQLSample.FunctionAttri.Foreground;
    cwin.SynSQLSyn1.DataTypeAttri.Foreground := SynSQLSynSQLSample.DataTypeAttri.Foreground;
    cwin.SynSQLSyn1.NumberAttri.Foreground := SynSQLSynSQLSample.NumberAttri.Foreground;
    cwin.SynSQLSyn1.StringAttri.Foreground := SynSQLSynSQLSample.StringAttri.Foreground;
    cwin.SynSQLSyn1.CommentAttri.Foreground := SynSQLSynSQLSample.CommentAttri.Foreground;
    cwin.SynSQLSyn1.ConditionalCommentAttri.Foreground := SynSQLSynSQLSample.ConditionalCommentAttri.Foreground;
    cwin.SynSQLSyn1.TablenameAttri.Foreground := SynSQLSynSQLSample.TablenameAttri.Foreground;
    cwin.SynSQLSyn1.SymbolAttri.Foreground := SynSQLSynSQLSample.SymbolAttri.Foreground;
    cwin.SynSQLSyn1.IdentifierAttri.Foreground := SynSQLSynSQLSample.IdentifierAttri.Foreground;
    cwin.SynSQLSyn1.DelimitedIdentifierAttri.Foreground := SynSQLSynSQLSample.DelimitedIdentifierAttri.Foreground;
    cwin.SynMemoQuery.ActiveLineColor := SynMemoSQLSample.ActiveLineColor;
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

  // Color-coding:
  SynSQLSynSQLSample.KeyAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLKEYATTRI, ColorToString(DEFAULT_SQLCOLKEYATTRI)));
  SynSQLSynSQLSample.FunctionAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLFUNCTIONATTRI, ColorToString(DEFAULT_SQLCOLFUNCTIONATTRI)));
  SynSQLSynSQLSample.DataTypeAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLDATATYPEATTRI, ColorToString(DEFAULT_SQLCOLDATATYPEATTRI)));
  SynSQLSynSQLSample.NumberAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLNUMBERATTRI, ColorToString(DEFAULT_SQLCOLNUMBERATTRI)));
  SynSQLSynSQLSample.StringAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLSTRINGATTRI, ColorToString(DEFAULT_SQLCOLSTRINGATTRI)));
  SynSQLSynSQLSample.CommentAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLCOMMENTATTRI, ColorToString(DEFAULT_SQLCOLCOMMENTATTRI)));
  SynSQLSynSQLSample.ConditionalCommentAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLCONDCOMMATTRI, ColorToString(DEFAULT_SQLCOLCONDCOMMATTRI)));
  SynSQLSynSQLSample.TableNameAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLTABLENAMEATTRI, ColorToString(DEFAULT_SQLCOLTABLENAMEATTRI)));
  SynSQLSynSQLSample.SymbolAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLSYMBOLATTRI, ColorToString(DEFAULT_SQLCOLSYMBOLATTRI)));
  SynSQLSynSQLSample.IdentifierAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLIDENTATTRI, ColorToString(DEFAULT_SQLCOLIDENTATTRI)));
  SynSQLSynSQLSample.DelimitedIdentifierAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLDELIMIDENTATTRI, ColorToString(DEFAULT_SQLCOLDELIMIDENTATTRI)));
  SynMemoSQLSample.ActiveLineColor := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLACTIVELINE, ColorToString(DEFAULT_SQLCOLACTIVELINE)));
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


end.
