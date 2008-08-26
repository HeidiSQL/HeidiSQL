unit options;


// -------------------------------------
// Preferences
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Registry, ExtCtrls, DBGrids;

type
  Toptionsform = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ButtonApply: TButton;
    TabSheet2: TTabSheet;
    CheckBoxAutoReconnect: TCheckBox;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Panel1: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    ComboBoxFonts: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    pnlKeywords: TPanel;
    ColorDialog1: TColorDialog;
    Label7: TLabel;
    pnlFunctions: TPanel;
    Label8: TLabel;
    pnlDatatypes: TPanel;
    Label9: TLabel;
    pnlNumeric: TPanel;
    Label10: TLabel;
    pnlString: TPanel;
    Label11: TLabel;
    pnlComments: TPanel;
    TabSheet5: TTabSheet;
    GroupBox1: TGroupBox;
    Label12: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    Edit3: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    CheckBoxConvertHTMLEntities: TCheckBox;
    Label18: TLabel;
    TabSheet7: TTabSheet;
    GroupBox2: TGroupBox;
    Panel8: TPanel;
    Label21: TLabel;
    Label23: TLabel;
    ComboBoxDataFonts: TComboBox;
    Edit4: TEdit;
    UpDownDataFontSize: TUpDown;
    Label24: TLabel;
    Label25: TLabel;
    Label22: TLabel;
    EditFontSize: TEdit;
    UpDownFontSize: TUpDown;
    Label19: TLabel;
    Label28: TLabel;
    pnlTablenames: TPanel;
    updownLogSQLNum: TUpDown;
    editLogSQLNum: TEdit;
    editDefaultColWidth: TEdit;
    updownDefaultColWidth: TUpDown;
    CheckBoxRestoreLastUsedDB: TCheckBox;
    chkRememberFilters: TCheckBox;
    chkLogToFile: TCheckBox;
    btnOpenLogFolder: TButton;
    Label29: TLabel;
    pnlActiveLine: TPanel;
    labelLogSnip: TLabel;
    editLogSnip: TEdit;
    updownLogSnip: TUpDown;
    labelSqlSnipHint: TLabel;
    chkUpdatecheck: TCheckBox;
    editUpdatecheckInterval: TEdit;
    updownUpdatecheckInterval: TUpDown;
    chkPreferShowTables: TCheckBox;
    chkUpdateCheckBuilds: TCheckBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure Apply(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FontsChange(Sender: TObject);
    procedure CallColorDialog(Sender: TObject);
    procedure DataFontsChange(Sender: TObject);
    procedure anyUpDownLimitChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure btnOpenLogFolderClick(Sender: TObject);
    procedure chkUpdatecheckClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation
uses childwin, main, helpers;
{$R *.DFM}


procedure Toptionsform.ButtonCancelClick(Sender: TObject);
begin
  // Cancel
  ModalResult := mrCancel;
end;

procedure Toptionsform.Modified(Sender: TObject);
begin
  // Modified
  ButtonApply.Enabled := true;
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
  reg.WriteBool(REGNAME_AUTORECONNECT, CheckBoxAutoReconnect.Checked);
  reg.WriteBool(REGNAME_CONVERTHTMLENTITIES, CheckBoxConvertHTMLEntities.Checked);
  reg.WriteBool(REGNAME_RESTORELASTUSEDDB, CheckBoxRestoreLastUsedDB.Checked);
  reg.WriteString(REGNAME_FONTNAME, ComboBoxFonts.Text);
  reg.WriteInteger(REGNAME_FONTSIZE, UpDownFontSize.Position);
  reg.WriteInteger(REGNAME_LOGSQLNUM, updownLogSQLNum.Position);
  reg.WriteInteger(REGNAME_LOGSQLWIDTH, updownLogSnip.Position);
  reg.WriteString(REGNAME_SQLCOLKEYATTRI, colortostring(pnlKeywords.Color));
  reg.WriteString(REGNAME_SQLCOLFUNCTIONATTRI, colortostring(pnlFunctions.Color));
  reg.WriteString(REGNAME_SQLCOLDATATYPEATTRI, colortostring(pnlDatatypes.Color));
  reg.WriteString(REGNAME_SQLCOLNUMBERATTRI, colortostring(pnlNumeric.Color));
  reg.WriteString(REGNAME_SQLCOLSTRINGATTRI, colortostring(pnlString.Color));
  reg.WriteString(REGNAME_SQLCOLCOMMENTATTRI, colortostring(pnlComments.Color));
  reg.WriteString(REGNAME_SQLCOLTABLENAMEATTRI, colortostring(pnlTablenames.Color));
  reg.WriteString(REGNAME_SQLCOLACTIVELINE, ColorToString(pnlActiveLine.Color));
  reg.WriteString(REGNAME_CSV_SEPARATOR, Edit1.Text);
  reg.WriteString(REGNAME_CSV_ENCLOSER, Edit2.Text);
  reg.WriteString(REGNAME_CSV_TERMINATOR, Edit3.Text);
  reg.WriteInteger(REGNAME_DEFAULTCOLWIDTH, updownDefaultColWidth.Position);
  reg.WriteString(REGNAME_DATAFONTNAME, Panel8.Font.Name);
  reg.WriteInteger(REGNAME_DATAFONTSIZE, UpDownDataFontSize.Position);
  reg.WriteBool(REGNAME_REMEMBERFILTERS, chkRememberFilters.Checked);
  reg.WriteBool(REGNAME_LOGTOFILE, chkLogToFile.Checked);
  reg.WriteBool(REGNAME_DO_UPDATECHECK, chkUpdatecheck.Checked);
  reg.WriteBool(REGNAME_DO_UPDATECHECK_BUILDS, chkUpdatecheckBuilds.Checked);
  reg.WriteInteger(REGNAME_UPDATECHECK_INTERVAL, updownUpdatecheckInterval.Position);
  reg.WriteBool(REGNAME_PREFER_SHOWTABLES, chkPreferShowTables.Checked);

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
    cwin.SynMemoQuery.Font := self.Panel1.Font;
    cwin.SynMemoSQLLog.Font := self.Panel1.Font;
    cwin.SynMemoProcessView.Font := self.Panel1.Font;
    cwin.SynMemoFilter.Font := self.Panel1.Font;
    cwin.SynSQLSyn1.KeyAttri.Foreground := self.pnlKeywords.Color;
    cwin.SynSQLSyn1.FunctionAttri.Foreground := self.pnlFunctions.Color;
    cwin.SynSQLSyn1.DataTypeAttri.Foreground := self.pnlDatatypes.Color;
    cwin.SynSQLSyn1.NumberAttri.Foreground := self.pnlNumeric.Color;
    cwin.SynSQLSyn1.StringAttri.Foreground := self.pnlString.Color;
    cwin.SynSQLSyn1.CommentAttri.Foreground := self.pnlComments.Color;
    cwin.SynSQLSyn1.TablenameAttri.Foreground := self.pnlTablenames.Color;
    cwin.SynMemoQuery.ActiveLineColor := self.pnlActiveLine.Color;
    cwin.DataGrid.Font := self.Panel8.font;
    cwin.QueryGrid.Font := self.Panel8.font;
    cwin.DataGrid.Repaint;
    cwin.QueryGrid.Repaint;
    cwin.prefRememberFilters := chkRememberFilters.Checked;
    cwin.prefLogsqlnum := self.updownLogSQLNum.Position;
    cwin.prefLogSqlWidth := self.updownLogSnip.Position;
    cwin.TrimSQLLog;
    if chkLogToFile.Checked then
      cwin.ActivateFileLogging
    else if cwin.prefLogToFile then
      cwin.DeactivateFileLogging;
    btnOpenLogFolder.Enabled := DirectoryExists(DirnameSessionLogs);
    cwin.prefDefaultColWidth := updownDefaultColWidth.Position;
    cwin.prefCSVSeparator := self.Edit1.text;
    cwin.prefCSVEncloser := self.Edit2.text;
    cwin.prefCSVTerminator := self.Edit3.text;
    cwin.prefConvertHTMLEntities := self.CheckBoxConvertHTMLEntities.Checked;
    cwin.prefPreferShowTables := chkPreferShowTables.Checked;
  end;

  // Settings have been applied, send a signal to the user
  ButtonApply.Enabled := false;

  Screen.Cursor := crDefault;
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
  fontname : String;
  fontsize : Integer;
  datafontname : String;
  datafontsize : Integer;
begin
  screen.Cursor := crHourGlass;

  // Read and display values
  fontname := Mainform.GetRegValue(REGNAME_FONTNAME, DEFAULT_FONTNAME);
  fontsize := Mainform.GetRegValue(REGNAME_FONTSIZE, DEFAULT_FONTSIZE);
  datafontname := Mainform.GetRegValue(REGNAME_DATAFONTNAME, DEFAULT_DATAFONTNAME);
  datafontsize := Mainform.GetRegValue(REGNAME_DATAFONTSIZE, DEFAULT_DATAFONTSIZE);
  CheckBoxAutoReconnect.Checked := Mainform.GetRegValue(REGNAME_AUTORECONNECT, DEFAULT_AUTORECONNECT);
  CheckBoxConvertHTMLEntities.Checked := Mainform.GetRegValue(REGNAME_CONVERTHTMLENTITIES, DEFAULT_CONVERTHTMLENTITIES);
  CheckBoxRestoreLastUsedDB.Checked := Mainform.GetRegValue(REGNAME_RESTORELASTUSEDDB, DEFAULT_RESTORELASTUSEDDB);
  updownLogSQLNum.Position := Mainform.GetRegValue(REGNAME_LOGSQLNUM, DEFAULT_LOGSQLNUM);
  updownLogSnip.Position := Mainform.GetRegValue(REGNAME_LOGSQLWIDTH, DEFAULT_LOGSQLWIDTH);
  chkUpdatecheck.Checked := Mainform.GetRegValue(REGNAME_DO_UPDATECHECK, DEFAULT_DO_UPDATECHECK);
  chkUpdatecheckBuilds.Checked := Mainform.GetRegValue(REGNAME_DO_UPDATECHECK_BUILDS, DEFAULT_DO_UPDATECHECK_BUILDS);
  updownUpdatecheckInterval.Position := Mainform.GetRegValue(REGNAME_UPDATECHECK_INTERVAL, DEFAULT_UPDATECHECK_INTERVAL);
  chkUpdatecheckClick(Sender);
  chkPreferShowTables.Checked := Mainform.GetRegValue(REGNAME_PREFER_SHOWTABLES, DEFAULT_PREFER_SHOWTABLES);

  // Default Column-Width in DBGrids:
  updownDefaultColWidth.Position := Mainform.GetRegValue(REGNAME_DEFAULTCOLWIDTH, DEFAULT_DEFAULTCOLWIDTH);
  // Color-coding:
  pnlKeywords.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLKEYATTRI, ColorToString(DEFAULT_SQLCOLKEYATTRI)));
  pnlFunctions.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLFUNCTIONATTRI, ColorToString(DEFAULT_SQLCOLFUNCTIONATTRI)));
  pnlDatatypes.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLDATATYPEATTRI, ColorToString(DEFAULT_SQLCOLDATATYPEATTRI)));
  pnlNumeric.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLNUMBERATTRI, ColorToString(DEFAULT_SQLCOLNUMBERATTRI)));
  pnlString.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLSTRINGATTRI, ColorToString(DEFAULT_SQLCOLSTRINGATTRI)));
  pnlComments.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLCOMMENTATTRI, ColorToString(DEFAULT_SQLCOLCOMMENTATTRI)));
  pnlTablenames.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLTABLENAMEATTRI, ColorToString(DEFAULT_SQLCOLTABLENAMEATTRI)));
  pnlActiveLine.Color := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLACTIVELINE, ColorToString(DEFAULT_SQLCOLACTIVELINE)));
  // CSV-Options:
  Edit1.Text := Mainform.GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  Edit2.Text := Mainform.GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  Edit3.Text := Mainform.GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  // Remember data pane filters across sessions
  chkRememberFilters.Checked := Mainform.GetRegValue(REGNAME_REMEMBERFILTERS, DEFAULT_REMEMBERFILTERS);
  // Log to file
  chkLogToFile.Checked := Mainform.GetRegValue(REGNAME_LOGTOFILE, DEFAULT_LOGTOFILE);
  btnOpenLogFolder.Enabled := DirectoryExists(DirnameSessionLogs);

  // SQL-Appearance:
  EnumFontFamilies(Canvas.Handle,  // HDC of Device-Context.
                   nil,            // Name of Font-Family (PChar)
                   @EnumFixedProc, // Address of Callback-Function
                   LPARAM(Pointer(ComboBoxFonts.Items))); // customized data

  ComboBoxFonts.ItemIndex := ComboBoxFonts.Items.IndexOf(fontname);
  UpDownFontSize.Position := fontsize;
  with Panel1.Font do begin
    Name := fontname;
    Size := fontsize;
  end;

  // Data-Appearance:
  with ComboBoxDataFonts do begin
    Items := Screen.Fonts;
    ItemIndex := Items.IndexOf(datafontname);
  end;
  UpDownDataFontSize.Position := datafontsize;
  with Panel8.Font do begin
    Name := datafontname;
    Size := datafontsize;
  end;


  ButtonApply.Enabled := false;
  screen.Cursor := crdefault;
end;



procedure Toptionsform.ButtonOKClick(Sender: TObject);
begin
  Apply(self);
  ModalResult := mrOK;
end;

procedure Toptionsform.FontsChange(Sender: TObject);
begin
  with Panel1.Font do begin
    Name := ComboBoxFonts.Items[ComboBoxFonts.ItemIndex];
    Size := UpDownFontSize.Position;
  end;
  Modified(self);
end;


procedure Toptionsform.CallColorDialog(Sender: TObject);
begin
  colordialog1.Color := (sender as TPanel).Color;
  if ColorDialog1.Execute then
  begin
    (sender as TPanel).Color := ColorDialog1.Color;
    modified(self);
  end;
end;

procedure Toptionsform.DataFontsChange(Sender: TObject);
begin
  with Panel8.Font do begin
    Name := ComboBoxDataFonts.Text;
//    Charset := GREEK_CHARSET;
    Size := UpDownDataFontSize.Position;
  end;
  Modified(self);
end;

procedure Toptionsform.anyUpDownLimitChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  modified(sender);
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
  Modified(sender);
end;

end.
