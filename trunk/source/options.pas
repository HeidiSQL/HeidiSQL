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
    Label27: TLabel;
    Panel9: TPanel;
    CheckBoxlimit: TCheckBox;
    EditLimit: TEdit;
    UpDownLimit: TUpDown;
    Label26: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label28: TLabel;
    pnlTablenames: TPanel;
    updownLogSQLNum: TUpDown;
    editLogSQLNum: TEdit;
    editDefaultColWidth: TEdit;
    updownDefaultColWidth: TUpDown;
    CheckBoxRestoreLastUsedDB: TCheckBox;
    chkRememberFilters: TCheckBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure Apply(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FontsChange(Sender: TObject);
    procedure CallColorDialog(Sender: TObject);
    procedure DataFontsChange(Sender: TObject);
    procedure CheckBoxlimitClick(Sender: TObject);
    procedure anyUpDownLimitChanging(Sender: TObject;
      var AllowChange: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function OptionsWindow (AOwner : TComponent; Flags : String = '') : Boolean;

var
  fontname : String = 'Courier New';
  fontsize : Integer = 9;
  datafontname : String = 'MS SANS SERIF';
  datafontcharset : String = '';
  datafontsize : Integer = 8;
  AutoReconnect : Boolean = false;

implementation
uses childwin, main;
{$R *.DFM}


function OptionsWindow (AOwner : TComponent; Flags : String = '') : Boolean;
var
  f : Toptionsform;
begin
  f := Toptionsform.Create(AOwner);
  Result := (f.ShowModal=mrOK); 
  FreeAndNil (f);
end;


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
begin
  Screen.Cursor := crHourGlass;

  // Open registry key
  reg := TRegistry.Create;
  reg.OpenKey(REGPATH, true);

  // Save values
  reg.WriteBool('AutoReconnect', CheckBoxAutoReconnect.Checked);
  reg.WriteBool('ConvertHTMLEntities', CheckBoxConvertHTMLEntities.Checked);
  reg.WriteBool('RestoreLastUsedDB', CheckBoxRestoreLastUsedDB.Checked);
  reg.WriteString('FontName', ComboBoxFonts.Text);
  reg.WriteInteger('FontSize', UpDownFontSize.Position);
  reg.WriteInteger('logsqlnum', updownLogSQLNum.Position);
  reg.WriteString('SQLColKeyAttri', colortostring(pnlKeywords.Color));
  reg.WriteString('SQLColFunctionAttri', colortostring(pnlFunctions.Color));
  reg.WriteString('SQLColDataTypeAttri', colortostring(pnlDatatypes.Color));
  reg.WriteString('SQLColNumberAttri', colortostring(pnlNumeric.Color));
  reg.WriteString('SQLColStringAttri', colortostring(pnlString.Color));
  reg.WriteString('SQLColCommentAttri', colortostring(pnlComments.Color));
  reg.WriteString('SQLColTablenameAttri', colortostring(pnlTablenames.Color));
  reg.WriteString('CSVSeparator', Edit1.Text);
  reg.WriteString('CSVEncloser', Edit2.Text);
  reg.WriteString('CSVTerminator', Edit3.Text);
  reg.WriteInteger('DefaultColWidth', updownDefaultColWidth.Position);
  reg.WriteBool('DataLimit', CheckBoxLimit.Checked);
  reg.WriteInteger('DataLimitEnd', UpDownLimit.Position);
  reg.WriteString('DataFontName', Panel8.Font.Name);
  reg.WriteInteger('DataFontSize', UpDownDataFontSize.Position);
  reg.WriteString('DataNullBackground', ColorToString(Panel9.color));
  reg.WriteBool('RememberFilters', chkRememberFilters.Checked);

  // Close registry key
  reg.CloseKey;
  reg.Free;

  // Set relevant properties in childwin
  cwin := Mainform.Childwin;
  if cwin <> nil then
  begin
    cwin.SynMemoQuery.Font := self.Panel1.Font;
    cwin.SynMemoSQLLog.Font := self.Panel1.Font;
    cwin.SynSQLSyn1.KeyAttri.Foreground := self.pnlKeywords.Color;
    cwin.SynSQLSyn1.FunctionAttri.Foreground := self.pnlFunctions.Color;
    cwin.SynSQLSyn1.DataTypeAttri.Foreground := self.pnlDatatypes.Color;
    cwin.SynSQLSyn1.NumberAttri.Foreground := self.pnlNumeric.Color;
    cwin.SynSQLSyn1.StringAttri.Foreground := self.pnlString.Color;
    cwin.SynSQLSyn1.CommentAttri.Foreground := self.pnlComments.Color;
    cwin.SynSQLSyn1.TablenameAttri.Foreground := self.pnlTablenames.Color;
    while cwin.SynMemoSQLLog.Lines.Count > updownLogSQLNum.Position do
      cwin.SynMemoSQLLog.Lines.Delete(0);
    cwin.gridData.Font := self.Panel8.font;
    cwin.gridQuery.Font := self.Panel8.font;
    cwin.DBMemo1.Font := self.Panel8.font;
    cwin.gridData.Refresh;
    // Set the grid-cells to always-edit-mode
    cwin.gridData.Options := cwin.gridData.Options + [dgAlwaysShowEditor];
    cwin.gridQuery.Options := cwin.gridQuery.Options + [dgAlwaysShowEditor];
    cwin.prefRememberFilters := chkRememberFilters.Checked;
    cwin.prefLogsqlnum := self.updownLogSQLNum.Position;
    cwin.prefDefaultColWidth := updownDefaultColWidth.Position;
    cwin.prefCSVSeparator := self.Edit1.text;
    cwin.prefCSVEncloser := self.Edit2.text;
    cwin.prefCSVTerminator := self.Edit3.text;
    cwin.prefConvertHTMLEntities := self.CheckBoxConvertHTMLEntities.Checked;
    cwin.prefDataNullBackground := Panel9.color;
  end;

  // Set relevant properties in mainform
  Mainform.CheckBoxLimit.Checked := CheckBoxLimit.Checked;
  Mainform.UpDownLimitEnd.Position := UpDownLimit.Position;

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
  reg : TRegistry;
begin
  screen.Cursor := crHourGlass;

  // Open registry key
  reg := TRegistry.Create;
  reg.OpenKey(REGPATH, true);

  // Read and display values
  if reg.ValueExists('FontName') then
    fontname := reg.ReadString('FontName');
  if reg.ValueExists('FontSize') then
    fontsize := reg.ReadInteger('FontSize');
  if reg.ValueExists('DataFontName') then
    datafontname := reg.ReadString('DataFontName');
  if reg.ValueExists('DataFontCharset') then
    datafontcharset := reg.ReadString('DataFontCharset');
  if reg.ValueExists('DataFontSize') then
    datafontsize := reg.ReadInteger('DataFontSize');
  if reg.ValueExists('AutoReconnect') then
    AutoReconnect := reg.ReadBool('AutoReconnect');
  if reg.ValueExists('ConvertHTMLEntities') then
    CheckBoxConvertHTMLEntities.Checked := reg.ReadBool('ConvertHTMLEntities');
  if reg.ValueExists('RestoreLastUsedDB') then
    CheckBoxRestoreLastUsedDB.Checked := reg.ReadBool('RestoreLastUsedDB');
  if reg.ValueExists('DataLimit') then
    CheckBoxLimit.Checked := reg.ReadBool('DataLimit');
  if reg.ValueExists('DataLimitEnd') then
    UpDownLimit.Position := reg.ReadInteger('DataLimitEnd');
  CheckBoxLimit.OnClick(self);
  if reg.ValueExists('logsqlnum') then
    updownLogSQLNum.Position := reg.ReadInteger('logsqlnum');
  // Default Column-Width in DBGrids:
  if reg.ValueExists('DefaultColWidth') then
    updownDefaultColWidth.Position := reg.ReadInteger('DefaultColWidth');

  // Color-coding:
  if reg.ValueExists('SQLColKeyAttri') then
    pnlKeywords.Color := StringToColor(reg.readstring('SQLColKeyAttri'))
  else
    pnlKeywords.Color := clBlue;
  if reg.ValueExists('SQLColFunctionAttri') then
    pnlFunctions.Color := StringToColor(reg.readstring('SQLColFunctionAttri'))
  else
    pnlFunctions.Color := clNavy;
  if reg.ValueExists('SQLColDataTypeAttri') then
    pnlDatatypes.Color := StringToColor(reg.readstring('SQLColDataTypeAttri'))
  else
    pnlDatatypes.Color := clMaroon;
  if reg.ValueExists('SQLColNumberAttri') then
    pnlNumeric.Color := StringToColor(reg.readstring('SQLColNumberAttri'))
  else
    pnlNumeric.Color := clPurple;
  if reg.ValueExists('SQLColStringAttri') then
    pnlString.Color := StringToColor(reg.readstring('SQLColStringAttri'))
  else
    pnlString.Color := clGreen;
  if reg.ValueExists('SQLColCommentAttri') then
    pnlComments.Color := StringToColor(reg.readstring('SQLColCommentAttri'))
  else
    pnlComments.Color := clGray;
  if reg.ValueExists('SQLColTablenameAttri') then
    pnlTablenames.Color := StringToColor(reg.readstring('SQLColTablenameAttri'))
  else
    pnlTablenames.Color := clFuchsia;


  Edit1.Text := ',';
  Edit2.Text := '';
  Edit3.Text := '\r\n';

  // CSV-Options:
  if reg.ValueExists('CSVSeparator') then
    Edit1.Text := reg.ReadString('CSVSeparator');
  if reg.ValueExists('CSVEncloser') then
    Edit2.Text := reg.ReadString('CSVEncloser');
  if reg.ValueExists('CSVTerminator') then
    Edit3.Text := reg.ReadString('CSVTerminator');

  if reg.ValueExists('DataNullBackground') then
    Panel9.Color := StringToColor(reg.ReadString('DataNullBackground'))
  else
    Panel9.Color := clAqua;

  // Remember data pane filters across sessions
  if reg.ValueExists('RememberFilters') then
    chkRememberFilters.Checked := reg.ReadBool('RememberFilters');

  // Close registry key  
  reg.CloseKey;
  reg.Free;

  // Miscellaneous:
  CheckBoxAutoReconnect.Checked := AutoReconnect;

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

procedure Toptionsform.CheckBoxlimitClick(Sender: TObject);
begin
  UpDownLimit.Enabled := CheckBoxLimit.Checked;
  EditLimit.Enabled := CheckBoxLimit.Checked;
  Modified(sender);
end;

procedure Toptionsform.anyUpDownLimitChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  modified(sender);
end;

end.
