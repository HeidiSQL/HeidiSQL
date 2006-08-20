unit options;


// -------------------------------------
// HeidiSQL
// Preferences
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Spin, Registry, ExtCtrls;

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
    SpinEditLogSQL: TSpinEdit;
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
    CheckBoxDataAlwaysEditMode: TCheckBox;
    Label27: TLabel;
    Panel9: TPanel;
    CheckBoxlimit: TCheckBox;
    EditLimit: TEdit;
    UpDownLimit: TUpDown;
    Label26: TLabel;
    Label19: TLabel;
    SpinEditDefaultColWidth: TSpinEdit;
    Label20: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure Apply(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FontsChange(Sender: TObject);
    procedure CallColorDialog(Sender: TObject);
    procedure DataFontsChange(Sender: TObject);
    procedure CheckBoxlimitClick(Sender: TObject);
    procedure UpDownLimitChanging(Sender: TObject;
      var AllowChange: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  optionsform: Toptionsform;
  fontname : String = 'Courier New';
  fontsize : Integer = 9;
  datafontname : String = 'MS SANS SERIF';
  datafontcharset : String = '';
  datafontsize : Integer = 8;
  AutoReconnect : Boolean = false;

implementation
uses childwin, main;
{$R *.DFM}


procedure Toptionsform.ButtonCancelClick(Sender: TObject);
begin
  // Cancel
  close;
end;

procedure Toptionsform.Modified(Sender: TObject);
begin
  // Modified
  ButtonApply.Enabled := true;
end;

procedure Toptionsform.Apply(Sender: TObject);
var
  i : Integer;
begin
  // Apply
  Screen.Cursor := crHourGlass;
  with TRegistry.Create do
  begin
    openkey(regpath, true);
    WriteBool('AutoReconnect', CheckBoxAutoReconnect.Checked);
    WriteBool('ConvertHTMLEntities', CheckBoxConvertHTMLEntities.Checked);
    WriteString('FontName', ComboBoxFonts.Text);
    WriteInteger('FontSize', UpDownFontSize.Position);
    WriteInteger('logsqlnum', SpinEditLogSQL.Value);
    WriteString('SQLColKeyAttri', colortostring(pnlKeywords.Color));
    WriteString('SQLColFunctionAttri', colortostring(pnlFunctions.Color));
    WriteString('SQLColDataTypeAttri', colortostring(pnlDatatypes.Color));
    WriteString('SQLColNumberAttri', colortostring(pnlNumeric.Color));
    WriteString('SQLColStringAttri', colortostring(pnlString.Color));
    WriteString('SQLColCommentAttri', colortostring(pnlComments.Color));
    WriteString('CSVSeparator', Edit1.Text);
    WriteString('CSVEncloser', Edit2.Text);
    WriteString('CSVTerminator', Edit3.Text);
    WriteInteger('DefaultColWidth', SpinEditDefaultColWidth.Value);
    WriteBool('DataLimit', CheckBoxLimit.Checked);
    WriteInteger('DataLimitEnd', UpDownLimit.Position);
    WriteString('DataFontName', Panel8.Font.Name);
    WriteInteger('DataFontSize', UpDownDataFontSize.Position);
    WriteString('DataNullBackground', ColorToString(Panel9.color));
  end;
  ButtonApply.Enabled := false;

  // window-specific preferences stored in childwindows
  if Mainform.MDIChildCount > 0 then
  begin
    for i:= 0 to Mainform.MDIChildCount -1 do
    with TMDIChild(Mainform.MDIChildren[i]) do
    begin
      SynMemoQuery.Font := self.Panel1.Font;
      SynMemoSQLLog.Font := self.Panel1.Font;
      SynSQLSyn1.KeyAttri.Foreground := self.pnlKeywords.Color;
      SynSQLSyn1.FunctionAttri.Foreground := self.pnlFunctions.Color;
      SynSQLSyn1.DataTypeAttri.Foreground := self.pnlDatatypes.Color;
      SynSQLSyn1.NumberAttri.Foreground := self.pnlNumeric.Color;
      SynSQLSyn1.StringAttri.Foreground := self.pnlString.Color;
      SynSQLSyn1.CommentAttri.Foreground := self.pnlComments.Color;
      while SynMemoSQLLog.Lines.Count > SpinEditLogSQL.Value do
        SynMemoSQLLog.Lines.Delete(0);
      gridData.Font := self.Panel8.font;
      gridQuery.Font := self.Panel8.font;
      DBMemo1.Font := self.Panel8.font;
      gridData.Refresh;
//      DBMemo1.Font.Charset := tfontcharset(177);
    end;
  end;

  // general preferences stored in mainform
  with Mainform do begin
    logsqlnum := self.SpinEditLogSQL.Value;
    DefaultColWidth := SpinEditDefaultColWidth.value;
    CSVSeparator := self.Edit1.text;
    CSVEncloser := self.Edit2.text;
    CSVTerminator := self.Edit3.text;
    ConvertHTMLEntities := self.CheckBoxConvertHTMLEntities.Checked;
    CheckBoxLimit.Checked := self.CheckBoxLimit.Checked;
    UpDownLimitEnd.Position := UpDownLimit.Position;
    DataAlwaysEditMode := CheckBoxDataAlwaysEditMode.Checked;
    DataNullBackground := Panel9.color;
  end;

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


begin
  screen.Cursor := crHourGlass;

  with TRegistry.Create do begin
    openkey(regpath, true);
    if ValueExists('FontName') then
      fontname := ReadString('FontName');
    if ValueExists('FontSize') then
      fontsize := ReadInteger('FontSize');
    if ValueExists('DataFontName') then
      datafontname := ReadString('DataFontName');
    if ValueExists('DataFontCharset') then
      datafontcharset := ReadString('DataFontCharset');
    if ValueExists('DataFontSize') then
      datafontsize := ReadInteger('DataFontSize');
    if ValueExists('AutoReconnect') then
      AutoReconnect := ReadBool('AutoReconnect');
    if ValueExists('ConvertHTMLEntities') then
      CheckBoxConvertHTMLEntities.Checked := ReadBool('ConvertHTMLEntities');
    if ValueExists('DataLimit') then
      CheckBoxLimit.Checked := ReadBool('DataLimit');
    if ValueExists('DataLimitEnd') then
      UpDownLimit.Position := ReadInteger('DataLimitEnd');
    CheckBoxLimit.OnClick(self);
    if ValueExists('logsqlnum') then
      SpinEditLogSQL.Value := ReadInteger('logsqlnum');
    // Default Column-Width in DBGrids:
    if ValueExists('DefaultColWidth') then
      SpinEditDefaultColWidth.Value := ReadInteger('DefaultColWidth');

    // Color-coding:
    if ValueExists('SQLColKeyAttri') then begin
      pnlKeywords.Color := StringToColor(readstring('SQLColKeyAttri'));
      pnlFunctions.Color := StringToColor(readstring('SQLColFunctionAttri'));
      pnlDatatypes.Color := StringToColor(readstring('SQLColDataTypeAttri'));
      pnlNumeric.Color := StringToColor(readstring('SQLColNumberAttri'));
      pnlString.Color := StringToColor(readstring('SQLColStringAttri'));
      pnlComments.Color := StringToColor(readstring('SQLColCommentAttri'));
    end else begin
      pnlKeywords.Color := clBlue;
      pnlFunctions.Color := clNavy;
      pnlDatatypes.Color := clMaroon;
      pnlNumeric.Color := clPurple;
      pnlString.Color := clGreen;
      pnlComments.Color := clGray;
    end;

    Edit1.Text := ',';
    Edit2.Text := '';
    Edit3.Text := '\r\n';

    // CSV-Options:
    if ValueExists('CSVSeparator') then
      Edit1.Text := ReadString('CSVSeparator');
    if ValueExists('CSVEncloser') then
      Edit2.Text := ReadString('CSVEncloser');
    if ValueExists('CSVTerminator') then
      Edit3.Text := ReadString('CSVTerminator');

    if ValueExists('DataAlwaysEditMode') then
      CheckBoxDataAlwaysEditMode.Checked := ReadBool('DataAlwaysEditMode');
    if ValueExists('DataNullBackground') then
      Panel9.Color := StringToColor(ReadString('DataNullBackground'))
    else
      Panel9.Color := clAqua;

    closekey;
  end;

  // Miscellaneous:
  CheckBoxAutoReconnect.Checked := AutoReconnect;

  // SQL-Appearance:
  EnumFontFamilies(Canvas.Handle,  // HDC des Device-Context.
                   nil,            // Name der Font-Family (PChar)
                   @EnumFixedProc, // Addresse der Callback-Funktion
                   LPARAM(Pointer(ComboBoxFonts.Items))); // Benutzerdef. Daten

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
  Close;
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

procedure Toptionsform.UpDownLimitChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  modified(sender);
end;

end.
