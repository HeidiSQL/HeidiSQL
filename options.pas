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
    Panel2: TPanel;
    ColorDialog1: TColorDialog;
    Label7: TLabel;
    Panel3: TPanel;
    Label8: TLabel;
    Panel4: TPanel;
    Label9: TLabel;
    Panel5: TPanel;
    Label10: TLabel;
    Panel6: TPanel;
    Label11: TLabel;
    Panel7: TPanel;
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
    SpinEditDefaultColWidth: TSpinEdit;
    Label19: TLabel;
    Label20: TLabel;
    CheckBoxNativeFieldTypes: TCheckBox;
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
    CheckBoxlimit: TCheckBox;
    UpDownLimit: TUpDown;
    EditLimit: TEdit;
    Label26: TLabel;
    CheckBoxDataAlwaysEditMode: TCheckBox;
    Label27: TLabel;
    Panel9: TPanel;
    Label28: TLabel;
    ComboBoxEncoding: TComboBox;
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
    WriteString('SQLColKeyAttri', colortostring(Panel2.Color));
    WriteString('SQLColFunctionAttri', colortostring(Panel3.Color));
    WriteString('SQLColDataTypeAttri', colortostring(Panel4.Color));
    WriteString('SQLColNumberAttri', colortostring(Panel5.Color));
    WriteString('SQLColStringAttri', colortostring(Panel6.Color));
    WriteString('SQLColCommentAttri', colortostring(Panel7.Color));
    WriteString('CSVSeparator', Edit1.Text);
    WriteString('CSVEncloser', Edit2.Text);
    WriteString('CSVTerminator', Edit3.Text);
    WriteInteger('DefaultColWidth', SpinEditDefaultColWidth.Value);
    WriteBool('NativeFieldTypes', CheckBoxNativeFieldTypes.Checked);
    WriteBool('DataLimit', CheckBoxLimit.Checked);
    WriteInteger('DataLimitEnd', UpDownLimit.Position);
    WriteString('DataFontName', Panel8.Font.Name);
    WriteInteger('DataFontSize', UpDownDataFontSize.Position);
    WriteString('DataNullBackground', ColorToString(Panel9.color));
    WriteInteger('DataEncoding', comboboxencoding.ItemIndex);
  end;
  ButtonApply.Enabled := false;

  // window-specific preferences stored in childwindows
  if Mainform.MDIChildCount > 0 then
  begin
    for i:= 0 to Mainform.MDIChildCount -1 do
    with TMDIChild(Mainform.MDIChildren[i]) do
    begin
      SynMemo1.Font := self.Panel1.Font;
      SynMemo2.Font := self.Panel1.Font;
      SynSQLSyn1.KeyAttri.Foreground := self.Panel2.Color;
      SynSQLSyn1.FunctionAttri.Foreground := self.Panel3.Color;
      SynSQLSyn1.DataTypeAttri.Foreground := self.Panel4.Color;
      SynSQLSyn1.NumberAttri.Foreground := self.Panel5.Color;
      SynSQLSyn1.StringAttri.Foreground := self.Panel6.Color;
      SynSQLSyn1.CommentAttri.Foreground := self.Panel7.Color;
      while SynMemo2.Lines.Count > SpinEditLogSQL.Value do
        SynMemo2.Lines.Delete(0);
      DBGrid1.Font := self.Panel8.font;
      DBGrid2.Font := self.Panel8.font;
      DBMemo1.Font := self.Panel8.font;
      DBGrid1.Refresh;
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
    NativeFieldTypes := CheckBoxNativeFieldTypes.Checked;
    CheckBoxLimit.Checked := self.CheckBoxLimit.Checked;
    UpDownLimitEnd.Position := UpDownLimit.Position;
    DataAlwaysEditMode := CheckBoxDataAlwaysEditMode.Checked;
    DataNullBackground := Panel9.color;
  end;

  Screen.Cursor := crDefault;
end;



procedure Toptionsform.FormShow(Sender: TObject);

// ----------- Callback.Funktion für Fixed_Pitch -----------------//
function EnumFixedProc(lpelf: PEnumLogFont;
                       lpntm: PNewTextMetric;
                       FontType: Integer;
                       Data: LPARAM)  // hier steht das Strings-Objekt
                       : Integer;     // 0 = Abbrechen
                       stdcall;       // Wichtig bei allen API-Callbacks
begin
  Result := 1;  // nicht abbrechen
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
    if ValueExists('NativeFieldTypes') then
      CheckBoxNativeFieldTypes.Checked := ReadBool('NativeFieldTypes');
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
      Panel2.Color := StringToColor(readstring('SQLColKeyAttri'));
      Panel3.Color := StringToColor(readstring('SQLColFunctionAttri'));
      Panel4.Color := StringToColor(readstring('SQLColDataTypeAttri'));
      Panel5.Color := StringToColor(readstring('SQLColNumberAttri'));
      Panel6.Color := StringToColor(readstring('SQLColStringAttri'));
      Panel7.Color := StringToColor(readstring('SQLColCommentAttri'));
    end else begin
      Panel2.Color := clBlue;
      Panel3.Color := clNavy;
      Panel4.Color := clMaroon;
      Panel5.Color := clPurple;
      Panel6.Color := clGreen;
      Panel7.Color := clGray;
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

    // Encoding
    if ValueExists('DataEncoding') then
      ComboBoxEncoding.ItemIndex := ReadInteger('DataEncoding');

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
