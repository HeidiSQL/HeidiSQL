unit loaddata;


// -------------------------------------
// Load Textfile into table
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, CheckLst,
  SynRegExpr, Buttons, ExtCtrls, ToolWin,
  mysql_connection;

type
  Tloaddataform = class(TForm)
    btnImport: TButton;
    btnCancel: TButton;
    OpenDialogCSVFile: TOpenDialog;
    PageControlMain: TPageControl;
    tabSource: TTabSheet;
    tabDestination: TTabSheet;
    lblDatabase: TLabel;
    comboDatabase: TComboBox;
    lblTable: TLabel;
    comboTable: TComboBox;
    lblColumns: TLabel;
    chklistColumns: TCheckListBox;
    ToolBarColMove: TToolBar;
    btnColUp: TToolButton;
    btnColDown: TToolButton;
    grpOptions: TGroupBox;
    chkLowPriority: TCheckBox;
    chkReplace: TCheckBox;
    chkIgnore: TCheckBox;
    lblDuplicates: TLabel;
    grpFilename: TGroupBox;
    editFilename: TButtonedEdit;
    grpFields: TGroupBox;
    lblFieldTerminater: TLabel;
    lblFieldEncloser: TLabel;
    lblFieldEscaper: TLabel;
    editFieldEscaper: TEdit;
    editFieldEncloser: TEdit;
    editFieldTerminator: TEdit;
    chkFieldsEnclosedOptionally: TCheckBox;
    grpLines: TGroupBox;
    lblIgnoreLinesCount: TLabel;
    updownIgnoreLines: TUpDown;
    editIgnoreLines: TEdit;
    editLineTerminator: TEdit;
    lblLineTerminator: TLabel;
    lblIgnoreLines: TLabel;
    lblFilename: TLabel;
    comboCharset: TComboBox;
    lblCharset: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure editFilenameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure comboDatabaseChange(Sender: TObject);
    procedure comboTableChange(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure chkReplaceClick(Sender: TObject);
    procedure chkIgnoreClick(Sender: TObject);
    procedure btnColUpClick(Sender: TObject);
    procedure btnColDownClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses Main, helpers;

{$R *.DFM}



procedure Tloaddataform.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
  // Restore settings
  editFilename.Text := GetRegValue(REGNAME_CSV_FILENAME, '');
  editFieldTerminator.Text := GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  editFieldEncloser.Text := GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  editLineTerminator.Text := GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  chkFieldsEnclosedOptionally.Checked :=  GetRegValue(REGNAME_CSV_ENCLOPTION, chkFieldsEnclosedOptionally.Checked);
  editFieldEscaper.Text := GetRegValue(REGNAME_CSV_ESCAPER, editFieldEscaper.Text);
  updownIgnoreLines.Position := GetRegValue(REGNAME_CSV_IGNORELINES, updownIgnoreLines.Position);
  chkLowPriority.Checked := GetRegValue(REGNAME_CSV_LOWPRIO, chkLowPriority.Checked);
  chkReplace.Checked := GetRegValue(REGNAME_CSV_REPLACE, chkReplace.Checked);
  chkIgnore.Checked := GetRegValue(REGNAME_CSV_IGNORE, chkIgnore.Checked);
end;


procedure Tloaddataform.FormDestroy(Sender: TObject);
begin
  // Save settings
  OpenRegistry;
  MainReg.WriteString(REGNAME_CSV_FILENAME, editFilename.Text);
  MainReg.WriteString(REGNAME_CSV_SEPARATOR, editFieldTerminator.Text);
  MainReg.WriteString(REGNAME_CSV_ENCLOSER, editFieldEncloser.Text);
  MainReg.WriteString(REGNAME_CSV_TERMINATOR, editLineTerminator.Text);
  MainReg.WriteBool(REGNAME_CSV_ENCLOPTION, chkFieldsEnclosedOptionally.Checked);
  MainReg.WriteString(REGNAME_CSV_ESCAPER, editFieldEscaper.Text);
  MainReg.WriteInteger(REGNAME_CSV_IGNORELINES, updownIgnoreLines.Position);
  MainReg.WriteBool(REGNAME_CSV_LOWPRIO, chkLowPriority.Checked);
  MainReg.WriteBool(REGNAME_CSV_REPLACE, chkReplace.Checked);
  MainReg.WriteBool(REGNAME_CSV_IGNORE, chkIgnore.Checked);
end;


procedure Tloaddataform.FormShow(Sender: TObject);
begin
  // read dbs and Tables from treeview
  comboDatabase.Items.Clear;
  comboDatabase.Items.Assign(Mainform.AllDatabases);
  comboDatabase.ItemIndex := comboDatabase.Items.IndexOf( Mainform.ActiveDatabase );
  if comboDatabase.ItemIndex = -1 then
    comboDatabase.ItemIndex := 0;
  comboDatabaseChange(self);
end;


procedure Tloaddataform.comboDatabaseChange(Sender: TObject);
var
  count, i, selCharsetIndex, v: Integer;
  DBObjects: TDBObjectList;
  seldb, seltable, dbcreate: String;
  rx: TRegExpr;
  DefCharset: String;
  CharsetTable: TMySQLQuery;
begin
  // read tables from db
  comboTable.Items.Clear;
  seldb := Mainform.ActiveDatabase;
  seltable := Mainform.SelectedTable.Name;
  DBObjects := Mainform.Connection.GetDBObjects(comboDatabase.Text);
  for i:=0 to DBObjects.Count-1 do begin
    if DBObjects[i].NodeType in [lntTable, lntView] then
      comboTable.Items.Add(DBObjects[i].Name);
    count := comboTable.Items.Count-1;
    if (comboDatabase.Text = seldb) and (comboTable.Items[count] = seltable) then
      comboTable.ItemIndex := count;
  end;
  if comboTable.ItemIndex = -1 then
    comboTable.ItemIndex := 0;

  comboTableChange(self);

  selCharsetIndex := comboCharset.ItemIndex;
  comboCharset.Enabled := False;
  comboCharset.Clear;
  v := Mainform.Connection.ServerVersionInt;
  if ((v >= 50038) and (v < 50100)) or (v >= 50117) then begin
    comboCharset.Enabled := True;
    // Detect db charset
    DefCharset := 'Let server/database decide';
    dbcreate := Mainform.Connection.GetVar('SHOW CREATE DATABASE '+Mainform.mask(comboDatabase.Text), 1);
    rx := TRegExpr.Create;
    rx.ModifierG := True;
    rx.Expression := 'CHARACTER SET (\w+)';
    if rx.Exec(dbcreate) then
      DefCharset := DefCharset + ' ('+rx.Match[1]+')';
    comboCharset.Items.Add(DefCharset);
    CharsetTable := Mainform.Connection.CharsetTable;
    CharsetTable.First;
    while not CharsetTable.Eof do begin
      comboCharset.Items.Add(CharsetTable.Col(1) + ' ('+CharsetTable.Col(0)+')');
      if CharsetTable.Col(0) = 'utf8' then begin
        i := comboCharset.Items.Count-1;
        comboCharset.Items[i] := comboCharset.Items[i] + ' - '+APPNAME+' output';
        if selCharsetIndex = -1 then
          selCharsetIndex := i;
      end;
      CharsetTable.Next;
    end;
    comboCharset.ItemIndex := selCharsetIndex;
  end else begin
    comboCharset.Items.Add('Unsupported by this server');
    comboCharset.ItemIndex := 0;
  end;
end;


procedure Tloaddataform.comboTableChange(Sender: TObject);
begin
  // fill columns:
  chklistColumns.Items.Clear;
  if (comboDatabase.Text <> '') and (comboTable.Text <> '') then
    chklistColumns.Items.Text := Mainform.Connection.GetCol('SHOW FIELDS FROM ' + mainform.mask(comboDatabase.Text) + '.' +  mainform.mask(comboTable.Text)).Text;

  // select all:
  ToggleCheckListBox( chklistColumns, True );

  // Ensure valid state of Import-Button
  editFilenameChange(sender);  
end;


procedure Tloaddataform.btnImportClick(Sender: TObject);
var
  query : String;
  col   : TStringList;
  i     : Integer;

  // Correctly escape field-terminator, line-terminator or encloser
  // and take care of already escaped characters like \t
  // See bug 1827494
  function escOptionString( str: String ): String;
  begin
    Result := '''' + StringReplace(str, '''', '\''', [rfReplaceAll]) + '''';
  end;
begin

  query := 'LOAD DATA ';

  if chkLowPriority.Checked then
    query := query + 'LOW_PRIORITY ';

  query := query + 'LOCAL INFILE ' + esc(editFilename.Text) + ' ';
  if chkReplace.Checked then
    query := query + 'REPLACE '
  else if chkIgnore.Checked then
    query := query + 'IGNORE ';
  query := query + 'INTO TABLE ' + Mainform.Mask(comboDatabase.Text) + '.' +  Mainform.Mask(comboTable.Text) + ' ';

  if comboCharset.ItemIndex > 0 then begin
    Mainform.Connection.CharsetTable.RecNo := comboCharset.ItemIndex-1;
    query := query + 'CHARACTER SET '+Mainform.Connection.CharsetTable.Col(0)+' ';
  end;

  // Fields:
  if (editFieldTerminator.Text <> '') or (editFieldEncloser.Text <> '') or (editFieldEscaper.Text <> '') then
    query := query + 'FIELDS ';
  if editFieldTerminator.Text <> '' then
    query := query + 'TERMINATED BY ' + escOptionString(editFieldTerminator.Text) + ' ';
  if editFieldEncloser.Text <> '' then
  begin
    if chkFieldsEnclosedOptionally.Checked then
      query := query + 'OPTIONALLY ';
    query := query + 'ENCLOSED BY ' + escOptionString(editFieldEncloser.Text) + ' ';
  end;
  if editFieldEscaper.Text <> '' then
    query := query + 'ESCAPED BY ' + escOptionString(editFieldEscaper.Text) + ' ';

  // Lines:
  if editLineTerminator.Text <> '' then
    query := query + 'LINES TERMINATED BY ' + escOptionString(editLineTerminator.Text) + ' ';
  if updownIgnoreLines.Position > 0 then
    query := query + 'IGNORE ' + inttostr(updownIgnoreLines.Position) + ' LINES ';

  col := TStringList.Create;
  for i:=0 to chklistColumns.Items.Count - 1 do
  begin
    if chklistColumns.checked[i] then
      col.Add(Mainform.Mask( chklistColumns.Items[i] ));
  end;

//  if col.Count < ColumnsCheckListBox.Items.Count then
  query := query + '(' + implodestr(',', col) + ')';

  try
    Mainform.Connection.Query(query);
  except
    on E:EDatabaseError do begin
      MessageDlg(E.Message, mtError, [mbOk], 0);
      ModalResult := mrNone;
    end;
  end;
end;

procedure Tloaddataform.btnOpenFileClick(Sender: TObject);
begin
  if OpenDialogCSVFile.Execute then
    editfilename.Text := OpenDialogCSVFile.FileName;
end;

procedure Tloaddataform.chkReplaceClick(Sender: TObject);
begin
  if chkReplace.Checked then
    chkIgnore.checked := false;
end;

procedure Tloaddataform.chkIgnoreClick(Sender: TObject);
begin
  if chkIgnore.Checked then
    chkReplace.checked := false;
end;

procedure Tloaddataform.btnColUpClick(Sender: TObject);
var
  strtemp : String;
  strchecked : boolean;
begin
  // move item up!
  if chklistColumns.ItemIndex > -1 then
  begin
    if chklistColumns.ItemIndex > 0 then
    begin // not first item...
      strtemp := chklistColumns.Items[chklistColumns.ItemIndex-1];
      strchecked := chklistColumns.Checked[chklistColumns.ItemIndex-1];
      // replace old with new item...
      chklistColumns.Items[chklistColumns.ItemIndex-1] := chklistColumns.Items[chklistColumns.ItemIndex];
      chklistColumns.Checked[chklistColumns.ItemIndex-1] := chklistColumns.Checked[chklistColumns.ItemIndex];
      // and set old item to its origin values...
      chklistColumns.Items[chklistColumns.ItemIndex] := strtemp;
      chklistColumns.Checked[chklistColumns.ItemIndex] := strchecked;

      chklistColumns.ItemIndex := chklistColumns.ItemIndex-1;
    end;
  end;
end;

procedure Tloaddataform.btnColDownClick(Sender: TObject);
var
  strtemp : String;
  strchecked : boolean;
begin
  // move item down!
  if chklistColumns.ItemIndex > -1 then
  begin
    if chklistColumns.ItemIndex < chklistColumns.Items.count-1 then
    begin // not last item...
      strtemp := chklistColumns.Items[chklistColumns.ItemIndex+1];
      strchecked := chklistColumns.Checked[chklistColumns.ItemIndex+1];
      // replace old with new item...
      chklistColumns.Items[chklistColumns.ItemIndex+1] := chklistColumns.Items[chklistColumns.ItemIndex];
      chklistColumns.Checked[chklistColumns.ItemIndex+1] := chklistColumns.Checked[chklistColumns.ItemIndex];
      // and set old item to its origin values...
      chklistColumns.Items[chklistColumns.ItemIndex] := strtemp;
      chklistColumns.Checked[chklistColumns.ItemIndex] := strchecked;

      chklistColumns.ItemIndex := chklistColumns.ItemIndex+1;
    end;
  end;
end;



{** Make "OK"-button only clickable if
 - filename is not empty
 - table is selected
 - columnnames could be fetched normally
 - filename exists
}
procedure Tloaddataform.editFilenameChange(Sender: TObject);
begin
  btnImport.Enabled := (editFilename.Text <> '')
    and (chklistColumns.Items.Count > 0)
    and (FileExists(editFilename.Text));
end;


end.
