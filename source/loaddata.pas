unit loaddata;


// -------------------------------------
// Load Textfile into table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, comctrls, Buttons, CheckLst, Registry, PngSpeedButton,
  WideStrings, TntCheckLst, TntStdCtrls, db, SynRegExpr;

type
  Tloaddataform = class(TForm)
    btnImport: TButton;
    btnCancel: TButton;
    OpenDialogCSVFile: TOpenDialog;
    PageControlMain: TPageControl;
    tabSource: TTabSheet;
    tabDestination: TTabSheet;
    lblDatabase: TLabel;
    comboDatabase: TTNTComboBox;
    lblTable: TLabel;
    comboTable: TTNTComboBox;
    lblColumns: TLabel;
    chklistColumns: TTNTCheckListBox;
    btnColUp: TPngSpeedButton;
    btnColDown: TPngSpeedButton;
    grpOptions: TGroupBox;
    chkLowPriority: TCheckBox;
    chkReplace: TCheckBox;
    chkIgnore: TCheckBox;
    lblDuplicates: TLabel;
    grpFilename: TGroupBox;
    editFilename: TEdit;
    btnOpenFile: TPngSpeedButton;
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
    dsCharsets: TDataset;
  public
    { Public declarations }
  end;

  function loaddataWindow(AOwner: TComponent): Boolean;

implementation

uses Main, Childwin, helpers;

{$R *.DFM}


{**
  Create form on demand
  @param TComponent Owner of form (should be calling form)
  @return Boolean Form closed using modalresult mrOK
}
function loaddataWindow(AOwner: TComponent): Boolean;
var
  f : Tloaddataform;
begin
  f := Tloaddataform.Create(AOwner);
  Result := (f.ShowModal=mrOK);
  FreeAndNil(f);
end;


{**
  FormCreat
}
procedure Tloaddataform.FormCreate(Sender: TObject);
begin
  // Assign images from main imagelist to speedbuttons
  btnOpenFile.PngImage := Mainform.PngImageListMain.PngImages[52].PngImage;
  btnColUp.PngImage := Mainform.PngImageListMain.PngImages[74].PngImage;
  btnColDown.PngImage := Mainform.PngImageListMain.PngImages[75].PngImage;
  InheritFont(Font);
end;

procedure Tloaddataform.FormShow(Sender: TObject);
begin
  // read dbs and Tables from treeview
  comboDatabase.Items.Clear;
  comboDatabase.Items.Assign(Mainform.ChildWin.Databases);
  comboDatabase.ItemIndex := comboDatabase.Items.IndexOf( Mainform.ChildWin.ActiveDatabase );
  if comboDatabase.ItemIndex = -1 then
    comboDatabase.ItemIndex := 0;

  comboDatabaseChange(self);
  // filename
  editFilename.Text := Mainform.GetRegValue(REGNAME_CSV_FILENAME, '');
  // Use options from CSV export
  editFieldTerminator.Text := Mainform.GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  editFieldEncloser.Text := Mainform.GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  editLineTerminator.Text := Mainform.GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  // Other options
  chkFieldsEnclosedOptionally.Checked :=  Mainform.GetRegValue(REGNAME_CSV_ENCLOPTION, chkFieldsEnclosedOptionally.Checked);
  editFieldEscaper.Text := Mainform.GetRegValue(REGNAME_CSV_ESCAPER, editFieldEscaper.Text);
  updownIgnoreLines.Position := Mainform.GetRegValue(REGNAME_CSV_IGNORELINES, updownIgnoreLines.Position);
  chkLowPriority.Checked := Mainform.GetRegValue(REGNAME_CSV_LOWPRIO, chkLowPriority.Checked);
  chkReplace.Checked := Mainform.GetRegValue(REGNAME_CSV_REPLACE, chkReplace.Checked);
  chkIgnore.Checked := Mainform.GetRegValue(REGNAME_CSV_IGNORE, chkIgnore.Checked);
end;


procedure Tloaddataform.comboDatabaseChange(Sender: TObject);
var
  count, i, selCharsetIndex: Integer;
  ds: TDataset;
  seldb, seltable, dbcreate: WideString;
  rx: TRegExpr;
  DefCharset: String;
begin
  // read tables from db
  comboTable.Items.Clear;
  seldb := Mainform.ChildWin.ActiveDatabase;
  seltable := Mainform.ChildWin.SelectedTable;
  ds := Mainform.ChildWin.FetchDbTableList(comboDatabase.Text);
  while not ds.Eof do begin
    comboTable.Items.Add(ds.Fields[0].AsWideString);
    count := comboTable.Items.Count-1;
    if (comboDatabase.Text = seldb) and (comboTable.Items[count] = seltable) then
      comboTable.ItemIndex := count;
    ds.Next;
  end;
  if comboTable.ItemIndex = -1 then
    comboTable.ItemIndex := 0;

  comboTableChange(self);

  selCharsetIndex := comboCharset.ItemIndex;
  comboCharset.Enabled := False;
  comboCharset.Clear;
  try
    if dsCharsets = nil then
      dsCharsets := Mainform.Childwin.GetResults('SHOW CHARSET');
    comboCharset.Enabled := True;
    // Detect db charset
    DefCharset := 'Let server/database decide';
    dbcreate := Mainform.Childwin.GetVar('SHOW CREATE DATABASE '+Mainform.mask(comboDatabase.Text), 1);
    rx := TRegExpr.Create;
    rx.ModifierG := True;
    rx.Expression := 'CHARACTER SET (\w+)';
    if rx.Exec(dbcreate) then
      DefCharset := DefCharset + ' ('+rx.Match[1]+')';
    comboCharset.Items.Add(DefCharset);
    dsCharsets.First;
    for i:=1 to dsCharsets.RecordCount do begin
      comboCharset.Items.Add(dsCharsets.Fields[1].AsWideString + ' ('+dsCharsets.Fields[0].AsWideString+')');
      if dsCharsets.Fields[0].AsWideString = 'utf8' then begin
        comboCharset.Items[i] := comboCharset.Items[i] + ' - '+APPNAME+' output';
        if selCharsetIndex = -1 then
          selCharsetIndex := i;
      end;
      dsCharsets.Next;
    end;
    comboCharset.ItemIndex := selCharsetIndex;
  except
    // Ignore it when the above statements don't work on pre 4.1 servers.
    // In that case, the combobox is disabled and we load the file without specifying charset.
  end;
end;


procedure Tloaddataform.comboTableChange(Sender: TObject);
var
  i : Integer;
  ds : TDataSet;
begin
  // fill columns:
  chklistColumns.Items.Clear;
  if (comboDatabase.Text <> '') and (comboTable.Text <> '') then begin
    ds := Mainform.ChildWin.GetResults( 'SHOW FIELDS FROM ' + mainform.mask(comboDatabase.Text) + '.' +  mainform.mask(comboTable.Text));
    for i:=1 to ds.RecordCount do
    begin
      chklistColumns.Items.Add(ds.Fields[0].AsWideString);
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
  end;

  // select all:
  ToggleCheckListBox( chklistColumns, True );

  // Ensure valid state of Import-Button
  editFilenameChange(sender);  
end;


procedure Tloaddataform.btnImportClick(Sender: TObject);
var
  query : WideString;
  col   : TWideStringList;
  i     : Integer;
  reg   : TRegistry;

  // Correctly escape field-terminator, line-terminator or encloser
  // and take care of already escaped characters like \t
  // See bug 1827494
  function escOptionString( str: String ): String;
  begin
    Result := '''' + StringReplace(str, '''', '\''', [rfReplaceAll]) + '''';
  end;
begin

  // Save settings
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, true) then
  begin
    // filename
    reg.WriteString( REGNAME_CSV_FILENAME, editFilename.Text );
    // Use options from CSV export
    reg.WriteString( REGNAME_CSV_SEPARATOR, editFieldTerminator.Text );
    reg.WriteString( REGNAME_CSV_ENCLOSER, editFieldEncloser.Text );
    reg.WriteString( REGNAME_CSV_TERMINATOR, editLineTerminator.Text );
    // Other options
    reg.WriteBool( REGNAME_CSV_ENCLOPTION, chkFieldsEnclosedOptionally.Checked );
    reg.WriteString( REGNAME_CSV_ESCAPER, editFieldEscaper.Text );
    reg.WriteInteger( REGNAME_CSV_IGNORELINES, updownIgnoreLines.Position );
    reg.WriteBool( REGNAME_CSV_LOWPRIO, chkLowPriority.Checked );
    reg.WriteBool( REGNAME_CSV_REPLACE, chkReplace.Checked );
    reg.WriteBool( REGNAME_CSV_IGNORE, chkIgnore.Checked );
  end;
  FreeAndNil(reg);

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
    dsCharsets.RecNo := comboCharset.ItemIndex;
    query := query + 'CHARACTER SET '+dsCharsets.Fields[0].AsString+' ';
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

  col := TWideStringList.Create;
  for i:=0 to chklistColumns.Items.Count - 1 do
  begin
    if chklistColumns.checked[i] then
      col.Add(Mainform.Mask( chklistColumns.Items[i] ));
  end;

//  if col.Count < ColumnsCheckListBox.Items.Count then
  query := query + '(' + implodestr(',', col) + ')';

  Mainform.ChildWin.ExecUpdateQuery(query);
  close;
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
