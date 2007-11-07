unit loaddata;


// -------------------------------------
// Load Textfile into table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, comctrls, Buttons, CheckLst, Registry;

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
    btnColUp: TBitBtn;
    btnColDown: TBitBtn;
    grpOptions: TGroupBox;
    chkLowPriority: TCheckBox;
    chkReplace: TCheckBox;
    chkIgnore: TCheckBox;
    lblDuplicates: TLabel;
    grpFilename: TGroupBox;
    editFilename: TEdit;
    btnOpenFile: TBitBtn;
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
    procedure editFilenameChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
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

  function loaddataWindow(AOwner: TComponent): Boolean;

implementation

uses Main, Childwin, helpers, Db;

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


procedure Tloaddataform.btnCancelClick(Sender: TObject);
begin
  close;
end;

procedure Tloaddataform.FormShow(Sender: TObject);
var
  tn : TTreeNode;
  i : Integer;
  reg : TRegistry;
begin
  // read dbs and Tables from treeview
  comboDatabase.Items.Clear;
  for i:=0 to Mainform.ChildWin.DBTree.Items.Count-1 do
  begin
    tn := Mainform.ChildWin.DBTree.Items[i];
    if tn.Level = 1 then
      comboDatabase.Items.Add(tn.Text);
  end;

  for i:=0 to comboDatabase.Items.Count-1 do
  begin
    if comboDatabase.Items[i] = Mainform.ChildWin.ActiveDatabase then
    begin
      comboDatabase.ItemIndex := i;
      break;
    end;
  end;
  if comboDatabase.ItemIndex = -1 then
    comboDatabase.ItemIndex := 0;

  comboDatabaseChange(self);
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, true) then
  begin
    // filename
    editFilename.Text := reg.ReadString('loadfilename');
    // Use options from CSV export
    editFieldTerminator.Text := reg.ReadString('CSVSeparator');
    editFieldEncloser.Text := reg.ReadString('CSVEncloser');
    editLineTerminator.Text := reg.ReadString('CSVTerminator');
    // Other options
    if reg.ValueExists('CSVImportFieldsEnclosedOptionally') then
      chkFieldsEnclosedOptionally.Checked := reg.ReadBool('CSVImportFieldsEnclosedOptionally');
    editFieldEscaper.Text := reg.ReadString('CSVImportFieldEscaper');
    if reg.ValueExists('CSVImportIgnoreLines') then
      updownIgnoreLines.Position := reg.ReadInteger('CSVImportIgnoreLines');
    if reg.ValueExists('CSVImportLowPriority') then
      chkLowPriority.Checked := reg.ReadBool('CSVImportLowPriority');
    if reg.ValueExists('CSVImportReplace') then
      chkReplace.Checked := reg.ReadBool('CSVImportReplace');
    if reg.ValueExists('CSVImportIgnore') then
      chkIgnore.Checked := reg.ReadBool('CSVImportIgnore');
  end;

end;


procedure Tloaddataform.comboDatabaseChange(Sender: TObject);
var
  i : Integer;
begin
  // read tables from db
  comboTable.Items.Clear;
  comboTable.Items := Mainform.ChildWin.GetCol( 'SHOW TABLES FROM ' + MainForm.mask( comboDatabase.Text ) );
  for i:=0 to comboTable.Items.Count-1 do
  begin
    if comboTable.Items[i] = Mainform.ChildWin.SelectedTable then
    begin
      comboTable.ItemIndex := i;
      break;
    end;
  end;
  if comboTable.ItemIndex = -1 then
    comboTable.ItemIndex := 0;

  comboTableChange(self);
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
      chklistColumns.Items.Add(ds.Fields[0].AsString);
      ds.Next;
    end;
  end;

  // select all:
  ToggleCheckListBox( chklistColumns, True );

  // Ensure valid state of Import-Button
  editFilenameChange(sender);  
end;


procedure Tloaddataform.btnImportClick(Sender: TObject);
var
  query : string;
  col   : TStringList;
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
    reg.WriteString( 'loadfilename', editFilename.Text );
    // Use options from CSV export
    reg.WriteString( 'CSVSeparator', editFieldTerminator.Text );
    reg.WriteString( 'CSVEncloser', editFieldEncloser.Text );
    reg.WriteString( 'CSVTerminator', editLineTerminator.Text );
    // Other options
    reg.WriteBool( 'CSVImportFieldsEnclosedOptionally', chkFieldsEnclosedOptionally.Checked );
    reg.WriteString( 'CSVImportFieldEscaper', editFieldEscaper.Text );
    reg.WriteInteger( 'CSVImportIgnoreLines', updownIgnoreLines.Position );
    reg.WriteBool( 'CSVImportLowPriority', chkLowPriority.Checked );
    reg.WriteBool( 'CSVImportReplace', chkReplace.Checked );
    reg.WriteBool( 'CSVImportIgnore', chkIgnore.Checked );
  end;

  query := 'LOAD DATA ';

  if chkLowPriority.Checked then
    query := query + 'LOW_PRIORITY ';

  query := query + 'LOCAL INFILE ' + esc(editFilename.Text) + ' ';
  if chkReplace.Checked then
    query := query + 'REPLACE '
  else if chkIgnore.Checked then
    query := query + 'IGNORE ';
  query := query + 'INTO TABLE ' + Mainform.Mask(comboDatabase.Text) + '.' +  Mainform.Mask(comboTable.Text) + ' ';

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
