unit loaddata;


// -------------------------------------
// Load Textfile into table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, comctrls, Spin, Buttons, CheckLst, Registry;

type
  Tloaddataform = class(TForm)
    bvlBottom: TBevel;
    btnImport: TButton;
    btnCancel: TButton;
    editFilename: TEdit;
    btnOpenFile: TBitBtn;
    chkLowPriority: TCheckBox;
    lblTable: TLabel;
    comboTable: TComboBox;
    lblFilename: TLabel;
    chkFieldsTerminated: TCheckBox;
    lblFields: TLabel;
    editFieldTerminator: TEdit;
    chkFieldsEnclosed: TCheckBox;
    editFieldEncloser: TEdit;
    chkFieldsEscaped: TCheckBox;
    editFieldEscaper: TEdit;
    chkFieldsEnclosedOptionally: TCheckBox;
    lblLines: TLabel;
    chkLinesTerminated: TCheckBox;
    editLineTerminator: TEdit;
    chkLinesIgnore: TCheckBox;
    lblIgnoreLines: TLabel;
    lblColumns: TLabel;
    chklistColumns: TCheckListBox;
    comboDatabase: TComboBox;
    lblDatabase: TLabel;
    OpenDialogCSVFile: TOpenDialog;
    chkReplace: TCheckBox;
    chkIgnore: TCheckBox;
    lblNote: TLabel;
    btnColUp: TBitBtn;
    btnColDown: TBitBtn;
    editIgnoreLines: TEdit;
    updownIgnoreLines: TUpDown;
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure comboDatabaseChange(Sender: TObject);
    procedure comboTableChange(Sender: TObject);
    procedure chkFieldsTerminatedClick(Sender: TObject);
    procedure chkFieldsEnclosedClick(Sender: TObject);
    procedure chkFieldsEscapedClick(Sender: TObject);
    procedure chkLinesTerminatedClick(Sender: TObject);
    procedure chkLinesIgnoreClick(Sender: TObject);
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
begin
  // read dbs and Tables from treeview
  comboDatabase.Items.Clear;
  for i:=0 to Mainform.ChildWin.DBTree.Items.Count-1 do
  begin
    tn := Mainform.ChildWin.DBTree.Items[i];
    if tn.Level = 1 then
      comboDatabase.Items.Add(tn.Text);
  end;

  with comboDatabase do
  begin
    for i:=0 to Items.Count-1 do
      if Items[i] = Mainform.ChildWin.ActualDatabase then
        ItemIndex := i;
    if ItemIndex = -1 then
      ItemIndex := 0;
  end;

  comboDatabaseChange(self);
  // filename
  with TRegistry.Create do
    if OpenKey(REGPATH, true) then
      editFilename.Text := ReadString('loadfilename');
  if editFilename.Text = '' then
    editFilename.Text := ExtractFilePath(paramstr(0)) + 'import.csv';
  editFilename.Text := stringreplace(editFilename.Text, '\', '/', [rfReplaceAll]);
end;


procedure Tloaddataform.comboDatabaseChange(Sender: TObject);
var
  i : Integer;
begin
  // read tables from db
  comboTable.Items.Clear;
  comboTable.Items := Mainform.ChildWin.GetCol( 'SHOW TABLES FROM ' + MainForm.mask( comboDatabase.Text ) );
  with comboTable do
  begin
    for i:=0 to Items.Count-1 do
      if Items[i] = Mainform.ChildWin.ActualTable then
        ItemIndex := i;
    if ItemIndex = -1 then
      ItemIndex := 0;
  end;

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
  for i:=0 to chklistColumns.Items.Count-1 do
    chklistColumns.checked[i] := true;
end;


procedure Tloaddataform.chkFieldsTerminatedClick(Sender: TObject);
begin
  editFieldTerminator.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.chkFieldsEnclosedClick(Sender: TObject);
begin
  editFieldEncloser.Enabled :=  (sender as TCheckBox).checked;
  chkFieldsEnclosedOptionally.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.chkFieldsEscapedClick(Sender: TObject);
begin
  editFieldEscaper.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.chkLinesTerminatedClick(Sender: TObject);
begin
  editLineTerminator.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.chkLinesIgnoreClick(Sender: TObject);
begin
  updownIgnoreLines.Enabled := (sender as TCheckBox).checked;
  editIgnoreLines.Enabled := (sender as TCheckBox).checked;
  lblIgnoreLines.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.btnImportClick(Sender: TObject);
var
  query : string;
  col   : TStringList;
  i     : Integer;
begin

  with TRegistry.Create do
    if OpenKey(REGPATH, true) then
      WriteString('loadfilename', editFilename.Text);

  query := 'LOAD DATA ';

  if chkLowPriority.Checked then
    query := query + 'LOW_PRIORITY ';

  query := query + 'LOCAL INFILE ' + esc(editFilename.Text) + ' ';
  if chkReplace.Checked then
    query := query + 'REPLACE '
  else if chkIgnore.Checked then
    query := query + 'IGNORE ';
  query := query + 'INTO TABLE ' + comboDatabase.Text + '.' +  comboTable.Text + ' ';

  // Fields:
  if chkFieldsTerminated.Checked or chkFieldsEnclosed.Checked or chkFieldsEscaped.Checked then
    query := query + 'FIELDS ';
  if chkFieldsTerminated.Checked then
    query := query + 'TERMINATED BY ''' + editFieldTerminator.Text + ''' ';
  if chkFieldsEnclosed.Checked then
  begin
    if chkFieldsEnclosedOptionally.Checked then
      query := query + 'OPTIONALLY ';
    query := query + 'ENCLOSED BY ''' + editFieldEncloser.Text + ''' ';
  end;
  if chkFieldsEscaped.Checked then
    query := query + 'ESCAPED BY ''' + editFieldEscaper.Text + ''' ';

  // Lines:
  if chkLinesTerminated.Checked then
    query := query + 'LINES TERMINATED BY ''' + editLineTerminator.Text + ''' ';
  if chkLinesIgnore.Checked then
    query := query + 'IGNORE ' + inttostr(updownIgnoreLines.Position) + ' LINES ';

  col := TStringList.Create;
  for i:=0 to chklistColumns.Items.Count - 1 do
  begin
    if chklistColumns.checked[i] then
      col.Add(chklistColumns.Items[i]);
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
  with chklistColumns do
    if ItemIndex > -1 then begin
      if ItemIndex > 0 then begin // not first item...
        strtemp := items[ItemIndex-1];
        strchecked := checked[ItemIndex-1];
        // replace old with new item...
        items[ItemIndex-1] := items[ItemIndex];
        checked[ItemIndex-1] := checked[ItemIndex];
        // and set old item to its origin values...
        items[ItemIndex] := strtemp;
        checked[ItemIndex] := strchecked;

        ItemIndex := ItemIndex-1;
      end;
    end;
end;

procedure Tloaddataform.btnColDownClick(Sender: TObject);
var
  strtemp : String;
  strchecked : boolean;
begin
  // move item down!
  with chklistColumns do
    if ItemIndex > -1 then begin
      if ItemIndex < items.count-1 then begin // not last item...
        strtemp := items[ItemIndex+1];
        strchecked := checked[ItemIndex+1];
        // replace old with new item...
        items[ItemIndex+1] := items[ItemIndex];
        checked[ItemIndex+1] := checked[ItemIndex];
        // and set old item to its origin values...
        items[ItemIndex] := strtemp;
        checked[ItemIndex] := strchecked;

        ItemIndex := ItemIndex+1;
      end;
    end;
end;

end.
