unit loaddata;


// -------------------------------------
// HeidiSQL
// Load Textfile into table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, comctrls, Spin, Buttons, CheckLst, Registry;

type
  Tloaddataform = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    EditFileName: TEdit;
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    TablesComboBox: TComboBox;
    Label2: TLabel;
    CheckBox2: TCheckBox;
    Label3: TLabel;
    Edit2: TEdit;
    CheckBox3: TCheckBox;
    Edit3: TEdit;
    CheckBox4: TCheckBox;
    Edit4: TEdit;
    CheckBox5: TCheckBox;
    Label4: TLabel;
    CheckBox6: TCheckBox;
    Edit5: TEdit;
    CheckBox7: TCheckBox;
    SpinEdit1: TSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    ColumnsCheckListBox: TCheckListBox;
    DBComboBox: TComboBox;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Label8: TLabel;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DBComboBoxChange(Sender: TObject);
    procedure TablesComboBoxChange(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  loaddataform: Tloaddataform;

implementation

uses Main, Childwin, helpers;

{$R *.DFM}

procedure Tloaddataform.Button2Click(Sender: TObject);
begin
  close;
end;

procedure Tloaddataform.FormShow(Sender: TObject);
var
  tn : TTreeNode;
  i : Integer;
begin
  // read dbs and Tables from treeview
  DBComboBox.Items.Clear;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Level = 1 then
        DBComboBox.Items.Add(tn.Text);
    end;

    with DBComboBox do
    begin
      for i:=0 to Items.Count-1 do
        if Items[i] = ActualDatabase then
          ItemIndex := i;
      if ItemIndex = -1 then
        ItemIndex := 0;
    end;

  end;
  DBComboBoxChange(self);
  // filename
  with TRegistry.Create do
    if OpenKey(regpath, true) then
      EditFileName.Text := ReadString('loadfilename');
  if EditFileName.Text = '' then
    EditFileName.Text := ExtractFilePath(paramstr(0)) + 'import.csv';
  EditFileName.Text := stringreplace(EditFileName.Text, '\', '/', [rfReplaceAll]);
end;


procedure Tloaddataform.DBComboBoxChange(Sender: TObject);
var
  tn, child : TTreeNode;
  i,j : Integer;
begin
  // read tables from db
  TablesComboBox.Items.Clear;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Text = DBComboBox.Text then
      begin
        child := tn.getFirstChild;
        for j:=0 to tn.Count-1 do
        begin
          TablesComboBox.Items.Add(child.Text);
          child := tn.getNextChild(child);
        end;
      end;
    end;

    with TablesComboBox do
    begin
      for i:=0 to Items.Count-1 do
        if Items[i] = ActualTable then
          ItemIndex := i;
      if ItemIndex = -1 then
        ItemIndex := 0;
    end;

  end;
  TablesComboBoxChange(self);
end;


procedure Tloaddataform.TablesComboBoxChange(Sender: TObject);
var
  i : Integer;
begin
  // fill columns:
  ColumnsCheckListBox.Items.Clear;
  if (DBComboBox.Text <> '') and (TablesComboBox.Text <> '') then
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    GetResults( 'SHOW FIELDS FROM ' + DBComboBox.Text + '.' +  TablesComboBox.Text, ZQuery3 );
    for i:=1 to ZQuery3.RecordCount do
    begin
      ColumnsCheckListBox.Items.Add(ZQuery3.Fields[0].AsString);
      ZQuery3.Next;
    end;
  end;

  // select all:
  for i:=0 to ColumnsCheckListBox.Items.Count-1 do
    ColumnsCheckListBox.checked[i] := true;
end;


procedure Tloaddataform.CheckBox2Click(Sender: TObject);
begin
  edit2.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.CheckBox3Click(Sender: TObject);
begin
  edit3.Enabled :=  (sender as TCheckBox).checked;
  checkbox5.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.CheckBox4Click(Sender: TObject);
begin
  edit4.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.CheckBox6Click(Sender: TObject);
begin
  edit5.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.CheckBox7Click(Sender: TObject);
begin
  spinedit1.Enabled :=  (sender as TCheckBox).checked;
  Label5.Enabled :=  (sender as TCheckBox).checked;
end;


procedure Tloaddataform.Button1Click(Sender: TObject);
var
  query : string;
  col   : TStringList;
  i     : Integer;
begin

  with TRegistry.Create do
    if OpenKey(regpath, true) then
      WriteString('loadfilename', EditFileName.Text);

  query := 'LOAD DATA ';

  if checkbox1.Checked then
    query := query + 'LOW_PRIORITY ';

  query := query + 'LOCAL INFILE ''' + escape_string(EditFileName.Text) + ''' ';
  if checkbox8.Checked then
    query := query + 'REPLACE '
  else if checkbox9.Checked then
    query := query + 'IGNORE ';
  query := query + 'INTO TABLE ' + DBComboBox.Text + '.' +  TablesComboBox.Text + ' ';

  // Fields:
  if checkbox2.Checked or checkbox3.Checked or checkbox4.Checked then
    query := query + 'FIELDS ';
  if checkbox2.Checked then
    query := query + 'TERMINATED BY ''' + Edit2.Text + ''' ';
  if checkbox3.Checked then
  begin
    if checkbox5.Checked then
      query := query + 'OPTIONALLY ';
    query := query + 'ENCLOSED BY ''' + Edit3.Text + ''' ';
  end;
  if checkbox4.Checked then
    query := query + 'ESCAPED BY ''' + Edit4.Text + ''' ';

  // Lines:
  if checkbox6.Checked then
    query := query + 'LINES TERMINATED BY ''' + Edit5.Text + ''' ';
  if checkbox7.Checked then
    query := query + 'IGNORE ' + inttostr(SpinEdit1.Value) + ' LINES ';

  col := TStringList.Create;
  for i:=0 to ColumnsCheckListBox.Items.Count - 1 do
  begin
    if ColumnsCheckListBox.checked[i] then
      col.Add(ColumnsCheckListBox.Items[i]);
  end;

//  if col.Count < ColumnsCheckListBox.Items.Count then
  query := query + '(' + implodestr(',', col) + ')';

  TMDIChild(Application.Mainform.ActiveMDIChild).ExecQuery(query);
  close;
end;

procedure Tloaddataform.BitBtn1Click(Sender: TObject);
begin
  if opendialog1.Execute then
    editfilename.Text := opendialog1.FileName;
end;

procedure Tloaddataform.CheckBox8Click(Sender: TObject);
begin
  if CheckBox8.Checked then
    CheckBox9.checked := false;
end;

procedure Tloaddataform.CheckBox9Click(Sender: TObject);
begin
  if CheckBox9.Checked then
    CheckBox8.checked := false;
end;

procedure Tloaddataform.BitBtn2Click(Sender: TObject);
var
  strtemp : String;
  strchecked : boolean;
begin
  // move item up!
  with ColumnsCheckListbox do
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

procedure Tloaddataform.BitBtn3Click(Sender: TObject);
var
  strtemp : String;
  strchecked : boolean;
begin
  // move item down!
  with ColumnsCheckListbox do
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
