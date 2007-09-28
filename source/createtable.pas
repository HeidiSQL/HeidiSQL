unit createtable;


// -------------------------------------
// Create table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ImgList, ToolWin,
  Menus, db;

type
  TCreateTableForm = class(TForm)
    ButtonCancel: TButton;
    ButtonCreate: TButton;
    GroupBox1: TGroupBox;
    ComboBoxType: TComboBox;
    EditLengthSet: TEdit;
    EditDefault: TEdit;
    CheckBoxPrimary: TCheckBox;
    CheckBoxBinary: TCheckBox;
    CheckBoxIndex: TCheckBox;
    CheckBoxUnique: TCheckBox;
    CheckBoxUnsigned: TCheckBox;
    CheckBoxZerofill: TCheckBox;
    CheckBoxNotNull: TCheckBox;
    CheckBoxAutoincrement: TCheckBox;
    lblFieldType: TLabel;
    lblLengthSet: TLabel;
    lblDefault: TLabel;
    EditTablename: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditDescription: TEdit;
    Label3: TLabel;
    ButtonMoveUp: TBitBtn;
    ButtonMoveDown: TBitBtn;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    ButtonChange: TButton;
    EditFieldname: TEdit;
    Bevel1: TBevel;
    Label4: TLabel;
    DBComboBox: TComboBox;
    ComboBoxTableType: TComboBox;
    Label5: TLabel;
    Bevel2: TBevel;
    ListboxColumns: TListBox;
    lblCharset: TLabel;
    lblCollation: TLabel;
    comboCharset: TComboBox;
    comboCollation: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure EditTablenameChange(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonCreateClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure CheckBoxPrimaryClick(Sender: TObject);
    procedure CheckBoxIndexClick(Sender: TObject);
    procedure CheckBoxUniqueClick(Sender: TObject);
    procedure CheckBoxBinaryClick(Sender: TObject);
    procedure CheckBoxUnsignedClick(Sender: TObject);
    procedure CheckBoxZerofillClick(Sender: TObject);
    procedure CheckBoxNotNullClick(Sender: TObject);
    procedure CheckBoxAutoincrementClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EditFieldnameChange(Sender: TObject);
    procedure ListboxColumnsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure refreshfields(Sender: TObject);
    procedure ButtonChangeClick(Sender: TObject);
    procedure ButtonsChange(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure EditLengthSetChange(Sender: TObject);
    procedure EditDefaultChange(Sender: TObject);
    procedure disableControls(Sender: TObject);
    procedure enableControls(Sender: TObject);
    procedure fillControls(Sender: TObject);
    procedure checktypes(Sender: TObject);
    procedure ButtonMoveUpClick(Sender: TObject);
    procedure ButtonMoveDownClick(Sender: TObject);
    procedure comboCharsetChange(Sender: TObject);
  private
    index : Integer;
    dsCollations : TDataSet;
    defaultCharset : String;
    { Private declarations }
  public
    { Public declarations }
  end;

  function CreateTableWindow (AOwner : TComponent; Database: string = '') : Boolean;

{$I const.inc}

implementation

uses
  Main, Childwin, helpers, mysql;

var
  fields : array of TMysqlField;

{$R *.DFM}


{**
  Create form on demand
  @param TComponent Owner of form (should be calling form)
  @return Boolean Form closed using modalresult mrOK
}
function CreateTableWindow (AOwner : TComponent; Database: string) : Boolean;
var
  f : TCreateTableForm;
begin
  f := TCreateTableForm.Create(AOwner);
  if Database <> '' then f.DBComboBox.SelText := Database;
  Result := (f.ShowModal=mrOK);
  FreeAndNil (f);
end;


{**
  Fetch list with character sets and collations from the server
  @todo: share these lists with other forms, fx createdatabase
}
procedure TCreateTableForm.FormCreate(Sender: TObject);
var
  charset : String;
begin
  try
    dsCollations := Mainform.Childwin.ExecSelectQuery('SHOW COLLATION');
    // Detect servers default charset
    defaultCharset := Mainform.Childwin.GetVar( 'SHOW VARIABLES LIKE '+esc('character_set_server'), 1 );
  except
    // Ignore it when the above statements don't work on pre 4.1 servers.
    // If the list(s) are nil, disable the combobox(es), so we create the db without charset.
  end;

  // Create a list with charsets from collations dataset
  comboCharset.Enabled := dsCollations <> nil;
  lblCharset.Enabled := comboCharset.Enabled;
  if comboCharset.Enabled then
  begin
    comboCharset.Items.BeginUpdate;
    dsCollations.First;
    while not dsCollations.Eof do
    begin
      charset := dsCollations.FieldByName('Charset').AsString;
      if comboCharset.Items.IndexOf(charset) = -1 then
        comboCharset.Items.Add(charset);
      dsCollations.Next;
    end;
    comboCharset.Items.EndUpdate;
  end;

  comboCollation.Enabled := dsCollations <> nil;
  lblCollation.Enabled := comboCollation.Enabled;
end;


procedure TCreateTableForm.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;


procedure TCreateTableForm.ButtonCreateClick(Sender: TObject);
var
  createQuery, primaryKey, uniqueKey, indexKey  : String;
  i : Integer;
  LengthSet : String;
  FieldType : TMysqlDataTypeRecord;
begin
  // Prepare Query:
  createQuery := 'CREATE TABLE ' + mainform.mask(DBComboBox.Text) + '.' + mainform.mask(EditTablename.Text) + ' (';

  // Columns
  for i := 0 to length(fields) - 1 do
  begin
    FieldType := MySqlDataTypeArray[fields[i].FieldType];
    createQuery := createQuery + mainform.mask(fields[i].Name) + ' ' +
      comboboxtype.items[fields[i].FieldType];                              // Typ
    LengthSet := fields[i].LengthSet;
    // Unset length if not allowed for fieldtype
    if not FieldType.HasLength then
      LengthSet := '';
    // Length required
    if FieldType.RequiresLength and (LengthSet = '') then
    begin
      if FieldType.DefLengthSet <> '' then
        LengthSet := FieldType.DefLengthSet
      else
        LengthSet := '50';
    end;
    if LengthSet <> '' then
      createQuery := createQuery  + ' (' + LengthSet + ')';                   // Length/Set
    if FieldType.HasBinary and fields[i].Binary then
      createQuery := createQuery  + ' BINARY';                                // Binary
    if FieldType.HasUnsigned and fields[i].Unsigned then
      createQuery := createQuery  + ' UNSIGNED';                              // Unsigned
    if FieldType.HasZerofill and fields[i].Zerofill then
      createQuery := createQuery  + ' ZEROFILL';                              // Zerofill
    if fields[i].Default <> '' then
      createQuery := createQuery  + ' DEFAULT ''' + fields[i].Default + '''';   // Default
    if fields[i].NotNull then
      createQuery := createQuery  + ' NOT NULL';                              // Not null
    if fields[i].AutoIncrement then
      createQuery := createQuery  + ' AUTO_INCREMENT';                         // AutoIncrement

    if i < length(fields)-1 then
      createQuery := createQuery + ', '
  end;

  // Indexes:
  primaryKey := '';
  uniqueKey := '';
  indexKey := '';
  for i := 0 to length(fields) - 1 do
  begin
    if fields[i].Primary then
    begin
      if primaryKey <> '' then primaryKey := primaryKey + ',';
      primaryKey := primaryKey + mainform.mask(fields[i].Name);
    end;
    if fields[i].Unique then
    begin
      if uniqueKey <> '' then uniqueKey := uniqueKey + ',';
      uniqueKey := uniqueKey + mainform.mask(fields[i].Name);
    end;
    if fields[i].Index then
    begin
      if indexKey <> '' then indexKey := indexKey + ',';
      indexKey := indexKey + mainform.mask(fields[i].Name);
    end;
  end;
  if primaryKey <> '' then
    createQuery := createQuery + ', PRIMARY KEY(' + primaryKey + ')';
  if uniqueKey <> '' then
    createQuery := createQuery + ', UNIQUE(' + uniqueKey + ')';
  if indexKey <> '' then
    createQuery := createQuery + ', INDEX(' + indexKey + ')';

  // End of columns + indexes:
  createQuery := createQuery + ') ';

  // Comment:
  if EditDescription.Text <> '' then
    createQuery := createQuery + ' COMMENT = "' + EditDescription.Text + '"';

  if (ComboBoxTableType.Text <> '') and (ComboBoxTableType.Text <> TBLTYPE_AUTOMATIC) then
    createQuery := createQuery + ' TYPE = ' + ComboBoxTableType.Text;

  if comboCharset.Enabled and (comboCharset.Text <> '') then
  begin
    createQuery := createQuery + ' /*!40100 DEFAULT CHARSET ' + comboCharset.Text;
    if comboCollation.Enabled and (comboCollation.Text <> '') then
      createQuery := createQuery + ' COLLATE ' + comboCollation.Text;
    createQuery := createQuery + ' */';
  end;

  // Execute CREATE statement and reload tablesList
  try
    Mainform.Childwin.ActiveDatabase := DBComboBox.Text;
    Mainform.ChildWin.ExecUpdateQuery( createQuery );
    Mainform.ChildWin.MenuRefreshClick(sender);
    Mainform.ChildWin.SelectedTable := EditTablename.Text;
    Mainform.ChildWin.ShowTable(EditTablename.Text);
    Close;
  except on E: THandledSQLError do;
  end;
end;



procedure TCreateTableForm.refreshfields(Sender: TObject);
var i : word;
begin
  // refresh field-list
  with ListboxColumns do
  begin
    Items.Clear;
    if length(fields) > 0 then
      for i := 0 to length(fields)-1 do
        Items.Add(fields[i].Name);
    ItemIndex := index;
  end;
  if index = -1 then
  begin
    buttonDelete.Enabled := false;
    buttonChange.Enabled := false;
    disableControls(self);
  end else
  begin
    enableControls(self);
    fillControls(self);
  end;
  if index = ListboxColumns.Items.Count - 1 then
    ButtonMoveDown.Enabled := false
  else
    ButtonMoveDown.Enabled := true;
  if index = 0 then
    ButtonMoveUp.Enabled := false
  else
    ButtonMoveUp.Enabled := true;
end;

procedure TCreateTableForm.ButtonDeleteClick(Sender: TObject);
var i : Word;
begin
  // delete field
  if length(fields) > 1 then
    for i := index to length(fields)-2 do
    begin
      fields[i] := fields[i+1];
    end;
  setlength(fields, length(fields)-1);
  dec(index);
  refreshfields(self);
  EditFieldNameChange(self);
  ListboxColumnsClick(self);
  if length(fields) = 0 then
  begin
    ButtonMoveUp.Enabled := false;
    ButtonMoveDown.Enabled := false;
    ButtonCreate.Enabled := false;
  end;
end;


procedure TCreateTableForm.checktypes(Sender: TObject);
var
  FieldType : TMysqlDataTypeRecord;
begin
  FieldType := MySqlDataTypeArray[ComboBoxType.ItemIndex];

  // "binary" is only valid for text-types
  CheckBoxBinary.Enabled := FieldType.HasBinary;
  if not CheckBoxBinary.Enabled then
    CheckBoxBinary.Checked := false; // Ensure checkbox is not ticked

  // "unsigned" is only valid for numerical columns
  CheckBoxUnsigned.Enabled := FieldType.HasUnsigned;
  if not CheckBoxUnsigned.Enabled then
    CheckBoxUnsigned.Checked := false; // Ensure checkbox is not ticked

  // "zerofill" is only valid for numerical and float-columns
  CheckBoxZerofill.Enabled := FieldType.HasZerofill;
  if not CheckBoxZerofill.Enabled then
    CheckBoxZerofill.Checked := false; // Ensure checkbox is not ticked

  // Length/Set
  EditLengthSet.Enabled := FieldType.HasLength;
  lblLengthSet.Enabled := EditLengthSet.Enabled;
  if FieldType.RequiresLength then // Render required field as bold
    lblLengthSet.Font.Style := lblLengthSet.Font.Style + [fsBold]
  else
    lblLengthSet.Font.Style := lblLengthSet.Font.Style - [fsBold];
  if not EditLengthSet.Enabled then
    EditLengthSet.Text := '';
  // Fill length/set value with default value if empty
  if FieldType.RequiresLength then
  begin
    if (EditLengthSet.Text = '') and (FieldType.DefLengthSet <> '') then
      EditLengthSet.Text := FieldType.DefLengthSet;
  end;

  // Default value
  EditDefault.Enabled := FieldType.HasDefault;
  lblDefault.Enabled := EditDefault.Enabled;
  if not EditDefault.Enabled then
    EditDefault.Text := ''; // Ensure empty default value

end;


procedure TCreateTableForm.ComboBoxTypeChange(Sender: TObject);
begin
  // Type
  fields[index].FieldType := ComboBoxType.ItemIndex;
  checktypes(self);
end;

procedure TCreateTableForm.EditLengthSetChange(Sender: TObject);
begin
  // LengthSet
  fields[index].LengthSet := EditLengthSet.Text;
end;


{***
  Check if tablename is valid and warn the user in case he's
  doing some crappy character here
}
procedure TCreateTableForm.EditTablenameChange(Sender: TObject);
begin
  ButtonCreate.Enabled := false;
  try
    ensureValidIdentifier( EditTablename.Text );
    EditTablename.Font.Color := clWindowText;
    EditTablename.Color := clWindow;
    // Enable "OK"-Button if we have a valid name AND there
    // is at least 1 column
    ButtonCreate.Enabled := (ListBoxColumns.Items.Count > 0);
  except
    EditTablename.Font.Color := clRed;
    EditTablename.Color := clYellow;
  end;
end;


procedure TCreateTableForm.EditDefaultChange(Sender: TObject);
begin
  // Default
  fields[index].Default := EditDefault.Text;
end;

procedure TCreateTableForm.CheckBoxPrimaryClick(Sender: TObject);
begin
  // Primary
  fields[index].Primary := CheckBoxPrimary.Checked;
end;

procedure TCreateTableForm.CheckBoxIndexClick(Sender: TObject);
begin
  // Index
  fields[index].Index := CheckBoxIndex.Checked;
end;

procedure TCreateTableForm.CheckBoxUniqueClick(Sender: TObject);
begin
  // Unique
  fields[index].Unique := CheckBoxUnique.Checked;
end;

procedure TCreateTableForm.CheckBoxBinaryClick(Sender: TObject);
begin
  // Binary
  fields[index].Binary := CheckBoxBinary.Checked;
end;

procedure TCreateTableForm.CheckBoxUnsignedClick(Sender: TObject);
begin
  // Unsigned
  fields[index].Unsigned := CheckBoxUnsigned.Checked;
end;

procedure TCreateTableForm.CheckBoxZerofillClick(Sender: TObject);
begin
  // Zerofill
  fields[index].Zerofill := CheckBoxZerofill.Checked;
end;

procedure TCreateTableForm.CheckBoxNotNullClick(Sender: TObject);
begin
  // Not Null
  fields[index].NotNull := CheckBoxNotNull.Checked;
end;

procedure TCreateTableForm.CheckBoxAutoincrementClick(Sender: TObject);
begin
  // AutoIncrement
  fields[index].AutoIncrement := CheckBoxAutoIncrement.Checked;
end;

procedure TCreateTableForm.Button1Click(Sender: TObject);
begin
  // Add new Field
  index := length(fields);
  setlength(fields, index+1);
  with fields[index] do
  begin
    Name := EditFieldName.Text;
    FieldType := 0;
    LengthSet := '';
    default := '';
    Primary := false;
    Index := false;
    Unique := false;
    Binary := false;
    Unsigned := true;
    Zerofill := false;
    NotNull := false;
    Autoincrement := false;
    ListboxColumns.Items.Add(Name);
  end;
  refreshfields(self);
  EditFieldnameChange(self);
  ListboxColumns.ItemIndex := index;
  // Call change-handler of Tablename-edit to check if
  // the Create-Button should be enabled
  EditTablenameChange(self);
end;

procedure TCreateTableForm.EditFieldnameChange(Sender: TObject);
var
  colExists : Boolean;
begin
  // Field Name EditChange
  colExists := notinlist(EditFieldName.Text, ListboxColumns.Items);
  buttonAdd.Enabled := colExists;
  buttonChange.Enabled := colExists;
  ButtonAdd.Default := colExists;
  try
    ensureValidIdentifier(EditFieldName.Text);
  except
    buttonAdd.Enabled := false;
    buttonChange.Enabled := false;
  end;
  ButtonsChange(self);
end;

procedure TCreateTableForm.ListboxColumnsClick(Sender: TObject);
begin
  // ListColumns Change
  index := ListboxColumns.ItemIndex;
  if index > -1 then
    editfieldname.Text := fields[index].Name;
  refreshfields(self);
end;


procedure TCreateTableForm.ButtonsChange(Sender: TObject);
begin
  // look for name and change buttons
  if index = -1 then
  begin
    buttonDelete.Enabled := false;
    buttonChange.Enabled := false;
  end else
  begin
    buttonDelete.Enabled := true;
    if notinlist(EditFieldName.Text, ListboxColumns.Items) then
      buttonChange.Enabled := true
    else
      buttonChange.Enabled := false;
  end;
end;

procedure TCreateTableForm.FormShow(Sender: TObject);
var
  i         : Integer;
  tn        : TTreeNode;
  menu      : TMenuItem;
begin
  // FormShow!

  // read dbs and Tables from treeview
  DBComboBox.Items.Clear;
  for i:=0 to Mainform.ChildWin.DBTree.Items.Count-1 do
  begin
    tn := Mainform.ChildWin.DBTree.Items[i];
    if tn.Level = 1 then
      DBComboBox.Items.Add(tn.Text);
  end;
  DBComboBox.ItemIndex := 0;
  for i:=0 to DBComboBox.Items.Count-1 do
  begin
    if DBComboBox.Items[i] = Mainform.ChildWin.ActiveDatabase then
      DBComboBox.ItemIndex := i;
  end;
  if DBComboBox.ItemIndex = -1 then
    DBComboBox.ItemIndex := 0;

  if Mainform.ChildWin.mysql_version >= 32300 then
  begin
    EditDescription.Visible := true;
    Label3.Visible := true;
  end
  else
  begin
    EditDescription.Visible := false;
    Label3.Visible := false;
  end;
  // Add all table types detected at application start to ComboboxTableType
  menu := Mainform.ChildWin.popupDbGrid.Items.Find('Change Type');
  if menu <> nil then
  begin
    ComboboxTableType.Items.Clear;
    ComboboxTableType.Items.Add( TBLTYPE_AUTOMATIC );
    for i := 0 to menu.Count - 1 do
    begin
      if menu.Items[i].Caption = '-' then // End of list
        break;
      if not menu.Items[i].Enabled then  // Not supported engine
        continue;
      ComboboxTableType.Items.Add( menu.Items[i].Caption );
    end;
  end;

  // Adds all datatypes for columns
  ComboboxType.Items.Clear;
  for i := Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do
  begin
    ComboboxType.Items.Add( MySqlDataTypeArray[i].Name );
  end;
    
    
  index := -1;
  setLength(fields, 0);
  ListboxColumns.Items.Clear;
  EditTableName.Text := 'TableName';
  EditFieldName.Text := 'FieldName';
  Editdescription.Text := '';
  ButtonCreate.Enabled := false;
  ButtonAdd.Enabled := true;

  // Preselect charset item in pulldown
  if comboCharset.Items.Count > 0 then
  begin
    if comboCharset.Items.IndexOf(defaultCharset) > -1 then
      comboCharset.ItemIndex := comboCharset.Items.IndexOf(defaultCharset)
    else
      comboCharset.ItemIndex := 0;
    // Invoke selecting default collation
    comboCharsetChange( Sender );
  end;

  disablecontrols(self);
end;

procedure TCreateTableForm.ButtonChangeClick(Sender: TObject);
begin
  // Change Fieldname
  fields[index].Name := editfieldname.Text;
  refreshfields(self);
  editfieldnamechange(self);
end;


procedure TCreateTableForm.disableControls(Sender: TObject);
begin
  // disable controls
  ButtonDelete.Enabled := false;
  lblFieldType.Enabled := false; // Type
  lblLengthSet.Enabled := false; // Length
  lblDefault.Enabled := false; // Default
  ComboBoxType.Enabled := false;
  EditLengthSet.Enabled := false;
  EditDefault.Enabled := false;
  CheckBoxPrimary.Enabled := false;
  CheckBoxIndex.Enabled := false;
  CheckBoxUnique.Enabled := false;
  CheckBoxBinary.Enabled := false;
  CheckBoxUnsigned.Enabled := false;
  CheckBoxZerofill.Enabled := false;
  CheckBoxNotNull.Enabled := false;
  CheckBoxAutoIncrement.Enabled := false;
end;


procedure TCreateTableForm.enableControls(Sender: TObject);
begin
  // enable controls
  lblFieldType.Enabled := true; // Type
  lblLengthSet.Enabled := true; // Length
  lblDefault.Enabled := true; // Default
  ComboBoxType.Enabled := true;
  EditLengthSet.Enabled := true;
  EditDefault.Enabled := true;
  CheckBoxPrimary.Enabled := true;
  CheckBoxIndex.Enabled := true;
  CheckBoxUnique.Enabled := true;
  CheckBoxBinary.Enabled := true;
  CheckBoxUnsigned.Enabled := true;
  CheckBoxZerofill.Enabled := true;
  CheckBoxNotNull.Enabled := true;
  CheckBoxAutoIncrement.Enabled := true;
end;


procedure TCreateTableForm.fillControls(Sender: TObject);
begin
  // fill controls with values
  with fields[index] do
  begin
    ComboBoxType.ItemIndex := FieldType;
    EditLengthSet.Text := LengthSet;
    EditDefault.Text := Default;
    CheckBoxPrimary.Checked := Primary;
    CheckBoxIndex.Checked := Index;
    CheckBoxUnique.Checked := Unique;
    CheckBoxBinary.Checked := Binary;
    CheckBoxUnsigned.Checked := Unsigned;
    CheckBoxZerofill.Checked := Zerofill;
    CheckBoxNotNull.Checked := NotNull;
    CheckBoxAutoIncrement.Checked := AutoIncrement;
  end;
  checktypes(self);
end;


procedure TCreateTableForm.ButtonMoveUpClick(Sender: TObject);
begin
  // move up
  setlength(fields, length(fields)+1);
  fields[length(fields)-1] := fields[index-1];
  fields[index-1] := fields[index];
  fields[index] := fields[length(fields)-1];
  setlength(fields, length(fields)-1);
  dec(index);
  refreshfields(self);
end;

procedure TCreateTableForm.ButtonMoveDownClick(Sender: TObject);
begin
  // move down
  setlength(fields, length(fields)+1);
  fields[length(fields)-1] := fields[index+1];
  fields[index+1] := fields[index];
  fields[index] := fields[length(fields)-1];
  setlength(fields, length(fields)-1);
  inc(index);
  refreshfields(self);
end;


{**
  Charset has been selected: Display fitting collations
  and select default one.
}
procedure TCreateTableForm.comboCharsetChange(Sender: TObject);
var
  defaultCollation : String;
begin
  // Abort if collations were not fetched successfully
  if dsCollations = nil then
    Exit;

  // Fill pulldown with fitting collations
  comboCollation.Items.BeginUpdate;
  comboCollation.Items.Clear;
  dsCollations.First;
  while not dsCollations.Eof do
  begin
    if dsCollations.FieldByName('Charset').AsString = comboCharset.Text then
    begin
      comboCollation.Items.Add( dsCollations.FieldByName('Collation').AsString );
      if dsCollations.FieldByName('Default').AsString = 'Yes' then
        defaultCollation := dsCollations.FieldByName('Collation').AsString;
    end;
    dsCollations.Next;
  end;

  // Preselect default collation
  if comboCollation.Items.IndexOf(defaultCollation) > -1 then
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(defaultCollation)
  else
    comboCollation.ItemIndex := 0;

  comboCollation.Items.EndUpdate;
end;

end.
