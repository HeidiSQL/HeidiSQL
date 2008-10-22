unit createtable;


// -------------------------------------
// Create table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ImgList, ToolWin,
  Menus, db, PngSpeedButton, TntStdCtrls;

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
    EditTablename: TTNTEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditDescription: TTNTEdit;
    Label3: TLabel;
    ButtonMoveUp: TPngSpeedButton;
    ButtonMoveDown: TPngSpeedButton;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    ButtonChange: TButton;
    EditFieldname: TEdit;
    Bevel1: TBevel;
    Label4: TLabel;
    DBComboBox: TTNTComboBox;
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
    procedure refreshColumnList(Sender: TObject);
    procedure ButtonChangeClick(Sender: TObject);
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

{$I const.inc}

implementation

uses
  Main, Childwin, helpers, mysql_structures;

var
  cols : array of TMysqlField;

{$R *.DFM}


{**
  Fetch list with character sets and collations from the server
  @todo: share these lists with other forms, fx createdatabase
}
procedure TCreateTableForm.FormCreate(Sender: TObject);
var
  charset : String;
begin
  // Assign images from main imagelist to speedbuttons
  ButtonMoveUp.PngImage := Mainform.PngImageListMain.PngImages[74].PngImage;
  ButtonMoveDown.PngImage := Mainform.PngImageListMain.PngImages[75].PngImage;

  InheritFont(Font);

  try
    dsCollations := Mainform.Childwin.GetResults('SHOW COLLATION');
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

  // Display supported engines in pulldown
  Mainform.Childwin.TableEnginesCombo( ComboBoxTableType );
end;


procedure TCreateTableForm.ButtonCreateClick(Sender: TObject);
var
  createQuery, primaryKey, uniqueKey, indexKey  : WideString;
  i : Integer;
  LengthSet : String;
  FieldType : TMysqlDataTypeRecord;
begin
  // Prepare Query:
  createQuery := 'CREATE TABLE ' + mainform.mask(DBComboBox.Text) + '.' + mainform.mask(EditTablename.Text) + ' (';

  // Columns
  for i := 0 to length(cols) - 1 do
  begin
    FieldType := MySqlDataTypeArray[cols[i].FieldType];
    createQuery := createQuery + mainform.mask(cols[i].Name) + ' ' +
      comboboxtype.items[cols[i].FieldType];                              // Typ
    LengthSet := cols[i].LengthSet;
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
    if FieldType.HasBinary and cols[i].Binary then
      createQuery := createQuery  + ' BINARY';                                // Binary
    if FieldType.HasUnsigned and cols[i].Unsigned then
      createQuery := createQuery  + ' UNSIGNED';                              // Unsigned
    if FieldType.HasZerofill and cols[i].Zerofill then
      createQuery := createQuery  + ' ZEROFILL';                              // Zerofill
    if cols[i].Default <> '' then
      createQuery := createQuery  + ' DEFAULT ''' + cols[i].Default + '''';   // Default
    if cols[i].NotNull then
      createQuery := createQuery  + ' NOT NULL';                              // Not null
    if cols[i].AutoIncrement then
      createQuery := createQuery  + ' AUTO_INCREMENT';                         // AutoIncrement

    if i < length(cols)-1 then
      createQuery := createQuery + ', '
  end;

  // Indexes:
  primaryKey := '';
  uniqueKey := '';
  indexKey := '';
  for i := 0 to length(cols) - 1 do
  begin
    if cols[i].Primary then
    begin
      if primaryKey <> '' then primaryKey := primaryKey + ',';
      primaryKey := primaryKey + mainform.mask(cols[i].Name);
    end;
    if cols[i].Unique then
    begin
      if uniqueKey <> '' then uniqueKey := uniqueKey + ',';
      uniqueKey := uniqueKey + mainform.mask(cols[i].Name);
    end;
    if cols[i].Index then
    begin
      if indexKey <> '' then indexKey := indexKey + ',';
      indexKey := indexKey + mainform.mask(cols[i].Name);
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

  if ComboBoxTableType.Text <> '' then
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
    Mainform.ChildWin.ExecUpdateQuery( createQuery, False, True );
    Mainform.ChildWin.MenuRefreshClick(sender);
    Mainform.ChildWin.SelectedTable := EditTablename.Text;
  except on E: THandledSQLError do
    // Keep the form open so the user can fix his faulty input
    ModalResult := mrNone;
  end;
end;



procedure TCreateTableForm.refreshColumnList(Sender: TObject);
var i : word;
begin
  // refresh list of columns
  with ListboxColumns do
  begin
    Items.Clear;
    if length(cols) > 0 then
      for i := 0 to length(cols)-1 do
        Items.Add(cols[i].Name);
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
  // remove column
  if length(cols) > 1 then
    for i := index to length(cols)-2 do
    begin
      cols[i] := cols[i+1];
    end;
  setlength(cols, length(cols)-1);
  dec(index);
  refreshColumnList(self);
  EditFieldNameChange(self);
  ListboxColumnsClick(self);
  if length(cols) = 0 then
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
  cols[index].FieldType := ComboBoxType.ItemIndex;
  checktypes(self);
end;

procedure TCreateTableForm.EditLengthSetChange(Sender: TObject);
begin
  // LengthSet
  cols[index].LengthSet := EditLengthSet.Text;
end;


{***
  Check if tablename is valid and warn the user in case he's
  doing some crappy character here
}
procedure TCreateTableForm.EditTablenameChange(Sender: TObject);
begin
  ButtonCreate.Enabled := false;
  EditTablename.Font.Color := clWindowText;
  EditTablename.Color := clWindow;
  try
    ensureValidIdentifier( EditTablename.Text );
    // Enable "OK"-Button if we have a valid name AND there
    // is at least 1 column
    ButtonCreate.Enabled := (ListBoxColumns.Items.Count > 0);
  except
    if EditTablename.Text <> '' then begin
      EditTablename.Font.Color := clRed;
      EditTablename.Color := clYellow;
    end;
  end;
end;


procedure TCreateTableForm.EditDefaultChange(Sender: TObject);
begin
  // Default
  cols[index].Default := EditDefault.Text;
end;

procedure TCreateTableForm.CheckBoxPrimaryClick(Sender: TObject);
begin
  // Primary
  cols[index].Primary := CheckBoxPrimary.Checked;
end;

procedure TCreateTableForm.CheckBoxIndexClick(Sender: TObject);
begin
  // Index
  cols[index].Index := CheckBoxIndex.Checked;
end;

procedure TCreateTableForm.CheckBoxUniqueClick(Sender: TObject);
begin
  // Unique
  cols[index].Unique := CheckBoxUnique.Checked;
end;

procedure TCreateTableForm.CheckBoxBinaryClick(Sender: TObject);
begin
  // Binary
  cols[index].Binary := CheckBoxBinary.Checked;
end;

procedure TCreateTableForm.CheckBoxUnsignedClick(Sender: TObject);
begin
  // Unsigned
  cols[index].Unsigned := CheckBoxUnsigned.Checked;
end;

procedure TCreateTableForm.CheckBoxZerofillClick(Sender: TObject);
begin
  // Zerofill
  cols[index].Zerofill := CheckBoxZerofill.Checked;
end;

procedure TCreateTableForm.CheckBoxNotNullClick(Sender: TObject);
begin
  // Not Null
  cols[index].NotNull := CheckBoxNotNull.Checked;
end;

procedure TCreateTableForm.CheckBoxAutoincrementClick(Sender: TObject);
begin
  // AutoIncrement
  cols[index].AutoIncrement := CheckBoxAutoIncrement.Checked;
  // Fix bug #160 - auto increment is only valid for PK columns
  if CheckBoxAutoIncrement.Checked then
    CheckboxPrimary.Checked := True;
end;

procedure TCreateTableForm.Button1Click(Sender: TObject);
begin
  // Add new Field
  index := length(cols);
  setlength(cols, index+1);
  with cols[index] do
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
  refreshColumnList(self);
  EditFieldnameChange(self);
  ListboxColumns.ItemIndex := index;
  // Call change-handler of Tablename-edit to check if
  // the Create-Button should be enabled
  EditTablenameChange(self);
end;

procedure TCreateTableForm.EditFieldnameChange(Sender: TObject);
var
  colExists, colSelected : Boolean;
  i: Integer;
begin
  // Field Name EditChange
  colExists := False;
  for i:=0 to ListboxColumns.Items.Count-1 do begin
    if EditFieldName.Text = ListboxColumns.Items[i] then begin
      colExists := True;
      break;
    end;
  end;
  colSelected := index > -1;
  buttonAdd.Enabled := not colExists;
  buttonChange.Enabled := (not colExists) and colSelected;
  buttonDelete.Enabled := colSelected;
  ButtonAdd.Default := not colExists;
  try
    ensureValidIdentifier(EditFieldName.Text);
  except
    buttonAdd.Enabled := false;
    buttonChange.Enabled := false;
  end;
end;

procedure TCreateTableForm.ListboxColumnsClick(Sender: TObject);
begin
  // ListColumns Change
  index := ListboxColumns.ItemIndex;
  if index > -1 then
    editfieldname.Text := cols[index].Name;
  refreshColumnList(self);
end;

procedure TCreateTableForm.FormShow(Sender: TObject);
var
  i: Integer;
begin
  // FormShow!

  // read dbs and Tables from treeview
  DBComboBox.Items.Clear;
  DBComboBox.Items.Assign(Mainform.ChildWin.Databases);
  // Preselect relevant database in pulldown 
  DBComboBox.ItemIndex := DBComboBox.Items.IndexOf( Mainform.ChildWin.ActiveDatabase );
  if (DBComboBox.ItemIndex = -1) and (DBComboBox.Items.Count > 0) then
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

  // Adds all datatypes for columns
  ComboboxType.Items.Clear;
  for i := Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do
  begin
    ComboboxType.Items.Add( MySqlDataTypeArray[i].Name );
  end;
    
    
  index := -1;
  setLength(cols, 0);
  ListboxColumns.Items.Clear;
  EditTableName.Text := 'Enter table name';
  EditFieldName.Text := 'Enter column name';
  Editdescription.Text := '';
  ButtonCreate.Enabled := false;
  ButtonAdd.Enabled := true;
  ButtonMoveUp.Enabled := False;
  ButtonMoveDown.Enabled := False;

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
  cols[index].Name := editfieldname.Text;
  refreshColumnList(self);
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
  with cols[index] do
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
  setlength(cols, length(cols)+1);
  cols[length(cols)-1] := cols[index-1];
  cols[index-1] := cols[index];
  cols[index] := cols[length(cols)-1];
  setlength(cols, length(cols)-1);
  dec(index);
  refreshColumnList(self);
end;

procedure TCreateTableForm.ButtonMoveDownClick(Sender: TObject);
begin
  // move down
  setlength(cols, length(cols)+1);
  cols[length(cols)-1] := cols[index+1];
  cols[index+1] := cols[index];
  cols[index] := cols[length(cols)-1];
  setlength(cols, length(cols)-1);
  inc(index);
  refreshColumnList(self);
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
