unit createtable;


// -------------------------------------
// HeidiSQL
// Create table
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ImgList, ToolWin,
  Menus;

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
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
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
    feldListe: TListBox;
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
    procedure feldListeClick(Sender: TObject);
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
  private
    index : Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

type TMysqlField = record
    Name : String[64];
    Typ : Byte;
    LengthSet : String;
    Default : String;
    Primary : Boolean;
    Index : Boolean;
    Unique : Boolean;
    Binary : Boolean;
    Unsigned : Boolean;
    Zerofill : Boolean;
    NotNull : Boolean;
    AutoIncrement : Boolean;
  end;

var
  CreateTableForm: TCreateTableForm;
  fields : array of TMysqlField;

const
  TBLTYPE_AUTOMATIC : String = '<Automatic>';

implementation

uses Main, Childwin, helpers;

{$R *.DFM}


procedure TCreateTableForm.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;


procedure TCreateTableForm.ButtonCreateClick(Sender: TObject);
var
  ctquery, pkstr, unstr, instr : String;
  i       : Integer;
begin
  with MainForm.ActiveMDIChild do
  begin
    // Prepare Query:
    ctquery := 'CREATE TABLE ' + mainform.mask(EditTablename.Text) + ' (';

    // Fields
    for i := 0 to length(fields) - 1 do
    begin
      ctquery := ctquery + mainform.mask(fields[i].Name) + ' ' +
        comboboxtype.items[fields[i].Typ];                              // Typ
      if fields[i].LengthSet <> '' then
        ctquery := ctquery  + ' (' + fields[i].LengthSet + ')';         // Length/Set
      if fields[i].Binary then
        ctquery := ctquery  + ' BINARY';                                // Binary
      if fields[i].Unsigned then
        ctquery := ctquery  + ' UNSIGNED';                              // Unsigned
      if fields[i].Zerofill then
        ctquery := ctquery  + ' ZEROFILL';                              // Zerofill
      if fields[i].Default <> '' then
        ctquery := ctquery  + ' DEFAULT ''' + fields[i].Default + '''';   // Default
      if fields[i].NotNull then
        ctquery := ctquery  + ' NOT NULL';                              // Not null
      if fields[i].AutoIncrement then
        ctquery := ctquery  + ' AUTO_INCREMENT';                         // AutoIncrement

      if i < length(fields)-1 then
        ctquery := ctquery + ', '
    end;

    // Indexes:
    pkstr := '';
    unstr := '';
    instr := '';
    for i := 0 to length(fields) - 1 do
    begin
      if fields[i].Primary then
      begin
        if pkstr <> '' then pkstr := pkstr + ',';
        pkstr := pkstr + mainform.mask(fields[i].Name);
      end;
      if fields[i].Unique then
      begin
        if unstr <> '' then unstr := unstr + ',';
        unstr := unstr + mainform.mask(fields[i].Name);
      end;
      if fields[i].Index then
      begin
        if instr <> '' then instr := instr + ',';
        instr := instr + mainform.mask(fields[i].Name);
      end;
    end;
    if pkstr <> '' then
      ctquery := ctquery + ', PRIMARY KEY(' + pkstr + ')';
    if unstr <> '' then
      ctquery := ctquery + ', UNIQUE(' + unstr + ')';
    if instr <> '' then
      ctquery := ctquery + ', INDEX(' + instr + ')';

    // End:
    ctquery := ctquery + ') ';

    // Comment:
    if EditDescription.Text <> '' then
      ctquery := ctquery + ' COMMENT = "' + EditDescription.Text + '"';

    if (ComboBoxTableType.Text <> '') and (ComboBoxTableType.Text <> TBLTYPE_AUTOMATIC) then
      ctquery := ctquery + ' TYPE = ' + ComboBoxTableType.Text;

    with TMDIChild(Application.Mainform.ActiveMDIChild) do
    begin
      ExecUseQuery( DBComboBox.Text );
      ExecQuery( ctquery );
      ShowDBProperties(self);
      ActualTable := EditTablename.Text;
    end;
  end;
  close;
end;



procedure TCreateTableForm.refreshfields(Sender: TObject);
var i : word;
begin
  // refresh field-list
  with feldListe do
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
  if index = feldListe.Items.Count - 1 then
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
  feldListeClick(self);
  if length(fields) = 0 then
  begin
    ButtonMoveUp.Enabled := false;
    ButtonMoveDown.Enabled := false;
    ButtonCreate.Enabled := false;
  end;
end;


procedure TCreateTableForm.checktypes(Sender: TObject);
begin
  // "binary" is only valid for char's and varchar's
  if ComboBoxType.ItemIndex in [13,14] then
    CheckBoxBinary.Enabled := true
  else begin
    CheckBoxBinary.Checked := false;
    CheckBoxBinary.Enabled := false;
  end;

  // "unsigned" is only valid for numerical columns (not for float's!)
  if ComboBoxType.ItemIndex in [0,1,2,3,4] then
    CheckBoxUnsigned.Enabled := true
  else begin
    CheckBoxUnsigned.Checked := false;
    CheckBoxUnsigned.Enabled := false;
  end;

  // "zerofill" is only valid for numerical and float-columns
  if ComboBoxType.ItemIndex in [0,1,2,3,4,5,6,7] then
    CheckBoxZerofill.Enabled := true
  else begin
    CheckBoxZerofill.Checked := false;
    CheckBoxZerofill.Enabled := false;
  end;
end;


procedure TCreateTableForm.ComboBoxTypeChange(Sender: TObject);
begin
  // Type
  fields[index].Typ := ComboBoxType.ItemIndex;
  checktypes(self);
end;

procedure TCreateTableForm.EditLengthSetChange(Sender: TObject);
begin
  // LengthSet
  fields[index].LengthSet := EditLengthSet.Text;
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
    Typ := 0;
    LengthSet := '3';
    default := '0';
    Primary := false;
    Index := false;
    Unique := false;
    Binary := false;
    Unsigned := true;
    Zerofill := false;
    NotNull := false;
    Autoincrement := false;
    feldListe.Items.Add(Name);
  end;
  refreshfields(self);
  EditFieldnameChange(self);
  feldListe.ItemIndex := index;
  // ButtonCreate enable!
  ButtonCreate.Enabled := length(fields)>0;
end;

procedure TCreateTableForm.EditFieldnameChange(Sender: TObject);
begin
  // Field Name EditChange
  if (validName(EditFieldName.Text)) and (notinlist(EditFieldName.Text, feldListe.Items)) then
  begin
    buttonAdd.Enabled := true;
    buttonChange.Enabled := true;
    ButtonAdd.Default := True;
  end
  else begin
    buttonAdd.Enabled := false;
    buttonChange.Enabled := false;
  end;
  ButtonsChange(self);
end;

procedure TCreateTableForm.feldListeClick(Sender: TObject);
begin
  // ListColumns Change
  index := feldListe.ItemIndex;
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
    if notinlist(EditFieldName.Text, feldListe.Items) then
      buttonChange.Enabled := true
    else
      buttonChange.Enabled := false;
  end;
end;

procedure TCreateTableForm.FormShow(Sender: TObject);
var
  i         : Integer;
  tn        : TTreeNode;
  mdichild  : TMDIChild;
  menu      : TMenuItem;
begin
  // FormShow!

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
    DBComboBox.ItemIndex := 0;
    for i:=0 to DBComboBox.Items.Count-1 do
    begin
      if DBComboBox.Items[i] = ActualDatabase then
        DBComboBox.ItemIndex := i;
    end;
    if DBComboBox.ItemIndex = -1 then
      DBComboBox.ItemIndex := 0;
  end;

  mdichild := TMDIChild(Application.Mainform.ActiveMDIChild);
  if mdichild.mysql_version >= 32300 then
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
  menu := mdichild.popupDbGrid.Items.Find('Change Type');
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
    
  index := -1;
  setLength(fields, 0);
  feldListe.Items.Clear;
  EditTableName.Text := 'TableName';
  EditFieldName.Text := 'FieldName';
  Editdescription.Text := '';
  ButtonCreate.Enabled := false;
  ButtonAdd.Enabled := true;
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
  Label6.Enabled := false; // Type
  Label7.Enabled := false; // Length
  Label8.Enabled := false; // Set
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
  Label6.Enabled := true; // Type
  Label7.Enabled := true; // Length
  Label8.Enabled := true; // Set
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
    ComboBoxType.ItemIndex := Typ;
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

end.
