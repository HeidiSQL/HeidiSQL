unit fieldeditor;


// -------------------------------------
// Field-/Index-Editor
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, ToolWin, ExtCtrls, Buttons;

type
  TFieldEditorMode = (femFieldAdd,femFieldUpdate,femIndexEditor);

  TFieldEditForm = class(TForm)
    pc: TPageControl;
    TabSheet1: TTabSheet;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    EditDefault: TEdit;
    EditLength: TEdit;
    ComboBoxType: TComboBox;
    EditFieldname: TEdit;
    GroupBox1: TGroupBox;
    CheckBoxBinary: TCheckBox;
    CheckBoxUnsigned: TCheckBox;
    CheckBoxZerofill: TCheckBox;
    CheckBoxNotNull: TCheckBox;
    CheckBoxAutoIncrement: TCheckBox;
    TabSheet2: TTabSheet;
    ComboBoxKeys: TComboBox;
    Label4: TLabel;
    CheckBoxUnique: TCheckBox;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    Label6: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label7: TLabel;
    ButtonAddPrimary: TButton;
    ComboBoxPosition: TComboBox;
    Label8: TLabel;
    CheckBoxFulltext: TCheckBox;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    TabSheet3: TTabSheet;
    Label9: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure AddUpdateField(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure pcChange(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure UpdateKeys(Sender: TObject);
    procedure ComboBoxKeysChange(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure InitFieldEditor(Sender: TObject);
    procedure InitIndexEditor(Sender: TObject);
    procedure RemoveField(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonAddPrimaryClick(Sender: TObject);
    procedure ShowKeys(index: Integer=0);
    procedure CheckBoxUniqueClick(Sender: TObject);
    procedure AddField(Sender: TObject);
    procedure ComboBoxKeysOnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure CheckBoxFulltextClick(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure togglebuttons(Sender: TObject);
    procedure ComboBoxKeysDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
    TempKeys : TStringList;
    FMode : TFieldEditorMode;
    FFieldName : String;
    FFlags : String;
    function GetInFieldMode: Boolean;
    function GetInIndexMode: Boolean;
  public
    //UpdateField  : Boolean;        // Update or Add a field? (is set in formshow)
    procedure Init (AMode : TFieldEditorMode; AFieldName : String = ''; AFlags : String = '');
    property InFieldMode : Boolean read GetInFieldMode;
    property InIndexMode : Boolean read GetInIndexMode;
  end;

  function FieldEditorWindow (AOwner : TComponent; AMode : TFieldEditorMode; AFieldName : String = ''; AFlags : String = '') : Boolean;

var
  FieldEditForm: TFieldEditForm;

const
  tempfieldname = 'temp_fieldname';
  
{$I const.inc}

  // field flags
  FF_HAS_LENGTH_PROP = 1;
  // FF_xxx = 2
  // FF_yyy = 4
  // FF_zzz = 8
  // ....

type
  TMysqlIndex = record
    Name : String[64];
    Columns : TStringList;
    Unique : Boolean;
    Fulltext : Boolean;
    Modified : Boolean;
    Ready : Boolean;
  end;

  type TMysqlDataTypeRecord = record
    Index : Integer;
    Name : String[15];
    Flags : Integer; // bitset : (bit 0 = NeedsLength, bit 1 = ....
    DefLength : Integer; // Can we auto-determine this from server ??
  end;

var
  MySqlDataTypeArray : array [0..24] of TMysqlDataTypeRecord =
  (
    (Index: 0; Name: 'TINYINT'),
    (Index: 1; Name: 'SMALLINT'),
    (Index: 2; Name: 'MEDIUMINT'),
    (Index: 3; Name: 'INT'),
    (Index: 4; Name: 'BIGINT'),
    (Index: 5; Name: 'FLOAT'),
    (Index: 6; Name: 'DOUBLE'),
    (Index: 7; Name: 'DECIMAL'),
    (Index: 8; Name: 'DATE'),
    (Index: 9; Name: 'DATETIME'),
    (Index: 10; Name: 'TIMESTAMP'),
    (Index: 11; Name: 'TIME'),
    (Index: 12; Name: 'YEAR'),
    (Index: 13; Name: 'CHAR'),
    (Index: 14; Name: 'VARCHAR'; Flags: FF_HAS_LENGTH_PROP; DefLength: 50),
    (Index: 15; Name: 'TINYBLOB'),
    (Index: 16; Name: 'TINYTEXT'),
    (Index: 17; Name: 'TEXT'),
    (Index: 18; Name: 'BLOB'),
    (Index: 19; Name: 'MEDIUMBLOB'),
    (Index: 20; Name: 'MEDIUMTEXT'),
    (Index: 21; Name: 'LONGBLOB'),
    (Index: 22; Name: 'LONGTEXT'),
    (Index: 23; Name: 'ENUM'),
    (Index: 24; Name: 'SET')
  );

implementation

uses helpers, childwin, Main;

var klist : Array of TMysqlIndex;
{$R *.DFM}

function FieldEditorWindow (AOwner : TComponent; AMode : TFieldEditorMode; AFieldName : String = ''; AFlags : String = '') : Boolean;
var
  f : TFieldEditForm;
begin
  f := TFieldEditForm.Create(AOwner);
  f.Init (AMode,AFieldName,AFlags);
  Result := (f.ShowModal = mrOK);
  FreeAndNil (f);
end;



procedure TFieldEditForm.Init(AMode: TFieldEditorMode; AFieldName,
  AFlags: String);
begin
  FMode := AMode;
  FFieldName := AFieldName;
  FFlags := AFlags;

  if InFieldMode then
    InitFieldEditor (nil);

  if InIndexMode then
    InitIndexEditor (nil);
end;

procedure TFieldEditForm.InitFieldEditor(Sender: TObject);
var
  strtype  : String;
  i : Integer;
begin
  // ====================
  // Field-Editor:
  // ====================

  //
  ComboBoxPosition.Items.Clear;
  ComboBoxPosition.Items.Add('At End of Table');
  ComboBoxPosition.Items.Add('At Beginning of Table');

  // get fieldlist

  // add fieldnames
  with TMDIChild(Application.Mainform.ActiveMDIChild).ListColumns do
    begin
      for i:=0 to items.Count-1 do
        ComboBoxPosition.Items.Add('AFTER ' + mainform.mask(items[i].Caption));
    end;

  case FMode of
    femFieldAdd:
      begin
        //ShowMessageFmt ('Add field',[]);

        EditFieldName.Text := 'FieldName';
        ComboBoxType.ItemIndex := 0;
        EditLength.Text := '';
        EditDefault.Text := '';
        CheckBoxUnsigned.Checked := true;

        if TMDIChild(Application.Mainform.ActiveMDIChild).ListColumns.Selected <> nil then
          ComboBoxPosition.ItemIndex := TMDIChild(Application.Mainform.ActiveMDIChild).ListColumns.Selected.Index+2
        else
          ComboBoxPosition.ItemIndex := 0;
      end;
    femFieldUpdate:
      begin
        //ShowMessageFmt ('Changing field: "%s"',[FFieldName]);

        // request field properties
        // SHOW COLUMNS FROM `db`.`table` LIKE '%s';

        with TMDIChild(Application.Mainform.ActiveMDIChild).ListColumns.Selected do
          begin


            EditFieldname.Text := Caption;
            EditLength.Text := getEnumValues(Subitems[0]);
            EditDefault.Text := Subitems[2];

            // extract field type
            strtype := UpperCase(Subitems[0]);
            if Pos ('(',strtype) > 0 then
              strtype := Trim(copy (strtype,0,Pos ('(',strtype)-1));

            // field field type structure
            for i := Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do
              begin
                if (strtype=MySqlDataTypeArray[i].Name) then
                  begin
                    if (MySqlDataTypeArray[i].Flags And FF_HAS_LENGTH_PROP) = FF_HAS_LENGTH_PROP then
                      begin
                        // enable / disable length field
                        // get default length ..
                      end;

                    ComboBoxType.ItemIndex := MySqlDataTypeArray[i].Index;
                    Break;
                  end;
              end;



            // set attributes:
            if pos('binary', strtype) <> 0 then
              begin
                CheckBoxBinary.Checked := true;
                CheckBoxUnsigned.Checked := false;
                CheckBoxZerofill.Checked := false;
              end
            else if pos('unsigned zerofill', strtype) <> 0 then
              begin
                CheckBoxBinary.Checked := false;
                CheckBoxUnsigned.Checked := true;
                CheckBoxZerofill.Checked := true;
              end
            else if pos('unsigned', strtype) <> 0 then
              begin
                CheckBoxBinary.Checked := false;
                CheckBoxUnsigned.Checked := true;
                CheckBoxZerofill.Checked := false;
              end
            else
              begin
                CheckBoxBinary.Checked := false;
                CheckBoxUnsigned.Checked := false;
                CheckBoxZerofill.Checked := false;
              end;

            if lowercase(Subitems[1]) = 'yes' then
              CheckBoxNotNull.Checked := false
            else
              checkBoxNotNull.Checked := true;

            if lowercase(Subitems[3]) = 'auto_increment' then
              CheckBoxAutoIncrement.Checked := True
            else
              CheckBoxAutoIncrement.Checked := False;

          end;
      end;
    femIndexEditor: ;
  end;

end;


procedure TFieldEditForm.InitIndexEditor(Sender: TObject);
var
  i : Integer;
begin
  // ====================
  // Index-Editor:
  // ====================
  ListBox1.Items.Clear;
  ListBox2.Items.Clear;
  setlength(klist, 0);
  TempKeys := TStringList.Create;

  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    ZQuery3.SQL.Clear();
    ZQuery3.SQL.Add( 'SHOW KEYS FROM ' + mainform.mask(ActualTable) );
    ZQuery3.Open;
    ZQuery3.First;
    for i:=1 to ZQuery3.RecordCount do
    begin
      if TempKeys.IndexOf(ZQuery3.Fields[2].AsString) = -1 then
      begin
        TempKeys.Add(ZQuery3.Fields[2].AsString);
        setlength(klist, length(klist)+1);
        klist[length(klist)-1].Name := ZQuery3.Fields[2].AsString;
        klist[length(klist)-1].Columns := TStringList.Create;
        klist[length(klist)-1].Columns.Add(ZQuery3.Fields[4].AsString);
        klist[length(klist)-1].Modified := false;
        klist[length(klist)-1].Unique := (ZQuery3.Fields[1].AsString = '0');
        if mysql_version < 40002 then
          klist[length(klist)-1].Fulltext := (ZQuery3.FieldByName('Comment').AsString = 'FULLTEXT')
        else
          klist[length(klist)-1].Fulltext := (ZQuery3.FieldByName('Index_type').AsString = 'FULLTEXT')
      end else
        klist[TempKeys.IndexOf(ZQuery3.Fields[2].AsString)].Columns.Add(ZQuery3.Fields[4].AsString);
      ZQuery3.Next;
    end;

    for i:=0 to ListColumns.Items.Count-1 do begin
      if ListColumns.Items[i] <> nil then
        self.ListBox2.Items.Add(ListColumns.Items[i].Caption);
    end;
  end;
  showkeys();
end;


procedure TFieldEditForm.FormShow(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to pc.PageCount - 1 do
    pc.Pages[i].TabVisible := False;

  if InFieldMode then
    begin
      Caption := TMDIChild(Mainform.ActiveMDIChild).ZQuery3.Connection.Hostname + ' - Field Editor';
      pc.Pages[0].TabVisible := True;
      pc.ActivePageIndex := 0;
      EditFieldName.SetFocus();
    end;

  if InIndexMode then
    begin
      Caption := TMDIChild(Mainform.ActiveMDIChild).ZQuery3.Connection.Hostname + ' - Index Editor';
      pc.Pages[1].TabVisible := True;
      pc.ActivePageIndex := 1;
      if Length(klist) > 0 then
        ComboBoxKeys.SetFocus();
    end;


  ComboBoxTypeChange(self);
  //pc.ActivePage := TabSheet1;

  pc.OnChange(self);
end;


function TFieldEditForm.GetInFieldMode: Boolean;
begin
  Result := FMode in [femFieldAdd,femFieldUpdate];
end;

function TFieldEditForm.GetInIndexMode: Boolean;
begin
  Result := FMode in [femIndexEditor];
end;

procedure TFieldEditForm.ComboBoxTypeChange(Sender: TObject);
begin
  // Attributes

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

  // Length/Set NOT valid for date/time/memo/blob-columns
  if ComboBoxType.ItemIndex in [8,9,11,15..22] then
    EditLength.Enabled := false
  else begin
    EditLength.Enabled := true;
  end;

  // "default" NOT valid for memo/blob-columns
  if ComboBoxType.ItemIndex in [15..22] then
    EditDefault.Enabled := false
  else begin
    EditDefault.Enabled := true;
  end;

end;

procedure TFieldEditForm.AddUpdateField(Sender: TObject);
var
  strNotNull,
  strAttributes,
  strAutoIncrement,
  strLengthSet,
  strDefault,
  strPosition,
  fielddef : String;
begin
  // Apply Changes to field-definition

  // move field if position changed
  if (ComboBoxPosition.ItemIndex > -1) and (FMode in [femFieldUpdate]) then
    begin // Move field position
      if MessageDLG('You are about to move a field''s position in the table-structure. While there is no handy one-query-method in MySQL to do that, this will be done in 4 steps:'+CRLF+
       ' 1. Adding a temporary field at the specified position'+CRLF+
       ' 2. Filling the temporary field with the same data as source field'+CRLF+
       ' 3. Dropping the source-field'+CRLF+
       ' 4. Renaming the temporary field to it''s original name.'+CRLF+CRLF+
       'Be aware that this method can mess up existing indexes in your table or even can result in losing data! If you are not sure you should not use this function on indexed fields.'+CRLF+CRLF+
       'Continue?',
       mtConfirmation,
       [mbYes, mbCancel],
       0
       ) <> mrYes then
      Exit;
    end;

  Screen.Cursor := crSQLWait;

  strAttributes := ''; // none of the 3 attributes binary, unsigned, zerofill
  strNotNull := '';
  strDefault := '';
  strAutoIncrement := '';

  if CheckBoxBinary.Checked = true then
    strAttributes := strAttributes + ' BINARY';

  if CheckBoxUnsigned.Checked = true then
    strAttributes := strAttributes + ' UNSIGNED';

  if CheckBoxZerofill.Checked = true then
    strAttributes := strAttributes + ' ZEROFILL';

  if (length(EditDefault.Text) > 0) and EditDefault.Enabled then
    strDefault := ' DEFAULT "' + escape_string(EditDefault.Text) + '"';

  if CheckBoxNotNull.Checked = True then
    strNotNull := ' NOT NULL';

  if CheckBoxAutoIncrement.Checked = True then
    strAutoIncrement := ' AUTO_INCREMENT';

  if (EditLength.text <> '') and EditLength.Enabled then
    strLengthSet := '(' + EditLength.text + ') '
  else
    strLengthSet := '';

  strPosition := '';
  case ComboBoxPosition.ItemIndex of
    0 : ;
    1 : strPosition := ' FIRST';
  else
    strPosition := ' ' + ComboBoxPosition.Text;
  end;

  fielddef := ComboBoxType.Text +     // Type
        strLengthSet +                // Length/Set
        strAttributes +               // Attribute
        strDefault +                  // Default
        strNotNull +                  // Not Null
        strAutoIncrement;             // Auto_increment


  with TMDIChild(Mainform.ActiveMDIChild) do
    begin
      if (FMode=femFieldAdd) then
        begin
          ExecQuery('ALTER TABLE ' + mainform.mask(ActualTable) +           // table
            ' ADD ' + mainform.mask(EditFieldname.Text) + ' ' +     // new name
            fielddef +
            strPosition                                             // Position
            )
        end
      else if (FMode=femFieldUpdate) then
        begin
          ExecQuery('ALTER TABLE ' + mainform.mask(ActualTable) +                      // table
            ' CHANGE ' + mainform.mask(ListColumns.Selected.Caption) + ' ' +     // old name
            mainform.mask(EditFieldName.Text) + ' ' +                          // new name
            fielddef
            );

          //ShowMessageFmt ('ComboBox position: %d',[ComboBoxPosition.ItemIndex]);

          if ComboBoxPosition.ItemIndex > -1 then  // Move field position
            begin
              ExecQuery('ALTER TABLE ' + mainform.mask(ActualTable) +           // table
              ' ADD ' + mainform.mask(tempfieldname) + ' ' +          // new name
              fielddef +
              strPosition                                             // Position
              );
              ExecQuery('UPDATE ' + mainform.mask(ActualTable) + ' SET '+mainform.mask(tempfieldname)+'='+mainform.mask(EditFieldName.Text));
              ExecQuery('ALTER TABLE ' + mainform.mask(ActualTable) + ' DROP '+mainform.mask(EditFieldName.Text));
              ExecQuery('ALTER TABLE ' + mainform.mask(ActualTable) + ' CHANGE '+
                mainform.mask(tempfieldname)+' '+mainform.mask(EditFieldName.Text) + ' ' +
                fielddef
              );
            end;  
        end;

      ShowTableProperties(self);
    end;

  Screen.Cursor := crDefault;
  close;
end;

procedure TFieldEditForm.ButtonCancelClick(Sender: TObject);
begin
  // cancel
  ModalResult := mrCancel;
end;

procedure TFieldEditForm.pcChange(Sender: TObject);
begin
  case FMode of
    femFieldAdd:    ButtonOK.Caption := 'Add';
    femFieldUpdate: ButtonOK.Caption := 'Update';
    femIndexEditor: ButtonOK.Caption := 'Update';
  else
    ButtonOK.Caption := 'Close';
  end;

end;


procedure TFieldEditForm.OKClick(Sender: TObject);
begin
  // add/update what?
  if InFieldMode then
    begin
      AddUpdateField(self);
      ModalResult := mrOK;
    end
  else if InIndexMode then
    begin
      UpdateKeys(self);
      ModalResult := mrOK;
    end
  else
    ModalResult := mrCancel;
end;

procedure TFieldEditForm.ComboBoxKeysChange(Sender: TObject);
var i : Integer;
begin
  // Change actual index!
  if ComboBoxKeys.ItemIndex > -1 then begin
    ListBox2.Items.Clear;
    with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
      for i:=0 to ListColumns.Items.Count-1 do
        if (ListColumns.Items[i] <> nil) and (klist[self.ComboBoxKeys.ItemIndex].columns.Indexof(ListColumns.Items[i].Caption)=-1) then
          self.ListBox2.Items.Add(ListColumns.Items[i].Caption);
    end;
    with klist[ComboBoxKeys.ItemIndex] do begin
      ListBox1.Items := Columns;
      ListBox1.Items := Columns;
      CheckBoxUnique.OnClick := nil;
      CheckBoxUnique.Checked := Unique;
      CheckBoxUnique.OnClick := CheckBoxUniqueClick;
      CheckBoxFulltext.OnClick := nil;
      CheckBoxFulltext.Checked := Fulltext;
      CheckBoxFulltext.OnClick := CheckBoxFulltextClick;
      CheckBoxUnique.Enabled := not (ComboBoxKeys.Text = 'PRIMARY');
      CheckBoxFulltext.Enabled := not (ComboBoxKeys.Text = 'PRIMARY');
      ButtonDelete.Enabled := true;
      ListBox1.Enabled := true;
      ListBox2.Enabled := true;
    end;
  end;
  togglebuttons(self);
end;


procedure TFieldEditForm.ButtonAddClick(Sender: TObject);
var kname : String;
begin
  // Add Index!
  kname := 'NewIndex';
  if not InputQuery('New Index...', 'Index-Name:', kname) then
    exit;
  if ComboBoxKeys.Items.IndexOf(kname) > -1 then begin
    MessageDlg('Index-Name '''+kname+''' already used!', mtError, [mbOk], 0);
    exit;
  end;
  setlength(klist, length(klist)+1);
  klist[length(klist)-1].Name := kname;
  klist[length(klist)-1].Columns := TStringList.Create;
  klist[length(klist)-1].Unique := false;
  klist[length(klist)-1].Fulltext := false;
  klist[length(klist)-1].Modified := true;
  showkeys(length(klist)-1);
end;


procedure TFieldEditForm.ButtonDeleteClick(Sender: TObject);
var i,j : Integer;
begin
  // Delete Index!
  i := ComboBoxKeys.ItemIndex;
  if i > -1 then
  if MessageDlg('Delete Index ''' + ComboBoxKeys.Text + ''' ?',
    mtConfirmation, [mbYes,mbCancel], 0) = mrYes then begin
    inc(i); // jump to next entry after the one to delete!
    for j:=i to length(klist)-1 do
      klist[j-1] := klist[j];
    setlength(klist, length(klist)-1);
    ShowKeys(i-2);
  end;
end;

procedure TFieldEditForm.ButtonAddPrimaryClick(Sender: TObject);
begin
  // Add primary key!
  setlength(klist, length(klist)+1);
  klist[length(klist)-1].Name := 'PRIMARY';
  klist[length(klist)-1].Columns := TStringList.Create;
  klist[length(klist)-1].Unique := false;
  klist[length(klist)-1].Fulltext := false;
  klist[length(klist)-1].Modified := true;
  ShowKeys(length(klist)-1);
  ButtonAddPrimary.Enabled := false;
end;


procedure TFieldEditForm.ShowKeys(index: Integer=0);
var
  i : Integer;
begin
  // Show indexes in combobox!
  ComboBoxKeys.Items.Clear;
  ButtonAddPrimary.Enabled := true;
  ButtonDelete.Enabled := false;
  ListBox1.Enabled := false;
  ListBox2.Enabled := false;
  BitBtn1.Enabled := false;
  BitBtn2.Enabled := false;
  for i:=0 to length(klist)-1 do
    ComboBoxKeys.Items.Add(klist[i].Name);

  if ComboBoxKeys.Items.IndexOf('PRIMARY') > -1 then
    ButtonAddPrimary.Enabled := false;

  if (index = -1) and (length(klist) > 0) then
    index := 0;
  if index < length(klist) then // careful: only if given index is < length(klist)
    ComboBoxKeys.ItemIndex := index;

  with ComboBoxKeys do
  if Items.Count = 0 then begin
    Enabled := false;
    Color := clBtnFace;
  end else begin
    Enabled := true;
    Color := clWindow;
  end;

  ComboBoxKeys.OnChange(self);
end;


procedure TFieldEditForm.CheckBoxUniqueClick(Sender: TObject);
begin
  // make index unique!
  klist[ComboBoxKeys.ItemIndex].Unique := CheckBoxUnique.Checked;
  if CheckBoxUnique.Checked then begin
    klist[ComboBoxKeys.ItemIndex].Fulltext := false;
    CheckBoxFulltext.Checked := false;
  end;
  klist[ComboBoxKeys.ItemIndex].Modified := true;
end;

procedure TFieldEditForm.CheckBoxFulltextClick(Sender: TObject);
begin
  // make index fulltext!
  klist[ComboBoxKeys.ItemIndex].Fulltext := CheckBoxFulltext.Checked;
  if CheckBoxFulltext.Checked then begin
    klist[ComboBoxKeys.ItemIndex].Unique := false;
    CheckBoxUnique.Checked := false;
  end;
  klist[ComboBoxKeys.ItemIndex].Modified := true;
end;


procedure TFieldEditForm.UpdateKeys(Sender: TObject);
var
  i,j, index : Integer;
  query1, query : String;
begin
  // update keys!
  // for each TempKey (see OnFormShow) check if it was changed or even deleted in klist
  query1 := 'ALTER TABLE ' + mainform.mask(TMDIChild(Application.Mainform.ActiveMDIChild).ActualTable);
  for i:=0 to TempKeys.Count-1 do begin

    index := -1;

    for j:=0 to length(klist)-1 do begin
      if TempKeys[i] = klist[j].Name then begin
        index := j;
        break;
      end;
    end;

    if (index > -1) and (klist[index].Modified) then begin
      // modify existing key
      // PK:
      if klist[index].Name = 'PRIMARY' then
        query := query1 + ' DROP PRIMARY KEY' +
          ', ADD PRIMARY KEY (' + implodestr(',', klist[index].Columns) + ')'
      // UNIQUE:
      else if klist[index].Unique then
        query := query1 + ' DROP INDEX ' + mainform.mask(klist[index].Name) +
          ', ADD UNIQUE ' + klist[index].Name + ' (' + implodestr(',', klist[index].Columns) + ')'
      // FULLTEXT:
      else if klist[index].Fulltext then
        query := query1 + ' DROP INDEX ' + mainform.mask(klist[index].Name) +
          ', ADD FULLTEXT ' + klist[index].Name + ' (' + implodestr(',', klist[index].Columns) + ')'
      // INDEX:
      else
        query := query1 + ' DROP INDEX '+ mainform.mask(klist[index].Name) +
          ', ADD INDEX '+klist[index].Name+' (' + implodestr(',', klist[index].Columns) + ')';
      TMDIChild(Application.Mainform.ActiveMDIChild).ExecQuery(query);
      klist[index].Modified := false;
    end

    else if index = -1 then begin
      // delete existing key
      if TempKeys[i] = 'PRIMARY' then
        query := query1 + ' DROP PRIMARY KEY'
      else
        query := query1 + ' DROP INDEX ' + mainform.mask(TempKeys[i]);
      TMDIChild(Application.Mainform.ActiveMDIChild).ExecQuery(query);
    end;

    if index > -1 then
      klist[index].Ready := true;
  end;

  for j:=0 to length(klist)-1 do begin
    if (not klist[j].Ready) then begin
      // Add a new key
      // PK:
      if klist[j].Name = 'PRIMARY' then
        query := query1 + ' ADD PRIMARY KEY (' + implodestr(',', klist[j].Columns) + ')'
      // UNIQUE:
      else if klist[j].Unique then
        query := query1 + ' ADD UNIQUE ' + mainform.mask(klist[j].Name) + ' (' + implodestr(',', klist[j].Columns) + ')'
      // UNIQUE:
      else if klist[j].Fulltext then
        query := query1 + ' ADD FULLTEXT ' + mainform.mask(klist[j].Name) + ' (' + implodestr(',', klist[j].Columns) + ')'
      // INDEX:
      else
        query := query1 + ' ADD INDEX '+ mainform.mask(klist[j].Name) + ' (' + implodestr(',', klist[j].Columns) + ')';
      TMDIChild(Application.Mainform.ActiveMDIChild).ExecQuery(query);
    end;
  end;

  TMDIChild(Application.Mainform.ActiveMDIChild).ShowTableProperties(self);
  close;
end;


procedure TFieldEditForm.AddField(Sender: TObject);
var sel : Integer;
begin
  // add column to index
  if ListBox2.ItemIndex > -1 then begin
    ListBox1.Items.Add(ListBox2.Items[ListBox2.ItemIndex]);
    klist[ComboBoxKeys.ItemIndex].Columns.Add(ListBox2.Items[ListBox2.ItemIndex]);
    if ListBox2.ItemIndex > 0 then
      sel := ListBox2.ItemIndex-1
    else if ListBox2.Items.Count > 1 then
      sel := 0
    else
      raise Exception.Create('Unexpected sel value in AddField.');
    ListBox2.Items.Delete(ListBox2.ItemIndex);
    klist[ComboBoxKeys.ItemIndex].Modified := true;
    ListBox2.ItemIndex := sel;
  end;
  togglebuttons(self);
end;

procedure TFieldEditForm.BitBtn3Click(Sender: TObject);
begin
  // add all columns to index
  ListBox1.Items.AddStrings(ListBox2.Items);
  klist[ComboBoxKeys.ItemIndex].Columns.AddStrings(ListBox2.Items);
  ListBox2.Items.Clear;
  klist[ComboBoxKeys.ItemIndex].Modified := true;
  togglebuttons(self);
end;


procedure TFieldEditForm.RemoveField(Sender: TObject);
var sel : Integer;
begin
  // delete one column from index
  if ListBox1.ItemIndex > -1 then begin
    klist[ComboBoxKeys.ItemIndex].Columns.Delete(ListBox1.ItemIndex);
    klist[ComboBoxKeys.ItemIndex].Modified := true;
    if ListBox1.ItemIndex > 0 then
      sel := ListBox1.ItemIndex-1
    else if ListBox1.Items.Count > 1 then
      sel := 0
    else
      raise Exception.Create('Unexpected sel value in RemoveField.');
    ListBox2.Items.Add(ListBox1.Items[ListBox1.ItemIndex]);
    ListBox1.Items.Delete(ListBox1.ItemIndex);
    ListBox1.ItemIndex := sel;
  end;
  togglebuttons(self);
end;


procedure TFieldEditForm.BitBtn4Click(Sender: TObject);
begin
  // delete all columns from index
  klist[ComboBoxKeys.ItemIndex].Columns.Clear;
  klist[ComboBoxKeys.ItemIndex].Modified := true;
  ListBox2.Items.AddStrings(ListBox1.Items);
  ListBox1.Items.Clear;
  togglebuttons(self);
end;


procedure TFieldEditForm.togglebuttons(Sender: TObject);
begin
  BitBtn1.Enabled := (ListBox2.ItemIndex > -1);
  BitBtn2.Enabled := (ListBox1.ItemIndex > -1);
  BitBtn3.Enabled := (ListBox2.Items.Count > 0);
  BitBtn4.Enabled := (ListBox1.Items.Count > 0);
end;


procedure TFieldEditForm.ComboBoxKeysOnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  // TODO: colors!!
end;




procedure TFieldEditForm.ComboBoxKeysDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  icon : Integer;
  c : tComboBox;
begin
  c := (Control as TComboBox);
  with c.Canvas do
    begin
      Brush.Color := clWindow;
      FillRect(rect);
      if (klist[index].Unique) and (klist[index].Name <> 'PRIMARY') then
        icon := 64
      else if klist[index].Fulltext then
        icon := 65
      else if klist[index].Name = 'PRIMARY' then
        icon := 26
      else
        icon := 63;
      Mainform.ImageList1.Draw(c.canvas, Rect.Left, Rect.Top, Icon);
      Font.Color := clWindowText;
      TextOut(Rect.Left + 18, Rect.Top, c.Items[Index]);
    end;
end;

end.


