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
    tabField: TTabSheet;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    lblName: TLabel;
    lblType: TLabel;
    lblLengthSet: TLabel;
    lblDefault: TLabel;
    EditDefault: TEdit;
    EditLength: TEdit;
    ComboBoxType: TComboBox;
    EditFieldname: TEdit;
    GroupBoxAttributes: TGroupBox;
    CheckBoxBinary: TCheckBox;
    CheckBoxUnsigned: TCheckBox;
    CheckBoxZerofill: TCheckBox;
    CheckBoxNotNull: TCheckBox;
    CheckBoxAutoIncrement: TCheckBox;
    tabIndexes: TTabSheet;
    ComboBoxKeys: TComboBoxEx;
    lblIndexName: TLabel;
    CheckBoxUnique: TCheckBox;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    lblColumnsUsed: TLabel;
    listColumnsUsed: TListBox;
    listColumnsAvailable: TListBox;
    btnAddColumnToIndex: TBitBtn;
    btnDeleteColumnFromIndex: TBitBtn;
    lblColumnsAvailable: TLabel;
    ButtonAddPrimary: TButton;
    ComboBoxPosition: TComboBox;
    lblPosition: TLabel;
    CheckBoxFulltext: TCheckBox;
    btnAddAllColumnsToIndex: TBitBtn;
    btnDeleteAllColumnsFromIndex: TBitBtn;
    btnDatatypeHelp: TButton;
    procedure btnDatatypeHelpClick(Sender: TObject);
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
    procedure CheckBoxFulltextClick(Sender: TObject);
    procedure btnAddAllColumnsToIndexClick(Sender: TObject);
    procedure btnDeleteAllColumnsFromIndexClick(Sender: TObject);
    procedure togglebuttons(Sender: TObject);
  private
    { Private declarations }
    TempKeys : TStringList;
    FMode : TFieldEditorMode;
    FModeWhenCalled : TFieldEditorMode;
    FFieldName : String;
    procedure ValidateControls;
  public
    { Public declarations }
  end;

  function FieldEditorWindow (AOwner : TComponent; AMode : TFieldEditorMode; AFieldName : String = '') : Boolean;

{$I const.inc}

implementation

uses
  helpers, childwin, Main, mysql, db;

var
  klist : Array of TMysqlIndex;

{$R *.DFM}



{***
  Create form
}
function FieldEditorWindow (AOwner : TComponent; AMode : TFieldEditorMode; AFieldName : String = '') : Boolean;
var
  f : TFieldEditForm;
begin
  f := TFieldEditForm.Create(AOwner);
  f.FMode := AMode;
  // Also remember original mode for restoring when switching pagecontrol tabs
  f.FModeWhenCalled := AMode;
  f.FFieldName := AFieldName;
  // Init both editors
  f.InitFieldEditor (nil);
  f.InitIndexEditor (nil);
  Result := (f.ShowModal = mrOK);
  FreeAndNil (f);
end;



{***
  Init Field-Editor
}
procedure TFieldEditForm.InitFieldEditor(Sender: TObject);
var
  strtype        : String;
  i              : Integer;
  ListColumns    : TListView;
begin

  // Initiate "Position"-combobox
  ComboBoxPosition.Items.Clear;
  ComboBoxPosition.Items.Add('At End of Table');
  ComboBoxPosition.Items.Add('At Beginning of Table');

	// Reference to childwin's column-ListView
  ListColumns := Mainform.ChildWin.ListColumns;

  // get fieldlist
  // add fieldnames
  for i:=0 to ListColumns.Items.Count-1 do
  begin
    ComboBoxPosition.Items.Add('AFTER ' + mainform.mask(ListColumns.Items[i].Caption));
  end;

  // re-fill datatypes-combobox
  ComboBoxType.Items.Clear;
  for i := Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do
  begin
    ComboBoxType.Items.Add( MySqlDataTypeArray[i].Name );
  end;

  CheckBoxAutoIncrement.Enabled := true;

  case FMode of
    // "Field" tab in Add-mode
    femFieldAdd, femIndexEditor:
    begin
      CheckBoxAutoIncrement.Enabled := false;
      EditFieldName.Text := 'FieldName';
      ComboBoxType.ItemIndex := 0;
      EditLength.Text := '';
      EditDefault.Text := '';
      CheckBoxUnsigned.Checked := true;

      if ListColumns.Selected <> nil then
        ComboBoxPosition.ItemIndex := ListColumns.Selected.Index+2
      else
        ComboBoxPosition.ItemIndex := 0;
    end;

    // "Field" tab in Update-mode
    femFieldUpdate:
    begin
      EditFieldname.Text := ListColumns.Selected.Caption;
      EditLength.Text := getEnumValues( ListColumns.Selected.Subitems[0] );
      EditDefault.Text := ListColumns.Selected.Subitems[2];

      // extract field type
      strtype := UpperCase( ListColumns.Selected.Subitems[0] );
      if Pos ('(',strtype) > 0 then
        strtype := Trim(copy (strtype,0,Pos ('(',strtype)-1));

      // field field type structure
      for i := Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do
      begin
        if (strtype=MySqlDataTypeArray[i].Name) then
        begin
          if MySqlDataTypeArray[i].HasLength then
          begin
            // enable / disable length field
            // get default length ..
          end;
          ComboBoxType.ItemIndex := MySqlDataTypeArray[i].Index;
          Break;
        end;
      end;

      // set attributes:
      strtype := LowerCase( ListColumns.Selected.Subitems[0] );
      CheckBoxBinary.Checked := pos('binary', strtype) > 0;
      CheckBoxZerofill.Checked := pos('zerofill', strtype) > 0;
      CheckBoxUnsigned.Checked := pos('unsigned', strtype) > 0;
      CheckBoxNotNull.Checked := lowercase(ListColumns.Selected.Subitems[1]) <> 'yes';
      CheckBoxAutoIncrement.Checked := lowercase(ListColumns.Selected.Subitems[3]) = 'auto_increment';

      // TODO: Disable 'auto increment' checkbox if field is not part of index or primary key.
    end;
  end;
end;



{***
  Init Index-Editor
  @todo cleanup code, get rid of WITH statement
}
procedure TFieldEditForm.InitIndexEditor(Sender: TObject);
var
  i : Integer;
  cwin : TMDIChild;
  ds: TDataSet;
begin
  listColumnsUsed.Items.Clear;
  listColumnsAvailable.Items.Clear;
  setlength(klist, 0);
  TempKeys := TStringList.Create;
  cwin := Mainform.ChildWin;
  ds := cwin.GetResults( 'SHOW KEYS FROM ' + mainform.mask(cwin.ActualTable) );
  for i:=1 to ds.RecordCount do
  begin
    if TempKeys.IndexOf(ds.Fields[2].AsString) = -1 then
    begin
      TempKeys.Add(ds.Fields[2].AsString);
      setlength(klist, length(klist)+1);
      klist[length(klist)-1].Name := ds.Fields[2].AsString;
      klist[length(klist)-1].Columns := TStringList.Create;
      klist[length(klist)-1].Columns.Add(ds.Fields[4].AsString);
      klist[length(klist)-1].Modified := false;
      klist[length(klist)-1].Unique := (ds.Fields[1].AsString = '0');
      if cwin.mysql_version < 40002 then
        klist[length(klist)-1].Fulltext := (ds.FieldByName('Comment').AsString = 'FULLTEXT')
      else
        klist[length(klist)-1].Fulltext := (ds.FieldByName('Index_type').AsString = 'FULLTEXT')
    end else
      klist[TempKeys.IndexOf(ds.Fields[2].AsString)].Columns.Add(ds.Fields[4].AsString);
    ds.Next;
  end;

  for i:=0 to cwin.ListColumns.Items.Count-1 do begin
    if cwin.ListColumns.Items[i] <> nil then
      self.listColumnsAvailable.Items.Add(cwin.ListColumns.Items[i].Caption);
  end;
  showkeys();
end;



{***
  FormShow
}
procedure TFieldEditForm.FormShow(Sender: TObject);
begin
  if fMode in [femFieldUpdate, femFieldAdd] then
    begin
      pc.ActivePage := tabField;
      EditFieldName.SetFocus();
    end;

  if fMode in [femIndexEditor] then
    begin
      pc.ActivePage := tabIndexes;
      if Length(klist) > 0 then
        ComboBoxKeys.SetFocus();
    end;

  ComboBoxTypeChange(self);
  ValidateControls;
end;



{***
  User selected a new datatype for the selected field.
  Take care of limitations of different datatypes,
  toggle clickability of certain checkboxes
}
procedure TFieldEditForm.ComboBoxTypeChange(Sender: TObject);
var
  FieldType : TMysqlDataTypeRecord;
begin
  // Attributes

  // Detect column-type
  FieldType := MySqlDataTypeArray[ComboBoxType.ItemIndex];

  // BINARY
  CheckBoxBinary.Enabled := FieldType.HasBinary;
  if not CheckBoxBinary.Enabled then
    CheckBoxBinary.Checked := false; // Ensure checkbox is not ticked

  // UNSIGNED
  CheckBoxUnsigned.Enabled := FieldType.HasUnsigned;
  if not CheckBoxUnsigned.Enabled then
    CheckBoxUnsigned.Checked := false; // Ensure checkbox is not ticked

  // ZEROFILL
  CheckBoxZerofill.Enabled := FieldType.HasZerofill;
  if not CheckBoxZerofill.Enabled then
    CheckBoxZerofill.Checked := false; // Ensure checkbox is not ticked

  // Length/Set
  EditLength.Enabled := FieldType.HasLength;
  lblLengthSet.Enabled := EditLength.Enabled;
  if FieldType.RequiresLength then // Render required field as bold
    lblLengthSet.Font.Style := lblLengthSet.Font.Style + [fsBold]
  else
    lblLengthSet.Font.Style := lblLengthSet.Font.Style - [fsBold];
  if not EditLength.Enabled then
    EditLength.Text := '';
  // Fill length/set value with default value if empty
  if FieldType.RequiresLength then
  begin
    if (EditLength.Text = '') and (FieldType.DefLengthSet <> '') then
      EditLength.Text := FieldType.DefLengthSet;
  end;

  // DEFAULT
  EditDefault.Enabled := FieldType.HasDefault;
  lblDefault.Enabled := EditDefault.Enabled;
  if not EditDefault.Enabled then
    EditDefault.Text := ''; // Ensure text is empty
end;



{***
  Add or update field
  @todo code cleanup
}
procedure TFieldEditForm.AddUpdateField(Sender: TObject);
var
  strNotNull,
  strAttributes,
  strAutoIncrement,
  strLengthSet,
  strDefault,
  strPosition,
  fielddef : String;
  cwin : TMDIChild;
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
  try
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
      strDefault := ' DEFAULT ' + esc(EditDefault.Text);

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

    fielddef :=
      ComboBoxType.Text +     // Type
      strLengthSet +          // Length/Set
      strAttributes +         // Attribute
      strDefault +            // Default
      strNotNull +            // Not Null
      strAutoIncrement;       // Auto_increment

    cwin := Mainform.ChildWin;

    if (FMode = femFieldAdd) then begin
      cwin.ExecUpdateQuery(
        'ALTER TABLE ' + mainform.mask(cwin.ActualTable) + ' ' +  // table
        'ADD ' + mainform.mask(EditFieldname.Text) + ' ' +        // new name
        fielddef +
        strPosition                                               // Position
      );
    end else if (FMode = femFieldUpdate) then begin
      cwin.ExecUpdateQuery(
        'ALTER TABLE ' + mainform.mask(cwin.ActualTable) + ' ' +              // table
        'CHANGE ' + mainform.mask(cwin.ListColumns.Selected.Caption) + ' ' +  // old name
        mainform.mask(EditFieldName.Text) + ' ' +                             // new name
        fielddef
      );

      //ShowMessageFmt ('ComboBox position: %d',[ComboBoxPosition.ItemIndex]);

      if ComboBoxPosition.ItemIndex > -1 then begin
        // Move field position
        cwin.ExecUpdateQuery(
          'ALTER TABLE ' + mainform.mask(cwin.ActualTable) + ' ' +  // table
          'ADD ' + mainform.mask(TEMPFIELDNAME) + ' ' +             // new name
          fielddef +
          strPosition                                               // Position
        );
        cwin.ExecUpdateQuery('UPDATE ' + mainform.mask(cwin.ActualTable) + ' SET '+mainform.mask(TEMPFIELDNAME)+'='+mainform.mask(EditFieldName.Text));
        cwin.ExecUpdateQuery('ALTER TABLE ' + mainform.mask(cwin.ActualTable) + ' DROP '+mainform.mask(EditFieldName.Text));
        cwin.ExecUpdateQuery(
          'ALTER TABLE ' + mainform.mask(cwin.ActualTable) + ' ' +
          'CHANGE ' + mainform.mask(TEMPFIELDNAME) + ' ' +
          mainform.mask(EditFieldName.Text) + ' ' +
          fielddef
        );
      end;
    end;
    cwin.ShowTableProperties(self);
    ModalResult := mrOK;
  except
    on E: THandledSQLError do;
  end;
  Screen.Cursor := crDefault;
end;



{***
  Cancel Form
}
procedure TFieldEditForm.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



{***
  User selected another tab in the main-pagecontrol
}
procedure TFieldEditForm.pcChange(Sender: TObject);
begin
  // Set FMode, according to selected tab
  if pc.ActivePage = tabField then
  begin
    if FModeWhenCalled = femFieldUpdate then
    begin
      // "Field" tab selected and original mode was "UpdateField"
      FMode := femFieldUpdate;
    end
    else
    begin
      // "Field" tab selected and original mode was "AddField"
      FMode := femFieldAdd;
    end;
  end
  else if pc.ActivePage = tabIndexes then
  begin
    // "Index" tab selected
    FMode := femIndexEditor;
  end;

  ValidateControls;

end;



{***
  OK clicked - call correct save-procedure according to current mode
}
procedure TFieldEditForm.OKClick(Sender: TObject);
begin
  // add/update what?
  if fMode in [femFieldUpdate, femFieldAdd] then
    begin
      AddUpdateField(self);
    end;
  if fMode in [femIndexEditor] then
    begin
      UpdateKeys(self);
    end
  else
    ModalResult := mrCancel;
end;



{***
  User selected another index in the combobox
  @todo code cleanup, get rid of WITH statement
}
procedure TFieldEditForm.ComboBoxKeysChange(Sender: TObject);
var i : Integer;
begin
  if ComboBoxKeys.ItemIndex > -1 then begin
    listColumnsAvailable.Items.Clear;
    for i:=0 to Mainform.ChildWin.ListColumns.Items.Count-1 do
      if (Mainform.ChildWin.ListColumns.Items[i] <> nil) and (klist[ComboBoxKeys.ItemIndex].columns.Indexof(Mainform.ChildWin.ListColumns.Items[i].Caption)=-1) then
        listColumnsAvailable.Items.Add(Mainform.ChildWin.ListColumns.Items[i].Caption);
    with klist[ComboBoxKeys.ItemIndex] do begin
      listColumnsUsed.Items := Columns;
      CheckBoxUnique.OnClick := nil;
      CheckBoxUnique.Checked := Unique;
      CheckBoxUnique.OnClick := CheckBoxUniqueClick;
      CheckBoxFulltext.OnClick := nil;
      CheckBoxFulltext.Checked := Fulltext;
      CheckBoxFulltext.OnClick := CheckBoxFulltextClick;
      CheckBoxUnique.Enabled := not (ComboBoxKeys.ItemsEx[ComboBoxKeys.ItemIndex].Caption = 'PRIMARY');
      CheckBoxFulltext.Enabled := not (ComboBoxKeys.ItemsEx[ComboBoxKeys.ItemIndex].Caption = 'PRIMARY');
      ButtonDelete.Enabled := true;
      listColumnsUsed.Enabled := true;
      listColumnsAvailable.Enabled := true;
    end;
  end;
  togglebuttons(self);
end;



{***
  Add new index, init that with default values
}
procedure TFieldEditForm.ButtonAddClick(Sender: TObject);
var kname : String;
begin
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



{***
  Delete existing index
}
procedure TFieldEditForm.ButtonDeleteClick(Sender: TObject);
var i,j : Integer;
begin
  i := ComboBoxKeys.ItemIndex;
  if i > -1 then
  if MessageDlg('Delete Index ''' + ComboBoxKeys.Items[ComboBoxKeys.ItemIndex] + ''' ?',
    mtConfirmation, [mbYes,mbCancel], 0) = mrYes then begin
    inc(i); // jump to next entry after the one to delete!
    for j:=i to length(klist)-1 do
      klist[j-1] := klist[j];
    setlength(klist, length(klist)-1);
    ShowKeys(i-2);
  end;
end;



{***
  Add primary key
}
procedure TFieldEditForm.ButtonAddPrimaryClick(Sender: TObject);
begin
  setlength(klist, length(klist)+1);
  klist[length(klist)-1].Name := 'PRIMARY';
  klist[length(klist)-1].Columns := TStringList.Create;
  klist[length(klist)-1].Unique := false;
  klist[length(klist)-1].Fulltext := false;
  klist[length(klist)-1].Modified := true;
  ShowKeys(length(klist)-1);
  ButtonAddPrimary.Enabled := false;
end;



{***
  Show indexes in combobox
  @param integer ItemIndex to select
}
procedure TFieldEditForm.ShowKeys(index: Integer=0);
var
  i, icon : Integer;
begin
  ComboBoxKeys.ItemsEx.Clear;
  ButtonAddPrimary.Enabled := true;
  ButtonDelete.Enabled := false;
  listColumnsUsed.Enabled := false;
  listColumnsAvailable.Enabled := false;
  btnAddColumnToIndex.Enabled := false;
  btnDeleteColumnFromIndex.Enabled := false;
  for i:=0 to length(klist)-1 do
  begin
    if (klist[i].Unique) and (klist[i].Name <> 'PRIMARY') then
      icon := ICONINDEX_UNIQUEKEY
    else if klist[i].Fulltext then
      icon := ICONINDEX_FULLTEXTKEY
    else if klist[i].Name = 'PRIMARY' then
      icon := ICONINDEX_PRIMARYKEY
    else
      icon := ICONINDEX_INDEXKEY;
    ComboBoxKeys.ItemsEx.AddItem( klist[i].Name, icon, icon, -1, 0, nil);
  end;

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



{***
  Make index unique!
}
procedure TFieldEditForm.CheckBoxUniqueClick(Sender: TObject);
begin
  klist[ComboBoxKeys.ItemIndex].Unique := CheckBoxUnique.Checked;
  if CheckBoxUnique.Checked then begin
    klist[ComboBoxKeys.ItemIndex].Fulltext := false;
    CheckBoxFulltext.Checked := false;
    ComboBoxKeys.ItemsEx[ComboBoxKeys.ItemIndex].ImageIndex := ICONINDEX_UNIQUEKEY;
  end
  else
  begin
    // Not unique, not fulltext
    ComboBoxKeys.ItemsEx[ComboBoxKeys.ItemIndex].ImageIndex := ICONINDEX_INDEXKEY;
  end;
  klist[ComboBoxKeys.ItemIndex].Modified := true;
end;



{***
  Make index fulltext!
}
procedure TFieldEditForm.CheckBoxFulltextClick(Sender: TObject);
begin
  klist[ComboBoxKeys.ItemIndex].Fulltext := CheckBoxFulltext.Checked;
  if CheckBoxFulltext.Checked then begin
    klist[ComboBoxKeys.ItemIndex].Unique := false;
    CheckBoxUnique.Checked := false;
    ComboBoxKeys.ItemsEx[ComboBoxKeys.ItemIndex].ImageIndex := ICONINDEX_FULLTEXTKEY;
  end
  else
  begin
    // Not unique, not fulltext
    ComboBoxKeys.ItemsEx[ComboBoxKeys.ItemIndex].ImageIndex := ICONINDEX_INDEXKEY;
  end;
  klist[ComboBoxKeys.ItemIndex].Modified := true;
end;



{***
  update keys!
  for each TempKey (see OnFormShow) check if it was changed or even deleted in klist
  @todo code cleanup, get rid of WITH statement
}
procedure TFieldEditForm.UpdateKeys(Sender: TObject);
var
  i,j,k, index : Integer;
  query : String;
  columns_sql : String;
  modifiedKeys : Array of Integer;
begin
  query := '';
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

      // Remember which keys were modified for the case of an SQL error
      // so we can switch them back to have .Modified := True
      SetLength( modifiedKeys, Length(modifiedKeys)+1 );
      modifiedKeys[Length(modifiedKeys)-1] := index;

      // Prepare columns-list
      columns_sql := '';
      for k := 0 to klist[index].Columns.Count - 1 do
      begin
        columns_sql := columns_sql + mainform.mask( klist[index].Columns[k] );
        if k < klist[index].Columns.Count-1 then
          columns_sql := columns_sql + ', ';
      end;

      // PK:
      if klist[index].Name = 'PRIMARY' then
        query := query + 'DROP PRIMARY KEY' +
          ', ADD PRIMARY KEY (' + columns_sql + '), '
      // UNIQUE:
      else if klist[index].Unique then
        query := query + 'DROP INDEX ' + mainform.mask(klist[index].Name) +
          ', ADD UNIQUE ' + mainform.mask(klist[index].Name) + ' (' + columns_sql + '), '
      // FULLTEXT:
      else if klist[index].Fulltext then
        query := query + 'DROP INDEX ' + mainform.mask(klist[index].Name) +
          ', ADD FULLTEXT ' + mainform.mask(klist[index].Name) + ' (' + columns_sql + '), '
      // INDEX:
      else
        query := query + 'DROP INDEX '+ mainform.mask(klist[index].Name) +
          ', ADD INDEX ' + mainform.mask(klist[index].Name) + ' (' + columns_sql + '), ';
      klist[index].Modified := false;
    end

    else if index = -1 then begin
      // delete existing key
      if TempKeys[i] = 'PRIMARY' then
        query := query + 'DROP PRIMARY KEY, '
      else
        query := query + 'DROP INDEX ' + mainform.mask(TempKeys[i]) + ', ';
    end;

    if index > -1 then
      klist[index].Ready := true;
  end;

  for j:=0 to length(klist)-1 do begin
    if (not klist[j].Ready) then begin
      // Add a new key

      // Prepare columns-list
      columns_sql := '';
      for k := 0 to klist[j].Columns.Count - 1 do
      begin
        columns_sql := columns_sql + mainform.mask( klist[j].Columns[k] );
        if k < klist[j].Columns.Count-1 then
          columns_sql := columns_sql + ', ';
      end;

      // PK:
      if klist[j].Name = 'PRIMARY' then
        query := query + 'ADD PRIMARY KEY (' + columns_sql + '), '
      // UNIQUE:
      else if klist[j].Unique then
        query := query + 'ADD UNIQUE ' + mainform.mask(klist[j].Name) + ' (' + columns_sql + '), '
      // UNIQUE:
      else if klist[j].Fulltext then
        query := query + 'ADD FULLTEXT ' + mainform.mask(klist[j].Name) + ' (' + columns_sql + '), '
      // INDEX:
      else
        query := query + 'ADD INDEX '+ mainform.mask(klist[j].Name) + ' (' + columns_sql + '), ';
    end;
  end;

  if query <> '' then
  begin
    // Remove trailing comma
    delete( query, Length(query)-1, 2 );
    query := 'ALTER TABLE ' + mainform.mask(Mainform.ChildWin.ActualTable) + ' ' + query;
    try
      // Send query
      Mainform.ChildWin.ExecUpdateQuery(query);
      // Refresh listColumns to display correct field-icons
      Mainform.ChildWin.ShowTableProperties(self);
      ModalResult := mrOK;
    except
      On E : Exception do
      begin
        MessageDlg( E.Message, mtError, [mbOK], 0 );
        // Ensure modified and new indexes will be processed next time the user clicks OK
        for i := 0 to Length(klist) - 1 do
          klist[i].Ready := False;
        for i := 0 to Length(modifiedKeys) - 1 do
          klist[modifiedKeys[i]].Modified := True;
      end;
    end;
  end;
end;



{***
  Add column to index
}
procedure TFieldEditForm.AddField(Sender: TObject);
var
  idx : Integer;
  item: string;
begin
  if listColumnsAvailable.ItemIndex > -1 then begin
    idx := listColumnsAvailable.ItemIndex;
    item := listColumnsAvailable.Items[idx];
    listColumnsUsed.Items.Add(item);
    klist[ComboBoxKeys.ItemIndex].Columns.Add(item);
    listColumnsAvailable.Items.Delete(idx);
    klist[ComboBoxKeys.ItemIndex].Modified := true;
    // Highlight previous item to added one.
    listColumnsAvailable.ItemIndex := Min(Max(idx - 1, 0), listColumnsAvailable.Items.Count - 1);
  end;
  togglebuttons(self);
end;



{***
  Add all columns to index
}
procedure TFieldEditForm.btnAddAllColumnsToIndexClick(Sender: TObject);
begin
  listColumnsUsed.Items.AddStrings(listColumnsAvailable.Items);
  klist[ComboBoxKeys.ItemIndex].Columns.AddStrings(listColumnsAvailable.Items);
  listColumnsAvailable.Items.Clear;
  klist[ComboBoxKeys.ItemIndex].Modified := true;
  togglebuttons(self);
end;



{***
  Delete column from index
}
procedure TFieldEditForm.RemoveField(Sender: TObject);
var
  idx : Integer;
  item: string;
begin
  if listColumnsUsed.ItemIndex > -1 then begin
    idx := listColumnsUsed.ItemIndex;
    item := listColumnsUsed.Items[idx];
    klist[ComboBoxKeys.ItemIndex].Columns.Delete(idx);
    klist[ComboBoxKeys.ItemIndex].Modified := true;
    listColumnsAvailable.Items.Add(item);
    listColumnsUsed.Items.Delete(idx);
    // Highlight previous item to removed one.
    listColumnsUsed.ItemIndex := Min(Max(idx - 1, 0), listColumnsUsed.Items.Count - 1);
  end;
  togglebuttons(self);
end;



{***
  Delete all columns from index
}
procedure TFieldEditForm.btnDeleteAllColumnsFromIndexClick(Sender: TObject);
begin
  klist[ComboBoxKeys.ItemIndex].Columns.Clear;
  klist[ComboBoxKeys.ItemIndex].Modified := true;
  listColumnsAvailable.Items.AddStrings(listColumnsUsed.Items);
  listColumnsUsed.Items.Clear;
  togglebuttons(self);
end;



{***
  Toggle enabled-status of Add- and Delete-Column-From-Index-buttons
}
procedure TFieldEditForm.togglebuttons(Sender: TObject);
begin
  btnAddColumnToIndex.Enabled := (listColumnsAvailable.ItemIndex > -1);
  btnDeleteColumnFromIndex.Enabled := (listColumnsUsed.ItemIndex > -1);
  btnAddAllColumnsToIndex.Enabled := (listColumnsAvailable.Items.Count > 0);
  btnDeleteAllColumnsFromIndex.Enabled := (listColumnsUsed.Items.Count > 0);
  ValidateControls;
end;



{***
  Call SQL help for selected datatype
}
procedure TFieldEditForm.btnDatatypeHelpClick(Sender: TObject);
begin
  Mainform.ChildWin.CallSQLHelpWithKeyword(ComboBoxType.Text);
end;



{***
  Ensure correct state of various controls
}
procedure TFieldEditForm.ValidateControls;
begin
  ButtonOK.Enabled := true;

  case FMode of

    femFieldUpdate:
    begin
      ButtonOK.Caption := 'Update Field';
    end;

    femFieldAdd:
    begin
      ButtonOK.Caption := 'Add Field';
    end;

    femIndexEditor:
    begin
      ButtonOK.Caption := 'Update Indexes';
      // Disable the button if a key was selected and no columns are listed on the left
      ButtonOK.Enabled := (ComboBoxKeys.ItemIndex = -1) or (listColumnsUsed.Items.Count > 0);
    end;

  end;
  Caption := Mainform.ChildWin.Description + ' - ' + ButtonOK.Caption;
end;

end.


