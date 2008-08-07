unit fieldeditor;


// -------------------------------------
// Field-/Index-Editor
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, ToolWin, ExtCtrls, Buttons, VirtualTrees,
  PngSpeedButton;

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
    btnAddColumnToIndex: TPngSpeedButton;
    btnDeleteColumnFromIndex: TPngSpeedButton;
    lblColumnsAvailable: TLabel;
    ButtonAddPrimary: TButton;
    ComboBoxPosition: TComboBox;
    lblPosition: TLabel;
    CheckBoxFulltext: TCheckBox;
    btnAddAllColumnsToIndex: TPngSpeedButton;
    btnDeleteAllColumnsFromIndex: TPngSpeedButton;
    btnDatatypeHelp: TButton;
    lblComment: TLabel;
    EditComment: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnDatatypeHelpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure AddUpdateField(Sender: TObject);
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
    procedure CheckBoxUnsignedClick(Sender: TObject);
    procedure CheckBoxZerofillClick(Sender: TObject);
    procedure ComboBoxTypeDrawItem(Control: TWinControl; Index: Integer; Rect:
        TRect; State: TOwnerDrawState);
    procedure ComboBoxTypeKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure ComboBoxTypeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure listClick(Sender: TObject);
  private
    { Private declarations }
    TempKeys : TStringList;
    FMode : TFieldEditorMode;
    FModeWhenCalled : TFieldEditorMode;
    FFieldName : String;
    FLastKey: Word;
    procedure ValidateControls;
    function IsCategory(index: Integer): Boolean;
    function IndexToType(index: Integer): Integer;
    function TypeToIndex(t: Integer): Integer;
  public
    { Public declarations }
  end;

  function FieldEditorWindow (AOwner : TComponent; AMode : TFieldEditorMode; AFieldName : String = '') : Boolean;

{$I const.inc}

implementation

uses
  helpers, childwin, Main, mysql_structures, db;

var
  klist : Array of TMysqlIndex;
  FieldEditForm : TFieldEditForm;

{$R *.DFM}



{***
  Create form
}
function FieldEditorWindow (AOwner : TComponent; AMode : TFieldEditorMode; AFieldName : String = '') : Boolean;
begin
  if FieldEditForm = nil then
    FieldEditForm := TFieldEditForm.Create(AOwner);
  FieldEditForm.FMode := AMode;
  // Also remember original mode for restoring when switching pagecontrol tabs
  FieldEditForm.FModeWhenCalled := AMode;
  FieldEditForm.FFieldName := AFieldName;
  // Init both editors
  FieldEditForm.InitFieldEditor (nil);
  FieldEditForm.InitIndexEditor (nil);
  Result := (FieldEditForm.ShowModal = mrOK);
end;


{**
  FormCreate
}
procedure TFieldEditForm.FormCreate(Sender: TObject);
begin
  // Assign images from main imagelist to speedbuttons
  btnAddAllColumnsToIndex.PngImage := Mainform.PngImageListMain.PngImages[78].PngImage;
  btnAddColumnToIndex.PngImage := Mainform.PngImageListMain.PngImages[76].PngImage;
  btnDeleteAllColumnsFromIndex.PngImage := Mainform.PngImageListMain.PngImages[79].PngImage;
  btnDeleteColumnFromIndex.PngImage := Mainform.PngImageListMain.PngImages[77].PngImage;
end;


{***
  Init Field-Editor
}
procedure TFieldEditForm.InitFieldEditor(Sender: TObject);
var
  strtype        : String;
  i, j           : Integer;
  ListColumns    : TVirtualStringTree;
  NodeData       : PVTreeData;
begin

  // Initiate "Position"-combobox
  ComboBoxPosition.Items.Clear;
  ComboBoxPosition.Items.Add('At End of Table');
  ComboBoxPosition.Items.Add('At Beginning of Table');

	// Reference to childwin's column-ListView
  ListColumns := Mainform.ChildWin.ListColumns;

  // get fieldlist
  // add fieldnames
  for i:=0 to High(Mainform.Childwin.VTRowDataListColumns) do
  begin
    ComboBoxPosition.Items.Add('AFTER ' + mainform.mask(Mainform.Childwin.VTRowDataListColumns[i].Captions[0]));
  end;

  // re-fill datatypes-combobox
  ComboBoxType.Items.Clear;
  for j := Low(MySqlDataTypeCategories) to High(MySqlDataTypeCategories) do begin
    ComboBoxType.Items.Add('   ' + MySqlDataTypeCategories[j].Name);
    for i := Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do begin
      if MySqlDataTypeArray[i].Category = MySqlDataTypeCategories[j].Index then begin
        ComboBoxType.Items.Add(MySqlDataTypeArray[i].Name);
      end;
    end;
  end;

  CheckBoxAutoIncrement.Enabled := true;

  // Disable Comment editing on old servers.
  EditComment.Enabled := Mainform.Childwin.mysql_version >= 40100;
  lblComment.Enabled := EditComment.Enabled;

  case FMode of
    // "Field" tab in Add-mode
    femFieldAdd, femIndexEditor:
    begin
      CheckBoxAutoIncrement.Enabled := false;
      EditFieldName.Text := 'FieldName';
      ComboBoxType.ItemIndex := 1;
      EditLength.Text := '';
      EditDefault.Text := '';
      EditComment.Text := '';
      CheckBoxUnsigned.Checked := true;

      if ListColumns.GetFirstSelected <> nil then
        ComboBoxPosition.ItemIndex := ListColumns.GetFirstSelected.Index+2
      else
        ComboBoxPosition.ItemIndex := 0;
    end;

    // "Field" tab in Update-mode
    femFieldUpdate:
    begin
      NodeData := ListColumns.GetNodeData(ListColumns.GetFirstSelected);
      EditFieldname.Text := FFieldName;
      EditLength.Text := getEnumValues( NodeData.Captions[1] );
      EditDefault.Text := NodeData.Captions[3];
      EditComment.Text := NodeData.Captions[5];

      // extract field type
      strtype := UpperCase( getFirstWord( NodeData.Captions[1] ) );

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
          ComboBoxType.ItemIndex := TypeToIndex(MySqlDataTypeArray[i].Index);
          Break;
        end;
      end;

      // set attributes:
      strtype := LowerCase( NodeData.Captions[1] );
      CheckBoxBinary.Checked := pos('binary', strtype) > 0;
      CheckBoxZerofill.Checked := pos('zerofill', strtype) > 0;
      CheckBoxUnsigned.Checked := pos('unsigned', strtype) > 0;
      CheckBoxNotNull.Checked := lowercase(NodeData.Captions[2]) <> 'yes';
      CheckBoxAutoIncrement.Checked := lowercase(NodeData.Captions[4]) = 'auto_increment';

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
  ds := cwin.GetResults( 'SHOW KEYS FROM ' + mainform.mask(cwin.SelectedTable) );
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
  ds.Close;
  FreeAndNil(ds);

  listColumnsAvailable.Items := GetVTCaptions(cwin.ListColumns);
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

function TFieldEditForm.IsCategory(index: Integer): Boolean;
begin
  Result := Copy(ComboBoxType.Items[index], 1, 2) = '  ';
end;

function TFieldEditForm.IndexToType(index: Integer): Integer;
var
  i: Integer;
begin
  for i := Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do begin
    if ComboBoxType.Items[index] = MySqlDataTypeArray[i].Name then begin
      Result := MySqlDataTypeArray[i].Index;
      Exit;
    end;
  end;
  raise Exception.Create('Internal error: invalid index ' + IntToStr(index) + '.');
end;

function TFieldEditForm.TypeToIndex(t: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to ComboBoxType.Items.Count - 1 do begin
    if ComboBoxType.Items[i] = MySqlDataTypeArray[t].Name then begin
      Result := i;
      Exit;
    end;
  end;
  raise Exception.Create('Internal error: invalid type ' + IntToStr(t) + '.');
end;

{***
  User selected a new datatype for the selected field.
  Take care of limitations of different datatypes,
  toggle clickability of certain checkboxes
}
procedure TFieldEditForm.ComboBoxTypeChange(Sender: TObject);
var
  FieldType : TMysqlDataTypeRecord;
  idx: Integer;
begin
  // Attributes

  // Skip column type categories
  if IsCategory(ComboBoxType.ItemIndex) then begin
    idx := ComboBoxType.ItemIndex;
    if FLastKey = VK_UP then idx := idx - 1
    else idx := idx + 1;
    if idx < 0 then idx := idx + 2;
    if idx >= ComboBoxType.Items.Count then idx := idx - 2;
    ComboBoxType.ItemIndex := idx;
  end;

  // Detect column-type
  FieldType := MySqlDataTypeArray[IndexToType(ComboBoxType.ItemIndex)];

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
  strNull,
  strAttributes,
  strAutoIncrement,
  strLengthSet,
  strDefault,
  strComment,
  strPosition,
  fielddef,
  sql_alterfield   : String;
  cwin : TMDIChild;
  i: Integer;
begin
  // Apply Changes to field-definition

  cwin := Mainform.ChildWin;

  // move field if position changed
  if (ComboBoxPosition.ItemIndex > -1) and (FMode in [femFieldUpdate]) and (cwin.mysql_version < 40001) then
    begin // Move field position
      if MessageDLG('You are about to move a column''s position in the table definition. While there is no handy MySQL way to do that, this will be done in 4 steps:'+CRLF+
       ' 1. Add a temporary column at the specified position'+CRLF+
       ' 2. Fill the temporary column with the same data as the source column'+CRLF+
       ' 3. Drop the source column'+CRLF+
       ' 4. Rename the temporary column to the same name the source column had.'+CRLF+CRLF+
       'Be aware that this method can mess up existing indexes in your table or even result in losing data! If you are not sure you should not use this function, especially on indexed columns.'+CRLF+CRLF+
       'Continue?',
       mtConfirmation,
       [mbYes, mbCancel],
       0
       ) <> mrYes then
      Exit;
    end;

  Screen.Cursor := crSQLWait;
  strAttributes := ''; // none of the 3 attributes binary, unsigned, zerofill
  strNull := '';
  strDefault := '';
  strAutoIncrement := '';

  if CheckBoxBinary.Checked = true then
    strAttributes := strAttributes + ' BINARY';

  if CheckBoxUnsigned.Checked = true then
    strAttributes := strAttributes + ' UNSIGNED';

  if CheckBoxZerofill.Checked = true then
    strAttributes := strAttributes + ' ZEROFILL';

  if (length(EditDefault.Text) > 0) and EditDefault.Enabled then
  begin
    strDefault := ' DEFAULT ';
    strDefault := strDefault + EditDefault.Text
  end;

  if CheckBoxNotNull.Enabled then
  begin
    if CheckBoxNotNull.Checked then
      strNull := ' NOT';
    strNull := strNull + ' NULL';
  end;

  if CheckBoxAutoIncrement.Checked = True then
    strAutoIncrement := ' AUTO_INCREMENT';

  if (EditLength.text <> '') and EditLength.Enabled then
    strLengthSet := '(' + EditLength.text + ') '
  else
    strLengthSet := '';

  if EditComment.Enabled and (length(EditComment.Text) > 0) then
    strComment := ' COMMENT ' + esc(EditComment.Text);

  strPosition := '';
  case ComboBoxPosition.ItemIndex of
    0 : for i:=High(Mainform.Childwin.VTRowDataListColumns) downto 0 do begin
          if Mainform.Childwin.VTRowDataListColumns[i].Captions[0] <> FFieldName then begin
            strPosition := ' AFTER ' + Mainform.Mask(Mainform.Childwin.VTRowDataListColumns[i].Captions[0]);
            break;
          end;
        end;
    1 : strPosition := ' FIRST';
  else
    strPosition := ' ' + ComboBoxPosition.Text;
  end;

  fielddef :=
    ComboBoxType.Text +     // Type
    strLengthSet +          // Length/Set
    strAttributes +         // Attribute
    strDefault +            // Default
    strNull +               // [NOT] Null
    strAutoIncrement +      // Auto_increment
    strComment;             // Comment

  try
    if (FMode = femFieldAdd) then begin
      cwin.ExecUpdateQuery(
        'ALTER TABLE ' + mainform.mask(cwin.SelectedTable) + ' ' +  // table
        'ADD ' + mainform.mask(EditFieldname.Text) + ' ' +        // new name
        fielddef +
        strPosition                                               // Position
      );
    end else if (FMode = femFieldUpdate) then begin

      sql_alterfield := 'ALTER TABLE ' + mainform.mask(cwin.SelectedTable) + ' ' +
          'CHANGE ' + mainform.mask(FFieldName) + ' ' +
          mainform.mask(EditFieldName.Text) + ' ' +
          fielddef;

      if cwin.mysql_version >= 40001 then
      begin
        // MySQL 4.0.1+ allows column moving in a ALTER TABLE statement.
        // @see http://dev.mysql.com/doc/refman/4.1/en/alter-table.html
        cwin.ExecUpdateQuery( sql_alterfield + strPosition );
      end
      else begin
        // Use manual mechanism on older servers
        cwin.ExecUpdateQuery( sql_alterfield );

        //ShowMessageFmt ('ComboBox position: %d',[ComboBoxPosition.ItemIndex]);

        if ComboBoxPosition.ItemIndex > -1 then begin
          // Move field position
          cwin.ExecUpdateQuery(
            'ALTER TABLE ' + mainform.mask(cwin.SelectedTable) + ' ' +  // table
            'ADD ' + mainform.mask(TEMPFIELDNAME) + ' ' +             // new name
            fielddef +
            strPosition                                               // Position
          );
          cwin.ExecUpdateQuery('UPDATE ' + mainform.mask(cwin.SelectedTable) + ' SET '+mainform.mask(TEMPFIELDNAME)+'='+mainform.mask(EditFieldName.Text));
          cwin.ExecUpdateQuery('ALTER TABLE ' + mainform.mask(cwin.SelectedTable) + ' DROP '+mainform.mask(EditFieldName.Text));

          cwin.ExecUpdateQuery(
            'ALTER TABLE ' + mainform.mask(cwin.SelectedTable) + ' ' +
            'CHANGE ' + mainform.mask(TEMPFIELDNAME) + ' ' +
            mainform.mask(EditFieldName.Text) + ' ' +
            fielddef
          );
        end;
      end;
    end;
    cwin.ShowTableProperties(cwin.SelectedTable);
  except
    on E: THandledSQLError do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      ModalResult := mrNone;
    end;
  end;
  Screen.Cursor := crDefault;
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
    AddUpdateField(self)
  else if fMode in [femIndexEditor] then
    UpdateKeys(self);
end;



{***
  User selected another index in the combobox
  @todo code cleanup, get rid of WITH statement
}
procedure TFieldEditForm.ComboBoxKeysChange(Sender: TObject);
var i, j : Integer;
begin
  if ComboBoxKeys.ItemIndex > -1 then begin
    listColumnsAvailable.Items := GetVTCaptions(Mainform.ChildWin.ListColumns);
    for i:=0 to klist[ComboBoxKeys.ItemIndex].columns.Count-1 do
    begin
      j := listColumnsAvailable.Items.IndexOf(klist[ComboBoxKeys.ItemIndex].columns[i]);
      if j > -1 then
        listColumnsAvailable.Items.Delete( j );
    end;
    with klist[ComboBoxKeys.ItemIndex] do begin
      listColumnsUsed.Items := Columns;
      CheckBoxUnique.OnClick := nil;
      CheckBoxUnique.Checked := Unique;
      CheckBoxUnique.OnClick := CheckBoxUniqueClick;
      CheckBoxFulltext.OnClick := nil;
      CheckBoxFulltext.Checked := Fulltext;
      CheckBoxFulltext.OnClick := CheckBoxFulltextClick;
    end;
  end;
  ValidateControls;
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
var
  idx : Integer;
begin
  idx := ComboBoxKeys.ItemIndex;
  if idx = -1 then
    Exit;
  klist[idx].Unique := CheckBoxUnique.Checked;
  if CheckBoxUnique.Checked then begin
    klist[idx].Fulltext := false;
    CheckBoxFulltext.Checked := false;
    ComboBoxKeys.ItemsEx[idx].ImageIndex := ICONINDEX_UNIQUEKEY;
  end else begin
    // Not unique, not fulltext
    ComboBoxKeys.ItemsEx[idx].ImageIndex := ICONINDEX_INDEXKEY;
  end;
  klist[idx].Modified := true;
end;



{***
  Make index fulltext!
}
procedure TFieldEditForm.CheckBoxFulltextClick(Sender: TObject);
var
  idx : Integer;
begin
  idx := ComboBoxKeys.ItemIndex;
  if idx = -1 then
    Exit;
  klist[idx].Fulltext := CheckBoxFulltext.Checked;
  if CheckBoxFulltext.Checked then begin
    klist[idx].Unique := false;
    CheckBoxUnique.Checked := false;
    ComboBoxKeys.ItemsEx[idx].ImageIndex := ICONINDEX_FULLTEXTKEY;
  end else begin
    // Not unique, not fulltext
    ComboBoxKeys.ItemsEx[idx].ImageIndex := ICONINDEX_INDEXKEY;
  end;
  klist[idx].Modified := true;
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
    query := 'ALTER TABLE ' + mainform.mask(Mainform.ChildWin.SelectedTable) + ' ' + query;
    try
      // Send query
      Mainform.ChildWin.ExecUpdateQuery(query);
      // Refresh listColumns to display correct field-icons
      Mainform.ChildWin.ShowTableProperties(Mainform.ChildWin.SelectedTable);
    except
      On E : Exception do
      begin
        MessageDlg( E.Message, mtError, [mbOK], 0 );
        // Ensure modified and new indexes will be processed next time the user clicks OK
        for i := 0 to Length(klist) - 1 do
          klist[i].Ready := False;
        for i := 0 to Length(modifiedKeys) - 1 do
          klist[modifiedKeys[i]].Modified := True;
        // Keep form open
        ModalResult := mrNone;
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
  ValidateControls;
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
  ValidateControls;
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
  ValidateControls;
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
  ValidateControls;
end;



{***
  Call SQL help for selected datatype
}
procedure TFieldEditForm.btnDatatypeHelpClick(Sender: TObject);
begin
  Mainform.CallSQLHelpWithKeyword(ComboBoxType.Text);
end;



{***
  Ensure correct state of various controls
}
procedure TFieldEditForm.ValidateControls;
var
  KeySelected : Boolean;
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
      KeySelected := ComboBoxKeys.ItemIndex > -1;
      // Disable the button if
      //  1) a key was selected and no columns are listed on the left
      //     or
      //  2) all existing keys were deleted
      ButtonOK.Enabled := (KeySelected and (listColumnsUsed.Items.Count > 0))
        or (Length(klist) < TempKeys.Count);
      // Buttons to add or remove columns, only enabled if key was selected and the
      // relevant list has items left
      btnAddColumnToIndex.Enabled := KeySelected and (listColumnsAvailable.ItemIndex > -1);
      btnAddAllColumnsToIndex.Enabled := KeySelected and (listColumnsAvailable.Items.Count > 0);
      btnDeleteColumnFromIndex.Enabled := KeySelected and (listColumnsUsed.ItemIndex > -1);
      btnDeleteAllColumnsFromIndex.Enabled := KeySelected and (listColumnsUsed.Items.Count > 0);
      CheckBoxUnique.Enabled := KeySelected and not (ComboBoxKeys.ItemsEx[ComboBoxKeys.ItemIndex].Caption = 'PRIMARY');
      CheckBoxFulltext.Enabled := KeySelected and not (ComboBoxKeys.ItemsEx[ComboBoxKeys.ItemIndex].Caption = 'PRIMARY');
      ButtonDelete.Enabled := KeySelected;
      listColumnsUsed.Enabled := KeySelected;
      listColumnsAvailable.Enabled := KeySelected;
    end;

  end;
  Caption := Mainform.ChildWin.SessionName + ' - ' + ButtonOK.Caption;
end;


{**
  Call ValidateControls if user selects an item in one of the lists
}
procedure TFieldEditForm.listClick(Sender: TObject);
begin
  ValidateControls;
end;


{**
  "Unsigned" option was (un)checked
  Ensure "Zerofill" gets unchecked too
}
procedure TFieldEditForm.CheckBoxUnsignedClick(Sender: TObject);
begin
  if not CheckboxUnsigned.Checked then
    CheckboxZerofill.Checked := False;
end;


{**
  "Zerofill" option was (un)checked.
  Ensure "Unsigned" is checked too
}
procedure TFieldEditForm.CheckBoxZerofillClick(Sender: TObject);
begin
  if CheckBoxZerofill.Checked then
    CheckboxUnsigned.Checked := True;
end;


procedure TFieldEditForm.ComboBoxTypeDrawItem(Control: TWinControl; Index:
    Integer; Rect: TRect; State: TOwnerDrawState);
var
  s: string;
{*dc: HDC;
  dpi: Integer;*}
begin
  with Control as TComboBox,Canvas do begin
    // custom DPI support
    {*
    // not useful unless SetDPIAware() is issued.
    dc := GetDC(MainForm.Handle);
    dpi := GetDeviceCaps(dc, LOGPIXELSX);
    ReleaseDC(MainForm.Handle, dc);
    Font.PixelsPerInch := dpi;
    *}
    Font.PixelsPerInch := 96;
    // decide colors
    Brush.Color := clWindow;
    Font.Color := clWindowText;
    if IsCategory(Index) then begin
      Font.Color := clWindow;
      Brush.Color := clWindowText
    end
    else if odSelected in State then begin
      Font.Color := clHighlightText;
      Brush.Color := clHighlight;
    end;
    // fill the rectangle first
    FillRect(Rect);
    // categories start with spaces to avoid hotkeys hitting them, reverse that
    s := Items[Index];
    if IsCategory(Index) then s := Copy(s, 4)
    else s := '   ' + s;
    // then print text
    TextOut(Rect.Left, Rect.Top, s);
  end;
end;

procedure TFieldEditForm.ComboBoxTypeKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  FLastKey := Key;
end;

procedure TFieldEditForm.ComboBoxTypeKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  FLastKey := 0;
end;


end.


