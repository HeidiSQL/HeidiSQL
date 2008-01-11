unit tbl_properties;


// -------------------------------------
// Advanced table-properties
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  Dialogs, StdCtrls, Db, Menus, SynMemo, SynEdit;

type
  Ttbl_properties_form = class(TForm)
    lblName: TLabel;
    editName: TEdit;
    lblComment: TLabel;
    editComment: TEdit;
    lblEngine: TLabel;
    comboEngine: TComboBox;
    lblCharset: TLabel;
    comboCharset: TComboBox;
    lblCollation: TLabel;
    comboCollation: TComboBox;
    lblAutoincrement: TLabel;
    editAutoincrement: TEdit;

    btnOK: TButton;
    btnCancel: TButton;

    lblCreate: TLabel;
    SynMemoCreate: TSynMemo;
    popupSynMemo: TPopupMenu;
    menuCopy: TMenuItem;
    menuSelectAll: TMenuItem;
    procedure FormDestroy(Sender: TObject);

    procedure comboCharsetChange(Sender: TObject);
    procedure editNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure menuSelectAllClick(Sender: TObject);
  private
    dsCollations: TDataSet;
    currentName,
    currentComment,
    currentEngine,
    currentCharset,
    currentCollation,
    currentAutoincrement : String;
  public
    DatabaseName,
    TableName : String;
  end;

implementation

uses
  Childwin, Main, helpers;

{$R *.DFM}


{**
  Form gets created. Fetch list with charsets, collations and engines once.
}
procedure Ttbl_properties_form.FormCreate(Sender: TObject);
var
  charset : String;
begin
  try
    dsCollations := Mainform.Childwin.GetResults('SHOW COLLATION');
  except
    // Ignore it when the above statement doesn't work on pre 4.1 servers.
    // If the list is nil, disable the combobox, so we create the db without charset.
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
    comboCharset.Sorted := True;
    comboCharset.Items.EndUpdate;
  end;

  // Only enable collation selection if list was fetched successfully
  comboCollation.Enabled := dsCollations <> nil;
  lblCollation.Enabled := comboCollation.Enabled;

  // Display supported engines in pulldown
  Mainform.Childwin.TableEnginesCombo( comboEngine );

  // Setup SynMemo
  SynMemoCreate.Highlighter := Mainform.Childwin.SynSQLSyn1;
  SynMemoCreate.Font.Name := Mainform.Childwin.SynMemoQuery.Font.Name;
  SynMemoCreate.Font.Size := Mainform.Childwin.SynMemoQuery.Font.Size;
end;


procedure Ttbl_properties_form.FormDestroy(Sender: TObject);
begin
  if dsCollations <> nil then dsCollations.Close;
  FreeAndNil(dsCollations);
end;

{**
  Form gets displayed.
}
procedure Ttbl_properties_form.FormShow(Sender: TObject);
var
  ds: TDataSet;
  sql : String;
begin
  // Fetch table properties
  sql := 'SHOW TABLE STATUS ';
  if DatabaseName <> '' then
    sql := sql + ' FROM ' + Mainform.mask(DatabaseName)+' ';
  sql := sql + 'LIKE '+esc(TableName);
  ds := Mainform.Childwin.GetResults(sql);

  // Table name
  currentName := ds.FieldByName('Name').AsString;
  editName.Text := currentName;

  // Comment
  currentComment := ds.FieldByName('Comment').AsString;
  editComment.Text := currentComment;

  // Engine
  currentEngine := ds.Fields[1].AsString;
  comboEngine.ItemIndex := comboEngine.Items.IndexOf(currentEngine);

  // Collation
  if ds.FindField('Collation') <> nil then
    currentCollation := ds.FieldByName('Collation').AsString;

  // Character set
  currentCharset := '';
  if dsCollations <> nil then
  begin
    dsCollations.First;
    while not dsCollations.Eof do
    begin
      if dsCollations.FieldByName('Collation').AsString = currentCollation then
      begin
        currentCharset := dsCollations.FieldByName('Charset').AsString;
        comboCharset.ItemIndex := comboCharset.Items.IndexOf(currentCharset);
        // Invoke selecting collation
        comboCharsetChange( Sender );
        break;
      end;
      dsCollations.Next;
    end;
  end;

  // Auto increment. Disabled if empty string.
  editAutoincrement.Text := '';
  editAutoincrement.Enabled := False;
  if ds.FieldByName('Auto_increment').AsString <> '' then
  begin
    currentAutoincrement := IntToStr(MakeInt(ds.FieldByName('Auto_increment').AsString));
    editAutoincrement.Text := currentAutoincrement;
    editAutoincrement.Enabled := True;
  end;
  lblAutoincrement.Enabled := editAutoincrement.Enabled;
  ds.Close;
  FreeAndNil(ds);

  // SQL preview
  sql := 'SHOW CREATE TABLE ';
  if DatabaseName <> '' then
    sql := sql + Mainform.mask(DatabaseName)+'.';
  sql := sql + Mainform.Childwin.mask(currentName);
  SynMemoCreate.Lines.Text := Mainform.Childwin.GetVar( sql, 1 );

  editName.SetFocus;
  editName.SelectAll;
end;


{**
  Charset has been selected: Display fitting collations
  and select default one.
}
procedure Ttbl_properties_form.comboCharsetChange(Sender: TObject);
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
  comboCollation.Sorted := True;

  // Preselect default or current collation
  if currentCollation <> '' then
    defaultCollation := currentCollation;
  if comboCollation.Items.IndexOf(defaultCollation) > -1 then
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(defaultCollation)
  else
    comboCollation.ItemIndex := 0;

  comboCollation.Items.EndUpdate;
end;


{**
  User writes something into editName
}
procedure Ttbl_properties_form.editNameChange(Sender: TObject);
begin
  try
    ensureValidIdentifier( editName.Text );
    editName.Font.Color := clWindowText;
    editName.Color := clWindow;
    // Enable "OK"-Button if we have a valid name
    btnOK.Enabled := True;
  except
    editName.Font.Color := clRed;
    editName.Color := clYellow;
    btnOK.Enabled := False;
  end;
end;


{**
  Select all text in a synmemo
}
procedure Ttbl_properties_form.menuSelectAllClick(Sender: TObject);
begin
  SynMemoCreate.SelectAll;
end;


{**
  Form is closing. Check ModalResult for which button was clicked.
}
procedure Ttbl_properties_form.FormClose(Sender: TObject; var Action:
    TCloseAction);
var
  sql, tmp : String;
  AlterSpecs : TStringList;
begin
  if ModalResult = mrOK then
  begin
    AlterSpecs := TStringList.Create;

    // Rename table
    if currentName <> editName.Text then
    begin
      tmp := 'RENAME ';
      if DatabaseName <> '' then
        tmp := tmp + Mainform.mask(DatabaseName)+'.';
      tmp := tmp + Mainform.mask(editName.Text);
      AlterSpecs.Add( tmp );
    end;

    // Comment
    if currentComment <> editComment.Text then
      AlterSpecs.Add( 'COMMENT = ' + esc(editComment.Text) );

    // Engine
    if comboEngine.Enabled and (currentEngine <> comboEngine.Text) then
      AlterSpecs.Add( 'ENGINE = ' + comboEngine.Text );

    // Charset
    if comboCharset.Enabled and (currentCharset <> comboCharset.Text) then
      AlterSpecs.Add( 'CHARSET ' + comboCharset.Text );

    // Collation
    if comboCollation.Enabled and (currentCollation <> comboCollation.Text) then
      AlterSpecs.Add( 'COLLATE ' + comboCollation.Text );

    // Auto_increment
    if editAutoincrement.Enabled and (currentAutoincrement <> editAutoincrement.Text) then
      AlterSpecs.Add( 'AUTO_INCREMENT = ' + IntToStr( MakeInt(editAutoincrement.Text) ) );

    if AlterSpecs.Count > 0 then
    begin
      sql := 'ALTER TABLE ';
      if DatabaseName <> '' then
        sql := sql + Mainform.mask(DatabaseName)+'.';
      sql := sql + Mainform.Childwin.mask(currentName) + ' ' + ImplodeStr(', ', AlterSpecs);
      try
        Mainform.ChildWin.ExecUpdateQuery( sql );
        if DatabaseName = '' then
          Mainform.ChildWin.MenuRefreshClick( Sender )
        else
          Mainform.ChildWin.PopulateTreeTableList( Mainform.ChildWin.DBRightClickSelectedItem.Parent, '', True );
      except
        On E:Exception do
        begin
          MessageDlg('Altering table was not successful: '+CRLF+CRLF+E.Message, mtError, [mbOK], 0);
          // Keep form open
          ModalResult := mrNone;
        end;
      end;
    end;

  end;
end;


end.
