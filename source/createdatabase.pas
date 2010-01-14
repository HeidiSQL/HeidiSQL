unit createdatabase;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, SynEdit, SynMemo,
  WideStrings,
  mysql_connection;

type
  TCreateDatabaseForm = class(TForm)
    editDBName: TEdit;
    lblDBName: TLabel;
    comboCharset: TComboBox;
    lblCharset: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblCollation: TLabel;
    comboCollation: TComboBox;
    lblPreview: TLabel;
    SynMemoPreview: TSynMemo;
    procedure btnOKClick(Sender: TObject);
    procedure comboCharsetChange(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure editDBNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetCreateStatement: String;
  private
    { Private declarations }
    CollationTable: TMySQLQuery;
    defaultCharset : String;
    currentCollation : String;
  public
    { Public declarations }
    modifyDB : String;
  end;


implementation

uses main, helpers;

{$R *.dfm}


{**
  Fetch list with character sets and collations from the server
}
procedure TCreateDatabaseForm.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
  // Setup SynMemoPreview
  SynMemoPreview.Highlighter := Mainform.SynSQLSyn1;
end;


{**
  Form gets displayed: Set default values.
}
procedure TCreateDatabaseForm.FormShow(Sender: TObject);
var
  selectCharset,
  currentCharset,
  sql_create : String;
  Charset: String;
  colpos: Integer;
begin
  CollationTable := Mainform.Connection.CollationTable;
  // Detect servers default charset
  defaultCharset := Mainform.Connection.GetVar( 'SHOW VARIABLES LIKE '+esc('character_set_server'), 1 );
  comboCharset.Enabled := Assigned(CollationTable);
  lblCharset.Enabled := comboCharset.Enabled;
  comboCollation.Enabled := comboCharset.Enabled;
  lblCollation.Enabled := comboCharset.Enabled;
  if comboCharset.Enabled then begin
    // Create a list with charsets from collations dataset
    comboCharset.Items.BeginUpdate;
    while not CollationTable.Eof do begin
      Charset := CollationTable.Col('Charset');
      if comboCharset.Items.IndexOf(Charset) = -1 then
        comboCharset.Items.Add(Charset);
      CollationTable.Next;
    end;
    comboCharset.Items.EndUpdate;
  end;

  if modifyDB = '' then
  begin
    Caption := 'Create database ...';
    editDBName.Text := '';
    editDBName.SetFocus;
    selectCharset := defaultCharset;
  end
  else begin
    Caption := 'Alter database ...';
    editDBName.Text := modifyDB;
    editDBName.SetFocus;
    editDBName.SelectAll;
    
    // Detect current charset and collation to be able to preselect them in the pulldowns
    sql_create := Mainform.Connection.GetVar('SHOW CREATE DATABASE '+Mainform.mask(modifyDB), 1);
    currentCharset := Copy( sql_create, pos('CHARACTER SET', sql_create)+14, Length(sql_create));
    currentCharset := GetFirstWord( currentCharset );
    if currentCharset <> '' then
      selectCharset := currentCharset
    else
      selectCharset := defaultCharset;
    currentCollation := '';
    colpos := pos('COLLATE', sql_create);
    if colpos > 0 then begin
      currentCollation := Copy( sql_create, colpos+8, Length(sql_create));
      currentCollation := GetFirstWord( currentCollation );
    end;
  end;

  // Preselect charset item in pulldown
  if comboCharset.Items.Count > 0 then
  begin
    if comboCharset.Items.IndexOf(selectCharset) > -1 then
      comboCharset.ItemIndex := comboCharset.Items.IndexOf(selectCharset)
    else
      comboCharset.ItemIndex := 0;
    // Invoke selecting default collation
    comboCharsetChange( Sender );
  end;

  // Invoke SQL preview
  Modified(Sender);
  MainForm.SetupSynEditors;
end;


{**
  Charset has been selected: Display fitting collations
  and select default one.
}
procedure TCreateDatabaseForm.comboCharsetChange(Sender: TObject);
var
  defaultCollation : String;
begin
  // Abort if collations were not fetched successfully
  if not Assigned(CollationTable) then
    Exit;

  // Fill pulldown with fitting collations
  comboCollation.Items.BeginUpdate;
  comboCollation.Items.Clear;
  CollationTable.First;
  while not CollationTable.Eof do begin
    if CollationTable.Col('Charset') = comboCharset.Text then
    begin
      comboCollation.Items.Add(CollationTable.Col('Collation'));
      if CollationTable.Col('Default') = 'Yes' then
        defaultCollation := CollationTable.Col('Collation');
    end;
    CollationTable.Next;
  end;

  // Preselect default or current collation
  if currentCollation <> '' then
    defaultCollation := currentCollation;
  if comboCollation.Items.IndexOf(defaultCollation) > -1 then
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(defaultCollation)
  else
    comboCollation.ItemIndex := 0;

  comboCollation.Items.EndUpdate;

  // Invoke SQL preview
  Modified(Sender);
end;


{**
  User writes something into editDBName
}
procedure TCreateDatabaseForm.editDBNameChange(Sender: TObject);
begin
  editDBName.Font.Color := clWindowText;
  editDBName.Color := clWindow;
  // Enable "OK"-Button by default
  btnOK.Enabled := True;
  try
    ensureValidIdentifier( editDBName.Text );
  except
    // Invalid database name
    if editDBName.Text <> '' then begin
      editDBName.Font.Color := clRed;
      editDBName.Color := clYellow;
    end;
    btnOK.Enabled := False;
  end;

  // Invoke SQL preview
  Modified(Sender);
end;


{**
  Create the database
}
procedure TCreateDatabaseForm.btnOKClick(Sender: TObject);
var
  sql : String;
  AllDatabases, Unions, ObjectsLeft: TStringList;
  ObjectsInNewDb, ObjectsInOldDb: TDBObjectList;
  i, j: Integer;
begin
  if modifyDB = '' then try
    sql := GetCreateStatement;
    Mainform.Connection.Query(sql);
    // Close form
    ModalResult := mrOK;
  except
    On E:Exception do
      MessageDlg( 'Creating database "'+editDBName.Text+'" failed:'+CRLF+CRLF+E.Message, mtError, [mbOK], 0 );
    // Keep form open
  end else try
    sql := 'ALTER DATABASE ' + Mainform.mask( modifyDB );
    if comboCharset.Enabled and (comboCharset.Text <> '') then
    begin
      sql := sql + ' CHARACTER SET ' + comboCharset.Text;
      if comboCollation.Enabled and (comboCollation.Text <> '') then
        sql := sql + ' COLLATE ' + comboCollation.Text;
    end;
    if modifyDB = editDBName.Text then begin
      // Alter database
      Mainform.Connection.Query(sql);
    end else begin
      // Rename database
      ObjectsInOldDb := MainForm.Connection.GetDBObjects(modifyDB, True);
      AllDatabases := Mainform.Connection.GetCol('SHOW DATABASES');
      if AllDatabases.IndexOf(editDBName.Text) > -1 then
        ObjectsInNewDb := MainForm.Connection.GetDBObjects(editDBName.Text, True)
      else
        ObjectsInNewDb := nil; // Silence compiler warning
      // Warn if there are tables with same names in new db
      for i:=0 to ObjectsInOldDb.Count-1 do begin
        if not (ObjectsInOldDb[i].NodeType in [lntTable, lntView]) then
          Raise Exception.Create('Database "'+modifyDB+'" contains stored routine(s), which cannot be moved.');
        if Assigned(ObjectsInNewDb) then begin
          for j:=0 to ObjectsInNewDb.Count-1 do begin
            if (ObjectsInOldDb[i].Name = ObjectsInNewDb[j].Name)
              and (ObjectsInOldDb[i].NodeType = ObjectsInNewDb[j].NodeType) then begin
              // One or more objects have a naming conflict
              Raise Exception.Create('Database "'+editDBName.Text+'" exists and has objects with same names as in "'+modifyDB+'"');
            end;
          end;
        end;
      end;

      if AllDatabases.IndexOf(editDBName.Text) = -1 then begin
        // Target db does not exist - create it
        Mainform.Connection.Query(GetCreateStatement);
      end else begin
        if MessageDlg('Database "'+editDBName.Text+'" exists. But it does not contain objects with same names as in '+
          '"'+modifyDB+'", so it''s uncritical to move everything.'+CRLF+CRLF+'Move all objects to "'+editDBName.Text+'"?',
          mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
          Exit;
      end;
      // Move all tables, views and procedures to target db
      sql := '';
      for i:=0 to ObjectsInOldDb.Count-1 do begin
        sql := sql + Mainform.mask(modifyDb)+'.'+Mainform.mask(ObjectsInOldDb[i].Name)+' TO '+
          Mainform.mask(editDBName.Text)+'.'+Mainform.mask(ObjectsInOldDb[i].Name)+', ';
      end;
      if sql <> '' then begin
        Delete(sql, Length(sql)-1, 2);
        sql := 'RENAME TABLE '+sql;
        Mainform.Connection.Query(sql);
        Mainform.Connection.ClearDbObjects(modifyDB);
        Mainform.Connection.ClearDbObjects(editDBName.Text);
      end;
      // Last step for renaming: drop source database
      ObjectsLeft := TStringList.Create;
      // Last check if old db is really empty, before we drop it. Especially triggers need to be checked.
      Unions := TStringList.Create;
      if Mainform.Connection.InformationSchemaObjects.IndexOf('TABLES') > -1 then
        Unions.Add('SELECT 1 FROM '+Mainform.mask(DBNAME_INFORMATION_SCHEMA)+'.TABLES WHERE TABLE_SCHEMA='+esc(modifyDB));
      if Mainform.Connection.InformationSchemaObjects.IndexOf('ROUTINES') > -1 then
        Unions.Add('SELECT 1 FROM '+Mainform.mask(DBNAME_INFORMATION_SCHEMA)+'.ROUTINES WHERE ROUTINE_SCHEMA='+esc(modifyDB));
      if Mainform.Connection.InformationSchemaObjects.IndexOf('TRIGGERS') > -1 then
        Unions.Add('SELECT 1 FROM '+Mainform.mask(DBNAME_INFORMATION_SCHEMA)+'.TRIGGERS WHERE TRIGGER_SCHEMA='+esc(modifyDB));
      if Unions.Count = 1 then
        ObjectsLeft := Mainform.Connection.GetCol(Unions[0])
      else if Unions.Count > 1 then
        ObjectsLeft := Mainform.Connection.GetCol('(' + implodestr(') UNION (', Unions) + ')');
      if ObjectsLeft.Count = 0 then begin
        Mainform.Connection.Query('DROP DATABASE '+Mainform.mask(modifyDB));
      end;
      FreeAndNil(ObjectsLeft);
    end;
    // Close form
    ModalResult := mrOK;
  except
    On E:Exception do
      MessageDlg( 'Altering database "'+editDBName.Text+'" failed:'+CRLF+CRLF+E.Message, mtError, [mbOK], 0 );
    // Keep form open
  end;
end;


{**
  Called on each change
}
procedure TCreateDatabaseForm.Modified(Sender: TObject);
begin
  SynMemoPreview.Clear;
  SynMemoPreview.Text := GetCreateStatement;
end;


{**
  Generate CREATE DATABASE statement, used for preview and execution
}
function TCreateDatabaseForm.GetCreateStatement: String;
begin
  Result := 'CREATE DATABASE ' + Mainform.mask( editDBName.Text );
  if comboCharset.Enabled and (comboCharset.Text <> '') then
  begin
    Result := Result + ' /*!40100 CHARACTER SET ' + comboCharset.Text;
    if comboCollation.Enabled and (comboCollation.Text <> '') then
      Result := Result + ' COLLATE ' + comboCollation.Text;
    Result := Result + ' */';
  end;
end;


{**
  Form gets closed: Reset potential modifyDB-value.
}
procedure TCreateDatabaseForm.FormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  modifyDB := '';
end;


end.
