unit createdatabase;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, SynEdit, SynMemo,
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
  CollationTable := MainForm.ActiveConnection.CollationTable;
  // Detect servers default charset
  defaultCharset := MainForm.ActiveConnection.GetVar( 'SHOW VARIABLES LIKE '+esc('character_set_server'), 1 );
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
    sql_create := MainForm.ActiveConnection.GetVar('SHOW CREATE DATABASE '+Mainform.mask(modifyDB), 1);
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
  Create the database
}
procedure TCreateDatabaseForm.btnOKClick(Sender: TObject);
var
  sql : String;
  AllDatabases: TStringList;
  ObjectsLeft: TDBObjectList;
  ObjectsInNewDb, ObjectsInOldDb: TDBObjectList;
  i, j: Integer;
begin
  if modifyDB = '' then try
    sql := GetCreateStatement;
    MainForm.ActiveConnection.Query(sql);
    // Close form
    ModalResult := mrOK;
  except
    on E:EDatabaseError do
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
      MainForm.ActiveConnection.Query(sql);
    end else begin
      // Rename database
      ObjectsInOldDb := MainForm.ActiveConnection.GetDBObjects(modifyDB, True);
      AllDatabases := MainForm.ActiveConnection.GetCol('SHOW DATABASES');
      if AllDatabases.IndexOf(editDBName.Text) > -1 then
        ObjectsInNewDb := MainForm.ActiveConnection.GetDBObjects(editDBName.Text, True)
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
        MainForm.ActiveConnection.Query(GetCreateStatement);
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
        MainForm.ActiveConnection.Query(sql);
        MainForm.ActiveConnection.ClearDbObjects(modifyDB);
        MainForm.ActiveConnection.ClearDbObjects(editDBName.Text);
      end;
      // Last check if old db is really empty, before we drop it.
      ObjectsLeft := MainForm.ActiveConnection.GetDBObjects(modifyDB);
      if ObjectsLeft.Count = 0 then begin
        MainForm.ActiveConnection.Query('DROP DATABASE '+Mainform.mask(modifyDB));
        MainForm.RefreshTree;
      end;
    end;
    // Close form
    ModalResult := mrOK;
  except
    on E:Exception do
      MessageDlg( 'Altering database "'+editDBName.Text+'" failed:'+CRLF+CRLF+E.Message, mtError, [mbOK], 0 );
    // Keep form open
  end;

  // Save new db name to registry
  AllDatabases := Explode(';', MainForm.ActiveConnection.Parameters.AllDatabases);
  if AllDatabases.Count > 0 then begin
    i := AllDatabases.IndexOf(modifyDB);
    if i > -1 then
      AllDatabases[i] := editDBname.Text
    else
      AllDatabases.Add(editDBname.Text);
    OpenRegistry(Mainform.ActiveConnection.SessionName);
    MainForm.ActiveConnection.Parameters.AllDatabases := ImplodeStr(';', AllDatabases);
    MainReg.WriteString(REGNAME_DATABASES, MainForm.ActiveConnection.Parameters.AllDatabases);
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