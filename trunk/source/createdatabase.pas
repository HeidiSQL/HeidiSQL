unit createdatabase;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, SynEdit, SynMemo,
  dbconnection;

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
    CollationTable: TDBQuery;
    defaultCharset : String;
    currentCollation : String;
    FConnection: TDBConnection;
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
  FConnection := MainForm.ActiveConnection;
  CollationTable := FConnection.CollationTable;
  // Detect servers default charset
  defaultCharset := FConnection.GetVar( 'SHOW VARIABLES LIKE '+esc('character_set_server'), 1 );
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
    sql_create := FConnection.GetVar('SHOW CREATE DATABASE '+FConnection.QuoteIdent(modifyDB), 1);
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
    FConnection.Query(sql);
    MainForm.RefreshTree;
    // Close form
    ModalResult := mrOK;
  except
    on E:EDatabaseError do
      ErrorDialog('Creating database "'+editDBName.Text+'" failed.', E.Message);
    // Keep form open
  end else try
    sql := 'ALTER DATABASE ' + FConnection.QuoteIdent( modifyDB );
    if comboCharset.Enabled and (comboCharset.Text <> '') then
    begin
      sql := sql + ' CHARACTER SET ' + comboCharset.Text;
      if comboCollation.Enabled and (comboCollation.Text <> '') then
        sql := sql + ' COLLATE ' + esc(comboCollation.Text);
    end;
    if modifyDB = editDBName.Text then begin
      // Alter database
      FConnection.Query(sql);
    end else begin
      // Rename database
      ObjectsInOldDb := FConnection.GetDBObjects(modifyDB, True);
      AllDatabases := FConnection.GetCol('SHOW DATABASES');
      if AllDatabases.IndexOf(editDBName.Text) > -1 then
        ObjectsInNewDb := FConnection.GetDBObjects(editDBName.Text, True)
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
        FConnection.Query(GetCreateStatement);
      end else begin
        if MessageDialog('Database "'+editDBName.Text+'" exists. But it does not contain objects with same names as in '+
          '"'+modifyDB+'", so it''s uncritical to move everything.'+CRLF+CRLF+'Move all objects to "'+editDBName.Text+'"?',
          mtConfirmation, [mbYes, mbCancel]) <> mrYes then
          Exit;
      end;
      // Move all tables, views and procedures to target db
      sql := '';
      for i:=0 to ObjectsInOldDb.Count-1 do begin
        sql := sql + FConnection.QuoteIdent(modifyDb)+'.'+FConnection.QuoteIdent(ObjectsInOldDb[i].Name)+' TO '+
          FConnection.QuoteIdent(editDBName.Text)+'.'+FConnection.QuoteIdent(ObjectsInOldDb[i].Name)+', ';
      end;
      if sql <> '' then begin
        Delete(sql, Length(sql)-1, 2);
        sql := 'RENAME TABLE '+sql;
        FConnection.Query(sql);
        FConnection.ClearDbObjects(modifyDB);
        FConnection.ClearDbObjects(editDBName.Text);
      end;
      // Last check if old db is really empty, before we drop it.
      ObjectsLeft := FConnection.GetDBObjects(modifyDB);
      if ObjectsLeft.Count = 0 then begin
        FConnection.Query('DROP DATABASE '+FConnection.QuoteIdent(modifyDB));
        MainForm.RefreshTree;
      end;
    end;
    // Close form
    ModalResult := mrOK;
  except
    on E:Exception do
      ErrorDialog('Altering database "'+editDBName.Text+'" failed.', E.Message);
    // Keep form open
  end;

  // Save new db name to registry
  AllDatabases := Explode(';', FConnection.Parameters.AllDatabasesStr);
  if AllDatabases.Count > 0 then begin
    i := AllDatabases.IndexOf(modifyDB);
    if i > -1 then
      AllDatabases[i] := editDBname.Text
    else
      AllDatabases.Add(editDBname.Text);
    AppSettings.SessionPath := FConnection.Parameters.SessionPath;
    FConnection.Parameters.AllDatabasesStr := ImplodeStr(';', AllDatabases);
    AppSettings.WriteString(asDatabases, FConnection.Parameters.AllDatabasesStr);
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
  Result := 'CREATE DATABASE ' + FConnection.QuoteIdent( editDBName.Text );
  if comboCharset.Enabled and (comboCharset.Text <> '') then
  begin
    Result := Result + ' /*!40100 CHARACTER SET ' + comboCharset.Text;
    if comboCollation.Enabled and (comboCollation.Text <> '') then
      Result := Result + ' COLLATE ' + esc(comboCollation.Text);
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
  Action := caFree;
end;


end.
