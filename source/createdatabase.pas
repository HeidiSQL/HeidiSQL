unit createdatabase;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SynEdit, SynMemo,
  dbconnection, dbstructures, gnugettext, SynRegExpr, extra_controls;

type
  TCreateDatabaseForm = class(TExtForm)
    editDBName: TEdit;
    lblDBName: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblCollation: TLabel;
    comboCollation: TComboBox;
    lblCreateCode: TLabel;
    SynMemoCreateCode: TSynMemo;
    lblServerDefaultCollation: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetCreateStatement: String;
  private
    { Private declarations }
    FConnection: TDBConnection;
  public
    { Public declarations }
    modifyDB : String;
  end;


implementation

uses main, apphelpers;

{$R *.dfm}


procedure TCreateDatabaseForm.FormCreate(Sender: TObject);
begin
  lblCreateCode.Caption := lblCreateCode.Caption + ':';
  // Setup SynMemoPreview
  SynMemoCreateCode.Highlighter := Mainform.SynSQLSynUsed;
end;


{**
  Form gets displayed: Set default values.
}
procedure TCreateDatabaseForm.FormShow(Sender: TObject);
var
  ServerCollation, PreviousCollation, CurrentCollation: String;
  Charset, CreateCode: String;
  CollationTable: TDBQuery;
  rx: TRegExpr;
begin
  FConnection := MainForm.ActiveConnection;
  CollationTable := FConnection.CollationTable;
  CurrentCollation := '';
  PreviousCollation := AppSettings.ReadString(asCreateDbCollation);

  // Detect servers default collation
  case FConnection.Parameters.NetTypeGroup of
    ngMySQL:
      ServerCollation := FConnection.GetSessionVariable('collation_server');
    else // TODO: Find out how to retrieve the server's default collation here
      ServerCollation := '';
  end;
  lblServerDefaultCollation.Caption := f_('Servers default: %s', [ServerCollation]);

  if modifyDB.IsEmpty then begin
    Caption := _('Create database ...');
    editDBName.Text := '';
  end
  else begin
    Caption := _('Alter database ...');
    editDBName.Text := modifyDB;

    // Detect current collation
    CreateCode := FConnection.GetVar('SHOW CREATE DATABASE '+FConnection.QuoteIdent(modifyDB), 1);
    rx := TRegExpr.Create;
    rx.Expression := '\sCHARACTER\s+SET\s+(\w+)\b';
    if rx.Exec(CreateCode) then
      Charset := rx.Match[1];
    rx.Expression := '\sCOLLATE\s+(\w+)\b';
    if rx.Exec(CreateCode) then
      CurrentCollation := rx.Match[1];
    rx.Free;
    // Find default collation of given charset
    if (CurrentCollation = '') and (Charset <> '') and Assigned(CollationTable) then begin
      while not CollationTable.Eof do begin
        if (CollationTable.Col('Charset') = Charset) and (LowerCase(CollationTable.Col('Default')) = 'yes') then
          CurrentCollation := CollationTable.Col('Collation');
        CollationTable.Next;
      end;
    end;
  end;

  // Populate collation combo box
  comboCollation.Enabled := Assigned(CollationTable);
  lblCollation.Enabled := comboCollation.Enabled;
  comboCollation.Clear;
  if comboCollation.Enabled then begin
    CollationTable.First;
    while not CollationTable.Eof do begin
      comboCollation.Items.Add(CollationTable.Col('Collation'));
      CollationTable.Next;
    end;
    // Pre-select best fitting collation
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(CurrentCollation);
    if comboCollation.ItemIndex = -1 then
      comboCollation.ItemIndex := comboCollation.Items.IndexOf(PreviousCollation);
    if comboCollation.ItemIndex = -1 then
      comboCollation.ItemIndex := comboCollation.Items.IndexOf(ServerCollation);
    if comboCollation.ItemIndex = -1 then
      comboCollation.ItemIndex := comboCollation.Items.IndexOf('utf8mb4_unicode_ci');
    if comboCollation.ItemIndex = -1 then
      comboCollation.ItemIndex := 0; // give up, use the first one
  end;

  editDBName.SetFocus;
  editDBName.SelectAll;

  // Invoke SQL preview
  Modified(Sender);
  MainForm.SetupSynEditors(Self);
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
  if modifyDB.IsEmpty then try
    sql := GetCreateStatement;
    FConnection.Query(sql);
    FConnection.ShowWarnings;
    AppSettings.WriteString(asCreateDbCollation, comboCollation.Text);
    MainForm.RefreshTree;
    // Close form
    ModalResult := mrOK;
  except
    on E:EDbError do
      ErrorDialog(f_('Creating database "%s" failed.', [editDBName.Text]), E.Message);
    // Keep form open
  end else try
    sql := 'ALTER DATABASE ' + FConnection.QuoteIdent(modifyDB);
    if comboCollation.Text <> '' then
      sql := sql + ' COLLATE ' + FConnection.EscapeString(comboCollation.Text);

    if modifyDB = editDBName.Text then begin
      // Alter database
      FConnection.Query(sql);
      FConnection.ShowWarnings;
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
          Raise Exception.CreateFmt(_('Database "%s" contains stored routine(s), which cannot be moved.'), [modifyDB]);
        if Assigned(ObjectsInNewDb) then begin
          for j:=0 to ObjectsInNewDb.Count-1 do begin
            if (ObjectsInOldDb[i].Name = ObjectsInNewDb[j].Name)
              and (ObjectsInOldDb[i].NodeType = ObjectsInNewDb[j].NodeType) then begin
              // One or more objects have a naming conflict
              Raise Exception.CreateFmt(_('Database "%s" exists and has objects with same names as in "%s"'), [editDBName.Text, modifyDB]);
            end;
          end;
        end;
      end;

      if AllDatabases.IndexOf(editDBName.Text) = -1 then begin
        // Target db does not exist - create it
        FConnection.Query(GetCreateStatement);
        FConnection.ShowWarnings;
      end else begin
        if MessageDialog(f_('Database "%s" exists. But it does not contain objects with same names as in "%s", so it''s uncritical to move everything. Move all objects to "%s"?', [editDBName.Text, modifyDB, editDBName.Text]),
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
        FConnection.ShowWarnings;
        FConnection.ClearDbObjects(modifyDB);
        FConnection.ClearDbObjects(editDBName.Text);
      end;
      // Last check if old db is really empty, before we drop it.
      ObjectsLeft := FConnection.GetDBObjects(modifyDB);
      if ObjectsLeft.Count = 0 then begin
        FConnection.Query('DROP DATABASE '+FConnection.QuoteIdent(modifyDB));
        FConnection.ShowWarnings;
        MainForm.RefreshTree;
      end;
    end;
    // Close form
    ModalResult := mrOK;
  except
    on E:Exception do
      ErrorDialog(f_('Altering database "%s" failed.', [editDBName.Text]), E.Message);
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
    FConnection.Parameters.AllDatabasesStr := Implode(';', AllDatabases);
    AppSettings.WriteString(asDatabases, FConnection.Parameters.AllDatabasesStr);
  end;
end;


{**
  Called on each change
}
procedure TCreateDatabaseForm.Modified(Sender: TObject);
begin
  SynMemoCreateCode.Clear;
  SynMemoCreateCode.Text := GetCreateStatement;
end;


{**
  Generate CREATE DATABASE statement, used for preview and execution
}
function TCreateDatabaseForm.GetCreateStatement: String;
begin
  Result := 'CREATE DATABASE ' + FConnection.QuoteIdent( editDBName.Text );
  if comboCollation.Enabled and (comboCollation.Text <> '') then
    Result := Result + ' /*!40100 COLLATE ' + FConnection.EscapeString(comboCollation.Text) + ' */';
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
