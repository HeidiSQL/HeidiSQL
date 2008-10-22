unit createdatabase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, db, Registry, SynEdit, SynMemo, TntStdCtrls;

type
  TCreateDatabaseForm = class(TForm)
    editDBName: TTNTEdit;
    lblDBName: TLabel;
    comboCharset: TComboBox;
    lblCharset: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblCollation: TLabel;
    comboCollation: TComboBox;
    lblPreview: TLabel;
    SynMemoPreview: TSynMemo;
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure comboCharsetChange(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure editDBNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetCreateStatement: WideString;
  private
    { Private declarations }
    dsCollations : TDataSet;
    defaultCharset : String;
    currentCollation : String;
  public
    { Public declarations }
    modifyDB : WideString;
  end;


implementation

uses main, childwin, helpers;

{$R *.dfm}


{**
  Fetch list with character sets and collations from the server
}
procedure TCreateDatabaseForm.FormCreate(Sender: TObject);
var
  charset: String;
begin
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

  // Setup SynMemoPreview
  SynMemoPreview.Highlighter := Mainform.Childwin.SynSQLSyn1;
  SynMemoPreview.Font := Mainform.Childwin.SynMemoQuery.Font;
end;


procedure TCreateDatabaseForm.FormDestroy(Sender: TObject);
begin
  if dsCollations <> nil then dsCollations.Close;
  FreeAndNil(dsCollations);
end;

{**
  Form gets displayed: Set default values.
}
procedure TCreateDatabaseForm.FormShow(Sender: TObject);
var
  selectCharset,
  currentCharset,
  sql_create : WideString;
begin
  if modifyDB = '' then
  begin
    Caption := 'Create database ...';
    editDBName.Enabled := True;
    editDBName.Text := '';
    editDBName.SetFocus;
    selectCharset := defaultCharset;
  end
  else begin
    Caption := 'Alter database ...';
    editDBName.Text := modifyDB;
    // "RENAME DB" supported since MySQL 5.1.7
    editDBName.Enabled := Mainform.Childwin.mysql_version >= 50107;
    // Set focus to first enabled component
    if editDBName.Enabled then
    begin
      editDBName.SetFocus;
      editDBName.SelectAll;
    end
    else
      comboCharset.SetFocus;
    
    // Detect current charset and collation to be able to preselect them in the pulldowns
    sql_create := Mainform.Childwin.GetVar('SHOW CREATE DATABASE '+Mainform.mask(modifyDB), 1);
    currentCharset := Copy( sql_create, pos('CHARACTER SET', sql_create)+14, Length(sql_create));
    currentCharset := GetFirstWord( currentCharset );
    if currentCharset <> '' then
      selectCharset := currentCharset
    else
      selectCharset := defaultCharset;
    currentCollation := Copy( sql_create, pos('COLLATE', sql_create)+8, Length(sql_create));
    currentCollation := GetFirstWord( currentCollation );
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
  sql : WideString;
  cwin: TMDIChild;
begin
  cwin := Mainform.Childwin;
  if modifyDB = '' then
  begin
    sql := GetCreateStatement;
    Try
      cwin.ExecUpdateQuery( sql );
      // Close form
      ModalResult := mrOK;
    except
      On E:Exception do
        MessageDlg( 'Creating database "'+editDBName.Text+'" failed:'+CRLF+CRLF+E.Message, mtError, [mbOK], 0 );
      // Keep form open
    end;
  end
  else begin
    sql := 'ALTER DATABASE ' + cwin.mask( modifyDB );
    if comboCharset.Enabled and (comboCharset.Text <> '') then
    begin
      sql := sql + ' CHARACTER SET ' + comboCharset.Text;
      if comboCollation.Enabled and (comboCollation.Text <> '') then
        sql := sql + ' COLLATE ' + comboCollation.Text;
    end;
    Try
      cwin.ExecUpdateQuery( sql );
      if modifyDB <> editDBName.Text then
      begin
        cwin.ExecUpdateQuery( 'RENAME DATABASE ' + cwin.mask( modifyDB )
          + ' TO ' + cwin.mask( editDBName.Text ) );
        cwin.DBtree.ResetNode(cwin.DBtree.GetFirst);
      end;
      // Close form
      ModalResult := mrOK;
    except
      On E:Exception do
        MessageDlg( 'Altering database "'+editDBName.Text+'" failed:'+CRLF+CRLF+E.Message, mtError, [mbOK], 0 );
      // Keep form open
    end;
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
function TCreateDatabaseForm.GetCreateStatement: WideString;
begin
  Result := 'CREATE DATABASE ' + Mainform.Childwin.mask( editDBName.Text );
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
