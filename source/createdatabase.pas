unit createdatabase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, db, Registry;

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
    procedure btnOKClick(Sender: TObject);
    procedure comboCharsetChange(Sender: TObject);
    procedure editDBNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    listCharsets : TStringList;
    dsCollations : TDataSet;
    defaultCharset : String;
  public
    { Public declarations }
  end;


implementation

uses main, childwin, helpers;

{$R *.dfm}


{**
  Fetch list with character sets and collations from the server
}
procedure TCreateDatabaseForm.FormCreate(Sender: TObject);
begin
  try
    listCharsets := Mainform.Childwin.GetCol('SHOW CHARACTER SET');
    dsCollations := Mainform.Childwin.ExecSelectQuery('SHOW COLLATION');
  except
    // Ignore it when the above statements don't work on pre 4.1 servers.
    // If the list(s) are nil, disable the combobox(es), so we create the db without charset.
  end;

  if (listCharsets <> nil) and (listCharsets.Count > 0) then
  begin
    comboCharset.Enabled := True;
    comboCharset.Items := listCharsets;
    // Detect servers default charset
    defaultCharset := Mainform.Childwin.GetVar( 'SHOW VARIABLES LIKE '+esc('character_set_server'), 1 );
  end;

  comboCollation.Enabled := dsCollations <> nil;
end;


{**
  Form gets displayed: Set default values.
}
procedure TCreateDatabaseForm.FormShow(Sender: TObject);
begin
  editDBName.Text := '';
  if comboCharset.Items.Count > 0 then
  begin
    if comboCharset.Items.IndexOf(defaultCharset) > -1 then
      comboCharset.ItemIndex := comboCharset.Items.IndexOf(defaultCharset)
    else
      comboCharset.ItemIndex := 0;
    // Invoke selecting default collation
    comboCharsetChange( Sender );
  end;
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
  // Preselect default collation
  if comboCollation.Items.IndexOf(defaultCollation) > -1 then
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(defaultCollation)
  else
    comboCollation.ItemIndex := 0;
  comboCollation.Items.EndUpdate;
end;


{**
  User writes something into editDBName
}
procedure TCreateDatabaseForm.editDBNameChange(Sender: TObject);
begin
  try
    ensureValidIdentifier( editDBName.Text );
    editDBName.Font.Color := clWindowText;
    editDBName.Color := clWindow;
    // Enable "OK"-Button if we have a valid name
    btnOK.Enabled := True;
  except
    editDBName.Font.Color := clRed;
    editDBName.Color := clYellow;
    btnOK.Enabled := False;
  end;
end;


{**
  Create the database
}
procedure TCreateDatabaseForm.btnOKClick(Sender: TObject);
var
  sql : String;
begin
  sql := 'CREATE DATABASE ' + Mainform.Childwin.mask( editDBName.Text );
  if comboCharset.Enabled and (comboCharset.Text <> '') then
    sql := sql + ' CHARACTER SET ' + comboCharset.Text;
  if comboCollation.Enabled and (comboCollation.Text <> '') then
    sql := sql + ' COLLATE ' + comboCollation.Text;
  Try
    Mainform.Childwin.ExecUpdateQuery( sql );
    // Close form
    ModalResult := mrOK;
  except
    On E:Exception do
      MessageDlg( 'Creating database "'+editDBName.Text+'" failed:'+CRLF+CRLF+E.Message, mtError, [mbOK], 0 );
    // Keep form open
  end;
end;


end.
