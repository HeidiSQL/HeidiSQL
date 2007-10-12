unit connections;


// -------------------------------------
// Connections (start-window)
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Registry, Buttons, ExtCtrls, ZPlainMySqlDriver;

type
  Tconnform = class(TForm)
    EditHost: TEdit;
    lblHost: TLabel;
    lblUsername: TLabel;
    EditUsername: TEdit;
    lblPassword: TLabel;
    EditPassword: TEdit;
    lblPort: TLabel;
    EditPort: TEdit;
    lblTimeout: TLabel;
    EditTimeout: TEdit;
    pnlScreen: TPanel;
    ComboBoxDescription: TComboBox;
    Image1: TImage;
    lblDescription: TLabel;
    ButtonSave: TBitBtn;
    ButtonNew: TBitBtn;
    ButtonDelete: TBitBtn;
    Bevel1: TBevel;
    ButtonCancel: TButton;
    ButtonConnect: TButton;
    CheckBoxCompressed: TCheckBox;
    lblSeconds: TLabel;
    lblOnlyDBs: TLabel;
    EditOnlyDBs: TEdit;
    TimerCloseFormReminder: TTimer;
    ButtonEditDesc: TSpeedButton;
    CheckBoxSorted: TCheckBox;
    ButtonSaveAndConnect: TButton;
    procedure ButtonSaveAndConnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ComboBoxDescriptionClick(Sender: TObject);
    procedure EnableDisable(Enable: Boolean);
    procedure Modified(Sender: TObject);
    procedure TimerCloseFormReminderTimer(Sender: TObject);
    procedure ButtonEditDescClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function ConnectionWindow (AOwner : TComponent; AFlags : String = '') : Boolean;


implementation
 uses Main, helpers, MysqlQueryThread, MysqlConn, ChildWin;

{$I const.inc}

{$R *.DFM}

function ConnectionWindow (AOwner : TComponent; AFlags : String = '') : Boolean;
var
  f : Tconnform;
begin
  f := Tconnform.Create(AOwner);
  Result := (f.ShowModal = mrOK);
  FreeAndNil (f);
end;


// Cancel
procedure Tconnform.ButtonCancelClick(Sender: TObject);
begin
  close;
end;


// Connect
procedure Tconnform.ButtonConnectClick(Sender: TObject);
var
  cp : TOpenConnProf;
  mysqlconn : TMysqlConn;
  f : TMDIChild;
begin
  Screen.Cursor := crSQLWait;

  // fill structure
  ZeroMemory (@cp,SizeOf(cp));

  with cp do
    begin
      MysqlParams.Protocol := 'mysql';
      MysqlParams.Host := trim( EditHost.Text );
      MysqlParams.Port := strToIntDef(EditPort.Text, MYSQL_PORT);
      MysqlParams.Database := '';
      MysqlParams.User := EditUsername.Text;
      MysqlParams.Pass := EditPassword.Text;

      // additional
      if CheckBoxCompressed.Checked then
        MysqlParams.PrpCompress := 'true'
      else
        MysqlParams.PrpCompress := 'false';

      MysqlParams.PrpTimeout := EditTimeout.Text;
      MysqlParams.PrpDbless := 'true';
      MysqlParams.PrpClientLocalFiles := 'true';
      MysqlParams.PrpClientInteractive := 'true';

      DatabaseList := EditOnlyDbs.Text;
      DatabaseListSort := CheckBoxSorted.Checked;
      Description := ComboBoxDescription.Text;
    end;

  ButtonConnect.Enabled := false;
  mainform.Showstatus('Connecting to ' + EditHost.Text + '...', 2, true);

  // Save last connection name to registry
  with TRegistry.Create do
  begin
    if OpenKey(REGPATH, true) then
      WriteString('lastcon', ComboBoxDescription.Text);
    CloseKey;
  end;

  mysqlconn := TMysqlConn.Create(@cp);

  // attempt to establish connection
  case mysqlconn.Connect() of
    MCR_SUCCESS:
      begin
        // create child window and pass it the conn params and the opened connection
        f := TMDIChild.Create(Application);
        f.Init(@cp,mysqlconn); // childwin responsible to free mysqlconn

        Close();
      end;
  else
    // attempt failed -- show error and keep window open
    MessageDlg ( 'Could not establish connection! Details:'+CRLF+CRLF+mysqlconn.LastError, mtError, [mbOK], 0);
    ButtonConnect.Enabled := True;
    FreeAndNil (mysqlconn);
  end;

  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  Screen.Cursor := crDefault;
  
end;



// Read all connections from registry
procedure Tconnform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure Tconnform.FormShow(Sender: TObject);
var
  i       : Integer;
  lastcon : String;
  AutoReconnect : Boolean;
begin
  Screen.Cursor := crHourglass;

  ComboBoxDescription.Items.Clear;
  ComboBoxDescription.Text := '';
  with TRegistry.Create do
  begin
    if OpenKey(REGPATH + '\Servers', true) then
    begin
      GetKeyNames(ComboBoxDescription.Items);
      CloseKey;
    end;
    OpenKey(REGPATH, true);
    lastcon := ReadString('lastcon');
    if ValueExists('AutoReconnect') then
      AutoReconnect := ReadBool('AutoReconnect')
    else
      AutoReconnect := false;
    CloseKey;
  end;
  if ComboBoxDescription.Items.Count > 0 then
  begin
    EnableDisable(true);

    // select one connection
    with ComboBoxDescription do
    begin
      ItemIndex := 0;
      for i:=0 to Items.Count -1 do
        if Items[i] = lastcon then
          ItemIndex := i;
    end;

    ComboBoxDescriptionClick(self);
    ComboBoxDescription.SetFocus;
  end else
  begin
    EditHost.Text := '';
    EditUsername.Text := '';
    EditPassword.Text := '';
    EditPort.Text := '';
    EditTimeout.Text := '';
    EditOnlyDBs.Text := '';
    CheckBoxCompressed.Checked := false;
    CheckBoxSorted.Checked := true;
    EnableDisable(false);
  end;
  Screen.Cursor := crDefault;

  if not main.appstarted then
  begin
    main.appstarted := true;
    if AutoReconnect and (ComboBoxDescription.ItemIndex > -1) then
    begin
      ButtonConnectClick(self);
      TimerCloseFormReminder.Enabled := true;
    end;
  end;
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
end;


procedure Tconnform.ButtonSaveAndConnectClick(Sender: TObject);
begin
  ButtonSaveClick( Sender );
  ButtonConnectClick( Sender );
end;

procedure Tconnform.ButtonSaveClick(Sender: TObject);
begin
  // save connection!
  Screen.Cursor := crHourglass;
  with TRegistry.Create do
  begin
    if OpenKey(REGPATH + '\Servers\' + ComboBoxDescription.Text, true) then
    begin
      WriteString('Host', EditHost.Text);
      WriteString('User', EditUsername.Text);
      WriteString('Password', encrypt(EditPassword.Text));
      WriteString('Port', EditPort.Text);
      WriteString('Timeout', EditTimeout.Text);
      WriteBool('Compressed', CheckBoxCompressed.Checked);
      WriteString('OnlyDBs', EditOnlyDBs.Text);
      WriteBool('OnlyDBsSorted', CheckBoxSorted.Checked);
      CloseKey;
    end;
  end;
  ComboBoxDescriptionClick(self);
  Screen.Cursor := crDefault;
end;


procedure Tconnform.ButtonNewClick(Sender: TObject);
var
  i : Integer;
  description : String;
begin
  // save new connection!
  with TRegistry.Create do
  begin
    i := 0;
    description := 'New Connection';
    while KeyExists(REGPATH + '\Servers\' + description) do
    begin
      inc(i);
      description := 'New Connection' + ' (' + inttostr(i) + ')';
    end;
    if not InputQuery('New Connection...', 'Description:', description) then
      exit;
    if KeyExists(REGPATH + '\Servers\' + description) then
    begin
      MessageDlg('Entry "' + description + '" already exists!', mtError, [mbOK], 0);
      exit;
    end;

    Screen.Cursor := crHourglass;
    ComboBoxDescription.Items.Add(description);
    ComboBoxDescription.ItemIndex := ComboBoxDescription.Items.Count - 1;

    if OpenKey(REGPATH + '\Servers\' + ComboBoxDescription.Text, true) then
    begin
      WriteString('Host', LOCAL_HOST);
      WriteString('User', 'root');
      WriteString('Password', encrypt(''));
      WriteString('Port', inttostr(MYSQL_PORT));
      WriteString('Timeout', inttostr(30));
      WriteBool('Compressed', false);
      WriteString('OnlyDBs', '');
      WriteBool('OnlyDBsSorted', true);
      CloseKey;
    end;
  end;

  EnableDisable(true);

  // show parameters:
  ComboBoxDescriptionClick(self);
  Screen.Cursor := crDefault;
end;


procedure Tconnform.ButtonDeleteClick(Sender: TObject);
begin
  if MessageDlg('Delete Entry "' + ComboBoxDescription.Text + '" ?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    with TRegistry.Create do
    begin
      if not DeleteKey(REGPATH + '\Servers\' + ComboBoxDescription.Text) then
        MessageDlg('Error while deleting Key from Registry!', mtError, [mbOK], 0);
    end;
    FormShow(self);
  end;
end;


procedure Tconnform.ComboBoxDescriptionClick(Sender: TObject);
begin
  // select one connection!
  Screen.Cursor := crHourglass;
  with TRegistry.Create do
  begin
    if OpenKey(REGPATH + '\Servers\' + ComboBoxDescription.Text, true) then
    begin
      EditHost.Text := ReadString('Host');
      EditUsername.Text := ReadString('User');
      EditPassword.Text := decrypt(ReadString('Password'));
      EditPort.Text := ReadString('Port');
      EditTimeout.Text := ReadString('Timeout');
      CheckBoxCompressed.Checked := ReadBool('Compressed');
      EditOnlyDBs.Text := ReadString('OnlyDBs');
      if ValueExists('OnlyDBsSorted') then
        CheckBoxSorted.Checked := ReadBool('OnlyDBsSorted')
      else
        CheckBoxSorted.Checked := false; // for existing connections from older HS-versions always off
      CloseKey;
    end;
  end;
  ButtonSave.Enabled := false;
  ButtonSaveAndConnect.Enabled := ButtonSave.Enabled;
  ButtonEditDesc.Enabled := ComboBoxDescription.ItemIndex > -1;
  Screen.Cursor := crDefault;
end;

procedure Tconnform.EnableDisable(Enable: Boolean);
begin
  // enable or disable all controls
  ComboBoxDescription.Enabled := Enable;
  ButtonConnect.Enabled := Enable;
  ButtonSave.Enabled := Enable;
  ButtonSaveAndConnect.Enabled := Enable;
  ButtonDelete.Enabled := Enable;
  ButtonEditDesc.Enabled := Enable;

  EditHost.Enabled := Enable;
  EditUsername.Enabled := Enable;
  EditPassword.Enabled := Enable;
  EditPort.Enabled := Enable;
  EditTimeout.Enabled := Enable;
  EditOnlyDBs.Enabled := Enable;
  CheckBoxCompressed.Enabled := Enable;
  CheckBoxSorted.Enabled := Enable;
  lblHost.Enabled := Enable;
  lblUsername.Enabled := Enable;
  lblPassword.Enabled := Enable;
  lblPort.Enabled := Enable;
  lblTimeout.Enabled := Enable;
  lblDescription.Enabled := Enable;
  lblSeconds.Enabled := Enable;
  lblOnlyDBs.Enabled := Enable;
end;


procedure Tconnform.Modified(Sender: TObject);
begin
  ButtonSave.Enabled := true;
  ButtonSaveAndConnect.Enabled := true;
  CheckBoxSorted.Enabled := EditOnlyDBs.Text <> '';
end;

procedure Tconnform.TimerCloseFormReminderTimer(Sender: TObject);
begin
  TimerCloseFormReminder.Enabled := false;
  close;
end;


{ rename connection-description }
procedure Tconnform.ButtonEditDescClick(Sender: TObject);
var
  newdesc, olddesc : String;
  idx : Integer;
begin
  olddesc := ComboBoxDescription.Text;
  newdesc := olddesc;
  if not InputQuery('Rename description', 'Rename description:', newdesc) then
    exit;
  if newdesc = olddesc then
    exit;
  if ComboBoxDescription.Items.IndexOf(newdesc) > -1 then begin
    MessageDLG('Description "'+newdesc+'" already exists!', mtError, [mbCancel], 0);
    exit;
  end;

  with TRegistry.Create do begin
    idx := ComboBoxDescription.ItemIndex;
    try
      MoveKey(REGPATH + '\Servers\' + olddesc, REGPATH + '\Servers\' + newdesc, true);
      ComboBoxDescription.Items[ComboBoxDescription.ItemIndex] := newdesc;
      ComboBoxDescription.ItemIndex := idx;
      ComboBoxDescriptionClick(self);
    except
      MessageDLG('Error on renaming.', mtError, [mbCancel], 0);
    end;
  end;

end;


end.
