unit connections;


// -------------------------------------
// HeidiSQL
// Connections (start-window)
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Registry, Buttons, ExtCtrls, ZPlainMySqlDriver;

type
  Tconnform = class(TForm)
    EditHost: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditBenutzer: TEdit;
    Label3: TLabel;
    EditPasswort: TEdit;
    Label4: TLabel;
    EditPort: TEdit;
    Label5: TLabel;
    EditTimeout: TEdit;
    Panel1: TPanel;
    ComboBoxDescription: TComboBox;
    Image1: TImage;
    Label6: TLabel;
    ButtonSave: TBitBtn;
    ButtonNew: TBitBtn;
    ButtonDelete: TBitBtn;
    Bevel1: TBevel;
    ButtonCancel: TButton;
    ButtonConnect: TButton;
    CheckBoxCompressed: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    EditOnlyDBs: TEdit;
    Timer1: TTimer;
    ButtonEditDesc: TSpeedButton;
    CheckBoxSorted: TCheckBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ComboBoxDescriptionClick(Sender: TObject);
    procedure EnableDisable(Enable: Boolean);
    procedure Modified(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ButtonEditDescClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  connform: Tconnform;

implementation
 uses Main, helpers, MysqlQueryThread;

const
	CRLF = #13#10;

{$R *.DFM}




// Cancel
procedure Tconnform.ButtonCancelClick(Sender: TObject);
begin
  close;
end;


// Connect
procedure Tconnform.ButtonConnectClick(Sender: TObject);
var
  cp : TConnParams;
begin

  // fill structure
  ZeroMemory (@cp,SizeOf(cp));

  cp.Protocol := 'mysql';
  cp.Host := EditHost.Text;
  cp.Port := strToIntDef(EditPort.Text, MYSQL_PORT);
  cp.Database := '';
  cp.User := EditBenutzer.Text;
  cp.Pass := EditPasswort.Text;

  // additional
  if CheckBoxCompressed.Checked then
    cp.PrpCompress := 'true'
  else
    cp.PrpCompress := 'false';

  cp.PrpTimeout := EditTimeout.Text;
  cp.PrpDbless := 'true';
  cp.PrpClientLocalFiles := 'true';
  cp.PrpClientInteractive := 'true';

  cp.DatabaseList := EditOnlyDbs.Text;
  cp.DatabaseListSort := CheckBoxSorted.Checked;
  cp.Description := ComboBoxDescription.Text;

  ButtonConnect.Enabled := false;
  mainform.Showstatus('Connecting to ' + EditHost.Text + '...', 2, true);
  with TRegistry.Create do
  begin
    if OpenKey(regpath, true) then
      WriteString('lastcon', ComboBoxDescription.Text);
    CloseKey;
  end;
  close;
  MainForm.connect(sender,@cp);
end;



// Read all connections from registry
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
    if OpenKey(regpath + '\Servers', true) then
    begin
      GetKeyNames(ComboBoxDescription.Items);
      CloseKey;
    end;
    OpenKey(regpath, true);
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
    EditBenutzer.Text := '';
    EditPasswort.Text := '';
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
      Timer1.Enabled := true;
    end;
  end;
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
end;


procedure Tconnform.ButtonSaveClick(Sender: TObject);
begin
  // save connection!
  Screen.Cursor := crHourglass;
  with TRegistry.Create do
  begin
    if OpenKey(regpath + '\Servers\' + ComboBoxDescription.Text, true) then
    begin
      WriteString('Host', EditHost.Text);
      WriteString('User', EditBenutzer.Text);
      WriteString('Password', encrypt(EditPasswort.Text));
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
    while KeyExists(regpath + '\Servers\' + description) do
    begin
      inc(i);
      description := 'New Connection' + ' (' + inttostr(i) + ')';
    end;
    if not InputQuery('New Connection...', 'Description:', description) then
      exit;
    if KeyExists(regpath + '\Servers\' + description) then
    begin
      MessageDlg('Entry "' + description + '" already exists!', mtError, [mbOK], 0);
      exit;
    end;

    Screen.Cursor := crHourglass;
    ComboBoxDescription.Items.Add(description);
    ComboBoxDescription.ItemIndex := ComboBoxDescription.Items.Count - 1;

    if OpenKey(regpath + '\Servers\' + ComboBoxDescription.Text, true) then
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
      if not DeleteKey(regpath + '\Servers\' + ComboBoxDescription.Text) then
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
    if OpenKey(regpath + '\Servers\' + ComboBoxDescription.Text, true) then
    begin
      EditHost.Text := ReadString('Host');
      EditBenutzer.Text := ReadString('User');
      EditPasswort.Text := decrypt(ReadString('Password'));
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
  ButtonEditDesc.Enabled := ComboBoxDescription.ItemIndex > -1;
  Screen.Cursor := crDefault;
end;

procedure Tconnform.EnableDisable(Enable: Boolean);
begin
  // enable or disable all controls
  ComboBoxDescription.Enabled := Enable;
  ButtonConnect.Enabled := Enable;
  ButtonSave.Enabled := Enable;
  ButtonDelete.Enabled := Enable;

  EditHost.Enabled := Enable;
  EditBenutzer.Enabled := Enable;
  EditPasswort.Enabled := Enable;
  EditPort.Enabled := Enable;
  EditTimeout.Enabled := Enable;
  EditOnlyDBs.Enabled := Enable;
  CheckBoxCompressed.Enabled := Enable;
  CheckBoxSorted.Enabled := Enable;
  Label1.Enabled := Enable;
  Label2.Enabled := Enable;
  Label3.Enabled := Enable;
  Label4.Enabled := Enable;
  Label5.Enabled := Enable;
  Label6.Enabled := Enable;
  Label7.Enabled := Enable;
  Label8.Enabled := Enable;
end;


procedure Tconnform.Modified(Sender: TObject);
begin
  ButtonSave.Enabled := true;
  CheckBoxSorted.Enabled := EditOnlyDBs.Text <> '';
end;

procedure Tconnform.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
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
      MoveKey(regpath + '\Servers\' + olddesc, regpath + '\Servers\' + newdesc, true);
      ComboBoxDescription.Items[ComboBoxDescription.ItemIndex] := newdesc;
      ComboBoxDescription.ItemIndex := idx;
      ComboBoxDescriptionClick(self);
    except
      MessageDLG('Error on renaming.', mtError, [mbCancel], 0);
    end;
  end;

end;


end.
