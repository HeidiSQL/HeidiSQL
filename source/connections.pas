unit connections;


// -------------------------------------
// Connections (start-window)
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Registry, Buttons, ExtCtrls, ZPlainMySqlDriver, PngBitBtn;

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
    Bevel1: TBevel;
    ButtonCancel: TButton;
    ButtonConnect: TButton;
    CheckBoxCompressed: TCheckBox;
    lblSeconds: TLabel;
    lblOnlyDBs: TLabel;
    EditOnlyDBs: TEdit;
    TimerCloseFormReminder: TTimer;
    CheckBoxSorted: TCheckBox;
    ButtonSaveAndConnect: TButton;
    btnNew: TPngBitBtn;
    btnSave: TPngBitBtn;
    btnDelete: TPngBitBtn;
    btnEditDesc: TPngBitBtn;
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
 uses Main, helpers, MysqlQueryThread, ChildWin;

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
  reg : TRegistry;
begin
  Screen.Cursor := crSQLWait;
  ButtonConnect.Enabled := false;

  if Mainform.CreateMDIChild(
    EditHost.Text,
    EditPort.Text,
    EditUsername.Text,
    EditPassword.Text,
    EditOnlyDBs.Text,
    EditTimeout.Text,
    IntToStr(Integer(CheckBoxCompressed.Checked)),
    IntToStr(Integer(CheckboxSorted.Checked)),
    ComboboxDescription.Text) then
  begin
    // Save last connection name to registry
    reg := TRegistry.Create;
    if reg.OpenKey(REGPATH, true) then
      reg.WriteString(REGNAME_LASTSESSION, ComboBoxDescription.Text);
    reg.CloseKey;
    reg.Free;
    Close;
  end else
    ButtonConnect.Enabled := True;
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
  reg : TRegistry;
begin
  Screen.Cursor := crHourglass;

  ComboBoxDescription.Items.Clear;
  ComboBoxDescription.Text := '';
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH + REGKEY_SESSIONS, true) then
  begin
    reg.GetKeyNames(ComboBoxDescription.Items);
    reg.Free;
  end;
  lastcon := Mainform.GetRegValue(REGNAME_LASTSESSION, '');
  AutoReconnect := Mainform.GetRegValue(REGNAME_AUTORECONNECT, DEFAULT_AUTORECONNECT);

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
    if OpenKey(REGPATH + REGKEY_SESSIONS + ComboBoxDescription.Text, true) then
    begin
      WriteString(REGNAME_HOST, EditHost.Text);
      WriteString(REGNAME_USER, EditUsername.Text);
      WriteString(REGNAME_PASSWORD, encrypt(EditPassword.Text));
      WriteString(REGNAME_PORT, EditPort.Text);
      WriteString(REGNAME_TIMEOUT, EditTimeout.Text);
      WriteBool(REGNAME_COMPRESSED, CheckBoxCompressed.Checked);
      WriteString(REGNAME_ONLYDBS, EditOnlyDBs.Text);
      WriteBool(REGNAME_ONLYDBSSORTED, CheckBoxSorted.Checked);
      CloseKey;
    end;
    Free;
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
    while KeyExists(REGPATH + REGKEY_SESSIONS + description) do
    begin
      inc(i);
      description := 'New Connection' + ' (' + inttostr(i) + ')';
    end;
    if not InputQuery('New Connection...', 'Description:', description) then
      exit;
    if KeyExists(REGPATH + REGKEY_SESSIONS + description) then
    begin
      MessageDlg('Entry "' + description + '" already exists!', mtError, [mbOK], 0);
      exit;
    end;

    Screen.Cursor := crHourglass;
    ComboBoxDescription.Items.Add(description);
    ComboBoxDescription.ItemIndex := ComboBoxDescription.Items.Count - 1;

    if OpenKey(REGPATH + REGKEY_SESSIONS + ComboBoxDescription.Text, true) then
    begin
      WriteString(REGNAME_HOST, DEFAULT_HOST);
      WriteString(REGNAME_USER, DEFAULT_USER);
      WriteString(REGNAME_PASSWORD, encrypt(DEFAULT_PASSWORD));
      WriteString(REGNAME_PORT, inttostr(DEFAULT_PORT));
      WriteString(REGNAME_TIMEOUT, inttostr(DEFAULT_TIMEOUT));
      WriteBool(REGNAME_COMPRESSED, DEFAULT_COMPRESSED);
      WriteString(REGNAME_ONLYDBS, '');
      WriteBool(REGNAME_ONLYDBSSORTED, DEFAULT_ONLYDBSSORTED);
      CloseKey;
    end;
    Free;
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
      if not DeleteKey(REGPATH + REGKEY_SESSIONS + ComboBoxDescription.Text) then
        MessageDlg('Error while deleting Key from Registry!', mtError, [mbOK], 0);
      Free;
    end;
    FormShow(self);
  end;
end;


procedure Tconnform.ComboBoxDescriptionClick(Sender: TObject);
var
  sessname : String;
begin
  // select one connection!
  Screen.Cursor := crHourglass;
  sessname := ComboBoxDescription.Text;
  EditHost.Text := Mainform.GetRegValue(REGNAME_HOST, '', sessname);
  EditUsername.Text := Mainform.GetRegValue(REGNAME_USER, '', sessname);
  EditPassword.Text := decrypt(Mainform.GetRegValue(REGNAME_PASSWORD, '', sessname));
  EditPort.Text := Mainform.GetRegValue(REGNAME_PORT, '', sessname);
  EditTimeout.Text := Mainform.GetRegValue(REGNAME_TIMEOUT, '', sessname);;
  CheckBoxCompressed.Checked := Mainform.GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, sessname);
  EditOnlyDBs.Text := Mainform.GetRegValue(REGNAME_ONLYDBS, '', sessname);
  CheckBoxSorted.Checked := Mainform.GetRegValue(REGNAME_ONLYDBSSORTED, DEFAULT_ONLYDBSSORTED, sessname);
  btnSave.Enabled := false;
  ButtonSaveAndConnect.Enabled := btnSave.Enabled;
  btnEditDesc.Enabled := ComboBoxDescription.ItemIndex > -1;
  Screen.Cursor := crDefault;
end;

procedure Tconnform.EnableDisable(Enable: Boolean);
begin
  // enable or disable all controls
  ComboBoxDescription.Enabled := Enable;
  ButtonConnect.Enabled := Enable;
  btnSave.Enabled := Enable;
  ButtonSaveAndConnect.Enabled := Enable;
  btnDelete.Enabled := Enable;
  btnEditDesc.Enabled := Enable;

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
      MoveKey(REGPATH + REGKEY_SESSIONS + olddesc, REGPATH + REGKEY_SESSIONS + newdesc, true);
      ComboBoxDescription.Items[ComboBoxDescription.ItemIndex] := newdesc;
      ComboBoxDescription.ItemIndex := idx;
      ComboBoxDescriptionClick(self);
    except
      MessageDLG('Error on renaming.', mtError, [mbCancel], 0);
    end;
    Free;
  end;

end;


end.
