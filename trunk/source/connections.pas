unit connections;


// -------------------------------------
// Connections (start-window)
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ZPlainMySqlDriver,
  PngSpeedButton, TntStdCtrls, ComCtrls, ToolWin;

type
  Tconnform = class(TForm)
    editHost: TEdit;
    lblHost: TLabel;
    lblUsername: TLabel;
    editUsername: TEdit;
    lblPassword: TLabel;
    editPassword: TEdit;
    lblPort: TLabel;
    editPort: TEdit;
    lblTimeout: TLabel;
    editTimeout: TEdit;
    pnlLogo: TPanel;
    comboSession: TComboBox;
    imgLogo: TImage;
    lblDescription: TLabel;
    btnCancel: TButton;
    btnConnect: TButton;
    chkCompressed: TCheckBox;
    lblSeconds: TLabel;
    lblOnlyDBs: TLabel;
    editOnlyDBs: TTntEdit;
    chkSorted: TCheckBox;
    btnSaveAndConnect: TButton;
    tlbEdit: TToolBar;
    btnNew: TToolButton;
    btnSave: TToolButton;
    btnDelete: TToolButton;
    btnSaveAs: TToolButton;
    btnEditDesc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveAndConnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure comboSessionSelect(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure ButtonEditDescClick(Sender: TObject);
  private
    { Private declarations }
    procedure FillSessionCombo(Sender: TObject);
  public
    { Public declarations }
  end;


implementation
 uses Main, helpers, MysqlQueryThread;

{$I const.inc}

{$R *.DFM}


{**
  FormCreate
}
procedure Tconnform.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
end;


// Connect
procedure Tconnform.btnConnectClick(Sender: TObject);
var
  btn: TButton;
begin
  Screen.Cursor := crHourglass;
  // Save last connection name to registry
  OpenRegistry;
  MainReg.WriteString(REGNAME_LASTSESSION, comboSession.Text);

  btn := Sender as TButton;
  btn.Enabled := false;

  if Mainform.InitConnection(
    editHost.Text,
    editPort.Text,
    editUsername.Text,
    editPassword.Text,
    editOnlyDBs.Text,
    editTimeout.Text,
    IntToStr(Integer(chkCompressed.Checked)),
    IntToStr(Integer(chkSorted.Checked))) then begin
    ModalResult := mrOK;
    Mainform.SessionName := comboSession.Text;
  end else begin
    ModalResult := mrNone;
    btn.Enabled := True;
  end;

  Screen.Cursor := crDefault;
end;



// Read all connections from registry
procedure Tconnform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure Tconnform.FillSessionCombo(Sender: TObject);
begin
  comboSession.Items.Clear;
  if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, true) then
    MainReg.GetKeyNames(comboSession.Items);
  if comboSession.Items.Count > 0 then begin
    comboSession.ItemIndex := 0;
    comboSessionSelect(Sender);
  end;
end;


procedure Tconnform.FormShow(Sender: TObject);
var
  LastSessionIndex: Integer;
begin
  Screen.Cursor := crHourglass;
  FillSessionCombo(Sender);
  LastSessionIndex := comboSession.Items.IndexOf(GetRegValue(REGNAME_LASTSESSION, ''));
  if LastSessionIndex > -1 then begin
    comboSession.ItemIndex := LastSessionIndex;
    comboSessionSelect(Sender);
  end;
  comboSession.SetFocus;
  Screen.Cursor := crDefault;
end;


procedure Tconnform.btnSaveAndConnectClick(Sender: TObject);
begin
  btnSaveClick(Sender);
  btnConnectClick(Sender);
end;

procedure Tconnform.btnSaveClick(Sender: TObject);
begin
  // save connection!
  Screen.Cursor := crHourglass;
  OpenRegistry(comboSession.Text);
  MainReg.WriteString(REGNAME_HOST, editHost.Text);
  MainReg.WriteString(REGNAME_USER, editUsername.Text);
  MainReg.WriteString(REGNAME_PASSWORD, encrypt(editPassword.Text));
  MainReg.WriteString(REGNAME_PORT, editPort.Text);
  MainReg.WriteString(REGNAME_TIMEOUT, editTimeout.Text);
  MainReg.WriteBool(REGNAME_COMPRESSED, chkCompressed.Checked);
  MainReg.WriteString(REGNAME_ONLYDBS, Utf8Encode(editOnlyDBs.Text));
  MainReg.WriteBool(REGNAME_ONLYDBSSORTED, chkSorted.Checked);
  comboSessionSelect(Sender);
  Screen.Cursor := crDefault;
end;


procedure Tconnform.btnSaveAsClick(Sender: TObject);
var
  newName: String;
  NameOK: Boolean;
begin
  // Save as ...
  newName := 'Enter new session name ...';
  NameOK := False;
  OpenRegistry;
  while not NameOK do begin
    if not InputQuery('Clone session ...', 'New session name:', newName) then
      Exit; // Cancelled
    NameOK := not MainReg.KeyExists(REGKEY_SESSIONS + newName);
    if not NameOK then
      MessageDlg('Session name '''+newName+''' already in use.', mtError, [mbOK], 0)
    else begin
      Screen.Cursor := crHourglass;
      MainReg.MoveKey(REGKEY_SESSIONS + comboSession.Text, REGKEY_SESSIONS + newName, False);
      Screen.Cursor := crDefault;
      FillSessionCombo(Sender);
      comboSession.ItemIndex := comboSession.Items.IndexOf(newName);
      comboSessionSelect(Sender);
    end;
  end;
end;


procedure Tconnform.btnNewClick(Sender: TObject);
var
  i : Integer;
  description : String;
begin
  // save new connection!
  i := 0;
  description := 'New Connection';
  while MainReg.KeyExists(REGPATH + REGKEY_SESSIONS + description) do begin
    inc(i);
    description := 'New Connection' + ' (' + inttostr(i) + ')';
  end;
  if not InputQuery('New Connection...', 'Description:', description) then
    exit;
  if MainReg.KeyExists(REGPATH + REGKEY_SESSIONS + description) then
  begin
    MessageDlg('Entry "' + description + '" already exists!', mtError, [mbOK], 0);
    exit;
  end;

  Screen.Cursor := crHourglass;
  OpenRegistry(description);
  MainReg.WriteString(REGNAME_HOST, DEFAULT_HOST);
  MainReg.WriteString(REGNAME_USER, DEFAULT_USER);
  MainReg.WriteString(REGNAME_PASSWORD, encrypt(DEFAULT_PASSWORD));
  MainReg.WriteString(REGNAME_PORT, inttostr(DEFAULT_PORT));
  MainReg.WriteString(REGNAME_TIMEOUT, inttostr(DEFAULT_TIMEOUT));
  MainReg.WriteBool(REGNAME_COMPRESSED, DEFAULT_COMPRESSED);
  MainReg.WriteString(REGNAME_ONLYDBS, '');
  MainReg.WriteBool(REGNAME_ONLYDBSSORTED, DEFAULT_ONLYDBSSORTED);

  // show parameters:
  FillSessionCombo(Sender);
  comboSession.ItemIndex := comboSession.Items.IndexOf(description);
  comboSessionSelect(Sender);
  Screen.Cursor := crDefault;
end;


procedure Tconnform.btnDeleteClick(Sender: TObject);
begin
  if MessageDlg('Delete Entry "' + comboSession.Text + '" ?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    if not MainReg.DeleteKey(REGPATH + REGKEY_SESSIONS + comboSession.Text) then
      MessageDlg('Error while deleting Key from Registry!', mtError, [mbOK], 0);
    FillSessionCombo(Sender);
  end;
end;


procedure Tconnform.comboSessionSelect(Sender: TObject);
var
  Session: String;
  SessionSelected: Boolean;
begin
  // select one connection!
  Screen.Cursor := crHourglass;
  OpenRegistry;
  Session := comboSession.Text;
  SessionSelected := (Session <> '') and MainReg.KeyExists(REGPATH + REGKEY_SESSIONS + Session);
  if SessionSelected then begin
    editHost.Text := GetRegValue(REGNAME_HOST, '', Session);
    editUsername.Text := GetRegValue(REGNAME_USER, '', Session);
    editPassword.Text := decrypt(GetRegValue(REGNAME_PASSWORD, '', Session));
    editPort.Text := GetRegValue(REGNAME_PORT, '', Session);
    editTimeout.Text := GetRegValue(REGNAME_TIMEOUT, '', Session);
    chkCompressed.Checked := GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, Session);
    editOnlyDBs.Text := Utf8Decode(GetRegValue(REGNAME_ONLYDBS, '', Session));
    chkSorted.Checked := GetRegValue(REGNAME_ONLYDBSSORTED, DEFAULT_ONLYDBSSORTED, Session);
  end else begin
    editHost.Text := '';
    editUsername.Text := '';
    editPassword.Text := '';
    editPort.Text := '';
    editTimeout.Text := '';
    chkCompressed.Checked := False;
    editOnlyDBs.Text := '';
    chkSorted.Checked := False;
  end;

  btnConnect.Enabled := SessionSelected;
  btnSave.Enabled := SessionSelected;
  btnSaveAndConnect.Enabled := SessionSelected;
  btnDelete.Enabled := SessionSelected;
  btnEditDesc.Enabled := SessionSelected;
  editHost.Enabled := SessionSelected;
  editUsername.Enabled := SessionSelected;
  editPassword.Enabled := SessionSelected;
  editPort.Enabled := SessionSelected;
  editTimeout.Enabled := SessionSelected;
  editOnlyDBs.Enabled := SessionSelected;
  chkCompressed.Enabled := SessionSelected;
  chkSorted.Enabled := SessionSelected;
  lblHost.Enabled := SessionSelected;
  lblUsername.Enabled := SessionSelected;
  lblPassword.Enabled := SessionSelected;
  lblPort.Enabled := SessionSelected;
  lblTimeout.Enabled := SessionSelected;
  lblDescription.Enabled := SessionSelected;
  lblSeconds.Enabled := SessionSelected;
  lblOnlyDBs.Enabled := SessionSelected;

  Screen.Cursor := crDefault;
end;


procedure Tconnform.Modified(Sender: TObject);
begin
  btnSave.Enabled := true;
  btnSaveAndConnect.Enabled := true;
  chkSorted.Enabled := editOnlyDBs.Text <> '';
end;


{ rename connection-description }
procedure Tconnform.ButtonEditDescClick(Sender: TObject);
var
  newdesc, olddesc : String;
  idx : Integer;
begin
  olddesc := comboSession.Text;
  newdesc := olddesc;
  if not InputQuery('Rename description', 'Rename description:', newdesc) then
    exit;
  if newdesc = olddesc then
    exit;
  if comboSession.Items.IndexOf(newdesc) > -1 then begin
    MessageDLG('Description "'+newdesc+'" already exists!', mtError, [mbCancel], 0);
    exit;
  end;

  idx := comboSession.ItemIndex;
  try
    MainReg.MoveKey(REGPATH + REGKEY_SESSIONS + olddesc, REGPATH + REGKEY_SESSIONS + newdesc, true);
    comboSession.Items[comboSession.ItemIndex] := newdesc;
    comboSession.ItemIndex := idx;
    comboSessionSelect(Sender);
  except
    MessageDLG('Error on renaming.', mtError, [mbCancel], 0);
  end;

end;


end.
