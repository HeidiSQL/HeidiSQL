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
    ButtonCancel: TButton;
    ButtonConnect: TButton;
    CheckBoxCompressed: TCheckBox;
    lblSeconds: TLabel;
    lblOnlyDBs: TLabel;
    EditOnlyDBs: TTNTEdit;
    CheckBoxSorted: TCheckBox;
    ButtonSaveAndConnect: TButton;
    ToolBar1: TToolBar;
    btnNew: TToolButton;
    btnSave: TToolButton;
    btnDelete: TToolButton;
    btnEditDesc: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveAndConnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ComboBoxDescriptionClick(Sender: TObject);
    procedure EnableDisable(Enable: Boolean);
    procedure Modified(Sender: TObject);
    procedure ButtonEditDescClick(Sender: TObject);
  private
    { Private declarations }
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
procedure Tconnform.ButtonConnectClick(Sender: TObject);
var
  btn: TButton;
begin
  Screen.Cursor := crHourglass;
  // Save last connection name to registry
  OpenRegistry;
  MainReg.WriteString(REGNAME_LASTSESSION, ComboBoxDescription.Text);

  btn := Sender as TButton;
  btn.Enabled := false;

  if Mainform.InitConnection(
    EditHost.Text,
    EditPort.Text,
    EditUsername.Text,
    EditPassword.Text,
    EditOnlyDBs.Text,
    EditTimeout.Text,
    IntToStr(Integer(CheckBoxCompressed.Checked)),
    IntToStr(Integer(CheckboxSorted.Checked))) then begin
    ModalResult := mrOK;
    Mainform.SessionName := ComboBoxDescription.Text;
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

procedure Tconnform.FormShow(Sender: TObject);
var
  i       : Integer;
  lastcon : String;
begin
  Screen.Cursor := crHourglass;

  ComboBoxDescription.Items.Clear;
  ComboBoxDescription.Text := '';
  if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, true) then
    MainReg.GetKeyNames(ComboBoxDescription.Items);
  lastcon := GetRegValue(REGNAME_LASTSESSION, '');

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
  MainForm.ShowStatus( STATUS_MSG_READY );
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
  OpenRegistry(ComboBoxDescription.Text);
  MainReg.WriteString(REGNAME_HOST, EditHost.Text);
  MainReg.WriteString(REGNAME_USER, EditUsername.Text);
  MainReg.WriteString(REGNAME_PASSWORD, encrypt(EditPassword.Text));
  MainReg.WriteString(REGNAME_PORT, EditPort.Text);
  MainReg.WriteString(REGNAME_TIMEOUT, EditTimeout.Text);
  MainReg.WriteBool(REGNAME_COMPRESSED, CheckBoxCompressed.Checked);
  MainReg.WriteString(REGNAME_ONLYDBS, Utf8Encode(EditOnlyDBs.Text));
  MainReg.WriteBool(REGNAME_ONLYDBSSORTED, CheckBoxSorted.Checked);
  ComboBoxDescriptionClick(self);
  Screen.Cursor := crDefault;
end;


procedure Tconnform.ButtonNewClick(Sender: TObject);
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
  ComboBoxDescription.Items.Add(description);
  ComboBoxDescription.ItemIndex := ComboBoxDescription.Items.Count - 1;

  OpenRegistry(ComboBoxDescription.Text);
  MainReg.WriteString(REGNAME_HOST, DEFAULT_HOST);
  MainReg.WriteString(REGNAME_USER, DEFAULT_USER);
  MainReg.WriteString(REGNAME_PASSWORD, encrypt(DEFAULT_PASSWORD));
  MainReg.WriteString(REGNAME_PORT, inttostr(DEFAULT_PORT));
  MainReg.WriteString(REGNAME_TIMEOUT, inttostr(DEFAULT_TIMEOUT));
  MainReg.WriteBool(REGNAME_COMPRESSED, DEFAULT_COMPRESSED);
  MainReg.WriteString(REGNAME_ONLYDBS, '');
  MainReg.WriteBool(REGNAME_ONLYDBSSORTED, DEFAULT_ONLYDBSSORTED);

  EnableDisable(true);

  // show parameters:
  ComboBoxDescriptionClick(self);
  Screen.Cursor := crDefault;
end;


procedure Tconnform.ButtonDeleteClick(Sender: TObject);
begin
  if MessageDlg('Delete Entry "' + ComboBoxDescription.Text + '" ?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    if not MainReg.DeleteKey(REGPATH + REGKEY_SESSIONS + ComboBoxDescription.Text) then
      MessageDlg('Error while deleting Key from Registry!', mtError, [mbOK], 0);
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
  EditHost.Text := GetRegValue(REGNAME_HOST, '', sessname);
  EditUsername.Text := GetRegValue(REGNAME_USER, '', sessname);
  EditPassword.Text := decrypt(GetRegValue(REGNAME_PASSWORD, '', sessname));
  EditPort.Text := GetRegValue(REGNAME_PORT, '', sessname);
  EditTimeout.Text := GetRegValue(REGNAME_TIMEOUT, '', sessname);;
  CheckBoxCompressed.Checked := GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, sessname);
  EditOnlyDBs.Text := Utf8Decode(GetRegValue(REGNAME_ONLYDBS, '', sessname));
  CheckBoxSorted.Checked := GetRegValue(REGNAME_ONLYDBSSORTED, DEFAULT_ONLYDBSSORTED, sessname);
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
  btnSave.Enabled := true;
  ButtonSaveAndConnect.Enabled := true;
  CheckBoxSorted.Enabled := EditOnlyDBs.Text <> '';
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

  idx := ComboBoxDescription.ItemIndex;
  try
    MainReg.MoveKey(REGPATH + REGKEY_SESSIONS + olddesc, REGPATH + REGKEY_SESSIONS + newdesc, true);
    ComboBoxDescription.Items[ComboBoxDescription.ItemIndex] := newdesc;
    ComboBoxDescription.ItemIndex := idx;
    ComboBoxDescriptionClick(self);
  except
    MessageDLG('Error on renaming.', mtError, [mbCancel], 0);
  end;

end;


end.
