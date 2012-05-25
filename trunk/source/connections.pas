unit connections;


// -------------------------------------
// Connections (start-window)
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  VirtualTrees, Menus, Graphics, Generics.Collections,
  dbconnection;

type
  Tconnform = class(TForm)
    btnCancel: TButton;
    btnOpen: TButton;
    btnSave: TButton;
    ListSessions: TVirtualStringTree;
    btnNew: TButton;
    btnDelete: TButton;
    popupSessions: TPopupMenu;
    Save1: TMenuItem;
    Delete1: TMenuItem;
    Saveas1: TMenuItem;
    TimerStatistics: TTimer;
    PageControlDetails: TPageControl;
    tabSettings: TTabSheet;
    lblStartupScript: TLabel;
    lblPort: TLabel;
    lblPassword: TLabel;
    lblHost: TLabel;
    lblUsername: TLabel;
    lblNetworkType: TLabel;
    editStartupScript: TButtonedEdit;
    chkCompressed: TCheckBox;
    editPort: TEdit;
    updownPort: TUpDown;
    editPassword: TEdit;
    editUsername: TEdit;
    editHost: TEdit;
    tabSSLOptions: TTabSheet;
    lblSSLPrivateKey: TLabel;
    lblSSLCACertificate: TLabel;
    lblSSLCertificate: TLabel;
    editSSLPrivateKey: TButtonedEdit;
    editSSLCACertificate: TButtonedEdit;
    editSSLCertificate: TButtonedEdit;
    tabStatistics: TTabSheet;
    lblLastConnectLeft: TLabel;
    lblCounterLeft: TLabel;
    lblCreatedLeft: TLabel;
    lblCreatedRight: TLabel;
    lblCounterRight: TLabel;
    lblLastConnectRight: TLabel;
    tabSSHtunnel: TTabSheet;
    editSSHlocalport: TEdit;
    editSSHUser: TEdit;
    editSSHPassword: TEdit;
    lblSSHLocalPort: TLabel;
    lblSSHUser: TLabel;
    lblSSHPassword: TLabel;
    editSSHPlinkExe: TButtonedEdit;
    lblSSHPlinkExe: TLabel;
    comboNetType: TComboBox;
    lblSSHhost: TLabel;
    editSSHhost: TEdit;
    editSSHport: TEdit;
    editSSHPrivateKey: TButtonedEdit;
    lblSSHkeyfile: TLabel;
    lblDownloadPlink: TLabel;
    comboDatabases: TComboBox;
    lblDatabase: TLabel;
    chkLoginPrompt: TCheckBox;
    lblPlinkTimeout: TLabel;
    editSSHTimeout: TEdit;
    updownSSHTimeout: TUpDown;
    chkWindowsAuth: TCheckBox;
    splitterMain: TSplitter;
    tabStart: TTabSheet;
    lblHelp: TLabel;
    chkWantSSL: TCheckBox;
    btnImportSettings: TButton;
    timerSettingsImport: TTimer;
    chkLocalTimeZone: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure ListSessionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure ListSessionsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ListSessionsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ListSessionsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: String);
    procedure ListSessionsFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure TimerStatisticsTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListSessionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      out EditLink: IVTEditLink);
    procedure PickFile(Sender: TObject);
    procedure editSSHPlinkExeChange(Sender: TObject);
    procedure editHostChange(Sender: TObject);
    procedure lblDownloadPlinkClick(Sender: TObject);
    procedure comboDatabasesDropDown(Sender: TObject);
    procedure chkLoginPromptClick(Sender: TObject);
    procedure ListSessionsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ListSessionsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure comboNetTypeChange(Sender: TObject);
    procedure splitterMainMoved(Sender: TObject);
    procedure btnImportSettingsClick(Sender: TObject);
    procedure timerSettingsImportTimer(Sender: TObject);
  private
    { Private declarations }
    FLoaded: Boolean;
    FSessions: TObjectList<TConnectionParameters>;
    FSessionModified, FOnlyPasswordModified, FSessionAdded: Boolean;
    FServerVersion: String;
    FSessionColor: TColor;
    FSettingsImportWaitTime: Cardinal;
    procedure SetSessions;
    function SelectedSession: String;
    function CurrentParams: TConnectionParameters;
    procedure FinalizeModifications(var CanProceed: Boolean);
    procedure SaveCurrentValues(Session: String; IsNew: Boolean);
    procedure ValidateControls;
  public
    { Public declarations }
  end;


implementation

uses Main, helpers, grideditlinks;

{$I const.inc}

{$R *.DFM}


procedure Tconnform.FormCreate(Sender: TObject);
var
  LastActiveSession: String;
  LastSessions: TStringList;
  PSess: PConnectionParameters;
  hSysMenu: THandle;
  nt: TNetType;
  Node: PVirtualNode;
  Params: TConnectionParameters;
begin
  // Fix GUI stuff
  InheritFont(Font);
  SetWindowSizeGrip(Handle, True);
  Width := GetRegValue(REGNAME_SESSMNGR_WINWIDTH, Width);
  Height := GetRegValue(REGNAME_SESSMNGR_WINHEIGHT, Height);
  ListSessions.Width := GetRegValue(REGNAME_SESSMNGR_LISTWIDTH, ListSessions.Width);
  splitterMain.OnMoved(Sender);
  FixVT(ListSessions);
  MainForm.RestoreListSetup(ListSessions);
  ListSessions.OnCompareNodes := MainForm.AnyGridCompareNodes;
  ListSessions.OnHeaderClick := MainForm.AnyGridHeaderClick;
  ListSessions.OnHeaderDraggedOut := MainForm.AnyGridHeaderDraggedOut;
  btnImportSettings.Caption := MainForm.actImportSettings.Caption;
  FLoaded := False;

  comboNetType.Clear;
  Params := TConnectionParameters.Create;
  for nt:=Low(nt) to High(nt) do
    comboNetType.Items.Add(Params.NetTypeName(nt, True));
  Params.Free;

  FSessions := TObjectList<TConnectionParameters>.Create;
  SetSessions;

  // Focus last session
  SelectNode(ListSessions, nil);
  LastSessions := Explode(DELIM, GetRegValue(REGNAME_LASTSESSIONS, ''));
  LastActiveSession := GetRegValue(REGNAME_LASTACTIVESESSION, '');
  if (LastActiveSession = '') and (LastSessions.Count > 0) then
    LastActiveSession := LastSessions[0];
  Node := ListSessions.GetFirst;
  while Assigned(Node) do begin
    PSess := ListSessions.GetNodeData(Node);
    if PSess.SessionName = LastActiveSession then
      SelectNode(ListSessions, Node);
    Node := ListSessions.GetNextSibling(Node);
  end;

  // Add own menu items to system menu
  hSysMenu := GetSystemMenu(Handle, False);
  AppendMenu(hSysMenu, MF_SEPARATOR, 0, #0);
  AppendMenu(hSysMenu, MF_STRING, MSG_UPDATECHECK, PChar(Mainform.actUpdateCheck.Caption));
  AppendMenu(hSysMenu, MF_STRING, MSG_PREFERENCES, PChar(Mainform.actPreferences.Caption));
  AppendMenu(hSysMenu, MF_STRING, MSG_ABOUT, PChar(Mainform.actAboutBox.Caption));
end;


procedure Tconnform.SetSessions;
var
  SessionNames: TStringList;
  i: Integer;
  Sess: TConnectionParameters;
begin
  // Initialize session tree
  SessionNames := TStringList.Create;
  MainReg.OpenKey(RegPath + REGKEY_SESSIONS, True);
  MainReg.GetKeyNames(SessionNames);
  for i:=0 to SessionNames.Count-1 do begin
    Sess := TConnectionParameters.ReadFromRegistry(SessionNames[i]);
    FSessions.Add(Sess);
  end;
  ListSessions.RootNodeCount := FSessions.Count;
end;


procedure Tconnform.FormDestroy(Sender: TObject);
begin
  // Save GUI stuff
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_SESSMNGR_LISTWIDTH, ListSessions.Width);
  MainReg.WriteInteger(REGNAME_SESSMNGR_WINWIDTH, Width);
  MainReg.WriteInteger(REGNAME_SESSMNGR_WINHEIGHT, Height);
  MainForm.SaveListSetup(ListSessions);
end;


procedure Tconnform.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Modifications? Ask if they should be saved.
  FinalizeModifications(CanClose);
end;


procedure Tconnform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Suspend calculating statistics as long as they're not visible
  TimerStatistics.Enabled := False;
  Action := caFree;
end;


procedure Tconnform.FormShow(Sender: TObject);
begin
  ListSessions.SetFocus;
  // Reactivate statistics
  TimerStatistics.Enabled := True;
  TimerStatistics.OnTimer(Sender);
  FLoaded := True;
end;


procedure Tconnform.btnOpenClick(Sender: TObject);
var
  Connection: TDBConnection;
begin
  // Connect to selected session
  Screen.Cursor := crHourglass;
  if Mainform.InitConnection(CurrentParams, True, Connection) then
    ModalResult := mrOK
  else begin
    TimerStatistics.OnTimer(Sender);
    ModalResult := mrNone;
  end;
  Screen.Cursor := crDefault;
end;


procedure Tconnform.SaveCurrentValues(Session: String; IsNew: Boolean);
var
  Sess: PConnectionParameters;
begin
  OpenRegistry(Session);
  MainReg.WriteString(REGNAME_HOST, editHost.Text);
  MainReg.WriteBool(REGNAME_WINDOWSAUTH, chkWindowsAuth.Checked);
  MainReg.WriteString(REGNAME_USER, editUsername.Text);
  MainReg.WriteString(REGNAME_PASSWORD, encrypt(editPassword.Text));
  MainReg.WriteBool(REGNAME_LOGINPROMPT, chkLoginPrompt.Checked);
  MainReg.WriteString(REGNAME_PORT, IntToStr(updownPort.Position));
  MainReg.WriteInteger(REGNAME_NETTYPE, comboNetType.ItemIndex);
  MainReg.WriteBool(REGNAME_COMPRESSED, chkCompressed.Checked);
  MainReg.WriteString(REGNAME_DATABASES, comboDatabases.Text);
  MainReg.WriteString(REGNAME_STARTUPSCRIPT, editStartupScript.Text);
  MainReg.WriteString(REGNAME_SSHHOST, editSSHHost.Text);
  MainReg.WriteInteger(REGNAME_SSHPORT, MakeInt(editSSHport.Text));
  MainReg.WriteString(REGNAME_SSHUSER, editSSHUser.Text);
  MainReg.WriteString(REGNAME_SSHPASSWORD, encrypt(editSSHPassword.Text));
  MainReg.WriteInteger(REGNAME_SSHTIMEOUT, updownSSHTimeout.Position);
  MainReg.WriteString(REGNAME_SSHKEY, editSSHPrivateKey.Text);
  MainReg.WriteInteger(REGNAME_SSHLOCALPORT, MakeInt(editSSHlocalport.Text));
  MainReg.WriteBool(REGNAME_SSL_ACTIVE, chkWantSSL.Checked);
  MainReg.WriteString(REGNAME_SSL_KEY, editSSLPrivateKey.Text);
  MainReg.WriteString(REGNAME_SSL_CERT, editSSLCertificate.Text);
  MainReg.WriteString(REGNAME_SSL_CA, editSSLCACertificate.Text);
  if IsNew then
    MainReg.WriteString(REGNAME_SESSIONCREATED, DateTimeToStr(Now));
  OpenRegistry;
  MainReg.WriteString(REGNAME_PLINKEXE, editSSHPlinkExe.Text);

  // Overtake edited values for in-memory parameter object
  Sess := ListSessions.GetNodeData(ListSessions.FocusedNode);
  Sess.Hostname := editHost.Text;
  Sess.Username := editUsername.Text;
  Sess.Password := editPassword.Text;
  Sess.LoginPrompt := chkLoginPrompt.Checked;
  Sess.WindowsAuth := chkWindowsAuth.Checked;
  Sess.Port := updownPort.Position;
  Sess.NetType := TNetType(comboNetType.ItemIndex);
  Sess.Compressed := chkCompressed.Checked;
  Sess.LocalTimeZone := chkLocalTimeZone.Checked;
  Sess.AllDatabasesStr := comboDatabases.Text;
  Sess.StartupScriptFilename := editStartupScript.Text;
  Sess.SSHHost := editSSHhost.Text;
  Sess.SSHPort := MakeInt(editSSHport.Text);
  Sess.SSHUser := editSSHUser.Text;
  Sess.SSHPassword := editSSHPassword.Text;
  Sess.SSHTimeout := updownSSHTimeout.Position;
  Sess.SSHPrivateKey := editSSHPrivateKey.Text;
  Sess.SSHLocalPort := MakeInt(editSSHlocalport.Text);
  Sess.WantSSL := chkWantSSL.Checked;
  Sess.SSLPrivateKey := editSSLPrivateKey.Text;
  Sess.SSLCertificate := editSSLCertificate.Text;
  Sess.SSLCACertificate := editSSLCACertificate.Text;

  FSessionModified := False;
  FSessionAdded := False;
  ListSessions.Invalidate;
  ValidateControls;
end;


procedure Tconnform.btnSaveClick(Sender: TObject);
begin
  // Save session settings
  SaveCurrentValues(SelectedSession, FSessionAdded);
end;


procedure Tconnform.btnSaveAsClick(Sender: TObject);
var
  newName: String;
  NameOK: Boolean;
  NewSess: TConnectionParameters;
  Node: PVirtualNode;
begin
  // Save session as ...
  newName := 'Enter new session name ...';
  NameOK := False;
  OpenRegistry;
  while not NameOK do begin
    if not InputQuery('Clone session ...', 'New session name:', newName) then
      Exit; // Cancelled
    NameOK := not MainReg.KeyExists(REGKEY_SESSIONS + newName);
    if not NameOK then
      ErrorDialog('Session name '''+newName+''' already in use.')
    else begin
      // Create the key and save its values
      OpenRegistry(newName);
      SaveCurrentValues(newName, True);
      NewSess := TConnectionParameters.ReadFromRegistry(newName);
      FSessions.Add(NewSess);
      Node := ListSessions.AddChild(nil, @NewSess);
      SelectNode(ListSessions, Node);
    end;
  end;
end;


procedure Tconnform.btnImportSettingsClick(Sender: TObject);
begin
  MainForm.actImportSettings.Execute;
  FSettingsImportWaitTime := 0;
  timerSettingsImport.Enabled := True;
end;


procedure Tconnform.timerSettingsImportTimer(Sender: TObject);
begin
  Inc(FSettingsImportWaitTime, timerSettingsImport.Interval);
  SetSessions;
  if FSessions.Count > 0 then
    timerSettingsImport.Enabled := False;
  if FSettingsImportWaitTime >= 10000 then begin
    timerSettingsImport.Enabled := False;
    MessageDialog('Imported sessions could not be detected. Restarting HeidiSQL may solve that.', mtWarning, [mbOK]);
  end;
end;


procedure Tconnform.btnNewClick(Sender: TObject);
var
  i: Integer;
  CanProceed: Boolean;
  NewSess: TConnectionParameters;
  Node: PVirtualNode;
begin
  // Create new session
  FinalizeModifications(CanProceed);
  if not CanProceed then
    Exit;

  i := 0;
  NewSess := TConnectionParameters.Create;
  NewSess.SessionName := 'Unnamed';
  while MainReg.KeyExists(RegPath + REGKEY_SESSIONS + NewSess.SessionName) do begin
    inc(i);
    NewSess.SessionName := 'Unnamed-' + IntToStr(i);
  end;
  FSessions.Add(NewSess);
  Node := ListSessions.AddChild(nil, @NewSess);
  // Select it
  SelectNode(ListSessions, Node);
  FSessionAdded := True;
  ValidateControls;
  ListSessions.EditNode(Node, 0);
end;


procedure Tconnform.btnDeleteClick(Sender: TObject);
var
  SessionKey: String;
  Sess: PConnectionParameters;
begin
  Sess := ListSessions.GetNodeData(ListSessions.FocusedNode);
  if MessageDialog('Delete session "' + Sess.SessionName + '" ?', mtConfirmation, [mbYes, mbCancel]) = mrYes then
  begin
    SessionKey := RegPath + REGKEY_SESSIONS + Sess.SessionName;
    if MainReg.KeyExists(SessionKey) then
      MainReg.DeleteKey(SessionKey);
    ListSessions.DeleteSelectedNodes;
    FSessions.Remove(Sess^);
    if (not Assigned(ListSessions.FocusedNode)) and (ListSessions.RootNodeCount > 0) then
      SelectNode(ListSessions, ListSessions.RootNodeCount-1)
    else
      SelectNode(ListSessions, nil);
  end;
end;


function Tconnform.SelectedSession: String;
var
  Sess: PConnectionParameters;
begin
  Sess := ListSessions.GetNodeData(ListSessions.FocusedNode);
  Result := Sess.SessionName;
end;


function Tconnform.CurrentParams: TConnectionParameters;
begin
  // Return non-stored parameters
  Result := TConnectionParameters.Create;
  Result.SessionName := SelectedSession;
  Result.SessionColor := FSessionColor;
  Result.NetType := TNetType(comboNetType.ItemIndex);
  Result.ServerVersion := FServerVersion;
  Result.Hostname := editHost.Text;
  Result.Username := editUsername.Text;
  Result.Password := editPassword.Text;
  Result.LoginPrompt := chkLoginPrompt.Checked;
  Result.WindowsAuth := chkWindowsAuth.Checked;
  if updownPort.Enabled then
    Result.Port := updownPort.Position
  else
    Result.Port := 0;
  Result.AllDatabasesStr := comboDatabases.Text;
  Result.SSHHost := editSSHHost.Text;
  Result.SSHPort := MakeInt(editSSHPort.Text);
  Result.SSHUser := editSSHuser.Text;
  Result.SSHPassword := editSSHpassword.Text;
  Result.SSHTimeout := updownSSHTimeout.Position;
  Result.SSHPrivateKey := editSSHPrivateKey.Text;
  Result.SSHLocalPort := MakeInt(editSSHlocalport.Text);
  Result.SSHPlinkExe := editSSHplinkexe.Text;
  Result.WantSSL := chkWantSSL.Checked;
  Result.SSLPrivateKey := editSSLPrivateKey.Text;
  Result.SSLCertificate := editSSLCertificate.Text;
  Result.SSLCACertificate := editSSLCACertificate.Text;
  Result.StartupScriptFilename := editStartupScript.Text;
  Result.Compressed := chkCompressed.Checked;
  Result.LocalTimeZone := chkLocalTimeZone.Checked;
end;


procedure Tconnform.ListSessionsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Sess: PConnectionParameters;
begin
  // A new session gets an additional plus symbol, editing gets a pencil
  if Column > 0 then
    ImageIndex := -1
  else case Kind of
    ikNormal, ikSelected: begin
      Sess := Sender.GetNodeData(Node);
      ImageIndex := Sess.ImageIndex;
    end;

    ikOverlay: if Node = Sender.FocusedNode then begin
      if FSessionAdded then
        ImageIndex := 163
      else if FSessionModified then
        ImageIndex := 162;
    end;

  end;
end;


procedure Tconnform.ListSessionsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TConnectionParameters);
end;


procedure Tconnform.ListSessionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Sess: PConnectionParameters;
begin
  // Display session name cell
  Sess := Sender.GetNodeData(Node);
  case Column of
    0: begin
      CellText := Sess.SessionName;
      if (FSessionModified or FSessionAdded) and (Node = Sender.FocusedNode) and (not Sender.IsEditing) then
        CellText := CellText + ' *';
    end;
    1: CellText := Sess.Hostname;
    2: CellText := Sess.Username;
    3: CellText := Sess.ServerVersion;
  end;
end;


procedure Tconnform.ListSessionsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Sess: PConnectionParameters;
begin
  Sess := Sender.GetNodeData(Node);
  Sess^ := FSessions[Node.Index];
end;


procedure Tconnform.ListSessionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  // Use our own text editor to rename a session
  EditLink := TInplaceEditorLink.Create(Sender as TVirtualStringTree);
end;


procedure Tconnform.ListSessionsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  SessionFocused: Boolean;
  Sess: PConnectionParameters;
begin
  // select one connection!
  Screen.Cursor := crHourglass;
  TimerStatistics.Enabled := False;
  SessionFocused := Assigned(Node);
  FLoaded := False;
  tabStart.TabVisible := not SessionFocused;
  tabSettings.TabVisible := SessionFocused;
  tabSSHtunnel.TabVisible := SessionFocused;
  tabSSLoptions.TabVisible := SessionFocused;
  tabStatistics.TabVisible := SessionFocused;

  if not SessionFocused then begin
    PageControlDetails.ActivePage := tabStart;
    if FSessions.Count = 0 then
      lblHelp.Caption := 'New here? In order to connect to a server, you have to create a so called '+
        '"session" at first. Just click the "New" button on the bottom left to create your first session.'+CRLF+CRLF+
        'Give it a friendly name (e.g. "Local DB server") so you''ll recall it the next time you start '+APPNAME+'.'
    else
      lblHelp.Caption := 'Please click a session on the left list to edit parameters, doubleclick to open it.';
  end else begin
    PageControlDetails.ActivePage := tabSettings;
    Sess := Sender.GetNodeData(Node);

    comboNetType.ItemIndex := Integer(Sess.NetType);
    editHost.Text := Sess.Hostname;
    editUsername.Text := Sess.Username;
    editPassword.Text := Sess.Password;
    chkLoginPrompt.Checked := Sess.LoginPrompt;
    chkWindowsAuth.Checked := Sess.WindowsAuth;
    updownPort.Position := Sess.Port;
    chkCompressed.Checked := Sess.Compressed;
    chkLocalTimeZone.Checked := Sess.LocalTimeZone;
    comboDatabases.Text := Sess.AllDatabasesStr;
    editStartupScript.Text := Sess.StartupScriptFilename;
    editSSHPlinkExe.Text := Sess.SSHPlinkExe;
    editSSHHost.Text := Sess.SSHHost;
    editSSHport.Text := IntToStr(Sess.SSHPort);
    editSSHUser.Text := Sess.SSHUser;
    editSSHPassword.Text := Sess.SSHPassword;
    updownSSHTimeout.Position := Sess.SSHTimeout;
    editSSHPrivateKey.Text := Sess.SSHPrivateKey;
    editSSHlocalport.Text := IntToStr(Sess.SSHLocalPort);
    chkWantSSL.Checked := Sess.WantSSL;
    editSSLPrivateKey.Text := Sess.SSLPrivateKey;
    editSSLCertificate.Text := Sess.SSLCertificate;
    editSSLCACertificate.Text := Sess.SSLCACertificate;
    FServerVersion := Sess.ServerVersion;
    FSessionColor := Sess.SessionColor;
  end;

  FLoaded := True;
  FSessionModified := False;
  FSessionAdded := False;
  ListSessions.Repaint;
  ValidateControls;
  TimerStatistics.Enabled := True;
  TimerStatistics.OnTimer(Sender);

  Screen.Cursor := crDefault;
end;


procedure Tconnform.TimerStatisticsTimer(Sender: TObject);
var
  LastConnect, Created, DummyDate: TDateTime;
  Connects, Refused: Integer;
begin
  // Continuously update statistics labels
  lblLastConnectRight.Caption := 'unknown or never';
  lblLastConnectRight.Hint := '';
  lblLastConnectRight.Enabled := False;
  lblCreatedRight.Caption := 'unknown';
  lblCreatedRight.Hint := '';
  lblCreatedRight.Enabled := False;
  lblCounterRight.Caption := 'not available';
  lblCounterRight.Enabled := False;

  if (not Assigned(ListSessions.FocusedNode))
    or (not MainReg.KeyExists(RegPath + REGKEY_SESSIONS + SelectedSession)) then
    Exit;

  DummyDate := StrToDateTime('2000-01-01');
  LastConnect := StrToDateTimeDef(GetRegValue(REGNAME_LASTCONNECT, '', SelectedSession), DummyDate);
  if LastConnect <> DummyDate then begin
    lblLastConnectRight.Hint := DateTimeToStr(LastConnect);
    lblLastConnectRight.Caption := DateBackFriendlyCaption(LastConnect);
    lblLastConnectRight.Enabled := True;
  end;
  Created := StrToDateTimeDef(GetRegValue(REGNAME_SESSIONCREATED, '', SelectedSession), DummyDate);
  if Created <> DummyDate then begin
    lblCreatedRight.Hint := DateTimeToStr(Created);
    lblCreatedRight.Caption := DateBackFriendlyCaption(Created);
    lblCreatedRight.Enabled := True;
  end;
  Connects := GetRegValue(REGNAME_CONNECTCOUNT, 0, SelectedSession);
  Refused := GetRegValue(REGNAME_REFUSEDCOUNT, 0, SelectedSession);
  lblCounterRight.Enabled := Connects + Refused > 0;
  if Connects > 0 then begin
    lblCounterRight.Caption := 'Successful connects: '+IntToStr(Connects);
    if Refused > 0 then
      lblCounterRight.Caption := lblCounterRight.Caption + ', unsuccessful: '+IntToStr(Refused);
  end else if Refused > 0 then
    lblCounterRight.Caption := 'Unsuccessful connects: '+IntToStr(Refused);
  Invalidate;
end;


procedure Tconnform.ListSessionsFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  if NewNode <> OldNode then
    FinalizeModifications(Allowed)
  else
    Allowed := False;
end;


procedure Tconnform.ListSessionsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  SessionKey: String;
  Connection: TDBConnection;
  Sess: PConnectionParameters;
  Names: TStringList;
  idx: Integer;
begin
  // Rename session
  Sess := Sender.GetNodeData(Node);
  OpenRegistry;
  Names := TStringList.Create;
  MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, true);
  MainReg.GetKeyNames(Names);
  idx := Names.IndexOf(Sess.SessionName);
  if idx > -1 then
    Names.Delete(idx);
  if Names.IndexOf(NewText) > -1 then begin
    ErrorDialog('Session "'+NewText+'" already exists!');
    NewText := Sess.SessionName;
  end else begin
    SessionKey := RegPath + REGKEY_SESSIONS + Sess.SessionName;
    if MainReg.KeyExists(SessionKey) then
      MainReg.MoveKey(SessionKey, RegPath + REGKEY_SESSIONS + NewText, true);
    // Also fix internal session names in main form, which gets used to store e.g. "lastuseddb" later
    for Connection in MainForm.Connections do begin
      if Connection.Parameters.SessionName = Sess.SessionName then
        Connection.Parameters.SessionName := NewText;
    end;
    MainForm.SetWindowCaption;
    Sess.SessionName := NewText;
  end;
end;


procedure Tconnform.editHostChange(Sender: TObject);
begin
  editSSHhost.TextHint := TEdit(Sender).Text;
  Modification(Sender);
end;


procedure Tconnform.chkLoginPromptClick(Sender: TObject);
var
  Checked: Boolean;
begin
  // Login prompt and SQL Server integrated Windows Auth are mutually exclusive
  Checked := TCheckBox(Sender).Checked;
  if Checked and (Sender = chkWindowsAuth) then
    chkLoginPrompt.Checked := False;
  if Checked and (Sender = chkLoginPrompt) then
    chkWindowsAuth.Checked := False;
  Modification(Sender);
end;


procedure Tconnform.comboDatabasesDropDown(Sender: TObject);
var
  Connection: TDBConnection;
  Params: TConnectionParameters;
begin
  // Try to connect and lookup database names
  Params := CurrentParams;
  Connection := Params.CreateConnection(Self);
  Connection.Parameters.AllDatabasesStr := '';
  Connection.LogPrefix := SelectedSession;
  Connection.OnLog := Mainform.LogSQL;
  comboDatabases.Items.Clear;
  Screen.Cursor := crHourglass;
  try
    Connection.Active := True;
    comboDatabases.Items := Connection.AllDatabases;
  except
    // Silence connection errors here - should be sufficient to log them
  end;
  FreeAndNil(Connection);
  Screen.Cursor := crDefault;
end;


procedure Tconnform.comboNetTypeChange(Sender: TObject);
var
  Params: TConnectionParameters;
begin
  // Autoset default port number as long as that was not modified by user
  if (not editPort.Modified) and (FLoaded) then begin
    Params := CurrentParams;
    case Params.NetTypeGroup of
      ngMySQL:
        updownPort.Position := DEFAULT_PORT;
      ngMSSQL:
        updownPort.Position := 1433;
    end;
    FreeAndNil(Params);
  end;
  Modification(Sender);
end;


procedure Tconnform.Modification(Sender: TObject);
var
  PasswordModified: Boolean;
  Sess: PConnectionParameters;
begin
  // Some modification -
  if FLoaded then begin
    Sess := ListSessions.GetNodeData(ListSessions.FocusedNode);
    FSessionModified := (Sess.Hostname <> editHost.Text)
      or (Sess.Username <> editUsername.Text)
      or (Sess.LoginPrompt <> chkLoginPrompt.Checked)
      or (Sess.WindowsAuth <> chkWindowsAuth.Checked)
      or (Sess.Port <> updownPort.Position)
      or (Sess.Compressed <> chkCompressed.Checked)
      or (Sess.LocalTimeZone <> chkLocalTimeZone.Checked)
      or (Sess.NetType <> TNetType(comboNetType.ItemIndex))
      or (Sess.StartupScriptFilename <> editStartupScript.Text)
      or (Sess.AllDatabasesStr <> comboDatabases.Text)
      or (Sess.SSHHost <> editSSHHost.Text)
      or (IntToStr(Sess.SSHPort) <> editSSHPort.Text)
      or (Sess.SSHPlinkExe <> editSSHPlinkExe.Text)
      or (IntToStr(Sess.SSHLocalPort) <> editSSHlocalport.Text)
      or (Sess.SSHUser <> editSSHUser.Text)
      or (Sess.SSHPassword <> editSSHPassword.Text)
      or (Sess.SSHTimeout <> updownSSHTimeout.Position)
      or (Sess.SSHPrivateKey <> editSSHPrivateKey.Text)
      or (Sess.WantSSL <> chkWantSSL.Checked)
      or (Sess.SSLPrivateKey <> editSSLPrivateKey.Text)
      or (Sess.SSLCertificate <> editSSLCertificate.Text)
      or (Sess.SSLCACertificate <> editSSLCACertificate.Text);
    PasswordModified := Sess.Password <> editPassword.Text;
    FOnlyPasswordModified := PasswordModified and (not FSessionModified);
    FSessionModified := FSessionModified or PasswordModified;

    ListSessions.Repaint;
    ValidateControls;
  end;
end;


procedure Tconnform.FinalizeModifications(var CanProceed: Boolean);
begin
  if (FSessionModified and (not FOnlyPasswordModified)) or FSessionAdded then begin
    case MessageDialog('Save modifications?', 'Settings for "'+SelectedSession+'" were changed.', mtConfirmation, [mbYes, mbNo, mbCancel]) of
      mrYes: begin
          btnSave.OnClick(Self);
          CanProceed := True;
        end;
      mrNo: begin
          CanProceed := True;
        end;
      mrCancel: CanProceed := False;
    end;
  end else
    CanProceed := True;
end;


procedure Tconnform.ValidateControls;
var
  SessionFocused: Boolean;
  Params: TConnectionParameters;
begin
  SessionFocused := Assigned(ListSessions.FocusedNode);

  btnOpen.Enabled := SessionFocused;
  btnNew.Enabled := not FSessionAdded;
  btnSave.Enabled := FSessionModified or FSessionAdded;
  btnDelete.Enabled := SessionFocused;
  btnOpen.Enabled := SessionFocused;

  if SessionFocused then begin
    // Validate session GUI stuff
    Params := CurrentParams;
    if Params.NetType = ntMySQL_NamedPipe then
      lblHost.Caption := 'Socket name:'
    else
      lblHost.Caption := 'Hostname / IP:';
    chkWindowsAuth.Enabled := Params.NetTypeGroup = ngMSSQL;
    lblUsername.Enabled := ((not chkLoginPrompt.Checked) or (not chkLoginPrompt.Enabled))
      and ((not chkWindowsAuth.Checked) or (not chkWindowsAuth.Enabled));
    editUsername.Enabled := lblUsername.Enabled;
    lblPassword.Enabled := lblUsername.Enabled;
    editPassword.Enabled := lblUsername.Enabled;
    lblPort.Enabled := Params.NetType in [ntMySQL_TCPIP, ntMySQL_SSHtunnel, ntMSSQL_TCPIP];
    if (Params.NetType = ntMSSQL_TCPIP) and (Pos('\', editHost.Text) > 0) then
      lblPort.Enabled := False; // Named instance without port
    editPort.Enabled := lblPort.Enabled;
    updownPort.Enabled := lblPort.Enabled;
    tabSSLoptions.TabVisible := Params.NetType = ntMySQL_TCPIP;
    lblSSLPrivateKey.Enabled := Params.WantSSL;
    editSSLPrivateKey.Enabled := Params.WantSSL;
    lblSSLCACertificate.Enabled := Params.WantSSL;
    editSSLCACertificate.Enabled := Params.WantSSL;
    lblSSLCertificate.Enabled := Params.WantSSL;
    editSSLCertificate.Enabled := Params.WantSSL;
    tabSSHtunnel.TabVisible := Params.NetType = ntMySQL_SSHtunnel;
    FreeAndNil(Params);
  end;
end;


procedure Tconnform.splitterMainMoved(Sender: TObject);
var
  ButtonWidth: Integer;
begin
  // Splitter resized - adjust width of buttons
  ButtonWidth := Round((ListSessions.Width - 2 * ListSessions.Margins.Left) / 3);
  btnNew.Width := ButtonWidth;
  btnSave.Width := ButtonWidth;
  btnDelete.Width := ButtonWidth;
  btnNew.Left := ListSessions.Left;
  btnSave.Left := btnNew.Left + btnNew.Width + ListSessions.Margins.Left;
  btnDelete.Left := btnSave.Left + btnSave.Width + ListSessions.Margins.Left;
end;


procedure Tconnform.PickFile(Sender: TObject);
var
  Selector: TOpenDialog;
  Edit: TButtonedEdit;
  i: Integer;
  Control: TControl;
begin
  // Select startup SQL file, SSL file or whatever button clicked
  Edit := Sender as TButtonedEdit;
  Selector := TOpenDialog.Create(Self);
  Selector.FileName := editStartupScript.Text;
  if Edit = editStartupScript then
    Selector.Filter := 'SQL-files (*.sql)|*.sql|All files (*.*)|*.*'
  else if Edit = editSSHPlinkExe then
    Selector.Filter := 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
  else if Edit = editSSHPrivateKey then
    Selector.Filter := 'PuTTY private key (*.ppk)|*.ppk|All files (*.*)|*.*'
  else
    Selector.Filter := 'Privacy Enhanced Mail certificates (*.pem)|*.pem|Certificates (*.crt)|*.crt|All files (*.*)|*.*';
  // Find relevant label and set open dialog's title
  for i:=0 to Edit.Parent.ControlCount - 1 do begin
    Control := Edit.Parent.Controls[i];
    if (Control is TLabel) and ((Control as TLabel).FocusControl = Edit) then begin
      Selector.Title := 'Select ' + (Control as TLabel).Caption;
      break;
    end;
  end;

  if Selector.Execute then begin
    Edit.Text := Selector.FileName;
    Modification(Selector);
  end;
  Selector.Free;
end;


procedure Tconnform.editSSHPlinkExeChange(Sender: TObject);
begin
  if not FileExists(editSSHPlinkExe.Text) then
    editSSHPlinkExe.Font.Color := clRed
  else
    editSSHPlinkExe.Font.Color := clWindowText;
  Modification(Sender);
end;


procedure Tconnform.lblDownloadPlinkClick(Sender: TObject);
begin
  ShellExec(TLabel(Sender).Hint);
end;


end.
