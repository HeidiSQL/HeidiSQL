unit connections;


// -------------------------------------
// Connections (start-window)
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, WideStrings,
  TntStdCtrls, VirtualTrees, Menus;

type
  Tconnform = class(TForm)
    lblSession: TLabel;
    btnCancel: TButton;
    btnOpen: TButton;
    btnSave: TButton;
    ListSessions: TVirtualStringTree;
    btnNew: TButton;
    btnDelete: TButton;
    grpDetails: TGroupBox;
    lblHost: TLabel;
    lblUsername: TLabel;
    lblPassword: TLabel;
    lblPort: TLabel;
    lblOnlyDBs: TLabel;
    editHost: TEdit;
    editUsername: TEdit;
    editPassword: TEdit;
    editPort: TEdit;
    chkCompressed: TCheckBox;
    radioTypeTCPIP: TRadioButton;
    radioTypeNamedPipe: TRadioButton;
    memoDatabases: TTntMemo;
    updownPort: TUpDown;
    lblLastConnectLeft: TLabel;
    lblLastConnectRight: TLabel;
    lblCreatedLeft: TLabel;
    lblCreatedRight: TLabel;
    lblNetworkType: TLabel;
    popupSessions: TPopupMenu;
    Save1: TMenuItem;
    Delete1: TMenuItem;
    Saveas1: TMenuItem;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure radioNetTypeClick(Sender: TObject);
    procedure ListSessionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure ListSessionsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ListSessionsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ListSessionsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure ListSessionsFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FLoaded: Boolean;
    FSessionNames: TStringlist;
    FSessionModified, FSessionAdded: Boolean;
    FOrgNetType: Byte;
    FOrgHost, FOrgUser, FOrgPassword, FOrgDatabases: WideString;
    FOrgCompressed: Boolean;
    FOrgPort: Integer;
    function SelectedSession: String;
    procedure RefreshSessionList(RefetchRegistry: Boolean);
    procedure FinalizeModifications(var CanProceed: Boolean);
    procedure SaveCurrentValues(Session: String; IsNew: Boolean);
  public
    { Public declarations }
  end;


implementation
 uses Main, helpers, MysqlQueryThread;

{$I const.inc}

{$R *.DFM}


procedure Tconnform.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle OR WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
end;


procedure Tconnform.FormCreate(Sender: TObject);
var
  LastSession: String;
begin
  // Fix GUI stuff
  InheritFont(Font);
  SetWindowSizeGrip(Handle, True);
  FixVT(ListSessions);
  FLoaded := False;
  FSessionNames := TStringList.Create;
  RefreshSessionList(True);
  // Focus last session
  LastSession := GetRegValue(REGNAME_LASTSESSION, '');
  btnSave.Enabled := False;
  btnDelete.Enabled := False;
  SelectNode(ListSessions, FSessionNames.IndexOf(LastSession));
end;


procedure Tconnform.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Modifications? Ask if they should be saved.
  FinalizeModifications(CanClose);
end;


procedure Tconnform.FormShow(Sender: TObject);
begin
  ListSessions.SetFocus;
  FLoaded := True;
end;


procedure Tconnform.btnOpenClick(Sender: TObject);
var
  ConType: Byte;
  CanProceed: Boolean;
begin
  // Connect to selected session
  FinalizeModifications(CanProceed);
  if not CanProceed then
    Exit;

  Screen.Cursor := crHourglass;
  // Save last connection name to registry
  OpenRegistry;
  MainReg.WriteString(REGNAME_LASTSESSION, SelectedSession);

  if radioTypeTCPIP.Checked then ConType := NETTYPE_TCPIP
  else ConType := NETTYPE_NAMEDPIPE;

  if Mainform.InitConnection(
    ConType,
    editHost.Text,
    editPort.Text,
    editUsername.Text,
    editPassword.Text,
    memoDatabases.Text,
    IntToStr(Integer(chkCompressed.Checked))) then begin
    ModalResult := mrOK;
    Mainform.SessionName := SelectedSession;
  end else begin
    ModalResult := mrNone;
  end;

  Screen.Cursor := crDefault;
end;


procedure Tconnform.SaveCurrentValues(Session: String; IsNew: Boolean);
begin
  OpenRegistry(Session);
  MainReg.WriteString(REGNAME_HOST, editHost.Text);
  MainReg.WriteString(REGNAME_USER, editUsername.Text);
  MainReg.WriteString(REGNAME_PASSWORD, encrypt(editPassword.Text));
  MainReg.WriteString(REGNAME_PORT, editPort.Text);
  if radioTypeTCPIP.Checked then
    MainReg.WriteInteger(REGNAME_NETTYPE, NETTYPE_TCPIP)
  else
    MainReg.WriteInteger(REGNAME_NETTYPE, NETTYPE_NAMEDPIPE);
  MainReg.WriteBool(REGNAME_COMPRESSED, chkCompressed.Checked);
  MainReg.WriteString(REGNAME_ONLYDBS, Utf8Encode(memoDatabases.Text));
  if IsNew then
    MainReg.WriteString(REGNAME_SESSIONCREATED, DateTimeToStr(Now));
  FSessionModified := False;
  FSessionAdded := False;
  RefreshSessionList(True);
  ListSessions.FocusedNode := nil;
  SelectNode(ListSessions, FSessionNames.IndexOf(Session));
  btnNew.Enabled := True;
  btnSave.Enabled := False;
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
      MessageDlg('Session name '''+newName+''' already in use.', mtError, [mbOK], 0)
    else begin
      // Create the key and save its values
      OpenRegistry(newName);
      SaveCurrentValues(newName, True);
    end;
  end;
end;


procedure Tconnform.btnNewClick(Sender: TObject);
var
  i, NewIdx: Integer;
  NewName: String;
begin
  // Create new session
  i := 0;
  NewName := 'Unnamed';
  while MainReg.KeyExists(REGPATH + REGKEY_SESSIONS + NewName) do begin
    inc(i);
    NewName := 'Unnamed-' + IntToStr(i);
  end;
  FSessionNames.Add(NewName);
  FSessionNames.Sort;
  NewIdx := FSessionNames.IndexOf(NewName);
  // Select it
  RefreshSessionList(False);
  SelectNode(ListSessions, NewIdx);
  FSessionAdded := True;
  ListSessions.EditNode(ListSessions.FocusedNode, ListSessions.FocusedColumn);
end;


procedure Tconnform.btnDeleteClick(Sender: TObject);
var
  SessionKey: String;
begin
  if MessageDlg('Delete session "' + SelectedSession + '" ?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    SessionKey := REGPATH + REGKEY_SESSIONS + SelectedSession;
    if MainReg.KeyExists(SessionKey) then
      MainReg.DeleteKey(SessionKey);
    FSessionNames.Delete(FSessionNames.IndexOf(SelectedSession));
    RefreshSessionList(False);
    if (not Assigned(ListSessions.FocusedNode)) and (ListSessions.RootNodeCount > 0) then
      SelectNode(ListSessions, ListSessions.RootNodeCount-1);
  end;
end;


function Tconnform.SelectedSession: String;
begin
  Result := FSessionNames[ListSessions.FocusedNode.Index];
end;


procedure Tconnform.RefreshSessionList(RefetchRegistry: Boolean);
begin
  // Refresh list of session names
  if RefetchRegistry then begin
    MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, True);
    MainReg.GetKeyNames(FSessionNames);
  end;
  ListSessions.RootNodeCount := FSessionNames.Count;
  ListSessions.Repaint;
end;


procedure Tconnform.ListSessionsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  // A new session gets an additional plus symbol, editing gets a pencil
  ImageIndex := 36;
  if Node = Sender.FocusedNode then begin
    if FSessionAdded then ImageIndex := 72
    else if FSessionModified then ImageIndex := 135;
  end
end;


procedure Tconnform.ListSessionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  // Display session name cell
  CellText := FSessionNames[Node.Index];
  if (FSessionModified or FSessionAdded) and (Node = Sender.FocusedNode) and (not Sender.IsEditing) then
    CellText := CellText + ' *';
end;


procedure Tconnform.ListSessionsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  SessionFocused, SessionExists: Boolean;
  LastConnect, Created, DummyDate: TDateTime;
begin
  // select one connection!
  Screen.Cursor := crHourglass;
  OpenRegistry;
  SessionFocused := Assigned(Node);
  SessionExists := SessionFocused;
  if SessionFocused then begin
    SessionExists := MainReg.KeyExists(REGPATH + REGKEY_SESSIONS + SelectedSession);
    lblLastConnectRight.Caption := 'unknown or never';
    lblLastConnectRight.Hint := '';
    lblLastConnectRight.Enabled := False;
    lblCreatedRight.Caption := 'unknown';
    lblCreatedRight.Hint := lblLastConnectRight.Hint;
    lblCreatedRight.Enabled := lblLastConnectRight.Enabled;

    if SessionExists then begin
      OpenRegistry(SelectedSession);
      FOrgNetType := GetRegValue(REGNAME_NETTYPE, DEFAULT_NETTYPE, SelectedSession);
      FOrgHost := GetRegValue(REGNAME_HOST, '', SelectedSession);
      FOrgUser := GetRegValue(REGNAME_USER, '', SelectedSession);
      FOrgPassword := decrypt(GetRegValue(REGNAME_PASSWORD, '', SelectedSession));
      FOrgPort := StrToIntDef(GetRegValue(REGNAME_PORT, '', SelectedSession), DEFAULT_PORT);
      FOrgCompressed := GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, SelectedSession);
      FOrgDatabases := Utf8Decode(GetRegValue(REGNAME_ONLYDBS, '', SelectedSession));
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

    end else begin
      // Editing a new session, not saved yet
      FOrgNetType := NETTYPE_TCPIP;
      FOrgHost := DEFAULT_HOST;
      FOrgUser := DEFAULT_USER;
      FOrgPassword := '';
      FOrgPort := DEFAULT_PORT;
      FOrgCompressed := DEFAULT_COMPRESSED;
      FOrgDatabases := '';
    end;

    case FOrgNetType of
      NETTYPE_NAMEDPIPE: radioTypeNamedPipe.Checked := True;
      else radioTypeTCPIP.Checked := True;
    end;
    radioNetTypeClick(Sender);
    editHost.Text := FOrgHost;
    editUsername.Text := FOrgUser;
    editPassword.Text := FOrgPassword;
    updownPort.Position := FOrgPort;
    chkCompressed.Checked := FOrgCompressed;
    memoDatabases.Text := FOrgDatabases;
  end;

  grpDetails.Visible := SessionFocused;
  btnOpen.Enabled := SessionFocused;
  btnNew.Enabled := SessionExists;
  btnSave.Enabled := not SessionExists;
  btnDelete.Enabled := SessionFocused;
  FSessionModified := False;
  FSessionAdded := False;
  ListSessions.Repaint;

  Screen.Cursor := crDefault;
end;


procedure Tconnform.ListSessionsFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  FinalizeModifications(Allowed);
end;


procedure Tconnform.ListSessionsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  SessionKey: String;
begin
  // Rename session
  OpenRegistry;
  if MainReg.KeyExists(REGKEY_SESSIONS + NewText) then begin
    MessageDLG('Session "'+NewText+'" already exists!', mtError, [mbCancel], 0);
    NewText := SelectedSession;
  end else begin
    SessionKey := REGPATH + REGKEY_SESSIONS + SelectedSession;
    if MainReg.KeyExists(SessionKey) then
      MainReg.MoveKey(SessionKey, REGPATH + REGKEY_SESSIONS + NewText, true);
    FSessionNames[FSessionNames.IndexOf(SelectedSession)] := NewText;
    RefreshSessionList(False);
  end;
end;


procedure Tconnform.Modification(Sender: TObject);
var
  NetType: Byte;
begin
  // Some modification -
  if FLoaded then begin
    if radioTypeTCPIP.Checked then NetType := NETTYPE_TCPIP
    else NetType := NETTYPE_NAMEDPIPE;
    FSessionModified := (FOrgHost <> editHost.Text) or (FOrgUser <> editUsername.Text)
      or (FOrgPassword <> editPassword.Text) or (FOrgPort <> updownPort.Position)
      or (FOrgCompressed <> chkCompressed.Checked) or (FOrgDatabases <> memoDatabases.Text)
      or (FOrgNetType <> NetType);
    ListSessions.Repaint;
    btnSave.Enabled := FSessionModified or FSessionAdded;
  end;
end;


procedure Tconnform.radioNetTypeClick(Sender: TObject);
begin
  // Toggle between TCP/IP and named pipes mode
  if radioTypeTCPIP.Checked then
    lblHost.Caption := '&Hostname / IP:'
  else
    lblHost.Caption := 'Socket name:';
  editPort.Enabled := radioTypeTCPIP.Checked;
  lblPort.Enabled := editPort.Enabled;
  updownPort.Enabled := editPort.Enabled;
  Modification(Sender);
end;


procedure Tconnform.FinalizeModifications(var CanProceed: Boolean);
begin
  if FSessionModified or FSessionAdded then begin
    case MessageDlg('Save settings for "'+SelectedSession+'"?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: begin
          btnSave.OnClick(Self);
          CanProceed := True;
        end;
      mrNo: begin
          RefreshSessionList(True);
          CanProceed := True;
        end;
      mrCancel: CanProceed := False;
    end;
  end else
    CanProceed := True;
end;


end.
