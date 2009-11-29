unit connections;


// -------------------------------------
// Connections (start-window)
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, WideStrings,
  TntStdCtrls, VirtualTrees, Menus, mysql_api;

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
    editHost: TEdit;
    editUsername: TEdit;
    editPassword: TEdit;
    editPort: TEdit;
    chkCompressed: TCheckBox;
    radioTypeTCPIP: TRadioButton;
    radioTypeNamedPipe: TRadioButton;
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
    TimerStatistics: TTimer;
    lblCounterLeft: TLabel;
    lblCounterRight: TLabel;
    lblHelp: TLabel;
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
    procedure FormDestroy(Sender: TObject);
    procedure TimerStatisticsTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListSessionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      out EditLink: IVTEditLink);
    procedure updownPortChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure editPortChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FLoaded: Boolean;
    FSessionNames: TStringlist;
    FSessionModified, FSessionAdded: Boolean;
    FOrgNetType: Byte;
    FOrgHost, FOrgUser, FOrgPassword: WideString;
    FOrgCompressed: Boolean;
    FOrgPort: Integer;
    FWidthListSessions: Byte; // Percentage values
    function SelectedSession: String;
    procedure RefreshSessionList(RefetchRegistry: Boolean);
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


procedure Tconnform.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle OR WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
end;


procedure Tconnform.FormCreate(Sender: TObject);
var
  LastSession: String;
  hSysMenu: THandle;
begin
  // Fix GUI stuff
  InheritFont(Font);
  SetWindowSizeGrip(Handle, True);
  FWidthListSessions := Round(100 / ClientWidth * ListSessions.Width);
  Width := GetRegValue(REGNAME_SESSMNGR_WINWIDTH, Width);
  Height := GetRegValue(REGNAME_SESSMNGR_WINHEIGHT, Height);
  FixVT(ListSessions);
  ListSessions.OnGetHint := Mainform.vstGetHint;
  FLoaded := False;
  FSessionNames := TStringList.Create;
  RefreshSessionList(True);
  // Focus last session
  LastSession := GetRegValue(REGNAME_LASTSESSION, '');
  SelectNode(ListSessions, FSessionNames.IndexOf(LastSession));
  ValidateControls;
  // Add own menu items to system menu
  hSysMenu := GetSystemMenu(Handle, False);
  AppendMenu(hSysMenu, MF_SEPARATOR, 0, #0);
  AppendMenu(hSysMenu, MF_STRING, MSG_UPDATECHECK, PAnsiChar(Mainform.actUpdateCheck.Caption));
  AppendMenu(hSysMenu, MF_STRING, MSG_ABOUT, PAnsiChar(Mainform.actAboutBox.Caption));
end;


procedure Tconnform.FormDestroy(Sender: TObject);
begin
  // Save GUI stuff
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_SESSMNGR_WINWIDTH, Width);
  MainReg.WriteInteger(REGNAME_SESSMNGR_WINHEIGHT, Height);
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
  ConType: Byte;
  Host, Socket: String;
begin
  // Connect to selected session
  Screen.Cursor := crHourglass;
  if radioTypeTCPIP.Checked then ConType := NETTYPE_TCPIP
  else ConType := NETTYPE_NAMEDPIPE;
  Host := editHost.Text;
  if ConType = NETTYPE_TCPIP then begin
    Socket := '';
    Host := editHost.Text;
  end else begin
    Socket := editHost.Text;
    Host := '.';
  end;
  if Mainform.InitConnection(
    Host,
    Socket,
    editPort.Text,
    editUsername.Text,
    editPassword.Text,
    IntToStr(Integer(chkCompressed.Checked)),
    SelectedSession) then begin
    ModalResult := mrOK;
  end else begin
    TimerStatistics.OnTimer(Sender);
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
  if IsNew then
    MainReg.WriteString(REGNAME_SESSIONCREATED, DateTimeToStr(Now));
  FSessionModified := False;
  FSessionAdded := False;
  RefreshSessionList(True);
  ListSessions.FocusedNode := nil;
  SelectNode(ListSessions, FSessionNames.IndexOf(Session));
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
  ValidateControls;
  ListSessions.EditNode(ListSessions.FocusedNode, ListSessions.FocusedColumn);
end;


procedure Tconnform.btnDeleteClick(Sender: TObject);
var
  SessionKey: String;
  Node: PVirtualNode;
begin
  if MessageDlg('Delete session "' + SelectedSession + '" ?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    SessionKey := REGPATH + REGKEY_SESSIONS + SelectedSession;
    if MainReg.KeyExists(SessionKey) then
      MainReg.DeleteKey(SessionKey);
    FSessionNames.Delete(FSessionNames.IndexOf(SelectedSession));
    RefreshSessionList(False);
    if (not Assigned(ListSessions.FocusedNode)) and (ListSessions.RootNodeCount > 0) then
      SelectNode(ListSessions, ListSessions.RootNodeCount-1)
    else begin
      Node := ListSessions.FocusedNode;
      ListSessions.FocusedNode := nil;
      ListSessions.FocusedNode := Node;
    end;
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


procedure Tconnform.ListSessionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  // Use our own text editor to rename a session
  EditLink := TInplaceEditorLink.Create(Sender as TVirtualStringTree);
end;


procedure Tconnform.ListSessionsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  SessionFocused, SessionExists: Boolean;
begin
  // select one connection!
  Screen.Cursor := crHourglass;
  TimerStatistics.Enabled := False;
  OpenRegistry;
  SessionFocused := Assigned(Node);
  if SessionFocused then begin
    SessionExists := MainReg.KeyExists(REGPATH + REGKEY_SESSIONS + SelectedSession);
    if SessionExists then begin
      OpenRegistry(SelectedSession);
      FOrgNetType := GetRegValue(REGNAME_NETTYPE, DEFAULT_NETTYPE, SelectedSession);
      FOrgHost := GetRegValue(REGNAME_HOST, '', SelectedSession);
      FOrgUser := GetRegValue(REGNAME_USER, '', SelectedSession);
      FOrgPassword := decrypt(GetRegValue(REGNAME_PASSWORD, '', SelectedSession));
      FOrgPort := StrToIntDef(GetRegValue(REGNAME_PORT, '', SelectedSession), DEFAULT_PORT);
      FOrgCompressed := GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, SelectedSession);
    end else begin
      // Editing a new session, not saved yet
      FOrgNetType := NETTYPE_TCPIP;
      FOrgHost := DEFAULT_HOST;
      FOrgUser := DEFAULT_USER;
      FOrgPassword := '';
      FOrgPort := DEFAULT_PORT;
      FOrgCompressed := DEFAULT_COMPRESSED;
    end;

    FLoaded := False;
    case FOrgNetType of
      NETTYPE_NAMEDPIPE: radioTypeNamedPipe.Checked := True;
      else radioTypeTCPIP.Checked := True;
    end;
    radioNetTypeClick(Sender);
    editHost.Text := FOrgHost;
    editUsername.Text := FOrgUser;
    editPassword.Text := FOrgPassword;
    editPort.Text := IntToStr(FOrgPort);
    chkCompressed.Checked := FOrgCompressed;
    FLoaded := True;
  end;

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
    or (not MainReg.KeyExists(REGPATH + REGKEY_SESSIONS + SelectedSession)) then
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
    // Also fix internal session name in main form, which gets used to store e.g. "lastuseddb" later
    if Mainform.SessionName = SelectedSession then begin
      Mainform.SessionName := NewText;
      Mainform.SetWindowCaption;
    end;
    FSessionNames[FSessionNames.IndexOf(SelectedSession)] := NewText;
    RefreshSessionList(False);
  end;
end;


procedure Tconnform.updownPortChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
var
  ud: TUpDown;
  NewVal: Integer;
begin
  // Work around smallint values of TUpDown, allow integer values
  ud := Sender as TUpDown;
  NewVal := ud.Tag;
  case Direction of
    updUp: Inc(NewVal);
    // "updNone" is fired when down arrow is pressed. VCL bug?
    updDown, updNone: Dec(NewVal);
  end;
  editPort.Text := IntToStr(NewVal);
  AllowChange := False;
end;


procedure Tconnform.editPortChange(Sender: TObject);
begin
  // Work around smallint values of TUpDown, allow integer values
  updownPort.Tag := MakeInt(TEdit(Sender).Text);
  Modification(Sender);
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
      or (FOrgPassword <> editPassword.Text) or (FOrgPort <> updownPort.Tag)
      or (FOrgCompressed <> chkCompressed.Checked)
      or (FOrgNetType <> NetType);
    ListSessions.Repaint;
    ValidateControls;
  end;
end;


procedure Tconnform.radioNetTypeClick(Sender: TObject);
begin
  // Toggle between TCP/IP and named pipes mode
  if radioTypeTCPIP.Checked then
    lblHost.Caption := 'Hostname / IP:'
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


procedure Tconnform.ValidateControls;
var
  SessionFocused: Boolean;
begin
  SessionFocused := Assigned(ListSessions.FocusedNode);

  btnOpen.Enabled := SessionFocused;
  btnNew.Enabled := not FSessionAdded;
  btnSave.Enabled := FSessionModified or FSessionAdded;
  btnDelete.Enabled := SessionFocused;
  btnOpen.Enabled := SessionFocused;

  if not SessionFocused then begin
    grpDetails.Visible := False;
    lblHelp.Visible := True;
    if FSessionNames.Count = 0 then
      lblHelp.Caption := 'New here? In order to connect to a MySQL server, you have to create a so called '+
        '"session" at first. Just click the "New" button on the bottom left to create your first session.'+CRLF+CRLF+
        'Give it a friendly name (e.g. "Local DB server") so you''ll recall it the next time you start '+APPNAME+'.'
    else
      lblHelp.Caption := 'Please click a session on the left list to edit parameters, doubleclick to open it.';
  end else begin
    lblHelp.Visible := False;
    grpDetails.Visible := True;
  end;
end;


procedure Tconnform.FormResize(Sender: TObject);
var
  ButtonWidth: Integer;
const
  Margin = 6;
begin
  // Resize form - adjust width of both main components
  ListSessions.Width := Round(ClientWidth / 100 * FWidthListSessions);
  grpDetails.Left := 2 * ListSessions.Left + ListSessions.Width;
  grpDetails.Width := ClientWidth - grpDetails.Left - Margin;
  lblHelp.Left := grpDetails.Left;
  ButtonWidth := Round((ListSessions.Width - 2 * Margin) / 3);
  btnNew.Width := ButtonWidth;
  btnSave.Width := ButtonWidth;
  btnDelete.Width := ButtonWidth;
  btnNew.Left := ListSessions.Left;
  btnSave.Left := btnNew.Left + btnNew.Width + Margin;
  btnDelete.Left := btnSave.Left + btnSave.Width + Margin;
end;


end.
