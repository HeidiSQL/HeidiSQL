unit connections;


// -------------------------------------
// Connections (start-window)
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  VirtualTrees, Menus, Graphics, Generics.Collections, ActiveX, extra_controls,
  dbconnection, gnugettext;

type
  Tconnform = class(TFormWithSizeGrip)
    btnCancel: TButton;
    btnOpen: TButton;
    btnSave: TButton;
    ListSessions: TVirtualStringTree;
    btnNew: TButton;
    btnDelete: TButton;
    popupSessions: TPopupMenu;
    menuSave: TMenuItem;
    menuDelete: TMenuItem;
    menuSaveAs: TMenuItem;
    TimerStatistics: TTimer;
    PageControlDetails: TPageControl;
    tabSettings: TTabSheet;
    lblPort: TLabel;
    lblPassword: TLabel;
    lblHost: TLabel;
    lblUsername: TLabel;
    lblNetworkType: TLabel;
    chkCompressed: TCheckBox;
    editPort: TEdit;
    updownPort: TUpDown;
    editPassword: TEdit;
    editUsername: TEdit;
    editHost: TEdit;
    tabAdvanced: TTabSheet;
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
    editDatabases: TButtonedEdit;
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
    popupNew: TPopupMenu;
    menuNewSessionInRoot: TMenuItem;
    menuNewFolderInRoot: TMenuItem;
    menuContextNewFolderInFolder: TMenuItem;
    menuContextNewSessionInFolder: TMenuItem;
    menuNewSessionInFolder: TMenuItem;
    menuNewFolderInFolder: TMenuItem;
    chkLocalTimeZone: TCheckBox;
    editStartupScript: TButtonedEdit;
    lblStartupScript: TLabel;
    chkFullTableStatus: TCheckBox;
    btnMore: TButton;
    popupMore: TPopupMenu;
    Checkforupdates1: TMenuItem;
    About1: TMenuItem;
    Preferences1: TMenuItem;
    Exportsettingsfile1: TMenuItem;
    Importsettingsfile1: TMenuItem;
    lblComment: TLabel;
    memoComment: TMemo;
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
    procedure editDatabasesRightButtonClick(Sender: TObject);
    procedure chkLoginPromptClick(Sender: TObject);
    procedure ListSessionsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure comboNetTypeChange(Sender: TObject);
    procedure splitterMainMoved(Sender: TObject);
    procedure btnImportSettingsClick(Sender: TObject);
    procedure timerSettingsImportTimer(Sender: TObject);
    procedure ListSessionsStructureChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Reason: TChangeReason);
    procedure ListSessionsDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure ListSessionsDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure btnMoreClick(Sender: TObject);
  private
    { Private declarations }
    FLoaded: Boolean;
    FSessionModified, FOnlyPasswordModified: Boolean;
    FServerVersion: String;
    FSessionColor: TColor;
    FSettingsImportWaitTime: Cardinal;
    FPopupDatabases: TPopupMenu;
    procedure RefreshSessions(ParentNode: PVirtualNode);
    function SelectedSessionPath: String;
    function CurrentParams: TConnectionParameters;
    procedure FinalizeModifications(var CanProceed: Boolean);
    procedure ValidateControls;
    function NodeSessionNames(Node: PVirtualNode; var RegKey: String): TStringList;
    procedure MenuDatabasesClick(Sender: TObject);
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
  nt: TNetType;
  Node: PVirtualNode;
  Params: TConnectionParameters;
begin
  // Fix GUI stuff
  TranslateComponent(Self);
  InheritFont(Font);
  Width := AppSettings.ReadInt(asSessionManagerWindowWidth);
  Height := AppSettings.ReadInt(asSessionManagerWindowHeight);
  ListSessions.Width := AppSettings.ReadInt(asSessionManagerListWidth);
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

  // Init sessions tree
  RefreshSessions(nil);

  // Focus last session
  SelectNode(ListSessions, nil);
  LastSessions := Explode(DELIM, AppSettings.ReadString(asLastSessions));
  LastActiveSession := AppSettings.ReadString(asLastActiveSession);
  if (LastActiveSession = '') and (LastSessions.Count > 0) then
    LastActiveSession := LastSessions[0];
  Node := ListSessions.GetFirst;
  while Assigned(Node) do begin
    PSess := ListSessions.GetNodeData(Node);
    if PSess.SessionPath = LastActiveSession then
      SelectNode(ListSessions, Node);
    Node := ListSessions.GetNext(Node);
  end;
end;


procedure Tconnform.RefreshSessions(ParentNode: PVirtualNode);
var
  SessionNames: TStringList;
  RegKey: String;
  i: Integer;
  Params: TConnectionParameters;
  SessNode: PVirtualNode;
begin
  // Initialize session tree
  if ParentNode=nil then
    ListSessions.Clear
  else
    ListSessions.DeleteChildren(ParentNode, True);
  SessionNames := NodeSessionNames(ParentNode, RegKey);
  for i:=0 to SessionNames.Count-1 do begin
    Params := TConnectionParameters.Create(RegKey+SessionNames[i]);
    SessNode := ListSessions.AddChild(ParentNode, PConnectionParameters(Params));
    if Params.IsFolder then
      RefreshSessions(SessNode);
  end;
end;


procedure Tconnform.FormDestroy(Sender: TObject);
begin
  // Save GUI stuff
  AppSettings.WriteInt(asSessionManagerListWidth, ListSessions.Width);
  AppSettings.WriteInt(asSessionManagerWindowWidth, Width);
  AppSettings.WriteInt(asSessionManagerWindowHeight, Height);
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
  if not btnOpen.Enabled then
    Exit;
  Screen.Cursor := crHourglass;
  if Mainform.InitConnection(CurrentParams, True, Connection) then
    ModalResult := mrOK
  else begin
    TimerStatistics.OnTimer(Sender);
    ModalResult := mrNone;
  end;
  Screen.Cursor := crDefault;
end;


procedure Tconnform.btnSaveClick(Sender: TObject);
var
  Sess: PConnectionParameters;
begin
  // Overtake edited values for current parameter object and save to registry
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
  Sess.FullTableStatus := chkFullTableStatus.Checked;
  Sess.AllDatabasesStr := editDatabases.Text;
  Sess.Comment := memoComment.Text;
  Sess.StartupScriptFilename := editStartupScript.Text;
  Sess.SSHPlinkExe := editSSHPlinkExe.Text;
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
  Sess.SaveToRegistry;

  FSessionModified := False;
  ListSessions.Invalidate;
  ValidateControls;
end;


procedure Tconnform.btnMoreClick(Sender: TObject);
var
  btn: TButton;
begin
  btn := Sender as TButton;
  btn.DropDownMenu.Popup(btn.ClientOrigin.X, btn.ClientOrigin.Y+btn.Height);
end;


procedure Tconnform.btnSaveAsClick(Sender: TObject);
var
  newName, ParentKey: String;
  NameOK: Boolean;
  NewSess: TConnectionParameters;
  Node: PVirtualNode;
  SessionNames: TStringList;
begin
  // Save session as ...
  newName := _('Enter new session name ...');
  NameOK := False;
  SessionNames := NodeSessionNames(ListSessions.FocusedNode.Parent, ParentKey);
  while not NameOK do begin
    if not InputQuery(_('Clone session ...'), _('New session name:'), newName) then
      Exit; // Cancelled
    NameOK := SessionNames.IndexOf(newName) = -1;
    if not NameOK then
      ErrorDialog(f_('Session "%s" already exists!', [ParentKey+newName]))
    else begin
      // Create the key and save its values
      NewSess := CurrentParams;
      NewSess.SessionPath := ParentKey+newName;
      NewSess.SaveToRegistry;
      Node := ListSessions.InsertNode(ListSessions.FocusedNode, amInsertAfter, PConnectionParameters(NewSess));
      FSessionModified := False;
      SelectNode(ListSessions, Node);
    end;
  end;
  SessionNames.Free;
end;


procedure Tconnform.btnImportSettingsClick(Sender: TObject);
begin
  MainForm.actImportSettings.Execute;
  FSettingsImportWaitTime := 0;
  timerSettingsImport.Enabled := MainForm.ImportSettingsDone;
end;


procedure Tconnform.timerSettingsImportTimer(Sender: TObject);
begin
  Inc(FSettingsImportWaitTime, timerSettingsImport.Interval);
  RefreshSessions(nil);
  if ListSessions.RootNodeCount > 0 then
    timerSettingsImport.Enabled := False;
  if FSettingsImportWaitTime >= 10000 then begin
    timerSettingsImport.Enabled := False;
    MessageDialog(f_('Imported sessions could not be detected. Restarting %s may solve that.', [APPNAME]), mtWarning, [mbOK]);
  end;
end;


procedure Tconnform.btnNewClick(Sender: TObject);
var
  i: Integer;
  CanProceed, CreateInRoot: Boolean;
  NewSess: TConnectionParameters;
  ParentSess: PConnectionParameters;
  ParentNode, NewNode: PVirtualNode;
  ParentPath: String;
  SiblingSessionNames: TStringList;
begin
  // Create new session or folder
  FinalizeModifications(CanProceed);
  if not CanProceed then
    Exit;

  CreateInRoot := (Sender = menuNewSessionInRoot) or (Sender = menuNewFolderInRoot);
  if Assigned(ListSessions.FocusedNode) then
    ParentSess := ListSessions.GetNodeData(ListSessions.FocusedNode)
  else
    ParentSess := nil;
  if CreateInRoot then
    ParentNode := nil
  else begin
    if ParentSess = nil then
      ParentNode := nil
    else if ParentSess.IsFolder then
      ParentNode := ListSessions.FocusedNode
    else
      ParentNode := ListSessions.FocusedNode.Parent;
  end;
  SiblingSessionNames := NodeSessionNames(ParentNode, ParentPath);

  NewSess := TConnectionParameters.Create;
  NewSess.IsFolder := (Sender = menuNewFolderInRoot) or (Sender = menuContextNewFolderInFolder) or (Sender = menuNewFolderInFolder);
  NewSess.SessionPath := ParentPath + 'Unnamed';
  i := 0;
  while SiblingSessionNames.IndexOf(NewSess.SessionName) > -1 do begin
    inc(i);
    NewSess.SessionPath := ParentPath + 'Unnamed-' + IntToStr(i);
  end;
  NewSess.SaveToRegistry;
  SiblingSessionNames.Free;
  NewNode := ListSessions.AddChild(ParentNode, PConnectionParameters(NewSess));
  // Select it
  SelectNode(ListSessions, NewNode);
  ValidateControls;
  ListSessions.EditNode(NewNode, 0);
end;


procedure Tconnform.btnDeleteClick(Sender: TObject);
var
  Sess: PConnectionParameters;
  Node, FocusNode: PVirtualNode;
begin
  Node := ListSessions.FocusedNode;
  Sess := ListSessions.GetNodeData(Node);
  if MessageDialog(f_('Delete session "%s"?', [Sess.SessionName]), mtConfirmation, [mbYes, mbCancel]) = mrYes then
  begin
    AppSettings.SessionPath := Sess.SessionPath;
    AppSettings.DeleteCurrentKey;
    if Assigned(Node.NextSibling) then
      FocusNode := Node.NextSibling
    else if Assigned(Node.PrevSibling) then
      FocusNode := Node.PrevSibling
    else
      FocusNode := Node.Parent;
    ListSessions.DeleteNode(Node);
    SelectNode(ListSessions, FocusNode);
    ListSessions.SetFocus;
  end;
end;


function Tconnform.SelectedSessionPath: String;
var
  Sess: PConnectionParameters;
begin
  Sess := ListSessions.GetNodeData(ListSessions.FocusedNode);
  Result := Sess.SessionPath;
end;


function Tconnform.CurrentParams: TConnectionParameters;
var
  FromReg: PConnectionParameters;
begin
  // Return non-stored parameters
  FromReg := ListSessions.GetNodeData(ListSessions.FocusedNode);
  if FromReg.IsFolder then begin
    Result := FromReg^;
  end else begin
    Result := TConnectionParameters.Create;
    Result.SessionPath := SelectedSessionPath;
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
    Result.AllDatabasesStr := editDatabases.Text;
    Result.Comment := memoComment.Text;
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
    Result.FullTableStatus := chkFullTableStatus.Checked;
  end;
end;


procedure Tconnform.ListSessionsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Sess: PConnectionParameters;
begin
  // An edited session gets an additional pencil symbol
  if Column > 0 then
    ImageIndex := -1
  else case Kind of
    ikNormal, ikSelected: begin
      Sess := Sender.GetNodeData(Node);
      ImageIndex := Sess.ImageIndex;
    end;

    ikOverlay:
      if (Node = Sender.FocusedNode) and FSessionModified then
        ImageIndex := 162;

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
  if Sess.IsFolder then begin
    case Column of
      0: CellText := Sess.SessionName;
      else CellText := '';
    end;
  end else begin
    case Column of
      0: begin
        CellText := Sess.SessionName;
        if FSessionModified and (Node = Sender.FocusedNode) and (not Sender.IsEditing) then
          CellText := CellText + ' *';
      end;
      1: CellText := Sess.Hostname;
      2: CellText := Sess.Username;
      3: CellText := Sess.ServerVersion;
      4: if Sess.LastConnect>0 then
          CellText := DateTimeToStr(Sess.LastConnect)
        else
          CellText := '';
      5: CellText := FormatNumber(Sess.Counter);
      6: CellText := Sess.Comment;
    end;
  end;
end;


function Tconnform.NodeSessionNames(Node: PVirtualNode; var RegKey: String): TStringList;
var
  Sess: PConnectionParameters;
  Folders: TStringList;
begin
  // Find sibling session names in a folder node

  if Node = nil then
    Node := ListSessions.RootNode;

  // Find registry sub path for given node
  RegKey := '';
  if Node <> ListSessions.RootNode then begin
    Sess := ListSessions.GetNodeData(Node);
    RegKey := Sess.SessionPath + '\';
  end;

  // Fetch from registry
  Folders := TStringList.Create;
  Result := AppSettings.GetSessionNames(RegKey, Folders);
  Result.AddStrings(Folders);
  Folders.Free;
end;


procedure Tconnform.ListSessionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  // Use our own text editor to rename a session
  EditLink := TInplaceEditorLink.Create(Sender as TVirtualStringTree);
end;


procedure Tconnform.ListSessionsDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  TargetNode, ParentNode: PVirtualNode;
  AttachMode: TVTNodeAttachMode;
  TargetSess, FocusedSess: PConnectionParameters;
  ParentKey: String;
  SiblingSessions: TStringList;
begin
  TargetNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  if not Assigned(TargetNode) then begin
    MessageBeep(MB_ICONEXCLAMATION);
    Exit;
  end;
  TargetSess := Sender.GetNodeData(TargetNode);
  FocusedSess := Sender.GetNodeData(ListSessions.FocusedNode);
  case Mode of
    dmAbove:
      AttachMode := amInsertBefore;
    dmOnNode:
      if TargetSess.IsFolder then
        AttachMode := amAddChildFirst
      else
        AttachMode := amInsertBefore;
    dmBelow:
      AttachMode := amInsertAfter;
    else
      AttachMode := amInsertAfter;
  end;
  if AttachMode in [amInsertBefore, amInsertAfter] then
    ParentNode := TargetNode.Parent
  else
    ParentNode := TargetNode;

  SiblingSessions := NodeSessionNames(ParentNode, ParentKey);
  // Test if target folder has an equal named node
  if SiblingSessions.IndexOf(FocusedSess.SessionName) > -1 then
    ErrorDialog(f_('Session "%s" already exists!', [ParentKey+FocusedSess.SessionName]))
  else begin
    try
      AppSettings.SessionPath := FocusedSess.SessionPath;
      AppSettings.MoveCurrentKey(REGKEY_SESSIONS+'\'+ParentKey+FocusedSess.SessionName);
      ListSessions.MoveTo(ListSessions.FocusedNode, TargetNode, AttachMode, False);
      FocusedSess.SessionPath := ParentKey+FocusedSess.SessionName;
    except
      on E:Exception do
        ErrorDialog(f_('Error while moving registry key: %s', [E.Message]));
    end;
  end;
  SiblingSessions.Free;
end;


procedure Tconnform.ListSessionsDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  TargetNode, ParentNode: PVirtualNode;
  TargetSess: PConnectionParameters;
begin
  // Allow node dragging everywhere except within the current folder
  TargetNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  TargetSess := Sender.GetNodeData(TargetNode);
  Accept := (Source = Sender)
    and Assigned(TargetSess)
    and (Mode <> dmNowhere)
    and (TargetNode <> ListSessions.FocusedNode.Parent);

  // Moving a folder into itself would create an infinite folder structure
  if Accept and TargetSess.IsFolder then
    Accept := Accept and (TargetNode <> ListSessions.FocusedNode);
  if Accept and (not TargetSess.IsFolder) then
    Accept := Accept and (TargetNode.Parent <> ListSessions.FocusedNode.Parent);

  if Accept then begin
    // Do not allow focused node to be moved somewhere below itself
    ParentNode := TargetNode.Parent;
    while Assigned(ParentNode) do begin
      Accept := Accept and (ParentNode <> ListSessions.FocusedNode);
      if not Accept then
        Break;
      ParentNode := ParentNode.Parent;
    end;
    // Shows the right tooltip on Aero GUI
    Effect := DROPEFFECT_MOVE;
  end;
end;


procedure Tconnform.ListSessionsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  SessionFocused, InFolder: Boolean;
  Sess: PConnectionParameters;
begin
  // select one connection!
  Screen.Cursor := crHourglass;
  TimerStatistics.Enabled := False;
  SessionFocused := False;
  InFolder := False;
  Sess := nil;
  if Assigned(Node) then begin
    Sess := Sender.GetNodeData(Node);
    SessionFocused := not Sess.IsFolder;
    InFolder := (ListSessions.GetNodeLevel(Node) > 0) or Sess.IsFolder;
  end;
  FLoaded := False;
  tabStart.TabVisible := not SessionFocused;
  tabSettings.TabVisible := SessionFocused;
  tabSSHtunnel.TabVisible := SessionFocused;
  tabAdvanced.TabVisible := SessionFocused;
  tabStatistics.TabVisible := SessionFocused;
  menuNewSessionInFolder.Enabled := InFolder;
  menuNewFolderInFolder.Enabled := InFolder;
  FreeAndNil(FPopupDatabases);

  if not SessionFocused then begin
    PageControlDetails.ActivePage := tabStart;
    if ListSessions.RootNodeCount = 0 then
      lblHelp.Caption := f_('New here? In order to connect to a server, you have to create a so called '+
        '"session" at first. Just click the "New" button on the bottom left to create your first session.'+
        'Give it a friendly name (e.g. "Local DB server") so you''ll recall it the next time you start %s.', [APPNAME])
    else
      lblHelp.Caption := _('Please click a session on the left list to edit parameters, doubleclick to open it.');
  end else begin
    PageControlDetails.ActivePage := tabSettings;

    comboNetType.ItemIndex := Integer(Sess.NetType);
    editHost.Text := Sess.Hostname;
    editUsername.Text := Sess.Username;
    editPassword.Text := Sess.Password;
    chkLoginPrompt.Checked := Sess.LoginPrompt;
    chkWindowsAuth.Checked := Sess.WindowsAuth;
    updownPort.Position := Sess.Port;
    chkCompressed.Checked := Sess.Compressed;
    chkLocalTimeZone.Checked := Sess.LocalTimeZone;
    chkFullTableStatus.Checked := Sess.FullTableStatus;
    editDatabases.Text := Sess.AllDatabasesStr;
    memoComment.Text := Sess.Comment;
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
  lblLastConnectRight.Caption := _('unknown or never');
  lblLastConnectRight.Hint := '';
  lblLastConnectRight.Enabled := False;
  lblCreatedRight.Caption := _('unknown');
  lblCreatedRight.Hint := '';
  lblCreatedRight.Enabled := False;
  lblCounterRight.Caption := _('not available');
  lblCounterRight.Enabled := False;

  if not Assigned(ListSessions.FocusedNode) then
    Exit;

  AppSettings.SessionPath := SelectedSessionPath;
  DummyDate := StrToDateTime('2000-01-01');
  LastConnect := StrToDateTimeDef(AppSettings.ReadString(asLastConnect), DummyDate);
  if LastConnect <> DummyDate then begin
    lblLastConnectRight.Hint := DateTimeToStr(LastConnect);
    lblLastConnectRight.Caption := DateBackFriendlyCaption(LastConnect);
    lblLastConnectRight.Enabled := True;
  end;
  Created := StrToDateTimeDef(AppSettings.ReadString(asSessionCreated), DummyDate);
  if Created <> DummyDate then begin
    lblCreatedRight.Hint := DateTimeToStr(Created);
    lblCreatedRight.Caption := DateBackFriendlyCaption(Created);
    lblCreatedRight.Enabled := True;
  end;
  Connects := AppSettings.ReadInt(asConnectCount);
  Refused := AppSettings.ReadInt(asRefusedCount);
  lblCounterRight.Enabled := Connects + Refused > 0;
  if Connects > 0 then begin
    lblCounterRight.Caption := f_('successful connects: %d', [Connects]);
    if Refused > 0 then
      lblCounterRight.Caption := lblCounterRight.Caption + ', '+_('unsuccessful')+': '+IntToStr(Refused);
  end else if Refused > 0 then
    lblCounterRight.Caption := f_('unsuccessful connects: %d', [Refused]);
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
  ParentKey: String;
  Connection: TDBConnection;
  Sess: PConnectionParameters;
  SiblingSessions: TStringList;
begin
  // Rename session
  Sess := Sender.GetNodeData(Node);
  SiblingSessions := NodeSessionNames(Node.Parent, ParentKey);
  if SiblingSessions.IndexOf(NewText) > -1 then begin
    ErrorDialog(f_('Session "%s" already exists!', [ParentKey+NewText]));
    NewText := Sess.SessionName;
  end else begin
    AppSettings.SessionPath := Sess.SessionPath;
    AppSettings.MoveCurrentKey(REGKEY_SESSIONS+'\'+ParentKey+NewText);
    // Also fix internal session names in main form, which gets used to store e.g. "lastuseddb" later
    for Connection in MainForm.Connections do begin
      if Connection.Parameters.SessionPath = Sess.SessionPath then
        Connection.Parameters.SessionPath := ParentKey+NewText;
    end;
    MainForm.SetWindowCaption;
    Sess.SessionPath := ParentKey+NewText;
  end;
  SiblingSessions.Free;
end;


procedure Tconnform.ListSessionsStructureChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Reason: TChangeReason);
begin
  // Node added or removed. Tree needs a repaint in some cases.
  // TODO: does not work
  Sender.Repaint;
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


procedure Tconnform.editDatabasesRightButtonClick(Sender: TObject);
var
  Connection: TDBConnection;
  Params: TConnectionParameters;
  Item: TMenuItem;
  DB: String;
  p: TPoint;
  Databases: TStringList;
begin
  if FPopupDatabases = nil then begin
    // Try to connect and lookup database names
    Params := CurrentParams;
    Connection := Params.CreateConnection(Self);
    Connection.Parameters.AllDatabasesStr := '';
    Connection.LogPrefix := SelectedSessionPath;
    Connection.OnLog := Mainform.LogSQL;
    FPopupDatabases := TPopupMenu.Create(Self);
    FPopupDatabases.AutoHotkeys := maManual;
    Screen.Cursor := crHourglass;
    try
      Connection.Active := True;
      for DB in Connection.AllDatabases do begin
        Item := TMenuItem.Create(FPopupDatabases);
        Item.Caption := DB;
        Item.OnClick := MenuDatabasesClick;
        Item.AutoCheck := True;
        FPopupDatabases.Items.Add(Item);
      end;
    except
      // Silence connection errors here - should be sufficient to log them
    end;
    FreeAndNil(Connection);
  end;

  // Check/uncheck items, based on semicolon list
  Databases := Explode(';', editDatabases.Text);
  for Item in FPopupDatabases.Items do begin
    Item.Checked := Databases.IndexOf(Item.Caption) > -1;
  end;
  Databases.Free;

  p := editDatabases.ClientToScreen(editDatabases.ClientRect.BottomRight);
  FPopupDatabases.Popup(p.X-editDatabases.Images.Width, p.Y);
  Screen.Cursor := crDefault;
end;


procedure Tconnform.MenuDatabasesClick(Sender: TObject);
var
  Item: TMenuItem;
  Databases: TStringList;
  SelStart: Integer;
begin
  Item := Sender as TMenuItem;
  Databases := Explode(';', editDatabases.Text);
  if Item.Checked then
    Databases.Add(Item.Caption)
  else
    Databases.Delete(Databases.IndexOf(Item.Caption));
  SelStart := editDatabases.SelStart;
  editDatabases.Text := implodestr(';', Databases);
  editDatabases.SelStart := SelStart;
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
        updownPort.Position := MakeInt(AppSettings.GetDefaultString(asPort));
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
      or (Sess.FullTableStatus <> chkFullTableStatus.Checked)
      or (Sess.NetType <> TNetType(comboNetType.ItemIndex))
      or (Sess.StartupScriptFilename <> editStartupScript.Text)
      or (Sess.AllDatabasesStr <> editDatabases.Text)
      or (Sess.Comment <> memoComment.Text)
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
    if (Sender=editHost) or (Sender=editUsername) or (Sender=editPassword) or
      (Sender=comboNetType) or (Sender=chkWindowsAuth) or (Sender=editPort) then begin
      // Be sure to use the modified connection params next time the user clicks the "Databases" pulldown
      FreeAndNil(FPopupDatabases);
    end;

    ListSessions.Repaint;
    ValidateControls;
  end;
end;


procedure Tconnform.FinalizeModifications(var CanProceed: Boolean);
begin
  if FSessionModified and (not FOnlyPasswordModified) then begin
    case MessageDialog(_('Save modifications?'), f_('Settings for "%s" were changed.', [SelectedSessionPath]), mtConfirmation, [mbYes, mbNo, mbCancel]) of
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
  SessionFocused, FolderFocused: Boolean;
  Params: TConnectionParameters;
begin
  SessionFocused := False;
  FolderFocused := False;
  if Assigned(ListSessions.FocusedNode) then begin
    Params := CurrentParams;
    SessionFocused := not Params.IsFolder;
    FolderFocused := Params.IsFolder;

    if SessionFocused then begin
      // Validate session GUI stuff
      if Params.NetType = ntMySQL_NamedPipe then
        lblHost.Caption := _('Socket name:')
      else
        lblHost.Caption := _('Hostname / IP:');
      chkWindowsAuth.Enabled := Params.IsMSSQL;
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
      chkWantSSL.Enabled := Params.NetType = ntMySQL_TCPIP;
      lblSSLPrivateKey.Enabled := Params.WantSSL;
      editSSLPrivateKey.Enabled := Params.WantSSL;
      lblSSLCACertificate.Enabled := Params.WantSSL;
      editSSLCACertificate.Enabled := Params.WantSSL;
      lblSSLCertificate.Enabled := Params.WantSSL;
      editSSLCertificate.Enabled := Params.WantSSL;
      tabSSHtunnel.TabVisible := Params.NetType = ntMySQL_SSHtunnel;
      Params.Free;
    end;
  end;

  // Main buttons
  btnOpen.Enabled := SessionFocused;
  btnSave.Enabled := SessionFocused and FSessionModified;
  btnDelete.Enabled := SessionFocused or FolderFocused;
  menuSave.Enabled := btnSave.Enabled;
  menuSaveAs.Enabled := SessionFocused;
  menuDelete.Enabled := btnDelete.Enabled;
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
    Selector.Filter := _('SQL files')+' (*.sql)|*.sql|'+_('All files')+' (*.*)|*.*'
  else if Edit = editSSHPlinkExe then
    Selector.Filter := _('Executables')+' (*.exe)|*.exe|'+_('All files')+' (*.*)|*.*'
  else if Edit = editSSHPrivateKey then
    Selector.Filter := _('PuTTY private key')+' (*.ppk)|*.ppk|'+_('All files')+' (*.*)|*.*'
  else
    Selector.Filter := _('Privacy Enhanced Mail certificates')+' (*.pem)|*.pem|'+_('Certificates')+' (*.crt)|*.crt|'+_('All files')+' (*.*)|*.*';
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
