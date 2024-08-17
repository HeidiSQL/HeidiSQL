unit connections;


// -------------------------------------
// Connections (start-window)
// -------------------------------------


interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  VirtualTrees, Vcl.Menus, Vcl.Graphics, System.Generics.Collections, Winapi.ActiveX, extra_controls, Winapi.Messages,
  dbconnection, gnugettext, SynRegExpr, System.Types, Vcl.GraphUtil, Data.Win.ADODB, System.StrUtils,
  System.Math, System.Actions, System.IOUtils, Vcl.ActnList, Vcl.StdActns, VirtualTrees.BaseTree, VirtualTrees.Types, VirtualTrees.EditLink,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.AncestorVCL;

type
  Tconnform = class(TExtForm)
    btnCancel: TButton;
    btnOpen: TButton;
    btnSave: TButton;
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
    editUsername: TButtonedEdit;
    editHost: TButtonedEdit;
    tabAdvanced: TTabSheet;
    tabStatistics: TTabSheet;
    lblLastConnectLeft: TLabel;
    lblCounterLeft: TLabel;
    lblCreatedLeft: TLabel;
    lblCreatedRight: TLabel;
    lblCounterRight1: TLabel;
    lblLastConnectRight: TLabel;
    tabSSHtunnel: TTabSheet;
    editSSHlocalport: TEdit;
    editSSHUser: TEdit;
    editSSHPassword: TEdit;
    lblSSHLocalPort: TLabel;
    lblSSHUser: TLabel;
    lblSSHPassword: TLabel;
    lblSSHExe: TLabel;
    comboNetType: TComboBoxEx;
    lblSSHhost: TLabel;
    editSSHhost: TEdit;
    editSSHport: TEdit;
    editSSHPrivateKey: TButtonedEdit;
    lblSSHkeyfile: TLabel;
    editDatabases: TButtonedEdit;
    lblDatabase: TLabel;
    chkLoginPrompt: TCheckBox;
    lblSSHTimeout: TLabel;
    editSSHTimeout: TEdit;
    updownSSHTimeout: TUpDown;
    chkWindowsAuth: TCheckBox;
    chkCleartextPluginEnabled: TCheckBox;
    splitterMain: TSplitter;
    tabStart: TTabSheet;
    lblHelp: TLabel;
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
    lblQueryTimeout: TLabel;
    editQueryTimeout: TEdit;
    updownQueryTimeout: TUpDown;
    menuMoreGeneralHelp: TMenuItem;
    menuRename: TMenuItem;
    lblKeepAlive: TLabel;
    editKeepAlive: TEdit;
    updownKeepAlive: TUpDown;
    lblCounterRight2: TLabel;
    lblCounterLeft2: TLabel;
    TimerButtonAnimation: TTimer;
    lblBackgroundColor: TLabel;
    ColorBoxBackgroundColor: TColorBox;
    comboLibrary: TComboBox;
    lblLibrary: TLabel;
    pnlLeft: TPanel;
    ListSessions: TVirtualStringTree;
    editSearch: TButtonedEdit;
    popupHost: TPopupMenu;
    menuFindDatabaseFiles: TMenuItem;
    menuAddDatabaseFiles: TMenuItem;
    lblIgnoreDatabasePattern: TLabel;
    editIgnoreDatabasePattern: TEdit;
    ActionListConnections: TActionList;
    actFilter: TAction;
    Filter1: TMenuItem;
    chkLogFileDdl: TCheckBox;
    editLogFilePath: TButtonedEdit;
    tabSSL: TTabSheet;
    chkWantSSL: TCheckBox;
    lblSSLPrivateKey: TLabel;
    lblSSLCACertificate: TLabel;
    lblSSLCertificate: TLabel;
    lblSSLcipher: TLabel;
    editSSLcipher: TEdit;
    editSSLCertificate: TButtonedEdit;
    editSSLCACertificate: TButtonedEdit;
    editSSLPrivateKey: TButtonedEdit;
    lblLogFile: TLabel;
    chkLogFileDml: TCheckBox;
    timerEditFilterDelay: TTimer;
    comboSSHExe: TComboBox;
    chkSSHActive: TCheckBox;
    comboSSLVerification: TComboBox;
    lblSSLVerification: TLabel;
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
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure ListSessionsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: String);
    procedure ListSessionsFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TimerStatisticsTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListSessionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      out EditLink: IVTEditLink);
    procedure PickFile(Sender: TObject);
    procedure editHostChange(Sender: TObject);
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
    procedure menuRenameClick(Sender: TObject);
    procedure TimerButtonAnimationTimer(Sender: TObject);
    procedure ColorBoxBackgroundColorGetColors(Sender: TCustomColorBox;
      Items: TStrings);
    procedure editTrim(Sender: TObject);
    procedure editSearchChange(Sender: TObject);
    procedure editSearchRightButtonClick(Sender: TObject);
    procedure editHostDblClick(Sender: TObject);
    procedure ListSessionsNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure FindAddDatabaseFilesClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListSessionsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure actFilterExecute(Sender: TObject);
    procedure timerEditFilterDelayTimer(Sender: TObject);
    procedure chkSSHActiveClick(Sender: TObject);
    procedure PageControlDetailsChange(Sender: TObject);
    procedure editUsernameRightButtonClick(Sender: TObject);
  private
    { Private declarations }
    FLoaded: Boolean;
    FSessionModified, FOnlyPasswordModified: Boolean;
    FServerVersion: String;
    FSettingsImportWaitTime: Cardinal;
    FPopupDatabases: TPopupMenu;
    FPopupCiphers: TPopupMenu;
    FButtonAnimationStep: Integer;
    FLastSelectedNetTypeGroup: TNetTypeGroup;
    function GetSelectedNetType: TNetType;
    procedure SetSelectedNetType(Value: TNetType);
    procedure RefreshSessions(ParentNode: PVirtualNode);
    function SelectedSessionPath: String;
    function CurrentParams: TConnectionParameters;
    procedure FinalizeModifications(var CanProceed: Boolean);
    procedure ValidateControls;
    function NodeSessionNames(Node: PVirtualNode; var RegKey: String): TStringList;
    function GetWindowCaption: String;
    procedure MenuDatabasesClick(Sender: TObject);
    procedure MenuCiphersClick(Sender: TObject);
    procedure WMNCLBUTTONDOWN(var Msg: TWMNCLButtonDown) ; message WM_NCLBUTTONDOWN;
    procedure WMNCLBUTTONUP(var Msg: TWMNCLButtonUp) ; message WM_NCLBUTTONUP;
    procedure RefreshBackgroundColors;
    property SelectedNetType: TNetType read GetSelectedNetType write SetSelectedNetType;
  public
    { Public declarations }
  end;


implementation

uses Main, apphelpers, grideditlinks, dbstructures.sqlite;

{$I const.inc}

{$R *.DFM}


procedure Tconnform.WMNCLBUTTONDOWN(var Msg: TWMNCLButtonDown) ;
begin
  if Msg.HitTest = HTHELP then
    Msg.Result := 0 // "eat" the message
  else
    inherited;
end;


procedure Tconnform.WMNCLBUTTONUP(var Msg: TWMNCLButtonUp) ;
begin
  if Msg.HitTest = HTHELP then begin
    Msg.Result := 0;
    Help(Self, 'connecting');
  end else
    inherited;
end;


function Tconnform.GetWindowCaption: String;
begin
  Result := APPNAME + ' ' + MainForm.AppVersion + ' - ' + _('Session manager');
  if not SelectedSessionPath.IsEmpty then
    Result := Result + ': ' + SelectedSessionPath;
end;


procedure Tconnform.FormCreate(Sender: TObject);
var
  NetTypeStr, FilenameHint, ExePath, ExeFile: String;
  nt: TNetType;
  ntg: TNetTypeGroup;
  Params: TConnectionParameters;
  ComboItem: TComboExItem;
  Placeholders: TStringList;
  i: Integer;
  ExeFiles: TStringDynArray;
begin
  // Fix GUI stuff
  HasSizeGrip := True;
  Caption := GetWindowCaption;

  ListSessions.OnCompareNodes := MainForm.AnyGridCompareNodes;
  ListSessions.OnHeaderClick := MainForm.AnyGridHeaderClick;
  ListSessions.OnHeaderDraggedOut := MainForm.AnyGridHeaderDraggedOut;
  btnImportSettings.Caption := MainForm.actImportSettings.Caption;
  FLoaded := False;

  comboNetType.Clear;
  Params := TConnectionParameters.Create;
  for ntg := Low(ntg) to High(ntg) do begin
    for nt:=Low(nt) to High(nt) do begin
      Params.NetType := nt;
      if Params.GetNetTypeGroup <> ntg then
        Continue;
      NetTypeStr := Params.NetTypeName(True);
      ComboItem := TComboExItem.Create(comboNetType.ItemsEx);
      ComboItem.Caption := NetTypeStr;
      ComboItem.ImageIndex := Params.ImageIndex;
      ComboItem.Data := Pointer(nt);
    end;
  end;
  Params.Free;

  // Create filename placeholders hint
  Placeholders := GetOutputFilenamePlaceholders;
  FilenameHint := _('Allows the following replacement patterns:');
  for i:=0 to Placeholders.Count-1 do begin
    FilenameHint := FilenameHint + CRLF + '%' + Placeholders.Names[i] + ': ' + Placeholders.ValueFromIndex[i];
  end;
  Placeholders.Free;
  editLogFilePath.Hint := FilenameHint;

  // Populate dropdown with supported SSH executables
  ExeFiles := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)), '*.exe');
  for ExePath in ExeFiles do begin
    ExeFile := ExtractFileName(ExePath);
    if ExecRegExprI('([pk]link|putty)', ExeFile) then begin
      comboSSHExe.Items.Add(ExeFile);
    end;
  end;
  SetLength(ExeFiles, 0);
  comboSSHExe.Items.Add('ssh.exe');
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
  // And while we're at it, collect custom colors for background color selector
  if ParentNode=nil then begin
    ListSessions.Clear;
  end else begin
    ListSessions.DeleteChildren(ParentNode, True);
  end;
  SessionNames := NodeSessionNames(ParentNode, RegKey);
  for i:=0 to SessionNames.Count-1 do begin
    Params := TConnectionParameters.Create(RegKey+SessionNames[i]);
    SessNode := ListSessions.AddChild(ParentNode, PConnectionParameters(Params));
    if Params.IsFolder then begin
      RefreshSessions(SessNode);
    end;
  end;
  if not Assigned(ParentNode) then
    RefreshBackgroundColors;
end;


procedure Tconnform.FormResize(Sender: TObject);
begin
  splitterMainMoved(splitterMain);
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
  // Save GUI stuff
  AppSettings.WriteIntDpiAware(asSessionManagerListWidth, Self, pnlLeft.Width);
  AppSettings.WriteIntDpiAware(asSessionManagerWindowWidth, Self, Width);
  AppSettings.WriteIntDpiAware(asSessionManagerWindowHeight, Self, Height);
  AppSettings.WriteInt(asSessionManagerWindowLeft, Left);
  AppSettings.WriteInt(asSessionManagerWindowTop, Top);
  SaveListSetup(ListSessions);
end;


procedure Tconnform.FormShow(Sender: TObject);
var
  LastActiveSession: String;
  LastSessions: TStringList;
  PSess: PConnectionParameters;
  Node: PVirtualNode;
begin
  Width := AppSettings.ReadIntDpiAware(asSessionManagerWindowWidth, Self);
  Height := AppSettings.ReadIntDpiAware(asSessionManagerWindowHeight, Self);
  Left := AppSettings.ReadInt(asSessionManagerWindowLeft, '', Left);
  Top := AppSettings.ReadInt(asSessionManagerWindowTop, '', Top);
  // Move to visible area if window was on a now plugged off monitor previously
  MakeFullyVisible;
  pnlLeft.Width := AppSettings.ReadIntDpiAware(asSessionManagerListWidth, Self);
  splitterMain.OnMoved(Sender);
  FixVT(ListSessions);
  RestoreListSetup(ListSessions);

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

  ListSessions.SetFocus;
  // Reactivate statistics
  TimerStatistics.Enabled := True;
  TimerStatistics.OnTimer(Sender);
  FLoaded := True;
end;


function Tconnform.GetSelectedNetType: TNetType;
begin
  Result := TNetType(comboNetType.ItemsEx[comboNetType.ItemIndex].Data);
end;


procedure Tconnform.SetSelectedNetType(Value: TNetType);
var
  i: Integer;
begin
  for i:=0 to comboNetType.ItemsEx.Count-1 do begin
    if TNetType(comboNetType.ItemsEx[i].Data) = Value then begin
      comboNetType.ItemIndex := i;
      Break;
    end;
  end;
end;


procedure Tconnform.btnOpenClick(Sender: TObject);
var
  Connection: TDBConnection;
  Params: TConnectionParameters;
begin
  // Connect to selected session
  Params := CurrentParams;

  if not btnOpen.Enabled then
    Exit;
  btnOpen.Enabled := False;
  FButtonAnimationStep := 0;
  TimerButtonAnimation.Enabled := True;
  Screen.Cursor := crHourglass;
  if Mainform.InitConnection(Params, True, Connection) then
    ModalResult := mrOK
  else begin
    TimerStatistics.OnTimer(Sender);
    ModalResult := mrNone;
  end;
  TimerButtonAnimation.Enabled := False;
  btnOpen.Enabled := True;
  btnOpen.Caption := _('Open');
  Screen.Cursor := crDefault;
end;


procedure Tconnform.btnSaveClick(Sender: TObject);
var
  Sess: PConnectionParameters;
  Conn: TDBConnection;
begin
  // Overtake edited values for current parameter object and save to registry
  if Assigned(ListSessions.FocusedNode) then begin
    Sess := ListSessions.GetNodeData(ListSessions.FocusedNode);
    Sess.Hostname := editHost.Text;
    Sess.Username := editUsername.Text;
    Sess.Password := editPassword.Text;
    Sess.LoginPrompt := chkLoginPrompt.Checked;
    Sess.WindowsAuth := chkWindowsAuth.Checked;
    Sess.CleartextPluginEnabled := chkCleartextPluginEnabled.Checked;
    Sess.Port := updownPort.Position;
    Sess.NetType := SelectedNetType;
    Sess.Compressed := chkCompressed.Checked;
    Sess.QueryTimeout := updownQueryTimeout.Position;
    Sess.KeepAlive := updownKeepAlive.Position;
    Sess.LocalTimeZone := chkLocalTimeZone.Checked;
    Sess.FullTableStatus := chkFullTableStatus.Checked;
    Sess.SessionColor := ColorBoxBackgroundColor.Selected;
    Sess.LibraryOrProvider := comboLibrary.Text;
    Sess.AllDatabasesStr := editDatabases.Text;
    Sess.Comment := memoComment.Text;
    Sess.StartupScriptFilename := editStartupScript.Text;
    Sess.SSHActive := chkSSHActive.Enabled and chkSSHActive.Checked;
    Sess.SSHExe := comboSSHExe.Text;
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
    Sess.SSLCipher := editSSLCipher.Text;
    Sess.SSLVerification := comboSSLVerification.ItemIndex;
    Sess.IgnoreDatabasePattern := editIgnoreDatabasePattern.Text;
    Sess.LogFileDdl := chkLogFileDdl.Checked;
    Sess.LogFileDml := chkLogFileDml.Checked;
    Sess.LogFilePath := editLogFilePath.Text;
    Sess.SaveToRegistry;

    // Apply session color (and othher settings) to opened connection(s)
    for Conn in MainForm.Connections do begin
      if Conn.Parameters.SessionPath = Sess.SessionPath then begin
        Conn.Parameters.SessionColor := Sess.SessionColor;
        MainForm.DBtree.Invalidate;
      end;
    end;
  end;

  FSessionModified := False;
  ListSessions.Invalidate;
  RefreshBackgroundColors;
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


procedure Tconnform.actFilterExecute(Sender: TObject);
begin
  editSearch.SetFocus;
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
  if not Assigned(ListSessions.FocusedNode) then
    Result := ''
  else begin
    Sess := ListSessions.GetNodeData(ListSessions.FocusedNode);
    Result := Sess.SessionPath;
  end;
end;


function Tconnform.CurrentParams: TConnectionParameters;
var
  FromReg: PConnectionParameters;
begin
  // Return non-stored parameters
  if not Assigned(ListSessions.FocusedNode) then begin
    Result := nil;
    Exit;
  end;

  FromReg := ListSessions.GetNodeData(ListSessions.FocusedNode);
  if FromReg.IsFolder then begin
    Result := FromReg^;
  end else begin
    Result := TConnectionParameters.Create;
    Result.SessionPath := SelectedSessionPath;
    Result.Counter := FromReg.Counter;
    Result.SessionColor := ColorBoxBackgroundColor.Selected;
    Result.NetType := SelectedNetType;
    Result.ServerVersion := FServerVersion;
    Result.Hostname := editHost.Text;
    Result.Username := editUsername.Text;
    Result.Password := editPassword.Text;
    Result.LoginPrompt := chkLoginPrompt.Checked;
    Result.WindowsAuth := chkWindowsAuth.Checked;
    Result.CleartextPluginEnabled := chkCleartextPluginEnabled.Checked;
    if updownPort.Enabled then
      Result.Port := updownPort.Position
    else
      Result.Port := 0;
    Result.AllDatabasesStr := editDatabases.Text;
    Result.LibraryOrProvider := comboLibrary.Text;
    Result.Comment := memoComment.Text;
    Result.SSHActive := chkSSHActive.Enabled and chkSSHActive.Checked;
    Result.SSHHost := editSSHHost.Text;
    Result.SSHPort := MakeInt(editSSHPort.Text);
    Result.SSHUser := editSSHuser.Text;
    Result.SSHPassword := editSSHpassword.Text;
    Result.SSHTimeout := updownSSHTimeout.Position;
    Result.SSHPrivateKey := editSSHPrivateKey.Text;
    Result.SSHLocalPort := MakeInt(editSSHlocalport.Text);
    Result.SSHExe := comboSSHExe.Text;
    Result.WantSSL := chkWantSSL.Checked;
    Result.SSLPrivateKey := editSSLPrivateKey.Text;
    Result.SSLCertificate := editSSLCertificate.Text;
    Result.SSLCACertificate := editSSLCACertificate.Text;
    Result.SSLCipher := editSSLCipher.Text;
    Result.SSLVerification := comboSSLVerification.ItemIndex;
    Result.StartupScriptFilename := editStartupScript.Text;
    Result.Compressed := chkCompressed.Checked;
    Result.QueryTimeout := updownQueryTimeout.Position;
    Result.KeepAlive := updownKeepAlive.Position;
    Result.LocalTimeZone := chkLocalTimeZone.Checked;
    Result.FullTableStatus := chkFullTableStatus.Checked;
    Result.SessionColor := ColorBoxBackgroundColor.Selected;
    Result.IgnoreDatabasePattern := editIgnoreDatabasePattern.Text;
    Result.LogFileDdl := chkLogFileDdl.Checked;
    Result.LogFileDml := chkLogFileDml.Checked;
    Result.LogFilePath := editLogFilePath.Text;
  end;
end;


procedure Tconnform.ListSessionsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
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


procedure Tconnform.ListSessionsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Session: PConnectionParameters;
begin
  // Paint custom background color
  if CellPaintMode=cpmPaint then begin
    Session := Sender.GetNodeData(Node);
    if Session.SessionColor <> AppSettings.GetDefaultInt(asTreeBackground) then begin
      TargetCanvas.Brush.Color := Session.SessionColor;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure Tconnform.ListSessionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  // Use our own text editor to rename a session
  EditLink := TInplaceEditorLink.Create(Sender as TVirtualStringTree, True, nil);
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
    and (Mode <> dmNowhere);
  if Accept and (Mode = dmOnNode) and (TargetNode = ListSessions.FocusedNode.Parent) then
    Accept := False;
  if Accept and (Mode in [dmAbove, dmBelow]) and (TargetNode.Parent = ListSessions.FocusedNode.Parent) then
    Accept := False;
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
  tabSSL.TabVisible := SessionFocused;
  tabStatistics.TabVisible := SessionFocused;
  menuRename.Enabled := Assigned(Node);
  menuNewSessionInFolder.Enabled := InFolder;
  menuNewFolderInFolder.Enabled := InFolder;
  FreeAndNil(FPopupDatabases);
  FreeAndNil(FPopupCiphers);

  if not SessionFocused then begin
    PageControlDetails.ActivePage := tabStart;
    if ListSessions.RootNodeCount = 0 then
      lblHelp.Caption := f_('New here? In order to connect to a server, you have to create a so called '+
        '"session" at first. Just click the "New" button on the bottom left to create your first session. '+
        'Give it a friendly name (e.g. "Local DB server") so you''ll recall it the next time you start %s.', [APPNAME])
    else
      lblHelp.Caption := _('Please click a session on the left list to edit parameters, doubleclick to open it.');
  end else begin
    PageControlDetails.ActivePage := tabSettings;

    SelectedNetType := Sess.NetType;
    FLastSelectedNetTypeGroup := Sess.NetTypeGroup;
    editHost.Text := Sess.Hostname;
    editUsername.Text := Sess.Username;
    editPassword.Text := Sess.Password;
    chkLoginPrompt.Checked := Sess.LoginPrompt;
    chkWindowsAuth.Checked := Sess.WindowsAuth;
    chkCleartextPluginEnabled.Checked := Sess.CleartextPluginEnabled;
    updownPort.Position := Sess.Port;
    chkCompressed.Checked := Sess.Compressed;
    updownQueryTimeout.Position := Sess.QueryTimeout;
    updownKeepAlive.Position := Sess.KeepAlive;
    chkLocalTimeZone.Checked := Sess.LocalTimeZone;
    chkFullTableStatus.Checked := Sess.FullTableStatus;
    ColorBoxBackgroundColor.Items.Objects[0] := TObject(Sess.SessionColor);
    if Sess.SessionColor = clNone then
      ColorBoxBackgroundColor.Selected := Sess.SessionColor
    else
      ColorBoxBackgroundColor.ItemIndex := 0;
    editDatabases.Text := Sess.AllDatabasesStr;
    comboLibrary.Items := Sess.GetLibraries;
    comboLibrary.ItemIndex := comboLibrary.Items.IndexOf(Sess.LibraryOrProvider);
    if (comboLibrary.ItemIndex = -1) and (comboLibrary.Items.Count > 0) then begin
      comboLibrary.ItemIndex := 0;
    end;
    memoComment.Text := Sess.Comment;
    editStartupScript.Text := Sess.StartupScriptFilename;
    chkSSHActive.Checked := Sess.SSHActive;
    comboSSHExe.Text := Sess.SSHExe;
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
    editSSLCipher.Text := Sess.SSLCipher;
    comboSSLVerification.ItemIndex := Sess.SSLVerification;
    editIgnoreDatabasePattern.Text := Sess.IgnoreDatabasePattern;
    chkLogFileDdl.Checked := Sess.LogFileDdl;
    chkLogFileDml.Checked := Sess.LogFileDml;
    editLogFilePath.Text := Sess.LogFilePath;
    FServerVersion := Sess.ServerVersion;
  end;

  FLoaded := True;
  FSessionModified := False;
  ListSessions.Repaint;
  ValidateControls;
  TimerStatistics.Enabled := True;
  TimerStatistics.OnTimer(Sender);

  Screen.Cursor := crDefault;
end;


procedure Tconnform.RefreshBackgroundColors;
begin
  // Trigger OnGetColors event
  ColorBoxBackgroundColor.Style := ColorBoxBackgroundColor.Style - [cbCustomColors];
  ColorBoxBackgroundColor.Style := ColorBoxBackgroundColor.Style + [cbCustomColors];
end;


procedure Tconnform.TimerStatisticsTimer(Sender: TObject);
var
  LastConnect, Created: TDateTime;
begin
  // Continuously update statistics labels
  lblLastConnectRight.Caption := _('unknown or never');
  lblLastConnectRight.Hint := '';
  lblLastConnectRight.Enabled := False;
  lblCreatedRight.Caption := _('unknown');
  lblCreatedRight.Hint := '';
  lblCreatedRight.Enabled := False;
  lblCounterRight1.Caption := '';
  lblCounterRight2.Caption := '';

  if not Assigned(ListSessions.FocusedNode) then
    Exit;

  AppSettings.SessionPath := SelectedSessionPath;
  if AppSettings.SessionPath.IsEmpty then
    Exit;

  LastConnect := StrToDateTimeDef(AppSettings.ReadString(asLastConnect), DateTimeNever);
  if LastConnect <> DateTimeNever then begin
    lblLastConnectRight.Hint := DateTimeToStr(LastConnect);
    lblLastConnectRight.Caption := DateBackFriendlyCaption(LastConnect);
    lblLastConnectRight.Enabled := True;
  end;
  Created := StrToDateTimeDef(AppSettings.ReadString(asSessionCreated), DateTimeNever);
  if Created <> DateTimeNever then begin
    lblCreatedRight.Hint := DateTimeToStr(Created);
    lblCreatedRight.Caption := DateBackFriendlyCaption(Created);
    lblCreatedRight.Enabled := True;
  end;
  lblCounterRight1.Caption := FormatNumber(AppSettings.ReadInt(asConnectCount));
  lblCounterRight2.Caption := FormatNumber(AppSettings.ReadInt(asRefusedCount));
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
  // Note that this is triggered only if the text was effectively changed
  Sess := Sender.GetNodeData(Node);

  SiblingSessions := NodeSessionNames(Node.Parent, ParentKey);

  if SiblingSessions.IndexOf(NewText) > -1 then begin
    ErrorDialog(
      f_('Session "%s" already exists!', [ParentKey+NewText])
      + sLineBreak + sLineBreak
      + _('If you want to change the case of a session name, you need to rename it before doing the actual case change.')
      );
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


procedure Tconnform.ListSessionsNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
const
  AllowedPos: THitPositions=[hiOnItemLabel, hiOnItemLeft, hiOnItemRight, hiOnNormalIcon];
var
  HitPos: THitPosition;
begin
  // Doubleclick to open a connection, only if mouse is really on a node,
  // not e.g. on the expand/collapse icon (see issue #820)
  for HitPos in HitInfo.HitPositions do begin
    if HitPos in AllowedPos then begin
      btnOpen.OnClick(Sender);
      Break;
    end;
  end;
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


procedure Tconnform.editHostDblClick(Sender: TObject);
begin
  if CurrentParams.NetType = ntSQLite then
    PickFile(Sender);
end;


procedure Tconnform.editTrim(Sender: TObject);
var
  Edit: TCustomEdit;
  Trimmed: String;
begin
  // Trim input
  Edit := Sender as TCustomEdit;
  Trimmed := Edit.Text;
  Trimmed := Trimmed.Trim([' ', #9]);
  if Edit.Text <> Trimmed then begin
    Edit.Text := Trimmed;
  end;
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


procedure Tconnform.chkSSHActiveClick(Sender: TObject);
begin
  if (comboSSHExe.Text = '') and (comboSSHExe.Items.Count > 0) then
    comboSSHExe.ItemIndex := 0;
  Modification(Sender);
end;

procedure Tconnform.ColorBoxBackgroundColorGetColors(Sender: TCustomColorBox;
  Items: TStrings);
var
  Node: PVirtualNode;
  PParams: PConnectionParameters;
  ColorName,
  ColorNamePrefix: String;
begin
  // Collect custom session colors into color selector
  ColorNamePrefix := _('Custom color:') + ' ';
  Node := ListSessions.GetFirst;
  while Assigned(Node) do begin
    PParams := ListSessions.GetNodeData(Node);
    ColorName := ColorNamePrefix + ColorToWebColorStr(PParams.SessionColor);
    if (PParams.SessionColor <> clNone) and (Items.IndexOf(ColorName) = -1) then begin
      Items.AddObject(ColorName, TObject(PParams.SessionColor));
    end;
    Node := ListSessions.GetNext(Node);
  end;
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
      if Params.NetTypeGroup = ngPgSQL then
        Databases := Connection.GetCol('SELECT datname FROM pg_database WHERE datistemplate=FALSE')
      else
        Databases := Connection.AllDatabases;
      for DB in Databases do begin
        Item := TMenuItem.Create(FPopupDatabases);
        Item.Caption := DB;
        Item.OnClick := MenuDatabasesClick;
        Item.AutoCheck := True;
        Item.RadioItem := Params.NetTypeGroup = ngPgSQL;
        FPopupDatabases.Items.Add(Item);
      end;
      Databases.Free;
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
  Databases := TStringList.Create;
  for Item in FPopupDatabases.Items do begin
    if Item.Checked then
      Databases.Add(Item.Caption);
  end;
  SelStart := editDatabases.SelStart;
  editDatabases.Text := Implode(';', Databases);
  editDatabases.SelStart := SelStart;
end;


procedure Tconnform.MenuCiphersClick(Sender: TObject);
begin
  editUsername.Text := TMenuItem(Sender).Caption;
end;


procedure Tconnform.editUsernameRightButtonClick(Sender: TObject);
var
  Params: TConnectionParameters;
  Item: TMenuItem;
  LibraryPath: String;
  Lib: TSQLiteLib;
  p: TPoint;
  i: Integer;
begin
  // Provide supported cipher names
  if FPopupCiphers = nil then begin
    FPopupCiphers := TPopupMenu.Create(Self);
    FPopupCiphers.AutoHotkeys := maManual;
    Params := CurrentParams;
    LibraryPath := ExtractFilePath(ParamStr(0)) + Params.LibraryOrProvider;
    // Throws EDbError on any failure:
    Lib := TSQLiteLib.CreateWithMultipleCipherFunctions(LibraryPath, Params.DefaultLibrary);
    for i:=1 to Lib.sqlite3mc_cipher_count() do begin
      Item := TMenuItem.Create(FPopupCiphers);
      Item.Caption := Utf8ToString(Lib.sqlite3mc_cipher_name(i));
      Item.OnClick := MenuCiphersClick;
      FPopupCiphers.Items.Add(Item);
    end;

  end;

  p := editUsername.ClientToScreen(editUsername.ClientRect.BottomRight);
  FPopupCiphers.Popup(p.X-editUsername.Images.Width, p.Y);
end;


procedure Tconnform.menuRenameClick(Sender: TObject);
begin
  // Start node editor to rename a session
  ListSessions.EditNode(ListSessions.FocusedNode, ListSessions.Header.MainColumn);
end;


procedure Tconnform.comboNetTypeChange(Sender: TObject);
var
  Params: TConnectionParameters;
  Libs: TStringList;
begin
  // Autoset default connection data as long as that was not modified by user
  // and only if net type group has now changed
  if not FLoaded then
    Exit;

  Params := CurrentParams;

  if Params.NetTypeGroup <> FLastSelectedNetTypeGroup then begin
    if not editPort.Modified then
      updownPort.Position := Params.DefaultPort;
    if not editUsername.Modified then
      editUsername.Text := Params.DefaultUsername;
    if not editIgnoreDatabasePattern.Modified then
      editIgnoreDatabasePattern.Text := Params.DefaultIgnoreDatabasePattern;
    if not editHost.Modified then
      editHost.Text := Params.DefaultHost;
    chkSSHActive.Checked := Params.DefaultSshActive;
  end;

  // Populate libraries combobox. Required on each net group change, and also between
  // SQLite and SQLite-encrypted.
  Libs := Params.GetLibraries;
  mainform.LogSQL(Libs.CommaText);
  if Libs.Text <> comboLibrary.Items.Text then begin
    comboLibrary.Items := Libs;
    comboLibrary.ItemIndex := comboLibrary.Items.IndexOf(Params.DefaultLibrary);
  end;

  FLastSelectedNetTypeGroup := Params.NetTypeGroup;
  FreeAndNil(Params);
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
    if Sess = nil then
      Exit;

    FSessionModified := (Sess.Hostname <> editHost.Text)
      or (Sess.Username <> editUsername.Text)
      or (Sess.LoginPrompt <> chkLoginPrompt.Checked)
      or (Sess.WindowsAuth <> chkWindowsAuth.Checked)
      or (Sess.CleartextPluginEnabled <> chkCleartextPluginEnabled.Checked)
      or (Sess.Port <> updownPort.Position)
      or (Sess.Compressed <> chkCompressed.Checked)
      or (Sess.QueryTimeout <> updownQueryTimeout.Position)
      or (Sess.KeepAlive <> updownKeepAlive.Position)
      or (Sess.LocalTimeZone <> chkLocalTimeZone.Checked)
      or (Sess.FullTableStatus <> chkFullTableStatus.Checked)
      or (Sess.SessionColor <> ColorBoxBackgroundColor.Selected)
      or (Sess.NetType <> SelectedNetType)
      or (Sess.StartupScriptFilename <> editStartupScript.Text)
      or (Sess.LibraryOrProvider <> comboLibrary.Text)
      or (Sess.AllDatabasesStr <> editDatabases.Text)
      or (Sess.Comment <> memoComment.Text)
      or (Sess.SSHActive <> chkSSHActive.Checked)
      or (Sess.SSHHost <> editSSHHost.Text)
      or (IntToStr(Sess.SSHPort) <> editSSHPort.Text)
      or (Sess.SSHExe <> comboSSHExe.Text)
      or (IntToStr(Sess.SSHLocalPort) <> editSSHlocalport.Text)
      or (Sess.SSHUser <> editSSHUser.Text)
      or (Sess.SSHPassword <> editSSHPassword.Text)
      or (Sess.SSHTimeout <> updownSSHTimeout.Position)
      or (Sess.SSHPrivateKey <> editSSHPrivateKey.Text)
      or (Sess.WantSSL <> chkWantSSL.Checked)
      or (Sess.SSLPrivateKey <> editSSLPrivateKey.Text)
      or (Sess.SSLCertificate <> editSSLCertificate.Text)
      or (Sess.SSLCACertificate <> editSSLCACertificate.Text)
      or (Sess.SSLCipher <> editSSLCipher.Text)
      or (Sess.SSLVerification <> comboSSLVerification.ItemIndex)
      or (Sess.IgnoreDatabasePattern <> editIgnoreDatabasePattern.Text)
      or (Sess.LogFileDdl <> chkLogFileDdl.Checked)
      or (Sess.LogFileDml <> chkLogFileDml.Checked)
      or (Sess.LogFilePath <> editLogFilePath.Text)
      ;
    PasswordModified := Sess.Password <> editPassword.Text;
    FOnlyPasswordModified := PasswordModified and (not FSessionModified);
    FSessionModified := FSessionModified or PasswordModified;
    if (Sender=editHost) or (Sender=editUsername) or (Sender=editPassword) or
      (Sender=comboNetType) or (Sender=chkWindowsAuth) or (Sender=editPort) or
      (Sender=chkCleartextPluginEnabled) then begin
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


procedure Tconnform.FindAddDatabaseFilesClick(Sender: TObject);
var
  PrevText: String;
begin
  // Append or replace filenames
  PrevText := editHost.Text;
  PickFile(editHost);
  if (Sender = menuAddDatabaseFiles)
    and (not PrevText.IsEmpty)
    and (editHost.Text <> PrevText)
    and (editHost.Text <> '') then begin
    editHost.Text := PrevText + DELIM + editHost.Text;
  end;
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
      // Validate session GUI stuff on "Settings" tab:
      lblHost.Caption := _('Hostname / IP:');
      lblUsername.Caption := _('User')+':';
      lblPassword.Caption := _('Password:');
      lblDatabase.Caption := _('Databases')+':';
      editDatabases.TextHint := _('Separated by semicolon');
      case Params.NetType of
        ntMySQL_NamedPipe: begin
          lblHost.Caption := _('Socket name:');
        end;
        ntPgSQL_TCPIP, ntPgSQL_SSHtunnel: begin
          lblDatabase.Caption := _('Database')+':';
          editDatabases.TextHint := _('Single database name');
        end;
        ntSQLite, ntSQLiteEncrypted: begin
          lblHost.Caption := _('Database filename(s)')+':';
          lblUsername.Caption := _('Cipher')+':';
          lblPassword.Caption := _('Key:');
          lblDatabase.Caption := _('Encryption parameters')+':';
          editDatabases.TextHint := _('Example:') + ' kdf_iter=4000;legacy=1;...';
        end
      end;
      editHost.RightButton.Visible := Params.IsAnySQLite;
      chkLoginPrompt.Enabled := Params.NetTypeGroup in [ngMySQL, ngMSSQL, ngPgSQL];
      chkWindowsAuth.Enabled := Params.IsAnyMSSQL or Params.IsAnyMySQL;
      lblUsername.Enabled := (Params.NetTypeGroup in [ngMySQL, ngMSSQL, ngPgSQL, ngInterbase])
        and ((not chkLoginPrompt.Checked) or (not chkLoginPrompt.Enabled))
        and ((not chkWindowsAuth.Checked) or (not chkWindowsAuth.Enabled));
      lblUsername.Enabled := lblUsername.Enabled or (Params.NetType = ntSQLiteEncrypted);
      editUsername.Enabled := lblUsername.Enabled;
      editUsername.RightButton.Visible := Params.NetType = ntSQLiteEncrypted;
      lblPassword.Enabled := lblUsername.Enabled;
      editPassword.Enabled := lblUsername.Enabled;
      lblPort.Enabled := Params.NetType in [ntMySQL_TCPIP, ntMySQL_SSHtunnel, ntMySQL_ProxySQLAdmin, ntMySQL_RDS, ntMSSQL_TCPIP, ntPgSQL_TCPIP, ntPgSQL_SSHtunnel, ntInterbase_TCPIP, ntFirebird_TCPIP];
      editPort.Enabled := lblPort.Enabled;
      updownPort.Enabled := lblPort.Enabled;
      chkCompressed.Enabled := Params.IsAnyMySQL;
      lblDatabase.Enabled := Params.NetTypeGroup in [ngMySQL, ngMSSQL, ngPgSQL, ngInterbase];
      lblDatabase.Enabled := lblDatabase.Enabled or (Params.NetType = ntSQLiteEncrypted);
      editDatabases.Enabled := lblDatabase.Enabled;
      editDatabases.RightButton.Visible := Params.NetTypeGroup in [ngMySQL, ngMSSQL, ngPgSQL, ngInterbase];
      // SSH tunnel tab:
      chkSSHActive.Enabled := Params.SshSupport;
      lblSSHExe.Enabled := Params.SSHActive;
      comboSSHExe.Enabled := Params.SSHActive;
      lblSSHhost.Enabled := Params.SSHActive;
      editSSHhost.Enabled := Params.SSHActive;
      editSSHport.Enabled := Params.SSHActive;
      lblSSHUser.Enabled := Params.SSHActive;
      editSSHUser.Enabled := Params.SSHActive;
      lblSSHPassword.Enabled := Params.SSHActive;
      editSSHPassword.Enabled := Params.SSHActive;
      lblSSHTimeout.Enabled := Params.SSHActive;
      editSSHTimeout.Enabled := Params.SSHActive;
      updownSSHTimeout.Enabled := Params.SSHActive;
      lblSSHkeyfile.Enabled := Params.SSHActive;
      editSSHPrivateKey.Enabled := Params.SSHActive;
      lblSSHLocalPort.Enabled := Params.SSHActive;
      editSSHlocalport.Enabled := Params.SSHActive;
      // Advanced tab:
      chkWantSSL.Enabled := Params.NetType in [ntMySQL_TCPIP, ntMySQL_SSHtunnel, ntMySQL_ProxySQLAdmin, ntMySQL_RDS, ntPgSQL_TCPIP, ntPgSQL_SSHtunnel];
      lblSSLPrivateKey.Enabled := Params.WantSSL;
      editSSLPrivateKey.Enabled := Params.WantSSL;
      lblSSLCACertificate.Enabled := Params.WantSSL;
      editSSLCACertificate.Enabled := Params.WantSSL;
      lblSSLCertificate.Enabled := Params.WantSSL;
      editSSLCertificate.Enabled := Params.WantSSL;
      lblSSLcipher.Enabled := Params.WantSSL;
      editSSLcipher.Enabled := Params.WantSSL;
      lblSSLVerification.Enabled := Params.WantSSL;
      comboSSLVerification.Enabled := Params.WantSSL;
      lblQueryTimeout.Enabled := True;
      editQueryTimeout.Enabled := lblQueryTimeout.Enabled;
      updownQueryTimeout.Enabled := lblQueryTimeout.Enabled;
      chkLocalTimeZone.Enabled := Params.NetTypeGroup = ngMySQL;
      chkFullTableStatus.Enabled := (Params.NetTypeGroup in [ngMySQL, ngPgSQL]) and (Params.NetType <> ntMySQL_ProxySQLAdmin);
      chkCleartextPluginEnabled.Enabled := Params.NetTypeGroup = ngMySQL;
      editLogFilePath.Enabled := Params.LogFileDdl or Params.LogFileDml;

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
  TExtForm.PageControlTabHighlight(PageControlDetails);

  Caption := GetWindowCaption;
end;


procedure Tconnform.splitterMainMoved(Sender: TObject);
var
  HorizSpace, ButtonWidth: Integer;
begin
  // Splitter resized - adjust width of bottom left buttons
  ButtonWidth := Round((pnlLeft.Width - 2 * pnlLeft.Margins.Left) / 3);
  btnNew.Width := ButtonWidth;
  btnSave.Width := ButtonWidth;
  btnDelete.Width := ButtonWidth;
  btnNew.Left := pnlLeft.Left;
  btnSave.Left := btnNew.Left + btnNew.Width + pnlLeft.Margins.Left;
  btnDelete.Left := btnSave.Left + btnSave.Width + pnlLeft.Margins.Left;

  // Resize bottom right buttons
  HorizSpace := PageControlDetails.Width - 2 * PageControlDetails.Margins.Right;
  ButtonWidth := Round(HorizSpace / 3);
  ButtonWidth := Max(ButtonWidth, ScaleSize(50));
  ButtonWidth := Min(ButtonWidth, ScaleSize(100));
  btnMore.Width := ButtonWidth;
  btnCancel.Width := ButtonWidth;
  btnOpen.Width := ButtonWidth;
  btnmore.Left := PageControlDetails.Left + PageControlDetails.Width - btnMore.Width;
  btnCancel.Left := btnMore.Left - btnMore.Width - PageControlDetails.Margins.Right;
  btnOpen.Left := btnCancel.Left - btnCancel.Width - PageControlDetails.Margins.Right;
end;


procedure Tconnform.TimerButtonAnimationTimer(Sender: TObject);
const
  MaxAnimationSteps = 3;
begin
  // Animate "Open" button
  btnOpen.Caption := _('Open') + StringOfChar(' ', FButtonAnimationStep) +
    '.' +
    StringOfChar(' ', MaxAnimationSteps-FButtonAnimationStep);
  btnOpen.Repaint;
  Inc(FButtonAnimationStep);
  if FButtonAnimationStep >= MaxAnimationSteps then
    FButtonAnimationStep := 0;
end;

procedure Tconnform.PageControlDetailsChange(Sender: TObject);
begin
  ValidateControls;
end;

procedure Tconnform.PickFile(Sender: TObject);
var
  Selector: TOpenDialog;
  Edit: TButtonedEdit;
  i: Integer;
  Control: TControl;
  FileNames: TStringList;
begin
  // Select startup SQL file, SSL file or whatever button clicked
  Edit := Sender as TButtonedEdit;
  Selector := TOpenDialog.Create(Self);
  if Edit = editHost then begin
    Selector.Filter := 'SQLite databases ('+FILEFILTER_SQLITEDB+')|'+FILEFILTER_SQLITEDB+'|'+_('All files')+' (*.*)|*.*';
    Selector.Options := Selector.Options - [ofFileMustExist];
    Selector.Options := Selector.Options + [ofAllowMultiSelect];
    Selector.DefaultExt := FILEEXT_SQLITEDB;
  end else if (Edit = editStartupScript) or (Edit = editLogFilePath) then
    Selector.Filter := _('SQL files')+' (*.sql)|*.sql|'+_('All files')+' (*.*)|*.*'
  else if Edit = editSSHPrivateKey then
    Selector.Filter := _('All files')+' (*.*)|*.*'
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
  // Set initial directory to the one from the edit's file
  Selector.InitialDir := ExtractFilePath(Edit.Text);
  if Selector.InitialDir.IsEmpty then
    Selector.InitialDir := TPath.GetPathRoot(Application.ExeName);
  if Selector.Execute then begin
    FileNames := TStringList.Create;
    FileNames.Assign(Selector.Files);
    for i:=0 to FileNames.Count-1 do begin
      // Remove path if it's the application directory
      if ExtractFilePath(FileNames[i]) = ExtractFilePath(Application.ExeName) then
        FileNames[i] := ExtractFileName(FileNames[i]);
    end;
    Edit.Text := Implode(DELIM, FileNames);
    Modification(Selector);
  end;
  Selector.Free;
end;


procedure Tconnform.editSearchChange(Sender: TObject);
begin
  // Filter session nodes - start delay
  timerEditFilterDelay.Enabled := False;
  timerEditFilterDelay.Enabled := True;
end;

procedure Tconnform.timerEditFilterDelayTimer(Sender: TObject);
begin
  // Filter session nodes
  FilterNodesByEdit(editSearch, ListSessions);
  timerEditFilterDelay.Enabled := False;
end;

procedure Tconnform.editSearchRightButtonClick(Sender: TObject);
begin
  editSearch.Clear;
end;

end.
