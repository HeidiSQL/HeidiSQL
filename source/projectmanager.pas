unit projectmanager;

interface

uses
  System.Classes, System.SysUtils, System.Math, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Menus, Vcl.Graphics, 
  Vcl.Dialogs, Vcl.Clipbrd, System.IOUtils, Vcl.FileCtrl, ShellAPI, Windows, System.IniFiles,
  System.Generics.Collections, VirtualTrees, VirtualTrees.Types, VirtualTrees.BaseTree, 
  apphelpers, System.UITypes, gnugettext, ComObj, ShlObj, ActiveX;

type
  // Project folder information
  TProjectFolder = class
  public
    Name: string;
    Path: string;
    constructor Create(const AName, APath: string);
  end;

  // Node data for VirtualTree
  PProjectNodeData = ^TProjectNodeData;
  TProjectNodeData = record
    NodeType: (ntProject, ntFolder, ntFile);
    ProjectIndex: Integer;         // Only for ntProject - index into FProjectFolders
    FullPath: string;              // For ntFolder and ntFile
    DisplayName: string;           // Display text
  end;

const
  // UI Layout Constants
  HEADER_PANEL_HEIGHT = 26;
  TOOLBAR_HEIGHT = 28;
  TOOLBAR_BUTTON_HEIGHT = 21;
  TOOLBAR_BUTTON_WIDTH = 22;
  DEFAULT_PANEL_WIDTH = 300;
  DEFAULT_PANEL_HEIGHT = 200;
  DEFAULT_NODE_HEIGHT = 20;
  HEADER_LABEL_LEFT = 8;
  HEADER_LABEL_TOP = 6;
  
  // File handling
  MAX_FILES_PER_FOLDER = 100;
  
  // System icons (using HeidiSQL's VirtualImageListMain indices)
  ICON_ADD_PROJECT = 45;        // Same as actDataInsert
  ICON_REMOVE_PROJECT = 46;     // Same as actDataDelete

type
  // Project Manager Panel - rechts positioniert wie andere Tool-Panels
  TProjectManagerPanel = class(TPanel)
  private
    FToolBar: TToolBar;
    FTreeView: TVirtualStringTree;
    FHeaderPanel: TPanel;
    FHeaderLabel: TLabel;
    FBtnAddProject: TToolButton;
    FBtnRemoveProject: TToolButton;
    FBtnRenameProject: TToolButton;
    FBtnRefresh: TToolButton;
    FPopupMenu: TPopupMenu;
    FControlsCreated: Boolean;
    FProjectFolders: TObjectList<TProjectFolder>;
    
    procedure CreateControls;
    procedure SetupToolbar;
    procedure SetupTreeView;
    procedure SetupPopupMenu;
    procedure BtnAddProjectClick(Sender: TObject);
    procedure BtnRemoveProjectClick(Sender: TObject);
    procedure BtnRenameProjectClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeViewGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure TreeViewInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure TreeViewInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure TreeViewGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure TreeViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeViewDblClick(Sender: TObject);
    procedure TreeViewKeyPress(Sender: TObject; var Key: Char);
    procedure TreeViewPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure PopupOpenFileClick(Sender: TObject);
    procedure PopupOpenWithSystemEditorClick(Sender: TObject);
    procedure PopupOpenFolderClick(Sender: TObject);
    procedure PopupCopyPathClick(Sender: TObject);
    procedure PopupRemoveProjectClick(Sender: TObject);
    procedure PopupRenameProjectClick(Sender: TObject);
    
    procedure LoadProjects;
    procedure SaveProjects;
    procedure RefreshProjectTree;
    function InitProjectsIniFile: TIniFile;
    function IsTextFile(const FileName: string): Boolean;
    function IsFileOpenInTab(const FilePath: string): Boolean;
    procedure OpenFileInQueryTab(const FilePath: string);
    procedure UpdateTreeAppearance; // Update visual state without rebuilding tree
    function IsShuttingDown: Boolean; // Check if component is being destroyed
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareForShutdown; // Call this before application shutdown
    procedure ToggleVisibility; // Toggle panel visibility
  end;

implementation

uses main;

// Modern Windows folder selection dialog interfaces and constants
const
  FOS_PICKFOLDERS = $00000020;
  FOS_FORCEFILESYSTEM = $00000040;
  FOS_PATHMUSTEXIST = $00000800;
  SIGDN_FILESYSPATH = $80058000;

type
  IShellItem = interface(IUnknown)
    ['{43826d1e-e718-42ee-bc55-a1e261c37bfe}']
    function BindToHandler(const pbc: IBindCtx; const bhid: TGUID; const riid: TGUID; out ppv): HRESULT; stdcall;
    function GetParent(out ppsi: IShellItem): HRESULT; stdcall;
    function GetDisplayName(sigdnName: DWORD; out ppszName: PWideChar): HRESULT; stdcall;
    function GetAttributes(sfgaoMask: DWORD; out psfgaoAttribs: DWORD): HRESULT; stdcall;
    function Compare(const psi: IShellItem; hint: DWORD; out piOrder: Integer): HRESULT; stdcall;
  end;

  IFileDialog = interface(IModalWindow)
    ['{42f85136-db7e-439c-85f1-e4075d135fc8}']
    function SetFileTypes(cFileTypes: UINT; const rgFilterSpec): HRESULT; stdcall;
    function SetFileTypeIndex(iFileType: UINT): HRESULT; stdcall;
    function GetFileTypeIndex(out piFileType: UINT): HRESULT; stdcall;
    function Advise(const pfde: IUnknown; out pdwCookie: DWORD): HRESULT; stdcall;
    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;
    function SetOptions(fos: DWORD): HRESULT; stdcall;
    function GetOptions(out pfos: DWORD): HRESULT; stdcall;
    function SetDefaultFolder(const psi: IShellItem): HRESULT; stdcall;
    function SetFolder(const psi: IShellItem): HRESULT; stdcall;
    function GetFolder(out ppsi: IShellItem): HRESULT; stdcall;
    function GetCurrentSelection(out ppsi: IShellItem): HRESULT; stdcall;
    function SetFileName(pszName: PWideChar): HRESULT; stdcall;
    function GetFileName(out pszName: PWideChar): HRESULT; stdcall;
    function SetTitle(pszTitle: PWideChar): HRESULT; stdcall;
    function SetOkButtonLabel(pszText: PWideChar): HRESULT; stdcall;
    function SetFileNameLabel(pszLabel: PWideChar): HRESULT; stdcall;
    function GetResult(out ppsi: IShellItem): HRESULT; stdcall;
    function AddPlace(const psi: IShellItem; fdap: DWORD): HRESULT; stdcall;
    function SetDefaultExtension(pszDefaultExtension: PWideChar): HRESULT; stdcall;
    function Close(hr: HRESULT): HRESULT; stdcall;
    function SetClientGuid(const guid: TGUID): HRESULT; stdcall;
    function ClearClientData: HRESULT; stdcall;
    function SetFilter(const pFilter: IUnknown): HRESULT; stdcall;
  end;

  IFileOpenDialog = interface(IFileDialog)
    ['{d57c7288-d4ad-4768-be02-9d969532d960}']
    function GetResults(out ppenum: IUnknown): HRESULT; stdcall;
    function GetSelectedItems(out ppsai: IUnknown): HRESULT; stdcall;
  end;

  IModalWindow = interface(IUnknown)
    ['{b4db1657-70d7-485e-8e3e-6fcb5a5c1802}']
    function Show(hwndOwner: HWND): HRESULT; stdcall;
  end;

const
  CLSID_FileOpenDialog: TGUID = '{DC1C5A9C-E88A-4dde-A5A1-60F82A20AEF7}';

{ TProjectFolder }

constructor TProjectFolder.Create(const AName, APath: string);
begin
  inherited Create;
  Name := AName;
  Path := APath;
end;

{ TProjectManagerPanel }

constructor TProjectManagerPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  // Basic panel setup - positioned in right panel at bottom
  Align := alBottom; // Position at bottom of right panel
  Caption := ''; // No caption to avoid visual clutter
  BevelOuter := bvNone; // Remove border to match other panels
  BevelInner := bvNone; // Remove inner border
  BorderStyle := bsNone; // Remove border completely
  Color := GetThemeColor(clWindow); // Use theme-appropriate background
  Width := DEFAULT_PANEL_WIDTH; // Use constant
  Height := DEFAULT_PANEL_HEIGHT; // Use constant
  Visible := True; // Default visible
  
  // Initialize project folders list
  FProjectFolders := TObjectList<TProjectFolder>.Create(True); // True = owns objects
  
  // Components will be created in CreateWnd when the handle is available
  FControlsCreated := False;
end;

procedure TProjectManagerPanel.CreateWnd;
begin
  inherited CreateWnd;
  
  // Create sub-components only after handle creation
  if not FControlsCreated then begin
    try
      CreateControls;
      LoadProjects;
      RefreshProjectTree;
      FControlsCreated := True;
    except
      on E: Exception do
      begin
        ErrorDialog(_('Error in CreateWnd: ') + E.Message);
      end;
    end;
  end;
end;

destructor TProjectManagerPanel.Destroy;
begin
  try
    // Save projects before cleanup
    if Assigned(FProjectFolders) then
      SaveProjects;
      
    // Clear tree view safely before destroying
    if Assigned(FTreeView) then
    begin
      // First, disconnect all event handlers to prevent callbacks during destruction
      FTreeView.OnGetText := nil;
      FTreeView.OnGetHint := nil;
      FTreeView.OnInitNode := nil;
      FTreeView.OnInitChildren := nil;
      FTreeView.OnGetImageIndex := nil;
      FTreeView.OnFreeNode := nil;
      FTreeView.OnDblClick := nil;
      FTreeView.OnKeyPress := nil;
      FTreeView.OnPaintText := nil;
      
      // Clear the tree content before destroying
      FTreeView.Clear;
      
      // Ensure parent relationship is cleared
      if Assigned(FTreeView.Parent) then
        FTreeView.Parent := nil;
    end;
    
    // Clean up project folders - TObjectList will free objects automatically
    FreeAndNil(FProjectFolders);
    
    // Disconnect from parent to avoid double-free issues
    if Assigned(Parent) then
      Parent := nil;
      
  except
    on E: Exception do
    begin
      // Suppress any errors during destruction to prevent access violations
      // but in debug mode, we might want to know about them
      {$IFDEF DEBUG}
      OutputDebugString(PChar('Error in TProjectManagerPanel.Destroy: ' + E.Message));
      {$ENDIF}
    end;
  end;
  
  try
    inherited Destroy;
  except
    on E: Exception do
    begin
      // Final safety net - log but don't re-raise
      {$IFDEF DEBUG}
      OutputDebugString(PChar('Error in inherited Destroy: ' + E.Message));
      {$ENDIF}
    end;
  end;
end;

procedure TProjectManagerPanel.PrepareForShutdown;
begin
  try
    // Save projects before shutdown
    if Assigned(FProjectFolders) then
      SaveProjects;
      
    // Clear event handlers to prevent access violations during shutdown
    if Assigned(FTreeView) then
    begin
      FTreeView.OnGetText := nil;
      FTreeView.OnGetHint := nil;
      FTreeView.OnInitNode := nil;
      FTreeView.OnInitChildren := nil;
      FTreeView.OnGetImageIndex := nil;
      FTreeView.OnFreeNode := nil;
      FTreeView.OnDblClick := nil;
      FTreeView.OnKeyPress := nil;
      FTreeView.OnPaintText := nil;
      FTreeView.Clear;
    end;
  except
    // Suppress any errors during shutdown preparation
  end;
end;

procedure TProjectManagerPanel.CreateControls;
begin
  // Header Panel with title
  FHeaderPanel := TPanel.Create(Self);
  FHeaderPanel.Parent := Self;
  FHeaderPanel.Align := alTop;
  FHeaderPanel.Height := HEADER_PANEL_HEIGHT;
  FHeaderPanel.BevelOuter := bvNone;
  FHeaderPanel.BevelInner := bvNone;
  FHeaderPanel.BorderStyle := bsNone;
  FHeaderPanel.Color := GetThemeColor(clWindow);
  FHeaderPanel.Caption := '';
  
  FHeaderLabel := TLabel.Create(Self);
  FHeaderLabel.Parent := FHeaderPanel;
  FHeaderLabel.Left := HEADER_LABEL_LEFT;
  FHeaderLabel.Top := HEADER_LABEL_TOP;
  FHeaderLabel.Caption := _('Project Manager');
  FHeaderLabel.Font.Style := [fsBold];
  FHeaderLabel.Font.Size := 9;
  FHeaderLabel.Font.Color := GetThemeColor(clWindowText);
  
  SetupToolbar;
  SetupPopupMenu;
  SetupTreeView;
end;

procedure TProjectManagerPanel.SetupToolbar;
begin
  FToolBar := TToolBar.Create(Self);
  FToolBar.Parent := Self;
  FToolBar.Align := alTop;
  FToolBar.Height := 28;
  FToolBar.ShowCaptions := False; // Icons only, no captions
  FToolBar.ButtonHeight := 21;
  FToolBar.ButtonWidth := 22;
  FToolBar.Flat := True;
  FToolBar.Transparent := False;
  FToolBar.Color := GetThemeColor(clWindow);
  FToolBar.AutoSize := True;
  FToolBar.Images := MainForm.VirtualImageListMain; // Use HeidiSQL's main image list
  FToolBar.ShowHint := True;
  FToolBar.BorderWidth := 0;
  FToolBar.EdgeBorders := [];
  FToolBar.EdgeInner := esNone;
  FToolBar.EdgeOuter := esNone;
  
  // Add Project Button - use same ImageIndex as actDataInsert (icons8-add)
  FBtnAddProject := TToolButton.Create(FToolBar);
  FBtnAddProject.Parent := FToolBar;
  FBtnAddProject.ImageIndex := 45; // Same as actDataInsert - icons8-add
  FBtnAddProject.Hint := _('Add Project Folder');
  FBtnAddProject.ShowHint := True;
  FBtnAddProject.OnClick := BtnAddProjectClick;
  
  // Remove Project Button - use same ImageIndex as actDataDelete (icons8-delete-button)
  FBtnRemoveProject := TToolButton.Create(FToolBar);
  FBtnRemoveProject.Parent := FToolBar;
  FBtnRemoveProject.ImageIndex := 46; // Same as actDataDelete - icons8-delete-button
  FBtnRemoveProject.Hint := _('Remove Project Folder');
  FBtnRemoveProject.ShowHint := True;
  FBtnRemoveProject.OnClick := BtnRemoveProjectClick;
  
  // Rename Project Button - use same ImageIndex as actRenameQueryTab
  FBtnRenameProject := TToolButton.Create(FToolBar);
  FBtnRenameProject.Parent := FToolBar;
  FBtnRenameProject.ImageIndex := MainForm.actRenameQueryTab.ImageIndex;
  FBtnRenameProject.Hint := _('Rename Project');
  FBtnRenameProject.ShowHint := True;
  FBtnRenameProject.OnClick := BtnRenameProjectClick;
  
  // Separator
  with TToolButton.Create(FToolBar) do begin
    Parent := FToolBar;
    Style := tbsSeparator;
    Width := 8;
  end;
  
  // Refresh Button - use same ImageIndex as actRefresh (icons8-circular-arrow-100)
  FBtnRefresh := TToolButton.Create(FToolBar);
  FBtnRefresh.Parent := FToolBar;
  FBtnRefresh.ImageIndex := MainForm.actRefresh.ImageIndex; // Use the actual refresh action ImageIndex
  FBtnRefresh.Hint := _('Refresh Projects');
  FBtnRefresh.ShowHint := True;
  FBtnRefresh.OnClick := BtnRefreshClick;
end;

procedure TProjectManagerPanel.SetupTreeView;
begin
  FTreeView := TVirtualStringTree.Create(Self);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone; // Remove border to match other panels
  FTreeView.Color := GetThemeColor(clWindow);
  FTreeView.Colors.BorderColor := GetThemeColor(clWindow); // No border color
  FTreeView.Colors.DisabledColor := GetThemeColor(clGrayText);
  FTreeView.Colors.DropMarkColor := GetThemeColor(clHighlight);
  FTreeView.Colors.DropTargetColor := GetThemeColor(clHighlight);
  FTreeView.Colors.FocusedSelectionColor := GetThemeColor(clHighlight);
  FTreeView.Colors.FocusedSelectionBorderColor := GetThemeColor(clHighlight);
  FTreeView.Colors.GridLineColor := GetThemeColor(clBtnShadow);
  FTreeView.Colors.HeaderHotColor := GetThemeColor(clBtnFace);
  FTreeView.Colors.HotColor := GetThemeColor(clBtnFace);
  FTreeView.Colors.SelectionRectangleBlendColor := GetThemeColor(clHighlight);
  FTreeView.Colors.SelectionRectangleBorderColor := GetThemeColor(clHighlight);
  FTreeView.Colors.TreeLineColor := GetThemeColor(clBtnShadow);
  FTreeView.Colors.UnfocusedSelectionColor := GetThemeColor(clBtnFace);
  FTreeView.Colors.UnfocusedSelectionBorderColor := GetThemeColor(clBtnFace);
  FTreeView.DefaultNodeHeight := 20; // Normal height since we're not using two lines
  FTreeView.Font.Size := 9;
  FTreeView.Font.Color := GetThemeColor(clWindowText);
  FTreeView.Header.AutoSizeIndex := 0;
  FTreeView.Header.DefaultHeight := 17;
  FTreeView.Header.Height := 17;
  FTreeView.Header.MainColumn := -1;
  FTreeView.Header.Options := [];
  FTreeView.HintMode := hmHint;
  FTreeView.HotCursor := crHandPoint;
  FTreeView.Images := GetSystemImageList;
  FTreeView.IncrementalSearch := isAll;
  FTreeView.NodeDataSize := SizeOf(TProjectNodeData);
  FTreeView.ParentShowHint := False;
  FTreeView.SelectionCurveRadius := 0;
  FTreeView.ShowHint := True;
  FTreeView.TabOrder := 0;
  FTreeView.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes];
  FTreeView.TreeOptions.MiscOptions := [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
  FTreeView.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware];
  FTreeView.TreeOptions.SelectionOptions := [toFullRowSelect];
  
  // Event handlers
  FTreeView.OnGetText := TreeViewGetText;
  FTreeView.OnGetHint := TreeViewGetHint;
  FTreeView.OnInitNode := TreeViewInitNode;
  FTreeView.OnInitChildren := TreeViewInitChildren;
  FTreeView.OnGetImageIndex := TreeViewGetImageIndex;
  FTreeView.OnFreeNode := TreeViewFreeNode;
  FTreeView.OnDblClick := TreeViewDblClick;
  FTreeView.OnKeyPress := TreeViewKeyPress;
  FTreeView.OnPaintText := TreeViewPaintText;
  
  // Set PopupMenu after all event handlers are assigned
  FTreeView.PopupMenu := FPopupMenu;
end;

procedure TProjectManagerPanel.SetupPopupMenu;
var
  MenuItem: TMenuItem;
begin
  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.Images := MainForm.VirtualImageListMain; // Use HeidiSQL's main image list
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('Open File');
  MenuItem.ImageIndex := MainForm.actLoadSQL.ImageIndex; // Use same as actLoadSQL (icons8-folder)
  MenuItem.OnClick := PopupOpenFileClick;
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('Open with System Editor');
  MenuItem.ImageIndex := MainForm.actLoadSQL.ImageIndex; // Use same as actLoadSQL (icons8-folder)
  MenuItem.OnClick := PopupOpenWithSystemEditorClick;
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('Open Folder');
  MenuItem.ImageIndex := MainForm.actLoadSQL.ImageIndex; // Use same as actLoadSQL (icons8-folder)
  MenuItem.OnClick := PopupOpenFolderClick;
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('Copy Path');
  MenuItem.ImageIndex := MainForm.actCopy.ImageIndex; // Use copy action
  MenuItem.OnClick := PopupCopyPathClick;
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('Add Project Folder...');
  MenuItem.ImageIndex := 45; // Same as actDataInsert - icons8-add
  MenuItem.OnClick := BtnAddProjectClick;
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('Remove Project');
  MenuItem.ImageIndex := 46; // Same as actDataDelete - icons8-delete-button
  MenuItem.OnClick := PopupRemoveProjectClick;
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('Rename Project');
  MenuItem.ImageIndex := MainForm.actRenameQueryTab.ImageIndex; // Use rename query tab action ImageIndex
  MenuItem.OnClick := PopupRenameProjectClick;
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);
  
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('Refresh');
  MenuItem.ImageIndex := MainForm.actRefresh.ImageIndex; // Use actual refresh action ImageIndex
  MenuItem.OnClick := BtnRefreshClick;
  FPopupMenu.Items.Add(MenuItem);
end;

procedure TProjectManagerPanel.BtnAddProjectClick(Sender: TObject);
var
  FolderPath, ProjectName: string;
  ProjectFolder: TProjectFolder;
  i: Integer;
  FileOpenDialog: IFileOpenDialog;
  hr: HRESULT;
  ShellItem: IShellItem;
  FolderPathPWideChar: PWideChar;
begin
  // Use modern Windows folder selection dialog
  FolderPath := '';
  
  hr := CoCreateInstance(CLSID_FileOpenDialog, nil, CLSCTX_INPROC_SERVER, IFileOpenDialog, FileOpenDialog);
  if SUCCEEDED(hr) then
  begin
    try
      // Configure the dialog for folder selection
      FileOpenDialog.SetOptions(FOS_PICKFOLDERS or FOS_FORCEFILESYSTEM or FOS_PATHMUSTEXIST);
      FileOpenDialog.SetTitle(PWideChar(_('Select Project Folder')));
      
      // Show the dialog
      hr := FileOpenDialog.Show(Application.MainForm.Handle);
      if SUCCEEDED(hr) then
      begin
        hr := FileOpenDialog.GetResult(ShellItem);
        if SUCCEEDED(hr) then
        begin
          hr := ShellItem.GetDisplayName(SIGDN_FILESYSPATH, FolderPathPWideChar);
          if SUCCEEDED(hr) then
          begin
            FolderPath := FolderPathPWideChar;
            CoTaskMemFree(FolderPathPWideChar);
          end;
        end;
      end;
    except
      // If modern dialog fails, fall back to old dialog
      SelectDirectory(_('Select Project Folder'), '', FolderPath);
    end;
  end
  else
  begin
    // Fall back to old dialog if COM interface creation fails
    if not SelectDirectory(_('Select Project Folder'), '', FolderPath) then
      Exit;
  end;
  
  // Continue with existing logic if a folder was selected
  if FolderPath <> '' then
  begin
    ProjectName := ExtractFileName(FolderPath);
    if ProjectName = '' then
      ProjectName := _('Root');
    
    // Allow user to customize the project name
    if InputQuery(_('Project Name'), _('Enter a name for this project:'), ProjectName) then
    begin
      // Trim whitespace and check for empty name
      ProjectName := Trim(ProjectName);
      if ProjectName = '' then
      begin
        ErrorDialog(_('Project name cannot be empty!'));
        Exit;
      end;
      
      // Check if project name already exists
      for i := 0 to FProjectFolders.Count - 1 do
      begin
        if SameText(FProjectFolders[i].Name, ProjectName) then
        begin
          ErrorDialog(_('A project with this name already exists!'));
          Exit;
        end;
      end;
      
      // Check if project path already exists
      for i := 0 to FProjectFolders.Count - 1 do
      begin
        if SameText(FProjectFolders[i].Path, FolderPath) then
        begin
          ErrorDialog(_('This folder is already added as a project!'));
          Exit;
        end;
      end;
      
      // Add new project
      ProjectFolder := TProjectFolder.Create(ProjectName, FolderPath);
      FProjectFolders.Add(ProjectFolder);
      
      SaveProjects;
      
      // Clear selection before refreshing to avoid issues
      FTreeView.ClearSelection;
      RefreshProjectTree;
    end;
  end;
end;

procedure TProjectManagerPanel.BtnRemoveProjectClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PProjectNodeData;
  ProjectIndex: Integer;
  ProjectName: string;
begin
  Node := FTreeView.FocusedNode;
  if not Assigned(Node) then
  begin
    ErrorDialog(_('Please select a project to remove.'));
    Exit;
  end;
  
  Data := FTreeView.GetNodeData(Node);
  if not Assigned(Data) then Exit;
  
  // Find root project node and store project index and name
  ProjectName := '';
  
  while Assigned(Node.Parent) do
  begin
    Node := Node.Parent;
    Data := FTreeView.GetNodeData(Node);
  end;
  
  if Assigned(Data) and (Data^.NodeType = ntProject) then
  begin
    // Use the project index for safety
    ProjectIndex := Data^.ProjectIndex;
    
    // Validate the project index
    if (ProjectIndex < 0) or (ProjectIndex >= FProjectFolders.Count) then
    begin
      ErrorDialog(_('Project not found in list.'));
      Exit;
    end;
    
    // Store the project name
    ProjectName := FProjectFolders[ProjectIndex].Name;
    
    if MessageDialog(f_('Remove project "%s"?', [ProjectName]), 
                  mtConfirmation, [mbYes, mbNo]) = mrYes then
    begin
      // Remove using the stored index
      FProjectFolders.Delete(ProjectIndex);
      
      SaveProjects;
      
      // Clear selection before refreshing to avoid issues
      FTreeView.ClearSelection;
      RefreshProjectTree;
    end;
  end;
end;

procedure TProjectManagerPanel.BtnRenameProjectClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PProjectNodeData;
  NewName: string;
  i: Integer;
  ProjectIndex: Integer;
  ProjectName: string;
begin
  Node := FTreeView.FocusedNode;
  if not Assigned(Node) then
  begin
    ErrorDialog(_('Please select a project to rename.'));
    Exit;
  end;
  
  Data := FTreeView.GetNodeData(Node);
  if not Assigned(Data) then Exit;
  
  // Find root project node and store project index and name
  ProjectName := '';
  
  while Assigned(Node.Parent) do
  begin
    Node := Node.Parent;
    Data := FTreeView.GetNodeData(Node);
  end;
  
  if Assigned(Data) and (Data^.NodeType = ntProject) then
  begin
    // Use the project index for safety
    ProjectIndex := Data^.ProjectIndex;
    
    // Validate the project index
    if (ProjectIndex < 0) or (ProjectIndex >= FProjectFolders.Count) then
    begin
      ErrorDialog(_('Project not found in list.'));
      Exit;
    end;
    
    // Store the project name
    ProjectName := FProjectFolders[ProjectIndex].Name;
    
    NewName := ProjectName;
    
    if InputQuery(_('Rename Project'), _('Enter new name for project:'), NewName) then
    begin
      // Trim whitespace and check for empty name
      NewName := Trim(NewName);
      if NewName = '' then
      begin
        ErrorDialog(_('Project name cannot be empty!'));
        Exit;
      end;
      
      // Check if the new name is different from the current name
      if SameText(NewName, ProjectName) then
      begin
        // Name unchanged, nothing to do
        Exit;
      end;
      
      // Check if project name already exists
      for i := 0 to FProjectFolders.Count - 1 do
      begin
        if (i <> ProjectIndex) and SameText(FProjectFolders[i].Name, NewName) then
        begin
          ErrorDialog(_('A project with this name already exists!'));
          Exit;
        end;
      end;
      
      // Update the project name using the stored index
      FProjectFolders[ProjectIndex].Name := NewName;
      
      SaveProjects;
      
      // Clear selection before refreshing to avoid issues
      FTreeView.ClearSelection;
      RefreshProjectTree;
    end;
  end
  else
  begin
    ErrorDialog(_('Please select a project to rename.'));
  end;
end;

procedure TProjectManagerPanel.BtnRefreshClick(Sender: TObject);
begin
  RefreshProjectTree;
end;

procedure TProjectManagerPanel.TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PProjectNodeData;
begin
  // Safety check for shutdown
  if IsShuttingDown then
  begin
    CellText := '';
    Exit;
  end;
  
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    case Data^.NodeType of
      ntProject: 
        if (Data^.ProjectIndex >= 0) and (Data^.ProjectIndex < FProjectFolders.Count) then
        begin
          if System.SysUtils.DirectoryExists(FProjectFolders[Data^.ProjectIndex].Path) then
            CellText := FProjectFolders[Data^.ProjectIndex].Name
          else
            CellText := FProjectFolders[Data^.ProjectIndex].Name + ' ' + _('(Path not found)');
        end
        else
          CellText := _('(Invalid Project)');
      ntFolder: CellText := Data^.DisplayName;
      ntFile: CellText := Data^.DisplayName;
    end;
  end
  else if FProjectFolders.Count = 0 then
  begin
    // Show placeholder text for empty state
    if Node.Index = 0 then
      CellText := _('No projects configured')
    else
      CellText := _('Click Add to add a project folder');
  end;
end;

procedure TProjectManagerPanel.TreeViewGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  Data: PProjectNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    case Data^.NodeType of
      ntProject: 
        if (Data^.ProjectIndex >= 0) and (Data^.ProjectIndex < FProjectFolders.Count) then
        begin
          if System.SysUtils.DirectoryExists(FProjectFolders[Data^.ProjectIndex].Path) then
            HintText := FProjectFolders[Data^.ProjectIndex].Path
          else
            HintText := FProjectFolders[Data^.ProjectIndex].Path + ' ' + _('(Path not found)');
        end
        else
          HintText := _('(Invalid Project)');
      ntFolder: HintText := Data^.FullPath;
      ntFile: HintText := Data^.FullPath;
    end;
  end;
end;

procedure TProjectManagerPanel.TreeViewInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PProjectNodeData;
  ParentData: PProjectNodeData;
  SearchRec: TSearchRec;
  FolderPath: string;
  CurrentIndex: Integer;
  FileCount: Integer;
  FindResult: Integer;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then Exit;
  
  if not Assigned(ParentNode) then
  begin
    // Root level - project folders
    if (Integer(Node.Index) < FProjectFolders.Count) then
    begin
      Data^.NodeType := ntProject;
      Data^.ProjectIndex := Node.Index;
      Data^.FullPath := FProjectFolders[Node.Index].Path;
      Data^.DisplayName := FProjectFolders[Node.Index].Name;
      
      // Check if project folder has children
      if System.SysUtils.DirectoryExists(FProjectFolders[Node.Index].Path) then
        Include(InitialStates, ivsHasChildren);
    end;
  end
  else
  begin
    // Child nodes - need to determine what this node represents
    ParentData := Sender.GetNodeData(ParentNode);
    if not Assigned(ParentData) then Exit;
    
    // Determine parent folder path
    case ParentData^.NodeType of
      ntProject: 
        if (ParentData^.ProjectIndex >= 0) and (ParentData^.ProjectIndex < FProjectFolders.Count) then
          FolderPath := FProjectFolders[ParentData^.ProjectIndex].Path
        else
          Exit;
      ntFolder: FolderPath := ParentData^.FullPath;
      else Exit;
    end;
    
    if not System.SysUtils.DirectoryExists(FolderPath) then Exit;
    
    CurrentIndex := 0;
    
    // First pass: add directories with error handling
    try
      FindResult := FindFirst(IncludeTrailingPathDelimiter(FolderPath) + '*', faDirectory, SearchRec);
      if FindResult = 0 then
      begin
        try
          repeat
            if (SearchRec.Attr and faDirectory) <> 0 then
            begin
              if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
              begin
                if CurrentIndex = Integer(Node.Index) then
                begin
                  Data^.NodeType := ntFolder;
                  Data^.DisplayName := SearchRec.Name;
                  Data^.FullPath := IncludeTrailingPathDelimiter(FolderPath) + SearchRec.Name;
                  
                  // Check if this folder has children
                  if System.SysUtils.DirectoryExists(Data^.FullPath) then
                    Include(InitialStates, ivsHasChildren);
                  
                  Exit;
                end;
                Inc(CurrentIndex);
              end;
            end;
          until FindNext(SearchRec) <> 0;
        finally
          System.SysUtils.FindClose(SearchRec);
        end;
      end;
    except
      // Ignore file system errors during directory enumeration
    end;
    
    // Second pass: add files with error handling
    FileCount := 0;
    try
      FindResult := FindFirst(IncludeTrailingPathDelimiter(FolderPath) + '*', faAnyFile, SearchRec);
      if FindResult = 0 then
      begin
        try
          repeat
            if (SearchRec.Attr and faDirectory) = 0 then
            begin
              if IsTextFile(SearchRec.Name) then
              begin
                if CurrentIndex = Integer(Node.Index) then
                begin
                  if FileCount >= 100 then
                  begin
                    Data^.NodeType := ntFile;
                    Data^.DisplayName := _('... (more files)');
                    Data^.FullPath := '';
                  end
                  else
                  begin
                    Data^.NodeType := ntFile;
                    Data^.DisplayName := SearchRec.Name;
                    Data^.FullPath := IncludeTrailingPathDelimiter(FolderPath) + SearchRec.Name;
                  end;
                  Exit;
                end;
                Inc(CurrentIndex);
                Inc(FileCount);
                if FileCount >= 100 then
                begin
                  if CurrentIndex = Integer(Node.Index) then
                  begin
                    Data^.NodeType := ntFile;
                    Data^.DisplayName := _('... (more files)');
                    Data^.FullPath := '';
                    Exit;
                  end;
                  Inc(CurrentIndex);
                  Break;
                end;
              end;
            end;
          until FindNext(SearchRec) <> 0;
        finally
          System.SysUtils.FindClose(SearchRec);
        end;
      end;
    except
      // Ignore file system errors during file enumeration
    end;
  end;
end;

procedure TProjectManagerPanel.TreeViewInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data: PProjectNodeData;
  SearchRec: TSearchRec;
  FolderPath: string;
  FileCount, DirCount: Integer;
  FindResult: Integer;
begin
  ChildCount := 0;
  Data := Sender.GetNodeData(Node);
  
  if not Assigned(Data) then Exit;
  
  // Determine folder path
  case Data^.NodeType of
    ntProject: 
      if (Data^.ProjectIndex >= 0) and (Data^.ProjectIndex < FProjectFolders.Count) then
        FolderPath := FProjectFolders[Data^.ProjectIndex].Path
      else
        Exit;
    ntFolder: FolderPath := Data^.FullPath;
    else Exit;
  end;
  
  if not System.SysUtils.DirectoryExists(FolderPath) then Exit;
  
  FileCount := 0;
  DirCount := 0;
  
  // Count directories with error handling
  try
    FindResult := FindFirst(IncludeTrailingPathDelimiter(FolderPath) + '*', faDirectory, SearchRec);
    if FindResult = 0 then
    begin
      try
        repeat
          if (SearchRec.Attr and faDirectory) <> 0 then
          begin
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
              Inc(DirCount);
          end;
        until FindNext(SearchRec) <> 0;
      finally
        System.SysUtils.FindClose(SearchRec);
      end;
    end;
  except
    // Ignore file system errors during directory enumeration
    DirCount := 0;
  end;
  
  // Count text files (limit to 100) with error handling
  try
    FindResult := FindFirst(IncludeTrailingPathDelimiter(FolderPath) + '*', faAnyFile, SearchRec);
    if FindResult = 0 then
    begin
      try
        repeat
          if (SearchRec.Attr and faDirectory) = 0 then
          begin
            if IsTextFile(SearchRec.Name) then
            begin
              Inc(FileCount);
              if FileCount >= 100 then
              begin
                Inc(FileCount); // For "... (more files)" entry
                Break;
              end;
            end;
          end;
        until FindNext(SearchRec) <> 0;
      finally
        System.SysUtils.FindClose(SearchRec);
      end;
    end;
  except
    // Ignore file system errors during file enumeration
    FileCount := 0;
  end;
  
  ChildCount := DirCount + FileCount;
end;

procedure TProjectManagerPanel.TreeViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PProjectNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    // Clear the record - no dynamic allocation to free
    FillChar(Data^, SizeOf(TProjectNodeData), 0);
  end;
end;

procedure TProjectManagerPanel.TreeViewGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Data: PProjectNodeData;
begin
  // Only handle normal and selected images, ignore state images
  if not (Kind in [ikNormal, ikSelected]) then
  begin
    ImageIndex := -1;
    Exit;
  end;
  
  // Safety check for shutdown
  if IsShuttingDown then
  begin
    ImageIndex := -1;
    Ghosted := False;
    Exit;
  end;
  
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    case Data^.NodeType of
      ntProject: 
        if (Data^.ProjectIndex >= 0) and (Data^.ProjectIndex < FProjectFolders.Count) then
        begin
          if System.SysUtils.DirectoryExists(FProjectFolders[Data^.ProjectIndex].Path) then
            ImageIndex := TImageIndex(GetSystemImageIndex(FProjectFolders[Data^.ProjectIndex].Path))
          else
            ImageIndex := TImageIndex(GetSystemImageIndex('')); // Default/error icon
        end
        else
          ImageIndex := TImageIndex(GetSystemImageIndex('')); // Default/error icon
      ntFolder: ImageIndex := TImageIndex(GetSystemImageIndex(Data^.FullPath));
      ntFile: ImageIndex := TImageIndex(GetSystemImageIndex(Data^.FullPath));
    else
      ImageIndex := TImageIndex(GetSystemImageIndex(''));
    end;
  end
  else
    ImageIndex := TImageIndex(GetSystemImageIndex(''));
  
  Ghosted := False;
end;

procedure TProjectManagerPanel.TreeViewDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PProjectNodeData;
  FileExt: string;
begin
  Node := FTreeView.FocusedNode;
  if Assigned(Node) then
  begin
    Data := FTreeView.GetNodeData(Node);
    if Assigned(Data) and (Data^.NodeType = ntFile) and (Data^.FullPath <> '') then
    begin
      FileExt := LowerCase(ExtractFileExt(Data^.FullPath));
      
      // Check if it's a supported SQL file type that can be opened in query tabs
      if (FileExt = '.sql') or (FileExt = '.tsql') or (FileExt = '.pgsql') or 
         (FileExt = '.psql') or (FileExt = '.ddl') or (FileExt = '.dml') or
         (FileExt = '.proc') or (FileExt = '.func') or (FileExt = '.view') or
         (FileExt = '.trigger') or (FileExt = '.sp') or (FileExt = '.udf') then
      begin
        OpenFileInQueryTab(Data^.FullPath);
      end
      else if (FileExt = '.db') or (FileExt = '.sqlite') or (FileExt = '.sqlite3') then
      begin
        // For database files, we could potentially open them as connections
        // For now, just inform the user
        ErrorDialog(_('Database files should be opened via the connection manager.') + #13#10 +
                   _('Use File > Connect to Database to open this SQLite database.'));
      end
      else if IsTextFile(ExtractFileName(Data^.FullPath)) then
      begin
        // For other text files, open with default system editor
        ShellExecute(0, 'open', PChar(Data^.FullPath), nil, nil, SW_SHOWNORMAL);
      end
      else
      begin
        ErrorDialog(f_('File type "%s" is not directly supported for editing.', [FileExt]) + #13#10 +
                   _('You can open it with the system default application via the context menu.'));
      end;
    end;
  end;
end;

procedure TProjectManagerPanel.TreeViewKeyPress(Sender: TObject; var Key: Char);
var
  Node: PVirtualNode;
  Data: PProjectNodeData;
  FileExt: string;
begin
  case Key of
    #13: // Enter key - open file
    begin
      Node := FTreeView.FocusedNode;
      if Assigned(Node) then
      begin
        Data := FTreeView.GetNodeData(Node);
        if Assigned(Data) and (Data^.NodeType = ntFile) and (Data^.FullPath <> '') then
        begin
          FileExt := LowerCase(ExtractFileExt(Data^.FullPath));
          
          // Check if it's a supported SQL file type that can be opened in query tabs
          if (FileExt = '.sql') or (FileExt = '.tsql') or (FileExt = '.pgsql') or 
             (FileExt = '.psql') or (FileExt = '.ddl') or (FileExt = '.dml') or
             (FileExt = '.proc') or (FileExt = '.func') or (FileExt = '.view') or
             (FileExt = '.trigger') or (FileExt = '.sp') or (FileExt = '.udf') then
          begin
            OpenFileInQueryTab(Data^.FullPath);
            Key := #0; // Consume the key
          end;
        end;
      end;
    end;
    
    'F': // F key - open folder
    begin
      if GetKeyState(VK_CONTROL) < 0 then // Ctrl+F
      begin
        PopupOpenFolderClick(Sender);
        Key := #0;
      end;
    end;
    
    'C': // C key - copy path
    begin
      if GetKeyState(VK_CONTROL) < 0 then // Ctrl+C
      begin
        PopupCopyPathClick(Sender);
        Key := #0;
      end;
    end;
    
    'E': // E key - open with system editor
    begin
      if GetKeyState(VK_CONTROL) < 0 then // Ctrl+E
      begin
        PopupOpenWithSystemEditorClick(Sender);
        Key := #0;
      end;
    end;
    
    #46: // Delete key - remove project (only for project nodes)
    begin
      Node := FTreeView.FocusedNode;
      if Assigned(Node) then
      begin
        Data := FTreeView.GetNodeData(Node);
        if Assigned(Data) and (Data^.NodeType = ntProject) then
        begin
          BtnRemoveProjectClick(Sender);
          Key := #0;
        end;
      end;
    end;
    
    #113: // F2 key - rename project
    begin
      Node := FTreeView.FocusedNode;
      if Assigned(Node) then
      begin
        Data := FTreeView.GetNodeData(Node);
        if Assigned(Data) and (Data^.NodeType = ntProject) then
        begin
          BtnRenameProjectClick(Sender);
          Key := #0;
        end;
      end;
    end;
  end;
end;

procedure TProjectManagerPanel.PopupOpenFileClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PProjectNodeData;
  FileExt: string;
begin
  Node := FTreeView.FocusedNode;
  if Assigned(Node) then
  begin
    Data := FTreeView.GetNodeData(Node);
    if Assigned(Data) and (Data^.NodeType = ntFile) and (Data^.FullPath <> '') then
    begin
      FileExt := LowerCase(ExtractFileExt(Data^.FullPath));
      
      // Check if it's a supported SQL file type
      if (FileExt = '.sql') or (FileExt = '.tsql') or (FileExt = '.pgsql') or 
         (FileExt = '.psql') or (FileExt = '.db') or (FileExt = '.sqlite') or 
         (FileExt = '.sqlite3') then
      begin
        OpenFileInQueryTab(Data^.FullPath);
      end
      else
      begin
        // Unsupported file type
        ErrorDialog(f_('File type "%s" is not currently supported.', [FileExt]) + #13#10 +
                   _('Supported file types:') + #13#10 +
                   '• .sql - ' + _('Standard SQL scripts') + #13#10 +
                   '• .tsql - ' + _('Transact-SQL for SQL Server') + #13#10 +
                   '• .pgsql/.psql - ' + _('PostgreSQL scripts') + #13#10 +
                   '• .db/.sqlite/.sqlite3 - ' + _('SQLite database files'));
      end;
    end;
  end;
end;

procedure TProjectManagerPanel.PopupOpenFolderClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PProjectNodeData;
  FolderPath: string;
begin
  Node := FTreeView.FocusedNode;
  if Assigned(Node) then
  begin
    Data := FTreeView.GetNodeData(Node);
    if Assigned(Data) then
    begin
      case Data^.NodeType of
        ntProject: 
          if (Data^.ProjectIndex >= 0) and (Data^.ProjectIndex < FProjectFolders.Count) then
            FolderPath := FProjectFolders[Data^.ProjectIndex].Path
          else
            FolderPath := '';
        ntFolder: FolderPath := Data^.FullPath;
        ntFile: FolderPath := ExtractFilePath(Data^.FullPath);
      end;
      
      if System.SysUtils.DirectoryExists(FolderPath) then
        ShellExecute(0, 'explore', PChar(FolderPath), nil, nil, SW_SHOWNORMAL)
      else
        ErrorDialog(f_('Folder not found: %s', [FolderPath]));
    end;
  end;
end;

procedure TProjectManagerPanel.PopupOpenWithSystemEditorClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PProjectNodeData;
begin
  Node := FTreeView.FocusedNode;
  if Assigned(Node) then
  begin
    Data := FTreeView.GetNodeData(Node);
    if Assigned(Data) and (Data^.NodeType = ntFile) and (Data^.FullPath <> '') then
    begin
      if FileExists(Data^.FullPath) then
        ShellExecute(0, 'open', PChar(Data^.FullPath), nil, nil, SW_SHOWNORMAL)
      else
        ErrorDialog(f_('File not found: %s', [Data^.FullPath]));
    end;
  end;
end;

procedure TProjectManagerPanel.PopupCopyPathClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PProjectNodeData;
  PathToCopy: string;
begin
  Node := FTreeView.FocusedNode;
  if Assigned(Node) then
  begin
    Data := FTreeView.GetNodeData(Node);
    if Assigned(Data) then
    begin
      case Data^.NodeType of
        ntProject: 
          if (Data^.ProjectIndex >= 0) and (Data^.ProjectIndex < FProjectFolders.Count) then
            PathToCopy := FProjectFolders[Data^.ProjectIndex].Path
          else
            PathToCopy := '';
        ntFolder: PathToCopy := Data^.FullPath;
        ntFile: PathToCopy := Data^.FullPath;
      end;
      
      if PathToCopy <> '' then
      begin
        try
          Vcl.Clipbrd.Clipboard.AsText := PathToCopy;
        except
          on E: Exception do
            ErrorDialog(_('Failed to copy path to clipboard: ') + E.Message);
        end;
      end;
    end;
  end;
end;

procedure TProjectManagerPanel.PopupRemoveProjectClick(Sender: TObject);
begin
  BtnRemoveProjectClick(Sender);
end;

procedure TProjectManagerPanel.PopupRenameProjectClick(Sender: TObject);
begin
  BtnRenameProjectClick(Sender);
end;

function TProjectManagerPanel.InitProjectsIniFile: TIniFile;
var
  WaitingSince: UInt64;
  Attempts: Integer;
  ProjectsIniFilename: String;
begin
  // Try to open projects.ini for writing or reading
  // Taking multiple application instances into account
  if AppSettings.PortableMode then
    ProjectsIniFilename := ExtractFilePath(Application.ExeName) + 'projects.ini'
  else
    ProjectsIniFilename := AppSettings.DirnameUserAppData + 'projects.ini';
  WaitingSince := GetTickCount64;
  Attempts := 0;
  while not FileIsWritable(ProjectsIniFilename) do begin
    if GetTickCount64 - WaitingSince > 3000 then
      Raise Exception.Create(f_('Could not open file %s', [ProjectsIniFilename]));
    Sleep(200);
    Inc(Attempts);
  end;
  if Attempts > 0 then begin
    // Could log this, but we'll skip it for now
  end;
  // Catch errors when file cannot be created
  if not FileExists(ProjectsIniFilename) then begin
    SaveUnicodeFile(ProjectsIniFilename, '', UTF8NoBOMEncoding);
  end;
  Result := TIniFile.Create(ProjectsIniFilename);
end;

procedure TProjectManagerPanel.LoadProjects;
var
  ProjectsIni: TIniFile;
  Sections: TStringList;
  Section: String;
  ProjectFolder: TProjectFolder;
  ProjectName, ProjectPath: String;
begin
  FProjectFolders.Clear;
  
  try
    ProjectsIni := InitProjectsIniFile;
    
    Sections := TStringList.Create;
    try
      ProjectsIni.ReadSections(Sections);
      
      for Section in Sections do
      begin
        ProjectName := ProjectsIni.ReadString(Section, 'Name', '');
        ProjectPath := ProjectsIni.ReadString(Section, 'Path', '');
        
        if (ProjectName <> '') and (ProjectPath <> '') then
        begin
          ProjectFolder := TProjectFolder.Create(ProjectName, ProjectPath);
          FProjectFolders.Add(ProjectFolder);
        end;
      end;
    finally
      Sections.Free;
    end;
    
    ProjectsIni.Free;
  except
    on E: Exception do
    begin
      // Ignore loading errors, start with empty project list
      // Could log this: 'Error loading projects: ' + E.Message
    end;
  end;
end;

procedure TProjectManagerPanel.SaveProjects;
var
  ProjectsIni: TIniFile;
  i: Integer;
  Section: String;
  Sections: TStringList;
begin
  // Safety check - don't save if we're shutting down or objects are invalid
  if not Assigned(FProjectFolders) or (FProjectFolders.Count = 0) then
    Exit;
    
  try
    ProjectsIni := InitProjectsIniFile;
    
    try
      // Clear all existing sections first
      Sections := TStringList.Create;
      try
        ProjectsIni.ReadSections(Sections);
        for Section in Sections do
          ProjectsIni.EraseSection(Section);
      finally
        Sections.Free;
      end;
      
      // Write all projects
      for i := 0 to FProjectFolders.Count - 1 do
      begin
        // Safety check for each project
        if not Assigned(FProjectFolders[i]) then
          Continue;
          
        Section := 'Project' + IntToStr(i);
        ProjectsIni.WriteString(Section, 'Name', FProjectFolders[i].Name);
        ProjectsIni.WriteString(Section, 'Path', FProjectFolders[i].Path);
      end;
    finally
      ProjectsIni.Free;
    end;
  except
    on E: Exception do
    begin
      // During shutdown, don't show message boxes as they can cause access violations
      // Could log this: 'Error saving projects: ' + E.Message
      if not (csDestroying in ComponentState) then
        ErrorDialog(_('Error saving projects: ') + E.Message);
    end;
  end;
end;

procedure TProjectManagerPanel.RefreshProjectTree;
var
  WasExpanded: array of Boolean;
  i, j: Integer;
  Node: PVirtualNode;
begin
  // Safety check - don't refresh if we're shutting down or objects are invalid
  if not Assigned(FTreeView) or not Assigned(FProjectFolders) or (csDestroying in ComponentState) then
    Exit;
    
  try
    // Store expansion state before clearing
    SetLength(WasExpanded, FProjectFolders.Count);
    for i := 0 to FProjectFolders.Count - 1 do
    begin
      Node := FTreeView.GetFirstChild(FTreeView.RootNode);
      // Skip to the i-th child
      for j := 0 to i - 1 do
      begin
        if Assigned(Node) then
          Node := FTreeView.GetNextSibling(Node);
      end;
      WasExpanded[i] := Assigned(Node) and (vsExpanded in Node.States);
    end;
    
    FTreeView.Clear;
    
    if FProjectFolders.Count = 0 then
    begin
      // Add placeholder nodes for empty state
      FTreeView.RootNodeCount := 2;
    end
    else
    begin
      FTreeView.RootNodeCount := FProjectFolders.Count;
      
      // Restore expansion state
      for i := 0 to Min(FProjectFolders.Count - 1, Length(WasExpanded) - 1) do
      begin
        if WasExpanded[i] then
        begin
          Node := FTreeView.GetFirstChild(FTreeView.RootNode);
          // Skip to the i-th child
          for j := 0 to i - 1 do
          begin
            if Assigned(Node) then
              Node := FTreeView.GetNextSibling(Node);
          end;
          if Assigned(Node) then
            FTreeView.Expanded[Node] := True;
        end;
      end;
    end;
  except
    // Suppress any errors during tree refresh to prevent access violations
  end;
end;

function TProjectManagerPanel.IsTextFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.sql') or (Ext = '.tsql') or (Ext = '.pgsql') or 
            (Ext = '.psql') or (Ext = '.db') or (Ext = '.sqlite') or 
            (Ext = '.sqlite3') or (Ext = '.ddl') or (Ext = '.dml') or
            (Ext = '.proc') or (Ext = '.func') or (Ext = '.view') or
            (Ext = '.trigger') or (Ext = '.sp') or (Ext = '.udf') or
            (Ext = '.txt') or (Ext = '.md') or (Ext = '.readme') or
            (Ext = '.log') or (Ext = '.csv') or (Ext = '.json') or
            (Ext = '.xml') or (Ext = '.yml') or (Ext = '.yaml');
end;

function TProjectManagerPanel.IsFileOpenInTab(const FilePath: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to MainForm.QueryTabs.Count - 1 do
  begin
    if SameText(MainForm.QueryTabs[i].MemoFilename, FilePath) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TProjectManagerPanel.TreeViewPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PProjectNodeData;
  WindowColor: TColor;
  Brightness: Integer;
begin
  // Safety check for shutdown
  if IsShuttingDown then
    Exit;
    
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    if (Data^.NodeType = ntFile) and (Data^.FullPath <> '') then
    begin
      // Handle file nodes with open-file highlighting
      if IsFileOpenInTab(Data^.FullPath) then
      begin
        // Make text bold
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
        
        // Calculate brightness of the background to choose appropriate text color
        WindowColor := GetThemeColor(clWindow);
        
        // Calculate brightness using weighted RGB values (perceived brightness formula)
        Brightness := Round((GetRValue(WindowColor) * 0.299) + 
                           (GetGValue(WindowColor) * 0.587) + 
                           (GetBValue(WindowColor) * 0.114));
        
        // Choose color based on background brightness
        if Brightness > 128 then
          // Light background - use dark blue
          TargetCanvas.Font.Color := RGB(0, 100, 200)
        else
          // Dark background - use light blue
          TargetCanvas.Font.Color := RGB(100, 200, 255);
      end;
    end;
  end;
end;

procedure TProjectManagerPanel.UpdateTreeAppearance;
begin
  // Just invalidate the tree to trigger a repaint with updated file states
  if Assigned(FTreeView) and not (csDestroying in ComponentState) then
  begin
    try
      FTreeView.Invalidate;
    except
      // Suppress any errors during tree invalidation
    end;
  end;
end;

procedure TProjectManagerPanel.OpenFileInQueryTab(const FilePath: string);
var
  Tab: TQueryTab;
  i: Integer;
  ExistingTab: TQueryTab;
begin
  try
    if not FileExists(FilePath) then
    begin
      ErrorDialog(f_('File not found: %s', [FilePath]));
      Exit;
    end;
    
    // First, check if the file is already open in an existing tab
    ExistingTab := nil;
    for i := 0 to MainForm.QueryTabs.Count - 1 do
    begin
      if SameText(MainForm.QueryTabs[i].MemoFilename, FilePath) then
      begin
        ExistingTab := MainForm.QueryTabs[i];
        Break;
      end;
    end;
    
    if Assigned(ExistingTab) then
    begin
      // File is already open, just focus the existing tab
      MainForm.SetMainTab(ExistingTab.TabSheet);
    end
    else
    begin
      // File is not open, create new tab and load the file
      Tab := MainForm.GetOrCreateEmptyQueryTab(True); // True = focus the tab
      
      // Load the file content into the tab
      if not Tab.LoadContents(FilePath, True, nil) then
      begin
        ErrorDialog(f_('Failed to load file: %s', [FilePath]));
      end
      else
      begin
        // Update tree appearance to highlight the newly opened file
        UpdateTreeAppearance;
      end;
    end;
    
  except
    on E: Exception do
    begin
      ErrorDialog(_('Error opening file: ') + E.Message);
    end;
  end;
end;

function TProjectManagerPanel.IsShuttingDown: Boolean;
begin
  Result := (csDestroying in ComponentState) or 
            not Assigned(FTreeView) or 
            not Assigned(FProjectFolders);
end;

procedure TProjectManagerPanel.ToggleVisibility;
begin
  Visible := not Visible;
  // Also toggle associated splitter if it exists
  if Assigned(Parent) then
  begin
    // Find the splitter in the same parent
    var i: Integer;
    for i := 0 to Parent.ControlCount - 1 do
    begin
      if (Parent.Controls[i] is TSplitter) and 
         (TSplitter(Parent.Controls[i]).Align = alBottom) then
      begin
        TSplitter(Parent.Controls[i]).Visible := Visible;
        Break;
      end;
    end;
  end;
end;

end.
