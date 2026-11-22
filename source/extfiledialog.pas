unit extfiledialog;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ShellCtrls, ComCtrls, EditBtn, apphelpers, extra_controls,
  Math, Masks, LazUTF8, LCLIntf
  {$IFDEF WINDOWS}, Windows, WinDirs, ActiveX, ShlObj {$ENDIF}
  {$IFNDEF WINDOWS}, generic_types {$ENDIF};

type

  { TfrmExtFileDialog }

  TfrmExtFileDialog = class(TExtForm)
    btnCancel: TButton;
    btnOk: TButton;
    tbViewStyle: TToolBar;
    tbtnNewFolder: TToolButton;
    tbtnSeparator1: TToolButton;
    tbtnViewIcons: TToolButton;
    tbtnViewDetails: TToolButton;
    comboRecentFolders: TComboBox;
    editSearch: TEditButton;
    pnlTopBar: TPanel;
    comboLineBreaks: TComboBox;
    comboEncoding: TComboBox;
    comboFileType: TComboBox;
    editFilename: TEdit;
    IdleTimerTreeNodeScrolltoview: TIdleTimer;
    lblLinebreaks: TLabel;
    lblEncoding: TLabel;
    lblFilename: TLabel;
    pnlBottom: TPanel;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    splitterMain: TSplitter;
    procedure comboEncodingChange(Sender: TObject);
    procedure comboFileTypeChange(Sender: TObject);
    procedure comboLineBreaksChange(Sender: TObject);
    procedure editFilenameEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerTreeNodeScrolltoviewTimer(Sender: TObject);
    procedure ShellListViewClick(Sender: TObject);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure ShellListViewFileAdded(Sender: TObject; Item: TListItem);
    procedure ShellListViewAddItem(Sender: TObject; const ABasePath: String;
      const AFileInfo: TSearchRec; var CanAdd: Boolean);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellListViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ShellListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ShellListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ShellListViewResize(Sender: TObject);
    procedure ShellListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShellListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ShellListViewEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure ShellListViewEdited(Sender: TObject; Item: TListItem;
      var AValue: String);
    procedure RenameTimerTimer(Sender: TObject);
    procedure btnViewIconsClick(Sender: TObject);
    procedure btnViewDetailsClick(Sender: TObject);
    procedure btnNewFolderClick(Sender: TObject);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure comboRecentFoldersDropDown(Sender: TObject);
    procedure comboRecentFoldersCloseUp(Sender: TObject);
    procedure ShellTreeViewResize(Sender: TObject);
    procedure editSearchChange(Sender: TObject);
    procedure editSearchButtonClick(Sender: TObject);
  private
    FInitialDir: String;
    FFilterNames: TStringList;
    FFilterMasks: TStringList;
    FFilterIndex: Integer;
    FDefaultExt: String;
    FEncodings: TStringList;
    FEncodingIndex: Integer;
    FLineBreakIndex: TLineBreaks;
    FOptions: TOpenOptions;
    FFiles: TStringList;
    FOnTypeChange: TNotifyEvent;
    FSortDirection: TSortDirection; // SortDirection is not published by TShellListView
    FRecentFoldersHintShown: Boolean;
    FRenameTimer: TTimer;
    FRenameCaption: String; // item clicked a second time, renamed when FRenameTimer fires
    FSkipQueuedEdit: Boolean;
    FSelectedByClick: Boolean;
    procedure StartRename(Item: TListItem);
    function CheckFileNameChars(const AName: String): Boolean;
    procedure RefreshListAsync(Data: PtrInt);
    function CompareFileNames(const Name1, Name2: String): Integer;
    procedure AddRecentFolder(const Folder: String);
    procedure ShowRecentFoldersHint;
    procedure UpdateFormMinSize;
    procedure UpdateViewStyleButton;
    procedure SetupListViewColumns;
    {$IFDEF WINDOWS}
    function ShowShellContextMenu(FileName: String; ScreenPos: TPoint; out Verb: String): Boolean;
    {$ENDIF}
    procedure SetTitle(AValue: String);
    function GetFileName: String;
    procedure SetFileName(const AValue: String);
    procedure SetInitialDir(const AValue: String);
    procedure SetFilterIndex(AValue: Integer);
    procedure SetOptions(AValue: TOpenOptions);
  public
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
    property Title: String write SetTitle;
    function Execute: Boolean;
    procedure AddFileType(FileMask, DisplayName: String);
    property FileName: String read GetFileName write SetFileName;
    property InitialDir: String read FInitialDir write SetInitialDir;
    property DefaultExt: String read FDefaultExt write FDefaultExt;
    property Options: TOpenOptions read FOptions write SetOptions;
    property Files: TStringList read FFiles;
    property FilterIndex: Integer read FFilterIndex write SetFilterIndex;
  end;

  // File-open-dialog with encoding selector
  TExtFileOpenDialog = class(TfrmExtFileDialog)
    procedure FormCreate(Sender: TObject); overload;
    procedure FormShow(Sender: TObject); overload;
    public
      property Encodings: TStringList read FEncodings write FEncodings;
      property EncodingIndex: Integer read FEncodingIndex write FEncodingIndex;
  end;

  TExtFileSaveDialog = class(TfrmExtFileDialog)
    procedure FormCreate(Sender: TObject); overload;
    procedure FormShow(Sender: TObject); overload;
    public
      property LineBreakIndex: TLineBreaks read FLineBreakIndex write FLineBreakIndex;
  end;

implementation

{$R *.lfm}

uses main;

{$IFDEF WINDOWS}
function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll' name 'StrCmpLogicalW';

var
  // State of the shell context menu while TrackPopupMenu runs, see ShellMenuWndProc
  // The LCL window procedure of the dialog, as returned by SetWindowLongPtrW, and
  // the same as callable procedure variable (absolute: no ordinal/pointer casts)
  ShellMenuOldWndProc: LONG_PTR = 0;
  ShellMenuOldWndProcFunc: Windows.WNDPROC absolute ShellMenuOldWndProc;
  ShellMenuCM2: IContextMenu2 = nil;
  ShellMenuCM3: IContextMenu3 = nil;

{ Temporary window procedure of the dialog while the shell context menu is open.
  Owner-drawn and dynamically filled items ("Send to", "New", shell extensions)
  need their WM_INITMENUPOPUP/WM_MEASUREITEM/WM_DRAWITEM/WM_MENUCHAR messages
  forwarded to IContextMenu2/3.
  None of the menu messages must reach the LCL: its handlers for WM_MEASUREITEM,
  WM_DRAWITEM, WM_INITMENUPOPUP, WM_MENUSELECT and WM_MENUCHAR take the item data
  of the menu items for TMenuItem objects ("TObject(itemData) is TMenuItem"),
  which crashes on the shell's item data. }
function ShellMenuWndProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  ForShell, IsMenuMsg: Boolean;
  MsgResult: LRESULT;
  MeasureItem: PMeasureItemStruct absolute lParam;
  DrawItem: PDrawItemStruct absolute lParam;
begin
  case Msg of
    WM_INITMENUPOPUP, WM_MENUCHAR: begin
      ForShell := True;
      IsMenuMsg := True;
    end;
    WM_MEASUREITEM: begin
      IsMenuMsg := MeasureItem^.CtlType = ODT_MENU;
      ForShell := IsMenuMsg;
    end;
    WM_DRAWITEM: begin
      IsMenuMsg := DrawItem^.CtlType = ODT_MENU;
      ForShell := IsMenuMsg;
    end;
    WM_INITMENU, WM_UNINITMENUPOPUP, WM_MENUSELECT, WM_ENTERIDLE, WM_MENURBUTTONUP,
    WM_MENUDRAG, WM_MENUGETOBJECT, WM_MENUCOMMAND, WM_ENTERMENULOOP, WM_EXITMENULOOP,
    WM_NEXTMENU: begin
      ForShell := False;
      IsMenuMsg := True;
    end;
    else begin
      ForShell := False;
      IsMenuMsg := False;
    end;
  end;
  if ForShell then begin
    MsgResult := 0;
    if Assigned(ShellMenuCM3) then begin
      if ShellMenuCM3.HandleMenuMsg2(Msg, wParam, lParam, @MsgResult) = S_OK then
        Exit(MsgResult);
    end
    else if Assigned(ShellMenuCM2) then begin
      if ShellMenuCM2.HandleMenuMsg(Msg, wParam, lParam) = S_OK then begin
        if Msg = WM_INITMENUPOPUP then
          Exit(0);
        Exit(1); // TRUE: WM_MEASUREITEM/WM_DRAWITEM handled
      end;
    end;
  end;
  if IsMenuMsg then
    Result := DefWindowProcW(Wnd, Msg, wParam, lParam)
  else
    Result := CallWindowProcW(ShellMenuOldWndProcFunc, Wnd, Msg, wParam, lParam);
end;
{$ENDIF}

function TfrmExtFileDialog.Execute: Boolean;
begin
  Result := ShowModal = mrOK;
end;

procedure TfrmExtFileDialog.AddFileType(FileMask, DisplayName: String);
begin
  FFilterNames.Add(DisplayName);
  FFilterMasks.Add(FileMask);
  comboFileType.Items.Add(DisplayName + ' (' + FileMask + ')');
  comboFileType.AutoSizeItemWidth;
end;

procedure TfrmExtFileDialog.FormCreate(Sender: TObject);
begin
  if ClassType = TfrmExtFileDialog then
    raise Exception.CreateFmt('Constructor of base class %s called. Use one of its descendants instead.', [ClassName]);
  {$IfNDef WINDOWS}
  ShellTreeView.Images := MainForm.ImageListMain;
  ShellTreeView.OnGetImageIndex := ShellTreeViewGetImageIndex;
  ShellTreeView.OnGetSelectedIndex := ShellTreeViewGetSelectedIndex;
  ShellListView.SmallImages := MainForm.ImageListMain;
  ShellListView.LargeImages := MainForm.ImageListMain;
  {$ENDIF}
  ShellListView.OnCompare := ShellListViewCompare;
  ShellListView.OnColumnClick := ShellListViewColumnClick;
  ShellListView.OnFileAdded := ShellListViewFileAdded;
  // AutoSizeColumns off: ShellCtrls' AdjustColWidths distributes the client width
  // over its 3 built-in columns only and would push additional columns out to the
  // right. Keep fixed widths for the secondary columns instead and let
  // ShellListViewResize fit the name column into the remaining space.
  ShellListView.AutoSizeColumns := False;
  with ShellListView.Columns.Add do begin
    Caption := _('Modified');
    Width := 140;
  end;
  ShellListView.OnResize := ShellListViewResize;
  SetupListViewColumns;
  if AppSettings.ReadString(asFileDialogViewStyle) = 'icon' then
    ShellListView.ViewStyle := vsIcon
  else
    ShellListView.ViewStyle := vsReport;
  UpdateViewStyleButton;
  // Restore the sorting from the last use of any file dialog
  ShellListView.SortColumn := EnsureRange(AppSettings.ReadInt(asFileDialogSortColumn), 0, ShellListView.Columns.Count-1);
  if AppSettings.ReadBool(asFileDialogSortDescending) then
    FSortDirection := sdDescending
  else
    FSortDirection := sdAscending;
  ShellListView.OnAddItem := ShellListViewAddItem;
  comboRecentFolders.OnDropDown := comboRecentFoldersDropDown;
  comboRecentFolders.OnCloseUp := comboRecentFoldersCloseUp;
  ShowRecentFoldersHint;
  ShellTreeView.OnResize := ShellTreeViewResize;
  editSearch.OnChange := editSearchChange;
  editSearch.OnButtonClick := editSearchButtonClick;
  {$IFDEF WINDOWS}
  ShellListView.OnContextPopup := ShellListViewContextPopup;
  {$ENDIF}
  // Inline renaming of files and folders, like in Explorer: F2, or a second click
  // on the label of the already selected item. ReadOnly must be off for
  // TListItem.EditCaption and the built-in F2 handling.
  ShellListView.ReadOnly := False;
  ShellListView.OnMouseDown := ShellListViewMouseDown;
  ShellListView.OnKeyDown := ShellListViewKeyDown;
  ShellListView.OnEditing := ShellListViewEditing;
  ShellListView.OnEdited := ShellListViewEdited;
  FRenameTimer := TTimer.Create(Self);
  FRenameTimer.Enabled := False;
  {$IFDEF WINDOWS}
  FRenameTimer.Interval := GetDoubleClickTime;
  {$ELSE}
  FRenameTimer.Interval := 500;
  {$ENDIF}
  FRenameTimer.OnTimer := RenameTimerTimer;
  FFilterNames := TStringList.Create;
  FFilterMasks := TStringList.Create;
  FFilterIndex := 0;
  FDefaultExt := '';
  FEncodings := TStringList.Create;
  FLineBreakIndex := lbsNone;
  FFiles := TStringList.Create;
  comboFileType.Items.Clear;
  editFilename.Text := '';
  comboLineBreaks.Items.Clear;
  FOnTypeChange := nil;
end;

procedure TfrmExtFileDialog.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(Self);
  AppSettings.WriteString(asFileDialogPreviousDir, ShellTreeView.Path);
  FFilterNames.Free;
  FFilterMasks.Free;
  FEncodings.Free;
  FFiles.Free;
end;

procedure TfrmExtFileDialog.FormShow(Sender: TObject);
var
  LineBreakIndexInt: Integer;
  PreviousDir: String;
begin
  PreviousDir := AppSettings.ReadString(asFileDialogPreviousDir);
  if not FInitialDir.IsEmpty then
    SetInitialDir(FInitialDir)
  else if not PreviousDir.IsEmpty then
    SetInitialDir(PreviousDir)
  else
    SetInitialDir(GetUserDir);

  SetFilterIndex(FFilterIndex);
  SetupListViewColumns;
  comboRecentFolders.Enabled := not (ofNoChangeDir in FOptions);
  tbtnNewFolder.Enabled := not (ofNoChangeDir in FOptions);
  ShellTreeViewResize(ShellTreeView);

  comboEncoding.Items.AddStrings(FEncodings, True);
  comboEncoding.AutoSizeItemWidth;
  if (FEncodingIndex >=0) and (FEncodingIndex < comboEncoding.Items.Count) then
    comboEncoding.ItemIndex := FEncodingIndex;

  comboLineBreaks.Items.Add(_('Windows linebreaks'));
  comboLineBreaks.Items.Add(_('UNIX linebreaks'));
  comboLineBreaks.Items.Add(_('Mac OS linebreaks'));
  LineBreakIndexInt := Integer(FLineBreakIndex)-1; // we skip lbsNone
  if (LineBreakIndexInt >=0) and (LineBreakIndexInt < comboLineBreaks.Items.Count) then
    comboLineBreaks.ItemIndex := LineBreakIndexInt;

  // Show the sort indicator on the initially sorted column
  if (ShellListView.SortColumn >= 0) and (ShellListView.SortColumn < ShellListView.Columns.Count) then begin
    if FSortDirection = sdDescending then
      ShellListView.Columns[ShellListView.SortColumn].SortIndicator := siDescending
    else
      ShellListView.Columns[ShellListView.SortColumn].SortIndicator := siAscending;
  end;
end;

procedure TfrmExtFileDialog.IdleTimerTreeNodeScrolltoviewTimer(Sender: TObject);
begin
  // Work around tree node not being in scroll area in the first place
  if not Visible then
    Exit;
  if Assigned(ShellTreeView.Selected) then
    ShellTreeView.Selected.MakeVisible;
  IdleTimerTreeNodeScrolltoview.AutoEnabled := False;
  IdleTimerTreeNodeScrolltoview.Enabled := False;
end;

procedure TfrmExtFileDialog.SetOptions(AValue: TOpenOptions);
begin
  if FOptions = AValue then
    Exit;
  FOptions := AValue;
  if (ofAllowMultiSelect in FOptions) <> ShellListView.MultiSelect then begin
    // This would crash if we would do it in FormShow. See issue #2410
    ShellListView.MultiSelect := ofAllowMultiSelect in FOptions;
  end;
end;

procedure TfrmExtFileDialog.comboFileTypeChange(Sender: TObject);
var
  FileMask: String;
begin
  FFilterIndex := comboFileType.ItemIndex;
  if (comboFileType.ItemIndex >= 0) and (FFilterMasks.Count > comboFileType.ItemIndex) then
    FileMask := FFilterMasks[comboFileType.ItemIndex]
  else
    FileMask := '*.*';
  if ShellListView.Items.Count > 0 then
    ShellListView.Items[0].MakeVisible(false);
  ShellListView.Mask := FileMask;
  // SetMask repopulates the list, but PopulateWithRoot's Sort is skipped inside
  // Items.BeginUpdate, see ShellTreeViewChange. Re-apply the current sorting.
  ShellListView.Sort;
  if Assigned(FOnTypeChange) then
    FOnTypeChange(Self);
end;

procedure TfrmExtFileDialog.comboLineBreaksChange(Sender: TObject);
begin
  case comboLineBreaks.ItemIndex of
    0: FLineBreakIndex := lbsWindows;
    1: FLineBreakIndex := lbsUnix;
    2: FLineBreakIndex := lbsMac;
  end;
end;

procedure TfrmExtFileDialog.editFilenameEditingDone(Sender: TObject);
begin
  FFiles.Clear;
  FFiles.Add(GetFileName);
end;

procedure TfrmExtFileDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;

  if ModalResult = mrCancel then
    Exit;

  // Do nothing when user clicks OK without a selected file. Imitates Windows behaviour.
  if FileName.IsEmpty then begin
    CanClose := False;
    Exit;
  end;

  // Only for save dialogs: Ask user whether to overwrite the selected file
  if (ofOverwritePrompt in FOptions) and (FileExists(FileName)) then begin
    CanClose := MessageDialog(f_('File already exists: %s'+sLineBreak+sLineBreak+'Overwrite it?', [FileName]), mtConfirmation, [mbYes, mbNo]) = mrYes;
  end;

  // Only for open dialogs: Error if file does not exist
  if (ofFileMustExist in FOptions) and (not FileExists(FileName)) then begin
    ErrorDialog(f_('File does not exist: %s', [FileName]));
    CanClose := False;
  end;

  if CanClose then
    AddRecentFolder(ShellTreeView.Path);
end;

procedure TfrmExtFileDialog.AddRecentFolder(const Folder: String);
const
  MaxRecentFolders = 20;
var
  Recent: TStringList;
  i: Integer;
  Dir: String;
begin
  // Most recent first, without duplicates, limited to MaxRecentFolders
  if Folder.IsEmpty then
    Exit;
  Dir := IncludeTrailingPathDelimiter(Folder);
  Recent := Explode(DELIM, AppSettings.ReadString(asFileDialogRecentFolders));
  try
    for i:=Recent.Count-1 downto 0 do begin
      if SameFileName(IncludeTrailingPathDelimiter(Recent[i]), Dir) then
        Recent.Delete(i);
    end;
    Recent.Insert(0, Dir);
    while Recent.Count > MaxRecentFolders do
      Recent.Delete(Recent.Count-1);
    AppSettings.WriteString(asFileDialogRecentFolders, Implode(DELIM, Recent));
  finally
    Recent.Free;
  end;
end;

procedure TfrmExtFileDialog.ShowRecentFoldersHint;
begin
  // A csDropDownList combo box does not display its TextHint. Show the hint as the
  // only, selected item instead. comboRecentFoldersDropDown replaces it by the
  // folder list, so it never appears in the drop down list itself.
  comboRecentFolders.Items.Text := _('Recent folders');
  comboRecentFolders.ItemIndex := 0;
  FRecentFoldersHintShown := True;
end;

procedure TfrmExtFileDialog.comboRecentFoldersDropDown(Sender: TObject);
var
  Recent: TStringList;
begin
  // Reload on every drop down, another dialog may have added folders meanwhile
  Recent := Explode(DELIM, AppSettings.ReadString(asFileDialogRecentFolders));
  try
    comboRecentFolders.Items.Assign(Recent);
  finally
    Recent.Free;
  end;
  comboRecentFolders.ItemIndex := -1;
  FRecentFoldersHintShown := False;
end;

procedure TfrmExtFileDialog.comboRecentFoldersCloseUp(Sender: TObject);
var
  Folder: String;
begin
  // Navigate on close up, not on select: OnSelect also fires while moving through
  // the dropped down list with the arrow keys. Escape leaves ItemIndex at -1.
  if FRecentFoldersHintShown or (comboRecentFolders.ItemIndex < 0) then begin
    ShowRecentFoldersHint;
    Exit;
  end;
  Folder := comboRecentFolders.Items[comboRecentFolders.ItemIndex];
  // Back to the hint, the tree shows the current folder anyway
  ShowRecentFoldersHint;
  if ofNoChangeDir in FOptions then
    ErrorDialog(_('You cannot change the directory in this context.'))
  else if not DirectoryExists(Folder) then
    ErrorDialog(f_('Folder does not exist: %s', [Folder]))
  else begin
    // The tree is connected to the list view, which follows the path change.
    // ShellTreeViewChange re-applies the sorting.
    ShellTreeView.Path := Folder;
  end;
end;

procedure TfrmExtFileDialog.editSearchChange(Sender: TObject);
begin
  // Repopulate, ShellListViewAddItem applies the search text
  ShellListView.UpdateView;
  ShellListView.Sort;
end;

procedure TfrmExtFileDialog.editSearchButtonClick(Sender: TObject);
begin
  editSearch.Clear; // triggers editSearchChange
  editSearch.SetFocus;
end;

procedure TfrmExtFileDialog.comboEncodingChange(Sender: TObject);
begin
  FEncodingIndex := comboEncoding.ItemIndex;
end;

procedure TfrmExtFileDialog.ShellListViewClick(Sender: TObject);
begin
  FSelectedByClick := False; // see ShellListViewMouseDown
  if ShellListView.Selected <> nil then
    editFilename.Text := ShellListView.Selected.Caption
  else
    editFilename.Text := '';
end;

procedure TfrmExtFileDialog.ShellListViewDblClick(Sender: TObject);
begin
  FRenameTimer.Enabled := False;
  // TCustomListView.DblClick queues the inline editor when ReadOnly is off. A double
  // click confirms the dialog here, so suppress that, see ShellListViewEditing.
  // ShowEditor bails out before asking OnEditing when there is no ItemIndex.
  FSkipQueuedEdit := ShellListView.ItemIndex >= 0;
  if ShellListView.Selected <> nil then
    ModalResult := mrOK;
end;

procedure TfrmExtFileDialog.ShellListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
begin
  // Explorer-like rename: a single click on the label of the item which is already
  // the only selected one, in the focused list. Delayed by the double click time,
  // so a double click still confirms the dialog.
  // On Win32, the native list view changes the selection before MouseDown fires, so
  // Item.Selected alone does not tell whether it was selected before this click.
  // FSelectedByClick is set in OnSelectItem while the button is down, and reset here
  // and in OnClick (for widgetsets selecting after MouseDown).
  FRenameTimer.Enabled := False;
  FRenameCaption := '';
  if FSelectedByClick then begin
    FSelectedByClick := False;
    Exit;
  end;
  if (Button <> mbLeft) or (Shift * [ssShift, ssCtrl, ssAlt, ssDouble] <> []) then
    Exit;
  if (not ShellListView.Focused) or ShellListView.IsEditing or (ShellListView.SelCount <> 1) then
    Exit;
  Item := ShellListView.GetItemAt(X, Y);
  if (Item = nil) or (not Item.Selected) then
    Exit;
  if not PtInRect(Item.DisplayRect(drLabel), Classes.Point(X, Y)) then
    Exit;
  FRenameCaption := Item.Caption;
  FRenameTimer.Enabled := True;
end;

procedure TfrmExtFileDialog.RenameTimerTimer(Sender: TObject);
var
  Item: TListItem;
begin
  FRenameTimer.Enabled := False;
  Item := ShellListView.Selected;
  if (Item <> nil) and (ShellListView.SelCount = 1) and (Item.Caption = FRenameCaption)
    and ShellListView.Focused and (not ShellListView.IsEditing) then
    StartRename(Item);
  FRenameCaption := '';
end;

procedure TfrmExtFileDialog.ShellListViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  // F2 is handled by TCustomListView.KeyDown before this event, see ShellListViewEditing.
  // Just stop a pending click rename on any key.
  FRenameTimer.Enabled := False;
end;

procedure TfrmExtFileDialog.StartRename(Item: TListItem);
begin
  FRenameTimer.Enabled := False;
  FSkipQueuedEdit := False;
  Item.EditCaption;
end;

procedure TfrmExtFileDialog.ShellListViewEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  // Reached via F2 (TCustomListView.KeyDown), StartRename, and the editor queued by
  // TCustomListView.DblClick, which is the one to suppress.
  AllowEdit := (Item <> nil) and (not FSkipQueuedEdit);
  FSkipQueuedEdit := False;
end;

function TfrmExtFileDialog.CheckFileNameChars(const AName: String): Boolean;
const
  // Typed, as a single char literal would otherwise be a Char constant, not indexable
  {$IFDEF WINDOWS}
  InvalidChars: String = '\/:*?"<>|';
  {$ELSE}
  InvalidChars: String = '/';
  {$ENDIF}
var
  CharList: String;
  i: Integer;
begin
  // Shows an error and returns False if AName contains a character not allowed in
  // file and folder names
  Result := True;
  CharList := '';
  for i:=1 to Length(InvalidChars) do begin
    Result := Result and (Pos(InvalidChars[i], AName) = 0);
    CharList := CharList + InvalidChars[i] + ' ';
  end;
  if not Result then
    ErrorDialog(f_('A file name cannot contain any of the following characters: %s', [CharList.Trim]));
end;

procedure TfrmExtFileDialog.ShellListViewEdited(Sender: TObject;
  Item: TListItem; var AValue: String);
var
  OldName, NewName, OldPath, NewPath: String;
  i: Integer;
  IsFolder: Boolean;
begin
  // Called before the caption is set to AValue. Rename the file system object here,
  // and reset AValue to the old caption if that is not possible or cancelled.
  OldName := Item.Caption;
  NewName := Trim(AValue);
  {$IFDEF WINDOWS}
  // Windows strips trailing dots silently, like Explorer does
  while NewName.EndsWith('.') do
    Delete(NewName, Length(NewName), 1);
  {$ENDIF}
  AValue := OldName;
  if NewName.IsEmpty or (NewName = OldName) or (NewName = '.') or (NewName = '..') then
    Exit;
  if not CheckFileNameChars(NewName) then
    Exit;

  OldPath := ShellListView.GetPathFromItem(Item);
  NewPath := ExtractFilePath(OldPath) + NewName;
  IsFolder := TShellListItem(Item).IsFolder;
  if (FileExists(NewPath) or DirectoryExists(NewPath)) and (not SameFileName(OldPath, NewPath)) then begin
    ErrorDialog(f_('A file or folder with the name "%s" already exists.', [NewName]));
    Exit;
  end;
  if (not IsFolder) and (not SameFileName(ExtractFileExt(OldName), ExtractFileExt(NewName))) then begin
    if MessageDialog(_('If you change a file name extension, the file might become unusable. Do you want to change it?'),
      mtConfirmation, [mbYes, mbNo]) <> mrYes then
      Exit;
  end;
  if not RenameFile(OldPath, NewPath) then begin
    ErrorDialog(f_('Could not rename "%s" to "%s": %s', [OldName, NewName, SysErrorMessage(GetLastOSError)]));
    Exit;
  end;

  AValue := NewName;
  i := FFiles.IndexOf(OldPath);
  if i >= 0 then
    FFiles[i] := NewPath;
  if Item.Selected then
    editFilename.Text := NewName;
  // Re-read the folder after the caption was updated: new sort position, file
  // info in TShellListItem, and the tree node of a renamed folder.
  Application.QueueAsyncCall(RefreshListAsync, 0);
end;

procedure TfrmExtFileDialog.RefreshListAsync(Data: PtrInt);
begin
  if ShellListView.IsEditing then
    Exit;
  // Also updates the tree below the current folder. Keeps the selection by caption.
  ShellListView.UpdateView;
  ShellListView.Sort;
end;

procedure TfrmExtFileDialog.ShellListViewFileAdded(Sender: TObject;
  Item: TListItem);
{$IFNDEF WINDOWS}
var
  Ext: String;
{$ENDIF}
begin
  if TShellListItem(Item).IsFolder then begin
    {$IFNDEF WINDOWS}
    Item.ImageIndex := ICONINDEX_FOLDER;
    {$ENDIF}
  end
  else begin
    // Size column: our own formatting instead of ShellCtrls' hardcoded "bytes/kB/MB".
    if Item.SubItems.Count > 0 then
      Item.SubItems[0] := FormatByteNumber(TShellListItem(Item).FileInfo.Size);
    // Additional "Modified" column, see Columns.Add in FormCreate.
    // Note: TSearchRec.Time is a DOS LongInt, TimeStamp is the TDateTime
    Item.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss', TShellListItem(Item).FileInfo.TimeStamp));
    {$IFNDEF WINDOWS}
    Ext := ExtractFileExt(ShellListView.GetPathFromItem(Item));
    Item.ImageIndex := GetFileExtImageIndex(Ext);
    if Item.ImageIndex = -1 then
      Item.ImageIndex := 66; // page with question mark
    {$ENDIF}
  end;
end;

procedure TfrmExtFileDialog.ShellListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  ListItem: TListItem;
begin
  // Selected by the current mouse click? Then this click must not start renaming.
  if Selected and (GetKeyState(VK_LBUTTON) < 0) then
    FSelectedByClick := True;
  FFiles.Clear;
  for ListItem in ShellListView.Items do begin
    if ListItem.Selected then begin
      FFiles.Add(ShellListView.GetPathFromItem(ListItem));
    end;
  end;
end;

procedure TfrmExtFileDialog.ShellListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  // Folders first
  if TShellListItem(Item1).isFolder <> TShellListItem(Item2).isFolder then begin
    if TShellListItem(Item1).isFolder then
      Compare := -1
    else
      Compare := 1;
    Exit;
  end;
  case ShellListView.SortColumn of
    1: // Size: numeric compare
      Compare := CompareValue(TShellListItem(Item1).FileInfo.Size, TShellListItem(Item2).FileInfo.Size);
    2: // Type: by file extension
      Compare := AnsiCompareText(ExtractFileExt(Item1.Caption), ExtractFileExt(Item2.Caption));
    3: // Modified: numeric compare of the file time
      Compare := CompareValue(TShellListItem(Item1).FileInfo.TimeStamp, TShellListItem(Item2).FileInfo.TimeStamp);
  else // Name
    Compare := CompareFileNames(Item1.Caption, Item2.Caption);
  end;
  if Compare = 0 then
    Compare := CompareFileNames(Item1.Caption, Item2.Caption); // deterministic order
  // TCustomListView reverses the compare result for descending order only when
  // OnCompare is NOT set (see _CompareListViewItems), so reverse it ourselves.
  // SortDirection is not published by TShellListView, hence our own FSortDirection.
  if FSortDirection = sdDescending then
    Compare := -Compare;
end;

function TfrmExtFileDialog.CompareFileNames(const Name1, Name2: String): Integer;
{$IFDEF WINDOWS}
var
  W1, W2: WideString;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Natural sort like in Windows Explorer: "file2" sorts before "file10"
  W1 := UTF8ToUTF16(Name1);
  W2 := UTF8ToUTF16(Name2);
  Result := StrCmpLogicalW(PWideChar(W1), PWideChar(W2));
  {$ELSE}
  Result := AnsiCompareText(Name1, Name2);
  {$ENDIF}
end;

procedure TfrmExtFileDialog.ShellListViewColumnClick(Sender: TObject;
  Column: TListColumn);
const
  DirToIndicator: array[TSortDirection] of TSortIndicator = (siAscending, siDescending);
var
  i: Integer;
  NewDirection: TSortDirection;
begin
  // The list view sorts itself after this event (AutoSort is on), so predict the
  // resulting direction here and place the indicator on the clicked column.
  // Note: TCustomListView.AutoSortIndicator is protected and not published by
  // TShellListView, so we cannot use it and set TColumn.SortIndicator manually.
  if Column.Index <> ShellListView.SortColumn then
    FSortDirection := sdAscending
  else if FSortDirection = sdAscending then
    FSortDirection := sdDescending
  else
    FSortDirection := sdAscending;
  NewDirection := FSortDirection;
  for i := 0 to ShellListView.Columns.Count-1 do
    ShellListView.Columns[i].SortIndicator := siNone;
  Column.SortIndicator := DirToIndicator[NewDirection];
  AppSettings.WriteInt(asFileDialogSortColumn, Column.Index);
  AppSettings.WriteBool(asFileDialogSortDescending, NewDirection = sdDescending);
end;

procedure TfrmExtFileDialog.ShellListViewAddItem(Sender: TObject;
  const ABasePath: String; const AFileInfo: TSearchRec; var CanAdd: Boolean);
begin
  // With a single mask, ShellCtrls leaves the matching to FindFirst. On Windows,
  // FindFirstFile also matches the 8.3 short names, so "*.txt" lets "file.txtfoo"
  // through. Re-check the long file name strictly against the current mask.
  if CanAdd and ((AFileInfo.Attr and faDirectory) = 0) and (not ShellListView.Mask.Trim.IsEmpty) then
    CanAdd := MatchesWindowsMaskList(AFileInfo.Name, ShellListView.Mask.Trim, ';', FileNameCaseSensitive);
  // Additional search filter, case-insensitive substring match on files only.
  // Folders stay visible for navigation.
  if CanAdd and ((AFileInfo.Attr and faDirectory) = 0) and (editSearch.Text <> '') then
    CanAdd := Pos(UTF8LowerCase(editSearch.Text), UTF8LowerCase(AFileInfo.Name)) > 0;
end;

procedure TfrmExtFileDialog.SetupListViewColumns;
begin
  // Fixed widths for the secondary columns; the name column gets the remaining
  // space (see ShellListViewResize). Re-applied on every show and view change,
  // because switching the view style resets the native column widths.
  if ShellListView.Columns.Count > 3 then begin
    ShellListView.Columns[1].Width := 80; // Size
    ShellListView.Columns[2].Width := 80; // Type
    ShellListView.Columns[3].Width := 140; // Modified
  end;
  ShellListViewResize(ShellListView);
end;

procedure TfrmExtFileDialog.ShellListViewResize(Sender: TObject);
var
  i, W: Integer;
begin
  if ShellListView.Columns.Count > 1 then
  begin
    W := ShellListView.ClientWidth;
    for i := 1 to ShellListView.Columns.Count-1 do
      Dec(W, ShellListView.Columns[i].Width);
    ShellListView.Columns[0].Width := Max(50, W);
  end;
end;

procedure TfrmExtFileDialog.btnViewIconsClick(Sender: TObject);
begin
  ShellListView.ViewStyle := vsIcon;
  AppSettings.WriteString(asFileDialogViewStyle, 'icon');
  // Repopulate so item icons match the new view style (large vs. small icons).
  // Keeps the current sort order, see ShellTreeViewChange.
  ShellListView.UpdateView;
  SetupListViewColumns;
  UpdateViewStyleButton;
end;

procedure TfrmExtFileDialog.btnViewDetailsClick(Sender: TObject);
begin
  ShellListView.ViewStyle := vsReport;
  AppSettings.WriteString(asFileDialogViewStyle, 'report');
  ShellListView.UpdateView;
  SetupListViewColumns;
  UpdateViewStyleButton;
end;

procedure TfrmExtFileDialog.btnNewFolderClick(Sender: TObject);
var
  ParentDir, FolderName, NewPath: String;
  i: Integer;
begin
  // Create a subfolder in the current folder, and navigate into it
  if ofNoChangeDir in FOptions then begin
    ErrorDialog(_('You cannot change the directory in this context.'));
    Exit;
  end;
  ParentDir := ShellTreeView.Path;
  if ParentDir.IsEmpty or (not DirectoryExists(ParentDir)) then
    Exit;
  ParentDir := IncludeTrailingPathDelimiter(ParentDir);
  // Suggest a name which does not exist yet: "New folder", "New folder (2)", ...
  FolderName := _('New folder');
  i := 1;
  while DirectoryExists(ParentDir + FolderName) or FileExists(ParentDir + FolderName) do begin
    Inc(i);
    FolderName := _('New folder') + ' (' + IntToStr(i) + ')';
  end;
  if not InputQuery(_('Create folder'), f_('Name of new folder in %s', [ParentDir]), FolderName) then
    Exit;
  FolderName := Trim(FolderName);
  {$IFDEF WINDOWS}
  // Windows strips trailing dots silently, like Explorer does
  while FolderName.EndsWith('.') do
    Delete(FolderName, Length(FolderName), 1);
  {$ENDIF}
  if FolderName.IsEmpty or (FolderName = '.') or (FolderName = '..') then
    Exit;
  if not CheckFileNameChars(FolderName) then
    Exit;
  NewPath := ParentDir + FolderName;
  if DirectoryExists(NewPath) or FileExists(NewPath) then begin
    ErrorDialog(f_('A file or folder with the name "%s" already exists.', [FolderName]));
    Exit;
  end;
  if not CreateDir(NewPath) then begin
    ErrorDialog(f_('Could not create folder "%s": %s', [NewPath, SysErrorMessage(GetLastOSError)]));
    Exit;
  end;
  // Re-read the children of the current tree node, so the new folder node exists,
  // then select it. The list view follows, ShellTreeViewChange re-applies the sorting.
  if Assigned(ShellTreeView.Selected) then
    ShellTreeView.Refresh(ShellTreeView.Selected);
  try
    ShellTreeView.Path := NewPath;
  except
    on E:EShellCtrl do
      ShellTreeView.Path := ParentDir;
  end;
end;

procedure TfrmExtFileDialog.UpdateViewStyleButton;
begin
  // The button matching the current view style stays pressed (Down), the other
  // one is up. Both share GroupIndex 1, so they behave like radio buttons.
  tbtnViewIcons.Down := ShellListView.ViewStyle = vsIcon;
  tbtnViewDetails.Down := ShellListView.ViewStyle = vsReport;
  tbtnViewIcons.Hint := _('Switch to icon view');
  tbtnViewDetails.Hint := _('Switch to details view');
  tbtnNewFolder.Hint := _('Create new folder');
end;

procedure TfrmExtFileDialog.ShellListViewContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
{$IFDEF WINDOWS}
var
  Item: TListItem;
  FileName, Verb: String;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  FRenameTimer.Enabled := False;
  Item := ShellListView.GetItemAt(MousePos.X, MousePos.Y);
  if (Item <> nil) and (not Item.Selected) then
    ShellListView.Selected := Item; // let the context menu act on the clicked item, like in Explorer
  if Item <> nil then
    FileName := ShellListView.GetPathFromItem(Item)
  else
    FileName := ShellTreeView.Path; // right-click on empty space: context menu of the current folder
  if FileName.IsEmpty then
    Exit;
  Handled := ShowShellContextMenu(FileName, ShellListView.ClientToScreen(MousePos), Verb);
  if not Handled then
    Exit;
  if SameText(Verb, 'rename') then begin
    // The shell's rename verb only works in Explorer windows, use our inline editor.
    // On empty space, the menu belongs to the current folder, which is not in the list.
    if Item <> nil then
      StartRename(Item);
  end
  else begin
    // Delete, cut, paste, new file etc.: show the result. Queued, as the list items
    // are recreated, and the context popup handler is still running.
    Application.QueueAsyncCall(RefreshListAsync, 0);
  end;
  {$ENDIF}
end;

procedure TfrmExtFileDialog.ShellTreeViewChange(Sender: TObject; Node: TTreeNode
  );
begin
  // Re-apply the current sorting after the list was repopulated: PopulateWithRoot's
  // internal Sort is skipped while the list view is in BeginUpdate (SetRoot), so a
  // folder change would leave the items in raw FindFirst order although the header
  // still shows the sort indicator. OnChange fires after the repopulation, so this
  // is the right place to sort again.
  ShellListView.Sort;
end;

procedure TfrmExtFileDialog.ShellTreeViewResize(Sender: TObject);
begin
  // Keep the recent folders combo as wide as the tree below it, also when the user
  // drags the splitter. Both have the same left border spacing. Anchoring is not
  // possible here, as the combo box lives on pnlTopBar and is no sibling of the tree.
  comboRecentFolders.Width := ShellTreeView.Width;
  UpdateFormMinSize;
end;

procedure TfrmExtFileDialog.UpdateFormMinSize;
begin
  // The splitter respects ShellListView.Constraints by itself, but resizing the form
  // would just clip the alClient list view. Derive the minimum form size from the
  // list view's minimum, plus everything around it (tree, splitter, panels, borders).
  // Re-calculated whenever the tree width changes.
  if (ShellListView.Width <= 0) or (ShellListView.Height <= 0) then
    Exit;
  Constraints.MinWidth := Width - ShellListView.Width + ShellListView.Constraints.MinWidth;
  Constraints.MinHeight := Height - ShellListView.Height + ShellListView.Constraints.MinHeight;
end;

procedure TfrmExtFileDialog.ShellTreeViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  AllowChange := not (ofNoChangeDir in FOptions);
  if AllowChange and (ShellListView.Items.Count > 0) then
    ShellListView.Items[0].MakeVisible(false);
end;

procedure TfrmExtFileDialog.ShellTreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex := IfThen(Node.Level = 0, 1, ICONINDEX_FOLDER);
end;

procedure TfrmExtFileDialog.ShellTreeViewGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex := IfThen(Node.Level = 0, 1, ICONINDEX_FOLDER);
end;

procedure TfrmExtFileDialog.SetTitle(AValue: String);
begin
  Caption := AValue;
end;

function TfrmExtFileDialog.GetFileName: String;
begin
  if (editFilename.Text <> '') and (ShellTreeView.Selected <> nil) then begin
    Result := ShellTreeView.Path + editFilename.Text;
    if IsEmpty(ExtractFileExt(Result)) and (not FDefaultExt.IsEmpty) then
      Result := Result + '.' + FDefaultExt;
  end
  else if ShellListView.Selected <> nil then
    Result := ShellListView.GetPathFromItem(ShellListView.Selected)
  else
    Result := '';
end;

procedure TfrmExtFileDialog.SetFileName(const AValue: String);
var
  fn: String;
begin
  if AValue.IsEmpty then
    Exit;
  fn := ExpandFileName(AValue);
  SetInitialDir(ExtractFilePath(fn));
  editFilename.Text := ExtractFileName(fn);
  ShellListView.Selected := ShellListView.FindCaption(0, fn, false, true, true);
end;

procedure TfrmExtFileDialog.SetInitialDir(const AValue: String);
var
  CurPath: String;
  i: Integer;
begin
  // Try to set path on tree
  // Note both .FileName and .InitialDir go here, and .FileName := '' sets the initial dir to the app directory.
  if AValue.IsEmpty then
    Exit;
  FInitialDir := AValue;
  CurPath := AValue;
  for i:=0 to 10 do begin
    try
      ShellTreeView.Path := CurPath;
      Break;
    except
      on E:EShellCtrl do begin
        // Go up to parent folder
        // Testable on connections > advanced > file logging, due to virtual template strings.
        CurPath := ExtractFilePath(ExcludeTrailingPathDelimiter(CurPath));
        // In case, re-enable changing directory in tree
        Exclude(FOptions, ofNoChangeDir);
      end;
    end;
  end;
end;

procedure TfrmExtFileDialog.SetFilterIndex(AValue: Integer);
begin
  if (AValue >= 0) and (AValue < comboFileType.Items.Count) then begin
    comboFileType.ItemIndex := AValue;
    comboFileTypeChange(Self);
  end;
end;


{$IFDEF WINDOWS}
function TfrmExtFileDialog.ShowShellContextMenu(FileName: String;
  ScreenPos: TPoint; out Verb: String): Boolean;
var
  DesktopFolder, Folder: IShellFolder;
  FullPidl: PItemIDList;
  RelPidl: LPCITEMIDLIST;
  FolderPtr: Pointer;
  Eaten, Attrs: ULONG;
  ContextMenu: IContextMenu;
  Menu: HMENU;
  Cmd, CmdFirst: Integer;
  InvokeInfo: TCMINVOKECOMMANDINFO;
  Wide: WideString;
  VerbBuf: array[0..63] of WideChar;
  NewWndProc: Windows.WNDPROC;
  NewWndProcValue: LONG_PTR absolute NewWndProc;
begin
  Result := False;
  Verb := '';
  FileName := ExcludeTrailingPathDelimiter(ExpandFileName(FileName));
  if not (FileExists(FileName) or DirectoryExists(FileName)) then
    Exit;
  if SHGetDesktopFolder(DesktopFolder) <> S_OK then
    Exit;
  Attrs := 0;
  RelPidl := nil;
  Wide := UTF8ToUTF16(FileName); // strings in this app are UTF-8
  // Absolute PIDL of the file or folder
  if DesktopFolder.ParseDisplayName(Handle, nil, PWideChar(Wide), Eaten, FullPidl, Attrs) <> S_OK then
    Exit;
  try
    // Parent folder + relative PIDL, so the menu refers to the item itself
    if SHBindToParent(FullPidl, IID_IShellFolder, @FolderPtr, RelPidl) <> S_OK then
      Exit;
    Folder := IShellFolder(FolderPtr);
    if Folder.GetUIObjectOf(Handle, 1, RelPidl, IID_IContextMenu, nil, ContextMenu) <> S_OK then
      Exit;
    Menu := CreatePopupMenu;
    try
      CmdFirst := 1;
      if ContextMenu.QueryContextMenu(Menu, 0, CmdFirst, $7FFF, CMF_NORMAL or CMF_EXPLORE) < 0 then
        Exit;
      // Route the menu messages to the shell during TrackPopupMenu, see ShellMenuWndProc
      if not Supports(ContextMenu, IContextMenu3, ShellMenuCM3) then
        ShellMenuCM3 := nil;
      if not Supports(ContextMenu, IContextMenu2, ShellMenuCM2) then
        ShellMenuCM2 := nil;
      NewWndProc := ShellMenuWndProc;
      ShellMenuOldWndProc := SetWindowLongPtrW(Handle, GWLP_WNDPROC, NewWndProcValue);
      try
        Cmd := Integer(TrackPopupMenu(Menu, TPM_RETURNCMD or TPM_NONOTIFY or TPM_LEFTALIGN or TPM_LEFTBUTTON, ScreenPos.X, ScreenPos.Y, 0, Handle, nil));
      finally
        SetWindowLongPtrW(Handle, GWLP_WNDPROC, ShellMenuOldWndProc);
        ShellMenuOldWndProc := 0;
        ShellMenuCM3 := nil;
        ShellMenuCM2 := nil;
      end;
      if Cmd <= 0 then
        Exit;
      // Language independent verb of the chosen command, e.g. "rename" or "delete".
      // Not all commands have one, e.g. those of shell extensions.
      VerbBuf[0] := #0; // only read on success, then null-terminated by the shell
      if ContextMenu.GetCommandString(Cmd - CmdFirst, GCS_VERBW, nil, PAnsiChar(@VerbBuf[0]), Length(VerbBuf)) = S_OK then
        Verb := UTF16ToUTF8(WideString(PWideChar(@VerbBuf[0])));
      Result := True;
      if SameText(Verb, 'rename') then
        Exit; // handled by the caller
      InvokeInfo := Default(TCMINVOKECOMMANDINFO);
      InvokeInfo.cbSize := SizeOf(InvokeInfo);
      InvokeInfo.hwnd := Handle;
      // MAKEINTRESOURCE: the command offset is passed as pointer value
      InvokeInfo.lpVerb := nil;
      Inc(InvokeInfo.lpVerb, Cmd - CmdFirst);
      InvokeInfo.nShow := SW_SHOWNORMAL;
      ContextMenu.InvokeCommand(InvokeInfo);
      Result := True;
    finally
      DestroyMenu(Menu);
    end;
  finally
    CoTaskMemFree(FullPidl); // RelPidl points into FullPidl and must not be freed separately
  end;
end;
{$ENDIF}

{ TExtFileOpenDialog }

procedure TExtFileOpenDialog.FormCreate(Sender: TObject);
begin
  inherited;
  Include(FOptions, ofFileMustExist);
  Caption := _('Open existing file');
end;

procedure TExtFileOpenDialog.FormShow(Sender: TObject);
var
  EncodingVisible: Boolean;
begin
  inherited;
  EncodingVisible := comboEncoding.Items.Count > 0;
  lblEncoding.Visible := EncodingVisible;
  comboEncoding.Visible := EncodingVisible;
end;



{ TExtFileSaveDialog }

procedure TExtFileSaveDialog.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := _('Save to file');
end;

procedure TExtFileSaveDialog.FormShow(Sender: TObject);
var
  LinebreaksVisible: Boolean;
begin
  inherited;
  LinebreaksVisible := FLineBreakIndex in [lbsWindows, lbsUnix, lbsMac];
  lblLinebreaks.Visible := LinebreaksVisible;
  comboLineBreaks.Visible := LinebreaksVisible;
end;

end.

