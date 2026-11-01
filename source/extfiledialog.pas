unit extfiledialog;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ShellCtrls, ComCtrls, apphelpers, extra_controls, Math, generic_types
  {$IFDEF WINDOWS}, Windows, WinDirs, ActiveX, ShlObj, LazUTF8 {$ENDIF};

type

  { TfrmExtFileDialog }

  TfrmExtFileDialog = class(TExtForm)
    btnCancel: TButton;
    btnOk: TButton;
    comboLineBreaks: TComboBox;
    comboEncoding: TComboBox;
    comboFileType: TComboBox;
    editFilename: TEdit;
    IdleTimerTreeNodeScrolltoview: TIdleTimer;
    lblPath: TLabel;
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
    procedure lblPathClick(Sender: TObject);
    procedure lblPathMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblPathMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ShellListViewClick(Sender: TObject);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure ShellListViewFileAdded(Sender: TObject; Item: TListItem);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellListViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ShellListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ShellListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
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
    FClickedPathPart: String;
    FSortDirection: TSortDirection; // SortDirection is not published by TShellListView
    function CompareFileNames(const Name1, Name2: String): Integer;
    {$IFDEF WINDOWS}
    function ShowShellContextMenu(FileName: String; ScreenPos: TPoint): Boolean;
    {$ENDIF}
    procedure SetTitle(AValue: String);
    function GetFileName: String;
    procedure SetFileName(const AValue: String);
    procedure SetInitialDir(const AValue: String);
    procedure SetFilterIndex(AValue: Integer);
    function GetPathPartAt(X: Integer): String;
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
  {$ENDIF}
  ShellListView.OnCompare := ShellListViewCompare;
  ShellListView.OnColumnClick := ShellListViewColumnClick;
  ShellListView.OnFileAdded := ShellListViewFileAdded;
  with ShellListView.Columns.Add do begin
    Caption := _('Modified');
    Width := 140;
  end;
  FSortDirection := sdAscending;
  {$IFDEF WINDOWS}
  ShellListView.OnContextPopup := ShellListViewContextPopup;
  {$ENDIF}
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
  if (ShellListView.SortColumn >= 0) and (ShellListView.SortColumn < ShellListView.Columns.Count) then
    ShellListView.Columns[ShellListView.SortColumn].SortIndicator := siAscending;
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

procedure TfrmExtFileDialog.lblPathClick(Sender: TObject);
begin
  if (not FClickedPathPart.IsEmpty) and DirectoryExists(FClickedPathPart) then begin
    if ofNoChangeDir in FOptions then
      ErrorDialog('You cannot change the directory in this context.')
    else
      ShellTreeView.Path := FClickedPathPart;
  end;
end;

procedure TfrmExtFileDialog.lblPathMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FClickedPathPart := GetPathPartAt(X);
end;

function TfrmExtFileDialog.GetPathPartAt(X: Integer): String;
var
  i, CharIndex, CurrentWidth: Integer;
  TextWidth: Integer;
begin
  CharIndex := -1;
  CurrentWidth := 0;
  Result := '';

  lblPath.Canvas.Font := lblPath.Font;
  for i := 1 to Length(lblPath.Caption) do
  begin
    TextWidth := lblPath.Canvas.TextWidth(Copy(lblPath.Caption, i, 1));
    if (X >= CurrentWidth) and (X < CurrentWidth + TextWidth) then
    begin
      CharIndex := i; // 1-based character index clicked
      Break;
    end;
    Inc(CurrentWidth, TextWidth);
  end;
  if CharIndex > 0 then begin
    i := 0;
    for i:=CharIndex to Length(lblPath.Caption) do begin
      if Copy(lblPath.Caption, i, 1) = PathDelim then
        break;
    end;
    Result := Copy(lblPath.Caption, 1, i);
  end;
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

procedure TfrmExtFileDialog.lblPathMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lblPath.Hint := GetPathPartAt(X);
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
end;

procedure TfrmExtFileDialog.comboEncodingChange(Sender: TObject);
begin
  FEncodingIndex := comboEncoding.ItemIndex;
end;

procedure TfrmExtFileDialog.ShellListViewClick(Sender: TObject);
begin
  if ShellListView.Selected <> nil then
    editFilename.Text := ShellListView.Selected.Caption
  else
    editFilename.Text := '';
end;

procedure TfrmExtFileDialog.ShellListViewDblClick(Sender: TObject);
begin
  if ShellListView.Selected <> nil then
    ModalResult := mrOK;
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
    // ShellCtrls stores the raw byte count in Item.Data.
    if Item.SubItems.Count > 0 then
      Item.SubItems[0] := FormatByteNumber(Int64(PtrInt(Item.Data)));
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
    1: // Size: numeric compare, ShellCtrls stores the raw byte count in Item.Data
      Compare := CompareValue(Int64(PtrInt(Item1.Data)), Int64(PtrInt(Item2.Data)));
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
end;

procedure TfrmExtFileDialog.ShellListViewContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  {$IFDEF WINDOWS}
  Item: TListItem;
  FileName: String;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  Item := ShellListView.GetItemAt(MousePos.X, MousePos.Y);
  if (Item <> nil) and (not Item.Selected) then
    ShellListView.Selected := Item; // let the context menu act on the clicked item, like in Explorer
  if Item <> nil then
    FileName := ShellListView.GetPathFromItem(Item)
  else
    FileName := ShellTreeView.Path; // right-click on empty space: context menu of the current folder
  if not FileName.IsEmpty then
    Handled := ShowShellContextMenu(FileName, ShellListView.ClientToScreen(MousePos));
  {$ENDIF}
end;

procedure TfrmExtFileDialog.ShellTreeViewChange(Sender: TObject; Node: TTreeNode
  );
begin
  lblPath.Caption := ShellTreeView.Path;
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
  ScreenPos: TPoint): Boolean;
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
begin
  Result := False;
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
      Cmd := Integer(TrackPopupMenu(Menu, TPM_RETURNCMD or TPM_NONOTIFY or TPM_LEFTALIGN or TPM_LEFTBUTTON, ScreenPos.X, ScreenPos.Y, 0, Handle, nil));
      if Cmd <= 0 then
        Exit;
      InvokeInfo := Default(TCMINVOKECOMMANDINFO);
      InvokeInfo.cbSize := SizeOf(InvokeInfo);
      InvokeInfo.hwnd := Handle;
      InvokeInfo.lpVerb := PAnsiChar(PtrUInt(Cmd - CmdFirst));
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

