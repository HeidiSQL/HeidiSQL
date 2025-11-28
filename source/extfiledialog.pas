unit extfiledialog;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ShellCtrls, ComCtrls, apphelpers, extra_controls;

type

  { TfrmExtFileDialog }

  TfrmExtFileDialog = class(TExtForm)
    btnCancel: TButton;
    btnOk: TButton;
    comboLineBreaks: TComboBox;
    comboEncoding: TComboBox;
    comboFileType: TComboBox;
    editFilename: TEdit;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShellListViewClick(Sender: TObject);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
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
    procedure SetTitle(AValue: String);
    function GetFileName: String;
    procedure SetFileName(const AValue: String);
    procedure SetInitialDir(const AValue: String);
    procedure SetFilterIndex(AValue: Integer);
  public
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
    property Title: String write SetTitle;
    function Execute: Boolean;
    procedure AddFileType(FileMask, DisplayName: String);
    property FileName: String read GetFileName write SetFileName;
    property InitialDir: String read FInitialDir write SetInitialDir;
    class var PreviousDir: String;
    property DefaultExt: String read FDefaultExt write FDefaultExt;
    property Options: TOpenOptions read FOptions write FOptions;
    property Files: TStringList read FFiles;
    property FilterIndex: Integer read FFilterIndex write SetFilterIndex;
  end;

  // File-open-dialog with encoding selector
  TExtFileOpenDialog = class(TfrmExtFileDialog)
    procedure FormShow(Sender: TObject); overload;
    public
      property Encodings: TStringList read FEncodings write FEncodings;
      property EncodingIndex: Integer read FEncodingIndex write FEncodingIndex;
  end;

  TExtFileSaveDialog = class(TfrmExtFileDialog)
    procedure FormShow(Sender: TObject); overload;
    public
      property LineBreakIndex: TLineBreaks read FLineBreakIndex write FLineBreakIndex;
  end;

implementation

{$R *.lfm}


function TfrmExtFileDialog.Execute: Boolean;
begin
  Result := ShowModal = mrOK;
end;

procedure TfrmExtFileDialog.AddFileType(FileMask, DisplayName: String);
begin
  FFilterNames.Add(DisplayName);
  FFilterMasks.Add(FileMask);
  comboFileType.Items.Add(DisplayName + ' (' + FileMask + ')');
end;

procedure TfrmExtFileDialog.FormCreate(Sender: TObject);
begin
  if ClassType = TfrmExtFileDialog then
    raise Exception.CreateFmt('Constructor of base class %s called. Use one of its descendants instead.', [ClassName]);
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
  PreviousDir := ShellTreeView.Path;
  FFilterNames.Free;
  FFilterMasks.Free;
  FEncodings.Free;
  FFiles.Free;
end;

procedure TfrmExtFileDialog.FormShow(Sender: TObject);
var
  LineBreakIndexInt: Integer;
begin
  ShellListView.MultiSelect := ofAllowMultiSelect in FOptions;
  // Todo: support ofFileMustExist
  if not FInitialDir.IsEmpty then
    SetInitialDir(FInitialDir)
  else if not PreviousDir.IsEmpty then
    SetInitialDir(PreviousDir)
  else
    SetInitialDir(GetUserDir);

  SetFilterIndex(FFilterIndex);

  comboEncoding.Items.AddStrings(FEncodings, True);
  if (FEncodingIndex >=0) and (FEncodingIndex < comboEncoding.Items.Count) then
    comboEncoding.ItemIndex := FEncodingIndex;

  comboLineBreaks.Items.Add(_('Windows linebreaks'));
  comboLineBreaks.Items.Add(_('UNIX linebreaks'));
  comboLineBreaks.Items.Add(_('Mac OS linebreaks'));
  LineBreakIndexInt := Integer(FLineBreakIndex)-1; // we skip lbsNone
  if (LineBreakIndexInt >=0) and (LineBreakIndexInt < comboLineBreaks.Items.Count) then
    comboLineBreaks.ItemIndex := LineBreakIndexInt;
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

  // Ask user whether to overwrite the selected file
  if (Self is TExtFileSaveDialog) and (ofOverwritePrompt in FOptions) and (FileExists(FileName)) then begin
    CanClose := MessageDialog(f_('File already exists: %s'+sLineBreak+sLineBreak+'Overwrite it?', [FileName]), mtConfirmation, [mbYes, mbNo]) = mrYes;
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

procedure TfrmExtFileDialog.ShellTreeViewChange(Sender: TObject; Node: TTreeNode
  );
begin
  lblPath.Caption := ShellTreeView.Path;
end;

procedure TfrmExtFileDialog.ShellTreeViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  AllowChange := not (ofNoChangeDir in FOptions);
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
  fn := ExpandFileName(AValue);
  SetInitialDir(ExtractFilePath(fn));
  editFilename.Text := ExtractFileName(fn);
  ShellListView.Selected := ShellListView.FindCaption(0, fn, false, true, true);
end;

procedure TfrmExtFileDialog.SetInitialDir(const AValue: String);
begin
  FInitialDir := AValue;
  try
    ShellTreeView.Path := FInitialDir;
  except
    on E:EInvalidPath do begin
      ErrorDialog(E.Message);
      // In case, re-enable changing directory in tree
      Exclude(FOptions, ofNoChangeDir);
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


{ TExtFileOpenDialog }

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

