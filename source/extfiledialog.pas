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
    comboEncoding: TComboBox;
    comboFileType: TComboBox;
    editFilename: TEdit;
    lblEncoding: TLabel;
    lblFilename: TLabel;
    pnlBottom: TPanel;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    splitterMain: TSplitter;
    procedure comboEncodingChange(Sender: TObject);
    procedure comboFileTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShellListViewClick(Sender: TObject);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FInitialDir: String;
    FFilterNames: TStringList;
    FFilterMasks: TStringList;
    FDefaultExt: String;
    FEncodings: TStringList;
    FEncodingIndex: Cardinal;
    FOptions: TOpenOptions;
    FFiles: TStringList;
    function GetFileName: String;
    procedure SetFileName(const AValue: String);
    procedure SetInitialDir(const AValue: String);
  public
    function Execute: Boolean;
    procedure AddFileType(FileMask, DisplayName: String);
    property FileName: String read GetFileName write SetFileName;
    property InitialDir: String read FInitialDir write SetInitialDir;
    class var PreviousDir: String;
    property DefaultExt: String read FDefaultExt write FDefaultExt;
    property Encodings: TStringList read FEncodings write FEncodings;
    property EncodingIndex: Cardinal read FEncodingIndex write FEncodingIndex;
    property Options: TOpenOptions read FOptions write FOptions;
    property Files: TStringList read FFiles;

  end;

  // File-open-dialog with encoding selector
  TExtFileOpenDialog = class(TfrmExtFileDialog)
    procedure FormCreate(Sender: TObject); overload;
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
  FEncodings := TStringList.Create;
  FFiles := TStringList.Create;
  comboFileType.Items.Clear;
  editFilename.Text := '';
  lblEncoding.Enabled := False;
  comboEncoding.Enabled := False;
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
begin
  ShellListView.MultiSelect := ofAllowMultiSelect in FOptions;
  ShellTreeView.Enabled := not (ofNoChangeDir in FOptions);
  // Todo: support ofOverwritePrompt, ofFileMustExist
  if FInitialDir.IsEmpty then begin
    if not PreviousDir.IsEmpty then
      SetInitialDir(PreviousDir)
    else
      SetInitialDir(GetUserDir);
  end;

  comboFileType.ItemIndex := 0;
  comboFileType.OnChange(Sender);

  comboEncoding.Items.AddStrings(FEncodings, True);
  if (FEncodingIndex >=0) and (FEncodingIndex < comboEncoding.Items.Count) then
    comboEncoding.ItemIndex := FEncodingIndex;
end;

procedure TfrmExtFileDialog.comboFileTypeChange(Sender: TObject);
var
  FileMask: String;
begin
  if (comboFileType.ItemIndex >= 0) and (FFilterMasks.Count > comboFileType.ItemIndex) then
    FileMask := FFilterMasks[comboFileType.ItemIndex]
  else
    FileMask := '*.*';
  ShellListView.Mask := FileMask;
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

function TfrmExtFileDialog.GetFileName: String;
begin
  if ShellListView.Selected <> nil then
    Result := ShellListView.GetPathFromItem(ShellListView.Selected)
  else
    Result := '';
end;

procedure TfrmExtFileDialog.SetFileName(const AValue: String);
var
  fn: String;
begin
  fn := ExpandFileName(AValue);
  ShellTreeView.Path := ExtractFilePath(fn);
  editFilename.Text := ExtractFileName(fn);
  ShellListView.Selected := ShellListView.FindCaption(0, fn, false, true, true);
end;

procedure TfrmExtFileDialog.SetInitialDir(const AValue: String);
begin
  ShellTreeView.Path := AValue;
end;



{ TExtFileOpenDialog }

procedure TExtFileOpenDialog.FormCreate(Sender: TObject);
begin
  inherited;
  lblEncoding.Enabled := True;
  comboEncoding.Enabled := True;
end;


end.

