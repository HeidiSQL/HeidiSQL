unit insertfiles;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, Buttons, ShellApi, Math, insertfiles_progress;

type TCol = record
  Name   : String;   // for displaying in lists
  isBLOB : Boolean;  // to decide whether to show or not
  Value  : String;   // for other fiels
  quote  : Boolean;  // dito
end;

type
  TfrmInsertFiles = class(TForm)
    ListViewFiles: TListView;
    ButtonAddFiles: TButton;
    ComboBoxDBs: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBoxTables: TComboBox;
    Label3: TLabel;
    ComboBoxColumns: TComboBox;
    ButtonInsert: TButton;
    ButtonCancel: TButton;
    ButtonRemoveFiles: TButton;
    LabelFileCount: TLabel;
    CheckBoxShowOnlyBlobs: TCheckBox;
    OpenDialog: TOpenDialog;
    LargeImages: TImageList;
    SmallImages: TImageList;
    ButtonClearList: TButton;
    GroupBox1: TGroupBox;
    ListBoxOtherFields: TListBox;
    CheckBoxQuote: TCheckBox;
    Label4: TLabel;
    ComboBoxValue: TComboBox;
    Label5: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ComboBoxDBsChange(Sender: TObject);
    procedure ComboBoxTablesChange(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure Modified;
    procedure ButtonAddFilesClick(Sender: TObject);
    procedure ListViewFilesClick(Sender: TObject);
    procedure ButtonRemoveFilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonClearListClick(Sender: TObject);
    procedure DisplayColumns(Sender: TObject);
    procedure ComboBoxColumnsChange(Sender: TObject);
    procedure ListViewFilesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBoxOtherFieldsClick(Sender: TObject);
    procedure FieldChange(Sender: TObject);
    procedure ButtonInsertClick(Sender: TObject);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure addfile(filename: String);
    procedure ListViewFilesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);

  private
    FProgressForm : TfrmInsertFilesProgress;

  public
    { Public declarations }
    cols : Array of TCol;
    procedure AcceptFiles( var msg : TMessage ); message WM_DROPFILES;
  end;

  function InsertFilesWindow (AOwner : TComponent; Flags : String = '') : Boolean;


implementation

uses main, childwin, helpers;

{$R *.DFM}


function InsertFilesWindow (AOwner : TComponent; Flags : String = '') : Boolean;
var
  f : TfrmInsertFiles;
begin
  f := TfrmInsertFiles.Create(AOwner);
  // todo: pass flags
  Result := (f.ShowModal=mrOK);
  FreeAndNil (f);
end;

{ FormShow }
procedure TfrmInsertFiles.FormShow(Sender: TObject);
var
  tn : TTreeNode;
  i : Integer;
begin
  ComboBoxDBs.Items.Clear;
  with TMDIChild(Mainform.ActiveMDIChild) do
  begin
    self.Caption := ZConn.HostName + ' - Insert files into table ...';
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Level = 1 then
        ComboBoxDBs.Items.Add(tn.Text);
    end;

    for i:=0 to ComboBoxDBs.Items.Count-1 do
    begin
      if ComboBoxDBs.Items[i] = ActualDatabase then
      begin
        ComboBoxDBs.ItemIndex := i;
      end;
    end;

    if ComboBoxDBs.ItemIndex = -1 then
    begin
      ComboBoxDBs.ItemIndex := 0;
    end;

  end;
  ComboBoxDBsChange(self);
end;


{ Read tables from selected DB }
procedure TfrmInsertFiles.ComboBoxDBsChange(Sender: TObject);
var
  tn, child : TTreeNode;
  i,j : Integer;
begin
  // read tables from db
  ComboBoxTables.Items.Clear;
  with TMDIChild(Mainform.ActiveMDIChild) do
  begin
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Text = ComboBoxDBs.Text then
      begin
        child := tn.getFirstChild;
        for j:=0 to tn.Count-1 do
        begin
          ComboBoxTables.Items.Add(child.Text);
          child := tn.getNextChild(child);
        end;
      end;
    end;
  end;
  if ComboBoxTables.Items.Count > 0 then
  begin
    ComboBoxTables.ItemIndex := 0;
  end;
  ComboBoxTablesChange(self);
end;

{ Show Columns from selected table }
procedure TfrmInsertFiles.ComboBoxTablesChange(Sender: TObject);
var i : Integer;
begin
  setlength(cols, 0);
  if ComboBoxTables.ItemIndex > -1 then
    with TMDIChild(Mainform.ActiveMDIChild) do
    begin
      GetResults('SHOW FIELDS FROM '+mainform.mask(ComboBoxDBs.Text)+'.'+mainform.mask(ComboBoxTables.Text), ZQuery3);
      for i:=1 to ZQuery3.RecordCount do
      begin
        setlength(cols, length(cols)+1);
        cols[length(cols)-1].Name := ZQuery3.Fields[0].AsString;
        cols[length(cols)-1].isBLOB := (pos('blob', lowercase(ZQuery3.Fields[1].AsString)) > 0) or (pos('text', lowercase(ZQuery3.Fields[1].AsString)) > 0);
        cols[length(cols)-1].Value := 'NULL';
        cols[length(cols)-1].Quote := false;
        ZQuery3.Next;
      end;
    end;
  DisplayColumns(self);
end;

{ display columns in combobox }
procedure TfrmInsertFiles.DisplayColumns(Sender: TObject);
var i : Integer;
  sel : String;
begin
  sel := ComboBoxColumns.Text;
  ComboBoxColumns.Items.Clear;
  for i:=0 to length(cols)-1 do begin
    if (CheckBoxShowOnlyBlobs.Checked and cols[i].isBLOB) or (not CheckBoxShowOnlyBlobs.Checked) then
      ComboBoxColumns.Items.Add(cols[i].Name)
  end;
  if ComboBoxColumns.Items.Count > 0 then begin
    if sel <> '' then
      ComboBoxColumns.ItemIndex := ComboBoxColumns.Items.IndexOf(sel);
    if ComboBoxColumns.ItemIndex = -1 then
      ComboBoxColumns.ItemIndex := 0;
  end;
  ComboBoxColumnsChange(self);
  Modified;
end;


{ Cancel! }
procedure TfrmInsertFiles.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

{ buttons need to be checked for being enabled }
procedure TfrmInsertFiles.Modified;
begin
  ButtonInsert.Enabled := (ComboBoxColumns.ItemIndex > -1) and (ListViewFiles.Items.Count > 0);
end;


procedure TfrmInsertFiles.addfile(filename: String);
var
  li : TListItem;
  filesize, attrs: Integer;
  Info    : TSHFileInfo;
begin
  attrs := filegetattr(filename);
  if attrs and faDirectory <> 0 then
    exit;
  li := ListViewFiles.Items.Add;
  li.Caption := filename;
  filesize := ceil(_getfilesize(filename) / 1024);
  li.SubItems.Add(Format('%u', [filesize]));
  SHGetFileInfo(PChar(filename),0,Info,SizeOf(TSHFileInfo),SHGFI_SYSIconIndex or SHGFI_TYPENAME);
  li.ImageIndex:=Info.IIcon;
end;

{ Add file(s) to list }
procedure TfrmInsertFiles.ButtonAddFilesClick(Sender: TObject);
var
  i : Integer;
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    ListViewFiles.Items.BeginUpdate;
    try
      for i:=0 to OpenDialog.Files.Count-1 do
        addfile(OpenDialog.Files[i]);
    finally
      ListViewFiles.Items.EndUpdate;
    end;
    ListViewFilesClick(self);
    Screen.Cursor := crDefault;
  end;
end;


{ files(s) have been (un)selected }
procedure TfrmInsertFiles.ListViewFilesClick(Sender: TObject);
var
   i: Integer;
   kbytes : Real;
begin
  ButtonRemoveFiles.Enabled := ListViewFiles.SelCount > 0;
  ButtonClearList.Enabled := ListViewFiles.Items.Count > 0;
  kbytes := 0;
  with LabelFileCount do
  begin
    for i:=0 to ListViewFiles.Items.Count-1 do
    begin
      if ListViewFiles.Items[i].SubItems.Count > 0 then
      begin
        kbytes := kbytes + strtointdef(ListViewFiles.Items[i].Subitems[0], 0);
      end;
    end;
    Caption := format('%u files, %.0n KB, %u files selected.', [ListViewFiles.Items.count, kbytes, ListViewFiles.selcount]);
  end;
  Modified;
end;

{ Remove one or more files from list }
procedure TfrmInsertFiles.ButtonRemoveFilesClick(Sender: TObject);
var
  i, lastdel : Integer;
begin
  Screen.Cursor := crHourglass;
  lastdel := -1;
  ListViewFiles.Items.BeginUpdate;
  for i:=ListViewFiles.Items.Count-1 downto 0 do
  begin
    if ListViewFiles.Items[i].Selected then
    begin
      ListViewFiles.Items[i].Delete;
      lastdel := i;
    end;
  end;
  ListViewFiles.Items.EndUpdate;
  if ListViewFiles.Items.count > lastdel then
  begin
    ListViewFiles.Selected := ListViewFiles.Items[lastdel];
  end
  else if ListViewFiles.Items.count > 0 then
  begin
    ListViewFiles.Selected := ListViewFiles.Items[lastdel-1];
  end;
  ListViewFiles.SetFocus;
  ListViewFilesClick(self); // count files and (de-)activate buttons
  Screen.Cursor := crDefault;
end;


{ Run only once }
procedure TfrmInsertFiles.FormCreate(Sender: TObject);
var
 SysIL : uint;
 SFI   : TSHFileInfo;

begin
  LargeImages := TImageList.Create(MainForm);
  SysIL := SHGetFileInfo('', 0, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if SysIL <> 0 then begin
    LargeImages.Handle := SysIL;
    LargeImages.ShareImages := TRUE;
  end;
  SmallImages := TImageList.Create(MainForm);
  SysIL := SHGetFileInfo('', 0, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysIL <> 0 then begin
    SmallImages.Handle := SysIL;
    SmallImages.ShareImages := TRUE;
  end;
  ListViewFiles.LargeImages:=LargeImages;
  ListViewFiles.SmallImages:=SmallImages;
  DragAcceptFiles( Handle , True );
end;



{ Clear list }
procedure TfrmInsertFiles.ButtonClearListClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  ListViewFiles.Items.BeginUpdate;
  ListViewFiles.Items.Clear;
  ListViewFiles.Items.EndUpdate;
  ListViewFilesClick(self);
  Screen.Cursor := crDefault;
end;

{ collect all other columns }
procedure TfrmInsertFiles.ComboBoxColumnsChange(Sender: TObject);
var i : Integer;
begin
  ListBoxOtherFields.Items.BeginUpdate;
  ListBoxOtherFields.Items.Clear;
  for i:=0 to length(cols)-1 do
  begin
    if ComboBoxColumns.Text <> cols[i].Name then
      ListBoxOtherFields.Items.Add(cols[i].Name);
  end;
  ListBoxOtherFields.Items.EndUpdate;
  ListBoxOtherFieldsClick(self);
end;

{ remove selected by pressing Delete }
procedure TfrmInsertFiles.ListViewFilesKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    ButtonRemoveFilesClick(self);
end;

{ other field selected => display value and quote }
procedure TfrmInsertFiles.ListBoxOtherFieldsClick(Sender: TObject);
var
  i : Integer;
  enable : Boolean;
begin
  enable := ListBoxOtherFields.ItemIndex > -1;
  ComboBoxValue.Enabled := enable;
  CheckBoxQuote.Enabled := enable;
  if not enable then exit;
  for i:=0 to length(cols)-1 do
  begin
    if ListBoxOtherFields.Items[ListBoxOtherFields.ItemIndex] = cols[i].Name then
      break;
  end;
  ComboBoxValue.Text := cols[i].Value;
  CheckBoxQuote.Checked := cols[i].Quote;
end;

{ store value and quote of field }
procedure TfrmInsertFiles.FieldChange(Sender: TObject);
var i : Integer;
begin
  if ListBoxOtherFields.ItemIndex = -1 then exit;
  for i:=0 to length(cols)-1 do
    if ListBoxOtherFields.Items[ListBoxOtherFields.ItemIndex] = cols[i].Name then break;
  cols[i].Value := ComboBoxValue.Text;
  cols[i].Quote := CheckBoxQuote.Checked;
end;

{ ok, let's rock! }
procedure TfrmInsertFiles.ButtonInsertClick(Sender: TObject);
begin
  FProgressForm := TfrmInsertFilesProgress.Create(Self); 
  FProgressForm.InsertFilesForm := Self;
  FProgressForm.ShowModal;
end;

{ Execute selected file }
procedure TfrmInsertFiles.ListViewFilesDblClick(Sender: TObject);
begin
  if ListViewFiles.Selected <> nil then
    ShellExec( ListViewFiles.Selected.Caption );
end;

{ for file-dropping into listview }
procedure TfrmInsertFiles.AcceptFiles( var msg : TMessage );
const
  cnMaxFileNameLen = 255;
var
  i,
  nCount     : integer;
  acFileName : array [0..cnMaxFileNameLen] of char;
begin
  // find out how many files we're accepting
  Screen.Cursor := crHourglass;
  nCount := DragQueryFile( msg.WParam,
                           $FFFFFFFF,
                           acFileName,
                           cnMaxFileNameLen );

  // query Windows one at a time for the file name
  ListViewFiles.Items.BeginUpdate;
  try
    for i := 0 to nCount-1 do
    begin
      DragQueryFile( msg.WParam, i, acFileName, cnMaxFileNameLen );
      // do your thing with the acFileName
      // MessageBox( Handle, acFileName, '', MB_OK );
      addfile(acFileName);
    end;
  finally
    ListViewFiles.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;

  // let Windows know that you're done
  DragFinish( msg.WParam );
end;


procedure TfrmInsertFiles.ListViewFilesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  ListViewFilesClick(self);
end;

end.
