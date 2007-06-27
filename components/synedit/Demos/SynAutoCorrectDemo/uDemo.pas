unit uDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SynAutoCorrect, ExtCtrls, SynEdit, ShellApi;

type
  TfrmDemo = class(TForm)
    edtEditor: TSynEdit;
    pnlContainer: TPanel;
    lblLabel1: TLabel;
    chkEnabled: TCheckBox;
    chkIgnoreCase: TCheckBox;
    chkMaintainCase: TCheckBox;
    chkBeepOnAutoCorrect: TCheckBox;
    lblLabel2: TLabel;
    lblLabel3: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    pnlSeparator: TPanel;
    pnlAbout: TPanel;
    lblLabel4: TLabel;
    lblWebsite: TLabel;
    lblEmail: TLabel;
    lblLabel5: TLabel;
    chkAutoCorrectOnMouseDown: TCheckBox;
    lbxItems: TListBox;
    btnAutoCorrectAll: TButton;
    sacAutoCorrect: TSynAutoCorrect;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure chkIgnoreCaseClick(Sender: TObject);
    procedure chkMaintainCaseClick(Sender: TObject);
    procedure chkBeepOnAutoCorrectClick(Sender: TObject);
    procedure lblWebsiteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure chkAutoCorrectOnMouseDownClick(Sender: TObject);
    procedure lbxItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbxItemsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAutoCorrectAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.DFM}

procedure TfrmDemo.FormCreate(Sender: TObject);
begin
  // load from registry
  sacAutoCorrect.LoadFromRegistry(HKEY_CURRENT_USER, 'Software\Aerodynamica\Components\AsSynAutoCorrect\Demo');
  lbxItems.Items.Assign(sacAutoCorrect.ReplaceItems);
end;

procedure TfrmDemo.btnAddClick(Sender: TObject);
var
  sReplaceFrom, sReplaceTo: String;

begin
  if InputQuery('Add...', 'Replace:', sReplaceFrom) then
    InputQuery('Add...', 'With:', sReplaceTo)
  else
    Exit;

  with sacAutoCorrect do
  begin
    if (sReplaceFrom <> '') and (sReplaceTo <> '') then
    begin
      Add(sReplaceFrom, sReplaceTo);
      lbxItems.Items.Assign(sacAutoCorrect.ReplaceItems);
    end;
  end;

  // update buttons
  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmDemo.btnDeleteClick(Sender: TObject);
begin
  // stop if nothing is selected
  if lbxItems.ItemIndex < 0 then
  begin
    MessageBox(0, 'Please select an item before executing this command!', 'Error', MB_APPLMODAL or MB_ICONERROR);

    Exit;
  end;

  sacAutoCorrect.Delete(lbxItems.ItemIndex);
  lbxItems.Items.Assign(sacAutoCorrect.ReplaceItems);

  // update buttons
  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmDemo.btnEditClick(Sender: TObject);
var
  sReplaceFrom, sReplaceTo, CurrentText: String;

begin
  // stop if nothing is selected
  if lbxItems.ItemIndex < 0 then
  begin
    MessageBox(0, 'Please select an item before executing this command!', 'Error', MB_APPLMODAL or MB_ICONERROR);

    Exit;
  end;

  // get the current item
  CurrentText := sacAutoCorrect.ReplaceItems[lbxItems.ItemIndex];
  sReplaceFrom := HalfString(CurrentText, True);
  sReplaceTo := HalfString(CurrentText, False);

  if InputQuery('Edit...', 'Replace:', sReplaceFrom) then
    InputQuery('Edit...', 'With:', sReplaceTo)
  else
    Exit;

  with sacAutoCorrect do
  begin
    Edit(lbxItems.ItemIndex, sReplaceFrom, sReplaceTo);

    lbxItems.Items.Assign(sacAutoCorrect.ReplaceItems);
  end;

  // update buttons
  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmDemo.chkEnabledClick(Sender: TObject);
begin
  sacAutoCorrect.Enabled := chkEnabled.Checked;
  edtEditor.SetFocus;
end;

procedure TfrmDemo.chkIgnoreCaseClick(Sender: TObject);
begin
  sacAutoCorrect.IgnoreCase := chkIgnoreCase.Checked;
  edtEditor.SetFocus;
end;

procedure TfrmDemo.chkMaintainCaseClick(Sender: TObject);
begin
  sacAutoCorrect.MaintainCase := chkMaintainCase.Checked;
  edtEditor.SetFocus;
end;

procedure TfrmDemo.chkBeepOnAutoCorrectClick(Sender: TObject);
begin
  sacAutoCorrect.BeepOnAutoCorrect := chkBeepOnAutoCorrect.Checked;
  edtEditor.SetFocus;
end;

procedure TfrmDemo.lblWebsiteClick(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'Open', PChar(Trim(TLabel(Sender).Caption)), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmDemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //MessageBox(0, 'Please visit our website at http://aerodynamica2.port5.com for more free Delphi components and quality freeware applications!', 'Note', MB_APPLMODAL or MB_ICONINFORMATION);
end;

procedure TfrmDemo.FormResize(Sender: TObject);
begin
  if Height < 435 then Height := 435;
  if Width < 568 then Width := 568;
end;

procedure TfrmDemo.chkAutoCorrectOnMouseDownClick(Sender: TObject);
begin
  sacAutoCorrect.AutoCorrectOnMouseDown := chkAutoCorrectOnMouseDown.Checked;
  edtEditor.SetFocus;
end;

procedure TfrmDemo.lbxItemsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  CurrentText: String;

begin
  // owner-drawn stuff
  CurrentText := lbxItems.Items[Index];

  with lbxItems do
  begin
    Canvas.FillRect(Rect);

    Canvas.TextOut(Rect.Left + 2, Rect.Top, HalfString(CurrentText, True));
    Canvas.TextOut(Rect.Left + (lbxItems.ClientWidth div 2) + 2, Rect.Top, HalfString(CurrentText, False));
  end;
end;

procedure TfrmDemo.lbxItemsClick(Sender: TObject);
begin
  // disable buttons
  btnDelete.Enabled := not lbxItems.ItemIndex < 0;
  btnEdit.Enabled := not lbxItems.ItemIndex < 0;
end;

procedure TfrmDemo.FormDestroy(Sender: TObject);
begin
  // save auto-corrections to registry
  sacAutoCorrect.SaveToRegistry(HKEY_CURRENT_USER, 'Software\Aerodynamica\Components\AsSynAutoCorrect\Demo');
end;

procedure TfrmDemo.btnAutoCorrectAllClick(Sender: TObject);
begin
  sacAutoCorrect.AutoCorrectAll;
  edtEditor.SetFocus;
end;

end.
