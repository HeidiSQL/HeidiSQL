unit uDemo;

interface

uses
  MMSystem,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SynAutoCorrect, ExtCtrls, SynEdit, ShellApi, ComCtrls, ImgList;

type
  TAsSoundOption = (assoAsync, assoNoDefault, assoNoWait);
  TAsSoundOptions = set of TAsSoundOption;
  TAsSoundType = (asstAlias, asstFileName, asstBeep);

  TAsSoundInfo = class(TPersistent)
  private
    FFileName: TFileName;
    FOptions: TAsSoundOptions;
    FSoundType: TAsSoundType;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    function Play: Boolean;
  published
    property FileName: TFileName read FFileName write FFileName;
    property Options: TAsSoundOptions read FOptions write FOptions;
    property SoundType: TAsSoundType read FSoundType write FSoundType
      default asstFileName;
  end;

  TfrmDemo = class(TForm)
    acrAutoCorrect: TSynAutoCorrect;
    diaOpen: TOpenDialog;
    pclDemo: TPageControl;
    tshOptions: TTabSheet;
    tshEditor: TTabSheet;
    edtEditor: TSynEdit;
    tshAbout: TTabSheet;
    pnlAboutContainer: TPanel;
    imgIcon: TImage;
    lblDescription: TLabel;
    lblLabel4: TLabel;
    lblLabel5: TLabel;
    lblEmail: TLabel;
    lblWebsite: TLabel;
    pnlMessage: TPanel;
    ilsPages: TImageList;
    btnDone: TButton;
    pnlOptionsContainer: TPanel;
    lblLabel2: TLabel;
    lblLabel3: TLabel;
    lblNote: TLabel;
    chkEnabled: TCheckBox;
    chkIgnoreCase: TCheckBox;
    chkMaintainCase: TCheckBox;
    chkCorrectOnMouseDown: TCheckBox;
    lbxItems: TListBox;
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    gbxSound: TGroupBox;
    lblType: TLabel;
    lblFile: TLabel;
    rboFile: TRadioButton;
    rboBeep: TRadioButton;
    edtFile: TEdit;
    btnBrowse: TButton;
    chkPlaySound: TCheckBox;
    shpBorder: TShape;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure chkIgnoreCaseClick(Sender: TObject);
    procedure chkMaintainCaseClick(Sender: TObject);
    procedure lblWebsiteClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lbxItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbxItemsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkCorrectOnMouseDownClick(Sender: TObject);
    procedure chkPlaySoundClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure rboFileClick(Sender: TObject);
    procedure rboBeepClick(Sender: TObject);
    procedure edtEditorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDoneClick(Sender: TObject);
    procedure tshOptionsResize(Sender: TObject);
    procedure tshAboutResize(Sender: TObject);
    procedure acrAutoCorrectCorrected(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.DFM}

{ TAsSoundInfo }

procedure TAsSoundInfo.Assign(Source: TPersistent);
begin
  if Source is TAsSoundInfo then
    with TAsSoundInfo(Source) do
    begin
      Self.FileName := FileName;
      Self.Options := Options;
      Self.SoundType := SoundType;
    end
    else
      inherited;
end;

constructor TAsSoundInfo.Create;
begin
  inherited Create;
  FFileName := '';
  FOptions := [assoAsync];
  FSoundType := asstBeep;
end;

destructor TAsSoundInfo.Destroy;
begin
  inherited;
end;

function TAsSoundInfo.Play: Boolean;
var
  o: Cardinal;

begin
  if FSoundType = asstBeep then
    Result := MessageBeep(0)
  else
  begin
    o := SND_SYNC;
    case FSoundType of
      asstAlias: o := o or SND_ALIAS;
      asstFileName: o := o or SND_FILENAME;
    end;
    if assoAsync in FOptions then o := o or SND_ASYNC;
    if assoNoDefault in FOptions then o := o or SND_NODEFAULT;
    if assoNoWait in FOptions then o := o or SND_NOWAIT;
    Result := PlaySound(PChar(FFileName), 0, o);
  end;
end;

{ TfrmDemo }
procedure TfrmDemo.FormCreate(Sender: TObject);
begin
  { Load from registry. }
  acrAutoCorrect.LoadFromRegistry(HKEY_CURRENT_USER,
    'Software\Aerodynamica\Components\SynAutoCorrect\Demo');
  lbxItems.Items.Assign(acrAutoCorrect.Items);
end;

procedure TfrmDemo.btnAddClick(Sender: TObject);
var
  Original, Correction: String;

begin
  if InputQuery('Add...', 'Original:', Original) then
    InputQuery('Add...', 'Correction:', Correction)
  else
    Exit;

  with acrAutoCorrect do
  begin
    if (Original <> '') and (Correction <> '') then
    begin
      Add(Original, Correction);
      lbxItems.Items.Assign(acrAutoCorrect.Items);
    end;
  end;

  { Update buttons. }
  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmDemo.btnDeleteClick(Sender: TObject);
begin
  { Error if nothing is selected. }
  if lbxItems.ItemIndex < 0 then
  begin
    MessageBox(0, 'Please select an item before executing this command!',
      'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  acrAutoCorrect.Delete(lbxItems.ItemIndex);
  lbxItems.Items.Assign(acrAutoCorrect.Items);

  { Update buttons. }
  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmDemo.btnEditClick(Sender: TObject);
var
  Original, Correction, CurrText: string;

begin
  { Error if nothing is selected. }
  if lbxItems.ItemIndex < 0 then
  begin
    MessageBox(0, 'Please select an item before executing this command!',
      'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  { Get current item. }
  CurrText := acrAutoCorrect.Items[lbxItems.ItemIndex];
  Original := acrAutoCorrect.HalfString(CurrText, True);
  Correction := acrAutoCorrect.HalfString(CurrText, False);

  if InputQuery('Edit...', 'Original:', Original) then
    InputQuery('Edit...', 'Correction:', Correction)
  else
    Exit;

  with acrAutoCorrect do
  begin
    Edit(lbxItems.ItemIndex, Original, Correction);
    lbxItems.Items.Assign(acrAutoCorrect.Items);
  end;

  { Update buttons. }
  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmDemo.chkEnabledClick(Sender: TObject);
begin
  acrAutoCorrect.Enabled := chkEnabled.Checked;
end;

procedure TfrmDemo.chkIgnoreCaseClick(Sender: TObject);
begin
  if chkIgnoreCase.Checked then
    acrAutoCorrect.Options := acrAutoCorrect.Options + [ascoIgnoreCase]
  else
    acrAutoCorrect.Options := acrAutoCorrect.Options - [ascoIgnoreCase];
end;

procedure TfrmDemo.chkMaintainCaseClick(Sender: TObject);
begin
  if chkMaintainCase.Checked then
    acrAutoCorrect.Options := acrAutoCorrect.Options + [ascoMaintainCase]
  else
    acrAutoCorrect.Options := acrAutoCorrect.Options - [ascoMaintainCase];
end;

procedure TfrmDemo.lblWebsiteClick(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'Open', PChar(Trim(TLabel(Sender).Caption)),
    nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmDemo.FormResize(Sender: TObject);
begin
  if Height < 435 then Height := 435;
  if Width < 568 then Width := 568;
end;

procedure TfrmDemo.lbxItemsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  s: string;

begin
  { Owner-drawn stuff. }
  s := lbxItems.Items[Index];
  with lbxItems do
  begin
    Canvas.FillRect(Rect);
    Canvas.TextOut(Rect.Left + 2, Rect.Top, acrAutoCorrect.HalfString(s, True));
    Canvas.TextOut(Rect.Left + (lbxItems.ClientWidth div 2) + 2, Rect.Top,
      acrAutoCorrect.HalfString(s, False));

    { Repaint separator. }
    FormPaint(nil);
  end;
end;

procedure TfrmDemo.lbxItemsClick(Sender: TObject);
begin
  { Disable buttons. }
  btnDelete.Enabled := lbxItems.ItemIndex > -1;
  btnEdit.Enabled := lbxItems.ItemIndex > -1;
end;

procedure TfrmDemo.FormDestroy(Sender: TObject);
begin
  { Save to registry. }
  acrAutoCorrect.SaveToRegistry(HKEY_CURRENT_USER,
    'Software\Aerodynamica\Components\SynAutoCorrect\Demo');
end;

procedure TfrmDemo.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { Capture shortcut for AutoCorrectAll. }
  if (ssCtrl in Shift) and (Key = Word('Q')) then acrAutoCorrect.AutoCorrectAll;
end;

procedure TfrmDemo.chkCorrectOnMouseDownClick(Sender: TObject);
begin
  if chkCorrectOnMouseDown.Checked then
    acrAutoCorrect.Options := acrAutoCorrect.Options + [ascoCorrectOnMouseDown]
  else
    acrAutoCorrect.Options := acrAutoCorrect.Options - [ascoCorrectOnMouseDown];
end;

procedure TfrmDemo.chkPlaySoundClick(Sender: TObject);
var
  i: Integer;

begin
  gbxSound.Enabled := chkPlaySound.Checked;
  for i := 0 to Pred(gbxSound.ControlCount) do
    TWinControl(gbxSound.Controls[i]).Enabled := chkPlaySound.Checked;
  rboBeepClick(nil);
end;

procedure TfrmDemo.btnBrowseClick(Sender: TObject);
begin
  if diaOpen.Execute then edtFile.Text := diaOpen.FileName;
end;

procedure TfrmDemo.rboFileClick(Sender: TObject);
begin
  lblFile.Enabled := rboFile.Checked;
  edtFile.Enabled := rboFile.Checked;
  btnBrowse.Enabled := rboFile.Checked;
end;

procedure TfrmDemo.rboBeepClick(Sender: TObject);
begin
  lblFile.Enabled := rboFile.Checked;
  edtFile.Enabled := rboFile.Checked;
  btnBrowse.Enabled := rboFile.Checked;
end;

{ Demonstration of user events (it was broken in previous releases). }
procedure TfrmDemo.edtEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  pnlMessage.Caption := 'User event handler: MouseDown';
end;

procedure TfrmDemo.edtEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  pnlMessage.Caption := 'User event handler: KeyDown';
end;

procedure TfrmDemo.FormPaint(Sender: TObject);
begin
  { Paints the line in the middle of the listbox. }
  with lbxItems.Canvas do
  begin
    Pen.Color := clBlack;
    PenPos := Point(lbxItems.Width div 2 - 8, 0);
    LineTo(lbxItems.Width div 2 - 8, lbxItems.Height);
  end;
end;

procedure TfrmDemo.FormShow(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrmDemo.btnDoneClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmDemo.tshOptionsResize(Sender: TObject);
begin
  { Center options panel. }
  pnlOptionsContainer.Left := (tshOptions.ClientWidth div 2) -
    (pnlOptionsContainer.Width div 2);
  pnlOptionsContainer.Top := (tshOptions.ClientHeight div 2) -
    (pnlOptionsContainer.Height div 2);
end;

procedure TfrmDemo.tshAboutResize(Sender: TObject);
begin
  { Center about panel. }
  pnlAboutContainer.Left := (tshAbout.ClientWidth div 2) -
    (pnlAboutContainer.Width div 2);
  pnlAboutContainer.Top := (tshAbout.ClientHeight div 2) -
    (pnlAboutContainer.Height div 2);
end;

procedure TfrmDemo.acrAutoCorrectCorrected(Sender: TObject);
var
  Player: TAsSoundInfo;
begin
  if not chkPlaySound.Checked then
    Exit;
  Player := TAsSoundInfo.Create;
  try
    if rboBeep.Checked then
      Player.SoundType := asstBeep
    else begin
      Player.SoundType := asstFileName;
      Player.FileName := edtFile.Text;
    end;
    Player.Play;
  finally
    Player.Free;
  end;
end;

end.
