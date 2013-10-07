/// SynFile Edit window
unit FileEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs,
{$ifdef USETMSPACK}
  TaskDialog,
{$endif}
  SynCommons, SynCrypto, SynGdiPlus, SynTaskDialog,
  SQLite3Commons, SQLite3UILogin, SQLite3UI, SQLite3i18n,
  FileTables;

type
  /// SynFile Edit window
  // - we don't use the standard Window generation (from SQLite3UIEdit),
  // but a custom window, created as RAD
  TEditForm = class(TVistaForm)
    Name: TLabeledEdit;
    KeyWords: TLabeledEdit;
    Memo: TMemo;
    procedure FormShow(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnPictureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fRec: TSQLFile;
    fReadOnly: boolean;
    BtnOk: TSynButton;
    BtnCancel: TSynButton;
    BtnPicture: TSynButton;
  public
    /// set the associated record to be edited
    function SetRec(const Value: TSQLFile): boolean;
	/// used to load a picture file into a BLOB content 
	// after 80% JPEG compression
    function LoadPicture(const FileName: TFileName; var Picture: RawByteString): boolean;
	/// read-only access to the edited record
    property Rec: TSQLFile read fRec;
	/// should be set to TRUE to disable any content editing
    property ReadOnly: boolean read fReadOnly write fReadOnly;
  end;

var
  /// SynFile Edit window instance
  EditForm: TEditForm;

/// will display a modal form asking for a password, then encrypt
// or uncrypt some BLOB content
// - returns TRUE if the password was correct and the data processed
// - returns FALSE on error (canceled or wrong password)
function Cypher(const Title: string; var Content: TSQLRawBlob; Encrypt: boolean): boolean;


implementation

{$R *.dfm}

{ TEditForm }

function TEditForm.SetRec(const Value: TSQLFile): boolean;
begin
  result := false;
  fRec := Value;
  Name.Text := U2S(Value.fName);
  KeyWords.Text := U2S(Value.fKeyWords);
  if Value.InheritsFrom(TSQLData) and not Value.InheritsFrom(TSQLSafeMemo) then begin
    Memo.Hide;
    ClientHeight := 165;
  end else begin
    Memo.Show;
    ClientHeight := 500;
    if Value.InheritsFrom(TSQLMemo) then
      Memo.Text := U2S(TSQLMemo(Value).Content) else
    if Value.InheritsFrom(TSQLSafeMemo) then
      with TSQLSafeMemo(Value) do
        if not Cypher(Rec.CaptionName,fData,false) then
          exit else
          Memo.Text := U2S(Data) else
      Memo.Hide;
  end;
  Name.ReadOnly := ReadOnly;
  KeyWords.ReadOnly := ReadOnly;
  if Memo.Visible then
    Memo.ReadOnly := ReadOnly;
  BtnCancel.Visible := not ReadOnly;
  result := true;
end;


procedure TEditForm.FormCreate(Sender: TObject);
resourcestring
  BtnPictureHint = 'Change the Picture associated with this record';
begin
  BtnOK := TSynButton.CreateKind(self,cbOK,337,437,100,41);
  BtnOK.ModalResult := mrNone;
  BtnOK.OnClick := BtnOKClick;
  BtnOK.Anchors := [akRight, akBottom];
  BtnCancel := TSynButton.CreateKind(self,cbCancel,457,437,100,41);
  BtnCancel.Anchors := [akRight, akBottom];
  BtnPicture := TSynButton.Create(self);
  BtnPicture.Parent := self;
  BtnPicture.SetBounds(392,21,105,25);
  BtnPicture.Hint := BtnPictureHint;
  BtnPicture.OnClick := BtnPictureClick;
end;

procedure TEditForm.FormShow(Sender: TObject);
begin
  Name.EditLabel.Caption := _('Name');
  KeyWords.EditLabel.Caption := _('KeyWords');
  BtnPicture.Caption := _('Picture');
  SetStyle(self);
end;

procedure TEditForm.BtnOkClick(Sender: TObject);
begin
  if ReadOnly then
    ModalResult := mrCancel else begin
    Rec.fModified := Iso8601Now;
    Rec.fName := trim(S2U(Name.Text));
    if Rec.fName='' then begin
      Name.SetFocus;
      ShowMessage(_('Name'),true);
      exit;
    end;
    Rec.fKeyWords := trim(S2U(KeyWords.Text));
    if Rec.InheritsFrom(TSQLMemo) then
      TSQLMemo(Rec).Content := S2U(Memo.Text) else
    if Rec.ClassType=TSQLSafeMemo then
    with TSQLSafeMemo(Rec) do begin
      Data := S2U(Memo.Text);
      if not Cypher(Rec.CaptionName,fData,true) then
        exit;
    end;
    ModalResult := mrOk;
  end;
end;

function Cypher(const Title: string; var Content: TSQLRawBlob; Encrypt: boolean): boolean;
resourcestring
  sEnterPassword = 'Enter password for this record:';
var AES: TAESFull;
    SHA: TSHA256Digest;
    PassWord: string;
    Len: integer;
begin
  result := Content='';
  if result then
    exit;
  if not TLoginForm.PassWord(Title,sEnterPassword,PassWord) then
    exit;
  SHA256Weak(S2U(PassWord), SHA);
  try
    Len := AES.EncodeDecode(SHA,256,length(Content),Encrypt,nil,nil,Pointer(Content),nil);
    if Len<0 then
      exit;
    SetString(Content,PAnsiChar(AES.outStreamCreated.Memory),Len);
    result := true;
  finally
    AES.OutStreamCreated.Free;
  end;
end;

procedure TEditForm.BtnPictureClick(Sender: TObject);
begin
  Rec.fPicture := '';
  with TOpenPictureDialog.Create(self) do
  try
    Title := BtnPicture.Hint;
    Filter := GraphicFilter(TGraphic);
    Options := [ofHideReadOnly,ofPathMustExist,ofFileMustExist,ofEnableSizing];
    if Execute then
      LoadPicture(FileName,RawByteString(Rec.fPicture));
  finally
    Free;
  end;
end;

function TEditForm.LoadPicture(const FileName: TFileName; var Picture: RawByteString): boolean;
var Pic: TSynPicture;
begin
  result := false;
  if not FileExists(FileName) then
    exit;
  Pic := TSynPicture.Create;
  try
    Pic.LoadFromFile(FileName);
    if Pic.Empty then
      exit;
    SaveAsRawByteString(Pic,Picture,gptJPG,80,300);
    result := true;
  finally
    Pic.Free;
  end;
end;


initialization
  Gdip.RegisterPictures;
end.
