unit About;

// -------------------------------------
// About-box
// -------------------------------------

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, ExtCtrls, SysUtils, ComCtrls, pngimage, gnugettext,
  Dialogs, SynRegExpr;

type
  TAboutBox = class(TForm)
    btnClose: TButton;
    btnForum: TButton;
    gboxCredits: TGroupBox;
    memoCredits: TMemo;
    lblAppName: TLabel;
    lblAppVersion: TLabel;
    lblAppCompiled: TLabel;
    lblAppWebpage: TLabel;
    btnUpdateCheck: TButton;
    ImageHeidisql: TImage;
    imgDonate: TImage;
    lblDonated: TLabel;
    editDonated: TEdit;
    btnDonatedOK: TButton;
    procedure OpenURL(Sender: TObject);
    procedure MouseOver(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure editDonatedEnter(Sender: TObject);
    procedure editDonatedExit(Sender: TObject);
    procedure btnDonatedOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  main, helpers;

{$R *.DFM}


procedure TAboutBox.OpenURL(Sender: TObject);
begin
  ShellExec( TControl(Sender).Hint );
end;


procedure TAboutBox.MouseOver(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  lblAppWebpage.Font.Color := clBlue;
  if (Sender is TLabel) then
  begin
    TLabel(Sender).Font.Color := clRed;
  end;
end;


procedure TAboutBox.btnDonatedOKClick(Sender: TObject);
var
  Check: TThreeStateBoolean;
begin
  AppSettings.WriteString(asDonatedEmail, editDonated.Text);
  Check := MainForm.HasDonated(True);
  case Check of
    nbUnset:
      MessageDialog(_('Could not check donation state.'), mtWarning, [mbOK]);
    nbFalse:
      ErrorDialog(_('Not a valid donators email address'));
    nbTrue:
      MessageDialog(_('Thanks for donating!'), mtInformation, [mbOK]);
  end;
  imgDonate.Visible := Check <> nbTrue;
  MainForm.imgDonate.Width := 122;
  MainForm.imgDonate.Visible := imgDonate.Visible;
end;


procedure TAboutBox.editDonatedEnter(Sender: TObject);
begin
  btnDonatedOK.Default := True;
  btnClose.Default := False;
end;


procedure TAboutBox.editDonatedExit(Sender: TObject);
begin
  btnDonatedOK.Default := False;
  btnClose.Default := True;
end;

procedure TAboutBox.FormShow(Sender: TObject);
var
  ReadMe, Credits: String;
  rx: TRegExpr;
begin
  Screen.Cursor := crHourGlass;

  TranslateComponent(Self);

  InheritFont(Font);
  InheritFont(lblAppName.Font);
  lblAppName.Font.Size := 14;
  InheritFont(lblAppWebpage.Font);
  imgDonate.Visible := MainForm.HasDonated(False) <> nbTrue;
  imgDonate.OnClick := MainForm.imgDonate.OnClick;
  editDonated.Text := AppSettings.ReadString(asDonatedEmail);

  // Assign text
  Caption := f_('About %s', [APPNAME]);
  lblAppName.Caption := APPNAME;
  lblAppVersion.Caption := _('Version') + ' ' + Mainform.AppVersion;
  lblAppCompiled.Caption := _('Compiled on:') + ' ' + DateTimeToStr(GetImageLinkTimeStamp(Application.ExeName));
  lblAppWebpage.Caption := AppDomain;
  lblAppWebpage.Hint := AppDomain;
  // Read credits from readme.txt
  rx := TRegExpr.Create;
  try
    ReadMe := ReadTextFile(ExtractFilePath(ParamStr(0)) + 'readme.txt', TEncoding.UTF8);
    rx.Expression := '\*\*\* Credits\:\s+(\S.+)$';
    if rx.Exec(ReadMe) then begin
      Credits := rx.Match[1];
      // Turn linebreaks into single spaces, and let TMemo.WordWrap insert fitting linebreaks
      rx.Expression := '(\S)'+CRLF+'(\S)';
      Credits := rx.Replace(Credits, '${1} ${2}', True);
      Credits := Trim(Credits);
      memoCredits.Text := Credits;
    end;
  except
    on E:Exception do
      memoCredits.Text := E.Message;
  end;
  rx.Free;

  Screen.Cursor := crDefault;
end;


end.

