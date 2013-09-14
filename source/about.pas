unit About;

// -------------------------------------
// About-box
// -------------------------------------

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, ExtCtrls, SysUtils, ComCtrls, pngimage, gnugettext,
  Dialogs;

type
  TAboutBox = class(TForm)
    btnClose: TButton;
    btnForum: TButton;
    MemoAuthors: TMemo;
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
begin
  AppSettings.WriteString(asDonatedEmail, editDonated.Text);
  if MainForm.HasDonated(True) then
    MessageDialog(_('Thanks for donating!'), mtInformation, [mbOK])
  else
    ErrorDialog(_('Not a valid donators email address'));
  imgDonate.Visible := not MainForm.HasDonated(False);
  MainForm.imgDonate.Width := 122;
  MainForm.imgDonate.Visible := not MainForm.HasDonated(False);
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
begin
  Screen.Cursor := crHourGlass;

  TranslateComponent(Self);

  InheritFont(Font);
  InheritFont(lblAppName.Font);
  lblAppName.Font.Size := 14;
  InheritFont(lblAppWebpage.Font);
  imgDonate.Hint := APPDOMAIN + imgDonate.Hint;
  imgDonate.Visible := not MainForm.HasDonated(False);
  editDonated.Text := AppSettings.ReadString(asDonatedEmail);

  // Assign text
  Caption := f_('About %s', [APPNAME]);
  lblAppName.Caption := APPNAME;
  lblAppVersion.Caption := _('Version') + ' ' + Mainform.AppVersion;
  lblAppCompiled.Caption := _('Compiled on:') + ' ' + DateTimeToStr(GetImageLinkTimeStamp(Application.ExeName));
  lblAppWebpage.Caption := AppDomain;
  lblAppWebpage.Hint := AppDomain;
  // Avoid scroll by removing blank line outside visible area in Authors text box
  MemoAuthors.Text := TrimRight(MemoAuthors.Text);
  MemoAuthors.WordWrap := True;

  Screen.Cursor := crDefault;
end;


end.

