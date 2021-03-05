unit About;

// -------------------------------------
// About-box
// -------------------------------------

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, ExtCtrls, SysUtils, ComCtrls, pngimage, gnugettext,
  Dialogs, SynRegExpr, Vcl.Menus, ClipBrd, extra_controls;

type
  TAboutBox = class(TExtForm)
    btnClose: TButton;
    lblAppName: TLabel;
    lblAppVersion: TLabel;
    lblAppCompiled: TLabel;
    lnklblWebpage: TLinkLabel;
    btnUpdateCheck: TButton;
    ImageHeidisql: TImage;
    imgDonate: TImage;
    lblDonated: TLabel;
    editDonated: TEdit;
    btnDonatedOK: TButton;
    lnklblCredits: TLinkLabel;
    popupLabels: TPopupMenu;
    menuCopyLabel: TMenuItem;
    lblEnvironment: TLabel;
    procedure OpenURL(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure editDonatedEnter(Sender: TObject);
    procedure editDonatedExit(Sender: TObject);
    procedure btnDonatedOKClick(Sender: TObject);
    procedure lnklblWebpageLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure lnklblCreditsLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure menuCopyLabelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  main, apphelpers;

{$R *.DFM}


procedure TAboutBox.OpenURL(Sender: TObject);
begin
  ShellExec( TControl(Sender).Hint );
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
      ErrorDialog(_('Not a valid donor email address'));
    nbTrue:
      MessageDialog(_('Thanks for donating!'), mtInformation, [mbOK]);
  end;
  imgDonate.Visible := Check <> nbTrue;
  MainForm.imgDonate.Visible := imgDonate.Visible;
  MainForm.FormResize(Self);
end;


procedure TAboutBox.menuCopyLabelClick(Sender: TObject);
var
  LabelComp: TComponent;
begin
  // Copy label caption
  LabelComp := PopupComponent(Sender);
  if LabelComp is TLabel then begin
    Clipboard.AsText := TLabel(LabelComp).Caption;
  end;
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

  // Apply special font properties after form creation, as that disables ParentFont, which prevents InheritFont() to apply
  lblAppName.Font.Size := Round(lblAppName.Font.Size * 1.5);
  lblAppName.Font.Style := [fsBold];

  imgDonate.Visible := MainForm.HasDonated(False) <> nbTrue;
  imgDonate.OnClick := MainForm.DonateClick;
  editDonated.Text := AppSettings.ReadString(asDonatedEmail);

  // Assign text
  Caption := f_('About %s', [APPNAME]);
  lblAppName.Caption := APPNAME;
  lblAppVersion.Caption := _('Version') + ' ' + Mainform.AppVersion + ' (' + IntToStr(GetExecutableBits) + ' Bit)';
  lblAppCompiled.Caption := _('Compiled on:') + ' ' + DateTimeToStr(GetImageLinkTimeStamp(Application.ExeName));
  lnklblWebpage.Caption := '<a href="'+APPDOMAIN+'?place='+EncodeURLParam(lnklblWebpage.Name)+'">'+APPDOMAIN+'</a>';
  lnklblCredits.Caption := '<a href="">'+lnklblCredits.Caption+'</a>';
  ImageHeidisql.Hint := APPDOMAIN+'?place='+EncodeURLParam(ImageHeidisql.Name);
  lblEnvironment.Caption := _('Environment:');
  if RunningAsUwp then begin
    lblEnvironment.Caption := lblEnvironment.Caption +
      ' Windows v'+IntToStr(Win32MajorVersion)+'.'+IntToStr(Win32MinorVersion) +
      ', Store Package ' + GetUwpFullName;
  end else if MainForm.IsWine then begin
    lblEnvironment.Caption := lblEnvironment.Caption +
      ' Linux/Wine';
  end else begin
    lblEnvironment.Caption := lblEnvironment.Caption +
      ' Windows v'+IntToStr(Win32MajorVersion)+'.'+IntToStr(Win32MinorVersion);
  end;

  Screen.Cursor := crDefault;
end;


procedure TAboutBox.lnklblCreditsLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  Help(Sender, 'credits');
end;

procedure TAboutBox.lnklblWebpageLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExec(Link);
end;

end.

