unit About;

// -------------------------------------
// About-box
// -------------------------------------

interface

uses
  Winapi.Windows, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.SysUtils, Vcl.ComCtrls, Vcl.Imaging.pngimage, gnugettext,
  Vcl.Dialogs, SynRegExpr, Vcl.Menus, Vcl.ClipBrd, extra_controls, generic_types, System.StrUtils;

type
  TAboutBox = class(TExtForm)
    btnClose: TButton;
    lblAppName: TLabel;
    lblAppVersion: TLabel;
    lblAppCompiled: TLabel;
    lnklblWebpage: TLinkLabel;
    btnUpdateCheck: TButton;
    ImageHeidisql: TImage;
    lblDonated: TLabel;
    editDonated: TEdit;
    btnDonatedOK: TButton;
    lnklblCredits: TLinkLabel;
    popupLabels: TPopupMenu;
    menuCopyLabel: TMenuItem;
    lblEnvironment: TLabel;
    btnDonate: TButton;
    lnklblCompiler: TLinkLabel;
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
    function GetDelphiVersion: String;
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
  btnDonate.Visible := Check <> nbTrue;
  MainForm.ToolBarDonate.Visible := btnDonate.Visible;
  MainForm.FormResize(Self);
end;


procedure TAboutBox.menuCopyLabelClick(Sender: TObject);
var
  LabelComp: TComponent;
begin
  // Copy label caption
  LabelComp := PopupComponent(Sender);
  if LabelComp is TLabel then begin
    Clipboard.TryAsText := TLabel(LabelComp).Caption;
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
var
  OsMajor, OsMinor, OsBuild: Integer;
begin
  Screen.Cursor := crHourGlass;

  // Apply special font properties after form creation, as that disables ParentFont, which prevents InheritFont() to apply
  lblAppName.Font.Size := Round(lblAppName.Font.Size * 1.5);
  lblAppName.Font.Style := [fsBold];

  btnDonate.Caption := f_('Donate to the %s project', [APPNAME]);
  btnDonate.Visible := MainForm.HasDonated(False) <> nbTrue;
  btnDonate.OnClick := MainForm.DonateClick;
  editDonated.Text := AppSettings.ReadString(asDonatedEmail);

  // Assign text
  Caption := f_('About %s', [APPNAME]);
  lblAppName.Caption := APPNAME;
  lblAppVersion.Caption := _('Version') + ' ' + Mainform.AppVersion + ' (' + IntToStr(GetExecutableBits) + ' Bit)';
  lblAppCompiled.Caption := _('Compiled on:') + ' ' + DateTimeToStr(GetImageLinkTimeStamp(Application.ExeName)) + ' with';
  lnklblCompiler.Top := lblAppCompiled.Top;
  lnklblCompiler.Left := lblAppCompiled.Left + lblAppCompiled.Width + Canvas.TextWidth(' ');
  lnklblCompiler.Caption := '<a href="https://www.embarcadero.com/products/delphi?utm_source='+APPNAME+'">'+GetDelphiVersion+'</a>';
  lnklblWebpage.Caption := '<a href="'+APPDOMAIN+'?place='+EncodeURLParam(lnklblWebpage.Name)+'">'+APPDOMAIN+'</a>';
  lnklblCredits.Caption := '<a href="">'+lnklblCredits.Caption+'</a>';
  ImageHeidisql.Hint := APPDOMAIN+'?place='+EncodeURLParam(ImageHeidisql.Name);
  lblEnvironment.Caption := _('Environment:');
  if IsWine then begin
    lblEnvironment.Caption := lblEnvironment.Caption +
      ' Linux/Wine';
  end else begin
    OsMajor := Win32MajorVersion;
    OsMinor := Win32MinorVersion;
    OsBuild := Win32BuildNumber;
    if (OsMajor = 10) and (OsBuild >= 22000) then
      OsMajor := 11;
    lblEnvironment.Caption := lblEnvironment.Caption +
      ' Windows ' +
      IntToStr(OsMajor) +
      IfThen(OsMinor > 0, '.'+IntToStr(OsMinor), '') +
      ' Build '+IntToStr(OsBuild);
  end;

  Screen.Cursor := crDefault;
  btnClose.TrySetFocus;
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

function TAboutBox.GetDelphiVersion: string;
begin
  {$IF Defined(VER360)}
    // Oldest/first official version where this gets used
    Result := '12';
  {$ELSEIF Defined(VER350)}
    Result := '11';
  {$ELSEIF Defined(VER340)}
    Result := '10.4';
  {$ELSE}
    Result := '10.3 or older';
  {$ENDIF}

  Result := 'Delphi ' + Result;
end;

end.

