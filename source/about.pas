unit About;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ComCtrls, ExtCtrls, SynEdit, SynHighlighterSQL, laz.VirtualTrees,
  RegExpr, Buttons, StdCtrls, Clipbrd, LCLIntf, StrUtils, LazVersion;

type
  TAboutBox = class(TForm)
    btnClose: TButton;
    lblAppName: TLabel;
    lblAppVersion: TLabel;
    lblAppCompiled: TLabel;
    lnklblWebpage: TLabel;
    btnUpdateCheck: TButton;
    ImageHeidisql: TImage;
    lblDonated: TLabel;
    editDonated: TEdit;
    btnDonatedOK: TButton;
    lnklblCredits: TLabel;
    popupLabels: TPopupMenu;
    menuCopyLabel: TMenuItem;
    lblEnvironment: TLabel;
    btnDonate: TButton;
    lnklblCompiler: TLabel;
    procedure OpenURL(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure editDonatedEnter(Sender: TObject);
    procedure editDonatedExit(Sender: TObject);
    procedure btnDonatedOKClick(Sender: TObject);
    procedure lnklblWebpageClick(Sender: TObject);
    procedure lnklblCreditsClick(Sender: TObject);
    procedure menuCopyLabelClick(Sender: TObject);
  private
    { Private declarations }
    function GetCompilerVersion: String;
  public
    { Public declarations }
  end;

implementation

uses
  main, apphelpers, generic_types;

{$R *.lfm}


procedure TAboutBox.OpenURL(Sender: TObject);
begin
  LCLIntf.OpenURL(TControl(Sender).Hint);
end;


procedure TAboutBox.btnDonatedOKClick(Sender: TObject);
var
  Check: TThreeStateBoolean;
begin
  //AppSettings.WriteString(asDonatedEmail, editDonated.Text);
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
  MainForm.ToolButtonDonate.Visible := btnDonate.Visible;
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
  editDonated.Text := ''; //AppSettings.ReadString(asDonatedEmail);

  // Assign text
  Caption := f_('About %s', [APPNAME]);
  lblAppName.Caption := APPNAME;
  lblAppVersion.Caption := _('Version') + ' ' + Mainform.AppVersion;
  lblAppCompiled.Caption := _('Compiled on:') + ' ' + {DateTimeToStr(GetImageLinkTimeStamp(Application.ExeName)) +} ' with';
  lnklblCompiler.Top := lblAppCompiled.Top;
  lnklblCompiler.Left := lblAppCompiled.Left + lblAppCompiled.Width + Canvas.TextWidth(' ');
  lnklblCompiler.Caption := GetCompilerVersion;
  lnklblCompiler.Hint := 'https://www.lazarus-ide.org/?utm_source='+APPNAME;
  lnklblWebpage.Caption := APPDOMAIN;
  lnklblWebpage.Hint := APPDOMAIN+'?place='+EncodeURLParam(lnklblWebpage.Name);

  lnklblCompiler.Font.Style := lnklblCompiler.Font.Style + [fsUnderline];
  lnklblWebpage.Font.Style := lnklblWebpage.Font.Style + [fsUnderline];
  lnklblCredits.Font.Style := lnklblCredits.Font.Style + [fsUnderline];

  ImageHeidisql.Hint := APPDOMAIN+'?place='+EncodeURLParam(ImageHeidisql.Name);
  lblEnvironment.Caption := _('Environment:') +
    {$IFDEF WINDOWS}'Windows'{$EndIf}
    {$IFDEF LINUX}'Linux'{$EndIf}
    {$IFDEF MACOS}'MacOS'{$EndIf}
    ;

  Screen.Cursor := crDefault;
  btnClose.TrySetFocus;
end;


procedure TAboutBox.lnklblCreditsClick(Sender: TObject);
begin
  Help(Sender, 'credits');
end;

procedure TAboutBox.lnklblWebpageClick(Sender: TObject);
begin
  LCLIntf.OpenURL((Sender as TLabel).Hint);
end;

function TAboutBox.GetCompilerVersion: string;
begin
  Result := 'Lazarus IDE v' + LazVersion.laz_version + ' & FreePascal v' + {$I %FPCVERSION%};
end;

end.

