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
    lblCredits: TLabel;
    procedure OpenURL(Sender: TObject);
    procedure MouseOver(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure editDonatedEnter(Sender: TObject);
    procedure editDonatedExit(Sender: TObject);
    procedure btnDonatedOKClick(Sender: TObject);
    procedure lblCreditsClick(Sender: TObject);
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
var
  i: Integer;
  lbl: TLabel;
begin
  for i:=0 to ComponentCount-1 do begin
    if Components[i] is TLabel then begin
      lbl := TLabel(Components[i]);
      if lbl.Font.Color = clRed then
        lbl.Font.Color := clBlue;
    end;
  end;
  if (Sender is TLabel) then begin
    lbl := Sender as TLabel;
    lbl.Font.Color := clRed;
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
      ErrorDialog(_('Not a valid donor email address'));
    nbTrue:
      MessageDialog(_('Thanks for donating!'), mtInformation, [mbOK]);
  end;
  imgDonate.Visible := Check <> nbTrue;
  MainForm.btnDonate.Visible := imgDonate.Visible;
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
  imgDonate.Visible := MainForm.HasDonated(False) <> nbTrue;
  imgDonate.OnClick := MainForm.btnDonate.OnClick;
  editDonated.Text := AppSettings.ReadString(asDonatedEmail);

  // Assign text
  Caption := f_('About %s', [APPNAME]);
  lblAppName.Caption := APPNAME;
  lblAppVersion.Caption := _('Version') + ' ' + Mainform.AppVersion + ' (' + IntToStr(GetExecutableBits) + ' Bit)';
  lblAppCompiled.Caption := _('Compiled on:') + ' ' + DateTimeToStr(GetImageLinkTimeStamp(Application.ExeName));
  lblAppWebpage.Caption := APPDOMAIN;
  lblAppWebpage.Hint := APPDOMAIN+'?place='+EncodeURLParam(lblAppWebpage.Name);
  ImageHeidisql.Hint := APPDOMAIN+'?place='+EncodeURLParam(ImageHeidisql.Name);

  Screen.Cursor := crDefault;
end;


procedure TAboutBox.lblCreditsClick(Sender: TObject);
begin
  Help(Sender, 'credits');
end;

end.

