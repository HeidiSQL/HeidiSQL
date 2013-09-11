unit About;

// -------------------------------------
// About-box
// -------------------------------------

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, ExtCtrls, SysUtils, ComCtrls, pngimage, gnugettext;

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
    procedure OpenURL(Sender: TObject);
    procedure MouseOver(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
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


procedure TAboutBox.FormShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;

  TranslateComponent(Self);

  InheritFont(Font);
  InheritFont(lblAppName.Font);
  lblAppName.Font.Size := 14;
  InheritFont(lblAppWebpage.Font);
  imgDonate.Hint := APPDOMAIN + imgDonate.Hint;

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

