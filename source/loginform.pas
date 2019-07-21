unit loginform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, gnugettext, System.UITypes, extra_controls;

type
  TfrmLogin = class(TExtForm)
    btnOK: TButton;
    pnlBackground: TPanel;
    lblPrompt: TLabel;
    lblUsername: TLabel;
    lblPassword: TLabel;
    editPassword: TEdit;
    editUsername: TEdit;
    imgIcon: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses apphelpers, main;

{$R *.dfm}
{$I const.inc}



procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  Caption := APPNAME + ' - Login';
  MainForm.VirtualImageListMain.GetBitmap(144, imgIcon.Picture.Bitmap);
  lblPrompt.Font.Size := 10;
  lblPrompt.Font.Color := GetThemeColor(clHotlight);
  lblPrompt.Font.Style := lblPrompt.Font.Style + [fsBold];
  editUsername.Text := '';
  editPassword.Text := '';
end;

procedure TfrmLogin.FormShow(Sender: TObject);
begin
  if editPassword.CanFocus and (editUsername.GetTextLen > 0) and (editPassword.GetTextLen = 0) then
    editPassword.SetFocus
  else if editUsername.CanFocus then
    editUsername.SetFocus;
end;

end.
