unit About;

// -------------------------------------
// About-box
// -------------------------------------

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, ExtCtrls, SysUtils, ComCtrls, pngimage;

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
    btnDonate: TButton;
    lblAppDescription: TLabel;
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
var
  Compiled       : TDateTime;
begin
  Screen.Cursor := crHourGlass;

  InheritFont(Font);
  InheritFont(lblAppName.Font);
  lblAppName.Font.Size := 14;
  InheritFont(lblAppWebpage.Font);

  // Assign text
  Caption := 'About '+AppName;
  lblAppName.Caption := AppName;
  lblAppDescription.Caption := Mainform.AppDescription;
  lblAppVersion.Caption := 'Version '+Mainform.AppVersion;
  FileAge(ParamStr(0), Compiled);
  lblAppCompiled.Caption := 'Compiled on: ' + DateTimeToStr(Compiled);
  lblAppWebpage.Caption := AppDomain;
  lblAppWebpage.Hint := AppDomain;
  // Avoid scroll by removing blank line outside visible area in Authors text box
  MemoAuthors.Text := TrimRight(MemoAuthors.Text);
  MemoAuthors.WordWrap := True;

  Screen.Cursor := crDefault;
end;


end.

