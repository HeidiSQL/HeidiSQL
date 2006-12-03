unit About;


// -------------------------------------
// About-box
// -------------------------------------


interface

uses Windows, Menus, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellApi, SysUtils, dialogs, ComCtrls;

type
  TAboutBox = class(TForm)
    PanelLogos: TPanel;
    ImageMysql: TImage;
    ButtonClose: TButton;
    PanelMain: TPanel;
    ProductName: TLabel;
    LabelVersion: TLabel;
    LabelCompiled: TLabel;
    LabelWebpage: TLabel;
    LabelAuthors: TLabel;
    ButtonDonate: TButton;
    ButtonBoard: TButton;
    ImageHeidisql: TImage;
    MemoThanks: TMemo;
    LabelThanks: TLabel;
    MemoAuthors: TMemo;
    procedure OKButtonClick(Sender: TObject);
    procedure OpenURL(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure MouseOver(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure AboutWindow (AOwner : TComponent);


implementation

uses main, helpers;
{$R *.DFM}


procedure AboutWindow (AOwner : TComponent);
var
  f : TAboutBox;
begin
  f := TAboutBox.Create(AOwner);
  f.ShowModal; // don't care about result
  FreeAndNil (f);
end;

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  close;
end;


procedure TAboutBox.OpenURL(Sender: TObject);
begin
  ShellExec( TControl(Sender).Hint );
end;



procedure TAboutBox.ButtonCloseClick(Sender: TObject);
begin
  close;
end;


procedure TAboutBox.MouseOver(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  LabelWebpage.Font.color := clBlue;
  if sender is TLabel then
    TLabel(Sender).Font.color := clRed;
end;


procedure TAboutBox.FormShow(Sender: TObject);
var
  CompiledInt : Integer;
  Compiled    : TDateTime;
begin
  Screen.Cursor := crHourGlass;

  // App-Version
  LabelVersion.Caption := 'Version ' + appversion;

  // Compile-date
  CompiledInt := Fileage(paramstr(0));
  Compiled := FileDateToDateTime(CompiledInt);
  LabelCompiled.Caption := 'Compiled on: ' + DateTimeToStr(Compiled);

  Screen.Cursor := crDefault;
end;

end.

