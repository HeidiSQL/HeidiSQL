unit About;


// -------------------------------------
// HeidiSQL
// About-box
// -------------------------------------


interface

uses Windows, Menus, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellApi, SysUtils, dialogs, ComCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Panel2: TPanel;
    ProductName: TLabel;
    LabelVersion: TLabel;
    LabelPlatform: TLabel;
    LabelCompiled: TLabel;
    Label1: TLabel;
    LabelWebpage: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Button2: TButton;
    Button3: TButton;
    Image2: TImage;
    procedure OKButtonClick(Sender: TObject);
    procedure OpenURL(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MouseOver(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses main;
{$R *.DFM}



procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  close;
end;


procedure TAboutBox.OpenURL(Sender: TObject);
var url :  Pchar;
begin
  url := pchar(TControl(Sender).Hint);
  shellexecute(0, 'open', url, Nil, Nil, sw_shownormal);
end;



procedure TAboutBox.Button1Click(Sender: TObject);
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
  OSVersion   : OSVERSIONINFO;
  os, ver     : String;
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

  // Windows-version
  FillChar(OSVersion,SizeOf(OSVersion),0);
  OSVersion.dwOSVersionInfoSize:=SizeOf(OSVersion);
  if not GetVersionEx(OSVersion) then RaiseLastWin32Error;
  case OSVersion.dwPlatformId of
    VER_PLATFORM_WIN32s : os := 'Windows® 3.1';
    VER_PLATFORM_WIN32_WINDOWS : os := 'Windows® 9x';
    VER_PLATFORM_WIN32_NT : os := 'Windows® NT';
  end;
  if (OSVersion.dwMajorVersion=5) and (OSVersion.dwMinorVersion=0) then
    os := 'Windows® 2000';
  if (OSVersion.dwMajorVersion=5) and (OSVersion.dwMinorVersion>0) then
    os := 'Windows® XP';
  ver := IntToStr(OSVersion.dwMajorVersion);
  if IntToStr(OSVersion.dwMinorVersion) <> '' then
    ver := ver + '.' + IntToStr(OSVersion.dwMinorVersion);
  ver := ver + ' ' + OSVersion.szCSDVersion;              // Servicepack
  LabelPlatform.Caption := 'Client-OS: ' + os + ' ' + ver;

  Screen.Cursor := crDefault;
end;

end.

