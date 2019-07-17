unit theme_preview;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, apphelpers,
  Vcl.ComCtrls, Vcl.GraphUtil, Vcl.Imaging.pngimage, extra_controls;

type
  TfrmThemePreview = class(TExtForm)
    StatusBarMain: TStatusBar;
    ScrollBoxImage: TScrollBox;
    imagePreview: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ScrollBoxImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
    FToggleCheckbox: TCheckBox;
    FLastStatusUpdate: Cardinal;
    FTempFile: String;
    procedure DownloadProgress(Sender: TObject);
  public
    { Public declarations }
    procedure LoadTheme(ThemeName: String);
  end;


implementation

{$R *.dfm}



procedure TfrmThemePreview.FormCreate(Sender: TObject);
begin
  FToggleCheckbox := TCheckBox(Owner);
end;


procedure TfrmThemePreview.FormShow(Sender: TObject);
begin
  Width := AppSettings.ReadInt(asThemePreviewWidth);
  Height := AppSettings.ReadInt(asThemePreviewHeight);
  Top := AppSettings.ReadInt(asThemePreviewTop);
  Left := AppSettings.ReadInt(asThemePreviewLeft);
  ToggleCheckBoxWithoutClick(FToggleCheckbox, True);
end;

procedure TfrmThemePreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  AppSettings.WriteInt(asThemePreviewWidth, Width);
  AppSettings.WriteInt(asThemePreviewHeight, Height);
  AppSettings.WriteInt(asThemePreviewTop, Top);
  AppSettings.WriteInt(asThemePreviewLeft, Left);
  ToggleCheckBoxWithoutClick(FToggleCheckbox, False);
  Action := caFree;
end;


procedure TfrmThemePreview.LoadTheme(ThemeName: String);
var
  Download: THttpDownload;
  ThemeImage: String;
begin
  Download := THttpDownload.Create(Self);
  ThemeImage := ThemeName;
  ThemeImage := ThemeName.Replace(' ', '-').ToLower;
  Download.URL := Format('%simages/themes/%s.png', [APPDOMAIN, ThemeImage]);
  StatusBarMain.SimpleText := 'Loading preview: ' + Download.URL;
  FTempFile := Format('%s%s-themepreview-%s.png', [GetTempDir, APPNAME, ThemeImage]);
  Download.OnProgress := DownloadProgress;
  try
    Download.SendRequest(FTempFile);
    imagePreview.Picture.LoadFromFile(FTempFile);
    StatusBarMain.SimpleText := Format('Theme name: %s', [ThemeName]);
  except
    on E:Exception do begin
      StatusBarMain.SimpleText := E.Message;
    end;
  end;
end;


procedure TfrmThemePreview.ScrollBoxImageMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  // Scrolling via mouse wheel
  // Do not use .ScrollBy(), which makes the scroll bar longer
  if KeyPressed(VK_SHIFT) then begin
    ScrollBoxImage.HorzScrollBar.Position := ScrollBoxImage.HorzScrollBar.Position - WheelDelta;
  end else begin
    ScrollBoxImage.VertScrollBar.Position := ScrollBoxImage.VertScrollBar.Position - WheelDelta;
  end;
  Handled := True;
end;

procedure TfrmThemePreview.DownloadProgress(Sender: TObject);
var
  Download: THttpDownload;
begin
  if FLastStatusUpdate > GetTickCount-200 then
    Exit;
  Download := Sender as THttpDownload;
  StatusBarMain.SimpleText := f_('Downloading: %s / %s', [FormatByteNumber(Download.BytesRead), FormatByteNumber(Download.ContentLength)]) + ' ...';
  FLastStatusUpdate := GetTickCount;
end;



end.
