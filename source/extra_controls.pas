unit extra_controls;

interface

uses
  Classes, SysUtils, Forms, Windows, Messages, System.Types, StdCtrls, Clipbrd,
  SizeGrip, SizeGripThemed, apphelpers, Vcl.Graphics, Vcl.Dialogs;

type
  // Form with a sizegrip in the lower right corner, without the need for a statusbar
  TExtForm = class(TForm)
    private
      FFontSet: Boolean;
      FPixelsPerInchOnDefaultMonitor: Integer;
    public
      constructor Create(AOwner: TComponent); override;
      procedure AddSizeGrip;
      function DpiScaleFactor(Form: TForm=nil): Double;
      procedure InheritFont(AFont: TFont);
    protected
      procedure DoShow; override;
  end;
  // Memo replacement which accepts any line break format
  TLineNormalizingMemo = class(TMemo)
    private
      procedure WMSetText(var msg: TWMSettext); message WM_SETTEXT;
      procedure WMPaste(var msg: TWMPaste); message WM_PASTE;
  end;


implementation


{ TExtForm }

constructor TExtForm.Create(AOwner: TComponent);
begin
  FPixelsPerInchOnDefaultMonitor := Screen.Monitors[0].PixelsPerInch;
  inherited;
  FFontSet := False;
end;


procedure TExtForm.DoShow;
begin
  // Expect the window to be on the wanted monitor now, so we can scale fonts according
  // to the screen's DPI setting
  if not FFontSet then begin
    InheritFont(Font);
    FFontSet := True;
  end;
  inherited;
end;


procedure TExtForm.AddSizeGrip;
var
  FGripper: TSizeGripThemed;
begin
  FGripper := TSizeGripThemed.Create(Self);
  FGripper.Themed := True;
  FGripper.Enabled := True;
  FGripper.Style := sgsWinXP;
end;


function TExtForm.DpiScaleFactor(Form: TForm=nil): Double;
begin
  Result := Monitor.PixelsPerInch / FPixelsPerInchOnDefaultMonitor;
end;


procedure TExtForm.InheritFont(AFont: TFont);
var
  LogFont: TLogFont;
  GUIFontName: String;
begin
  // Set custom font if set, or default system font.
  // In high-dpi mode, the font *size* is increased automatically somewhere in the VCL,
  // caused by a form's .Scaled property. So we don't increase it here again.
  // To test this, you really need to log off/on Windows!
  GUIFontName := AppSettings.ReadString(asGUIFontName);
  if not GUIFontName.IsEmpty then begin
    // Apply user specified font
    AFont.Name := GUIFontName;
    // Set size on top of automatic dpi-increased size
    AFont.Size := Round(AppSettings.ReadInt(asGUIFontSize) * DpiScaleFactor);
  end else begin
    // Apply system font. See issue #3204.
    // Code taken from http://www.gerixsoft.com/blog/delphi/system-font
    if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(TLogFont), @LogFont, 0) then begin
      AFont.Height := Round(LogFont.lfHeight * DpiScaleFactor);
      AFont.Orientation := LogFont.lfOrientation;
      AFont.Charset := TFontCharset(LogFont.lfCharSet);
      AFont.Name := PChar(@LogFont.lfFaceName);
      case LogFont.lfPitchAndFamily and $F of
        VARIABLE_PITCH: AFont.Pitch := fpVariable;
        FIXED_PITCH: AFont.Pitch := fpFixed;
        else AFont.Pitch := fpDefault;
      end;
    end else begin
      ErrorDialog('Could not detect system font, using SystemParametersInfo.');
    end;
  end;
end;




{ TLineNormalizingMemo }

procedure TLineNormalizingMemo.WMSetText(var msg: TWMSettext);
var
  s: string;
begin
  s := msg.Text;
  s := AdjustLineBreaks(s);
  msg.Text := PChar(s);
  inherited;
end;


procedure TLineNormalizingMemo.WMPaste(var msg: TWMPaste);
var
  s: string;
begin
  if Clipboard.HasFormat(cf_Text) then begin
    s := Clipboard.AsText;
    s := AdjustLineBreaks(s);
    SelText := s;
  end;
end;

end.
