unit extra_controls;

interface

uses
  Classes, SysUtils, Forms, Windows, Messages, System.Types, StdCtrls, Clipbrd,
  SizeGrip, apphelpers, Vcl.Graphics, Vcl.Dialogs, gnugettext, Vcl.ImgList, Vcl.ComCtrls;

type
  // Form with a sizegrip in the lower right corner, without the need for a statusbar
  TExtForm = class(TForm)
    private
      FSizeGrip: TSizeGripXP;
      function GetHasSizeGrip: Boolean;
      procedure SetHasSizeGrip(Value: Boolean);
    public
      constructor Create(AOwner: TComponent); override;
      procedure InheritFont(AFont: TFont);
      property HasSizeGrip: Boolean read GetHasSizeGrip write SetHasSizeGrip default False;
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
var
  OldImageList: TCustomImageList;
  i: Integer;
  Cmp: TComponent;
begin
  inherited;

  InheritFont(Font);
  HasSizeGrip := False;

  // Reduce flicker on Windows 10
  // See https://www.heidisql.com/forum.php?t=19141
  if CheckWin32Version(6, 2) then begin
    DoubleBuffered := True;
  end;

  // Translation and related fixes
  // Issue #557: Apply images *after* translating main menu, so top items don't get unused
  // space left besides them.
  if (Menu <> nil) and (Menu.Images <> nil) then begin
    OldImageList := Menu.Images;
    Menu.Images := nil;
    TranslateComponent(Self);
    Menu.Images := OldImageList;
  end else begin
    TranslateComponent(Self);
  end;
  // Work around broken dropdown (tool)button on Wine after translation:
  // https://sourceforge.net/p/dxgettext/bugs/80/
  for i:=0 to ComponentCount-1 do begin
    Cmp := Components[i];
    if (Cmp is TButton) and (TButton(Cmp).Style = bsSplitButton) then begin
      TButton(Cmp).Style := bsPushButton;
      TButton(Cmp).Style := bsSplitButton;
    end;
    if (Cmp is TToolButton) and (TToolButton(Cmp).Style = tbsDropDown) then begin
      TToolButton(Cmp).Style := tbsButton;
      TToolButton(Cmp).Style := tbsDropDown;
    end;
  end;
end;


function TExtForm.GetHasSizeGrip: Boolean;
begin
  Result := FSizeGrip <> nil;
end;


procedure TExtForm.SetHasSizeGrip(Value: Boolean);
begin
  if Value then begin
    FSizeGrip := TSizeGripXP.Create(Self);
    FSizeGrip.Enabled := True;
  end else begin
    if FSizeGrip <> nil then
      FreeAndNil(FSizeGrip);
  end;
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
    AFont.Size := AppSettings.ReadInt(asGUIFontSize);
  end else begin
    // Apply system font. See issue #3204.
    // Code taken from http://www.gerixsoft.com/blog/delphi/system-font
    if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(TLogFont), @LogFont, 0) then begin
      AFont.Height := LogFont.lfHeight;
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
