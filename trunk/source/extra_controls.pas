unit extra_controls;

interface

uses
  Classes, SysUtils, Forms, Windows, Messages, System.Types, StdCtrls, Clipbrd;

type
  // Form with a sizegrip in the lower right corner, without the need for a statusbar
  TFormWithSizeGrip = class(TForm)
    private
      FSizeGripRect: TRect;
      FSizeGripWidth: Integer;
      FSizeGripHeight: Integer;
      procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Paint; override;
      procedure Resize; override;
  end;
  // Memo replacement which accepts any line break format
  TLineNormalizingMemo = class(TMemo)
    private
      procedure WMSetText(var msg: TWMSettext); message WM_SETTEXT;
      procedure WMPaste(var msg: TWMPaste); message WM_PASTE;
  end;


implementation


{ TFormWithSizeGrip }

constructor TFormWithSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
  FSizeGripWidth := GetSystemMetrics(SM_CXVSCROLL);
  FSizeGripHeight := GetSystemMetrics(SM_CYHSCROLL);
end;


procedure TFormWithSizeGrip.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
  if PtInRect(FSizeGripRect, ScreenToClient(SmallPointToPoint(Msg.Pos))) then
    Msg.Result := HTBOTTOMRIGHT;
end;


procedure TFormWithSizeGrip.Paint;
begin
  inherited;
  DrawFrameControl(Canvas.Handle, FSizeGripRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
end;


procedure TFormWithSizeGrip.Resize;
begin
  inherited;
  FSizeGripRect := ClientRect;
  FSizeGripRect.Left := FSizeGripRect.Right - FSizeGripWidth;
  FSizeGripRect.Top := FSizeGripRect.Bottom - FSizeGripHeight;
  Refresh;
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
