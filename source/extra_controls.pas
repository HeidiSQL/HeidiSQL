unit extra_controls;

interface

uses
  Classes, SysUtils, Forms, Windows, Messages;

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


end.
