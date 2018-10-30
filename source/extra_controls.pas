unit extra_controls;

interface

uses
  Classes, SysUtils, Forms, Windows, Messages, System.Types, StdCtrls, Clipbrd,
  SizeGrip, SizeGripThemed;

type
  // Form with a sizegrip in the lower right corner, without the need for a statusbar
  TFormWithSizeGrip = class(TForm)
    private
      FGripper: TSizeGripThemed;
    public
      constructor Create(AOwner: TComponent); override;
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
  FGripper := TSizeGripThemed.Create(Self);
  FGripper.Themed := True;
  FGripper.Enabled := True;
  FGripper.Style := sgsWinXP;
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
