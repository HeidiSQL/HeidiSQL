unit SubForm1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, ThemeMgr, Buttons;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    PaintBox1: TPaintBox;
    ListView2: TListView;
    procedure PaintBox1Paint(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Main, ThemeSrv;

//----------------------------------------------------------------------------------------------------------------------

procedure TForm1.PaintBox1Paint(Sender: TObject);

var
  R: TRect;
  Details: TThemedElementDetails;
  
begin
  with ThemeServices, PaintBox1, Canvas do
    if ThemesEnabled then
    begin
      R := ClientRect;
      Details := GetElementDetails(tebHeaderBackgroundNormal);
      DrawElement(Handle, Details, R);
      Font := Self.Font;
      InflateRect(R, -10, -10);
      DrawText(Handle, Details, 'Form with controls which are all subclassed.', R, DT_VCENTER, 0);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
