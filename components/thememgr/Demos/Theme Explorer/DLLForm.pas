unit DLLForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ThemeMgr, ImgList;

type
  TThemedForm = class(TForm)
    CloseButton: TButton;
    ListView1: TListView;
    RadioGroup1: TRadioGroup;
    DLLTM: TThemeManager;
    SmallImages: TImageList;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ThemedForm: TThemedForm;

procedure HideDLLForm;
procedure ShowDLLForm;
procedure SetApplicationHandle(Handle: HWnd);

exports
  ShowDLLForm,
  HideDLLForm,
  SetApplicationHandle;
  
//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

procedure HideDLLForm;

begin
  FreeAndNil(ThemedForm);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ShowDLLForm;

begin
  if ThemedForm = nil then
    ThemedForm := TThemedForm.Create(nil);
  ThemedForm.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemedForm.CloseButtonClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetApplicationHandle(Handle: HWnd);

begin
  Application.Handle := Handle;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemedForm.FormDestroy(Sender: TObject);

begin
  Beep;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
finalization
  ThemedForm.Free;
end.
