unit SubForm3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TForm3 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Buttons;
  
//----------------------------------------------------------------------------------------------------------------------

procedure TForm3.FormCreate(Sender: TObject);

var
  I: Integer;
  
begin
  // In order to avoid trouble at design time we create the contents of the tabsheet dynamically. It is only to show
  // how the subclassing affects speed. We have 10 tabsheets and fill each with 100 control for a total of
  // 1000 controls (plus the tabsheet). All of them are subclassed. 

  Randomize;
  // Tabsheet 1
  for I := 0 to 99 do
    with TButton.Create(Self) do
    begin
      Left := Random(Tabsheet1.Width);
      Top := Random(Tabsheet1.Height);
      Width := 100;
      Height := 24;
      Parent := TabSheet1;
      Caption := IntToStr(I);
      Show;
    end;

  // Tabsheet 2
  for I := 0 to 99 do
    with TGroupBox.Create(Self) do
    begin
      Left := Random(Tabsheet2.Width);
      Top := Random(Tabsheet2.Height);
      Parent := TabSheet2;
      Caption := IntToStr(I);
      Show;
    end;

  // Tabsheet 3
  for I := 0 to 99 do
    with TBitBtn.Create(Self) do
    begin
      Left := Random(Tabsheet3.Width);
      Top := Random(Tabsheet3.Height);
      Width := 100;
      Height := 24;
      Enabled := Boolean(Random(2));
      Parent := TabSheet3;
      Caption := IntToStr(I);
      Kind := TBitBtnKind(Random(Ord(High(TBitBtnKind)) + 1));
      Show;
    end;

  // Tabsheet 4
  for I := 0 to 99 do
    with TListView.Create(Self) do
    begin
      Left := Random(Tabsheet4.Width);
      Top := Random(Tabsheet4.Height);
      Parent := TabSheet4;
      Show;
    end;

  // Tabsheet 5
  for I := 0 to 99 do
    with TCheckBox.Create(Self) do
    begin
      Left := Random(Tabsheet5.Width);
      Top := Random(Tabsheet5.Height);
      Parent := TabSheet5;
      Caption := IntToStr(I);
      Show;
    end;

  // Tabsheet 6
  for I := 0 to 99 do
    with TRadioButton.Create(Self) do
    begin
      Left := Random(Tabsheet6.Width);
      Top := Random(Tabsheet6.Height);
      Parent := TabSheet6;
      Caption := IntToStr(I);
      Show;
    end;

  // Tabsheet 7
  for I := 0 to 99 do
    with TButton.Create(Self) do
    begin
      Left := Random(Tabsheet7.Width);
      Top := Random(Tabsheet7.Height);
      Parent := TabSheet7;
      Caption := IntToStr(I);
      Show;
    end;

  // Tabsheet 8
  for I := 0 to 99 do
    with TGroupBox.Create(Self) do
    begin
      Left := Random(Tabsheet8.Width);
      Top := Random(Tabsheet8.Height);
      Parent := TabSheet8;
      Caption := IntToStr(I);
      Show;
    end;

  // Tabsheet 9
  for I := 0 to 99 do
    with TBitBtn.Create(Self) do
    begin
      Left := Random(Tabsheet9.Width);
      Top := Random(Tabsheet9.Height);
      Parent := TabSheet9;
      Caption := IntToStr(I);
      Kind := TBitBtnKind(Random(Ord(High(TBitBtnKind)) + 1));
      Show;
    end;

  // Tabsheet 10
  for I := 0 to 99 do
    with TCheckBox.Create(Self) do
    begin
      Left := Random(Tabsheet10.Width);
      Top := Random(Tabsheet10.Height);
      Parent := TabSheet10;
      Caption := IntToStr(I);
      Show;
    end;

  Tabsheet1.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  Action := caFree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TForm3.Button1Click(Sender: TObject);
begin
  TForm3.Create(Application).Show;
end;

end.
