unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TntForms, StdCtrls, CheckLst, TntStdCtrls, TntDBCtrls, TntCheckLst;

type
  TMainForm = class(TTntForm)
    TntComboBox1: TTntComboBox;
    TntEdit1: TTntEdit;
    TntMemo1: TTntMemo;
    TntListBox1: TTntListBox;
    TntCheckListBox1: TTntCheckListBox;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Label3: TTntLabel;
    Label4: TTntLabel;
    Label5: TTntLabel;
    Button1: TTntButton;
    Button2: TTntButton;
    Button3: TTntButton;
    Button4: TTntButton;
    Label6: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Font.Name := 'MS Shell Dlg 2'
  else
    Font.Name := 'MS Shell Dlg';
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Caption := TntEdit1.Text;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  TntComboBox1.Items.Add(TntEdit1.Text);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  TntListBox1.Items.Add(TntEdit1.Text);
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  TntCheckListbox1.Items.Add(TntEdit1.Text);
end;

end.
