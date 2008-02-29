unit editvar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmEditVariable = class(TForm)
    editValue: TEdit;
    rgScope: TRadioGroup;
    lblValue: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure editValueChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    VarName, VarValue: String;
  end;

implementation

uses main, helpers;

{$R *.dfm}


procedure TfrmEditVariable.FormShow(Sender: TObject);
begin
  lblValue.Caption := 'New value for "'+VarName+'":';
  editValue.Text := VarValue;
  editValue.SelectAll;
  editValue.SetFocus;
end;


{**
  Compose SQL query and set the new variable value
}
procedure TfrmEditVariable.btnOKClick(Sender: TObject);
var
  sql: String;
begin
  // Syntax taken from http://dev.mysql.com/doc/refman/4.1/en/using-system-variables.html
  sql := 'SET @@';
  if rgScope.ItemIndex = 0 then
    sql := sql + 'session'
  else
    sql := sql + 'global';
  sql := sql + '.' + VarName + ' = ';

  // Test if the original value is numerical and should be passed without quotes
  // Avoids SQL error "Wrong argument type to variable"
  if IntToStr(MakeInt(VarValue)) = VarValue then
    sql := sql + IntToStr(MakeInt(editValue.Text))
  else
    sql := sql + esc(editValue.Text);

  // Set the value and keep the form open in any error case
  try
    Mainform.Childwin.ExecUpdateQuery(sql, False, True);
  except
    ModalResult := mrNone;
  end;
end;


{**
  Enable the OK button only if the value is different from the initial one
}
procedure TfrmEditVariable.editValueChange(Sender: TObject);
begin
  btnOK.Enabled := editValue.Text <> VarValue;
end;


end.
