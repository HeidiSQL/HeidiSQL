unit editvar;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  dbconnection, mysql_structures, ComCtrls;

type
  TfrmEditVariable = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    grpScope: TGroupBox;
    radioScopeSession: TRadioButton;
    radioScopeGlobal: TRadioButton;
    gbValue: TGroupBox;
    radioBooleanOn: TRadioButton;
    radioBooleanOff: TRadioButton;
    comboEnum: TComboBox;
    editNumber: TEdit;
    UpDownNumber: TUpDown;
    editString: TEdit;
    lblString: TLabel;
    lblNumber: TLabel;
    lblEnum: TLabel;
    lblBoolean: TLabel;
    btnHelp: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
    FVar: TServerVariable;
  public
    { Public declarations }
    VarName, VarValue: String;
  end;


implementation

uses main, helpers;

{$R *.dfm}


procedure TfrmEditVariable.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
  SetWindowSizeGrip(Handle, True);
  Width := GetRegValue(REGNAME_EDITVARWINWIDTH, Width);
  Height := GetRegValue(REGNAME_EDITVARWINHEIGHT, Height);
end;


procedure TfrmEditVariable.FormDestroy(Sender: TObject);
begin
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_EDITVARWINWIDTH, Width);
  MainReg.WriteInteger(REGNAME_EDITVARWINHEIGHT, Height);
end;


procedure TfrmEditVariable.FormShow(Sender: TObject);
var
  i: Integer;
  val: String;
begin
  // Find var name in predefined documented list of variables
  for i:=Low(MySQLVariables) to High(MySQLVariables) do begin
    if MySQLVariables[i].Name = VarName then begin
      FVar := MySQLVariables[i];
      break;
    end;
  end;
  // Verify variable type by value
  if (FVar.VarType = vtNumeric) and (not IsNumeric(VarValue)) then
    FVar.VarType := vtString;
  if (FVar.VarType = vtEnum) and (Pos(UpperCase(VarValue), UpperCase(FVar.EnumValues))=0) then
    FVar.VarType := vtString;
  if (FVar.VarType = vtBoolean) and (Pos(UpperCase(VarValue), 'ON,OFF,0,1,YES,NO')=0) then
    FVar.VarType := vtString;
  if (FVar.VarType = vtString) and (Pos(UpperCase(VarValue), 'ON,OFF,0,1,YES,NO')>0) then
    FVar.VarType := vtBoolean;

  gbValue.Caption := VarName;

  lblString.Enabled := FVar.VarType = vtString;
  editString.Enabled := FVar.VarType = vtString;
  lblNumber.Enabled := FVar.VarType = vtNumeric;
  editNumber.Enabled := FVar.VarType = vtNumeric;
  UpDownNumber.Enabled := FVar.VarType = vtNumeric;
  lblEnum.Enabled := FVar.VarType = vtEnum;
  comboEnum.Enabled := FVar.VarType = vtEnum;
  lblBoolean.Enabled := FVar.VarType = vtBoolean;
  radioBooleanOn.Enabled := FVar.VarType = vtBoolean;
  radioBooleanOff.Enabled := FVar.VarType = vtBoolean;

  case FVar.VarType of
    vtString: begin
      editString.Text := VarValue;
      editString.SelectAll;
      editString.SetFocus;
    end;
    vtNumeric: begin
      UpDownNumber.Position := MakeInt(VarValue);
      editNumber.SelectAll;
      editNumber.SetFocus;
    end;
    vtBoolean: begin
      val := UpperCase(VarValue);
      if (val='ON') or (val='1') or (val='YES') then begin
        radioBooleanOn.Checked := True;
        radioBooleanOn.SetFocus;
      end else begin
        radioBooleanOff.Checked := True;
        radioBooleanOff.SetFocus;
      end;
    end;
    vtEnum: begin
      comboEnum.Items.CommaText := FVar.EnumValues;
      comboEnum.ItemIndex := comboEnum.Items.IndexOf(UpperCase(VarValue));
      comboEnum.SetFocus;
    end;
  end;

  radioScopeSession.Enabled := FVar.VarScope in [vsSession, vsBoth];
  radioScopeGlobal.Enabled := FVar.VarScope in [vsGlobal, vsBoth];
  if radioScopeSession.Enabled then
    radioScopeSession.Checked := True
  else if radioScopeGlobal.Enabled then
    radioScopeGlobal.Checked := True;
end;


procedure TfrmEditVariable.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


{**
  Compose SQL query and set the new variable value
}
procedure TfrmEditVariable.btnOKClick(Sender: TObject);
var
  sql, val: String;
begin
  // Syntax taken from http://dev.mysql.com/doc/refman/4.1/en/using-system-variables.html
  sql := 'SET @@';
  if radioScopeSession.Checked then
    sql := sql + 'session'
  else
    sql := sql + 'global';
  sql := sql + '.' + VarName + ' = ';

  case FVar.VarType of
    vtNumeric: val := IntToStr(UpDownNumber.Position);
    vtString: val := MainForm.ActiveConnection.EscapeString(editString.Text);
    vtBoolean: val := IntToStr(Integer(radioBooleanOn.Checked));
    vtEnum: val := MainForm.ActiveConnection.EscapeString(comboEnum.Text);
  end;
  sql := sql + val;

  // Set the value and keep the form open in any error case
  try
    MainForm.ActiveConnection.Query(sql);
  except
    on E:EDatabaseError do begin
      ModalResult := mrNone;
      ErrorDialog(E.Message);
    end;
  end;
end;


procedure TfrmEditVariable.btnHelpClick(Sender: TObject);
begin
  ShellExec('http://dev.mysql.com/doc/refman/5.6/en/dynamic-system-variables.html');
end;


end.
