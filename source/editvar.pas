unit editvar;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  dbconnection, dbstructures, ComCtrls, gnugettext, SynRegExpr, extra_controls;

type
  TVarType = (vtString, vtNumeric, vtBoolean, vtEnum);
  EVariableError = class(Exception);

  TfrmEditVariable = class(TExtForm)
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
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
    FVar: TServerVariable;
    FVarType: TVarType;
    FVarValue: String;
    procedure SetVarName(Value: String);
  public
    { Public declarations }
    property VarName: String write SetVarName;
    property VarValue: String write FVarValue;
  end;


implementation

uses main, apphelpers;

{$R *.dfm}


procedure TfrmEditVariable.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
  Width := AppSettings.ReadInt(asEditVarWindowWidth);
  Height := AppSettings.ReadInt(asEditVarWindowHeight);
end;


procedure TfrmEditVariable.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asEditVarWindowWidth, Width);
  AppSettings.WriteInt(asEditVarWindowHeight, Height);
end;


procedure TfrmEditVariable.SetVarName(Value: String);
var
  i: Integer;
  Found: Boolean;
begin
  // Find var name in predefined documented list of variables
  Found := False;
  for i:=Low(MySQLVariables) to High(MySQLVariables) do begin
    if MySQLVariables[i].Name = Value then begin
      FVar := MySQLVariables[i];
      Found := True;
      if not FVar.IsDynamic then
        raise EVariableError.CreateFmt(_('"%s" is a read only variable, not editable.'), [Value]);
      break;
    end;
  end;
  if not Found then
    raise EVariableError.CreateFmt(_('Could not find %s variable in internal mapping.'), [Value]);
end;


procedure TfrmEditVariable.FormShow(Sender: TObject);
var
  val: String;
begin
  // Verify variable type by value
  FVarType := vtString;
  if IsInt(FVarValue) then
    FVarType := vtNumeric;
  if (FVar.EnumValues <> '') and (Pos(UpperCase(FVarValue), UpperCase(FVar.EnumValues))>0) then
    FVarType := vtEnum;
  if (FVarType <> vtEnum) and (Pos(UpperCase(FVarValue), 'ON,OFF,0,1,YES,NO')>0) then
    FVarType := vtBoolean;

  gbValue.Caption := FVar.Name;

  lblString.Enabled := FVarType = vtString;
  editString.Enabled := FVarType = vtString;
  lblNumber.Enabled := FVarType = vtNumeric;
  editNumber.Enabled := FVarType = vtNumeric;
  UpDownNumber.Enabled := FVarType = vtNumeric;
  lblEnum.Enabled := FVarType = vtEnum;
  comboEnum.Enabled := FVarType = vtEnum;
  lblBoolean.Enabled := FVarType = vtBoolean;
  radioBooleanOn.Enabled := FVarType = vtBoolean;
  radioBooleanOff.Enabled := FVarType = vtBoolean;

  case FVarType of
    vtString: begin
      editString.Text := FVarValue;
      editString.SelectAll;
      editString.SetFocus;
    end;
    vtNumeric: begin
      UpDownNumber.Position := MakeInt(FVarValue);
      editNumber.SelectAll;
      editNumber.SetFocus;
    end;
    vtBoolean: begin
      val := UpperCase(FVarValue);
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
      comboEnum.ItemIndex := comboEnum.Items.IndexOf(UpperCase(FVarValue));
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
  sql := sql + '.' + FVar.Name + ' = ';

  case FVarType of
    vtNumeric: val := IntToStr(UpDownNumber.Position);
    vtString: begin
      // Various variables are int64 or float, for which we have no specific
      // editor other than the string editor. Do not quote these to avoid
      // "Incorrect argument type to variable.." See issue #3153.
      if ExecRegExpr('^\d+(\.\d*)?$', FVarValue) then
        val := editString.Text
      else
        val := MainForm.ActiveConnection.EscapeString(editString.Text);
    end;
    vtBoolean: val := IntToStr(Integer(radioBooleanOn.Checked));
    vtEnum: val := MainForm.ActiveConnection.EscapeString(comboEnum.Text);
  end;
  sql := sql + val;

  // Set the value and keep the form open in any error case
  try
    MainForm.ActiveConnection.Query(sql);
  except
    on E:EDbError do begin
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
