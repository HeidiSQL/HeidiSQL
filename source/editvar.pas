unit editvar;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  dbconnection, dbstructures, Vcl.ComCtrls, gnugettext, SynRegExpr, extra_controls;

type
  TVarType = (vtString, vtNumeric, vtBoolean);

  TfrmEditVariable = class(TExtForm)
    btnOK: TButton;
    btnCancel: TButton;
    grpScope: TGroupBox;
    radioScopeSession: TRadioButton;
    radioScopeGlobal: TRadioButton;
    gbValue: TGroupBox;
    radioBooleanOn: TRadioButton;
    radioBooleanOff: TRadioButton;
    editNumber: TEdit;
    UpDownNumber: TUpDown;
    editString: TEdit;
    lblString: TLabel;
    lblNumber: TLabel;
    lblBoolean: TLabel;
    btnHelp: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure radioScopeClick(Sender: TObject);
  private
    { Private declarations }
    FVarName: String;
    FVarType: TVarType;
    FVarValue: String;
    FSessionValue: String;
    FGlobalValue: String;
    FHasSessionValue: Boolean;
    FHasGlobalValue: Boolean;
    procedure LoadValue;
  public
    { Public declarations }
    procedure SetSessionValue(Value: String);
    procedure SetGlobalValue(Value: String);
    property VarName: String write FVarName;
  end;


implementation

uses System.StrUtils, main, apphelpers;

{$R *.dfm}


const
  BooleanValues: array[0..5] of String = ('ON', 'OFF', 'YES', 'NO', 'TRUE', 'FALSE');


procedure TfrmEditVariable.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
end;


procedure TfrmEditVariable.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteIntDpiAware(asEditVarWindowWidth, Self, Width);
  AppSettings.WriteIntDpiAware(asEditVarWindowHeight, Self, Height);
end;


procedure TfrmEditVariable.SetSessionValue(Value: String);
begin
  FSessionValue := Value;
  FHasSessionValue := True;
end;


procedure TfrmEditVariable.SetGlobalValue(Value: String);
begin
  FGlobalValue := Value;
  FHasGlobalValue := True;
end;


procedure TfrmEditVariable.FormShow(Sender: TObject);
begin
  Width := AppSettings.ReadIntDpiAware(asEditVarWindowWidth, Self);
  Height := AppSettings.ReadIntDpiAware(asEditVarWindowHeight, Self);

  gbValue.Caption := FVarName;

  // Scope availability is derived from the server's SHOW SESSION/GLOBAL VARIABLES
  // results. Variables in neither list cannot exist, so leave both enabled then.
  radioScopeSession.Enabled := FHasSessionValue or (not FHasGlobalValue);
  radioScopeGlobal.Enabled := FHasGlobalValue or (not FHasSessionValue);
  if radioScopeSession.Enabled then
    radioScopeSession.Checked := True
  else
    radioScopeGlobal.Checked := True;
  LoadValue;
end;


procedure TfrmEditVariable.radioScopeClick(Sender: TObject);
begin
  if Visible and (Sender as TRadioButton).Checked then
    LoadValue;
end;


{**
  Detect value type from the value of the selected scope and prepare the editor
}
procedure TfrmEditVariable.LoadValue;
var
  val: String;
begin
  if radioScopeGlobal.Checked then
    FVarValue := FGlobalValue
  else
    FVarValue := FSessionValue;

  val := UpperCase(FVarValue);
  if IsInt(FVarValue) then
    FVarType := vtNumeric
  else if MatchStr(val, BooleanValues) then
    FVarType := vtBoolean
  else
    FVarType := vtString;

  lblString.Enabled := FVarType = vtString;
  editString.Enabled := FVarType = vtString;
  lblNumber.Enabled := FVarType = vtNumeric;
  editNumber.Enabled := FVarType = vtNumeric;
  UpDownNumber.Enabled := FVarType = vtNumeric;
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
      if (val='ON') or (val='YES') or (val='TRUE') then begin
        radioBooleanOn.Checked := True;
        radioBooleanOn.SetFocus;
      end else begin
        radioBooleanOff.Checked := True;
        radioBooleanOff.SetFocus;
      end;
    end;
  end;
end;


{**
  Compose SQL query and set the new variable value
}
procedure TfrmEditVariable.btnOKClick(Sender: TObject);
var
  sql, val: String;
  Conn: TDBConnection;
begin
  // Syntax taken from http://dev.mysql.com/doc/refman/4.1/en/using-system-variables.html
  sql := 'SET @@';
  if radioScopeGlobal.Checked then
    sql := sql + 'global'
  else
    sql := sql + 'session';
  sql := sql + '.' + FVarName + ' = ';

  Conn := MainForm.ActiveConnection;

  case FVarType of
    vtNumeric: val := IntToStr(UpDownNumber.Position);
    vtString: begin
      // Various variables are int64 or float, for which we have no specific
      // editor other than the string editor. Do not quote these to avoid
      // "Incorrect argument type to variable.." See issue #3153.
      if ExecRegExpr('^\d+(\.\d*)?$', FVarValue) then
        val := editString.Text
      else
        val := Conn.EscapeString(editString.Text);
    end;
    vtBoolean: val := IntToStr(Integer(radioBooleanOn.Checked));
  end;
  sql := sql + val;

  // Set the value and keep the form open in any error case, e.g. for read-only
  // variables or when the chosen scope is not supported by the variable
  try
    Conn.Query(sql);
    Conn.ShowWarnings;
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
