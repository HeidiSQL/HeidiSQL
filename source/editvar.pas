unit editvar;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, SpinEx,
  dbconnection, dbstructures, ComCtrls, RegExpr, extra_controls;

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
    spinNumber: TSpinEditEx;
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
    procedure radioScopeChange(Sender: TObject);
    procedure spinNumberKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

uses StrUtils, LCLType, main, apphelpers;

{$R *.lfm}


const
  BooleanValues: array[0..5] of String = ('ON', 'OFF', 'YES', 'NO', 'TRUE', 'FALSE');


procedure TfrmEditVariable.FormCreate(Sender: TObject);
begin
  Width := AppSettings.ReadInt(asEditVarWindowWidth);
  Height := AppSettings.ReadInt(asEditVarWindowHeight);
end;


procedure TfrmEditVariable.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asEditVarWindowWidth, ScaleFormToDesign(Width));
  AppSettings.WriteInt(asEditVarWindowHeight, ScaleFormToDesign(Height));
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
var
  Conn: TDBConnection;
begin
  Conn := MainForm.ActiveConnection;
  gbValue.Caption := FVarName;
  btnHelp.Visible := Conn.Parameters.NetTypeGroup = ngMySQL;

  // Scope availability is derived from the server's session and global variables
  // results. Variables in neither list cannot exist, so leave both enabled then.
  radioScopeSession.Enabled := Conn.SqlProvider.Has(qSetSessionVariable)
    and (FHasSessionValue or (not FHasGlobalValue));
  radioScopeGlobal.Enabled := Conn.SqlProvider.Has(qSetGlobalVariable)
    and (FHasGlobalValue or (not FHasSessionValue));
  if radioScopeSession.Enabled then
    radioScopeSession.Checked := True
  else
    radioScopeGlobal.Checked := True;
  LoadValue;
end;


procedure TfrmEditVariable.radioScopeChange(Sender: TObject);
begin
  if Visible and (Sender as TRadioButton).Checked then
    LoadValue;
end;


procedure TfrmEditVariable.spinNumberKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // TSpinEditEx swallows Escape for resetting its value, so the Cancel button
  // would never get it
  if (Key = VK_ESCAPE) and (Shift = []) then begin
    Key := 0;
    ModalResult := mrCancel;
  end;
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
  spinNumber.Enabled := FVarType = vtNumeric;
  lblBoolean.Enabled := FVarType = vtBoolean;
  radioBooleanOn.Enabled := FVarType = vtBoolean;
  radioBooleanOff.Enabled := FVarType = vtBoolean;

  case FVarType of
    vtString: begin
      editString.Text := FVarValue;
      editString.SelectAll;
      editString.TrySetFocus;
    end;
    vtNumeric: begin
      spinNumber.Value := MakeInt(FVarValue);
      spinNumber.SelectAll;
      spinNumber.TrySetFocus;
    end;
    vtBoolean: begin
      if (val='ON') or (val='YES') or (val='TRUE') then begin
        radioBooleanOn.Checked := True;
        radioBooleanOn.TrySetFocus;
      end else begin
        radioBooleanOff.Checked := True;
        radioBooleanOff.TrySetFocus;
      end;
    end;
  end;
end;


{**
  Compose SQL query and set the new variable value
}
procedure TfrmEditVariable.btnOKClick(Sender: TObject);
var
  val: String;
  Conn: TDBConnection;
  QueryId: TQueryId;
begin
  Conn := MainForm.ActiveConnection;

  case FVarType of
    vtNumeric: val := spinNumber.Value.ToString;
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

  if radioScopeGlobal.Checked then
    QueryId := qSetGlobalVariable
  else
    QueryId := qSetSessionVariable;

  // Set the value and keep the form open in any error case, e.g. for read-only
  // variables or when the chosen scope is not supported by the variable
  try
    Conn.Query(Conn.SqlProvider.GetSql(QueryId, [FVarName, Conn.EscapeString(FVarName), val]));
    if (QueryId = qSetGlobalVariable) and Conn.SqlProvider.Has(qApplyGlobalVariables) then
      Conn.Query(Conn.SqlProvider.GetSql(qApplyGlobalVariables));
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
