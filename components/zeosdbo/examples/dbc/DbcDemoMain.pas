unit DbcDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, Grids;

type
  TfrmDBCDemo = class(TForm)
    Button1: TButton;
    cobProtocol: TComboBox;
    Pannel1: TPanel;
    Label1: TLabel;
    edtHostName: TEdit;
    Label2: TLabel;
    spePort: TSpinEdit;
    Label3: TLabel;
    edtDatabase: TEdit;
    Label4: TLabel;
    edtUsername: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    edtPassword: TEdit;
    sgResults: TStringGrid;
    Panel1: TPanel;
    memSQL: TMemo;
    Label7: TLabel;
    Button2: TButton;
    Panel2: TPanel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure sgResultsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure sgResultsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure Button5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDBCDemo: TfrmDBCDemo;

implementation

uses ZClasses, ZDbcIntfs, ZCompatibility, 
//Only those drivers will be supported for which you load the proper unit, see below
ZDbcMySql, ZDbcInterbase6, ZDbcPostgreSql, ZDbcDBLib;

{$R *.dfm}

var
  Connection: IZConnection;
  ResultSet: IZResultSet;
  LastRowNr: Integer;//This is to detect row nr change
  InsertState: Boolean;

{**
This is just fills the protocols combobox with the available protocols
}
procedure TfrmDBCDemo.FormShow(Sender: TObject);
var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  cobProtocol.Clear;
  Drivers := DriverManager.GetDrivers;
  for I := 0 to Drivers.Count - 1 do
  begin
    Protocols := (Drivers.Items[I] as IZDriver).GetSupportedProtocols;
    for J := 0 to High(Protocols) do
      cobProtocol.Items.Add(Protocols[J]);
  end;
  cobProtocol.Sorted := True;
end;

{**
Close the connection if active
}
procedure TfrmDBCDemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Connection) then
    if not Connection.IsClosed then
      Connection.Close;
end;

{**
The connection is made here.
You can see how the connection string is generated and how to connect to the database.
}
procedure TfrmDBCDemo.Button1Click(Sender: TObject);
var
  Url: string;
begin
  if spePort.Value <> 0 then
  begin
    Url := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s', [cobProtocol.Text, edtHostName.Text,
      spePort.Value, edtDatabase.Text, edtUserName.Text, edtPassword.Text]);
  end
  else
  begin
    Url := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s', [cobProtocol.Text, edtHostName.Text,
      edtDatabase.Text, edtUserName.Text, edtPassword.Text]);
  end;
  Connection := DriverManager.GetConnectionWithParams(Url, nil);
  Connection.SetAutoCommit(True);
  Connection.SetTransactionIsolation(tiReadCommitted);
  Connection.Open;
end;

{**
A statement is executed here
If the statement returns a resultset then the resultset is loaded into the stringgrid.
There methods for access by name also, for example GetStringByName
}
procedure TfrmDBCDemo.Button2Click(Sender: TObject);
var
  Statement: IZStatement;
  I: Integer;
  Value: string;
begin
  Statement := Connection.CreateStatement;
//This is not neccesseary if you do not want to modify the data 
  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery(memSQL.Text);
  sgResults.RowCount := 1;
  sgResults.ColCount := 1;
//Was any resultset returned?
  if Assigned(ResultSet) then
  begin
    sgResults.ColCount := ResultSet.GetMetadata.GetColumnCount;
    for I := 1 to ResultSet.GetMetadata.GetColumnCount do
      sgResults.Cells[I - 1, 0] := ResultSet.GetMetadata.GetColumnName(I);
//Make a cycle for each record in the resultset
    while ResultSet.Next do
    begin
      sgResults.RowCount := sgResults.RowCount + 1;
      sgResults.FixedRows := 1;
      for I := 1 to ResultSet.GetMetadata.GetColumnCount do
      begin
//read out the proper value for the column
        case ResultSet.GetMetadata.GetColumnType(I) of
          stBoolean: if ResultSet.GetBoolean(I) then Value := 'True' else Value := 'False';
          stByte: Value := IntToStr(ResultSet.GetByte(I));
          stShort: Value := IntToStr(ResultSet.GetShort(I));
          stInteger: Value := IntToStr(ResultSet.GetInt(I));
          stLong: Value := IntToStr(ResultSet.GetLong(I));
          stFloat: Value := FloatToStr(ResultSet.GetFloat(I));
          stDouble: Value := FloatToStr(ResultSet.GetDouble(I));
          stBigDecimal: Value := FloatToStr(ResultSet.GetBigDecimal(I));
          stBytes, stBinaryStream: Value := ResultSet.GetBlob(I).GetString;
          stDate: Value := DateToStr(ResultSet.GetDate(I));
          stTime: Value := TimeToStr(ResultSet.GetTime(I));
          stTimeStamp: Value := DateTimeToStr(ResultSet.GetTimeStamp(I));
          stAsciiStream, stUnicodeStream: Value := ResultSet.GetBlob(I).GetString;
        else
          Value := ResultSet.GetString(I);
        end;
        if ResultSet.IsNull(I) then
          Value := '';
        sgResults.Cells[I - 1, sgResults.RowCount - 1] := Value;
      end;
    end;
  end;
end;

procedure TfrmDBCDemo.sgResultsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if LastRowNr = AROw then
    Exit;//Exit when the row nr has not changed
  if Assigned(ResultSet) then
//Position the resultset to the selected row
    ResultSet.MoveAbsolute(ARow);
  InsertState := False;
  LastRowNr := AROw;
end;

type
  THackGrid = class(TStringGrid);

procedure TfrmDBCDemo.Button3Click(Sender: TObject);
begin
  if Assigned(ResultSet) then
  begin
    if not InsertState then
    begin
//Position the resultset to the selected row
      ResultSet.MoveAbsolute(sgResults.Row);
      LastRowNr := -1;
//Delete the selected row
      ResultSet.DeleteRow;
    end;
//Delete the selected row from the stringgrid
    THackGrid(sgResults).DeleteRow(sgResults.Row);
  end;
end;

{**
This method writes the modified data into the resultset.
Resultset also supports Updating field by name, for example UpdateStringByName
}
procedure TfrmDBCDemo.sgResultsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  if Value = '' then
    ResultSet.UpdateNull(ACol + 1)
  else
    case ResultSet.GetMetadata.GetColumnType(ACol + 1) of
      stBoolean: ResultSet.UpdateBoolean(ACol + 1, UpperCase(Copy(Value, 1, 1)) = 'T');
      stByte: ResultSet.UpdateByte(ACol + 1, StrToInt(Value));
      stShort: ResultSet.UpdateShort(ACol + 1, StrToInt(Value));
      stInteger: ResultSet.UpdateInt(ACol + 1, StrToInt(Value));
      stLong: ResultSet.UpdateLong(ACol + 1, StrToInt(Value));
      stFloat: ResultSet.UpdateFloat(ACol + 1, StrToFloat(Value));
      stDouble: ResultSet.UpdateFloat(ACol + 1, StrToFloat(Value));
      stBigDecimal: ResultSet.UpdateFloat(ACol + 1, StrToFloat(Value));
      stBytes, stBinaryStream: ;
      stDate: ResultSet.UpdateDate(ACol + 1, StrToDate(Value));
      stTime: ResultSet.UpdateTime(ACol + 1, StrToTime(Value));
      stTimeStamp: ResultSet.UpdateTimeStamp(ACol + 1, StrToDateTime(Value));
      stAsciiStream, stUnicodeStream: ResultSet.UpdateString(ACol + 1, Value);
    else
      ResultSet.UpdateString(ACOl + 1, Value);
    end;
end;

{**
Post the updates. If we are in insert status then insert otherwise update.
}
procedure TfrmDBCDemo.Button4Click(Sender: TObject);
begin
  if Assigned(ResultSet) then
//Update the selected row
    if InsertState then
      ResultSet.InsertRow
    else
      ResultSet.UpdateRow;
end;

{**
Prepare for insert
}
procedure TfrmDBCDemo.Button5Click(Sender: TObject);
begin
  sgResults.RowCount := sgResults.RowCount + 1;
  sgResults.Row := sgResults.RowCount - 1;
  sgResults.Col := 0;
  ResultSet.MoveToInsertRow;
  LastRowNr := sgResults.Row;
  InsertState := True;
end;

end.
