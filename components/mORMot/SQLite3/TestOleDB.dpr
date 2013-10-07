program TestOleDB;

{$INCLUDE Synopse.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  SynCommons,
{$ifndef DELPHI5OROLDER}
  SQLite3Commons,
{$endif}
  SynDB,
  SynDBOracle,
  SynOleDB;

var Props: TOleDBConnectionProperties;

procedure TestISQLDBRowsColumns;
var I: ISQLDBRows;
begin
  SynDBLog.Enter;
  I := Props.Execute('select * from Sales.Customer where AccountNumber like ?',['AW000001%']);
  while I.Step do
    assert(Copy(I['AccountNumber'],1,8)='AW000001');
end;

{$ifndef DELPHI5OROLDER}
procedure TestISQLDBRowsVariant;
var Customer: Variant;
begin
  SynDBLog.Enter;
  with Props.Execute('select * from Sales.Customer where AccountNumber like ?',
    ['AW000001%'],@Customer) do
    while Step do
      assert(Copy(Customer.AccountNumber,1,8)='AW000001');
end;
{$endif}

var Conn: TSQLDBConnection;
    Query: TSQLDBStatement;
    F: TFileStream;
begin
{$ifndef DELPHI5OROLDER}
  SynDBLog := TSQLLog;
{$endif}
  with SynDBLog.Family do begin
    Level := LOG_VERBOSE;
    AutoFlushTimeOut := 10;
    HighResolutionTimeStamp := true;
  end;
  Props := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
  try
    //Props.ConnectionStringDialogExecute;
    Conn := Props.NewConnection;
    try
      Conn.Connect; // optional
      Query := Conn.NewStatement;
      try
        Query.Execute('select * from Sales.Customer where AccountNumber like ?',true,['AW000001%']);
        F := TFileStream.Create(ChangeFileExt(paramstr(0),'.json'),fmCreate);
        try
          Query.FetchAllToJSON(F,false);
        finally
          F.Free;
        end;
      finally
        Query.Free;
      end;
      // Writeln(#13#10'Press [Enter]'); readln;
    finally
      Conn.Free;
    end;
    Props.ThreadSafeConnection.Connect; // don't benchmark ISQLDBRows connection
    TestISQLDBRowsColumns;
{$ifndef DELPHI5OROLDER}
    TestISQLDBRowsVariant;
{$endif}
  finally
    Props.Free;
  end;
end.
