{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZDbcPostgreSql;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  ZCompatibility, Classes, SysUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainPostgreSqlDriver, ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser;

type

  {** Implements PostgreSQL Database Driver. }
  TZPostgreSQLDriver = class(TZAbstractDriver)
  private
    FPostgreSQL73PlainDriver: IZPostgreSQLPlainDriver;
    FPostgreSQL74PlainDriver: IZPostgreSQLPlainDriver;
    FPostgreSQL8xPlainDriver: IZPostgreSQLPlainDriver;
  protected
    function GetPlainDriver(Url: string): IZPostgreSQLPlainDriver;
  public
    constructor Create;
    function Connect(Url: string; Info: TStrings): IZConnection; override;

    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Defines a PostgreSQL specific connection. }
  IZPostgreSQLConnection = interface(IZConnection)
    ['{8E62EA93-5A49-4F20-928A-0EA44ABCE5DB}']

    function IsOidAsBlob: Boolean;

    function GetTypeNameByOid(Id: Oid): string;
    function GetPlainDriver: IZPostgreSQLPlainDriver;
    function GetConnectionHandle: PZPostgreSQLConnect;
    function GetServerMajorVersion: Integer;
    function GetServerMinorVersion: Integer;
  end;

  {** Implements PostgreSQL Database Connection. }
  TZPostgreSQLConnection = class(TZAbstractConnection, IZPostgreSQLConnection)
  private
    FHandle: PZPostgreSQLConnect;
    FBeginRequired: Boolean;
    FTypeList: TStrings;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FOidAsBlob: Boolean;
    FClientCodePage: string;
    FServerMajorVersion: Integer;
    FServerMinorVersion: Integer;
  protected
    function BuildConnectStr: string;
    procedure StartTransactionSupport;
    procedure LoadServerVersion;
  public
    constructor Create(Driver: IZDriver; Url: string;
      PlainDriver: IZPostgreSQLPlainDriver; HostName: string; Port: Integer;
      Database: string; User: string; Password: string; Info: TStrings);
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(SQL: string; Info: TStrings):
      IZPreparedStatement; override;

    function CreateSequence(Sequence: string; BlockSize: Integer):
      IZSequence; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    function IsOidAsBlob: Boolean;

    function GetTypeNameByOid(Id: Oid): string;
    function GetPlainDriver: IZPostgreSQLPlainDriver;
    function GetConnectionHandle: PZPostgreSQLConnect;

    function GetServerMajorVersion: Integer;
    function GetServerMinorVersion: Integer;
  end;

  {** Implements a Interbase 6 sequence. }
  TZPostgreSQLSequence = class(TZAbstractSequence)
  public
    function GetCurrentValue: Int64; override;
    function GetNextValue: Int64; override;
  end;


var
  {** The common driver manager object. }
  PostgreSQLDriver: IZDriver;

implementation

uses
  ZMessages, ZSysUtils, ZDbcUtils, ZDbcPostgreSqlStatement,
  ZDbcPostgreSqlUtils, ZDbcPostgreSqlMetadata, ZPostgreSqlToken,
  ZPostgreSqlAnalyser;

{ TZPostgreSQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZPostgreSQLDriver.Create;
begin
  FPostgreSQL73PlainDriver := TZPostgreSQL73PlainDriver.Create;
  FPostgreSQL74PlainDriver := TZPostgreSQL74PlainDriver.Create;
  FPostgreSQL8xPlainDriver := TZPostgreSQL8xPlainDriver.Create;
end;

{**
  Attempts to make a database connection to the given URL.
  The driver should return "null" if it realizes it is the wrong kind
  of driver to connect to the given URL.  This will be common, as when
  the JDBC driver manager is asked to connect to a given URL it passes
  the URL to each loaded driver in turn.

  <P>The driver should raise a SQLException if it is the right
  driver to connect to the given URL, but has trouble connecting to
  the database.

  <P>The java.util.Properties argument can be used to passed arbitrary
  string tag/value pairs as connection arguments.
  Normally at least "user" and "password" properties should be
  included in the Properties.

  @param url the URL of the database to which to connect
  @param info a list of arbitrary string tag/value pairs as
    connection arguments. Normally at least a "user" and
    "password" property should be included.
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
function TZPostgreSQLDriver.Connect(Url: string; Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  PlainDriver: IZPostgreSQLPlainDriver;
begin
  TempInfo := TStringList.Create;
  try
    PlainDriver := GetPlainDriver(Url);
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, TempInfo);
    Result := TZPostgreSQLConnection.Create(Self, Url, PlainDriver, HostName,
      Port, Database, UserName, Password, TempInfo);
  finally
    TempInfo.Free;
  end;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZPostgreSQLDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZPostgreSQLDriver.GetMinorVersion: Integer;
begin
  Result := 3;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZPostgreSQLDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZPostgreSQLTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZPostgreSQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZPostgreSQLStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Get a name of the supported subprotocol.
  For example: postgresql74 or postgresql8x
}
function TZPostgreSQLDriver.GetSupportedProtocols: TStringDynArray;
begin
  SetLength(Result, 4);
  Result[0] := 'postgresql';
  Result[1] := FPostgreSQL73PlainDriver.GetProtocol;
  Result[2] := FPostgreSQL74PlainDriver.GetProtocol;
  Result[3] := FPostgreSQL8xPlainDriver.GetProtocol;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected protocol.
}
function TZPostgreSQLDriver.GetPlainDriver(Url: string): IZPostgreSQLPlainDriver;
var
  Protocol: string;
begin
  Protocol := ResolveConnectionProtocol(Url, GetSupportedProtocols);

  if Protocol = FPostgreSQL73PlainDriver.GetProtocol then
    Result := FPostgreSQL73PlainDriver
  else if Protocol = FPostgreSQL74PlainDriver.GetProtocol then
    Result := FPostgreSQL74PlainDriver
  else if Protocol = FPostgreSQL8xPlainDriver.GetProtocol then
    Result := FPostgreSQL8xPlainDriver
  else Result := FPostgreSQL8xPlainDriver;
  Result.Initialize;
end;

{ TZPostgreSQLConnection }

{**
  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param PlainDriver a PostgreSQL plain driver.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZPostgreSQLConnection.Create(Driver: IZDriver; Url: string;
  PlainDriver: IZPostgreSQLPlainDriver; HostName: string; Port: Integer;
  Database, User, Password: string; Info: TStrings);
begin
  inherited Create(Driver, Url, HostName, Port, Database, User, Password, Info,
    TZPostgreSQLDatabaseMetadata.Create(Self, Url, Info));

  { Sets a default PostgreSQL port }
  if Self.Port = 0 then Self.Port := 5432;

  { Define connect options. }
  if Info.Values['beginreq'] <> '' then
    FBeginRequired := StrToBoolEx(Info.Values['beginreq'])
  else FBeginRequired := True;

  FPlainDriver := PlainDriver;
  TransactIsolationLevel := tiNone;

  { Processes connection properties. }
  if Info.Values['oidasblob'] <> '' then
    FOidAsBlob := StrToBoolEx(Info.Values['oidasblob'])
  else FOidAsBlob := False;
  FClientCodePage := Trim(Info.Values['codepage']);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLConnection.Destroy;
begin
  if FTypeList <> nil then
    FTypeList.Free;
  inherited Destroy;
end;

{**
  Builds a connection string for PostgreSQL.
  @return a built connection string.
}
function TZPostgreSQLConnection.BuildConnectStr: string;
var
  ConnectTimeout: Integer;
begin
  if IsIpAddr(HostName) then
  begin
    Result := Format('hostaddr=%s port=%d dbname=%s user=%s password=%s',
     [HostName, Port, Database, User, Password]);
  end
  else
  begin
    Result := Format('host=%s port=%d dbname=%s user=%s password=%s',
     [HostName, Port, Database, User, Password]);
  end;

  { Sets a connection timeout. }
  ConnectTimeout := StrToIntDef(Info.Values['timeout'], -1);
  if ConnectTimeout >= 0 then
    Result := Result + Format(' connect_timeout=%d', [ConnectTimeout]);
end;

{**
  Checks is oid should be treated as Large Object.
  @return <code>True</code> if oid should represent a Large Object. 
}
function TZPostgreSQLConnection.IsOidAsBlob: Boolean;
begin
  Result := FOidAsBlob;
end;

{**
  Starts a transaction support.
}
procedure TZPostgreSQLConnection.StartTransactionSupport;
var
  QueryHandle: PZPostgreSQLResult;
  SQL: PChar;
begin
  if TransactIsolationLevel <> tiNone then
  begin
    if FBeginRequired then
    begin
      SQL := 'BEGIN';
      QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
      FPlainDriver.Clear(QueryHandle);
      CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end;

    if TransactIsolationLevel = tiReadCommitted then
    begin
      SQL := 'SET TRANSACTION ISOLATION LEVEL READ COMMITTED';
      QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
      FPlainDriver.Clear(QueryHandle);
      CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end
    else if TransactIsolationLevel = tiSerializable then
    begin
      SQL := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
      QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
      FPlainDriver.Clear(QueryHandle);
      CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end
    else
      raise EZSQLException.Create(SIsolationIsNotSupported);
  end;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZPostgreSQLConnection.Open;
var
  LogMessage: string;
  QueryHandle: PZPostgreSQLResult;
  SQL: PChar;
begin
  if not Closed then Exit;

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);

  { Connect to PostgreSQL database. }
  FHandle := FPlainDriver.ConnectDatabase(PChar(BuildConnectStr));
  if FPlainDriver.GetStatus(FHandle) = CONNECTION_BAD then
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcConnect, LogMessage)
  else
    DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol, LogMessage);

  { Sets a client codepage. }
  if FClientCodePage <> '' then
  begin
    SQL := PChar(Format('SET CLIENT_ENCODING = ''%s''', [FClientCodePage]));
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    FPlainDriver.Clear(QueryHandle);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  end;

  { Turn on transaction mode }
  StartTransactionSupport;
  { Setup notification mechanism }
//  PQsetNoticeProcessor(FHandle, NoticeProc, Self);

  inherited Open;
end;

{**
  Creates a <code>Statement</code> object for sending
  SQL statements to the database.
  SQL statements without parameters are normally
  executed using Statement objects. If the same SQL statement
  is executed many times, it is more efficient to use a
  <code>PreparedStatement</code> object.
  <P>
  Result sets created using the returned <code>Statement</code>
  object will by default have forward-only type and read-only concurrency.

  @param Info a statement parameters.
  @return a new Statement object
}
function TZPostgreSQLConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then Open;
  Result := TZPostgreSQLStatement.Create(FPlainDriver, Self, Info, FHandle);
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  A SQL statement with or without IN parameters can be
  pre-compiled and stored in a PreparedStatement object. This
  object can then be used to efficiently execute this statement
  multiple times.

  <P><B>Note:</B> This method is optimized for handling
  parametric SQL statements that benefit from precompilation. If
  the driver supports precompilation,
  the method <code>prepareStatement</code> will send
  the statement to the database for precompilation. Some drivers
  may not support precompilation. In this case, the statement may
  not be sent to the database until the <code>PreparedStatement</code> is
  executed.  This has no direct effect on users; however, it does
  affect which method throws certain SQLExceptions.

  Result sets created using the returned PreparedStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZPostgreSQLConnection.CreatePreparedStatement(
  SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZPostgreSQLPreparedStatement.Create(FPlainDriver,
    Self, SQL, Info, FHandle);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.Commit;
var
  QueryHandle: PZPostgreSQLResult;
  SQL: PChar;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'COMMIT';
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    FPlainDriver.Clear(QueryHandle);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

    StartTransactionSupport;
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.Rollback;
var
  QueryHandle: PZPostgreSQLResult;
  SQL: PChar;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'ROLLBACK';
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    FPlainDriver.Clear(QueryHandle);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

    StartTransactionSupport;
  end;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZPostgreSQLConnection.Close;
var
  LogMessage: string;
begin
  if not Closed then
  begin
    FPlainDriver.Finish(FHandle);
    FHandle := nil;
    LogMessage := Format('DISCONNECT FROM "%s"', [Database]);
    DriverManager.LogMessage(lcDisconnect, FPlainDriver.GetProtocol, LogMessage);
  end;
  inherited Close;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZPostgreSQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  QueryHandle: PZPostgreSQLResult;
  SQL: PChar;
begin
  if not (Level in [tiNone, tiReadCommitted, tiSerializable]) then
    raise EZSQLException.Create(SIsolationIsNotSupported);

  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'END';
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    FPlainDriver.Clear(QueryHandle);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  end;

  inherited SetTransactionIsolation(Level);

  if not Closed then
    StartTransactionSupport;
end;

{**
  Gets a reference to PostgreSQL connection handle.
  @return a reference to PostgreSQL connection handle.
}
function TZPostgreSQLConnection.GetConnectionHandle: PZPostgreSQLConnect;
begin
  Result := FHandle;
end;

{**
  Gets a PostgreSQL plain driver interface.
  @return a PostgreSQL plain driver interface.
}
function TZPostgreSQLConnection.GetPlainDriver: IZPostgreSQLPlainDriver;
begin
  Result := FPlainDriver;
end;

{**
  Gets a type name by it's oid number.
  @param Id a type oid number.
  @return a type name or empty string if there was no such type found.
}
function TZPostgreSQLConnection.GetTypeNameByOid(Id: Oid): string;
var
  I: Integer;
  QueryHandle: PZPostgreSQLResult;
  SQL: PChar;
  TypeCode: Integer;
  TypeName: string;
begin
  if Closed then Open;

  { Fill the list with existed types }
  if not Assigned(FTypeList) then
  begin
    SQL := 'SELECT oid, typname FROM pg_type WHERE oid < 10000';

    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    CheckPostgreSQLError(Self, FPlainDriver, FHandle, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

    FTypeList := TStringList.Create;
    for I := 0 to FPlainDriver.GetRowCount(QueryHandle)-1 do
    begin
      TypeCode := StrToIntDef(StrPas(
        FPlainDriver.GetValue(QueryHandle, I, 0)), 0);
      TypeName := StrPas(FPlainDriver.GetValue(QueryHandle, I, 1));

      FTypeList.AddObject(TypeName, TObject(TypeCode));
    end;
    FPlainDriver.Clear(QueryHandle);
  end;

  I := FTypeList.IndexOfObject(TObject(Id));
  if I >= 0 then
    Result := FTypeList[I]
  else Result := '';
end;

{**
  Gets a server major version.
  @return a server major version number.
}
function TZPostgreSQLConnection.GetServerMajorVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerMajorVersion;
end;

{**
  Gets a server minor version.
  @return a server minor version number.
}
function TZPostgreSQLConnection.GetServerMinorVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerMinorVersion;
end;

{**
  Loads a server major and minor version numbers.
}
procedure TZPostgreSQLConnection.LoadServerVersion;
var
  Temp: string;
  List: TStrings;
  QueryHandle: PZPostgreSQLResult;
  SQL: PChar;
begin
  if Closed then Open;

  SQL := 'SELECT version()';
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
  CheckPostgreSQLError(Self, FPlainDriver, FHandle, lcExecute, SQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  Temp := FPlainDriver.GetValue(QueryHandle, 0, 0);
  FPlainDriver.Clear(QueryHandle);

  List := TStringList.Create;
  try
    { Splits string by space }
    PutSplitString(List, Temp, ' ');
    { first - PostgreSQL, second X.Y.Z}
    Temp := List.Strings[1];
    { Splits string by dot }
    PutSplitString(List, Temp, '.');

    FServerMajorVersion := StrToIntDef(List.Strings[0], 0);
    if List.Count > 1 then
      FServerMinorVersion := GetMinorVersion(List.Strings[1])
    else
      FServerMinorVersion := 0;
  finally
    List.Free;
  end;
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZPostgreSQLConnection.CreateSequence(Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := TZPostgreSQLSequence.Create(Self, Sequence, BlockSize);
end;

{ TZInterbase6Sequence }

{**
  Gets the current unique key generated by this sequence.
  @param the last generated unique key.
}
function TZPostgreSQLSequence.GetCurrentValue: Int64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery(
    Format('SELECT CURRVAL(''%s'')', [Name]));
  if ResultSet.Next then
    Result := ResultSet.GetLong(1)
  else
    Result := inherited GetCurrentValue;
  ResultSet.Close;
  Statement.Close;
end;

{**
  Gets the next unique key generated by this sequence.
  @param the next generated unique key.
}
function TZPostgreSQLSequence.GetNextValue: Int64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery(
    Format('SELECT NEXTVAL(''%s'')', [Name]));
  if ResultSet.Next then
    Result := ResultSet.GetLong(1)
  else
    Result := inherited GetNextValue;
  ResultSet.Close;
  Statement.Close;
end;

initialization
  PostgreSQLDriver := TZPostgreSQLDriver.Create;
  DriverManager.RegisterDriver(PostgreSQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(PostgreSQLDriver);
  PostgreSQLDriver := nil;
end.

