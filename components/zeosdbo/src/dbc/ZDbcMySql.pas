{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySql;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  ZCompatibility, Classes, SysUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainMySqlDriver, ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser;

type

  {** Implements MySQL Database Driver. }
  TZMySQLDriver = class(TZAbstractDriver)
  private
    FMySQL320PlainDriver: IZMySQLPlainDriver;
    FMySQL323PlainDriver: IZMySQLPlainDriver;
    FMySQL40PlainDriver: IZMySQLPlainDriver;
    FMySQL41PlainDriver: IZMySQLPlainDriver;
  protected
    function GetPlainDriver(Url: string): IZMySQLPlainDriver;
  public
    constructor Create;
    function Connect(Url: string; Info: TStrings): IZConnection; override;

    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a MYSQL specific connection interface. }
  IZMySQLConnection = interface (IZConnection)
    ['{68E33DD3-4CDC-4BFC-8A28-E9F2EE94E457}']

    function GetPlainDriver: IZMySQLPlainDriver;
    function GetConnectionHandle: PZMySQLConnect;
  end;

  {** Implements MySQL Database Connection. }
  TZMySQLConnection = class(TZAbstractConnection, IZMySQLConnection)
  private
    FCatalog: string;
    FPlainDriver: IZMySQLPlainDriver;
    FHandle: PZMySQLConnect;
    FClientCodePage: string;
  public
    constructor Create(Driver: IZDriver; Url: string;
      PlainDriver: IZMySQLPlainDriver; HostName: string; Port: Integer;
      Database: string; User: string; Password: string; Info: TStrings);
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(SQL: string; Info: TStrings):
      IZPreparedStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;
    function Ping: Boolean; override;
    function GetAffectedRowsFromLastPost: Int64; override;

    procedure SetCatalog(Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    procedure SetAutoCommit(AutoCommit: Boolean); override;

    function GetPlainDriver: IZMySQLPlainDriver;
    function GetConnectionHandle: PZMySQLConnect;
  end;

var
  {** The common driver manager object. }
  MySQLDriver: IZDriver;

implementation

uses
  ZMessages, ZSysUtils, ZDbcUtils, ZDbcMySqlStatement, ZMySqlToken,
  ZDbcMySqlUtils, ZDbcMySqlMetadata, ZMySqlAnalyser;

{ TZMySQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZMySQLDriver.Create;
begin
  FMySQL320PlainDriver := TZMySQL320PlainDriver.Create;
  FMySQL323PlainDriver := TZMySQL323PlainDriver.Create;
  FMySQL40PlainDriver := TZMySQL40PlainDriver.Create;
  FMySQL41PlainDriver := TZMySQL41PlainDriver.Create;
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
function TZMySQLDriver.Connect(Url: string; Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  PlainDriver: IZMySQLPlainDriver;
begin
  TempInfo := TStringList.Create;
  try
    PlainDriver := GetPlainDriver(Url);
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, TempInfo);
    Result := TZMySQLConnection.Create(Self, Url, PlainDriver, HostName, Port,
      Database, UserName, Password, TempInfo);
  finally
    TempInfo.Free;
  end;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZMySQLDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZMySQLDriver.GetMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZMySQLDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZMySQLTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZMySQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZMySQLStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Get a name of the supported subprotocol.
  For example: mysql, oracle8 or postgresql72
}
function TZMySQLDriver.GetSupportedProtocols: TStringDynArray;
begin
  SetLength(Result, 5);
  Result[0] := 'mysql';
  Result[1] := FMySQL320PlainDriver.GetProtocol;
  Result[2] := FMySQL323PlainDriver.GetProtocol;
  Result[3] := FMySQL40PlainDriver.GetProtocol;
  Result[4] := FMySQL41PlainDriver.GetProtocol;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected protocol.
}
function TZMySQLDriver.GetPlainDriver(Url: string): IZMySQLPlainDriver;
var
  Protocol: string;
begin
  Protocol := ResolveConnectionProtocol(Url, GetSupportedProtocols);
  if Protocol = FMySQL320PlainDriver.GetProtocol then
    Result := FMySQL320PlainDriver
  else if Protocol = FMySQL323PlainDriver.GetProtocol then
    Result := FMySQL323PlainDriver
  else if Protocol = FMySQL40PlainDriver.GetProtocol then
    Result := FMySQL40PlainDriver
  else if Protocol = FMySQL41PlainDriver.GetProtocol then
    Result := FMySQL41PlainDriver
  else Result := FMySQL41PlainDriver;
  Result.Initialize;
end;

{ TZMySQLConnection }

{**
  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param PlainDriver a MySQL plain driver.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZMySQLConnection.Create(Driver: IZDriver; Url: string;
  PlainDriver: IZMySQLPlainDriver; HostName: string; Port: Integer;
  Database, User, Password: string; Info: TStrings);
begin
  inherited Create(Driver, Url, HostName, Port, Database, User, Password, Info,
    TZMySQLDatabaseMetadata.Create(Self, Url, Info));

  { Sets a default properties }
  FPlainDriver := PlainDriver;
  if Self.Port = 0 then Self.Port := MYSQL_PORT;
  AutoCommit := True;
  TransactIsolationLevel := tiNone;

  { Processes connection properties. }
  FClientCodePage := Trim(Info.Values['codepage']);

  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLConnection.Destroy;
begin
  inherited Destroy;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZMySQLConnection.Open;
var
  LogMessage: string;
  OldLevel: TZTransactIsolationLevel;
  OldAutoCommit: Boolean;
  ConnectTimeout: Integer;
  SQL: PChar;
begin
  if not Closed then Exit;

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);

  FPlainDriver.Init(FHandle);
  try
    { Sets a default port number. }
    if Port = 0 then Port := MYSQL_PORT;
    { Turn on compression protocol. }
    if StrToBoolEx(Info.Values['compress']) then
      FPlainDriver.SetOptions(FHandle, MYSQL_OPT_COMPRESS, nil);
    { Sets connection timeout. }
    ConnectTimeout := StrToIntDef(Info.Values['timeout'], 0);
    if ConnectTimeout >= 0 then
    begin
      FPlainDriver.SetOptions(FHandle, MYSQL_OPT_CONNECT_TIMEOUT,
        PChar(@ConnectTimeout));
    end;
    { Connect to MySQL database. }
    if StrToBoolEx(Info.Values['dbless']) then
    begin
      if FPlainDriver.RealConnect(FHandle, PChar(HostName), PChar(User),
        PChar(Password), nil, Port, nil, 0) = nil then
      begin
        CheckMySQLError(FPlainDriver, FHandle, lcConnect, LogMessage);
        DriverManager.LogError(lcConnect, FPlainDriver.GetProtocol, LogMessage,
          0, SUnknownError);
        raise EZSQLException.Create(SCanNotConnectToServer);
      end;
      DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol, LogMessage);
    end
    else
    begin
      if FPlainDriver.RealConnect(FHandle, PChar(HostName), PChar(User),
        PChar(Password), PChar(Database), Port, nil,
        _CLIENT_CONNECT_WITH_DB) = nil then
      begin
        CheckMySQLError(FPlainDriver, FHandle, lcConnect, LogMessage);
        DriverManager.LogError(lcConnect, FPlainDriver.GetProtocol, LogMessage,
          0, SUnknownError);
        raise EZSQLException.Create(SCanNotConnectToServer);
      end;
      DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol, LogMessage);
    end;

    { Sets a client codepage. }
    if FClientCodePage <> '' then
    begin
      SQL := PChar(Format('SET CHARACTER SET %s', [FClientCodePage]));
      FPlainDriver.ExecQuery(FHandle, SQL);
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end;

    { Sets transaction isolation level. }
    OldLevel := TransactIsolationLevel;
    TransactIsolationLevel := tiNone;
    SetTransactionIsolation(OldLevel);

    { Sets an auto commit mode. }
    OldAutoCommit := AutoCommit;
    AutoCommit := True;
    SetAutoCommit(OldAutoCommit);
  except
    FPlainDriver.Close(FHandle);
    FPlainDriver.Despose(FHandle);
    FHandle := nil;
    raise;
  end;

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
function TZMySQLConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then Open;
  Result := TZMySQLStatement.Create(FPlainDriver, Self, Info, FHandle);
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
function TZMySQLConnection.CreatePreparedStatement(SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZMySQLPreparedStatement.Create(FPlainDriver, Self, SQL,
    Info, FHandle);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Commit;
var
  SQL: PChar;
begin
  if (TransactIsolationLevel <> tiNone) and (AutoCommit <> True)
    and not Closed then
  begin
    SQL := 'COMMIT';
    FPlainDriver.ExecQuery(FHandle, SQL);
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Rollback;
var
  SQL: PChar;
begin
  if (TransactIsolationLevel <> tiNone) and (AutoCommit <> True)
    and not Closed then
  begin
    SQL := 'ROLLBACK';
    FPlainDriver.ExecQuery(FHandle, SQL);
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
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
procedure TZMySQLConnection.Close;
var
  LogMessage: string;
begin
  if not Closed then
  begin
    FPlainDriver.Close(FHandle);
    FPlainDriver.Despose(FHandle);
    FHandle := nil;
    LogMessage := Format('DISCONNECT FROM "%s"', [Database]);
    DriverManager.LogMessage(lcDisconnect, FPlainDriver.GetProtocol, LogMessage);
  end;
  inherited Close;
end;

{**
  Returns true if a network ping to the database server succeeds.
  @return true if a network ping to the database server succeeds
}
function TZMySQLConnection.Ping: Boolean;
begin
  if Closed then Result := false
  else Result := FPlainDriver.Ping(FHandle) = 0;
end;

{**
  Gets the count of rows which were affected by the last INSERT, DELETE or UPDATE
  @return Int64
}
function TZMySQLConnection.GetAffectedRowsFromLastPost: Int64;
begin
  Result := FPlainDriver.GetAffectedRows(FHandle);
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZMySQLConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZMySQLConnection.SetCatalog(Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZMySQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  SQL: PChar;
begin
  if TransactIsolationLevel <> Level then
  begin
    inherited SetTransactionIsolation(Level);

    if not Closed then
    begin
      case TransactIsolationLevel of
        tiNone, tiReadUncommitted:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED';
            FPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiReadCommitted:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED';
            FPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiRepeatableRead:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ';
            FPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiSerializable:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE';
            FPlainDriver.ExecQuery(FHandle, SQL);
          end;
        else
          SQL := '';
      end;
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
      if SQL <> '' then
        DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end;
  end;
end;

{**
  Sets this connection's auto-commit mode.
  If a connection is in auto-commit mode, then all its SQL
  statements will be executed and committed as individual
  transactions.  Otherwise, its SQL statements are grouped into
  transactions that are terminated by a call to either
  the method <code>commit</code> or the method <code>rollback</code>.
  By default, new connections are in auto-commit mode.

  The commit occurs when the statement completes or the next
  execute occurs, whichever comes first. In the case of
  statements returning a ResultSet, the statement completes when
  the last row of the ResultSet has been retrieved or the
  ResultSet has been closed. In advanced cases, a single
  statement may return multiple results as well as output
  parameter values. In these cases the commit occurs when all results and
  output parameter values have been retrieved.

  @param autoCommit true enables auto-commit; false disables auto-commit.
}
procedure TZMySQLConnection.SetAutoCommit(AutoCommit: Boolean);
var
  SQL: PChar;
begin
  if AutoCommit <> Self.AutoCommit then
  begin
    inherited SetAutoCommit(AutoCommit);

    if not Closed then
    begin
      if AutoCommit then
        SQL := 'SET AUTOCOMMIT=1'
      else SQL := 'SET AUTOCOMMIT=0';
      FPlainDriver.ExecQuery(FHandle, SQL);
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end;
  end;
end;

{**
  Gets a reference to MySQL connection handle.
  @return a reference to MySQL connection handle.
}
function TZMySQLConnection.GetConnectionHandle: PZMySQLConnect;
begin
  Result := FHandle;
end;

{**
  Gets a MySQL plain driver interface.
  @return a MySQL plain driver interface.
}
function TZMySQLConnection.GetPlainDriver: IZMySQLPlainDriver;
begin
  Result := FPlainDriver;
end;

initialization
  MySQLDriver := TZMySQLDriver.Create;
  DriverManager.RegisterDriver(MySQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(MySQLDriver);
  MySQLDriver := nil;
end.

