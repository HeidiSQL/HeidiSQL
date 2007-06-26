{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcSqLite;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  ZCompatibility, Classes, SysUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainSqLiteDriver, ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser;

type

  {** Implements SQLite Database Driver. }
  TZSQLiteDriver = class(TZAbstractDriver)
  private
    FSQLite28PlainDriver: IZSQLitePlainDriver;
    FSQLite3PlainDriver: IZSQLitePlainDriver;
  protected
    function GetPlainDriver(const Url: string): IZSQLitePlainDriver;
  public
    constructor Create;
    function Connect(const Url: string; Info: TStrings): IZConnection; override;

    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a SQLite specific connection interface. }
  IZSQLiteConnection = interface (IZConnection)
    ['{A4B797A9-7CF7-4DE9-A5BB-693DD32D07D2}']

    function GetPlainDriver: IZSQLitePlainDriver;
    function GetConnectionHandle: Psqlite;
  end;

  {** Implements SQLite Database Connection. }
  TZSQLiteConnection = class(TZAbstractConnection, IZSQLiteConnection)
  private
    FCatalog: string;
    FPlainDriver: IZSQLitePlainDriver;
    FHandle: Psqlite;

  protected
    procedure StartTransactionSupport;

  public
    constructor Create(Driver: IZDriver; const Url: string;
      PlainDriver: IZSQLitePlainDriver; const HostName: string; Port: Integer;
      const Database: string; const User: string; const Password: string; Info: TStrings);
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    function GetPlainDriver: IZSQLitePlainDriver;
    function GetConnectionHandle: Psqlite;
  end;

var
  {** The common driver manager object. }
  SQLiteDriver: IZDriver;

implementation

uses
  ZMessages, ZSysUtils, ZDbcUtils, ZDbcSqLiteStatement, ZSqLiteToken,
  ZDbcSqLiteUtils, ZDbcSqLiteMetadata, ZSqLiteAnalyser;

{ TZSQLiteDriver }

{**
  Constructs this object with default properties.
}
constructor TZSQLiteDriver.Create;
begin
  FSQLite28PlainDriver := TZSQLite28PlainDriver.Create;
  FSQLite3PlainDriver := TZSQLite3PlainDriver.Create;
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
function TZSQLiteDriver.Connect(const Url: string; Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  PlainDriver: IZSQLitePlainDriver;
begin
  TempInfo := TStringList.Create;
  try
    PlainDriver := GetPlainDriver(Url);
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, TempInfo);
    Result := TZSQLiteConnection.Create(Self, Url, PlainDriver, HostName, Port,
      Database, UserName, Password, TempInfo);
  finally
    TempInfo.Free;
  end;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZSQLiteDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZSQLiteDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZSQLiteDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZSQLiteTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZSQLiteDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZSQLiteStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Get a name of the supported subprotocol.
  For example: mysql, oracle8 or postgresql72
}
function TZSQLiteDriver.GetSupportedProtocols: TStringDynArray;
begin
  SetLength(Result, 3);
  Result[0] := 'sqlite';
  Result[1] := FSQLite28PlainDriver.GetProtocol;
  Result[2] := FSQLite3PlainDriver.GetProtocol;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected protocol.
}
function TZSQLiteDriver.GetPlainDriver(const Url: string): IZSQLitePlainDriver;
var
  Protocol: string;
begin
  Protocol := ResolveConnectionProtocol(Url, GetSupportedProtocols);
  if Protocol = FSQLite28PlainDriver.GetProtocol then
    Result := FSQLite28PlainDriver
  else if Protocol = FSQLite3PlainDriver.GetProtocol then
    Result := FSQLite3PlainDriver
  else Result := FSQLite28PlainDriver;
  Result.Initialize;
end;

{ TZSQLiteConnection }

{**
  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param PlainDriver a SQLite plain driver.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZSQLiteConnection.Create(Driver: IZDriver; const Url: string;
  PlainDriver: IZSQLitePlainDriver; const HostName: string; Port: Integer;
  const Database, User, Password: string; Info: TStrings);
begin
  inherited Create(Driver, Url, HostName, Port, Database, User, Password, Info,
    TZSQLiteDatabaseMetadata.Create(Self, Url, Info));

  { Sets a default properties }
  FPlainDriver := PlainDriver;
  AutoCommit := True;
  TransactIsolationLevel := tiNone;

  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSQLiteConnection.Destroy;
begin
  inherited Destroy;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZSQLiteConnection.Open;
var
  ErrorCode: Integer;
  ErrorMessage: PChar;
  LogMessage: string;
  SQL: string;
begin
  if not Closed then Exit;

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);

  FHandle := FPlainDriver.Open(PChar(Database), 0, ErrorMessage);
  if FHandle = nil then
  begin
    CheckSQLiteError(FPlainDriver, SQLITE_ERROR, ErrorMessage,
      lcConnect, LogMessage);
  end;
  DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol, LogMessage);

  try
    SQL := 'PRAGMA show_datatypes = ON';
    ErrorCode := FPlainDriver.Execute(FHandle, PChar(SQL),
      nil, nil, ErrorMessage);
    CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
{
    SQL := 'PRAGMA empty_result_callbacks = ON';
    ErrorCode := FPlainDriver.Execute(FHandle, PChar(SQL),
      nil, nil, ErrorMessage);
    CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
}
    StartTransactionSupport;
  except
    FPlainDriver.Close(FHandle);
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
function TZSQLiteConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then Open;
  Result := TZSQLiteStatement.Create(FPlainDriver, Self, Info, FHandle);
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
function TZSQLiteConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZSQLitePreparedStatement.Create(FPlainDriver, Self, SQL,
    Info, FHandle);
end;

{**
  Starts a transaction support.
}
procedure TZSQLiteConnection.StartTransactionSupport;
var
  ErrorCode: Integer;
  ErrorMessage: PChar;
  SQL: PChar;
begin
  if TransactIsolationLevel <> tiNone then
  begin
    SQL := 'BEGIN TRANSACTION';
    ErrorCode := FPlainDriver.Execute(FHandle, PChar(SQL), nil, nil,
      ErrorMessage);
    CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZSQLiteConnection.Commit;
var
  ErrorCode: Integer;
  ErrorMessage: PChar;
  SQL: PChar;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'COMMIT TRANSACTION';
    ErrorCode := FPlainDriver.Execute(FHandle, PChar(SQL), nil, nil,
      ErrorMessage);
    CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
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
procedure TZSQLiteConnection.Rollback;
var
  ErrorCode: Integer;
  ErrorMessage: PChar;
  SQL: PChar;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'ROLLBACK TRANSACTION';
    ErrorCode := FPlainDriver.Execute(FHandle, PChar(SQL), nil, nil,
      ErrorMessage);
    CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
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
procedure TZSQLiteConnection.Close;
var
  LogMessage: string;
begin
  if not Closed then
  begin
    FPlainDriver.Close(FHandle);
    FHandle := nil;
    LogMessage := Format('DISCONNECT FROM "%s"', [Database]);
    DriverManager.LogMessage(lcDisconnect, FPlainDriver.GetProtocol, LogMessage);
  end;
  inherited Close;
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZSQLiteConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZSQLiteConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZSQLiteConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  ErrorCode: Integer;
  ErrorMessage: PChar;
  SQL: PChar;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'ROLLBACK TRANSACTION';
    ErrorCode := FPlainDriver.Execute(FHandle, PChar(SQL), nil, nil,
      ErrorMessage);
    CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  end;

  inherited SetTransactionIsolation(Level);

  if not Closed then
    StartTransactionSupport;
end;

{**
  Gets a reference to SQLite connection handle.
  @return a reference to SQLite connection handle.
}
function TZSQLiteConnection.GetConnectionHandle: Psqlite;
begin
  Result := FHandle;
end;

{**
  Gets a SQLite plain driver interface.
  @return a SQLite plain driver interface.
}
function TZSQLiteConnection.GetPlainDriver: IZSQLitePlainDriver;
begin
  Result := FPlainDriver;
end;

initialization
  SQLiteDriver := TZSQLiteDriver.Create;
  DriverManager.RegisterDriver(SQLiteDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(SQLiteDriver);
  SQLiteDriver := nil;
end.

