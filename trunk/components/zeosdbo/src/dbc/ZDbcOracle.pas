{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
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

unit ZDbcOracle;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  ZCompatibility, Classes, SysUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainOracleDriver, ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser;

type

  {** Implements Oracle Database Driver. }
  TZOracleDriver = class(TZAbstractDriver)
  private
    FOracle9iPlainDriver: IZOraclePlainDriver;
  protected
    function GetPlainDriver(const Url: string): IZOraclePlainDriver;
  public
    constructor Create;
    function Connect(const Url: string; Info: TStrings): IZConnection; override;

    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a Oracle specific connection interface. }
  IZOracleConnection = interface (IZConnection)
    ['{C7F36FDF-8A64-477B-A0EB-3E8AB7C09F8D}']

    function GetPlainDriver: IZOraclePlainDriver;
    function GetConnectionHandle: POCIEnv;
    function GetContextHandle: POCISvcCtx;
    function GetErrorHandle: POCIError;
    function GetServerHandle: POCIServer;
    function GetSessionHandle: POCISession;
    function GetTransactionHandle: POCITrans;
  end;

  {** Implements Oracle Database Connection. }
  TZOracleConnection = class(TZAbstractConnection, IZOracleConnection)
  private
    FCatalog: string;
    FPlainDriver: IZOraclePlainDriver;
    FHandle: POCIEnv;
    FContextHandle: POCISvcCtx;
    FErrorHandle: POCIError;
    FServerHandle: POCIServer;
    FSessionHandle: POCISession;
    FTransHandle: POCITrans;
    FClientCodePage: string;

  protected
    procedure StartTransactionSupport;

  public
    constructor Create(Driver: IZDriver; const Url: string;
      PlainDriver: IZOraclePlainDriver; const HostName: string; Port: Integer; const SocketName: string;
      const Database: string; const User: string; const Password: string; Info: TStrings);
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: WideString; Info: TStrings):
      IZPreparedStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence; override;

    function GetPlainDriver: IZOraclePlainDriver;
    function GetConnectionHandle: POCIEnv;
    function GetContextHandle: POCISvcCtx;
    function GetErrorHandle: POCIError;
    function GetServerHandle: POCIServer;
    function GetSessionHandle: POCISession;
    function GetTransactionHandle: POCITrans;
  end;

  TZOracleSequence = class(TZAbstractSequence)
  public
    function GetCurrentValue: Int64; override;
    function GetNextValue: Int64; override;
  end;

var
  {** The common driver manager object. }
  OracleDriver: IZDriver;

implementation

uses
  ZMessages, ZSysUtils, ZDbcUtils, ZGenericSqlToken, ZDbcOracleStatement,
  ZDbcOracleUtils, ZDbcOracleMetadata, ZOracleToken, ZOracleAnalyser;

{ TZOracleDriver }

{**
  Constructs this object with default properties.
}
constructor TZOracleDriver.Create;
begin
  FOracle9iPlainDriver := TZOracle9iPlainDriver.Create;
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
function TZOracleDriver.Connect(const Url: string; Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  SocketName: string;
  PlainDriver: IZOraclePlainDriver;
begin
  TempInfo := TStringList.Create;
  try
    PlainDriver := GetPlainDriver(Url);
    ResolveDatabaseUrl(Url, Info, HostName, Port, SocketName, Database,
      UserName, Password, TempInfo);
    Result := TZOracleConnection.Create(Self, Url, PlainDriver, HostName, Port, SocketName,
      Database, UserName, Password, TempInfo);
  finally
    TempInfo.Free;
  end;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZOracleDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZOracleDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZOracleDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZOracleTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZOracleDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZOracleStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Get a name of the supported subprotocol.
  For example: oracle, oracle8 or postgresql72
}
function TZOracleDriver.GetSupportedProtocols: TStringDynArray;
begin
  SetLength(Result, 2);
  Result[0] := 'oracle';
  Result[1] := FOracle9iPlainDriver.GetProtocol;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected protocol.
}
function TZOracleDriver.GetPlainDriver(const Url: string): IZOraclePlainDriver;
var
  Protocol: string;
begin
  Protocol := ResolveConnectionProtocol(Url, GetSupportedProtocols);
  if Protocol = FOracle9iPlainDriver.GetProtocol then
    Result := FOracle9iPlainDriver
  else Result := FOracle9iPlainDriver;
  Result.Initialize;
end;

{ TZOracleConnection }

{**
  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param PlainDriver a Oracle plain driver.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZOracleConnection.Create(Driver: IZDriver; const Url: string;
  PlainDriver: IZOraclePlainDriver; const HostName: string; Port: Integer; const SocketName: string;
  const Database, User, Password: string; Info: TStrings);
begin
  inherited Create(Driver, Url, HostName, Port, SocketName, Database, User, Password, Info,
    TZOracleDatabaseMetadata.Create(Self, Url, Info));

  { Sets a default properties }
  FPlainDriver := PlainDriver;
  FHandle := nil;
  if Self.Port = 0 then Self.Port := 1521;
  AutoCommit := True;
  TransactIsolationLevel := tiNone;

  { Processes connection properties. }
  FClientCodePage := Trim(Info.Values['codepage']);

  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleConnection.Destroy;
begin
  if FHandle <> nil then
  begin
    FPlainDriver.HandleFree(FHandle, OCI_HTYPE_ENV);
    FHandle := nil;
  end;

  inherited Destroy;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZOracleConnection.Open;
var
  Status: Integer;
  LogMessage: string;
//  ConnectTimeout: Integer;
//  SQL: PChar;

  procedure CleanupOnFail;
  begin
    FPlainDriver.HandleFree(FContextHandle, OCI_HTYPE_SVCCTX);
    FContextHandle := nil;
    FPlainDriver.HandleFree(FErrorHandle, OCI_HTYPE_ERROR);
    FErrorHandle := nil;
    FPlainDriver.HandleFree(FServerHandle, OCI_HTYPE_SERVER);
    FServerHandle := nil;
  end;

begin
  if not Closed then Exit;

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);

  { Sets a default port number. }
  if Port = 0 then Port := 1521;
  { Sets connection timeout. }
//  ConnectTimeout := StrToIntDef(Info.Values['timeout'], 0);

  { Connect to Oracle database. }
  if FHandle = nil then
    FPlainDriver.EnvInit(FHandle, OCI_DEFAULT, 0, nil);
  FErrorHandle := nil;
  FPlainDriver.HandleAlloc(FHandle, FErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  FServerHandle := nil;
  FPlainDriver.HandleAlloc(FHandle, FServerHandle, OCI_HTYPE_SERVER, 0, nil);
  FContextHandle := nil;
  FPlainDriver.HandleAlloc(FHandle, FContextHandle, OCI_HTYPE_SVCCTX, 0, nil);

  Status := FPlainDriver.ServerAttach(FServerHandle, FErrorHandle,
    PChar(string(Database)), Length(Database), 0);
  try
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcConnect, LogMessage);
  except
    CleanupOnFail;
    raise;
  end;

  FPlainDriver.AttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FServerHandle, 0,
    OCI_ATTR_SERVER, FErrorHandle);
  FPlainDriver.HandleAlloc(FHandle, FSessionHandle, OCI_HTYPE_SESSION, 0, nil);
  FPlainDriver.AttrSet(FSessionHandle, OCI_HTYPE_SESSION, PChar(string(User)),
    Length(User), OCI_ATTR_USERNAME, FErrorHandle);
  FPlainDriver.AttrSet(FSessionHandle, OCI_HTYPE_SESSION, PChar(string(Password)),
    Length(Password), OCI_ATTR_PASSWORD, FErrorHandle);
  Status := FPlainDriver.SessionBegin(FContextHandle, FErrorHandle,
    FSessionHandle, OCI_CRED_RDBMS, OCI_DEFAULT);
  try
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcConnect, LogMessage);
  except
    CleanupOnFail;
    raise;
  end;
  FPlainDriver.AttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FSessionHandle, 0,
    OCI_ATTR_SESSION, FErrorHandle);

  DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol, LogMessage);

(*
  { Sets a client codepage. }
  if FClientCodePage <> '' then
  begin
    SQL := PChar(Format('SET CHARACTER SET %s', [FClientCodePage]));
    FPlainDriver.ExecQuery(FHandle, SQL);
    CheckOracleError(FPlainDriver, FHandle, lcExecute, SQL);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  end;
*)
  StartTransactionSupport;

  inherited Open;
end;

{**
  Starts a transaction support.
}
procedure TZOracleConnection.StartTransactionSupport;
var
  SQL: PChar;
  Status: Integer;
  Isolation: Integer;
begin
  if TransactIsolationLevel = tiNone then
  begin
    SQL := 'SET TRANSACTION ISOLATION LEVEL DEFAULT';
    Isolation := OCI_DEFAULT;
  end
  else if TransactIsolationLevel = tiReadCommitted then
  begin
// Behaviour changed by mdaems 31/05/2006 : Read Committed is the default
// isolation level used by oracle. This property should not be abused to add
// the non-standard isolation level 'read only' thats invented by oracle.
//    SQL := 'SET TRANSACTION ISOLATION LEVEL READONLY';
//    Isolation := OCI_TRANS_READONLY;
    SQL := 'SET TRANSACTION ISOLATION LEVEL DEFAULT';
    Isolation := OCI_DEFAULT;
  end
  else if TransactIsolationLevel = tiRepeatableRead then
  begin
    SQL := 'SET TRANSACTION ISOLATION LEVEL READWRITE';
    Isolation := OCI_TRANS_READWRITE;
  end
  else if TransactIsolationLevel = tiSerializable then
  begin
    SQL := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
    Isolation := OCI_TRANS_SERIALIZABLE;
  end
  else
    raise EZSQLException.Create(SIsolationIsNotSupported);

  FTransHandle := nil;
  FPlainDriver.HandleAlloc(FHandle, FTransHandle, OCI_HTYPE_TRANS, 0, nil);
  FPlainDriver.AttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FTransHandle, 0,
    OCI_ATTR_TRANS, FErrorHandle);

  Status := FPlainDriver.TransStart(FContextHandle, FErrorHandle, 0, Isolation);
  CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, SQL);

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
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
function TZOracleConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then Open;
  Result := TZOracleStatement.Create(FPlainDriver, Self, Info);
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
function TZOracleConnection.CreatePreparedStatement(const SQL: WideString;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZOraclePreparedStatement.Create(FPlainDriver, Self, String(SQL), Info);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZOracleConnection.Commit;
var
  Status: Integer;
  SQL: PChar;
begin
  if not Closed then
  begin
    SQL := 'COMMIT';

    Status := FPlainDriver.TransCommit(FContextHandle, FErrorHandle,
      OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, SQL);

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
procedure TZOracleConnection.Rollback;
var
  Status: Integer;
  SQL: PChar;
begin
  if not Closed then
  begin
    SQL := 'ROLLBACK';

    Status := FPlainDriver.TransRollback(FContextHandle, FErrorHandle,
      OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, SQL);

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
procedure TZOracleConnection.Close;
var
  Status: Integer;
  LogMessage: string;
begin
  if not Closed then
  begin
    LogMessage := Format('DISCONNECT FROM "%s"', [Database]);

    { Closes started transaction }
    Status := FPlainDriver.TransRollback(FContextHandle, FErrorHandle,
      OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcDisconnect,
      LogMessage);
    FPlainDriver.HandleFree(FTransHandle, OCI_HTYPE_TRANS);
    FTransHandle := nil;

    { Closes the session }
    Status := FPlainDriver.SessionEnd(FContextHandle, FErrorHandle,
      FSessionHandle, OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcDisconnect,
      LogMessage);

    { Detaches from the server }
    Status := FPlainDriver.ServerDetach(FServerHandle, FErrorHandle,
      OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcDisconnect,
      LogMessage);

    { Frees all handlers }
    FPlainDriver.HandleFree(FSessionHandle, OCI_HTYPE_SESSION);
    FSessionHandle := nil;
    FPlainDriver.HandleFree(FContextHandle, OCI_HTYPE_SVCCTX);
    FContextHandle := nil;
    FPlainDriver.HandleFree(FServerHandle, OCI_HTYPE_SERVER);
    FServerHandle := nil;
    FPlainDriver.HandleFree(FErrorHandle, OCI_HTYPE_ERROR);
    FErrorHandle := nil;

    DriverManager.LogMessage(lcDisconnect, FPlainDriver.GetProtocol, LogMessage);
  end;
  inherited Close;
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZOracleConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZOracleConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZOracleConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  Status: Integer;
  SQL: PChar;
begin
  if TransactIsolationLevel <> Level then
  begin
    inherited SetTransactionIsolation(Level);

    if not Closed then
    begin
      SQL := 'END TRANSACTION';
      Status := FPlainDriver.TransRollback(FContextHandle, FErrorHandle,
        OCI_DEFAULT);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, SQL);
      FPlainDriver.HandleFree(FTransHandle, OCI_HTYPE_TRANS);
      FTransHandle := nil;
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

      StartTransactionSupport;
    end;
  end;
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZOracleConnection.CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence; 
begin
  Result := TZOracleSequence.Create(Self, Sequence, BlockSize);
end;
{**
  Gets a Oracle plain driver interface.
  @return a Oracle plain driver interface.
}
function TZOracleConnection.GetPlainDriver: IZOraclePlainDriver;
begin
  Result := FPlainDriver;
end;

{**
  Gets a reference to Oracle connection handle.
  @return a reference to Oracle connection handle.
}
function TZOracleConnection.GetConnectionHandle: POCIEnv;
begin
  Result := FHandle;
end;

{**
  Gets a reference to Oracle context handle.
  @return a reference to Oracle context handle.
}
function TZOracleConnection.GetContextHandle: POCISvcCtx;
begin
  Result := FContextHandle;
end;

{**
  Gets a reference to Oracle error handle.
  @return a reference to Oracle error handle.
}
function TZOracleConnection.GetErrorHandle: POCIError;
begin
  Result := FErrorHandle;
end;

{**
  Gets a reference to Oracle server handle.
  @return a reference to Oracle server handle.
}
function TZOracleConnection.GetServerHandle: POCIServer;
begin
  Result := FServerHandle;
end;

{**
  Gets a reference to Oracle session handle.
  @return a reference to Oracle session handle.
}
function TZOracleConnection.GetSessionHandle: POCISession;
begin
  Result := FSessionHandle;
end;

{**
  Gets a reference to Oracle transaction handle.
  @return a reference to Oracle transacton handle.
}
function TZOracleConnection.GetTransactionHandle: POCITrans;
begin
  Result := FTransHandle;
end;

{ TZOracleSequence }

{**
  Gets the current unique key generated by this sequence.
  @param the last generated unique key.
}
function TZOracleSequence.GetCurrentValue: Int64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery(
    Format('SELECT %s.CURRVAL FROM DUAL', [Name]));
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
function TZOracleSequence.GetNextValue: Int64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery(
    Format('SELECT %s.NEXTVAL FROM DUAL', [Name]));
  if ResultSet.Next then
    Result := ResultSet.GetLong(1)
  else
    Result := inherited GetNextValue;
  ResultSet.Close;
  Statement.Close;
end;

initialization
  OracleDriver := TZOracleDriver.Create;
  DriverManager.RegisterDriver(OracleDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(OracleDriver);
  OracleDriver := nil;
end.

