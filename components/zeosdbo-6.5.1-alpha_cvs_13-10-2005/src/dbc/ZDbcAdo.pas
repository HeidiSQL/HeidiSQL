{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               ADO Connectivity Classes                  }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Janos Fegyverneki                 }
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

unit ZDbcAdo;

interface

{$I ZDbc.inc}

uses
{$IFNDEF UNIX}
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
{$ENDIF}
  Classes, ZDbcConnection, ZDbcIntfs, ZCompatibility, ZPlainDriver,
  ZPlainAdoDriver, ZPlainAdo;

type
  {** Implements Ado Database Driver. }
  TZAdoDriver = class(TZAbstractDriver)
  private
    FAdoPlainDriver: IZPlainDriver;
  public
    constructor Create;
    function Connect(Url: string; Info: TStrings): IZConnection; override;

    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;
  end;

  {** Represents an Ado specific connection interface. }
  IZAdoConnection = interface (IZConnection)
    ['{50D1AF76-0174-41CD-B90B-4FB770EFB14F}']
    function GetAdoConnection: ZPlainAdo.Connection;
    procedure InternalExecuteStatement(SQL: string);
    procedure CheckAdoError;
  end;

  {** Implements a generic Ado Connection. }
  TZAdoConnection = class(TZAbstractConnection, IZAdoConnection)
  private
    procedure ReStartTransactionSupport;
  protected
    FAdoConnection: ZPlainAdo.Connection;
    FPlainDriver: IZPlainDriver;
    function GetAdoConnection: ZPlainAdo.Connection; virtual;
    procedure InternalExecuteStatement(SQL: string); virtual;
    procedure CheckAdoError; virtual;
    procedure StartTransaction; virtual;
  public
    constructor Create(Driver: IZDriver; Url: string;
      PlainDriver: IZPlainDriver; HostName: string; Port: Integer;
      Database: string; User: string; Password: string; Info: TStrings);

    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(SQL: string; Info: TStrings):
      IZCallableStatement; override;

    function NativeSQL(SQL: string): string; override;

    procedure SetAutoCommit(AutoCommit: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;

    procedure SetReadOnly(ReadOnly: Boolean); override;

    procedure SetCatalog(Catalog: string); override;
    function GetCatalog: string; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;
  end;

var
  {** The common driver manager object. }
  AdoDriver: IZDriver;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  SysUtils, ActiveX, ZDbcUtils, ZDbcLogging,
  ZDbcAdoStatement, ZDbcAdoMetaData;

const                                                //adXactUnspecified
  IL: array[TZTransactIsolationLevel] of TOleEnum = (adXactChaos, adXactReadUncommitted, adXactReadCommitted, adXactRepeatableRead, adXactSerializable);

{ TZDBLibDriver }

{**
  Constructs this object with default properties.
}
constructor TZAdoDriver.Create;
begin
  FAdoPlainDriver := TZAdoPlainDriver.Create;
end;

{**
  Get a name of the supported subprotocol.
}
function TZAdoDriver.GetSupportedProtocols: TStringDynArray;
begin
  SetLength(Result, 1);
  Result[0] := FAdoPlainDriver.GetProtocol;
end;

{**
  Attempts to make a database connection to the given URL.
}
function TZAdoDriver.Connect(Url: string; Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  Protocol: string;
  PlainDriver: IZPlainDriver;
begin
  TempInfo := TStringList.Create;
  try
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, TempInfo);
    Protocol := ResolveConnectionProtocol(Url, GetSupportedProtocols);
    if Protocol = FAdoPlainDriver.GetProtocol then
      PlainDriver := FAdoPlainDriver;
    PlainDriver.Initialize;
    Result := TZAdoConnection.Create(Self, Url, PlainDriver, HostName,
      Port, Database, UserName, Password, TempInfo);
  finally
    TempInfo.Free;
  end;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZAdoDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZAdoDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{ TZAdoConnection }

{**
  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZAdoConnection.Create(Driver: IZDriver; Url: string;
  PlainDriver: IZPlainDriver; HostName: string; Port: Integer;
  Database: string; User: string; Password: string; Info: TStrings);
begin
  FAdoConnection := CoConnection.Create;
  FPLainDriver := PlainDriver;
  inherited Create(Driver, Url, HostName, Port, Database, User, Password, Info,
    TZAdoDatabaseMetadata.Create(Self, Url, Info));
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAdoConnection.Destroy;
begin
  Close;
  FAdoConnection := nil;
  inherited Destroy;
end;

{**
  Just return the Ado Connection
}
function TZAdoConnection.GetAdoConnection: ZPlainAdo.Connection;
begin
  Result := FAdoConnection;
end;

{**
  Executes simple statements internally.
}
procedure TZAdoConnection.InternalExecuteStatement(SQL: string);
var
  RowsAffected: OleVariant;
begin
  try
    FAdoConnection.Execute(SQL, RowsAffected, adExecuteNoRecords);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, FPlainDriver.GetProtocol, SQL, 0, E.Message);
      raise;
    end;
  end;
end;

procedure TZAdoConnection.CheckAdoError;
begin
end;

{**
  Starts a transaction support.
}
procedure TZAdoConnection.ReStartTransactionSupport;
begin
  if Closed then Exit;

  if not (AutoCommit or (GetTransactionIsolation = tiNone)) then
    StartTransaction;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZAdoConnection.Open;
var
  LogMessage: string;
begin
  if not Closed then Exit;

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);
  try
    if ReadOnly then
      FAdoConnection.Set_Mode(adModeRead)
    else
      FAdoConnection.Set_Mode(adModeUnknown);
    FAdoConnection.Open(Database, User, Password, -1{adConnectUnspecified});
    FAdoConnection.Set_CursorLocation(adUseClient);
    DriverManager.LogMessage(lcConnect, FPLainDriver.GetProtocol, LogMessage);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcConnect, FPlainDriver.GetProtocol, LogMessage, 0, E.Message);
      raise;
    end;
  end;

  inherited Open;

  FAdoConnection.IsolationLevel := IL[GetTransactionIsolation];
  ReStartTransactionSupport;
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
function TZAdoConnection.CreateRegularStatement(Info: TStrings): IZStatement;
begin
  if IsClosed then Open;
  Result := TZAdoStatement.Create(FPlainDriver, Self, '', Info);
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
function TZAdoConnection.CreatePreparedStatement(
  SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZAdoPreparedStatement.Create(FPLainDriver, Self, SQL, Info);
end;

{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures.
  The <code>CallableStatement</code> object provides
  methods for setting up its IN and OUT parameters, and
  methods for executing the call to a stored procedure.

  <P><B>Note:</B> This method is optimized for handling stored
  procedure call statements. Some drivers may send the call
  statement to the database when the method <code>prepareCall</code>
  is done; others
  may wait until the <code>CallableStatement</code> object
  is executed. This has no
  direct effect on users; however, it does affect which method
  throws certain SQLExceptions.

  Result sets created using the returned CallableStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZAdoConnection.CreateCallableStatement(SQL: string; Info: TStrings):
  IZCallableStatement;
begin
  if IsClosed then Open;
  Result := TZAdoCallableStatement.Create(FPlainDriver, Self, SQL, Info);
end;

{**
  Converts the given SQL statement into the system's native SQL grammar.
  A driver may convert the JDBC sql grammar into its system's
  native SQL grammar prior to sending it; this method returns the
  native form of the statement that the driver would have sent.

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders
  @return the native form of this statement
}
function TZAdoConnection.NativeSQL(SQL: string): string;
begin
  Result := SQL;
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
procedure TZAdoConnection.SetAutoCommit(AutoCommit: Boolean);
begin
  if GetAutoCommit = AutoCommit then  Exit;
  if not Closed and AutoCommit then
  begin
    if FAdoConnection.State = adStateOpen then
      FAdoConnection.CommitTrans;
    DriverManager.LogMessage(lcExecute, FPLainDriver.GetProtocol, 'COMMIT');
  end;
  inherited;
  ReStartTransactionSupport;
end;

{**
  Attempts to change the transaction isolation level to the one given.
  The constants defined in the interface <code>Connection</code>
  are the possible transaction isolation levels.

  <P><B>Note:</B> This method cannot be called while
  in the middle of a transaction.

  @param level one of the TRANSACTION_* isolation values with the
    exception of TRANSACTION_NONE; some databases may not support other values
  @see DatabaseMetaData#supportsTransactionIsolationLevel
}
procedure TZAdoConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if GetTransactionIsolation = Level then Exit;

  if not Closed and not AutoCommit and (GetTransactionIsolation <> tiNone) then
  begin
    FAdoConnection.CommitTrans;
    DriverManager.LogMessage(lcExecute, FPLainDriver.GetProtocol, 'COMMIT');
  end;

  inherited;

  if not Closed then
    FAdoConnection.IsolationLevel := IL[Level];

  RestartTransactionSupport;
end;

{**
  Starts a new transaction. Used internally.
}
procedure TZAdoConnection.StartTransaction;
var
  LogMessage: string;
begin
  LogMessage := 'BEGIN TRANSACTION';
  try
    FAdoConnection.BeginTrans;
    DriverManager.LogMessage(lcExecute, FPLainDriver.GetProtocol, LogMessage);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, FPlainDriver.GetProtocol, LogMessage, 0, E.Message);
      raise;
    end;
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZAdoConnection.Commit;
var
  LogMessage: string;
begin
  LogMessage := 'COMMIT';
  try
    FAdoConnection.CommitTrans;
    DriverManager.LogMessage(lcExecute, FPLainDriver.GetProtocol, LogMessage);
    StartTransaction;
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, FPlainDriver.GetProtocol, LogMessage, 0, E.Message);
      raise;
    end;
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZAdoConnection.Rollback;
var
  LogMessage: string;
begin
  LogMessage := 'ROLLBACK';
  try
    FAdoConnection.RollbackTrans;
    DriverManager.LogMessage(lcExecute, FPLainDriver.GetProtocol, LogMessage);
    StartTransaction;
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, FPlainDriver.GetProtocol, LogMessage, 0, E.Message);
      raise;
    end;
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
procedure TZAdoConnection.Close;
var
  LogMessage: string;
begin
  if Closed then Exit;
  SetAutoCommit(True);

  LogMessage := Format('CLOSE CONNECTION TO "%s"', [Database]);
  try
    if FAdoConnection.State = adStateOpen then
      FAdoConnection.Close;
    DriverManager.LogMessage(lcExecute, FPLainDriver.GetProtocol, LogMessage);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, FPlainDriver.GetProtocol, LogMessage, 0, E.Message);
      raise;
    end;
  end;

  inherited;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZAdoConnection.SetReadOnly(ReadOnly: Boolean);
begin
  inherited;
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZAdoConnection.SetCatalog(Catalog: string);
var
  LogMessage: string;
begin
  if Closed then Exit;

  LogMessage := Format('SET CATALOG %s', [Catalog]);
  try
    FAdoConnection.DefaultDatabase := Catalog;
    DriverManager.LogMessage(lcExecute, FPLainDriver.GetProtocol, LogMessage);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, FPlainDriver.GetProtocol, LogMessage, 0, E.Message);
      raise;
    end;
  end;
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZAdoConnection.GetCatalog: string;
begin
  Result := FAdoConnection.DefaultDatabase;
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZAdoConnection.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZAdoConnection.ClearWarnings;
begin
end;

initialization
  AdoDriver := TZAdoDriver.Create;
  DriverManager.RegisterDriver(AdoDriver);
finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(AdoDriver);
  AdoDriver := nil;
end.
