{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
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

unit ZDbcConnection;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
{$IFDEF VER130BELOW}
  {$IFDEF WIN32}
    Comobj,
  {$ENDIF}
{$ENDIF}
  Classes, SysUtils, ZClasses, ZDbcIntfs, ZTokenizer, ZCompatibility,
  ZGenericSqlToken, ZGenericSqlAnalyser;

type

  {** Implements Abstract Database Driver. }
  TZAbstractDriver = class(TInterfacedObject, IZDriver)
  private
    FTokenizer: IZTokenizer;
    FAnalyser: IZStatementAnalyser;

  protected
    property Tokenizer: IZTokenizer read FTokenizer write FTokenizer;
    property Analyser: IZStatementAnalyser read FAnalyser write FAnalyser;

  public
    constructor Create;
    destructor Destroy; override;

    function GetSupportedProtocols: TStringDynArray; virtual; abstract;
    function Connect(Url: string; Info: TStrings): IZConnection; virtual;
    function AcceptsURL(Url: string): Boolean; virtual;

    function GetPropertyInfo(Url: string;
      Info: TStrings): TStrings; virtual;
    function GetMajorVersion: Integer; virtual;
    function GetMinorVersion: Integer; virtual;

    function GetTokenizer: IZTokenizer; virtual;
    function GetStatementAnalyser: IZStatementAnalyser; virtual;
  end;

  {** Implements Abstract Database Connection. }
  TZAbstractConnection = class(TInterfacedObject, IZConnection)
  private
    FDriver: IZDriver;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUser: string;
    FPassword: string;
    FInfo: TStrings;
    FAutoCommit: Boolean;
    FReadOnly: Boolean;
    FTransactIsolationLevel: TZTransactIsolationLevel;
    FClosed: Boolean;
    FMetadata: TContainedObject;
  protected
    constructor Create(Driver: IZDriver; Url: string; HostName: string;
      Port: Integer; Database: string; User: string; Password: string;
      Info: TStrings; Metadata: TContainedObject);
    procedure RaiseUnsupportedException;

    function CreateRegularStatement(Info: TStrings): IZStatement;
      virtual;
    function CreatePreparedStatement(SQL: string; Info: TStrings):
      IZPreparedStatement; virtual;
    function CreateCallableStatement(SQL: string; Info: TStrings):
      IZCallableStatement; virtual;

    property Driver: IZDriver read FDriver write FDriver;
    property HostName: string read FHostName write FHostName;
    property Port: Integer read FPort write FPort;
    property Database: string read FDatabase write FDatabase;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Info: TStrings read FInfo;
    property AutoCommit: Boolean read FAutoCommit write FAutoCommit;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property TransactIsolationLevel: TZTransactIsolationLevel
      read FTransactIsolationLevel write FTransactIsolationLevel;
    property Closed: Boolean read FClosed write FClosed;
  public
    destructor Destroy; override;

    function CreateStatement: IZStatement;
    function PrepareStatement(SQL: string): IZPreparedStatement;
    function PrepareCall(SQL: string): IZCallableStatement;

    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(SQL: string; Info: TStrings):
      IZCallableStatement;

    function CreateNotification(Event: string): IZNotification; virtual;
    function CreateSequence(Sequence: string; BlockSize: Integer):
      IZSequence; virtual;

    function NativeSQL(SQL: string): string; virtual;

    procedure SetAutoCommit(AutoCommit: Boolean); virtual;
    function GetAutoCommit: Boolean; virtual;

    procedure Commit; virtual;
    procedure Rollback; virtual;

    procedure Open; virtual;
    procedure Close; virtual;
    function IsClosed: Boolean; virtual;
    function Ping: Boolean; virtual;
    function GetAffectedRowsFromLastPost: Int64; virtual;

    function GetDriver: IZDriver;
    function GetMetadata: IZDatabaseMetadata;
    function GetParameters: TStrings;

    procedure SetReadOnly(ReadOnly: Boolean); virtual;
    function IsReadOnly: Boolean; virtual;

    procedure SetCatalog(Catalog: string); virtual;
    function GetCatalog: string; virtual;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); virtual;
    function GetTransactionIsolation: TZTransactIsolationLevel; virtual;

    function GetWarnings: EZSQLWarning; virtual;
    procedure ClearWarnings; virtual;
  end;

  {** Implements Abstract Database notification. }
  TZAbstractNotification = class(TInterfacedObject, IZNotification)
  private
    FEventName: string;
    FConnection: IZConnection;
  protected
    constructor Create(Connection: IZConnection; EventName: string);

    property EventName: string read FEventName write FEventName;
    property Connection: IZConnection read FConnection write FConnection;
  public
    function GetEvent: string;
    procedure Listen; virtual;
    procedure Unlisten; virtual;
    procedure DoNotify; virtual;
    function CheckEvents: string; virtual;

    function GetConnection: IZConnection; virtual;
  end;

  {** Implements Abstract Sequence generator. }
  TZAbstractSequence = class(TInterfacedObject, IZSequence)
  private
    FName: string;
    FBlockSize: Integer;
    FConnection: IZConnection;
  protected
    function GetName: string; virtual;
    function GetBlockSize: Integer; virtual;
    procedure SetName(const Value: string); virtual;
    procedure SetBlockSize(const Value: Integer); virtual;
    property Connection: IZConnection read FConnection write FConnection;
  public
    constructor Create(Connection: IZConnection; Name: string;
      BlockSize: Integer);

    function GetCurrentValue: Int64; virtual;
    function GetNextValue: Int64; virtual;

    function GetConnection: IZConnection; virtual;

    property Name: string read GetName write SetName;
    property BlockSize: Integer read GetBlockSize write SetBlockSize;
  end;

implementation

uses ZMessages, ZSysUtils, ZDbcUtils, ZDbcMetadata;

{ TZAbstractDriver }

{**
  Constructs this object with default properties.
}
constructor TZAbstractDriver.Create;
begin
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractDriver.Destroy;
begin
  inherited Destroy;
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
function TZAbstractDriver.Connect(Url: string; Info: TStrings): IZConnection;
begin
  Result := nil;
end;

{**
  Returns true if the driver thinks that it can open a connection
  to the given URL.  Typically drivers will return true if they
  understand the subprotocol specified in the URL and false if
  they don't.
  @param url the URL of the database
  @return true if this driver can connect to the given URL
}
function TZAbstractDriver.AcceptsURL(Url: string): Boolean;
var
  I: Integer;
  Protocols: TStringDynArray;
begin
  Result := False;
  Protocols := GetSupportedProtocols;
  for I := Low(Protocols) to High(Protocols) do
  begin
    Result := StartsWith(Url, Format('zdbc:%s:', [Protocols[I]]));
    if Result then
      Break;
  end;
end;

{**
  Gets information about the possible properties for this driver.
  <p>The getPropertyInfo method is intended to allow a generic GUI tool to
  discover what properties it should prompt a human for in order to get
  enough information to connect to a database.  Note that depending on
  the values the human has supplied so far, additional values may become
  necessary, so it may be necessary to iterate though several calls
  to getPropertyInfo.

  @param url the URL of the database to which to connect
  @param info a proposed list of tag/value pairs that will be sent on
    connect open
  @return an array of DriverPropertyInfo objects describing possible
    properties.  This array may be an empty array if no properties
    are required.
}
function TZAbstractDriver.GetPropertyInfo(Url: string;
  Info: TStrings): TStrings;
begin
  Result := nil;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZAbstractDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZAbstractDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Creates a generic statement analyser object.
  @returns a generic statement analyser object.
}
function TZAbstractDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZGenericStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Creates a generic tokenizer object.
  @returns a created generic tokenizer object.
}
function TZAbstractDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZGenericSQLTokenizer.Create;
  Result := Tokenizer;
end;

{ TZAbstractConnection }

{**
  Constructs this object and assignes the main properties.
  @param Driver a ZDBC driver interface.
  @param Url a connection URL.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZAbstractConnection.Create(Driver: IZDriver; Url: string;
  HostName: string; Port: Integer; Database: string; User: string;
  Password: string; Info: TStrings; Metadata: TContainedObject);
begin
  FDriver := Driver;
  FHostName := HostName;
  FPort := Port;
  FDatabase := Database;
  FMetadata := Metadata;

  FInfo := TStringList.Create;
  if Info <> nil then
    FInfo.AddStrings(Info);

  if User <> '' then
    FUser := User
  else FUser := FInfo.Values['username'];
  if Password <> '' then
    FPassword := Password
  else FPassword := FInfo.Values['password'];

  FAutoCommit := True;
  FClosed := True;
  FReadOnly := True;
  FTransactIsolationLevel := tiNone;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractConnection.Destroy;
begin
  if not FClosed then Close;
  FInfo.Free;
  FMetadata.Free;
  inherited Destroy;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZAbstractConnection.Open;
begin
  FClosed := False;
end;

{**
  Raises unsupported operation exception.
}
procedure TZAbstractConnection.RaiseUnsupportedException;
begin
  raise EZSQLException.Create(SUnsupportedOperation);
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

  @return a new Statement object
}
function TZAbstractConnection.CreateStatement: IZStatement;
begin
  Result := CreateRegularStatement(nil);
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
function TZAbstractConnection.CreateStatementWithParams(Info: TStrings):
  IZStatement;
begin
  Result := CreateRegularStatement(Info);
end;

{**
  Creates a regular statement object.
  @param SQL a SQL query string.
  @param Info a statement parameters.
  @returns a created statement.
}
function TZAbstractConnection.CreateRegularStatement(
  Info: TStrings): IZStatement;
begin
  Result := nil;
  RaiseUnsupportedException;
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
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZAbstractConnection.PrepareStatement(
  SQL: string): IZPreparedStatement;
begin
  Result := CreatePreparedStatement(SQL, nil);
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  @param SQL a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZAbstractConnection.PrepareStatementWithParams(SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  Result := CreatePreparedStatement(SQL, Info);
end;

{**
  Creates a prepared statement object.
  @param SQL a SQL query string.
  @param Info a statement parameters.
  @returns a created statement.
}
function TZAbstractConnection.CreatePreparedStatement(SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  Result := nil;
  RaiseUnsupportedException;
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
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZAbstractConnection.PrepareCall(
  SQL: string): IZCallableStatement;
begin
  Result := CreateCallableStatement(SQL, nil);
end;

{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures.
  The <code>CallableStatement</code> object provides
  methods for setting up its IN and OUT parameters, and
  methods for executing the call to a stored procedure.

  @param SQL a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZAbstractConnection.PrepareCallWithParams(SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  Result := CreateCallableStatement(SQL, Info);
end;

{**
  Creates a callable statement object.
  @param SQL a SQL query string.
  @param Info a statement parameters.
  @returns a created statement.
}
function TZAbstractConnection.CreateCallableStatement(SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  Result := nil;
  RaiseUnsupportedException;
end;

{**
  Creates an object to send/recieve notifications from SQL server.
  @param Event an event name.
  @returns a created notification object.
}
function TZAbstractConnection.CreateNotification(Event: string): IZNotification;
begin
  Result := nil;
  RaiseUnsupportedException;
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZAbstractConnection.CreateSequence(Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := nil;
  RaiseUnsupportedException;
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
function TZAbstractConnection.NativeSQL(SQL: string): string;
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
procedure TZAbstractConnection.SetAutoCommit(AutoCommit: Boolean);
begin
  FAutoCommit := AutoCommit;
end;

{**
  Gets the current auto-commit state.
  @return the current state of auto-commit mode
  @see #setAutoCommit
}
function TZAbstractConnection.GetAutoCommit: Boolean;
begin
  Result := FAutoCommit;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZAbstractConnection.Commit;
begin
  RaiseUnsupportedException;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZAbstractConnection.Rollback;
begin
  RaiseUnsupportedException;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZAbstractConnection.Close;
begin
  FClosed := True;
end;

{**
  Tests to see if a Connection is closed.
  @return true if the connection is closed; false if it's still open
}
function TZAbstractConnection.IsClosed: Boolean;
begin
  Result := FClosed;
end;

{**
  Returns true if a network ping to the database server succeeds.
  @return true if a network ping to the database server succeeds
}
function TZAbstractConnection.Ping: Boolean;
begin
  raise Exception.Create('Ping() is unsupported by this particular DB driver.');
end;

{**
  Gets the count of rows which were affected by the last INSERT, DELETE or UPDATE
  @return Int64
}
function TZAbstractConnection.GetAffectedRowsFromLastPost: Int64;
begin
  raise Exception.Create('GetAffectedRowsFromLastPost() is unsupported by this particular DB driver.');
end;

{**
  Gets the parent ZDBC driver.
  @returns the parent ZDBC driver interface.
}
function TZAbstractConnection.GetDriver: IZDriver;
begin
  Result := FDriver;
end;

{**
  Gets the metadata regarding this connection's database.
  A Connection's database is able to provide information
  describing its tables, its supported SQL grammar, its stored
  procedures, the capabilities of this connection, and so on. This
  information is made available through a DatabaseMetaData
  object.

  @return a DatabaseMetaData object for this Connection
}
function TZAbstractConnection.GetMetadata: IZDatabaseMetadata;
begin
  Result := FMetadata as IZDatabaseMetadata;
end;

{**
  Gets a connection parameters.
  @returns a list with connection parameters.
}
function TZAbstractConnection.GetParameters: TStrings;
begin
  Result := Info;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZAbstractConnection.SetReadOnly(ReadOnly: Boolean);
begin
  FReadOnly := ReadOnly;
end;

{**
  Tests to see if the connection is in read-only mode.
  @return true if connection is read-only and false otherwise
}
function TZAbstractConnection.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZAbstractConnection.SetCatalog(Catalog: string);
begin
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZAbstractConnection.GetCatalog: string;
begin
  Result := '';
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
procedure TZAbstractConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  FTransactIsolationLevel := Level;
end;

{**
  Gets this Connection's current transaction isolation level.
  @return the current TRANSACTION_* mode value
}
function TZAbstractConnection.GetTransactionIsolation: TZTransactIsolationLevel;
begin
  Result := FTransactIsolationLevel;
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZAbstractConnection.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZAbstractConnection.ClearWarnings;
begin
end;

{ TZAbstractNotification }

{**
  Creates this object and assignes the main properties.
  @param Connection a database connection object.
  @param EventName a name of the SQL event.
}
constructor TZAbstractNotification.Create(Connection: IZConnection;
  EventName: string);
begin
  FConnection := Connection;
  FEventName := EventName;
end;

{**
  Gets an event name.
  @return an event name for this notification.
}
function TZAbstractNotification.GetEvent: string;
begin
  Result := FEventName;
end;

{**
  Sets a listener to the specified event.
}
procedure TZAbstractNotification.Listen;
begin
end;

{**
  Removes a listener to the specified event.
}
procedure TZAbstractNotification.Unlisten;
begin
end;

{**
  Checks for any pending events.
  @return a string with incoming events??
}
function TZAbstractNotification.CheckEvents: string;
begin
  Result := '';
end;

{**
  Sends a notification string.
}
procedure TZAbstractNotification.DoNotify;
begin
end;

{**
  Returns the <code>Connection</code> object
  that produced this <code>Statement</code> object.
  @return the connection that produced this statement
}
function TZAbstractNotification.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{ TZAbstractSequence }

{**
  Creates this sequence object.
  @param Connection an SQL connection interface.
  @param Name a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to server.
}
constructor TZAbstractSequence.Create(Connection: IZConnection;
  Name: string; BlockSize: Integer);
begin
  FConnection := Connection;
  FName := Name;
  FBlockSize := BlockSize;
end;

{**
  Returns the <code>Connection</code> object
  that produced this <code>Statement</code> object.
  @return the connection that produced this statement
}
function TZAbstractSequence.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{**
  Returns a name of the sequence generator.
  @return a name of this sequence generator.
}
function TZAbstractSequence.GetName: string;
begin
  Result := FName;
end;

{**
  Returns the assigned block size for this sequence.
  @return the assigned block size.
}
function TZAbstractSequence.GetBlockSize: Integer;
begin
  Result := FBlockSize;
end;

{**
  Gets the current unique key generated by this sequence.
  @param the last generated unique key.
}
function TZAbstractSequence.GetCurrentValue: Int64;
begin
  Result := 0;
end;

{**
  Gets the next unique key generated by this sequence.
  @param the next generated unique key.
}
function TZAbstractSequence.GetNextValue: Int64;
begin
  Result := 0;
end;

{**
  Sets the block size for this sequence.
  @param Value the block size.
}
procedure TZAbstractSequence.SetBlockSize(const Value: Integer);
begin
  FBlockSize := Value;
end;

{**
  Sets a name of the sequence generator.
  @param Value a name of this sequence generator.
}
procedure TZAbstractSequence.SetName(const Value: string);
begin
  FName := Value;
end;

end.
