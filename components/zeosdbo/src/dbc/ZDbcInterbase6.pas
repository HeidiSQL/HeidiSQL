{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  ZCompatibility, Classes, SysUtils, ZDbcUtils, ZDbcIntfs, ZDbcConnection,
  Contnrs, ZPlainInterbaseDriver, ZPlainFirebirdDriver,
  ZPlainFirebirdInterbaseConstants, ZSysUtils, ZDbcInterbase6Utils, ZDbcLogging,
  ZDbcGenericResolver, ZTokenizer, ZGenericSqlAnalyser;

type

  {** Implements Interbase6 Database Driver. }
  TZInterbase6Driver = class(TZAbstractDriver)
  private
    FInterbase6PlainDriver: IZInterbase6PlainDriver;
    FInterbase5PlainDriver: IZInterbase5PlainDriver;
    FFirebird10PlainDriver: IZFirebird10PlainDriver;
    FFirebird15PlainDriver: IZFirebird15PlainDriver;
    FFirebird20PlainDriver: IZFirebird20PlainDriver;
    // embedded drivers
    FFirebirdD15PlainDriver: IZFirebird15PlainDriver;
    FFirebirdD20PlainDriver: IZFirebird20PlainDriver;

  protected
    function GetPlainDriver(const Url: string): IZInterbasePlainDriver;
  public
    constructor Create;
    function Connect(const Url: string; Info: TStrings): IZConnection; override;

    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a Interbase specific connection interface. }
  IZInterbase6Connection = interface (IZConnection)
    ['{E870E4FE-21EB-4725-B5D8-38B8A2B12D0B}']
    function GetDBHandle: PISC_DB_HANDLE;
    function GetTrHandle: PISC_TR_HANDLE;
    function GetDialect: Word;
    function GetPlainDriver: IZInterbasePlainDriver;
    procedure CreateNewDatabase(const SQL: String);
  end;

  {** Implements Interbase6 Database Connection. }
  TZInterbase6Connection = class(TZAbstractConnection, IZInterbase6Connection)
  private
    FDialect: Word;
    FHandle: TISC_DB_HANDLE;
    FTrHandle: TISC_TR_HANDLE;
    FStatusVector: TARRAY_ISC_STATUS;
    FPlainDriver: IZInterbasePlainDriver;
  private
    procedure StartTransaction; virtual;
  public
    constructor Create(Driver: IZDriver; const Url: string;
      PlainDriver: IZInterbasePlainDriver;
      const HostName: string; Port: Integer; const Database: string;
      const User: string; const Password: string; Info: TStrings);
    destructor Destroy; override;

    function GetDBHandle: PISC_DB_HANDLE;
    function GetTrHandle: PISC_TR_HANDLE;
    function GetDialect: Word;
    function GetPlainDriver: IZInterbasePlainDriver;
    procedure CreateNewDatabase(const SQL: String);

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    function CreateSequence(const Sequence: string; BlockSize: Integer):
      IZSequence; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;
  end;

  {** Implements a specialized cached resolver for Interbase/Firebird. }
  TZInterbase6CachedResolver = class(TZGenericCachedResolver)
  public
    function FormCalculateStatement(Columns: TObjectList): string; override;
  end;

  {** Implements a Interbase 6 sequence. }
  TZInterbase6Sequence = class(TZAbstractSequence)
  public
    function GetCurrentValue: Int64; override;
    function GetNextValue: Int64; override;
    function GetCurrentValueSQL: string; override;
    function GetNextValueSQL: string; override;
  end;


var
  {** The common driver manager object. }
  Interbase6Driver: IZDriver;

implementation

uses ZDbcInterbase6Statement, ZDbcInterbase6Metadata,
  ZInterbaseToken, ZInterbaseAnalyser;

{ TZInterbase6Driver }

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
function TZInterbase6Driver.Connect(const Url: string;
  Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  PlainDriver: IZInterbasePlainDriver;
begin
 TempInfo := TStringList.Create;
 try
   ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, TempInfo);
   PlainDriver := GetPlainDriver(Url);
   Result := TZInterbase6Connection.Create(Self, Url, PlainDriver, HostName,
     Port, Database, UserName, Password, TempInfo);
 finally
   TempInfo.Free;
 end;
end;

{**
  Constructs this object with default properties.
}
constructor TZInterbase6Driver.Create;
begin
  FInterbase6PlainDriver := TZInterbase6PlainDriver.Create;
  FInterbase5PlainDriver := TZInterbase5PlainDriver.Create;
  FFirebird10PlainDriver := TZFirebird10PlainDriver.Create;
  FFirebird15PlainDriver := TZFirebird15PlainDriver.Create;
  FFirebird20PlainDriver := TZFirebird20PlainDriver.Create;
  // embedded drivers
  FFirebirdD15PlainDriver := TZFirebirdD15PlainDriver.Create;
  FFirebirdD20PlainDriver := TZFirebirdD20PlainDriver.Create;

end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZInterbase6Driver.GetMajorVersion: Integer;
begin
 Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZInterbase6Driver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZInterbase6Driver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZInterbaseTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZInterbase6Driver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZInterbaseStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected protocol.
}
function TZInterbase6Driver.GetPlainDriver(
  const Url: string): IZInterbasePlainDriver;
var
  Protocol: string;
begin
  Protocol := ResolveConnectionProtocol(Url, GetSupportedProtocols);

  if Protocol = FInterbase5PlainDriver.GetProtocol then
    Result := FInterbase5PlainDriver
  else if Protocol = FInterbase6PlainDriver.GetProtocol then
    Result := FInterbase6PlainDriver
  else if Protocol = FFirebird10PlainDriver.GetProtocol then
    Result := FFirebird10PlainDriver
  else if Protocol = FFirebird15PlainDriver.GetProtocol then
    Result := FFirebird15PlainDriver
  else if Protocol = FFirebird20PlainDriver.GetProtocol then
    Result := FFirebird20PlainDriver
  // embedded drivers
  else if Protocol = FFirebirdD15PlainDriver.GetProtocol then
    Result := FFirebirdD15PlainDriver
  else if Protocol = FFirebirdD20PlainDriver.GetProtocol then
    Result := FFirebirdD20PlainDriver
  // Generic driver
  else Result := FInterbase6PlainDriver;
  Result.Initialize;
end;

{**
  Get a name of the supported subprotocol.
  For example: mysql, oracle8 or postgresql72
}
function TZInterbase6Driver.GetSupportedProtocols: TStringDynArray;
begin
  SetLength(Result, 7);
  Result[0] := 'interbase-5';
  Result[1] := 'interbase-6';
  Result[2] := 'firebird-1.0';
  Result[3] := 'firebird-1.5';
  Result[4] := 'firebird-2.0';
  // embedded drivers
  Result[5] := 'firebirdd-1.5';
  Result[6] := 'firebirdd-2.0';

end;

{ TZInterbase6Connection }

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZInterbase6Connection.Close;
begin
  if Closed then Exit;

  if FTrHandle <> nil then
  begin
    if AutoCommit then
    begin
      FPlainDriver.isc_commit_transaction(@FStatusVector, @FTrHandle);
      DriverManager.LogMessage(lcTransaction, FPlainDriver.GetProtocol,
        Format('COMMITT TRANSACTION "%s"', [Database]));
    end else begin
      FPlainDriver.isc_rollback_transaction(@FStatusVector, @FTrHandle);
      DriverManager.LogMessage(lcTransaction, FPlainDriver.GetProtocol,
        Format('ROLLBACK TRANSACTION "%s"', [Database]));
    end;
    FTrHandle := nil;
    CheckInterbase6Error(FPlainDriver, FStatusVector, lcDisconnect);
  end;

  if FHandle <> nil then
  begin
    FPlainDriver.isc_detach_database(@FStatusVector, @FHandle);
    FHandle := nil;
    CheckInterbase6Error(FPlainDriver, FStatusVector, lcDisconnect);
  end;

  DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol,
      Format('DISCONNECT FROM "%s"', [Database]));

  inherited Close;
end;

{**
   Commit current transaction
}
procedure TZInterbase6Connection.Commit;
begin
  if Closed then Exit;

  if FTrHandle <> nil then
  begin
    FPlainDriver.isc_commit_retaining(@FStatusVector, @FTrHandle);
    CheckInterbase6Error(FPlainDriver, FStatusVector, lcTransaction);
    DriverManager.LogMessage(lcTransaction,
      FPlainDriver.GetProtocol, 'TRANSACTION COMMIT');
  end;
end;

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
constructor TZInterbase6Connection.Create(Driver: IZDriver; const Url: string;
  PlainDriver: IZInterbasePlainDriver; const HostName: string; Port: Integer;
  const Database, User, Password: string; Info: TStrings);
var
  RoleName: string;
  ClientCodePage: string;
  UserSetDialect: string;
begin
  inherited Create(Driver, Url, HostName, Port, Database, User, Password, Info,
    TZInterbase6DatabaseMetadata.Create(Self, Url, Info));

  FPlainDriver := PlainDriver;

  { Sets a default Interbase port }
  if Self.Port = 0 then
    Self.Port := 3050;

  { set default sql dialect it can be overriden }
  if FPlainDriver.GetProtocol = 'interbase-5' then
    FDialect := 1
  else FDialect := 3;

  UserSetDialect := Trim(Info.Values['dialect']);
  if UserSetDialect <> '' then
    FDialect := StrToIntDef(UserSetDialect, FDialect);

  { Processes connection properties. }
  self.Info.Values['isc_dpb_username'] := User;
  self.Info.Values['isc_dpb_password'] := Password;

  ClientCodePage := Trim(Info.Values['codepage']);
  if ClientCodePage <> '' then
    self.Info.Values['isc_dpb_lc_ctype'] := UpperCase(ClientCodePage);

  RoleName := Trim(Info.Values['rolename']);
  if RoleName <> '' then
    self.Info.Values['isc_dpb_sql_role_name'] := UpperCase(RoleName);
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
function TZInterbase6Connection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then Open;
  Result := TZInterbase6Statement.Create(Self, Info);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZInterbase6Connection.Destroy;
begin
  if not Closed then
    Close;

  inherited Destroy;
end;

{**
   Get database connection handle.
   @return database handle
}
function TZInterbase6Connection.GetDBHandle: PISC_DB_HANDLE;
begin
  Result := @FHandle;
end;

{**
   Return Interbase dialect number. Dialect a dialect Interbase SQL
   must be 1 or 2 or 3.
   @return dialect number
}
function TZInterbase6Connection.GetDialect: Word;
begin
  Result := FDialect;
end;

{**
   Return native interbase plain driver
   @return plain driver
}
function TZInterbase6Connection.GetPlainDriver: IZInterbasePlainDriver;
begin
  Result := FPlainDriver;
end;

{**
   Get Interbase transaction handle
   @return transaction handle
}
function TZInterbase6Connection.GetTrHandle: PISC_TR_HANDLE;
begin
  Result := @FTrHandle;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZInterbase6Connection.Open;
var
  DPB: PChar;
  FDPBLength: Word;
  DBName: array[0..512] of Char;
begin
  if not Closed then Exit;

  if TransactIsolationLevel = tiReadUncommitted then
    raise EZSQLException.Create('Isolation level do not capable');

  DPB := GenerateDPB(Info, FDPBLength, FDialect);

  if HostName <> '' then
  begin
    if Port <> 3050 then
      StrPCopy(DBName, HostName + '/' + IntToStr(Port) + ':' + Database)
    else
      StrPCopy(DBName, HostName + ':' + Database)
  end else
  StrPCopy(DBName, Database);

  try
    { Create new db if needed }
    if Info.Values['createNewDatabase'] <> '' then
    begin
      CreateNewDatabase(Info.Values['createNewDatabase']);
      { Logging connection action }
      DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol,
        Format('CRATE DATABSE "%s" AS USER "%s"', [Info.Values['createNewDatabase'], User]));
    end;

    { Connect to Interbase6 database. }
    FHandle := nil;
    FPlainDriver.isc_attach_database(@FStatusVector, StrLen(DBName), DBName,
        @FHandle, FDPBLength, DPB);

    { Check connection error }
    CheckInterbase6Error(FPlainDriver, FStatusVector, lcConnect);

    { Logging connection action }
    DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol,
      Format('CONNECT TO "%s" AS USER "%s"', [Database, User]));

    { Start transaction }
    StartTransaction;

    inherited Open;
  finally
    StrDispose(DPB);
  end;
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
function TZInterbase6Connection.CreatePreparedStatement(
  const SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZInterbase6PreparedStatement.Create(Self, SQL, Info);
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
function TZInterbase6Connection.CreateCallableStatement(const SQL: string;
  Info: TStrings): IZCallableStatement;
begin
 if IsClosed then Open;
 Result := TZInterbase6CallableStatement.Create(Self, SQL, Info);
end;

{**
   Conver parameters list to Interbase6 parameter index and values
   and sore it in the list.
   <P><B>Note:</B>
   Parameter value sored in list as value.
   Interbase6 parameter index store as object link.
   @see #GenerateDPB
   @see #GenerateTPB
   @param the list of Interbase6 prepared parameters
}

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZInterbase6Connection.Rollback;
begin
  if FTrHandle <> nil then
  begin
    FPlainDriver.isc_rollback_retaining(@FStatusVector, @FTrHandle);
    CheckInterbase6Error(FPlainDriver, FStatusVector);
    DriverManager.LogMessage(lcTransaction,
      FPlainDriver.GetProtocol, 'TRANSACTION ROLLBACK');
  end;
end;

{**
   Start Interbase transaction
}
procedure TZInterbase6Connection.StartTransaction;
var
  Params: TStrings;
  PTEB: PISC_TEB;
begin
  PTEB := nil;
  Params := TStringList.Create;

  { Set transaction parameters by TransactIsolationLevel }
  Params.Add('isc_tpb_version3');
  case TransactIsolationLevel of
    tiReadCommitted:
      begin
        Params.Add('isc_tpb_read_committed');
        Params.Add('isc_tpb_rec_version');
        Params.Add('isc_tpb_nowait');
      end;
    tiRepeatableRead:
      begin
        Params.Add('isc_tpb_concurrency');
        Params.Add('isc_tpb_nowait');
      end;
    tiSerializable:
      begin
        Params.Add('isc_tpb_consistency');
      end;
  else
    begin
      { Add user defined parameters for traansaction }
      Params.Clear;
      Params.AddStrings(Info);
    end;
  end;

  try
    { GenerateTPB return PTEB with null pointer tpb_address from defaul
      transaction }
    PTEB := GenerateTPB(Params, FHandle);
    FPlainDriver.isc_start_multiple(@FStatusVector, @FTrHandle, 1, PTEB);
    CheckInterbase6Error(FPlainDriver, FStatusVector, lcTransaction);
    DriverManager.LogMessage(lcTransaction, FPlainDriver.GetProtocol,
      'TRANSACTION STARTED.');
  finally
    Params.Free;
    StrDispose(PTEB.tpb_address);
    FreeMem(PTEB);
  end
end;

{**
  Creates new database
  @param SQL a sql strinf for creation database
}
procedure TZInterbase6Connection.CreateNewDatabase(const SQL: String);
var
  DbHandle: PISC_DB_HANDLE;
  TrHandle: PISC_TR_HANDLE;
begin
  Close;
  DbHandle := nil;
  TrHandle := nil;
  FPlainDriver.isc_dsql_execute_immediate(@FStatusVector, @DbHandle, @TrHandle, 0, PChar(sql),
                                          FDialect, nil);
  CheckInterbase6Error(FPlainDriver, FStatusVector, lcExecute, SQL);
  FPlainDriver.isc_detach_database(@FStatusVector, @DbHandle);
  CheckInterbase6Error(FPlainDriver, FStatusVector, lcExecute, SQL);
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZInterbase6Connection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := TZInterbase6Sequence.Create(Self, Sequence, BlockSize);
end;

{ TZInterbase6CachedResolver }

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZInterbase6CachedResolver.FormCalculateStatement(
  Columns: TObjectList): string;
// --> ms, 30/10/2005
var iPos: Integer;
begin
  Result := inherited FormCalculateStatement(Columns);
  if Result <> '' then begin
    iPos := pos('FROM', uppercase(Result));
    if iPos > 0 then begin
      Result := copy(Result, 1, iPos+3) + ' RDB$DATABASE';
    end
    else begin
      Result := Result + ' FROM RDB$DATABASE';
    end;
  end;
// <-- ms
end;

{ TZInterbase6Sequence }

{**
  Gets the current unique key generated by this sequence.
  @param the next generated unique key.
}
function TZInterbase6Sequence.GetCurrentValue: Int64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery(Format(
    'SELECT GEN_ID("%s", 0) FROM rdb$generators ' +
    'WHERE rdb$generators.rdb$generator_name = ''%s''', [Name, Name]));
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
function TZInterbase6Sequence.GetCurrentValueSQL: string;
begin
  Result := Format(' GEN_ID("%s", 0) ', [Name]);
end;

function TZInterbase6Sequence.GetNextValue: Int64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery(Format(
    'SELECT GEN_ID("%s", %d) FROM rdb$generators ' +
    'WHERE rdb$generators.rdb$generator_name = ''%s''', [Name, BlockSize, Name]));
  if ResultSet.Next then
    Result := ResultSet.GetLong(1)
  else
    Result := inherited GetNextValue;
  ResultSet.Close;
  Statement.Close;
end;

function TZInterbase6Sequence.GetNextValueSQL: string;
begin
  Result := Format(' GEN_ID("%s", %d) ', [Name, BlockSize]);
end;

initialization
  Interbase6Driver := TZInterbase6Driver.Create;
  DriverManager.RegisterDriver(Interbase6Driver);

finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(Interbase6Driver);
  Interbase6Driver := nil;
end.
