{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Database Connection Component              }
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

unit ZConnection;

interface

{$I ZComponent.inc}

uses
{$ifdef COMPILER_10_UP}
  DBCommonTypes,
{$endif}
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
{$IFNDEF UNIX}
{$IFDEF ENABLE_ADO}
  ZDbcAdo,
{$ENDIF}
{$ENDIF}
{$IFDEF ENABLE_DBLIB}
  ZDbcDbLib,
{$ENDIF}
{$IFDEF ENABLE_MYSQL}
  ZDbcMySql,
{$ENDIF}
{$IFDEF ENABLE_POSTGRESQL}
  ZDbcPostgreSql,
{$ENDIF}
{$IFDEF ENABLE_INTERBASE}
  ZDbcInterbase6,
{$ENDIF}
{$IFDEF ENABLE_SQLITE}
  ZDbcSqLite,
{$ENDIF}
{$IFDEF ENABLE_ORACLE}
  ZDbcOracle,
{$ENDIF}
{$IFDEF ENABLE_ASA}
  ZDbcASA,
{$ENDIF}
 {$IFDEF FPC}
  SysUtils, Classes, ZDbcIntfs, DB,ZCompatibility;
 {$ELSE}
  {$IFNDEF VER180}
   SysUtils, Classes, ZDbcIntfs, DB,ZCompatibility;
  {$ELSE}
   SysUtils, Classes, ZDbcIntfs, DB,ZCompatibility;
  {$ENDIF}
 {$ENDIF}


type

  {** Represents a component which wraps a connection to database. }
  TZConnection = class(TComponent)
  private
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUser: string;
    FPassword: string;
    FCatalog: string;
    FProperties: TStrings;
    FAutoCommit: Boolean;
    FReadOnly: Boolean;
    FTransactIsolationLevel: TZTransactIsolationLevel;
    FConnection: IZConnection;
    FDatasets: TList;
    FLoginPrompt: Boolean;
    FStreamedConnected: Boolean;
    FExplicitTransactionCounter: Integer;
    FSQLHourGlass: Boolean;
    FDesignConnection: Boolean;

    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FBeforeReconnect: TNotifyEvent;
    FAfterReconnect: TNotifyEvent;
    FOnCommit: TNotifyEvent;
    FOnRollback: TNotifyEvent;
    FOnStartTransaction: TNotifyEvent;
    FOnLogin: TLoginEvent;

    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
    procedure SetProperties(Value: TStrings);
    procedure SetTransactIsolationLevel(Value: TZTransactIsolationLevel);
    procedure SetAutoCommit(Value: Boolean);
    function GetDbcDriver: IZDriver;
    function GetInTransaction: Boolean;
    function GetClientVersion: Integer;
    function GetServerVersion: Integer;
    procedure DoBeforeConnect;
    procedure DoAfterConnect;
    procedure DoBeforeDisconnect;
    procedure DoAfterDisconnect;
    procedure DoBeforeReconnect;
    procedure DoAfterReconnect;
    procedure DoCommit;
    procedure DoRollback;
    procedure DoStartTransaction;

    procedure CheckConnected;
    procedure CheckAutoCommitMode;
    procedure CheckNonAutoCommitMode;

    function ConstructURL(const UserName, Password: string): string;

    procedure CloseAllDataSets;
    procedure UnregisterAllDataSets;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;

    property StreamedConnected: Boolean read FStreamedConnected write FStreamedConnected;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Reconnect;
    function Ping: Boolean; virtual;
    function GetAffectedRowsFromLastPost: Int64;
    function GetThreadId: Cardinal;
    function GetEscapeString(const Value: string): string;

    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    procedure PrepareTransaction(const transactionid: string); virtual;
    procedure CommitPrepared(const transactionid: string); virtual;
    procedure RollbackPrepared(const transactionid: string); virtual;
    function PingServer: Boolean; virtual;

    procedure RegisterDataSet(DataSet: TDataset);
    procedure UnregisterDataSet(DataSet: TDataset);

    procedure GetProtocolNames(List: TStrings);
    procedure GetCatalogNames(List: TStrings);
    procedure GetSchemaNames(List: TStrings);
    procedure GetTableNames(const Pattern: string; List: TStrings);overload;
		procedure GetTableNames(const tablePattern,shemaPattern: string; List: TStrings);overload;

    procedure GetStoredProcNames(const Pattern: string; List: TStrings);

    property InTransaction: Boolean read GetInTransaction;

    property DbcDriver: IZDriver read GetDbcDriver;
    property DbcConnection: IZConnection read FConnection;
    property ClientVersion: Integer read GetClientVersion;
    property ServerVersion: Integer read GetServerVersion;
    procedure ShowSQLHourGlass;
    procedure HideSQLHourGlass;

  published
    property Protocol: string read FProtocol write FProtocol;
    property HostName: string read FHostName write FHostName;
    property Port: Integer read FPort write FPort default 0;
    property Database: string read FDatabase write FDatabase;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Catalog: string read FCatalog write FCatalog;
    property Properties: TStrings read FProperties write SetProperties;
    property AutoCommit: Boolean read FAutoCommit write SetAutoCommit
      default True;
    property ReadOnly: Boolean read FReadOnly write FReadOnly
      default False;
    property TransactIsolationLevel: TZTransactIsolationLevel
      read FTransactIsolationLevel write SetTransactIsolationLevel
      default tiNone;
    property Connected: Boolean read GetConnected write SetConnected
      default False;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt
      default False;
    property Version: string read GetVersion write SetVersion stored False;
    property DesignConnection: Boolean read FDesignConnection
      write FDesignConnection default False;

    property BeforeConnect: TNotifyEvent
      read FBeforeConnect write FBeforeConnect;
    property AfterConnect: TNotifyEvent
      read FAfterConnect write FAfterConnect;
    property BeforeDisconnect: TNotifyEvent
      read FBeforeDisconnect write FBeforeDisconnect;
    property AfterDisconnect: TNotifyEvent
      read FAfterDisconnect write FAfterDisconnect;
    property BeforeReconnect: TNotifyEvent
      read FBeforeReconnect write FBeforeReconnect;
    property AfterReconnect: TNotifyEvent
      read FAfterReconnect write FAfterReconnect;
    property SQLHourGlass: Boolean read FSQLHourGlass write FSQLHourGlass
      default False;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property OnRollback: TNotifyEvent read FOnRollback write FOnRollback;
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
    property OnStartTransaction: TNotifyEvent
      read FOnStartTransaction write FOnStartTransaction;
  end;

implementation

uses ZMessages, ZClasses, ZAbstractRODataset;

var
  SqlHourGlassLock: Integer;
  CursorBackup: TDBScreenCursor;

{ TZConnection }

{**
  Constructs this component and assignes the main properties.
  @param AOwner an owner component.
}
constructor TZConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoCommit := True;
  FReadOnly := False;
  FTransactIsolationLevel := tiNone;
  FConnection := nil;
  FProperties := TStringList.Create;
  FDatasets := TList.Create;
  FLoginPrompt := False;
  FDesignConnection := False;
end;

{**
  Destroys this component and cleanups the memory.
}
destructor TZConnection.Destroy;
begin
  Disconnect;
  UnregisterAllDataSets;
  FProperties.Free;
  FDatasets.Free;
  inherited Destroy;
end;

{**
  This methode is required to support proper component initialization.
  Without it, the connection can start connecting before every property is loaded!
}
procedure TZConnection.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedConnected then
      if (csDesigning in ComponentState) or not FDesignConnection then
        SetConnected(True);
  except
    if csDesigning in ComponentState then
    {$IFNDEF VER130BELOW}
      if Assigned(Classes.ApplicationHandleException) then
        Classes.ApplicationHandleException(ExceptObject)
      else
    {$ENDIF}
        ShowException(ExceptObject, ExceptAddr)
    else
      raise;
  end;
end;

{**
  Gets an open connection flag.
  @return <code>True</code> if the connection is open
    or <code>False</code> otherwise.
}
function TZConnection.GetConnected: Boolean;
begin
  Result := (FConnection <> nil) and not FConnection.IsClosed;
end;

{**
  Sets a new open connection flag.
  @param Value <code>True</code> to open the connection
    and <code>False</code> to close it.
}
procedure TZConnection.SetConnected(Value: Boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedConnected := True
  else
  begin
    if Value <> GetConnected then
    begin
      if Value then Connect
      else Disconnect;
    end;
  end;
end;

function TZConnection.GetAffectedRowsFromLastPost: Int64;
begin
  Result := FConnection.GetAffectedRowsFromLastPost;
end;


{**
  Returns the ID of the current session
}
function TZConnection.GetThreadId: Cardinal;
begin
  Result := FConnection.GetThreadId;
end;

function TZConnection.GetEscapeString(const Value: string): string;
begin
  Result := FConnection.GetEscapeString(Value);
end;


{**
  Sets a new connection properties.
  @param Value a list with new connection properties.
}
procedure TZConnection.SetProperties(Value: TStrings);
begin
  if Value <> nil then
    FProperties.Text := Value.Text
  else FProperties.Clear;
end;

{**
  Sets autocommit flag.
  @param Value <code>True</code> to turn autocommit on.
}
procedure TZConnection.SetAutoCommit(Value: Boolean);
begin
  if FAutoCommit <> Value then
  begin
    if FExplicitTransactionCounter > 0 then
      raise Exception.Create(SInvalidOperationInTrans);
    FAutoCommit := Value;
    ShowSQLHourGlass;
    try
      if FConnection <> nil then
        FConnection.SetAutoCommit(Value);
    finally
      HideSqlHourGlass
    end;
  end;
end;

{**
  Sets transact isolation level.
  @param Value a transact isolation level.
}
procedure TZConnection.SetTransactIsolationLevel(
  Value: TZTransactIsolationLevel);
begin
  if FTransactIsolationLevel <> Value then
  begin
    FTransactIsolationLevel := Value;
    ShowSqlhourGlass;
    try
      if FConnection <> nil then
        FConnection.SetTransactionIsolation(Value);
    finally
      HideSqlHourGlass
    end;
  end;
end;

{**
  Gets a ZDBC driver for the specified protocol.
  @returns a ZDBC driver interface.
}
function TZConnection.GetDbcDriver: IZDriver;
begin
  if FConnection <> nil then
    Result := FConnection.GetDriver
  else Result := DriverManager.GetDriver(ConstructURL('', ''));
end;

{**
  Checks is the connection started a transaction.
  @returns <code>True</code> if connection in manual transaction mode
    and transaction is started.
}
function TZConnection.GetInTransaction: Boolean;
begin
  CheckConnected;
  Result := not FAutoCommit or (FExplicitTransactionCounter > 0);
end;

{**
  Gets client's full version number.
  The format of the version resturned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZConnection.GetClientVersion: Integer;
begin
 Result := DbcConnection.GetClientVersion;
end;

{**
  Gets server's full version number.
  The format of the version resturned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZConnection.GetServerVersion: Integer;
begin
 Result := DbcConnection.GetHostVersion;
end;

{**
  Constructs ZDBC connection URL string.
  @param UserName a name of the user.
  @param Password a user password.
  @returns a constructed connection URL.
}
function TZConnection.ConstructURL(const UserName, Password: string): string;
begin
  if Port <> 0 then
  begin
    Result := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s', [FProtocol, FHostName,
      FPort, FDatabase, UserName, Password]);
  end
  else
  begin
    Result := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s', [FProtocol, FHostName,
      FDatabase, UserName, Password]);
  end;
end;

{**
  Fires an event before connection open
}
procedure TZConnection.DoBeforeConnect;
begin
  if Assigned(FBeforeConnect) then
    FBeforeConnect(Self);
end;

{**
  Fires an event after connection open
}
procedure TZConnection.DoAfterConnect;
begin
  if Assigned(FAfterConnect) then
    FAfterConnect(Self);
end;

{**
  Fires an event before connection close
}
procedure TZConnection.DoBeforeDisconnect;
begin
  if Assigned(FBeforeDisconnect) then
    FBeforeDisconnect(Self);
end;

{**
  Fires an event after connection close
}
procedure TZConnection.DoAfterDisconnect;
begin
  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

{**
  Fires an event before reconnect
}
procedure TZConnection.DoBeforeReconnect;
begin
  if Assigned(FBeforeReconnect) then
    FBeforeReconnect(Self);
end;

{**
  Fires an event after reconnect
}
procedure TZConnection.DoAfterReconnect;
begin
  if Assigned(FAfterReconnect) then
    FAfterReconnect(Self);
end;

{**
  Fires an event after transaction commit
}
procedure TZConnection.DoCommit;
begin
  if Assigned(FOnCommit) then
    FOnCommit(Self);
end;

{**
  Fires an event after transaction rollback
}
procedure TZConnection.DoRollback;
begin
  if Assigned(FOnRollback) then
    FOnRollback(Self);
end;

{**
  Fires an event after transaction start
}
procedure TZConnection.DoStartTransaction;
begin
  if Assigned(FOnStartTransaction) then
    FOnStartTransaction(Self);
end;

{**
  Establish a connection with database.
}
procedure TZConnection.Connect;
var
//Local variables declared in order to preserve the original property value
//and to avoid the storage of password
  Username, Password: string;
begin
  if FConnection = nil then
  begin
    try
      DoBeforeConnect;
    except
//This is here to support aborting the Connection in BeforeConnect event without fatal errors
      on E: EAbort do
        Exit;
    end;

    UserName := FUser;
    Password := FPassword;

    if FLoginPrompt then
    begin
      { Defines user name }
      if UserName = '' then
        UserName := FProperties.Values['UID'];
      if UserName = '' then
        UserName := FProperties.Values['username'];

      { Defines user password }
      if Password = '' then
        Password := FProperties.Values['PWD'];
      if Password = '' then
        Password := FProperties.Values['password'];

      if Assigned(FOnLogin) then
        FOnLogin(Self, UserName, Password)
      else
      begin
        if Assigned(LoginDialogProc) then
        begin
          if not LoginDialogProc(FDatabase, UserName, Password) then
            Exit;
        end
        else
          raise Exception.Create(SLoginPromptFailure);
      end;
    end;

    ShowSqlHourGlass;
    try
      FConnection := DriverManager.GetConnectionWithParams(
        ConstructURL(UserName, Password), FProperties);
      try
        with FConnection do
        begin
          SetAutoCommit(FAutoCommit);
          SetReadOnly(FReadOnly);
          SetCatalog(FCatalog);
          SetTransactionIsolation(FTransactIsolationLevel);
          Open;
        end;
      except
        FConnection := nil;
        raise;
      end;
    finally
      HideSqlHourGlass;
    end;

    if not FConnection.IsClosed then
      DoAfterConnect;
  end;
end;

{**
  Closes and removes the connection with database
}
procedure TZConnection.Disconnect;
begin
  if FConnection <> nil then
  begin
    DoBeforeDisconnect;

    ShowSqlHourGlass;
    try
      CloseAllDataSets;
      FConnection.Close;
      FConnection := nil;
    finally
      HideSqlHourGlass;
    end;

    DoAfterDisconnect;
  end;
end;


{**
  Sends a ping to the server.
}
function TZConnection.Ping: Boolean; 
begin 
  Result := (FConnection <> nil) and (FConnection.PingServer=0); 
end; 


{**
  Reconnect, doesn't destroy DataSets if successful.
}
procedure TZConnection.Reconnect;
begin
  if FConnection <> nil then
  begin
    DoBeforeReconnect;

    ShowSqlHourGlass;
    try
      try
        FConnection.Close;
        FConnection.Open;
      except
        CloseAllDataSets;
        raise;
      end;
    finally
      HideSqlHourGlass;
    end;

    DoAfterReconnect;
  end;
end;

{**
  Checks if this connection is active.
}
procedure TZConnection.CheckConnected;
begin
  if FConnection = nil then
    raise EZDatabaseError.Create(SConnectionIsNotOpened);
end;

{**
  Checks if this connection is in auto-commit mode.
}
procedure TZConnection.CheckNonAutoCommitMode;
begin
  if FAutoCommit then
    raise EZDatabaseError.Create(SInvalidOpInAutoCommit);
end;

{**
  Checks if this connection is in auto-commit mode.
}
procedure TZConnection.CheckAutoCommitMode;
begin
  if not FAutoCommit and (FExplicitTransactionCounter = 0) then
    raise EZDatabaseError.Create(SInvalidOpInNonAutoCommit);
end;

{**
  Commits the current transaction.
}
procedure TZConnection.StartTransaction;
begin
  CheckAutoCommitMode;

  if FExplicitTransactionCounter = 0 then
    AutoCommit := False;
  DoStartTransaction;
  Inc(FExplicitTransactionCounter);
end;

{**
  Commits the current transaction.
}
procedure TZConnection.Commit;
var
  ExplicitTran: Boolean;
begin
  CheckConnected;
  CheckNonAutoCommitMode;

  ExplicitTran := FExplicitTransactionCounter > 0;
  if FExplicitTransactionCounter < 2 then
  //when 0 then AutoCommit was turned off, when 1 StartTransaction was used
  begin
    ShowSQLHourGlass;
    try
      try
        FConnection.Commit;
      finally
        FExplicitTransactionCounter := 0;
        if ExplicitTran then
          AutoCommit := True;
      end;
    finally
      HideSQLHourGlass;
    end;
    DoCommit;
  end
  else
    Dec(FExplicitTransactionCounter);
end;

procedure TZConnection.CommitPrepared(const transactionid: string);
var
  oldlev: TZTransactIsolationLevel;
begin
  CheckAutoCommitMode;
  oldlev := TransactIsolationLevel;
  TransactIsolationLevel := tiNone;
  FConnection.CommitPrepared(transactionid);
  TransactIsolationLevel := oldLev;
end;

{**
  Rollbacks the current transaction.
}
procedure TZConnection.Rollback;
var
  ExplicitTran: Boolean;
begin
  CheckConnected;
  CheckNonAutoCommitMode;

  ExplicitTran := FExplicitTransactionCounter > 0;
  if FExplicitTransactionCounter < 2 then
  //when 0 then AutoCommit was turned off, when 1 StartTransaction was used
  begin
    ShowSQLHourGlass;
    try
      try
        FConnection.RollBack;
      finally
        FExplicitTransactionCounter := 0;
        if ExplicitTran then
          AutoCommit := True;
      end;
    finally
      HideSQLHourGlass;
    end;
    DoRollback;
  end
  else
    Dec(FExplicitTransactionCounter);
end;

procedure TZConnection.RollbackPrepared(const transactionid: string);
var
  oldlev: TZTransactIsolationLevel;
begin
  CheckAutoCommitMode;
  oldlev := TransactIsolationLevel;
  TransactIsolationLevel := tiNone;
  FConnection.RollbackPrepared(transactionid);
  TransactIsolationLevel := oldLev;
end;

{procedure TZConnection.RollbackPrepared(transactionid: string);
begin

end;

**
  Processes component notifications.
  @param AComponent a changed component object.
  @param Operation a component operation code.
}
procedure TZConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent is TDataset) then
  begin
    UnregisterDataSet(TDataset(AComponent));
  end;
end;

function TZConnection.PingServer: Boolean;
begin
 Result := (FConnection.PingServer=0);
end;

procedure TZConnection.PrepareTransaction(const transactionid: string);
{var
  ExplicitTran: Boolean;}
begin
  CheckConnected;
  CheckNonAutoCommitMode;
  if FExplicitTransactionCounter<>1 then begin
    raise EZDatabaseError.Create(SInvalidOpPrepare);
  end;
    ShowSQLHourGlass;
    try
      try
        FConnection.PrepareTransaction(transactionid);
      finally
        FExplicitTransactionCounter := 0;
        AutoCommit := True;
      end;
    finally
      HideSQLHourGlass;
    end;
end;


{procedure TZConnection.PrepareTransaction(const transactionid: string);
begin

end;

*procedure TZConnection.PrepareTransaction(const transactionid: string);
begin

end;

*
  Closes all registered datasets.
}
procedure TZConnection.CloseAllDataSets;
var
  I: Integer;
  Current: TDataset;
begin
  for I := 0 to FDatasets.Count - 1 do
  begin
    Current := TDataset(FDatasets[I]);
    try
      Current.Close;
    except
      // Ignore.
    end;
  end;
end;

{**
  Registers a new dataset object.
  @param DataSet a new dataset to be registered.
}
procedure TZConnection.RegisterDataSet(DataSet: TDataset);
begin
  FDatasets.Add(DataSet);
end;

{**
  Unregisters a new dataset object.
  @param DataSet a new dataset to be unregistered.
}
procedure TZConnection.UnregisterDataSet(DataSet: TDataset);
begin
  FDatasets.Remove(DataSet);
end;

{**
  Unregisters a new dataset object.
  @param DataSet a new dataset to be unregistered.
}
procedure TZConnection.UnregisterAllDataSets;
var
  I: Integer;
  Current: TZAbstractRODataset;
begin
  for I := FDatasets.Count - 1 downto 0 do
  begin
    Current := TZAbstractRODataset(FDatasets[I]);
    FDatasets.Remove(Current);
    try
      Current.Connection := nil;
    except
      // Ignore.
    end;
  end;
end;

{**
  Turn on sql hourglass cursor
}
procedure TZConnection.ShowSQLHourGlass;
begin
  if not FSqlHourGlass then
    Exit;

  if SqlHourGlassLock = 0 then
  begin
    if Assigned(DBScreen) then
    begin
      CursorBackup := DBScreen.Cursor;
      if CursorBackup <> dcrOther then
        DBScreen.Cursor := dcrSQLWait;
    end;
  end;
  Inc(SqlHourGlassLock);
end;

{**
  Turn off sql hourglass cursor
}
procedure TZConnection.HideSQLHourGlass;
begin
  if not FSqlHourGlass then
    Exit;

  if SqlHourGlassLock > 0 then
    Dec(SqlHourGlassLock);
  if SqlHourGlassLock = 0 then
  begin
    if CursorBackup <> dcrOther then
      if Assigned(DBScreen) then
        DBScreen.Cursor := CursorBackup;
  end;
end;

{**
  Fills string list with registered protocol names.
  @param List a string list to fill out.
}
procedure TZConnection.GetProtocolNames(List: TStrings);
var
  I, J: Integer;
  Drivers: IZCollection;
  Driver: IZDriver;
  Protocols: TStringDynArray;
begin
  List.Clear;
  Protocols := nil; // Makes compiler happy
  Drivers := DriverManager.GetDrivers;
  for I := 0 to Drivers.Count - 1 do
  begin
    Driver := Drivers[I] as IZDriver;
    Protocols := Driver.GetSupportedProtocols;
    for J := Low(Protocols) to High(Protocols) do
      List.Add(Protocols[J]);
  end;
end;

{**
  Fills string list with catalog names.
  @param List a string list to fill out.
}
procedure TZConnection.GetCatalogNames(List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetCatalogs;
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('TABLE_CAT'));
end;

{**
  Fills string list with schema names.
  @param List a string list to fill out.
}
procedure TZConnection.GetSchemaNames(List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetSchemas;
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('TABLE_SCHEM'));
end;

{**
  Fills string list with table names.
  @param Pattern a pattern for table names.
  @param List a string list to fill out.
}
procedure TZConnection.GetTableNames(const Pattern: string; List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetTables('', '', Pattern, nil);
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('TABLE_NAME'));
end;

{**
  Fills string list with table names.
  @param tablePattern a pattern for table names.
  @param shemaPattern a pattern for shema names.
  @param List a string list to fill out.
}
procedure TZConnection.GetTableNames(const tablePattern,shemaPattern: string; List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetTables('', shemaPattern, tablePattern, nil);
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('TABLE_NAME'));
end;

{**
  Fills string list with stored procedure names.
  @param Pattern a pattern for table names.
  @param List a string list to fill out.
}
procedure TZConnection.GetStoredProcNames(const Pattern: string;
  List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetProcedures('', '', Pattern);
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('PROCEDURE_NAME'));
end;

{**
  Returns the current version of zeosdbo.
}
function TZConnection.GetVersion: string;
begin
  Result := ZEOS_VERSION;
end;

procedure TZConnection.SetVersion(const Value: string);
begin
end;

initialization
  SqlHourGlassLock := 0;
end.
