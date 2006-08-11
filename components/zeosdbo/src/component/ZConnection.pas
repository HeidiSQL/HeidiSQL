{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Database Connection Component              }
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
  SysUtils, Classes, ZDbcIntfs, DB, ZCompatibility;

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

    function ConstructURL(UserName: string; Password: string): string;

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
    function Ping: Boolean;

    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    procedure RegisterDataSet(DataSet: TDataset);
    procedure UnregisterDataSet(DataSet: TDataset);

    procedure GetProtocolNames(List: TStrings);
    procedure GetCatalogNames(List: TStrings);
    procedure GetSchemaNames(List: TStrings);
    procedure GetTableNames(const Pattern: string; List: TStrings);
    procedure GetStoredProcNames(const Pattern: string; List: TStrings);

    property InTransaction: Boolean read GetInTransaction;

    property DbcDriver: IZDriver read GetDbcDriver;
    property DbcConnection: IZConnection read FConnection;

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

function TZConnection.Ping: Boolean;
begin
  Result := (FConnection <> nil) and FConnection.Ping;
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
  Constructs ZDBC connection URL string.
  @param UserName a name of the user.
  @param Password a user password.
  @returns a constructed connection URL.
}
function TZConnection.ConstructURL(UserName: string; Password: string): string;
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

{**
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

{**
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
