unit types_helpers;

interface

uses
  Vcl.Graphics;

type

  // === Database ===

  TNetType = (ntMySQL_TCPIP, ntMySQL_NamedPipe, ntMySQL_SSHtunnel,
    ntMSSQL_NamedPipe, ntMSSQL_TCPIP, ntMSSQL_SPX, ntMSSQL_VINES, ntMSSQL_RPC,
    ntPgSQL_TCPIP,
    ntMariaDB_TCPIP, ntMariaDB_NamedPipe, ntMariaDB_SSHtunnel);
  TNetTypeGroup = (ngMySQL, ngMSSQL, ngPgSQL, ngMariaDB);

  // === Users and Privileges ===

  TNodeObjectKind = (
    okGlobal, okSchema, okGroupTables, okGroupViews, okGroupProcedures,
    okGroupFunctions, okGroupTriggers, okGroupEvents, okTable, okView,
    okProcedure, okFunction, {okTrigger, okEvent, }okColumn
  );
  TNodeObjectKinds = set of TNodeObjectKind;

  TPrivilege = (prNone, prExecute, prProcess, prSelect, prShowDatabases,
    prShowView, prAlter, prAlterRoutine, prCreate, prCreateRoutine,
    prCreateTableSpace, prCreateTemporaryTables, prCreateView, prDelete, prDrop,
    prEvent, prIndex, prInsert, prReferences, prTrigger, prUpdate, prCreateRole,
    prCreateUser, prDropRole, prFile, prGrant, prLockTables, prReload,
    prReplicationClient, prReplicationSlave, prShutdown, prSuper);

  TPrivileges = set of TPrivilege;

  TPrivilegeHelper = record helper for TPrivilege
  public
    function AvailableForObjectKind(const AObjectKind: TNodeObjectKind): Boolean;
    function AvailableForServer(
      const ANetTypeGroup: TNetTypeGroup;
      const AVersion: Integer): Boolean;
    function ColorBG: TColor;
    function ColorFG: TColor;
    function ToString: string;
    function FromString(const AString: string): Boolean;

  const
    CL_VIOLET_FG = clPurple;
    CL_GREEN_FG  = clGreen;
    CL_RED_FG    = clMaroon;
    CL_BLUE_FG   = clNavy;
    CL_VIOLET_BG = $00FFE1FF;
    CL_GREEN_BG  = $00E1FFE1;
    CL_RED_BG    = $00E1E1FF;
    CL_BLUE_BG   = $00FFE1E1;
  end;

  TPrivilegesHelper = record helper for TPrivileges
  public
    function ToString: string;
    function FromGrant(const AGrant: string): Boolean;
    function FromString(const AString: string): Boolean;
  end;

  TUserKind = (ukGroupUsers, ukGroupRoles, ukUser, ukRole, ukHost);
  TUserChangedPart = (ucpCaption, ucpProperties, ucpPrivileges);
  TUserChangedParts = set of TUserChangedPart;
  TUserProblem = (upNone, upEmptyPassword, upInvalidPasswordLen, upSkipNameResolve, upUnknown);


implementation

uses
  helpers, SynRegExpr, System.Classes, System.SysUtils;

const
  S_REGEX_GRANTS = '^GRANT\s+(.+)\s+ON\s+((TABLE|FUNCTION|PROCEDURE)\s+)?(\*|[`"]([^`"]+)[`"])\.(\*|[`"]([^`"]+)[`"])\s+TO\s+\S+(\s+IDENTIFIED\s+BY\s+(PASSWORD)?\s+''?([^'']+)''?)?(\s+.+)?$';

{ TPrivilegeHelper }

function TPrivilegeHelper.AvailableForObjectKind(
  const AObjectKind: TNodeObjectKind): Boolean;
begin
  Result:= FALSE;
  case Self of
    prAlter:
      Result := AObjectKind in [okGlobal, okSchema, okTable];
    prAlterRoutine:
      Result := AObjectKind in [okGlobal, okSchema, okProcedure, okFunction];
    prCreate:
      Result := AObjectKind in [okGlobal, okSchema, okTable];
    prCreateRole:
      Result := AObjectKind in [okGlobal];
    prCreateRoutine:
      Result := AObjectKind in [okGlobal, okSchema, okProcedure, okFunction];
    prCreateTableSpace:
      Result := AObjectKind in [okGlobal];
    prCreateTemporaryTables:
      Result := AObjectKind in [okGlobal, okSchema];
    prCreateUser:
      Result := AObjectKind in [okGlobal];
    prCreateView:
      Result := AObjectKind in [okGlobal, okSchema, okView];
    prDelete:
      Result := AObjectKind in [okGlobal, okSchema, okTable];
    prDrop:
      Result := AObjectKind in [okGlobal, okSchema, okTable, okView];
    prDropRole:
      Result := AObjectKind in [okGlobal];
    prEvent:
      Result := AObjectKind in [okGlobal, okSchema];
    prExecute:
      Result := AObjectKind in [okGlobal, okSchema, okProcedure, okFunction];
    prFile:
      Result := AObjectKind in [okGlobal];
    prGrant:
      Result := AObjectKind in [okGlobal, okSchema, okTable, okProcedure,
        okFunction, okColumn];
    prIndex:
      Result := AObjectKind in [okGlobal, okSchema, okTable];
    prInsert:
      Result := AObjectKind in [okGlobal, okSchema, okTable, okColumn];
    prLockTables:
      Result := AObjectKind in [okGlobal, okSchema];
    prProcess:
      Result := AObjectKind in [okGlobal];
    prReferences:
      Result := AObjectKind in [okGlobal, okSchema, okTable];
    prReload:
      Result := AObjectKind in [okGlobal];
    prReplicationClient:
      Result := AObjectKind in [okGlobal];
    prReplicationSlave:
      Result := AObjectKind in [okGlobal];
    prSelect:
      Result := AObjectKind in [okGlobal, okSchema, okTable, okColumn];
    prShowDatabases:
      Result := AObjectKind in [okGlobal];
    prShowView:
      Result := AObjectKind in [okGlobal, okSchema, okView];
    prShutdown:
      Result := AObjectKind in [okGlobal];
    prSuper:
      Result := AObjectKind in [okGlobal];
    prTrigger:
      Result := AObjectKind in [okGlobal, okSchema];
    prUpdate:
      Result := AObjectKind in [okGlobal, okSchema, okTable, okColumn];
  end;
end;

function TPrivilegeHelper.AvailableForServer(const ANetTypeGroup: TNetTypeGroup;
  const AVersion: Integer): Boolean;
var
  NTG: TNetTypeGroup;
  V: Integer;
begin
  Result:= FALSE;
  NTG:= ANetTypeGroup;
  V:= AVersion;
  case Self of
    // TODO: Add other supported servers and their min versions!

    prAlter:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prAlterRoutine:
      Result := ((NTG = ngMySQL) and (V >= 50003)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prCreate:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prCreateRole:
      Result := ((NTG = ngMySQL) and (V >= 80000)) or
                ((NTG = ngMariaDB) and (FALSE)); // Not supported in MariaDB! Here we have to have CREATE USER privilege.
    prCreateRoutine:
      Result := ((NTG = ngMySQL) and (V >= 50003)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prCreateTableSpace:
      Result := ((NTG = ngMySQL) and (V >= 50404)) or
                ((NTG = ngMariaDB) and (FALSE));
    prCreateTemporaryTables:
      Result := ((NTG = ngMySQL) and (V >= 40002)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prCreateUser:
      Result := ((NTG = ngMySQL) and (V >= 50003)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prCreateView:
      Result := ((NTG = ngMySQL) and (V >= 50001)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prDelete:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prDrop:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prDropRole:
      Result := ((NTG = ngMySQL) and (V >= 80000)) or
                ((NTG = ngMariaDB) and (FALSE)); // Not supported in MariaDB! Here we have to have CREATE USER privilege.
    prEvent:
      Result := ((NTG = ngMySQL) and (V >= 50106)) or
                ((NTG = ngMariaDB) and (V >= 50106));
    prExecute:
      Result := ((NTG = ngMySQL) and (V >= 40002)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prFile:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prGrant:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prIndex:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prInsert:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prLockTables:
      Result := ((NTG = ngMySQL) and (V >= 40002)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prProcess:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prReferences:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prReload:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prReplicationClient:
      Result := ((NTG = ngMySQL) and (V >= 40002)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prReplicationSlave:
      Result := ((NTG = ngMySQL) and (V >= 40002)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prSelect:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prShowDatabases:
      Result := ((NTG = ngMySQL) and (V >= 40002)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prShowView:
      Result := ((NTG = ngMySQL) and (V >= 50001)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prShutdown:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prSuper:
      Result := ((NTG = ngMySQL) and (V >= 40002)) or
                ((NTG = ngMariaDB) and (V >= 50100));
    prTrigger:
      Result := ((NTG = ngMySQL) and (V >= 50106)) or
                ((NTG = ngMariaDB) and (V >= 50106));
    prUpdate:
      Result := ((NTG = ngMySQL) and (V >= 0)) or
                ((NTG = ngMariaDB) and (V >= 50100));
  end;
end;

function TPrivilegeHelper.ColorBG: TColor;
begin
  Result:= clNone;
  case Self of
      prExecute:                Result:= CL_GREEN_BG;
      prProcess:                Result:= CL_GREEN_BG;
      prSelect:                 Result:= CL_GREEN_BG;
      prShowDatabases:          Result:= CL_GREEN_BG;
      prShowView:               Result:= CL_GREEN_BG;
      prAlter:                  Result:= CL_RED_BG;
      prAlterRoutine:           Result:= CL_RED_BG;
      prCreate:                 Result:= CL_RED_BG;
      prCreateRoutine:          Result:= CL_RED_BG;
      prCreateTableSpace:       Result:= CL_RED_BG;
      prCreateTemporaryTables:  Result:= CL_RED_BG;
      prCreateView:             Result:= CL_RED_BG;
      prDelete:                 Result:= CL_RED_BG;
      prDrop:                   Result:= CL_RED_BG;
      prEvent:                  Result:= CL_RED_BG;
      prIndex:                  Result:= CL_RED_BG;
      prInsert:                 Result:= CL_RED_BG;
      prReferences:             Result:= CL_RED_BG;
      prTrigger:                Result:= CL_RED_BG;
      prUpdate:                 Result:= CL_RED_BG;
      prCreateRole:             Result:= CL_BLUE_BG;
      prCreateUser:             Result:= CL_BLUE_BG;
      prDropRole:               Result:= CL_BLUE_BG;
      prFile:                   Result:= CL_BLUE_BG;
      prGrant:                  Result:= CL_BLUE_BG;
      prLockTables:             Result:= CL_BLUE_BG;
      prReload:                 Result:= CL_BLUE_BG;
      prReplicationClient:      Result:= CL_BLUE_BG;
      prReplicationSlave:       Result:= CL_BLUE_BG;
      prShutdown:               Result:= CL_BLUE_BG;
      prSuper:                  Result:= CL_BLUE_BG;
  end;
end;

function TPrivilegeHelper.ColorFG: TColor;
begin
  Result:= clNone;
  case Self of
      prExecute:                Result:= CL_GREEN_FG;
      prProcess:                Result:= CL_GREEN_FG;
      prSelect:                 Result:= CL_GREEN_FG;
      prShowDatabases:          Result:= CL_GREEN_FG;
      prShowView:               Result:= CL_GREEN_FG;
      prAlter:                  Result:= CL_RED_FG;
      prAlterRoutine:           Result:= CL_RED_FG;
      prCreate:                 Result:= CL_RED_FG;
      prCreateRoutine:          Result:= CL_RED_FG;
      prCreateTableSpace:       Result:= CL_RED_FG;
      prCreateTemporaryTables:  Result:= CL_RED_FG;
      prCreateView:             Result:= CL_RED_FG;
      prDelete:                 Result:= CL_RED_FG;
      prDrop:                   Result:= CL_RED_FG;
      prEvent:                  Result:= CL_RED_FG;
      prIndex:                  Result:= CL_RED_FG;
      prInsert:                 Result:= CL_RED_FG;
      prReferences:             Result:= CL_RED_FG;
      prTrigger:                Result:= CL_RED_FG;
      prUpdate:                 Result:= CL_RED_FG;
      prCreateRole:             Result:= CL_BLUE_FG;
      prCreateUser:             Result:= CL_BLUE_FG;
      prDropRole:               Result:= CL_BLUE_FG;
      prFile:                   Result:= CL_BLUE_FG;
      prGrant:                  Result:= CL_BLUE_FG;
      prLockTables:             Result:= CL_BLUE_FG;
      prReload:                 Result:= CL_BLUE_FG;
      prReplicationClient:      Result:= CL_BLUE_FG;
      prReplicationSlave:       Result:= CL_BLUE_FG;
      prShutdown:               Result:= CL_BLUE_FG;
      prSuper:                  Result:= CL_BLUE_FG;
  end;
end;

function TPrivilegeHelper.FromString(const AString: string): Boolean;
begin
  Self:= prNone;
  if CompareText(AString, 'ALTER') = 0 then
    Self:= prAlter
  else if CompareText(AString, 'ALTER ROUTINE') = 0 then
    Self:= prAlterRoutine
  else if CompareText(AString, 'CREATE') = 0 then
    Self:= prCreate
  else if CompareText(AString, 'CREATE ROLE') = 0 then
    Self:= prCreateRole
  else if CompareText(AString, 'CREATE ROUTINE') = 0 then
    Self:= prCreateRoutine
  else if CompareText(AString, 'CREATE TABLESPACE') = 0 then
    Self:= prCreateTableSpace
  else if CompareText(AString, 'CREATE TEMPORARY TABLES') = 0 then
    Self:= prCreateTemporaryTables
  else if CompareText(AString, 'CREATE USER') = 0 then
    Self:= prCreateUser
  else if CompareText(AString, 'CREATE VIEW') = 0 then
    Self:= prCreateView
  else if CompareText(AString, 'DELETE') = 0 then
    Self:= prDelete
  else if CompareText(AString, 'DROP') = 0 then
    Self:= prDrop
  else if CompareText(AString, 'DROP ROLE') = 0 then
    Self:= prDropRole
  else if CompareText(AString, 'EVENT') = 0 then
    Self:= prEvent
  else if CompareText(AString, 'EXECUTE') = 0 then
    Self:= prExecute
  else if CompareText(AString, 'FILE') = 0 then
    Self:= prFile
  else if CompareText(AString, 'GRANT') = 0 then
    Self:= prGrant
  else if CompareText(AString, 'INDEX') = 0 then
    Self:= prIndex
  else if CompareText(AString, 'INSERT') = 0 then
    Self:= prInsert
  else if CompareText(AString, 'LOCK TABLES') = 0 then
    Self:= prLockTables
  else if CompareText(AString, 'PROCESS') = 0 then
    Self:= prProcess
  else if CompareText(AString, 'REFERENCES') = 0 then
    Self:= prReferences
  else if CompareText(AString, 'RELOAD') = 0 then
    Self:= prReload
  else if CompareText(AString, 'REPLICATION CLIENT') = 0 then
    Self:= prReplicationClient
  else if CompareText(AString, 'REPLICATION SLAVE') = 0 then
    Self:= prReplicationSlave
  else if CompareText(AString, 'SELECT') = 0 then
    Self:= prSelect
  else if CompareText(AString, 'SHOW DATABASES') = 0 then
    Self:= prShowDatabases
  else if CompareText(AString, 'SHOW VIEW') = 0 then
    Self:= prShowView
  else if CompareText(AString, 'SHUTDOWN') = 0 then
    Self:= prShutdown
  else if CompareText(AString, 'SUPER') = 0 then
    Self:= prSuper
  else if CompareText(AString, 'TRIGGER') = 0 then
    Self:= prTrigger
  else if CompareText(AString, 'UPDATE') = 0 then
    Self:= prUpdate;
  Result:= (Self <> prNone);
end;

function TPrivilegeHelper.ToString: string;
begin
  case Self of
    prAlter:                  Result:= 'ALTER';
    prAlterRoutine:           Result:= 'ALTER ROUTINE';
    prCreate:                 Result:= 'CREATE';
    prCreateRole:             Result:= 'CREATE ROLE';
    prCreateRoutine:          Result:= 'CREATE ROUTINE';
    prCreateTableSpace:       Result:= 'CREATE TABLE SPACE';
    prCreateTemporaryTables:  Result:= 'CREATE TEMPORARY TABLES';
    prCreateUser:             Result:= 'CREATE USER';
    prCreateView:             Result:= 'CREATE VIEW';
    prDelete:                 Result:= 'DELETE';
    prDrop:                   Result:= 'DROP';
    prDropRole:               Result:= 'DROP ROLE';
    prEvent:                  Result:= 'EVENT';
    prExecute:                Result:= 'EXECUTE';
    prFile:                   Result:= 'FILE';
    prGrant:                  Result:= 'GRANT';
    prIndex:                  Result:= 'INDEX';
    prInsert:                 Result:= 'INSERT';
    prLockTables:             Result:= 'LOCK TABLES';
    prProcess:                Result:= 'PROCESS';
    prReferences:             Result:= 'REFERENCES';
    prReload:                 Result:= 'RELOAD';
    prReplicationClient:      Result:= 'REPLICATION CLIENT';
    prReplicationSlave:       Result:= 'REPLICATION SLAVE';
    prSelect:                 Result:= 'SELECT';
    prShowDatabases:          Result:= 'SHOW DATABASES';
    prShowView:               Result:= 'SHOW VIEW';
    prShutdown:               Result:= 'SHUTDOWN';
    prSuper:                  Result:= 'SUPER';
    prTrigger:                Result:= 'TRIGGER';
    prUpdate:                 Result:= 'UPDATE';
  end;
end;

{ TPrivilegesHelper }

function TPrivilegesHelper.FromGrant(const AGrant: string): Boolean;
var
  R: TRegExpr;
  S: string;
begin
  Result:= FALSE;
  R := TRegExpr.Create;
  try
    R.ModifierI:= True;
    R.Expression:= S_REGEX_GRANTS;
    if R.Exec(AGrant) then begin
      S:= R.Match[1];
      if S.Length > 0 then begin
        Result:= Self.FromString(S);
      end;
    end;
  finally
    FreeAndNil(R);
  end;
end;

function TPrivilegesHelper.FromString(const AString: string): Boolean;
var
  P: TPrivilege;
  S: string;
  SL: TStringList;
begin
  Result:= FALSE;
  if CompareText(AString.Trim, 'ALL PRIVILEGES') = 0 then begin
    for P:= Low(TPrivilege) to High(TPrivilege) do begin
      Self:= Self + [P] - [prNone];
    end;
    Result:= TRUE;
  end else begin
    SL:= Explode(',', AString);
    try
      for S in SL do begin
        if P.FromString(S.Trim) then begin
          Self:= Self + [P] - [prNone];
        end;
      end;
      Result:= (SL.Count > 0);
    finally
      FreeAndNil(SL);
    end;
  end;
end;

function TPrivilegesHelper.ToString: string;
var
  P: TPrivilege;
  S: string;
begin
  Result:= '';
  S:= '';
  for P in Self do begin
    S:= S + ',' + P.ToString;
  end;
  Result:= Copy(S, 2, Length(S));
end;

end.
