unit mssqlfixed;

// SQLDB TMSSQLConnection with access to all result sets of a batch.
// TMSSQLConnection.Fetch as shipped with FPC 3.2.2 discards all result sets after the first one,
// so "SELECT 1; SELECT 2" sent in one go returned only one result.
//
// Usage: BeginResults, execute the batch with ExecSQL on a prepared query, then open one query
// per result set while NextResult returns True, and finally EndResults. In between, no other
// statement must be sent through this connection.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, MSSQLConn, dblib;

type

  { TFixedMSSQLConnection }

  TFixedMSSQLConnection = class(TMSSQLConnection)
  private
    FKeepResults: Boolean;   // Between BeginResults and EndResults
    FPending: Boolean;       // Batch has a current result set, maybe followed by more
    FAdoptResult: Boolean;   // Next Execute takes over the current result set instead of sending SQL
    FRowsDone: Boolean;      // All rows of the current result set fetched
    procedure DiscardResults(Cancel: Boolean);
  protected
    procedure DoInternalConnect; override;
    procedure UnPrepareStatement(cursor: TSQLCursor); override;
    procedure FreeFldBuffers(cursor: TSQLCursor); override;
    procedure Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams); override;
    function Fetch(cursor: TSQLCursor): boolean; override;
  public
    // Keep further result sets after the first one
    procedure BeginResults;
    // Discard result sets not yet taken over, and continue as usual
    procedure EndResults;
    // Move to the next result set. If True, the next opened query takes it over.
    // Adds affected rows of statements without result set in between.
    function NextResult(var AffectedRows: Int64): Boolean;
    // Number and text of server errors from the last statement or result set, without
    // the generic DB-Library message "General SQL Server error". Empty if none.
    function ServerErrorNo: Integer;
    function ServerErrorText: String;
  end;

implementation

var
  PrevErrHandler: DBERRHANDLE_PROC = nil;
  PrevMsgHandler: DBMSGHANDLE_PROC = nil;
  // Messages since the last result set, mssqlconn keeps its own copies private
  LastErrorStr, LastMsgStr: String;
  LastErrorNo: Integer;
  // Server messages with error severity
  LastServerErrorStr: String;
  LastServerErrorNo: Integer;

function ChainedErrHandler(dbproc: PDBPROCESS; severity, dberr, oserr: INT; dberrstr, oserrstr: PAnsiChar): INT; cdecl;
begin
  LastErrorStr := LastErrorStr + sLineBreak + dberrstr;
  LastErrorNo := dberr;
  Result := PrevErrHandler(dbproc, severity, dberr, oserr, dberrstr, oserrstr);
end;

function ChainedMsgHandler(dbproc: PDBPROCESS; msgno: DBINT; msgstate, severity: INT; msgtext, srvname, procname: PAnsiChar; line: DBUSMALLINT): INT; cdecl;
begin
  LastMsgStr := LastMsgStr + sLineBreak + msgtext;
  // Severity 0 to 10 are informational messages, e.g. from PRINT or USE
  if severity > 10 then begin
    if LastServerErrorStr = '' then
      LastServerErrorNo := msgno
    else
      LastServerErrorStr := LastServerErrorStr + sLineBreak;
    LastServerErrorStr := LastServerErrorStr + msgtext;
  end;
  Result := PrevMsgHandler(dbproc, msgno, msgstate, severity, msgtext, srvname, procname, line);
end;

procedure ResetMessages;
begin
  LastErrorStr := '';
  LastMsgStr := '';
  LastErrorNo := 0;
  LastServerErrorStr := '';
  LastServerErrorNo := 0;
end;


{ TFixedMSSQLConnection }

procedure TFixedMSSQLConnection.DoInternalConnect;
var
  PrevErr: DBERRHANDLE_PROC;
  PrevMsg: DBMSGHANDLE_PROC;
begin
  ResetMessages;
  inherited;
  // Wrap the handlers which mssqlconn installs on its first connect
  PrevErr := dberrhandle(@ChainedErrHandler);
  if PrevErr <> @ChainedErrHandler then
    PrevErrHandler := PrevErr;
  PrevMsg := dbmsghandle(@ChainedMsgHandler);
  if PrevMsg <> @ChainedMsgHandler then
    PrevMsgHandler := PrevMsg;
end;

procedure TFixedMSSQLConnection.BeginResults;
begin
  FKeepResults := True;
  FPending := False;
  FAdoptResult := False;
  FRowsDone := False;
end;

procedure TFixedMSSQLConnection.EndResults;
begin
  if FPending then
    DiscardResults(True);
  FKeepResults := False;
  FAdoptResult := False;
end;

procedure TFixedMSSQLConnection.DiscardResults(Cancel: Boolean);
begin
  FPending := False;
  FRowsDone := False;
  if Cancel then
    dbcancel(PDBPROCESS(Handle));
  try
    // Resets the private fetch status of TMSSQLConnection, which would otherwise cancel
    // the next statement in UnPrepareStatement
    inherited Fetch(nil);
  except
    // Errors of discarded results are not of interest
    on E:EDatabaseError do;
  end;
end;

procedure TFixedMSSQLConnection.UnPrepareStatement(cursor: TSQLCursor);
begin
  if FKeepResults then begin
    // Inherited method cancels all pending result sets
    cursor.FPrepared := False;
  end else
    inherited;
end;

procedure TFixedMSSQLConnection.FreeFldBuffers(cursor: TSQLCursor);
begin
  // Inherited method cancels all pending result sets, and TSQLConnection's one is empty
  if not FKeepResults then
    inherited;
end;

procedure TFixedMSSQLConnection.Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams);
begin
  if FAdoptResult then begin
    // Current result set was found by inherited Execute or NextResult
    FAdoptResult := False;
    FRowsDone := False;
    cursor.FSelectable := True;
  end else begin
    ResetMessages;
    if FKeepResults then begin
      FPending := False;
      FRowsDone := False;
    end;
    inherited;
    if FKeepResults then begin
      FPending := cursor.FSelectable;
      FAdoptResult := FPending;
    end;
  end;
end;

function TFixedMSSQLConnection.Fetch(cursor: TSQLCursor): boolean;
var
  St: STATUS;
begin
  if not (FKeepResults and FPending) then
    Exit(inherited Fetch(cursor));
  // Same as inherited, but leaves remaining result sets for NextResult
  repeat
    St := dbnextrow(PDBPROCESS(Handle));
    Result := St = REG_ROW;
  until Result or (St = NO_MORE_ROWS) or (St = FAIL);
  if St = FAIL then begin
    // Network failure or similar. Inherited method raises the error.
    FPending := False;
    Result := inherited Fetch(cursor);
  end else if St = NO_MORE_ROWS then
    FRowsDone := True;
end;

function TFixedMSSQLConnection.NextResult(var AffectedRows: Int64): Boolean;
var
  h: PDBPROCESS;
  Res: RETCODE;
  St: STATUS;
  ErrorText: String;
  ErrorNo: Integer;
begin
  Result := False;
  if not (FKeepResults and FPending) then
    Exit;
  h := PDBPROCESS(Handle);
  if not FRowsDone then begin
    // Rows of the current result set were not fetched completely
    repeat
      St := dbnextrow(h);
    until (St = NO_MORE_ROWS) or (St = FAIL);
  end;
  FRowsDone := False;
  ResetMessages;
  repeat
    Res := dbresults(h);
    if Res = SUCCEED then begin
      if dbcmdrow(h) = SUCCEED then begin
        FAdoptResult := True;
        Exit(True);
      end;
      // Statement without result set
      repeat
        St := dbnextrow(h);
      until (St = NO_MORE_ROWS) or (St = FAIL);
      if (not Assigned(dbiscount)) or dbiscount(h) then
        if dbcount(h) > 0 then
          Inc(AffectedRows, dbcount(h));
    end;
  until Res <> SUCCEED;

  // NO_MORE_RESULTS, or FAIL from a failed statement
  ErrorText := Trim(LastErrorStr);
  if LastMsgStr.Trim <> '' then
    ErrorText := ErrorText + sLineBreak + Trim(LastMsgStr);
  ErrorNo := LastErrorNo;
  DiscardResults(Res <> NO_MORE_RESULTS);
  if Res = FAIL then
    raise EMSSQLDatabaseError.CreateFmt('Error %d : %s', [ErrorNo, ErrorText], Self, ErrorNo, '');
end;

function TFixedMSSQLConnection.ServerErrorNo: Integer;
begin
  Result := LastServerErrorNo;
end;

function TFixedMSSQLConnection.ServerErrorText: String;
begin
  Result := LastServerErrorStr;
end;

end.
