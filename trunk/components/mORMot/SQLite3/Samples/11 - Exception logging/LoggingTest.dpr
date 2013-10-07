{
  Synopse SQLite3 database framework

  Sample 11 - Exception logging
    purpose of this sample is to show basic logging mechanism of the framework

  TO HAVE LINE NUMBERS IN THE LOG FILE:
  - Go to Project/Options then set the Linker/File map setting to "Detailed"

  Version 1.0 - April 14, 2011
    - Initial Release

}
program LoggingTest;

uses
  Windows,
  SysUtils,
  //SynZip,
{$ifdef CONDITIONALEXPRESSIONS}
  SQLite3Commons,
{$endif}
  SynCommons;

type
  /// a class just to show how methods are handled
  TTestLogClass = class
  protected
    procedure TestLog;
  end;

  /// a custom exception used to show how Delphi exception are handled and
  // can be ignored on request
  ECustomException = class(Exception);

{$ifndef CONDITIONALEXPRESSIONS}
  // SQLite3Commons doesn't compile under Delphi 5 (yet)
  TSQLLog = TSynLog;
{$endif}

var
  TestLevel: TSynLogInfo = high(TSynLogInfo);

const
  sTestLevel: PWinAnsiChar = 'TestLevel';
  sTestSet: PWinAnsiChar = 'set';

procedure TTestLogClass.TestLog;
var ILog: ISynLog;
    S: TSynLogInfos;
begin
  ILog := TSQLLog.Enter(self);
  // do some stuff
  ILog.Log(sllCustom1);
  ILog.Log(sllInfo,sTestLevel,TypeInfo(TSynLogInfo),TestLevel);
  ILog.Log(sllInfo,sTestSet,TypeInfo(TSynLogInfos),S);
  ILog.Log(sllDebug,ILog.Instance);
  if TestLevel=low(TestLevel) then
    TTestLogClass(nil).ClassName; // will raise an access violation
  dec(TestLevel);
  TestLog;
end;

procedure TestLogProc;
var ILog: ISynLog;
begin
  ILog := TSQLLog.Enter;
  ILog.Log(sllDebug,'GarbageCollector',GarbageCollector);
  ILog.Log(sllDebug,GarbageCollector);
end;


procedure TestsLog;

{$ifdef CONDITIONALEXPRESSIONS}
  // SQLite3Commons doesn't compile under Delphi 5 (yet)
  procedure TestPeopleProc;
  var People: TSQLRecordPeople;
      Log: ISynLog;
  begin
    Log := TSQLLog.Enter;
    People := TSQLRecordPeople.Create;
    try
      People.ID := 16;
      People.FirstName := 'Louis';
      People.LastName := 'Croivébaton';
      People.YearOfBirth := 1754;
      People.YearOfDeath := 1793;
      Log.Log(sllInfo,People);
    finally
      People.Free;
    end;
  end;
{$endif}

  procedure Proc2(n1, n2: Integer); forward; // test nested

  procedure Proc1(n1, n2: Integer);
  begin
    if n1 = 0 then
    try
      TTestLogClass(nil).ClassName; // will raise an access violation
    except
      on E: Exception do
        TSQLLog.Add.Log(sllInfo,'^^^^^^^^ recursion test Proc1',e);
    end else
    Proc2(n1 - 1, n2);
  end;

  procedure Proc2(n1, n2: Integer);
  begin
    if n2 = 0 then
      try
        TTestLogClass(nil).ClassName; // will raise an access violation
      except
        on E: Exception do
          TSQLLog.Add.Log(sllInfo,'^^^^^^^^ recursion test Proc2',e);
      end else
      Proc1(n1, n2 - 1);
  end;

var dummy: integer;
begin
  // first, set the TSQLLog family parameters
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    //Level := [sllException,sllExceptionOS];
    //PerThreadLog := true;
    //HighResolutionTimeStamp := true;
    //AutoFlushTimeOut := 5;
    OnArchive := EventArchiveSynLZ;
    //OnArchive := EventArchiveZip;
    ArchiveAfterDays := 1; // archive after one day
  end;
  // try some low-level common exceptions
  try
    dummy := 0;
    if 10 div dummy=0 then; // will raise EDivByZero
  except
    on E: exception do
      TSQLLog.Add.Log(sllStackTrace,'^^^^^^^^ the first sample, divide by 0',E);
  end;
  try
    readln; // will raise EIOError (no console is available to read from)
  except
    on E: exception do
      TSQLLog.Add.Log(sllStackTrace,'^^^^^^^^ the next sample, I/O error',E);
  end;
  // try EAccessViolation in nested procedure calls (see stack trace)
  Proc1(5,7);
  Proc2(7,5);
  // try a method recursive call, with an EAccessViolation raised within
  with TTestLogClass.Create do
  try
    try
      TestLog;
    except
      on Exception do; // just ignore now
    end;
  finally
    Free;
  end;
  // try a procedure call with Enter/Auto-Leave
  TestLogProc;
{$ifdef CONDITIONALEXPRESSIONS}
  // try a procedure call with Enter/Auto-Leave and a TSQLRecordPeople logging
  TestPeopleProc;
{$endif}
  // try a custom Delphi exception
  try
    raise ECustomException.Create('Test exception'); // logged to TSQLLog
  except
    on E: Exception do
      TSQLLog.Add.Log(sllInfo,'^^^^^^^^  custom exception type',E);
  end;
  // try a custom Delphi exception after been marked as to be ignored
  TSQLLog.Family.ExceptionIgnore.Add(ECustomException);
  try
    raise ECustomException.Create('Test exception');
  except
    on E: Exception do
      TSQLLog.Add.Log(sllInfo,'^^^^^^^^  nothing should be logged just above',E);
  end;
end;

begin
  TestsLog;
end.

