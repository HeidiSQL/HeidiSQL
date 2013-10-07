/// automated tests for common units of the Synopse mORMot Framework
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3SelfTests;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.16
  - first public release, corresponding to SQLite3 Framework 1.16
  - all mORMot tests are now implemented in this separated unit: this is
    requested by bugs in the Delphi XE2 background compilers: main compiler
    was OK with our code (i.e. it compiles into .exe and run as expected), but
    background IDE compilers (used e.g. for syntax checking) was not able
    to compile the tests within the main .dpr source code

  Version 1.17
  - fixed LVCL and Delphi 5 compilation issues

}

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64

/// this is the main entry point of the tests
// - this procedure will create a console, then run all available tests
procedure SQLite3ConsoleTests;


implementation

uses
  Windows,
  Classes,
  SynLZ,
  SynLZO,
  SynCrypto,
  SynCrtSock,
  SynCommons,
  SynSQLite3,
{$ifndef DELPHI5OROLDER}
{$ifndef CPU64}
{$ifndef FPC}
  SQLite3Commons,
  SynBigTable,
  SQLite3,
  SQLite3FastCgiServer,
  SQLite3HttpClient,
  SQLite3HttpServer,
  SQLite3Service,
  //SQLite3BigTable,
{$endif FPC}
{$endif CPU64}
{$endif DELPHI5OROLDER}
{$ifndef FPC}
{$ifndef LVCL}
  SynPdf,          
  Contnrs,
  SynGdiPlus,
  SynDB,
  SynOleDB,
  SynDBOracle,
  SynDBODBC,
  SynDBSQLite3,
{$ifndef FPC}
{$ifndef DELPHI5OROLDER}
{$ifndef CPU64}
  SQLite3DB,
{$endif CPU64}
{$endif DELPHI5OROLDER}
{$endif FPC}
{$endif LVCL}
  //SynDBODBC,
{$endif FPC}
  SynZip,
  SynSelfTests;

type
  /// Synopse mORMot Framework unitary testing
  // - this class will launch all available SW tests for the freeware Synopse
  // mORMot framework
  // - inherits from TSynTestsLogged in order to create enhanced log information
  // in case of any test case failure
  TTestSynopsemORMotFramework = class(TSynTestsLogged)
  published
    /// test the freeware Synopse library
    // - low level functions and classes, cryptographic or compression routines,
    // PDF generation
    procedure SynopseLibraries;
{$ifndef FPC}
{$ifndef CPU64}
{$ifndef DELPHI5OROLDER}
    /// test the freeware Synopse mORMot framework
    // - access to SQLite3 or external engines, ORM features, Client/Server
    procedure _mORMot;
{$endif}
{$endif}
{$endif}
  end;


{ TTestSynopsemORMotFramework }

procedure TTestSynopsemORMotFramework.SynopseLibraries;
begin
  //exit;
  AddCase([TTestLowLevelCommon,
{$ifndef FPC}
{$ifndef CPU64}
{$ifndef DELPHI5OROLDER}
    TTestLowLevelTypes, TTestBigTable,
{$endif}
{$endif}
{$endif}
    TTestCryptographicRoutines, TTestCompression
{$ifndef FPC}
{$ifndef LVCL}
    , TTestSynopsePDF
{$endif}
{$endif} ]);
end;

{$ifdef FPC}
type // SQLite3Commons unit doesn't compile with FPC yet
  TSQLLog = TSynLog;
{$else}
{$ifdef CPU64}
type // SQLite3Commons unit doesn't compile with Delphi 64 bit yet
  TSQLLog = TSynLog;
{$else}
{$ifdef DELPHI5OROLDER}
type // SQLite3Commons unit doesn't compile with Delphi 5 yet
  TSQLLog = TSynLog;
{$else}
procedure TTestSynopsemORMotFramework._mORMot;
begin
  AddCase([TTestBasicClasses,TTestFileBased,TTestFileBasedWAL,TTestMemoryBased]);
  AddCase([TTestClientServerAccess]);
  AddCase([TTestServiceOrientedArchitecture]);
  //exit;
{$ifndef LVCL}
  AddCase([TTestExternalDatabase]);
{$endif}
end;
{$endif CPU64}
{$endif DELPHI5OROLDER}
{$endif FPC}

procedure SQLite3ConsoleTests;
begin
  AllocConsole;
  // will create around 280 MB of log file, if executed
  if false then
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    HighResolutionTimeStamp := true;
    TSynLogTestLog := TSQLLog;
    {$ifndef FPC}
    {$ifndef LVCL}
    SynDBLog := TSQLLog;
    {$endif}
    {$endif}
    {$ifdef WITHLOG}
    {$ifndef FPC}
    {$ifndef DELPHI5OROLDER}
    {$ifndef CPU64}
    SQLite3Log := TSQLLog;
    SynSQLite3Log := TSQLLog;
    {$endif}
    {$endif}
    {$endif}
    {$endif}
  end;
  // testing is performed by some dedicated classes defined in the above units
  with TTestSynopsemORMotFramework.Create('Synopse mORMot Framework Automated tests') do
  try
    if ParamCount<>0 then begin
      SaveToFile(paramstr(1)); // DestPath on command line -> export to file
      Writeln(Ident,#13#10#13#10' Running tests... please wait');
    end;
    Run;
    if ParamCount<>0 then
      exit; // direct exit if an external file was generated
  finally
    Free;
  end;
  WriteLn('Done - Press ENTER to Exit');
  ReadLn;
end;

end.

