{
   Synopse SQLite3 database framework

   Sample 04 - HTTP Client-Server
     purpose of this sample is to show HTTP Client/Server SQLite3 database usage:

   - a TSQLSampleRecord class is defined in shared unit SampleData.pas
   - this sample uses two projects, Project04Client.dpr and Project04Server.dpr
   - a SQLite3 server is initialized in Project04Server
   - the CreateMissingTables method will create all necessary tables in the
     SQLite3 database
   - one or more client instances can be run in Project04Client
   - the purpose of the Client form in Unit1.pas is to add a record to the
     database; the Time field is filled with the current date and time
   - the 'Find a previous message' button show how to perform a basic query
   - since the framework use UTF-8 encoding, we use some basic functions for
     fast conversion to/from the User Interface; in real applications,
     you should better use our SQLite3i18n unit and the corresponding
     TLanguageFile.StringToUTF8() and TLanguageFile.UTF8ToString() methods
   - note that you didn't need to write any SQL statement, only define a
     class and call some methods; even the query was made very easy (just an
     obvious WHERE clause to write)
   - thanks to the true object oriented modeling of the framework, the same
     exact Unit1 is used for both static in-memory database engine, or
     with SQLite3 database storage, in local mode or in Client/Server mode:
     only the TForm1.Database object creation instance was modified
   - look at the tiny size of the EXE (even with SQLite3 engine embedded), less
     than 400KB for the server, and 80KB for the client, with LVCL :)


  Version 1.0 - February 07, 2010

  Version 1.16
  - added authentication to the remote process

}

program Project04Server;

uses
  Forms,
  Unit2 in 'Unit2.pas' {Form1},
  SampleData in '..\01 - In Memory ORM\SampleData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
