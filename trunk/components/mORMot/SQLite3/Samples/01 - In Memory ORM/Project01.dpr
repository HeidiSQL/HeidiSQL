{
   Synopse SQLite3 database framework

   Sample 01 - In Memory ORM
     purpose of this sample is to show the basic ORM usage of the framework:

   - a TRecord class is defined in Unit1.pas
   - a static server (i.e. in-memory database) is initialized (see
     TSQLRestServerStatic.Create below); 
     it will store the data in a JSON file in the disk and won't require
     the SQLite3 database engine
   - the purpose of the form in Unit1.pas is to add a record to the
     database; the Time field is filled with the current date and time
   - the 'Find a previous message' button show how to perform a basic query
   - on application quit, the Database.Destroy will update the JSON file
   - since the framework use UTF-8 encoding, we use some basic functions for
     fast conversion to/from the User Interface; in real applications,
     you should better use our SQLite3i18n unit and the corresponding
     TLanguageFile.StringToUTF8() and TLanguageFile.UTF8ToString() methods
   - note the tiny size of the EXE (since we don't use SQLite3), less than
     80KB with LVCL :)

  Version 1.0 - January 24, 2010
    - Initial Release

  Version 1.1 - April 14, 2011
    - use TSQLRestServerStaticInMemory instead of abstract TSQLRestServerStatic

}

program Project01;

uses
  Forms,
  SysUtils,
  SQLite3Commons,
  Unit1 in 'Unit1.pas' {Form1},
  SampleData in 'SampleData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Form1.Caption := ' Sample 01 - In Memory ORM';
  Form1.Database := TSQLRestServerStaticInMemory.Create(TSQLSampleRecord,nil,
    ChangeFileExt(paramstr(0),'.db'));
  Application.Run;
end.
