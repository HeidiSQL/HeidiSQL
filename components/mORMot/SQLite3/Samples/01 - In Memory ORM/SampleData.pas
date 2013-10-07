/// it's a good practice to put all data definition into a stand-alone unit
// - this unit will be shared between client and server
unit SampleData;

interface

uses
  SynCommons,
  SQLite3Commons;

type
  /// here we declare the class containing the data
  // - it just has to inherits from TSQLRecord, and the published
  // properties will be used for the ORM (and all SQL creation)
  // - the beginning of the class name must be 'TSQL' for proper table naming
  // in client/server environnment
  TSQLSampleRecord = class(TSQLRecord)
  private
    fQuestion: RawUTF8;
    fName: RawUTF8;
    fTime: TModTime;
  published
    property Time: TModTime read fTime write fTime;
    property Name: RawUTF8 read fName write fName;
    property Question: RawUTF8 read fQuestion write fQuestion;
  end;

/// an easy way to create a database model for client and server
function CreateSampleModel: TSQLModel;


implementation

function CreateSampleModel: TSQLModel;
begin
  result := TSQLModel.Create([TSQLSampleRecord]);
end;

end.
