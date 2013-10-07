/// SynFile ORM definitions shared by both client and server
unit FileTables;

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  SysUtils,
  Classes,
  SynCommons,
  SynCrypto,
  SynZip,
  SQLite3Commons,
  SQLite3i18n;

type
  /// the internal events/states, as used by the TSQLAuditTrail table
  TFileEvent = (
    feUnknownState,
    feServerStarted,
    feServerShutdown,
    feRecordCreated,
    feRecordModified,
    feRecordDeleted,
    feRecordDigitallySigned,
    feRecordImported,
    feRecordExported
  );

  /// the internal available actions, as used by the User Interface
  TFileAction = (
    faNoAction,
    faMark,
    faUnmarkAll,
    faQuery,
    faRefresh,
    faCreate,
    faEdit,
    faCopy,
    faExport,
    faImport,
    faDelete,
    faSign,
    faPrintPreview,
    faExtract,
    faSettings
  );
  /// set of available actions
  TFileActions = set of TFileAction;

  /// some actions to be used by the User Interface of a Preview window
  TPreviewAction = (
    paPrint, paAsPdf, paAsText,
    paWithPicture, paDetails
  );
  TPreviewActions = set of TPreviewAction;

  /// an abstract class, with common fields
  TSQLFile = class(TSQLRecordSigned)
  public
    fName: RawUTF8;
    fModified: TTimeLog;
    fCreated: TTimeLog;
    fPicture: TSQLRawBlob;
    fKeyWords: RawUTF8;
  published
    property Name: RawUTF8 read fName write fName;
    property Created: TTimeLog read fCreated write fCreated;
    property Modified: TTimeLog read fModified write fModified;
    property Picture: TSQLRawBlob read fPicture write fPicture;
    property KeyWords: RawUTF8 read fKeyWords write fKeyWords;
    property SignatureTime;
    property Signature;
  end;

  /// an uncrypted Memo table
  // - will contain some text
  TSQLMemo = class(TSQLFile)
  public
    fContent: RawUTF8;
  published
    property Content: RawUTF8 read fContent write fContent;
  end;

  /// an uncrypted Data table
  // - can contain any binary file content
  // - is also used a parent for all cyphered tables (since the
  // content is crypted, it should be binary, i.e. a BLOB field)
  TSQLData = class(TSQLFile)
  public
    fData: TSQLRawBlob;
  published
    property Data: TSQLRawBlob read fData write fData;
  end;

  /// a crypted SafeMemo table
  // - will contain some text after AES-256 cypher
  // - just a direct sub class ot TSQLData to create the "SafeMemo" table
  // with the exact same fields as the "Data" table
  TSQLSafeMemo = class(TSQLData);

  /// a crypted SafeData table
  // - will contain some binary file content after AES-256 cypher
  // - just a direct sub class ot TSQLData to create the "SafeData" table
  // with the exact same fields as the "Data" table
  TSQLSafeData = class(TSQLData);

  /// an AuditTrail table, used to track events and status
  TSQLAuditTrail = class(TSQLRecord)
  protected
    fStatusMessage: RawUTF8;
    fStatus: TFileEvent;
    fAssociatedRecord: TRecordReference;
    fTime: TTimeLog;
  published
    property Time: TTimeLog read fTime write fTime;
    property Status: TFileEvent read fStatus write fStatus;
    property StatusMessage: RawUTF8 read fStatusMessage write fStatusMessage;
    property AssociatedRecord: TRecordReference read fAssociatedRecord write fAssociatedRecord;
  end;

  /// the type of custom main User Interface description of SynFile
  TFileRibbonTabParameters = object(TSQLRibbonTabParameters)
    /// the SynFile actions
    Actions: TFileActions;
  end;

const
  /// will define the first User Interface ribbon group, i.e. main tables
  GROUP_MAIN = 0;
  /// will define the 2nd User Interface ribbon group, i.e. uncrypted tables
  GROUP_CLEAR = 1;
  /// will define the 3d User Interface ribbon group, i.e. crypted tables
  GROUP_SAFE = 2;
  /// some default actions, available for all tables
  DEF_ACTIONS = [faMark..faPrintPreview,faSettings];
  /// actions available for data tables (not for TSQLAuditTrail)
  DEF_ACTIONS_DATA = DEF_ACTIONS+[faExtract]-[faImport,faExport];
  /// default fields available for User Interface Grid
  DEF_SELECT = 'Name,Created,Modified,KeyWords,SignatureTime';
  /// the TCP/IP port used for the HTTP server
  // - this is shared as constant by both client and server side
  // - in a production application, should be made customizable
  SERVER_HTTP_PORT = '888';

const
  /// this constant will define most of the User Interface property
  // - the framework will create most User Interface content from the
  // values stored within
  FileTabs: array[0..4] of TFileRibbonTabParameters = (
    (Table: TSQLAuditTrail;
     Select: 'Time,Status,StatusMessage'; Group: GROUP_MAIN;
     FieldWidth: 'gIZ'; ShowID: true; ReverseOrder: true; Layout: llClient;
     Actions: [faDelete,faMark,faUnmarkAll,faQuery,faRefresh,faPrintPreview,faSettings]),
    (Table: TSQLMemo;
     Select: DEF_SELECT; Group: GROUP_CLEAR; FieldWidth: 'IddId'; Actions: DEF_ACTIONS),
    (Table: TSQLData;
     Select: DEF_SELECT; Group: GROUP_CLEAR; FieldWidth: 'IddId'; Actions: DEF_ACTIONS_DATA),
    (Table: TSQLSafeMemo;
     Select: DEF_SELECT; Group: GROUP_SAFE; FieldWidth: 'IddId';  Actions: DEF_ACTIONS),
    (Table: TSQLSafeData;
     Select: DEF_SELECT; Group: GROUP_SAFE; FieldWidth: 'IddId';  Actions: DEF_ACTIONS_DATA)
  );
  /// used to map which actions/buttons must be grouped in the toolbar
  FileActionsToolbar: array[0..3] of TFileActions =
    ( [faRefresh,faCreate,faEdit,faCopy,faExtract], [faExport..faPrintPreview],
      [faMark..faQuery], [faSettings] );
  /// FileActionsToolbar[FileActionsToolbar_MARKINDEX] will be the marked actions
  // i.e. [faMark..faQuery]
  FileActionsToolbar_MARKINDEX = 2;

resourcestring
  sFileTabsGroup = 'Main,Clear,Safe';
  sFileActionsToolbar = '%%,Record Managment,Select,Settings';
  sFileActionsHints =
  'Mark rows'#13+   { faMark }
  'UnMark all rows'#13+   { faUnmarkAll }
  'Perform a custom query on the list and mark resulting rows'#13+   { faQuery }
  'Refresh the current list from the database'#13+   { faRefresh }
  'Create a new empty %s'#13+   { faCreate }
  'Edit this %s'#13+   { faEdit }
  'Create a new %s, with the same initial values as in the current selected record'#13+   { faCopy }
  'Export one or more %s records report'#13+   { faExport }
  'Import one or multiple files as %s records'#13+   { faImport }
  'Delete the selected %s records'#13+   { faDelete }
  'Digitally sign the selected %s'#13+   { faSign }
  'Print one or more %s Reports'#13+   { faPrintPreview }
  'Extract the embedded file into any folder'#13+  { faExtract }
  'Change the Program''s settings';   { faSettings }

/// create the database model to be used
// - shared by both client and server sides
function CreateFileModel(Owner: TSQLRest): TSQLModel;


implementation

function CreateFileModel(Owner: TSQLRest): TSQLModel;
var Classes: array[0..high(FileTabs)] of TSQLRecordClass;
    i: integer;
begin
  for i := 0 to high(FileTabs) do
    Classes[i] := FileTabs[i].Table;
  result := TSQLModel.Create(Classes,'root');
  result.Owner := Owner;
  result.SetActions(TypeInfo(TFileAction));
  result.SetEvents(TypeInfo(TFileEvent));
end;


initialization
  ExeVersionRetrieve(3);
end.
