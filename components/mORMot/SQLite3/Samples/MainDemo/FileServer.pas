/// SynFile server handling
unit FileServer;

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  SysUtils,
  Classes,
  SynCommons,
  SQLite3Commons,
  SQLite3i18n,
  SQLite3HttpServer,
  SQLite3,
  FileTables;

type
  /// a server to access SynFile data content
  TFileServer = class(TSQLRestserverDB)
  private
    fTempAuditTrail: TSQLAuditTrail;
  public
    /// the runing HTTP/1.1 server
    Server: TSQLite3HttpServer;
  	/// create the database and HTTP/1.1 server
    constructor Create;
	  /// release used memory and data
    destructor Destroy; override;
  	/// add a row to the TSQLAuditTrail table
    procedure AddAuditTrail(aEvent: TFileEvent; const aMessage: RawUTF8='';
      aAssociatedRecord: TRecordReference=0);
	  /// database server-side trigger which will add an event to the
  	// TSQLAuditTrail table
    function OnDatabaseUpdateEvent(Sender: TSQLRestServer;
      Event: TSQLEvent; aTable: TSQLRecordClass; aID: integer): boolean;
  published
    /// a RESTful service used from the client side to add an event
  	// to the TSQLAuditTrail table
	  // - an optional database record can be specified in order to be
  	// associated with the event
    function Event(var aParams: TSQLRestServerCallBackParams): Integer;
  end;


implementation


{ TFileServer }

procedure TFileServer.AddAuditTrail(aEvent: TFileEvent;
  const aMessage: RawUTF8; aAssociatedRecord: TRecordReference);
var T: TSQLRecordClass;
    tmp: RawUTF8;
begin
  if fTempAuditTrail=nil then
    fTempAuditTrail := TSQLAuditTrail.Create;
  fTempAuditTrail.Time := Iso8601Now;
  fTempAuditTrail.Status := aEvent;
  fTempAuditTrail.StatusMessage := aMessage;
  fTempAuditTrail.AssociatedRecord := aAssociatedRecord;
  if (aMessage='') and (aAssociatedRecord<>0) then
    with RecordRef(aAssociatedRecord) do begin
      T := Table(Model);
      if T.InheritsFrom(TSQLFile) then
        tmp := '"'+OneFieldValue(T,'Name',ID)+'"' else
        tmp := {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(ID);
      fTempAuditTrail.StatusMessage := T.RecordProps.SQLTableName+'  '+tmp;
    end;
  Add(fTempAuditTrail,true);
end;

constructor TFileServer.Create;
begin
  inherited Create(CreateFileModel(self),ChangeFileExt(paramstr(0),'.db3'));
  CreateMissingTables(ExeVersion.Version.Version32);
  Server := TSQLite3HttpServer.Create(SERVER_HTTP_PORT,self);
  AddAuditTrail(feServerStarted);
  OnUpdateEvent := OnDatabaseUpdateEvent;
end;

destructor TFileServer.Destroy;
begin
  try
    AddAuditTrail(feServerShutdown);
    FreeAndNil(fTempAuditTrail);
    FreeAndNil(Server);
  finally
    inherited;
  end;
end;

function TFileServer.Event(var aParams: TSQLRestServerCallBackParams): Integer;
var E: integer;
begin
  if UrlDecodeInteger(aParams.Parameters,'EVENT=',E) and
     (E>0) and (E<=ord(High(TFileEvent))) then begin
    AddAuditTrail(TFileEvent(E),'',RecordReference(Model,aParams.Table,aParams.Context.ID));
    result := HTML_SUCCESS;
  end else
    result := HTML_BADREQUEST;
end;

function TFileServer.OnDatabaseUpdateEvent(Sender: TSQLRestServer;
  Event: TSQLEvent; aTable: TSQLRecordClass; aID: integer): boolean;
const EVENT_FROM_SQLEVENT: array[TSQLEvent] of TFileEvent = (
  feRecordCreated, feRecordModified, feRecordDeleted);
begin
  result := true;
  if aTable.InheritsFrom(TSQLFile) then
    AddAuditTrail(EVENT_FROM_SQLEVENT[Event], '', Model.RecordReference(aTable,aID));
end;

end.
