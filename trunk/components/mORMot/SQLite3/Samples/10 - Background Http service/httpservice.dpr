/// implements a background Service serving HTTP pages
program HttpService;

uses
  {$I SynDprUses.inc}
  Windows,
  Classes,
  SysUtils,
  WinSvc,
  SynCommons,
  SQLite3Service,
  SQLite3Commons,
  SQLite3,
  SQLite3HTTPServer,
  SampleData in '..\01 - In Memory ORM\SampleData.pas';

// define this conditional if you want the GDI messages to be accessible
// from the background service
{.$define USEMESSAGES}


/// we will run the service with administrator rights
{$R VistaAdm.res}

type
  /// class implementing the background Service
  TSQLite3HttpService = class(TService)
  public
    /// the associated database model
    Model: TSQLModel;
    /// the associated DB
    DB: TSQLRestServerDB;
    /// the background Server processing all requests
    Server: TSQLite3HttpServer;

    /// event trigerred to start the service
    // - e.g. create the Server instance
    procedure DoStart(Sender: TService);
    /// event trigerred to stop the service
    // - e.g. destroy the Server instance
    procedure DoStop(Sender: TService);

    /// initialize the background Service
    constructor Create; reintroduce;
    /// release memory
    destructor Destroy; override;
  end;


const
  HTTPSERVICENAME = 'mORMotHttpServerService';
  HTTPSERVICEDISPLAYNAME = 'mORMot Http Server Service';


{ TSQLite3HttpService }

constructor TSQLite3HttpService.Create;
begin
  inherited Create(HTTPSERVICENAME,HTTPSERVICEDISPLAYNAME);
  OnStart := DoStart;
  OnStop := DoStop;
  OnResume := DoStart; // trivial Pause/Resume actions
  OnPause := DoStop;
end;

destructor TSQLite3HttpService.Destroy;
begin
  DoStop(nil);
  inherited;
end;

procedure TSQLite3HttpService.DoStart(Sender: TService);
begin
  if Server<>nil then
    DoStop(nil); // should never happen
  Model := CreateSampleModel;
  DB := TSQLRestServerDB.Create(Model,ChangeFileExt(paramstr(0),'.db3'));
  DB.CreateMissingTables(0);
  Server := TSQLite3HttpServer.Create('8080',[DB]);
  TSQLLog.Family.Level := LOG_VERBOSE;
  TSQLLog.Add.Log(sllInfo,'Server % started by %',[Server.HttpServer,Server]);
end;

procedure TSQLite3HttpService.DoStop(Sender: TService);
begin
  if Server=nil then
    exit;
  TSQLLog.Add.Log(sllInfo,'Server % stopped by %',[Server.HttpServer,Server]);
  FreeAndNil(Server);
  FreeAndNil(DB);
  FreeAndNil(Model);
end;

procedure CheckParameters;
var i: integer;
    param: string;
begin
  with TServiceController.CreateOpenService('','',HTTPSERVICENAME) do
  // allow to control the service
  try
    if State<>ssErrorRetrievingState then
      for i := 1 to ParamCount do begin
        param := SysUtils.LowerCase(paramstr(i));
        if param='/install' then
          TServiceController.CreateNewService('','',HTTPSERVICENAME,
              HTTPSERVICEDISPLAYNAME, paramstr(0),'','','','',
              SERVICE_ALL_ACCESS,
              SERVICE_WIN32_OWN_PROCESS
                {$ifdef USEMESSAGES}or SERVICE_INTERACTIVE_PROCESS{$endif},
              SERVICE_AUTO_START).  // auto start at every boot
            Free else
        if param='/uninstall' then begin
           Stop;
           Delete;
        end else
        if param='/stop' then
          Stop else
        if param='/start' then
          Start([]);
      end;
  finally
    Free;
  end;
end;

begin
  if ParamCount<>0 then
    CheckParameters else
    with TSQLite3HttpService.Create do
    try
      // launches the registered Services execution = do all the magic
      ServicesRun;
    finally
      Free;
    end;
end.
