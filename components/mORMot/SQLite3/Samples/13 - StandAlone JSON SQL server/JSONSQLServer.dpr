/// serve SQLite3 results from SQL using HTTP server
program JSONSQLServer;

{

  This "13 - StandAlone JSON SQL server" sample's aim is to directly
    serve SQLite3 JSON results from SQL using HTTP server.

  It will expect the incoming SQL statement to be POSTED as HTTP body, which
  will be executed and returned as JSON.

  This default implementation will just serve the Test.db3 file as generated
  by our regression tests.

  But it is a very rough mechanism:
  - No security is included;
  - You can make your process run out of memory if the request returns too much rows;
  - All incoming inputs will not be checked;
  - No statement cache is used;
  - No test was performed;

  Therefore, this method is much less efficient than the one implemented by mORMot.
  This is just a rough sample - do not use it in production - you shall better
  use the mORMot framework instead.

  Using SynDB classes instead of directly SynSQLite3 will allow to use any other DB,
  not only SQlite3.

  see http://synopse.info/forum/viewtopic.php?id=607 for the initial request
  
}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  SynCommons,
  SynZip,
  SynDB,
  SynDBSQLite3,
  SynCrtSock;

type
  TJSONServer = class
  protected
    fProps: TSQLDBConnectionProperties;
    fServer: THttpApiServer;
    function Process(
      const InURL, InMethod, InHeaders, InContent, InContentType: RawByteString;
      out OutContent, OutContentType, OutCustomHeader: RawByteString): cardinal;
  public
    constructor Create(Props: TSQLDBConnectionProperties);
    destructor Destroy; override;
  end;


{ TJSONServer }

constructor TJSONServer.Create(Props: TSQLDBConnectionProperties);
var Conn: TSQLDBConnection;
begin
  fProps := Props;
  Conn := fProps.ThreadSafeConnection;
  if not Conn.Connected then
    Conn.Connect;
  fServer := THttpApiServer.Create(false);
  fServer.AddUrl('root','888',false,'+');
  fServer.RegisterCompress(CompressDeflate); // our server will deflate JSON :)
  fServer.OnRequest := Process;
end;

destructor TJSONServer.Destroy;
begin
  fServer.Free;
  inherited;
end;

function TJSONServer.Process(const InURL, InMethod, InHeaders, InContent,
  InContentType: RawByteString; out OutContent, OutContentType,
  OutCustomHeader: RawByteString): cardinal;
begin
  try
    if length(InContent)<5 then
      raise Exception.CreateFmt('Invalid request %s %s',[InMethod,InURL]);
    OutContentType := JSON_CONTENT_TYPE;
    OutContent := fProps.Execute(InContent,[]).FetchAllAsJSON(true);
    result := 200;
  except
    on E: Exception do begin
      OutContentType := TEXT_CONTENT_TYPE;
      OutContent := StringToAnsi7(E.ClassName+': '+E.Message)+#13#10+InContent;
      result := 504;
    end;
  end;
end;

var Props: TSQLDBConnectionProperties;
begin
  Props := TSQLDBSQLite3ConnectionProperties.Create(
    StringToUTF8('..\..\exe\test.db3'),'','','');
  try
    with TJSONServer.Create(Props) do
    try
      write('Server is now running on http://localhost:888/root'#13#10+
        'and will serve ',ExpandFileName(UTF8ToString(Props.ServerName)),
        ' content'#13#10#13#10'Press [Enter] to quit');
      readln;
    finally
      Free;
    end;
  finally
    Props.Free;
  end;
end.
