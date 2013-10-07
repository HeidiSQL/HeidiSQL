/// sample program which will serve C:\ content on http://localhost:888/root
program HttpApiServer;

{$APPTYPE CONSOLE}

uses
  //{$I SynDprUses.inc}
  SysUtils, 
  SynCommons,
  SynZip,
  SynCrtSock;

type
  TTestServer = class
  protected
    fPath: TFileName;
    fServer: THttpApiServer;
    function Process(
      const InURL, InMethod, InHeaders, InContent, InContentType: TSockData;
      out OutContent, OutContentType, OutCustomHeader: TSockData): cardinal;
  public
    constructor Create(const Path: TFileName);
    destructor Destroy; override;
  end;


{ TTestServer }

constructor TTestServer.Create(const Path: TFileName);
begin
  fServer := THttpApiServer.Create(false);
  fServer.AddUrl('root','888',false,'+');
  fServer.RegisterCompress(CompressDeflate); // our server will deflate html :)
  fServer.OnRequest := Process;
  fPath := IncludeTrailingPathDelimiter(Path);
end;

destructor TTestServer.Destroy;
begin
  fServer.Free;
  inherited;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TTestServer.Process(
      const InURL, InMethod, InHeaders, InContent, InContentType: TSockData;
      out OutContent, OutContentType, OutCustomHeader: TSockData): cardinal;
var W: TTextWriter;
    FileName: TFileName;
    FN, SRName, href: RawUTF8;
    i: integer;
    SR: TSearchRec;
    
  procedure hrefCompute;
  begin
    SRName := StringToUTF8(SR.Name);
    href := FN+StringReplaceChars(SRName,'\','/');
  end;

begin
  if not IdemPChar(pointer(InURL),'/ROOT') then begin
    result := 404;
    exit;
  end;
  FN := StringReplaceChars(UrlDecode(copy(InURL,7,maxInt)),'/','\');
  while (FN<>'') and (FN[1]='\') do
    delete(FN,1,1);
  while (FN<>'') and (FN[length(FN)]='\') do
    delete(FN,length(FN),1);
  FileName := fPath+UTF8ToString(FN);
  if DirectoryExists(FileName) then begin
    // reply directory listing as html
    W := TTextWriter.CreateOwnedStream;
    try
      W.Add('<html><body style="font-family: Arial">'+
        '<h3>%</h3><p><table>',[FN]);
      FN := StringReplaceChars(FN,'\','/');
      if FN<>'' then
        FN := FN+'/';
      if FindFirst(FileName+'\*.*',faDirectory,SR)=0 then begin
        repeat
          if (SR.Attr and faDirectory<>0) and (SR.Name<>'.') then begin
            hrefCompute;
            if SRName='..' then begin
              i := length(FN);
              while (i>0) and (FN[i]='/') do dec(i);
              while (i>0) and (FN[i]<>'/') do dec(i);
              href := copy(FN,1,i);
            end;
            W.Add('<tr><td><b><a href="/root/%">[%]</a></b></td></tr>',[href,SRName]);
          end;
        until FindNext(SR)<>0;
        FindClose(SR);
      end;
      if FindFirst(FileName+'\*.*',faAnyFile-faDirectory-faHidden,SR)=0 then begin
        repeat
          hrefCompute;
          if SR.Attr and faDirectory=0 then
            W.Add('<tr><td><b><a href="/root/%">%</a></b></td><td>%</td><td>%</td></td></tr>',
              [href,SRName,KB(SR.Size),DateTimeToStr(FileDateToDateTime(SR.Time))]);
        until FindNext(SR)<>0;
        FindClose(SR);
      end;
      W.AddString('</table></p><p><i>Powered by <strong>THttpApiServer</strong></i> - '+
        'see <a href=http://synopse.info>http://synopse.info</a></p></body></html>');
      OutContent := W.Text;
      OutContentType := HTML_CONTENT_TYPE;
      result := 200;
    finally
      W.Free;
    end;
  end else begin
    // http.sys will send the specified file from kernel mode
    OutContent := StringToUTF8(FileName);
    OutContentType := HTTP_RESP_STATICFILE;
    result := 200; // THttpApiServer.Execute will return 404 if not found
  end;
end;

begin
  with TTestServer.Create('C:\') do
  try
    write('Server is now running on http://localhost:888/root'#13#10#13#10+
      'Press [Enter] to quit');
    readln;
  finally
    Free;
  end;
end.