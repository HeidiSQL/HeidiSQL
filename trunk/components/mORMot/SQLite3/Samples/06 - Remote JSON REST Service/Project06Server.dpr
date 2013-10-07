program Project06Server;

{$APPTYPE CONSOLE}

uses
  SynCommons,
  SQLite3Commons,
  SysUtils;

type
  // TSQLRestServerFullMemory kind of server is light and enough for our purpose
  TServiceServer = class(TSQLRestServerFullMemory)
  published
    function Sum(var aParams: TSQLRestServerCallBackParams): Integer;
  end;


{ TServiceServer }

function TServiceServer.Sum(var aParams: TSQLRestServerCallBackParams): Integer;
var a,b: Extended;
begin
  if not UrlDecodeNeedParameters(aParams.Parameters,'A,B') then begin
    result := 404; // invalid Request
    aParams.ErrorMsg^ := 'Missing Parameter'; // custom error message
    exit;
  end;
  while aParams.Parameters<>nil do begin
    UrlDecodeExtended(aParams.Parameters,'A=',a);
    UrlDecodeExtended(aParams.Parameters,'B=',b,@aParams.Parameters);
  end;
  aParams.Resp := JSONEncodeResult([a+b]);
  // same as : aResp := JSONEncode(['result',a+b],TempMemoryStream);
  result := 200; // success
end;

var
  aModel: TSQLModel;
begin
  aModel := TSQLModel.Create([],'service');
  try
    with TServiceServer.Create(aModel) do
    try
      if ExportServerNamedPipe('RestService') then
        writeln('Background server is running.'#10) else
        writeln('Error launching the server'#10);
      write('Press [Enter] to close the server.');
      readln;
    finally
      Free;
    end;
  finally
    aModel.Free;
  end;
end.
