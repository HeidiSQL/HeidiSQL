/// this server will use TSQLRestServerFullMemory over HTTP
program Project14ServerHttp;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  SynCommons,
  SQLite3Commons,
  SQLite3HttpServer,
  Project14Interface;

type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1,n2: integer): integer;
  end;

function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1+n2;
end;


var
  aModel: TSQLModel;
  aServer: TSQLRestServer;
  aHTTPServer: TSQLite3HttpServer;
begin
  aModel := TSQLModel.Create([],ROOT_NAME);
  try
    aServer := TSQLRestServerFullMemory.Create(aModel,'test.json',false,true);
    try
      aServer.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
      aHTTPServer := TSQLite3HttpServer.Create('888',[aServer]);
      try
        writeln('Background server is running.'#10);
        write('Press [Enter] to close the server.');
        readln;
      finally
        aHTTPServer.Free;
      end;
    finally
      aServer.Free;
    end;
  finally
    aModel.Free;
  end;
end.
