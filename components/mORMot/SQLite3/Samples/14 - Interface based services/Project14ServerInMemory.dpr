/// this server will use TSQLRestServerFullMemory kind of in-memory server
// - it will only by 200 KB big with LVCL :)
program Project14ServerInMemory;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  SynCommons,
  SQLite3Commons,
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
begin
  aModel := TSQLModel.Create([],ROOT_NAME);
  try
    with TSQLRestServerFullMemory.Create(aModel,'test.json',false,true) do
    try
      ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
      if ExportServerNamedPipe(APPLICATION_NAME) then
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
