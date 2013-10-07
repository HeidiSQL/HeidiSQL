program Project14Server;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SQLite3Commons,
  SQLite3,
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
    with TSQLRestServerDB.Create(aModel,ChangeFileExt(paramstr(0),'.db'),true) do
    try
      CreateMissingTables; // we need AuthGroup and AuthUser tables
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
