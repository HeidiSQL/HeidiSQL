/// library sample code, which makes use of the logging feature
library MyLibrary;

{
  In the Project / Options / Linker tab, the Map files option should be set
  to detailed, in order to demonstrate how libraries can have their own
  symbols file (we need a .map to have this information and create its .mab)

}
uses
  SynCommons;

{$R *.res}

procedure Test;
begin
   TSynLog.Family.Level := LOG_VERBOSE;
   TSynLog.Enter.Log(sllDebug, 'Called from Test exported procedure');
end;

exports Test;

end.
