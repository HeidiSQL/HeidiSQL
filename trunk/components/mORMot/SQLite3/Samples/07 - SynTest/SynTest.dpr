program SynTest;

{$APPTYPE CONSOLE}

uses
  SynTestTest;

begin
  with TTestSuit.Create do
  try
    Run;
    readln;
  finally
    Free;
  end;
end.
