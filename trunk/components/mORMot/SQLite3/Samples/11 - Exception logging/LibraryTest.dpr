program LibraryTest;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure Test; external 'MyLibrary.dll';

begin
  Test;
end.
