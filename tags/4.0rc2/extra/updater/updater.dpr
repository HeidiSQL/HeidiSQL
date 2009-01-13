program updater;

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows;

const
  _maxloop = 10;

procedure Update;
var
  _currentfilename: String;
  _oldfilename: String;
  _newfilename: String;
  _minloop: Integer;
begin
  if ( ParamCount > 0 ) then
  begin
    _currentfilename := ParamStr(1) + '.exe';
    _oldfilename := ParamStr(1) + '.old';
    _newfilename := ParamStr(1) + '.new';
	
    // try to delete the old file
    if ( FileExists( _oldfilename ) ) then
    begin
      DeleteFile( PAnsiChar( _oldfilename ) );
    end;
	
    // try to execute
    _minloop := 0;
    repeat
      // try to rename the current filename
      if ( RenameFile( _currentfilename, _oldfilename ) ) then
      begin
        // rename the new file with the current filename
        RenameFile( _newfilename, _currentfilename );
		
        // call again the application
        WinExec( PAnsiChar( _currentfilename ), 0 );
        Exit;
      end;
	  
      // if doesn't rename is because the application still running, so
      // wait for 2 seconds and try again
      Sleep( 2000 );
      _minloop := _minloop + 1;
    until ( _minloop <= _maxloop );
  end;
end;

begin
  Update();
end.
