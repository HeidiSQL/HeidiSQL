unit helpers;

interface

type
  procedure ExtractUpdater;
  procedure UpdateItWith(const _file: String);

implementation

{$R updater.res}


{**
  Extract the updater from resource.
}
procedure ExtractUpdater;
var
  ri: HRSRC;
  rh: THandle;
  bf: PChar;
  ms: TMemoryStream;
begin
  // look for resource UPDATER
  ri := FindResource( HInstance, 'UPDATER', 'EXE' );

  // define the Handle
  rh := LoadResource( HInstance, ri );
  
  if ( rh <> 0 ) then
  begin
    // alloc memory space
    ms := TMemoryStream.Create();
    try
      ms.Clear();
      bf := LockResource( rh );

      // load the resource on memory stream
      ms.WriteBuffer( bf[0], SizeOfResource( HInstance, ri ) );
      ms.Seek( 0, 0 );

      // save the resource like executable
      ms.SaveToFile( ExtractFilePath( Application.ExeName ) + 'updater.exe' );
    finally
      // free memory
      UnlockResource( rh );
      FreeResource( rh );
      ms.Free();
    end;
  end;
end;



{**
  Update the current application with the file.

  @param _file the file that will replace the current application
}
procedure UpdateItWith(const _file: String);
var
  app: String;
begin
  // sanitize: try to remove the oldest updater
  DeleteFile( PAnsiChar( ExtractFilePath( Application.ExeName ) + 'updater.exe' ) );

  // retrive the application name without extension
  app := Copy( Application.Exename, 1, ( Length( Application.Exename ) - 4 ) );

  // sanitize: try to remove the oldest file
  DeleteFile( PAnsiChar( app + '.old' ) );

  // verify the file existence
  if ( FileExists( _file ) ) then
  begin
    // copy to current application folder the newest file
    CopyFile( PChar( _file ), PChar( app + '.new' ), False );

    // extract the updater from current application resource
    ExtractUpdater();

    // call the updater
    WinExec( PAnsiChar( '"' + ExtractFilePath( Application.ExeName ) + 'updater.exe" "' + app + '"' ), 0 );

    // stop running the current application
    Application.Terminate();
  end;
end;



