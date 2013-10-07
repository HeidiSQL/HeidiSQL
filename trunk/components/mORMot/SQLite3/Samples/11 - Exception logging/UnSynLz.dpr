/// sample program able to uncompres .log.synlz archived files into visualizable
// .log files as created by TSynLog
// - if some .synlz file name or wildchar pattern is specified as command line
// parameter, it will process all matching files
// - if no file name nor pattern is specified in the command line, will search
// for '*.synlz' in the current folder
// - uncompression will be stored in the same directory as the original .synlz
// - you can make unsynlz.exe file small if you define LVCL as conditional in
// the Project options and set the ..\lib\LVCL directories as expected
program UnSynLz;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SynCommons;

procedure Process(const FileName: TFileName);
var SR: TSearchRec;
    Path: TFileName;
begin
  if (GetFileNameExtIndex(FileName,'synlz')=0) and
     (FindFirst(FileName,faAnyFile,SR)=0) then
  try
    Path := ExtractFilePath(FileName);
    repeat
      if (SR.Name[1]='.') or (faDirectory and SR.Attr<>0) then
        Continue;
      write(SR.Name);
      if FileUnSynLZ(Path+SR.Name,Path+copy(SR.Name,1,length(SR.Name)-6),LOG_MAGIC) then
        writeln(' OK') else
        writeln(' Error');
    until FindNext(SR)<>0;
  finally
    FindClose(SR);
  end;
end;

begin
  if paramCount>0 then
    Process(paramstr(1)) else
    Process('*.synlz');
end.
