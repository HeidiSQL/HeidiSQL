{
  SizeGripTest.dpr

  This file is part of the SizeGripper.pas sample application.
  Info at http://flocke.vssd.de/prog/code/pascal/sizegrip/

  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  All rights reserved.
}

program SizeGripTest;

uses
  Forms,
  main in 'main.pas' {Form1},
  SizeGrip in '..\SizeGrip.pas',
  SizeGripThemed in '..\SizeGripThemed.pas',
  SizeGripHWND in '..\SizeGripHWND.pas';

{$R *.res}
{$R manifest.res}

begin
  Application.Initialize;
  Application.Title := 'SizeGrip Test';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
