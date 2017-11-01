program Hook_Overloaded_Method;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CPUID in '..\..\..\..\Source\CPUID.pas',
  DDetours in '..\..\..\..\Source\DDetours.pas',
  InstDecode in '..\..\..\..\Source\InstDecode.pas';

type
  TShowMsg = procedure(Value: Integer);

var

  TrampoShowMsg: TShowMsg;
  { When hooking overloaded method ,
    Delphi does not reconize the desired method .
    So we need to use this trick!
  }
  MyMethodPtr: TShowMsg;

procedure ShowMsg(const S: String); overload;
begin
  Writeln(S);
end;

procedure ShowMsg(Value: Integer); overload;
begin
  Writeln(Value);
end;

procedure ShowMsgHooked(Value: Integer);
begin
  Writeln('Method hooked successfully!');
  TrampoShowMsg(Value + 1);
end;

begin
  MyMethodPtr := ShowMsg;
  @TrampoShowMsg := InterceptCreate(@MyMethodPtr, @ShowMsgHooked);
  ShowMsg('Hi');
  ShowMsg(2015);
  ReadLn;

end.
