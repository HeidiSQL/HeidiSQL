// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program Demo1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  InstDecode in '..\..\..\Source\InstDecode.pas';

procedure Foo;
asm
  {$IFDEF CPUX64}
  PUSH RAX
  XOR EAX,EAX
  MOV EAX,5
  ADD EAX,EDX
  POP RAX
  NOP
  NOP
  NOP
  MOV RAX,1
  {$ELSE !CPUX64}
  PUSH EAX
  XOR EAX,EAX
  MOV EAX,5
  ADD EAX,EDX
  POP EAX
  NOP
  NOP
  NOP
  MOV EAX,1
  {$ENDIF CPUX64}
end;

var
  Inst: TInstruction;
  nInst: Integer;

begin
  // Foo;
  Inst := Default (TInstruction);
  Inst.Archi := CPUX;
  Inst.NextInst := @Foo;
  nInst := 0;
  while (Inst.OpType <> otRET) do
  begin
    inc(nInst);
    Inst.Addr := Inst.NextInst;
    DecodeInst(@Inst);
    Writeln(Format('OpCode : 0x%.2x | Length : %d', [Inst.OpCode, Inst.InstSize]));
  end;
  Writeln('-------------------------------');
  Writeln(Format('Total instructions : %d', [nInst]));
  ReadLn;

end.
