{********************************************************************
 * Workaround for the Integrated Debugger Libc.execve bug           *
 *                                                                  *
 * (C) 2003 Andreas Hausladen (Andreas.Hausladen@gmx.de)            *
 *                                                                  *
 *                                                                  *
 * This software is provided 'as-is', without any express or        *
 * implied warranty. In no event will the author be held liable     *
 * for any damages arising from the use of this software.           *
 *                                                                  *
 * Permission is granted to anyone to use this software for any     *
 * purpose, including commercial applications, and to alter it      *
 * and redistribute it freely, subject to the following             *
 * restrictions:                                                    *
 *                                                                  *
 *   1. The origin of this software must not be misrepresented,     *
 *      you must not claim that you wrote the original software.    *
 *      If you use this software in a product, an acknowledgment    *
 *      in the product documentation would be appreciated but is    *
 *      not required.                                               *
 *                                                                  *
 *   2. Altered source versions must be plainly marked as such, and *
 *      must not be misrepresented as being the original software.  *
 *                                                                  *
 *   3. This notice may not be removed or altered from any source   *
 *      distribution.                                               *
 *                                                                  *
 ********************************************************************}

unit LibcExec;
{$ALIGN 8}
{$DEBUGINFO OFF}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$STACKFRAMES OFF}
interface

implementation
{$IFDEF LINUX}
uses
  Libc;

const
  SYS_execve = 11;

{$OPTIMIZATION OFF}
function __execve(PathName: PChar; const argv: PPChar;
  const envp: PPChar): Integer; cdecl;
begin
  { Do exactly the same things that the original execve@GLIBC_2.1 does. }
  pthread_kill_other_threads_np;
  Result := Libc.syscall(SYS_execve, PathName, argv, envp);
end;
{$OPTIMIZATION ON}

procedure InitExecveRedirect;
type
  PRedirectCodeRec = ^TRedirectCodeRec;
  TRedirectCodeRec = packed record
    Code: Byte;         // $E9          jmp Distance
    Distance: Integer;
  end;
const
  Prot = PROT_READ or PROT_WRITE or PROT_EXEC;
  Size = SizeOf(TRedirectCodeRec);
var
  Addr: PRedirectCodeRec;
  AlignedAddr: Cardinal;
  PageSize: Cardinal;
begin
  PageSize := getpagesize;
  Addr := dlsym(Pointer(-1), 'execve');
  { Obtain write access to the memory page. }
  AlignedAddr := Cardinal(Addr) and not (PageSize - 1);
  if Cardinal(Addr) + Size >= AlignedAddr + PageSize then
    PageSize := PageSize * 2;
  mprotect(Pointer(AlignedAddr), PageSize, Prot);
  { Redirect Libc.execve }
  Addr^.Code := $E9;
  Addr^.Distance := Integer(@__execve) - Integer(Addr) - Size;
end;


initialization

  { Maybe someone has a better code to detect integrated debugging. This code
    tests only for $DELPHI/$BCB environment variable. }
  if (getenv('DELPHI') <> nil) or (getenv('BCB') <> nil) then
    // started within Kylix IDE
    InitExecveRedirect;

{$ENDIF}
end.

